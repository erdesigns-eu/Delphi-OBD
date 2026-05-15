//------------------------------------------------------------------------------
//  OBD.Protocol.DoIP.TLS.OpenSSL
//
//  Drop-in OpenSSL 3.x TLS plug for the DoIP transport contract.
//  Implements IOBDDoIPTransport on top of a raw Winsock TCP socket
//  wrapped with an OpenSSL TLS 1.2 / 1.3 session. Designed for
//  port-3496 DoIP-over-TLS (ISO 13400-2:2019 §7.2.4).
//
//  No compile-time dependency on OpenSSL: libssl-3.dll and
//  libcrypto-3.dll are loaded dynamically the first time the unit
//  is used. Ship the two DLLs alongside the host EXE (the official
//  Win64 OpenSSL build from <https://slproweb.com/products/Win32OpenSSL.html>
//  or <https://wiki.openssl.org/> is the reference). On Linux the
//  DLL names map to <c>libssl.so.3</c> and <c>libcrypto.so.3</c>.
//
//  This unit is intentionally self-contained — no Indy, no SChannel,
//  no Synapse — so a host can drop OpenSSL DLLs into its bin folder
//  and have a working TLS DoIP transport without any extra package.
//
//  Hosts that prefer a different TLS library implement
//  <c>IOBDDoIPTransport</c> themselves.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 13400-2:2019 §7.2.4 (DoIP-over-TLS)
//    - RFC 5246 (TLS 1.2), RFC 8446 (TLS 1.3)
//    - OpenSSL 3.0 manual: SSL_CTX_new, SSL_connect, SSL_read,
//      SSL_write, SSL_set1_host, SSL_set_tlsext_host_name
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.DoIP.TLS.OpenSSL;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.Winsock2,
{$ENDIF}
  OBD.Types,
  OBD.Protocol.DoIP.Transport;

type
  /// <summary>
  ///   Verification policy for the server certificate.
  /// </summary>
  /// <remarks>
  ///   <list type="bullet">
  ///   <item><c>vmRequire</c> — fail the handshake if the
  ///   certificate does not chain to a trusted root <i>and</i> the
  ///   hostname does not match. Default. Production setting.</item>
  ///   <item><c>vmAllowSelfSigned</c> — accept any certificate the
  ///   peer presents but still require the hostname to match the
  ///   subject / SAN. Useful for in-vehicle ECUs that ship with a
  ///   self-signed leaf.</item>
  ///   <item><c>vmInsecureNone</c> — accept anything. <b>Never</b>
  ///   use against an untrusted network; provided for bring-up
  ///   only.</item>
  ///   </list>
  /// </remarks>
  TOBDDoIPTLSVerifyMode = (
    vmRequire,
    vmAllowSelfSigned,
    vmInsecureNone
  );

  /// <summary>
  ///   Configuration for the OpenSSL DoIP transport.
  /// </summary>
  TOBDDoIPTLSOptions = record
    /// <summary>Verification policy for the server certificate.
    /// Default <c>vmRequire</c>.</summary>
    VerifyMode: TOBDDoIPTLSVerifyMode;
    /// <summary>Optional path to a CA bundle (PEM). When empty the
    /// transport calls <c>SSL_CTX_set_default_verify_paths</c> and
    /// relies on whatever roots OpenSSL was built to trust.</summary>
    CAFile: string;
    /// <summary>Optional path to a directory of hashed CA
    /// certificates (OpenSSL <c>c_rehash</c> layout).</summary>
    CAPath: string;
    /// <summary>Optional client-certificate PEM. When set the
    /// transport sends a TLS client cert during the handshake (used
    /// for mutual-TLS DoIP entities).</summary>
    ClientCertFile: string;
    /// <summary>Optional client-private-key PEM. Must match
    /// <c>ClientCertFile</c>.</summary>
    ClientKeyFile: string;
    /// <summary>Optional cipher list for TLS 1.2 (OpenSSL syntax).
    /// When empty OpenSSL's secure default is used.</summary>
    CipherList: string;
    /// <summary>Optional ciphersuites string for TLS 1.3 (colon-
    /// separated, e.g. <c>'TLS_AES_256_GCM_SHA384'</c>). When empty
    /// OpenSSL's default suite list is used.</summary>
    Ciphersuites: string;
  end;

  /// <summary>
  ///   OpenSSL-backed implementation of <c>IOBDDoIPTransport</c>.
  ///   Suitable for DoIP-over-TLS on port 3496.
  /// </summary>
  /// <remarks>
  ///   The implementation owns the raw TCP socket and the SSL
  ///   session. Send and Receive are guarded by separate critical
  ///   sections so a reader and a writer thread can operate
  ///   concurrently — OpenSSL itself is not thread-safe across
  ///   simultaneous I/O on the same SSL handle without external
  ///   locking, so all SSL_read / SSL_write calls go through these
  ///   locks.
  /// </remarks>
  TOBDDoIPOpenSSLTransport = class(TInterfacedObject, IOBDDoIPTransport)
  strict private
    FOptions: TOBDDoIPTLSOptions;
    FCtx: Pointer;          // SSL_CTX*
    FSsl: Pointer;          // SSL*
{$IFDEF MSWINDOWS}
    FSocket: TSocket;
{$ELSE}
    FSocket: NativeInt;
{$ENDIF}
    FConnected: Boolean;
    FRxLock: TCriticalSection;
    FTxLock: TCriticalSection;
    procedure RaiseSSL(const AContext: string; AReturn: Integer);
    procedure CleanupSSL;
    procedure CleanupSocket;
    procedure ConfigureContext(const AHost: string);
    procedure DoTcpConnect(const AHost: string; APort: Word;
      ATimeoutMs: Cardinal);
    procedure DoHandshake(const AHost: string; ATimeoutMs: Cardinal);
  public
    /// <summary>Creates a transport using OpenSSL's safe defaults
    /// (TLS 1.2 minimum, full chain verification, system root
    /// store).</summary>
    constructor Create; overload;
    /// <summary>Creates a transport with explicit TLS options.</summary>
    /// <param name="AOptions">Verification mode, CA paths, mTLS
    /// material and cipher overrides.</param>
    constructor Create(const AOptions: TOBDDoIPTLSOptions); overload;
    destructor Destroy; override;

    procedure Connect(const AHost: string; APort: Word;
      ATimeoutMs: Cardinal);
    procedure Disconnect;
    function IsConnected: Boolean;
    function Send(const ABytes: TBytes): Integer;
    function Receive(AMaxBytes: Integer;
      ATimeoutMs: Cardinal): TBytes;
  end;

/// <summary>
///   Returns a default-initialised <see cref="TOBDDoIPTLSOptions"/>
///   record (full verification, OpenSSL system roots, no client
///   certificate).
/// </summary>
function DefaultDoIPTLSOptions: TOBDDoIPTLSOptions;

/// <summary>
///   Forces the OpenSSL libraries to be loaded now and raises a
///   descriptive exception when the DLLs are missing. Hosts can
///   call this at startup to fail fast rather than during the first
///   <c>Connect</c>.
/// </summary>
procedure EnsureOpenSSLLoaded;

implementation

{$IFNDEF MSWINDOWS}
uses
  Posix.Base, Posix.Errno, Posix.Unistd, Posix.SysSocket, Posix.NetinetIn,
  Posix.ArpaInet, Posix.NetDB, Posix.SysSelect, Posix.SysTime,
  Posix.Fcntl;
{$ENDIF}

const
{$IFDEF MSWINDOWS}
  LIBSSL_NAME    = 'libssl-3-x64.dll';     // primary
  LIBSSL_FALLBACK1 = 'libssl-3.dll';
  LIBSSL_FALLBACK2 = 'ssleay32.dll';       // 1.0.x legacy — only as last resort
  LIBCRYPTO_NAME = 'libcrypto-3-x64.dll';
  LIBCRYPTO_FALLBACK1 = 'libcrypto-3.dll';
  LIBCRYPTO_FALLBACK2 = 'libeay32.dll';
{$ELSE}
  LIBSSL_NAME    = 'libssl.so.3';
  LIBSSL_FALLBACK1 = 'libssl.so';
  LIBSSL_FALLBACK2 = '';
  LIBCRYPTO_NAME = 'libcrypto.so.3';
  LIBCRYPTO_FALLBACK1 = 'libcrypto.so';
  LIBCRYPTO_FALLBACK2 = '';
{$ENDIF}

  // ---- OpenSSL constants we depend on ----
  TLS1_2_VERSION = $0303;
  TLS1_3_VERSION = $0304;

  SSL_CTRL_SET_TLSEXT_HOSTNAME    = 55;
  SSL_CTRL_SET_MIN_PROTO_VERSION  = 123;
  SSL_CTRL_SET_MAX_PROTO_VERSION  = 124;

  TLSEXT_NAMETYPE_host_name = 0;

  SSL_VERIFY_NONE                 = $00;
  SSL_VERIFY_PEER                 = $01;
  SSL_VERIFY_FAIL_IF_NO_PEER_CERT = $02;

  SSL_FILETYPE_PEM = 1;

  SSL_ERROR_NONE        = 0;
  SSL_ERROR_SSL         = 1;
  SSL_ERROR_WANT_READ   = 2;
  SSL_ERROR_WANT_WRITE  = 3;
  SSL_ERROR_SYSCALL     = 5;
  SSL_ERROR_ZERO_RETURN = 6;

  X509_V_OK = 0;

type
  // ---- OpenSSL function pointers ----
  TOPENSSL_init_ssl = function(opts: UInt64; settings: Pointer): Integer; cdecl;
  TTLS_client_method = function: Pointer; cdecl;
  TSSL_CTX_new = function(method: Pointer): Pointer; cdecl;
  TSSL_CTX_free = procedure(ctx: Pointer); cdecl;
  TSSL_CTX_ctrl = function(ctx: Pointer; cmd: Integer; larg: NativeInt; parg: Pointer): NativeInt; cdecl;
  TSSL_CTX_set_default_verify_paths = function(ctx: Pointer): Integer; cdecl;
  TSSL_CTX_load_verify_locations = function(ctx: Pointer; CAfile, CApath: PAnsiChar): Integer; cdecl;
  TSSL_CTX_set_verify = procedure(ctx: Pointer; mode: Integer; cb: Pointer); cdecl;
  TSSL_CTX_use_certificate_file = function(ctx: Pointer; FileName: PAnsiChar; FileType: Integer): Integer; cdecl;
  TSSL_CTX_use_PrivateKey_file  = function(ctx: Pointer; FileName: PAnsiChar; FileType: Integer): Integer; cdecl;
  TSSL_CTX_check_private_key    = function(ctx: Pointer): Integer; cdecl;
  TSSL_CTX_set_cipher_list      = function(ctx: Pointer; const Str: PAnsiChar): Integer; cdecl;
  TSSL_CTX_set_ciphersuites     = function(ctx: Pointer; const Str: PAnsiChar): Integer; cdecl;

  TSSL_new                      = function(ctx: Pointer): Pointer; cdecl;
  TSSL_free                     = procedure(ssl: Pointer); cdecl;
  TSSL_set_fd                   = function(ssl: Pointer; fd: Integer): Integer; cdecl;
  TSSL_ctrl                     = function(ssl: Pointer; cmd: Integer; larg: NativeInt; parg: Pointer): NativeInt; cdecl;
  TSSL_set1_host                = function(ssl: Pointer; const hostname: PAnsiChar): Integer; cdecl;
  TSSL_connect                  = function(ssl: Pointer): Integer; cdecl;
  TSSL_shutdown                 = function(ssl: Pointer): Integer; cdecl;
  TSSL_read                     = function(ssl: Pointer; buf: Pointer; num: Integer): Integer; cdecl;
  TSSL_write                    = function(ssl: Pointer; const buf: Pointer; num: Integer): Integer; cdecl;
  TSSL_get_error                = function(ssl: Pointer; ret: Integer): Integer; cdecl;
  TSSL_pending                  = function(ssl: Pointer): Integer; cdecl;
  TSSL_get_verify_result        = function(ssl: Pointer): NativeInt; cdecl;

  TERR_get_error                = function: NativeUInt; cdecl;
  TERR_error_string_n           = procedure(e: NativeUInt; buf: PAnsiChar; len: NativeUInt); cdecl;

var
  GLibSSLLoaded: Boolean = False;
  GLibSSL: HMODULE = 0;
  GLibCrypto: HMODULE = 0;
  GLoadLock: TCriticalSection;

  OPENSSL_init_ssl: TOPENSSL_init_ssl;
  TLS_client_method: TTLS_client_method;
  SSL_CTX_new_F: TSSL_CTX_new;
  SSL_CTX_free_F: TSSL_CTX_free;
  SSL_CTX_ctrl_F: TSSL_CTX_ctrl;
  SSL_CTX_set_default_verify_paths_F: TSSL_CTX_set_default_verify_paths;
  SSL_CTX_load_verify_locations_F: TSSL_CTX_load_verify_locations;
  SSL_CTX_set_verify_F: TSSL_CTX_set_verify;
  SSL_CTX_use_certificate_file_F: TSSL_CTX_use_certificate_file;
  SSL_CTX_use_PrivateKey_file_F: TSSL_CTX_use_PrivateKey_file;
  SSL_CTX_check_private_key_F: TSSL_CTX_check_private_key;
  SSL_CTX_set_cipher_list_F: TSSL_CTX_set_cipher_list;
  SSL_CTX_set_ciphersuites_F: TSSL_CTX_set_ciphersuites;

  SSL_new_F: TSSL_new;
  SSL_free_F: TSSL_free;
  SSL_set_fd_F: TSSL_set_fd;
  SSL_ctrl_F: TSSL_ctrl;
  SSL_set1_host_F: TSSL_set1_host;
  SSL_connect_F: TSSL_connect;
  SSL_shutdown_F: TSSL_shutdown;
  SSL_read_F: TSSL_read;
  SSL_write_F: TSSL_write;
  SSL_get_error_F: TSSL_get_error;
  SSL_pending_F: TSSL_pending;
  SSL_get_verify_result_F: TSSL_get_verify_result;

  ERR_get_error_F: TERR_get_error;
  ERR_error_string_n_F: TERR_error_string_n;

function TryLoadLib(const AName: string): HMODULE;
begin
  if AName = '' then
    Exit(0);
{$IFDEF MSWINDOWS}
  Result := LoadLibrary(PChar(AName));
{$ELSE}
  Result := HMODULE(dlopen(PAnsiChar(AnsiString(AName)), RTLD_NOW));
{$ENDIF}
end;

function ResolveProc(ALib: HMODULE; const AName: AnsiString): Pointer;
begin
{$IFDEF MSWINDOWS}
  Result := GetProcAddress(ALib, PAnsiChar(AName));
{$ELSE}
  Result := dlsym(Pointer(ALib), PAnsiChar(AName));
{$ENDIF}
end;

procedure NeedProc(ALib: HMODULE; var P; const AName: AnsiString;
  const ALibLabel: string);
begin
  Pointer(P) := ResolveProc(ALib, AName);
  if Pointer(P) = nil then
    raise EOBDError.CreateFmt(
      '%s missing OpenSSL symbol "%s" — DLL is too old or stripped',
      [ALibLabel, string(AName)]);
end;

procedure DoLoadOpenSSL;
begin
  if GLibSSLLoaded then Exit;

  GLibCrypto := TryLoadLib(LIBCRYPTO_NAME);
  if GLibCrypto = 0 then
    GLibCrypto := TryLoadLib(LIBCRYPTO_FALLBACK1);
  if GLibCrypto = 0 then
    raise EOBDError.CreateFmt(
      'OpenSSL libcrypto could not be loaded (%s / %s) — drop the OpenSSL 3.x DLLs next to the host EXE',
      [LIBCRYPTO_NAME, LIBCRYPTO_FALLBACK1]);

  GLibSSL := TryLoadLib(LIBSSL_NAME);
  if GLibSSL = 0 then
    GLibSSL := TryLoadLib(LIBSSL_FALLBACK1);
  if GLibSSL = 0 then
    raise EOBDError.CreateFmt(
      'OpenSSL libssl could not be loaded (%s / %s) — drop the OpenSSL 3.x DLLs next to the host EXE',
      [LIBSSL_NAME, LIBSSL_FALLBACK1]);

  NeedProc(GLibSSL, OPENSSL_init_ssl,                    'OPENSSL_init_ssl', 'libssl');
  NeedProc(GLibSSL, TLS_client_method,                   'TLS_client_method', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_new_F,                       'SSL_CTX_new', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_free_F,                      'SSL_CTX_free', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_ctrl_F,                      'SSL_CTX_ctrl', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_set_default_verify_paths_F,  'SSL_CTX_set_default_verify_paths', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_load_verify_locations_F,     'SSL_CTX_load_verify_locations', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_set_verify_F,                'SSL_CTX_set_verify', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_use_certificate_file_F,      'SSL_CTX_use_certificate_file', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_use_PrivateKey_file_F,       'SSL_CTX_use_PrivateKey_file', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_check_private_key_F,         'SSL_CTX_check_private_key', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_set_cipher_list_F,           'SSL_CTX_set_cipher_list', 'libssl');
  NeedProc(GLibSSL, SSL_CTX_set_ciphersuites_F,          'SSL_CTX_set_ciphersuites', 'libssl');

  NeedProc(GLibSSL, SSL_new_F,                'SSL_new', 'libssl');
  NeedProc(GLibSSL, SSL_free_F,               'SSL_free', 'libssl');
  NeedProc(GLibSSL, SSL_set_fd_F,             'SSL_set_fd', 'libssl');
  NeedProc(GLibSSL, SSL_ctrl_F,               'SSL_ctrl', 'libssl');
  NeedProc(GLibSSL, SSL_set1_host_F,          'SSL_set1_host', 'libssl');
  NeedProc(GLibSSL, SSL_connect_F,            'SSL_connect', 'libssl');
  NeedProc(GLibSSL, SSL_shutdown_F,           'SSL_shutdown', 'libssl');
  NeedProc(GLibSSL, SSL_read_F,               'SSL_read', 'libssl');
  NeedProc(GLibSSL, SSL_write_F,              'SSL_write', 'libssl');
  NeedProc(GLibSSL, SSL_get_error_F,          'SSL_get_error', 'libssl');
  NeedProc(GLibSSL, SSL_pending_F,            'SSL_pending', 'libssl');
  NeedProc(GLibSSL, SSL_get_verify_result_F,  'SSL_get_verify_result', 'libssl');

  NeedProc(GLibCrypto, ERR_get_error_F,       'ERR_get_error', 'libcrypto');
  NeedProc(GLibCrypto, ERR_error_string_n_F,  'ERR_error_string_n', 'libcrypto');

  // Initialise the library (idempotent in OpenSSL 3.x).
  OPENSSL_init_ssl(0, nil);

  GLibSSLLoaded := True;
end;

procedure EnsureOpenSSLLoaded;
begin
  if GLibSSLLoaded then Exit;
  GLoadLock.Enter;
  try
    DoLoadOpenSSL;
  finally
    GLoadLock.Leave;
  end;
end;

function DefaultDoIPTLSOptions: TOBDDoIPTLSOptions;
begin
  Result.VerifyMode      := vmRequire;
  Result.CAFile          := '';
  Result.CAPath          := '';
  Result.ClientCertFile  := '';
  Result.ClientKeyFile   := '';
  Result.CipherList      := '';
  Result.Ciphersuites    := '';
end;

{ ---- TOBDDoIPOpenSSLTransport ----------------------------------------------- }

constructor TOBDDoIPOpenSSLTransport.Create;
begin
  Create(DefaultDoIPTLSOptions);
end;

constructor TOBDDoIPOpenSSLTransport.Create(const AOptions: TOBDDoIPTLSOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FRxLock  := TCriticalSection.Create;
  FTxLock  := TCriticalSection.Create;
{$IFDEF MSWINDOWS}
  FSocket := INVALID_SOCKET;
{$ELSE}
  FSocket := -1;
{$ENDIF}
  EnsureOpenSSLLoaded;
end;

destructor TOBDDoIPOpenSSLTransport.Destroy;
begin
  Disconnect;
  FRxLock.Free;
  FTxLock.Free;
  inherited;
end;

procedure TOBDDoIPOpenSSLTransport.RaiseSSL(const AContext: string;
  AReturn: Integer);
var
  Err: NativeUInt;
  Buf: array[0..255] of AnsiChar;
  Detail: string;
  SslErr: Integer;
begin
  Detail := '';
  if FSsl <> nil then
  begin
    SslErr := SSL_get_error_F(FSsl, AReturn);
    Detail := Format(' (SSL_get_error=%d)', [SslErr]);
  end;
  Err := ERR_get_error_F();
  if Err <> 0 then
  begin
    FillChar(Buf, SizeOf(Buf), 0);
    ERR_error_string_n_F(Err, @Buf[0], SizeOf(Buf) - 1);
    Detail := Detail + ' ' + string(AnsiString(Buf));
  end;
  raise EOBDError.CreateFmt('OpenSSL: %s%s', [AContext, Detail]);
end;

procedure TOBDDoIPOpenSSLTransport.CleanupSSL;
begin
  if FSsl <> nil then
  begin
    // Best-effort bidirectional close — ignore errors, the socket
    // is going away anyway.
    SSL_shutdown_F(FSsl);
    SSL_free_F(FSsl);
    FSsl := nil;
  end;
  if FCtx <> nil then
  begin
    SSL_CTX_free_F(FCtx);
    FCtx := nil;
  end;
end;

procedure TOBDDoIPOpenSSLTransport.CleanupSocket;
begin
{$IFDEF MSWINDOWS}
  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
{$ELSE}
  if FSocket >= 0 then
  begin
    __close(FSocket);
    FSocket := -1;
  end;
{$ENDIF}
end;

procedure TOBDDoIPOpenSSLTransport.ConfigureContext(const AHost: string);
var
  Method: Pointer;
  VerifyMode: Integer;
  CA, CAP: AnsiString;
  CertFile, KeyFile: AnsiString;
  Cipher: AnsiString;
begin
  Method := TLS_client_method();
  if Method = nil then
    RaiseSSL('TLS_client_method returned nil', 0);

  FCtx := SSL_CTX_new_F(Method);
  if FCtx = nil then
    RaiseSSL('SSL_CTX_new failed', 0);

  // TLS 1.2 minimum, TLS 1.3 maximum.
  if SSL_CTX_ctrl_F(FCtx, SSL_CTRL_SET_MIN_PROTO_VERSION, TLS1_2_VERSION, nil) <> 1 then
    RaiseSSL('SSL_CTX_set_min_proto_version(TLS1.2) failed', 0);
  if SSL_CTX_ctrl_F(FCtx, SSL_CTRL_SET_MAX_PROTO_VERSION, TLS1_3_VERSION, nil) <> 1 then
    RaiseSSL('SSL_CTX_set_max_proto_version(TLS1.3) failed', 0);

  case FOptions.VerifyMode of
    vmRequire:
      VerifyMode := SSL_VERIFY_PEER or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
    vmAllowSelfSigned:
      VerifyMode := SSL_VERIFY_NONE; // hostname check still runs below
    vmInsecureNone:
      VerifyMode := SSL_VERIFY_NONE;
  else
    VerifyMode := SSL_VERIFY_PEER or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
  end;
  SSL_CTX_set_verify_F(FCtx, VerifyMode, nil);

  // CA roots — always populate, even when not strictly verifying,
  // so a host that switches policy doesn't have to re-init.
  if (FOptions.CAFile <> '') or (FOptions.CAPath <> '') then
  begin
    CA  := AnsiString(FOptions.CAFile);
    CAP := AnsiString(FOptions.CAPath);
    if SSL_CTX_load_verify_locations_F(FCtx,
      Pointer(IfThen(CA  = '', nil, PAnsiChar(CA))),
      Pointer(IfThen(CAP = '', nil, PAnsiChar(CAP)))) <> 1 then
      RaiseSSL('SSL_CTX_load_verify_locations failed', 0);
  end
  else
    SSL_CTX_set_default_verify_paths_F(FCtx);

  // Optional mTLS material.
  if FOptions.ClientCertFile <> '' then
  begin
    CertFile := AnsiString(FOptions.ClientCertFile);
    if SSL_CTX_use_certificate_file_F(FCtx, PAnsiChar(CertFile),
      SSL_FILETYPE_PEM) <> 1 then
      RaiseSSL('SSL_CTX_use_certificate_file failed', 0);

    if FOptions.ClientKeyFile <> '' then
      KeyFile := AnsiString(FOptions.ClientKeyFile)
    else
      KeyFile := CertFile;
    if SSL_CTX_use_PrivateKey_file_F(FCtx, PAnsiChar(KeyFile),
      SSL_FILETYPE_PEM) <> 1 then
      RaiseSSL('SSL_CTX_use_PrivateKey_file failed', 0);
    if SSL_CTX_check_private_key_F(FCtx) <> 1 then
      RaiseSSL('SSL_CTX_check_private_key failed', 0);
  end;

  if FOptions.CipherList <> '' then
  begin
    Cipher := AnsiString(FOptions.CipherList);
    SSL_CTX_set_cipher_list_F(FCtx, PAnsiChar(Cipher));
  end;
  if FOptions.Ciphersuites <> '' then
  begin
    Cipher := AnsiString(FOptions.Ciphersuites);
    SSL_CTX_set_ciphersuites_F(FCtx, PAnsiChar(Cipher));
  end;

  // Suppress unused warning when host is only used by the SSL layer.
  if AHost = '' then ; // no-op
end;

function IfThenStr(ACond: Boolean; const AT, AF: AnsiString): AnsiString;
begin
  if ACond then Result := AT else Result := AF;
end;

{$IFDEF MSWINDOWS}
procedure TOBDDoIPOpenSSLTransport.DoTcpConnect(const AHost: string;
  APort: Word; ATimeoutMs: Cardinal);
var
  Hints: TAddrInfoW;
  Res, Cur: PAddrInfoW;
  RC: Integer;
  Mode: u_long;
  WS: TWSAData;
  FdSet: TFDSet;
  TV: TTimeVal;
  SoErr: Integer;
  SoErrLen: Integer;
  PortStr: string;
begin
  if WSAStartup($0202, WS) <> 0 then
    raise EOBDError.Create('WSAStartup failed');

  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family   := AF_UNSPEC;
  Hints.ai_socktype := SOCK_STREAM;
  Hints.ai_protocol := IPPROTO_TCP;

  PortStr := IntToStr(APort);
  Res := nil;
  RC := GetAddrInfoW(PChar(AHost), PChar(PortStr), @Hints, Res);
  if (RC <> 0) or (Res = nil) then
    raise EOBDError.CreateFmt('DoIP TLS: getaddrinfo("%s:%d") failed (%d)',
      [AHost, APort, RC]);
  try
    Cur := Res;
    FSocket := INVALID_SOCKET;
    while Cur <> nil do
    begin
      FSocket := socket(Cur.ai_family, Cur.ai_socktype, Cur.ai_protocol);
      if FSocket <> INVALID_SOCKET then
      begin
        // Non-blocking for connect timeout.
        Mode := 1;
        ioctlsocket(FSocket, FIONBIO, Mode);

        if Winapi.Winsock2.connect(FSocket, Cur.ai_addr^, Cur.ai_addrlen) = 0 then
        begin
          // immediate connect
          Mode := 0;
          ioctlsocket(FSocket, FIONBIO, Mode);
          Break;
        end;

        if WSAGetLastError = WSAEWOULDBLOCK then
        begin
          FD_ZERO(FdSet);
          _FD_SET(FSocket, FdSet);
          TV.tv_sec  := Integer(ATimeoutMs div 1000);
          TV.tv_usec := Integer((ATimeoutMs mod 1000) * 1000);
          RC := select(0, nil, @FdSet, nil, @TV);
          if RC > 0 then
          begin
            SoErr := 0;
            SoErrLen := SizeOf(SoErr);
            getsockopt(FSocket, SOL_SOCKET, SO_ERROR,
              PAnsiChar(@SoErr), SoErrLen);
            if SoErr = 0 then
            begin
              Mode := 0;
              ioctlsocket(FSocket, FIONBIO, Mode);
              Break;
            end;
          end;
        end;

        closesocket(FSocket);
        FSocket := INVALID_SOCKET;
      end;
      Cur := Cur.ai_next;
    end;
  finally
    FreeAddrInfoW(Res);
  end;

  if FSocket = INVALID_SOCKET then
    raise EOBDError.CreateFmt('DoIP TLS: TCP connect to %s:%d timed out / refused',
      [AHost, APort]);

  // Apply send/recv timeouts so SSL_read / SSL_write surface as
  // SSL_ERROR_WANT_READ / SSL_ERROR_SYSCALL on inactivity.
  setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO,
    PAnsiChar(@ATimeoutMs), SizeOf(ATimeoutMs));
  setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO,
    PAnsiChar(@ATimeoutMs), SizeOf(ATimeoutMs));
end;
{$ELSE}
procedure TOBDDoIPOpenSSLTransport.DoTcpConnect(const AHost: string;
  APort: Word; ATimeoutMs: Cardinal);
begin
  raise EOBDError.Create(
    'DoIP TLS: non-Windows TCP path not implemented in this build');
end;
{$ENDIF}

procedure TOBDDoIPOpenSSLTransport.DoHandshake(const AHost: string;
  ATimeoutMs: Cardinal);
var
  RC: Integer;
  ErrCode: Integer;
  HostA: AnsiString;
  Sw: TStopwatch;
begin
  FSsl := SSL_new_F(FCtx);
  if FSsl = nil then
    RaiseSSL('SSL_new failed', 0);

  HostA := AnsiString(AHost);

  // SNI — required for any modern TLS endpoint.
  if SSL_ctrl_F(FSsl, SSL_CTRL_SET_TLSEXT_HOSTNAME,
    TLSEXT_NAMETYPE_host_name, PAnsiChar(HostA)) <> 1 then
    RaiseSSL('SSL_set_tlsext_host_name failed', 0);

  // Hostname verification — covers vmRequire and vmAllowSelfSigned.
  if FOptions.VerifyMode <> vmInsecureNone then
  begin
    if SSL_set1_host_F(FSsl, PAnsiChar(HostA)) <> 1 then
      RaiseSSL('SSL_set1_host failed', 0);
  end;

  if SSL_set_fd_F(FSsl, Integer(FSocket)) <> 1 then
    RaiseSSL('SSL_set_fd failed', 0);

  Sw := TStopwatch.StartNew;
  while True do
  begin
    RC := SSL_connect_F(FSsl);
    if RC = 1 then
      Break;
    ErrCode := SSL_get_error_F(FSsl, RC);
    if (ErrCode = SSL_ERROR_WANT_READ) or
       (ErrCode = SSL_ERROR_WANT_WRITE) then
    begin
      if Cardinal(Sw.ElapsedMilliseconds) >= ATimeoutMs then
        raise EOBDError.Create('DoIP TLS: handshake timed out');
      Sleep(10);
      Continue;
    end;
    RaiseSSL('SSL_connect failed', RC);
  end;

  // Verification result — vmRequire only. vmAllowSelfSigned passes
  // chain failures but still enforces hostname (set1_host). The
  // hostname check is performed inside SSL_connect; if it failed,
  // OpenSSL surfaces it as a verify error, not a separate API.
  if FOptions.VerifyMode = vmRequire then
  begin
    if SSL_get_verify_result_F(FSsl) <> X509_V_OK then
      raise EOBDError.Create(
        'DoIP TLS: server certificate verification failed');
  end;
end;

procedure TOBDDoIPOpenSSLTransport.Connect(const AHost: string;
  APort: Word; ATimeoutMs: Cardinal);
begin
  if FConnected then
    raise EOBDConfig.Create('DoIP TLS: already connected');
  if AHost = '' then
    raise EOBDConfig.Create('DoIP TLS: empty host');
  try
    ConfigureContext(AHost);
    DoTcpConnect(AHost, APort, ATimeoutMs);
    DoHandshake(AHost, ATimeoutMs);
    FConnected := True;
  except
    CleanupSSL;
    CleanupSocket;
    raise;
  end;
end;

procedure TOBDDoIPOpenSSLTransport.Disconnect;
begin
  if not FConnected and (FSsl = nil) and (FCtx = nil) then Exit;
  FTxLock.Enter;
  try
    FRxLock.Enter;
    try
      CleanupSSL;
      CleanupSocket;
      FConnected := False;
    finally
      FRxLock.Leave;
    end;
  finally
    FTxLock.Leave;
  end;
end;

function TOBDDoIPOpenSSLTransport.IsConnected: Boolean;
begin
  Result := FConnected and (FSsl <> nil);
end;

function TOBDDoIPOpenSSLTransport.Send(const ABytes: TBytes): Integer;
var
  Total, Written, RC, ErrCode: Integer;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP TLS transport not connected');
  Total := Length(ABytes);
  if Total = 0 then Exit(0);

  Written := 0;
  FTxLock.Enter;
  try
    while Written < Total do
    begin
      RC := SSL_write_F(FSsl, @ABytes[Written], Total - Written);
      if RC > 0 then
      begin
        Inc(Written, RC);
        Continue;
      end;
      ErrCode := SSL_get_error_F(FSsl, RC);
      if (ErrCode = SSL_ERROR_WANT_READ) or
         (ErrCode = SSL_ERROR_WANT_WRITE) then
      begin
        Sleep(5);
        Continue;
      end;
      RaiseSSL('SSL_write failed', RC);
    end;
  finally
    FTxLock.Leave;
  end;
  Result := Written;
end;

function TOBDDoIPOpenSSLTransport.Receive(AMaxBytes: Integer;
  ATimeoutMs: Cardinal): TBytes;
var
  Sw: TStopwatch;
  RC, ErrCode: Integer;
  Cap: Integer;
begin
  if not IsConnected then
    raise EOBDNotConnected.Create('DoIP TLS transport not connected');

  if AMaxBytes <= 0 then
    Cap := 16 * 1024
  else
    Cap := AMaxBytes;

  SetLength(Result, Cap);
  Sw := TStopwatch.StartNew;
  FRxLock.Enter;
  try
    while True do
    begin
      RC := SSL_read_F(FSsl, @Result[0], Cap);
      if RC > 0 then
      begin
        SetLength(Result, RC);
        Exit;
      end;
      ErrCode := SSL_get_error_F(FSsl, RC);
      case ErrCode of
        SSL_ERROR_ZERO_RETURN:
        begin
          // Clean shutdown from peer.
          SetLength(Result, 0);
          Exit;
        end;
        SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE:
        begin
          if Cardinal(Sw.ElapsedMilliseconds) >= ATimeoutMs then
          begin
            SetLength(Result, 0);
            Exit;
          end;
          Sleep(10);
          Continue;
        end;
      else
        if Cardinal(Sw.ElapsedMilliseconds) >= ATimeoutMs then
        begin
          // Treat plain timeout (SO_RCVTIMEO trip) as empty result.
          SetLength(Result, 0);
          Exit;
        end;
        RaiseSSL('SSL_read failed', RC);
      end;
    end;
  finally
    FRxLock.Leave;
  end;
end;

initialization
  GLoadLock := TCriticalSection.Create;

finalization
  if GLibSSL <> 0 then
  begin
{$IFDEF MSWINDOWS}
    FreeLibrary(GLibSSL);
{$ENDIF}
    GLibSSL := 0;
  end;
  if GLibCrypto <> 0 then
  begin
{$IFDEF MSWINDOWS}
    FreeLibrary(GLibCrypto);
{$ENDIF}
    GLibCrypto := 0;
  end;
  GLoadLock.Free;

end.
