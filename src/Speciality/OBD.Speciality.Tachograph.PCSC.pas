//------------------------------------------------------------------------------
//  OBD.Speciality.Tachograph.PCSC
//
//  TOBDTachoPCSC — dynamic PC/SC bridge for the Tachograph
//  workshop / driver card reader. Wraps the standard PC/SC API
//  exposed by Windows winscard.dll and Linux libpcsclite.so so the
//  workshop tool can:
//
//    1. List readers (ListReaders)
//    2. Connect to a reader and the inserted card (Connect)
//    3. Send APDUs to the card (Transmit)
//    4. Disconnect cleanly (Disconnect)
//
//  Card-specific APDUs (e.g. SELECT EF.ApplicationIdentification on
//  the Tachograph application) are built by the host using the
//  Tachograph command catalogue from Annex IC §3.6. This bridge
//  only owns the transport.
//
//  Same dynamic-load pattern as the OpenSSL DoIP plug — no compile-
//  time dependency on PC/SC.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - PC/SC Workgroup Reference Implementation
//    - MSDN winscard.h
//    - Commission Implementing Regulation (EU) 2016/799 Annex IC §3
//
//  History     :
//    2026-05-09  ERD  Phase 7 follow-up.
//------------------------------------------------------------------------------

unit OBD.Speciality.Tachograph.PCSC;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  OBD.Types;

const
  /// <summary>SCARD_SCOPE_USER — context for the calling user.</summary>
  SCARD_SCOPE_USER     = 0;
  SCARD_SCOPE_SYSTEM   = 2;
  /// <summary>SCARD_SHARE_SHARED — share with other apps.</summary>
  SCARD_SHARE_SHARED   = 2;
  SCARD_SHARE_EXCLUSIVE = 1;
  /// <summary>SCARD_PROTOCOL_T0 / T1 / Tx — protocol bitmap.</summary>
  SCARD_PROTOCOL_T0    = $00000001;
  SCARD_PROTOCOL_T1    = $00000002;
  SCARD_PROTOCOL_RAW   = $00010000;
  SCARD_PROTOCOL_ANY   = SCARD_PROTOCOL_T0 or SCARD_PROTOCOL_T1;
  /// <summary>Disconnect-disposition flags.</summary>
  SCARD_LEAVE_CARD     = 0;
  SCARD_RESET_CARD     = 1;
  SCARD_UNPOWER_CARD   = 2;
  SCARD_EJECT_CARD     = 3;

  /// <summary>SCARD_S_SUCCESS — return code on success.</summary>
  SCARD_S_SUCCESS      = 0;

type
  /// <summary>PC/SC bridge component.</summary>
  TOBDTachoPCSC = class(TComponent)
  strict private
    FContext: NativeUInt;
    FCard: NativeUInt;
    FActiveProtocol: Cardinal;
    procedure EnsureLoaded;
    procedure RaiseSCardError(const AContext: string; ACode: NativeInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Establishes a PC/SC user-scope context. Call once
    /// before <see cref="ListReaders"/> / <see cref="Connect"/>.</summary>
    procedure Establish;
    /// <summary>Releases the context. Idempotent.</summary>
    procedure Release;

    /// <summary>Returns the names of every reader the resource
    /// manager knows about. Empty array when none are
    /// connected.</summary>
    function ListReaders: TArray<string>;

    /// <summary>Connects to the inserted card on
    /// <c>AReaderName</c>.</summary>
    /// <param name="AReaderName">Reader-name string from
    /// <see cref="ListReaders"/>.</param>
    /// <param name="AShareMode">Sharing mode (default SHARED).</param>
    /// <param name="APreferredProtocols">Bitmap of acceptable
    /// protocols (default T0|T1).</param>
    procedure Connect(const AReaderName: string;
      AShareMode: Cardinal = SCARD_SHARE_SHARED;
      APreferredProtocols: Cardinal = SCARD_PROTOCOL_ANY);
    /// <summary>Disconnects from the card.</summary>
    procedure Disconnect(ADisposition: Cardinal = SCARD_LEAVE_CARD);
    /// <summary>True when a card session is open.</summary>
    function IsConnected: Boolean;

    /// <summary>Sends a CASE-4 APDU and returns the response
    /// (data + 2-byte SW1/SW2). Caller pre-pends CLA INS P1 P2 Lc
    /// data Le or appropriate variant.</summary>
    function Transmit(const AAPDU: TBytes): TBytes;

    /// <summary>Currently-negotiated protocol bitmap.</summary>
    property ActiveProtocol: Cardinal read FActiveProtocol;
  end;

implementation

const
{$IFDEF MSWINDOWS}
  PCSC_LIB    = 'winscard.dll';
  PCSC_PCI_T0 = 'g_rgSCardT0Pci';
  PCSC_PCI_T1 = 'g_rgSCardT1Pci';
{$ELSE}
  PCSC_LIB    = 'libpcsclite.so.1';
  PCSC_PCI_T0 = 'g_rgSCardT0Pci';
  PCSC_PCI_T1 = 'g_rgSCardT1Pci';
{$ENDIF}

type
  // PC/SC function pointer types. NativeUInt for SCARDCONTEXT and
  // SCARDHANDLE so the same code works on 32- and 64-bit hosts.
  TSCardEstablishContext = function(dwScope: Cardinal;
    pvReserved1, pvReserved2: Pointer;
    var phContext: NativeUInt): NativeInt; stdcall;
  TSCardReleaseContext = function(hContext: NativeUInt): NativeInt; stdcall;
  TSCardListReadersA = function(hContext: NativeUInt;
    mszGroups: PAnsiChar; mszReaders: PAnsiChar;
    var pcchReaders: Cardinal): NativeInt; stdcall;
  TSCardListReadersW = function(hContext: NativeUInt;
    mszGroups: PWideChar; mszReaders: PWideChar;
    var pcchReaders: Cardinal): NativeInt; stdcall;
  TSCardConnectA = function(hContext: NativeUInt;
    szReader: PAnsiChar; dwShareMode, dwPreferredProtocols: Cardinal;
    var phCard: NativeUInt; var pdwActiveProtocol: Cardinal): NativeInt; stdcall;
  TSCardConnectW = function(hContext: NativeUInt;
    szReader: PWideChar; dwShareMode, dwPreferredProtocols: Cardinal;
    var phCard: NativeUInt; var pdwActiveProtocol: Cardinal): NativeInt; stdcall;
  TSCardDisconnect = function(hCard: NativeUInt;
    dwDisposition: Cardinal): NativeInt; stdcall;
  TSCardTransmit = function(hCard: NativeUInt;
    pioSendPci: Pointer; pbSendBuffer: Pointer; cbSendLength: Cardinal;
    pioRecvPci: Pointer; pbRecvBuffer: Pointer;
    var pcbRecvLength: Cardinal): NativeInt; stdcall;

var
  GLoadLock: TCriticalSection;
  GLib: HMODULE = 0;
  SCardEstablishContext_F: TSCardEstablishContext;
  SCardReleaseContext_F: TSCardReleaseContext;
  SCardListReadersA_F: TSCardListReadersA;
  SCardListReadersW_F: TSCardListReadersW;
  SCardConnectA_F: TSCardConnectA;
  SCardConnectW_F: TSCardConnectW;
  SCardDisconnect_F: TSCardDisconnect;
  SCardTransmit_F: TSCardTransmit;
  G_PCI_T0: Pointer = nil;
  G_PCI_T1: Pointer = nil;

procedure DoLoad;
{$IFDEF MSWINDOWS}
  function NeedProc(const AName: AnsiString): Pointer;
  begin
    Result := GetProcAddress(GLib, PAnsiChar(AName));
    if Result = nil then
      raise EOBDError.CreateFmt(
        '%s missing PC/SC symbol "%s"', [PCSC_LIB, string(AName)]);
  end;
{$ENDIF}
begin
  if GLib <> 0 then Exit;
{$IFDEF MSWINDOWS}
  GLib := LoadLibrary(PChar(PCSC_LIB));
{$ELSE}
  GLib := 0; // Posix host: dlopen would go here.
{$ENDIF}
  if GLib = 0 then
    raise EOBDError.CreateFmt(
      'PC/SC library could not be loaded (%s)', [PCSC_LIB]);
{$IFDEF MSWINDOWS}
  Pointer(@SCardEstablishContext_F) := NeedProc('SCardEstablishContext');
  Pointer(@SCardReleaseContext_F)   := NeedProc('SCardReleaseContext');
  Pointer(@SCardListReadersA_F)     := NeedProc('SCardListReadersA');
  Pointer(@SCardListReadersW_F)     := NeedProc('SCardListReadersW');
  Pointer(@SCardConnectA_F)         := NeedProc('SCardConnectA');
  Pointer(@SCardConnectW_F)         := NeedProc('SCardConnectW');
  Pointer(@SCardDisconnect_F)       := NeedProc('SCardDisconnect');
  Pointer(@SCardTransmit_F)         := NeedProc('SCardTransmit');
  G_PCI_T0 := GetProcAddress(GLib, PCSC_PCI_T0);
  G_PCI_T1 := GetProcAddress(GLib, PCSC_PCI_T1);
{$ENDIF}
end;

procedure EnsurePCSCLoaded;
begin
  if GLib <> 0 then Exit;
  GLoadLock.Enter;
  try
    DoLoad;
  finally
    GLoadLock.Leave;
  end;
end;

constructor TOBDTachoPCSC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TOBDTachoPCSC.Destroy;
begin
  if FCard <> 0 then
    try Disconnect; except end;
  Release;
  inherited;
end;

procedure TOBDTachoPCSC.EnsureLoaded;
begin
  EnsurePCSCLoaded;
end;

procedure TOBDTachoPCSC.RaiseSCardError(const AContext: string;
  ACode: NativeInt);
begin
  raise EOBDError.CreateFmt('PC/SC %s failed (0x%.8X)',
    [AContext, ACode]);
end;

procedure TOBDTachoPCSC.Establish;
var
  RC: NativeInt;
begin
  EnsureLoaded;
  if FContext <> 0 then Exit;
  RC := SCardEstablishContext_F(SCARD_SCOPE_USER, nil, nil, FContext);
  if RC <> SCARD_S_SUCCESS then
    RaiseSCardError('SCardEstablishContext', RC);
end;

procedure TOBDTachoPCSC.Release;
begin
  if FContext = 0 then Exit;
  SCardReleaseContext_F(FContext);
  FContext := 0;
end;

function TOBDTachoPCSC.ListReaders: TArray<string>;
{$IFDEF MSWINDOWS}
var
  RC: NativeInt;
  Len: Cardinal;
  Buf: array of WideChar;
  S: string;
  Acc: TArray<string>;
  I: Integer;
  Start: Integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  EnsureLoaded;
  if FContext = 0 then Establish;
  Len := 0;
  RC := SCardListReadersW_F(FContext, nil, nil, Len);
  if (RC <> SCARD_S_SUCCESS) or (Len = 0) then Exit(nil);
  SetLength(Buf, Len);
  RC := SCardListReadersW_F(FContext, nil, @Buf[0], Len);
  if RC <> SCARD_S_SUCCESS then RaiseSCardError('SCardListReaders', RC);
  // Multi-string: name1 #0 name2 #0 ... #0 #0
  Start := 0;
  Acc := nil;
  for I := 0 to Length(Buf) - 1 do
  begin
    if Buf[I] = #0 then
    begin
      if I > Start then
      begin
        SetString(S, PChar(@Buf[Start]), I - Start);
        SetLength(Acc, Length(Acc) + 1);
        Acc[High(Acc)] := S;
      end;
      Start := I + 1;
    end;
  end;
  Result := Acc;
{$ELSE}
  Result := nil; // Posix host: requires dlopen-based loader.
{$ENDIF}
end;

procedure TOBDTachoPCSC.Connect(const AReaderName: string;
  AShareMode, APreferredProtocols: Cardinal);
{$IFDEF MSWINDOWS}
var
  RC: NativeInt;
  Name: WideString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  EnsureLoaded;
  if FContext = 0 then Establish;
  Name := AReaderName;
  RC := SCardConnectW_F(FContext, PWideChar(Name),
    AShareMode, APreferredProtocols, FCard, FActiveProtocol);
  if RC <> SCARD_S_SUCCESS then RaiseSCardError('SCardConnect', RC);
{$ELSE}
  raise EOBDError.Create('PC/SC: non-Windows path not implemented');
{$ENDIF}
end;

procedure TOBDTachoPCSC.Disconnect(ADisposition: Cardinal);
begin
  if FCard = 0 then Exit;
  SCardDisconnect_F(FCard, ADisposition);
  FCard := 0;
  FActiveProtocol := 0;
end;

function TOBDTachoPCSC.IsConnected: Boolean;
begin
  Result := FCard <> 0;
end;

function TOBDTachoPCSC.Transmit(const AAPDU: TBytes): TBytes;
var
  RC: NativeInt;
  Send: TBytes;
  Recv: array[0..263] of Byte; // 256-byte max APDU + 2 SW + 1 cushion
  RecvLen: Cardinal;
  PCI: Pointer;
begin
  if FCard = 0 then
    raise EOBDError.Create('PC/SC: card not connected');
  Send := Copy(AAPDU, 0, Length(AAPDU));
  if FActiveProtocol = SCARD_PROTOCOL_T0 then PCI := G_PCI_T0
  else if FActiveProtocol = SCARD_PROTOCOL_T1 then PCI := G_PCI_T1
  else PCI := G_PCI_T1;
  RecvLen := Length(Recv);
  RC := SCardTransmit_F(FCard, PCI, @Send[0], Cardinal(Length(Send)),
    nil, @Recv[0], RecvLen);
  if RC <> SCARD_S_SUCCESS then RaiseSCardError('SCardTransmit', RC);
  SetLength(Result, RecvLen);
  if RecvLen > 0 then
    Move(Recv[0], Result[0], RecvLen);
end;

initialization
  GLoadLock := TCriticalSection.Create;

finalization
{$IFDEF MSWINDOWS}
  if GLib <> 0 then FreeLibrary(GLib);
{$ENDIF}
  GLib := 0;
  GLoadLock.Free;

end.
