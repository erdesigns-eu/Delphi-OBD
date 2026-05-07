//------------------------------------------------------------------------------
// UNIT           : OBD.SecureSettings.pas
// CONTENTS       : Encrypted at-rest settings via Windows DPAPI
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Wraps `CryptProtectData` / `CryptUnprotectData` so
//                  dealer codes, security-access keys, and similar
//                  sensitive strings stay encrypted on disk against the
//                  current Windows user. DPAPI ties the ciphertext to
//                  the user account — copying the file to another
//                  machine doesn't grant decryption.
//------------------------------------------------------------------------------
unit OBD.SecureSettings;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.IniFiles,
  WinApi.Windows;

type
  /// <summary>Raised when DPAPI returns an error.</summary>
  EOBDDpapiError = class(Exception);

  /// <summary>
  ///   Persistent INI-style storage where every value is DPAPI-encrypted.
  ///   On disk you see Base64-encoded ciphertext; in memory you handle
  ///   plaintext. Keys are in the clear (so the file remains
  ///   debuggable); values aren't.
  /// </summary>
  TOBDSecureSettings = class
  strict private
    FFilePath: string;
    FIni: TMemIniFile;
    function ProtectString(const Plain: string): string;
    function UnprotectString(const Cipher: string): string;
  public
    constructor Create(const AFilePath: string);
    destructor Destroy; override;

    /// <summary>Read an encrypted value. Returns Default if missing or undecryptable.</summary>
    function ReadString(const Section, Key, Default: string): string;
    /// <summary>Write a value, DPAPI-encrypting first.</summary>
    procedure WriteString(const Section, Key, Value: string);
    /// <summary>Remove a key (no-op if absent).</summary>
    procedure DeleteKey(const Section, Key: string);
    /// <summary>Persist to disk.</summary>
    procedure Save;
    /// <summary>List of section names (in the clear).</summary>
    procedure ReadSections(Out: TStrings);

    property FilePath: string read FFilePath;
  end;

/// <summary>Encrypt arbitrary bytes with DPAPI (current user scope).</summary>
function DPAPIEncrypt(const Plain: TBytes): TBytes;
/// <summary>Decrypt previously-encrypted bytes.</summary>
function DPAPIDecrypt(const Cipher: TBytes): TBytes;

implementation

uses
  System.NetEncoding;

const
  CRYPTPROTECT_UI_FORBIDDEN = $01;

type
  DATA_BLOB = record
    cbData: DWORD;
    pbData: PByte;
  end;
  PDATA_BLOB = ^DATA_BLOB;

function CryptProtectData(pDataIn: PDATA_BLOB; szDataDescr: PWideChar;
  pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer;
  pPromptStruct: Pointer; dwFlags: DWORD;
  pDataOut: PDATA_BLOB): BOOL; stdcall;
  external 'crypt32.dll' name 'CryptProtectData';

function CryptUnprotectData(pDataIn: PDATA_BLOB;
  ppszDataDescr: PPWideChar;
  pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer;
  pPromptStruct: Pointer; dwFlags: DWORD;
  pDataOut: PDATA_BLOB): BOOL; stdcall;
  external 'crypt32.dll' name 'CryptUnprotectData';

function DPAPIEncrypt(const Plain: TBytes): TBytes;
var
  In_, Out_: DATA_BLOB;
begin
  if Length(Plain) = 0 then Exit(nil);
  In_.cbData := Length(Plain);
  In_.pbData := @Plain[0];
  Out_.cbData := 0;
  Out_.pbData := nil;
  if not CryptProtectData(@In_, nil, nil, nil, nil,
       CRYPTPROTECT_UI_FORBIDDEN, @Out_) then
    raise EOBDDpapiError.CreateFmt('CryptProtectData failed: %d',
      [GetLastError]);
  try
    SetLength(Result, Out_.cbData);
    if Out_.cbData > 0 then Move(Out_.pbData^, Result[0], Out_.cbData);
  finally
    if Out_.pbData <> nil then LocalFree(HLOCAL(Out_.pbData));
  end;
end;

function DPAPIDecrypt(const Cipher: TBytes): TBytes;
var
  In_, Out_: DATA_BLOB;
begin
  if Length(Cipher) = 0 then Exit(nil);
  In_.cbData := Length(Cipher);
  In_.pbData := @Cipher[0];
  Out_.cbData := 0;
  Out_.pbData := nil;
  if not CryptUnprotectData(@In_, nil, nil, nil, nil,
       CRYPTPROTECT_UI_FORBIDDEN, @Out_) then
    raise EOBDDpapiError.CreateFmt('CryptUnprotectData failed: %d',
      [GetLastError]);
  try
    SetLength(Result, Out_.cbData);
    if Out_.cbData > 0 then Move(Out_.pbData^, Result[0], Out_.cbData);
  finally
    if Out_.pbData <> nil then LocalFree(HLOCAL(Out_.pbData));
  end;
end;

//==============================================================================
// TOBDSecureSettings
//==============================================================================
constructor TOBDSecureSettings.Create(const AFilePath: string);
begin
  inherited Create;
  FFilePath := AFilePath;
  ForceDirectories(TPath.GetDirectoryName(FFilePath));
  FIni := TMemIniFile.Create(FFilePath);
end;

destructor TOBDSecureSettings.Destroy;
begin
  FIni.Free;
  inherited;
end;

function TOBDSecureSettings.ProtectString(const Plain: string): string;
var
  Bytes, Encrypted: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(Plain);
  Encrypted := DPAPIEncrypt(Bytes);
  Result := TNetEncoding.Base64.EncodeBytesToString(Encrypted);
end;

function TOBDSecureSettings.UnprotectString(const Cipher: string): string;
var
  Encrypted, Decrypted: TBytes;
begin
  if Cipher = '' then Exit('');
  Encrypted := TNetEncoding.Base64.DecodeStringToBytes(Cipher);
  Decrypted := DPAPIDecrypt(Encrypted);
  Result := TEncoding.UTF8.GetString(Decrypted);
end;

function TOBDSecureSettings.ReadString(const Section, Key, Default: string): string;
var
  Cipher: string;
begin
  Cipher := FIni.ReadString(Section, Key, '');
  if Cipher = '' then Exit(Default);
  try
    Result := UnprotectString(Cipher);
  except
    // If the file was tampered with or moved between user accounts the
    // ciphertext won't decrypt — fall back to Default rather than
    // crashing the calling code.
    Result := Default;
  end;
end;

procedure TOBDSecureSettings.WriteString(const Section, Key, Value: string);
begin
  FIni.WriteString(Section, Key, ProtectString(Value));
end;

procedure TOBDSecureSettings.DeleteKey(const Section, Key: string);
begin
  FIni.DeleteKey(Section, Key);
end;

procedure TOBDSecureSettings.Save;
begin
  FIni.UpdateFile;
end;

procedure TOBDSecureSettings.ReadSections(Out: TStrings);
begin
  FIni.ReadSections(Out);
end;

end.
