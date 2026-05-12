//------------------------------------------------------------------------------
//  OBD.SecureSettings
//
//  TOBDSecureSettings — Windows-DPAPI-backed name/value
//  configuration store. Each (Key, Value) is persisted into a
//  DPAPI-protected blob; the host picks the scope at construction
//  time:
//
//    - ssCurrentUser: encrypted under the current Windows user
//      profile. Other accounts on the same machine cannot read
//      the file even when they have NTFS access to it.
//    - ssLocalMachine: encrypted under the machine key. Every
//      account on this machine can read; portability is broken
//      (the file does not decrypt on a different machine).
//
//  Use ssCurrentUser for per-user tool preferences (API keys,
//  OEM tokens, security-access seeds bound to a workshop
//  technician). Use ssLocalMachine for shared workstation
//  setups where multiple operators must share one cached
//  configuration on the same physical PC.
//
//  Storage path defaults to
//  <c>%LOCALAPPDATA%\&lt;AppName&gt;\settings.dat</c> for the
//  current-user scope and to
//  <c>%PROGRAMDATA%\&lt;AppName&gt;\settings.dat</c> for the
//  machine scope; both can be overridden by the explicit-path
//  constructor.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - Microsoft Docs: CryptProtectData / CryptUnprotectData
//      (CRYPTPROTECT_LOCAL_MACHINE flag for the machine scope)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.SecureSettings;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  /// <summary>Raised on storage / DPAPI errors.</summary>
  EOBDSecureSettings = class(Exception);

  /// <summary>DPAPI protection scope.</summary>
  TOBDSecureSettingsScope = (
    /// <summary>Encrypt under the current Windows user (default).
    /// Other accounts on the same machine cannot decrypt.</summary>
    ssCurrentUser,
    /// <summary>Encrypt under the local machine key. Every
    /// account on this PC can decrypt; the file does not
    /// decrypt on a different machine.</summary>
    ssLocalMachine
  );

  /// <summary>
  ///   Encrypted name/value configuration store.
  /// </summary>
  /// <remarks>
  ///   Construct once per host application. Reads / writes are
  ///   reentrant. <see cref="Save"/> persists to disk; the
  ///   store auto-loads on first access.
  /// </remarks>
  TOBDSecureSettings = class
  strict private
    FFilePath: string;
    FScope: TOBDSecureSettingsScope;
    FLock: TCriticalSection;
    FCache: TDictionary<string, string>;
    FLoaded: Boolean;
    procedure EnsureLoaded;
    function ProtectBlob(const APlainText: TBytes): TBytes;
    function UnprotectBlob(const ACipherText: TBytes): TBytes;
  public
    /// <summary>
    ///   Constructs a store backed by the default per-scope path.
    /// </summary>
    /// <param name="AAppName">Application name folder.</param>
    /// <param name="AScope">DPAPI scope. Default
    /// <c>ssCurrentUser</c>.</param>
    constructor Create(const AAppName: string;
      AScope: TOBDSecureSettingsScope = ssCurrentUser); overload;
    /// <summary>Constructs a store at the explicit path.</summary>
    /// <param name="AFilePath">Storage file path.</param>
    /// <param name="AScope">DPAPI scope.</param>
    constructor CreateAt(const AFilePath: string;
      AScope: TOBDSecureSettingsScope = ssCurrentUser);
    /// <summary>Frees state without writing — call
    /// <see cref="Save"/> first to persist.</summary>
    destructor Destroy; override;

    /// <summary>Reads a value, returning <c>ADefault</c> when
    /// the key is unknown.</summary>
    /// <param name="AKey">Setting key.</param>
    /// <param name="ADefault">Fallback value.</param>
    /// <returns>The stored value or <c>ADefault</c>.</returns>
    function Read(const AKey: string;
      const ADefault: string = ''): string;
    /// <summary>Writes a value (in-memory; call
    /// <see cref="Save"/> to persist).</summary>
    /// <param name="AKey">Setting key.</param>
    /// <param name="AValue">Value to store.</param>
    procedure Write(const AKey: string; const AValue: string);
    /// <summary>Removes a key from the store.</summary>
    /// <param name="AKey">Setting key.</param>
    procedure Delete(const AKey: string);
    /// <summary>Returns <c>True</c> when the key is present.</summary>
    /// <param name="AKey">Setting key.</param>
    function HasKey(const AKey: string): Boolean;

    /// <summary>Number of stored keys.</summary>
    function Count: Integer;
    /// <summary>Snapshot of every key.</summary>
    /// <returns>Array of keys (no defined order).</returns>
    function Keys: TArray<string>;

    /// <summary>Persists the current in-memory state to disk
    /// under the configured DPAPI scope.</summary>
    /// <exception cref="EOBDSecureSettings">
    ///   DPAPI returned an error or the destination is
    ///   unwritable.
    /// </exception>
    procedure Save;

    /// <summary>Drops every key (in-memory only — call
    /// <see cref="Save"/> to commit).</summary>
    procedure Clear;

    /// <summary>Storage file path.</summary>
    property FilePath: string read FFilePath;
    /// <summary>DPAPI protection scope.</summary>
    property Scope: TOBDSecureSettingsScope read FScope;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;

const
  CRYPTPROTECT_LOCAL_MACHINE = $00000004;
  CRYPTPROTECT_UI_FORBIDDEN  = $00000001;

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
  external 'Crypt32.dll' name 'CryptProtectData';

function CryptUnprotectData(pDataIn: PDATA_BLOB;
  szDataDescr: PPWideChar; pOptionalEntropy: PDATA_BLOB;
  pvReserved: Pointer; pPromptStruct: Pointer; dwFlags: DWORD;
  pDataOut: PDATA_BLOB): BOOL; stdcall;
  external 'Crypt32.dll' name 'CryptUnprotectData';

function FlagsForScope(AScope: TOBDSecureSettingsScope): DWORD;
begin
  Result := CRYPTPROTECT_UI_FORBIDDEN;
  if AScope = ssLocalMachine then
    Result := Result or CRYPTPROTECT_LOCAL_MACHINE;
end;

function DPAPIProtect(const APlain: TBytes;
  AScope: TOBDSecureSettingsScope): TBytes;
var
  In_, Out_: DATA_BLOB;
begin
  In_.cbData := Length(APlain);
  if In_.cbData > 0 then
    In_.pbData := @APlain[0]
  else
    In_.pbData := nil;
  if not CryptProtectData(@In_, nil, nil, nil, nil,
                          FlagsForScope(AScope), @Out_) then
    raise EOBDSecureSettings.CreateFmt(
      'CryptProtectData failed: %d', [GetLastError]);
  try
    SetLength(Result, Out_.cbData);
    if Out_.cbData > 0 then
      Move(Out_.pbData^, Result[0], Out_.cbData);
  finally
    if Out_.pbData <> nil then
      LocalFree(HLOCAL(Out_.pbData));
  end;
end;

function DPAPIUnprotect(const ACipher: TBytes;
  AScope: TOBDSecureSettingsScope): TBytes;
var
  In_, Out_: DATA_BLOB;
begin
  In_.cbData := Length(ACipher);
  if In_.cbData > 0 then
    In_.pbData := @ACipher[0]
  else
    In_.pbData := nil;
  if not CryptUnprotectData(@In_, nil, nil, nil, nil,
                            FlagsForScope(AScope), @Out_) then
    raise EOBDSecureSettings.CreateFmt(
      'CryptUnprotectData failed: %d', [GetLastError]);
  try
    SetLength(Result, Out_.cbData);
    if Out_.cbData > 0 then
      Move(Out_.pbData^, Result[0], Out_.cbData);
  finally
    if Out_.pbData <> nil then
      LocalFree(HLOCAL(Out_.pbData));
  end;
end;
{$ELSE}
function DPAPIProtect(const APlain: TBytes;
  AScope: TOBDSecureSettingsScope): TBytes;
begin
  raise EOBDSecureSettings.Create(
    'TOBDSecureSettings: DPAPI is Windows-only');
end;

function DPAPIUnprotect(const ACipher: TBytes;
  AScope: TOBDSecureSettingsScope): TBytes;
begin
  raise EOBDSecureSettings.Create(
    'TOBDSecureSettings: DPAPI is Windows-only');
end;
{$ENDIF}

function DefaultStorePath(const AAppName: string;
  AScope: TOBDSecureSettingsScope): string;
var
  Base: string;
  EnvVar: string;
begin
  {$IFDEF MSWINDOWS}
  if AScope = ssLocalMachine then
    EnvVar := 'PROGRAMDATA'
  else
    EnvVar := 'LOCALAPPDATA';
  Base := GetEnvironmentVariable(EnvVar);
  if Base = '' then
    Base := TPath.GetHomePath;
  {$ELSE}
  Base := TPath.GetHomePath;
  {$ENDIF}
  Result := TPath.Combine(TPath.Combine(Base, AAppName), 'settings.dat');
end;

{ TOBDSecureSettings }

constructor TOBDSecureSettings.Create(const AAppName: string;
  AScope: TOBDSecureSettingsScope);
begin
  CreateAt(DefaultStorePath(AAppName, AScope), AScope);
end;

constructor TOBDSecureSettings.CreateAt(const AFilePath: string;
  AScope: TOBDSecureSettingsScope);
begin
  inherited Create;
  FFilePath := AFilePath;
  FScope := AScope;
  FLock := TCriticalSection.Create;
  FCache := TDictionary<string, string>.Create;
end;

destructor TOBDSecureSettings.Destroy;
begin
  FCache.Free;
  FLock.Free;
  inherited;
end;

function TOBDSecureSettings.ProtectBlob(const APlainText: TBytes): TBytes;
begin
  Result := DPAPIProtect(APlainText, FScope);
end;

function TOBDSecureSettings.UnprotectBlob(const ACipherText: TBytes): TBytes;
begin
  Result := DPAPIUnprotect(ACipherText, FScope);
end;

procedure TOBDSecureSettings.EnsureLoaded;
var
  Raw: TBytes;
  PlainBytes: TBytes;
  PlainText: string;
  Root: TJSONValue;
  Obj: TJSONObject;
  I: Integer;
  Pair: TJSONPair;
begin
  if FLoaded then
    Exit;
  FLoaded := True;
  if not TFile.Exists(FFilePath) then
    Exit;
  try
    Raw := TFile.ReadAllBytes(FFilePath);
    PlainBytes := UnprotectBlob(Raw);
    PlainText := TEncoding.UTF8.GetString(PlainBytes);
  except
    on E: Exception do
      raise EOBDSecureSettings.CreateFmt(
        'TOBDSecureSettings: cannot decrypt "%s": %s',
        [FFilePath, E.Message]);
  end;
  Root := TJSONObject.ParseJSONValue(PlainText);
  if not (Root is TJSONObject) then
  begin
    Root.Free;
    raise EOBDSecureSettings.CreateFmt(
      'TOBDSecureSettings: malformed JSON in "%s"', [FFilePath]);
  end;
  Obj := Root as TJSONObject;
  try
    for I := 0 to Obj.Count - 1 do
    begin
      Pair := Obj.Pairs[I];
      FCache.AddOrSetValue(Pair.JsonString.Value,
        Pair.JsonValue.Value);
    end;
  finally
    Root.Free;
  end;
end;

function TOBDSecureSettings.Read(const AKey: string;
  const ADefault: string): string;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    if not FCache.TryGetValue(AKey, Result) then
      Result := ADefault;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecureSettings.Write(const AKey: string;
  const AValue: string);
begin
  FLock.Enter;
  try
    EnsureLoaded;
    FCache.AddOrSetValue(AKey, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecureSettings.Delete(const AKey: string);
begin
  FLock.Enter;
  try
    EnsureLoaded;
    FCache.Remove(AKey);
  finally
    FLock.Leave;
  end;
end;

function TOBDSecureSettings.HasKey(const AKey: string): Boolean;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    Result := FCache.ContainsKey(AKey);
  finally
    FLock.Leave;
  end;
end;

function TOBDSecureSettings.Count: Integer;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    Result := FCache.Count;
  finally
    FLock.Leave;
  end;
end;

function TOBDSecureSettings.Keys: TArray<string>;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    Result := FCache.Keys.ToArray;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecureSettings.Save;
var
  Obj: TJSONObject;
  Pair: TPair<string, string>;
  PlainText: string;
  PlainBytes: TBytes;
  Cipher: TBytes;
  Dir: string;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    Obj := TJSONObject.Create;
    try
      for Pair in FCache do
        Obj.AddPair(Pair.Key, Pair.Value);
      PlainText := Obj.ToString;
    finally
      Obj.Free;
    end;
    PlainBytes := TEncoding.UTF8.GetBytes(PlainText);
    Cipher := ProtectBlob(PlainBytes);
    Dir := TPath.GetDirectoryName(FFilePath);
    if (Dir <> '') and not TDirectory.Exists(Dir) then
      TDirectory.CreateDirectory(Dir);
    TFile.WriteAllBytes(FFilePath, Cipher);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSecureSettings.Clear;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    FCache.Clear;
  finally
    FLock.Leave;
  end;
end;

end.
