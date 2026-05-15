//------------------------------------------------------------------------------
//  OBD.Flash.OEM.Catalog
//
//  TOBDFlashOEMCatalog — loader for per-platform overrides on
//  the OEM bootloader handshakes. Schema lives in
//  <c>data/schemas/oem-flash-handshake-catalog.schema.json</c>.
//
//  Hosts populate the catalogue from their own ground-truth
//  sources (ISTA / ODX / vendor service docs) and call
//  <see cref="ApplyTo"/> against a vendor-specific
//  <c>TOBDFlashHandshakeXxx</c> instance to overlay the platform's
//  session sub-function, security level, RIDs and addressing
//  format bytes before <c>Run</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.Flash.OEM.Catalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Flash.OEM.Common,
  OBD.Flash.OEM.VAG,
  OBD.Flash.OEM.BMW,
  OBD.Flash.OEM.Ford,
  OBD.Flash.OEM.HMG,
  OBD.Flash.OEM.Mercedes,
  OBD.Flash.OEM.Stellantis,
  OBD.Flash.OEM.Toyota;

type
  /// <summary>One platform / ECU entry.</summary>
  TOBDOEMPlatform = record
    Name: string;
    EcuFamily: string;
    SessionSubFunction: Byte;
    HasSessionSubFunction: Boolean;
    ExtendedFirst: Boolean;
    HasExtendedFirst: Boolean;
    SecurityLevel: Byte;
    HasSecurityLevel: Boolean;
    EraseRoutineID: Word;
    HasEraseRoutineID: Boolean;
    VerifyRoutineID: Word;
    HasVerifyRoutineID: Boolean;
    AddressFormatBytes: Byte;
    HasAddressFormatBytes: Boolean;
    LengthFormatBytes: Byte;
    HasLengthFormatBytes: Boolean;
    DataFormatIdentifier: Byte;
    HasDataFormatIdentifier: Boolean;
    Notes: string;
  end;

  /// <summary>Decoded catalogue.</summary>
  TOBDOEMCatalogDoc = record
    Vendor: string;
    VendorDisplay: string;
    Source: string;
    Platforms: TArray<TOBDOEMPlatform>;
  end;

  /// <summary>Process-wide registry of OEM-handshake catalogues.
  /// Thread-safe.</summary>
  TOBDFlashOEMCatalog = class
  strict private
    class var FInstance: TOBDFlashOEMCatalog;
    FLock: TCriticalSection;
    FDocs: TList<TOBDOEMCatalogDoc>;
    procedure ParseDocument(const ARoot: TJSONObject;
      out ADoc: TOBDOEMCatalogDoc);
  public
    constructor Create;
    destructor Destroy; override;
    class function Default: TOBDFlashOEMCatalog;
    class procedure ReleaseDefault;

    /// <summary>Loads one catalogue file. Replaces existing
    /// entries with the same vendor.</summary>
    procedure LoadFile(const AFileName: string);
    procedure Clear;
    function Count: Integer;
    /// <summary>Looks up a platform by (vendor, name).</summary>
    function TryGetPlatform(const AVendor, AName: string;
      out APlatform: TOBDOEMPlatform): Boolean;

    /// <summary>Overlays the platform fields onto a VAG handshake.</summary>
    class procedure ApplyTo(AHandshake: TOBDFlashHandshakeVAG;
      const APlatform: TOBDOEMPlatform); overload; static;
    class procedure ApplyTo(AHandshake: TOBDFlashHandshakeBMW;
      const APlatform: TOBDOEMPlatform); overload; static;
    class procedure ApplyTo(AHandshake: TOBDFlashHandshakeFord;
      const APlatform: TOBDOEMPlatform); overload; static;
    class procedure ApplyTo(AHandshake: TOBDFlashHandshakeHMG;
      const APlatform: TOBDOEMPlatform); overload; static;
    class procedure ApplyTo(AHandshake: TOBDFlashHandshakeMercedes;
      const APlatform: TOBDOEMPlatform); overload; static;
    class procedure ApplyTo(AHandshake: TOBDFlashHandshakeStellantis;
      const APlatform: TOBDOEMPlatform); overload; static;
    class procedure ApplyTo(AHandshake: TOBDFlashHandshakeToyota;
      const APlatform: TOBDOEMPlatform); overload; static;
  end;

implementation

function ParseUIntJSON(AVal: TJSONValue; ADefault: Int64): Int64;
var
  S: string;
begin
  if AVal is TJSONNumber then Exit(TJSONNumber(AVal).AsInt64);
  if AVal is TJSONString then
  begin
    S := Trim(TJSONString(AVal).Value);
    if (Length(S) >= 2) and (S[1] = '0') and
       CharInSet(S[2], ['x', 'X']) then
      Exit(StrToInt64('$' + Copy(S, 3, MaxInt)));
    Exit(StrToInt64(S));
  end;
  Result := ADefault;
end;

function ReadString(AObj: TJSONObject; const AKey, ADef: string): string;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AKey);
  if V is TJSONString then Result := V.Value else Result := ADef;
end;

function ReadBool(AObj: TJSONObject; const AKey: string;
  ADef, out AHas: Boolean): Boolean;
var
  V: TJSONValue;
begin
  AHas := False;
  V := AObj.GetValue(AKey);
  if V is TJSONBool then begin AHas := True; Exit(TJSONBool(V).AsBoolean); end;
  Result := ADef;
end;

function ReadByte(AObj: TJSONObject; const AKey: string;
  ADef: Byte; out AHas: Boolean): Byte;
var
  V: TJSONValue;
begin
  AHas := False;
  V := AObj.GetValue(AKey);
  if V <> nil then begin AHas := True; Exit(Byte(ParseUIntJSON(V, ADef))); end;
  Result := ADef;
end;

function ReadWord(AObj: TJSONObject; const AKey: string;
  ADef: Word; out AHas: Boolean): Word;
var
  V: TJSONValue;
begin
  AHas := False;
  V := AObj.GetValue(AKey);
  if V <> nil then begin AHas := True; Exit(Word(ParseUIntJSON(V, ADef))); end;
  Result := ADef;
end;

constructor TOBDFlashOEMCatalog.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FDocs := TList<TOBDOEMCatalogDoc>.Create;
end;

destructor TOBDFlashOEMCatalog.Destroy;
begin
  FDocs.Free;
  FLock.Free;
  inherited;
end;

class function TOBDFlashOEMCatalog.Default: TOBDFlashOEMCatalog;
begin
  if FInstance = nil then FInstance := TOBDFlashOEMCatalog.Create;
  Result := FInstance;
end;

class procedure TOBDFlashOEMCatalog.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDFlashOEMCatalog.Clear;
begin
  FLock.Enter;
  try FDocs.Clear;
  finally FLock.Leave; end;
end;

function TOBDFlashOEMCatalog.Count: Integer;
begin
  FLock.Enter;
  try Result := FDocs.Count;
  finally FLock.Leave; end;
end;

procedure TOBDFlashOEMCatalog.ParseDocument(const ARoot: TJSONObject;
  out ADoc: TOBDOEMCatalogDoc);
var
  V: TJSONValue;
  Arr: TJSONArray;
  PlatObj: TJSONObject;
  I: Integer;
  Plat: TOBDOEMPlatform;
  Acc: TList<TOBDOEMPlatform>;
  Has: Boolean;
begin
  ADoc := Default(TOBDOEMCatalogDoc);
  V := ARoot.GetValue('version');
  if not (V is TJSONNumber) or (TJSONNumber(V).AsInt64 <> 1) then
    raise EOBDProtocol.Create('OEM handshake catalogue: version must be 1');
  ADoc.Vendor        := ReadString(ARoot, 'vendor', '');
  ADoc.VendorDisplay := ReadString(ARoot, 'vendor_display', '');
  ADoc.Source        := ReadString(ARoot, 'source', '');
  if ADoc.Vendor = '' then
    raise EOBDProtocol.Create('OEM handshake catalogue: vendor required');

  V := ARoot.GetValue('platforms');
  if not (V is TJSONArray) then
    raise EOBDProtocol.Create(
      'OEM handshake catalogue: platforms must be an array');
  Arr := V as TJSONArray;
  Acc := TList<TOBDOEMPlatform>.Create;
  try
    for I := 0 to Arr.Count - 1 do
    begin
      if not (Arr.Items[I] is TJSONObject) then Continue;
      PlatObj := Arr.Items[I] as TJSONObject;
      Plat := Default(TOBDOEMPlatform);
      Plat.Name := ReadString(PlatObj, 'name', '');
      if Plat.Name = '' then Continue;
      Plat.EcuFamily := ReadString(PlatObj, 'ecu_family', '');
      Plat.SessionSubFunction := ReadByte(PlatObj, 'session_subfunction', 0,
        Plat.HasSessionSubFunction);
      Plat.ExtendedFirst := ReadBool(PlatObj, 'extended_first', False,
        Plat.HasExtendedFirst);
      Plat.SecurityLevel := ReadByte(PlatObj, 'security_level', 0,
        Plat.HasSecurityLevel);
      Plat.EraseRoutineID := ReadWord(PlatObj, 'erase_routine_id', 0,
        Plat.HasEraseRoutineID);
      Plat.VerifyRoutineID := ReadWord(PlatObj, 'verify_routine_id', 0,
        Plat.HasVerifyRoutineID);
      Plat.AddressFormatBytes := ReadByte(PlatObj, 'address_format_bytes', 0,
        Plat.HasAddressFormatBytes);
      Plat.LengthFormatBytes := ReadByte(PlatObj, 'length_format_bytes', 0,
        Plat.HasLengthFormatBytes);
      Plat.DataFormatIdentifier := ReadByte(PlatObj, 'data_format_identifier',
        0, Plat.HasDataFormatIdentifier);
      Plat.Notes := ReadString(PlatObj, 'notes', '');
      Acc.Add(Plat);
      Has := False; if Has then ; // suppress warning
    end;
    ADoc.Platforms := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

procedure TOBDFlashOEMCatalog.LoadFile(const AFileName: string);
var
  Json: string;
  Doc: TJSONValue;
  Loaded: TOBDOEMCatalogDoc;
  I: Integer;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDProtocol.CreateFmt(
      'OEM handshake catalogue not found: %s', [AFileName]);
  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
  if not (Doc is TJSONObject) then
  begin
    if Doc <> nil then Doc.Free;
    raise EOBDProtocol.CreateFmt(
      'OEM handshake catalogue: %s root not an object', [AFileName]);
  end;
  try
    ParseDocument(Doc as TJSONObject, Loaded);
    FLock.Enter;
    try
      // Replace any existing entry for the same vendor.
      for I := FDocs.Count - 1 downto 0 do
        if SameText(FDocs[I].Vendor, Loaded.Vendor) then
          FDocs.Delete(I);
      FDocs.Add(Loaded);
    finally
      FLock.Leave;
    end;
  finally
    Doc.Free;
  end;
end;

function TOBDFlashOEMCatalog.TryGetPlatform(const AVendor, AName: string;
  out APlatform: TOBDOEMPlatform): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  FLock.Enter;
  try
    for I := 0 to FDocs.Count - 1 do
      if SameText(FDocs[I].Vendor, AVendor) then
        for J := 0 to High(FDocs[I].Platforms) do
          if SameText(FDocs[I].Platforms[J].Name, AName) then
          begin
            APlatform := FDocs[I].Platforms[J];
            Exit(True);
          end;
  finally
    FLock.Leave;
  end;
end;

{ ---- ApplyTo overloads ----------------------------------------------------- }

class procedure TOBDFlashOEMCatalog.ApplyTo(AHandshake: TOBDFlashHandshakeVAG;
  const APlatform: TOBDOEMPlatform);
begin
  if APlatform.HasSecurityLevel  then AHandshake.SecurityLevel  := APlatform.SecurityLevel;
  if APlatform.HasEraseRoutineID then AHandshake.EraseRoutineID := APlatform.EraseRoutineID;
end;

class procedure TOBDFlashOEMCatalog.ApplyTo(AHandshake: TOBDFlashHandshakeBMW;
  const APlatform: TOBDOEMPlatform);
begin
  if APlatform.HasExtendedFirst   then AHandshake.ExtendedFirst  := APlatform.ExtendedFirst;
  if APlatform.HasSecurityLevel   then AHandshake.SecurityLevel  := APlatform.SecurityLevel;
  if APlatform.HasEraseRoutineID  then AHandshake.EraseRoutineID := APlatform.EraseRoutineID;
end;

class procedure TOBDFlashOEMCatalog.ApplyTo(AHandshake: TOBDFlashHandshakeFord;
  const APlatform: TOBDOEMPlatform);
begin
  if APlatform.HasSecurityLevel  then AHandshake.SecurityLevel  := APlatform.SecurityLevel;
  if APlatform.HasEraseRoutineID then AHandshake.EraseRoutineID := APlatform.EraseRoutineID;
end;

class procedure TOBDFlashOEMCatalog.ApplyTo(AHandshake: TOBDFlashHandshakeHMG;
  const APlatform: TOBDOEMPlatform);
begin
  if APlatform.HasSecurityLevel  then AHandshake.SecurityLevel  := APlatform.SecurityLevel;
  if APlatform.HasEraseRoutineID then AHandshake.EraseRoutineID := APlatform.EraseRoutineID;
end;

class procedure TOBDFlashOEMCatalog.ApplyTo(
  AHandshake: TOBDFlashHandshakeMercedes;
  const APlatform: TOBDOEMPlatform);
begin
  if APlatform.HasSecurityLevel  then AHandshake.SecurityLevel  := APlatform.SecurityLevel;
  if APlatform.HasEraseRoutineID then AHandshake.EraseRoutineID := APlatform.EraseRoutineID;
end;

class procedure TOBDFlashOEMCatalog.ApplyTo(
  AHandshake: TOBDFlashHandshakeStellantis;
  const APlatform: TOBDOEMPlatform);
begin
  if APlatform.HasSecurityLevel  then AHandshake.SecurityLevel  := APlatform.SecurityLevel;
  if APlatform.HasEraseRoutineID then AHandshake.EraseRoutineID := APlatform.EraseRoutineID;
end;

class procedure TOBDFlashOEMCatalog.ApplyTo(
  AHandshake: TOBDFlashHandshakeToyota;
  const APlatform: TOBDOEMPlatform);
begin
  if APlatform.HasSecurityLevel  then AHandshake.SecurityLevel  := APlatform.SecurityLevel;
  if APlatform.HasEraseRoutineID then AHandshake.EraseRoutineID := APlatform.EraseRoutineID;
end;

initialization

finalization
  TOBDFlashOEMCatalog.ReleaseDefault;

end.
