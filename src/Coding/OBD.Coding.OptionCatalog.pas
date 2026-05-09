//------------------------------------------------------------------------------
//  OBD.Coding.OptionCatalog
//
//  TOBDCodingOptionCatalog — loader for OEM coding-option name
//  catalogues. The package ships only the schema document
//  (data/schemas/oem-coding-catalog.schema.json); option content
//  is dealer / workshop-database material that hosts populate
//  themselves and load through this unit.
//
//  Schema kinds covered:
//
//    byte_bit         — VAG / Mercedes single-bit options
//    byte_field       — Mercedes sub-byte fields
//    byte_range       — VAG multi-byte ranges
//    tlv_id           — BMW CAFD / Stellantis Proxi entries
//    config_word      — HMG / Toyota id-width-value records
//    asbuilt_section  — Ford AsBuilt named sections
//    menu_index       — Honda flat-array indices
//
//  The loader validates each entry against the schema's
//  per-kind rules. Hosts query the catalogue by vendor + name
//  and feed the resulting addressing record into the matching
//  per-OEM coder unit (TOBDCodingVAG, TOBDCodingBMW, …).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 follow-up.
//------------------------------------------------------------------------------

unit OBD.Coding.OptionCatalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>Vendor key (matches the unit suffix of the OEM
  /// coder this catalogue applies to).</summary>
  TOBDOEMVendor = (
    ovVAG, ovBMW, ovFord, ovHMG, ovHonda,
    ovMercedes, ovStellantis, ovToyota
  );

  /// <summary>Addressing kind. Matches the schema's
  /// <c>addressing.kind</c> enum verbatim.</summary>
  TOBDOptionAddressing = (
    oaByteBit,
    oaByteField,
    oaByteRange,
    oaTlvID,
    oaConfigWord,
    oaAsBuiltSection,
    oaMenuIndex
  );

  /// <summary>Endianness selector for byte-range addressing.</summary>
  TOBDOptionEndian = (oeBig, oeLittle);

  /// <summary>One enumerated value label.</summary>
  TOBDOptionValueLabel = record
    Raw: UInt32;
    Label_: string;
    Description: string;
  end;

  /// <summary>Decoded addressing record. The fields are tagged
  /// by <c>Kind</c>; only the fields relevant to that kind are
  /// populated.</summary>
  TOBDOptionAddressingInfo = record
    Kind: TOBDOptionAddressing;
    // byte_bit / byte_field / byte_range / asbuilt_section
    Byte_: Integer;
    Bit: Integer;
    Shift: Integer;
    Width: Integer;
    Offset: Integer;
    Length_: Integer;
    Endian: TOBDOptionEndian;
    // tlv_id / config_word
    ID: UInt32;
    // asbuilt_section
    Section: string;
    // menu_index
    Index: Integer;
  end;

  /// <summary>Decoded option entry.</summary>
  TOBDOptionEntry = record
    Name: string;
    Description: string;
    Addressing: TOBDOptionAddressingInfo;
    Values: TArray<TOBDOptionValueLabel>;
    DefaultRaw: UInt32;
    HasDefault: Boolean;
    Tags: TArray<string>;
  end;

  /// <summary>One loaded catalogue.</summary>
  TOBDCodingOptionDoc = record
    Vendor: TOBDOEMVendor;
    Module: string;
    ModuleVersion: string;
    Source: string;
    Options: TArray<TOBDOptionEntry>;
  end;

  /// <summary>Process-wide registry of option catalogues. Thread-
  /// safe.</summary>
  TOBDCodingOptionCatalog = class
  strict private
    class var FInstance: TOBDCodingOptionCatalog;
    FLock: TCriticalSection;
    FDocs: TList<TOBDCodingOptionDoc>;
    procedure ParseDocument(const ARoot: TJSONObject;
      const AFileName: string; out ADoc: TOBDCodingOptionDoc);
    procedure ParseAddressing(const AObj: TJSONObject;
      var AInfo: TOBDOptionAddressingInfo);
    procedure ParseValues(const AArr: TJSONArray;
      var AValues: TArray<TOBDOptionValueLabel>);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>The shared instance.</summary>
    class function Default: TOBDCodingOptionCatalog;
    class procedure ReleaseDefault;

    /// <summary>Loads one catalogue file. Replaces existing
    /// catalogues for the same (vendor, module) pair.</summary>
    /// <exception cref="EOBDConfig">Schema violation or
    /// malformed JSON.</exception>
    procedure LoadFile(const AFileName: string);

    /// <summary>Loads every <c>*.json</c> file under <c>ARoot</c>
    /// recursively.</summary>
    /// <returns>Number of files loaded.</returns>
    function LoadDirectory(const ARoot: string): Integer;

    /// <summary>Removes every loaded catalogue.</summary>
    procedure Clear;

    /// <summary>Number of loaded catalogues.</summary>
    function Count: Integer;
    /// <summary>Returns the catalogue for <c>(AVendor, AModule)</c>.
    /// Module match is case-insensitive.</summary>
    function TryGet(AVendor: TOBDOEMVendor; const AModule: string;
      out ADoc: TOBDCodingOptionDoc): Boolean;
    /// <summary>Looks up an option by name within a catalogue.</summary>
    function TryFindOption(AVendor: TOBDOEMVendor; const AModule, AName: string;
      out AEntry: TOBDOptionEntry): Boolean;
    /// <summary>Returns the value label for <c>ARaw</c> within an
    /// entry. Empty when the entry has no enumerated values or
    /// none match.</summary>
    class function LookupLabel(const AEntry: TOBDOptionEntry;
      ARaw: UInt32): string; static;
  end;

/// <summary>Maps the schema's vendor string to the enum.</summary>
function ParseVendor(const AText: string): TOBDOEMVendor;

implementation

function ParseVendor(const AText: string): TOBDOEMVendor;
var
  Lower: string;
begin
  Lower := LowerCase(Trim(AText));
  if      Lower = 'vag'        then Result := ovVAG
  else if Lower = 'bmw'        then Result := ovBMW
  else if Lower = 'ford'       then Result := ovFord
  else if Lower = 'hmg'        then Result := ovHMG
  else if Lower = 'honda'      then Result := ovHonda
  else if Lower = 'mercedes'   then Result := ovMercedes
  else if Lower = 'stellantis' then Result := ovStellantis
  else if Lower = 'toyota'     then Result := ovToyota
  else
    raise EOBDConfig.CreateFmt(
      'OEM coding catalogue: vendor "%s" not in schema enum', [AText]);
end;

function ParseAddrKind(const AText: string): TOBDOptionAddressing;
var
  Lower: string;
begin
  Lower := LowerCase(Trim(AText));
  if      Lower = 'byte_bit'         then Result := oaByteBit
  else if Lower = 'byte_field'       then Result := oaByteField
  else if Lower = 'byte_range'       then Result := oaByteRange
  else if Lower = 'tlv_id'           then Result := oaTlvID
  else if Lower = 'config_word'      then Result := oaConfigWord
  else if Lower = 'asbuilt_section'  then Result := oaAsBuiltSection
  else if Lower = 'menu_index'       then Result := oaMenuIndex
  else
    raise EOBDConfig.CreateFmt(
      'OEM coding catalogue: addressing.kind "%s" not in schema',
      [AText]);
end;

function GetJSONStr(AObj: TJSONObject; const AKey, ADefault: string): string;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AKey);
  if V is TJSONString then Result := TJSONString(V).Value
  else Result := ADefault;
end;

function GetJSONInt(AObj: TJSONObject; const AKey: string;
  ADefault: Int64): Int64;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AKey);
  if V is TJSONNumber then Result := TJSONNumber(V).AsInt64
  else Result := ADefault;
end;

function HasJSON(AObj: TJSONObject; const AKey: string): Boolean;
begin
  Result := AObj.GetValue(AKey) <> nil;
end;

{ ---- TOBDCodingOptionCatalog ----------------------------------------------- }

constructor TOBDCodingOptionCatalog.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FDocs := TList<TOBDCodingOptionDoc>.Create;
end;

destructor TOBDCodingOptionCatalog.Destroy;
begin
  FDocs.Free;
  FLock.Free;
  inherited;
end;

class function TOBDCodingOptionCatalog.Default: TOBDCodingOptionCatalog;
begin
  if FInstance = nil then
    FInstance := TOBDCodingOptionCatalog.Create;
  Result := FInstance;
end;

class procedure TOBDCodingOptionCatalog.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDCodingOptionCatalog.Clear;
begin
  FLock.Enter;
  try FDocs.Clear;
  finally FLock.Leave; end;
end;

function TOBDCodingOptionCatalog.Count: Integer;
begin
  FLock.Enter;
  try Result := FDocs.Count;
  finally FLock.Leave; end;
end;

procedure TOBDCodingOptionCatalog.ParseValues(const AArr: TJSONArray;
  var AValues: TArray<TOBDOptionValueLabel>);
var
  I: Integer;
  Obj: TJSONObject;
  Entry: TOBDOptionValueLabel;
begin
  SetLength(AValues, AArr.Count);
  for I := 0 to AArr.Count - 1 do
  begin
    if not (AArr.Items[I] is TJSONObject) then
      raise EOBDConfig.CreateFmt(
        'OEM coding catalogue: values[%d] is not an object', [I]);
    Obj := AArr.Items[I] as TJSONObject;
    Entry := Default(TOBDOptionValueLabel);
    if not HasJSON(Obj, 'raw') then
      raise EOBDConfig.CreateFmt('values[%d].raw missing', [I]);
    if not HasJSON(Obj, 'label') then
      raise EOBDConfig.CreateFmt('values[%d].label missing', [I]);
    Entry.Raw := UInt32(GetJSONInt(Obj, 'raw', 0));
    Entry.Label_ := GetJSONStr(Obj, 'label', '');
    Entry.Description := GetJSONStr(Obj, 'description', '');
    AValues[I] := Entry;
  end;
end;

procedure TOBDCodingOptionCatalog.ParseAddressing(const AObj: TJSONObject;
  var AInfo: TOBDOptionAddressingInfo);
var
  KindStr: string;
  EndianStr: string;
begin
  AInfo := Default(TOBDOptionAddressingInfo);
  AInfo.Endian := oeBig;
  KindStr := GetJSONStr(AObj, 'kind', '');
  if KindStr = '' then
    raise EOBDConfig.Create('addressing.kind missing');
  AInfo.Kind := ParseAddrKind(KindStr);

  case AInfo.Kind of
    oaByteBit:
      begin
        if not HasJSON(AObj, 'byte') or not HasJSON(AObj, 'bit') then
          raise EOBDConfig.Create(
            'byte_bit requires both "byte" and "bit"');
        AInfo.Byte_ := GetJSONInt(AObj, 'byte', 0);
        AInfo.Bit   := GetJSONInt(AObj, 'bit', 0);
        if (AInfo.Bit < 0) or (AInfo.Bit > 7) then
          raise EOBDConfig.CreateFmt('byte_bit.bit %d out of range',
            [AInfo.Bit]);
      end;
    oaByteField:
      begin
        if not HasJSON(AObj, 'byte') or not HasJSON(AObj, 'shift') or
           not HasJSON(AObj, 'width') then
          raise EOBDConfig.Create(
            'byte_field requires "byte", "shift", "width"');
        AInfo.Byte_ := GetJSONInt(AObj, 'byte', 0);
        AInfo.Shift := GetJSONInt(AObj, 'shift', 0);
        AInfo.Width := GetJSONInt(AObj, 'width', 1);
        if (AInfo.Shift < 0) or (AInfo.Shift > 7) or
           (AInfo.Width < 1) or (AInfo.Width > 8) or
           (AInfo.Shift + AInfo.Width > 8) then
          raise EOBDConfig.CreateFmt(
            'byte_field shift=%d width=%d invalid', [AInfo.Shift, AInfo.Width]);
      end;
    oaByteRange:
      begin
        if not HasJSON(AObj, 'offset') or not HasJSON(AObj, 'length') then
          raise EOBDConfig.Create(
            'byte_range requires "offset" and "length"');
        AInfo.Offset  := GetJSONInt(AObj, 'offset', 0);
        AInfo.Length_ := GetJSONInt(AObj, 'length', 0);
        if AInfo.Length_ < 1 then
          raise EOBDConfig.Create('byte_range.length must be ≥ 1');
        EndianStr := LowerCase(GetJSONStr(AObj, 'endian', 'big'));
        if EndianStr = 'little' then AInfo.Endian := oeLittle
        else if EndianStr = 'big' then AInfo.Endian := oeBig
        else raise EOBDConfig.CreateFmt('byte_range.endian "%s" invalid',
          [EndianStr]);
      end;
    oaTlvID:
      begin
        if not HasJSON(AObj, 'id') then
          raise EOBDConfig.Create('tlv_id requires "id"');
        AInfo.ID := UInt32(GetJSONInt(AObj, 'id', 0));
        if HasJSON(AObj, 'bit') then
          AInfo.Bit := GetJSONInt(AObj, 'bit', 0)
        else
          AInfo.Bit := -1;
      end;
    oaConfigWord:
      begin
        if not HasJSON(AObj, 'id') or not HasJSON(AObj, 'width') then
          raise EOBDConfig.Create(
            'config_word requires "id" and "width"');
        AInfo.ID := UInt32(GetJSONInt(AObj, 'id', 0));
        AInfo.Width := GetJSONInt(AObj, 'width', 1);
        if not (AInfo.Width in [1, 2, 4]) then
          raise EOBDConfig.CreateFmt('config_word.width %d not in {1,2,4}',
            [AInfo.Width]);
      end;
    oaAsBuiltSection:
      begin
        if not HasJSON(AObj, 'section') or not HasJSON(AObj, 'offset') then
          raise EOBDConfig.Create(
            'asbuilt_section requires "section" and "offset"');
        AInfo.Section := GetJSONStr(AObj, 'section', '');
        AInfo.Offset  := GetJSONInt(AObj, 'offset', 0);
        if HasJSON(AObj, 'bit') then
          AInfo.Bit := GetJSONInt(AObj, 'bit', 0)
        else
          AInfo.Bit := -1;
      end;
    oaMenuIndex:
      begin
        if not HasJSON(AObj, 'index') then
          raise EOBDConfig.Create('menu_index requires "index"');
        AInfo.Index := GetJSONInt(AObj, 'index', 0);
      end;
  end;
end;

procedure TOBDCodingOptionCatalog.ParseDocument(const ARoot: TJSONObject;
  const AFileName: string; out ADoc: TOBDCodingOptionDoc);
var
  V: TJSONValue;
  Arr: TJSONArray;
  Obj, AddrObj: TJSONObject;
  Entry: TOBDOptionEntry;
  TagsArr: TJSONArray;
  I, J: Integer;
  Version: Int64;
  TagsAcc: TList<string>;
begin
  ADoc := Default(TOBDCodingOptionDoc);
  Version := GetJSONInt(ARoot, 'version', 0);
  if Version <> 1 then
    raise EOBDConfig.CreateFmt('%s: schema version must be 1, got %d',
      [AFileName, Version]);
  ADoc.Vendor := ParseVendor(GetJSONStr(ARoot, 'vendor', ''));
  ADoc.Module := GetJSONStr(ARoot, 'module', '');
  if ADoc.Module = '' then
    raise EOBDConfig.CreateFmt('%s: "module" is required', [AFileName]);
  ADoc.ModuleVersion := GetJSONStr(ARoot, 'module_version', '');
  ADoc.Source := GetJSONStr(ARoot, 'source', '');

  V := ARoot.GetValue('options');
  if not (V is TJSONArray) then
    raise EOBDConfig.CreateFmt('%s: "options" must be an array',
      [AFileName]);
  Arr := V as TJSONArray;
  SetLength(ADoc.Options, Arr.Count);

  TagsAcc := TList<string>.Create;
  try
    for I := 0 to Arr.Count - 1 do
    begin
      if not (Arr.Items[I] is TJSONObject) then
        raise EOBDConfig.CreateFmt('%s: options[%d] not an object',
          [AFileName, I]);
      Obj := Arr.Items[I] as TJSONObject;
      Entry := Default(TOBDOptionEntry);
      Entry.Name := GetJSONStr(Obj, 'name', '');
      if Entry.Name = '' then
        raise EOBDConfig.CreateFmt('%s: options[%d].name missing',
          [AFileName, I]);
      Entry.Description := GetJSONStr(Obj, 'description', '');

      V := Obj.GetValue('addressing');
      if not (V is TJSONObject) then
        raise EOBDConfig.CreateFmt('%s: options[%d].addressing missing',
          [AFileName, I]);
      AddrObj := V as TJSONObject;
      ParseAddressing(AddrObj, Entry.Addressing);

      V := Obj.GetValue('values');
      if V is TJSONArray then
        ParseValues(V as TJSONArray, Entry.Values);

      if HasJSON(Obj, 'default_raw') then
      begin
        Entry.DefaultRaw := UInt32(GetJSONInt(Obj, 'default_raw', 0));
        Entry.HasDefault := True;
      end;

      V := Obj.GetValue('tags');
      if V is TJSONArray then
      begin
        TagsArr := V as TJSONArray;
        TagsAcc.Clear;
        for J := 0 to TagsArr.Count - 1 do
          if TagsArr.Items[J] is TJSONString then
            TagsAcc.Add(TJSONString(TagsArr.Items[J]).Value);
        Entry.Tags := TagsAcc.ToArray;
      end;

      ADoc.Options[I] := Entry;
    end;
  finally
    TagsAcc.Free;
  end;
end;

procedure TOBDCodingOptionCatalog.LoadFile(const AFileName: string);
var
  Json: string;
  Doc: TJSONValue;
  Root: TJSONObject;
  Loaded: TOBDCodingOptionDoc;
  I: Integer;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDConfig.CreateFmt('OEM coding catalogue not found: %s',
      [AFileName]);
  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
  if Doc = nil then
    raise EOBDConfig.CreateFmt('%s: invalid JSON', [AFileName]);
  try
    if not (Doc is TJSONObject) then
      raise EOBDConfig.CreateFmt('%s: root not an object', [AFileName]);
    Root := Doc as TJSONObject;
    ParseDocument(Root, AFileName, Loaded);
    FLock.Enter;
    try
      // Replace existing matching (vendor, module).
      for I := FDocs.Count - 1 downto 0 do
        if (FDocs[I].Vendor = Loaded.Vendor) and
           SameText(FDocs[I].Module, Loaded.Module) then
          FDocs.Delete(I);
      FDocs.Add(Loaded);
    finally
      FLock.Leave;
    end;
  finally
    Doc.Free;
  end;
end;

function TOBDCodingOptionCatalog.LoadDirectory(const ARoot: string): Integer;
var
  Files: TArray<string>;
  F: string;
begin
  Result := 0;
  if not TDirectory.Exists(ARoot) then
    raise EOBDConfig.CreateFmt('Catalogue directory not found: %s', [ARoot]);
  Files := TDirectory.GetFiles(ARoot, '*.json',
    TSearchOption.soAllDirectories);
  for F in Files do
  begin
    LoadFile(F);
    Inc(Result);
  end;
end;

function TOBDCodingOptionCatalog.TryGet(AVendor: TOBDOEMVendor;
  const AModule: string; out ADoc: TOBDCodingOptionDoc): Boolean;
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to FDocs.Count - 1 do
      if (FDocs[I].Vendor = AVendor) and
         SameText(FDocs[I].Module, AModule) then
      begin
        ADoc := FDocs[I];
        Exit(True);
      end;
    Result := False;
  finally
    FLock.Leave;
  end;
end;

function TOBDCodingOptionCatalog.TryFindOption(AVendor: TOBDOEMVendor;
  const AModule, AName: string;
  out AEntry: TOBDOptionEntry): Boolean;
var
  Doc: TOBDCodingOptionDoc;
  I: Integer;
begin
  Result := False;
  if not TryGet(AVendor, AModule, Doc) then Exit;
  for I := 0 to High(Doc.Options) do
    if SameText(Doc.Options[I].Name, AName) then
    begin
      AEntry := Doc.Options[I];
      Exit(True);
    end;
end;

class function TOBDCodingOptionCatalog.LookupLabel(
  const AEntry: TOBDOptionEntry; ARaw: UInt32): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AEntry.Values) do
    if AEntry.Values[I].Raw = ARaw then
      Exit(AEntry.Values[I].Label_);
end;

initialization

finalization
  TOBDCodingOptionCatalog.ReleaseDefault;

end.
