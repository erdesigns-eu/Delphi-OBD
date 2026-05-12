//------------------------------------------------------------------------------
//  OBD.OEM.Catalog.JSON
//
//  JSON-driven OEM catalogue loader. Reads DIDs, routines, ECUs,
//  DTC ranges, coding blocks, adaptations, actuator tests, live
//  PIDs and DTC extended-data records from a vendor catalogue
//  file, carrying provenance (<c>source</c>) and verification
//  (<c>verified</c>) metadata so callers can filter unverified
//  entries out of production-critical paths.
//
//  Hex literals are accepted as <c>0xNNNN</c>, <c>$NNNN</c> or
//  plain decimal. Each catalogue file is one top-level JSON
//  object — see <c>docs/CATALOG_FORMAT.md</c> for the schema.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Catalog.JSON;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  System.Generics.Defaults,
  OBD.OEM.Types,
  OBD.OEM.Extensions;

type
  /// <summary>Raised on catalogue-load / parse errors.</summary>
  EOBDCatalogError = class(Exception);

  /// <summary>
  ///   Decoder kinds the JSON catalogue can describe. Mirrors
  ///   <see cref="TOBDOEMDecoderKind"/> but lives on the JSON
  ///   parser side so the public schema stays free of parsing
  ///   concerns.
  /// </summary>
  TOBDDecoderKind = (
    dkUnknown,
    dkAscii,
    dkHex,
    dkUInt8,
    dkUInt16BE,
    dkUInt32BE,
    dkInt16BE,
    dkInt32BE,
    dkBcdDate,
    dkEnum,
    dkBitmask,
    dkSeconds);

  /// <summary>
  ///   Compiled form of a <c>decoder</c> sub-object.
  ///   <c>EnumValues</c> and <c>BitNames</c> hold their lookup
  ///   maps for fast decode and are owned by the parent
  ///   catalogue.
  /// </summary>
  TOBDDecoderSpec = record
    /// <summary>Decoder kind.</summary>
    Kind: TOBDDecoderKind;
    /// <summary>Fixed payload size for enum / bitmask kinds.</summary>
    Size: Integer;
    /// <summary>Multiplicative scale (default 1.0).</summary>
    Scale: Double;
    /// <summary>Additive offset.</summary>
    Offset: Double;
    /// <summary>Display unit suffix.</summary>
    Unit_: string;
    /// <summary>Value → label map for enum kinds.</summary>
    EnumValues: TDictionary<Cardinal, string>;
    /// <summary>Bit-index → label map for bitmask kinds.</summary>
    BitNames: TDictionary<Integer, string>;
  end;

  /// <summary>DID entry as loaded from JSON.</summary>
  TOBDOEMDIDEntry = record
    /// <summary>DID value.</summary>
    DID: Word;
    /// <summary>Short snake_case key.</summary>
    Name: string;
    /// <summary>Human-readable description.</summary>
    Description: string;
    /// <summary>Provenance — where the entry came from.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
    /// <summary>Owning ECU CAN-ID (0 = global).</summary>
    EcuAddress: Word;
    /// <summary>Compiled decoder, if any.</summary>
    Decoder: TOBDDecoderSpec;
  end;

  /// <summary>Routine entry as loaded from JSON.</summary>
  TOBDOEMRoutineEntry = record
    /// <summary>Routine ID.</summary>
    Identifier: Word;
    /// <summary>Short snake_case key.</summary>
    Name: string;
    /// <summary>Human-readable description.</summary>
    Description: string;
    /// <summary>Provenance.</summary>
    Source: string;
    /// <summary>Spec-confirmed.</summary>
    Verified: Boolean;
    /// <summary>Owning ECU CAN-ID (0 = global).</summary>
    EcuAddress: Word;
  end;

  /// <summary>ECU bus-map entry as loaded from JSON.</summary>
  TOBDOEMECUEntry = record
    /// <summary>CAN-ID.</summary>
    Address: Word;
    /// <summary>Short snake_case key.</summary>
    Name: string;
    /// <summary>Display label.</summary>
    CommonName: string;
  end;

  /// <summary>DTC code-range entry (start-end inclusive).</summary>
  TOBDOEMDtcRange = record
    /// <summary>Range start code.</summary>
    StartCode: string;
    /// <summary>Range end code.</summary>
    EndCode: string;
    /// <summary>Provenance.</summary>
    Source: string;
  end;

  /// <summary>Coding-field entry as loaded from JSON.</summary>
  TOBDCodingFieldEntry = record
    Name: string;
    Label_: string;
    Description: string;
    KindStr: string;
    ByteOffset: Integer;
    BitOffset: Integer;
    BitWidth: Integer;
    DefaultValue: Int64;
    DefaultAscii: string;
    MinValue: Int64;
    MaxValue: Int64;
    EnumValues: TArray<TPair<Integer, string>>;
  end;

  /// <summary>Coding-block entry as loaded from JSON.</summary>
  TOBDCodingBlockEntry = record
    DataIdentifier: Word;
    Name: string;
    Description: string;
    Source: string;
    Verified: Boolean;
    EcuAddress: Word;
    PayloadSize: Integer;
    Fields: TArray<TOBDCodingFieldEntry>;
  end;

  /// <summary>Adaptation-channel entry as loaded from JSON.</summary>
  TOBDAdaptationEntry = record
    Channel: Word;
    Name: string;
    Description: string;
    Source: string;
    Verified: Boolean;
    EcuAddress: Word;
    KindStr: string;
    MinValue: Int64;
    MaxValue: Int64;
    DefaultValue: Int64;
    Unit_: string;
    EnumValues: TArray<TPair<Integer, string>>;
  end;

  /// <summary>Actuator-test entry as loaded from JSON.</summary>
  TOBDActuatorTestEntry = record
    Identifier: Word;
    Name: string;
    Description: string;
    Source: string;
    Verified: Boolean;
    EcuAddress: Word;
    DurationMs: Cardinal;
    SafetyWarning: string;
    ExpectedResponseKind: string;
    ExpectedResponseLabel: string;
  end;

  /// <summary>Live-PID entry as loaded from JSON.</summary>
  TOBDLivePIDEntry = record
    Mode: string;
    PID: Word;
    Name: string;
    Description: string;
    Source: string;
    Verified: Boolean;
    EcuAddress: Word;
    FrameOffset: Integer;
    DecoderKindStr: string;
    Scale: Double;
    Offset: Double;
    Unit_: string;
  end;

  /// <summary>DTC extended-data record as loaded from JSON.</summary>
  TOBDDtcExtendedDataEntry = record
    DtcCode: string;
    RecordNumber: Byte;
    KindStr: string;
    Description: string;
    Source: string;
    Verified: Boolean;
    DecoderKindStr: string;
    Scale: Double;
    Offset: Double;
    Unit_: string;
  end;

  /// <summary>
  ///   A complete catalogue as loaded from one JSON file. Owned
  ///   by the caller; entries can be freely walked.
  /// </summary>
  TOBDOEMJSONCatalog = class
  strict private
    FVersion: Integer;
    FManufacturerKey: string;
    FDisplayName: string;
    FApplicableWMIs: TArray<string>;
    FDefaultSource: string;
    FDefaultEcuAddress: Word;
    FDIDs: TList<TOBDOEMDIDEntry>;
    FRoutines: TList<TOBDOEMRoutineEntry>;
    FDtcRanges: TList<TOBDOEMDtcRange>;
    FECUs: TList<TOBDOEMECUEntry>;
    FCodingBlocks: TList<TOBDCodingBlockEntry>;
    FAdaptations: TList<TOBDAdaptationEntry>;
    FActuatorTests: TList<TOBDActuatorTestEntry>;
    FLivePIDs: TList<TOBDLivePIDEntry>;
    FDtcExtended: TList<TOBDDtcExtendedDataEntry>;
    function ParseHexOrInt(const S: string): Cardinal;
    function ParseDecoder(Obj: TJSONObject): TOBDDecoderSpec;
    function ParseEnumValues(Obj: TJSONObject;
      const Key: string): TArray<TPair<Integer, string>>;
    procedure LoadFromJSON(Root: TJSONObject);
    procedure LoadDIDs(Arr: TJSONArray);
    procedure LoadRoutines(Arr: TJSONArray);
    procedure LoadDtcRanges(Arr: TJSONArray);
    procedure LoadECUs(Arr: TJSONArray);
    procedure LoadCodingBlocks(Arr: TJSONArray);
    procedure LoadAdaptations(Arr: TJSONArray);
    procedure LoadActuatorTests(Arr: TJSONArray);
    procedure LoadLivePIDs(Arr: TJSONArray);
    procedure LoadDtcExtended(Arr: TJSONArray);
  public
    /// <summary>Loads a catalogue from disk.</summary>
    /// <param name="FilePath">Absolute path to the JSON
    /// file.</param>
    /// <exception cref="EOBDCatalogError">File missing or
    /// malformed.</exception>
    constructor Create(const FilePath: string); overload;
    /// <summary>Loads a catalogue from in-memory JSON text.</summary>
    /// <param name="JsonText">JSON object source.</param>
    constructor CreateFromText(const JsonText: string); overload;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>Catalogue DIDs cast to
    /// <see cref="TOBDOEMDataIdentifier"/>.</summary>
    function AsBaseDIDs: TArray<TOBDOEMDataIdentifier>;
    /// <summary>Catalogue routines cast to
    /// <see cref="TOBDOEMRoutine"/>.</summary>
    function AsBaseRoutines: TArray<TOBDOEMRoutine>;
    /// <summary>Catalogue ECUs cast to
    /// <see cref="TOBDOEMECU"/>.</summary>
    function AsBaseECUs: TArray<TOBDOEMECU>;

    /// <summary>
    ///   Applies the decoder for <c>DID</c> (if any) to
    ///   <c>Payload</c>. Returns the formatted string, or empty
    ///   when no decoder is bound or the payload is too short.
    /// </summary>
    /// <param name="DID">DID being decoded.</param>
    /// <param name="Payload">Response bytes.</param>
    function DecodePayload(const DID: Word;
      const Payload: TBytes): string;

    /// <summary>Looks up a DID.</summary>
    /// <param name="DID">DID value.</param>
    /// <param name="Entry">Out: matching entry on success.</param>
    function FindDID(const DID: Word;
      out Entry: TOBDOEMDIDEntry): Boolean;

    /// <summary>Schema version.</summary>
    property Version: Integer read FVersion;
    /// <summary>Manufacturer key from the catalogue header.</summary>
    property ManufacturerKey: string read FManufacturerKey;
    /// <summary>Display name from the catalogue header.</summary>
    property DisplayName: string read FDisplayName;
    /// <summary>WMIs this catalogue claims.</summary>
    property ApplicableWMIs: TArray<string> read FApplicableWMIs;
    /// <summary>Default <c>source</c> applied to entries that omit
    /// it.</summary>
    property DefaultSource: string read FDefaultSource;
    /// <summary>Default ECU address applied to entries that omit
    /// it.</summary>
    property DefaultEcuAddress: Word read FDefaultEcuAddress;

    function DIDCount: Integer;
    function DID(Index: Integer): TOBDOEMDIDEntry;
    function RoutineCount: Integer;
    function Routine(Index: Integer): TOBDOEMRoutineEntry;
    function DtcRangeCount: Integer;
    function DtcRange(Index: Integer): TOBDOEMDtcRange;
    function ECUCount: Integer;
    function ECU(Index: Integer): TOBDOEMECUEntry;
    function CodingBlockCount: Integer;
    function CodingBlock(Index: Integer): TOBDCodingBlockEntry;
    function AdaptationCount: Integer;
    function Adaptation(Index: Integer): TOBDAdaptationEntry;
    function ActuatorTestCount: Integer;
    function ActuatorTest(Index: Integer): TOBDActuatorTestEntry;
    function LivePIDCount: Integer;
    function LivePID(Index: Integer): TOBDLivePIDEntry;
    function DtcExtendedCount: Integer;
    function DtcExtended(Index: Integer): TOBDDtcExtendedDataEntry;
  end;

/// <summary>Maps the JSON decoder-kind string to
/// <see cref="TOBDOEMDecoderKind"/>.</summary>
/// <param name="S">JSON tag.</param>
function ParseOEMDecoderKind(const S: string): TOBDOEMDecoderKind;
/// <summary>Maps the JSON coding-field-kind string to
/// <see cref="TOBDCodingFieldKind"/>.</summary>
/// <param name="S">JSON tag.</param>
function ParseCodingFieldKind(const S: string): TOBDCodingFieldKind;
/// <summary>Maps the JSON adaptation-kind string to
/// <see cref="TOBDAdaptationKind"/>.</summary>
/// <param name="S">JSON tag.</param>
function ParseAdaptationKind(const S: string): TOBDAdaptationKind;
/// <summary>Maps the JSON actuator-response-kind string to
/// <see cref="TOBDActuatorResponseKind"/>.</summary>
/// <param name="S">JSON tag.</param>
function ParseActuatorResponseKind(
  const S: string): TOBDActuatorResponseKind;
/// <summary>Maps the JSON live-PID mode string to
/// <see cref="TOBDLivePIDMode"/>.</summary>
/// <param name="S">JSON tag.</param>
function ParseLivePIDMode(const S: string): TOBDLivePIDMode;
/// <summary>Maps the JSON DTC-extended-data-kind string to
/// <see cref="TOBDDtcExtendedDataKind"/>.</summary>
/// <param name="S">JSON tag.</param>
function ParseDtcExtendedKind(
  const S: string): TOBDDtcExtendedDataKind;

implementation

uses
  System.Math,
  System.NetEncoding;

constructor TOBDOEMJSONCatalog.Create(const FilePath: string);
var
  Text: string;
begin
  if not TFile.Exists(FilePath) then
    raise EOBDCatalogError.CreateFmt(
      'Catalog file %s not found', [FilePath]);
  Text := TFile.ReadAllText(FilePath, TEncoding.UTF8);
  CreateFromText(Text);
end;

constructor TOBDOEMJSONCatalog.CreateFromText(const JsonText: string);
var
  Value: TJSONValue;
begin
  inherited Create;
  FDIDs := TList<TOBDOEMDIDEntry>.Create;
  FRoutines := TList<TOBDOEMRoutineEntry>.Create;
  FDtcRanges := TList<TOBDOEMDtcRange>.Create;
  FECUs := TList<TOBDOEMECUEntry>.Create;
  FCodingBlocks := TList<TOBDCodingBlockEntry>.Create;
  FAdaptations := TList<TOBDAdaptationEntry>.Create;
  FActuatorTests := TList<TOBDActuatorTestEntry>.Create;
  FLivePIDs := TList<TOBDLivePIDEntry>.Create;
  FDtcExtended := TList<TOBDDtcExtendedDataEntry>.Create;

  Value := TJSONObject.ParseJSONValue(JsonText);
  if not (Value is TJSONObject) then
  begin
    Value.Free;
    raise EOBDCatalogError.Create('Catalog root must be a JSON object');
  end;
  try
    LoadFromJSON(TJSONObject(Value));
  finally
    Value.Free;
  end;
end;

destructor TOBDOEMJSONCatalog.Destroy;
var
  I: Integer;
begin
  for I := 0 to FDIDs.Count - 1 do
  begin
    if Assigned(FDIDs[I].Decoder.EnumValues) then
      FDIDs[I].Decoder.EnumValues.Free;
    if Assigned(FDIDs[I].Decoder.BitNames) then
      FDIDs[I].Decoder.BitNames.Free;
  end;
  FDIDs.Free;
  FRoutines.Free;
  FDtcRanges.Free;
  FECUs.Free;
  FCodingBlocks.Free;
  FAdaptations.Free;
  FActuatorTests.Free;
  FLivePIDs.Free;
  FDtcExtended.Free;
  inherited;
end;

function TOBDOEMJSONCatalog.ParseHexOrInt(const S: string): Cardinal;
var
  Trimmed: string;
begin
  Trimmed := Trim(S);
  if Trimmed.StartsWith('0x', True) or Trimmed.StartsWith('$') then
    Result := StrToInt('$' + Trimmed
      .Replace('0x', '', [rfIgnoreCase])
      .Replace('$', ''))
  else
    Result := StrToInt(Trimmed);
end;

function TOBDOEMJSONCatalog.ParseDecoder(
  Obj: TJSONObject): TOBDDecoderSpec;
const
  KindMap: array[0..10] of
    record Tag: string; Kind: TOBDDecoderKind end = (
    (Tag: 'ascii';     Kind: dkAscii),
    (Tag: 'hex';       Kind: dkHex),
    (Tag: 'uint8';     Kind: dkUInt8),
    (Tag: 'uint16_be'; Kind: dkUInt16BE),
    (Tag: 'uint32_be'; Kind: dkUInt32BE),
    (Tag: 'int16_be';  Kind: dkInt16BE),
    (Tag: 'int32_be';  Kind: dkInt32BE),
    (Tag: 'bcd_date';  Kind: dkBcdDate),
    (Tag: 'enum';      Kind: dkEnum),
    (Tag: 'bitmask';   Kind: dkBitmask),
    (Tag: 'seconds';   Kind: dkSeconds));
var
  KindStr: string;
  I: Integer;
  ValuesObj, BitsObj: TJSONObject;
  Pair: TJSONPair;
begin
  Result := Default(TOBDDecoderSpec);
  Result.Kind := dkUnknown;
  Result.Scale := 1.0;
  if Obj = nil then
    Exit;

  KindStr := Obj.GetValue<string>('kind', '');
  for I := Low(KindMap) to High(KindMap) do
    if KindMap[I].Tag = KindStr then
    begin
      Result.Kind := KindMap[I].Kind;
      Break;
    end;

  Result.Size := Obj.GetValue<Integer>('size', 0);
  Result.Scale := Obj.GetValue<Double>('scale', 1.0);
  Result.Offset := Obj.GetValue<Double>('offset', 0.0);
  Result.Unit_ := Obj.GetValue<string>('unit', '');

  if (Result.Kind = dkEnum) and Assigned(Obj.GetValue('values')) then
  begin
    Result.EnumValues := TDictionary<Cardinal, string>.Create;
    ValuesObj := Obj.GetValue<TJSONObject>('values');
    if Assigned(ValuesObj) then
      for Pair in ValuesObj do
        Result.EnumValues.AddOrSetValue(
          ParseHexOrInt(Pair.JsonString.Value),
          (Pair.JsonValue as TJSONString).Value);
  end;

  if (Result.Kind = dkBitmask) and Assigned(Obj.GetValue('bits')) then
  begin
    Result.BitNames := TDictionary<Integer, string>.Create;
    BitsObj := Obj.GetValue<TJSONObject>('bits');
    if Assigned(BitsObj) then
      for Pair in BitsObj do
        Result.BitNames.AddOrSetValue(
          StrToInt(Pair.JsonString.Value),
          (Pair.JsonValue as TJSONString).Value);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadFromJSON(Root: TJSONObject);
var
  WMIArr: TJSONArray;
  I: Integer;
  Arr: TJSONArray;
begin
  FVersion := Root.GetValue<Integer>('version', 1);
  FManufacturerKey := Root.GetValue<string>('manufacturer_key', '');
  FDisplayName := Root.GetValue<string>('display_name', '');
  FDefaultSource := Root.GetValue<string>('default_source', '');
  FDefaultEcuAddress := 0;
  if Assigned(Root.GetValue('default_ecu_address')) then
    FDefaultEcuAddress := ParseHexOrInt(
      Root.GetValue<string>('default_ecu_address', '0'));

  WMIArr := Root.GetValue<TJSONArray>('applicable_wmis');
  if Assigned(WMIArr) then
  begin
    SetLength(FApplicableWMIs, WMIArr.Count);
    for I := 0 to WMIArr.Count - 1 do
      FApplicableWMIs[I] := WMIArr.Items[I].Value;
  end;

  Arr := Root.GetValue<TJSONArray>('ecus');
  if Assigned(Arr) then
    LoadECUs(Arr);

  Arr := Root.GetValue<TJSONArray>('dids');
  if Assigned(Arr) then
    LoadDIDs(Arr);

  Arr := Root.GetValue<TJSONArray>('routines');
  if Assigned(Arr) then
    LoadRoutines(Arr);

  Arr := Root.GetValue<TJSONArray>('dtc_ranges');
  if Assigned(Arr) then
    LoadDtcRanges(Arr);

  Arr := Root.GetValue<TJSONArray>('coding_blocks');
  if Assigned(Arr) then
    LoadCodingBlocks(Arr);

  Arr := Root.GetValue<TJSONArray>('adaptations');
  if Assigned(Arr) then
    LoadAdaptations(Arr);

  Arr := Root.GetValue<TJSONArray>('actuator_tests');
  if Assigned(Arr) then
    LoadActuatorTests(Arr);

  Arr := Root.GetValue<TJSONArray>('live_pids');
  if Assigned(Arr) then
    LoadLivePIDs(Arr);

  Arr := Root.GetValue<TJSONArray>('dtc_extended_data');
  if Assigned(Arr) then
    LoadDtcExtended(Arr);
end;

procedure TOBDOEMJSONCatalog.LoadDIDs(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMDIDEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDOEMDIDEntry);
    Entry.DID := ParseHexOrInt(Item.GetValue<string>('did', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    EcuStr := Item.GetValue<string>('ecu_address', '');
    if EcuStr <> '' then
      Entry.EcuAddress := ParseHexOrInt(EcuStr)
    else
      Entry.EcuAddress := FDefaultEcuAddress;
    Entry.Decoder := ParseDecoder(Item.GetValue<TJSONObject>('decoder'));
    FDIDs.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadRoutines(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMRoutineEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDOEMRoutineEntry);
    Entry.Identifier := ParseHexOrInt(Item.GetValue<string>('id', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    EcuStr := Item.GetValue<string>('ecu_address', '');
    if EcuStr <> '' then
      Entry.EcuAddress := ParseHexOrInt(EcuStr)
    else
      Entry.EcuAddress := FDefaultEcuAddress;
    FRoutines.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadECUs(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMECUEntry;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDOEMECUEntry);
    Entry.Address := ParseHexOrInt(Item.GetValue<string>('address', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.CommonName := Item.GetValue<string>('common_name', Entry.Name);
    FECUs.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadDtcRanges(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMDtcRange;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDOEMDtcRange);
    Entry.StartCode := Item.GetValue<string>('start', '');
    Entry.EndCode := Item.GetValue<string>('end', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    FDtcRanges.Add(Entry);
  end;
end;

function ParseOEMDecoderKind(const S: string): TOBDOEMDecoderKind;
const
  Map: array[0..10] of
    record Tag: string; Kind: TOBDOEMDecoderKind end = (
    (Tag: 'ascii';     Kind: TOBDOEMDecoderKind.dkAscii),
    (Tag: 'hex';       Kind: TOBDOEMDecoderKind.dkHex),
    (Tag: 'uint8';     Kind: TOBDOEMDecoderKind.dkUInt8),
    (Tag: 'uint16_be'; Kind: TOBDOEMDecoderKind.dkUInt16BE),
    (Tag: 'uint32_be'; Kind: TOBDOEMDecoderKind.dkUInt32BE),
    (Tag: 'int16_be';  Kind: TOBDOEMDecoderKind.dkInt16BE),
    (Tag: 'int32_be';  Kind: TOBDOEMDecoderKind.dkInt32BE),
    (Tag: 'bcd_date';  Kind: TOBDOEMDecoderKind.dkBcdDate),
    (Tag: 'enum';      Kind: TOBDOEMDecoderKind.dkEnum),
    (Tag: 'bitmask';   Kind: TOBDOEMDecoderKind.dkBitmask),
    (Tag: 'seconds';   Kind: TOBDOEMDecoderKind.dkSeconds));
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then
      Exit(Map[I].Kind);
  Result := TOBDOEMDecoderKind.dkUnknown;
end;

function ParseCodingFieldKind(const S: string): TOBDCodingFieldKind;
const
  Map: array[0..8] of
    record Tag: string; Kind: TOBDCodingFieldKind end = (
    (Tag: 'bit';       Kind: cfkBit),
    (Tag: 'uint8';     Kind: cfkUInt8),
    (Tag: 'uint16_be'; Kind: cfkUInt16BE),
    (Tag: 'uint32_be'; Kind: cfkUInt32BE),
    (Tag: 'int16_be';  Kind: cfkInt16BE),
    (Tag: 'int32_be';  Kind: cfkInt32BE),
    (Tag: 'ascii';     Kind: cfkAscii),
    (Tag: 'enum';      Kind: cfkEnum),
    (Tag: 'bitmask';   Kind: cfkBitmask));
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then
      Exit(Map[I].Kind);
  Result := cfkUnknown;
end;

function ParseAdaptationKind(const S: string): TOBDAdaptationKind;
const
  Map: array[0..5] of
    record Tag: string; Kind: TOBDAdaptationKind end = (
    (Tag: 'uint8';     Kind: adkUInt8),
    (Tag: 'uint16_be'; Kind: adkUInt16BE),
    (Tag: 'uint32_be'; Kind: adkUInt32BE),
    (Tag: 'int16_be';  Kind: adkInt16BE),
    (Tag: 'int32_be';  Kind: adkInt32BE),
    (Tag: 'enum';      Kind: adkEnum));
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then
      Exit(Map[I].Kind);
  Result := adkUnknown;
end;

function ParseActuatorResponseKind(
  const S: string): TOBDActuatorResponseKind;
var
  Lower: string;
begin
  Lower := LowerCase(S);
  if Lower = 'boolean'   then Exit(arkBoolean);
  if Lower = 'uint8'     then Exit(arkUInt8);
  if Lower = 'uint16_be' then Exit(arkUInt16BE);
  if Lower = 'ascii'     then Exit(arkAscii);
  Result := arkNone;
end;

function ParseLivePIDMode(const S: string): TOBDLivePIDMode;
var
  Lower: string;
begin
  Lower := LowerCase(S);
  if Lower = 'service01' then Exit(lpmService01);
  if Lower = 'service22' then Exit(lpmService22);
  Result := lpmUnknown;
end;

function ParseDtcExtendedKind(
  const S: string): TOBDDtcExtendedDataKind;
const
  Map: array[0..5] of
    record Tag: string; Kind: TOBDDtcExtendedDataKind end = (
    (Tag: 'occurrence_counter';    Kind: xdkOccurrenceCounter),
    (Tag: 'aging_counter';         Kind: xdkAgingCounter),
    (Tag: 'miles_since_cleared';   Kind: xdkMilesSinceCleared),
    (Tag: 'freeze_frame_template'; Kind: xdkFreezeFrameTemplate),
    (Tag: 'oem_status_byte';       Kind: xdkOemStatusByte),
    (Tag: 'environmental_data';    Kind: xdkEnvironmentalData));
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then
      Exit(Map[I].Kind);
  Result := xdkUnknown;
end;

function TOBDOEMJSONCatalog.ParseEnumValues(Obj: TJSONObject;
  const Key: string): TArray<TPair<Integer, string>>;
var
  ValuesObj: TJSONObject;
  Pair: TJSONPair;
  Acc: TList<TPair<Integer, string>>;
begin
  Result := nil;
  if Obj = nil then
    Exit;
  ValuesObj := Obj.GetValue<TJSONObject>(Key);
  if ValuesObj = nil then
    Exit;
  Acc := TList<TPair<Integer, string>>.Create;
  try
    for Pair in ValuesObj do
      Acc.Add(TPair<Integer, string>.Create(
        Integer(ParseHexOrInt(Pair.JsonString.Value)),
        (Pair.JsonValue as TJSONString).Value));
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

procedure TOBDOEMJSONCatalog.LoadCodingBlocks(Arr: TJSONArray);
var
  I, J: Integer;
  Item, FieldItem: TJSONObject;
  FieldsArr: TJSONArray;
  Entry: TOBDCodingBlockEntry;
  Field: TOBDCodingFieldEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDCodingBlockEntry);
    Entry.DataIdentifier := ParseHexOrInt(Item.GetValue<string>('did', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    EcuStr := Item.GetValue<string>('ecu_address', '');
    if EcuStr <> '' then
      Entry.EcuAddress := ParseHexOrInt(EcuStr)
    else
      Entry.EcuAddress := FDefaultEcuAddress;
    Entry.PayloadSize := Item.GetValue<Integer>('payload_size', 0);

    FieldsArr := Item.GetValue<TJSONArray>('fields');
    if Assigned(FieldsArr) then
    begin
      SetLength(Entry.Fields, FieldsArr.Count);
      for J := 0 to FieldsArr.Count - 1 do
      begin
        if not (FieldsArr.Items[J] is TJSONObject) then
          Continue;
        FieldItem := TJSONObject(FieldsArr.Items[J]);
        Field := Default(TOBDCodingFieldEntry);
        Field.Name := FieldItem.GetValue<string>('name', '');
        Field.Label_ := FieldItem.GetValue<string>('label', Field.Name);
        Field.Description := FieldItem.GetValue<string>('description', '');
        Field.KindStr := FieldItem.GetValue<string>('kind', '');
        Field.ByteOffset := FieldItem.GetValue<Integer>('byte_offset', 0);
        Field.BitOffset := FieldItem.GetValue<Integer>('bit_offset', 0);
        Field.BitWidth := FieldItem.GetValue<Integer>('bit_width', 0);
        Field.DefaultValue := FieldItem.GetValue<Int64>('default', 0);
        Field.DefaultAscii := FieldItem.GetValue<string>('default_ascii', '');
        Field.MinValue := FieldItem.GetValue<Int64>('min', Low(Int64));
        Field.MaxValue := FieldItem.GetValue<Int64>('max', High(Int64));
        Field.EnumValues := ParseEnumValues(FieldItem, 'values');
        Entry.Fields[J] := Field;
      end;
    end;

    FCodingBlocks.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadAdaptations(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDAdaptationEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDAdaptationEntry);
    Entry.Channel := ParseHexOrInt(Item.GetValue<string>('channel', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    EcuStr := Item.GetValue<string>('ecu_address', '');
    if EcuStr <> '' then
      Entry.EcuAddress := ParseHexOrInt(EcuStr)
    else
      Entry.EcuAddress := FDefaultEcuAddress;
    Entry.KindStr := Item.GetValue<string>('kind', '');
    Entry.MinValue := Item.GetValue<Int64>('min', Low(Int64));
    Entry.MaxValue := Item.GetValue<Int64>('max', High(Int64));
    Entry.DefaultValue := Item.GetValue<Int64>('default', 0);
    Entry.Unit_ := Item.GetValue<string>('unit', '');
    Entry.EnumValues := ParseEnumValues(Item, 'values');
    FAdaptations.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadActuatorTests(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDActuatorTestEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDActuatorTestEntry);
    Entry.Identifier := ParseHexOrInt(Item.GetValue<string>('id', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    EcuStr := Item.GetValue<string>('ecu_address', '');
    if EcuStr <> '' then
      Entry.EcuAddress := ParseHexOrInt(EcuStr)
    else
      Entry.EcuAddress := FDefaultEcuAddress;
    Entry.DurationMs := Item.GetValue<Cardinal>('duration_ms', 0);
    Entry.SafetyWarning := Item.GetValue<string>('safety_warning', '');
    Entry.ExpectedResponseKind :=
      Item.GetValue<string>('response_kind', '');
    Entry.ExpectedResponseLabel :=
      Item.GetValue<string>('response_label', '');
    FActuatorTests.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadLivePIDs(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDLivePIDEntry;
  EcuStr: string;
  Decoder: TJSONObject;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDLivePIDEntry);
    Entry.Mode := Item.GetValue<string>('mode', 'service22');
    Entry.PID := ParseHexOrInt(Item.GetValue<string>('pid', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    EcuStr := Item.GetValue<string>('ecu_address', '');
    if EcuStr <> '' then
      Entry.EcuAddress := ParseHexOrInt(EcuStr)
    else
      Entry.EcuAddress := FDefaultEcuAddress;
    Entry.FrameOffset := Item.GetValue<Integer>('frame_offset', 0);
    Decoder := Item.GetValue<TJSONObject>('decoder');
    if Assigned(Decoder) then
    begin
      Entry.DecoderKindStr := Decoder.GetValue<string>('kind', '');
      Entry.Scale := Decoder.GetValue<Double>('scale', 1.0);
      Entry.Offset := Decoder.GetValue<Double>('offset', 0.0);
      Entry.Unit_ := Decoder.GetValue<string>('unit', '');
    end
    else
      Entry.Scale := 1.0;
    FLivePIDs.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadDtcExtended(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDDtcExtendedDataEntry;
  Decoder: TJSONObject;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then
      Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDDtcExtendedDataEntry);
    Entry.DtcCode := Item.GetValue<string>('code', '');
    Entry.RecordNumber := Byte(ParseHexOrInt(
      Item.GetValue<string>('record', '0')));
    Entry.KindStr := Item.GetValue<string>('kind', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    Decoder := Item.GetValue<TJSONObject>('decoder');
    if Assigned(Decoder) then
    begin
      Entry.DecoderKindStr := Decoder.GetValue<string>('kind', '');
      Entry.Scale := Decoder.GetValue<Double>('scale', 1.0);
      Entry.Offset := Decoder.GetValue<Double>('offset', 0.0);
      Entry.Unit_ := Decoder.GetValue<string>('unit', '');
    end
    else
      Entry.Scale := 1.0;
    FDtcExtended.Add(Entry);
  end;
end;

function TOBDOEMJSONCatalog.AsBaseDIDs: TArray<TOBDOEMDataIdentifier>;
var
  I: Integer;
  Out_: TOBDOEMDataIdentifier;
begin
  SetLength(Result, FDIDs.Count);
  for I := 0 to FDIDs.Count - 1 do
  begin
    Out_ := Default(TOBDOEMDataIdentifier);
    Out_.DID := FDIDs[I].DID;
    Out_.Name := FDIDs[I].Name;
    Out_.Description := FDIDs[I].Description;
    Out_.EcuAddress := FDIDs[I].EcuAddress;
    Result[I] := Out_;
  end;
end;

function TOBDOEMJSONCatalog.AsBaseRoutines: TArray<TOBDOEMRoutine>;
var
  I: Integer;
  Out_: TOBDOEMRoutine;
begin
  SetLength(Result, FRoutines.Count);
  for I := 0 to FRoutines.Count - 1 do
  begin
    Out_ := Default(TOBDOEMRoutine);
    Out_.Identifier := FRoutines[I].Identifier;
    Out_.Name := FRoutines[I].Name;
    Out_.Description := FRoutines[I].Description;
    Out_.EcuAddress := FRoutines[I].EcuAddress;
    Result[I] := Out_;
  end;
end;

function TOBDOEMJSONCatalog.AsBaseECUs: TArray<TOBDOEMECU>;
var
  I: Integer;
begin
  SetLength(Result, FECUs.Count);
  for I := 0 to FECUs.Count - 1 do
    Result[I] := MakeOEMECU(FECUs[I].Address, FECUs[I].Name,
      FECUs[I].CommonName);
end;

function TOBDOEMJSONCatalog.FindDID(const DID: Word;
  out Entry: TOBDOEMDIDEntry): Boolean;
var
  I: Integer;
begin
  for I := 0 to FDIDs.Count - 1 do
    if FDIDs[I].DID = DID then
    begin
      Entry := FDIDs[I];
      Exit(True);
    end;
  Result := False;
end;

function ReadUInt(const Payload: TBytes; Size: Integer): UInt64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Size - 1 do
    Result := (Result shl 8) or Payload[I];
end;

function ReadInt(const Payload: TBytes; Size: Integer): Int64;
var
  Mask: UInt64;
  U: UInt64;
begin
  U := ReadUInt(Payload, Size);
  Mask := UInt64(1) shl ((Size * 8) - 1);
  if (U and Mask) <> 0 then
    Result := Int64(U) - Int64(UInt64(1) shl (Size * 8))
  else
    Result := Int64(U);
end;

function FormatNumeric(Value: Double; const Spec: TOBDDecoderSpec): string;
var
  Combined: Double;
begin
  Combined := (Value * Spec.Scale) + Spec.Offset;
  if Spec.Unit_ <> '' then
    Result := Format('%.6g %s', [Combined, Spec.Unit_])
  else
    Result := Format('%.6g', [Combined]);
end;

function HexFallback(const Payload: TBytes): string;
var
  I: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    for I := 0 to High(Payload) do
    begin
      if I > 0 then
        Builder.Append(' ');
      Builder.Append(IntToHex(Payload[I], 2));
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TOBDOEMJSONCatalog.DecodePayload(const DID: Word;
  const Payload: TBytes): string;
var
  Entry: TOBDOEMDIDEntry;
  Spec: TOBDDecoderSpec;
  N: UInt64;
  S: Int64;
  Lookup: string;
  Bit, BitCount: Integer;
  Names: TArray<string>;
  Nm: string;
begin
  Result := '';
  if not FindDID(DID, Entry) then
    Exit;
  Spec := Entry.Decoder;

  case Spec.Kind of
    dkAscii:
      Result := TEncoding.ASCII.GetString(Payload);
    dkHex, dkUnknown:
      Result := HexFallback(Payload);
    dkUInt8:
      if Length(Payload) >= 1 then
        Result := FormatNumeric(Payload[0], Spec);
    dkUInt16BE:
      if Length(Payload) >= 2 then
        Result := FormatNumeric(ReadUInt(Payload, 2), Spec);
    dkUInt32BE:
      if Length(Payload) >= 4 then
        Result := FormatNumeric(ReadUInt(Payload, 4), Spec);
    dkInt16BE:
      if Length(Payload) >= 2 then
        Result := FormatNumeric(ReadInt(Payload, 2), Spec);
    dkInt32BE:
      if Length(Payload) >= 4 then
        Result := FormatNumeric(ReadInt(Payload, 4), Spec);
    dkBcdDate:
      if Length(Payload) >= 3 then
        Result := Format('20%.2x-%.2x-%.2x',
          [Payload[0], Payload[1], Payload[2]]);
    dkSeconds:
      if Length(Payload) >= 4 then
      begin
        S := Int64(ReadUInt(Payload, 4));
        Result := Format('%d s (%.2f h)', [S, S / 3600.0]);
      end;
    dkEnum:
      if Spec.Size > 0 then
      begin
        if Length(Payload) < Spec.Size then
          Exit;
        N := ReadUInt(Payload, Spec.Size);
        if Assigned(Spec.EnumValues) and
           Spec.EnumValues.TryGetValue(N, Lookup) then
          Result := Lookup
        else
          Result := Format('0x%.*x', [Spec.Size * 2, N]);
      end;
    dkBitmask:
      if Spec.Size > 0 then
      begin
        if Length(Payload) < Spec.Size then
          Exit;
        N := ReadUInt(Payload, Spec.Size);
        SetLength(Names, 0);
        BitCount := Spec.Size * 8;
        for Bit := 0 to BitCount - 1 do
          if (N and (UInt64(1) shl Bit)) <> 0 then
          begin
            Nm := '';
            if Assigned(Spec.BitNames) then
              Spec.BitNames.TryGetValue(Bit, Nm);
            if Nm = '' then
              Nm := Format('bit_%d', [Bit]);
            Names := Names + [Nm];
          end;
        Result := string.Join(',', Names);
      end;
  end;
end;

function TOBDOEMJSONCatalog.DIDCount: Integer;
begin
  Result := FDIDs.Count;
end;

function TOBDOEMJSONCatalog.DID(Index: Integer): TOBDOEMDIDEntry;
begin
  Result := FDIDs[Index];
end;

function TOBDOEMJSONCatalog.RoutineCount: Integer;
begin
  Result := FRoutines.Count;
end;

function TOBDOEMJSONCatalog.Routine(Index: Integer): TOBDOEMRoutineEntry;
begin
  Result := FRoutines[Index];
end;

function TOBDOEMJSONCatalog.DtcRangeCount: Integer;
begin
  Result := FDtcRanges.Count;
end;

function TOBDOEMJSONCatalog.DtcRange(Index: Integer): TOBDOEMDtcRange;
begin
  Result := FDtcRanges[Index];
end;

function TOBDOEMJSONCatalog.ECUCount: Integer;
begin
  Result := FECUs.Count;
end;

function TOBDOEMJSONCatalog.ECU(Index: Integer): TOBDOEMECUEntry;
begin
  Result := FECUs[Index];
end;

function TOBDOEMJSONCatalog.CodingBlockCount: Integer;
begin
  Result := FCodingBlocks.Count;
end;

function TOBDOEMJSONCatalog.CodingBlock(
  Index: Integer): TOBDCodingBlockEntry;
begin
  Result := FCodingBlocks[Index];
end;

function TOBDOEMJSONCatalog.AdaptationCount: Integer;
begin
  Result := FAdaptations.Count;
end;

function TOBDOEMJSONCatalog.Adaptation(
  Index: Integer): TOBDAdaptationEntry;
begin
  Result := FAdaptations[Index];
end;

function TOBDOEMJSONCatalog.ActuatorTestCount: Integer;
begin
  Result := FActuatorTests.Count;
end;

function TOBDOEMJSONCatalog.ActuatorTest(
  Index: Integer): TOBDActuatorTestEntry;
begin
  Result := FActuatorTests[Index];
end;

function TOBDOEMJSONCatalog.LivePIDCount: Integer;
begin
  Result := FLivePIDs.Count;
end;

function TOBDOEMJSONCatalog.LivePID(Index: Integer): TOBDLivePIDEntry;
begin
  Result := FLivePIDs[Index];
end;

function TOBDOEMJSONCatalog.DtcExtendedCount: Integer;
begin
  Result := FDtcExtended.Count;
end;

function TOBDOEMJSONCatalog.DtcExtended(
  Index: Integer): TOBDDtcExtendedDataEntry;
begin
  Result := FDtcExtended[Index];
end;

end.
