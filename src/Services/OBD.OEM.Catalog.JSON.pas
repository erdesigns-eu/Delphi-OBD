//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Catalog.JSON.pas
// CONTENTS       : JSON-driven OEM catalog loader (DIDs + routines +
//                  decoders) with provenance / verification metadata
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Catalog format documented in docs/CATALOG_FORMAT.md.
//                  Each entry carries `source` + `verified` so callers
//                  can distinguish OEM-spec-confirmed entries from
//                  community / starter data.
//------------------------------------------------------------------------------
unit OBD.OEM.Catalog.JSON;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.Generics.Collections, System.Generics.Defaults,
  OBD.OEM;

type
  EOBDCatalogError = class(Exception);

  /// <summary>
  ///   Decoder kinds the JSON catalog can describe. Pascal-side
  ///   payload decoding is in <c>DecodePayload</c>.
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
    dkSeconds
  );

  /// <summary>
  ///   Compiled form of a `decoder` sub-object. `enum` and `bitmask`
  ///   keep their lookup map ready for fast decode.
  /// </summary>
  TOBDDecoderSpec = record
    Kind: TOBDDecoderKind;
    Size: Integer;          // for fixed-width enum / bitmask
    Scale: Double;          // numeric scale factor (default 1.0)
    Offset: Double;         // numeric offset (default 0.0)
    Unit_: string;          // suffix appended to numeric output
    EnumValues: TDictionary<Cardinal, string>;
    BitNames: TDictionary<Integer, string>;
  end;

  /// <summary>
  ///   DID entry as loaded from JSON. Carries the verification +
  ///   provenance flags so callers can filter unverified entries out
  ///   of production-critical paths.
  /// </summary>
  TOBDOEMDIDEntry = record
    DID: Word;
    Name: string;
    Description: string;
    Source: string;
    Verified: Boolean;
    EcuAddress: Word;       // 0 = not scoped
    Decoder: TOBDDecoderSpec;
  end;

  TOBDOEMRoutineEntry = record
    Identifier: Word;
    Name: string;
    Description: string;
    Source: string;
    Verified: Boolean;
    EcuAddress: Word;       // 0 = not scoped
  end;

  TOBDOEMECUEntry = record
    Address: Word;
    Name: string;
    CommonName: string;
  end;

  TOBDOEMDtcRange = record
    StartCode: string;
    EndCode: string;
    Source: string;
  end;

  /// <summary>
  ///   v3.29 — extended-catalog parser entries. These mirror
  ///   the public records in <c>OBD.OEM</c> but carry the same source +
  ///   verified provenance flags as the legacy DID / routine entries.
  /// </summary>
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

  TOBDLivePIDEntry = record
    Mode: string;             // "service01" | "service22"
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
  ///   A complete catalog as loaded from one JSON file. Owned by the
  ///   caller; the in-memory entries can be freely walked.
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
    constructor Create(const FilePath: string); overload;
    constructor CreateFromText(const JsonText: string); overload;
    destructor Destroy; override;

    /// <summary>
    ///   Cast catalog DIDs to the <c>TOBDOEMDataIdentifier</c> shape
    ///   expected by <c>TOBDOEMExtensionBase</c>. Drops decoder/source.
    /// </summary>
    function AsBaseDIDs: TArray<TOBDOEMDataIdentifier>;
    /// <summary>
    ///   Cast catalog routines to the base shape.
    /// </summary>
    function AsBaseRoutines: TArray<TOBDOEMRoutine>;

    /// <summary>
    ///   Apply the decoder for <c>DID</c> (if any) to <c>Payload</c>.
    ///   Returns the formatted string, or empty if no decoder / payload too short.
    /// </summary>
    function DecodePayload(const DID: Word; const Payload: TBytes): string;

    /// <summary>
    ///   Find a DID; returns False if absent.
    /// </summary>
    function FindDID(const DID: Word; out Entry: TOBDOEMDIDEntry): Boolean;

    /// <summary>
    ///   ECUs declared by the catalog (top-level <c>ecus</c> array).
    /// </summary>
    function AsBaseECUs: TArray<TOBDOEMECU>;

    property Version: Integer read FVersion;
    property ManufacturerKey: string read FManufacturerKey;
    property DisplayName: string read FDisplayName;
    property ApplicableWMIs: TArray<string> read FApplicableWMIs;
    property DefaultSource: string read FDefaultSource;
    property DefaultEcuAddress: Word read FDefaultEcuAddress;
    function DIDCount: Integer;
    function DID(Index: Integer): TOBDOEMDIDEntry;
    function RoutineCount: Integer;
    function Routine(Index: Integer): TOBDOEMRoutineEntry;
    function DtcRangeCount: Integer;
    function DtcRange(Index: Integer): TOBDOEMDtcRange;
    function ECUCount: Integer;
    function ECU(Index: Integer): TOBDOEMECUEntry;

    /// <summary>
    ///   v3.29 — extended-catalog accessors.
    /// </summary>
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

/// <summary>
///   Convert the JSON-side decoder-kind string to the
///   <c>TOBDOEMDecoderKind</c> the public schema uses. Returns
///   <c>dkUnknown</c> for unrecognised strings.
/// </summary>
function ParseOEMDecoderKind(const S: string): TOBDOEMDecoderKind;
function ParseCodingFieldKind(const S: string): TOBDCodingFieldKind;
function ParseAdaptationKind(const S: string): TOBDAdaptationKind;
function ParseActuatorResponseKind(const S: string): TOBDActuatorResponseKind;
function ParseLivePIDMode(const S: string): TOBDLivePIDMode;
function ParseDtcExtendedKind(const S: string): TOBDDtcExtendedDataKind;

implementation

uses
  System.Math, System.NetEncoding;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDOEMJSONCatalog.Create(const FilePath: string);
var
  Text: string;
begin
  if not TFile.Exists(FilePath) then
    raise EOBDCatalogError.CreateFmt('Catalog file %s not found', [FilePath]);
  Text := TFile.ReadAllText(FilePath, TEncoding.UTF8);
  CreateFromText(Text);
end;

//------------------------------------------------------------------------------
// CREATE FROM TEXT
//------------------------------------------------------------------------------
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
    raise EOBDCatalogError.Create('Catalog root must be a JSON object');
  try
    LoadFromJSON(TJSONObject(Value));
  finally
    Value.Free;
  end;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDOEMJSONCatalog.Destroy;
var
  I: Integer;
begin
  // Decoder records own dictionaries — free per entry.
  for I := 0 to FDIDs.Count - 1 do
  begin
    if Assigned(FDIDs[I].Decoder.EnumValues) then FDIDs[I].Decoder.EnumValues.Free;
    if Assigned(FDIDs[I].Decoder.BitNames) then FDIDs[I].Decoder.BitNames.Free;
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

//------------------------------------------------------------------------------
// PARSE HEX OR INT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.ParseHexOrInt(const S: string): Cardinal;
var
  Trimmed: string;
begin
  Trimmed := Trim(S);
  if Trimmed.StartsWith('0x', True) or Trimmed.StartsWith('$') then
    Result := StrToInt('$' + Trimmed.Replace('0x', '', [rfIgnoreCase]).Replace('$', ''))
  else
    Result := StrToInt(Trimmed);
end;

//------------------------------------------------------------------------------
// PARSE DECODER
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.ParseDecoder(Obj: TJSONObject): TOBDDecoderSpec;
const
  KindMap: array[0..10] of record Tag: string; Kind: TOBDDecoderKind end = (
    (Tag: 'ascii'; Kind: dkAscii),
    (Tag: 'hex'; Kind: dkHex),
    (Tag: 'uint8'; Kind: dkUInt8),
    (Tag: 'uint16_be'; Kind: dkUInt16BE),
    (Tag: 'uint32_be'; Kind: dkUInt32BE),
    (Tag: 'int16_be'; Kind: dkInt16BE),
    (Tag: 'int32_be'; Kind: dkInt32BE),
    (Tag: 'bcd_date'; Kind: dkBcdDate),
    (Tag: 'enum'; Kind: dkEnum),
    (Tag: 'bitmask'; Kind: dkBitmask),
    (Tag: 'seconds'; Kind: dkSeconds)
  );
var
  KindStr: string;
  I: Integer;
  ValuesObj, BitsObj: TJSONObject;
  Pair: TJSONPair;
begin
  Result := Default(TOBDDecoderSpec);
  Result.Kind := dkUnknown;
  Result.Scale := 1.0;
  Result.Size := 0;
  if Obj = nil then Exit;

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

//------------------------------------------------------------------------------
// LOAD FROM JSON
//------------------------------------------------------------------------------
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
    FDefaultEcuAddress :=
      ParseHexOrInt(Root.GetValue<string>('default_ecu_address', '0'));

  WMIArr := Root.GetValue<TJSONArray>('applicable_wmis');
  if Assigned(WMIArr) then
  begin
    SetLength(FApplicableWMIs, WMIArr.Count);
    for I := 0 to WMIArr.Count - 1 do
      FApplicableWMIs[I] := WMIArr.Items[I].Value;
  end;

  // Load ECUs first so per-DID/routine entries can reference an
  // already-known address — not strictly required for parsing but
  // useful when validation lands later.
  Arr := Root.GetValue<TJSONArray>('ecus');
  if Assigned(Arr) then LoadECUs(Arr);

  Arr := Root.GetValue<TJSONArray>('dids');
  if Assigned(Arr) then LoadDIDs(Arr);

  Arr := Root.GetValue<TJSONArray>('routines');
  if Assigned(Arr) then LoadRoutines(Arr);

  Arr := Root.GetValue<TJSONArray>('dtc_ranges');
  if Assigned(Arr) then LoadDtcRanges(Arr);

  // v3.29 Phase A — extended catalog sections. All optional.
  Arr := Root.GetValue<TJSONArray>('coding_blocks');
  if Assigned(Arr) then LoadCodingBlocks(Arr);

  Arr := Root.GetValue<TJSONArray>('adaptations');
  if Assigned(Arr) then LoadAdaptations(Arr);

  Arr := Root.GetValue<TJSONArray>('actuator_tests');
  if Assigned(Arr) then LoadActuatorTests(Arr);

  Arr := Root.GetValue<TJSONArray>('live_pids');
  if Assigned(Arr) then LoadLivePIDs(Arr);

  Arr := Root.GetValue<TJSONArray>('dtc_extended_data');
  if Assigned(Arr) then LoadDtcExtended(Arr);
end;

//------------------------------------------------------------------------------
// LOAD DIDS
//------------------------------------------------------------------------------
procedure TOBDOEMJSONCatalog.LoadDIDs(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMDIDEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
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

//------------------------------------------------------------------------------
// LOAD ROUTINES
//------------------------------------------------------------------------------
procedure TOBDOEMJSONCatalog.LoadRoutines(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMRoutineEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
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

//------------------------------------------------------------------------------
// LOAD ECUS
//------------------------------------------------------------------------------
procedure TOBDOEMJSONCatalog.LoadECUs(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMECUEntry;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDOEMECUEntry);
    Entry.Address := ParseHexOrInt(Item.GetValue<string>('address', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.CommonName := Item.GetValue<string>('common_name', Entry.Name);
    FECUs.Add(Entry);
  end;
end;

//------------------------------------------------------------------------------
// LOAD DTC RANGES
//------------------------------------------------------------------------------
procedure TOBDOEMJSONCatalog.LoadDtcRanges(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMDtcRange;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDOEMDtcRange);
    Entry.StartCode := Item.GetValue<string>('start', '');
    Entry.EndCode := Item.GetValue<string>('end', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    FDtcRanges.Add(Entry);
  end;
end;

//------------------------------------------------------------------------------
// EXTENDED CATALOG — v3.29 Phase A
//------------------------------------------------------------------------------
function ParseOEMDecoderKind(const S: string): TOBDOEMDecoderKind;
const
  Map: array[0..10] of record Tag: string; Kind: TOBDOEMDecoderKind end = (
    (Tag: 'ascii'; Kind: dkAscii),
    (Tag: 'hex'; Kind: dkHex),
    (Tag: 'uint8'; Kind: dkUInt8),
    (Tag: 'uint16_be'; Kind: dkUInt16BE),
    (Tag: 'uint32_be'; Kind: dkUInt32BE),
    (Tag: 'int16_be'; Kind: dkInt16BE),
    (Tag: 'int32_be'; Kind: dkInt32BE),
    (Tag: 'bcd_date'; Kind: dkBcdDate),
    (Tag: 'enum'; Kind: dkEnum),
    (Tag: 'bitmask'; Kind: dkBitmask),
    (Tag: 'seconds'; Kind: dkSeconds)
  );
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then Exit(Map[I].Kind);
  Result := dkUnknown;
end;

//------------------------------------------------------------------------------
// PARSE CODING FIELD KIND
//------------------------------------------------------------------------------
function ParseCodingFieldKind(const S: string): TOBDCodingFieldKind;
const
  Map: array[0..8] of record Tag: string; Kind: TOBDCodingFieldKind end = (
    (Tag: 'bit'; Kind: cfkBit),
    (Tag: 'uint8'; Kind: cfkUInt8),
    (Tag: 'uint16_be'; Kind: cfkUInt16BE),
    (Tag: 'uint32_be'; Kind: cfkUInt32BE),
    (Tag: 'int16_be'; Kind: cfkInt16BE),
    (Tag: 'int32_be'; Kind: cfkInt32BE),
    (Tag: 'ascii'; Kind: cfkAscii),
    (Tag: 'enum'; Kind: cfkEnum),
    (Tag: 'bitmask'; Kind: cfkBitmask)
  );
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then Exit(Map[I].Kind);
  Result := cfkUnknown;
end;

//------------------------------------------------------------------------------
// PARSE ADAPTATION KIND
//------------------------------------------------------------------------------
function ParseAdaptationKind(const S: string): TOBDAdaptationKind;
const
  Map: array[0..5] of record Tag: string; Kind: TOBDAdaptationKind end = (
    (Tag: 'uint8'; Kind: adkUInt8),
    (Tag: 'uint16_be'; Kind: adkUInt16BE),
    (Tag: 'uint32_be'; Kind: adkUInt32BE),
    (Tag: 'int16_be'; Kind: adkInt16BE),
    (Tag: 'int32_be'; Kind: adkInt32BE),
    (Tag: 'enum'; Kind: adkEnum)
  );
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then Exit(Map[I].Kind);
  Result := adkUnknown;
end;

//------------------------------------------------------------------------------
// PARSE ACTUATOR RESPONSE KIND
//------------------------------------------------------------------------------
function ParseActuatorResponseKind(const S: string): TOBDActuatorResponseKind;
var
  Lower: string;
begin
  Lower := LowerCase(S);
  if Lower = 'boolean' then Exit(arkBoolean);
  if Lower = 'uint8' then Exit(arkUInt8);
  if Lower = 'uint16_be' then Exit(arkUInt16BE);
  if Lower = 'ascii' then Exit(arkAscii);
  Result := arkNone;
end;

//------------------------------------------------------------------------------
// PARSE LIVE PIDMODE
//------------------------------------------------------------------------------
function ParseLivePIDMode(const S: string): TOBDLivePIDMode;
var
  Lower: string;
begin
  Lower := LowerCase(S);
  if Lower = 'service01' then Exit(lpmService01);
  if Lower = 'service22' then Exit(lpmService22);
  Result := lpmUnknown;
end;

//------------------------------------------------------------------------------
// PARSE DTC EXTENDED KIND
//------------------------------------------------------------------------------
function ParseDtcExtendedKind(const S: string): TOBDDtcExtendedDataKind;
const
  Map: array[0..5] of record Tag: string; Kind: TOBDDtcExtendedDataKind end = (
    (Tag: 'occurrence_counter'; Kind: xdkOccurrenceCounter),
    (Tag: 'aging_counter'; Kind: xdkAgingCounter),
    (Tag: 'miles_since_cleared'; Kind: xdkMilesSinceCleared),
    (Tag: 'freeze_frame_template'; Kind: xdkFreezeFrameTemplate),
    (Tag: 'oem_status_byte'; Kind: xdkOemStatusByte),
    (Tag: 'environmental_data'; Kind: xdkEnvironmentalData)
  );
var
  I: Integer;
  Lower: string;
begin
  Lower := LowerCase(S);
  for I := Low(Map) to High(Map) do
    if Map[I].Tag = Lower then Exit(Map[I].Kind);
  Result := xdkUnknown;
end;

//------------------------------------------------------------------------------
// PARSE ENUM VALUES
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.ParseEnumValues(Obj: TJSONObject;
  const Key: string): TArray<TPair<Integer, string>>;
var
  ValuesObj: TJSONObject;
  Pair: TJSONPair;
  Acc: TList<TPair<Integer, string>>;
begin
  Result := nil;
  if Obj = nil then Exit;
  ValuesObj := Obj.GetValue<TJSONObject>(Key);
  if ValuesObj = nil then Exit;
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

//------------------------------------------------------------------------------
// LOAD CODING BLOCKS
//------------------------------------------------------------------------------
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
    if not (Arr.Items[I] is TJSONObject) then Continue;
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
        if not (FieldsArr.Items[J] is TJSONObject) then Continue;
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

//------------------------------------------------------------------------------
// LOAD ADAPTATIONS
//------------------------------------------------------------------------------
procedure TOBDOEMJSONCatalog.LoadAdaptations(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDAdaptationEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
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

//------------------------------------------------------------------------------
// LOAD ACTUATOR TESTS
//------------------------------------------------------------------------------
procedure TOBDOEMJSONCatalog.LoadActuatorTests(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDActuatorTestEntry;
  EcuStr: string;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
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
    Entry.ExpectedResponseKind := Item.GetValue<string>('response_kind', '');
    Entry.ExpectedResponseLabel := Item.GetValue<string>('response_label', '');
    FActuatorTests.Add(Entry);
  end;
end;

//------------------------------------------------------------------------------
// LOAD LIVE PIDS
//------------------------------------------------------------------------------
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
    if not (Arr.Items[I] is TJSONObject) then Continue;
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

//------------------------------------------------------------------------------
// LOAD DTC EXTENDED
//------------------------------------------------------------------------------
procedure TOBDOEMJSONCatalog.LoadDtcExtended(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDDtcExtendedDataEntry;
  Decoder: TJSONObject;
begin
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDDtcExtendedDataEntry);
    Entry.DtcCode := Item.GetValue<string>('code', '');
    Entry.RecordNumber := Byte(ParseHexOrInt(Item.GetValue<string>('record', '0')));
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

//------------------------------------------------------------------------------
// CONVERSIONS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// AS BASE ROUTINES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// AS BASE ECUS
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.AsBaseECUs: TArray<TOBDOEMECU>;
var
  I: Integer;
begin
  SetLength(Result, FECUs.Count);
  for I := 0 to FECUs.Count - 1 do
    Result[I] := MakeOEMECU(FECUs[I].Address, FECUs[I].Name, FECUs[I].CommonName);
end;

//------------------------------------------------------------------------------
// FIND DID
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// PAYLOAD DECODING
//------------------------------------------------------------------------------
function ReadUInt(const Payload: TBytes; Size: Integer): UInt64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Size - 1 do Result := (Result shl 8) or Payload[I];
end;

//------------------------------------------------------------------------------
// READ INT
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FORMAT NUMERIC
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// HEX FALLBACK
//------------------------------------------------------------------------------
function HexFallback(const Payload: TBytes): string;
var
  I: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    for I := 0 to High(Payload) do
    begin
      if I > 0 then Builder.Append(' ');
      Builder.Append(IntToHex(Payload[I], 2));
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

//------------------------------------------------------------------------------
// DECODE PAYLOAD
//------------------------------------------------------------------------------
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
  if not FindDID(DID, Entry) then Exit;
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
        if Length(Payload) < Spec.Size then Exit;
        N := ReadUInt(Payload, Spec.Size);
        if Assigned(Spec.EnumValues) and Spec.EnumValues.TryGetValue(N, Lookup) then
          Result := Lookup
        else
          Result := Format('0x%.*x', [Spec.Size * 2, N]);
      end;

    dkBitmask:
      if Spec.Size > 0 then
      begin
        if Length(Payload) < Spec.Size then Exit;
        N := ReadUInt(Payload, Spec.Size);
        SetLength(Names, 0);
        BitCount := Spec.Size * 8;
        for Bit := 0 to BitCount - 1 do
          if (N and (UInt64(1) shl Bit)) <> 0 then
          begin
            Nm := '';
            if Assigned(Spec.BitNames) then
              Spec.BitNames.TryGetValue(Bit, Nm);
            if Nm = '' then Nm := Format('bit_%d', [Bit]);
            Names := Names + [Nm];
          end;
        Result := string.Join(',', Names);
      end;
  end;
end;

//------------------------------------------------------------------------------
// DIDCOUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.DIDCount: Integer;
begin
  Result := FDIDs.Count;
end;

//------------------------------------------------------------------------------
// DID
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.DID(Index: Integer): TOBDOEMDIDEntry;
begin
  Result := FDIDs[Index];
end;

//------------------------------------------------------------------------------
// ROUTINE COUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.RoutineCount: Integer;
begin
  Result := FRoutines.Count;
end;

//------------------------------------------------------------------------------
// ROUTINE
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.Routine(Index: Integer): TOBDOEMRoutineEntry;
begin
  Result := FRoutines[Index];
end;

//------------------------------------------------------------------------------
// DTC RANGE COUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.DtcRangeCount: Integer;
begin
  Result := FDtcRanges.Count;
end;

//------------------------------------------------------------------------------
// DTC RANGE
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.DtcRange(Index: Integer): TOBDOEMDtcRange;
begin
  Result := FDtcRanges[Index];
end;

//------------------------------------------------------------------------------
// ECUCOUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.ECUCount: Integer;
begin
  Result := FECUs.Count;
end;

//------------------------------------------------------------------------------
// ECU
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.ECU(Index: Integer): TOBDOEMECUEntry;
begin
  Result := FECUs[Index];
end;

//------------------------------------------------------------------------------
// CODING BLOCK COUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.CodingBlockCount: Integer;
begin
  Result := FCodingBlocks.Count;
end;

//------------------------------------------------------------------------------
// CODING BLOCK
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.CodingBlock(Index: Integer): TOBDCodingBlockEntry;
begin
  Result := FCodingBlocks[Index];
end;

//------------------------------------------------------------------------------
// ADAPTATION COUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.AdaptationCount: Integer;
begin
  Result := FAdaptations.Count;
end;

//------------------------------------------------------------------------------
// ADAPTATION
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.Adaptation(Index: Integer): TOBDAdaptationEntry;
begin
  Result := FAdaptations[Index];
end;

//------------------------------------------------------------------------------
// ACTUATOR TEST COUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.ActuatorTestCount: Integer;
begin
  Result := FActuatorTests.Count;
end;

//------------------------------------------------------------------------------
// ACTUATOR TEST
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.ActuatorTest(Index: Integer): TOBDActuatorTestEntry;
begin
  Result := FActuatorTests[Index];
end;

//------------------------------------------------------------------------------
// LIVE PIDCOUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.LivePIDCount: Integer;
begin
  Result := FLivePIDs.Count;
end;

//------------------------------------------------------------------------------
// LIVE PID
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.LivePID(Index: Integer): TOBDLivePIDEntry;
begin
  Result := FLivePIDs[Index];
end;

//------------------------------------------------------------------------------
// DTC EXTENDED COUNT
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.DtcExtendedCount: Integer;
begin
  Result := FDtcExtended.Count;
end;

//------------------------------------------------------------------------------
// DTC EXTENDED
//------------------------------------------------------------------------------
function TOBDOEMJSONCatalog.DtcExtended(Index: Integer): TOBDDtcExtendedDataEntry;
begin
  Result := FDtcExtended[Index];
end;

end.
