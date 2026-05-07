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
  end;

  TOBDOEMDtcRange = record
    StartCode: string;
    EndCode: string;
    Source: string;
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
    FDIDs: TList<TOBDOEMDIDEntry>;
    FRoutines: TList<TOBDOEMRoutineEntry>;
    FDtcRanges: TList<TOBDOEMDtcRange>;
    function ParseHexOrInt(const S: string): Cardinal;
    function ParseDecoder(Obj: TJSONObject): TOBDDecoderSpec;
    procedure LoadFromJSON(Root: TJSONObject);
    procedure LoadDIDs(Arr: TJSONArray);
    procedure LoadRoutines(Arr: TJSONArray);
    procedure LoadDtcRanges(Arr: TJSONArray);
  public
    constructor Create(const FilePath: string); overload;
    constructor CreateFromText(const JsonText: string); overload;
    destructor Destroy; override;

    /// <summary>Cast catalog DIDs to the <c>TOBDOEMDataIdentifier</c> shape
    /// expected by <c>TOBDOEMExtensionBase</c>. Drops decoder/source.</summary>
    function AsBaseDIDs: TArray<TOBDOEMDataIdentifier>;
    /// <summary>Cast catalog routines to the base shape.</summary>
    function AsBaseRoutines: TArray<TOBDOEMRoutine>;

    /// <summary>Apply the decoder for <c>DID</c> (if any) to <c>Payload</c>.
    /// Returns the formatted string, or empty if no decoder / payload too short.</summary>
    function DecodePayload(const DID: Word; const Payload: TBytes): string;

    /// <summary>Find a DID; returns False if absent.</summary>
    function FindDID(const DID: Word; out Entry: TOBDOEMDIDEntry): Boolean;

    property Version: Integer read FVersion;
    property ManufacturerKey: string read FManufacturerKey;
    property DisplayName: string read FDisplayName;
    property ApplicableWMIs: TArray<string> read FApplicableWMIs;
    property DefaultSource: string read FDefaultSource;
    function DIDCount: Integer;
    function DID(Index: Integer): TOBDOEMDIDEntry;
    function RoutineCount: Integer;
    function Routine(Index: Integer): TOBDOEMRoutineEntry;
    function DtcRangeCount: Integer;
    function DtcRange(Index: Integer): TOBDOEMDtcRange;
  end;

implementation

uses
  System.Math, System.NetEncoding;

constructor TOBDOEMJSONCatalog.Create(const FilePath: string);
var
  Text: string;
begin
  if not TFile.Exists(FilePath) then
    raise EOBDCatalogError.CreateFmt('Catalog file %s not found', [FilePath]);
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

  Value := TJSONObject.ParseJSONValue(JsonText);
  if not (Value is TJSONObject) then
    raise EOBDCatalogError.Create('Catalog root must be a JSON object');
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
  // Decoder records own dictionaries — free per entry.
  for I := 0 to FDIDs.Count - 1 do
  begin
    if Assigned(FDIDs[I].Decoder.EnumValues) then FDIDs[I].Decoder.EnumValues.Free;
    if Assigned(FDIDs[I].Decoder.BitNames) then FDIDs[I].Decoder.BitNames.Free;
  end;
  FDIDs.Free;
  FRoutines.Free;
  FDtcRanges.Free;
  inherited;
end;

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
    begin Result.Kind := KindMap[I].Kind; Break; end;

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

  WMIArr := Root.GetValue<TJSONArray>('applicable_wmis');
  if Assigned(WMIArr) then
  begin
    SetLength(FApplicableWMIs, WMIArr.Count);
    for I := 0 to WMIArr.Count - 1 do
      FApplicableWMIs[I] := WMIArr.Items[I].Value;
  end;

  Arr := Root.GetValue<TJSONArray>('dids');
  if Assigned(Arr) then LoadDIDs(Arr);

  Arr := Root.GetValue<TJSONArray>('routines');
  if Assigned(Arr) then LoadRoutines(Arr);

  Arr := Root.GetValue<TJSONArray>('dtc_ranges');
  if Assigned(Arr) then LoadDtcRanges(Arr);
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
    if not (Arr.Items[I] is TJSONObject) then Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDOEMDIDEntry);
    Entry.DID := ParseHexOrInt(Item.GetValue<string>('did', '0'));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Source := Item.GetValue<string>('source', FDefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    EcuStr := Item.GetValue<string>('ecu_address', '');
    if EcuStr <> '' then Entry.EcuAddress := ParseHexOrInt(EcuStr);
    Entry.Decoder := ParseDecoder(Item.GetValue<TJSONObject>('decoder'));
    FDIDs.Add(Entry);
  end;
end;

procedure TOBDOEMJSONCatalog.LoadRoutines(Arr: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDOEMRoutineEntry;
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
    FRoutines.Add(Entry);
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
    Out_.DID := FDIDs[I].DID;
    Out_.Name := FDIDs[I].Name;
    Out_.Description := FDIDs[I].Description;
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
    Out_.Identifier := FRoutines[I].Identifier;
    Out_.Name := FRoutines[I].Name;
    Out_.Description := FRoutines[I].Description;
    Result[I] := Out_;
  end;
end;

function TOBDOEMJSONCatalog.FindDID(const DID: Word;
  out Entry: TOBDOEMDIDEntry): Boolean;
var I: Integer;
begin
  for I := 0 to FDIDs.Count - 1 do
    if FDIDs[I].DID = DID then
    begin Entry := FDIDs[I]; Exit(True); end;
  Result := False;
end;

//------------------------------------------------------------------------------
// PAYLOAD DECODING
//------------------------------------------------------------------------------
function ReadUInt(const Payload: TBytes; Size: Integer): UInt64;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Size - 1 do Result := (Result shl 8) or Payload[I];
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
var Combined: Double;
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
      if I > 0 then Builder.Append(' ');
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

function TOBDOEMJSONCatalog.DIDCount: Integer;
begin Result := FDIDs.Count; end;

function TOBDOEMJSONCatalog.DID(Index: Integer): TOBDOEMDIDEntry;
begin Result := FDIDs[Index]; end;

function TOBDOEMJSONCatalog.RoutineCount: Integer;
begin Result := FRoutines.Count; end;

function TOBDOEMJSONCatalog.Routine(Index: Integer): TOBDOEMRoutineEntry;
begin Result := FRoutines[Index]; end;

function TOBDOEMJSONCatalog.DtcRangeCount: Integer;
begin Result := FDtcRanges.Count; end;

function TOBDOEMJSONCatalog.DtcRange(Index: Integer): TOBDOEMDtcRange;
begin Result := FDtcRanges[Index]; end;

end.
