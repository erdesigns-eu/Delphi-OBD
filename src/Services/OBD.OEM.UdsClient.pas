//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.UdsClient
// CONTENTS       : High-level catalog-driven UDS client. Given a JSON
//                  OEM catalog and a transport that does ISO-TP
//                  request/response, exposes:
//                    - ReadDID(name)            → typed decoded value
//                    - WriteAdaptation(channel, value) with min/max validation
//                    - ExecuteRoutine(name, args)
//                    - ReadCodingBlock / WriteCodingBlock with bit-level
//                      pack/unpack and read-modify-write safety
//                    - RunActuatorTest(name) with safety-warning gate
//                    - ReadDtcs(statusMask)     → array of DTC instances
//                    - StreamLivePIDs(names, callback)
//                  The transport (IOBDDiagnosticTransport) is an
//                  abstraction over CAN / DoIP / virtual / mock so the
//                  client itself doesn't depend on a wire protocol.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.UdsClient;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.DateUtils,
  System.Generics.Collections,
  OBD.OEM, OBD.OEM.Catalog.JSON, OBD.OEM.DTC;

type
  EOBDUdsClientError       = class(Exception);
  EOBDUdsNoSession         = class(EOBDUdsClientError);
  EOBDUdsTransportError    = class(EOBDUdsClientError);
  EOBDUdsActuatorSafety    = class(EOBDUdsClientError);
  EOBDUdsValidation        = class(EOBDUdsClientError);
  EOBDUdsCatalogMiss       = class(EOBDUdsClientError);
  EOBDUdsNegativeResponse  = class(EOBDUdsClientError);
  EOBDUdsCodingError       = class(EOBDUdsClientError);

  /// <summary>One round-trip diag message: caller passes the
  /// service+payload, transport returns the response payload (or
  /// raises). Decouples the client from CAN/DoIP wiring.
  /// <c>Request</c> begins with the service byte (0x22, 0x2E, 0x31,
  /// ...); transport prepends ISO-TP framing. Response begins with
  /// the positive-response byte (service+0x40); on a negative
  /// response (0x7F xx NRC), the transport raises
  /// <c>EOBDUdsNegativeResponse</c> with the NRC byte set.</summary>
  IOBDDiagnosticTransport = interface
    ['{F1A2B3C4-D5E6-4789-AB12-345678901234}']
    function  SendReceive(const Request: TBytes;
                          TimeoutMs: Cardinal = 1500): TBytes;
    procedure SetTargetECU(Address: Word);
    function  TargetECU: Word;
  end;

  /// <summary>Decoded value tagged with its catalog metadata.
  /// Numeric kinds populate <c>AsFloat</c> after scale/offset;
  /// integer kinds populate <c>AsInteger</c>; text kinds populate
  /// <c>AsString</c>. <c>Formatted</c> is always set to a
  /// display-ready string ("23.5 °C", "ON", "0xAA BB").</summary>
  TOBDDecodedValue = record
    Name: string;
    Kind: TOBDOEMDecoderKind;
    AsString: string;
    AsFloat: Double;
    AsInteger: Int64;
    AsBytes: TBytes;
    Unit_: string;
    Formatted: string;
    RawPayload: TBytes;
  end;

  /// <summary>One sample emitted by the live-PID streamer.</summary>
  TOBDLiveSample = record
    Name: string;
    Decoded: TOBDDecodedValue;
    Timestamp: TDateTime;
  end;

  TOBDLiveSampleEvent = reference to procedure(const Sample: TOBDLiveSample);

  /// <summary>One DTC reported by the ECU. <c>StatusByte</c> is the
  /// ISO 14229 §11.4 status byte; <c>Description</c> is filled
  /// from the catalog if known.</summary>
  TOBDDtcInstance = record
    Code: string;
    StatusByte: Byte;
    Description: string;
    Severity: TOBDDtcSeverity;
  end;

  /// <summary>Coding-block snapshot returned by <c>ReadCodingBlock</c>.
  /// Field name → typed value (Int64 for numeric/bit/enum; string
  /// for ASCII). Hand back to <c>WriteCodingBlock</c> after edits;
  /// the client repacks the modified payload bit-by-bit and writes
  /// it via Service 2E. Read-modify-write preserves bits not covered
  /// by the catalog block.</summary>
  TOBDCodingValues = class
  strict private
    FInts: TDictionary<string, Int64>;
    FStrs: TDictionary<string, string>;
    FRaw: TBytes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetInt(const Name: string; Value: Int64);
    function  GetInt(const Name: string): Int64;
    procedure SetStr(const Name: string; const Value: string);
    function  GetStr(const Name: string): string;
    function  HasField(const Name: string): Boolean;
    function  IntFields: TArray<TPair<string, Int64>>;
    function  StrFields: TArray<TPair<string, string>>;
    /// <summary>The original payload as read from the ECU. Used by
    /// <c>WriteCodingBlock</c> to preserve uncovered bits.</summary>
    property Raw: TBytes read FRaw write FRaw;
  end;

  /// <summary>Result of an actuator test or routine execution.</summary>
  TOBDActuatorResult = record
    Status: Byte;
    Message: string;
    RawResponse: TBytes;
  end;

  /// <summary>Stream handle returned by <c>StreamLivePIDs</c>; call
  /// <c>Stop</c> to terminate the polling thread.</summary>
  IOBDStreamHandle = interface
    ['{B1C2D3E4-5F60-7890-1234-567890ABCDEF}']
    procedure Stop;
    function  IsRunning: Boolean;
  end;

  /// <summary>The high-level catalog-driven UDS client.</summary>
  IOBDUdsClient = interface
    ['{0A1B2C3D-4E5F-6789-ABCD-EF0123456789}']
    procedure OpenSession(const Catalog: TOBDOEMJSONCatalog;
                          const Transport: IOBDDiagnosticTransport;
                          ECUAddress: Word);
    procedure CloseSession;
    function  IsOpen: Boolean;

    function  ReadDID(const NameOrHex: string): TOBDDecodedValue;
    function  WriteAdaptation(const ChannelOrHex: string;
                              Value: Int64): Boolean;
    function  ExecuteRoutine(const NameOrHex: string;
                             const Args: TBytes;
                             RoutineType: Byte = $01): TOBDActuatorResult;

    function  ReadCodingBlock(const Name: string): TOBDCodingValues;
    procedure WriteCodingBlock(const Name: string;
                               const Values: TOBDCodingValues);

    function  RunActuatorTest(const Name: string;
                              AcknowledgeSafetyWarning: Boolean = False)
                              : TOBDActuatorResult;

    function  ReadDtcs(StatusMask: Byte = $FF): TArray<TOBDDtcInstance>;

    function  StreamLivePIDs(const Names: array of string;
                             const OnSample: TOBDLiveSampleEvent;
                             PollIntervalMs: Cardinal = 100)
                             : IOBDStreamHandle;
  end;

/// <summary>Construct a fresh UDS client. Call <c>OpenSession</c>
/// before any other method.</summary>
function CreateUdsClient: IOBDUdsClient;

/// <summary>Decode a raw payload using a catalog DID's decoder spec.
/// Exposed for unit tests; production callers go through
/// <c>IOBDUdsClient.ReadDID</c>.</summary>
function DecodePayloadAs(const Catalog: TOBDOEMJSONCatalog;
                        const DID: Word;
                        const Payload: TBytes): TOBDDecodedValue;

implementation

uses
  System.Math, System.Variants, System.StrUtils;

//==============================================================================
// Helpers
//==============================================================================

/// <summary>Parse <c>"0xABCD"</c> or <c>"abcd"</c> or <c>"43981"</c>
/// (decimal) into a Word. Raises on malformed input.</summary>
function ParseHexOrDecWord(const S: string): Word;
var
  Stripped: string;
  Big: Int64;
begin
  Stripped := Trim(S);
  if Stripped = '' then
    raise EOBDUdsValidation.Create('empty identifier');
  if (Length(Stripped) > 2) and (Stripped[1] = '0') and
     ((Stripped[2] = 'x') or (Stripped[2] = 'X')) then
    Big := StrToInt64('$' + Copy(Stripped, 3, MaxInt))
  else if not TryStrToInt64(Stripped, Big) then
    Big := StrToInt64('$' + Stripped);
  if (Big < 0) or (Big > $FFFF) then
    raise EOBDUdsValidation.CreateFmt('identifier out of range: %s', [S]);
  Result := Word(Big);
end;

/// <summary>Decode a 3-byte UDS DTC representation into the
/// canonical 5-character "P/B/C/U" code per ISO 15031-5 / SAE
/// J2012. Bits 15-14 of the first 16 bits select the system letter;
/// the remaining 14 bits decode as 4 hex digits. The third byte is
/// the failure-mode extension and is typically 0 for stored
/// codes — it isn't part of the displayed code.</summary>
function DecodeDtcCode(const B0, B1, B2: Byte): string;
const
  Letters: array[0..3] of Char = ('P', 'C', 'B', 'U');
var
  Letter: Char;
  Code14: Word;
begin
  Letter := Letters[(B0 shr 6) and $03];
  Code14 := ((Word(B0) and $3F) shl 8) or B1;
  Result := Format('%s%.4X', [Letter, Code14]);
end;

/// <summary>Look up a DID by its <c>Name</c> (preferred) or hex
/// identifier. Raises <c>EOBDUdsCatalogMiss</c> if neither resolves
/// inside the supplied catalog.</summary>
function ResolveDID(const Catalog: TOBDOEMJSONCatalog;
                    const NameOrHex: string;
                    out Entry: TOBDOEMDIDEntry): Word;
var
  I: Integer;
  Cur: TOBDOEMDIDEntry;
  Trimmed: string;
begin
  Trimmed := Trim(NameOrHex);
  // Name match wins; case-insensitive.
  for I := 0 to Catalog.DIDCount - 1 do
  begin
    Cur := Catalog.DID(I);
    if SameText(Cur.Name, Trimmed) then
    begin
      Entry := Cur;
      Exit(Cur.DID);
    end;
  end;
  // Fall through to hex/decimal lookup.
  Result := ParseHexOrDecWord(Trimmed);
  if not Catalog.FindDID(Result, Entry) then
    raise EOBDUdsCatalogMiss.CreateFmt(
      'DID %s not present in catalog', [NameOrHex]);
end;

/// <summary>Look up an adaptation channel by name or hex.</summary>
function ResolveAdaptation(const Catalog: TOBDOEMJSONCatalog;
                           const NameOrHex: string;
                           out Entry: TOBDAdaptationEntry): Word;
var
  I: Integer;
  Cur: TOBDAdaptationEntry;
  Trimmed: string;
begin
  Trimmed := Trim(NameOrHex);
  for I := 0 to Catalog.AdaptationCount - 1 do
  begin
    Cur := Catalog.Adaptation(I);
    if SameText(Cur.Name, Trimmed) then
    begin
      Entry := Cur;
      Exit(Cur.Channel);
    end;
  end;
  Result := ParseHexOrDecWord(Trimmed);
  for I := 0 to Catalog.AdaptationCount - 1 do
  begin
    Cur := Catalog.Adaptation(I);
    if Cur.Channel = Result then
    begin
      Entry := Cur;
      Exit;
    end;
  end;
  raise EOBDUdsCatalogMiss.CreateFmt(
    'Adaptation channel %s not in catalog', [NameOrHex]);
end;

/// <summary>Look up an actuator test by name or hex.</summary>
function ResolveActuator(const Catalog: TOBDOEMJSONCatalog;
                         const NameOrHex: string;
                         out Entry: TOBDActuatorTestEntry): Word;
var
  I: Integer;
  Cur: TOBDActuatorTestEntry;
  Trimmed: string;
begin
  Trimmed := Trim(NameOrHex);
  for I := 0 to Catalog.ActuatorTestCount - 1 do
  begin
    Cur := Catalog.ActuatorTest(I);
    if SameText(Cur.Name, Trimmed) then
    begin
      Entry := Cur;
      Exit(Cur.Identifier);
    end;
  end;
  Result := ParseHexOrDecWord(Trimmed);
  for I := 0 to Catalog.ActuatorTestCount - 1 do
  begin
    Cur := Catalog.ActuatorTest(I);
    if Cur.Identifier = Result then
    begin
      Entry := Cur;
      Exit;
    end;
  end;
  raise EOBDUdsCatalogMiss.CreateFmt(
    'Actuator test %s not in catalog', [NameOrHex]);
end;

/// <summary>Look up a coding block by name. (Coding blocks are
/// always identified by name; the underlying DID can collide across
/// blocks for very-different coding payloads.)</summary>
function ResolveCodingBlock(const Catalog: TOBDOEMJSONCatalog;
                            const Name: string;
                            out Entry: TOBDCodingBlockEntry): Boolean;
var
  I: Integer;
  Cur: TOBDCodingBlockEntry;
begin
  Result := False;
  for I := 0 to Catalog.CodingBlockCount - 1 do
  begin
    Cur := Catalog.CodingBlock(I);
    if SameText(Cur.Name, Name) then
    begin
      Entry := Cur;
      Exit(True);
    end;
  end;
end;

/// <summary>Look up a routine by name or hex.</summary>
function ResolveRoutine(const Catalog: TOBDOEMJSONCatalog;
                        const NameOrHex: string;
                        out Entry: TOBDOEMRoutineEntry): Word;
var
  I: Integer;
  Cur: TOBDOEMRoutineEntry;
  Trimmed: string;
begin
  Trimmed := Trim(NameOrHex);
  for I := 0 to Catalog.RoutineCount - 1 do
  begin
    Cur := Catalog.Routine(I);
    if SameText(Cur.Name, Trimmed) then
    begin
      Entry := Cur;
      Exit(Cur.Identifier);
    end;
  end;
  Result := ParseHexOrDecWord(Trimmed);
  for I := 0 to Catalog.RoutineCount - 1 do
  begin
    Cur := Catalog.Routine(I);
    if Cur.Identifier = Result then
    begin
      Entry := Cur;
      Exit;
    end;
  end;
  raise EOBDUdsCatalogMiss.CreateFmt(
    'Routine %s not in catalog', [NameOrHex]);
end;

//==============================================================================
// UDS request builders (kept here so the unit is self-contained — we
// don't pull TUDSProtocol because we only need one-shot byte builders.)
//==============================================================================

function BuildReadDataByIdentifier(const DID: Word): TBytes;
begin
  Result := [$22, Byte(DID shr 8), Byte(DID and $FF)];
end;

function BuildWriteDataByIdentifier(const DID: Word; const Data: TBytes): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 3 + Length(Data));
  Result[0] := $2E;
  Result[1] := Byte(DID shr 8);
  Result[2] := Byte(DID and $FF);
  for I := 0 to High(Data) do
    Result[3 + I] := Data[I];
end;

function BuildRoutineControl(const RoutineType: Byte; const RID: Word;
                             const Args: TBytes): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 4 + Length(Args));
  Result[0] := $31;
  Result[1] := RoutineType;
  Result[2] := Byte(RID shr 8);
  Result[3] := Byte(RID and $FF);
  for I := 0 to High(Args) do
    Result[4 + I] := Args[I];
end;

function BuildReadDtcByStatusMask(const StatusMask: Byte): TBytes;
begin
  Result := [$19, $02, StatusMask];
end;

//==============================================================================
// Decoder dispatch (E.2)
//==============================================================================

function FormatNumeric(const Value: Double; const Unit_: string): string;
begin
  if Frac(Value) = 0 then
    Result := IntToStr(Trunc(Value))
  else
    Result := FormatFloat('0.###', Value);
  if Unit_ <> '' then
    Result := Result + ' ' + Unit_;
end;

function ReadUInt16BE(const B: TBytes; Offset: Integer): Word;
begin
  Result := (Word(B[Offset]) shl 8) or B[Offset + 1];
end;

function ReadInt16BE(const B: TBytes; Offset: Integer): SmallInt;
begin
  Result := SmallInt((Word(B[Offset]) shl 8) or B[Offset + 1]);
end;

function ReadUInt32BE(const B: TBytes; Offset: Integer): Cardinal;
begin
  Result := (Cardinal(B[Offset]) shl 24) or
            (Cardinal(B[Offset + 1]) shl 16) or
            (Cardinal(B[Offset + 2]) shl 8) or
            B[Offset + 3];
end;

function ReadInt32BE(const B: TBytes; Offset: Integer): Integer;
begin
  Result := Integer(ReadUInt32BE(B, Offset));
end;

function HexDump(const B: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(B) do
  begin
    if I > 0 then Result := Result + ' ';
    Result := Result + IntToHex(B[I], 2);
  end;
end;

function DecodePayloadAs(const Catalog: TOBDOEMJSONCatalog;
                        const DID: Word;
                        const Payload: TBytes): TOBDDecodedValue;
var
  Entry: TOBDOEMDIDEntry;
  Spec: TOBDDecoderSpec;
  EnumKey: Cardinal;
  EnumLabel: string;
begin
  Result := Default(TOBDDecodedValue);
  Result.RawPayload := Payload;

  if not Catalog.FindDID(DID, Entry) then
  begin
    Result.Kind := dkUnknown;
    Result.AsBytes := Payload;
    Result.Formatted := HexDump(Payload);
    Exit;
  end;

  Result.Name := Entry.Name;
  Spec := Entry.Decoder;
  // The compile-time-public Kind enum doesn't have to match the
  // JSON-side TOBDDecoderKind 1:1, but they share the relevant tags
  // through ParseOEMDecoderKind. Clients usually only care about
  // Kind's broad bucket (numeric vs. string vs. bytes); subtle
  // bit-width differences are absorbed in AsFloat / AsInteger.
  Result.Kind := dkUnknown; // updated below per branch
  Result.Unit_ := Spec.Unit_;

  case Spec.Kind of
    dkUInt8:
      begin
        Result.Kind := dkUInt8;
        if Length(Payload) < 1 then Exit;
        Result.AsInteger := Payload[0];
        Result.AsFloat := (Result.AsInteger * Spec.Scale) + Spec.Offset;
        Result.Formatted := FormatNumeric(Result.AsFloat, Spec.Unit_);
      end;
    dkUInt16BE:
      begin
        Result.Kind := dkUInt16BE;
        if Length(Payload) < 2 then Exit;
        Result.AsInteger := ReadUInt16BE(Payload, 0);
        Result.AsFloat := (Result.AsInteger * Spec.Scale) + Spec.Offset;
        Result.Formatted := FormatNumeric(Result.AsFloat, Spec.Unit_);
      end;
    dkUInt32BE:
      begin
        Result.Kind := dkUInt32BE;
        if Length(Payload) < 4 then Exit;
        Result.AsInteger := ReadUInt32BE(Payload, 0);
        Result.AsFloat := (Result.AsInteger * Spec.Scale) + Spec.Offset;
        Result.Formatted := FormatNumeric(Result.AsFloat, Spec.Unit_);
      end;
    dkInt16BE:
      begin
        Result.Kind := dkInt16BE;
        if Length(Payload) < 2 then Exit;
        Result.AsInteger := ReadInt16BE(Payload, 0);
        Result.AsFloat := (Result.AsInteger * Spec.Scale) + Spec.Offset;
        Result.Formatted := FormatNumeric(Result.AsFloat, Spec.Unit_);
      end;
    dkInt32BE:
      begin
        Result.Kind := dkInt32BE;
        if Length(Payload) < 4 then Exit;
        Result.AsInteger := ReadInt32BE(Payload, 0);
        Result.AsFloat := (Result.AsInteger * Spec.Scale) + Spec.Offset;
        Result.Formatted := FormatNumeric(Result.AsFloat, Spec.Unit_);
      end;
    dkAscii:
      begin
        Result.Kind := dkAscii;
        if Length(Payload) > 0 then
          SetString(Result.AsString, PAnsiChar(@Payload[0]), Length(Payload))
        else
          Result.AsString := '';
        // Strip trailing NULs / whitespace.
        Result.AsString := Trim(Result.AsString);
        Result.Formatted := Result.AsString;
      end;
    dkHex:
      begin
        Result.Kind := dkHex;
        Result.AsBytes := Payload;
        Result.Formatted := HexDump(Payload);
      end;
    dkEnum:
      begin
        Result.Kind := dkEnum;
        if Length(Payload) >= 1 then
          Result.AsInteger := Payload[0];
        EnumKey := Cardinal(Result.AsInteger);
        Result.AsString := '';
        if Assigned(Spec.EnumValues) and
           Spec.EnumValues.TryGetValue(EnumKey, EnumLabel) then
          Result.AsString := EnumLabel;
        if Result.AsString = '' then
          Result.Formatted := Format('0x%s', [IntToHex(EnumKey, 2)])
        else
          Result.Formatted := Result.AsString;
      end;
    dkBitmask:
      begin
        Result.Kind := dkBitmask;
        if Length(Payload) >= 1 then
          Result.AsInteger := Payload[0];
        Result.AsBytes := Payload;
        Result.Formatted := '0x' + IntToHex(Result.AsInteger, 2);
      end;
    dkBcdDate:
      begin
        Result.Kind := dkBcdDate;
        if Length(Payload) >= 3 then
          Result.Formatted := Format('%.2x.%.2x.%.2x',
            [Payload[0], Payload[1], Payload[2]]);
      end;
    dkSeconds:
      begin
        Result.Kind := dkSeconds;
        case Length(Payload) of
          1: Result.AsInteger := Payload[0];
          2: Result.AsInteger := ReadUInt16BE(Payload, 0);
          4: Result.AsInteger := ReadUInt32BE(Payload, 0);
        end;
        Result.AsFloat := Result.AsInteger;
        Result.Formatted := IntToStr(Result.AsInteger) + ' s';
      end;
  else
    // dkUnknown / unrecognised — hex-dump fallback.
    Result.Kind := dkUnknown;
    Result.AsBytes := Payload;
    Result.Formatted := HexDump(Payload);
  end;

  // If the catalog provides a friendlier formatted string, prefer it.
  if Result.Formatted = '' then
  begin
    var Friendly: string := Catalog.DecodePayload(DID, Payload);
    if Friendly <> '' then
      Result.Formatted := Friendly;
  end;
end;

//==============================================================================
// TOBDCodingValues
//==============================================================================

constructor TOBDCodingValues.Create;
begin
  inherited Create;
  FInts := TDictionary<string, Int64>.Create;
  FStrs := TDictionary<string, string>.Create;
end;

destructor TOBDCodingValues.Destroy;
begin
  FInts.Free;
  FStrs.Free;
  inherited;
end;

procedure TOBDCodingValues.SetInt(const Name: string; Value: Int64);
begin
  FInts.AddOrSetValue(Name, Value);
end;

function TOBDCodingValues.GetInt(const Name: string): Int64;
begin
  if not FInts.TryGetValue(Name, Result) then
    raise EOBDUdsCodingError.CreateFmt('coding field not present: %s', [Name]);
end;

procedure TOBDCodingValues.SetStr(const Name: string; const Value: string);
begin
  FStrs.AddOrSetValue(Name, Value);
end;

function TOBDCodingValues.GetStr(const Name: string): string;
begin
  if not FStrs.TryGetValue(Name, Result) then
    raise EOBDUdsCodingError.CreateFmt('coding field not present: %s', [Name]);
end;

function TOBDCodingValues.HasField(const Name: string): Boolean;
begin
  Result := FInts.ContainsKey(Name) or FStrs.ContainsKey(Name);
end;

function TOBDCodingValues.IntFields: TArray<TPair<string, Int64>>;
var
  I: Integer;
  Pair: TPair<string, Int64>;
begin
  SetLength(Result, FInts.Count);
  I := 0;
  for Pair in FInts do
  begin
    Result[I] := Pair;
    Inc(I);
  end;
end;

function TOBDCodingValues.StrFields: TArray<TPair<string, string>>;
var
  I: Integer;
  Pair: TPair<string, string>;
begin
  SetLength(Result, FStrs.Count);
  I := 0;
  for Pair in FStrs do
  begin
    Result[I] := Pair;
    Inc(I);
  end;
end;

//==============================================================================
// Coding-block bit-level pack / unpack (E.3)
//==============================================================================

function FieldBitWidth(const Field: TOBDCodingFieldEntry): Integer;
begin
  if Field.BitWidth > 0 then
    Exit(Field.BitWidth);
  case ParseCodingFieldKind(Field.KindStr) of
    cfkBit:      Result := 1;
    cfkUInt8:    Result := 8;
    cfkUInt16BE: Result := 16;
    cfkUInt32BE: Result := 32;
    cfkInt16BE:  Result := 16;
    cfkInt32BE:  Result := 32;
    cfkEnum:     Result := 8;  // default
    cfkBitmask:  Result := 8;
  else
    Result := 0;
  end;
end;

/// <summary>Read <c>BitWidth</c> bits starting at byte
/// <c>ByteOffset</c>, bit <c>BitOffset</c> (LSB-first within the
/// byte, big-endian across bytes for multi-byte fields).</summary>
function UnpackBits(const Payload: TBytes;
                    ByteOffset, BitOffset, BitWidth: Integer): Int64;
var
  AbsBit, EndBit, B: Integer;
  Bit: Boolean;
begin
  Result := 0;
  if BitWidth <= 0 then Exit;
  AbsBit := ByteOffset * 8 + BitOffset;
  EndBit := AbsBit + BitWidth;
  if EndBit > Length(Payload) * 8 then
    raise EOBDUdsCodingError.CreateFmt(
      'coding field exceeds payload (offset=%d.%d width=%d, payload=%d B)',
      [ByteOffset, BitOffset, BitWidth, Length(Payload)]);
  for B := AbsBit to EndBit - 1 do
  begin
    Bit := (Payload[B div 8] and (1 shl (B mod 8))) <> 0;
    if Bit then
      Result := Result or (Int64(1) shl (B - AbsBit));
  end;
end;

/// <summary>Pack <c>Value</c> into <c>BitWidth</c> bits starting at
/// the given offset. Mutates <c>Payload</c> in place. Bits not
/// covered by the field are preserved (read-modify-write
/// safe).</summary>
procedure PackBits(var Payload: TBytes;
                   ByteOffset, BitOffset, BitWidth: Integer;
                   Value: Int64);
var
  AbsBit, EndBit, B: Integer;
  Mask: Byte;
begin
  if BitWidth <= 0 then Exit;
  AbsBit := ByteOffset * 8 + BitOffset;
  EndBit := AbsBit + BitWidth;
  if EndBit > Length(Payload) * 8 then
    raise EOBDUdsCodingError.CreateFmt(
      'coding field exceeds payload (offset=%d.%d width=%d, payload=%d B)',
      [ByteOffset, BitOffset, BitWidth, Length(Payload)]);
  for B := AbsBit to EndBit - 1 do
  begin
    Mask := 1 shl (B mod 8);
    if (Value and (Int64(1) shl (B - AbsBit))) <> 0 then
      Payload[B div 8] := Payload[B div 8] or Mask
    else
      Payload[B div 8] := Payload[B div 8] and not Mask;
  end;
end;

//==============================================================================
// TStreamHandle / TStreamThread
//==============================================================================

type
  TUdsStreamThread = class;

  TStreamHandle = class(TInterfacedObject, IOBDStreamHandle)
  strict private
    FThread: TUdsStreamThread;
  public
    constructor Create(Thread: TUdsStreamThread);
    destructor Destroy; override;
    procedure Stop;
    function  IsRunning: Boolean;
  end;

  TUdsStreamThread = class(TThread)
  strict private
    FTransport: IOBDDiagnosticTransport;
    FCatalog: TOBDOEMJSONCatalog;
    FPids: TArray<TOBDOEMLivePIDEntry>;
    FOnSample: TOBDLiveSampleEvent;
    FInterval: Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(const ATransport: IOBDDiagnosticTransport;
                       const ACatalog: TOBDOEMJSONCatalog;
                       const APids: TArray<TOBDOEMLivePIDEntry>;
                       const AOnSample: TOBDLiveSampleEvent;
                       AInterval: Cardinal);
  end;

constructor TUdsStreamThread.Create(const ATransport: IOBDDiagnosticTransport;
                                    const ACatalog: TOBDOEMJSONCatalog;
                                    const APids: TArray<TOBDOEMLivePIDEntry>;
                                    const AOnSample: TOBDLiveSampleEvent;
                                    AInterval: Cardinal);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTransport := ATransport;
  FCatalog := ACatalog;
  FPids := APids;
  FOnSample := AOnSample;
  FInterval := AInterval;
end;

procedure TUdsStreamThread.Execute;
var
  PID: TOBDOEMLivePIDEntry;
  Req, Resp: TBytes;
  Sample: TOBDLiveSample;
  Decoded: TOBDDecodedValue;
  PayloadOffset: Integer;
  Payload: TBytes;
  I: Integer;
begin
  while not Terminated do
  begin
    for PID in FPids do
    begin
      if Terminated then Break;
      try
        // Service 22 read of the PID (we treat PID as a 16-bit DID).
        Req := BuildReadDataByIdentifier(PID.PID);
        Resp := FTransport.SendReceive(Req, 1000);
        // Strip 0x62 + 2-byte echo of DID.
        if Length(Resp) >= 3 then
        begin
          PayloadOffset := 3 + PID.FrameOffset;
          if PayloadOffset < Length(Resp) then
          begin
            SetLength(Payload, Length(Resp) - PayloadOffset);
            for I := 0 to High(Payload) do
              Payload[I] := Resp[PayloadOffset + I];
            Decoded := DecodePayloadAs(FCatalog, PID.PID, Payload);
            Sample.Name := PID.Name;
            Sample.Decoded := Decoded;
            Sample.Timestamp := Now;
            if Assigned(FOnSample) then
              FOnSample(Sample);
          end;
        end;
      except
        // Single-PID error mustn't kill the stream; swallow and
        // continue. Production code would log via the standard
        // logger sink.
      end;
    end;
    // Sleep in 25-ms chunks so Terminate is responsive.
    I := 0;
    while (I < Integer(FInterval)) and not Terminated do
    begin
      Sleep(Min(25, Integer(FInterval) - I));
      Inc(I, 25);
    end;
  end;
end;

constructor TStreamHandle.Create(Thread: TUdsStreamThread);
begin
  inherited Create;
  FThread := Thread;
end;

destructor TStreamHandle.Destroy;
begin
  Stop;
  inherited;
end;

procedure TStreamHandle.Stop;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

function TStreamHandle.IsRunning: Boolean;
begin
  Result := Assigned(FThread) and not FThread.Finished;
end;

//==============================================================================
// TOBDUdsClient
//==============================================================================

type
  TOBDUdsClient = class(TInterfacedObject, IOBDUdsClient)
  strict private
    FCatalog: TOBDOEMJSONCatalog;
    FTransport: IOBDDiagnosticTransport;
    FOpen: Boolean;
    FECUAddress: Word;
    FLock: TCriticalSection;
    procedure EnsureOpen;
    function  StripDIDEcho(const Resp: TBytes; DID: Word): TBytes;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure OpenSession(const Catalog: TOBDOEMJSONCatalog;
                          const Transport: IOBDDiagnosticTransport;
                          ECUAddress: Word);
    procedure CloseSession;
    function  IsOpen: Boolean;
    function  ReadDID(const NameOrHex: string): TOBDDecodedValue;
    function  WriteAdaptation(const ChannelOrHex: string;
                              Value: Int64): Boolean;
    function  ExecuteRoutine(const NameOrHex: string;
                             const Args: TBytes;
                             RoutineType: Byte = $01): TOBDActuatorResult;
    function  ReadCodingBlock(const Name: string): TOBDCodingValues;
    procedure WriteCodingBlock(const Name: string;
                               const Values: TOBDCodingValues);
    function  RunActuatorTest(const Name: string;
                              AcknowledgeSafetyWarning: Boolean = False)
                              : TOBDActuatorResult;
    function  ReadDtcs(StatusMask: Byte = $FF): TArray<TOBDDtcInstance>;
    function  StreamLivePIDs(const Names: array of string;
                             const OnSample: TOBDLiveSampleEvent;
                             PollIntervalMs: Cardinal = 100)
                             : IOBDStreamHandle;
  end;

constructor TOBDUdsClient.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TOBDUdsClient.Destroy;
begin
  CloseSession;
  FLock.Free;
  inherited;
end;

procedure TOBDUdsClient.OpenSession(const Catalog: TOBDOEMJSONCatalog;
                                   const Transport: IOBDDiagnosticTransport;
                                   ECUAddress: Word);
begin
  FLock.Enter;
  try
    if FOpen then
      raise EOBDUdsClientError.Create('session already open; CloseSession first');
    if Catalog = nil then
      raise EOBDUdsValidation.Create('catalog is nil');
    if Transport = nil then
      raise EOBDUdsValidation.Create('transport is nil');
    FCatalog := Catalog;
    FTransport := Transport;
    FECUAddress := ECUAddress;
    FTransport.SetTargetECU(ECUAddress);
    FOpen := True;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDUdsClient.CloseSession;
begin
  FLock.Enter;
  try
    FCatalog := nil;
    FTransport := nil;
    FOpen := False;
  finally
    FLock.Leave;
  end;
end;

function TOBDUdsClient.IsOpen: Boolean;
begin
  Result := FOpen;
end;

procedure TOBDUdsClient.EnsureOpen;
begin
  if not FOpen then
    raise EOBDUdsNoSession.Create('OpenSession must be called first');
end;

function TOBDUdsClient.StripDIDEcho(const Resp: TBytes; DID: Word): TBytes;
var
  I: Integer;
begin
  if Length(Resp) < 3 then
    raise EOBDUdsTransportError.CreateFmt(
      'response too short (got %d bytes)', [Length(Resp)]);
  if Resp[0] <> $62 then
    raise EOBDUdsTransportError.CreateFmt(
      'expected positive RDBI response (0x62), got 0x%s',
      [IntToHex(Resp[0], 2)]);
  if (Word(Resp[1]) shl 8 or Resp[2]) <> DID then
    raise EOBDUdsTransportError.Create(
      'DID echo mismatch in RDBI response');
  SetLength(Result, Length(Resp) - 3);
  for I := 0 to High(Result) do
    Result[I] := Resp[3 + I];
end;

function TOBDUdsClient.ReadDID(const NameOrHex: string): TOBDDecodedValue;
var
  Entry: TOBDOEMDIDEntry;
  DID: Word;
  Req, Resp, Payload: TBytes;
begin
  EnsureOpen;
  DID := ResolveDID(FCatalog, NameOrHex, Entry);
  if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
    FTransport.SetTargetECU(Entry.EcuAddress);
  try
    Req := BuildReadDataByIdentifier(DID);
    Resp := FTransport.SendReceive(Req, 1500);
    Payload := StripDIDEcho(Resp, DID);
    Result := DecodePayloadAs(FCatalog, DID, Payload);
  finally
    if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
      FTransport.SetTargetECU(FECUAddress);
  end;
end;

function TOBDUdsClient.WriteAdaptation(const ChannelOrHex: string;
                                       Value: Int64): Boolean;
var
  Entry: TOBDAdaptationEntry;
  Channel: Word;
  Kind: TOBDAdaptationKind;
  Data, Req, Resp: TBytes;
begin
  EnsureOpen;
  Channel := ResolveAdaptation(FCatalog, ChannelOrHex, Entry);
  Kind := ParseAdaptationKind(Entry.KindStr);
  // Validate against catalog min/max.
  if (Entry.MinValue <> 0) or (Entry.MaxValue <> 0) then
    if (Value < Entry.MinValue) or (Value > Entry.MaxValue) then
      raise EOBDUdsValidation.CreateFmt(
        'adaptation %s = %d outside [%d..%d]',
        [Entry.Name, Value, Entry.MinValue, Entry.MaxValue]);
  // Pack the value per adaptation kind.
  case Kind of
    adkUInt8,
    adkEnum:     Data := [Byte(Value)];
    adkUInt16BE,
    adkInt16BE:  Data := [Byte(Value shr 8), Byte(Value)];
    adkUInt32BE,
    adkInt32BE:  Data := [Byte(Value shr 24), Byte(Value shr 16),
                          Byte(Value shr 8),  Byte(Value)];
  else
    raise EOBDUdsValidation.CreateFmt(
      'unknown adaptation kind for %s', [Entry.Name]);
  end;

  if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
    FTransport.SetTargetECU(Entry.EcuAddress);
  try
    Req := BuildWriteDataByIdentifier(Channel, Data);
    Resp := FTransport.SendReceive(Req, 1500);
    Result := (Length(Resp) >= 1) and (Resp[0] = $6E);
  finally
    if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
      FTransport.SetTargetECU(FECUAddress);
  end;
end;

function TOBDUdsClient.ExecuteRoutine(const NameOrHex: string;
                                      const Args: TBytes;
                                      RoutineType: Byte = $01): TOBDActuatorResult;
var
  Entry: TOBDOEMRoutineEntry;
  RID: Word;
  Req, Resp: TBytes;
begin
  EnsureOpen;
  Result := Default(TOBDActuatorResult);
  RID := ResolveRoutine(FCatalog, NameOrHex, Entry);
  if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
    FTransport.SetTargetECU(Entry.EcuAddress);
  try
    Req := BuildRoutineControl(RoutineType, RID, Args);
    Resp := FTransport.SendReceive(Req, 5000);
    Result.RawResponse := Resp;
    if (Length(Resp) >= 4) and (Resp[0] = $71) then
    begin
      Result.Status := 0;
      Result.Message := Format('%s (%s) ok', [Entry.Name, Entry.Description]);
    end
    else
    begin
      Result.Status := 1;
      Result.Message := 'unexpected routine response: ' + HexDump(Resp);
    end;
  finally
    if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
      FTransport.SetTargetECU(FECUAddress);
  end;
end;

function TOBDUdsClient.ReadCodingBlock(const Name: string): TOBDCodingValues;
var
  Block: TOBDCodingBlockEntry;
  Field: TOBDCodingFieldEntry;
  Width, Value: Int64;
  Req, Resp, Payload: TBytes;
  Kind: TOBDCodingFieldKind;
  I: Integer;
begin
  EnsureOpen;
  if not ResolveCodingBlock(FCatalog, Name, Block) then
    raise EOBDUdsCatalogMiss.CreateFmt(
      'coding block %s not in catalog', [Name]);
  if (Block.EcuAddress <> 0) and (Block.EcuAddress <> FECUAddress) then
    FTransport.SetTargetECU(Block.EcuAddress);
  try
    Req := BuildReadDataByIdentifier(Block.DataIdentifier);
    Resp := FTransport.SendReceive(Req, 1500);
    Payload := StripDIDEcho(Resp, Block.DataIdentifier);
    if (Block.PayloadSize > 0) and (Length(Payload) < Block.PayloadSize) then
      raise EOBDUdsCodingError.CreateFmt(
        'coding block %s expected %d bytes, got %d',
        [Block.Name, Block.PayloadSize, Length(Payload)]);
    Result := TOBDCodingValues.Create;
    Result.Raw := Payload;
    for I := 0 to High(Block.Fields) do
    begin
      Field := Block.Fields[I];
      Kind := ParseCodingFieldKind(Field.KindStr);
      if Kind = cfkAscii then
      begin
        var S: string := '';
        for var J := 0 to Field.BitWidth - 1 do
          if (Field.ByteOffset + J) < Length(Payload) then
            S := S + Char(Payload[Field.ByteOffset + J]);
        Result.SetStr(Field.Name, Trim(S));
      end
      else
      begin
        Width := FieldBitWidth(Field);
        Value := UnpackBits(Payload, Field.ByteOffset, Field.BitOffset, Width);
        Result.SetInt(Field.Name, Value);
      end;
    end;
  finally
    if (Block.EcuAddress <> 0) and (Block.EcuAddress <> FECUAddress) then
      FTransport.SetTargetECU(FECUAddress);
  end;
end;

procedure TOBDUdsClient.WriteCodingBlock(const Name: string;
                                        const Values: TOBDCodingValues);
var
  Block: TOBDCodingBlockEntry;
  Field: TOBDCodingFieldEntry;
  Width: Int64;
  Payload, Req, Resp: TBytes;
  I, J: Integer;
  S: string;
begin
  EnsureOpen;
  if not ResolveCodingBlock(FCatalog, Name, Block) then
    raise EOBDUdsCatalogMiss.CreateFmt(
      'coding block %s not in catalog', [Name]);
  // Start from the as-read raw payload to preserve uncovered bits.
  Payload := Copy(Values.Raw, 0, Length(Values.Raw));
  if Length(Payload) < Block.PayloadSize then
  begin
    SetLength(Payload, Block.PayloadSize);
  end;
  for I := 0 to High(Block.Fields) do
  begin
    Field := Block.Fields[I];
    if not Values.HasField(Field.Name) then Continue;
    if ParseCodingFieldKind(Field.KindStr) = cfkAscii then
    begin
      S := Values.GetStr(Field.Name);
      for J := 0 to Field.BitWidth - 1 do
      begin
        if (Field.ByteOffset + J) < Length(Payload) then
        begin
          if J < Length(S) then
            Payload[Field.ByteOffset + J] := Byte(S[J + 1])
          else
            Payload[Field.ByteOffset + J] := 0;
        end;
      end;
    end
    else
    begin
      Width := FieldBitWidth(Field);
      // Validate min/max if specified.
      var V: Int64 := Values.GetInt(Field.Name);
      if (Field.MinValue <> 0) or (Field.MaxValue <> 0) then
        if (V < Field.MinValue) or (V > Field.MaxValue) then
          raise EOBDUdsValidation.CreateFmt(
            'coding field %s = %d outside [%d..%d]',
            [Field.Name, V, Field.MinValue, Field.MaxValue]);
      PackBits(Payload, Field.ByteOffset, Field.BitOffset, Width, V);
    end;
  end;
  if (Block.EcuAddress <> 0) and (Block.EcuAddress <> FECUAddress) then
    FTransport.SetTargetECU(Block.EcuAddress);
  try
    Req := BuildWriteDataByIdentifier(Block.DataIdentifier, Payload);
    Resp := FTransport.SendReceive(Req, 3000);
    if (Length(Resp) < 1) or (Resp[0] <> $6E) then
      raise EOBDUdsTransportError.CreateFmt(
        'WriteDataByIdentifier rejected for coding block %s: %s',
        [Block.Name, HexDump(Resp)]);
  finally
    if (Block.EcuAddress <> 0) and (Block.EcuAddress <> FECUAddress) then
      FTransport.SetTargetECU(FECUAddress);
  end;
end;

function TOBDUdsClient.RunActuatorTest(const Name: string;
                                       AcknowledgeSafetyWarning: Boolean): TOBDActuatorResult;
var
  Entry: TOBDActuatorTestEntry;
  RID: Word;
  Req, Resp: TBytes;
  Timeout: Cardinal;
begin
  EnsureOpen;
  Result := Default(TOBDActuatorResult);
  RID := ResolveActuator(FCatalog, Name, Entry);
  if (Entry.SafetyWarning <> '') and not AcknowledgeSafetyWarning then
    raise EOBDUdsActuatorSafety.CreateFmt(
      'actuator %s requires safety acknowledgement: %s',
      [Entry.Name, Entry.SafetyWarning]);
  // Default timeout = duration_ms + 2 s safety margin (or 5 s if unset).
  if Entry.DurationMs > 0 then
    Timeout := Entry.DurationMs + 2000
  else
    Timeout := 5000;
  if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
    FTransport.SetTargetECU(Entry.EcuAddress);
  try
    // Most actuators are RoutineControl start (0x31 0x01 RID-hi RID-lo).
    Req := BuildRoutineControl($01, RID, []);
    Resp := FTransport.SendReceive(Req, Timeout);
    Result.RawResponse := Resp;
    if (Length(Resp) >= 4) and (Resp[0] = $71) then
    begin
      Result.Status := 0;
      Result.Message := Format('%s ok', [Entry.Name]);
    end
    else
    begin
      Result.Status := 1;
      Result.Message := 'unexpected actuator response: ' + HexDump(Resp);
    end;
  finally
    if (Entry.EcuAddress <> 0) and (Entry.EcuAddress <> FECUAddress) then
      FTransport.SetTargetECU(FECUAddress);
  end;
end;

function TOBDUdsClient.ReadDtcs(StatusMask: Byte): TArray<TOBDDtcInstance>;
var
  Req, Resp: TBytes;
  I, Count, Off: Integer;
  Code: string;
  Status: Byte;
  Inst: TOBDDtcInstance;
begin
  EnsureOpen;
  SetLength(Result, 0);
  Req := BuildReadDtcByStatusMask(StatusMask);
  Resp := FTransport.SendReceive(Req, 2000);
  // Format: 59 02 <availMask> [3-byte DTC + 1-byte status]*
  if (Length(Resp) < 3) or (Resp[0] <> $59) or (Resp[1] <> $02) then
    Exit;
  Count := (Length(Resp) - 3) div 4;
  for I := 0 to Count - 1 do
  begin
    Off := 3 + I * 4;
    Code := DecodeDtcCode(Resp[Off], Resp[Off + 1], Resp[Off + 2]);
    Status := Resp[Off + 3];
    Inst.Code := Code;
    Inst.StatusByte := Status;
    Inst.Description := '';
    Inst.Severity := dtcSeverityUnknown;
    Result := Result + [Inst];
  end;
end;

function TOBDUdsClient.StreamLivePIDs(const Names: array of string;
                                     const OnSample: TOBDLiveSampleEvent;
                                     PollIntervalMs: Cardinal): IOBDStreamHandle;
var
  Pids: TArray<TOBDOEMLivePIDEntry>;
  I, J: Integer;
  Cur: TOBDOEMLivePIDEntry;
  Match: Boolean;
  Thread: TUdsStreamThread;
begin
  EnsureOpen;
  SetLength(Pids, 0);
  for I := 0 to High(Names) do
  begin
    Match := False;
    for J := 0 to FCatalog.LivePIDCount - 1 do
    begin
      Cur := FCatalog.LivePID(J);
      if SameText(Cur.Name, Names[I]) then
      begin
        Pids := Pids + [Cur];
        Match := True;
        Break;
      end;
    end;
    if not Match then
      raise EOBDUdsCatalogMiss.CreateFmt(
        'live PID %s not in catalog', [Names[I]]);
  end;
  if Length(Pids) = 0 then
    raise EOBDUdsValidation.Create('no PIDs to stream');
  Thread := TUdsStreamThread.Create(FTransport, FCatalog, Pids,
                                     OnSample, PollIntervalMs);
  Thread.Start;
  Result := TStreamHandle.Create(Thread);
end;

function CreateUdsClient: IOBDUdsClient;
begin
  Result := TOBDUdsClient.Create;
end;

end.
