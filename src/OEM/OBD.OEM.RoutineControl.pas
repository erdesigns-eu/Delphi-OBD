//------------------------------------------------------------------------------
//  OBD.OEM.RoutineControl
//
//  UDS Service 0x31 (RoutineControl) wire helpers, argument
//  builders, response readers and schema-driven decoders.
//
//  ISO 14229-1 §10.5.4 — RoutineControl is structured as:
//
//    request:  <c>31 SF RID [DATA…]</c>
//    response: <c>71 SF RID [STATUS…]</c>
//
//  where <c>SF</c> is <c>01</c> StartRoutine, <c>02</c> StopRoutine
//  or <c>03</c> RequestRoutineResults. Per-routine input / output
//  schemas vary; this unit ships the builder / reader primitives
//  plus a schema struct that caller-supplied registries can hang
//  on top.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.RoutineControl;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.OEM.Coding;

type
  /// <summary>Raised on routine-framing or decoder errors.</summary>
  EOBDRoutineError = class(Exception);

  /// <summary>UDS RoutineControl sub-function.</summary>
  TOBDRoutineSubFunction = (
    rcStart           = $01,
    rcStop            = $02,
    rcRequestResults  = $03);

  /// <summary>
  ///   One field in a routine's argument or response schema.
  ///   Mirrors the DID decoder spec so callers can reuse decoder
  ///   kinds across both interfaces.
  /// </summary>
  TOBDRoutineFieldKind = (
    rfkUInt8,
    rfkUInt16BE,
    rfkUInt32BE,
    rfkInt8,
    rfkInt16BE,
    rfkInt32BE,
    /// <summary><c>Size</c> bytes; padded with <c>$00</c> on encode.</summary>
    rfkAscii,
    /// <summary>Raw hex bytes, <c>Size</c> long.</summary>
    rfkHex,
    /// <summary>3 bytes YYMMDD.</summary>
    rfkBcdDate,
    /// <summary><c>Size</c> bytes (1, 2 or 4).</summary>
    rfkEnum,
    /// <summary><c>Size</c> bytes (1, 2 or 4).</summary>
    rfkBitmask);

  /// <summary>One field in a routine schema.</summary>
  TOBDRoutineField = record
    Name: string;
    Description: string;
    Kind: TOBDRoutineFieldKind;
    /// <summary>Width in bytes for variable-width kinds.</summary>
    Size: Integer;
    /// <summary>Multiplicative scale (default 1.0).</summary>
    Scale: Double;
    /// <summary>Additive offset.</summary>
    Offset: Double;
    /// <summary>Display unit suffix.</summary>
    Unit_: string;
    /// <summary>Value → label map for enum fields.</summary>
    EnumValues: TDictionary<Cardinal, string>;
    /// <summary>Bit-index → label map for bitmask fields.</summary>
    BitNames: TDictionary<Integer, string>;
  end;

  /// <summary>Schema for one routine.</summary>
  /// <remarks>
  ///   <para><c>InputFields</c> is the expected ordered argument
  ///   list for a StartRoutine request; <c>OutputFields</c> is
  ///   what the response encodes after the SF + RID echo. Either
  ///   may be empty.</para>
  /// </remarks>
  TOBDRoutineSchema = record
    Identifier: Word;
    Name: string;
    Description: string;
    InputFields: TArray<TOBDRoutineField>;
    OutputFields: TArray<TOBDRoutineField>;
  end;

  /// <summary>One decoded field.</summary>
  TOBDDecodedField = record
    /// <summary>Field name.</summary>
    Name: string;
    /// <summary>Rendered "name = value" string.</summary>
    Display: string;
    /// <summary>Raw slice the field consumed.</summary>
    Raw: TBytes;
  end;

  /// <summary>
  ///   Routine-request builder (mutable, builder-pattern; resets
  ///   on <see cref="Clear"/>).
  /// </summary>
  TOBDRoutineRequestBuilder = record
  private
    FBytes: TBytes;
    procedure AppendByte(B: Byte);
    procedure AppendBytes(const Source: TBytes);
  public
    procedure AddUInt8(const Value: Byte);
    procedure AddUInt16BE(const Value: Word);
    procedure AddUInt32BE(const Value: Cardinal);
    procedure AddInt16BE(const Value: SmallInt);
    procedure AddInt32BE(const Value: Integer);
    /// <summary>
    ///   ASCII bytes; pads with <c>0x00</c> to
    ///   <c>FixedLength</c> when > 0, raises when the input is
    ///   longer than <c>FixedLength</c>.
    /// </summary>
    /// <param name="S">ASCII string.</param>
    /// <param name="FixedLength">Pad-to length; 0 = no padding.</param>
    procedure AddAscii(const S: string;
      const FixedLength: Integer = 0);
    procedure AddRawBytes(const Bytes: TBytes);
    /// <summary>3 BCD bytes <c>YY MM DD</c>.</summary>
    procedure AddBcdDate(const Year, Month, Day: Byte);
    /// <summary>One BCD byte (year 00..99).</summary>
    procedure AddBcdYear(const Year: Byte);
    /// <summary>Accumulated payload (no SID / SF / RID).</summary>
    function PayloadBytes: TBytes;
    /// <summary>
    ///   Wraps the payload as a complete frame:
    ///   <c>31 SF HiRID LoRID PAYLOAD</c>.
    /// </summary>
    /// <param name="SubFunction">Routine sub-function.</param>
    /// <param name="RID">Routine identifier.</param>
    function ToFrame(const SubFunction: TOBDRoutineSubFunction;
      const RID: Word): TBytes;
    /// <summary>Resets the builder.</summary>
    procedure Clear;
  end;

  /// <summary>Cursor-based response reader (raises on
  /// under-read).</summary>
  TOBDRoutineResponseReader = record
  private
    FBytes: TBytes;
    FCursor: Integer;
    procedure RequireBytes(const Count: Integer);
  public
    /// <summary>Wraps a byte array.</summary>
    /// <param name="Bytes">Bytes to read from.</param>
    class function Wrap(
      const Bytes: TBytes): TOBDRoutineResponseReader; static;
    function ReadUInt8: Byte;
    function ReadUInt16BE: Word;
    function ReadUInt32BE: Cardinal;
    function ReadInt16BE: SmallInt;
    function ReadInt32BE: Integer;
    /// <summary>Reads <c>ByteCount</c> bytes as ASCII; trailing
    /// <c>#0</c> padding is trimmed.</summary>
    function ReadAscii(const ByteCount: Integer): string;
    function ReadHexBytes(const ByteCount: Integer): TBytes;
    /// <summary>Reads 3 BCD bytes as <c>"20YY-MM-DD"</c>.</summary>
    function ReadBcdDate: string;
    /// <summary><c>True</c> when more bytes remain.</summary>
    function HasMore: Boolean;
    /// <summary>Bytes left to read.</summary>
    function Remaining: Integer;
    /// <summary>Current cursor position.</summary>
    property Cursor: Integer read FCursor;
  end;

/// <summary>Builds a <c>31 01 RID [DATA]</c> StartRoutine
/// frame.</summary>
/// <param name="RID">Routine identifier.</param>
/// <param name="InputData">Optional argument bytes.</param>
function BuildStartRoutine(const RID: Word;
  const InputData: TBytes = nil): TBytes;
/// <summary>Builds a <c>31 02 RID</c> StopRoutine frame.</summary>
/// <param name="RID">Routine identifier.</param>
function BuildStopRoutine(const RID: Word): TBytes;
/// <summary>Builds a <c>31 03 RID</c> RequestRoutineResults
/// frame.</summary>
/// <param name="RID">Routine identifier.</param>
function BuildRequestRoutineResults(const RID: Word): TBytes;

/// <summary>
///   Parses a positive <c>71 SF RID [STATUS]</c> response.
///   Returns the status payload (everything after the RID).
/// </summary>
/// <param name="Response">Response bytes.</param>
/// <param name="ExpectedSF">Expected sub-function echo.</param>
/// <param name="ExpectedRID">Expected RID echo.</param>
/// <exception cref="EOBDRoutineError">Negative response,
/// short payload or SID / SF / RID mismatch.</exception>
function ParseRoutineResponse(const Response: TBytes;
  const ExpectedSF: TOBDRoutineSubFunction;
  const ExpectedRID: Word): TBytes;

/// <summary>
///   Decodes <c>Bytes</c> against <c>Schema.OutputFields</c>.
///   Stops without error when the payload is shorter than the
///   schema (trailing optional fields are simply absent).
/// </summary>
/// <param name="Schema">Routine schema.</param>
/// <param name="Bytes">Status payload to decode.</param>
function DecodeRoutineOutput(const Schema: TOBDRoutineSchema;
  const Bytes: TBytes): TArray<TOBDDecodedField>;

implementation

uses
  System.Math;

function BytesAppend(const A, B: TBytes): TBytes;
begin
  SetLength(Result, Length(A) + Length(B));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A));
  if Length(B) > 0 then
    Move(B[0], Result[Length(A)], Length(B));
end;

function FormatNumeric(const Value: Double;
  const Field: TOBDRoutineField): string;
var
  Combined: Double;
begin
  Combined := (Value * Field.Scale) + Field.Offset;
  if Field.Unit_ <> '' then
    Result := Format('%.6g %s', [Combined, Field.Unit_])
  else
    Result := Format('%.6g', [Combined]);
end;

{ TOBDRoutineRequestBuilder }

procedure TOBDRoutineRequestBuilder.AppendByte(B: Byte);
var
  N: Integer;
begin
  N := Length(FBytes);
  SetLength(FBytes, N + 1);
  FBytes[N] := B;
end;

procedure TOBDRoutineRequestBuilder.AppendBytes(const Source: TBytes);
begin
  FBytes := BytesAppend(FBytes, Source);
end;

procedure TOBDRoutineRequestBuilder.AddUInt8(const Value: Byte);
begin
  AppendByte(Value);
end;

procedure TOBDRoutineRequestBuilder.AddUInt16BE(const Value: Word);
begin
  AppendByte(Byte(Value shr 8));
  AppendByte(Byte(Value and $FF));
end;

procedure TOBDRoutineRequestBuilder.AddUInt32BE(const Value: Cardinal);
begin
  AppendByte(Byte(Value shr 24));
  AppendByte(Byte((Value shr 16) and $FF));
  AppendByte(Byte((Value shr 8) and $FF));
  AppendByte(Byte(Value and $FF));
end;

procedure TOBDRoutineRequestBuilder.AddInt16BE(const Value: SmallInt);
begin
  AddUInt16BE(Word(Value));
end;

procedure TOBDRoutineRequestBuilder.AddInt32BE(const Value: Integer);
begin
  AddUInt32BE(Cardinal(Value));
end;

procedure TOBDRoutineRequestBuilder.AddAscii(const S: string;
  const FixedLength: Integer);
var
  Bytes: TBytes;
  Padded: TBytes;
begin
  Bytes := TEncoding.ASCII.GetBytes(S);
  if FixedLength = 0 then
    AppendBytes(Bytes)
  else
  begin
    if Length(Bytes) > FixedLength then
      raise EOBDRoutineError.CreateFmt(
        'ASCII field too long: "%s" exceeds %d bytes',
        [S, FixedLength]);
    SetLength(Padded, FixedLength);
    if Length(Bytes) > 0 then
      Move(Bytes[0], Padded[0], Length(Bytes));
    AppendBytes(Padded);
  end;
end;

procedure TOBDRoutineRequestBuilder.AddRawBytes(const Bytes: TBytes);
begin
  AppendBytes(Bytes);
end;

function ByteToBcd(const B: Byte): Byte;
begin
  if B > 99 then
    raise EOBDRoutineError.CreateFmt(
      'BCD value out of range: %d', [B]);
  Result := ((B div 10) shl 4) or (B mod 10);
end;

procedure TOBDRoutineRequestBuilder.AddBcdDate(
  const Year, Month, Day: Byte);
begin
  AppendByte(ByteToBcd(Year));
  AppendByte(ByteToBcd(Month));
  AppendByte(ByteToBcd(Day));
end;

procedure TOBDRoutineRequestBuilder.AddBcdYear(const Year: Byte);
begin
  AppendByte(ByteToBcd(Year));
end;

function TOBDRoutineRequestBuilder.PayloadBytes: TBytes;
begin
  Result := Copy(FBytes, 0, Length(FBytes));
end;

function TOBDRoutineRequestBuilder.ToFrame(
  const SubFunction: TOBDRoutineSubFunction;
  const RID: Word): TBytes;
var
  Header: TBytes;
begin
  Header := TBytes.Create($31, Byte(SubFunction),
    Byte(RID shr 8), Byte(RID and $FF));
  Result := BytesAppend(Header, FBytes);
end;

procedure TOBDRoutineRequestBuilder.Clear;
begin
  SetLength(FBytes, 0);
end;

{ TOBDRoutineResponseReader }

class function TOBDRoutineResponseReader.Wrap(
  const Bytes: TBytes): TOBDRoutineResponseReader;
begin
  Result.FBytes := Bytes;
  Result.FCursor := 0;
end;

procedure TOBDRoutineResponseReader.RequireBytes(const Count: Integer);
begin
  if FCursor + Count > Length(FBytes) then
    raise EOBDRoutineError.CreateFmt(
      'Routine response under-read at offset %d (need %d, have %d)',
      [FCursor, Count, Length(FBytes) - FCursor]);
end;

function TOBDRoutineResponseReader.ReadUInt8: Byte;
begin
  RequireBytes(1);
  Result := FBytes[FCursor];
  Inc(FCursor);
end;

function TOBDRoutineResponseReader.ReadUInt16BE: Word;
begin
  RequireBytes(2);
  Result := (Word(FBytes[FCursor]) shl 8) or FBytes[FCursor + 1];
  Inc(FCursor, 2);
end;

function TOBDRoutineResponseReader.ReadUInt32BE: Cardinal;
begin
  RequireBytes(4);
  Result := (Cardinal(FBytes[FCursor]) shl 24) or
            (Cardinal(FBytes[FCursor + 1]) shl 16) or
            (Cardinal(FBytes[FCursor + 2]) shl 8) or
             Cardinal(FBytes[FCursor + 3]);
  Inc(FCursor, 4);
end;

function TOBDRoutineResponseReader.ReadInt16BE: SmallInt;
begin
  Result := SmallInt(ReadUInt16BE);
end;

function TOBDRoutineResponseReader.ReadInt32BE: Integer;
begin
  Result := Integer(ReadUInt32BE);
end;

function TOBDRoutineResponseReader.ReadAscii(
  const ByteCount: Integer): string;
var
  Slice: TBytes;
begin
  RequireBytes(ByteCount);
  SetLength(Slice, ByteCount);
  if ByteCount > 0 then
    Move(FBytes[FCursor], Slice[0], ByteCount);
  Inc(FCursor, ByteCount);
  Result := TEncoding.ASCII.GetString(Slice);
  Result := Result.TrimRight([#0]);
end;

function TOBDRoutineResponseReader.ReadHexBytes(
  const ByteCount: Integer): TBytes;
begin
  RequireBytes(ByteCount);
  SetLength(Result, ByteCount);
  if ByteCount > 0 then
    Move(FBytes[FCursor], Result[0], ByteCount);
  Inc(FCursor, ByteCount);
end;

function TOBDRoutineResponseReader.ReadBcdDate: string;
begin
  RequireBytes(3);
  Result := Format('20%.2x-%.2x-%.2x',
    [FBytes[FCursor], FBytes[FCursor + 1], FBytes[FCursor + 2]]);
  Inc(FCursor, 3);
end;

function TOBDRoutineResponseReader.HasMore: Boolean;
begin
  Result := FCursor < Length(FBytes);
end;

function TOBDRoutineResponseReader.Remaining: Integer;
begin
  Result := Length(FBytes) - FCursor;
end;

function BuildStartRoutine(const RID: Word;
  const InputData: TBytes): TBytes;
var
  Header: TBytes;
begin
  Header := TBytes.Create($31, $01,
    Byte(RID shr 8), Byte(RID and $FF));
  Result := BytesAppend(Header, InputData);
end;

function BuildStopRoutine(const RID: Word): TBytes;
begin
  Result := TBytes.Create($31, $02,
    Byte(RID shr 8), Byte(RID and $FF));
end;

function BuildRequestRoutineResults(const RID: Word): TBytes;
begin
  Result := TBytes.Create($31, $03,
    Byte(RID shr 8), Byte(RID and $FF));
end;

function ParseRoutineResponse(const Response: TBytes;
  const ExpectedSF: TOBDRoutineSubFunction;
  const ExpectedRID: Word): TBytes;
var
  GotRID: Word;
begin
  if Length(Response) < 4 then
  begin
    if (Length(Response) >= 3) and (Response[0] = $7F) and
       (Response[1] = $31) then
      raise EOBDRoutineError.CreateFmt(
        'Negative routine response: NRC 0x%.2X', [Response[2]]);
    raise EOBDRoutineError.CreateFmt(
      'Routine response too short: %d bytes (need at least 4)',
      [Length(Response)]);
  end;
  if Response[0] = $7F then
  begin
    if Response[1] <> $31 then
      raise EOBDRoutineError.CreateFmt(
        'Negative response targets the wrong SID 0x%.2X (expected 0x31)',
        [Response[1]]);
    raise EOBDRoutineError.CreateFmt(
      'Negative routine response: NRC 0x%.2X', [Response[2]]);
  end;
  if Response[0] <> $71 then
    raise EOBDRoutineError.CreateFmt(
      'Unexpected response SID 0x%.2X (want 0x71)', [Response[0]]);
  if Response[1] <> Byte(ExpectedSF) then
    raise EOBDRoutineError.CreateFmt(
      'Routine response SF mismatch: got 0x%.2X, expected 0x%.2X',
      [Response[1], Byte(ExpectedSF)]);
  GotRID := (Word(Response[2]) shl 8) or Response[3];
  if GotRID <> ExpectedRID then
    raise EOBDRoutineError.CreateFmt(
      'Routine response RID mismatch: got 0x%.4X, expected 0x%.4X',
      [GotRID, ExpectedRID]);
  Result := Copy(Response, 4, Length(Response) - 4);
end;

function DecodeField(const Field: TOBDRoutineField;
  var Reader: TOBDRoutineResponseReader): TOBDDecodedField;
var
  StartCursor, BitCount, Bit: Integer;
  N: UInt64;
  Lookup: string;
  Names: TArray<string>;
  Nm: string;
begin
  Result := Default(TOBDDecodedField);
  Result.Name := Field.Name;
  StartCursor := Reader.Cursor;
  case Field.Kind of
    rfkUInt8:
      Result.Display := Format('%s = %s',
        [Field.Name, FormatNumeric(Reader.ReadUInt8, Field)]);
    rfkUInt16BE:
      Result.Display := Format('%s = %s',
        [Field.Name, FormatNumeric(Reader.ReadUInt16BE, Field)]);
    rfkUInt32BE:
      Result.Display := Format('%s = %s',
        [Field.Name, FormatNumeric(Reader.ReadUInt32BE, Field)]);
    rfkInt8:
      Result.Display := Format('%s = %s',
        [Field.Name,
         FormatNumeric(SmallInt(ShortInt(Reader.ReadUInt8)), Field)]);
    rfkInt16BE:
      Result.Display := Format('%s = %s',
        [Field.Name, FormatNumeric(Reader.ReadInt16BE, Field)]);
    rfkInt32BE:
      Result.Display := Format('%s = %s',
        [Field.Name, FormatNumeric(Reader.ReadInt32BE, Field)]);
    rfkAscii:
      Result.Display := Format('%s = "%s"',
        [Field.Name, Reader.ReadAscii(Field.Size)]);
    rfkHex:
      Result.Display := Format('%s = %s',
        [Field.Name,
         BytesToHexString(Reader.ReadHexBytes(Field.Size), ' ')]);
    rfkBcdDate:
      Result.Display := Format('%s = %s',
        [Field.Name, Reader.ReadBcdDate]);
    rfkEnum:
      begin
        case Field.Size of
          1: N := Reader.ReadUInt8;
          2: N := Reader.ReadUInt16BE;
          4: N := Reader.ReadUInt32BE;
        else
          raise EOBDRoutineError.CreateFmt(
            'Enum field "%s" has unsupported size %d',
            [Field.Name, Field.Size]);
        end;
        if Assigned(Field.EnumValues) and
           Field.EnumValues.TryGetValue(N, Lookup) then
          Result.Display := Format('%s = %s',
            [Field.Name, Lookup])
        else
          Result.Display := Format('%s = 0x%.*x',
            [Field.Name, Field.Size * 2, N]);
      end;
    rfkBitmask:
      begin
        case Field.Size of
          1: N := Reader.ReadUInt8;
          2: N := Reader.ReadUInt16BE;
          4: N := Reader.ReadUInt32BE;
        else
          raise EOBDRoutineError.CreateFmt(
            'Bitmask field "%s" has unsupported size %d',
            [Field.Name, Field.Size]);
        end;
        SetLength(Names, 0);
        BitCount := Field.Size * 8;
        for Bit := 0 to BitCount - 1 do
          if (N and (UInt64(1) shl Bit)) <> 0 then
          begin
            Nm := '';
            if Assigned(Field.BitNames) then
              Field.BitNames.TryGetValue(Bit, Nm);
            if Nm = '' then
              Nm := Format('bit_%d', [Bit]);
            Names := Names + [Nm];
          end;
        Result.Display := Format('%s = %s',
          [Field.Name, string.Join(',', Names)]);
      end;
  else
    raise EOBDRoutineError.CreateFmt(
      'Unknown routine field kind for "%s"', [Field.Name]);
  end;
  Result.Raw := Copy(Reader.FBytes, StartCursor,
    Reader.Cursor - StartCursor);
end;

function DecodeRoutineOutput(const Schema: TOBDRoutineSchema;
  const Bytes: TBytes): TArray<TOBDDecodedField>;
var
  Reader: TOBDRoutineResponseReader;
  Decoded: TList<TOBDDecodedField>;
  F: TOBDRoutineField;
begin
  Reader := TOBDRoutineResponseReader.Wrap(Bytes);
  Decoded := TList<TOBDDecodedField>.Create;
  try
    for F in Schema.OutputFields do
    begin
      if Reader.Remaining = 0 then
        Break;
      Decoded.Add(DecodeField(F, Reader));
    end;
    Result := Decoded.ToArray;
  finally
    Decoded.Free;
  end;
end;

end.
