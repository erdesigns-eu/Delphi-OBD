//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.RoutineControl
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.RoutineControl;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TRequestBuilderTests = class
  public
    /// <summary>Uint8 and uint16 b e encode big endian.</summary>
    [Test] procedure Uint8AndUint16BEEncodeBigEndian;
    /// <summary>Int32 b e encodes negative.</summary>
    [Test] procedure Int32BEEncodesNegative;
    /// <summary>Ascii pads and rejects too long.</summary>
    [Test] procedure AsciiPadsAndRejectsTooLong;
    /// <summary>Bcd date encodes year month day.</summary>
    [Test] procedure BcdDateEncodesYearMonthDay;
    /// <summary>Bcd year rejects out of range.</summary>
    [Test] procedure BcdYearRejectsOutOfRange;
    /// <summary>To frame wraps with sid and rid.</summary>
    [Test] procedure ToFrameWrapsWithSidAndRid;
    /// <summary>Clear resets builder.</summary>
    [Test] procedure ClearResetsBuilder;
  end;

  [TestFixture]
  TResponseReaderTests = class
  public
    /// <summary>Reads big endian multi byte.</summary>
    [Test] procedure ReadsBigEndianMultiByte;
    /// <summary>Reads ascii and strips zero pad.</summary>
    [Test] procedure ReadsAsciiAndStripsZeroPad;
    /// <summary>Reads bcd date.</summary>
    [Test] procedure ReadsBcdDate;
    /// <summary>Reads hex slice.</summary>
    [Test] procedure ReadsHexSlice;
    /// <summary>Under read raises.</summary>
    [Test] procedure UnderReadRaises;
    /// <summary>Has more reflects cursor.</summary>
    [Test] procedure HasMoreReflectsCursor;
  end;

  [TestFixture]
  TWireFrameTests = class
  public
    /// <summary>Build start routine without data.</summary>
    [Test] procedure BuildStartRoutineWithoutData;
    /// <summary>Build start routine appends data.</summary>
    [Test] procedure BuildStartRoutineAppendsData;
    /// <summary>Build stop and request results.</summary>
    [Test] procedure BuildStopAndRequestResults;
    /// <summary>Parse accepts positive response.</summary>
    [Test] procedure ParseAcceptsPositiveResponse;
    /// <summary>Parse rejects wrong s i d.</summary>
    [Test] procedure ParseRejectsWrongSID;
    /// <summary>Parse rejects wrong sub function.</summary>
    [Test] procedure ParseRejectsWrongSubFunction;
    /// <summary>Parse rejects wrong r i d.</summary>
    [Test] procedure ParseRejectsWrongRID;
    /// <summary>Parse raises on negative response.</summary>
    [Test] procedure ParseRaisesOnNegativeResponse;
    /// <summary>Parse handles empty status payload.</summary>
    [Test] procedure ParseHandlesEmptyStatusPayload;
  end;

  [TestFixture]
  TSchemaDecodeTests = class
  public
    /// <summary>Decodes u int8 with scale and offset.</summary>
    [Test] procedure DecodesUInt8WithScaleAndOffset;
    /// <summary>Decodes ascii and u int32.</summary>
    [Test] procedure DecodesAsciiAndUInt32;
    /// <summary>Decodes bitmask with named bits.</summary>
    [Test] procedure DecodesBitmaskWithNamedBits;
    /// <summary>Decodes enum with fallback.</summary>
    [Test] procedure DecodesEnumWithFallback;
    /// <summary>Stops on truncated response.</summary>
    [Test] procedure StopsOnTruncatedResponse;
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections,
  OBD.OEM.RoutineControl;

//==============================================================================
// Builder
//==============================================================================
procedure TRequestBuilderTests.Uint8AndUint16BEEncodeBigEndian;
var
  Builder: TOBDRoutineRequestBuilder;
  Bytes: TBytes;
begin
  Builder.AddUInt8($AB);
  Builder.AddUInt16BE($1234);
  Bytes := Builder.PayloadBytes;
  Assert.AreEqual(3, Length(Bytes));
  Assert.AreEqual(Byte($AB), Bytes[0]);
  Assert.AreEqual(Byte($12), Bytes[1]);
  Assert.AreEqual(Byte($34), Bytes[2]);
end;

procedure TRequestBuilderTests.Int32BEEncodesNegative;
var
  Builder: TOBDRoutineRequestBuilder;
  Bytes: TBytes;
begin
  Builder.AddInt32BE(-1);
  Bytes := Builder.PayloadBytes;
  Assert.AreEqual(4, Length(Bytes));
  Assert.AreEqual(Byte($FF), Bytes[0]);
  Assert.AreEqual(Byte($FF), Bytes[3]);
end;

procedure TRequestBuilderTests.AsciiPadsAndRejectsTooLong;
var
  Builder: TOBDRoutineRequestBuilder;
  Bytes: TBytes;
begin
  Builder.AddAscii('VW', 4);
  Bytes := Builder.PayloadBytes;
  Assert.AreEqual(4, Length(Bytes));
  Assert.AreEqual(Byte(Ord('V')), Bytes[0]);
  Assert.AreEqual(Byte(Ord('W')), Bytes[1]);
  Assert.AreEqual(Byte($00), Bytes[2]);
  Assert.AreEqual(Byte($00), Bytes[3]);

  Assert.WillRaise(
    procedure
    var B: TOBDRoutineRequestBuilder;
    begin B.AddAscii('TOOLONG', 4); end,
    EOBDRoutineError);
end;

procedure TRequestBuilderTests.BcdDateEncodesYearMonthDay;
var
  Builder: TOBDRoutineRequestBuilder;
  Bytes: TBytes;
begin
  Builder.AddBcdDate(25, 11, 7);   // 2025-11-07 → 25 11 07 BCD
  Bytes := Builder.PayloadBytes;
  Assert.AreEqual(Byte($25), Bytes[0]);
  Assert.AreEqual(Byte($11), Bytes[1]);
  Assert.AreEqual(Byte($07), Bytes[2]);
end;

procedure TRequestBuilderTests.BcdYearRejectsOutOfRange;
begin
  Assert.WillRaise(
    procedure
    var B: TOBDRoutineRequestBuilder;
    begin B.AddBcdYear(150); end,
    EOBDRoutineError);
end;

procedure TRequestBuilderTests.ToFrameWrapsWithSidAndRid;
var
  Builder: TOBDRoutineRequestBuilder;
  Frame: TBytes;
begin
  Builder.AddUInt8($AA);
  Frame := Builder.ToFrame(rcStart, $0203);
  Assert.AreEqual(5, Length(Frame));
  Assert.AreEqual(Byte($31), Frame[0]);
  Assert.AreEqual(Byte($01), Frame[1]);
  Assert.AreEqual(Byte($02), Frame[2]);
  Assert.AreEqual(Byte($03), Frame[3]);
  Assert.AreEqual(Byte($AA), Frame[4]);
end;

procedure TRequestBuilderTests.ClearResetsBuilder;
var
  Builder: TOBDRoutineRequestBuilder;
begin
  Builder.AddUInt32BE($DEADBEEF);
  Builder.Clear;
  Assert.AreEqual(0, Length(Builder.PayloadBytes));
end;

//==============================================================================
// Reader
//==============================================================================
procedure TResponseReaderTests.ReadsBigEndianMultiByte;
var
  R: TOBDRoutineResponseReader;
begin
  R := TOBDRoutineResponseReader.Wrap(
    TBytes.Create($AB, $12, $34, $56, $78));
  Assert.AreEqual(Byte($AB), R.ReadUInt8);
  Assert.AreEqual(Word($1234), R.ReadUInt16BE);
  Assert.AreEqual(Word($5678), R.ReadUInt16BE);
end;

procedure TResponseReaderTests.ReadsAsciiAndStripsZeroPad;
var
  R: TOBDRoutineResponseReader;
begin
  R := TOBDRoutineResponseReader.Wrap(
    TBytes.Create(Ord('V'), Ord('W'), $00, $00));
  Assert.AreEqual('VW', R.ReadAscii(4));
end;

procedure TResponseReaderTests.ReadsBcdDate;
var
  R: TOBDRoutineResponseReader;
begin
  R := TOBDRoutineResponseReader.Wrap(TBytes.Create($25, $03, $14));
  Assert.AreEqual('2025-03-14', R.ReadBcdDate);
end;

procedure TResponseReaderTests.ReadsHexSlice;
var
  R: TOBDRoutineResponseReader;
  Slice: TBytes;
begin
  R := TOBDRoutineResponseReader.Wrap(
    TBytes.Create($AB, $CD, $EF, $11));
  Slice := R.ReadHexBytes(3);
  Assert.AreEqual(3, Length(Slice));
  Assert.AreEqual(Byte($AB), Slice[0]);
  Assert.AreEqual(Byte($EF), Slice[2]);
  Assert.AreEqual(1, R.Remaining);
end;

procedure TResponseReaderTests.UnderReadRaises;
var
  R: TOBDRoutineResponseReader;
begin
  R := TOBDRoutineResponseReader.Wrap(TBytes.Create($AB));
  Assert.WillRaise(
    procedure begin R.ReadUInt32BE; end,
    EOBDRoutineError);
end;

procedure TResponseReaderTests.HasMoreReflectsCursor;
var
  R: TOBDRoutineResponseReader;
begin
  R := TOBDRoutineResponseReader.Wrap(TBytes.Create($AB, $CD));
  Assert.IsTrue(R.HasMore);
  R.ReadUInt16BE;
  Assert.IsFalse(R.HasMore);
end;

//==============================================================================
// Wire frames
//==============================================================================
procedure TWireFrameTests.BuildStartRoutineWithoutData;
var
  F: TBytes;
begin
  F := BuildStartRoutine($0203);
  Assert.AreEqual(4, Length(F));
  Assert.AreEqual(Byte($31), F[0]);
  Assert.AreEqual(Byte($01), F[1]);
  Assert.AreEqual(Byte($02), F[2]);
  Assert.AreEqual(Byte($03), F[3]);
end;

procedure TWireFrameTests.BuildStartRoutineAppendsData;
var
  F: TBytes;
begin
  F := BuildStartRoutine($FF00, TBytes.Create($DE, $AD));
  Assert.AreEqual(6, Length(F));
  Assert.AreEqual(Byte($DE), F[4]);
  Assert.AreEqual(Byte($AD), F[5]);
end;

procedure TWireFrameTests.BuildStopAndRequestResults;
var
  Stop, Req: TBytes;
begin
  Stop := BuildStopRoutine($0301);
  Req := BuildRequestRoutineResults($0301);
  Assert.AreEqual(Byte($02), Stop[1]);
  Assert.AreEqual(Byte($03), Req[1]);
end;

procedure TWireFrameTests.ParseAcceptsPositiveResponse;
var
  Status: TBytes;
begin
  Status := ParseRoutineResponse(
    TBytes.Create($71, $01, $02, $03, $00, $11, $22),
    rcStart, $0203);
  Assert.AreEqual(3, Length(Status));
  Assert.AreEqual(Byte($00), Status[0]);
  Assert.AreEqual(Byte($22), Status[2]);
end;

procedure TWireFrameTests.ParseRejectsWrongSID;
begin
  Assert.WillRaise(
    procedure begin
      ParseRoutineResponse(TBytes.Create($72, $01, $02, $03),
        rcStart, $0203);
    end,
    EOBDRoutineError);
end;

procedure TWireFrameTests.ParseRejectsWrongSubFunction;
begin
  Assert.WillRaise(
    procedure begin
      ParseRoutineResponse(TBytes.Create($71, $02, $02, $03),
        rcStart, $0203);
    end,
    EOBDRoutineError);
end;

procedure TWireFrameTests.ParseRejectsWrongRID;
begin
  Assert.WillRaise(
    procedure begin
      ParseRoutineResponse(TBytes.Create($71, $01, $03, $03),
        rcStart, $0203);
    end,
    EOBDRoutineError);
end;

procedure TWireFrameTests.ParseRaisesOnNegativeResponse;
begin
  Assert.WillRaise(
    procedure begin
      // 7F 31 33 — securityAccessDenied (NRC 0x33).
      ParseRoutineResponse(TBytes.Create($7F, $31, $33),
        rcStart, $0203);
    end,
    EOBDRoutineError);
end;

procedure TWireFrameTests.ParseHandlesEmptyStatusPayload;
var
  Status: TBytes;
begin
  Status := ParseRoutineResponse(TBytes.Create($71, $01, $02, $03),
    rcStart, $0203);
  Assert.AreEqual(0, Length(Status));
end;

//==============================================================================
// Schema decode
//==============================================================================
function MakeField(const Name: string; const Kind: TOBDRoutineFieldKind;
  const Size: Integer = 0; const Scale: Double = 1.0;
  const Offset: Double = 0.0; const Unit_: string = ''): TOBDRoutineField;
begin
  Result := Default(TOBDRoutineField);
  Result.Name := Name;
  Result.Kind := Kind;
  Result.Size := Size;
  Result.Scale := Scale;
  Result.Offset := Offset;
  Result.Unit_ := Unit_;
end;

procedure TSchemaDecodeTests.DecodesUInt8WithScaleAndOffset;
var
  Schema: TOBDRoutineSchema;
  Decoded: TArray<TOBDDecodedField>;
begin
  Schema := Default(TOBDRoutineSchema);
  Schema.OutputFields := [MakeField('temp', rfkUInt8, 0, 0.5, -10, 'C')];
  Decoded := DecodeRoutineOutput(Schema, TBytes.Create(40));
  Assert.AreEqual(1, Length(Decoded));
  Assert.AreEqual('temp = 10 C', Decoded[0].Display);
end;

procedure TSchemaDecodeTests.DecodesAsciiAndUInt32;
var
  Schema: TOBDRoutineSchema;
  Decoded: TArray<TOBDDecodedField>;
begin
  Schema := Default(TOBDRoutineSchema);
  Schema.OutputFields := [
    MakeField('part_no', rfkAscii, 4),
    MakeField('mileage', rfkUInt32BE, 0, 1.0, 0.0, 'km')
  ];
  Decoded := DecodeRoutineOutput(Schema,
    TBytes.Create(Ord('A'), Ord('B'), Ord('C'), Ord('D'),
      $00, $01, $86, $A0));   // 100000 km
  Assert.AreEqual(2, Length(Decoded));
  Assert.AreEqual('part_no = "ABCD"', Decoded[0].Display);
  Assert.AreEqual('mileage = 100000 km', Decoded[1].Display);
end;

procedure TSchemaDecodeTests.DecodesBitmaskWithNamedBits;
var
  Field: TOBDRoutineField;
  Schema: TOBDRoutineSchema;
  Decoded: TArray<TOBDDecodedField>;
begin
  Field := MakeField('flags', rfkBitmask, 1);
  Field.BitNames := TDictionary<Integer, string>.Create;
  Field.BitNames.AddOrSetValue(0, 'ready');
  Field.BitNames.AddOrSetValue(1, 'running');
  Field.BitNames.AddOrSetValue(3, 'fault');
  try
    Schema := Default(TOBDRoutineSchema);
    Schema.OutputFields := [Field];
    Decoded := DecodeRoutineOutput(Schema, TBytes.Create($0B));
    Assert.AreEqual('flags = ready,running,fault', Decoded[0].Display);
  finally
    Field.BitNames.Free;
  end;
end;

procedure TSchemaDecodeTests.DecodesEnumWithFallback;
var
  Field: TOBDRoutineField;
  Schema: TOBDRoutineSchema;
  Decoded: TArray<TOBDDecodedField>;
begin
  Field := MakeField('state', rfkEnum, 1);
  Field.EnumValues := TDictionary<Cardinal, string>.Create;
  Field.EnumValues.AddOrSetValue($01, 'pass');
  Field.EnumValues.AddOrSetValue($02, 'fail');
  try
    Schema := Default(TOBDRoutineSchema);
    Schema.OutputFields := [Field];
    Decoded := DecodeRoutineOutput(Schema, TBytes.Create($02));
    Assert.AreEqual('state = fail', Decoded[0].Display);
    Decoded := DecodeRoutineOutput(Schema, TBytes.Create($99));
    Assert.AreEqual('state = 0x99', Decoded[0].Display);
  finally
    Field.EnumValues.Free;
  end;
end;

procedure TSchemaDecodeTests.StopsOnTruncatedResponse;
var
  Schema: TOBDRoutineSchema;
  Decoded: TArray<TOBDDecodedField>;
begin
  Schema := Default(TOBDRoutineSchema);
  Schema.OutputFields := [
    MakeField('first', rfkUInt8),
    MakeField('second', rfkUInt8),
    MakeField('third', rfkUInt8)
  ];
  Decoded := DecodeRoutineOutput(Schema, TBytes.Create($AA, $BB));
  Assert.AreEqual(2, Length(Decoded), 'truncated response decodes prefix only');
end;

initialization
  TDUnitX.RegisterTestFixture(TRequestBuilderTests);
  TDUnitX.RegisterTestFixture(TResponseReaderTests);
  TDUnitX.RegisterTestFixture(TWireFrameTests);
  TDUnitX.RegisterTestFixture(TSchemaDecodeTests);

end.
