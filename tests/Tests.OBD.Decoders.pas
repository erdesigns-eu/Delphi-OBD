//------------------------------------------------------------------------------
//  Tests.OBD.Decoders
//
//  DUnitX coverage for the built-in scaling primitives in OBD.Decoders.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial coverage: linear, percentage,
//                     temperature, fueltrim, rpm, speed, maf, ascii,
//                     bitfield, raw — including under-length payloads
//                     and clamping.
//------------------------------------------------------------------------------

unit Tests.OBD.Decoders;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   Built-in decoder coverage. Asserts both the canonical
  ///   high-school formulas and the under-length / boundary cases that
  ///   tend to break in real wire data.
  /// </summary>
  [TestFixture]
  TDecoderTests = class
  public
    /// <summary>Linear decoder applies <c>scale</c> + <c>offset</c>
    /// using big-endian byte order.</summary>
    [Test] procedure LinearOneByte;
    /// <summary>Linear decoder reads two bytes big-endian.</summary>
    [Test] procedure LinearTwoBytesBigEndian;
    /// <summary>Linear decoder honours descriptor offset (e.g. timing
    /// advance).</summary>
    [Test] procedure LinearWithOffset;
    /// <summary>Percentage decoder maps 255 → 100%, 0 → 0%, 128 → ~50%.</summary>
    [Test] procedure PercentageMidpointAndExtremes;
    /// <summary>Temperature decoder applies the ISO 15031 -40 offset.</summary>
    [Test] procedure TemperatureOffsetIsApplied;
    /// <summary>Fuel trim decoder is symmetric around 128.</summary>
    [Test] procedure FuelTrimNeutralIsZero;
    /// <summary>RPM decoder scales <c>(256A + B)/4</c>.</summary>
    [Test] procedure RPMScale;
    /// <summary>Vehicle speed decoder is identity-in-km/h.</summary>
    [Test] procedure SpeedIdentity;
    /// <summary>MAF decoder scales <c>(256A + B)/100</c>.</summary>
    [Test] procedure MAFScale;
    /// <summary>ASCII decoder strips trailing NUL / space pad bytes.</summary>
    [Test] procedure ASCIIStripsPad;
    /// <summary>Bitfield decoder packs up to eight bytes big-endian.</summary>
    [Test] procedure BitfieldBigEndian;
    /// <summary>Raw decoder copies bytes and sets <c>vkRawOnly</c>.</summary>
    [Test] procedure RawPassthrough;
    /// <summary>Under-length payload yields <c>vkEmpty</c> rather than
    /// raising.</summary>
    [Test] procedure UnderLengthYieldsEmpty;
    /// <summary>Min/Max clamp applied when both are non-zero.</summary>
    [Test] procedure ClampApplied;
    /// <summary>Registry lookup is case-insensitive.</summary>
    [Test] procedure RegistryIsCaseInsensitive;
    /// <summary>Registering a nil decoder raises <c>EOBDConfig</c>.</summary>
    [Test] procedure RegisterNilRaises;
  end;

implementation

uses
  System.SysUtils,
  OBD.Types,
  OBD.Decoders;

function MakeDescriptor(const ADecoder: string; ALength: Byte = 1;
  AScale: Double = 1; AOffset: Double = 0;
  AMin: Double = 0; AMax: Double = 0): TOBDPIDDescriptor;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.DecoderName := ADecoder;
  Result.Length := ALength;
  Result.Scale := AScale;
  Result.Offset := AOffset;
  Result.Min := AMin;
  Result.Max := AMax;
end;

procedure TDecoderTests.LinearOneByte;
var
  V: TOBDValue;
begin
  V := DecodeLinear(TBytes.Create(100), MakeDescriptor('linear', 1, 1, 0));
  Assert.AreEqual(Ord(vkFloat), Ord(V.Kind));
  Assert.AreEqual(100.0, V.AsFloat, 1E-9);
end;

procedure TDecoderTests.LinearTwoBytesBigEndian;
var
  V: TOBDValue;
begin
  // 0x0102 = 258
  V := DecodeLinear(TBytes.Create($01, $02), MakeDescriptor('linear', 2, 1, 0));
  Assert.AreEqual(258.0, V.AsFloat, 1E-9);
end;

procedure TDecoderTests.LinearWithOffset;
var
  V: TOBDValue;
begin
  // PID 0x0E timing advance: 0.5 * A - 64
  V := DecodeLinear(TBytes.Create(192), MakeDescriptor('linear', 1, 0.5, -64));
  Assert.AreEqual(32.0, V.AsFloat, 1E-9);
end;

procedure TDecoderTests.PercentageMidpointAndExtremes;
var
  V: TOBDValue;
begin
  V := DecodePercentage(TBytes.Create(0), MakeDescriptor('percentage'));
  Assert.AreEqual(0.0, V.AsFloat, 1E-9);

  V := DecodePercentage(TBytes.Create(255), MakeDescriptor('percentage'));
  Assert.AreEqual(100.0, V.AsFloat, 1E-9);

  V := DecodePercentage(TBytes.Create(128), MakeDescriptor('percentage'));
  Assert.IsTrue((V.AsFloat > 50.1) and (V.AsFloat < 50.3));
  Assert.AreEqual('%', V.UnitName);
end;

procedure TDecoderTests.TemperatureOffsetIsApplied;
var
  V: TOBDValue;
begin
  V := DecodeTemperature(TBytes.Create(40), MakeDescriptor('temperature'));
  Assert.AreEqual(Ord(vkInteger), Ord(V.Kind));
  Assert.AreEqual<Int64>(0, V.AsInteger);
  Assert.AreEqual('°C', V.UnitName);

  V := DecodeTemperature(TBytes.Create(0), MakeDescriptor('temperature'));
  Assert.AreEqual<Int64>(-40, V.AsInteger);

  V := DecodeTemperature(TBytes.Create(255), MakeDescriptor('temperature'));
  Assert.AreEqual<Int64>(215, V.AsInteger);
end;

procedure TDecoderTests.FuelTrimNeutralIsZero;
var
  V: TOBDValue;
begin
  V := DecodeFuelTrim(TBytes.Create(128), MakeDescriptor('fueltrim'));
  Assert.AreEqual(0.0, V.AsFloat, 1E-9);

  V := DecodeFuelTrim(TBytes.Create(0), MakeDescriptor('fueltrim'));
  Assert.AreEqual(-100.0, V.AsFloat, 1E-9);
end;

procedure TDecoderTests.RPMScale;
var
  V: TOBDValue;
begin
  // 0x0AF8 = 2808 → 702 rpm
  V := DecodeRPM(TBytes.Create($0A, $F8), MakeDescriptor('rpm', 2));
  Assert.AreEqual(702.0, V.AsFloat, 1E-9);
  Assert.AreEqual('rpm', V.UnitName);
end;

procedure TDecoderTests.SpeedIdentity;
var
  V: TOBDValue;
begin
  V := DecodeSpeed(TBytes.Create(60), MakeDescriptor('speed'));
  Assert.AreEqual<Int64>(60, V.AsInteger);
  Assert.AreEqual('km/h', V.UnitName);
end;

procedure TDecoderTests.MAFScale;
var
  V: TOBDValue;
begin
  // 0x0BB8 = 3000 → 30.0 g/s
  V := DecodeMAF(TBytes.Create($0B, $B8), MakeDescriptor('maf', 2));
  Assert.AreEqual(30.0, V.AsFloat, 1E-9);
end;

procedure TDecoderTests.ASCIIStripsPad;
var
  V: TOBDValue;
begin
  V := DecodeASCII(TBytes.Create(Ord('V'), Ord('I'), Ord('N'), 0, 0),
    MakeDescriptor('ascii', 5));
  Assert.AreEqual(Ord(vkString), Ord(V.Kind));
  Assert.AreEqual('VIN', V.AsString);
end;

procedure TDecoderTests.BitfieldBigEndian;
var
  V: TOBDValue;
begin
  V := DecodeBitField(TBytes.Create($80, $00, $00, $00),
    MakeDescriptor('bitfield', 4));
  Assert.AreEqual(Ord(vkBitField), Ord(V.Kind));
  Assert.AreEqual<UInt64>($80000000, V.AsBitField);
end;

procedure TDecoderTests.RawPassthrough;
var
  V: TOBDValue;
begin
  V := DecodeRaw(TBytes.Create(1, 2, 3), MakeDescriptor('raw', 3));
  Assert.AreEqual(Ord(vkRawOnly), Ord(V.Kind));
  Assert.AreEqual<NativeInt>(3, Length(V.Raw));
  Assert.AreEqual<Byte>(2, V.Raw[1]);
end;

procedure TDecoderTests.UnderLengthYieldsEmpty;
var
  V: TOBDValue;
begin
  V := DecodeRPM(TBytes.Create($0A), MakeDescriptor('rpm', 2));
  Assert.AreEqual(Ord(vkEmpty), Ord(V.Kind));

  V := DecodePercentage(nil, MakeDescriptor('percentage'));
  Assert.AreEqual(Ord(vkEmpty), Ord(V.Kind));
end;

procedure TDecoderTests.ClampApplied;
var
  V: TOBDValue;
  Desc: TOBDPIDDescriptor;
begin
  Desc := MakeDescriptor('linear', 1, 1, 0, 10, 50);
  V := DecodeLinear(TBytes.Create(200), Desc);
  Assert.AreEqual(50.0, V.AsFloat, 1E-9);
  V := DecodeLinear(TBytes.Create(0), Desc);
  Assert.AreEqual(10.0, V.AsFloat, 1E-9);
end;

procedure TDecoderTests.RegistryIsCaseInsensitive;
var
  Decoder: TOBDDecoderFunc;
begin
  Assert.IsTrue(TOBDDecoderRegistry.Default.TryGet('LINEAR', Decoder));
  Assert.IsTrue(TOBDDecoderRegistry.Default.TryGet(' Linear ', Decoder));
end;

procedure TDecoderTests.RegisterNilRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      TOBDDecoderRegistry.Default.Register('zzz', nil);
    end,
    EOBDConfig);
end;

initialization
  TDUnitX.RegisterTestFixture(TDecoderTests);

end.
