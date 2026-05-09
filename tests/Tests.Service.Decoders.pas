//------------------------------------------------------------------------------
// UNIT           : Tests.Service.Decoders
// CONTENTS       : Golden tests for OBD-II Service response decoders (PIDs)
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Inputs and expected outputs follow SAE J1979 PID formulas:
//                    Coolant temp (PID 05):  A − 40   °C
//                    Throttle pos (PID 11):  (A/255)*100   %
//                    Fuel trim   (PID 06): (A/1.28)−100   %
//                    Fuel pres.  (PID 0A):  A * 3   kPa
//                    RPM         (PID 0C): ((A*256)+B)/4   rpm
//                    Timing adv. (PID 0E):  (A/2)−64   °
//                    MAF         (PID 10): ((A*256)+B)/100 g/s
//------------------------------------------------------------------------------
unit Tests.Service.Decoders;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TServiceResponseDispatchTests = class
  public
    [Test] procedure Dispatch_PositiveServiceResponse_ExtractsServiceAndPID;
    [Test] procedure Dispatch_NegativeResponse_FlagsError;
    [Test] procedure Dispatch_TooShortResponse_Fails;
    [Test] procedure Dispatch_Service03_HasNoPID_ButCarriesData;
  end;

  [TestFixture]
  TPidDecoderTests = class
  public
    // ---- Percentage (PID 11 etc.) ----
    [Test]
    [TestCase('Zero',     '0,0')]
    [TestCase('Half',     '128,50.196078431372548')]
    [TestCase('Full',     '255,100')]
    procedure Percentage_KnownInputs(const Byte0: Integer; const ExpectedPercent: Double);

    [Test]
    procedure Percentage_EmptyData_Fails;

    // ---- Temperature (PID 05) ----
    [Test]
    [TestCase('FreezingMinus40',  '0,-40')]
    [TestCase('Coolant_83C',      '123,83')]
    [TestCase('Max',              '255,215')]
    procedure Temperature_KnownInputs(const Byte0, ExpectedC: Integer);

    // ---- Fuel trim (PID 06) ----
    [Test]
    [TestCase('Center_0pct',  '128,0')]
    [TestCase('Min',          '0,-100')]
    procedure FuelTrim_KnownInputs(const Byte0: Integer; const ExpectedPercent: Double);

    // ---- Fuel pressure (PID 0A) ----
    [Test]
    [TestCase('Zero',  '0,0')]
    [TestCase('100*3', '100,300')]
    procedure FuelPressure_KnownInputs(const Byte0, ExpectedKPa: Integer);

    // ---- Engine RPM (PID 0C) ----
    [Test]
    procedure EngineRPM_DecodesKnownTwoByteValue;

    [Test]
    procedure EngineRPM_TooShort_Fails;

    // ---- Timing advance (PID 0E) ----
    [Test]
    [TestCase('Center_0deg',  '128,0')]
    [TestCase('PlusSixteen',  '160,16')]
    procedure TimingAdvance_KnownInputs(const Byte0: Integer; const ExpectedDeg: Double);

    // ---- MAF (PID 10) ----
    [Test]
    procedure MassAirFlow_DecodesKnownTwoByteValue;
  end;

implementation

uses
  System.SysUtils,
  OBD.Response.Decoders;

const
  // Floating-point tolerance — protects against the implementation switching
  // between Single/Double or trivial rounding tweaks without losing precision.
  EPSILON = 0.0001;

{ TServiceResponseDispatchTests }

//------------------------------------------------------------------------------
// DISPATCH_POSITIVE SERVICE RESPONSE_EXTRACTS SERVICE AND PID
//------------------------------------------------------------------------------
procedure TServiceResponseDispatchTests.Dispatch_PositiveServiceResponse_ExtractsServiceAndPID;
var
  Decoder: TOBDServiceResponseDecoder;
  Resp: TBytes;
  Err: Boolean;
  ServiceID, PID: Integer;
  Data: TBytes;
begin
  // Service 01 PID 0C (RPM) reply: 41 0C 1A F8
  Resp := TBytes.Create($41, $0C, $1A, $F8);
  Decoder := TOBDServiceResponseDecoder.Create;
  try
    Assert.IsTrue(Decoder.DecodeServiceResponse(Resp, Err, ServiceID, PID, Data));
    Assert.IsFalse(Err);
    Assert.AreEqual(1, ServiceID);
    Assert.AreEqual($0C, PID);
    Assert.AreEqual(2, Length(Data));
    Assert.AreEqual(Byte($1A), Data[0]);
    Assert.AreEqual(Byte($F8), Data[1]);
  finally
    Decoder.Free;
  end;
end;

//------------------------------------------------------------------------------
// DISPATCH_NEGATIVE RESPONSE_FLAGS ERROR
//------------------------------------------------------------------------------
procedure TServiceResponseDispatchTests.Dispatch_NegativeResponse_FlagsError;
var
  Decoder: TOBDServiceResponseDecoder;
  Resp: TBytes;
  Err: Boolean;
  ServiceID, PID: Integer;
  Data: TBytes;
begin
  // Negative response: 7F <service> <NRC>
  Resp := TBytes.Create($7F, $01, $12);
  Decoder := TOBDServiceResponseDecoder.Create;
  try
    Decoder.DecodeServiceResponse(Resp, Err, ServiceID, PID, Data);
    Assert.IsTrue(Err, 'Negative response should set Error=True');
  finally
    Decoder.Free;
  end;
end;

//------------------------------------------------------------------------------
// DISPATCH_TOO SHORT RESPONSE_FAILS
//------------------------------------------------------------------------------
procedure TServiceResponseDispatchTests.Dispatch_TooShortResponse_Fails;
var
  Decoder: TOBDServiceResponseDecoder;
  Err: Boolean;
  ServiceID, PID: Integer;
  Data, Empty: TBytes;
begin
  SetLength(Empty, 0);
  Decoder := TOBDServiceResponseDecoder.Create;
  try
    Assert.IsFalse(Decoder.DecodeServiceResponse(Empty, Err, ServiceID, PID, Data));
  finally
    Decoder.Free;
  end;
end;

//------------------------------------------------------------------------------
// DISPATCH_SERVICE03_HAS NO PID_BUT CARRIES DATA
//------------------------------------------------------------------------------
procedure TServiceResponseDispatchTests.Dispatch_Service03_HasNoPID_ButCarriesData;
var
  Decoder: TOBDServiceResponseDecoder;
  Resp: TBytes;
  Err: Boolean;
  ServiceID, PID: Integer;
  Data: TBytes;
begin
  // Service 03 (stored DTCs) — one DTC P0143 packed as 01 43 plus filler.
  // Format: 43 <count> <DTC bytes...>
  Resp := TBytes.Create($43, $02, $01, $43, $80, $13);
  Decoder := TOBDServiceResponseDecoder.Create;
  try
    Assert.IsTrue(Decoder.DecodeServiceResponse(Resp, Err, ServiceID, PID, Data));
    Assert.IsFalse(Err);
    Assert.AreEqual(3, ServiceID);
    Assert.AreEqual(0, PID, 'Service 03 must report PID = 0');
    Assert.IsTrue(Length(Data) > 0, 'DTC payload must be carried in Data');
  finally
    Decoder.Free;
  end;
end;

{ TPidDecoderTests }

//------------------------------------------------------------------------------
// PERCENTAGE_KNOWN INPUTS
//------------------------------------------------------------------------------
procedure TPidDecoderTests.Percentage_KnownInputs(
  const Byte0: Integer; const ExpectedPercent: Double);
var
  D: TOBDPercentageDecoder;
  Pct: Double;
begin
  D := TOBDPercentageDecoder.Create;
  try
    Assert.IsTrue(D.Parse(TBytes.Create(Byte(Byte0)), Pct));
    Assert.AreEqual(ExpectedPercent, Pct, EPSILON);
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// PERCENTAGE_EMPTY DATA_FAILS
//------------------------------------------------------------------------------
procedure TPidDecoderTests.Percentage_EmptyData_Fails;
var
  D: TOBDPercentageDecoder;
  Empty: TBytes;
  Pct: Double;
begin
  SetLength(Empty, 0);
  D := TOBDPercentageDecoder.Create;
  try
    Assert.IsFalse(D.Parse(Empty, Pct));
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// TEMPERATURE_KNOWN INPUTS
//------------------------------------------------------------------------------
procedure TPidDecoderTests.Temperature_KnownInputs(
  const Byte0, ExpectedC: Integer);
var
  D: TOBDTemperatureDecoder;
  T: Integer;
begin
  D := TOBDTemperatureDecoder.Create;
  try
    Assert.IsTrue(D.Parse(TBytes.Create(Byte(Byte0)), T));
    Assert.AreEqual(ExpectedC, T);
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// FUEL TRIM_KNOWN INPUTS
//------------------------------------------------------------------------------
procedure TPidDecoderTests.FuelTrim_KnownInputs(
  const Byte0: Integer; const ExpectedPercent: Double);
var
  D: TOBDFuelTrimDecoder;
  Pct: Double;
begin
  D := TOBDFuelTrimDecoder.Create;
  try
    Assert.IsTrue(D.Parse(TBytes.Create(Byte(Byte0)), Pct));
    Assert.AreEqual(ExpectedPercent, Pct, EPSILON);
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// FUEL PRESSURE_KNOWN INPUTS
//------------------------------------------------------------------------------
procedure TPidDecoderTests.FuelPressure_KnownInputs(
  const Byte0, ExpectedKPa: Integer);
var
  D: TOBDFuelPressureDecoder;
  Pressure: Integer;
begin
  D := TOBDFuelPressureDecoder.Create;
  try
    Assert.IsTrue(D.Parse(TBytes.Create(Byte(Byte0)), Pressure));
    Assert.AreEqual(ExpectedKPa, Pressure);
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// ENGINE RPM_DECODES KNOWN TWO BYTE VALUE
//------------------------------------------------------------------------------
procedure TPidDecoderTests.EngineRPM_DecodesKnownTwoByteValue;
var
  D: TOBDEngineRPMDecoder;
  RPM: Integer;
begin
  // (A * 256 + B) / 4 → ($1A * 256 + $F8) / 4 = 6904 / 4 = 1726
  D := TOBDEngineRPMDecoder.Create;
  try
    Assert.IsTrue(D.Parse(TBytes.Create($1A, $F8), RPM));
    Assert.AreEqual(1726, RPM);
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// ENGINE RPM_TOO SHORT_FAILS
//------------------------------------------------------------------------------
procedure TPidDecoderTests.EngineRPM_TooShort_Fails;
var
  D: TOBDEngineRPMDecoder;
  RPM: Integer;
begin
  D := TOBDEngineRPMDecoder.Create;
  try
    Assert.IsFalse(D.Parse(TBytes.Create($1A), RPM));
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// TIMING ADVANCE_KNOWN INPUTS
//------------------------------------------------------------------------------
procedure TPidDecoderTests.TimingAdvance_KnownInputs(
  const Byte0: Integer; const ExpectedDeg: Double);
var
  D: TOBDTimingAdvanceDecoder;
  Deg: Double;
begin
  D := TOBDTimingAdvanceDecoder.Create;
  try
    Assert.IsTrue(D.Parse(TBytes.Create(Byte(Byte0)), Deg));
    Assert.AreEqual(ExpectedDeg, Deg, EPSILON);
  finally
    D.Free;
  end;
end;

//------------------------------------------------------------------------------
// MASS AIR FLOW_DECODES KNOWN TWO BYTE VALUE
//------------------------------------------------------------------------------
procedure TPidDecoderTests.MassAirFlow_DecodesKnownTwoByteValue;
var
  D: TOBDMassAirFlowRateDecoder;
  MAF: Double;
begin
  // ((A * 256 + B) / 100) → (5*256 + 220)/100 = 1500/100 = 15.0
  D := TOBDMassAirFlowRateDecoder.Create;
  try
    Assert.IsTrue(D.Parse(TBytes.Create($05, $DC), MAF));
    Assert.AreEqual(15.0, MAF, EPSILON);
  finally
    D.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceResponseDispatchTests);
  TDUnitX.RegisterTestFixture(TPidDecoderTests);

end.
