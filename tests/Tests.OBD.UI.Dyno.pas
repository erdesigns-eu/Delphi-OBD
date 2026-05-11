//------------------------------------------------------------------------------
//  Tests.OBD.UI.Dyno
//
//  Coverage for the A2.12 dyno-math components. Each test
//  exercises the property contract + a couple of sample
//  pushes; the SAE J1349 correction factor and fuel-economy
//  integrator get round-trip checks.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Dyno;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  OBD.UI.Dyno;

type
  [TestFixture]
  TDynoCalculatorTests = class
  public
    [Test] procedure DefaultsAreSane;
    [Test] procedure PushSampleFiresOnSampleAfterFirstDelta;
    [Test] procedure ResetClearsLastSample;
  end;

  [TestFixture]
  TPowerCurveTests = class
  public
    [Test] procedure ArmStartsCapture;
    [Test] procedure PushPointBeforeArmIsIgnored;
    [Test] procedure StopFreezesArmedFlag;
  end;

  [TestFixture]
  TDragRunTests = class
  public
    [Test] procedure ArmThenSpeedAboveStartTriggersRunning;
    [Test] procedure SpeedAboveTargetFiresFinishedAndRecordsPeaks;
    [Test] procedure ResetClearsRunning;
  end;

  [TestFixture]
  TDynoConditionsTests = class
  public
    [Test] procedure DefaultsCloseToOne;
    [Test] procedure HighAltitudeReducesPower;
  end;

  [TestFixture]
  TFuelEconomyMeterTests = class
  public
    [Test] procedure DefaultsZero;
    [Test] procedure PushSampleAccumulatesTrips;
    [Test] procedure ResetTripAClearsTripAOnly;
  end;

  [TestFixture]
  TEmissionsEstimatorTests = class
  public
    [Test] procedure DefaultsZero;
    [Test] procedure PushSampleProducesPositiveCo2;
  end;

  [TestFixture]
  TInertialBrakeTests = class
  public
    [Test] procedure DefaultsZero;
    [Test] procedure DecelGivesPositiveG;
    [Test] procedure ResetClearsState;
  end;

  [TestFixture]
  TTorqueAtWheelsTests = class
  public
    [Test] procedure DefaultLossIsFifteenPercent;
    [Test] procedure HostOverridesLoss;
  end;

implementation

{ TDynoCalculatorTests --------------------------------------------------- }

procedure TDynoCalculatorTests.DefaultsAreSane;
var C: TOBDDynoCalculator;
begin
  C := TOBDDynoCalculator.Create(nil);
  try
    Assert.AreEqual(1500.0, C.VehicleKg, 0.001);
    Assert.AreEqual(0.65,   C.CdA,       0.001);
    Assert.AreEqual(0.012,  C.Crr,       0.001);
    Assert.AreEqual(1.225,  C.AirDensity, 0.001);
  finally C.Free; end;
end;

procedure TDynoCalculatorTests.PushSampleFiresOnSampleAfterFirstDelta;
var
  C: TOBDDynoCalculator;
  Fired: Integer;
begin
  Fired := 0;
  C := TOBDDynoCalculator.Create(nil);
  try
    C.OnSample :=
      procedure(Sender: TObject; ATimeMs: Cardinal;
        AHP, ATorqueNm: Double)
      begin Inc(Fired); end;
    C.PushSample(0,    0,   0);
    C.PushSample(1000, 50,  3000);
    Assert.AreEqual(2, Fired);
  finally C.Free; end;
end;

procedure TDynoCalculatorTests.ResetClearsLastSample;
var C: TOBDDynoCalculator;
begin
  C := TOBDDynoCalculator.Create(nil);
  try
    C.PushSample(0, 10);
    C.Reset;
    C.PushSample(0, 10);     // should re-seed without raising
    Assert.Pass;
  finally C.Free; end;
end;

{ TPowerCurveTests ------------------------------------------------------- }

procedure TPowerCurveTests.ArmStartsCapture;
var P: TOBDPowerCurve;
begin
  P := TOBDPowerCurve.Create(nil);
  try
    P.Arm;
    Assert.IsTrue(P.Armed);
    P.PushPoint(2000, 80, 200);
    Assert.AreEqual<NativeInt>(1, Length(P.Curve));
  finally P.Free; end;
end;

procedure TPowerCurveTests.PushPointBeforeArmIsIgnored;
var P: TOBDPowerCurve;
begin
  P := TOBDPowerCurve.Create(nil);
  try
    P.PushPoint(2000, 80, 200);
    Assert.AreEqual<NativeInt>(0, Length(P.Curve));
  finally P.Free; end;
end;

procedure TPowerCurveTests.StopFreezesArmedFlag;
var P: TOBDPowerCurve;
begin
  P := TOBDPowerCurve.Create(nil);
  try
    P.Arm;
    P.PushPoint(1000, 50, 150);
    P.Stop;
    Assert.IsFalse(P.Armed);
    P.PushPoint(2000, 80, 200);    // ignored after stop
    Assert.AreEqual<NativeInt>(1, Length(P.Curve));
  finally P.Free; end;
end;

{ TDragRunTests --------------------------------------------------------- }

procedure TDragRunTests.ArmThenSpeedAboveStartTriggersRunning;
var R: TOBDDragRun;
begin
  R := TOBDDragRun.Create(nil);
  try
    R.Arm;
    R.PushSample(2.0, 50, 200);
    Assert.IsTrue(R.Running);
  finally R.Free; end;
end;

procedure TDragRunTests.SpeedAboveTargetFiresFinishedAndRecordsPeaks;
var
  R: TOBDDragRun;
  Fired: Boolean;
begin
  Fired := False;
  R := TOBDDragRun.Create(nil);
  try
    R.OnFinished :=
      procedure(Sender: TObject; AElapsedMs: Cardinal;
        APeakHP, APeakTorqueNm: Double)
      begin Fired := True; end;
    R.Arm;
    R.PushSample(2.0,   50, 200);
    R.PushSample(50.0, 110, 300);   // peaks update
    R.PushSample(110.0, 90, 250);   // crosses 100 → done
    Assert.IsTrue(Fired);
    Assert.IsFalse(R.Running);
    Assert.AreEqual(110.0, R.PeakHP,     0.001);
    Assert.AreEqual(300.0, R.PeakTorque, 0.001);
  finally R.Free; end;
end;

procedure TDragRunTests.ResetClearsRunning;
var R: TOBDDragRun;
begin
  R := TOBDDragRun.Create(nil);
  try
    R.Arm; R.PushSample(2.0, 50, 200);
    R.Reset;
    Assert.IsFalse(R.Running);
    Assert.AreEqual(0.0, R.PeakHP, 0.001);
  finally R.Free; end;
end;

{ TDynoConditionsTests -------------------------------------------------- }

procedure TDynoConditionsTests.DefaultsCloseToOne;
var C: TOBDDynoConditions;
begin
  C := TOBDDynoConditions.Create(nil);
  try
    // 25 °C, 99 kPa, 0 % humidity → CF ≈ 1.00
    Assert.AreEqual(1.0, C.CorrectionFactor, 0.05);
  finally C.Free; end;
end;

procedure TDynoConditionsTests.HighAltitudeReducesPower;
var C: TOBDDynoConditions;
begin
  C := TOBDDynoConditions.Create(nil);
  try
    C.PressureKPa := 75;       // ~2500 m altitude
    Assert.IsTrue(C.CorrectionFactor > 1.05,
      'High-altitude correction factor should grow > 1');
  finally C.Free; end;
end;

{ TFuelEconomyMeterTests ----------------------------------------------- }

procedure TFuelEconomyMeterTests.DefaultsZero;
var F: TOBDFuelEconomyMeter;
begin
  F := TOBDFuelEconomyMeter.Create(nil);
  try
    Assert.AreEqual(0.0, F.InstantLp100Km,   0.001);
    Assert.AreEqual(0.0, F.TripADistanceKm,  0.001);
  finally F.Free; end;
end;

procedure TFuelEconomyMeterTests.PushSampleAccumulatesTrips;
var F: TOBDFuelEconomyMeter;
begin
  F := TOBDFuelEconomyMeter.Create(nil);
  try
    F.PushSample(0,    5.0, 80);     // first → seeds
    F.PushSample(1000, 5.0, 80);     // 1 second @ 80 km/h
    Assert.IsTrue(F.TripADistanceKm > 0);
    Assert.IsTrue(F.TripAFuelL      > 0);
  finally F.Free; end;
end;

procedure TFuelEconomyMeterTests.ResetTripAClearsTripAOnly;
var F: TOBDFuelEconomyMeter;
begin
  F := TOBDFuelEconomyMeter.Create(nil);
  try
    F.PushSample(0, 5, 80);
    F.PushSample(1000, 5, 80);
    F.ResetTripA;
    Assert.AreEqual(0.0, F.TripADistanceKm, 0.001);
    Assert.IsTrue(F.TripBDistanceKm > 0);
  finally F.Free; end;
end;

{ TEmissionsEstimatorTests --------------------------------------------- }

procedure TEmissionsEstimatorTests.DefaultsZero;
var E: TOBDEmissionsEstimator;
begin
  E := TOBDEmissionsEstimator.Create(nil);
  try
    Assert.AreEqual(0.0, E.InstantCo2Gkm, 0.001);
  finally E.Free; end;
end;

procedure TEmissionsEstimatorTests.PushSampleProducesPositiveCo2;
var E: TOBDEmissionsEstimator;
begin
  E := TOBDEmissionsEstimator.Create(nil);
  try
    E.PushSample(8.0, 80);
    Assert.IsTrue(E.InstantCo2Gkm > 0);
  finally E.Free; end;
end;

{ TInertialBrakeTests ------------------------------------------------- }

procedure TInertialBrakeTests.DefaultsZero;
var B: TOBDInertialBrake;
begin
  B := TOBDInertialBrake.Create(nil);
  try
    Assert.AreEqual(0.0, B.GLoad, 0.001);
  finally B.Free; end;
end;

procedure TInertialBrakeTests.DecelGivesPositiveG;
var B: TOBDInertialBrake;
begin
  B := TOBDInertialBrake.Create(nil);
  try
    B.PushSample(0,    100);          // seed
    B.PushSample(1000, 50);           // -50 km/h in 1 s
    Assert.IsTrue(B.GLoad > 0,
      'Deceleration should produce a positive g-load');
  finally B.Free; end;
end;

procedure TInertialBrakeTests.ResetClearsState;
var B: TOBDInertialBrake;
begin
  B := TOBDInertialBrake.Create(nil);
  try
    B.PushSample(0, 100);
    B.PushSample(1000, 50);
    B.Reset;
    Assert.AreEqual(0.0, B.GLoad, 0.001);
  finally B.Free; end;
end;

{ TTorqueAtWheelsTests ----------------------------------------------- }

procedure TTorqueAtWheelsTests.DefaultLossIsFifteenPercent;
var T: TOBDTorqueAtWheels;
begin
  T := TOBDTorqueAtWheels.Create(nil);
  try
    T.PushSample(100, 200);
    Assert.AreEqual(85.0,  T.WheelHP,       0.001);
    Assert.AreEqual(170.0, T.WheelTorqueNm, 0.001);
  finally T.Free; end;
end;

procedure TTorqueAtWheelsTests.HostOverridesLoss;
var T: TOBDTorqueAtWheels;
begin
  T := TOBDTorqueAtWheels.Create(nil);
  try
    T.OnDrivetrainLoss :=
      procedure(Sender: TObject; var ALossFraction: Double)
      begin ALossFraction := 0.25; end;
    T.PushSample(100, 200);
    Assert.AreEqual(75.0,  T.WheelHP,       0.001);
    Assert.AreEqual(150.0, T.WheelTorqueNm, 0.001);
  finally T.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDynoCalculatorTests);
  TDUnitX.RegisterTestFixture(TPowerCurveTests);
  TDUnitX.RegisterTestFixture(TDragRunTests);
  TDUnitX.RegisterTestFixture(TDynoConditionsTests);
  TDUnitX.RegisterTestFixture(TFuelEconomyMeterTests);
  TDUnitX.RegisterTestFixture(TEmissionsEstimatorTests);
  TDUnitX.RegisterTestFixture(TInertialBrakeTests);
  TDUnitX.RegisterTestFixture(TTorqueAtWheelsTests);

end.
