//------------------------------------------------------------------------------
// UNIT           : Tests.EV.BatteryHealth
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.EV.BatteryHealth;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TBatteryHealthTests = class
  public
    [Test] procedure ImbalanceFlatPackHasZeroSpread;
    [Test] procedure ImbalanceSpreadAndStdDev;
    [Test] procedure ImbalanceOutlierBeyondThreeSigma;
    [Test] procedure ImbalanceEmptyArrayRaises;
    [Test] procedure SoHAtRatedCapacityIsOne;
    [Test] procedure SoHAtHalfCapacityIsHalf;
    [Test] procedure SoHRatedZeroRaises;
    [Test] procedure SoHTemperatureDeratingComposite;
    [Test] procedure ChargingSessionRoundTrips;
    [Test] procedure ChargingSessionEndBeforeStartRaises;
    [Test] procedure ChargingSessionOutOfRangeSoCRaises;
  end;

implementation

uses
  System.SysUtils, System.Math, OBD.EV.BatteryHealth;

procedure TBatteryHealthTests.ImbalanceFlatPackHasZeroSpread;
var R: TOBDCellImbalance;
begin
  R := ComputeCellImbalance([3.7, 3.7, 3.7, 3.7]);
  Assert.AreEqual(Single(0.0), R.SpreadVolts, 0.0001);
  Assert.AreEqual(Single(0.0), R.StdDev, 0.0001);
  Assert.AreEqual(-1, R.OutlierIndex);
end;

procedure TBatteryHealthTests.ImbalanceSpreadAndStdDev;
var R: TOBDCellImbalance;
begin
  R := ComputeCellImbalance([3.6, 3.7, 3.8, 3.7]);
  Assert.AreEqual(Single(3.6), R.MinVoltage, 0.0001);
  Assert.AreEqual(Single(3.8), R.MaxVoltage, 0.0001);
  Assert.AreEqual(Single(0.2), R.SpreadVolts, 0.0001);
  Assert.AreEqual(Single(3.7), R.MeanVoltage, 0.0001);
  Assert.IsTrue(R.StdDev > 0);
end;

procedure TBatteryHealthTests.ImbalanceOutlierBeyondThreeSigma;
var
  V: array of Single;
  R: TOBDCellImbalance;
  I: Integer;
begin
  // 99 cells around 3.700 + one cell at 4.50 -> definitely > 3 sigma.
  SetLength(V, 100);
  for I := 0 to 98 do V[I] := 3.700 + (Random - 0.5) * 0.001;
  V[42] := 4.500;
  R := ComputeCellImbalance(V);
  Assert.AreEqual(42, R.OutlierIndex);
  Assert.IsTrue(R.OutlierDeltaSigma > 3.0);
end;

procedure TBatteryHealthTests.ImbalanceEmptyArrayRaises;
var V: array of Single;
begin
  SetLength(V, 0);
  Assert.WillRaise(
    procedure begin ComputeCellImbalance(V); end,
    EOBDBatteryHealth);
end;

procedure TBatteryHealthTests.SoHAtRatedCapacityIsOne;
var R: TOBDBatterySoH;
begin
  R := ComputeBatterySoH(77.0, 77.0);
  Assert.AreEqual(Single(1.0), R.SoHFromCapacity, 0.0001);
  Assert.AreEqual(Single(1.0), R.CompositeSoH, 0.0001);
end;

procedure TBatteryHealthTests.SoHAtHalfCapacityIsHalf;
var R: TOBDBatterySoH;
begin
  R := ComputeBatterySoH(100.0, 50.0);
  Assert.AreEqual(Single(0.5), R.SoHFromCapacity, 0.0001);
end;

procedure TBatteryHealthTests.SoHRatedZeroRaises;
begin
  Assert.WillRaise(
    procedure begin ComputeBatterySoH(0, 50); end,
    EOBDBatteryHealth);
end;

procedure TBatteryHealthTests.SoHTemperatureDeratingComposite;
var R: TOBDBatterySoH;
begin
  R := ComputeBatterySoH(100, 80, 250, 0.9);
  Assert.AreEqual(Single(0.8), R.SoHFromCapacity, 0.0001);
  Assert.AreEqual(Single(0.72), R.CompositeSoH, 0.0001);
  Assert.AreEqual(250, R.EquivalentFullCycles);
end;

procedure TBatteryHealthTests.ChargingSessionRoundTrips;
var
  Raw, Out_: TOBDChargingSession;
begin
  Raw := Default(TOBDChargingSession);
  Raw.StartSoCPercent := 20;
  Raw.EndSoCPercent := 80;
  Raw.EnergyDeliveredKwh := 45;
  Raw.PeakPowerKw := 150;
  Raw.AverageBatteryTempC := 28;
  Raw.DurationSeconds := 1800;
  Raw.SessionType := 'DC';
  Out_ := NormaliseChargingSession(Raw);
  Assert.AreEqual(Single(45.0), Out_.EnergyDeliveredKwh, 0.0001);
  Assert.AreEqual('DC', Out_.SessionType);
end;

procedure TBatteryHealthTests.ChargingSessionEndBeforeStartRaises;
var Raw: TOBDChargingSession;
begin
  Raw := Default(TOBDChargingSession);
  Raw.StartSoCPercent := 80;
  Raw.EndSoCPercent := 60;
  Assert.WillRaise(
    procedure begin NormaliseChargingSession(Raw); end,
    EOBDBatteryHealth);
end;

procedure TBatteryHealthTests.ChargingSessionOutOfRangeSoCRaises;
var Raw: TOBDChargingSession;
begin
  Raw := Default(TOBDChargingSession);
  Raw.StartSoCPercent := -1;
  Assert.WillRaise(
    procedure begin NormaliseChargingSession(Raw); end,
    EOBDBatteryHealth);
end;

initialization
  TDUnitX.RegisterTestFixture(TBatteryHealthTests);

end.
