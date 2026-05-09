//------------------------------------------------------------------------------
// UNIT           : Tests.EV.BatteryHealth.pas
// CONTENTS       : Tests for OBD.EV.BatteryHealth
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
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
    /// <summary>
    ///   Imbalance flat pack has zero spread.
    /// </summary>
    [Test] procedure ImbalanceFlatPackHasZeroSpread;
    /// <summary>
    ///   Imbalance spread and std dev.
    /// </summary>
    [Test] procedure ImbalanceSpreadAndStdDev;
    /// <summary>
    ///   Imbalance outlier beyond three sigma.
    /// </summary>
    [Test] procedure ImbalanceOutlierBeyondThreeSigma;
    /// <summary>
    ///   Imbalance empty array raises.
    /// </summary>
    [Test] procedure ImbalanceEmptyArrayRaises;
    /// <summary>
    ///   So h at rated capacity is one.
    /// </summary>
    [Test] procedure SoHAtRatedCapacityIsOne;
    /// <summary>
    ///   So h at half capacity is half.
    /// </summary>
    [Test] procedure SoHAtHalfCapacityIsHalf;
    /// <summary>
    ///   So h rated zero raises.
    /// </summary>
    [Test] procedure SoHRatedZeroRaises;
    /// <summary>
    ///   So h temperature derating composite.
    /// </summary>
    [Test] procedure SoHTemperatureDeratingComposite;
    /// <summary>
    ///   Charging session round trips.
    /// </summary>
    [Test] procedure ChargingSessionRoundTrips;
    /// <summary>
    ///   Charging session end before start raises.
    /// </summary>
    [Test] procedure ChargingSessionEndBeforeStartRaises;
    /// <summary>
    ///   Charging session out of range so c raises.
    /// </summary>
    [Test] procedure ChargingSessionOutOfRangeSoCRaises;
  end;

implementation

uses
  System.SysUtils, System.Math, OBD.EV.BatteryHealth;

//------------------------------------------------------------------------------
// IMBALANCE FLAT PACK HAS ZERO SPREAD
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.ImbalanceFlatPackHasZeroSpread;
var
  R: TOBDCellImbalance;
begin
  R := ComputeCellImbalance([3.7, 3.7, 3.7, 3.7]);
  Assert.AreEqual(Single(0.0), R.SpreadVolts, 0.0001);
  Assert.AreEqual(Single(0.0), R.StdDev, 0.0001);
  Assert.AreEqual(-1, R.OutlierIndex);
end;

//------------------------------------------------------------------------------
// IMBALANCE SPREAD AND STD DEV
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.ImbalanceSpreadAndStdDev;
var
  R: TOBDCellImbalance;
begin
  R := ComputeCellImbalance([3.6, 3.7, 3.8, 3.7]);
  Assert.AreEqual(Single(3.6), R.MinVoltage, 0.0001);
  Assert.AreEqual(Single(3.8), R.MaxVoltage, 0.0001);
  Assert.AreEqual(Single(0.2), R.SpreadVolts, 0.0001);
  Assert.AreEqual(Single(3.7), R.MeanVoltage, 0.0001);
  Assert.IsTrue(R.StdDev > 0);
end;

//------------------------------------------------------------------------------
// IMBALANCE OUTLIER BEYOND THREE SIGMA
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// IMBALANCE EMPTY ARRAY RAISES
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.ImbalanceEmptyArrayRaises;
var
  V: array of Single;
begin
  SetLength(V, 0);
  Assert.WillRaise(
    procedure begin ComputeCellImbalance(V); end,
    EOBDBatteryHealth);
end;

//------------------------------------------------------------------------------
// SO HAT RATED CAPACITY IS ONE
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.SoHAtRatedCapacityIsOne;
var
  R: TOBDBatterySoH;
begin
  R := ComputeBatterySoH(77.0, 77.0);
  Assert.AreEqual(Single(1.0), R.SoHFromCapacity, 0.0001);
  Assert.AreEqual(Single(1.0), R.CompositeSoH, 0.0001);
end;

//------------------------------------------------------------------------------
// SO HAT HALF CAPACITY IS HALF
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.SoHAtHalfCapacityIsHalf;
var
  R: TOBDBatterySoH;
begin
  R := ComputeBatterySoH(100.0, 50.0);
  Assert.AreEqual(Single(0.5), R.SoHFromCapacity, 0.0001);
end;

//------------------------------------------------------------------------------
// SO HRATED ZERO RAISES
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.SoHRatedZeroRaises;
begin
  Assert.WillRaise(
    procedure begin ComputeBatterySoH(0, 50); end,
    EOBDBatteryHealth);
end;

//------------------------------------------------------------------------------
// SO HTEMPERATURE DERATING COMPOSITE
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.SoHTemperatureDeratingComposite;
var
  R: TOBDBatterySoH;
begin
  R := ComputeBatterySoH(100, 80, 250, 0.9);
  Assert.AreEqual(Single(0.8), R.SoHFromCapacity, 0.0001);
  Assert.AreEqual(Single(0.72), R.CompositeSoH, 0.0001);
  Assert.AreEqual(250, R.EquivalentFullCycles);
end;

//------------------------------------------------------------------------------
// CHARGING SESSION ROUND TRIPS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// CHARGING SESSION END BEFORE START RAISES
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.ChargingSessionEndBeforeStartRaises;
var
  Raw: TOBDChargingSession;
begin
  Raw := Default(TOBDChargingSession);
  Raw.StartSoCPercent := 80;
  Raw.EndSoCPercent := 60;
  Assert.WillRaise(
    procedure begin NormaliseChargingSession(Raw); end,
    EOBDBatteryHealth);
end;

//------------------------------------------------------------------------------
// CHARGING SESSION OUT OF RANGE SO CRAISES
//------------------------------------------------------------------------------
procedure TBatteryHealthTests.ChargingSessionOutOfRangeSoCRaises;
var
  Raw: TOBDChargingSession;
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
