//------------------------------------------------------------------------------
//  Tests.OBD.UI.Gauges.Specialised
//
//  Non-visual contract for the A2.5 specialised gauges:
//  TOBDBoostGauge, TOBDAFRGauge, TOBDStateOfChargeBar,
//  TOBDRegenIndicator.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Gauges.Specialised;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Specialised;

type
  [TestFixture]
  TBoostGaugeTests = class
  public
    [Test] procedure DefaultsRangeMinusOneToTwoBar;
    [Test] procedure DefaultsThreeZones;
    [Test] procedure SetWarningStartRebuildsZones;
  end;

  [TestFixture]
  TAFRGaugeTests = class
  public
    [Test] procedure DefaultsStoich147;
    [Test] procedure DefaultsTwoZones;
    [Test] procedure SetStoichRoundTrip;
  end;

  [TestFixture]
  TStateOfChargeBarTests = class
  public
    [Test] procedure DefaultsSOCZero;
    [Test] procedure SOCClampsLow;
    [Test] procedure SOCClampsHigh;
    [Test] procedure TimeToFullClampsNegative;
    [Test] procedure ChargingToggle;
  end;

  [TestFixture]
  TRegenIndicatorTests = class
  public
    [Test] procedure DefaultsPowerZero;
    [Test] procedure PositivePowerRoundTrip;
    [Test] procedure NegativePowerRoundTrip;
    [Test] procedure MaxPowerClampsLow;
    [Test] procedure MaxRegenClampsLow;
  end;

implementation

{ TBoostGaugeTests --------------------------------------------------------- }

procedure TBoostGaugeTests.DefaultsRangeMinusOneToTwoBar;
var G: TOBDBoostGauge;
begin
  G := TOBDBoostGauge.Create(nil);
  try
    Assert.AreEqual(-1.0, G.Min, 0.001);
    Assert.AreEqual( 2.0, G.Max, 0.001);
    Assert.AreEqual('bar', G.&Unit);
  finally G.Free; end;
end;

procedure TBoostGaugeTests.DefaultsThreeZones;
var G: TOBDBoostGauge;
begin
  G := TOBDBoostGauge.Create(nil);
  try
    Assert.AreEqual<NativeInt>(3, Length(G.Zones));
  finally G.Free; end;
end;

procedure TBoostGaugeTests.SetWarningStartRebuildsZones;
var
  G: TOBDBoostGauge;
  Z: TOBDGaugeZones;
begin
  G := TOBDBoostGauge.Create(nil);
  try
    G.WarningStart := 1.8;
    Z := G.Zones;
    Assert.AreEqual<NativeInt>(3, Length(Z));
    Assert.AreEqual(1.8, Z[1].EndValue,  0.001);
    Assert.AreEqual(1.8, Z[2].StartValue, 0.001);
  finally G.Free; end;
end;

{ TAFRGaugeTests ----------------------------------------------------------- }

procedure TAFRGaugeTests.DefaultsStoich147;
var G: TOBDAFRGauge;
begin
  G := TOBDAFRGauge.Create(nil);
  try
    Assert.AreEqual(14.7, G.Stoich, 0.001);
    Assert.AreEqual(16.0, G.LeanStart, 0.001);
    Assert.AreEqual(13.0, G.RichStart, 0.001);
  finally G.Free; end;
end;

procedure TAFRGaugeTests.DefaultsTwoZones;
var G: TOBDAFRGauge;
begin
  G := TOBDAFRGauge.Create(nil);
  try
    Assert.AreEqual<NativeInt>(2, Length(G.Zones));
  finally G.Free; end;
end;

procedure TAFRGaugeTests.SetStoichRoundTrip;
var G: TOBDAFRGauge;
begin
  G := TOBDAFRGauge.Create(nil);
  try
    G.Stoich := 9.7;     // E85
    Assert.AreEqual(9.7, G.Stoich, 0.001);
  finally G.Free; end;
end;

{ TStateOfChargeBarTests -------------------------------------------------- }

procedure TStateOfChargeBarTests.DefaultsSOCZero;
var B: TOBDStateOfChargeBar;
begin
  B := TOBDStateOfChargeBar.Create(nil);
  try
    Assert.AreEqual(0.0, B.SOC, 0.001);
    Assert.IsFalse(B.Charging);
    Assert.IsTrue(B.ShowReadout);
    Assert.AreEqual(20.0, B.LowThreshold, 0.001);
  finally B.Free; end;
end;

procedure TStateOfChargeBarTests.SOCClampsLow;
var B: TOBDStateOfChargeBar;
begin
  B := TOBDStateOfChargeBar.Create(nil);
  try
    B.SOC := -10;
    Assert.AreEqual(0.0, B.SOC, 0.001);
  finally B.Free; end;
end;

procedure TStateOfChargeBarTests.SOCClampsHigh;
var B: TOBDStateOfChargeBar;
begin
  B := TOBDStateOfChargeBar.Create(nil);
  try
    B.SOC := 250;
    Assert.AreEqual(100.0, B.SOC, 0.001);
  finally B.Free; end;
end;

procedure TStateOfChargeBarTests.TimeToFullClampsNegative;
var B: TOBDStateOfChargeBar;
begin
  B := TOBDStateOfChargeBar.Create(nil);
  try
    B.TimeToFullMs := -1000;
    Assert.AreEqual<Int64>(0, B.TimeToFullMs);
  finally B.Free; end;
end;

procedure TStateOfChargeBarTests.ChargingToggle;
var B: TOBDStateOfChargeBar;
begin
  B := TOBDStateOfChargeBar.Create(nil);
  try
    B.Charging := True;
    Assert.IsTrue(B.Charging);
    B.Charging := False;
    Assert.IsFalse(B.Charging);
  finally B.Free; end;
end;

{ TRegenIndicatorTests ---------------------------------------------------- }

procedure TRegenIndicatorTests.DefaultsPowerZero;
var R: TOBDRegenIndicator;
begin
  R := TOBDRegenIndicator.Create(nil);
  try
    Assert.AreEqual(0.0, R.Power, 0.001);
    Assert.AreEqual(200.0, R.MaxPower, 0.001);
    Assert.AreEqual(80.0,  R.MaxRegen, 0.001);
  finally R.Free; end;
end;

procedure TRegenIndicatorTests.PositivePowerRoundTrip;
var R: TOBDRegenIndicator;
begin
  R := TOBDRegenIndicator.Create(nil);
  try
    R.Power := 35;
    Assert.AreEqual(35.0, R.Power, 0.001);
  finally R.Free; end;
end;

procedure TRegenIndicatorTests.NegativePowerRoundTrip;
var R: TOBDRegenIndicator;
begin
  R := TOBDRegenIndicator.Create(nil);
  try
    R.Power := -120;
    Assert.AreEqual(-120.0, R.Power, 0.001);
  finally R.Free; end;
end;

procedure TRegenIndicatorTests.MaxPowerClampsLow;
var R: TOBDRegenIndicator;
begin
  R := TOBDRegenIndicator.Create(nil);
  try
    R.MaxPower := 0;
    Assert.AreEqual(1.0, R.MaxPower, 0.001);
  finally R.Free; end;
end;

procedure TRegenIndicatorTests.MaxRegenClampsLow;
var R: TOBDRegenIndicator;
begin
  R := TOBDRegenIndicator.Create(nil);
  try
    R.MaxRegen := -5;
    Assert.AreEqual(1.0, R.MaxRegen, 0.001);
  finally R.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TBoostGaugeTests);
  TDUnitX.RegisterTestFixture(TAFRGaugeTests);
  TDUnitX.RegisterTestFixture(TStateOfChargeBarTests);
  TDUnitX.RegisterTestFixture(TRegenIndicatorTests);

end.
