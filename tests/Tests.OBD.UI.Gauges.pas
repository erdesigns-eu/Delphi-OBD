//------------------------------------------------------------------------------
//  Tests.OBD.UI.Gauges
//
//  Coverage for the non-visual contract of the gauge family:
//  zone resolution, value clamp, normalisation, tick config,
//  tachometer redline defaults, arc start/sweep wiring,
//  linear-gauge orientation default.
//
//  Paint behaviour itself needs a windowed message loop and a
//  visible canvas; out of unit-test scope (manual smoke on
//  the demo dashboard).
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Gauges;

interface

uses
  System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Base,
  OBD.UI.Gauges.Dial,
  OBD.UI.Gauges.Linear;

type
  [TestFixture]
  TGaugeTypesTests = class
  public
    [Test] procedure MakeGaugeZoneSetsFields;
    [Test] procedure ResolveZoneFindsCoveringZone;
    [Test] procedure ResolveZoneReturnsZeroWhenNoMatch;
    [Test] procedure NormaliseClampsBelowMin;
    [Test] procedure NormaliseClampsAboveMax;
    [Test] procedure NormaliseMidRangeIsHalf;
    [Test] procedure NormaliseDegenerateRangeIsZero;
    [Test] procedure DefaultTickConfigHasSaneDefaults;
  end;

  [TestFixture]
  TCircularGaugeTests = class
  public
    [Test] procedure DefaultsMinZeroMaxHundred;
    [Test] procedure ValueClampsBelowMin;
    [Test] procedure ValueClampsAboveMax;
    [Test] procedure DecoupledValueSetterFiresOnValueChanged;
    [Test] procedure FormatValueRespectsDecimals;
    [Test] procedure FormatValueAppendsUnit;
  end;

  [TestFixture]
  TTachometerTests = class
  public
    [Test] procedure DefaultsRPMRange;
    [Test] procedure DefaultZonesHaveWarningAndRedline;
    [Test] procedure SetRedlineRebuildsZones;
  end;

  [TestFixture]
  TArcGaugeTests = class
  public
    [Test] procedure DefaultsTo270Sweep;
    [Test] procedure StartAngleCustomisable;
  end;

  [TestFixture]
  TLinearGaugeTests = class
  public
    [Test] procedure DefaultsHorizontalShowValueTrue;
    [Test] procedure OrientationToggle;
  end;

implementation

{ TGaugeTypesTests --------------------------------------------------------- }

procedure TGaugeTypesTests.MakeGaugeZoneSetsFields;
var Z: TOBDGaugeZone;
begin
  Z := MakeGaugeZone(50.0, 100.0, clRed);
  Assert.AreEqual(50.0,  Z.StartValue, 0.001);
  Assert.AreEqual(100.0, Z.EndValue,   0.001);
  Assert.AreEqual<TColor>(clRed, Z.Color);
end;

procedure TGaugeTypesTests.ResolveZoneFindsCoveringZone;
var
  Zones: TOBDGaugeZones;
  Z: TOBDGaugeZone;
begin
  SetLength(Zones, 2);
  Zones[0] := MakeGaugeZone(0, 50,  clBlue);
  Zones[1] := MakeGaugeZone(50, 100, clRed);
  Z := ResolveZone(Zones, 75);
  Assert.AreEqual<TColor>(clRed, Z.Color);
end;

procedure TGaugeTypesTests.ResolveZoneReturnsZeroWhenNoMatch;
var
  Zones: TOBDGaugeZones;
  Z: TOBDGaugeZone;
begin
  SetLength(Zones, 1);
  Zones[0] := MakeGaugeZone(0, 50, clBlue);
  Z := ResolveZone(Zones, 60);
  Assert.AreEqual(0.0, Z.StartValue, 0.001);
  Assert.AreEqual(0.0, Z.EndValue,   0.001);
end;

procedure TGaugeTypesTests.NormaliseClampsBelowMin;
begin
  Assert.AreEqual(0.0, NormaliseValue(0, 100, -10), 0.001);
end;

procedure TGaugeTypesTests.NormaliseClampsAboveMax;
begin
  Assert.AreEqual(1.0, NormaliseValue(0, 100, 110), 0.001);
end;

procedure TGaugeTypesTests.NormaliseMidRangeIsHalf;
begin
  Assert.AreEqual(0.5, NormaliseValue(0, 100, 50), 0.001);
end;

procedure TGaugeTypesTests.NormaliseDegenerateRangeIsZero;
begin
  // Equal min / max would otherwise divide by zero.
  Assert.AreEqual(0.0, NormaliseValue(42, 42, 42), 0.001);
end;

procedure TGaugeTypesTests.DefaultTickConfigHasSaneDefaults;
var Cfg: TOBDGaugeTickConfig;
begin
  Cfg := DefaultTickConfig;
  Assert.AreEqual(10.0, Cfg.MajorInterval, 0.001);
  Assert.IsTrue(Cfg.MinorTicksPerMajor > 0);
  Assert.IsTrue(Cfg.ShowLabels);
end;

{ TCircularGaugeTests ------------------------------------------------------ }

procedure TCircularGaugeTests.DefaultsMinZeroMaxHundred;
var G: TOBDCircularGauge;
begin
  G := TOBDCircularGauge.Create(nil);
  try
    Assert.AreEqual(0.0,   G.Min, 0.001);
    Assert.AreEqual(100.0, G.Max, 0.001);
  finally G.Free; end;
end;

procedure TCircularGaugeTests.ValueClampsBelowMin;
var G: TOBDCircularGauge;
begin
  G := TOBDCircularGauge.Create(nil);
  try
    G.Value := -50;
    Assert.AreEqual(0.0, G.Value, 0.001);
  finally G.Free; end;
end;

procedure TCircularGaugeTests.ValueClampsAboveMax;
var G: TOBDCircularGauge;
begin
  G := TOBDCircularGauge.Create(nil);
  try
    G.Value := 250;
    Assert.AreEqual(100.0, G.Value, 0.001);
  finally G.Free; end;
end;

procedure TCircularGaugeTests.DecoupledValueSetterFiresOnValueChanged;
var
  G: TOBDCircularGauge;
  Got: Double;
  Fired: Boolean;
begin
  Fired := False;
  Got := 0;
  G := TOBDCircularGauge.Create(nil);
  try
    G.AnimateValueChanges := False;     // synchronous path
    G.OnValueChanged :=
      procedure(Sender: TObject; AValue: Double)
      begin Fired := True; Got := AValue; end;
    G.Value := 42;
    Assert.IsTrue(Fired);
    Assert.AreEqual(42.0, Got, 0.001);
  finally G.Free; end;
end;

procedure TCircularGaugeTests.FormatValueRespectsDecimals;
var G: TOBDCircularGauge;
begin
  G := TOBDCircularGauge.Create(nil);
  try
    G.Decimals := 2;
    G.&Unit := '';
    G.AnimateValueChanges := False;
    G.Value := 3.14159;
    Assert.AreEqual('3.14',
      G.FormatValue(G.Value));
  finally G.Free; end;
end;

procedure TCircularGaugeTests.FormatValueAppendsUnit;
var G: TOBDCircularGauge;
begin
  G := TOBDCircularGauge.Create(nil);
  try
    G.&Unit := 'km/h';
    G.Decimals := 0;
    G.AnimateValueChanges := False;
    G.Value := 88;
    Assert.AreEqual('88 km/h', G.FormatValue(G.Value));
  finally G.Free; end;
end;

{ TTachometerTests --------------------------------------------------------- }

procedure TTachometerTests.DefaultsRPMRange;
var T: TOBDTachometer;
begin
  T := TOBDTachometer.Create(nil);
  try
    Assert.AreEqual(0.0,    T.Min, 0.001);
    Assert.AreEqual(8000.0, T.Max, 0.001);
    Assert.AreEqual('rpm',  T.&Unit);
  finally T.Free; end;
end;

procedure TTachometerTests.DefaultZonesHaveWarningAndRedline;
var
  T: TOBDTachometer;
  Z: TOBDGaugeZones;
begin
  T := TOBDTachometer.Create(nil);
  try
    Z := T.Zones;
    Assert.AreEqual(2, Length(Z),
      'Default tachometer should ship warning + redline zones');
    // First zone is warning (amber), second is redline (red).
    Assert.IsTrue(Z[1].StartValue >= Z[0].StartValue);
  finally T.Free; end;
end;

procedure TTachometerTests.SetRedlineRebuildsZones;
var
  T: TOBDTachometer;
  Z: TOBDGaugeZones;
begin
  T := TOBDTachometer.Create(nil);
  try
    T.Redline := 6500;
    Z := T.Zones;
    Assert.AreEqual(6500.0, Z[1].StartValue, 0.001);
  finally T.Free; end;
end;

{ TArcGaugeTests ----------------------------------------------------------- }

procedure TArcGaugeTests.DefaultsTo270Sweep;
var A: TOBDArcGauge;
begin
  A := TOBDArcGauge.Create(nil);
  try
    Assert.AreEqual(270.0, A.SweepAngle, 0.001);
  finally A.Free; end;
end;

procedure TArcGaugeTests.StartAngleCustomisable;
var A: TOBDArcGauge;
begin
  A := TOBDArcGauge.Create(nil);
  try
    A.StartAngle := 90;
    Assert.AreEqual(90.0, A.StartAngle, 0.001);
  finally A.Free; end;
end;

{ TLinearGaugeTests -------------------------------------------------------- }

procedure TLinearGaugeTests.DefaultsHorizontalShowValueTrue;
var L: TOBDLinearGauge;
begin
  L := TOBDLinearGauge.Create(nil);
  try
    Assert.AreEqual(Ord(loHorizontal), Ord(L.Orientation));
    Assert.IsTrue(L.ShowValueText);
  finally L.Free; end;
end;

procedure TLinearGaugeTests.OrientationToggle;
var L: TOBDLinearGauge;
begin
  L := TOBDLinearGauge.Create(nil);
  try
    L.Orientation := loVertical;
    Assert.AreEqual(Ord(loVertical), Ord(L.Orientation));
  finally L.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGaugeTypesTests);
  TDUnitX.RegisterTestFixture(TCircularGaugeTests);
  TDUnitX.RegisterTestFixture(TTachometerTests);
  TDUnitX.RegisterTestFixture(TArcGaugeTests);
  TDUnitX.RegisterTestFixture(TLinearGaugeTests);

end.
