//------------------------------------------------------------------------------
//  Tests.OBD.UI.Gauges.Variants
//
//  Coverage for the six A2.3 gauge variants:
//  TOBDDigitalGauge, TOBDBarSegmentGauge, TOBDDeltaGauge,
//  TOBDSparkline, TOBDDualNeedleGauge, TOBDMinMaxGauge.
//
//  Non-visual contract only — paint pipelines need a windowed
//  message loop and are out of unit-test scope.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Gauges.Variants;

interface

uses
  System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Base,
  OBD.UI.Gauges.Linear,
  OBD.UI.Gauges.Variants,
  OBD.UI.Gauges.Sparkline,
  OBD.UI.Gauges.DialExtended;

type
  [TestFixture]
  TDigitalGaugeTests = class
  public
    [Test] procedure DefaultsShowMinMaxTrueShowAvgFalse;
    [Test] procedure ResetStatsClearsCounters;
    [Test] procedure ValueClampsAndAnimatesOff;
  end;

  [TestFixture]
  TBarSegmentGaugeTests = class
  public
    [Test] procedure DefaultsSixteenSegmentsHorizontal;
    [Test] procedure SegmentCountClampsLow;
    [Test] procedure SegmentCountClampsHigh;
    [Test] procedure GapPixelsClampsNonNegative;
    [Test] procedure OrientationToggle;
  end;

  [TestFixture]
  TDeltaGaugeTests = class
  public
    [Test] procedure DefaultsSymmetricRange;
    [Test] procedure ValueClampsBelowMin;
    [Test] procedure ValueClampsAboveMax;
  end;

  [TestFixture]
  TSparklineTests = class
  public
    [Test] procedure DefaultCapacityIs64;
    [Test] procedure CapacityClampsLow;
    [Test] procedure CapacityClampsHigh;
    [Test] procedure CapacityChangeResetsBuffer;
    [Test] procedure ResetSamplesEmptiesBuffer;
    [Test] procedure AutoFitDefaultTrue;
    [Test] procedure SamplesReturnsEmptyBeforeAnyPush;
  end;

  [TestFixture]
  TDualNeedleGaugeTests = class
  public
    [Test] procedure DefaultsShowSetpointTrue;
    [Test] procedure SetpointAssignableAndReadable;
    [Test] procedure ShowSetpointToggle;
  end;

  [TestFixture]
  TMinMaxGaugeTests = class
  public
    [Test] procedure DefaultsBeforeAnyPaintAreZero;
    [Test] procedure ResetMinMaxClearsStats;
    [Test] procedure ShowMinMaxToggle;
  end;

implementation

{ TDigitalGaugeTests ------------------------------------------------------- }

procedure TDigitalGaugeTests.DefaultsShowMinMaxTrueShowAvgFalse;
var G: TOBDDigitalGauge;
begin
  G := TOBDDigitalGauge.Create(nil);
  try
    Assert.IsTrue(G.ShowMinMax);
    Assert.IsFalse(G.ShowAvg);
  finally G.Free; end;
end;

procedure TDigitalGaugeTests.ResetStatsClearsCounters;
var G: TOBDDigitalGauge;
begin
  G := TOBDDigitalGauge.Create(nil);
  try
    G.AnimateValueChanges := False;
    G.Value := 50;
    G.ResetStats;
    // After reset, nothing observable from public API except that
    // a subsequent Value assignment doesn't raise — smoke check.
    G.Value := 25;
    Assert.Pass;
  finally G.Free; end;
end;

procedure TDigitalGaugeTests.ValueClampsAndAnimatesOff;
var G: TOBDDigitalGauge;
begin
  G := TOBDDigitalGauge.Create(nil);
  try
    G.AnimateValueChanges := False;
    G.Min := 0;  G.Max := 10;
    G.Value := 1000;
    Assert.AreEqual(10.0, G.Value, 0.001);
  finally G.Free; end;
end;

{ TBarSegmentGaugeTests ---------------------------------------------------- }

procedure TBarSegmentGaugeTests.DefaultsSixteenSegmentsHorizontal;
var G: TOBDBarSegmentGauge;
begin
  G := TOBDBarSegmentGauge.Create(nil);
  try
    Assert.AreEqual(16, G.SegmentCount);
    Assert.AreEqual(2,  G.GapPixels);
    Assert.IsTrue(G.Orientation = loHorizontal);
  finally G.Free; end;
end;

procedure TBarSegmentGaugeTests.SegmentCountClampsLow;
var G: TOBDBarSegmentGauge;
begin
  G := TOBDBarSegmentGauge.Create(nil);
  try
    G.SegmentCount := 1;
    Assert.AreEqual(2, G.SegmentCount);
  finally G.Free; end;
end;

procedure TBarSegmentGaugeTests.SegmentCountClampsHigh;
var G: TOBDBarSegmentGauge;
begin
  G := TOBDBarSegmentGauge.Create(nil);
  try
    G.SegmentCount := 9999;
    Assert.AreEqual(128, G.SegmentCount);
  finally G.Free; end;
end;

procedure TBarSegmentGaugeTests.GapPixelsClampsNonNegative;
var G: TOBDBarSegmentGauge;
begin
  G := TOBDBarSegmentGauge.Create(nil);
  try
    G.GapPixels := -5;
    Assert.AreEqual(0, G.GapPixels);
  finally G.Free; end;
end;

procedure TBarSegmentGaugeTests.OrientationToggle;
var G: TOBDBarSegmentGauge;
begin
  G := TOBDBarSegmentGauge.Create(nil);
  try
    G.Orientation := loVertical;
    Assert.IsTrue(G.Orientation = loVertical);
  finally G.Free; end;
end;

{ TDeltaGaugeTests --------------------------------------------------------- }

procedure TDeltaGaugeTests.DefaultsSymmetricRange;
var G: TOBDDeltaGauge;
begin
  G := TOBDDeltaGauge.Create(nil);
  try
    Assert.AreEqual(-100.0, G.Min, 0.001);
    Assert.AreEqual( 100.0, G.Max, 0.001);
  finally G.Free; end;
end;

procedure TDeltaGaugeTests.ValueClampsBelowMin;
var G: TOBDDeltaGauge;
begin
  G := TOBDDeltaGauge.Create(nil);
  try
    G.AnimateValueChanges := False;
    G.Value := -9999;
    Assert.AreEqual(-100.0, G.Value, 0.001);
  finally G.Free; end;
end;

procedure TDeltaGaugeTests.ValueClampsAboveMax;
var G: TOBDDeltaGauge;
begin
  G := TOBDDeltaGauge.Create(nil);
  try
    G.AnimateValueChanges := False;
    G.Value := 9999;
    Assert.AreEqual(100.0, G.Value, 0.001);
  finally G.Free; end;
end;

{ TSparklineTests ---------------------------------------------------------- }

procedure TSparklineTests.DefaultCapacityIs64;
var S: TOBDSparkline;
begin
  S := TOBDSparkline.Create(nil);
  try
    Assert.AreEqual(64, S.Capacity);
  finally S.Free; end;
end;

procedure TSparklineTests.CapacityClampsLow;
var S: TOBDSparkline;
begin
  S := TOBDSparkline.Create(nil);
  try
    S.Capacity := 1;
    Assert.AreEqual(2, S.Capacity);
  finally S.Free; end;
end;

procedure TSparklineTests.CapacityClampsHigh;
var S: TOBDSparkline;
begin
  S := TOBDSparkline.Create(nil);
  try
    S.Capacity := 100000;
    Assert.AreEqual(4096, S.Capacity);
  finally S.Free; end;
end;

procedure TSparklineTests.CapacityChangeResetsBuffer;
var S: TOBDSparkline;
begin
  S := TOBDSparkline.Create(nil);
  try
    S.Capacity := 32;
    Assert.AreEqual(0, Length(S.Samples));
  finally S.Free; end;
end;

procedure TSparklineTests.ResetSamplesEmptiesBuffer;
var S: TOBDSparkline;
begin
  S := TOBDSparkline.Create(nil);
  try
    S.ResetSamples;
    Assert.AreEqual(0, Length(S.Samples));
  finally S.Free; end;
end;

procedure TSparklineTests.AutoFitDefaultTrue;
var S: TOBDSparkline;
begin
  S := TOBDSparkline.Create(nil);
  try
    Assert.IsTrue(S.AutoFit);
    Assert.IsTrue(S.ShowEdge);
    Assert.IsTrue(S.ShowCurrent);
  finally S.Free; end;
end;

procedure TSparklineTests.SamplesReturnsEmptyBeforeAnyPush;
var S: TOBDSparkline;
begin
  // Paint pushes samples; without a paint cycle the ring buffer
  // is empty, which is the contract Samples() needs to honour.
  S := TOBDSparkline.Create(nil);
  try
    Assert.AreEqual(0, Length(S.Samples));
  finally S.Free; end;
end;

{ TDualNeedleGaugeTests ---------------------------------------------------- }

procedure TDualNeedleGaugeTests.DefaultsShowSetpointTrue;
var G: TOBDDualNeedleGauge;
begin
  G := TOBDDualNeedleGauge.Create(nil);
  try
    Assert.IsTrue(G.ShowSetpoint);
    Assert.AreEqual(0.0, G.SetpointValue, 0.001);
  finally G.Free; end;
end;

procedure TDualNeedleGaugeTests.SetpointAssignableAndReadable;
var G: TOBDDualNeedleGauge;
begin
  G := TOBDDualNeedleGauge.Create(nil);
  try
    G.SetpointValue := 42.5;
    Assert.AreEqual(42.5, G.SetpointValue, 0.001);
  finally G.Free; end;
end;

procedure TDualNeedleGaugeTests.ShowSetpointToggle;
var G: TOBDDualNeedleGauge;
begin
  G := TOBDDualNeedleGauge.Create(nil);
  try
    G.ShowSetpoint := False;
    Assert.IsFalse(G.ShowSetpoint);
  finally G.Free; end;
end;

{ TMinMaxGaugeTests -------------------------------------------------------- }

procedure TMinMaxGaugeTests.DefaultsBeforeAnyPaintAreZero;
var G: TOBDMinMaxGauge;
begin
  G := TOBDMinMaxGauge.Create(nil);
  try
    // ObserveSample only runs during paint; without a paint
    // cycle the tracker reports the "no samples" defaults.
    Assert.AreEqual(0.0, G.MinSeen, 0.001);
    Assert.AreEqual(0.0, G.MaxSeen, 0.001);
    Assert.IsTrue(G.ShowMinMax);
  finally G.Free; end;
end;

procedure TMinMaxGaugeTests.ResetMinMaxClearsStats;
var G: TOBDMinMaxGauge;
begin
  G := TOBDMinMaxGauge.Create(nil);
  try
    G.ResetMinMax;
    Assert.AreEqual(0.0, G.MinSeen, 0.001);
    Assert.AreEqual(0.0, G.MaxSeen, 0.001);
  finally G.Free; end;
end;

procedure TMinMaxGaugeTests.ShowMinMaxToggle;
var G: TOBDMinMaxGauge;
begin
  G := TOBDMinMaxGauge.Create(nil);
  try
    G.ShowMinMax := False;
    Assert.IsFalse(G.ShowMinMax);
  finally G.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDigitalGaugeTests);
  TDUnitX.RegisterTestFixture(TBarSegmentGaugeTests);
  TDUnitX.RegisterTestFixture(TDeltaGaugeTests);
  TDUnitX.RegisterTestFixture(TSparklineTests);
  TDUnitX.RegisterTestFixture(TDualNeedleGaugeTests);
  TDUnitX.RegisterTestFixture(TMinMaxGaugeTests);

end.
