//------------------------------------------------------------------------------
//  Tests.OBD.UI.Shift
//
//  Non-visual contract for the A2.5 shift / gear visuals.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Shift;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Shift;

type
  [TestFixture]
  TShiftLightTests = class
  public
    [Test] procedure DefaultsRange4000to7000;
    [Test] procedure RPMRoundTrip;
    [Test] procedure FlashPeriodClampsLow;
  end;

  [TestFixture]
  TShiftLightBarTests = class
  public
    [Test] procedure DefaultsTenSegments;
    [Test] procedure SegmentCountClampsLow;
    [Test] procedure SegmentCountClampsHigh;
    [Test] procedure GapPixelsClampsNegative;
  end;

  [TestFixture]
  TGearIndicatorTests = class
  public
    [Test] procedure DefaultsGearN;
    [Test] procedure GearCharRoundTrip;
    [Test] procedure SetGearIntInRange;
    [Test] procedure SetGearIntOutOfRangeRendersBlank;
  end;

implementation

{ TShiftLightTests --------------------------------------------------------- }

procedure TShiftLightTests.DefaultsRange4000to7000;
var L: TOBDShiftLight;
begin
  L := TOBDShiftLight.Create(nil);
  try
    Assert.AreEqual(4000.0, L.RPMStart,   0.001);
    Assert.AreEqual(7000.0, L.RPMRedline, 0.001);
    Assert.AreEqual<Cardinal>(200, L.FlashPeriodMs);
  finally L.Free; end;
end;

procedure TShiftLightTests.RPMRoundTrip;
var L: TOBDShiftLight;
begin
  L := TOBDShiftLight.Create(nil);
  try
    L.RPM := 6500;
    Assert.AreEqual(6500.0, L.RPM, 0.001);
  finally L.Free; end;
end;

procedure TShiftLightTests.FlashPeriodClampsLow;
var L: TOBDShiftLight;
begin
  L := TOBDShiftLight.Create(nil);
  try
    L.FlashPeriodMs := 10;
    Assert.AreEqual<Cardinal>(50, L.FlashPeriodMs);
  finally L.Free; end;
end;

{ TShiftLightBarTests ------------------------------------------------------ }

procedure TShiftLightBarTests.DefaultsTenSegments;
var B: TOBDShiftLightBar;
begin
  B := TOBDShiftLightBar.Create(nil);
  try
    Assert.AreEqual(10, B.SegmentCount);
    Assert.AreEqual(3,  B.GapPixels);
  finally B.Free; end;
end;

procedure TShiftLightBarTests.SegmentCountClampsLow;
var B: TOBDShiftLightBar;
begin
  B := TOBDShiftLightBar.Create(nil);
  try
    B.SegmentCount := 1;
    Assert.AreEqual(3, B.SegmentCount);
  finally B.Free; end;
end;

procedure TShiftLightBarTests.SegmentCountClampsHigh;
var B: TOBDShiftLightBar;
begin
  B := TOBDShiftLightBar.Create(nil);
  try
    B.SegmentCount := 9999;
    Assert.AreEqual(64, B.SegmentCount);
  finally B.Free; end;
end;

procedure TShiftLightBarTests.GapPixelsClampsNegative;
var B: TOBDShiftLightBar;
begin
  B := TOBDShiftLightBar.Create(nil);
  try
    B.GapPixels := -5;
    Assert.AreEqual(0, B.GapPixels);
  finally B.Free; end;
end;

{ TGearIndicatorTests ----------------------------------------------------- }

procedure TGearIndicatorTests.DefaultsGearN;
var G: TOBDGearIndicator;
begin
  G := TOBDGearIndicator.Create(nil);
  try
    Assert.AreEqual<Char>('N', G.Gear);
  finally G.Free; end;
end;

procedure TGearIndicatorTests.GearCharRoundTrip;
var G: TOBDGearIndicator;
begin
  G := TOBDGearIndicator.Create(nil);
  try
    G.Gear := '3';     Assert.AreEqual<Char>('3', G.Gear);
    G.Gear := 'R';     Assert.AreEqual<Char>('R', G.Gear);
    G.Gear := 'P';     Assert.AreEqual<Char>('P', G.Gear);
  finally G.Free; end;
end;

procedure TGearIndicatorTests.SetGearIntInRange;
var G: TOBDGearIndicator;
begin
  G := TOBDGearIndicator.Create(nil);
  try
    G.SetGearInt(5);
    Assert.AreEqual<Char>('5', G.Gear);
  finally G.Free; end;
end;

procedure TGearIndicatorTests.SetGearIntOutOfRangeRendersBlank;
var G: TOBDGearIndicator;
begin
  G := TOBDGearIndicator.Create(nil);
  try
    G.SetGearInt(99);
    Assert.AreEqual<Char>(' ', G.Gear);
  finally G.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TShiftLightTests);
  TDUnitX.RegisterTestFixture(TShiftLightBarTests);
  TDUnitX.RegisterTestFixture(TGearIndicatorTests);

end.
