//------------------------------------------------------------------------------
//  Tests.OBD.UI.Telltales
//
//  Non-visual contract for the four A2.4 telltales
//  (TOBDMILLamp / TOBDDTCBadge / TOBDReadinessLamp /
//  TOBDDashLamp). Paint + flash timing are out of unit-test
//  scope.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Telltales;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Telltales;

type
  [TestFixture]
  TMILLampTests = class
  public
    [Test] procedure DefaultsOffWarningColor;
    [Test] procedure StateRoundTrip;
    [Test] procedure FlashPeriodClampsLow;
  end;

  [TestFixture]
  TDTCBadgeTests = class
  public
    [Test] procedure DefaultsCountZeroPulseEnabled;
    [Test] procedure CountRoundTrip;
    [Test] procedure NegativeCountIsLegal;
    [Test] procedure PulseToggle;
  end;

  [TestFixture]
  TReadinessLampTests = class
  public
    [Test] procedure DefaultsNotSupported;
    [Test] procedure TriStateRoundTrip;
  end;

  [TestFixture]
  TDashLampTests = class
  public
    [Test] procedure DefaultsOilGlyphOff;
    [Test] procedure GlyphRoundTrip;
    [Test] procedure StateRoundTrip;
  end;

implementation

{ TMILLampTests ------------------------------------------------------------ }

procedure TMILLampTests.DefaultsOffWarningColor;
var L: TOBDMILLamp;
begin
  L := TOBDMILLamp.Create(nil);
  try
    Assert.IsTrue(L.State = tsOff);
    Assert.AreEqual<Cardinal>(600, L.FlashPeriodMs);
  finally L.Free; end;
end;

procedure TMILLampTests.StateRoundTrip;
var L: TOBDMILLamp;
begin
  L := TOBDMILLamp.Create(nil);
  try
    L.State := tsOn;       Assert.IsTrue(L.State = tsOn);
    L.State := tsFlashing; Assert.IsTrue(L.State = tsFlashing);
    L.State := tsOff;      Assert.IsTrue(L.State = tsOff);
  finally L.Free; end;
end;

procedure TMILLampTests.FlashPeriodClampsLow;
var L: TOBDMILLamp;
begin
  L := TOBDMILLamp.Create(nil);
  try
    L.FlashPeriodMs := 1;
    Assert.AreEqual<Cardinal>(100, L.FlashPeriodMs);
  finally L.Free; end;
end;

{ TDTCBadgeTests ----------------------------------------------------------- }

procedure TDTCBadgeTests.DefaultsCountZeroPulseEnabled;
var B: TOBDDTCBadge;
begin
  B := TOBDDTCBadge.Create(nil);
  try
    Assert.AreEqual(0, B.Count);
    Assert.IsTrue(B.PulseOnChange);
  finally B.Free; end;
end;

procedure TDTCBadgeTests.CountRoundTrip;
var B: TOBDDTCBadge;
begin
  B := TOBDDTCBadge.Create(nil);
  try
    B.Count := 7;
    Assert.AreEqual(7, B.Count);
  finally B.Free; end;
end;

procedure TDTCBadgeTests.NegativeCountIsLegal;
var B: TOBDDTCBadge;
begin
  B := TOBDDTCBadge.Create(nil);
  try
    // Negative = hidden state, used to represent "unknown".
    B.Count := -1;
    Assert.AreEqual(-1, B.Count);
  finally B.Free; end;
end;

procedure TDTCBadgeTests.PulseToggle;
var B: TOBDDTCBadge;
begin
  B := TOBDDTCBadge.Create(nil);
  try
    B.PulseOnChange := False;
    Assert.IsFalse(B.PulseOnChange);
  finally B.Free; end;
end;

{ TReadinessLampTests ----------------------------------------------------- }

procedure TReadinessLampTests.DefaultsNotSupported;
var L: TOBDReadinessLamp;
begin
  L := TOBDReadinessLamp.Create(nil);
  try
    Assert.IsTrue(L.State = rsNotSupported);
  finally L.Free; end;
end;

procedure TReadinessLampTests.TriStateRoundTrip;
var L: TOBDReadinessLamp;
begin
  L := TOBDReadinessLamp.Create(nil);
  try
    L.State := rsIncomplete; Assert.IsTrue(L.State = rsIncomplete);
    L.State := rsComplete;   Assert.IsTrue(L.State = rsComplete);
    L.State := rsNotSupported;
    Assert.IsTrue(L.State = rsNotSupported);
  finally L.Free; end;
end;

{ TDashLampTests ----------------------------------------------------------- }

procedure TDashLampTests.DefaultsOilGlyphOff;
var L: TOBDDashLamp;
begin
  L := TOBDDashLamp.Create(nil);
  try
    Assert.IsTrue(L.Glyph = dgOil);
    Assert.IsTrue(L.State = tsOff);
  finally L.Free; end;
end;

procedure TDashLampTests.GlyphRoundTrip;
var L: TOBDDashLamp;
begin
  L := TOBDDashLamp.Create(nil);
  try
    L.Glyph := dgBattery;   Assert.IsTrue(L.Glyph = dgBattery);
    L.Glyph := dgTPMS;      Assert.IsTrue(L.Glyph = dgTPMS);
    L.Glyph := dgEngineTemp;
    Assert.IsTrue(L.Glyph = dgEngineTemp);
  finally L.Free; end;
end;

procedure TDashLampTests.StateRoundTrip;
var L: TOBDDashLamp;
begin
  L := TOBDDashLamp.Create(nil);
  try
    L.State := tsOn;       Assert.IsTrue(L.State = tsOn);
    L.State := tsFlashing; Assert.IsTrue(L.State = tsFlashing);
  finally L.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMILLampTests);
  TDUnitX.RegisterTestFixture(TDTCBadgeTests);
  TDUnitX.RegisterTestFixture(TReadinessLampTests);
  TDUnitX.RegisterTestFixture(TDashLampTests);

end.
