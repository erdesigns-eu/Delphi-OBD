//------------------------------------------------------------------------------
//  Tests.OBD.UI.Indicators
//
//  Non-visual contract for the two A2.4 indicators
//  (TOBDLED + TOBDMatrixDisplay). Paint pipeline + flash
//  timing live behind a windowed message loop and are out of
//  unit-test scope.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Indicators;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Indicators;

type
  [TestFixture]
  TLEDTests = class
  public
    [Test] procedure DefaultsOffStateAndDefaults;
    [Test] procedure StateRoundTrip;
    [Test] procedure DiameterClampsLow;
    [Test] procedure DiameterClampsHigh;
    [Test] procedure FlashPeriodClampsLow;
  end;

  [TestFixture]
  TMatrixDisplayTests = class
  public
    [Test] procedure DefaultsEmptyTextAccent;
    [Test] procedure SetTextRoundTrip;
    [Test] procedure DotSizeClampsLow;
    [Test] procedure DotSizeClampsHigh;
    [Test] procedure DotGapClampsNegative;
    [Test] procedure ScrollSpeedZeroDisablesScroll;
  end;

implementation

{ TLEDTests --------------------------------------------------------------- }

procedure TLEDTests.DefaultsOffStateAndDefaults;
var L: TOBDLED;
begin
  L := TOBDLED.Create(nil);
  try
    Assert.IsTrue(L.State = lsOff);
    Assert.AreEqual(16, L.Diameter);
    Assert.AreEqual<Cardinal>(700, L.FlashPeriodMs);
  finally L.Free; end;
end;

procedure TLEDTests.StateRoundTrip;
var L: TOBDLED;
begin
  L := TOBDLED.Create(nil);
  try
    L.State := lsOn;       Assert.IsTrue(L.State = lsOn);
    L.State := lsFlashing; Assert.IsTrue(L.State = lsFlashing);
    L.State := lsOff;      Assert.IsTrue(L.State = lsOff);
  finally L.Free; end;
end;

procedure TLEDTests.DiameterClampsLow;
var L: TOBDLED;
begin
  L := TOBDLED.Create(nil);
  try
    L.Diameter := 0;
    Assert.AreEqual(4, L.Diameter);
  finally L.Free; end;
end;

procedure TLEDTests.DiameterClampsHigh;
var L: TOBDLED;
begin
  L := TOBDLED.Create(nil);
  try
    L.Diameter := 9999;
    Assert.AreEqual(512, L.Diameter);
  finally L.Free; end;
end;

procedure TLEDTests.FlashPeriodClampsLow;
var L: TOBDLED;
begin
  L := TOBDLED.Create(nil);
  try
    L.FlashPeriodMs := 10;
    Assert.AreEqual<Cardinal>(100, L.FlashPeriodMs);
  finally L.Free; end;
end;

{ TMatrixDisplayTests ------------------------------------------------------ }

procedure TMatrixDisplayTests.DefaultsEmptyTextAccent;
var M: TOBDMatrixDisplay;
begin
  M := TOBDMatrixDisplay.Create(nil);
  try
    Assert.AreEqual('', M.Text);
    Assert.AreEqual(3, M.DotSize);
    Assert.AreEqual(1, M.DotGap);
    Assert.AreEqual<Cardinal>(90, M.ScrollSpeedMs);
  finally M.Free; end;
end;

procedure TMatrixDisplayTests.SetTextRoundTrip;
var M: TOBDMatrixDisplay;
begin
  M := TOBDMatrixDisplay.Create(nil);
  try
    M.Text := 'HELLO 123';
    Assert.AreEqual('HELLO 123', M.Text);
  finally M.Free; end;
end;

procedure TMatrixDisplayTests.DotSizeClampsLow;
var M: TOBDMatrixDisplay;
begin
  M := TOBDMatrixDisplay.Create(nil);
  try
    M.DotSize := 0;
    Assert.AreEqual(1, M.DotSize);
  finally M.Free; end;
end;

procedure TMatrixDisplayTests.DotSizeClampsHigh;
var M: TOBDMatrixDisplay;
begin
  M := TOBDMatrixDisplay.Create(nil);
  try
    M.DotSize := 999;
    Assert.AreEqual(32, M.DotSize);
  finally M.Free; end;
end;

procedure TMatrixDisplayTests.DotGapClampsNegative;
var M: TOBDMatrixDisplay;
begin
  M := TOBDMatrixDisplay.Create(nil);
  try
    M.DotGap := -5;
    Assert.AreEqual(0, M.DotGap);
  finally M.Free; end;
end;

procedure TMatrixDisplayTests.ScrollSpeedZeroDisablesScroll;
var M: TOBDMatrixDisplay;
begin
  M := TOBDMatrixDisplay.Create(nil);
  try
    M.Text := 'this is a long marquee that would scroll';
    M.ScrollSpeedMs := 0;
    Assert.AreEqual<Cardinal>(0, M.ScrollSpeedMs);
    // Smoke: no exception; static text path.
  finally M.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLEDTests);
  TDUnitX.RegisterTestFixture(TMatrixDisplayTests);

end.
