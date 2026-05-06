//------------------------------------------------------------------------------
// UNIT           : Tests.Components.Smoke
// CONTENTS       : Lightweight render-path smoke tests for the visual
//                  components — NOT full image snapshot tests.
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : True image-diff snapshot tests (render to TSkSurface, hash,
//                  compare vs golden) are a v2.2 follow-up — they require a
//                  baseline-capture workflow and golden-image storage layout
//                  this milestone doesn't ship yet. Until then these tests
//                  pin the construction / property-mutation / destruction
//                  cycle so the rendering pipeline can't silently throw.
//------------------------------------------------------------------------------
unit Tests.Components.Smoke;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TComponentSmokeTests = class
  public
    [Test] procedure CircularGauge_ConstructsAndAcceptsMinMaxValue;
    [Test] procedure CircularGauge_ValueIsClampedIntoRange;
    [Test] procedure Led_ConstructsAndAcceptsState;
    [Test] procedure MatrixDisplay_Constructs;
    [Test] procedure TouchHeader_Constructs;
    [Test] procedure TouchStatusbar_Constructs;
    [Test] procedure TouchSubheader_Constructs;
  end;

implementation

uses
  System.Classes,
  OBD.CircularGauge,
  OBD.LED,
  OBD.MatrixDisplay,
  OBD.Touch.Header,
  OBD.Touch.Statusbar,
  OBD.Touch.Subheader;

{ TComponentSmokeTests }

procedure TComponentSmokeTests.CircularGauge_ConstructsAndAcceptsMinMaxValue;
var
  G: TOBDCircularGauge;
begin
  G := TOBDCircularGauge.Create(nil);
  try
    G.Min := 0;
    G.Max := 8000;
    G.Value := 1726;
    Assert.AreEqual(Single(0),    G.Min);
    Assert.AreEqual(Single(8000), G.Max);
    Assert.AreEqual(Single(1726), G.Value);
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.CircularGauge_ValueIsClampedIntoRange;
var
  G: TOBDCircularGauge;
begin
  // Setting Min > current Value should pull Value up; Max < current Value
  // should pull it down. Pins the clamping invariant the gauge enforces.
  G := TOBDCircularGauge.Create(nil);
  try
    G.Min := 0;
    G.Max := 100;
    G.Value := 50;
    G.Min := 60;
    Assert.IsTrue(G.Value >= 60, 'Min push must pull Value upward');

    G.Max := 70;
    G.Value := 80;
    Assert.IsTrue(G.Value <= 70, 'Max ceiling must keep Value below Max');
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.Led_ConstructsAndAcceptsState;
var
  L: TOBDLed;
begin
  L := TOBDLed.Create(nil);
  try
    L.State := lsOn;
    Assert.AreEqual(Ord(lsOn), Ord(L.State));
    L.State := lsOff;
    Assert.AreEqual(Ord(lsOff), Ord(L.State));
    L.State := lsGrayed;
    Assert.AreEqual(Ord(lsGrayed), Ord(L.State));
  finally
    L.Free;
  end;
end;

procedure TComponentSmokeTests.MatrixDisplay_Constructs;
var
  M: TOBDMatrixDisplay;
begin
  M := TOBDMatrixDisplay.Create(nil);
  try
    Assert.IsNotNull(M);
  finally
    M.Free;
  end;
end;

procedure TComponentSmokeTests.TouchHeader_Constructs;
var
  H: TOBDTouchHeader;
begin
  H := TOBDTouchHeader.Create(nil);
  try
    Assert.IsNotNull(H);
  finally
    H.Free;
  end;
end;

procedure TComponentSmokeTests.TouchStatusbar_Constructs;
var
  S: TOBDTouchStatusbar;
begin
  S := TOBDTouchStatusbar.Create(nil);
  try
    Assert.IsNotNull(S);
  finally
    S.Free;
  end;
end;

procedure TComponentSmokeTests.TouchSubheader_Constructs;
var
  S: TOBDTouchSubheader;
begin
  S := TOBDTouchSubheader.Create(nil);
  try
    Assert.IsNotNull(S);
  finally
    S.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TComponentSmokeTests);

end.
