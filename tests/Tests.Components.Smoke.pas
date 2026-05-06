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
    [Test] procedure LinearGauge_ConstructsAndAcceptsMinMaxValue;
    [Test] procedure LinearGauge_ValueIsClampedIntoRange;
    [Test] procedure LinearGauge_OrientationAndDirectionToggle;
    [Test] procedure Tachometer_ConstructsWithRpmDefaults;
    [Test] procedure Tachometer_ShiftLightActiveAboveShiftPoint;
    [Test] procedure TrendGraph_AddSeries_AndPushValues;
    [Test] procedure TrendGraph_RingBufferOverwritesOldest;
    [Test] procedure TrendGraph_ResizeMaxSamplesPreservesRecent;
    [Test] procedure DtcList_AddRemoveClear;
    [Test] procedure DtcList_SelectedIndexClampsOnRemove;
    [Test] procedure Terminal_LogMethodsAppendInOrder;
    [Test] procedure Terminal_MaxLinesEvictsOldest;
    [Test] procedure Knob_ValueClampsAndSnaps;
    [Test] procedure Knob_OnChangeFires;
    [Test] procedure SegmentedSwitch_AddSegmentsAndSelect;
    [Test] procedure SegmentedSwitch_SelectedIndexClamps;
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
  OBD.LinearGauge,
  OBD.Tachometer,
  OBD.TrendGraph,
  OBD.DtcList,
  OBD.Terminal,
  OBD.Knob,
  OBD.SegmentedSwitch,
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

procedure TComponentSmokeTests.LinearGauge_ConstructsAndAcceptsMinMaxValue;
var
  G: TOBDLinearGauge;
begin
  G := TOBDLinearGauge.Create(nil);
  try
    G.Min := 0;
    G.Max := 200;
    G.Value := 75;
    Assert.AreEqual(Single(0),   G.Min);
    Assert.AreEqual(Single(200), G.Max);
    Assert.AreEqual(Single(75),  G.Value);
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.LinearGauge_ValueIsClampedIntoRange;
var
  G: TOBDLinearGauge;
begin
  G := TOBDLinearGauge.Create(nil);
  try
    G.Min := 0;
    G.Max := 100;
    G.Value := 200;
    Assert.AreEqual(Single(100), G.Value, 'over-Max value must clamp to Max');
    G.Value := -10;
    Assert.AreEqual(Single(0), G.Value, 'under-Min value must clamp to Min');
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.LinearGauge_OrientationAndDirectionToggle;
var
  G: TOBDLinearGauge;
begin
  G := TOBDLinearGauge.Create(nil);
  try
    G.Orientation := loVertical;
    Assert.AreEqual(Ord(loVertical), Ord(G.Orientation));
    G.Direction := ldReversed;
    Assert.AreEqual(Ord(ldReversed), Ord(G.Direction));
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.Tachometer_ConstructsWithRpmDefaults;
var
  T: TOBDTachometer;
begin
  T := TOBDTachometer.Create(nil);
  try
    Assert.AreEqual(Single(0),    T.Min);
    Assert.AreEqual(Single(8000), T.Max);
    Assert.AreEqual(Single(6500), T.RedlineFrom);
    Assert.AreEqual(Single(6000), T.ShiftPoint);
    Assert.IsFalse(T.ShiftLightActive,
      'ShiftLight must be off at default RPM (0)');
  finally
    T.Free;
  end;
end;

procedure TComponentSmokeTests.Tachometer_ShiftLightActiveAboveShiftPoint;
var
  T: TOBDTachometer;
begin
  T := TOBDTachometer.Create(nil);
  try
    // Disable animation so DisplayValue tracks Value immediately.
    T.AnimationEnabled := False;
    T.ShiftPoint := 5000;
    T.Value := 4500;
    Assert.IsFalse(T.ShiftLightActive,
      'ShiftLight off below ShiftPoint');
    T.Value := 5500;
    Assert.IsTrue(T.ShiftLightActive,
      'ShiftLight on above ShiftPoint');
  finally
    T.Free;
  end;
end;

procedure TComponentSmokeTests.TrendGraph_AddSeries_AndPushValues;
var
  G: TOBDTrendGraph;
  Idx: Integer;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    G.MaxSamples := 16;
    Idx := G.AddSeries('RPM', $00FF0000, 0, 8000);
    Assert.AreEqual(0, Idx);
    Assert.AreEqual(1, G.SeriesCount);
    G.PushValue(Idx, 1500);
    G.PushValue(Idx, 2000);
    G.PushValue(Idx, 2500);
    Assert.AreEqual(3, G.Series[0].Count);
    Assert.AreEqual(Single(1500), G.Series[0].Values[0], 'oldest sample');
    Assert.AreEqual(Single(2500), G.Series[0].Values[2], 'newest sample');
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.TrendGraph_RingBufferOverwritesOldest;
var
  G: TOBDTrendGraph;
  Idx, I: Integer;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    G.MaxSamples := 4;
    Idx := G.AddSeries('test', clRed, 0, 100);
    for I := 1 to 6 do G.PushValue(Idx, I);
    // After pushing 6 values into a buffer of 4, oldest two are gone:
    // expected sequence: 3, 4, 5, 6
    Assert.AreEqual(4, G.Series[0].Count);
    Assert.AreEqual(Single(3), G.Series[0].Values[0]);
    Assert.AreEqual(Single(6), G.Series[0].Values[3]);
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.TrendGraph_ResizeMaxSamplesPreservesRecent;
var
  G: TOBDTrendGraph;
  Idx, I: Integer;
begin
  G := TOBDTrendGraph.Create(nil);
  try
    G.MaxSamples := 8;
    Idx := G.AddSeries('test', clRed, 0, 100);
    for I := 1 to 8 do G.PushValue(Idx, I);
    G.MaxSamples := 4;
    // Resize keeps the most recent 4 samples: 5, 6, 7, 8
    Assert.AreEqual(4, G.Series[0].Count);
    Assert.AreEqual(Single(5), G.Series[0].Values[0]);
    Assert.AreEqual(Single(8), G.Series[0].Values[3]);
  finally
    G.Free;
  end;
end;

procedure TComponentSmokeTests.DtcList_AddRemoveClear;
var
  L: TOBDDtcList;
begin
  L := TOBDDtcList.Create(nil);
  try
    Assert.AreEqual(0, L.ItemCount);
    L.AddItem('P0301', 'Cylinder 1 misfire detected', dsWarning, dsActive);
    L.AddItem('P0420', 'Catalyst efficiency below threshold', dsCritical, dsActive);
    L.AddItem('B1234', 'Body code', dsInfo, dsHistory);
    Assert.AreEqual(3, L.ItemCount);
    Assert.AreEqual('P0301', L.Items[0].Code);
    L.RemoveItem(1);
    Assert.AreEqual(2, L.ItemCount);
    Assert.AreEqual('B1234', L.Items[1].Code);
    L.ClearItems;
    Assert.AreEqual(0, L.ItemCount);
    Assert.AreEqual(-1, L.SelectedIndex);
  finally
    L.Free;
  end;
end;

procedure TComponentSmokeTests.DtcList_SelectedIndexClampsOnRemove;
var
  L: TOBDDtcList;
begin
  L := TOBDDtcList.Create(nil);
  try
    L.AddItem('P0001', 'first');
    L.AddItem('P0002', 'second');
    L.AddItem('P0003', 'third');
    L.SelectedIndex := 2;
    L.RemoveItem(2);  // last item gone — selection must clamp
    Assert.IsTrue(L.SelectedIndex < L.ItemCount,
      'SelectedIndex must remain in range after RemoveItem');
  finally
    L.Free;
  end;
end;

procedure TComponentSmokeTests.Terminal_LogMethodsAppendInOrder;
var
  T: TOBDTerminal;
begin
  T := TOBDTerminal.Create(nil);
  try
    T.LogSent('AT Z');
    T.LogReceived('ELM327 v1.5');
    T.LogInfo('protocol auto');
    T.LogError('NO DATA');
    Assert.AreEqual(4, T.LineCount);
    Assert.AreEqual('AT Z',         T.GetLine(0).Text);
    Assert.AreEqual(Ord(tdSent),    Ord(T.GetLine(0).Direction));
    Assert.AreEqual(Ord(tdReceived),Ord(T.GetLine(1).Direction));
    Assert.AreEqual(Ord(tdInfo),    Ord(T.GetLine(2).Direction));
    Assert.AreEqual(Ord(tdError),   Ord(T.GetLine(3).Direction));
  finally
    T.Free;
  end;
end;

procedure TComponentSmokeTests.Terminal_MaxLinesEvictsOldest;
var
  T: TOBDTerminal;
  I: Integer;
begin
  T := TOBDTerminal.Create(nil);
  try
    T.MaxLines := 3;
    for I := 1 to 5 do T.LogSent('cmd ' + IntToStr(I));
    Assert.AreEqual(3, T.LineCount, 'buffer must cap at MaxLines');
    Assert.AreEqual('cmd 3', T.GetLine(0).Text, 'oldest two evicted');
    Assert.AreEqual('cmd 5', T.GetLine(2).Text);
  finally
    T.Free;
  end;
end;

type
  // Helper for capturing OnChange events on the Knob without a global var.
  TKnobChangeRecorder = class
  public
    LastValue: Single;
    Fired: Boolean;
    procedure HandleChange(Sender: TObject; const Value: Single);
  end;

procedure TKnobChangeRecorder.HandleChange(Sender: TObject; const Value: Single);
begin
  Fired := True;
  LastValue := Value;
end;

procedure TComponentSmokeTests.Knob_ValueClampsAndSnaps;
var
  K: TOBDKnob;
begin
  K := TOBDKnob.Create(nil);
  try
    K.Min := 0;
    K.Max := 100;
    K.Step := 5;
    K.Value := 17;
    Assert.AreEqual(Single(15), K.Value, 'Value snaps to nearest Step');
    K.Value := 200;
    Assert.AreEqual(Single(100), K.Value, 'Value clamps to Max');
    K.Value := -10;
    Assert.AreEqual(Single(0), K.Value, 'Value clamps to Min');
  finally
    K.Free;
  end;
end;

procedure TComponentSmokeTests.Knob_OnChangeFires;
var
  K: TOBDKnob;
  Recorder: TKnobChangeRecorder;
begin
  Recorder := TKnobChangeRecorder.Create;
  K := TOBDKnob.Create(nil);
  try
    K.Step := 1;
    K.OnChange := Recorder.HandleChange;
    K.Value := 25;
    Assert.IsTrue(Recorder.Fired, 'OnChange must fire on Value change');
    Assert.AreEqual(Single(25), Recorder.LastValue);
  finally
    K.Free;
    Recorder.Free;
  end;
end;

procedure TComponentSmokeTests.SegmentedSwitch_AddSegmentsAndSelect;
var
  S: TOBDSegmentedSwitch;
begin
  S := TOBDSegmentedSwitch.Create(nil);
  try
    S.Segments.Add('AT');
    S.Segments.Add('OBD');
    S.Segments.Add('UDS');
    Assert.AreEqual(3, S.Segments.Count);
    S.SelectedIndex := 1;
    Assert.AreEqual(1, S.SelectedIndex);
  finally
    S.Free;
  end;
end;

procedure TComponentSmokeTests.SegmentedSwitch_SelectedIndexClamps;
var
  S: TOBDSegmentedSwitch;
begin
  S := TOBDSegmentedSwitch.Create(nil);
  try
    S.Segments.Add('one');
    S.Segments.Add('two');
    S.SelectedIndex := 5;  // out-of-range — clamp
    Assert.AreEqual(1, S.SelectedIndex);
    S.SelectedIndex := -3; // negative — clamp to 0
    Assert.AreEqual(0, S.SelectedIndex);
  finally
    S.Free;
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
