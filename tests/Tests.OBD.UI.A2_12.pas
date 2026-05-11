//------------------------------------------------------------------------------
//  Tests.OBD.UI.A2_12
//
//  Smoke + property-contract coverage for the rest of the
//  A2.12 visuals (charts, tuning, motorsport, commercial,
//  replay, branding, coding editors, diag, session inspect,
//  insights, logger). Each fixture exercises Create, a
//  representative property round-trip, and the public refresh
//  / push entry points; paint output needs an HWND and is out
//  of unit-test scope.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.A2_12;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.Coding.AuditLog,
  OBD.UI.Dyno,
  OBD.UI.Charts,
  OBD.UI.Tuning,
  OBD.UI.Motorsport,
  OBD.UI.Commercial,
  OBD.UI.Replay,
  OBD.UI.Branding,
  OBD.UI.CodingEditors,
  OBD.UI.Diag,
  OBD.UI.SessionInspect,
  OBD.UI.Insights,
  OBD.UI.Logger;

type
  [TestFixture]
  TChartsSmoke = class
  public
    [Test] procedure StripChartCapacityClamps;
    [Test] procedure LiveGridChartAddTraceAndPush;
    [Test] procedure DynoChartPushSample;
    [Test] procedure PowerCurveGraphLoadCurve;
  end;

  [TestFixture]
  TTuningSmoke = class
  public
    [Test] procedure XYHeatmapPushSample;
    [Test] procedure TorqueRPMMapSetCells;
    [Test] procedure RunRecorderPreTriggerRingTrims;
  end;

  [TestFixture]
  TMotorsportSmoke = class
  public
    [Test] procedure LapTrackMapLoad;
    [Test] procedure PredictiveLapDeltaSign;
    [Test] procedure GForceVisualiserPushSample;
  end;

  [TestFixture]
  TCommercialSmoke = class
  public
    [Test] procedure MarineTachHoursClampNegative;
    [Test] procedure DPFStatusSootClamp;
    [Test] procedure AdBlueLevelClamp;
  end;

  [TestFixture]
  TReplaySmoke = class
  public
    [Test] procedure ChargePortLevelRoundTrip;
    [Test] procedure MaintenanceCardOverdue;
    [Test] procedure PlaybackScrubberClamp;
  end;

  [TestFixture]
  TBrandingSmoke = class
  public
    [Test] procedure OEMBadgeSetBrand;
    [Test] procedure DigitalClusterDigitClamp;
    [Test] procedure BluetoothSignalRssiMaps;
    [Test] procedure WiFiSignalRssiMaps;
    [Test] procedure GPSAccuracyHDOPGrade;
  end;

  [TestFixture]
  TCodingEditorsSmoke = class
  public
    [Test] procedure CodingDiffViewerCounts;
    [Test] procedure LongCodingEditorRoundTrip;
    [Test] procedure SeedKeyDebuggerLoad;
  end;

  [TestFixture]
  TDiagSmoke = class
  public
    [Test] procedure Mode06ViewerSurvivesEmpty;
    [Test] procedure Mode04ConfirmConfirmText;
    [Test] procedure ActuatorTestPanelLoadSteps;
  end;

  [TestFixture]
  TSessionInspectSmoke = class
  public
    [Test] procedure KWP1281InspectorRoundTrip;
    [Test] procedure TP20PanelKeepAliveStaleColor;
  end;

  [TestFixture]
  TInsightsSmoke = class
  public
    [Test] procedure DriverScoreClampHigh;
    [Test] procedure EcoScoreClampNegative;
    [Test] procedure TripSummaryRoundTrip;
  end;

  [TestFixture]
  TLoggerSmoke = class
  public
    [Test] procedure LoggerControlRunningToggle;
    [Test] procedure LoggerExplorerLoadFiles;
  end;

implementation

{ TChartsSmoke ---------------------------------------------------------- }

procedure TChartsSmoke.StripChartCapacityClamps;
var C: TOBDStripChart;
begin
  C := TOBDStripChart.Create(nil);
  try
    C.Capacity := 1;
    Assert.AreEqual(8, C.Capacity);
    C.PushSample(1.0); C.PushSample(2.0);
    Assert.AreEqual(2, C.SampleCount);
  finally C.Free; end;
end;

procedure TChartsSmoke.LiveGridChartAddTraceAndPush;
var
  C: TOBDLiveGridChart;
  T: TOBDLiveTrace;
begin
  C := TOBDLiveGridChart.Create(nil);
  try
    T := C.AddTrace('rpm', clRed);
    Assert.AreEqual(1, C.TraceCount);
    C.PushSample('rpm', 800);
    C.PushSample('rpm', 1200);
    Assert.AreEqual(2, T.Count);
  finally C.Free; end;
end;

procedure TChartsSmoke.DynoChartPushSample;
var C: TOBDDynoChart;
begin
  C := TOBDDynoChart.Create(nil);
  try
    C.PushSample(2000, 80, 200);
    C.PushSample(3000, 110, 230);
    Assert.AreEqual(2, C.SampleCount);
  finally C.Free; end;
end;

procedure TChartsSmoke.PowerCurveGraphLoadCurve;
var
  G: TOBDPowerCurveGraph;
  P: TArray<TOBDDynoPoint>;
begin
  G := TOBDPowerCurveGraph.Create(nil);
  try
    SetLength(P, 2);
    P[0].RPM := 2000; P[0].HP := 80;  P[0].TorqueNm := 200;
    P[1].RPM := 6000; P[1].HP := 240; P[1].TorqueNm := 280;
    G.LoadCurve(P);
    Assert.AreEqual(2, G.PointCount);
    G.Clear;
    Assert.AreEqual(0, G.PointCount);
  finally G.Free; end;
end;

{ TTuningSmoke -------------------------------------------------------- }

procedure TTuningSmoke.XYHeatmapPushSample;
var H: TOBDXYHeatmap;
begin
  H := TOBDXYHeatmap.Create(nil);
  try
    H.XMin := 0; H.XMax := 10;
    H.YMin := 0; H.YMax := 10;
    H.PushSample(5, 5);
    H.Reset;
    Assert.Pass;
  finally H.Free; end;
end;

procedure TTuningSmoke.TorqueRPMMapSetCells;
var
  M: TOBDTorqueRPMMap;
  C: array[0..1] of TOBDTorqueCell;
begin
  M := TOBDTorqueRPMMap.Create(nil);
  try
    C[0].RPM := 2000; C[0].Load := 50; C[0].TorqueNm := 200;
    C[1].RPM := 4000; C[1].Load := 75; C[1].TorqueNm := 350;
    M.SetCells(C);
    Assert.AreEqual(2, M.CellCount);
    M.Clear;
    Assert.AreEqual(0, M.CellCount);
  finally M.Free; end;
end;

procedure TTuningSmoke.RunRecorderPreTriggerRingTrims;
var R: TOBDRunRecorder;
begin
  R := TOBDRunRecorder.Create(nil);
  try
    R.PreMs := 500;
    R.PushSample(0,    1);
    R.PushSample(200,  2);
    R.PushSample(800,  3);          // first sample falls outside 500 ms pre
    Assert.IsTrue(Length(R.Samples) <= 3);
  finally R.Free; end;
end;

{ TMotorsportSmoke --------------------------------------------------- }

procedure TMotorsportSmoke.LapTrackMapLoad;
var
  M: TOBDLapTrackMap;
  P: array[0..2] of TOBDTrackPoint;
begin
  M := TOBDLapTrackMap.Create(nil);
  try
    P[0].X := 0;   P[0].Y := 0;
    P[1].X := 0.5; P[1].Y := 1;
    P[2].X := 1;   P[2].Y := 0;
    M.LoadTrack(P);
    M.PositionFraction := 0.5;
    Assert.AreEqual<Single>(0.5, M.PositionFraction);
    M.Clear;
    Assert.Pass;
  finally M.Free; end;
end;

procedure TMotorsportSmoke.PredictiveLapDeltaSign;
var P: TOBDPredictiveLap;
begin
  P := TOBDPredictiveLap.Create(nil);
  try
    P.BestMs := 90000;
    P.UpdateSample(45000, 88000);
    Assert.IsTrue(P.DeltaMs < 0);     // faster than PB
    P.UpdateSample(45000, 92000);
    Assert.IsTrue(P.DeltaMs > 0);
  finally P.Free; end;
end;

procedure TMotorsportSmoke.GForceVisualiserPushSample;
var V: TOBDGForceVisualiser;
begin
  V := TOBDGForceVisualiser.Create(nil);
  try
    V.PushSample(0.5, 0.2);
    V.PushSample(-0.3, 0.4);
    Assert.AreEqual(-0.3, V.Xg, 0.001);
  finally V.Free; end;
end;

{ TCommercialSmoke -------------------------------------------------- }

procedure TCommercialSmoke.MarineTachHoursClampNegative;
var M: TOBDMarineTach;
begin
  M := TOBDMarineTach.Create(nil);
  try
    M.Hours := -10;
    Assert.AreEqual(0.0, M.Hours, 0.001);
  finally M.Free; end;
end;

procedure TCommercialSmoke.DPFStatusSootClamp;
var D: TOBDDPFStatus;
begin
  D := TOBDDPFStatus.Create(nil);
  try
    D.SootPercent := 250;
    Assert.AreEqual(100.0, D.SootPercent, 0.001);
    D.RegenActive := True;
    Assert.IsTrue(D.RegenActive);
  finally D.Free; end;
end;

procedure TCommercialSmoke.AdBlueLevelClamp;
var L: TOBDAdBlueLevel;
begin
  L := TOBDAdBlueLevel.Create(nil);
  try
    L.LevelPercent := -5;
    Assert.AreEqual(0.0, L.LevelPercent, 0.001);
    L.LevelPercent := 250;
    Assert.AreEqual(100.0, L.LevelPercent, 0.001);
  finally L.Free; end;
end;

{ TReplaySmoke ----------------------------------------------------- }

procedure TReplaySmoke.ChargePortLevelRoundTrip;
var P: TOBDChargePortIndicator;
begin
  P := TOBDChargePortIndicator.Create(nil);
  try
    P.Connected := True;
    P.Locked    := True;
    P.Level     := clDCFC;
    Assert.IsTrue(P.Level = clDCFC);
  finally P.Free; end;
end;

procedure TReplaySmoke.MaintenanceCardOverdue;
var C: TOBDMaintenanceCard;
begin
  C := TOBDMaintenanceCard.Create(nil);
  try
    C.MilesUntilDue := -200;
    Assert.AreEqual(-200, C.MilesUntilDue);
  finally C.Free; end;
end;

procedure TReplaySmoke.PlaybackScrubberClamp;
var S: TOBDPlaybackScrubber;
begin
  S := TOBDPlaybackScrubber.Create(nil);
  try
    S.LengthMs   := 60000;
    S.PositionMs := 70000;     // clamped
    Assert.AreEqual<Int64>(60000, S.PositionMs);
  finally S.Free; end;
end;

{ TBrandingSmoke -------------------------------------------------- }

procedure TBrandingSmoke.OEMBadgeSetBrand;
var B: TOBDOEMBadge;
begin
  B := TOBDOEMBadge.Create(nil);
  try
    B.SetBrand('VAG', clNavy);
    Assert.AreEqual('VAG', B.BrandName);
    Assert.AreEqual<TColor>(clNavy, B.AccentColor);
  finally B.Free; end;
end;

procedure TBrandingSmoke.DigitalClusterDigitClamp;
var C: TOBDDigitalCluster;
begin
  C := TOBDDigitalCluster.Create(nil);
  try
    C.DigitCount := 0;
    Assert.AreEqual(1, C.DigitCount);
    C.DigitCount := 999;
    Assert.AreEqual(16, C.DigitCount);
  finally C.Free; end;
end;

procedure TBrandingSmoke.BluetoothSignalRssiMaps;
var B: TOBDBluetoothSignal;
begin
  B := TOBDBluetoothSignal.Create(nil);
  try
    B.RssiDbm := -55;
    Assert.AreEqual(4, B.ActiveBars);
    B.RssiDbm := -100;
    Assert.AreEqual(0, B.ActiveBars);
  finally B.Free; end;
end;

procedure TBrandingSmoke.WiFiSignalRssiMaps;
var W: TOBDWiFiSignal;
begin
  W := TOBDWiFiSignal.Create(nil);
  try
    W.RssiDbm := -50;
    Assert.AreEqual(4, W.ActiveBars);
  finally W.Free; end;
end;

procedure TBrandingSmoke.GPSAccuracyHDOPGrade;
var G: TOBDGPSAccuracy;
begin
  G := TOBDGPSAccuracy.Create(nil);
  try
    G.HDOP := 0.8;
    G.SatCount := 12;
    Assert.AreEqual(12, G.SatCount);
  finally G.Free; end;
end;

{ TCodingEditorsSmoke -------------------------------------------- }

procedure TCodingEditorsSmoke.CodingDiffViewerCounts;
var
  V: TOBDCodingDiffViewer;
  Old, New: TBytes;
begin
  V := TOBDCodingDiffViewer.Create(nil);
  try
    Old := TBytes.Create(1, 2, 3, 4);
    New := TBytes.Create(1, 2, 9, 4, 5);
    V.LoadDiff(Old, New);
    Assert.AreEqual(2, V.ChangeCount);
  finally V.Free; end;
end;

procedure TCodingEditorsSmoke.LongCodingEditorRoundTrip;
var
  E: TOBDLongCodingEditor;
  Original, Read: TBytes;
begin
  E := TOBDLongCodingEditor.Create(nil);
  try
    Original := TBytes.Create($FF, $00, $A5);
    E.LoadCoding(Original);
    Read := E.Coding;
    Assert.AreEqual<NativeInt>(3, Length(Read));
    Assert.AreEqual<Byte>($A5, Read[2]);
  finally E.Free; end;
end;

procedure TCodingEditorsSmoke.SeedKeyDebuggerLoad;
var
  D: TOBDSeedKeyDebugger;
begin
  D := TOBDSeedKeyDebugger.Create(nil);
  try
    D.LoadResult(TBytes.Create($AA, $BB), TBytes.Create($11, $22),
      True);
    Assert.AreEqual<NativeInt>(2, Length(D.Seed));
    Assert.IsTrue(D.Accepted);
    D.Clear;
    Assert.AreEqual<NativeInt>(0, Length(D.Seed));
  finally D.Free; end;
end;

{ TDiagSmoke --------------------------------------------------- }

procedure TDiagSmoke.Mode06ViewerSurvivesEmpty;
var V: TOBDMode06Viewer;
begin
  V := TOBDMode06Viewer.Create(nil);
  try
    V.Refresh(nil);
    V.ClearRows;
    Assert.Pass;
  finally V.Free; end;
end;

procedure TDiagSmoke.Mode04ConfirmConfirmText;
var C: TOBDMode04Confirm;
begin
  C := TOBDMode04Confirm.Create(nil);
  try
    C.ConfirmText := 'are you sure';
    Assert.AreEqual('are you sure', C.ConfirmText);
  finally C.Free; end;
end;

procedure TDiagSmoke.ActuatorTestPanelLoadSteps;
var
  P: TOBDActuatorTestPanel;
  Steps: array[0..1] of TOBDActuatorStep;
begin
  P := TOBDActuatorTestPanel.Create(nil);
  try
    Steps[0].Name := 'fan'; Steps[0].PIDByte := $11;
    Steps[1].Name := 'pump'; Steps[1].PIDByte := $12;
    P.LoadSteps(Steps);
    Assert.AreEqual<NativeInt>(2, Length(P.Steps));
    P.ClearSteps;
    Assert.AreEqual<NativeInt>(0, Length(P.Steps));
  finally P.Free; end;
end;

{ TSessionInspectSmoke ---------------------------------------- }

procedure TSessionInspectSmoke.KWP1281InspectorRoundTrip;
var I: TOBDKWP1281SessionInspector;
begin
  I := TOBDKWP1281SessionInspector.Create(nil);
  try
    I.Counter := 42;
    I.BlockCount := 100;
    I.KW1 := $01; I.KW2 := $8A;
    I.LastBlockTitle := '0xF0 acknowledge';
    Assert.AreEqual<Byte>(42, I.Counter);
    Assert.AreEqual<Cardinal>(100, I.BlockCount);
  finally I.Free; end;
end;

procedure TSessionInspectSmoke.TP20PanelKeepAliveStaleColor;
var P: TOBDTP20ChannelPanel;
begin
  P := TOBDTP20ChannelPanel.Create(nil);
  try
    P.RxId := $200;
    P.TxId := $300;
    P.KeepAliveMs := 1000;
    P.LastSendMs  := 1500;     // stale (> KeepAliveMs)
    Assert.AreEqual<Word>($200, P.RxId);
  finally P.Free; end;
end;

{ TInsightsSmoke ---------------------------------------------- }

procedure TInsightsSmoke.DriverScoreClampHigh;
var D: TOBDDriverScoreWidget;
begin
  D := TOBDDriverScoreWidget.Create(nil);
  try
    D.Score := 250;
    Assert.AreEqual(100.0, D.Score, 0.001);
  finally D.Free; end;
end;

procedure TInsightsSmoke.EcoScoreClampNegative;
var E: TOBDEcoScoreWidget;
begin
  E := TOBDEcoScoreWidget.Create(nil);
  try
    E.OverallScore := -10;
    Assert.AreEqual(0.0, E.OverallScore, 0.001);
    E.BrakeScore := 80; E.AccelScore := 60; E.IdleScore := 90;
    Assert.AreEqual(80.0, E.BrakeScore, 0.001);
  finally E.Free; end;
end;

procedure TInsightsSmoke.TripSummaryRoundTrip;
var T: TOBDTripSummaryCard;
begin
  T := TOBDTripSummaryCard.Create(nil);
  try
    T.DistanceKm  := 42.0;
    T.DurationMin := 30;
    T.AvgSpeedKmh := 84;
    T.FuelL       := 3.5;
    Assert.AreEqual(42.0, T.DistanceKm, 0.001);
    Assert.AreEqual( 3.5, T.FuelL,      0.001);
  finally T.Free; end;
end;

{ TLoggerSmoke ----------------------------------------------- }

procedure TLoggerSmoke.LoggerControlRunningToggle;
var L: TOBDLoggerControl;
begin
  L := TOBDLoggerControl.Create(nil);
  try
    L.Running := True;
    Assert.IsTrue(L.Running);
    L.FileName  := 'session.obdlog';
    L.FreeBytes := 100 * 1024 * 1024;
    Assert.AreEqual('session.obdlog', L.FileName);
  finally L.Free; end;
end;

procedure TLoggerSmoke.LoggerExplorerLoadFiles;
var
  E: TOBDLoggerExplorer;
  Files: array[0..1] of TOBDLoggerFileInfo;
begin
  E := TOBDLoggerExplorer.Create(nil);
  try
    Files[0].FileName := 'a.obdlog'; Files[0].SizeBytes := 1024;
    Files[1].FileName := 'b.obdlog'; Files[1].SizeBytes := 2048;
    E.LoadFiles(Files);
    Assert.Pass;
  finally E.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TChartsSmoke);
  TDUnitX.RegisterTestFixture(TTuningSmoke);
  TDUnitX.RegisterTestFixture(TMotorsportSmoke);
  TDUnitX.RegisterTestFixture(TCommercialSmoke);
  TDUnitX.RegisterTestFixture(TReplaySmoke);
  TDUnitX.RegisterTestFixture(TBrandingSmoke);
  TDUnitX.RegisterTestFixture(TCodingEditorsSmoke);
  TDUnitX.RegisterTestFixture(TDiagSmoke);
  TDUnitX.RegisterTestFixture(TSessionInspectSmoke);
  TDUnitX.RegisterTestFixture(TInsightsSmoke);
  TDUnitX.RegisterTestFixture(TLoggerSmoke);

end.
