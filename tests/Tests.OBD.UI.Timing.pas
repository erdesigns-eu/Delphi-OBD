//------------------------------------------------------------------------------
//  Tests.OBD.UI.Timing
//
//  Non-visual contract for the A2.5 drag / lap / accel-graph
//  visuals. The state-machine transitions are exercised by
//  driving the public Speed setter (drag) / StartLap+EndLap
//  (lap) / PushSample (accel) — stopwatch tick + repaint are
//  out of unit-test scope.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Timing;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Timing;

type
  [TestFixture]
  TDragTimerTests = class
  public
    [Test] procedure DefaultsIdleZeroToHundred;
    [Test] procedure ArmTransitionsToArmed;
    [Test] procedure SpeedAboveStartArmedTransitionsToCapturing;
    [Test] procedure SpeedAboveTargetTransitionsToDone;
    [Test] procedure ResetClearsLastKeepsBest;
    [Test] procedure ResetBestClearsBest;
    [Test] procedure FormatTimeRendersMmSsHh;
  end;

  [TestFixture]
  TLapTimerTests = class
  public
    [Test] procedure DefaultsNotRunningZeroLaps;
    [Test] procedure StartLapSetsRunning;
    [Test] procedure EndLapIncrementsCount;
    [Test] procedure ResetClearsAll;
  end;

  [TestFixture]
  TAccelGraphTests = class
  public
    [Test] procedure DefaultsCapacity512;
    [Test] procedure CapacityClampsLow;
    [Test] procedure CapacityClampsHigh;
    [Test] procedure PushSampleIncrementsCount;
    [Test] procedure ResetClearsSamples;
    [Test] procedure RingOverflowDropsOldest;
  end;

implementation

{ TDragTimerTests ---------------------------------------------------------- }

procedure TDragTimerTests.DefaultsIdleZeroToHundred;
var D: TOBDDragTimer;
begin
  D := TOBDDragTimer.Create(nil);
  try
    Assert.IsTrue(D.State = dsIdle);
    Assert.AreEqual(100.0, D.TargetSpeed, 0.001);
    Assert.AreEqual<Cardinal>(0, D.LastMs);
    Assert.AreEqual<Cardinal>(0, D.BestMs);
  finally D.Free; end;
end;

procedure TDragTimerTests.ArmTransitionsToArmed;
var D: TOBDDragTimer;
begin
  D := TOBDDragTimer.Create(nil);
  try
    D.Arm;
    Assert.IsTrue(D.State = dsArmed);
  finally D.Free; end;
end;

procedure TDragTimerTests.SpeedAboveStartArmedTransitionsToCapturing;
var D: TOBDDragTimer;
begin
  D := TOBDDragTimer.Create(nil);
  try
    D.Arm;
    D.Speed := 5.0;
    Assert.IsTrue(D.State = dsCapturing);
  finally D.Free; end;
end;

procedure TDragTimerTests.SpeedAboveTargetTransitionsToDone;
var D: TOBDDragTimer;
begin
  D := TOBDDragTimer.Create(nil);
  try
    D.Arm;
    D.Speed := 5.0;       // capturing
    D.Speed := 110.0;     // exceeds 100
    Assert.IsTrue(D.State = dsDone);
    Assert.IsTrue(D.LastMs >= 0);
  finally D.Free; end;
end;

procedure TDragTimerTests.ResetClearsLastKeepsBest;
var D: TOBDDragTimer;
begin
  D := TOBDDragTimer.Create(nil);
  try
    D.Arm;
    D.Speed := 5.0;
    D.Speed := 110.0;
    var BestBefore := D.BestMs;
    D.Reset;
    Assert.IsTrue(D.State = dsIdle);
    Assert.AreEqual<Cardinal>(0, D.LastMs);
    Assert.AreEqual<Cardinal>(BestBefore, D.BestMs);
  finally D.Free; end;
end;

procedure TDragTimerTests.ResetBestClearsBest;
var D: TOBDDragTimer;
begin
  D := TOBDDragTimer.Create(nil);
  try
    D.Arm;
    D.Speed := 5.0;
    D.Speed := 110.0;
    D.ResetBest;
    Assert.AreEqual<Cardinal>(0, D.BestMs);
  finally D.Free; end;
end;

procedure TDragTimerTests.FormatTimeRendersMmSsHh;
begin
  Assert.AreEqual('0:05.43', TOBDDragTimer.FormatTime(5430));
  Assert.AreEqual('1:00.00', TOBDDragTimer.FormatTime(60000));
  Assert.AreEqual('0:00.00', TOBDDragTimer.FormatTime(0));
end;

{ TLapTimerTests ----------------------------------------------------------- }

procedure TLapTimerTests.DefaultsNotRunningZeroLaps;
var L: TOBDLapTimer;
begin
  L := TOBDLapTimer.Create(nil);
  try
    Assert.IsFalse(L.Running);
    Assert.AreEqual(0, L.LapCount);
    Assert.AreEqual<Cardinal>(0, L.BestMs);
  finally L.Free; end;
end;

procedure TLapTimerTests.StartLapSetsRunning;
var L: TOBDLapTimer;
begin
  L := TOBDLapTimer.Create(nil);
  try
    L.StartLap;
    Assert.IsTrue(L.Running);
  finally L.Free; end;
end;

procedure TLapTimerTests.EndLapIncrementsCount;
var L: TOBDLapTimer;
begin
  L := TOBDLapTimer.Create(nil);
  try
    L.StartLap;
    L.EndLap;
    Assert.AreEqual(1, L.LapCount);
    Assert.IsFalse(L.Running);
  finally L.Free; end;
end;

procedure TLapTimerTests.ResetClearsAll;
var L: TOBDLapTimer;
begin
  L := TOBDLapTimer.Create(nil);
  try
    L.StartLap;
    L.EndLap;
    L.Reset;
    Assert.AreEqual(0, L.LapCount);
    Assert.AreEqual<Cardinal>(0, L.BestMs);
    Assert.AreEqual<Cardinal>(0, L.LastMs);
  finally L.Free; end;
end;

{ TAccelGraphTests --------------------------------------------------------- }

procedure TAccelGraphTests.DefaultsCapacity512;
var G: TOBDAccelGraph;
begin
  G := TOBDAccelGraph.Create(nil);
  try
    Assert.AreEqual(512, G.Capacity);
    Assert.AreEqual<Cardinal>(10000, G.MaxTimeMs);
    Assert.AreEqual(0, G.SampleCount);
  finally G.Free; end;
end;

procedure TAccelGraphTests.CapacityClampsLow;
var G: TOBDAccelGraph;
begin
  G := TOBDAccelGraph.Create(nil);
  try
    G.Capacity := 1;
    Assert.AreEqual(32, G.Capacity);
  finally G.Free; end;
end;

procedure TAccelGraphTests.CapacityClampsHigh;
var G: TOBDAccelGraph;
begin
  G := TOBDAccelGraph.Create(nil);
  try
    G.Capacity := 999999;
    Assert.AreEqual(16384, G.Capacity);
  finally G.Free; end;
end;

procedure TAccelGraphTests.PushSampleIncrementsCount;
var G: TOBDAccelGraph;
begin
  G := TOBDAccelGraph.Create(nil);
  try
    G.PushSample(100, 10);
    G.PushSample(200, 20);
    G.PushSample(300, 30);
    Assert.AreEqual(3, G.SampleCount);
  finally G.Free; end;
end;

procedure TAccelGraphTests.ResetClearsSamples;
var G: TOBDAccelGraph;
begin
  G := TOBDAccelGraph.Create(nil);
  try
    G.PushSample(100, 10);
    G.Reset;
    Assert.AreEqual(0, G.SampleCount);
  finally G.Free; end;
end;

procedure TAccelGraphTests.RingOverflowDropsOldest;
var
  G: TOBDAccelGraph;
  I: Integer;
begin
  G := TOBDAccelGraph.Create(nil);
  try
    G.Capacity := 32;
    for I := 0 to 49 do
      G.PushSample(Cardinal(I * 10), I * 1.0);
    // Buffer caps at 32; further pushes shift older samples
    // out, so SampleCount stays at Capacity.
    Assert.AreEqual(32, G.SampleCount);
  finally G.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDragTimerTests);
  TDUnitX.RegisterTestFixture(TLapTimerTests);
  TDUnitX.RegisterTestFixture(TAccelGraphTests);

end.
