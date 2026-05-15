//------------------------------------------------------------------------------
//  Tests.OBD.UI.Session
//
//  Non-visual contract for the A2.9 session / progress visuals.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Session;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.Flash.Phases,
  OBD.UI.Session;

type
  [TestFixture]
  TFlashProgressTests = class
  public
    [Test] procedure DefaultsPreflightZero;
    [Test] procedure OverallPercentClampsHigh;
    [Test] procedure OverallPercentClampsLow;
    [Test] procedure UpdateRoundTrip;
    [Test] procedure ErroredToggle;
  end;

  [TestFixture]
  TCodingSessionPanelTests = class
  public
    [Test] procedure DefaultsIdle;
    [Test] procedure StateRoundTrip;
    [Test] procedure WriteCountClampsNegative;
    [Test] procedure RollbackToggle;
  end;

  [TestFixture]
  TXCPProgressBarTests = class
  public
    [Test] procedure DefaultsZero;
    [Test] procedure UpdateRoundTrip;
    [Test] procedure UpdateZeroTotalPreservesPreviousTotal;
    [Test] procedure KBPerSecondClampsNegative;
  end;

  [TestFixture]
  TRecorderToolbarTests = class
  public
    [Test] procedure DefaultsNotRecording;
    [Test] procedure RecordingPausedRoundTrip;
    [Test] procedure DurationAndFrameCountClampNegative;
  end;

implementation

{ TFlashProgressTests ----------------------------------------------------- }

procedure TFlashProgressTests.DefaultsPreflightZero;
var P: TOBDFlashProgress;
begin
  P := TOBDFlashProgress.Create(nil);
  try
    Assert.IsTrue(P.CurrentPhase = fpPreflight);
    Assert.AreEqual<Single>(0, P.OverallPercent);
    Assert.AreEqual<Single>(0, P.PhasePercent);
    Assert.IsFalse(P.Errored);
  finally P.Free; end;
end;

procedure TFlashProgressTests.OverallPercentClampsHigh;
var P: TOBDFlashProgress;
begin
  P := TOBDFlashProgress.Create(nil);
  try
    P.OverallPercent := 250;
    Assert.AreEqual<Single>(100, P.OverallPercent);
  finally P.Free; end;
end;

procedure TFlashProgressTests.OverallPercentClampsLow;
var P: TOBDFlashProgress;
begin
  P := TOBDFlashProgress.Create(nil);
  try
    P.OverallPercent := -10;
    Assert.AreEqual<Single>(0, P.OverallPercent);
  finally P.Free; end;
end;

procedure TFlashProgressTests.UpdateRoundTrip;
var P: TOBDFlashProgress;
begin
  P := TOBDFlashProgress.Create(nil);
  try
    P.Update(fpTransfer, 42.5, 70.0, 'block 42/100');
    Assert.IsTrue(P.CurrentPhase = fpTransfer);
    Assert.AreEqual<Single>(42.5, P.OverallPercent);
    Assert.AreEqual<Single>(70.0, P.PhasePercent);
    Assert.AreEqual('block 42/100', P.StatusText);
  finally P.Free; end;
end;

procedure TFlashProgressTests.ErroredToggle;
var P: TOBDFlashProgress;
begin
  P := TOBDFlashProgress.Create(nil);
  try
    P.Errored := True;
    Assert.IsTrue(P.Errored);
  finally P.Free; end;
end;

{ TCodingSessionPanelTests ------------------------------------------------ }

procedure TCodingSessionPanelTests.DefaultsIdle;
var P: TOBDCodingSessionPanel;
begin
  P := TOBDCodingSessionPanel.Create(nil);
  try
    Assert.IsTrue(P.State = cdsIdle);
    Assert.AreEqual(0, P.WriteCount);
    Assert.IsFalse(P.RollbackAvailable);
  finally P.Free; end;
end;

procedure TCodingSessionPanelTests.StateRoundTrip;
var P: TOBDCodingSessionPanel;
begin
  P := TOBDCodingSessionPanel.Create(nil);
  try
    P.State := cdsWriting;     Assert.IsTrue(P.State = cdsWriting);
    P.State := cdsError;       Assert.IsTrue(P.State = cdsError);
    P.State := cdsCompleted;   Assert.IsTrue(P.State = cdsCompleted);
  finally P.Free; end;
end;

procedure TCodingSessionPanelTests.WriteCountClampsNegative;
var P: TOBDCodingSessionPanel;
begin
  P := TOBDCodingSessionPanel.Create(nil);
  try
    P.WriteCount := -3;
    Assert.AreEqual(0, P.WriteCount);
  finally P.Free; end;
end;

procedure TCodingSessionPanelTests.RollbackToggle;
var P: TOBDCodingSessionPanel;
begin
  P := TOBDCodingSessionPanel.Create(nil);
  try
    P.RollbackAvailable := True;
    Assert.IsTrue(P.RollbackAvailable);
  finally P.Free; end;
end;

{ TXCPProgressBarTests ---------------------------------------------------- }

procedure TXCPProgressBarTests.DefaultsZero;
var B: TOBDXCPProgressBar;
begin
  B := TOBDXCPProgressBar.Create(nil);
  try
    Assert.AreEqual<Int64>(0, B.BytesDone);
    Assert.AreEqual<Int64>(0, B.BytesTotal);
    Assert.AreEqual(0.0, B.KBPerSecond, 0.001);
  finally B.Free; end;
end;

procedure TXCPProgressBarTests.UpdateRoundTrip;
var B: TOBDXCPProgressBar;
begin
  B := TOBDXCPProgressBar.Create(nil);
  try
    B.Update(1024, 65536, 256.0, 'Upload');
    Assert.AreEqual<Int64>(1024, B.BytesDone);
    Assert.AreEqual<Int64>(65536, B.BytesTotal);
    Assert.AreEqual(256.0, B.KBPerSecond, 0.001);
    Assert.AreEqual('Upload', B.&Label);
  finally B.Free; end;
end;

procedure TXCPProgressBarTests.UpdateZeroTotalPreservesPreviousTotal;
var B: TOBDXCPProgressBar;
begin
  B := TOBDXCPProgressBar.Create(nil);
  try
    B.Update(0, 65536, 0);
    B.Update(2048, 0, 128);     // ABytesTotal=0 → keep 65536
    Assert.AreEqual<Int64>(65536, B.BytesTotal);
    Assert.AreEqual<Int64>(2048, B.BytesDone);
  finally B.Free; end;
end;

procedure TXCPProgressBarTests.KBPerSecondClampsNegative;
var B: TOBDXCPProgressBar;
begin
  B := TOBDXCPProgressBar.Create(nil);
  try
    B.KBPerSecond := -1.0;
    Assert.AreEqual(0.0, B.KBPerSecond, 0.001);
  finally B.Free; end;
end;

{ TRecorderToolbarTests --------------------------------------------------- }

procedure TRecorderToolbarTests.DefaultsNotRecording;
var T: TOBDRecorderToolbar;
begin
  T := TOBDRecorderToolbar.Create(nil);
  try
    Assert.IsFalse(T.Recording);
    Assert.IsFalse(T.Paused);
    Assert.AreEqual<Int64>(0, T.DurationMs);
    Assert.AreEqual<Int64>(0, T.FrameCount);
  finally T.Free; end;
end;

procedure TRecorderToolbarTests.RecordingPausedRoundTrip;
var T: TOBDRecorderToolbar;
begin
  T := TOBDRecorderToolbar.Create(nil);
  try
    T.Recording := True;
    T.Paused    := True;
    Assert.IsTrue(T.Recording);
    Assert.IsTrue(T.Paused);
  finally T.Free; end;
end;

procedure TRecorderToolbarTests.DurationAndFrameCountClampNegative;
var T: TOBDRecorderToolbar;
begin
  T := TOBDRecorderToolbar.Create(nil);
  try
    T.DurationMs := -100;
    T.FrameCount := -50;
    Assert.AreEqual<Int64>(0, T.DurationMs);
    Assert.AreEqual<Int64>(0, T.FrameCount);
  finally T.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFlashProgressTests);
  TDUnitX.RegisterTestFixture(TCodingSessionPanelTests);
  TDUnitX.RegisterTestFixture(TXCPProgressBarTests);
  TDUnitX.RegisterTestFixture(TRecorderToolbarTests);

end.
