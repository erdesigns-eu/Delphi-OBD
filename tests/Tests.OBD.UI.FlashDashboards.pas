//------------------------------------------------------------------------------
//  Tests.OBD.UI.FlashDashboards
//
//  Non-visual contract for the A2.11 flash-control dashboards.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.FlashDashboards;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.Coding.AuditLog,
  OBD.UI.FlashDashboards;

type
  [TestFixture]
  TFlashSafetyDashboardTests = class
  public
    [Test] procedure DefaultsHaveFiveGates;
    [Test] procedure ArmedFalseUntilEveryRequiredPasses;
    [Test] procedure UpdateGateAddsWhenMissing;
    [Test] procedure ClearGatesEmpties;
    [Test] procedure OptionalGateDoesNotBlockArm;
  end;

  [TestFixture]
  TFlashCheckpointTimelineTests = class
  public
    [Test] procedure DefaultsCapacity64;
    [Test] procedure CapacityClampsLow;
    [Test] procedure PushEventGrows;
    [Test] procedure CapacityShrinkDropsOldest;
    [Test] procedure ClearEmptiesEvents;
  end;

  [TestFixture]
  TFlashAuditTailTests = class
  public
    [Test] procedure DefaultsMaxRows200;
    [Test] procedure MaxRowsClampsLow;
    [Test] procedure PushEntryAppends;
    [Test] procedure ClearEmpties;
  end;

implementation

{ TFlashSafetyDashboardTests --------------------------------------------- }

procedure TFlashSafetyDashboardTests.DefaultsHaveFiveGates;
var D: TOBDFlashSafetyDashboard;
begin
  D := TOBDFlashSafetyDashboard.Create(nil);
  try
    Assert.AreEqual<NativeInt>(5, Length(D.Gates));
  finally D.Free; end;
end;

procedure TFlashSafetyDashboardTests.ArmedFalseUntilEveryRequiredPasses;
var D: TOBDFlashSafetyDashboard;
begin
  D := TOBDFlashSafetyDashboard.Create(nil);
  try
    Assert.IsFalse(D.Armed);
    D.UpdateGate('Voltage',     '12.6 V', fgsPass);
    D.UpdateGate('Temperature', '32 C',   fgsPass);
    D.UpdateGate('Battery',     'OK',     fgsPass);
    D.UpdateGate('Image',       'sha ok', fgsPass);
    D.UpdateGate('Signature',   'verified', fgsWarning);
    Assert.IsFalse(D.Armed,
      'Warning on a required gate must still block arm');
    D.UpdateGate('Signature',   'verified', fgsPass);
    Assert.IsTrue(D.Armed);
  finally D.Free; end;
end;

procedure TFlashSafetyDashboardTests.UpdateGateAddsWhenMissing;
var D: TOBDFlashSafetyDashboard;
begin
  D := TOBDFlashSafetyDashboard.Create(nil);
  try
    D.UpdateGate('Custom Probe', '', fgsPass);
    Assert.AreEqual<NativeInt>(6, Length(D.Gates));
  finally D.Free; end;
end;

procedure TFlashSafetyDashboardTests.ClearGatesEmpties;
var D: TOBDFlashSafetyDashboard;
begin
  D := TOBDFlashSafetyDashboard.Create(nil);
  try
    D.ClearGates;
    Assert.AreEqual<NativeInt>(0, Length(D.Gates));
    Assert.IsFalse(D.Armed);
  finally D.Free; end;
end;

procedure TFlashSafetyDashboardTests.OptionalGateDoesNotBlockArm;
var D: TOBDFlashSafetyDashboard;
begin
  D := TOBDFlashSafetyDashboard.Create(nil);
  try
    D.ClearGates;
    D.AddGate('A', 'ok', fgsPass, True);
    D.AddGate('B', 'note', fgsFail, False);    // optional
    Assert.IsTrue(D.Armed,
      'Optional fail must NOT block the arm gate');
  finally D.Free; end;
end;

{ TFlashCheckpointTimelineTests ------------------------------------------ }

procedure TFlashCheckpointTimelineTests.DefaultsCapacity64;
var T: TOBDFlashCheckpointTimeline;
begin
  T := TOBDFlashCheckpointTimeline.Create(nil);
  try
    Assert.AreEqual(64, T.Capacity);
    Assert.AreEqual(0, T.EventCount);
  finally T.Free; end;
end;

procedure TFlashCheckpointTimelineTests.CapacityClampsLow;
var T: TOBDFlashCheckpointTimeline;
begin
  T := TOBDFlashCheckpointTimeline.Create(nil);
  try
    T.Capacity := 1;
    Assert.AreEqual(4, T.Capacity);
  finally T.Free; end;
end;

procedure TFlashCheckpointTimelineTests.PushEventGrows;
var T: TOBDFlashCheckpointTimeline;
begin
  T := TOBDFlashCheckpointTimeline.Create(nil);
  try
    T.PushEvent(ckCreated, 'cp-001');
    T.PushEvent(ckRestored, 'cp-001 restore');
    Assert.AreEqual(2, T.EventCount);
    Assert.IsTrue(T.Events[1].Kind = ckRestored);
  finally T.Free; end;
end;

procedure TFlashCheckpointTimelineTests.CapacityShrinkDropsOldest;
var
  T: TOBDFlashCheckpointTimeline;
  I: Integer;
begin
  T := TOBDFlashCheckpointTimeline.Create(nil);
  try
    for I := 1 to 10 do T.PushEvent(ckCreated, 'cp ' + I.ToString);
    T.Capacity := 4;
    Assert.AreEqual(4, T.EventCount);
    Assert.AreEqual('cp 7', T.Events[0].Label_);
  finally T.Free; end;
end;

procedure TFlashCheckpointTimelineTests.ClearEmptiesEvents;
var T: TOBDFlashCheckpointTimeline;
begin
  T := TOBDFlashCheckpointTimeline.Create(nil);
  try
    T.PushEvent(ckCreated, 'a');
    T.ClearEvents;
    Assert.AreEqual(0, T.EventCount);
  finally T.Free; end;
end;

{ TFlashAuditTailTests --------------------------------------------------- }

procedure TFlashAuditTailTests.DefaultsMaxRows200;
var T: TOBDFlashAuditTail;
begin
  T := TOBDFlashAuditTail.Create(nil);
  try
    Assert.AreEqual(200, T.MaxRows);
  finally T.Free; end;
end;

procedure TFlashAuditTailTests.MaxRowsClampsLow;
var T: TOBDFlashAuditTail;
begin
  T := TOBDFlashAuditTail.Create(nil);
  try
    T.MaxRows := 1;
    Assert.AreEqual(10, T.MaxRows);
  finally T.Free; end;
end;

procedure TFlashAuditTailTests.PushEntryAppends;
var
  T: TOBDFlashAuditTail;
  E: TOBDAuditEntry;
begin
  T := TOBDFlashAuditTail.Create(nil);
  try
    E := Default(TOBDAuditEntry);
    E.Timestamp := Now;
    E.Kind      := akWrite;
    E.Target    := '0x1234';
    E.Message   := 'wrote 4 bytes';
    T.PushEntry(E);
    Assert.AreEqual(1, T.Items.Count);
  finally T.Free; end;
end;

procedure TFlashAuditTailTests.ClearEmpties;
var
  T: TOBDFlashAuditTail;
  E: TOBDAuditEntry;
begin
  T := TOBDFlashAuditTail.Create(nil);
  try
    E := Default(TOBDAuditEntry);
    E.Timestamp := Now;
    E.Kind      := akSessionStart;
    T.PushEntry(E);
    T.ClearTail;
    Assert.AreEqual(0, T.Items.Count);
  finally T.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFlashSafetyDashboardTests);
  TDUnitX.RegisterTestFixture(TFlashCheckpointTimelineTests);
  TDUnitX.RegisterTestFixture(TFlashAuditTailTests);

end.
