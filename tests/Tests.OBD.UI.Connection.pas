//------------------------------------------------------------------------------
//  Tests.OBD.UI.Connection
//
//  Non-visual contract for the A2.9 connection-state visuals.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Connection;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.Connection.Types,
  OBD.Connection,
  OBD.UI.Connection;

type
  [TestFixture]
  TConnectionStateLampTests = class
  public
    [Test] procedure DefaultsClosed;
    [Test] procedure StateRoundTrip;
    [Test] procedure FreeNotificationClearsConnection;
  end;

  [TestFixture]
  TDoIPStatusPanelTests = class
  public
    [Test] procedure DefaultsZeros;
    [Test] procedure TargetEIDRoundTrip;
    [Test] procedure HeartbeatAgeClampsNegative;
    [Test] procedure StaleThresholdRoundTrip;
  end;

  [TestFixture]
  TSecurityAccessLampTests = class
  public
    [Test] procedure DefaultsLocked;
    [Test] procedure UnlockSetsLevel;
    [Test] procedure LockoutClampsNegative;
  end;

  [TestFixture]
  TSecOCStatusLampTests = class
  public
    [Test] procedure DefaultsUnknown;
    [Test] procedure FreshnessRoundTrip;
    [Test] procedure KeyHealthRoundTrip;
  end;

implementation

{ TConnectionStateLampTests ----------------------------------------------- }

procedure TConnectionStateLampTests.DefaultsClosed;
var L: TOBDConnectionStateLamp;
begin
  L := TOBDConnectionStateLamp.Create(nil);
  try
    Assert.IsTrue(L.State = csClosed);
    Assert.IsTrue(L.ShowCaption);
  finally L.Free; end;
end;

procedure TConnectionStateLampTests.StateRoundTrip;
var L: TOBDConnectionStateLamp;
begin
  L := TOBDConnectionStateLamp.Create(nil);
  try
    L.State := csOpening; Assert.IsTrue(L.State = csOpening);
    L.State := csOpen;    Assert.IsTrue(L.State = csOpen);
    L.State := csError;   Assert.IsTrue(L.State = csError);
  finally L.Free; end;
end;

procedure TConnectionStateLampTests.FreeNotificationClearsConnection;
var
  L: TOBDConnectionStateLamp;
  C: TOBDConnection;
begin
  C := TOBDConnection.Create(nil);
  L := TOBDConnectionStateLamp.Create(nil);
  try
    L.Connection := C;
    Assert.IsNotNull(L.Connection);
    C.Free; C := nil;
    Assert.IsNull(L.Connection);
  finally
    L.Free;
    if C <> nil then C.Free;
  end;
end;

{ TDoIPStatusPanelTests --------------------------------------------------- }

procedure TDoIPStatusPanelTests.DefaultsZeros;
var P: TOBDDoIPStatusPanel;
begin
  P := TOBDDoIPStatusPanel.Create(nil);
  try
    Assert.AreEqual<Word>(0, P.TargetEID);
    Assert.AreEqual<Byte>(0, P.ActivationType);
    Assert.AreEqual<Int64>(0, P.HeartbeatAgeMs);
    Assert.AreEqual<Int64>(5000, P.HeartbeatStaleMs);
  finally P.Free; end;
end;

procedure TDoIPStatusPanelTests.TargetEIDRoundTrip;
var P: TOBDDoIPStatusPanel;
begin
  P := TOBDDoIPStatusPanel.Create(nil);
  try
    P.TargetEID := $1234;
    Assert.AreEqual<Word>($1234, P.TargetEID);
  finally P.Free; end;
end;

procedure TDoIPStatusPanelTests.HeartbeatAgeClampsNegative;
var P: TOBDDoIPStatusPanel;
begin
  P := TOBDDoIPStatusPanel.Create(nil);
  try
    P.HeartbeatAgeMs := -100;
    Assert.AreEqual<Int64>(0, P.HeartbeatAgeMs);
  finally P.Free; end;
end;

procedure TDoIPStatusPanelTests.StaleThresholdRoundTrip;
var P: TOBDDoIPStatusPanel;
begin
  P := TOBDDoIPStatusPanel.Create(nil);
  try
    P.HeartbeatStaleMs := 10000;
    Assert.AreEqual<Int64>(10000, P.HeartbeatStaleMs);
  finally P.Free; end;
end;

{ TSecurityAccessLampTests ------------------------------------------------ }

procedure TSecurityAccessLampTests.DefaultsLocked;
var L: TOBDSecurityAccessLamp;
begin
  L := TOBDSecurityAccessLamp.Create(nil);
  try
    Assert.IsFalse(L.Unlocked);
    Assert.AreEqual<Byte>(0, L.Level);
    Assert.AreEqual<Int64>(0, L.LockoutMs);
  finally L.Free; end;
end;

procedure TSecurityAccessLampTests.UnlockSetsLevel;
var L: TOBDSecurityAccessLamp;
begin
  L := TOBDSecurityAccessLamp.Create(nil);
  try
    L.Unlocked := True;
    L.Level    := $03;
    Assert.IsTrue(L.Unlocked);
    Assert.AreEqual<Byte>($03, L.Level);
  finally L.Free; end;
end;

procedure TSecurityAccessLampTests.LockoutClampsNegative;
var L: TOBDSecurityAccessLamp;
begin
  L := TOBDSecurityAccessLamp.Create(nil);
  try
    L.LockoutMs := -250;
    Assert.AreEqual<Int64>(0, L.LockoutMs);
  finally L.Free; end;
end;

{ TSecOCStatusLampTests --------------------------------------------------- }

procedure TSecOCStatusLampTests.DefaultsUnknown;
var L: TOBDSecOCStatusLamp;
begin
  L := TOBDSecOCStatusLamp.Create(nil);
  try
    Assert.IsTrue(L.Freshness = sfUnknown);
    Assert.IsTrue(L.KeyHealth = khUnknown);
  finally L.Free; end;
end;

procedure TSecOCStatusLampTests.FreshnessRoundTrip;
var L: TOBDSecOCStatusLamp;
begin
  L := TOBDSecOCStatusLamp.Create(nil);
  try
    L.Freshness := sfOK;
    Assert.IsTrue(L.Freshness = sfOK);
    L.Freshness := sfExpired;
    Assert.IsTrue(L.Freshness = sfExpired);
  finally L.Free; end;
end;

procedure TSecOCStatusLampTests.KeyHealthRoundTrip;
var L: TOBDSecOCStatusLamp;
begin
  L := TOBDSecOCStatusLamp.Create(nil);
  try
    L.KeyHealth := khHealthy;
    Assert.IsTrue(L.KeyHealth = khHealthy);
    L.KeyHealth := khStale;
    Assert.IsTrue(L.KeyHealth = khStale);
  finally L.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TConnectionStateLampTests);
  TDUnitX.RegisterTestFixture(TDoIPStatusPanelTests);
  TDUnitX.RegisterTestFixture(TSecurityAccessLampTests);
  TDUnitX.RegisterTestFixture(TSecOCStatusLampTests);

end.
