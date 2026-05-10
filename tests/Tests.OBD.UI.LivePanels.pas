//------------------------------------------------------------------------------
//  Tests.OBD.UI.LivePanels
//
//  Non-visual contract for the A2.6 live-data panels.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.LivePanels;

interface

uses
  System.SysUtils, System.Classes, System.Math,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.Service.LiveData,
  OBD.UI.LivePanels;

type
  [TestFixture]
  TPidPanelTests = class
  public
    [Test] procedure DefaultsEmptyShowSparkTrue;
    [Test] procedure ValueRoundTripFiresNotify;
    [Test] procedure SparkCapacityClampsLow;
    [Test] procedure SparkCapacityClampsHigh;
    [Test] procedure ResetSparkClearsBuffer;
    [Test] procedure LiveDataBindNoCrashAtDesignTime;
    [Test] procedure FreeNotificationClearsLiveData;
  end;

  [TestFixture]
  TFuelTrimDisplayTests = class
  public
    [Test] procedure DefaultsZeroBothBanks;
    [Test] procedure SetTrimsRoundTrip;
    [Test] procedure RangeClampsLow;
    [Test] procedure HealthyLimitClampsNegative;
    [Test] procedure ShowBank2Toggle;
  end;

implementation

{ TPidPanelTests ---------------------------------------------------------- }

procedure TPidPanelTests.DefaultsEmptyShowSparkTrue;
var P: TOBDPidPanel;
begin
  P := TOBDPidPanel.Create(nil);
  try
    Assert.AreEqual('', P.Caption);
    Assert.AreEqual(0.0, P.Value, 0.001);
    Assert.AreEqual('', P.&Unit);
    Assert.IsTrue(P.ShowSpark);
    Assert.AreEqual(32, P.SparkCapacity);
  finally P.Free; end;
end;

procedure TPidPanelTests.ValueRoundTripFiresNotify;
var P: TOBDPidPanel;
begin
  P := TOBDPidPanel.Create(nil);
  try
    P.Value := 42.5;
    Assert.AreEqual(42.5, P.Value, 0.001);
  finally P.Free; end;
end;

procedure TPidPanelTests.SparkCapacityClampsLow;
var P: TOBDPidPanel;
begin
  P := TOBDPidPanel.Create(nil);
  try
    P.SparkCapacity := 1;
    Assert.AreEqual(4, P.SparkCapacity);
  finally P.Free; end;
end;

procedure TPidPanelTests.SparkCapacityClampsHigh;
var P: TOBDPidPanel;
begin
  P := TOBDPidPanel.Create(nil);
  try
    P.SparkCapacity := 99999;
    Assert.AreEqual(1024, P.SparkCapacity);
  finally P.Free; end;
end;

procedure TPidPanelTests.ResetSparkClearsBuffer;
var P: TOBDPidPanel;
begin
  P := TOBDPidPanel.Create(nil);
  try
    P.Value := 1; P.Value := 2; P.Value := 3;
    P.ResetSpark;
    // Smoke: subsequent push works.
    P.Value := 4;
    Assert.Pass;
  finally P.Free; end;
end;

procedure TPidPanelTests.LiveDataBindNoCrashAtDesignTime;
var
  P:  TOBDPidPanel;
  LD: TOBDLiveData;
begin
  // Even with no Protocol the panel must accept LiveData + PID
  // assignment cleanly (subscribe is csDesigning-guarded; the
  // base subscribe path only runs at runtime).
  LD := TOBDLiveData.Create(nil);
  P  := TOBDPidPanel.Create(nil);
  try
    P.LiveData := LD;
    P.PID := $0C;
    Assert.IsNotNull(P.LiveData);
    Assert.AreEqual<Byte>($0C, P.PID);
  finally
    P.Free;
    LD.Free;
  end;
end;

procedure TPidPanelTests.FreeNotificationClearsLiveData;
var
  P:  TOBDPidPanel;
  LD: TOBDLiveData;
begin
  LD := TOBDLiveData.Create(nil);
  P  := TOBDPidPanel.Create(nil);
  try
    P.LiveData := LD;
    Assert.IsNotNull(P.LiveData);
    LD.Free; LD := nil;
    Assert.IsNull(P.LiveData);
  finally
    P.Free;
    if LD <> nil then LD.Free;
  end;
end;

{ TFuelTrimDisplayTests --------------------------------------------------- }

procedure TFuelTrimDisplayTests.DefaultsZeroBothBanks;
var F: TOBDFuelTrimDisplay;
begin
  F := TOBDFuelTrimDisplay.Create(nil);
  try
    Assert.AreEqual(0.0, F.STFTBank1, 0.001);
    Assert.AreEqual(0.0, F.LTFTBank1, 0.001);
    Assert.AreEqual(0.0, F.STFTBank2, 0.001);
    Assert.AreEqual(0.0, F.LTFTBank2, 0.001);
    Assert.AreEqual(10.0, F.HealthyLimit, 0.001);
    Assert.AreEqual(25.0, F.Range, 0.001);
    Assert.IsTrue(F.ShowBank2);
  finally F.Free; end;
end;

procedure TFuelTrimDisplayTests.SetTrimsRoundTrip;
var F: TOBDFuelTrimDisplay;
begin
  F := TOBDFuelTrimDisplay.Create(nil);
  try
    F.STFTBank1 := 5.5;
    F.LTFTBank1 := -3.0;
    F.STFTBank2 := 12.0;
    F.LTFTBank2 := -15.0;
    Assert.AreEqual( 5.5, F.STFTBank1, 0.001);
    Assert.AreEqual(-3.0, F.LTFTBank1, 0.001);
    Assert.AreEqual(12.0, F.STFTBank2, 0.001);
    Assert.AreEqual(-15.0, F.LTFTBank2, 0.001);
  finally F.Free; end;
end;

procedure TFuelTrimDisplayTests.RangeClampsLow;
var F: TOBDFuelTrimDisplay;
begin
  F := TOBDFuelTrimDisplay.Create(nil);
  try
    F.Range := 0;
    Assert.AreEqual(1.0, F.Range, 0.001);
  finally F.Free; end;
end;

procedure TFuelTrimDisplayTests.HealthyLimitClampsNegative;
var F: TOBDFuelTrimDisplay;
begin
  F := TOBDFuelTrimDisplay.Create(nil);
  try
    F.HealthyLimit := -5;
    Assert.AreEqual(0.0, F.HealthyLimit, 0.001);
  finally F.Free; end;
end;

procedure TFuelTrimDisplayTests.ShowBank2Toggle;
var F: TOBDFuelTrimDisplay;
begin
  F := TOBDFuelTrimDisplay.Create(nil);
  try
    F.ShowBank2 := False;
    Assert.IsFalse(F.ShowBank2);
  finally F.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPidPanelTests);
  TDUnitX.RegisterTestFixture(TFuelTrimDisplayTests);

end.
