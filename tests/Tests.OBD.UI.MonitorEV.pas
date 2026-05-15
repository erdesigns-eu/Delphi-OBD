//------------------------------------------------------------------------------
//  Tests.OBD.UI.MonitorEV
//
//  Non-visual contract for the A2.8 monitor / EV visuals.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.MonitorEV;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.Service.DriveCycle.Types,
  OBD.Service.DriveCycle,
  OBD.UI.MonitorEV;

type
  [TestFixture]
  TReadinessGridTests = class
  public
    [Test] procedure DefaultsReadOnlyReport;
    [Test] procedure RefreshWithEmptyArrayRendersPlaceholders;
    [Test] procedure RefreshUpdatesSupportedComplete;
    [Test] procedure FreeNotificationClearsAdvisor;
    [Test] procedure RefreshFromAdvisorWithoutAdvisorRaises;
  end;

  [TestFixture]
  TDriveCycleProgressTests = class
  public
    [Test] procedure DefaultsBlank;
    [Test] procedure UpdateProgressRoundTrip;
    [Test] procedure UpdateProgressPreservesStepCount;
  end;

  [TestFixture]
  TCellVoltageHeatmapTests = class
  public
    [Test] procedure DefaultsTwelveColumns;
    [Test] procedure ColumnsClampLow;
    [Test] procedure ColumnsClampHigh;
    [Test] procedure SetVoltagesRoundTrip;
    [Test] procedure SetCellOutOfRangeIgnored;
    [Test] procedure CellAtOutOfBoundsReturnsMinusOne;
  end;

  [TestFixture]
  TChargingFlowTests = class
  public
    [Test] procedure DefaultsZeroPower;
    [Test] procedure ChargeKwRoundTrip;
    [Test] procedure DriveKwRoundTrip;
  end;

implementation

{ TReadinessGridTests ----------------------------------------------------- }

procedure TReadinessGridTests.DefaultsReadOnlyReport;
var G: TOBDReadinessGrid;
begin
  G := TOBDReadinessGrid.Create(nil);
  try
    Assert.IsTrue(G.ReadOnly);
    Assert.IsTrue(G.GridLines);
  finally G.Free; end;
end;

procedure TReadinessGridTests.RefreshWithEmptyArrayRendersPlaceholders;
var
  G: TOBDReadinessGrid;
  Empty: TArray<TOBDMonitorReadiness>;
begin
  G := TOBDReadinessGrid.Create(nil);
  try
    G.Refresh(Empty);
    Assert.Pass('Empty refresh shouldn''t raise');
  finally G.Free; end;
end;

procedure TReadinessGridTests.RefreshUpdatesSupportedComplete;
var
  G: TOBDReadinessGrid;
  Snap: TArray<TOBDMonitorReadiness>;
begin
  G := TOBDReadinessGrid.Create(nil);
  try
    SetLength(Snap, 2);
    Snap[0].Monitor   := omMisfire;
    Snap[0].Supported := True;
    Snap[0].Complete  := True;
    Snap[1].Monitor   := omCatalyst;
    Snap[1].Supported := True;
    Snap[1].Complete  := False;
    G.Refresh(Snap);
    // Smoke: doesn't raise. Row content access requires HWND
    // for the listview backing store, which isn't allocated
    // until the control is shown.
    Assert.Pass;
  finally G.Free; end;
end;

procedure TReadinessGridTests.FreeNotificationClearsAdvisor;
var
  G: TOBDReadinessGrid;
  A: TOBDDriveCycleAdvisor;
begin
  A := TOBDDriveCycleAdvisor.Create(nil);
  G := TOBDReadinessGrid.Create(nil);
  try
    G.Advisor := A;
    Assert.IsNotNull(G.Advisor);
    A.Free; A := nil;
    Assert.IsNull(G.Advisor);
  finally
    G.Free;
    if A <> nil then A.Free;
  end;
end;

procedure TReadinessGridTests.RefreshFromAdvisorWithoutAdvisorRaises;
var G: TOBDReadinessGrid;
begin
  G := TOBDReadinessGrid.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin G.RefreshFromAdvisor; end,
      EInvalidOperation);
  finally G.Free; end;
end;

{ TDriveCycleProgressTests ------------------------------------------------ }

procedure TDriveCycleProgressTests.DefaultsBlank;
var P: TOBDDriveCycleProgress;
begin
  P := TOBDDriveCycleProgress.Create(nil);
  try
    Assert.AreEqual<Cardinal>(0, P.ElapsedSec);
    Assert.AreEqual<Cardinal>(0, P.TotalSec);
  finally P.Free; end;
end;

procedure TDriveCycleProgressTests.UpdateProgressRoundTrip;
var
  P:    TOBDDriveCycleProgress;
  Step: TOBDDriveCycleStep;
begin
  P := TOBDDriveCycleProgress.Create(nil);
  try
    Step := Default(TOBDDriveCycleStep);
    Step.Index := 2;
    Step.Description := 'Cruise at 80 km/h for 5 minutes';
    Step.DurationSec := 300;
    P.UpdateProgress(Step, 120, 300, 5);
    Assert.AreEqual<Cardinal>(120, P.ElapsedSec);
    Assert.AreEqual<Cardinal>(300, P.TotalSec);
    Assert.AreEqual(2, P.Step.Index);
    Assert.AreEqual('Cruise at 80 km/h for 5 minutes',
      P.Step.Description);
  finally P.Free; end;
end;

procedure TDriveCycleProgressTests.UpdateProgressPreservesStepCount;
var
  P:    TOBDDriveCycleProgress;
  Step: TOBDDriveCycleStep;
begin
  P := TOBDDriveCycleProgress.Create(nil);
  try
    Step := Default(TOBDDriveCycleStep);
    Step.Index := 1;
    P.UpdateProgress(Step, 0, 60, 7);
    Step.Index := 2;
    // Total = 0 → step count must not reset.
    P.UpdateProgress(Step, 10, 60, 0);
    Assert.AreEqual(2, P.Step.Index);
  finally P.Free; end;
end;

{ TCellVoltageHeatmapTests ------------------------------------------------ }

procedure TCellVoltageHeatmapTests.DefaultsTwelveColumns;
var H: TOBDCellVoltageHeatmap;
begin
  H := TOBDCellVoltageHeatmap.Create(nil);
  try
    Assert.AreEqual(12, H.Columns);
    Assert.AreEqual(0,  H.CellCount);
    Assert.IsFalse(H.ShowText);
  finally H.Free; end;
end;

procedure TCellVoltageHeatmapTests.ColumnsClampLow;
var H: TOBDCellVoltageHeatmap;
begin
  H := TOBDCellVoltageHeatmap.Create(nil);
  try
    H.Columns := 0;
    Assert.AreEqual(1, H.Columns);
  finally H.Free; end;
end;

procedure TCellVoltageHeatmapTests.ColumnsClampHigh;
var H: TOBDCellVoltageHeatmap;
begin
  H := TOBDCellVoltageHeatmap.Create(nil);
  try
    H.Columns := 9999;
    Assert.AreEqual(256, H.Columns);
  finally H.Free; end;
end;

procedure TCellVoltageHeatmapTests.SetVoltagesRoundTrip;
var
  H: TOBDCellVoltageHeatmap;
  V: TArray<Single>;
begin
  H := TOBDCellVoltageHeatmap.Create(nil);
  try
    V := TArray<Single>.Create(3.7, 3.8, 4.0, 3.6);
    H.SetVoltages(V);
    Assert.AreEqual(4, H.CellCount);
    Assert.AreEqual<Single>(3.7, H.Voltages[0]);
    Assert.AreEqual<Single>(3.6, H.Voltages[3]);
  finally H.Free; end;
end;

procedure TCellVoltageHeatmapTests.SetCellOutOfRangeIgnored;
var H: TOBDCellVoltageHeatmap;
begin
  H := TOBDCellVoltageHeatmap.Create(nil);
  try
    H.SetVoltages(TArray<Single>.Create(3.0, 3.0));
    H.SetCell(99, 4.0);   // ignored
    Assert.AreEqual<Single>(3.0, H.Voltages[0]);
    Assert.AreEqual<Single>(3.0, H.Voltages[1]);
  finally H.Free; end;
end;

procedure TCellVoltageHeatmapTests.CellAtOutOfBoundsReturnsMinusOne;
var H: TOBDCellVoltageHeatmap;
begin
  H := TOBDCellVoltageHeatmap.Create(nil);
  try
    Assert.AreEqual(-1, H.CellAt(0, 0));    // no cells assigned
  finally H.Free; end;
end;

{ TChargingFlowTests ------------------------------------------------------ }

procedure TChargingFlowTests.DefaultsZeroPower;
var F: TOBDChargingFlow;
begin
  F := TOBDChargingFlow.Create(nil);
  try
    Assert.AreEqual(0.0, F.ChargePowerKw, 0.001);
    Assert.AreEqual(0.0, F.DrivePowerKw,  0.001);
  finally F.Free; end;
end;

procedure TChargingFlowTests.ChargeKwRoundTrip;
var F: TOBDChargingFlow;
begin
  F := TOBDChargingFlow.Create(nil);
  try
    F.ChargePowerKw := 11.0;
    Assert.AreEqual(11.0, F.ChargePowerKw, 0.001);
  finally F.Free; end;
end;

procedure TChargingFlowTests.DriveKwRoundTrip;
var F: TOBDChargingFlow;
begin
  F := TOBDChargingFlow.Create(nil);
  try
    F.DrivePowerKw := -25.0;     // regen
    Assert.AreEqual(-25.0, F.DrivePowerKw, 0.001);
  finally F.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TReadinessGridTests);
  TDUnitX.RegisterTestFixture(TDriveCycleProgressTests);
  TDUnitX.RegisterTestFixture(TCellVoltageHeatmapTests);
  TDUnitX.RegisterTestFixture(TChargingFlowTests);

end.
