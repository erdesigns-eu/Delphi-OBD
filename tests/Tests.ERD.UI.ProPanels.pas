//------------------------------------------------------------------------------
//  Tests.ERD.UI.ProPanels
//
//  Rendering and behaviour tests for the OBD Studio Pro ECU, security,
//  measurement and actuator panels.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.ProPanels;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.EcuTopology,
  ERD.UI.KeySlots,
  ERD.UI.SignalBrowser,
  ERD.UI.SignalGrid,
  ERD.UI.ActuatorPanel,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDEcuTopologyTests = class
  strict private
    FTopology: TOBDEcuTopology;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddAndSelectEcu;
  end;

  [TestFixture]
  TOBDEcuListTests = class
  strict private
    FList: TOBDEcuList;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddAndSelectEcu;
  end;

  [TestFixture]
  TOBDKeySlotsTests = class
  strict private
    FSlots: TOBDKeySlots;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddSlotsAndSelect;
  end;

  [TestFixture]
  TOBDSignalBrowserTests = class
  strict private
    FBrowser: TOBDSignalBrowser;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddSignalAndFilter;
  end;

  [TestFixture]
  TOBDSignalGridTests = class
  strict private
    FGrid: TOBDSignalGrid;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure PushValueTracksMinMax;
    [Test] procedure AddSignalIsUnique;
  end;

  [TestFixture]
  TOBDActuatorPanelTests = class
  strict private
    FPanel: TOBDActuatorPanel;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddTestAndClampProgress;
  end;

implementation

procedure TOBDEcuTopologyTests.Setup;
begin
  FTopology := TOBDEcuTopology.Create(nil);
  FTopology.SetBounds(0, 0, 900, 520);
end;

procedure TOBDEcuTopologyTests.TearDown;
begin
  FreeAndNil(FTopology);
end;

procedure TOBDEcuTopologyTests.PreviewDraws;
begin
  FTopology.ForcePreview := True;
  Assert.IsTrue(InkRatio(FTopology) > 0.02, 'preview topology is empty');
end;

procedure TOBDEcuTopologyTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FTopology) > 0.002, 'empty topology draws nothing');
end;

procedure TOBDEcuTopologyTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  A := RenderControl(FTopology);
  try
    FTopology.Density := dnTablet;
    B := RenderControl(FTopology);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet topology did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDEcuTopologyTests.AddAndSelectEcu;
begin
  FTopology.AddBus('Powertrain CAN', '500 kbit/s');
  FTopology.AddEcu('01', 'Engine', '04L906056HT', 0, ensFaults, 2);
  Assert.AreEqual(1, FTopology.Ecus.Count);
  FTopology.SelectedAddress := '01';
  Assert.AreEqual(0, FTopology.ItemIndex);
  Assert.AreEqual('01', FTopology.SelectedAddress);
end;

procedure TOBDEcuListTests.Setup;
begin
  FList := TOBDEcuList.Create(nil);
  FList.SetBounds(0, 0, 680, 460);
end;

procedure TOBDEcuListTests.TearDown;
begin
  FreeAndNil(FList);
end;

procedure TOBDEcuListTests.PreviewDraws;
begin
  FList.ForcePreview := True;
  Assert.IsTrue(InkRatio(FList) > 0.02, 'preview ECU list is empty');
end;

procedure TOBDEcuListTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FList) > 0.002, 'empty ECU list draws nothing');
end;

procedure TOBDEcuListTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  A := RenderControl(FList);
  try
    FList.Density := dnTablet;
    B := RenderControl(FList);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet ECU list did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDEcuListTests.AddAndSelectEcu;
begin
  FList.AddBus('Comfort CAN', '500 kbit/s');
  FList.AddEcu('09', 'Central electrics', '5Q0937084CK', 0, ensFaults, 1);
  Assert.AreEqual(1, FList.Ecus.Count);
  FList.SelectedAddress := '09';
  Assert.AreEqual(0, FList.ItemIndex);
  Assert.AreEqual('09', FList.SelectedAddress);
end;

procedure TOBDKeySlotsTests.Setup;
begin
  FSlots := TOBDKeySlots.Create(nil);
  FSlots.SetBounds(0, 0, 520, 152);
end;

procedure TOBDKeySlotsTests.TearDown;
begin
  FreeAndNil(FSlots);
end;

procedure TOBDKeySlotsTests.PreviewDraws;
begin
  FSlots.ForcePreview := True;
  Assert.IsTrue(InkRatio(FSlots) > 0.02, 'preview key slots are empty');
end;

procedure TOBDKeySlotsTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FSlots) > 0.002, 'empty key slots draw nothing');
end;

procedure TOBDKeySlotsTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FSlots.ForcePreview := True;
  A := RenderControl(FSlots);
  try
    FSlots.Density := dnTablet;
    B := RenderControl(FSlots);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet key slots did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDKeySlotsTests.AddSlotsAndSelect;
begin
  FSlots.AddSlot('Key 1', 'Learned', kssLearned);
  FSlots.AddSlot('Key 2', 'Empty', kssEmpty);
  FSlots.ItemIndex := 1;
  Assert.AreEqual(2, FSlots.Count);
  Assert.AreEqual(1, FSlots.ItemIndex);
  Assert.AreEqual(Integer(kssEmpty), Integer(FSlots.Slots[1].State));
end;

procedure TOBDSignalBrowserTests.Setup;
begin
  FBrowser := TOBDSignalBrowser.Create(nil);
  FBrowser.SetBounds(0, 0, 380, 560);
end;

procedure TOBDSignalBrowserTests.TearDown;
begin
  FreeAndNil(FBrowser);
end;

procedure TOBDSignalBrowserTests.PreviewDraws;
begin
  FBrowser.ForcePreview := True;
  Assert.IsTrue(InkRatio(FBrowser) > 0.02, 'preview signal browser is empty');
end;

procedure TOBDSignalBrowserTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FBrowser) > 0.002, 'empty signal browser draws nothing');
end;

procedure TOBDSignalBrowserTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FBrowser.ForcePreview := True;
  A := RenderControl(FBrowser);
  try
    FBrowser.Density := dnTablet;
    B := RenderControl(FBrowser);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet signal browser did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDSignalBrowserTests.AddSignalAndFilter;
begin
  FBrowser.AddSignal('Air system', 'Boost pressure actual', 'hPa', '0x180000',
    'UWORD', True);
  FBrowser.FilterText := 'boost';
  Assert.AreEqual(1, FBrowser.Count);
  Assert.IsTrue(FBrowser.Signals[0].Checked);
  Assert.AreEqual('boost', FBrowser.FilterText);
end;

procedure TOBDSignalGridTests.Setup;
begin
  FGrid := TOBDSignalGrid.Create(nil);
  FGrid.SetBounds(0, 0, 640, 360);
end;

procedure TOBDSignalGridTests.TearDown;
begin
  FreeAndNil(FGrid);
end;

procedure TOBDSignalGridTests.PreviewDraws;
begin
  FGrid.ForcePreview := True;
  Assert.IsTrue(InkRatio(FGrid) > 0.02, 'preview signal grid is empty');
end;

procedure TOBDSignalGridTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FGrid) > 0.002, 'empty signal grid draws nothing');
end;

procedure TOBDSignalGridTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FGrid.ForcePreview := True;
  A := RenderControl(FGrid);
  try
    FGrid.Density := dnTablet;
    B := RenderControl(FGrid);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet signal grid did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDSignalGridTests.PushValueTracksMinMax;
begin
  FGrid.AddSignal('Boost', 'hPa');
  FGrid.PushValue(0, 1000);
  FGrid.PushValue(0, 1200);
  FGrid.PushValue(0, 900);
  Assert.AreEqual(Double(900), FGrid.Signals[0].Value, 1e-9);
  Assert.AreEqual(Double(900), FGrid.Signals[0].MinValue, 1e-9);
  Assert.AreEqual(Double(1200), FGrid.Signals[0].MaxValue, 1e-9);
  Assert.AreEqual(3, FGrid.Signals[0].SampleCount);
  FGrid.ResetMinMax;
  Assert.AreEqual(Double(900), FGrid.Signals[0].MinValue, 1e-9);
end;

procedure TOBDSignalGridTests.AddSignalIsUnique;
begin
  Assert.AreEqual(0, FGrid.AddSignal('Rail pressure', 'bar'));
  Assert.AreEqual(0, FGrid.AddSignal('Rail pressure', 'bar'));
  Assert.AreEqual(1, FGrid.Count);
end;

procedure TOBDActuatorPanelTests.Setup;
begin
  FPanel := TOBDActuatorPanel.Create(nil);
  FPanel.SetBounds(0, 0, 560, 430);
end;

procedure TOBDActuatorPanelTests.TearDown;
begin
  FreeAndNil(FPanel);
end;

procedure TOBDActuatorPanelTests.PreviewDraws;
begin
  FPanel.ForcePreview := True;
  Assert.IsTrue(InkRatio(FPanel) > 0.02, 'preview actuator panel is empty');
end;

procedure TOBDActuatorPanelTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FPanel) > 0.002, 'empty actuator panel draws nothing');
end;

procedure TOBDActuatorPanelTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FPanel.ForcePreview := True;
  A := RenderControl(FPanel);
  try
    FPanel.Density := dnTablet;
    B := RenderControl(FPanel);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet actuator panel did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDActuatorPanelTests.AddTestAndClampProgress;
begin
  FPanel.AddTest('Radiator fan', '0 rpm');
  FPanel.Items[0].State := apsRunning;
  FPanel.Items[0].Progress := 150;
  Assert.AreEqual(1, FPanel.Count);
  Assert.AreEqual(Integer(apsRunning), Integer(FPanel.Items[0].State));
  Assert.AreEqual(100, FPanel.Items[0].Progress);
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDEcuTopologyTests);
  TDUnitX.RegisterTestFixture(TOBDEcuListTests);
  TDUnitX.RegisterTestFixture(TOBDKeySlotsTests);
  TDUnitX.RegisterTestFixture(TOBDSignalBrowserTests);
  TDUnitX.RegisterTestFixture(TOBDSignalGridTests);
  TDUnitX.RegisterTestFixture(TOBDActuatorPanelTests);

end.
