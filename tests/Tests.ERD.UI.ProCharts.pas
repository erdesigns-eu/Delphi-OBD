//------------------------------------------------------------------------------
//  Tests.ERD.UI.ProCharts
//
//  Rendering and behaviour tests for the OBD Studio Pro chart controls:
//  the XY chart, calibration map grid, lap timer, G-meter and battery
//  cell matrix.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.ProCharts;

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
  ERD.UI.XYChart,
  ERD.UI.MapGrid,
  ERD.UI.LapTimer,
  ERD.UI.GMeter,
  ERD.UI.CellMatrix,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDXYChartTests = class
  strict private
    FChart: TOBDXYChart;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddSeriesAndPoints;
    [Test] procedure ClearRemovesPoints;
  end;

  [TestFixture]
  TOBDMapGridTests = class
  strict private
    FGrid: TOBDMapGrid;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure SetAxesAndValues;
    [Test] procedure OperatingPointSelects;
  end;

  [TestFixture]
  TOBDLapTimerTests = class
  strict private
    FTimer: TOBDLapTimer;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure StartStopReset;
    [Test] procedure AddSplitStores;
  end;

  [TestFixture]
  TOBDGMeterTests = class
  strict private
    FMeter: TOBDGMeter;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure SampleUpdatesPeak;
    [Test] procedure ResetPeakClears;
  end;

  [TestFixture]
  TOBDCellMatrixTests = class
  strict private
    FMatrix: TOBDCellMatrix;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure SetModuleStores;
    [Test] procedure CellVoltageRoundTrips;
  end;

implementation

{ TOBDXYChartTests }

procedure TOBDXYChartTests.Setup;
begin
  FChart := TOBDXYChart.Create(nil);
  FChart.SetBounds(0, 0, 760, 340);
end;

procedure TOBDXYChartTests.TearDown;
begin
  FreeAndNil(FChart);
end;

procedure TOBDXYChartTests.PreviewDraws;
begin
  FChart.ForcePreview := True;
  Assert.IsTrue(InkRatio(FChart) > 0.02, 'preview chart is empty');
end;

procedure TOBDXYChartTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FChart) > 0.002, 'empty chart draws nothing');
end;

procedure TOBDXYChartTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FChart.ForcePreview := True;
  A := RenderControl(FChart);
  try
    FChart.Density := dnTablet;
    B := RenderControl(FChart);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet chart did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDXYChartTests.AddSeriesAndPoints;
var
  S: TOBDXYSeries;
begin
  S := FChart.AddSeries('Power', 'kW', xyaLeft, xyckAccent);
  Assert.AreEqual(1, FChart.Series.Count);
  FChart.AddPoint(0, 1000, 20);
  FChart.AddPoint(0, 2000, 55);
  Assert.AreEqual(2, S.PointCount);
end;

procedure TOBDXYChartTests.ClearRemovesPoints;
var
  S: TOBDXYSeries;
begin
  S := FChart.AddSeries('Torque', 'Nm', xyaRight, xyckSubtle);
  FChart.SetPoints(0, [1000, 2000, 3000], [100, 200, 300]);
  Assert.AreEqual(3, S.PointCount);
  FChart.Clear;
  Assert.AreEqual(0, S.PointCount);
end;

{ TOBDMapGridTests }

procedure TOBDMapGridTests.Setup;
begin
  FGrid := TOBDMapGrid.Create(nil);
  FGrid.SetBounds(0, 0, 520, 360);
end;

procedure TOBDMapGridTests.TearDown;
begin
  FreeAndNil(FGrid);
end;

procedure TOBDMapGridTests.PreviewDraws;
begin
  FGrid.ForcePreview := True;
  Assert.IsTrue(InkRatio(FGrid) > 0.02, 'preview map is empty');
end;

procedure TOBDMapGridTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FGrid) > 0.002, 'empty map draws nothing');
end;

procedure TOBDMapGridTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FGrid.ForcePreview := True;
  A := RenderControl(FGrid);
  try
    FGrid.Density := dnTablet;
    B := RenderControl(FGrid);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet map did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDMapGridTests.SetAxesAndValues;
begin
  FGrid.SetAxes([800, 1600, 2400, 3200], [0, 25, 50, 75]);
  Assert.AreEqual(4, FGrid.ColCount);
  Assert.AreEqual(4, FGrid.RowCount);
  FGrid.Values[1, 2] := 12.5;
  Assert.AreEqual(12.5, FGrid.Values[1, 2], 0.001);
end;

procedure TOBDMapGridTests.OperatingPointSelects;
begin
  FGrid.SetAxes([800, 1600, 2400, 3200], [0, 25, 50, 75]);
  FGrid.SetOperatingPoint(2400, 50);
  Assert.IsTrue(InkRatio(FGrid) > 0.002, 'operating point draws nothing');
end;

{ TOBDLapTimerTests }

procedure TOBDLapTimerTests.Setup;
begin
  FTimer := TOBDLapTimer.Create(nil);
  FTimer.SetBounds(0, 0, 420, 420);
end;

procedure TOBDLapTimerTests.TearDown;
begin
  FreeAndNil(FTimer);
end;

procedure TOBDLapTimerTests.PreviewDraws;
begin
  FTimer.ForcePreview := True;
  Assert.IsTrue(InkRatio(FTimer) > 0.02, 'preview timer is empty');
end;

procedure TOBDLapTimerTests.EmptyStillDraws;
begin
  FTimer.ClearSplits;
  Assert.IsTrue(InkRatio(FTimer) > 0.002, 'empty timer draws nothing');
end;

procedure TOBDLapTimerTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FTimer.ForcePreview := True;
  A := RenderControl(FTimer);
  try
    FTimer.Density := dnTablet;
    B := RenderControl(FTimer);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet timer did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDLapTimerTests.StartStopReset;
begin
  FTimer.Start;
  Assert.AreEqual(Ord(ltsRunning), Ord(FTimer.State));
  FTimer.Stop;
  Assert.AreEqual(Ord(ltsFinished), Ord(FTimer.State));
  FTimer.Reset;
  Assert.AreEqual(Ord(ltsStaged), Ord(FTimer.State));
  Assert.AreEqual(Int64(0), FTimer.ElapsedMs);
end;

procedure TOBDLapTimerTests.AddSplitStores;
begin
  FTimer.ClearSplits;
  FTimer.AddSplit('60 ft', '2.41 s', '', -0.02);
  FTimer.AddSplit('1/4 mile', '17.62 s', '129 km/h', 0.14);
  Assert.AreEqual(2, FTimer.Splits.Count);
end;

{ TOBDGMeterTests }

procedure TOBDGMeterTests.Setup;
begin
  FMeter := TOBDGMeter.Create(nil);
  FMeter.SetBounds(0, 0, 300, 360);
end;

procedure TOBDGMeterTests.TearDown;
begin
  FreeAndNil(FMeter);
end;

procedure TOBDGMeterTests.PreviewDraws;
begin
  FMeter.ForcePreview := True;
  Assert.IsTrue(InkRatio(FMeter) > 0.02, 'preview meter is empty');
end;

procedure TOBDGMeterTests.EmptyStillDraws;
begin
  FMeter.ClearTrail;
  Assert.IsTrue(InkRatio(FMeter) > 0.002, 'empty meter draws nothing');
end;

procedure TOBDGMeterTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FMeter.ForcePreview := True;
  A := RenderControl(FMeter);
  try
    FMeter.Density := dnTablet;
    B := RenderControl(FMeter);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet meter did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDGMeterTests.SampleUpdatesPeak;
begin
  FMeter.ResetPeak;
  FMeter.AddSample(0.3, -0.4);
  Assert.IsTrue(FMeter.PeakG > 0.49, 'peak not updated');
end;

procedure TOBDGMeterTests.ResetPeakClears;
begin
  FMeter.AddSample(0.6, -0.6);
  FMeter.ResetPeak;
  Assert.AreEqual(0.0, FMeter.PeakG, 0.0001);
end;

{ TOBDCellMatrixTests }

procedure TOBDCellMatrixTests.Setup;
begin
  FMatrix := TOBDCellMatrix.Create(nil);
  FMatrix.SetBounds(0, 0, 620, 360);
end;

procedure TOBDCellMatrixTests.TearDown;
begin
  FreeAndNil(FMatrix);
end;

procedure TOBDCellMatrixTests.PreviewDraws;
begin
  FMatrix.ForcePreview := True;
  Assert.IsTrue(InkRatio(FMatrix) > 0.02, 'preview matrix is empty');
end;

procedure TOBDCellMatrixTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FMatrix) > 0.002, 'empty matrix draws nothing');
end;

procedure TOBDCellMatrixTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FMatrix.ForcePreview := True;
  A := RenderControl(FMatrix);
  try
    FMatrix.Density := dnTablet;
    B := RenderControl(FMatrix);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet matrix did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDCellMatrixTests.SetModuleStores;
begin
  FMatrix.ModuleCount := 2;
  FMatrix.CellsPerModule := 3;
  FMatrix.SetModule(0, [3.70, 3.75, 3.80], 24);
  Assert.AreEqual(3.75, FMatrix.CellVoltage[0, 1], 0.001);
  Assert.AreEqual(24.0, FMatrix.ModuleTemperature[0], 0.001);
end;

procedure TOBDCellMatrixTests.CellVoltageRoundTrips;
begin
  FMatrix.ModuleCount := 2;
  FMatrix.CellsPerModule := 3;
  FMatrix.CellVoltage[1, 2] := 3.712;
  Assert.AreEqual(3.712, FMatrix.CellVoltage[1, 2], 0.0001);
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDXYChartTests);
  TDUnitX.RegisterTestFixture(TOBDMapGridTests);
  TDUnitX.RegisterTestFixture(TOBDLapTimerTests);
  TDUnitX.RegisterTestFixture(TOBDGMeterTests);
  TDUnitX.RegisterTestFixture(TOBDCellMatrixTests);

end.
