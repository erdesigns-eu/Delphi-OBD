//------------------------------------------------------------------------------
//  Tests.ERD.UI.Panels
//
//  Rendering and behaviour tests for TOBDTrendChart, TOBDLiveDataGrid,
//  TOBDStatusLamp, TOBDConnectionBar and TOBDMatrixDisplay. Each control
//  is drawn off-screen in its preview, empty and live states, and the
//  pixels are checked, so a control that paints nothing fails.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Panels;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Units,
  ERD.UI.TrendChart,
  ERD.UI.LiveDataGrid,
  ERD.UI.StatusLamp,
  ERD.UI.ConnectionBar,
  ERD.UI.MatrixDisplay,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDTrendChartTests = class
  strict private
    FChart: TOBDTrendChart;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsTraces;
    [Test] procedure NoChannelsStillDraws;
    [Test] procedure SamplesDrawInChannelColour;
    [Test] procedure ValueAtReturnsLastSampleAtOrBefore;
    [Test] procedure HistoryIsTrimmed;
    [Test] procedure ViewFollowsLatestSample;
    [Test] procedure PausedViewScrolls;
    [Test] procedure SettingsRoundTrip;
  end;

  [TestFixture]
  TOBDLiveDataGridTests = class
  strict private
    FGrid: TOBDLiveDataGrid;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsRows;
    [Test] procedure EmptyGridStillDraws;
    [Test] procedure AddPIDIsUnique;
    [Test] procedure PushValueTracksMinMax;
    [Test] procedure PushUnknownPIDAddsRow;
    [Test] procedure CheckedPIDsFollowSetChecked;
    [Test] procedure RowsDraw;
    [Test] procedure SettingsRoundTrip;
  end;

  [TestFixture]
  TOBDStatusLampTests = class
  strict private
    FLamp: TOBDStatusLamp;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsLamp;
    [Test] procedure UnknownStateStillDraws;
    [Test] procedure OkPaintsSuccessColour;
    [Test] procedure MonitorStatusSetsMIL;
    [Test] procedure MonitorStatusReadiness;
  end;

  [TestFixture]
  TOBDConnectionBarTests = class
  strict private
    FBar: TOBDConnectionBar;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsSegments;
    [Test] procedure DisconnectedStillDraws;
    [Test] procedure ConnectedPaintsSuccessColour;
    [Test] procedure LowBatteryPaintsWarning;
  end;

  [TestFixture]
  TOBDMatrixDisplayTests = class
  strict private
    FMatrix: TOBDMatrixDisplay;
    function LitDots: Integer;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure EmptyTextHasNoLitDots;
    [Test] procedure TextLightsDots;
    [Test] procedure RenderDrawsBoardAndDots;
    [Test] procedure ScrollLeftMovesText;
    [Test] procedure TickerPresetScrolls;
    [Test] procedure LCDPresetUsesSquareDots;
    [Test] procedure ValuePresetShowsChannelValue;
    [Test] procedure ValueModeWithoutDataShowsDashes;
    [Test] procedure ShowAlertAlarmBlinks;
    [Test] procedure IconLightsDots;
    [Test] procedure LinesRotateOnPass;
    [Test] procedure SettingsRoundTrip;
  end;

implementation

{ TOBDTrendChartTests -------------------------------------------------------- }

procedure TOBDTrendChartTests.Setup;
begin
  FChart := TOBDTrendChart.Create(nil);
  FChart.SetBounds(0, 0, 480, 240);
end;

procedure TOBDTrendChartTests.TearDown;
begin
  FreeAndNil(FChart);
end;

procedure TOBDTrendChartTests.PreviewDrawsTraces;
begin
  FChart.ForcePreview := True;
  Assert.IsTrue(InkRatio(FChart) > 0.02, 'preview chart is (nearly) empty');
end;

procedure TOBDTrendChartTests.NoChannelsStillDraws;
begin
  Assert.AreEqual(0, FChart.Channels.Count);
  Assert.IsTrue(InkRatio(FChart) > 0.002, 'chart without channels is empty');
end;

procedure TOBDTrendChartTests.SamplesDrawInChannelColour;
var
  Ch: TOBDTrendChannel;
  I: Integer;
begin
  Ch := FChart.AddChannel('RPM', $0C, 0, 6000, 'rpm');
  Ch.Color := clFuchsia;
  for I := 0 to 59 do
    FChart.AddSampleAt(0, I * 1000, 800 + (I mod 10) * 400);
  Assert.IsTrue(RenderedNear(FChart, clFuchsia, 40) > 50,
    'trace not drawn in the channel colour');
end;

procedure TOBDTrendChartTests.ValueAtReturnsLastSampleAtOrBefore;
var
  Ch: TOBDTrendChannel;
begin
  Ch := FChart.AddChannel('Coolant', $05, -40, 130);
  FChart.AddSampleAt(0, 1000, 20);
  FChart.AddSampleAt(0, 2000, 30);
  FChart.AddSampleAt(0, 3000, 40);
  Assert.IsTrue(IsNan(Ch.ValueAt(500)));
  Assert.AreEqual(Double(20), Ch.ValueAt(1000), 1e-9);
  Assert.AreEqual(Double(30), Ch.ValueAt(2999), 1e-9);
  Assert.AreEqual(Double(40), Ch.ValueAt(99999), 1e-9);
end;

procedure TOBDTrendChartTests.HistoryIsTrimmed;
var
  Ch: TOBDTrendChannel;
  I: Integer;
begin
  FChart.HistorySec := 60;
  Ch := FChart.AddChannel('Load', $04, 0, 100);
  for I := 0 to 300 do
    FChart.AddSampleAt(0, Int64(I) * 1000, I mod 100);
  Assert.IsTrue(Ch.SampleCount <= 62, 'history not trimmed');
  Assert.IsTrue(Ch.Sample(0).TimeMs >= 239000);
end;

procedure TOBDTrendChartTests.ViewFollowsLatestSample;
begin
  FChart.AddChannel('Speed', $0D, 0, 250, 'km/h');
  FChart.AddSampleAt(0, 200000, 50);
  Assert.AreEqual(Int64(200000), FChart.ViewEndMs);
end;

procedure TOBDTrendChartTests.PausedViewScrolls;
begin
  FChart.AddChannel('Speed', $0D, 0, 250, 'km/h');
  FChart.AddSampleAt(0, 200000, 50);
  FChart.Paused := True;
  FChart.ScrollBy(5000);
  Assert.AreEqual(Int64(195000), FChart.ViewEndMs);
  FChart.AddSampleAt(0, 210000, 60);
  Assert.AreEqual(Int64(195000), FChart.ViewEndMs,
    'paused view moved with new data');
  FChart.Paused := False;
  Assert.AreEqual(Int64(210000), FChart.ViewEndMs);
end;

procedure TOBDTrendChartTests.SettingsRoundTrip;
var
  O: TJSONObject;
  C: TOBDTrendChart;
begin
  FChart.TimeWindowSec := 120;
  FChart.AddChannel('O2 B1S1', $14, 0, 1.275, 'V').Decimals := 2;
  FChart.AddChannel('RPM', $0C, 0, 7000, 'rpm');
  O := TJSONObject.Create;
  C := TOBDTrendChart.Create(nil);
  try
    FChart.SaveSettings(O);
    C.LoadSettings(O);
    Assert.AreEqual(120, C.TimeWindowSec);
    Assert.AreEqual(2, C.Channels.Count);
    Assert.AreEqual('O2 B1S1', C.Channels[0].Caption);
    Assert.AreEqual(Integer($14), Integer(C.Channels[0].PID));
    Assert.AreEqual(2, Integer(C.Channels[0].Decimals));
    Assert.AreEqual(Double(7000), C.Channels[1].Max, 1e-9);
  finally
    C.Free;
    O.Free;
  end;
end;

{ TOBDLiveDataGridTests ------------------------------------------------------ }

procedure TOBDLiveDataGridTests.Setup;
begin
  FGrid := TOBDLiveDataGrid.Create(nil);
  FGrid.SetBounds(0, 0, 520, 260);
end;

procedure TOBDLiveDataGridTests.TearDown;
begin
  FreeAndNil(FGrid);
end;

procedure TOBDLiveDataGridTests.PreviewDrawsRows;
begin
  FGrid.ForcePreview := True;
  Assert.IsTrue(InkRatio(FGrid) > 0.03, 'preview grid is (nearly) empty');
end;

procedure TOBDLiveDataGridTests.EmptyGridStillDraws;
begin
  Assert.AreEqual(0, FGrid.RowCount);
  Assert.IsTrue(InkRatio(FGrid) > 0.002, 'empty grid draws nothing');
end;

procedure TOBDLiveDataGridTests.AddPIDIsUnique;
begin
  Assert.AreEqual(0, FGrid.AddPID($0C));
  Assert.AreEqual(1, FGrid.AddPID($0D));
  Assert.AreEqual(0, FGrid.AddPID($0C));
  Assert.AreEqual(2, FGrid.RowCount);
  Assert.AreEqual(Integer($0D), Integer(FGrid.Row(1).PID));
end;

procedure TOBDLiveDataGridTests.PushValueTracksMinMax;
var
  R: TOBDLiveDataRow;
begin
  FGrid.AddPID($05, 'Coolant');
  FGrid.PushValue($05, 60, OBD_DEGREE_SIGN + 'C');
  FGrid.PushValue($05, 92);
  FGrid.PushValue($05, 85);
  R := FGrid.Row(0);
  Assert.AreEqual(Double(85), R.Value, 1e-9);
  Assert.AreEqual(Double(60), R.MinValue, 1e-9);
  Assert.AreEqual(Double(92), R.MaxValue, 1e-9);
  FGrid.ResetMinMax;
  Assert.IsTrue(IsNan(FGrid.Row(0).MinValue));
end;

procedure TOBDLiveDataGridTests.PushUnknownPIDAddsRow;
begin
  FGrid.PushValue($0D, 48, 'km/h');
  Assert.AreEqual(1, FGrid.RowCount);
  Assert.AreEqual('km/h', FGrid.Row(0).UnitText);
end;

procedure TOBDLiveDataGridTests.CheckedPIDsFollowSetChecked;
var
  Checked: TBytes;
begin
  FGrid.SetPIDs(TBytes.Create($04, $05, $0C, $0D));
  FGrid.SetChecked($05, True);
  FGrid.SetChecked($0D, True);
  Assert.IsTrue(FGrid.IsChecked($05));
  Assert.IsFalse(FGrid.IsChecked($04));
  Assert.AreEqual(2, Length(FGrid.CheckedPIDs));
  FGrid.SetChecked($05, False);
  Checked := FGrid.CheckedPIDs;
  Assert.AreEqual(1, Length(Checked));
  Assert.AreEqual(Integer($0D), Integer(Checked[0]));
end;

procedure TOBDLiveDataGridTests.RowsDraw;
var
  Empty: Double;
begin
  Empty := InkRatio(FGrid);
  FGrid.SetPIDs(TBytes.Create($04, $05, $0C, $0D, $11));
  FGrid.PushValue($0C, 850, 'rpm');
  Assert.IsTrue(InkRatio(FGrid) > Empty, 'rows do not draw');
end;

procedure TOBDLiveDataGridTests.SettingsRoundTrip;
var
  O: TJSONObject;
  G: TOBDLiveDataGrid;
begin
  FGrid.SetPIDs(TBytes.Create($04, $05, $0C));
  FGrid.SetChecked($0C, True);
  FGrid.StaleAfterMs := 5000;
  O := TJSONObject.Create;
  G := TOBDLiveDataGrid.Create(nil);
  try
    FGrid.SaveSettings(O);
    G.LoadSettings(O);
    Assert.AreEqual(3, G.RowCount);
    Assert.IsTrue(G.IsChecked($0C));
    Assert.IsFalse(G.IsChecked($04));
    Assert.AreEqual(5000, Integer(G.StaleAfterMs));
  finally
    G.Free;
    O.Free;
  end;
end;

{ TOBDStatusLampTests -------------------------------------------------------- }

procedure TOBDStatusLampTests.Setup;
begin
  FLamp := TOBDStatusLamp.Create(nil);
end;

procedure TOBDStatusLampTests.TearDown;
begin
  FreeAndNil(FLamp);
end;

procedure TOBDStatusLampTests.PreviewDrawsLamp;
begin
  FLamp.ForcePreview := True;
  Assert.IsTrue(InkRatio(FLamp) > 0.05, 'preview lamp is (nearly) empty');
end;

procedure TOBDStatusLampTests.UnknownStateStillDraws;
begin
  Assert.AreEqual(Ord(lstUnknown), Ord(FLamp.State));
  Assert.IsTrue(InkRatio(FLamp) > 0.02, 'unknown lamp is empty');
end;

procedure TOBDStatusLampTests.OkPaintsSuccessColour;
begin
  FLamp.Kind := lmkConnection;
  FLamp.State := lstOk;
  Assert.IsTrue(RenderedNear(FLamp, BRAND_PALETTE_LIGHT.Success, 30) > 20,
    'OK lamp not drawn in the success colour');
end;

procedure TOBDStatusLampTests.MonitorStatusSetsMIL;
begin
  FLamp.Kind := lmkMIL;
  FLamp.ApplyMonitorStatus(TBytes.Create($83, $07, $00, $00));
  Assert.AreEqual(Ord(lstAlarm), Ord(FLamp.State));
  Assert.AreEqual(3, FLamp.DTCCount);
  FLamp.ApplyMonitorStatus(TBytes.Create($00, $07, $00, $00));
  Assert.AreEqual(Ord(lstOk), Ord(FLamp.State));
end;

procedure TOBDStatusLampTests.MonitorStatusReadiness;
begin
  FLamp.Kind := lmkReadiness;
  // Byte C: catalyst supported; byte D: catalyst incomplete.
  FLamp.ApplyMonitorStatus(TBytes.Create($00, $07, $01, $01));
  Assert.AreEqual(Ord(lstWarning), Ord(FLamp.State));
  FLamp.ApplyMonitorStatus(TBytes.Create($00, $07, $01, $00));
  Assert.AreEqual(Ord(lstOk), Ord(FLamp.State));
end;

{ TOBDConnectionBarTests ----------------------------------------------------- }

procedure TOBDConnectionBarTests.Setup;
begin
  FBar := TOBDConnectionBar.Create(nil);
  FBar.SetBounds(0, 0, 900, 36);
end;

procedure TOBDConnectionBarTests.TearDown;
begin
  FreeAndNil(FBar);
end;

procedure TOBDConnectionBarTests.PreviewDrawsSegments;
begin
  FBar.ForcePreview := True;
  Assert.IsTrue(InkRatio(FBar) > 0.02, 'preview bar is (nearly) empty');
end;

procedure TOBDConnectionBarTests.DisconnectedStillDraws;
begin
  Assert.AreEqual(Ord(lnkDisconnected), Ord(FBar.LinkState));
  Assert.IsTrue(InkRatio(FBar) > 0.01, 'disconnected bar is empty');
end;

procedure TOBDConnectionBarTests.ConnectedPaintsSuccessColour;
begin
  FBar.LinkState := lnkConnected;
  FBar.AdapterText := 'ELM327 v1.5';
  FBar.ProtocolText := 'ISO 15765-4 CAN';
  Assert.IsTrue(RenderedNear(FBar, BRAND_PALETTE_LIGHT.Success, 30) > 10,
    'connected state not drawn in the success colour');
end;

procedure TOBDConnectionBarTests.LowBatteryPaintsWarning;
begin
  FBar.LinkState := lnkConnected;
  FBar.Battery.PushValue(11.8);
  Assert.IsTrue(RenderedNear(FBar, BRAND_PALETTE_LIGHT.Warning, 30) > 5,
    'low battery not drawn in the warning colour');
end;

{ TOBDMatrixDisplayTests ----------------------------------------------------- }

procedure TOBDMatrixDisplayTests.Setup;
begin
  FMatrix := TOBDMatrixDisplay.Create(nil);
  FMatrix.SetBounds(0, 0, 384, 60);
end;

procedure TOBDMatrixDisplayTests.TearDown;
begin
  FreeAndNil(FMatrix);
end;

function TOBDMatrixDisplayTests.LitDots: Integer;
var
  F: TArray<TColor>;
  I: Integer;
begin
  Result := 0;
  F := FMatrix.BuildFrame;
  for I := 0 to High(F) do
    if F[I] <> clNone then
      Inc(Result);
end;

procedure TOBDMatrixDisplayTests.EmptyTextHasNoLitDots;
begin
  FMatrix.Text := '';
  Assert.AreEqual(0, LitDots);
end;

procedure TOBDMatrixDisplayTests.TextLightsDots;
var
  C, R: Integer;
  Any: Boolean;
begin
  FMatrix.Text := 'OBD';
  Assert.IsTrue(LitDots > 20);
  Any := False;
  for R := 0 to FMatrix.Rows - 1 do
    for C := 0 to 5 do
      Any := Any or FMatrix.IsDotOn(C, R);
  Assert.IsTrue(Any, 'left-aligned text does not start at column 0');
  Assert.IsFalse(FMatrix.IsDotOn(-1, 0));
  Assert.IsFalse(FMatrix.IsDotOn(0, FMatrix.Rows));
end;

procedure TOBDMatrixDisplayTests.RenderDrawsBoardAndDots;
var
  Blank: Double;
begin
  FMatrix.Text := '';
  Blank := InkRatio(FMatrix);
  Assert.IsTrue(Blank > 0.05, 'unlit dots are not drawn');
  FMatrix.Text := 'CHECK ENGINE';
  Assert.IsTrue(RenderedNear(FMatrix, FMatrix.DotColor, 30) > 100,
    'lit dots not drawn in the dot colour');
end;

procedure TOBDMatrixDisplayTests.ScrollLeftMovesText;
var
  Before: Integer;
begin
  FMatrix.Text := 'HELLO';
  FMatrix.Scroll := mxsLeft;
  Before := FMatrix.ScrollX;
  FMatrix.Step;
  FMatrix.Step;
  Assert.AreEqual(Before - 2, FMatrix.ScrollX);
end;

procedure TOBDMatrixDisplayTests.TickerPresetScrolls;
begin
  FMatrix.Preset := mxpTicker;
  Assert.AreEqual(Ord(mxsLeft), Ord(FMatrix.Scroll));
  Assert.AreEqual(Ord(mxmText), Ord(FMatrix.Mode));
  Assert.AreEqual(96, FMatrix.Columns);
end;

procedure TOBDMatrixDisplayTests.LCDPresetUsesSquareDots;
begin
  FMatrix.Preset := mxpLCD;
  Assert.AreEqual(Ord(mxdSquare), Ord(FMatrix.DotShape));
  Assert.AreNotEqual<TColor>(clNone, FMatrix.BoardColor);
  FMatrix.Text := 'P0301';
  Assert.IsTrue(InkRatio(FMatrix) > 0.05);
end;

procedure TOBDMatrixDisplayTests.ValuePresetShowsChannelValue;
begin
  FMatrix.Preset := mxpValue;
  Assert.AreEqual(Ord(mxmValue), Ord(FMatrix.Mode));
  FMatrix.Caption := '';
  FMatrix.&Unit := 'rpm';
  FMatrix.Channel.PushValue(2450);
  Assert.AreEqual('2450 rpm', FMatrix.CurrentText);
  Assert.AreEqual(Ord(dstLive), Ord(FMatrix.DataState));
  Assert.IsTrue(LitDots > 20);
end;

procedure TOBDMatrixDisplayTests.ValueModeWithoutDataShowsDashes;
begin
  FMatrix.Mode := mxmValue;
  FMatrix.Caption := '';
  FMatrix.&Unit := '';
  Assert.AreEqual('--', FMatrix.CurrentText);
  Assert.AreEqual(Ord(dstNoData), Ord(FMatrix.DataState));
  Assert.IsTrue(LitDots > 0, 'no-data value shows nothing');
end;

procedure TOBDMatrixDisplayTests.ShowAlertAlarmBlinks;
begin
  FMatrix.ShowAlert('OIL PRESSURE', alvAlarm);
  Assert.IsTrue(FMatrix.Blink);
  Assert.AreEqual(Ord(mxiWarning), Ord(FMatrix.Icon));
  Assert.AreEqual('OIL PRESSURE', FMatrix.CurrentText);
  FMatrix.ShowAlert('READY', alvNormal);
  Assert.IsFalse(FMatrix.Blink);
  Assert.AreEqual(Ord(mxiCheck), Ord(FMatrix.Icon));
end;

procedure TOBDMatrixDisplayTests.IconLightsDots;
begin
  FMatrix.Text := '';
  FMatrix.Icon := mxiEngine;
  Assert.IsTrue(LitDots > 5, 'icon does not light any dots');
end;

procedure TOBDMatrixDisplayTests.LinesRotateOnPass;
var
  I: Integer;
  Seen: TStringList;
begin
  FMatrix.Lines.Text := 'FIRST'#13#10'SECOND';
  FMatrix.Scroll := mxsLeft;
  Seen := TStringList.Create;
  try
    Seen.Duplicates := dupIgnore;
    Seen.Sorted := True;
    for I := 0 to 400 do
    begin
      Seen.Add(FMatrix.CurrentText);
      FMatrix.Step;
    end;
    Assert.AreEqual(2, Seen.Count, 'messages do not rotate');
  finally
    Seen.Free;
  end;
end;

procedure TOBDMatrixDisplayTests.SettingsRoundTrip;
var
  O: TJSONObject;
  M: TOBDMatrixDisplay;
begin
  FMatrix.Preset := mxpWarning;
  FMatrix.Text := 'SERVICE DUE';
  O := TJSONObject.Create;
  M := TOBDMatrixDisplay.Create(nil);
  try
    FMatrix.SaveSettings(O);
    M.LoadSettings(O);
    Assert.AreEqual('SERVICE DUE', M.Text);
    Assert.AreEqual(Ord(mxsLeft), Ord(M.Scroll));
    Assert.AreEqual(Ord(mxiWarning), Ord(M.Icon));
    Assert.AreEqual<TColor>(FMatrix.DotColor, M.DotColor);
  finally
    M.Free;
    O.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDTrendChartTests);
  TDUnitX.RegisterTestFixture(TOBDLiveDataGridTests);
  TDUnitX.RegisterTestFixture(TOBDStatusLampTests);
  TDUnitX.RegisterTestFixture(TOBDConnectionBarTests);
  TDUnitX.RegisterTestFixture(TOBDMatrixDisplayTests);

end.
