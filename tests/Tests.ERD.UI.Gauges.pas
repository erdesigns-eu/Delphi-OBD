//------------------------------------------------------------------------------
//  Tests.ERD.UI.Gauges
//
//  Rendering and behaviour tests for TOBDDialGauge, TOBDBarGauge and
//  TOBDValueTile. Every control is drawn off-screen and the pixels are
//  checked: the design-time preview, the "no data" state, a live value,
//  alert colouring, the dark theme and imperial units must all produce
//  visible output.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Gauges;

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
  ERD.UI.Theme,
  ERD.UI.Units,
  ERD.UI.Gauges.Base,
  ERD.UI.Gauges.Dial,
  ERD.UI.Gauges.Bar,
  ERD.UI.ValueTile,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDDialGaugeTests = class
  strict private
    FDial: TOBDDialGauge;
    FTheme: TOBDTheme;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsDial;
    [Test] procedure NoDataStateStillDraws;
    [Test] procedure LiveValueMovesNeedle;
    [Test] procedure AlarmPaintsDangerColour;
    [Test] procedure OverRangeIsOutOfRange;
    [Test] procedure StaleAfterTimeout;
    [Test] procedure DarkThemeUsesDarkBackground;
    [Test] procedure WindowsThemeUsesSystemBackground;
    [Test] procedure ImperialFormatsFahrenheit;
    [Test] procedure SessionMinMaxTracked;
    [Test] procedure SettingsRoundTrip;
  end;

  [TestFixture]
  TOBDBarGaugeTests = class
  strict private
    FBar: TOBDBarGauge;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsBar;
    [Test] procedure NoDataStateStillDraws;
    [Test] procedure LiveValueUsesAccentFill;
    [Test] procedure VerticalDraws;
    [Test] procedure CentreZeroDrawsBothSides;
    [Test] procedure SettingsRoundTrip;
  end;

  [TestFixture]
  TOBDValueTileTests = class
  strict private
    FTile: TOBDValueTile;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsTile;
    [Test] procedure NoDataStateStillDraws;
    [Test] procedure LiveValueChangesOutput;
    [Test] procedure RisingTrendDetected;
    [Test] procedure FallingTrendDetected;
    [Test] procedure ClearHistoryIsSteady;
    [Test] procedure WarningPaintsWarningColour;
    [Test] procedure SettingsRoundTrip;
  end;

implementation

function LightPalette: TOBDThemePalette;
begin
  Result := BRAND_PALETTE_LIGHT;
end;

{ TOBDDialGaugeTests --------------------------------------------------------- }

procedure TOBDDialGaugeTests.Setup;
begin
  FTheme := TOBDTheme.Create(nil);
  FTheme.Mode := tmLight;
  FDial := TOBDDialGauge.Create(nil);
  FDial.Theme := FTheme;
  FDial.SetBounds(0, 0, 220, 220);
  FDial.AnimateValueChanges := False;
  FDial.Min := 0;
  FDial.Max := 120;
  FDial.Caption := 'Coolant';
  FDial.&Unit := OBD_DEGREE_SIGN + 'C';
end;

procedure TOBDDialGaugeTests.TearDown;
begin
  FreeAndNil(FDial);
  FreeAndNil(FTheme);
end;

procedure TOBDDialGaugeTests.PreviewDrawsDial;
begin
  FDial.ForcePreview := True;
  Assert.AreEqual(Ord(dstLive), Ord(FDial.DataState));
  Assert.IsTrue(InkRatio(FDial) > 0.05, 'preview dial is (nearly) empty');
end;

procedure TOBDDialGaugeTests.NoDataStateStillDraws;
begin
  Assert.AreEqual(Ord(dstNoData), Ord(FDial.DataState));
  Assert.IsTrue(InkRatio(FDial) > 0.02, 'dial without data is empty');
end;

procedure TOBDDialGaugeTests.LiveValueMovesNeedle;
var
  A, B: TBitmap;
begin
  FDial.Value := 10;
  A := RenderControl(FDial);
  try
    FDial.Value := 100;
    B := RenderControl(FDial);
    try
      Assert.AreEqual(Ord(dstLive), Ord(FDial.DataState));
      Assert.IsTrue(BitmapsDiffer(A, B, 50), 'needle did not move');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDDialGaugeTests.AlarmPaintsDangerColour;
begin
  FDial.Alerts.SetHigh(100, 110);
  FDial.Value := 115;
  Assert.AreEqual(Ord(alvAlarm), Ord(FDial.AlertLevel));
  Assert.IsTrue(RenderedNear(FDial, LightPalette.Danger, 30) > 20,
    'alarm colour not drawn');
end;

procedure TOBDDialGaugeTests.OverRangeIsOutOfRange;
begin
  FDial.Value := 150;
  Assert.AreEqual(Ord(dstOutOfRange), Ord(FDial.DataState));
  Assert.IsTrue(InkRatio(FDial) > 0.02);
end;

procedure TOBDDialGaugeTests.StaleAfterTimeout;
begin
  FDial.Channel.StaleAfterMs := 20;
  FDial.Value := 50;
  Sleep(80);
  Assert.AreEqual(Ord(dstStale), Ord(FDial.DataState));
  Assert.IsTrue(InkRatio(FDial) > 0.02);
end;

procedure TOBDDialGaugeTests.DarkThemeUsesDarkBackground;
var
  Bmp: TBitmap;
begin
  FTheme.Mode := tmDark;
  FDial.ForcePreview := True;
  Bmp := RenderControl(FDial);
  try
    Assert.AreEqual<TColor>(BRAND_PALETTE_DARK.Background,
      ColorToRGB(Bmp.Canvas.Pixels[0, 0]));
    Assert.IsTrue(InkPixels(Bmp) > 100);
  finally
    Bmp.Free;
  end;
end;

procedure TOBDDialGaugeTests.WindowsThemeUsesSystemBackground;
var
  Bmp: TBitmap;
begin
  FTheme.Mode := tmWindows;
  FDial.ForcePreview := True;
  Bmp := RenderControl(FDial);
  try
    Assert.AreEqual<TColor>(WindowsPalette.Background,
      ColorToRGB(Bmp.Canvas.Pixels[0, 0]));
    Assert.IsTrue(InkPixels(Bmp) > 100);
  finally
    Bmp.Free;
  end;
end;

procedure TOBDDialGaugeTests.ImperialFormatsFahrenheit;
begin
  FTheme.UnitSystem := usImperial;
  Assert.AreEqual(OBD_DEGREE_SIGN + 'F', FDial.DisplayUnit);
  Assert.AreEqual('212 ' + OBD_DEGREE_SIGN + 'F', FDial.FormatValue(100));
  FDial.Value := 90;
  Assert.IsTrue(InkRatio(FDial) > 0.05);
end;

procedure TOBDDialGaugeTests.SessionMinMaxTracked;
begin
  FDial.Value := 40;
  FDial.Value := 95;
  FDial.Value := 60;
  Assert.AreEqual(Double(40), FDial.SessionMin, 1e-9);
  Assert.AreEqual(Double(95), FDial.SessionMax, 1e-9);
  FDial.ResetMinMax;
  Assert.IsTrue(IsNan(FDial.SessionMin));
end;

procedure TOBDDialGaugeTests.SettingsRoundTrip;
var
  O: TJSONObject;
  D: TOBDDialGauge;
begin
  FDial.Channel.PID := $05;
  FDial.Decimals := 1;
  FDial.Alerts.SetHigh(100, 110);
  O := TJSONObject.Create;
  D := TOBDDialGauge.Create(nil);
  try
    FDial.SaveSettings(O);
    D.LoadSettings(O);
    Assert.AreEqual('Coolant', D.Caption);
    Assert.AreEqual(FDial.&Unit, D.&Unit);
    Assert.AreEqual(Double(120), D.Max, 1e-9);
    Assert.AreEqual(Byte(1), D.Decimals);
    Assert.AreEqual(Byte($05), D.Channel.PID);
    Assert.IsTrue(alkHighAlarm in D.Alerts.Kinds);
    Assert.AreEqual(Double(110), D.Alerts.HighAlarm, 1e-9);
  finally
    D.Free;
    O.Free;
  end;
end;

{ TOBDBarGaugeTests ---------------------------------------------------------- }

procedure TOBDBarGaugeTests.Setup;
begin
  FBar := TOBDBarGauge.Create(nil);
  FBar.SetBounds(0, 0, 260, 70);
  FBar.AnimateValueChanges := False;
  FBar.Min := 0;
  FBar.Max := 100;
  FBar.Caption := 'Throttle';
  FBar.&Unit := '%';
end;

procedure TOBDBarGaugeTests.TearDown;
begin
  FreeAndNil(FBar);
end;

procedure TOBDBarGaugeTests.PreviewDrawsBar;
begin
  FBar.ForcePreview := True;
  Assert.IsTrue(InkRatio(FBar) > 0.05, 'preview bar is (nearly) empty');
end;

procedure TOBDBarGaugeTests.NoDataStateStillDraws;
begin
  Assert.AreEqual(Ord(dstNoData), Ord(FBar.DataState));
  Assert.IsTrue(InkRatio(FBar) > 0.02, 'bar without data is empty');
end;

procedure TOBDBarGaugeTests.LiveValueUsesAccentFill;
begin
  FBar.Value := 60;
  Assert.IsTrue(RenderedNear(FBar, LightPalette.Accent, 30) > 50,
    'bar fill not drawn in the accent colour');
end;

procedure TOBDBarGaugeTests.VerticalDraws;
begin
  FBar.Orientation := bgoVertical;
  FBar.SetBounds(0, 0, 80, 240);
  FBar.Value := 40;
  Assert.IsTrue(InkRatio(FBar) > 0.05);
end;

procedure TOBDBarGaugeTests.CentreZeroDrawsBothSides;
var
  A, B: TBitmap;
begin
  FBar.CentreZero := True;
  FBar.Min := -25;
  FBar.Max := 25;
  FBar.Value := -10;
  A := RenderControl(FBar);
  try
    FBar.Value := 10;
    B := RenderControl(FBar);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 50),
        'negative and positive trims render the same');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDBarGaugeTests.SettingsRoundTrip;
var
  O: TJSONObject;
  B: TOBDBarGauge;
begin
  FBar.Orientation := bgoVertical;
  FBar.CentreZero := True;
  O := TJSONObject.Create;
  B := TOBDBarGauge.Create(nil);
  try
    FBar.SaveSettings(O);
    B.LoadSettings(O);
    Assert.AreEqual(Ord(bgoVertical), Ord(B.Orientation));
    Assert.IsTrue(B.CentreZero);
    Assert.AreEqual('Throttle', B.Caption);
  finally
    B.Free;
    O.Free;
  end;
end;

{ TOBDValueTileTests --------------------------------------------------------- }

procedure TOBDValueTileTests.Setup;
begin
  FTile := TOBDValueTile.Create(nil);
  FTile.SetBounds(0, 0, 220, 140);
  FTile.AnimateValueChanges := False;
  FTile.Min := 0;
  FTile.Max := 100;
  FTile.Caption := 'Engine load';
  FTile.&Unit := '%';
end;

procedure TOBDValueTileTests.TearDown;
begin
  FreeAndNil(FTile);
end;

procedure TOBDValueTileTests.PreviewDrawsTile;
begin
  FTile.ForcePreview := True;
  Assert.IsTrue(InkRatio(FTile) > 0.03, 'preview tile is (nearly) empty');
end;

procedure TOBDValueTileTests.NoDataStateStillDraws;
begin
  Assert.AreEqual(Ord(dstNoData), Ord(FTile.DataState));
  Assert.IsTrue(InkRatio(FTile) > 0.01, 'tile without data is empty');
end;

procedure TOBDValueTileTests.LiveValueChangesOutput;
var
  A, B: TBitmap;
begin
  FTile.Value := 12;
  A := RenderControl(FTile);
  try
    FTile.Value := 87;
    B := RenderControl(FTile);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tile does not show the value');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDValueTileTests.RisingTrendDetected;
var
  I: Integer;
begin
  for I := 1 to 12 do
    FTile.Value := I * 5;
  Assert.AreEqual(Ord(tdRising), Ord(FTile.Trend));
end;

procedure TOBDValueTileTests.FallingTrendDetected;
var
  I: Integer;
begin
  for I := 12 downto 1 do
    FTile.Value := I * 5;
  Assert.AreEqual(Ord(tdFalling), Ord(FTile.Trend));
end;

procedure TOBDValueTileTests.ClearHistoryIsSteady;
var
  I: Integer;
begin
  for I := 1 to 12 do
    FTile.Value := I * 5;
  FTile.ClearHistory;
  Assert.AreEqual(Ord(tdSteady), Ord(FTile.Trend));
end;

procedure TOBDValueTileTests.WarningPaintsWarningColour;
begin
  FTile.Alerts.SetHigh(80, 95);
  FTile.Value := 85;
  Assert.AreEqual(Ord(alvWarning), Ord(FTile.AlertLevel));
  Assert.IsTrue(RenderedNear(FTile, LightPalette.Warning, 30) > 20,
    'warning colour not drawn');
end;

procedure TOBDValueTileTests.SettingsRoundTrip;
var
  O: TJSONObject;
  T: TOBDValueTile;
begin
  FTile.ShowSparkline := False;
  FTile.ShowTrend := False;
  O := TJSONObject.Create;
  T := TOBDValueTile.Create(nil);
  try
    FTile.SaveSettings(O);
    T.LoadSettings(O);
    Assert.IsFalse(T.ShowSparkline);
    Assert.IsFalse(T.ShowTrend);
    Assert.AreEqual('Engine load', T.Caption);
  finally
    T.Free;
    O.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDDialGaugeTests);
  TDUnitX.RegisterTestFixture(TOBDBarGaugeTests);
  TDUnitX.RegisterTestFixture(TOBDValueTileTests);

end.
