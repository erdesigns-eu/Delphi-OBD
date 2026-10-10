//------------------------------------------------------------------------------
//  OBDStudioDashboard - sample 18
//
//  Workshop dashboard built from the OBD Dashboard controls: two dial
//  gauges, two bar gauges, a value tile, a check-engine lamp, a trend
//  chart and a dot-matrix ticker on a TOBDDashboard grid, with a
//  connection bar on top. A timer feeds simulated engine data into the
//  controls' channel bindings, so the sample runs without a vehicle.
//  In a real application set Dashboard.Source to a TOBDLiveData and the
//  tiles receive their PIDs from it.
//
//  The toolbar switches the theme (ERDesigns light / dark, Windows
//  colours, follow the system), metric or imperial units and the
//  dashboard's edit mode (move, resize and remove tiles), and saves /
//  loads the layout as JSON in the Documents folder.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

program OBDStudioDashboard;

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.IOUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  ERD.UI.Types         in '..\..\src\UI\ERD.UI.Types.pas',
  ERD.UI.Units         in '..\..\src\UI\ERD.UI.Units.pas',
  ERD.UI.Theme         in '..\..\src\UI\ERD.UI.Theme.pas',
  ERD.UI.Gauges.Dial   in '..\..\src\UI\ERD.UI.Gauges.Dial.pas',
  ERD.UI.Gauges.Bar    in '..\..\src\UI\ERD.UI.Gauges.Bar.pas',
  ERD.UI.ValueTile     in '..\..\src\UI\ERD.UI.ValueTile.pas',
  ERD.UI.StatusLamp    in '..\..\src\UI\ERD.UI.StatusLamp.pas',
  ERD.UI.ConnectionBar in '..\..\src\UI\ERD.UI.ConnectionBar.pas',
  ERD.UI.TrendChart    in '..\..\src\UI\ERD.UI.TrendChart.pas',
  ERD.UI.MatrixDisplay in '..\..\src\UI\ERD.UI.MatrixDisplay.pas',
  ERD.UI.Dashboard     in '..\..\src\UI\ERD.UI.Dashboard.pas';

type
  TMainForm = class(TForm)
  strict private
    FTheme: TOBDTheme;
    FToolbar: TPanel;
    FThemeBox: TComboBox;
    FUnitBox: TComboBox;
    FEditBox: TCheckBox;
    FLinkBar: TOBDConnectionBar;
    FDashboard: TOBDDashboard;
    FTimer: TTimer;
    FTime: Double;
    function LayoutFile: string;
    procedure AddButton(const ACaption: string; ALeft: Integer;
      AOnClick: TNotifyEvent);
    procedure BuildToolbar;
    procedure BuildDefaultLayout;
    procedure ApplyFormColours;
    procedure ThemeBoxChange(Sender: TObject);
    procedure UnitBoxChange(Sender: TObject);
    procedure EditBoxClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure ThemeChange(Sender: TObject);
    procedure TimerTick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'OBD Studio - Dashboard';
  Width := 1100;
  Height := 820;
  Position := poScreenCenter;

  FTheme := TOBDTheme.Create(Self);
  FTheme.Mode := tmLight;
  FTheme.OnChange := ThemeChange;

  BuildToolbar;

  FLinkBar := TOBDConnectionBar.Create(Self);
  FLinkBar.Parent := Self;
  FLinkBar.Top := FToolbar.Height;
  FLinkBar.Align := alTop;
  FLinkBar.Theme := FTheme;
  FLinkBar.LinkState := lnkConnected;
  FLinkBar.AdapterText := 'Simulator';
  FLinkBar.ProtocolText := 'ISO 15765-4 CAN 11/500';
  FLinkBar.VIN := 'WVWZZZ1KZAW000000';

  FDashboard := TOBDDashboard.Create(Self);
  FDashboard.Parent := Self;
  FDashboard.Align := alClient;
  FDashboard.Theme := FTheme;
  FDashboard.Columns := 4;
  FDashboard.Rows := 6;
  BuildDefaultLayout;

  ApplyFormColours;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 100;
  FTimer.OnTimer := TimerTick;
end;

function TMainForm.LayoutFile: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'obd-studio-dashboard.json');
end;

procedure TMainForm.AddButton(const ACaption: string; ALeft: Integer;
  AOnClick: TNotifyEvent);
var
  B: TButton;
begin
  B := TButton.Create(Self);
  B.Parent := FToolbar;
  B.SetBounds(ALeft, 8, 100, 26);
  B.Caption := ACaption;
  B.OnClick := AOnClick;
end;

procedure TMainForm.BuildToolbar;
begin
  FToolbar := TPanel.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align := alTop;
  FToolbar.Height := 42;
  FToolbar.BevelOuter := bvNone;
  FToolbar.ParentBackground := False;

  FThemeBox := TComboBox.Create(Self);
  FThemeBox.Parent := FToolbar;
  FThemeBox.Style := csDropDownList;
  FThemeBox.SetBounds(10, 10, 150, 24);
  FThemeBox.Items.Add('ERDesigns light');
  FThemeBox.Items.Add('ERDesigns dark');
  FThemeBox.Items.Add('Windows colours');
  FThemeBox.Items.Add('Follow system');
  FThemeBox.ItemIndex := 0;
  FThemeBox.OnChange := ThemeBoxChange;

  FUnitBox := TComboBox.Create(Self);
  FUnitBox.Parent := FToolbar;
  FUnitBox.Style := csDropDownList;
  FUnitBox.SetBounds(170, 10, 110, 24);
  FUnitBox.Items.Add('Metric');
  FUnitBox.Items.Add('Imperial');
  FUnitBox.ItemIndex := 0;
  FUnitBox.OnChange := UnitBoxChange;

  FEditBox := TCheckBox.Create(Self);
  FEditBox.Parent := FToolbar;
  FEditBox.SetBounds(296, 12, 100, 20);
  FEditBox.Caption := 'Edit layout';
  FEditBox.OnClick := EditBoxClick;

  AddButton('Save layout', 410, SaveClick);
  AddButton('Load layout', 520, LoadClick);
  AddButton('Default layout', 630, ResetClick);
end;

procedure TMainForm.BuildDefaultLayout;
var
  Rpm, Speed: TOBDDialGauge;
  Coolant, Load: TOBDBarGauge;
  Battery: TOBDValueTile;
  Mil: TOBDStatusLamp;
  Chart: TOBDTrendChart;
  Ticker: TOBDMatrixDisplay;
begin
  FDashboard.ClearTiles;

  Rpm := FDashboard.AddTile('dial', 0, 0, 2, 2) as TOBDDialGauge;
  Rpm.Caption := 'Engine speed';
  Rpm.&Unit := 'rpm';
  Rpm.Max := 7000;
  Rpm.Alerts.SetHigh(5500, 6500);
  Rpm.Channel.PID := $0C;

  Speed := FDashboard.AddTile('dial', 2, 0, 2, 2) as TOBDDialGauge;
  Speed.Caption := 'Vehicle speed';
  Speed.&Unit := 'km/h';
  Speed.Max := 240;
  Speed.Channel.PID := $0D;

  Coolant := FDashboard.AddTile('bar', 0, 2) as TOBDBarGauge;
  Coolant.Caption := 'Coolant';
  Coolant.&Unit := OBD_DEGREE_SIGN + 'C';
  Coolant.Min := 40;
  Coolant.Max := 130;
  Coolant.Orientation := bgoVertical;
  Coolant.Alerts.SetHigh(105, 115);
  Coolant.Channel.PID := $05;

  Load := FDashboard.AddTile('bar', 1, 2) as TOBDBarGauge;
  Load.Caption := 'Engine load';
  Load.&Unit := '%';
  Load.Max := 100;
  Load.Orientation := bgoVertical;
  Load.Channel.PID := $04;

  Battery := FDashboard.AddTile('value', 2, 2) as TOBDValueTile;
  Battery.Caption := 'Battery';
  Battery.&Unit := 'V';
  Battery.Min := 10;
  Battery.Max := 16;
  Battery.Decimals := 1;
  Battery.Alerts.SetLow(12.0, 11.5);
  Battery.ShowSparkline := True;
  Battery.Channel.PID := $42;

  Mil := FDashboard.AddTile('lamp', 3, 2) as TOBDStatusLamp;
  Mil.Kind := lmkMIL;
  Mil.Caption := 'Check engine';
  Mil.State := lstOk;

  Chart := FDashboard.AddTile('trend', 0, 3, 4, 2) as TOBDTrendChart;
  Chart.TimeWindowSec := 30;
  Chart.AddChannel('Engine speed', $0C, 0, 7000, 'rpm');
  Chart.AddChannel('Vehicle speed', $0D, 0, 240, 'km/h');

  Ticker := FDashboard.AddTile('matrix', 0, 5, 4, 1) as TOBDMatrixDisplay;
  Ticker.Preset := mxpTicker;
  Ticker.Text := 'OBD STUDIO  -  SIMULATED ENGINE DATA';
end;

procedure TMainForm.ApplyFormColours;
var
  P: TOBDThemePalette;
begin
  P := FTheme.Palette;
  Color := P.Background;
  FToolbar.Color := P.GaugeFace;
  FToolbar.Font.Color := P.ForegroundText;
end;

procedure TMainForm.ThemeBoxChange(Sender: TObject);
begin
  case FThemeBox.ItemIndex of
    0:
      FTheme.Mode := tmLight;
    1:
      FTheme.Mode := tmDark;
    2:
      FTheme.Mode := tmWindows;
  else
    FTheme.Mode := tmAuto;
  end;
end;

procedure TMainForm.UnitBoxChange(Sender: TObject);
begin
  if FUnitBox.ItemIndex = 1 then
    FTheme.UnitSystem := usImperial
  else
    FTheme.UnitSystem := usMetric;
end;

procedure TMainForm.EditBoxClick(Sender: TObject);
begin
  FDashboard.EditMode := FEditBox.Checked;
end;

procedure TMainForm.SaveClick(Sender: TObject);
begin
  FDashboard.SaveLayoutToFile(LayoutFile);
end;

procedure TMainForm.LoadClick(Sender: TObject);
begin
  if TFile.Exists(LayoutFile) then
    FDashboard.LoadLayoutFromFile(LayoutFile);
end;

procedure TMainForm.ResetClick(Sender: TObject);
begin
  BuildDefaultLayout;
end;

procedure TMainForm.ThemeChange(Sender: TObject);
begin
  ApplyFormColours;
end;

procedure TMainForm.TimerTick(Sender: TObject);
var
  I: Integer;
  Throttle, Rpm, Speed, Coolant, Volts: Double;
  Ctl: TControl;
begin
  FTime := FTime + FTimer.Interval / 1000;
  // A slow accelerate / cruise / coast cycle with some engine noise.
  Throttle := 0.5 + 0.45 * Sin(FTime / 6);
  Rpm := 800 + Throttle * 5200 + 120 * Sin(FTime * 7);
  Speed := System.Math.Max(0.0, Throttle * 160 + 20 * Sin(FTime / 3));
  Coolant := 88 + 25 * System.Math.Max(0.0, Sin(FTime / 25));
  Volts := 13.9 - 0.3 * Throttle + 0.05 * Sin(FTime * 3);
  FLinkBar.Battery.PushValue(Volts);

  for I := 0 to FDashboard.Tiles.Count - 1 do
  begin
    Ctl := FDashboard.Tiles[I].Control;
    if Ctl is TOBDTrendChart then
    begin
      if TOBDTrendChart(Ctl).Channels.Count > 1 then
      begin
        TOBDTrendChart(Ctl).AddSample(0, Rpm);
        TOBDTrendChart(Ctl).AddSample(1, Speed);
      end;
    end
    else if Ctl is TOBDStatusLamp then
    begin
      if Coolant >= 112 then
        TOBDStatusLamp(Ctl).State := lstWarning
      else
        TOBDStatusLamp(Ctl).State := lstOk;
    end
    else if Ctl is TOBDDialGauge then
    begin
      case TOBDDialGauge(Ctl).Channel.PID of
        $0C:
          TOBDDialGauge(Ctl).Channel.PushValue(Rpm);
        $0D:
          TOBDDialGauge(Ctl).Channel.PushValue(Speed);
      end;
    end
    else if Ctl is TOBDBarGauge then
    begin
      case TOBDBarGauge(Ctl).Channel.PID of
        $05:
          TOBDBarGauge(Ctl).Channel.PushValue(Coolant);
        $04:
          TOBDBarGauge(Ctl).Channel.PushValue(Throttle * 100);
      end;
    end
    else if Ctl is TOBDValueTile then
      TOBDValueTile(Ctl).Channel.PushValue(Volts);
  end;
end;

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
