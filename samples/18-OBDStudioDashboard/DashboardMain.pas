//------------------------------------------------------------------------------
//  DashboardMain
//
//  Main form of sample 18. Every control is placed in the form
//  designer (DashboardMain.dfm): a TOBDTheme, a TOBDConnectionBar and a
//  TOBDDashboard whose tiles are two dial gauges, two bar gauges, a
//  value tile, a check-engine lamp, a trend chart and a dot-matrix
//  ticker. Each tile has its Mode 01 PID in Channel.PID.
//
//  tmrSimulation feeds simulated engine data into the tiles through
//  their channel bindings, so the sample runs without a vehicle. With
//  a TOBDLiveData on the form, set OBDDashboard.Source to it and
//  disable the timer: the tiles then receive their PIDs from the car.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit DashboardMain;

interface

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
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Theme,
  ERD.UI.Gauges.Base,
  ERD.UI.Gauges.Dial,
  ERD.UI.Gauges.Bar,
  ERD.UI.ValueTile,
  ERD.UI.StatusLamp,
  ERD.UI.ConnectionBar,
  ERD.UI.TrendChart,
  ERD.UI.MatrixDisplay,
  ERD.UI.Dashboard;

type
  TfrmDashboard = class(TForm)
    OBDTheme: TOBDTheme;
    pnlToolbar: TPanel;
    lblTheme: TLabel;
    cbTheme: TComboBox;
    lblUnits: TLabel;
    cbUnits: TComboBox;
    chkEditLayout: TCheckBox;
    btnSaveLayout: TButton;
    btnLoadLayout: TButton;
    OBDConnectionBar: TOBDConnectionBar;
    OBDDashboard: TOBDDashboard;
    dlEngineSpeed: TOBDDialGauge;
    dlVehicleSpeed: TOBDDialGauge;
    barCoolant: TOBDBarGauge;
    barEngineLoad: TOBDBarGauge;
    tileBattery: TOBDValueTile;
    lampMIL: TOBDStatusLamp;
    chtTrend: TOBDTrendChart;
    mxTicker: TOBDMatrixDisplay;
    tmrSimulation: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure cbThemeChange(Sender: TObject);
    procedure cbUnitsChange(Sender: TObject);
    procedure chkEditLayoutClick(Sender: TObject);
    procedure btnSaveLayoutClick(Sender: TObject);
    procedure btnLoadLayoutClick(Sender: TObject);
    procedure OBDThemeChange(Sender: TObject);
    procedure tmrSimulationTimer(Sender: TObject);
  strict private
    FTime: Double;
    FThrottle: Double;
    FCoolant: Double;
    function LayoutFile: string;
    function SimulatedValue(APID: Byte): Double;
    procedure ApplyToolbarColours;
  end;

var
  frmDashboard: TfrmDashboard;

implementation

{$R *.dfm}

const
  PID_ENGINE_LOAD = $04;
  PID_COOLANT = $05;
  PID_ENGINE_SPEED = $0C;
  PID_VEHICLE_SPEED = $0D;
  PID_MODULE_VOLTAGE = $42;

procedure TfrmDashboard.FormCreate(Sender: TObject);
begin
  ApplyToolbarColours;
end;

function TfrmDashboard.LayoutFile: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'obd-studio-dashboard.json');
end;

function TfrmDashboard.SimulatedValue(APID: Byte): Double;
begin
  case APID of
    PID_ENGINE_LOAD:
      Result := FThrottle * 100;
    PID_COOLANT:
      Result := FCoolant;
    PID_ENGINE_SPEED:
      Result := 800 + FThrottle * 5200 + 120 * Sin(FTime * 7);
    PID_VEHICLE_SPEED:
      Result := System.Math.Max(0.0, FThrottle * 160 + 20 * Sin(FTime / 3));
    PID_MODULE_VOLTAGE:
      Result := 13.9 - 0.3 * FThrottle + 0.05 * Sin(FTime * 3);
  else
    Result := NaN;
  end;
end;

procedure TfrmDashboard.ApplyToolbarColours;
var
  P: TOBDThemePalette;
begin
  P := OBDTheme.Palette;
  Color := P.Background;
  pnlToolbar.Color := P.GaugeFace;
  pnlToolbar.Font.Color := P.ForegroundText;
end;

procedure TfrmDashboard.cbThemeChange(Sender: TObject);
begin
  case cbTheme.ItemIndex of
    0:
      OBDTheme.Mode := tmLight;
    1:
      OBDTheme.Mode := tmDark;
    2:
      OBDTheme.Mode := tmWindows;
  else
    OBDTheme.Mode := tmAuto;
  end;
end;

procedure TfrmDashboard.cbUnitsChange(Sender: TObject);
begin
  if cbUnits.ItemIndex = 1 then
    OBDTheme.UnitSystem := usImperial
  else
    OBDTheme.UnitSystem := usMetric;
end;

procedure TfrmDashboard.chkEditLayoutClick(Sender: TObject);
begin
  OBDDashboard.EditMode := chkEditLayout.Checked;
end;

procedure TfrmDashboard.btnSaveLayoutClick(Sender: TObject);
begin
  OBDDashboard.SaveLayoutToFile(LayoutFile);
end;

procedure TfrmDashboard.btnLoadLayoutClick(Sender: TObject);
begin
  if TFile.Exists(LayoutFile) then
    OBDDashboard.LoadLayoutFromFile(LayoutFile);
end;

procedure TfrmDashboard.OBDThemeChange(Sender: TObject);
begin
  ApplyToolbarColours;
end;

procedure TfrmDashboard.tmrSimulationTimer(Sender: TObject);
var
  I, J: Integer;
  V: Double;
  Ctl: TOBDCustomControl;
  Chart: TOBDTrendChart;
begin
  FTime := FTime + tmrSimulation.Interval / 1000;
  // A slow accelerate / cruise / coast cycle with some engine noise.
  FThrottle := 0.5 + 0.45 * Sin(FTime / 6);
  FCoolant := 88 + 25 * System.Math.Max(0.0, Sin(FTime / 25));
  OBDConnectionBar.Battery.PushValue(SimulatedValue(PID_MODULE_VOLTAGE));

  // Walk the tiles rather than the form fields: a layout loaded from
  // JSON creates new tile controls that the form has no fields for.
  for I := 0 to OBDDashboard.Tiles.Count - 1 do
  begin
    Ctl := OBDDashboard.Tiles[I].Control;
    if Ctl is TOBDGaugeBase then
    begin
      V := SimulatedValue(TOBDGaugeBase(Ctl).Channel.PID);
      if not IsNaN(V) then
        TOBDGaugeBase(Ctl).Channel.PushValue(V);
    end
    else if Ctl is TOBDTrendChart then
    begin
      Chart := TOBDTrendChart(Ctl);
      for J := 0 to Chart.Channels.Count - 1 do
      begin
        V := SimulatedValue(Chart.Channels[J].PID);
        if not IsNaN(V) then
          Chart.AddSample(J, V);
      end;
    end
    else if Ctl is TOBDStatusLamp then
    begin
      if FCoolant >= 112 then
        TOBDStatusLamp(Ctl).State := lstWarning
      else
        TOBDStatusLamp(Ctl).State := lstOk;
    end;
  end;
end;

end.
