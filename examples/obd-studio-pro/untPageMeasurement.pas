//------------------------------------------------------------------------------
//  untPageMeasurement
//
//  OBD Studio Pro measurement page: an A2L-style signal browser, live
//  measurement grid, trend chart and calibration map for the sample
//  2016 VW Golf VII 1.6 TDI with a sticking EGR valve.
//
//  Components used: TOBDSignalBrowser, TOBDSignalGrid, TOBDTrendChart
//  and TOBDMapGrid.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageMeasurement;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.UI.SignalBrowser,
  ERD.UI.SignalGrid,
  ERD.UI.TrendChart,
  ERD.UI.MapGrid,
  untProPage,
  untSimulator;

type
  TfrmMeasurement = class(TFrame, IProPage)
    pnlBrowser: TPanel;
    sigBrowser: TOBDSignalBrowser;
    pnlRight: TPanel;
    sigGrid: TOBDSignalGrid;
    pnlLower: TPanel;
    mapInjection: TOBDMapGrid;
    chtSignals: TOBDTrendChart;
    procedure sigBrowserCheckChange(Sender: TObject; AIndex: Integer;
      AChecked: Boolean);
  private
    FHost: IProHost;
    FTick: Integer;
    procedure AddBrowserSignals;
    procedure ConfigureMap;
    procedure RebuildSelection;
    procedure PushSelectedValues;
    procedure PushMapPoint;
    function SignalValue(AIndex: Integer): Double;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

type
  TMeasurementSignalDef = record
    Group: string;
    Name: string;
    UnitText: string;
    Address: string;
    DataType: string;
    PID: Byte;
    MinValue: Double;
    MaxValue: Double;
    Decimals: Byte;
    DefaultSelected: Boolean;
  end;

const
  MEASUREMENT_SIGNALS: array [0 .. 13] of TMeasurementSignalDef = (
    (Group: 'Air system'; Name: 'Engine speed'; UnitText: 'rpm';
      Address: '0x1000'; DataType: 'U16'; PID: PID_ENGINE_SPEED;
      MinValue: 0; MaxValue: 5000; Decimals: 0; DefaultSelected: True),
    (Group: 'Air system'; Name: 'Mass air flow'; UnitText: 'g/s';
      Address: '0x1010'; DataType: 'U16'; PID: PID_MAF;
      MinValue: 0; MaxValue: 120; Decimals: 1; DefaultSelected: True),
    (Group: 'Air system'; Name: 'Intake manifold pressure'; UnitText: 'kPa';
      Address: '0x1014'; DataType: 'U8'; PID: PID_MAP;
      MinValue: 80; MaxValue: 260; Decimals: 0; DefaultSelected: True),
    (Group: 'EGR diagnostics'; Name: 'Commanded EGR'; UnitText: '%';
      Address: '0x1040'; DataType: 'U8'; PID: PID_COMMANDED_EGR;
      MinValue: 0; MaxValue: 100; Decimals: 1; DefaultSelected: True),
    (Group: 'EGR diagnostics'; Name: 'EGR error'; UnitText: '%';
      Address: '0x1042'; DataType: 'S8'; PID: PID_EGR_ERROR;
      MinValue: -50; MaxValue: 50; Decimals: 1; DefaultSelected: True),
    (Group: 'Exhaust'; Name: 'DPF differential pressure'; UnitText: 'kPa';
      Address: '0x1060'; DataType: 'U8'; PID: PID_DPF_DP;
      MinValue: 0; MaxValue: 30; Decimals: 1; DefaultSelected: False),
    (Group: 'Thermal model'; Name: 'Coolant temperature'; UnitText: #176'C';
      Address: '0x1080'; DataType: 'S8'; PID: PID_COOLANT;
      MinValue: 40; MaxValue: 130; Decimals: 0; DefaultSelected: True),
    (Group: 'Thermal model'; Name: 'Engine oil temperature'; UnitText: #176'C';
      Address: '0x1082'; DataType: 'S8'; PID: PID_OIL_TEMP;
      MinValue: 40; MaxValue: 140; Decimals: 0; DefaultSelected: False),
    (Group: 'Thermal model'; Name: 'Intake air temperature'; UnitText: #176'C';
      Address: '0x1084'; DataType: 'S8'; PID: PID_INTAKE_TEMP;
      MinValue: -20; MaxValue: 80; Decimals: 0; DefaultSelected: False),
    (Group: 'Driver demand'; Name: 'Calculated load'; UnitText: '%';
      Address: '0x10A0'; DataType: 'U8'; PID: PID_ENGINE_LOAD;
      MinValue: 0; MaxValue: 100; Decimals: 1; DefaultSelected: False),
    (Group: 'Driver demand'; Name: 'Vehicle speed'; UnitText: 'km/h';
      Address: '0x10A2'; DataType: 'U8'; PID: PID_VEHICLE_SPEED;
      MinValue: 0; MaxValue: 160; Decimals: 0; DefaultSelected: False),
    (Group: 'Driver demand'; Name: 'Accelerator position'; UnitText: '%';
      Address: '0x10A4'; DataType: 'U8'; PID: PID_THROTTLE;
      MinValue: 0; MaxValue: 100; Decimals: 0; DefaultSelected: False),
    (Group: 'Electrical'; Name: 'Control module voltage'; UnitText: 'V';
      Address: '0x10C0'; DataType: 'U16'; PID: PID_MODULE_VOLTAGE;
      MinValue: 10; MaxValue: 16; Decimals: 1; DefaultSelected: False),
    (Group: 'Fuel system'; Name: 'Fuel level'; UnitText: '%';
      Address: '0x10E0'; DataType: 'U8'; PID: PID_FUEL_LEVEL;
      MinValue: 0; MaxValue: 100; Decimals: 0; DefaultSelected: False)
  );

  MAP_X_AXIS: array [0 .. 7] of Double = (800, 1200, 1600, 2000, 2400, 2800,
    3400, 4200);
  MAP_Y_AXIS: array [0 .. 7] of Double = (0, 15, 30, 45, 60, 75, 90, 100);

{ TfrmMeasurement }

procedure TfrmMeasurement.AddBrowserSignals;
var
  I: Integer;
begin
  sigBrowser.Clear;
  for I := Low(MEASUREMENT_SIGNALS) to High(MEASUREMENT_SIGNALS) do
    sigBrowser.AddSignal(MEASUREMENT_SIGNALS[I].Group,
      MEASUREMENT_SIGNALS[I].Name, MEASUREMENT_SIGNALS[I].UnitText,
      MEASUREMENT_SIGNALS[I].Address, MEASUREMENT_SIGNALS[I].DataType,
      MEASUREMENT_SIGNALS[I].DefaultSelected);
end;

procedure TfrmMeasurement.ConfigureMap;
var
  Row, Col: Integer;
  Base: Double;
begin
  mapInjection.SetAxes(MAP_X_AXIS, MAP_Y_AXIS);
  for Row := 0 to mapInjection.RowCount - 1 do
    for Col := 0 to mapInjection.ColCount - 1 do
    begin
      Base := 4.0 + Row * 5.8 + Col * 1.15 + Row * Col * 0.28;
      mapInjection.Values[Row, Col] := Base;
    end;
  PushMapPoint;
end;

procedure TfrmMeasurement.ConnectionChanged;
begin
  if FHost = nil then
    Exit;
  if FHost.Connected then
  begin
    sigGrid.ResetMinMax;
    chtSignals.ClearSamples;
    FHost.Notify(tkSuccess, 'Measurement running',
      'XCP DAQ samples are coming from 01 Engine electronics.');
    FHost.Log('Measurement page: DAQ RUNNING on EDC17C64, 10 ms group active');
  end
  else
  begin
    FHost.LogWarning('Measurement page: DAQ stopped because the adapter disconnected');
  end;
end;

procedure TfrmMeasurement.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FTick := 0;
  AddBrowserSignals;
  ConfigureMap;
  RebuildSelection;
end;

procedure TfrmMeasurement.PushMapPoint;
var
  Load, Rpm: Double;
begin
  Rpm := SignalValue(0);
  Load := SignalValue(9);
  if IsNan(Rpm) or IsNan(Load) then
    mapInjection.ClearOperatingPoint
  else
    mapInjection.SetOperatingPoint(Rpm, Load);
end;

procedure TfrmMeasurement.PushSelectedValues;
var
  I, GridIndex, ChartIndex: Integer;
  Value: Double;
begin
  GridIndex := 0;
  ChartIndex := 0;
  for I := Low(MEASUREMENT_SIGNALS) to High(MEASUREMENT_SIGNALS) do
    if sigBrowser.Signals[I].Checked then
    begin
      Value := SignalValue(I);
      sigGrid.PushValue(GridIndex, Value);
      if ChartIndex < chtSignals.Channels.Count then
      begin
        chtSignals.Channels[ChartIndex].Decimals :=
          MEASUREMENT_SIGNALS[I].Decimals;
        chtSignals.AddSample(ChartIndex, Value);
        Inc(ChartIndex);
      end;
      Inc(GridIndex);
    end;
  PushMapPoint;
end;

procedure TfrmMeasurement.RebuildSelection;
var
  I, ChartCount: Integer;
  Channel: TOBDTrendChannel;
begin
  sigGrid.Clear;
  chtSignals.Channels.Clear;
  ChartCount := 0;
  for I := Low(MEASUREMENT_SIGNALS) to High(MEASUREMENT_SIGNALS) do
    if sigBrowser.Signals[I].Checked then
    begin
      sigGrid.AddSignal(MEASUREMENT_SIGNALS[I].Name,
        MEASUREMENT_SIGNALS[I].UnitText);
      if ChartCount < OBD_TREND_MAX_CHANNELS then
      begin
        Channel := chtSignals.AddChannel(MEASUREMENT_SIGNALS[I].Name,
          MEASUREMENT_SIGNALS[I].PID, MEASUREMENT_SIGNALS[I].MinValue,
          MEASUREMENT_SIGNALS[I].MaxValue, MEASUREMENT_SIGNALS[I].UnitText);
        Channel.Decimals := MEASUREMENT_SIGNALS[I].Decimals;
        Inc(ChartCount);
      end;
    end;
  PushSelectedValues;
end;

function TfrmMeasurement.SignalValue(AIndex: Integer): Double;
var
  Sim: TEngineSimulator;
  T: Double;
begin
  if (AIndex < Low(MEASUREMENT_SIGNALS)) or
    (AIndex > High(MEASUREMENT_SIGNALS)) then
    Exit(NaN);

  if (FHost <> nil) and FHost.Connected then
  begin
    Sim := FHost.Simulator;
    if (Sim <> nil) and Sim.Has(MEASUREMENT_SIGNALS[AIndex].PID) then
      Exit(Sim.Value(MEASUREMENT_SIGNALS[AIndex].PID));
  end;

  T := FTick / 10.0;
  case MEASUREMENT_SIGNALS[AIndex].PID of
    PID_ENGINE_SPEED:
      Result := 820 + 260 * Sin(T * 0.9);
    PID_MAF:
      Result := 18 + 4 * Sin(T * 0.7);
    PID_MAP:
      Result := 105 + 12 * Sin(T * 0.8);
    PID_COMMANDED_EGR:
      Result := 34 + 7 * Sin(T * 0.45);
    PID_EGR_ERROR:
      Result := -24 + 3 * Sin(T * 0.8);
    PID_DPF_DP:
      Result := 2.2 + 0.4 * Sin(T * 0.6);
    PID_COOLANT:
      Result := 82 + 2 * Sin(T * 0.2);
    PID_OIL_TEMP:
      Result := 78 + 2 * Sin(T * 0.2);
    PID_INTAKE_TEMP:
      Result := 29 + 2 * Sin(T * 0.5);
    PID_ENGINE_LOAD:
      Result := 31 + 7 * Sin(T * 0.6);
    PID_VEHICLE_SPEED:
      Result := 42 + 9 * Sin(T * 0.3);
    PID_THROTTLE:
      Result := 18 + 4 * Sin(T * 0.7);
    PID_MODULE_VOLTAGE:
      Result := 14.1 + 0.1 * Sin(T * 1.1);
  else
    Result := 58;
  end;
end;

procedure TfrmMeasurement.sigBrowserCheckChange(Sender: TObject;
  AIndex: Integer; AChecked: Boolean);
begin
  sigBrowser.ItemIndex := AIndex;
  RebuildSelection;
  if FHost <> nil then
    if AChecked then
      FHost.Log(Format('Measurement page: %s added to DAQ list',
        [MEASUREMENT_SIGNALS[AIndex].Name]))
    else
      FHost.Log(Format('Measurement page: %s removed from DAQ list',
        [MEASUREMENT_SIGNALS[AIndex].Name]));
end;

procedure TfrmMeasurement.Tick;
begin
  Inc(FTick);
  PushSelectedValues;
end;

end.
