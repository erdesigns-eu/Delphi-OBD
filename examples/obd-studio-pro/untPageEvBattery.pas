//------------------------------------------------------------------------------
//  untPageEvBattery
//
//  OBD Studio Pro EV battery page for a 2017 BMW i3 94 Ah pack, using
//  TOBDCellMatrix for the 96 cells, TOBDValueTile summary cards,
//  TOBDKeyValueList, TOBDPreconditionList and TOBDTrendChart.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageEvBattery;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.Card, ERD.UI.Buttons, ERD.UI.ValueTile, ERD.UI.TrendChart,
  ERD.UI.CellMatrix, ERD.UI.KeyValueList, ERD.UI.PreconditionList,
  untProPage, untSimulator;

type
  TfrmEvBattery = class(TFrame, IProPage)
    pnlStatus: TPanel;
    cardStatus: TOBDCard;
    lblBatteryTitle: TLabel;
    lblBatteryState: TLabel;
    lblVehicleTitle: TLabel;
    lblVehicle: TLabel;
    lblPackTitle: TLabel;
    lblPack: TLabel;
    lblMileageTitle: TLabel;
    lblMileage: TLabel;
    lblControlTitle: TLabel;
    lblControl: TLabel;
    btnExportReport: TOBDButton;
    btnBalanceCheck: TOBDButton;
    pnlTiles: TPanel;
    tileSoH: TOBDValueTile;
    tileSoC: TOBDValueTile;
    tilePackVoltage: TOBDValueTile;
    tileCurrent: TOBDValueTile;
    tileCellSpread: TOBDValueTile;
    tileTemperature: TOBDValueTile;
    pnlBody: TPanel;
    cellMatrix: TOBDCellMatrix;
    pnlRight: TPanel;
    kvSummary: TOBDKeyValueList;
    preBattery: TOBDPreconditionList;
    chtBattery: TOBDTrendChart;
    procedure btnExportReportClick(Sender: TObject);
    procedure btnBalanceCheckClick(Sender: TObject);
  private
    FHost: IProHost;
    FPhase: Double;
    FTestRunning: Boolean;
    FTestTicks: Integer;
    FCellMin: Double;
    FCellMax: Double;
    FCellAvg: Double;
    FMinIndex: Integer;
    FMaxIndex: Integer;
    FPackVoltage: Double;
    FPackCurrent: Double;
    procedure ConfigureData;
    procedure ConfigureSummary;
    procedure ConfigurePreconditions;
    procedure UpdateCells;
    procedure UpdateTiles;
    procedure UpdateSummary;
    procedure SetSummaryValue(const AKey, AValue: string; ATone: TOBDStatusKind);
    function BaseCellVoltage(AIndex: Integer): Double;
    function ModuleTemperature(AModule: Integer): Double;
    function CellName(AIndex: Integer): string;
    procedure StartBalanceCheck;
    procedure FinishBalanceCheck;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

function TfrmEvBattery.BaseCellVoltage(AIndex: Integer): Double;
begin
  Result := 3.752 + 0.012 * Sin(AIndex * 0.7) + 0.006 * Cos(AIndex * 1.9);
  if AIndex = 76 then
    Result := 3.712
  else if AIndex = 40 then
    Result := 3.791;
end;

function TfrmEvBattery.ModuleTemperature(AModule: Integer): Double;
const
  TEMPS: array [0 .. 7] of Double = (23, 24, 24, 25, 26, 25, 24, 31);
begin
  if (AModule >= Low(TEMPS)) and (AModule <= High(TEMPS)) then
    Result := TEMPS[AModule]
  else
    Result := 25;
  Result := Result + 0.4 * Sin(FPhase + AModule * 0.8);
end;

function TfrmEvBattery.CellName(AIndex: Integer): string;
begin
  Result := Format('cell %.0f', [AIndex + 1.0]);
end;

procedure TfrmEvBattery.ConfigureData;
begin
  chtBattery.Channels.Clear;
  chtBattery.AddChannel('Pack voltage', 0, 340, 380, 'V');
  chtBattery.AddChannel('Current', 0, -80, 80, 'A');
  UpdateCells;
  UpdateTiles;
end;

procedure TfrmEvBattery.ConfigureSummary;
var
  Item: TOBDKeyValueItem;
begin
  kvSummary.Clear;
  kvSummary.Add('Cell summary', 'Mean cell voltage', '');
  Item := kvSummary.Add('Cell summary', 'Spread (max - min)', '', kvkTone);
  Item.Tone := skWarning;
  kvSummary.Add('Cell summary', 'Highest', '');
  Item := kvSummary.Add('Cell summary', 'Lowest', '', kvkTone);
  Item.Tone := skWarning;
  Item := kvSummary.Add('Cell summary', 'Balancing',
    'off ' + #183 + ' last 2 days ago', kvkChip);
  Item.Tone := skNeutral;
  kvSummary.Add('Cell summary', 'Battery', 'BMW i3 94 Ah ' + #183 +
    ' 96 cells');
  UpdateSummary;
end;

procedure TfrmEvBattery.ConfigurePreconditions;
begin
  preBattery.Clear;
  preBattery.AddPrecondition(pcnMet, 'Vehicle in READY / parked', 'P ' + #183 + ' handbrake');
  preBattery.AddPrecondition(pcnMet, 'High-voltage system', 'No faults');
  preBattery.AddPrecondition(pcnMet, '12 V battery >= 12.5 V', '13.9 V (DC/DC)');
  preBattery.AddPrecondition(pcnMet, 'Battery temperature 15-35 ' + #176'C',
    '23-31 ' + #176'C');
  preBattery.AddPrecondition(pcnNotRequired, 'Charging cable', 'Not required');
end;

procedure TfrmEvBattery.SetSummaryValue(const AKey, AValue: string;
  ATone: TOBDStatusKind);
var
  I: Integer;
begin
  kvSummary.SetValue(AKey, AValue);
  for I := 0 to kvSummary.Items.Count - 1 do
    if SameText(kvSummary.Items[I].Key, AKey) then
    begin
      kvSummary.Items[I].Tone := ATone;
      Break;
    end;
end;

procedure TfrmEvBattery.UpdateCells;
var
  M, C, I: Integer;
  Values: array [0 .. 11] of Double;
  V, Sum: Double;
begin
  FCellMin := 99;
  FCellMax := 0;
  FMinIndex := 0;
  FMaxIndex := 0;
  Sum := 0;
  for M := 0 to 7 do
  begin
    for C := 0 to 11 do
    begin
      I := M * 12 + C;
      V := BaseCellVoltage(I) + 0.0015 * Sin(FPhase + I * 0.37);
      Values[C] := V;
      Sum := Sum + V;
      if V < FCellMin then
      begin
        FCellMin := V;
        FMinIndex := I;
      end;
      if V > FCellMax then
      begin
        FCellMax := V;
        FMaxIndex := I;
      end;
    end;
    cellMatrix.SetModule(M, Values, ModuleTemperature(M));
  end;
  FCellAvg := Sum / 96;
  FPackVoltage := Sum;
  FPackCurrent := -2.4 + 1.6 * Sin(FPhase * 0.6);
end;

procedure TfrmEvBattery.UpdateTiles;
begin
  tileSoH.Value := 91;
  tileSoC.Value := 64 + 0.3 * Sin(FPhase * 0.25);
  tilePackVoltage.Value := FPackVoltage;
  tileCurrent.Value := FPackCurrent;
  tileCellSpread.Value := (FCellMax - FCellMin) * 1000;
  tileTemperature.Value := ModuleTemperature(7);
  chtBattery.AddSample(0, FPackVoltage);
  chtBattery.AddSample(1, FPackCurrent);
end;

procedure TfrmEvBattery.UpdateSummary;
var
  SpreadMv: Double;
begin
  SpreadMv := (FCellMax - FCellMin) * 1000;
  SetSummaryValue('Mean cell voltage', Format('%.3f V', [FCellAvg]),
    skNeutral);
  SetSummaryValue('Spread (max - min)', Format('%.0f mV', [SpreadMv]),
    skWarning);
  SetSummaryValue('Highest', Format('%s ' + #183 + ' %.3f V',
    [CellName(FMaxIndex), FCellMax]), skNeutral);
  SetSummaryValue('Lowest', Format('%s ' + #183 + ' %.3f V',
    [CellName(FMinIndex), FCellMin]), skWarning);
  if FTestRunning then
    SetSummaryValue('Balancing', 'checking ' + #183 + ' module load active',
      skWarning)
  else
    SetSummaryValue('Balancing', 'off ' + #183 + ' last 2 days ago',
      skNeutral);
end;

procedure TfrmEvBattery.StartBalanceCheck;
begin
  if not (Assigned(FHost) and FHost.Connected) then
  begin
    if Assigned(FHost) then
      FHost.Notify(tkWarning, 'Battery test', 'Connect before starting the balance check.');
    Exit;
  end;
  FTestRunning := True;
  FTestTicks := 0;
  btnBalanceCheck.Enabled := False;
  FHost.Log('HV battery balance check started for BMW i3 94 Ah pack.');
  FHost.SetProgress('Battery balance check', 0);
end;

procedure TfrmEvBattery.FinishBalanceCheck;
begin
  if not FTestRunning then
    Exit;
  FTestRunning := False;
  btnBalanceCheck.Enabled := Assigned(FHost) and FHost.Connected;
  if Assigned(FHost) then
  begin
    FHost.SetProgress('', -1);
    FHost.Notify(tkWarning, 'Cell 77 is low',
      Format('%.3f V, %.0f mV spread. Balance recommended.',
      [FCellMin, (FCellMax - FCellMin) * 1000]));
    FHost.LogWarning(Format('Balance check found weak cell 77 at %.3f V.',
      [FCellMin]));
  end;
  UpdateSummary;
end;

procedure TfrmEvBattery.btnExportReportClick(Sender: TObject);
begin
  if Assigned(FHost) then
  begin
    FHost.Notify(tkInfo, 'HV battery report', 'Report data copied to the session log.');
    FHost.Log(Format('BMW i3 battery report: SoH %.0f %%, SoC %.0f %%, spread %.0f mV.',
      [tileSoH.Value, tileSoC.Value, tileCellSpread.Value]));
  end;
end;

procedure TfrmEvBattery.btnBalanceCheckClick(Sender: TObject);
begin
  StartBalanceCheck;
end;

procedure TfrmEvBattery.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FPhase := 0;
  ConfigureData;
  ConfigureSummary;
  ConfigurePreconditions;
  ConnectionChanged;
end;

procedure TfrmEvBattery.Tick;
begin
  FPhase := FPhase + 0.1;
  UpdateCells;
  UpdateTiles;
  UpdateSummary;
  if FTestRunning then
  begin
    Inc(FTestTicks);
    if Assigned(FHost) then
      FHost.SetProgress('Battery balance check', EnsureRange(FTestTicks * 4, 0, 100));
    if FTestTicks >= 25 then
      FinishBalanceCheck;
  end;
end;

procedure TfrmEvBattery.ConnectionChanged;
begin
  if FTestRunning and not (Assigned(FHost) and FHost.Connected) then
    FinishBalanceCheck;
  btnBalanceCheck.Enabled := Assigned(FHost) and FHost.Connected and not FTestRunning;
  if Assigned(FHost) and FHost.Connected then
    lblBatteryState.Caption := 'READY ' + #183 + ' NOT CHARGING'
  else
    lblBatteryState.Caption := 'OFFLINE SAMPLE';
end;

end.
