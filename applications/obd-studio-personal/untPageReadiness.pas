//------------------------------------------------------------------------------
//  untPageReadiness
//
//  Readiness-monitor and inspection-advisor page for OBD-Studio Personal.
//------------------------------------------------------------------------------

unit untPageReadiness;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  ERD.I18n,
  ERD.UI.Toast,
  ERD.UI.Card,
  ERD.UI.Chips,
  ERD.UI.DataGrid,
  ERD.UI.KeyValueList,
  ERD.UI.Paint,
  ERD.UI.ReadinessPanel,
  ERD.Service.DTCs,
  ERD.Service.DriveCycle.Types,
  ERD.Service.Inspection,
  untPersonalPage;

type
  TReadinessSnapshot = record
    SinceClear: TArray<TOBDMonitorReadiness>;
    ThisCycle: TArray<TOBDMonitorReadiness>;
    Status: TOBDMonitorStatus;
    StatusOK: Boolean;
    StatusError: string;
    Inspection: TOBDInspectionResult;
    CountryCodes: TArray<string>;
    Country: string;
    FuelType: TOBDInspectionFuelType;
    DistanceSinceClear: Integer;
    WarmUpsSinceClear: Integer;
  end;

  TfrmPageReadiness = class(TFrame, IPersonalPage)
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FReading: Boolean;
    FLoadingCountries: Boolean;
    FCountryCodes: TArray<string>;
    FLastSnapshot: TReadinessSnapshot;
    pnlTop: TPanel;
    lblTitle: TLabel;
    lblSummary: TLabel;
    lblCountry: TLabel;
    cmbCountry: TComboBox;
    btnRefresh: TButton;
    btnStartAdvisor: TButton;
    btnStopAdvisor: TButton;
    Scroll: TScrollBox;
    ReadinessPanel: TOBDReadinessPanel;
    CycleCard: TOBDCard;
    CycleGrid: TOBDDataGrid;
    RulesCard: TOBDCard;
    RulesList: TOBDKeyValueList;
    AdvisorBanner: TOBDBanner;
    AdvisorCard: TOBDCard;
    AdvisorGrid: TOBDDataGrid;
    procedure BuildUI;
    procedure ConfigureGrids;
    procedure RefreshClicked(Sender: TObject);
    procedure CountryChanged(Sender: TObject);
    procedure StartAdvisorClicked(Sender: TObject);
    procedure StopAdvisorClicked(Sender: TObject);
    procedure AdvisorStep(Sender: TObject; const AStep: TOBDDriveCycleStep;
      AElapsedSec, ATotalSec: Cardinal);
    procedure AdvisorReady(Sender: TObject; AMonitor: TOBDMonitor);
    procedure AdvisorAllReady(Sender: TObject);
    procedure FillCountries(const ACodes: TArray<string>);
    procedure RefreshData;
    procedure ApplySnapshot(const ASnapshot: TReadinessSnapshot);
    procedure FillReadinessPanel(const ASnapshot: TReadinessSnapshot);
    procedure FillCycleGrid(const AReadiness: TArray<TOBDMonitorReadiness>);
    procedure FillInspection(const ASnapshot: TReadinessSnapshot);
    procedure FillAdvisor(const AReadiness: TArray<TOBDMonitorReadiness>);
    procedure StopAdvisor;
    function SelectedCountryCode: string;
    function Connected: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
  end;

implementation

{$R *.dfm}

function PanelMonitor(AMonitor: TOBDMonitor): TOBDReadinessMonitor;
begin
  case AMonitor of
    omMisfire:
      Result := rmMisfire;
    omFuelSystem:
      Result := rmFuelSystem;
    omComprehensive:
      Result := rmComponents;
    omCatalyst:
      Result := rmCatalyst;
    omHeatedCatalyst:
      Result := rmHeatedCatalyst;
    omEvaporativeSystem:
      Result := rmEvaporativeSystem;
    omSecondaryAirSystem:
      Result := rmSecondaryAirSystem;
    omACRefrigerant:
      Result := rmACRefrigerant;
    omOxygenSensor:
      Result := rmOxygenSensor;
    omOxygenSensorHeater:
      Result := rmOxygenSensorHeater;
    omEGRSystem:
      Result := rmEGRSystem;
    omNMHCCatalyst:
      Result := rmNMHCCatalyst;
    omNOxAftertreatment:
      Result := rmNOxSCRAftertreatment;
    omBoostPressureSystem:
      Result := rmBoostPressure;
    omExhaustGasSensor:
      Result := rmExhaustGasSensor;
    omPMFilter:
      Result := rmPMFilter;
  else
    Result := rmEGRVVTSystem;
  end;
end;

function MonitorStateText(const AItem: TOBDMonitorReadiness): string;
begin
  if not AItem.Supported then
    Result := Tr('personal.readiness.unsupported', 'Not supported')
  else if AItem.Complete then
    Result := Tr('personal.readiness.complete', 'Complete')
  else
    Result := Tr('personal.readiness.incomplete', 'Incomplete');
end;

function MonitorStateTone(const AItem: TOBDMonitorReadiness): TOBDStatusKind;
begin
  if not AItem.Supported then
    Result := skNeutral
  else if AItem.Complete then
    Result := skSuccess
  else
    Result := skWarning;
end;

function ReadinessState(const AItem: TOBDMonitorReadiness): TOBDMonitorState;
begin
  if not AItem.Supported then
    Result := msUnsupported
  else if AItem.Complete then
    Result := msComplete
  else
    Result := msIncomplete;
end;

function VerdictText(AVerdict: TOBDInspectionVerdict): string;
begin
  case AVerdict of
    ivPass:
      Result := Tr('personal.readiness.verdict.pass', 'Likely pass');
    ivFail:
      Result := Tr('personal.readiness.verdict.fail', 'Likely fail');
    ivNotApplicable:
      Result := Tr('personal.readiness.verdict.na', 'Not applicable');
  else
    Result := Tr('personal.readiness.verdict.unknown', 'Unknown');
  end;
end;

function VerdictTone(AVerdict: TOBDInspectionVerdict): TOBDStatusKind;
begin
  case AVerdict of
    ivPass:
      Result := skSuccess;
    ivFail:
      Result := skDanger;
    ivNotApplicable:
      Result := skNeutral;
  else
    Result := skWarning;
  end;
end;

function InspectionRegimeForCountry(const ACountry: string): TOBDInspectionRegime;
begin
  if SameText(ACountry, 'NL') then
    Result := irAPK
  else if SameText(ACountry, 'BE') then
    Result := irKeuring
  else if SameText(ACountry, 'FR') then
    Result := irControleTechnique
  else if SameText(ACountry, 'GB') or SameText(ACountry, 'UK') then
    Result := irMOT
  else if SameText(ACountry, 'DE') then
    Result := irHUAU
  else if SameText(ACountry, 'IE') then
    Result := irNCT
  else
    Result := irCustom;
end;

function CountIncomplete(const AReadiness: TArray<TOBDMonitorReadiness>): Integer;
var
  Item: TOBDMonitorReadiness;
begin
  Result := 0;
  for Item in AReadiness do
    if Item.Supported and not Item.Complete then
      Inc(Result);
end;

function CountSupported(const AReadiness: TArray<TOBDMonitorReadiness>): Integer;
var
  Item: TOBDMonitorReadiness;
begin
  Result := 0;
  for Item in AReadiness do
    if Item.Supported then
      Inc(Result);
end;

function MinutesText(ASeconds: Cardinal): string;
begin
  if ASeconds = 0 then
    Result := ''
  else if ASeconds < 60 then
    Result := Format('%d s', [ASeconds])
  else
    Result := Format('%d min', [Ceil(ASeconds / 60)]);
end;

{ TfrmPageReadiness }

constructor TfrmPageReadiness.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

destructor TfrmPageReadiness.Destroy;
begin
  StopAdvisor;
  inherited Destroy;
end;

procedure TfrmPageReadiness.BuildUI;
begin
  Align := alClient;
  ParentBackground := False;

  pnlTop := TPanel.Create(Self);
  pnlTop.Parent := Self;
  pnlTop.Align := alTop;
  pnlTop.Height := 96;
  pnlTop.BevelOuter := bvNone;
  pnlTop.ParentBackground := False;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := pnlTop;
  lblTitle.Left := 16;
  lblTitle.Top := 10;
  lblTitle.Font.Style := [fsBold];
  lblTitle.Font.Size := 15;

  lblSummary := TLabel.Create(Self);
  lblSummary.Parent := pnlTop;
  lblSummary.Left := 16;
  lblSummary.Top := 42;
  lblSummary.Width := 560;
  lblSummary.AutoSize := False;

  lblCountry := TLabel.Create(Self);
  lblCountry.Parent := pnlTop;
  lblCountry.Left := 620;
  lblCountry.Top := 14;

  cmbCountry := TComboBox.Create(Self);
  cmbCountry.Parent := pnlTop;
  cmbCountry.Left := 620;
  cmbCountry.Top := 36;
  cmbCountry.Width := 120;
  cmbCountry.Style := csDropDownList;
  cmbCountry.OnChange := CountryChanged;

  btnRefresh := TButton.Create(Self);
  btnRefresh.Parent := pnlTop;
  btnRefresh.Left := 760;
  btnRefresh.Top := 34;
  btnRefresh.Width := 110;
  btnRefresh.OnClick := RefreshClicked;

  btnStartAdvisor := TButton.Create(Self);
  btnStartAdvisor.Parent := pnlTop;
  btnStartAdvisor.Left := 884;
  btnStartAdvisor.Top := 34;
  btnStartAdvisor.Width := 126;
  btnStartAdvisor.OnClick := StartAdvisorClicked;

  btnStopAdvisor := TButton.Create(Self);
  btnStopAdvisor.Parent := pnlTop;
  btnStopAdvisor.Left := 1022;
  btnStopAdvisor.Top := 34;
  btnStopAdvisor.Width := 112;
  btnStopAdvisor.Enabled := False;
  btnStopAdvisor.OnClick := StopAdvisorClicked;

  Scroll := TScrollBox.Create(Self);
  Scroll.Parent := Self;
  Scroll.Align := alClient;
  Scroll.BorderStyle := bsNone;
  Scroll.VertScrollBar.Tracking := True;

  ReadinessPanel := TOBDReadinessPanel.Create(Self);
  ReadinessPanel.Parent := Scroll;
  ReadinessPanel.Align := alTop;
  ReadinessPanel.Top := 0;
  ReadinessPanel.Height := 376;
  ReadinessPanel.Margins.SetBounds(16, 12, 16, 8);
  ReadinessPanel.AlignWithMargins := True;
  ReadinessPanel.Layout := rlFull;

  CycleCard := TOBDCard.Create(Self);
  CycleCard.Parent := Scroll;
  CycleCard.Align := alTop;
  CycleCard.Top := 392;
  CycleCard.Height := 246;
  CycleCard.Margins.SetBounds(16, 8, 16, 8);
  CycleCard.AlignWithMargins := True;
  CycleCard.Padding.SetBounds(10, 10, 10, 10);

  CycleGrid := TOBDDataGrid.Create(Self);
  CycleGrid.Parent := CycleCard;
  CycleGrid.Align := alClient;
  CycleGrid.BorderVisible := False;

  RulesCard := TOBDCard.Create(Self);
  RulesCard.Parent := Scroll;
  RulesCard.Align := alTop;
  RulesCard.Top := 654;
  RulesCard.Height := 232;
  RulesCard.Margins.SetBounds(16, 8, 16, 8);
  RulesCard.AlignWithMargins := True;
  RulesCard.Padding.SetBounds(10, 10, 10, 10);

  RulesList := TOBDKeyValueList.Create(Self);
  RulesList.Parent := RulesCard;
  RulesList.Align := alClient;
  RulesList.KeyWidth := 210;
  RulesList.WordWrap := True;

  AdvisorBanner := TOBDBanner.Create(Self);
  AdvisorBanner.Parent := Scroll;
  AdvisorBanner.Align := alTop;
  AdvisorBanner.Top := 902;
  AdvisorBanner.Height := 72;
  AdvisorBanner.Margins.SetBounds(16, 8, 16, 8);
  AdvisorBanner.AlignWithMargins := True;
  AdvisorBanner.Kind := bnInfo;

  AdvisorCard := TOBDCard.Create(Self);
  AdvisorCard.Parent := Scroll;
  AdvisorCard.Align := alTop;
  AdvisorCard.Top := 990;
  AdvisorCard.Height := 300;
  AdvisorCard.Margins.SetBounds(16, 8, 16, 16);
  AdvisorCard.AlignWithMargins := True;
  AdvisorCard.Padding.SetBounds(10, 10, 10, 10);

  AdvisorGrid := TOBDDataGrid.Create(Self);
  AdvisorGrid.Parent := AdvisorCard;
  AdvisorGrid.Align := alClient;
  AdvisorGrid.BorderVisible := False;

  ConfigureGrids;
end;

procedure TfrmPageReadiness.ConfigureGrids;
var
  Col: TOBDDataGridColumn;
begin
  CycleGrid.DataColumns.Clear;
  Col := CycleGrid.DataColumns.Add;
  Col.Caption := 'Monitor';
  Col.Width := 220;
  Col := CycleGrid.DataColumns.Add;
  Col.Caption := 'Since clear';
  Col.Width := 120;
  Col.Kind := dckChip;
  Col := CycleGrid.DataColumns.Add;
  Col.Caption := 'This drive';
  Col.Width := 120;
  Col.Kind := dckChip;
  Col := CycleGrid.DataColumns.Add;
  Col.Caption := 'Action';
  Col.Width := 0;
  CycleGrid.EmptyGlyph := glInfo;

  AdvisorGrid.DataColumns.Clear;
  Col := AdvisorGrid.DataColumns.Add;
  Col.Caption := 'Monitor';
  Col.Width := 190;
  Col := AdvisorGrid.DataColumns.Add;
  Col.Caption := '#';
  Col.Width := 48;
  Col.Alignment := taRightJustify;
  Col := AdvisorGrid.DataColumns.Add;
  Col.Caption := 'Step';
  Col.Width := 0;
  Col := AdvisorGrid.DataColumns.Add;
  Col.Caption := 'Time';
  Col.Width := 88;
  AdvisorGrid.EmptyGlyph := glInfo;
end;

function TfrmPageReadiness.PageKey: string;
begin
  Result := PAGE_READINESS;
end;

procedure TfrmPageReadiness.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  Translate;
  if FHost <> nil then
    FillCountries(FHost.Session.Inspection.CountryCodes);
end;

procedure TfrmPageReadiness.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive then
    RefreshData
  else
    StopAdvisor;
end;

procedure TfrmPageReadiness.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
  begin
    StopAdvisor;
    ReadinessPanel.Clear;
    CycleGrid.Clear;
    AdvisorGrid.Clear;
    if FHost <> nil then
      FHost.SetBadge(PAGE_READINESS, 0);
  end
  else if FActive then
    RefreshData;
end;

procedure TfrmPageReadiness.UnitsChanged;
begin
  if FActive then
    RefreshData;
end;

procedure TfrmPageReadiness.Translate;
begin
  lblTitle.Caption := Tr('personal.readiness.title', 'Readiness');
  lblSummary.Caption := Tr('personal.readiness.summary',
    'Emission monitors, inspection rules and a drive-cycle advisor.');
  lblCountry.Caption := Tr('personal.readiness.country', 'Inspection country');
  btnRefresh.Caption := Tr('personal.readiness.refresh', 'Read');
  btnStartAdvisor.Caption := Tr('personal.readiness.startAdvisor', 'Start advisor');
  btnStopAdvisor.Caption := Tr('personal.readiness.stopAdvisor', 'Stop');
  ReadinessPanel.InspectionName := Tr('personal.readiness.inspection',
    'emissions inspection');
  CycleCard.Title := Tr('personal.readiness.driveCycleCard',
    'This drive cycle');
  RulesCard.Title := Tr('personal.readiness.rulesCard', 'Inspection verdict');
  AdvisorBanner.Title := Tr('personal.readiness.advisorTitle',
    'Drive-cycle advisor');
  AdvisorBanner.Text := Tr('personal.readiness.advisorText',
    'Complete the listed drive steps safely and obey traffic laws. Stop the advisor before disconnecting.');
  AdvisorCard.Title := Tr('personal.readiness.advisorSteps',
    'Steps for incomplete monitors');
  CycleGrid.EmptyTitle := Tr('personal.readiness.noReadiness',
    'Read readiness to see monitor status');
  CycleGrid.EmptyHint := Tr('personal.readiness.noCycleHint',
    'Vehicles that do not support PID 41 will show only since-clear data.');
  AdvisorGrid.EmptyTitle := Tr('personal.readiness.noAdvisor',
    'No incomplete supported monitors');
  AdvisorGrid.EmptyHint := Tr('personal.readiness.noAdvisorHint',
    'Readiness is complete or the vehicle did not report monitor details.');
  ConfigureGrids;
end;

function TfrmPageReadiness.Connected: Boolean;
begin
  Result := (FHost <> nil) and FHost.Connected;
end;

procedure TfrmPageReadiness.RefreshClicked(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmPageReadiness.CountryChanged(Sender: TObject);
begin
  if FLoadingCountries or (FHost = nil) then
    Exit;
  FHost.Settings.InspectionCountry := SelectedCountryCode;
  FHost.Session.Inspection.Country := FHost.Settings.InspectionCountry;
  FHost.SettingsChanged;
  if FActive and Connected then
    RefreshData;
end;

procedure TfrmPageReadiness.StartAdvisorClicked(Sender: TObject);
var
  Item: TOBDMonitorReadiness;
  TargetSet: Boolean;
begin
  if not Connected then
    Exit;
  TargetSet := False;
  for Item in FLastSnapshot.SinceClear do
    if Item.Supported and not Item.Complete then
    begin
      FHost.Session.DriveCycle.TargetMonitor := Item.Monitor;
      TargetSet := True;
      Break;
    end;
  if not TargetSet then
  begin
    FHost.Notify(tkInfo, Tr('personal.readiness.advisorReadyTitle',
      'No advisor needed'), Tr('personal.readiness.advisorReadyText',
      'All supported monitors already report complete.'));
    Exit;
  end;
  FHost.Session.DriveCycle.OnStep := AdvisorStep;
  FHost.Session.DriveCycle.OnReady := AdvisorReady;
  FHost.Session.DriveCycle.OnAllReady := AdvisorAllReady;
  FHost.Session.DriveCycle.AutoStop := True;
  FHost.Session.DriveCycle.Start;
  btnStartAdvisor.Enabled := False;
  btnStopAdvisor.Enabled := True;
end;

procedure TfrmPageReadiness.StopAdvisorClicked(Sender: TObject);
begin
  StopAdvisor;
end;

procedure TfrmPageReadiness.StopAdvisor;
begin
  if (FHost <> nil) and (FHost.Session <> nil) then
  begin
    FHost.Session.DriveCycle.Stop;
    FHost.Session.DriveCycle.OnStep := nil;
    FHost.Session.DriveCycle.OnReady := nil;
    FHost.Session.DriveCycle.OnAllReady := nil;
  end;
  if btnStartAdvisor <> nil then
    btnStartAdvisor.Enabled := Connected;
  if btnStopAdvisor <> nil then
    btnStopAdvisor.Enabled := False;
end;

procedure TfrmPageReadiness.AdvisorStep(Sender: TObject;
  const AStep: TOBDDriveCycleStep; AElapsedSec, ATotalSec: Cardinal);
begin
  AdvisorBanner.Kind := bnInfo;
  AdvisorBanner.Title := TrFmt('personal.readiness.stepTitle',
    'Drive-cycle step %d', [AStep.Index]);
  AdvisorBanner.Text := AStep.Description;
end;

procedure TfrmPageReadiness.AdvisorReady(Sender: TObject; AMonitor: TOBDMonitor);
begin
  FHost.Notify(tkSuccess, Tr('personal.readiness.monitorReadyTitle',
    'Monitor complete'), TrFmt('personal.readiness.monitorReadyText',
    '%s has completed.', [MonitorName(AMonitor)]));
  RefreshData;
end;

procedure TfrmPageReadiness.AdvisorAllReady(Sender: TObject);
begin
  AdvisorBanner.Kind := bnSuccess;
  AdvisorBanner.Title := Tr('personal.readiness.allReadyTitle',
    'All targeted monitors complete');
  AdvisorBanner.Text := Tr('personal.readiness.allReadyText',
    'The vehicle now reports the advised monitor complete.');
  StopAdvisor;
  RefreshData;
end;

procedure TfrmPageReadiness.FillCountries(const ACodes: TArray<string>);
var
  I, Selected: Integer;
  Current: string;
begin
  SetLength(FCountryCodes, Length(ACodes));
  Current := '';
  if FHost <> nil then
    Current := FHost.Settings.InspectionCountry;
  FLoadingCountries := True;
  try
    cmbCountry.Items.BeginUpdate;
    try
      cmbCountry.Items.Clear;
      Selected := -1;
      for I := 0 to High(ACodes) do
      begin
        FCountryCodes[I] := UpperCase(ACodes[I]);
        cmbCountry.Items.Add(FCountryCodes[I]);
        if SameText(FCountryCodes[I], Current) then
          Selected := I;
      end;
      cmbCountry.ItemIndex := Selected;
    finally
      cmbCountry.Items.EndUpdate;
    end;
  finally
    FLoadingCountries := False;
  end;
end;

function TfrmPageReadiness.SelectedCountryCode: string;
begin
  if (cmbCountry.ItemIndex >= 0) and (cmbCountry.ItemIndex <= High(FCountryCodes)) then
    Result := FCountryCodes[cmbCountry.ItemIndex]
  else if FHost <> nil then
    Result := FHost.Settings.InspectionCountry
  else
    Result := '';
end;

procedure TfrmPageReadiness.RefreshData;
var
  Snapshot: TReadinessSnapshot;
begin
  if FReading or (FHost = nil) then
    Exit;
  btnRefresh.Enabled := False;
  btnStartAdvisor.Enabled := False;
  FReading := True;
  if not Connected then
  begin
    FReading := False;
    btnRefresh.Enabled := True;
    Exit;
  end;

  Snapshot := Default(TReadinessSnapshot);
  Snapshot.Country := FHost.Settings.InspectionCountry;
  FHost.Session.RunJob(
    procedure
    var
      Value: Double;
    begin
      Snapshot.CountryCodes := FHost.Session.Inspection.CountryCodes;
      FHost.Session.Inspection.Country := Snapshot.Country;
      Snapshot.SinceClear := FHost.Session.DriveCycle.ReadReadiness;
      Snapshot.ThisCycle := FHost.Session.DriveCycle.ReadReadinessThisCycle;
      Snapshot.StatusOK := FHost.Session.DriveCycle.ReadMonitorStatus($01,
        Snapshot.Status, Snapshot.StatusError);
      if Snapshot.StatusOK and (Length(Snapshot.Status.Monitors) > 0) then
        Snapshot.SinceClear := Snapshot.Status.Monitors;
      if Snapshot.Status.CompressionIgnition then
        Snapshot.FuelType := iftDiesel
      else
        Snapshot.FuelType := iftPetrol;
      Snapshot.DistanceSinceClear := -1;
      Snapshot.WarmUpsSinceClear := -1;
      try
        Value := FHost.Session.LiveData.Read($31).Value;
        if not IsNan(Value) then
          Snapshot.DistanceSinceClear := Round(Value);
      except
      end;
      try
        Value := FHost.Session.LiveData.Read($30).Value;
        if not IsNan(Value) then
          Snapshot.WarmUpsSinceClear := Round(Value);
      except
      end;
      Snapshot.Inspection := FHost.Session.Inspection.Evaluate(
        Snapshot.SinceClear, Snapshot.Status.MILOn, Snapshot.Status.DTCCount,
        Snapshot.FuelType, 0);
    end,
    procedure(const AError: string)
    begin
      FReading := False;
      btnRefresh.Enabled := True;
      btnStartAdvisor.Enabled := Connected;
      if AError <> '' then
      begin
        FHost.Notify(tkDanger, Tr('personal.readiness.readFailed',
          'Readiness read failed'), AError);
        Exit;
      end;
      FillCountries(Snapshot.CountryCodes);
      ApplySnapshot(Snapshot);
    end);
end;

procedure TfrmPageReadiness.ApplySnapshot(const ASnapshot: TReadinessSnapshot);
var
  Incomplete: Integer;
begin
  FLastSnapshot := ASnapshot;
  Incomplete := CountIncomplete(ASnapshot.SinceClear);
  FillReadinessPanel(ASnapshot);
  FillCycleGrid(ASnapshot.SinceClear);
  FillInspection(ASnapshot);
  FillAdvisor(ASnapshot.SinceClear);
  lblSummary.Caption := TrFmt('personal.readiness.summaryCounts',
    '%d of %d supported monitors incomplete. MIL: %s.',
    [Incomplete, CountSupported(ASnapshot.SinceClear),
    IfThen(ASnapshot.Status.MILOn, Tr('personal.common.yes', 'yes'),
    Tr('personal.common.no', 'no'))]);
  if FHost <> nil then
    FHost.SetBadge(PAGE_READINESS, Incomplete);
end;

procedure TfrmPageReadiness.FillReadinessPanel(
  const ASnapshot: TReadinessSnapshot);
var
  Item: TOBDMonitorReadiness;
begin
  ReadinessPanel.BeginUpdate;
  try
    ReadinessPanel.Clear;
    ReadinessPanel.MilOn := ASnapshot.Status.MILOn;
    ReadinessPanel.DtcCount := ASnapshot.Status.DTCCount;
    ReadinessPanel.CompressionIgnition := ASnapshot.Status.CompressionIgnition;
    ReadinessPanel.DistanceSinceClear := ASnapshot.DistanceSinceClear;
    ReadinessPanel.WarmUpsSinceClear := ASnapshot.WarmUpsSinceClear;
    ReadinessPanel.AllowedIncomplete := Max(0,
      ASnapshot.Inspection.AllowedIncompleteMonitors);
    ReadinessPanel.InspectionRegime := InspectionRegimeForCountry(ASnapshot.Country);
    ReadinessPanel.InspectionName := ASnapshot.Inspection.InspectionName;
    ReadinessPanel.UpdatedText := FormatDateTime('yyyy-mm-dd hh:nn', Now);
    for Item in ASnapshot.SinceClear do
      ReadinessPanel.SetMonitor(PanelMonitor(Item.Monitor), ReadinessState(Item),
        IfThen(Item.Supported and not Item.Complete,
        Tr('personal.readiness.monitorNote',
        'Drive until this monitor has completed.'), ''));
  finally
    ReadinessPanel.EndUpdate;
  end;
end;

procedure TfrmPageReadiness.FillCycleGrid(
  const AReadiness: TArray<TOBDMonitorReadiness>);
var
  I, Row: Integer;
  SinceItem, CycleItem: TOBDMonitorReadiness;
  CycleText: string;
begin
  CycleGrid.BeginUpdate;
  try
    CycleGrid.Clear;
    if Length(AReadiness) = 0 then
      Exit;
    for I := 0 to High(AReadiness) do
    begin
      SinceItem := AReadiness[I];
      if I <= High(FLastSnapshot.ThisCycle) then
      begin
        CycleItem := FLastSnapshot.ThisCycle[I];
        CycleText := MonitorStateText(CycleItem);
      end
      else
      begin
        CycleItem := SinceItem;
        CycleItem.Supported := False;
        CycleText := Tr('personal.readiness.pid41Unsupported', 'Not reported');
      end;
      Row := CycleGrid.AddRow([MonitorName(SinceItem.Monitor),
        MonitorStateText(SinceItem), CycleText,
        IfThen(SinceItem.Supported and not SinceItem.Complete,
        Tr('personal.readiness.driveAction', 'Complete drive cycle'), '')]);
      CycleGrid.CellTone[1, Row] := MonitorStateTone(SinceItem);
      CycleGrid.CellTone[2, Row] := MonitorStateTone(CycleItem);
    end;
  finally
    CycleGrid.EndUpdate;
  end;
end;

procedure TfrmPageReadiness.FillInspection(const ASnapshot: TReadinessSnapshot);
var
  Reason: TOBDInspectionReason;
  Sources: string;
  Source: string;
  Item: TOBDKeyValueItem;
begin
  Sources := '';
  for Source in ASnapshot.Inspection.Sources do
  begin
    if Sources <> '' then
      Sources := Sources + ', ';
    Sources := Sources + Source;
  end;

  RulesList.BeginUpdate;
  try
    RulesList.Clear;
    Item := RulesList.Add(Tr('personal.readiness.rules', 'Rules'),
      Tr('personal.readiness.verdict', 'Verdict'),
      VerdictText(ASnapshot.Inspection.Verdict), kvkChip);
    Item.Tone := VerdictTone(ASnapshot.Inspection.Verdict);
    RulesList.Add(Tr('personal.readiness.rules', 'Rules'),
      Tr('personal.readiness.program', 'Programme'),
      ASnapshot.Inspection.InspectionName);
    RulesList.Add(Tr('personal.readiness.rules', 'Rules'),
      Tr('personal.readiness.countryCode', 'Country'),
      ASnapshot.Inspection.CountryCode);
    RulesList.Add(Tr('personal.readiness.rules', 'Rules'),
      Tr('personal.readiness.allowance', 'Allowed incomplete'),
      IntToStr(ASnapshot.Inspection.AllowedIncompleteMonitors));
    RulesList.Add(Tr('personal.readiness.rules', 'Rules'),
      Tr('personal.readiness.incompleteCount', 'Incomplete monitors'),
      IntToStr(ASnapshot.Inspection.IncompleteMonitorCount));
    RulesList.Add(Tr('personal.readiness.rules', 'Rules'),
      Tr('personal.readiness.verified', 'Verified'),
      IfThen(ASnapshot.Inspection.Verified, Tr('personal.common.yes', 'yes'),
      Tr('personal.common.no', 'no')));
    for Reason in ASnapshot.Inspection.Reasons do
      RulesList.Add(Tr('personal.readiness.reasons', 'Reasons'),
        InspectionReasonKindName(Reason.Kind), Reason.Text);
    if Sources <> '' then
      RulesList.Add(Tr('personal.readiness.sources', 'Sources'),
        Tr('personal.readiness.source', 'Source'), Sources);
  finally
    RulesList.EndUpdate;
  end;
end;

procedure TfrmPageReadiness.FillAdvisor(
  const AReadiness: TArray<TOBDMonitorReadiness>);
var
  Item: TOBDMonitorReadiness;
  Cycle: TOBDDriveCycle;
  Step: TOBDDriveCycleStep;
begin
  AdvisorGrid.BeginUpdate;
  try
    AdvisorGrid.Clear;
    for Item in AReadiness do
      if Item.Supported and not Item.Complete then
      begin
        Cycle := FHost.Session.DriveCycle.GetCycle(Item.Monitor);
        if Length(Cycle.Steps) = 0 then
          AdvisorGrid.AddRow([MonitorName(Item.Monitor), '-',
            Tr('personal.readiness.noCycle',
            'No specific drive cycle is catalogued; follow the manufacturer procedure.'), ''])
        else
          for Step in Cycle.Steps do
            AdvisorGrid.AddRow([MonitorName(Item.Monitor), IntToStr(Step.Index),
              Step.Description, MinutesText(Step.DurationSec)]);
      end;
  finally
    AdvisorGrid.EndUpdate;
  end;
end;

end.
