//------------------------------------------------------------------------------
//  untPageDashboard
//
//  Configurable live dashboard page for OBD-Studio Personal.
//------------------------------------------------------------------------------

unit untPageDashboard;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.IOUtils,
  System.JSON,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  ERD.I18n,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.UI.Units,
  ERD.UI.Control,
  ERD.UI.Dashboard,
  ERD.UI.Gauges.Base,
  ERD.UI.Gauges.Dial,
  ERD.UI.Gauges.Bar,
  ERD.UI.ValueTile,
  ERD.UI.TrendChart,
  ERD.Service.Catalog,
  ERD.Service.LiveData,
  untSettings,
  untPersonalPage;

type
  TfrmPageDashboard = class(TFrame, IPersonalPage, IPersonalCommands)
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FLoadingLayout: Boolean;
    FPendingCol: Integer;
    FPendingRow: Integer;
    pnlTop: TPanel;
    lblTitle: TLabel;
    lblSummary: TLabel;
    cmbPID: TComboBox;
    cmbGauge: TComboBox;
    btnAdd: TButton;
    btnEdit: TButton;
    btnSave: TButton;
    btnReset: TButton;
    Dashboard: TOBDDashboard;
    procedure BuildUI;
    procedure TopResize(Sender: TObject);
    procedure LoadLayout;
    procedure SaveLayout;
    procedure BuildDefaultLayout;
    procedure FillPIDChoices;
    procedure ConfigureGauge(AControl: TOBDCustomControl; APID: Byte;
      const ACaption, AUnit: string; AMin, AMax: Double; ADecimals: Byte);
    procedure AddConfiguredGauge(const AKind: string; APID: Byte;
      ACol, ARow, AColSpan, ARowSpan: Integer);
    procedure AddClicked(Sender: TObject);
    procedure EditClicked(Sender: TObject);
    procedure SaveClicked(Sender: TObject);
    procedure ResetClicked(Sender: TObject);
    procedure DashboardLayoutChanged(Sender: TObject);
    procedure DashboardEditModeChanged(Sender: TObject);
    procedure DashboardEmptyCell(Sender: TObject; ACol, ARow: Integer);
    procedure StartPolling;
    procedure StopPolling;
    procedure UpdateSummary;
    function Connected: Boolean;
    function LayoutFileName: string;
    function SelectedPID: Byte;
    function SelectedGaugeKind: string;
    function DashboardPIDs: TBytes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
    // IPersonalCommands
    function Commands: TPersonalCommands;
    function CommandEnabled(ACommand: TPersonalCommand): Boolean;
    function CommandChecked(ACommand: TPersonalCommand): Boolean;
    procedure ExecuteCommand(ACommand: TPersonalCommand);
  end;

implementation

{$R *.dfm}

const
  DASHBOARD_POLL_INTERVAL_MS = 750;

function PIDText(APID: Byte): string;
begin
  Result := '0x' + IntToHex(APID, 2);
end;

function PIDInArray(const APIDs: TBytes; APID: Byte): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(APIDs) do
    if APIDs[I] = APID then
      Exit(True);
  Result := False;
end;

function AddPID(const APIDs: TBytes; APID: Byte): TBytes;
begin
  Result := Copy(APIDs, 0, Length(APIDs));
  if not PIDInArray(Result, APID) then
    Result := Result + [APID];
end;

procedure ResolvePID(APID: Byte; out AName, AUnit: string);
var
  Info: TOBDPIDInfo;
begin
  AName := '';
  AUnit := '';
  if TOBDServiceCatalog.Default.TryGetPID(APID, Info) then
  begin
    AName := Info.Description;
    if AName = '' then
      AName := Info.Name;
    AUnit := Info.Decoder.Unit_;
  end;
  if AName <> '' then
    Exit;
  case APID of
    $04: begin AName := 'Calculated engine load'; AUnit := '%'; end;
    $05: begin AName := 'Engine coolant temperature'; AUnit := OBD_DEGREE_SIGN + 'C'; end;
    $0C: begin AName := 'Engine speed'; AUnit := 'rpm'; end;
    $0D: begin AName := 'Vehicle speed'; AUnit := 'km/h'; end;
    $0F: begin AName := 'Intake air temperature'; AUnit := OBD_DEGREE_SIGN + 'C'; end;
    $10: begin AName := 'Mass air flow'; AUnit := 'g/s'; end;
    $11: begin AName := 'Throttle position'; AUnit := '%'; end;
    $2F: begin AName := 'Fuel tank level'; AUnit := '%'; end;
    $33: begin AName := 'Barometric pressure'; AUnit := 'kPa'; end;
    $42: begin AName := 'Control module voltage'; AUnit := 'V'; end;
    $46: begin AName := 'Ambient air temperature'; AUnit := OBD_DEGREE_SIGN + 'C'; end;
    $5C: begin AName := 'Engine oil temperature'; AUnit := OBD_DEGREE_SIGN + 'C'; end;
    $5E: begin AName := 'Engine fuel rate'; AUnit := 'L/h'; end;
  else
    AName := 'PID ' + PIDText(APID);
  end;
end;

procedure PIDRange(APID: Byte; out AMin, AMax: Double; out ADecimals: Byte);
begin
  ADecimals := 0;
  case APID of
    $04, $11, $2F:
      begin AMin := 0; AMax := 100; ADecimals := 1; end;
    $05:
      begin AMin := -40; AMax := 130; end;
    $0C:
      begin AMin := 0; AMax := 8000; end;
    $0D:
      begin AMin := 0; AMax := 220; end;
    $0F, $46:
      begin AMin := -40; AMax := 100; end;
    $10:
      begin AMin := 0; AMax := 200; ADecimals := 1; end;
    $33:
      begin AMin := 0; AMax := 120; end;
    $42:
      begin AMin := 0; AMax := 18; ADecimals := 1; end;
    $5C:
      begin AMin := -40; AMax := 160; end;
    $5E:
      begin AMin := 0; AMax := 80; ADecimals := 1; end;
  else
    AMin := 0;
    AMax := 100;
    ADecimals := 1;
  end;
end;

{ TfrmPageDashboard }

constructor TfrmPageDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPendingCol := -1;
  FPendingRow := -1;
  BuildUI;
end;

destructor TfrmPageDashboard.Destroy;
begin
  StopPolling;
  inherited Destroy;
end;

procedure TfrmPageDashboard.BuildUI;
begin
  Align := alClient;
  ParentBackground := False;

  pnlTop := TPanel.Create(Self);
  pnlTop.Parent := Self;
  pnlTop.Align := alTop;
  pnlTop.Height := 76;
  pnlTop.BevelOuter := bvNone;
  pnlTop.ParentBackground := False;
  pnlTop.OnResize := TopResize;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := pnlTop;
  lblTitle.Left := 16;
  lblTitle.Top := 10;
  lblTitle.Font.Style := [fsBold];
  lblTitle.Font.Height := -18;

  lblSummary := TLabel.Create(Self);
  lblSummary.Parent := pnlTop;
  lblSummary.Left := 16;
  lblSummary.Top := 38;
  lblSummary.Font.Color := clGrayText;

  cmbPID := TComboBox.Create(Self);
  cmbPID.Parent := pnlTop;
  cmbPID.Style := csDropDownList;
  cmbPID.Top := 22;
  cmbPID.Width := 230;

  cmbGauge := TComboBox.Create(Self);
  cmbGauge.Parent := pnlTop;
  cmbGauge.Style := csDropDownList;
  cmbGauge.Top := 22;
  cmbGauge.Width := 112;
  cmbGauge.Items.Add('Dial gauge');
  cmbGauge.Items.Add('Bar gauge');
  cmbGauge.Items.Add('Value tile');
  cmbGauge.ItemIndex := 2;

  btnAdd := TButton.Create(Self);
  btnAdd.Parent := pnlTop;
  btnAdd.Top := 22;
  btnAdd.Width := 74;
  btnAdd.OnClick := AddClicked;

  btnEdit := TButton.Create(Self);
  btnEdit.Parent := pnlTop;
  btnEdit.Top := 22;
  btnEdit.Width := 84;
  btnEdit.OnClick := EditClicked;

  btnSave := TButton.Create(Self);
  btnSave.Parent := pnlTop;
  btnSave.Top := 22;
  btnSave.Width := 78;
  btnSave.OnClick := SaveClicked;

  btnReset := TButton.Create(Self);
  btnReset.Parent := pnlTop;
  btnReset.Top := 22;
  btnReset.Width := 82;
  btnReset.OnClick := ResetClicked;

  Dashboard := TOBDDashboard.Create(Self);
  Dashboard.Parent := Self;
  Dashboard.Align := alClient;
  Dashboard.Columns := 4;
  Dashboard.Rows := 4;
  Dashboard.Gap := 12;
  Dashboard.OnLayoutChanged := DashboardLayoutChanged;
  Dashboard.OnEditModeChanged := DashboardEditModeChanged;
  Dashboard.OnEmptyCellClick := DashboardEmptyCell;

  FillPIDChoices;
  Translate;
  TopResize(pnlTop);
end;

procedure TfrmPageDashboard.TopResize(Sender: TObject);
var
  X: Integer;
begin
  X := pnlTop.ClientWidth - 16;
  btnReset.Left := X - btnReset.Width;
  X := btnReset.Left - 8;
  btnSave.Left := X - btnSave.Width;
  X := btnSave.Left - 8;
  btnEdit.Left := X - btnEdit.Width;
  X := btnEdit.Left - 8;
  btnAdd.Left := X - btnAdd.Width;
  X := btnAdd.Left - 8;
  cmbGauge.Left := X - cmbGauge.Width;
  X := cmbGauge.Left - 8;
  cmbPID.Left := X - cmbPID.Width;
end;

function TfrmPageDashboard.Connected: Boolean;
begin
  Result := (FHost <> nil) and FHost.Connected;
end;

function TfrmPageDashboard.PageKey: string;
begin
  Result := PAGE_DASHBOARD;
end;

procedure TfrmPageDashboard.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  Dashboard.Source := FHost.Session.LiveData;
  LoadLayout;
  UpdateSummary;
end;

procedure TfrmPageDashboard.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if FActive then
    StartPolling
  else
    StopPolling;
end;

procedure TfrmPageDashboard.ConnectionChanged(AConnected: Boolean);
begin
  if AConnected then
  begin
    Dashboard.Source := FHost.Session.LiveData;
    if FActive then
      StartPolling;
  end
  else
    StopPolling;
  UpdateSummary;
end;

procedure TfrmPageDashboard.UnitsChanged;
begin
  Dashboard.Invalidate;
end;

procedure TfrmPageDashboard.Translate;
begin
  lblTitle.Caption := Tr('personal.dashboard.title', 'Dashboard');
  btnAdd.Caption := Tr('personal.dashboard.add', 'Add');
  btnSave.Caption := Tr('personal.dashboard.save', 'Save');
  btnReset.Caption := Tr('personal.dashboard.reset', 'Defaults');
  DashboardEditModeChanged(Dashboard);
  UpdateSummary;
end;

function TfrmPageDashboard.LayoutFileName: string;
begin
  Result := TPath.Combine(TPersonalSettings.SettingsFolder, 'dashboard.json');
end;

procedure TfrmPageDashboard.LoadLayout;
var
  Value: TJSONValue;
  Text: string;
begin
  if not TFile.Exists(LayoutFileName) then
  begin
    BuildDefaultLayout;
    Exit;
  end;
  Text := TFile.ReadAllText(LayoutFileName, TEncoding.UTF8);
  Value := TJSONObject.ParseJSONValue(Text);
  try
    if Value is TJSONObject then
    begin
      FLoadingLayout := True;
      try
        Dashboard.LoadLayout(TJSONObject(Value).ToJSON);
      finally
        FLoadingLayout := False;
      end;
    end
    else
      BuildDefaultLayout;
  finally
    Value.Free;
  end;
end;

procedure TfrmPageDashboard.SaveLayout;
begin
  ForceDirectories(ExtractFilePath(LayoutFileName));
  Dashboard.SaveLayoutToFile(LayoutFileName);
end;

procedure TfrmPageDashboard.FillPIDChoices;
const
  PIDs: array [0 .. 13] of Byte = ($0C, $0D, $05, $04, $11, $42, $0F, $2F,
    $10, $0B, $33, $46, $5C, $5E);
var
  I: Integer;
  NameText, UnitText: string;
begin
  cmbPID.Items.BeginUpdate;
  try
    cmbPID.Items.Clear;
    for I := 0 to High(PIDs) do
    begin
      ResolvePID(PIDs[I], NameText, UnitText);
      cmbPID.Items.Add(PIDText(PIDs[I]) + '  ' + NameText);
    end;
    cmbPID.ItemIndex := 0;
  finally
    cmbPID.Items.EndUpdate;
  end;
end;

function TfrmPageDashboard.SelectedPID: Byte;
var
  S, Hex: string;
  SpacePos, Value: Integer;
begin
  Result := $0C;
  if cmbPID.ItemIndex < 0 then
    Exit;
  S := cmbPID.Items[cmbPID.ItemIndex];
  SpacePos := Pos(' ', S);
  if SpacePos > 0 then
    Hex := Copy(S, 1, SpacePos - 1)
  else
    Hex := S;
  Hex := StringReplace(Hex, '0x', '$', [rfIgnoreCase]);
  if TryStrToInt(Hex, Value) then
    Result := Byte(EnsureRange(Value, 0, 255));
end;

function TfrmPageDashboard.SelectedGaugeKind: string;
begin
  case cmbGauge.ItemIndex of
    0: Result := 'dial';
    1: Result := 'bar';
  else
    Result := 'value';
  end;
end;

procedure TfrmPageDashboard.ConfigureGauge(AControl: TOBDCustomControl;
  APID: Byte; const ACaption, AUnit: string; AMin, AMax: Double;
  ADecimals: Byte);
var
  Gauge: TOBDGaugeBase;
begin
  if not(AControl is TOBDGaugeBase) then
    Exit;
  Gauge := TOBDGaugeBase(AControl);
  Gauge.Caption := ACaption;
  Gauge.&Unit := AUnit;
  Gauge.Min := AMin;
  Gauge.Max := AMax;
  Gauge.Decimals := ADecimals;
  Gauge.Channel.PID := APID;
  Gauge.Channel.Source := FHost.Session.LiveData;
  case APID of
    $05, $5C:
      Gauge.Alerts.SetHigh(105, 115);
    $0C:
      Gauge.Alerts.SetHigh(6000, 6800);
    $42:
      Gauge.Alerts.SetLow(12.0, 11.5);
  end;
end;

procedure TfrmPageDashboard.AddConfiguredGauge(const AKind: string; APID: Byte;
  ACol, ARow, AColSpan, ARowSpan: Integer);
var
  Control: TOBDCustomControl;
  NameText, UnitText: string;
  MinValue, MaxValue: Double;
  Decimals: Byte;
begin
  ResolvePID(APID, NameText, UnitText);
  PIDRange(APID, MinValue, MaxValue, Decimals);
  Control := Dashboard.AddTile(AKind, ACol, ARow, AColSpan, ARowSpan);
  ConfigureGauge(Control, APID, NameText, UnitText, MinValue, MaxValue, Decimals);
end;

procedure TfrmPageDashboard.BuildDefaultLayout;
begin
  FLoadingLayout := True;
  try
    Dashboard.ClearTiles;
    Dashboard.Columns := 4;
    Dashboard.Rows := 4;
    Dashboard.Gap := 12;
    AddConfiguredGauge('dial', $0C, 0, 0, 1, 2);
    AddConfiguredGauge('dial', $0D, 1, 0, 1, 2);
    AddConfiguredGauge('value', $05, 2, 0, 1, 1);
    AddConfiguredGauge('bar', $04, 3, 0, 1, 1);
    AddConfiguredGauge('bar', $11, 2, 1, 1, 1);
    AddConfiguredGauge('value', $42, 3, 1, 1, 1);
    AddConfiguredGauge('value', $0F, 0, 2, 1, 1);
    AddConfiguredGauge('bar', $2F, 1, 2, 1, 1);
  finally
    FLoadingLayout := False;
  end;
  SaveLayout;
end;

procedure TfrmPageDashboard.AddClicked(Sender: TObject);
var
  Col, Row: Integer;
begin
  Col := FPendingCol;
  Row := FPendingRow;
  AddConfiguredGauge(SelectedGaugeKind, SelectedPID, Col, Row, 1, 1);
  FPendingCol := -1;
  FPendingRow := -1;
  SaveLayout;
  StartPolling;
end;

procedure TfrmPageDashboard.EditClicked(Sender: TObject);
begin
  Dashboard.EditMode := not Dashboard.EditMode;
end;

procedure TfrmPageDashboard.SaveClicked(Sender: TObject);
begin
  SaveLayout;
  Dashboard.EditMode := False;
  FHost.Notify(tkSuccess, Tr('personal.dashboard.savedTitle', 'Dashboard'),
    Tr('personal.dashboard.savedText', 'Dashboard layout saved.'));
end;

procedure TfrmPageDashboard.ResetClicked(Sender: TObject);
begin
  if (FHost = nil) or FHost.Confirm(dkWarning,
    Tr('personal.dashboard.resetTitle', 'Reset dashboard'),
    Tr('personal.dashboard.resetText', 'Replace the current dashboard layout with the default gauges?'),
    Tr('personal.dashboard.resetConfirm', 'Reset')) then
  begin
    BuildDefaultLayout;
    StartPolling;
    UpdateSummary;
  end;
end;

procedure TfrmPageDashboard.DashboardLayoutChanged(Sender: TObject);
begin
  if FLoadingLayout then
    Exit;
  SaveLayout;
  StartPolling;
  UpdateSummary;
end;

procedure TfrmPageDashboard.DashboardEditModeChanged(Sender: TObject);
begin
  if Dashboard.EditMode then
    btnEdit.Caption := Tr('personal.dashboard.done', 'Done')
  else
    btnEdit.Caption := Tr('personal.dashboard.edit', 'Edit layout');
end;

procedure TfrmPageDashboard.DashboardEmptyCell(Sender: TObject; ACol,
  ARow: Integer);
begin
  FPendingCol := ACol;
  FPendingRow := ARow;
  lblSummary.Caption := Format(Tr('personal.dashboard.cellSelected',
    'Cell %d,%d selected. Pick a PID and gauge type, then Add.'),
    [ACol + 1, ARow + 1]);
end;

function TfrmPageDashboard.DashboardPIDs: TBytes;
var
  I, J: Integer;
  Control: TControl;
  Gauge: TOBDGaugeBase;
  Chart: TOBDTrendChart;
begin
  Result := nil;
  for I := 0 to Dashboard.Tiles.Count - 1 do
  begin
    Control := Dashboard.Tiles[I].Control;
    if Control is TOBDGaugeBase then
    begin
      Gauge := TOBDGaugeBase(Control);
      Result := AddPID(Result, Gauge.Channel.PID);
    end
    else if Control is TOBDTrendChart then
    begin
      Chart := TOBDTrendChart(Control);
      for J := 0 to Chart.Channels.Count - 1 do
        Result := AddPID(Result, Chart.Channels[J].PID);
    end;
  end;
end;

procedure TfrmPageDashboard.StartPolling;
var
  PIDs: TBytes;
begin
  StopPolling;
  if not FActive or not Connected then
    Exit;
  PIDs := DashboardPIDs;
  if Length(PIDs) = 0 then
    Exit;
  try
    FHost.Session.LiveData.Poll(PIDs, DASHBOARD_POLL_INTERVAL_MS);
  except
    on E: Exception do
      FHost.Notify(tkWarning, Tr('personal.dashboard.pollTitle', 'Dashboard'),
        Tr('personal.dashboard.pollText', 'Dashboard polling could not start: ') + E.Message);
  end;
end;

procedure TfrmPageDashboard.StopPolling;
begin
  if (FHost <> nil) and (FHost.Session <> nil) and
    FHost.Session.LiveData.IsPolling then
    FHost.Session.LiveData.PollStop;
end;

procedure TfrmPageDashboard.UpdateSummary;
var
  PIDs: TBytes;
begin
  if FHost = nil then
    Exit;
  PIDs := DashboardPIDs;
  if not Connected then
    lblSummary.Caption := Tr('personal.dashboard.offline',
      'Connect to a vehicle to animate the gauges.')
  else if Dashboard.EditMode then
    lblSummary.Caption := Tr('personal.dashboard.editHint',
      'Drag gauges to move them, drag a corner to resize, or use the close handle to remove one.')
  else
    lblSummary.Caption := Format(Tr('personal.dashboard.summary',
      '%d gauges · %d live PIDs.'), [Dashboard.Tiles.Count, Length(PIDs)]);
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageDashboard.Commands: TPersonalCommands;
begin
  Result := [pcAddGauge, pcEditDashboard, pcSaveDashboard, pcResetDashboard];
end;

function TfrmPageDashboard.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  Result := (FHost <> nil) and (ACommand in Commands);
end;

function TfrmPageDashboard.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := (ACommand = pcEditDashboard) and Dashboard.EditMode;
end;

procedure TfrmPageDashboard.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcAddGauge:
      AddClicked(Self);
    pcEditDashboard:
      EditClicked(Self);
    pcSaveDashboard:
      SaveClicked(Self);
    pcResetDashboard:
      ResetClicked(Self);
  end;
end;

end.
