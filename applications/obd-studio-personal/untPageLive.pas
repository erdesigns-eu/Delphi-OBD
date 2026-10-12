//------------------------------------------------------------------------------
//  untPageLive
//
//  Live data page for OBD-Studio Personal. It discovers the vehicle's Mode 01
//  sensors, lets the user choose which channels to watch, polls those channels
//  while the page is active and can record the selected channels as CSV.
//------------------------------------------------------------------------------

unit untPageLive;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils,
  System.IOUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Graphics,
  ERD.I18n,
  ERD.UI.Toast,
  ERD.UI.Units,
  ERD.UI.LiveDataGrid,
  ERD.UI.TrendChart,
  ERD.UI.ValueTile,
  ERD.Service.Catalog,
  ERD.Service.LiveData,
  ERD.Recorder.CSV,
  untPersonalPage;

type
  TfrmPageLive = class(TFrame, IPersonalPage)
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FLoading: Boolean;
    FRecording: Boolean;
    FSupportedPIDs: TBytes;
    FSelectedPIDs: TBytes;
    FSubscribedPIDs: TBytes;
    FRecordingStarted: TDateTime;
    pnlTop: TPanel;
    lblTitle: TLabel;
    lblSummary: TLabel;
    edtSearch: TEdit;
    btnRefresh: TButton;
    btnSnapshot: TButton;
    chkRecord: TCheckBox;
    pcView: TPageControl;
    tabList: TTabSheet;
    tabTiles: TTabSheet;
    splList: TSplitter;
    grdSensors: TOBDLiveDataGrid;
    sbCharts: TScrollBox;
    sbTiles: TScrollBox;
    tmrRecording: TTimer;
    procedure TopResize(Sender: TObject);
    procedure BuildUI;
    procedure DiscoverSensors;
    procedure ApplySupportedPIDs(const APIDs: TBytes);
    procedure ApplySearchFilter;
    procedure ApplyGridChecks;
    procedure SensorChecked(Sender: TObject; APID: Byte; AChecked: Boolean);
    procedure SearchChanged(Sender: TObject);
    procedure RefreshClicked(Sender: TObject);
    procedure SnapshotClicked(Sender: TObject);
    procedure RecordClicked(Sender: TObject);
    procedure RecordingTimer(Sender: TObject);
    procedure ViewChanged(Sender: TObject);
    procedure LiveValue(Sender: TObject; const AValue: TOBDPIDValue);
    procedure SelectDefaultPIDs;
    procedure SetPIDSelected(APID: Byte; ASelected: Boolean);
    procedure RebuildCharts;
    procedure RebuildTiles;
    procedure StartPolling;
    procedure StopPolling;
    procedure SubscribeSelected;
    procedure UnsubscribeSelected;
    procedure StartCSVRecording;
    procedure StopCSVRecording;
    procedure UpdateRecordingStatus;
    procedure UpdateSummary;
    function Connected: Boolean;
    function RecordingFileName: string;
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

const
  POLL_INTERVAL_MS = 750;

function PIDText(APID: Byte): string;
begin
  Result := '0x' + IntToHex(APID, 2);
end;

function SamePID(const ALeft, ARight: TBytes): Boolean;
var
  I: Integer;
begin
  Result := Length(ALeft) = Length(ARight);
  if not Result then
    Exit;
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then
      Exit(False);
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

function RemovePID(const APIDs: TBytes; APID: Byte): TBytes;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(APIDs) do
    if APIDs[I] <> APID then
      Result := Result + [APIDs[I]];
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
    $0A: begin AName := 'Fuel pressure'; AUnit := 'kPa'; end;
    $0B: begin AName := 'Intake manifold pressure'; AUnit := 'kPa'; end;
    $0C: begin AName := 'Engine speed'; AUnit := 'rpm'; end;
    $0D: begin AName := 'Vehicle speed'; AUnit := 'km/h'; end;
    $0F: begin AName := 'Intake air temperature'; AUnit := OBD_DEGREE_SIGN + 'C'; end;
    $10: begin AName := 'Mass air flow'; AUnit := 'g/s'; end;
    $11: begin AName := 'Throttle position'; AUnit := '%'; end;
    $21: begin AName := 'Distance with MIL on'; AUnit := 'km'; end;
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
    $0A, $0B, $33:
      begin AMin := 0; AMax := 300; end;
    $0C:
      begin AMin := 0; AMax := 8000; end;
    $0D:
      begin AMin := 0; AMax := 220; end;
    $0F, $46:
      begin AMin := -40; AMax := 100; end;
    $10:
      begin AMin := 0; AMax := 200; ADecimals := 1; end;
    $21:
      begin AMin := 0; AMax := 1000; end;
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

{ TfrmPageLive }

constructor TfrmPageLive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

destructor TfrmPageLive.Destroy;
begin
  StopCSVRecording;
  StopPolling;
  inherited Destroy;
end;

procedure TfrmPageLive.BuildUI;
begin
  Align := alClient;
  ParentBackground := False;

  pnlTop := TPanel.Create(Self);
  pnlTop.Parent := Self;
  pnlTop.Align := alTop;
  pnlTop.Height := 74;
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

  chkRecord := TCheckBox.Create(Self);
  chkRecord.Parent := pnlTop;
  chkRecord.Caption := 'Record to CSV';
  chkRecord.Top := 26;
  chkRecord.Width := 116;
  chkRecord.Anchors := [akTop, akRight];
  chkRecord.OnClick := RecordClicked;

  btnSnapshot := TButton.Create(Self);
  btnSnapshot.Parent := pnlTop;
  btnSnapshot.Caption := 'Snapshot';
  btnSnapshot.Top := 22;
  btnSnapshot.Width := 88;
  btnSnapshot.Anchors := [akTop, akRight];
  btnSnapshot.OnClick := SnapshotClicked;

  btnRefresh := TButton.Create(Self);
  btnRefresh.Parent := pnlTop;
  btnRefresh.Caption := 'Refresh';
  btnRefresh.Top := 22;
  btnRefresh.Width := 76;
  btnRefresh.Anchors := [akTop, akRight];
  btnRefresh.OnClick := RefreshClicked;

  edtSearch := TEdit.Create(Self);
  edtSearch.Parent := pnlTop;
  edtSearch.Top := 22;
  edtSearch.Width := 220;
  edtSearch.Anchors := [akTop, akRight];
  edtSearch.TextHint := 'Search sensors';
  edtSearch.OnChange := SearchChanged;

  pcView := TPageControl.Create(Self);
  pcView.Parent := Self;
  pcView.Align := alClient;
  pcView.OnChange := ViewChanged;

  tabList := TTabSheet.Create(Self);
  tabList.PageControl := pcView;

  tabTiles := TTabSheet.Create(Self);
  tabTiles.PageControl := pcView;

  grdSensors := TOBDLiveDataGrid.Create(Self);
  grdSensors.Parent := tabList;
  grdSensors.Align := alLeft;
  grdSensors.Width := 560;
  grdSensors.ShowCheckboxes := True;
  grdSensors.OnCheckChanged := SensorChecked;

  splList := TSplitter.Create(Self);
  splList.Parent := tabList;
  splList.Align := alLeft;
  splList.Width := 6;

  sbCharts := TScrollBox.Create(Self);
  sbCharts.Parent := tabList;
  sbCharts.Align := alClient;
  sbCharts.BorderStyle := bsNone;
  sbCharts.VertScrollBar.Tracking := True;

  sbTiles := TScrollBox.Create(Self);
  sbTiles.Parent := tabTiles;
  sbTiles.Align := alClient;
  sbTiles.BorderStyle := bsNone;
  sbTiles.VertScrollBar.Tracking := True;

  tmrRecording := TTimer.Create(Self);
  tmrRecording.Enabled := False;
  tmrRecording.Interval := 1000;
  tmrRecording.OnTimer := RecordingTimer;

  Translate;
  TopResize(pnlTop);
end;

procedure TfrmPageLive.TopResize(Sender: TObject);
var
  X: Integer;
begin
  X := pnlTop.ClientWidth - 16;
  chkRecord.Left := X - chkRecord.Width;
  X := chkRecord.Left - 10;
  btnSnapshot.Left := X - btnSnapshot.Width;
  X := btnSnapshot.Left - 8;
  btnRefresh.Left := X - btnRefresh.Width;
  X := btnRefresh.Left - 8;
  edtSearch.Left := X - edtSearch.Width;
end;

function TfrmPageLive.Connected: Boolean;
begin
  Result := (FHost <> nil) and FHost.Connected;
end;

function TfrmPageLive.PageKey: string;
begin
  Result := PAGE_LIVE;
end;

procedure TfrmPageLive.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  grdSensors.AssignDataSource(FHost.Session.LiveData);
  UpdateSummary;
end;

procedure TfrmPageLive.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if FActive then
  begin
    if Connected then
      DiscoverSensors
    else
      UpdateSummary;
  end
  else
  begin
    StopCSVRecording;
    StopPolling;
  end;
end;

procedure TfrmPageLive.ConnectionChanged(AConnected: Boolean);
begin
  if AConnected then
  begin
    grdSensors.AssignDataSource(FHost.Session.LiveData);
    if FActive then
      DiscoverSensors;
  end
  else
  begin
    StopCSVRecording;
    StopPolling;
    FSupportedPIDs := nil;
    grdSensors.Clear;
    RebuildCharts;
    RebuildTiles;
    UpdateSummary;
  end;
end;

procedure TfrmPageLive.UnitsChanged;
begin
  grdSensors.Invalidate;
  RebuildCharts;
  RebuildTiles;
end;

procedure TfrmPageLive.Translate;
begin
  lblTitle.Caption := Tr('personal.live.title', 'Live data');
  btnRefresh.Caption := Tr('personal.live.refresh', 'Refresh');
  btnSnapshot.Caption := Tr('personal.live.snapshot', 'Snapshot');
  chkRecord.Caption := Tr('personal.live.record', 'Record to CSV');
  tabList.Caption := Tr('personal.live.list', 'Sensor list');
  tabTiles.Caption := Tr('personal.live.tiles', 'Value grid');
  edtSearch.TextHint := Tr('personal.live.search', 'Search sensors');
  UpdateSummary;
end;

procedure TfrmPageLive.DiscoverSensors;
var
  Supported: TBytes;
begin
  if not Connected then
    Exit;
  Supported := nil;
  FHost.SetProgress(Tr('personal.live.discovering', 'Reading supported sensors'), 10);
  FHost.Session.RunJob(
    procedure
    begin
      Supported := FHost.Session.LiveData.SupportedPIDs;
    end,
    procedure(const AError: string)
    begin
      FHost.SetProgress('', -1);
      if not FActive then
        Exit;
      if AError <> '' then
      begin
        FHost.Notify(tkWarning, Tr('personal.live.failedTitle', 'Live data'),
          Tr('personal.live.failedText', 'Could not read supported sensors: ') + AError);
        UpdateSummary;
        Exit;
      end;
      ApplySupportedPIDs(Supported);
    end);
end;

procedure TfrmPageLive.ApplySupportedPIDs(const APIDs: TBytes);
begin
  FSupportedPIDs := Copy(APIDs, 0, Length(APIDs));
  if Length(FSelectedPIDs) = 0 then
    SelectDefaultPIDs;
  ApplySearchFilter;
  RebuildCharts;
  RebuildTiles;
  StartPolling;
  UpdateSummary;
end;

procedure TfrmPageLive.SelectDefaultPIDs;
const
  Defaults: array [0 .. 7] of Byte = ($0C, $0D, $05, $04, $11, $42, $0F, $2F);
var
  I: Integer;
begin
  FSelectedPIDs := nil;
  for I := 0 to High(Defaults) do
    if PIDInArray(FSupportedPIDs, Defaults[I]) then
      FSelectedPIDs := AddPID(FSelectedPIDs, Defaults[I]);
  I := 0;
  while (Length(FSelectedPIDs) < 4) and (I <= High(FSupportedPIDs)) do
  begin
    FSelectedPIDs := AddPID(FSelectedPIDs, FSupportedPIDs[I]);
    Inc(I);
  end;
end;

procedure TfrmPageLive.ApplySearchFilter;
var
  Filter, NameText, UnitText, Haystack: string;
  Filtered: TBytes;
  I: Integer;
begin
  Filter := LowerCase(Trim(edtSearch.Text));
  Filtered := nil;
  for I := 0 to High(FSupportedPIDs) do
  begin
    ResolvePID(FSupportedPIDs[I], NameText, UnitText);
    Haystack := LowerCase(NameText + ' ' + UnitText + ' ' + PIDText(FSupportedPIDs[I]));
    if (Filter = '') or (Pos(Filter, Haystack) > 0) then
      Filtered := Filtered + [FSupportedPIDs[I]];
  end;
  FLoading := True;
  try
    grdSensors.SetPIDs(Filtered);
    ApplyGridChecks;
  finally
    FLoading := False;
  end;
  UpdateSummary;
end;

procedure TfrmPageLive.ApplyGridChecks;
var
  I: Integer;
begin
  for I := 0 to High(FSelectedPIDs) do
    grdSensors.SetChecked(FSelectedPIDs[I], True);
end;

procedure TfrmPageLive.SensorChecked(Sender: TObject; APID: Byte;
  AChecked: Boolean);
begin
  if FLoading then
    Exit;
  SetPIDSelected(APID, AChecked);
end;

procedure TfrmPageLive.SetPIDSelected(APID: Byte; ASelected: Boolean);
begin
  if ASelected then
    FSelectedPIDs := AddPID(FSelectedPIDs, APID)
  else
    FSelectedPIDs := RemovePID(FSelectedPIDs, APID);
  RebuildCharts;
  RebuildTiles;
  if FRecording then
  begin
    StopCSVRecording;
    if Length(FSelectedPIDs) > 0 then
      StartCSVRecording;
  end;
  StartPolling;
  UpdateSummary;
end;

procedure TfrmPageLive.SearchChanged(Sender: TObject);
begin
  ApplySearchFilter;
end;

procedure TfrmPageLive.RefreshClicked(Sender: TObject);
begin
  if Connected then
    DiscoverSensors
  else if FHost <> nil then
    FHost.Notify(tkInfo, Tr('personal.live.notConnectedTitle', 'Live data'),
      Tr('personal.live.notConnectedText', 'Connect to a vehicle to read supported sensors.'));
end;

procedure TfrmPageLive.SnapshotClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.Notify(tkSuccess, Tr('personal.live.snapshotTitle', 'Snapshot'),
      Format(Tr('personal.live.snapshotText', '%d selected sensors are on screen.'),
        [Length(FSelectedPIDs)]));
end;

procedure TfrmPageLive.RecordClicked(Sender: TObject);
begin
  if chkRecord.Checked then
    StartCSVRecording
  else
    StopCSVRecording;
end;

procedure TfrmPageLive.RecordingTimer(Sender: TObject);
begin
  UpdateRecordingStatus;
end;

procedure TfrmPageLive.ViewChanged(Sender: TObject);
begin
  RebuildTiles;
end;

procedure TfrmPageLive.LiveValue(Sender: TObject; const AValue: TOBDPIDValue);
begin
  if AValue.PID = 0 then
    Exit;
  UpdateSummary;
end;

procedure TfrmPageLive.RebuildCharts;
var
  I: Integer;
  Chart: TOBDTrendChart;
  Channel: TOBDTrendChannel;
  NameText, UnitText: string;
  MinValue, MaxValue: Double;
  Decimals: Byte;
begin
  while sbCharts.ControlCount > 0 do
    sbCharts.Controls[0].Free;
  for I := 0 to High(FSelectedPIDs) do
  begin
    ResolvePID(FSelectedPIDs[I], NameText, UnitText);
    PIDRange(FSelectedPIDs[I], MinValue, MaxValue, Decimals);
    Chart := TOBDTrendChart.Create(Self);
    Chart.Parent := sbCharts;
    Chart.Align := alTop;
    Chart.AlignWithMargins := True;
    Chart.Margins.SetBounds(10, 10, 10, 0);
    Chart.Height := 180;
    Chart.TimeWindowSec := 60;
    Chart.HistorySec := 600;
    Chart.Source := FHost.Session.LiveData;
    Channel := Chart.AddChannel(NameText, FSelectedPIDs[I], MinValue, MaxValue,
      UnitText);
    Channel.Decimals := Decimals;
  end;
end;

procedure TfrmPageLive.RebuildTiles;
var
  I, Col, TileWidth, TileHeight: Integer;
  Tile: TOBDValueTile;
  NameText, UnitText: string;
  MinValue, MaxValue: Double;
  Decimals: Byte;
begin
  while sbTiles.ControlCount > 0 do
    sbTiles.Controls[0].Free;
  TileWidth := 210;
  TileHeight := 128;
  for I := 0 to High(FSelectedPIDs) do
  begin
    ResolvePID(FSelectedPIDs[I], NameText, UnitText);
    PIDRange(FSelectedPIDs[I], MinValue, MaxValue, Decimals);
    Col := I mod 3;
    Tile := TOBDValueTile.Create(Self);
    Tile.Parent := sbTiles;
    Tile.Left := 12 + Col * (TileWidth + 12);
    Tile.Top := 12 + (I div 3) * (TileHeight + 12);
    Tile.Width := TileWidth;
    Tile.Height := TileHeight;
    Tile.Caption := NameText;
    Tile.&Unit := UnitText;
    Tile.Min := MinValue;
    Tile.Max := MaxValue;
    Tile.Decimals := Decimals;
    Tile.Channel.PID := FSelectedPIDs[I];
    Tile.Channel.Source := FHost.Session.LiveData;
  end;
end;

procedure TfrmPageLive.StartPolling;
begin
  StopPolling;
  if not FActive or not Connected or (Length(FSelectedPIDs) = 0) then
    Exit;
  SubscribeSelected;
  try
    FHost.Session.LiveData.Poll(FSelectedPIDs, POLL_INTERVAL_MS);
  except
    on E: Exception do
      FHost.Notify(tkWarning, Tr('personal.live.pollTitle', 'Live data'),
        Tr('personal.live.pollText', 'Polling could not be started: ') + E.Message);
  end;
end;

procedure TfrmPageLive.StopPolling;
begin
  UnsubscribeSelected;
  if (FHost <> nil) and (FHost.Session <> nil) and
    FHost.Session.LiveData.IsPolling then
    FHost.Session.LiveData.PollStop;
end;

procedure TfrmPageLive.SubscribeSelected;
var
  I: Integer;
begin
  UnsubscribeSelected;
  if (FHost = nil) or (FHost.Session = nil) then
    Exit;
  for I := 0 to High(FSelectedPIDs) do
  begin
    FHost.Session.LiveData.Subscribe(FSelectedPIDs[I], LiveValue);
    FSubscribedPIDs := FSubscribedPIDs + [FSelectedPIDs[I]];
  end;
end;

procedure TfrmPageLive.UnsubscribeSelected;
var
  I: Integer;
begin
  if (FHost <> nil) and (FHost.Session <> nil) then
    for I := 0 to High(FSubscribedPIDs) do
      FHost.Session.LiveData.Unsubscribe(FSubscribedPIDs[I], LiveValue);
  FSubscribedPIDs := nil;
end;

function TfrmPageLive.RecordingFileName: string;
begin
  Result := TPath.Combine(FHost.Settings.DataSubFolder('Recordings'),
    'live-' + FormatDateTime('yyyymmdd-hhnnss', Now) + '.csv');
end;

procedure TfrmPageLive.StartCSVRecording;
var
  I: Integer;
  Logger: TOBDCSVLogger;
begin
  if FRecording then
    Exit;
  if not Connected then
  begin
    chkRecord.Checked := False;
    FHost.Notify(tkInfo, Tr('personal.live.recordConnectTitle', 'Record live data'),
      Tr('personal.live.recordConnectText', 'Connect to a vehicle before recording.'));
    Exit;
  end;
  if Length(FSelectedPIDs) = 0 then
  begin
    chkRecord.Checked := False;
    FHost.Notify(tkInfo, Tr('personal.live.recordSelectTitle', 'Record live data'),
      Tr('personal.live.recordSelectText', 'Select at least one sensor to record.'));
    Exit;
  end;
  Logger := FHost.Session.CSVLogger;
  if Logger.Active then
    Logger.Stop;
  Logger.FileName := RecordingFileName;
  Logger.ClearPIDs;
  for I := 0 to High(FSelectedPIDs) do
    Logger.AddPID(FSelectedPIDs[I]);
  Logger.Start;
  FRecording := True;
  FRecordingStarted := Now;
  tmrRecording.Enabled := True;
  chkRecord.Checked := True;
  UpdateRecordingStatus;
  FHost.Notify(tkSuccess, Tr('personal.live.recordStarted', 'Recording started'),
    ExtractFileName(Logger.FileName));
end;

procedure TfrmPageLive.StopCSVRecording;
begin
  if not FRecording and ((FHost = nil) or not FHost.Session.CSVLogger.Active) then
    Exit;
  tmrRecording.Enabled := False;
  if (FHost <> nil) and (FHost.Session <> nil) and FHost.Session.CSVLogger.Active then
    FHost.Session.CSVLogger.Stop;
  FRecording := False;
  chkRecord.Checked := False;
  if FHost <> nil then
    FHost.SetRecording('');
end;

procedure TfrmPageLive.UpdateRecordingStatus;
var
  Seconds: Integer;
begin
  if not FRecording then
    Exit;
  Seconds := Round((Now - FRecordingStarted) * SecsPerDay);
  if Seconds < 0 then
    Seconds := 0;
  FHost.SetRecording(Format('REC %.2d:%.2d', [Seconds div 60, Seconds mod 60]));
end;

procedure TfrmPageLive.UpdateSummary;
var
  Text: string;
begin
  if FHost = nil then
    Exit;
  if not Connected then
    Text := Tr('personal.live.offline', 'Connect to a vehicle to read supported sensors.')
  else if Length(FSupportedPIDs) = 0 then
    Text := Tr('personal.live.empty', 'Supported sensors have not been read yet.')
  else
    Text := Format(Tr('personal.live.summary', '%d of %d sensors selected.'),
      [Length(FSelectedPIDs), Length(FSupportedPIDs)]);
  lblSummary.Caption := Text;
end;

end.
