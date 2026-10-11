//------------------------------------------------------------------------------
//  untMain
//
//  Main form of the OBD Studio example: a complete workshop application
//  built from the Delphi-OBD UI components.
//
//    - TOBDTitleBar with quick-access and caption buttons, switching
//      between a TOBDRibbon (with a contextual tab set and a
//      TOBDBackstage) and a classic menu bar from mnuMain.
//    - TOBDSidebar navigating eight pages; TOBDStatusBar with status,
//      progress and link panels.
//    - Dashboard: TOBDDashboard with dial, bar and value gauges, a
//      check-engine lamp, a trend chart and a dot-matrix ticker.
//    - Live data: TOBDLiveDataGrid driving a TOBDTrendChart, with a
//      TOBDScrollBar to scroll back through the recorded history.
//    - Trouble codes, freeze frame, readiness and vehicle pages with the
//      diagnostic panels; a workshop job card made of the small
//      controls (edits, combo box, segmented, check boxes, radio
//      buttons, chips, badge, range bar, step progress, buttons).
//    - Adapter log (TOBDLogViewer / TOBDTerminal) and settings
//      (TOBDInspector / TOBDRangeEditor).
//    - TOBDToastManager, TOBDDialog, TOBDHintStyle and TOBDPopupMenu.
//
//  Every control is placed in the form designer (untMain.dfm). The data
//  comes from TEngineSimulator (untSimulator.pas): a diesel with a
//  sticking EGR valve, so every screen shows a realistic case. Replace
//  the simulator with a TOBDLiveData / TOBDConnection to use a car.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Types, System.UITypes, System.Math, System.IOUtils, System.Actions,
  Vcl.Menus, Vcl.ActnList, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Control, ERD.UI.Theme, ERD.UI.Hint,
  ERD.UI.Toast, ERD.UI.Dialogs, ERD.UI.Menus, ERD.UI.TitleBar, ERD.UI.Ribbon,
  ERD.UI.Backstage, ERD.UI.Sidebar, ERD.UI.StatusBar, ERD.UI.ToolBar,
  ERD.UI.Tabs, ERD.UI.Card, ERD.UI.Buttons, ERD.UI.Edits, ERD.UI.Segmented,
  ERD.UI.Chips, ERD.UI.Progress, ERD.UI.RangeBar, ERD.UI.ScrollBar,
  ERD.UI.ConnectionBar, ERD.UI.Dashboard, ERD.UI.Gauges.Base,
  ERD.UI.Gauges.Dial, ERD.UI.Gauges.Bar, ERD.UI.ValueTile, ERD.UI.StatusLamp,
  ERD.UI.TrendChart, ERD.UI.MatrixDisplay, ERD.UI.LiveDataGrid,
  ERD.UI.DtcPanel, ERD.UI.FreezeFrameView, ERD.UI.RangeProfiles,
  ERD.UI.RangeEditor, ERD.UI.ReadinessPanel, ERD.UI.VehicleCard,
  ERD.UI.Inspector, ERD.UI.Terminal, ERD.UI.LogViewer,
  untSimulator;

type
  TfrmMain = class(TForm)
    OBDTheme: TOBDTheme;
    OBDHintStyle: TOBDHintStyle;
    OBDToasts: TOBDToastManager;
    dlgConfirm: TOBDDialog;
    RangeProfile: TOBDRangeProfile;
    alMain: TActionList;
    actConnect: TAction;
    actDisconnect: TAction;
    actReadCodes: TAction;
    actClearCodes: TAction;
    actEditLayout: TAction;
    actAddTile: TAction;
    actSaveLayout: TAction;
    actLoadLayout: TAction;
    actResetLayout: TAction;
    actPauseTrend: TAction;
    actResetMinMax: TAction;
    actReport: TAction;
    actThemeLight: TAction;
    actThemeDark: TAction;
    actThemeWindows: TAction;
    actDensityDesktop: TAction;
    actDensityTablet: TAction;
    actUnitsMetric: TAction;
    actUnitsImperial: TAction;
    actMenuBar: TAction;
    actAbout: TAction;
    actExit: TAction;
    mnuMain: TMainMenu;
    miFile: TMenuItem;
    miReport: TMenuItem;
    miFileSep: TMenuItem;
    miExit: TMenuItem;
    miVehicle: TMenuItem;
    miConnect: TMenuItem;
    miDisconnect: TMenuItem;
    miVehicleSep: TMenuItem;
    miReadCodes: TMenuItem;
    miClearCodes: TMenuItem;
    miDashboard: TMenuItem;
    miEditLayout: TMenuItem;
    miAddTile: TMenuItem;
    miDashboardSep: TMenuItem;
    miSaveLayout: TMenuItem;
    miLoadLayout: TMenuItem;
    miResetLayout: TMenuItem;
    miView: TMenuItem;
    miThemeLight: TMenuItem;
    miThemeDark: TMenuItem;
    miThemeWindows: TMenuItem;
    miViewSep1: TMenuItem;
    miDensityDesktop: TMenuItem;
    miDensityTablet: TMenuItem;
    miViewSep2: TMenuItem;
    miUnitsMetric: TMenuItem;
    miUnitsImperial: TMenuItem;
    miViewSep3: TMenuItem;
    miMenuBar: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    pmDashboard: TOBDPopupMenu;
    pmiEditLayout: TMenuItem;
    pmiAddTile: TMenuItem;
    pmiSep: TMenuItem;
    pmiSaveLayout: TMenuItem;
    pmiLoadLayout: TMenuItem;
    pmiResetLayout: TMenuItem;
    tmrConnect: TTimer;
    tmrSimulation: TTimer;
    tmrReadCodes: TTimer;
    TitleBar: TOBDTitleBar;
    rbnMain: TOBDRibbon;
    sbMain: TOBDStatusBar;
    pnlBody: TPanel;
    sdbMain: TOBDSidebar;
    pnlPages: TPanel;
    cbConnection: TOBDConnectionBar;
    pgDashboard: TPanel;
    tbDashboard: TOBDToolBar;
    dshMain: TOBDDashboard;
    dlEngineSpeed: TOBDDialGauge;
    dlVehicleSpeed: TOBDDialGauge;
    barCoolant: TOBDBarGauge;
    barEngineLoad: TOBDBarGauge;
    tileBattery: TOBDValueTile;
    tileEgrError: TOBDValueTile;
    tileMaf: TOBDValueTile;
    barBoost: TOBDBarGauge;
    tileFuel: TOBDValueTile;
    lampMIL: TOBDStatusLamp;
    chtDashboard: TOBDTrendChart;
    mxTicker: TOBDMatrixDisplay;
    pgLiveData: TPanel;
    tbLive: TOBDToolBar;
    sbHistory: TOBDScrollBar;
    chtLive: TOBDTrendChart;
    grdLive: TOBDLiveDataGrid;
    pgCodes: TPanel;
    bnrCodes: TOBDBanner;
    pnlDtc: TOBDDtcPanel;
    pgFreezeFrame: TPanel;
    ffView: TOBDFreezeFrameView;
    pgReadiness: TPanel;
    rdPanel: TOBDReadinessPanel;
    pgVehicle: TPanel;
    vcVehicle: TOBDVehicleInfoCard;
    cdJob: TOBDCard;
    edCustomer: TOBDEdit;
    edPlate: TOBDEdit;
    edVin: TOBDEdit;
    btnDecodeVin: TOBDButton;
    cbJobType: TOBDComboBox;
    segFuel: TOBDSegmented;
    chkRoadTest: TOBDCheckBox;
    chkCustomerWaits: TOBDCheckBox;
    rbPriorityNormal: TOBDRadioButton;
    rbPriorityUrgent: TOBDRadioButton;
    chpJobStatus: TOBDChip;
    chpMil: TOBDChip;
    bdgCodes: TOBDBadge;
    rbrCharging: TOBDRangeBar;
    pbJob: TOBDProgressBar;
    btnStartScan: TOBDButton;
    btnCancelJob: TOBDButton;
    pgLog: TPanel;
    tabsLog: TOBDTabs;
    lvLog: TOBDLogViewer;
    trmAdapter: TOBDTerminal;
    pgSettings: TPanel;
    insSettings: TOBDInspector;
    rngEditor: TOBDRangeEditor;
    bsMain: TOBDBackstage;
    rpReport: TOBDReportPreview;
    cdAbout: TOBDCard;
    bnrAbout: TOBDBanner;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OBDThemeChange(Sender: TObject);
    procedure tmrConnectTimer(Sender: TObject);
    procedure tmrSimulationTimer(Sender: TObject);
    procedure tmrReadCodesTimer(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actReadCodesExecute(Sender: TObject);
    procedure actClearCodesExecute(Sender: TObject);
    procedure actEditLayoutExecute(Sender: TObject);
    procedure actAddTileExecute(Sender: TObject);
    procedure actSaveLayoutExecute(Sender: TObject);
    procedure actLoadLayoutExecute(Sender: TObject);
    procedure actResetLayoutExecute(Sender: TObject);
    procedure actPauseTrendExecute(Sender: TObject);
    procedure actResetMinMaxExecute(Sender: TObject);
    procedure actReportExecute(Sender: TObject);
    procedure actThemeExecute(Sender: TObject);
    procedure actDensityExecute(Sender: TObject);
    procedure actUnitsExecute(Sender: TObject);
    procedure actMenuBarExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure TitleBarButtonClick(Sender: TObject;
      AButton: TOBDCaptionButton);
    procedure TitleBarCommandStyleChange(Sender: TObject);
    procedure rbnMainItemClick(Sender: TObject; AItem: TOBDRibbonItem);
    procedure sdbMainChange(Sender: TObject);
    procedure sbMainPanelClick(Sender: TObject; Panel: TOBDStatusPanel);
    procedure dshMainEditModeChanged(Sender: TObject);
    procedure dshMainEmptyCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure mxTickerPassComplete(Sender: TObject);
    procedure tbLiveClearClick(Sender: TObject);
    procedure grdLiveCheckChanged(Sender: TObject; APID: Byte;
      AChecked: Boolean);
    procedure chtLivePauseChanged(Sender: TObject);
    procedure sbHistoryChange(Sender: TObject);
    procedure pnlDtcReadCodes(Sender: TObject);
    procedure pnlDtcClearCodes(Sender: TObject);
    procedure ffViewEditRanges(Sender: TObject);
    procedure btnDecodeVinClick(Sender: TObject);
    procedure btnStartScanClick(Sender: TObject);
    procedure btnCancelJobClick(Sender: TObject);
    procedure tabsLogChange(Sender: TObject);
    procedure insSettingsPropertyChanged(Sender: TObject;
      AProperty: TOBDInspectorProperty);
    procedure rngEditorSave(Sender: TObject);
    procedure rngEditorCancel(Sender: TObject);
    procedure rpReportPaintPage(Sender: TObject; ACanvas: TCanvas;
      const APaperRect: TRect; APageIndex: Integer; AScale: Single);
    procedure bsExitClick(Sender: TObject);
  private
    { Private declarations }
    FSim: TEngineSimulator;
    FPages: TArray<TPanel>;
    FConnected: Boolean;
    FCodesRead: Boolean;
    FCodesCleared: Boolean;
    FEgrWarned: Boolean;
    FReadProgress: Integer;
    FPollIndex: Integer;
    FTicks: Integer;
    FDefaultLayout: string;
    FHistoryMaxPos: Integer;
    FHistoryOffsetMs: Int64;
    FSyncing: Boolean;
    FGarageName: string;
    function DataFolder: string;
    function LayoutFile: string;
    function RangesFile: string;
    function IsDark: Boolean;
    function MilOn: Boolean;
    function TickerText: string;
    procedure ShowPage(AIndex: Integer);
    procedure ApplyChrome;
    procedure SyncCommands;
    procedure SyncSettings;
    procedure SetSetting(const AName, AValue: string);
    procedure SetTrendChannel(APID: Byte; AVisible: Boolean);
    procedure AddValueTile(ACol, ARow: Integer);
    procedure PushDashboard;
    procedure PushLiveData;
    procedure PollAdapter;
    procedure PopulateVehicle;
    procedure PopulateFreezeFrame;
    procedure PopulateReadiness;
    procedure PopulateCodes;
    procedure PopulateSettings;
    procedure ClearCodes;
    procedure UpdateCodeIndicators;
    procedure SetJobStep(AIndex: Integer);
    procedure ToastShowCodes(Sender: TObject);
    procedure ToastShowFreezeFrame(Sender: TObject);
    procedure ToastConnect(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
  VEHICLE_VIN = 'WVWZZZAUZGW123456';
  ECU_ENGINE = 'Engine (7E8)';
  PROTOCOL_TEXT = 'ISO 15765-4 CAN (11 bit, 500 kbaud)';
  PROTOCOL_COMPACT = 'CAN 11/500';
  DEG_C = #176'C';

  PAGE_DASHBOARD = 0;
  PAGE_LIVE_DATA = 1;
  PAGE_CODES = 2;
  PAGE_FREEZE_FRAME = 3;
  PAGE_READINESS = 4;
  PAGE_VEHICLE = 5;
  PAGE_LOG = 6;
  PAGE_SETTINGS = 7;

  // Ribbon items with a Tag of PAGE_TAG + n open page n.
  PAGE_TAG = 100;

  // Status bar panels (see sbMain.Panels in the DFM).
  SP_LINK = 0;
  SP_PROTOCOL = 1;
  SP_PROGRESS = 2;
  SP_UNITS = 4;
  SP_THEME = 5;

  // Backstage items (see bsMain.Items in the DFM).
  BS_REPORT = 0;
  BS_ABOUT = 1;

  // Workshop job steps (see pbJob.Steps in the DFM).
  STEP_CONNECT = 1;
  STEP_READ = 2;
  STEP_DIAGNOSE = 3;
  STEP_REPAIR = 4;

  INSPECTIONS: array [TOBDInspectionRegime] of string = ('Generic OBD-II',
    'APK (NL)', 'Keuring (BE)', 'Controle technique (FR)', 'MOT (UK)',
    'HU/AU (DE)', 'NCT (IE)', 'Custom');

  PAPER_INK = TColor($001A1A1A);
  PAPER_MUTED = TColor($005C636A);
  PAPER_RULE = TColor($00E6E2E2);

{ Start-up / shut-down ------------------------------------------------------- }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FSim := TEngineSimulator.Create;
  FGarageName := 'Garage Peeters';
  FPages := [pgDashboard, pgLiveData, pgCodes, pgFreezeFrame, pgReadiness,
    pgVehicle, pgLog, pgSettings];

  // Controls created at run time (toasts, dialogs, dashboard tiles the
  // user adds) pick this theme up as well.
  TOBDTheme.RegisterDefault(OBDTheme);

  if TFile.Exists(RangesFile) then
    RangeProfile.LoadFromFile(RangesFile)
  else
    RangeProfile.LoadDefaults;
  // Remember the designed dashboard so "Reset layout" can restore it.
  FDefaultLayout := dshMain.SaveLayout;

  for I := Low(SIM_PIDS) to High(SIM_PIDS) do
    grdLive.AddPID(SIM_PIDS[I].PID, SIM_PIDS[I].Name);
  // Checked rows are plotted in the chart (see grdLiveCheckChanged).
  grdLive.SetChecked(PID_ENGINE_SPEED, True);
  grdLive.SetChecked(PID_COMMANDED_EGR, True);
  grdLive.SetChecked(PID_EGR_ERROR, True);

  FSyncing := True;
  try
    sbHistory.Max := chtLive.HistorySec;
    sbHistory.PageSize := chtLive.TimeWindowSec;
    sbHistory.Position := sbHistory.Max;
    FHistoryMaxPos := sbHistory.Position;
  finally
    FSyncing := False;
  end;

  PopulateVehicle;
  PopulateFreezeFrame;
  PopulateReadiness;
  PopulateSettings;
  UpdateCodeIndicators;
  SetJobStep(STEP_CONNECT);

  lvLog.Info('OBD Studio started');
  lvLog.Info('Range profile "' + RangeProfile.ProfileName + '" loaded (' +
    IntToStr(RangeProfile.Ranges.Count) + ' ranges)');

  ShowPage(PAGE_DASHBOARD);
  ApplyChrome;
  SyncCommands;
  SyncSettings;
  actConnect.Execute;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  tmrConnect.Enabled := False;
  tmrSimulation.Enabled := False;
  tmrReadCodes.Enabled := False;
  FreeAndNil(FSim);
end;

function TfrmMain.DataFolder: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'OBD Studio');
end;

function TfrmMain.LayoutFile: string;
begin
  Result := TPath.Combine(DataFolder, 'dashboard.json');
end;

function TfrmMain.RangesFile: string;
begin
  Result := TPath.Combine(DataFolder, 'ranges.json');
end;

function TfrmMain.IsDark: Boolean;
begin
  Result := OBDIsDarkPalette(OBDTheme.Palette);
end;

function TfrmMain.MilOn: Boolean;
begin
  // The sticking EGR valve has set P0401 and the check engine light,
  // until the codes are cleared.
  Result := not FCodesCleared;
end;

{ Navigation and chrome ----------------------------------------------------- }

procedure TfrmMain.ShowPage(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex > High(FPages)) then
    Exit;
  FPages[AIndex].Visible := True;
  for I := 0 to High(FPages) do
    if I <> AIndex then
      FPages[I].Visible := False;
  FSyncing := True;
  try
    for I := 0 to sdbMain.Items.Count - 1 do
      if sdbMain.Items[I].Tag = AIndex then
        sdbMain.ItemIndex := I;
  finally
    FSyncing := False;
  end;
  if bsMain.Visible then
    bsMain.Close;
end;

procedure TfrmMain.sdbMainChange(Sender: TObject);
begin
  if FSyncing or (sdbMain.ItemIndex < 0) then
    Exit;
  ShowPage(sdbMain.Items[sdbMain.ItemIndex].Tag);
end;

procedure TfrmMain.rbnMainItemClick(Sender: TObject; AItem: TOBDRibbonItem);
begin
  if (AItem.Tag >= PAGE_TAG) and (AItem.Tag <= PAGE_TAG + High(FPages)) then
    ShowPage(AItem.Tag - PAGE_TAG);
end;

procedure TfrmMain.ApplyChrome;
var
  P: TOBDThemePalette;
  I: Integer;
begin
  // The OBD controls paint themselves; the plain VCL panels that hold
  // the pages take the theme's background colour.
  P := OBDTheme.Palette;
  Color := P.Background;
  pnlBody.Color := P.Background;
  pnlPages.Color := P.Background;
  for I := 0 to High(FPages) do
    FPages[I].Color := P.Background;
  // The non-visual components have their own density.
  OBDToasts.Density := OBDTheme.Density;
  dlgConfirm.Density := OBDTheme.Density;
  OBDHintStyle.Density := OBDTheme.Density;
  pmDashboard.Density := OBDTheme.Density;
end;

procedure TfrmMain.OBDThemeChange(Sender: TObject);
begin
  ApplyChrome;
  SyncCommands;
  SyncSettings;
  rpReport.Invalidate;
end;

procedure TfrmMain.SyncCommands;
const
  UNIT_NAMES: array [TOBDUnitSystem] of string = ('Metric', 'Imperial');
var
  I: Integer;
begin
  // Ribbon and tool bar items show their action's Checked state as Down.
  actThemeLight.Checked := OBDTheme.Mode = tmLight;
  actThemeDark.Checked := OBDTheme.Mode = tmDark;
  actThemeWindows.Checked := OBDTheme.Mode in [tmAuto, tmWindows];
  actDensityDesktop.Checked := OBDTheme.Density = dnDesktop;
  actDensityTablet.Checked := OBDTheme.Density = dnTablet;
  actUnitsMetric.Checked := OBDTheme.UnitSystem = usMetric;
  actUnitsImperial.Checked := OBDTheme.UnitSystem = usImperial;
  actEditLayout.Checked := dshMain.EditMode;
  actPauseTrend.Checked := chtLive.Paused;
  actMenuBar.Checked := TitleBar.CommandStyle = csMenu;

  actConnect.Enabled := not FConnected and not tmrConnect.Enabled;
  actDisconnect.Enabled := FConnected or tmrConnect.Enabled;
  actReadCodes.Enabled := FConnected and not tmrReadCodes.Enabled;
  actClearCodes.Enabled := FConnected and FCodesRead and MilOn and
    not tmrReadCodes.Enabled;

  // The layout tools are only shown while the dashboard is edited.
  if rbnMain.ContextualTabs.Count > 0 then
    rbnMain.ContextualTabs[0].Visible := dshMain.EditMode;

  for I := 0 to TitleBar.Buttons.Count - 1 do
    if TitleBar.Buttons[I].Name = 'theme' then
    begin
      if IsDark then
      begin
        TitleBar.Buttons[I].Glyph := glSun;
        TitleBar.Buttons[I].Hint := 'Switch to the light theme';
      end
      else
      begin
        TitleBar.Buttons[I].Glyph := glMoon;
        TitleBar.Buttons[I].Hint := 'Switch to the dark theme';
      end;
    end
    else if TitleBar.Buttons[I].Name = 'density' then
      TitleBar.Buttons[I].Down := OBDTheme.Density = dnTablet;

  sbMain.Panels[SP_UNITS].Text := UNIT_NAMES[OBDTheme.UnitSystem];
  if IsDark then
    sbMain.Panels[SP_THEME].Text := 'Dark theme'
  else
    sbMain.Panels[SP_THEME].Text := 'Light theme';
end;

procedure TfrmMain.TitleBarButtonClick(Sender: TObject;
  AButton: TOBDCaptionButton);
begin
  if AButton.Name = 'theme' then
  begin
    if IsDark then
      OBDTheme.Mode := tmLight
    else
      OBDTheme.Mode := tmDark;
  end
  else if AButton.Name = 'density' then
  begin
    if OBDTheme.Density = dnDesktop then
      OBDTheme.Density := dnTablet
    else
      OBDTheme.Density := dnDesktop;
  end
  else if AButton.Name = 'notifications' then
  begin
    AButton.Badge := '';
    if FCodesRead and MilOn then
      OBDToasts.Show(tkWarning, 'P0401 stored',
        'EGR flow insufficient. The check engine light is on.',
        'Show codes', ToastShowCodes)
    else
      OBDToasts.Show(tkInfo, 'No new notifications',
        'Everything is up to date.');
  end
  else if AButton.Name = 'help' then
    actAbout.Execute
  else if AButton.Name = 'save' then
    actSaveLayout.Execute
  else if AButton.Name = 'read' then
    actReadCodes.Execute
  else if AButton.Name = 'report' then
    actReport.Execute;
  SyncCommands;
end;

procedure TfrmMain.TitleBarCommandStyleChange(Sender: TObject);
begin
  if (TitleBar.CommandStyle = csMenu) and bsMain.Visible then
    bsMain.Close;
  SyncCommands;
  SyncSettings;
end;

procedure TfrmMain.sbMainPanelClick(Sender: TObject; Panel: TOBDStatusPanel);
begin
  case Panel.Index of
    SP_LINK:
      if not FConnected then
        actConnect.Execute;
    SP_UNITS:
      if OBDTheme.UnitSystem = usMetric then
        OBDTheme.UnitSystem := usImperial
      else
        OBDTheme.UnitSystem := usMetric;
    SP_THEME:
      if IsDark then
        OBDTheme.Mode := tmLight
      else
        OBDTheme.Mode := tmDark;
  end;
end;

{ View commands ------------------------------------------------------------- }

procedure TfrmMain.actThemeExecute(Sender: TObject);
begin
  // actThemeLight / actThemeDark / actThemeWindows have Tag 0 / 1 / 2.
  case TComponent(Sender).Tag of
    1:
      OBDTheme.Mode := tmDark;
    2:
      OBDTheme.Mode := tmWindows;
  else
    OBDTheme.Mode := tmLight;
  end;
  SyncCommands;
end;

procedure TfrmMain.actDensityExecute(Sender: TObject);
begin
  // actDensityDesktop / actDensityTablet have Tag 0 / 1.
  if TComponent(Sender).Tag = 1 then
    OBDTheme.Density := dnTablet
  else
    OBDTheme.Density := dnDesktop;
  SyncCommands;
end;

procedure TfrmMain.actUnitsExecute(Sender: TObject);
begin
  // actUnitsMetric / actUnitsImperial have Tag 0 / 1.
  if TComponent(Sender).Tag = 1 then
    OBDTheme.UnitSystem := usImperial
  else
    OBDTheme.UnitSystem := usMetric;
  SyncCommands;
end;

procedure TfrmMain.actMenuBarExecute(Sender: TObject);
begin
  if TitleBar.CommandStyle = csRibbon then
    TitleBar.CommandStyle := csMenu
  else
    TitleBar.CommandStyle := csRibbon;
end;

procedure TfrmMain.actReportExecute(Sender: TObject);
begin
  rpReport.PageIndex := 0;
  rpReport.Invalidate;
  bsMain.Show(BS_REPORT);
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  bsMain.Show(BS_ABOUT);
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.bsExitClick(Sender: TObject);
begin
  Close;
end;

{ Connection ---------------------------------------------------------------- }

procedure TfrmMain.actConnectExecute(Sender: TObject);
begin
  if FConnected or tmrConnect.Enabled then
    Exit;
  cbConnection.LinkState := lnkConnecting;
  cbConnection.AdapterText := 'Simulator (ELM327 v2.3)';
  cbConnection.ProtocolText := 'Searching...';
  sbMain.Panels[SP_LINK].Text := 'Connecting...';
  sbMain.Panels[SP_LINK].StatusKind := skWarning;
  TitleBar.StatusCaption := 'Connecting';
  TitleBar.StatusKind := skWarning;
  trmAdapter.LogInfo('Opening simulator adapter');
  trmAdapter.LogSent('ATZ');
  trmAdapter.LogReceived('ELM327 v2.3');
  trmAdapter.LogSent('ATE0');
  trmAdapter.LogReceived('OK');
  trmAdapter.LogSent('ATSP0');
  trmAdapter.LogReceived('OK');
  trmAdapter.LogSent('01 00');
  trmAdapter.LogInfo('SEARCHING...');
  lvLog.Info('Connecting to the vehicle');
  SetJobStep(STEP_CONNECT);
  tmrConnect.Enabled := True;
  SyncCommands;
end;

procedure TfrmMain.tmrConnectTimer(Sender: TObject);
begin
  tmrConnect.Enabled := False;
  FConnected := True;
  FEgrWarned := False;
  trmAdapter.LogReceived('41 00 98 3B A0 13');
  trmAdapter.LogSent('01 01');
  if MilOn then
    trmAdapter.LogReceived('41 01 81 07 65 00')
  else
    trmAdapter.LogReceived('41 01 00 07 65 00');

  cbConnection.LinkState := lnkConnected;
  cbConnection.ProtocolText := PROTOCOL_COMPACT;
  cbConnection.VIN := VEHICLE_VIN;
  sbMain.Panels[SP_LINK].Text := 'Connected';
  sbMain.Panels[SP_LINK].StatusKind := skSuccess;
  sbMain.Panels[SP_PROTOCOL].Text := PROTOCOL_TEXT;
  TitleBar.StatusCaption := 'Connected';
  TitleBar.StatusKind := skSuccess;
  vcVehicle.Connected := True;
  vcVehicle.ScanTimeText := 'Connected at ' + FormatDateTime('hh:nn', Now);
  UpdateCodeIndicators;
  if not FCodesRead then
    SetJobStep(STEP_READ);

  tmrSimulation.Enabled := True;
  lvLog.Info('Connected: ' + PROTOCOL_TEXT + ', VIN ' + VEHICLE_VIN);
  if MilOn then
  begin
    lvLog.Warn('Check engine light (MIL) is on');
    OBDToasts.Show(tkSuccess, 'Connected to Golf VII 1.6 TDI',
      'The check engine light is on. Read the trouble codes to see why.',
      'Read codes', ToastShowCodes);
  end
  else
    OBDToasts.Show(tkSuccess, 'Connected to Golf VII 1.6 TDI', PROTOCOL_TEXT);
  SyncCommands;
end;

procedure TfrmMain.actDisconnectExecute(Sender: TObject);
begin
  tmrConnect.Enabled := False;
  tmrSimulation.Enabled := False;
  tmrReadCodes.Enabled := False;
  FConnected := False;
  trmAdapter.LogSent('ATPC');
  trmAdapter.LogReceived('OK');
  trmAdapter.LogInfo('Adapter closed');
  cbConnection.LinkState := lnkDisconnected;
  cbConnection.ProtocolText := '';
  sbMain.Panels[SP_LINK].Text := 'Disconnected';
  sbMain.Panels[SP_LINK].StatusKind := skNeutral;
  sbMain.Panels[SP_PROTOCOL].Text := '';
  sbMain.Panels[SP_PROGRESS].Visible := False;
  TitleBar.StatusCaption := 'Offline';
  TitleBar.StatusKind := skNeutral;
  vcVehicle.Connected := False;
  UpdateCodeIndicators;
  lvLog.Info('Disconnected');
  // The gauges keep their last value and show it as stale.
  OBDToasts.Show(tkInfo, 'Disconnected',
    'The adapter was closed. Values on screen are no longer updated.',
    'Connect', ToastConnect);
  SyncCommands;
end;

procedure TfrmMain.ToastShowCodes(Sender: TObject);
begin
  ShowPage(PAGE_CODES);
  if FConnected and not FCodesRead then
    actReadCodes.Execute;
end;

procedure TfrmMain.ToastShowFreezeFrame(Sender: TObject);
begin
  ShowPage(PAGE_FREEZE_FRAME);
end;

procedure TfrmMain.ToastConnect(Sender: TObject);
begin
  actConnect.Execute;
end;

{ Simulation ---------------------------------------------------------------- }

procedure TfrmMain.tmrSimulationTimer(Sender: TObject);
begin
  FSim.Step(tmrSimulation.Interval / 1000);
  Inc(FTicks);
  PushDashboard;
  PushLiveData;
  if FTicks mod 10 = 0 then
    PollAdapter;

  if not FEgrWarned and (FSim.Value(PID_EGR_ERROR) < -20) then
  begin
    FEgrWarned := True;
    lvLog.Warn(Format('EGR error %.1f %% at %.0f rpm: actual EGR flow is ' +
      'far below commanded', [FSim.Value(PID_EGR_ERROR),
      FSim.Value(PID_ENGINE_SPEED)]));
    // mxTicker is nil once a saved layout replaced the designed tiles.
    if mxTicker <> nil then
      mxTicker.ShowAlert('EGR FLOW LOW', alvWarning);
  end;
end;

procedure TfrmMain.PushDashboard;
var
  I, J: Integer;
  Ctl: TOBDCustomControl;
  Gauge: TOBDGaugeBase;
  Chart: TOBDTrendChart;
  Matrix: TOBDMatrixDisplay;
begin
  cbConnection.Battery.PushValue(FSim.Value(PID_MODULE_VOLTAGE));
  // Walk the tiles rather than the form fields: a layout loaded from
  // JSON replaces the designed tiles with new controls.
  for I := 0 to dshMain.Tiles.Count - 1 do
  begin
    Ctl := dshMain.Tiles[I].Control;
    if Ctl is TOBDGaugeBase then
    begin
      Gauge := TOBDGaugeBase(Ctl);
      if FSim.Has(Gauge.Channel.PID) then
        Gauge.Channel.PushValue(FSim.Value(Gauge.Channel.PID));
    end
    else if Ctl is TOBDTrendChart then
    begin
      Chart := TOBDTrendChart(Ctl);
      for J := 0 to Chart.Channels.Count - 1 do
        if FSim.Has(Chart.Channels[J].PID) then
          Chart.AddSample(J, FSim.Value(Chart.Channels[J].PID));
    end
    else if Ctl is TOBDStatusLamp then
    begin
      if MilOn then
        TOBDStatusLamp(Ctl).State := lstAlarm
      else
        TOBDStatusLamp(Ctl).State := lstOk;
    end
    else if Ctl is TOBDMatrixDisplay then
    begin
      Matrix := TOBDMatrixDisplay(Ctl);
      if not Assigned(Matrix.OnPassComplete) then
        Matrix.OnPassComplete := mxTickerPassComplete;
    end;
  end;
end;

procedure TfrmMain.PushLiveData;
var
  I: Integer;
begin
  for I := Low(SIM_PIDS) to High(SIM_PIDS) do
    grdLive.PushValue(SIM_PIDS[I].PID, FSim.Value(SIM_PIDS[I].PID),
      SIM_PIDS[I].UnitText);
  for I := 0 to chtLive.Channels.Count - 1 do
    if FSim.Has(chtLive.Channels[I].PID) then
      chtLive.AddSample(I, FSim.Value(chtLive.Channels[I].PID));
  if FTicks mod 5 = 0 then
  begin
    for I := Low(SIM_PIDS) to High(SIM_PIDS) do
      ffView.SetLive(SIM_PIDS[I].PID, FSim.Value(SIM_PIDS[I].PID));
    rbrCharging.Value := FSim.Value(PID_MODULE_VOLTAGE);
  end;
end;

procedure TfrmMain.PollAdapter;
var
  PID: Byte;
begin
  // What the adapter traffic behind the live data looks like.
  PID := SIM_PIDS[FPollIndex mod Length(SIM_PIDS)].PID;
  Inc(FPollIndex);
  trmAdapter.LogSent('01 ' + IntToHex(PID, 2));
  trmAdapter.LogReceived(FSim.Response(PID));
end;

function TfrmMain.TickerText: string;
var
  Speed, Coolant: Double;
  SpeedUnit, TempUnit: string;
begin
  Speed := FSim.Value(PID_VEHICLE_SPEED);
  Coolant := FSim.Value(PID_COOLANT);
  if OBDTheme.UnitSystem = usImperial then
  begin
    Speed := Speed * 0.621371;
    Coolant := Coolant * 1.8 + 32;
    SpeedUnit := 'MPH';
    TempUnit := 'F';
  end
  else
  begin
    SpeedUnit := 'KM/H';
    TempUnit := 'C';
  end;
  Result := Format('GOLF 1.6 TDI   %.0f RPM   %.0f %s   COOLANT %.0f %s   ' +
    'EGR ERROR %.0f %%   %.1f V', [FSim.Value(PID_ENGINE_SPEED), Speed,
    SpeedUnit, Coolant, TempUnit, FSim.Value(PID_EGR_ERROR),
    FSim.Value(PID_MODULE_VOLTAGE)]);
  if FCodesRead and MilOn then
    Result := Result + '   CHECK ENGINE: P0401';
end;

procedure TfrmMain.mxTickerPassComplete(Sender: TObject);
begin
  if FConnected then
    TOBDMatrixDisplay(Sender).Text := TickerText;
end;

{ Dashboard ----------------------------------------------------------------- }

procedure TfrmMain.actEditLayoutExecute(Sender: TObject);
begin
  ShowPage(PAGE_DASHBOARD);
  dshMain.EditMode := not dshMain.EditMode;
end;

procedure TfrmMain.dshMainEditModeChanged(Sender: TObject);
begin
  SyncCommands;
  // The contextual "Layout" tab follows the normal tabs.
  if dshMain.EditMode and (TitleBar.CommandStyle = csRibbon) then
    rbnMain.ActiveTab := rbnMain.Tabs.Count
  else if not dshMain.EditMode and
    (rbnMain.ActiveTab >= rbnMain.Tabs.Count) then
    rbnMain.ActiveTab := 0;
  if dshMain.EditMode then
    lvLog.Info('Dashboard edit mode: drag tiles to move, drag the corner ' +
      'to resize, click an empty cell to add a tile');
end;

procedure TfrmMain.actSaveLayoutExecute(Sender: TObject);
begin
  ForceDirectories(DataFolder);
  dshMain.SaveLayoutToFile(LayoutFile);
  lvLog.Info('Dashboard layout saved to ' + LayoutFile);
  OBDToasts.Show(tkSuccess, 'Layout saved', LayoutFile);
end;

procedure TfrmMain.actLoadLayoutExecute(Sender: TObject);
begin
  if not TFile.Exists(LayoutFile) then
  begin
    OBDToasts.Show(tkWarning, 'No saved layout',
      'Save the dashboard layout first (Ctrl+S).');
    Exit;
  end;
  dshMain.LoadLayoutFromFile(LayoutFile);
  lvLog.Info('Dashboard layout loaded from ' + LayoutFile);
  OBDToasts.Show(tkSuccess, 'Layout loaded', LayoutFile);
end;

procedure TfrmMain.actResetLayoutExecute(Sender: TObject);
begin
  dlgConfirm.Kind := dkConfirm;
  dlgConfirm.Title := 'Reset the dashboard?';
  dlgConfirm.Text := 'All tiles go back to the original layout. ' +
    'Tiles you added are removed.';
  dlgConfirm.Buttons := [mbYes, mbNo];
  dlgConfirm.ButtonCaptions.Text := 'mbYes=Reset layout' + sLineBreak +
    'mbNo=Keep my layout';
  dlgConfirm.DefaultButton := mbNo;
  dlgConfirm.DangerButton := mbYes;
  dlgConfirm.CheckBoxCaption := '';
  if dlgConfirm.Execute <> mrYes then
    Exit;
  dshMain.LoadLayout(FDefaultLayout);
  lvLog.Info('Dashboard layout reset');
end;

procedure TfrmMain.actAddTileExecute(Sender: TObject);
var
  C, R: Integer;
begin
  ShowPage(PAGE_DASHBOARD);
  if not dshMain.FindFree(1, 1, C, R) then
  begin
    // Grow the grid by a row rather than refusing.
    dshMain.Rows := dshMain.Rows + 1;
    if not dshMain.FindFree(1, 1, C, R) then
      Exit;
  end;
  AddValueTile(C, R);
end;

procedure TfrmMain.dshMainEmptyCellClick(Sender: TObject; ACol, ARow: Integer);
begin
  AddValueTile(ACol, ARow);
end;

procedure TfrmMain.AddValueTile(ACol, ARow: Integer);
var
  I, J, Pick: Integer;
  Used: Boolean;
  Ctl: TOBDCustomControl;
  Gauge: TOBDGaugeBase;
begin
  // Pick the first simulated PID that is not on the dashboard yet.
  Pick := 0;
  for I := Low(SIM_PIDS) to High(SIM_PIDS) do
  begin
    Used := False;
    for J := 0 to dshMain.Tiles.Count - 1 do
      if (dshMain.Tiles[J].Control is TOBDGaugeBase) and
        (TOBDGaugeBase(dshMain.Tiles[J].Control).Channel.PID =
        SIM_PIDS[I].PID) then
        Used := True;
    if not Used then
    begin
      Pick := I;
      Break;
    end;
  end;

  Ctl := dshMain.AddTile('value', ACol, ARow);
  if not (Ctl is TOBDGaugeBase) then
    Exit;
  Gauge := TOBDGaugeBase(Ctl);
  Gauge.Caption := SIM_PIDS[Pick].Name;
  Gauge.&Unit := SIM_PIDS[Pick].UnitText;
  Gauge.Min := SIM_PIDS[Pick].Min;
  Gauge.Max := SIM_PIDS[Pick].Max;
  Gauge.Decimals := SIM_PIDS[Pick].Decimals;
  Gauge.Channel.PID := SIM_PIDS[Pick].PID;
  lvLog.Info('Tile added: ' + SIM_PIDS[Pick].Name);
end;

{ Live data ----------------------------------------------------------------- }

procedure TfrmMain.SetTrendChannel(APID: Byte; AVisible: Boolean);
var
  I, Index: Integer;
  Channel: TOBDTrendChannel;
begin
  for I := chtLive.Channels.Count - 1 downto 0 do
    if chtLive.Channels[I].PID = APID then
    begin
      if not AVisible then
        chtLive.Channels.Delete(I);
      Exit;
    end;
  if not AVisible then
    Exit;
  Index := SimPIDIndex(APID);
  if Index < 0 then
    Exit;
  Channel := chtLive.AddChannel(SIM_PIDS[Index].Name, APID,
    SIM_PIDS[Index].Min, SIM_PIDS[Index].Max, SIM_PIDS[Index].UnitText);
  Channel.Decimals := SIM_PIDS[Index].Decimals;
end;

procedure TfrmMain.grdLiveCheckChanged(Sender: TObject; APID: Byte;
  AChecked: Boolean);
begin
  SetTrendChannel(APID, AChecked);
end;

procedure TfrmMain.actPauseTrendExecute(Sender: TObject);
begin
  chtLive.Paused := not chtLive.Paused;
end;

procedure TfrmMain.chtLivePauseChanged(Sender: TObject);
begin
  // Pausing or resuming starts at the newest sample again.
  FHistoryOffsetMs := 0;
  if not chtLive.Paused then
  begin
    FSyncing := True;
    try
      sbHistory.Position := FHistoryMaxPos;
    finally
      FSyncing := False;
    end;
  end;
  SyncCommands;
end;

procedure TfrmMain.sbHistoryChange(Sender: TObject);
var
  Wanted: Int64;
begin
  if FSyncing then
    Exit;
  // The scroll bar runs from the oldest history (left) to now (right).
  Wanted := Int64(FHistoryMaxPos - sbHistory.Position) * 1000;
  if (Wanted > 0) and not chtLive.Paused then
    chtLive.Paused := True;
  if chtLive.Paused then
  begin
    chtLive.ScrollBy(Wanted - FHistoryOffsetMs);
    FHistoryOffsetMs := Wanted;
  end;
end;

procedure TfrmMain.actResetMinMaxExecute(Sender: TObject);
var
  I: Integer;
begin
  grdLive.ResetMinMax;
  for I := 0 to dshMain.Tiles.Count - 1 do
    if dshMain.Tiles[I].Control is TOBDGaugeBase then
      TOBDGaugeBase(dshMain.Tiles[I].Control).ResetMinMax;
  lvLog.Info('Minimum / maximum values reset');
end;

procedure TfrmMain.tbLiveClearClick(Sender: TObject);
begin
  chtLive.ClearSamples;
end;

{ Trouble codes ------------------------------------------------------------- }

procedure TfrmMain.actReadCodesExecute(Sender: TObject);
begin
  if not FConnected then
  begin
    OBDToasts.Show(tkWarning, 'Not connected',
      'Connect to the vehicle before reading codes.', 'Connect',
      ToastConnect);
    Exit;
  end;
  if tmrReadCodes.Enabled then
    Exit;
  FReadProgress := 0;
  sbMain.Panels[SP_PROGRESS].Text := 'Reading codes';
  sbMain.Panels[SP_PROGRESS].Position := 0;
  sbMain.Panels[SP_PROGRESS].Visible := True;
  lvLog.Info('Reading stored, pending and permanent codes');
  trmAdapter.LogSent('03');
  tmrReadCodes.Enabled := True;
  SyncCommands;
end;

procedure TfrmMain.tmrReadCodesTimer(Sender: TObject);
begin
  Inc(FReadProgress, 5);
  sbMain.Panels[SP_PROGRESS].Position := System.Math.Min(FReadProgress, 100);
  case FReadProgress of
    35:
      begin
        if MilOn then
          trmAdapter.LogReceived('43 01 04 01')
        else
          trmAdapter.LogReceived('43 00');
        trmAdapter.LogSent('07');
      end;
    70:
      begin
        if MilOn then
          trmAdapter.LogReceived('47 01 20 02')
        else
          trmAdapter.LogReceived('47 00');
        trmAdapter.LogSent('0A');
      end;
  end;
  if FReadProgress < 100 then
    Exit;

  tmrReadCodes.Enabled := False;
  trmAdapter.LogReceived('4A 01 04 01');
  sbMain.Panels[SP_PROGRESS].Visible := False;
  PopulateCodes;
  lvLog.Warn(Format('%d trouble code(s) read', [pnlDtc.Count]));
  if MilOn then
  begin
    OBDToasts.Show(tkWarning, 'P0401: EGR flow insufficient',
      'Stored, pending and permanent codes were read. The freeze frame ' +
      'shows the conditions when P0401 was set.', 'Freeze frame',
      ToastShowFreezeFrame);
    SetJobStep(STEP_DIAGNOSE);
  end
  else
    OBDToasts.Show(tkInfo, 'Codes read',
      'Only the permanent code is left. The ECU removes it once the EGR ' +
      'monitor has passed.');
  SyncCommands;
end;

procedure TfrmMain.PopulateCodes;
const
  DESC_P0401 = 'Exhaust gas recirculation flow insufficient detected';
begin
  pnlDtc.BeginUpdate;
  try
    pnlDtc.Clear;
    if MilOn then
    begin
      pnlDtc.AddCode('P0401', DESC_P0401, 'Powertrain', ECU_ENGINE, dsStored,
        ['Engine speed=2140 rpm', 'Calculated load=62.4 %',
        'Coolant=84 ' + DEG_C, 'Vehicle speed=78 km/h',
        'Commanded EGR=38.0 %', 'EGR error=-31.5 %',
        'Intake pressure=152 kPa', 'Module voltage=14.1 V']);
      pnlDtc.AddCode('P2002',
        'Diesel particulate filter efficiency below threshold (bank 1)',
        'Powertrain', ECU_ENGINE, dsPending);
    end;
    // Permanent codes cannot be cleared with a scan tool: the ECU
    // removes them once the monitor has run and passed.
    pnlDtc.AddCode('P0401', DESC_P0401, 'Powertrain', ECU_ENGINE,
      dsPermanent);
  finally
    pnlDtc.EndUpdate;
  end;
  pnlDtc.LastRead := Now;
  FCodesRead := True;
  UpdateCodeIndicators;
end;

procedure TfrmMain.actClearCodesExecute(Sender: TObject);
begin
  dlgConfirm.Kind := dkDanger;
  dlgConfirm.Title := 'Clear trouble codes?';
  dlgConfirm.Text := 'This clears the stored and pending codes and the ' +
    'freeze frame, and turns the check engine light off. All readiness ' +
    'monitors are reset: the vehicle cannot pass an emissions inspection ' +
    'until it has been driven.';
  dlgConfirm.Buttons := [mbYes, mbCancel];
  dlgConfirm.ButtonCaptions.Text := 'mbYes=Clear codes';
  dlgConfirm.DefaultButton := mbCancel;
  dlgConfirm.DangerButton := mbYes;
  dlgConfirm.CheckBoxCaption := 'The codes are saved in the job report';
  dlgConfirm.CheckBoxChecked := True;
  if dlgConfirm.Execute = mrYes then
    ClearCodes;
end;

procedure TfrmMain.ClearCodes;
begin
  if not FConnected then
    Exit;
  trmAdapter.LogSent('04');
  trmAdapter.LogReceived('44');
  FCodesCleared := True;
  PopulateCodes;
  PopulateReadiness;
  ffView.Clear;
  ffView.DtcCode := '';
  ffView.Title := 'No freeze frame stored';
  ffView.Subtitle := 'The freeze frame was erased together with the codes.';
  ffView.HeaderInfo := '';
  lvLog.Warn('Trouble codes cleared, readiness monitors reset');
  OBDToasts.Show(tkSuccess, 'Codes cleared',
    'Check engine light off. Permanent code P0401 stays until the EGR ' +
    'monitor passes.');
  SetJobStep(STEP_REPAIR);
  SyncCommands;
end;

procedure TfrmMain.pnlDtcReadCodes(Sender: TObject);
begin
  actReadCodes.Execute;
end;

procedure TfrmMain.pnlDtcClearCodes(Sender: TObject);
begin
  // The panel already asked for confirmation (ConfirmClear).
  ClearCodes;
end;

procedure TfrmMain.UpdateCodeIndicators;
var
  I, Count: Integer;
begin
  if FCodesRead then
    Count := pnlDtc.Count
  else
    Count := 0;

  for I := 0 to sdbMain.Items.Count - 1 do
    if sdbMain.Items[I].Tag = PAGE_CODES then
      sdbMain.Items[I].Badge := Count;
  for I := 0 to TitleBar.Buttons.Count - 1 do
    if TitleBar.Buttons[I].Name = 'notifications' then
    begin
      if FCodesRead and MilOn then
        TitleBar.Buttons[I].Badge := '1'
      else
        TitleBar.Buttons[I].Badge := '';
    end;

  bdgCodes.Count := Count;
  vcVehicle.MilOn := MilOn;
  rdPanel.MilOn := MilOn;
  if MilOn then
  begin
    rdPanel.DtcCount := 1;
    chpMil.Caption := 'MIL on';
    chpMil.Kind := skDanger;
  end
  else
  begin
    rdPanel.DtcCount := 0;
    chpMil.Caption := 'MIL off';
    chpMil.Kind := skSuccess;
  end;

  if not FCodesRead then
  begin
    bnrCodes.Kind := bnInfo;
    bnrCodes.AlertIcon := False;
    bnrCodes.Title := 'Codes not read yet';
    bnrCodes.Text := 'Connect to the vehicle and press Read codes (F5).';
  end
  else if MilOn then
  begin
    bnrCodes.Kind := bnWarning;
    bnrCodes.AlertIcon := True;
    bnrCodes.Title := 'Check engine light is on';
    bnrCodes.Text := 'P0401: EGR flow insufficient. Live data confirms it: ' +
      'the EGR error stays far below zero while EGR is commanded. Inspect ' +
      'the EGR valve and cooler for carbon build-up.';
  end
  else
  begin
    bnrCodes.Kind := bnSuccess;
    bnrCodes.AlertIcon := False;
    bnrCodes.Title := 'Stored and pending codes cleared';
    bnrCodes.Text := 'Permanent code P0401 stays until the EGR monitor ' +
      'has run and passed. Readiness was reset: drive the vehicle before ' +
      'the inspection.';
  end;
end;

{ Freeze frame / readiness / vehicle ----------------------------------------- }

procedure TfrmMain.PopulateFreezeFrame;
begin
  ffView.BeginUpdate;
  try
    ffView.Clear;
    ffView.DtcCode := 'P0401';
    ffView.Title := 'Exhaust gas recirculation flow insufficient';
    ffView.Subtitle := 'Freeze frame 0 - ' + ECU_ENGINE;
    ffView.HeaderInfo := '128 310 km - 2 days ago';
    ffView.AddValue(PID_ENGINE_SPEED, 'engine_speed', 'Engine speed', 2140,
      'rpm', 0);
    ffView.AddValue(PID_ENGINE_LOAD, 'calculated_load', 'Calculated load',
      62.4, '%', 1);
    ffView.AddValue(PID_COOLANT, 'coolant_temp', 'Coolant temperature', 84,
      DEG_C, 0);
    ffView.AddValue(PID_VEHICLE_SPEED, 'vehicle_speed', 'Vehicle speed', 78,
      'km/h', 0);
    ffView.AddValue(PID_MAP, 'intake_map', 'Intake manifold pressure', 152,
      'kPa', 0);
    ffView.AddValue(0, 'boost_desired', 'Boost desired pressure', 168,
      'kPa', 0);
    ffView.AddValue(PID_INTAKE_TEMP, 'intake_air_temp',
      'Intake air temperature', 31, DEG_C, 0);
    ffView.AddValue(PID_MAF, 'maf', 'Mass air flow', 18.4, 'g/s', 1);
    ffView.AddValue(PID_COMMANDED_EGR, 'commanded_egr', 'Commanded EGR', 38,
      '%', 1);
    ffView.AddValue(PID_EGR_ERROR, 'egr_error', 'EGR error', -31.5, '%', 1);
    ffView.AddValue(PID_DPF_DP, 'dpf_dp', 'DPF differential pressure', 4.2,
      'kPa', 1);
    ffView.AddValue(PID_MODULE_VOLTAGE, 'control_module_voltage',
      'Control module voltage', 14.1, 'V', 1);
    ffView.AddText(PID_FUEL_SYSTEM, 'fuel_system_status',
      'Fuel system status', 'Closed loop');
  finally
    ffView.EndUpdate;
  end;
end;

procedure TfrmMain.ffViewEditRanges(Sender: TObject);
begin
  ShowPage(PAGE_SETTINGS);
end;

procedure TfrmMain.PopulateReadiness;
begin
  rdPanel.BeginUpdate;
  try
    rdPanel.Clear;
    rdPanel.CompressionIgnition := True;
    rdPanel.SetMonitor(rmMisfire, msComplete);
    rdPanel.SetMonitor(rmFuelSystem, msComplete);
    rdPanel.SetMonitor(rmComponents, msComplete);
    if FCodesCleared then
    begin
      // Clearing codes resets every non-continuous monitor.
      rdPanel.SetMonitor(rmNMHCCatalyst, msIncomplete);
      rdPanel.SetMonitor(rmNOxSCRAftertreatment, msIncomplete);
      rdPanel.SetMonitor(rmBoostPressure, msIncomplete);
      rdPanel.SetMonitor(rmExhaustGasSensor, msIncomplete);
      rdPanel.SetMonitor(rmPMFilter, msIncomplete);
      rdPanel.SetMonitor(rmEGRVVTSystem, msIncomplete,
        'Repair the EGR valve, then drive to run this monitor');
      rdPanel.DistanceSinceClear := 0;
      rdPanel.WarmUpsSinceClear := 0;
    end
    else
    begin
      rdPanel.SetMonitor(rmNMHCCatalyst, msComplete);
      rdPanel.SetMonitor(rmNOxSCRAftertreatment, msComplete);
      rdPanel.SetMonitor(rmBoostPressure, msComplete);
      rdPanel.SetMonitor(rmExhaustGasSensor, msComplete);
      rdPanel.SetMonitor(rmPMFilter, msIncomplete,
        'Regeneration pending, see P2002');
      rdPanel.SetMonitor(rmEGRVVTSystem, msIncomplete,
        'Failed: insufficient flow (P0401)');
      rdPanel.DistanceSinceClear := 1840;
      rdPanel.WarmUpsSinceClear := 27;
    end;
    rdPanel.UpdatedText := 'Read at ' + FormatDateTime('hh:nn', Now);
  finally
    rdPanel.EndUpdate;
  end;
end;

procedure TfrmMain.PopulateVehicle;
begin
  vcVehicle.LoadFromVIN(VEHICLE_VIN);
  vcVehicle.Make := 'Volkswagen';
  vcVehicle.Model := 'Golf VII';
  vcVehicle.ModelYear := 2016;
  vcVehicle.BodyStyle := 'Hatchback';
  vcVehicle.Engine := '1.6 TDI';
  vcVehicle.FuelType := 'Diesel';
  vcVehicle.EngineCode := 'CLHA';
  vcVehicle.EnginePower := '81 kW (110 hp)';
  vcVehicle.Protocol := PROTOCOL_TEXT;
  vcVehicle.ProtocolCompact := PROTOCOL_COMPACT;
  vcVehicle.EcuCount := 3;
  vcVehicle.Odometer := '128 450 km';
  vcVehicle.CalibrationID := '04L906021DT 4519';
  edVin.Text := VEHICLE_VIN;
end;

procedure TfrmMain.btnDecodeVinClick(Sender: TObject);
var
  VIN: string;
begin
  VIN := UpperCase(Trim(edVin.Text));
  if Length(VIN) <> 17 then
  begin
    OBDToasts.Show(tkWarning, 'Invalid VIN',
      'A VIN has exactly 17 characters.');
    Exit;
  end;
  if VIN = VEHICLE_VIN then
    PopulateVehicle
  else
    vcVehicle.LoadFromVIN(VIN);
  if vcVehicle.VINCheckDigitValid then
    OBDToasts.Show(tkSuccess, 'VIN decoded', VIN)
  else
    OBDToasts.Show(tkInfo, 'VIN decoded', VIN + ' has no valid check ' +
      'digit, which is normal for most European vehicles.');
end;

procedure TfrmMain.SetJobStep(AIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to pbJob.Steps.Count - 1 do
    if I < AIndex then
      pbJob.Steps[I].State := stDone
    else if I = AIndex then
      pbJob.Steps[I].State := stActive
    else
      pbJob.Steps[I].State := stPending;
  if AIndex >= STEP_REPAIR then
  begin
    chpJobStatus.Caption := 'Road test needed';
    chpJobStatus.Kind := skWarning;
  end
  else
  begin
    chpJobStatus.Caption := 'In progress';
    chpJobStatus.Kind := skAccent;
  end;
end;

procedure TfrmMain.btnStartScanClick(Sender: TObject);
begin
  if not FConnected then
    actConnect.Execute
  else
  begin
    ShowPage(PAGE_CODES);
    actReadCodes.Execute;
  end;
end;

procedure TfrmMain.btnCancelJobClick(Sender: TObject);
begin
  dlgConfirm.Kind := dkWarning;
  dlgConfirm.Title := 'Cancel this job?';
  dlgConfirm.Text := 'The customer details and the job progress are ' +
    'cleared. Readings from the vehicle are kept in the log.';
  dlgConfirm.Buttons := [mbYes, mbNo];
  dlgConfirm.ButtonCaptions.Text := 'mbYes=Cancel job' + sLineBreak +
    'mbNo=Keep job';
  dlgConfirm.DefaultButton := mbNo;
  dlgConfirm.DangerButton := mbYes;
  dlgConfirm.CheckBoxCaption := '';
  if dlgConfirm.Execute <> mrYes then
    Exit;
  edCustomer.Text := '';
  edPlate.Text := '';
  cbJobType.ItemIndex := -1;
  chkRoadTest.Checked := False;
  chkCustomerWaits.Checked := False;
  rbPriorityNormal.Checked := True;
  SetJobStep(0);
  lvLog.Info('Job cancelled');
end;

{ Log ----------------------------------------------------------------------- }

procedure TfrmMain.tabsLogChange(Sender: TObject);
begin
  lvLog.Visible := tabsLog.TabIndex = 0;
  trmAdapter.Visible := tabsLog.TabIndex = 1;
end;

{ Settings ------------------------------------------------------------------ }

procedure TfrmMain.PopulateSettings;
var
  Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
  R: TOBDInspectionRegime;
begin
  insSettings.BeginUpdate;
  try
    insSettings.Clear;
    Cat := insSettings.AddCategory('Appearance');
    Prop := Cat.AddProperty('Theme', 'Light', ivPickList);
    Prop.PickList.Add('Light');
    Prop.PickList.Add('Dark');
    Prop.PickList.Add('Windows');
    Prop.Hint := 'Light, dark, or follow the Windows app mode';
    Prop := Cat.AddProperty('Density', 'Desktop', ivPickList);
    Prop.PickList.Add('Desktop');
    Prop.PickList.Add('Tablet');
    Prop.Hint := 'Tablet makes every control larger for touch screens';
    Prop := Cat.AddProperty('Units', 'Metric', ivPickList);
    Prop.PickList.Add('Metric');
    Prop.PickList.Add('Imperial');
    Prop := Cat.AddProperty('Commands', 'Ribbon', ivPickList);
    Prop.PickList.Add('Ribbon');
    Prop.PickList.Add('Menu bar');
    Prop := Cat.AddProperty('Notifications', 'In window', ivPickList);
    Prop.PickList.Add('In window');
    Prop.PickList.Add('On desktop');
    Prop.Hint := 'On desktop shows notifications in the screen corner, ' +
      'also when the window is not maximised';

    Cat := insSettings.AddCategory('Workshop');
    Cat.AddProperty('Garage name', FGarageName);
    Prop := Cat.AddProperty('Inspection',
      INSPECTIONS[rdPanel.InspectionRegime], ivPickList);
    for R := irGeneric to irNCT do
      Prop.PickList.Add(INSPECTIONS[R]);
    Prop.Hint := 'Rules applied on the readiness page';

    Cat := insSettings.AddCategory('Connection');
    Cat.AddProperty('Adapter', 'Simulator (ELM327 v2.3)', ivReadOnly);
    Cat.AddProperty('Protocol', PROTOCOL_TEXT, ivReadOnly);
    Prop := Cat.AddProperty('Refresh interval (ms)',
      IntToStr(tmrSimulation.Interval));
    Prop.Hint := '50 - 2000 ms';

    Cat := insSettings.AddCategory('Simulation');
    Prop := Cat.AddProperty('EGR valve', '', ivCheck);
    Prop.CheckedCaption := 'Sticking';
    Prop.UncheckedCaption := 'Repaired';
    Prop.Checked := FSim.EgrFault;
    Prop.Hint := 'Repair the valve and watch the EGR error return to zero';
  finally
    insSettings.EndUpdate;
  end;
end;

procedure TfrmMain.SetSetting(const AName, AValue: string);
var
  Prop: TOBDInspectorProperty;
begin
  Prop := insSettings.FindProperty(AName);
  if (Prop <> nil) and (Prop.Value <> AValue) then
    Prop.Value := AValue;
end;

procedure TfrmMain.SyncSettings;
const
  THEMES: array [TOBDThemeMode] of string = ('Windows', 'Light', 'Dark',
    'Windows');
  DENSITIES: array [TOBDDensity] of string = ('Desktop', 'Tablet');
  UNITS: array [TOBDUnitSystem] of string = ('Metric', 'Imperial');
  COMMANDS: array [TOBDCommandStyle] of string = ('Menu bar', 'Ribbon');
  PLACEMENTS: array [TOBDToastPlacement] of string = ('In window',
    'On desktop');
begin
  FSyncing := True;
  try
    SetSetting('Theme', THEMES[OBDTheme.Mode]);
    SetSetting('Density', DENSITIES[OBDTheme.Density]);
    SetSetting('Units', UNITS[OBDTheme.UnitSystem]);
    SetSetting('Commands', COMMANDS[TitleBar.CommandStyle]);
    SetSetting('Notifications', PLACEMENTS[OBDToasts.Placement]);
  finally
    FSyncing := False;
  end;
end;

procedure TfrmMain.insSettingsPropertyChanged(Sender: TObject;
  AProperty: TOBDInspectorProperty);
var
  Index: Integer;
begin
  if FSyncing then
    Exit;
  Index := AProperty.PickList.IndexOf(AProperty.Value);
  if AProperty.Name = 'Theme' then
  begin
    case Index of
      1:
        OBDTheme.Mode := tmDark;
      2:
        OBDTheme.Mode := tmWindows;
    else
      OBDTheme.Mode := tmLight;
    end;
  end
  else if AProperty.Name = 'Density' then
  begin
    if Index = 1 then
      OBDTheme.Density := dnTablet
    else
      OBDTheme.Density := dnDesktop;
  end
  else if AProperty.Name = 'Units' then
  begin
    if Index = 1 then
      OBDTheme.UnitSystem := usImperial
    else
      OBDTheme.UnitSystem := usMetric;
  end
  else if AProperty.Name = 'Commands' then
  begin
    if Index = 1 then
      TitleBar.CommandStyle := csMenu
    else
      TitleBar.CommandStyle := csRibbon;
  end
  else if AProperty.Name = 'Notifications' then
  begin
    if Index = 1 then
      OBDToasts.Placement := tlScreen
    else
      OBDToasts.Placement := tlForm;
  end
  else if AProperty.Name = 'Garage name' then
  begin
    FGarageName := Trim(AProperty.Value);
    TitleBar.Subtitle := FGarageName;
    sdbMain.Subtitle := FGarageName;
    rpReport.Invalidate;
  end
  else if AProperty.Name = 'Inspection' then
  begin
    if Index >= 0 then
      rdPanel.InspectionRegime := TOBDInspectionRegime(Index);
  end
  else if AProperty.Name = 'Refresh interval (ms)' then
  begin
    tmrSimulation.Interval := EnsureRange(StrToIntDef(AProperty.Value, 100),
      50, 2000);
    FSyncing := True;
    try
      AProperty.Value := IntToStr(tmrSimulation.Interval);
    finally
      FSyncing := False;
    end;
  end
  else if AProperty.Name = 'EGR valve' then
  begin
    FSim.EgrFault := AProperty.Checked;
    FEgrWarned := False;
    if FSim.EgrFault then
      lvLog.Info('Simulation: EGR valve sticking')
    else
      lvLog.Info('Simulation: EGR valve repaired');
  end;
  SyncCommands;
end;

procedure TfrmMain.rngEditorSave(Sender: TObject);
begin
  ForceDirectories(DataFolder);
  RangeProfile.SaveToFile(RangesFile);
  lvLog.Info('Value ranges saved to ' + RangesFile);
  OBDToasts.Show(tkSuccess, 'Ranges saved',
    'The freeze frame now uses the new normal ranges.');
end;

procedure TfrmMain.rngEditorCancel(Sender: TObject);
begin
  ShowPage(PAGE_FREEZE_FRAME);
end;

{ Report -------------------------------------------------------------------- }

procedure TfrmMain.rpReportPaintPage(Sender: TObject; ACanvas: TCanvas;
  const APaperRect: TRect; APageIndex: Integer; AScale: Single);
const
  STATUS_TEXT: array [TOBDDtcStatus] of string = ('Stored', 'Pending',
    'Permanent');
var
  P: TOBDPainter;
  K: Single;
  L, R, Y, I: Integer;
  Item: TOBDDtcItem;
  ValueText, ReadyText: string;

  function Px(AValue: Single): Integer;
  begin
    Result := Round(AValue * K);
  end;

  procedure Section(const ACaption: string);
  begin
    Inc(Y, Px(10));
    P.Text(L, Y, ACaption, 8.5 * K, PAPER_MUTED, twSemibold);
    Inc(Y, Px(8));
    P.FillRect(Rect(L, Y, R, Y + System.Math.Max(1, Px(1))), PAPER_RULE);
    Inc(Y, Px(12));
  end;

  procedure Line(const ALeft, ARight: string; AWeight: TOBDTextWeight);
  begin
    P.Text(L, Y, ALeft, 9.5 * K, PAPER_INK, AWeight);
    P.Text(R, Y, ARight, 9.5 * K, PAPER_MUTED, twRegular, taRightJustify);
    Inc(Y, Px(15));
  end;

begin
  P := TOBDPainter.Create(ACanvas, OBDTheme.Palette, rpReport.CurrentPPI);
  try
    K := AScale;
    L := APaperRect.Left + Px(28);
    R := APaperRect.Right - Px(28);
    Y := APaperRect.Top + Px(36);

    // Header
    P.Text(L, Y, FGarageName, 13 * K, PAPER_INK, twBold);
    P.Text(R, Y, FormatDateTime('d mmm yyyy', Date), 9.5 * K, PAPER_MUTED,
      twRegular, taRightJustify);
    Inc(Y, Px(16));
    P.Text(L, Y, 'Diagnostic report', 9.5 * K, PAPER_MUTED);
    P.Text(R, Y, Format('Page %d of %d', [APageIndex + 1,
      rpReport.PageCount]), 9.5 * K, PAPER_MUTED, twRegular, taRightJustify);
    Inc(Y, Px(14));
    P.FillRect(Rect(L, Y, R, Y + System.Math.Max(1, Px(2))),
      OBDTheme.Palette.Accent);
    Inc(Y, Px(12));

    if APageIndex = 0 then
    begin
      Section('VEHICLE');
      Line(vcVehicle.Make + ' ' + vcVehicle.Model + ' ' + vcVehicle.Engine,
        'VIN ' + vcVehicle.VIN, twSemibold);
      Line(IntToStr(vcVehicle.ModelYear) + ' - ' + vcVehicle.FuelType +
        ' - ' + vcVehicle.EngineCode + ' - ' + vcVehicle.EnginePower,
        vcVehicle.Odometer, twRegular);

      Section('TROUBLE CODES');
      if not FCodesRead then
        Line('Codes have not been read', '', twRegular)
      else
        for I := 0 to pnlDtc.Items.Count - 1 do
        begin
          Item := pnlDtc.Items[I];
          Line(Item.Code, STATUS_TEXT[Item.Status], twMonoBold);
          P.Text(L, Y - Px(3), Item.Description, 8.5 * K, PAPER_MUTED);
          Inc(Y, Px(12));
        end;

      if rdPanel.Ready then
        ReadyText := 'Ready'
      else
        ReadyText := 'Not ready';
      Section('READINESS - ' + UpperCase(INSPECTIONS[rdPanel.InspectionRegime]));
      Line(Format('%d of %d monitors complete', [rdPanel.SupportedCount -
        rdPanel.IncompleteCount, rdPanel.SupportedCount]), ReadyText,
        twRegular);
    end
    else
    begin
      Section('LIVE DATA SNAPSHOT - ' + FormatDateTime('hh:nn:ss', Now));
      for I := Low(SIM_PIDS) to High(SIM_PIDS) do
      begin
        if FConnected then
          ValueText := FormatFloat('0.' + StringOfChar('0',
            SIM_PIDS[I].Decimals), FSim.Value(SIM_PIDS[I].PID)) + ' ' +
            SIM_PIDS[I].UnitText
        else
          ValueText := 'not connected';
        Line(SIM_PIDS[I].Name, ValueText, twRegular);
      end;

      Section('TECHNICIAN NOTES');
      P.WrapText(Rect(L, Y - Px(6), R, Y + Px(80)),
        'The EGR error stays at about -80 % of the commanded flow under ' +
        'light load: the EGR valve is sticking. Clean or replace the ' +
        'valve, clear the codes and drive a full cycle so the EGR and DPF ' +
        'monitors can run.', 9.5 * K, PAPER_INK);
    end;

    P.Text(L, APaperRect.Bottom - Px(22), 'Generated by OBD Studio - ' +
      'Delphi-OBD example application', 8 * K, PAPER_MUTED);
  finally
    P.Free;
  end;
end;

end.
