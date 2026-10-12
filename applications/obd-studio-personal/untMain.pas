//------------------------------------------------------------------------------
//  untMain
//
//  Main form of OBD-Studio Personal: the shell around the pages.
//
//  Title bar (theme, density, help, connect) with the commands as a menu
//  bar or a ribbon (the user picks one in Settings or the View menu), the
//  sidebar with the page groups of the mockups (Vehicle, Tests, Drive,
//  Tools, Settings; it scrolls when the window is low), the status bar
//  (link, adapter, protocol, battery, progress, codes and the
//  recording indicator) and the host the page frames talk to through
//  IPersonalHost. The form owns the settings and the vehicle session.
//
//  Languages: the library catalogue languages\<code>.json and the
//  application catalogue languages\personal\<code>.json are merged into
//  one catalogue, so the components and the pages switch together.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.AppEvnts,
  ERD.I18n,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Theme,
  ERD.UI.TitleBar,
  ERD.UI.Ribbon,
  ERD.UI.Sidebar,
  ERD.UI.StatusBar,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.UI.Hint,
  untSettings,
  untSession,
  untPersonalPage;

const
  WM_PERSONAL_STARTED = WM_APP + 1;

type
  TfrmMain = class(TForm, IPersonalHost)
    TitleBar: TOBDTitleBar;
    rbnMain: TOBDRibbon;
    sdbMain: TOBDSidebar;
    pnlPages: TPanel;
    sbMain: TOBDStatusBar;
    OBDTheme: TOBDTheme;
    OBDHintStyle: TOBDHintStyle;
    OBDToasts: TOBDToastManager;
    dlgConfirm: TOBDDialog;
    tmrBattery: TTimer;
    alMain: TActionList;
    actConnect: TAction;
    actDisconnect: TAction;
    actPageOverview: TAction;
    actPageCodes: TAction;
    actPageReadiness: TAction;
    actPageLive: TAction;
    actPageDashboard: TAction;
    actPageTests: TAction;
    actPageInfo: TAction;
    actPageFuel: TAction;
    actPagePerformance: TAction;
    actPageEV: TAction;
    actPageAdapter: TAction;
    actPageRecordings: TAction;
    actPageConsole: TAction;
    actPageSettings: TAction;
    actThemeLight: TAction;
    actThemeDark: TAction;
    actThemeWindows: TAction;
    actDensityDesktop: TAction;
    actDensityTablet: TAction;
    actUnitsMetric: TAction;
    actUnitsImperial: TAction;
    actMenuBar: TAction;
    actRibbon: TAction;
    actAbout: TAction;
    actExit: TAction;
    mnuMain: TMainMenu;
    miFile: TMenuItem;
    miConnect: TMenuItem;
    miDisconnect: TMenuItem;
    miFileSep1: TMenuItem;
    miPageSettings: TMenuItem;
    miFileSep2: TMenuItem;
    miExit: TMenuItem;
    miVehicle: TMenuItem;
    miPageOverview: TMenuItem;
    miPageCodes: TMenuItem;
    miPageReadiness: TMenuItem;
    miPageLive: TMenuItem;
    miPageDashboard: TMenuItem;
    miTests: TMenuItem;
    miPageTests: TMenuItem;
    miPageInfo: TMenuItem;
    miDrive: TMenuItem;
    miPageFuel: TMenuItem;
    miPagePerformance: TMenuItem;
    miPageEV: TMenuItem;
    miTools: TMenuItem;
    miPageAdapter: TMenuItem;
    miPageRecordings: TMenuItem;
    miPageConsole: TMenuItem;
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
    miRibbon: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    aeMain: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sdbMainChange(Sender: TObject);
    procedure TitleBarButtonClick(Sender: TObject; AButton: TOBDCaptionButton);
    procedure sbMainPanelClick(Sender: TObject; Panel: TOBDStatusPanel);
    procedure OBDThemeChange(Sender: TObject);
    procedure tmrBatteryTimer(Sender: TObject);
    procedure TitleBarCommandStyleChange(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actPageExecute(Sender: TObject);
    procedure actThemeExecute(Sender: TObject);
    procedure actDensityExecute(Sender: TObject);
    procedure actUnitsExecute(Sender: TObject);
    procedure actCommandsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure aeMainIdle(Sender: TObject; var Done: Boolean);
  strict private
    FSettings: TPersonalSettings;
    FSession: TPersonalSession;
    FPages: TArray<TControl>;
    FKeys: TArray<string>;
    FPageActions: TArray<TAction>;
    // Page commands: one action each, the page that owns it, the coloured
    // ribbon tab of every page (nil without commands), the Home group and
    // the menu of the active page.
    FCommandActions: array[TPersonalCommand] of TAction;
    FCommandPages: array[TPersonalCommand] of Integer;
    FPageTabs: TArray<TOBDContextualTabSet>;
    FDiagnosisGroup: TOBDRibbonGroup;
    FPageMenu: TMenuItem;
    FActive: Integer;
    FSyncing: Boolean;
    FActivity: TStringList;
    FCodeCount: Integer;
    FMIL: Boolean;
    FCatalogError: string;
    procedure WMPersonalStarted(var Msg: TMessage); message WM_PERSONAL_STARTED;
    procedure LoadCatalogs;
    procedure AddPage(APage: TControl);
    procedure CreatePages;
    function PageIndex(const AKey: string): Integer;
    function SidebarItemOf(AIndex: Integer): TOBDSidebarItem;
    procedure ActivatePage(AIndex: Integer);
    procedure CreateCommands;
    procedure ShowPageCommands;
    procedure UpdateCommandStates;
    procedure actCommandExecute(Sender: TObject);
    procedure ApplyChrome;
    procedure ApplySettings;
    procedure ApplyLanguage;
    procedure TranslateShell;
    procedure TranslateCommands;
    procedure TranslatePageCommands;
    procedure SyncTitleButtons;
    procedure SyncCommands;
    procedure SyncStatus;
    procedure SessionStateChanged(Sender: TObject);
    procedure SessionStatus(Sender: TObject);
    procedure RestoreWindow;
    procedure StoreWindow;
    procedure ShowAbout;
    procedure WriteActivity(const ALevel, AText: string);
    function IsDark: Boolean;
    function AppFolder: string;
  public
    // IPersonalHost
    function Session: TPersonalSession;
    function Settings: TPersonalSettings;
    function Connected: Boolean;
    procedure Notify(AKind: TOBDToastKind; const ATitle, AText: string);
    procedure Log(const AText: string);
    procedure LogWarning(const AText: string);
    function ActivityLog: TStrings;
    procedure SetProgress(const AText: string; APercent: Integer);
    function Confirm(AKind: TOBDDialogKind; const ATitle, AText,
      AConfirmCaption: string): Boolean;
    procedure ShowPage(const AKey: string);
    procedure SetBadge(const AKey: string; ACount: Integer);
    procedure SetCodeSummary(ACount: Integer; AMIL: Boolean);
    procedure ToggleConnection;
    procedure SettingsChanged;
    function LanguageFolder: string;
    procedure SetRecording(const AText: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.UITypes,
  ERD.Service.Catalog,
  untWelcome,
  untPageOverview,
  untPageCodes,
  untPageReadiness,
  untPageLive,
  untPageDashboard,
  untPageTests,
  untPageInfo,
  untPageFuel,
  untPagePerformance,
  untPageEV,
  untPageAdapter,
  untPageRecordings,
  untPageConsole,
  untPageSettings;

const
  // Status bar panels, in the order of the DFM.
  SP_LINK      = 0;
  SP_ADAPTER   = 1;
  SP_PROTOCOL  = 2;
  SP_BATTERY   = 3;
  SP_PROGRESS  = 4;
  SP_CODES     = 5;
  SP_RECORDING = 6;

  ACTIVITY_MAX_LINES = 5000;

  // Page commands that save, copy or export results: they form the
  // "Save and share" group of the page tab.
  SHARE_COMMANDS: TPersonalCommands = [pcSaveVehicleReport, pcSaveCodeReport,
    pcExportTests, pcCopyVehicleInfo, pcSaveVehicleInfo, pcExportRecording,
    pcShowRecording, pcSaveConsole];
  // Page commands that are also on the Home tab and in the Vehicle and
  // Tools menus, whatever page is open.
  DIAGNOSIS_COMMANDS: TPersonalCommands = [pcScanVehicle, pcReadCodes,
    pcClearCodes, pcRecordSession];
  // Page commands that run without bringing their page to the front.
  BACKGROUND_COMMANDS: TPersonalCommands = [pcRecordSession];

function CommandGlyph(ACommand: TPersonalCommand): TOBDGlyph;
begin
  case ACommand of
    pcScanVehicle:
      Result := glHealth;
    pcSaveVehicleReport, pcSaveCodeReport:
      Result := glReport;
    pcReadCodes, pcReadTests:
      Result := glRead;
    pcClearCodes, pcDeleteRecording, pcClearConsole:
      Result := glClear;
    pcLookUpCode:
      Result := glGlobe;
    pcStartAdvisor:
      Result := glPlay;
    pcStopAdvisor, pcCancelRun:
      Result := glStop;
    pcRecordLive, pcRecordSession:
      Result := glRecord;
    pcAddGauge:
      Result := glPlus;
    pcEditDashboard, pcRenameRecording:
      Result := glEdit;
    pcSaveDashboard, pcSaveVehicleInfo, pcSaveConsole:
      Result := glSave;
    pcResetDashboard, pcResetTrip:
      Result := glUndo;
    pcExportTests, pcExportRecording:
      Result := glExport;
    pcCopyVehicleInfo:
      Result := glCopy;
    pcArmRun:
      Result := glFlag;
    pcShowRecording:
      Result := glOpen;
  else
    // Read again / refresh.
    Result := glRefresh;
  end;
end;

function CommandColor(ACommand: TPersonalCommand): TOBDRibbonItemColor;
begin
  case ACommand of
    pcScanVehicle:
      Result := ricAccent;
    pcClearCodes, pcDeleteRecording:
      Result := ricDanger;
  else
    Result := ricNormal;
  end;
end;

{ Start and stop ------------------------------------------------------------ }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Nothing in here may raise: the VCL swallows an exception from
  // OnCreate and shows a form that is only half set up.
  FActive := -1;
  // Indexed by page index, like the Tag of the sidebar items.
  FPageActions := [actPageOverview, actPageCodes, actPageReadiness,
    actPageLive, actPageDashboard, actPageTests, actPageInfo, actPageFuel,
    actPagePerformance, actPageEV, actPageAdapter, actPageRecordings,
    actPageConsole, actPageSettings];
  // Frames and dialogs created at run time pick this theme up as well.
  TOBDTheme.RegisterDefault(OBDTheme);
  FActivity := TStringList.Create;
  FSettings := TPersonalSettings.Create;
  FSettings.Load;

  FSession := TPersonalSession.Create(Self);
  FSession.OnStateChanged := SessionStateChanged;
  FSession.OnStatus := SessionStatus;
  FSession.Inspection.Country := FSettings.InspectionCountry;

  LoadCatalogs;
  CreatePages;
  CreateCommands;
  RestoreWindow;
  ApplySettings;
  SyncStatus;
  Log('OBD-Studio Personal started');
  // The welcome wizard and the automatic connect run once the window is
  // on screen.
  PostMessage(Handle, WM_PERSONAL_STARTED, 0, 0);
end;

procedure TfrmMain.LoadCatalogs;
begin
  // The PID and trouble code catalogues ship next to the program. Without
  // them the program still runs, with empty code texts, normal ranges,
  // VIN decoding and inspection rules; the user is told once the window
  // is on screen.
  try
    TOBDServiceCatalog.Default.LoadDirectory(
      TPath.Combine(AppFolder, 'catalogs'));
  except
    on E: Exception do
    begin
      FCatalogError := E.Message;
      LogWarning('Catalogues not loaded: ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.WMPersonalStarted(var Msg: TMessage);
begin
  if FCatalogError <> '' then
    Notify(tkWarning, Tr('personal.shell.catalogsTitle',
      'Catalogues not found'), TrFmt('personal.shell.catalogsText',
      'Copy the catalogs folder of the repository next to the program ' +
      '(%s). Until then trouble code texts, normal ranges, VIN decoding ' +
      'and inspection rules are empty.',
      [TPath.Combine(AppFolder, 'catalogs')]));
  if not FSettings.EulaAccepted or not FSettings.WelcomeDone then
  begin
    if not TfrmWelcome.Execute(Self, FSettings, LanguageFolder) then
    begin
      // The licence was declined: the program may not be used.
      Close;
      Exit;
    end;
    FSettings.Save;
    ApplySettings;
    if FSettings.ConnectionKind = pckDemo then
      FSession.Connect(FSettings)
    else
      ShowPage(PAGE_ADAPTER);
  end
  else if FSettings.AutoConnect then
    FSession.Connect(FSettings);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FActive >= 0) and (FActive <= High(FKeys)) then
    FSettings.LastPage := FKeys[FActive];
  StoreWindow;
  try
    FSettings.Save;
  except
    on E: Exception do
      // Closing must not fail because the settings folder is read-only.
      WriteActivity('WARN', 'Settings not saved: ' + E.Message);
  end;
  if FSession <> nil then
    FSession.Disconnect;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  tmrBattery.Enabled := False;
  if FSession <> nil then
  begin
    FSession.OnStateChanged := nil;
    FSession.OnStatus := nil;
  end;
  // The session goes before the pages so that no polling thread calls
  // into a page that is being destroyed.
  FreeAndNil(FSession);
  FreeAndNil(FSettings);
  FreeAndNil(FActivity);
end;

function TfrmMain.AppFolder: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function TfrmMain.IsDark: Boolean;
begin
  Result := OBDIsDarkPalette(OBDTheme.Palette);
end;

{ Pages --------------------------------------------------------------------- }

procedure TfrmMain.AddPage(APage: TControl);
var
  Page: IPersonalPage;
begin
  if not Supports(APage, IPersonalPage, Page) then
    raise Exception.Create(APage.ClassName +
      ' does not implement IPersonalPage');
  APage.Visible := False;
  APage.Parent := pnlPages;
  APage.Align := alClient;
  FPages := FPages + [APage];
  FKeys := FKeys + [Page.PageKey];
  Page.Init(Self);
end;

procedure TfrmMain.CreatePages;
begin
  // The order matches the Tag of the sidebar items in the DFM.
  AddPage(TfrmPageOverview.Create(Self));
  AddPage(TfrmPageCodes.Create(Self));
  AddPage(TfrmPageReadiness.Create(Self));
  AddPage(TfrmPageLive.Create(Self));
  AddPage(TfrmPageDashboard.Create(Self));
  AddPage(TfrmPageTests.Create(Self));
  AddPage(TfrmPageInfo.Create(Self));
  AddPage(TfrmPageFuel.Create(Self));
  AddPage(TfrmPagePerformance.Create(Self));
  AddPage(TfrmPageEV.Create(Self));
  AddPage(TfrmPageAdapter.Create(Self));
  AddPage(TfrmPageRecordings.Create(Self));
  AddPage(TfrmPageConsole.Create(Self));
  AddPage(TfrmPageSettings.Create(Self));
end;

function TfrmMain.PageIndex(const AKey: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FKeys) do
    if SameText(FKeys[I], AKey) then
      Exit(I);
  Result := -1;
end;

function TfrmMain.SidebarItemOf(AIndex: Integer): TOBDSidebarItem;
var
  I: Integer;
begin
  for I := 0 to sdbMain.Items.Count - 1 do
    if sdbMain.Items[I].Tag = AIndex then
      Exit(sdbMain.Items[I]);
  Result := nil;
end;

procedure TfrmMain.ActivatePage(AIndex: Integer);
var
  I: Integer;
  Page: IPersonalPage;
begin
  if (AIndex < 0) or (AIndex > High(FPages)) or (AIndex = FActive) then
    Exit;
  if (FActive >= 0) and Supports(FPages[FActive], IPersonalPage, Page) then
    Page.Activated(False);
  FActive := AIndex;
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
  SyncCommands;
  ShowPageCommands;
  if Supports(FPages[AIndex], IPersonalPage, Page) then
    Page.Activated(True);
end;

procedure TfrmMain.sdbMainChange(Sender: TObject);
begin
  if FSyncing or (sdbMain.ItemIndex < 0) then
    Exit;
  ActivatePage(sdbMain.Items[sdbMain.ItemIndex].Tag);
end;

{ Chrome -------------------------------------------------------------------- }

procedure TfrmMain.ApplyChrome;
var
  P: TOBDThemePalette;
  I: Integer;
begin
  // The OBD controls paint themselves; the plain VCL panel and the page
  // frames take the theme's background colour.
  P := OBDTheme.Palette;
  Color := P.Background;
  pnlPages.Color := P.Background;
  for I := 0 to High(FPages) do
    if FPages[I] is TFrame then
    begin
      TFrame(FPages[I]).ParentBackground := False;
      TFrame(FPages[I]).Color := P.Background;
    end;
  // The non-visual components have their own density.
  OBDToasts.Density := OBDTheme.Density;
  dlgConfirm.Density := OBDTheme.Density;
  OBDHintStyle.Density := OBDTheme.Density;
  SyncTitleButtons;
  SyncCommands;
end;

procedure TfrmMain.OBDThemeChange(Sender: TObject);
begin
  ApplyChrome;
end;

procedure TfrmMain.ApplySettings;
var
  UnitsChanged: Boolean;
  I: Integer;
  Page: IPersonalPage;
begin
  UnitsChanged := OBDTheme.UnitSystem <> FSettings.UnitSystem;
  OBDTheme.Mode := FSettings.ThemeMode;
  OBDTheme.Density := FSettings.Density;
  OBDTheme.UnitSystem := FSettings.UnitSystem;
  FSession.Inspection.Country := FSettings.InspectionCountry;
  // The title bar shows the menu bar or the ribbon below it.
  if FSettings.UseRibbon then
    TitleBar.CommandStyle := csRibbon
  else
    TitleBar.CommandStyle := csMenu;
  ApplyChrome;
  ApplyLanguage;
  if UnitsChanged then
    for I := 0 to High(FPages) do
      if Supports(FPages[I], IPersonalPage, Page) then
        Page.UnitsChanged;
  if FActive < 0 then
  begin
    I := PageIndex(FSettings.LastPage);
    if I < 0 then
      I := PageIndex(PAGE_OVERVIEW);
    ActivatePage(I);
  end;
end;

procedure TfrmMain.SyncTitleButtons;
var
  I: Integer;
  Button: TOBDCaptionButton;
begin
  for I := 0 to TitleBar.Buttons.Count - 1 do
  begin
    Button := TitleBar.Buttons[I];
    if Button.Name = 'theme' then
    begin
      if IsDark then
      begin
        Button.Glyph := glSun;
        Button.Hint := Tr('personal.shell.themeLight',
          'Switch to the light theme');
      end
      else
      begin
        Button.Glyph := glMoon;
        Button.Hint := Tr('personal.shell.themeDark',
          'Switch to the dark theme');
      end;
    end
    else if Button.Name = 'density' then
    begin
      Button.Down := OBDTheme.Density = dnTablet;
      Button.Hint := Tr('personal.shell.density',
        'Tablet density (larger controls for touch)');
    end
    else if Button.Name = 'help' then
      Button.Hint := Tr('personal.shell.about', 'About OBD-Studio Personal');
  end;
  for I := 0 to TitleBar.QuickAccess.Count - 1 do
  begin
    Button := TitleBar.QuickAccess[I];
    if Button.Name = 'connect' then
    begin
      Button.Down := Connected;
      if Connected then
        Button.Hint := Tr('personal.shell.disconnect', 'Disconnect')
      else
        Button.Hint := Tr('personal.shell.connect', 'Connect');
    end;
  end;
end;

procedure TfrmMain.TitleBarButtonClick(Sender: TObject;
  AButton: TOBDCaptionButton);
begin
  if AButton.Name = 'theme' then
  begin
    if IsDark then
      FSettings.ThemeMode := tmLight
    else
      FSettings.ThemeMode := tmDark;
    SettingsChanged;
  end
  else if AButton.Name = 'density' then
  begin
    if FSettings.Density = dnDesktop then
      FSettings.Density := dnTablet
    else
      FSettings.Density := dnDesktop;
    SettingsChanged;
  end
  else if AButton.Name = 'help' then
    ShowAbout
  else if AButton.Name = 'connect' then
    ToggleConnection;
end;

{ Commands ------------------------------------------------------------------ }

procedure TfrmMain.SyncCommands;
var
  I: Integer;
  Busy: Boolean;
begin
  // The menu items and the ribbon buttons follow their action: Checked
  // shows as a check mark in the menu and as a pressed ribbon button.
  Busy := (FSession <> nil) and (FSession.State in [plsConnecting, plsOnline]);
  actConnect.Enabled := (FSession <> nil) and not Busy;
  actDisconnect.Enabled := Busy;
  for I := 0 to High(FPageActions) do
    FPageActions[I].Checked := I = FActive;
  actThemeLight.Checked := OBDTheme.Mode = tmLight;
  actThemeDark.Checked := OBDTheme.Mode = tmDark;
  actThemeWindows.Checked := OBDTheme.Mode in [tmAuto, tmWindows];
  actDensityDesktop.Checked := OBDTheme.Density = dnDesktop;
  actDensityTablet.Checked := OBDTheme.Density = dnTablet;
  actUnitsMetric.Checked := OBDTheme.UnitSystem = usMetric;
  actUnitsImperial.Checked := OBDTheme.UnitSystem = usImperial;
  actMenuBar.Checked := TitleBar.CommandStyle = csMenu;
  actRibbon.Checked := TitleBar.CommandStyle = csRibbon;
  UpdateCommandStates;
end;

procedure TfrmMain.CreateCommands;
var
  C: TPersonalCommand;
  I: Integer;
  PageCommands: IPersonalCommands;
  Owned, Actions, Shares: TPersonalCommands;
  Act: TAction;
  TabSet: TOBDContextualTabSet;
  Tab: TOBDRibbonTab;
  Group: TOBDRibbonGroup;

  procedure AddRibbonItem(AGroup: TOBDRibbonGroup; ACommand: TPersonalCommand;
    ASize: TOBDRibbonItemSize);
  var
    Item: TOBDRibbonItem;
  begin
    Item := AGroup.Items.Add;
    Item.Glyph := CommandGlyph(ACommand);
    Item.Size := ASize;
    Item.Color := CommandColor(ACommand);
    Item.Action := FCommandActions[ACommand];
  end;

  procedure AddMenuItem(AParent: TMenuItem; ACommand: TPersonalCommand;
    APage: Integer);
  var
    Item: TMenuItem;
  begin
    Item := TMenuItem.Create(Self);
    Item.Action := FCommandActions[ACommand];
    Item.Tag := APage;
    AParent.Add(Item);
  end;

  procedure AddMenuSeparator(AParent: TMenuItem; APage: Integer);
  var
    Item: TMenuItem;
  begin
    Item := TMenuItem.Create(Self);
    Item.Caption := '-';
    Item.Tag := APage;
    AParent.Add(Item);
  end;

begin
  // The buttons of the pages become commands in the ribbon and the menu
  // bar. Every page with commands gets a coloured ribbon tab and a menu,
  // shown while the page is open (ShowPageCommands). Caption and hint
  // come from TranslatePageCommands, Enabled and Checked from the page
  // while the application is idle (UpdateCommandStates).
  for C := Low(TPersonalCommand) to High(TPersonalCommand) do
  begin
    FCommandPages[C] := -1;
    Act := TAction.Create(Self);
    Act.ActionList := alMain;
    Act.Category := 'Page';
    Act.Tag := Ord(C);
    Act.Enabled := False;
    Act.OnExecute := actCommandExecute;
    FCommandActions[C] := Act;
  end;

  FPageMenu := TMenuItem.Create(Self);
  FPageMenu.Visible := False;
  mnuMain.Items.Insert(miView.MenuIndex, FPageMenu);

  SetLength(FPageTabs, Length(FPages));
  for I := 0 to High(FPages) do
  begin
    FPageTabs[I] := nil;
    if not Supports(FPages[I], IPersonalCommands, PageCommands) then
      Continue;
    Owned := PageCommands.Commands;
    if Owned = [] then
      Continue;
    for C in Owned do
      FCommandPages[C] := I;
    Actions := Owned - SHARE_COMMANDS;
    Shares := Owned * SHARE_COMMANDS;

    TabSet := rbnMain.ContextualTabs.Add;
    TabSet.Tag := I;
    Tab := TabSet.Tabs.Add;
    if Actions <> [] then
    begin
      Group := Tab.Groups.Add;
      Group.Tag := 0;
      for C in Actions do
      begin
        AddRibbonItem(Group, C, risLarge);
        AddMenuItem(FPageMenu, C, I);
      end;
    end;
    if Shares <> [] then
    begin
      if Actions <> [] then
        AddMenuSeparator(FPageMenu, I);
      Group := Tab.Groups.Add;
      Group.Tag := 1;
      for C in Shares do
      begin
        AddRibbonItem(Group, C, risSmall);
        AddMenuItem(FPageMenu, C, I);
      end;
    end;
    FPageTabs[I] := TabSet;
  end;

  // Scan, read and clear codes and the session recorder are also on the
  // Home tab, in the Vehicle menu and in the Tools menu.
  if rbnMain.Tabs.Count > 0 then
  begin
    FDiagnosisGroup := rbnMain.Tabs[0].Groups.Add;
    for C in DIAGNOSIS_COMMANDS do
      AddRibbonItem(FDiagnosisGroup, C, risLarge);
  end;
  Actions := DIAGNOSIS_COMMANDS - BACKGROUND_COMMANDS;
  AddMenuSeparator(miVehicle, -1);
  for C in Actions do
    AddMenuItem(miVehicle, C, -1);
  Actions := DIAGNOSIS_COMMANDS * BACKGROUND_COMMANDS;
  AddMenuSeparator(miTools, -1);
  for C in Actions do
    AddMenuItem(miTools, C, -1);
end;

procedure TfrmMain.ShowPageCommands;
var
  I, NormalTabs: Integer;
  OnPageTab, HasCommands: Boolean;
begin
  // Only the tab and the menu of the active page are shown. Whoever works
  // in the page tab stays in it when opening another page; without page
  // commands the ribbon goes back to Home.
  NormalTabs := 0;
  for I := 0 to rbnMain.Tabs.Count - 1 do
    if rbnMain.Tabs[I].Visible then
      Inc(NormalTabs);
  OnPageTab := rbnMain.ActiveTab >= NormalTabs;
  HasCommands := False;
  for I := 0 to High(FPageTabs) do
    if FPageTabs[I] <> nil then
    begin
      FPageTabs[I].Visible := I = FActive;
      if I = FActive then
        HasCommands := True;
    end;
  if OnPageTab then
  begin
    if HasCommands then
      rbnMain.ActiveTab := NormalTabs
    else
      rbnMain.ActiveTab := 0;
  end;
  if FPageMenu <> nil then
  begin
    for I := 0 to FPageMenu.Count - 1 do
      FPageMenu.Items[I].Visible := FPageMenu.Items[I].Tag = FActive;
    if (FActive >= 0) and (FActive <= High(FPageActions)) then
      FPageMenu.Caption := FPageActions[FActive].Caption;
    FPageMenu.Visible := HasCommands;
    TitleBar.MenuChanged;
  end;
  UpdateCommandStates;
end;

procedure TfrmMain.UpdateCommandStates;
var
  C: TPersonalCommand;
  Page: Integer;
  PageCommands: IPersonalCommands;
begin
  for C := Low(TPersonalCommand) to High(TPersonalCommand) do
  begin
    if FCommandActions[C] = nil then
      Continue;
    Page := FCommandPages[C];
    if (Page >= 0) and (Page <= High(FPages)) and
      Supports(FPages[Page], IPersonalCommands, PageCommands) then
    begin
      FCommandActions[C].Enabled := PageCommands.CommandEnabled(C);
      FCommandActions[C].Checked := PageCommands.CommandChecked(C);
    end
    else
      FCommandActions[C].Enabled := False;
  end;
end;

procedure TfrmMain.aeMainIdle(Sender: TObject; var Done: Boolean);
begin
  // The VCL updates actions only for controls and Form.Menu; ribbon
  // buttons and the title-bar menu follow the pages from here. Setting
  // an unchanged Enabled or Checked does nothing.
  UpdateCommandStates;
end;

procedure TfrmMain.actCommandExecute(Sender: TObject);
var
  Value: NativeInt;
  Command: TPersonalCommand;
  Page: Integer;
  PageCommands: IPersonalCommands;
begin
  // The command actions have Ord(command) as Tag.
  Value := TComponent(Sender).Tag;
  if (Value < Ord(Low(TPersonalCommand))) or
    (Value > Ord(High(TPersonalCommand))) then
    Exit;
  Command := TPersonalCommand(Value);
  Page := FCommandPages[Command];
  if (Page < 0) or (Page > High(FPages)) or
    not Supports(FPages[Page], IPersonalCommands, PageCommands) then
    Exit;
  // The page comes to the front to show the result. Opening it can start
  // the same job (the codes page reads the codes when it opens), so the
  // command only runs when it is still enabled.
  if not (Command in BACKGROUND_COMMANDS) then
    ActivatePage(Page);
  if PageCommands.CommandEnabled(Command) then
    PageCommands.ExecuteCommand(Command);
  UpdateCommandStates;
end;

procedure TfrmMain.TitleBarCommandStyleChange(Sender: TObject);
begin
  SyncCommands;
end;

procedure TfrmMain.actConnectExecute(Sender: TObject);
begin
  FSession.Connect(FSettings);
end;

procedure TfrmMain.actDisconnectExecute(Sender: TObject);
begin
  FSession.Disconnect;
end;

procedure TfrmMain.actPageExecute(Sender: TObject);
begin
  // The page actions have the page index as Tag.
  ActivatePage(TComponent(Sender).Tag);
end;

procedure TfrmMain.actThemeExecute(Sender: TObject);
begin
  // actThemeLight / actThemeDark / actThemeWindows have Tag 0 / 1 / 2.
  case TComponent(Sender).Tag of
    1:
      FSettings.ThemeMode := tmDark;
    2:
      FSettings.ThemeMode := tmWindows;
  else
    FSettings.ThemeMode := tmLight;
  end;
  SettingsChanged;
end;

procedure TfrmMain.actDensityExecute(Sender: TObject);
begin
  if TComponent(Sender).Tag = 1 then
    FSettings.Density := dnTablet
  else
    FSettings.Density := dnDesktop;
  SettingsChanged;
end;

procedure TfrmMain.actUnitsExecute(Sender: TObject);
begin
  if TComponent(Sender).Tag = 1 then
    FSettings.UnitSystem := usImperial
  else
    FSettings.UnitSystem := usMetric;
  SettingsChanged;
end;

procedure TfrmMain.actCommandsExecute(Sender: TObject);
begin
  // actMenuBar has Tag 0, actRibbon Tag 1.
  FSettings.UseRibbon := TComponent(Sender).Tag = 1;
  SettingsChanged;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.sbMainPanelClick(Sender: TObject; Panel: TOBDStatusPanel);
begin
  case Panel.Index of
    SP_LINK, SP_ADAPTER:
      ShowPage(PAGE_ADAPTER);
    SP_CODES:
      ShowPage(PAGE_CODES);
    SP_RECORDING:
      ShowPage(PAGE_RECORDINGS);
  end;
end;

procedure TfrmMain.ShowAbout;
begin
  dlgConfirm.Kind := dkInfo;
  dlgConfirm.Title := Tr('personal.shell.about', 'About OBD-Studio Personal');
  dlgConfirm.Text := TrFmt('personal.shell.aboutText',
    'OBD-Studio Personal %s'#13#10'Free for personal, non-commercial use.'#13#10 +
    'Built with the Delphi-OBD components by ERDesigns.'#13#10#13#10 +
    'Settings: %s'#13#10'Your data: %s',
    [PERSONAL_VERSION, FSettings.FileName, FSettings.DataFolder]);
  dlgConfirm.Buttons := [mbOK];
  dlgConfirm.ButtonCaptions.Text := '';
  dlgConfirm.DefaultButton := mbOK;
  dlgConfirm.DangerButton := mbNo;
  dlgConfirm.CheckBoxCaption := '';
  dlgConfirm.Execute;
end;

{ Languages ----------------------------------------------------------------- }

function TfrmMain.LanguageFolder: string;
begin
  Result := TPath.Combine(AppFolder, 'languages');
end;

// Merges the strings of the application catalogue into the library
// catalogue of the same language; either file may be missing.
function MergedCatalog(const ALibraryFile, AAppFile, ACode: string): string;
var
  Value, StringsValue, AppStringsValue: TJSONValue;
  Root, Strings, AppStrings: TJSONObject;
  Pair: TJSONPair;
begin
  Root := nil;
  if TFile.Exists(ALibraryFile) then
  begin
    Value := TJSONObject.ParseJSONValue(
      TFile.ReadAllText(ALibraryFile, TEncoding.UTF8));
    if Value is TJSONObject then
      Root := TJSONObject(Value)
    else
      Value.Free;
  end;
  if Root = nil then
  begin
    Root := TJSONObject.Create;
    Root.AddPair('language', ACode);
    Root.AddPair('strings', TJSONObject.Create);
  end;
  try
    if TFile.Exists(AAppFile) then
    begin
      Value := TJSONObject.ParseJSONValue(
        TFile.ReadAllText(AAppFile, TEncoding.UTF8));
      try
        StringsValue := Root.GetValue('strings');
        AppStringsValue := nil;
        if Value is TJSONObject then
          AppStringsValue := TJSONObject(Value).GetValue('strings');
        if (StringsValue is TJSONObject) and
          (AppStringsValue is TJSONObject) then
        begin
          Strings := TJSONObject(StringsValue);
          AppStrings := TJSONObject(AppStringsValue);
          for Pair in AppStrings do
          begin
            Strings.RemovePair(Pair.JsonString.Value).Free;
            Strings.AddPair(Pair.JsonString.Value,
              TJSONValue(Pair.JsonValue.Clone));
          end;
        end;
      finally
        Value.Free;
      end;
    end;
    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

procedure TfrmMain.ApplyLanguage;
var
  Code: string;
begin
  Code := FSettings.Language;
  if Code = '' then
    Code := OBDDefaultLanguageCode;
  if not OBDIsSupportedLanguage(Code) then
    Code := 'en';
  if Code <> 'en' then
  try
    OBDI18n.LoadLanguageFromString(MergedCatalog(
      TPath.Combine(LanguageFolder, Code + '.json'),
      TPath.Combine(TPath.Combine(LanguageFolder, 'personal'), Code + '.json'),
      Code));
  except
    on E: Exception do
    begin
      WriteActivity('WARN', 'Language ' + Code + ' not loaded: ' + E.Message);
      Code := 'en';
    end;
  end;
  OBDI18n.CurrentLanguage := Code;
  TranslateShell;
end;

procedure TfrmMain.TranslateShell;
var
  I: Integer;
  Item: TOBDSidebarItem;
  Page: IPersonalPage;
begin
  TranslateCommands;
  // The sidebar shows the same captions as the page commands. Its hint
  // stays empty: a collapsed sidebar shows the caption and badge instead.
  for I := 0 to High(FPageActions) do
  begin
    Item := SidebarItemOf(I);
    if Item <> nil then
      Item.Caption := FPageActions[I].Caption;
  end;
  for I := 0 to sdbMain.Items.Count - 1 do
  begin
    Item := sdbMain.Items[I];
    if Item.Tag <= 4 then
      Item.Group := Tr('personal.nav.groupVehicle', 'Vehicle')
    else if Item.Tag <= 6 then
      Item.Group := Tr('personal.nav.groupTests', 'Tests')
    else if Item.Tag <= 9 then
      Item.Group := Tr('personal.nav.groupDrive', 'Drive')
    else if Item.Tag <= 12 then
      Item.Group := Tr('personal.nav.groupTools', 'Tools');
  end;
  SyncTitleButtons;
  SyncStatus;
  for I := 0 to High(FPages) do
    if Supports(FPages[I], IPersonalPage, Page) then
      Page.Translate;
end;

procedure TfrmMain.TranslateCommands;

  procedure SetCommand(AAction: TAction; const ACaption, AHint: string);
  begin
    AAction.Caption := ACaption;
    AAction.Hint := AHint;
  end;

  procedure SetGroupCaption(ATab, AGroup: Integer; const ACaption: string);
  begin
    if (ATab < rbnMain.Tabs.Count) and
      (AGroup < rbnMain.Tabs[ATab].Groups.Count) then
      rbnMain.Tabs[ATab].Groups[AGroup].Caption := ACaption;
  end;

begin
  // Menu items and ribbon buttons take caption and hint from the action.
  SetCommand(actConnect, Tr('personal.cmd.connect', 'Connect'),
    Tr('personal.cmd.connectHint', 'Connect with the saved adapter'));
  SetCommand(actDisconnect, Tr('personal.cmd.disconnect', 'Disconnect'),
    Tr('personal.cmd.disconnectHint', 'Close the connection'));
  SetCommand(actPageOverview, Tr('personal.nav.overview', 'Overview'),
    Tr('personal.nav.overviewHint',
    'Vehicle, codes and readiness at a glance'));
  SetCommand(actPageCodes, Tr('personal.nav.codes', 'Trouble codes'),
    Tr('personal.nav.codesHint', 'Read and clear the trouble codes'));
  SetCommand(actPageReadiness, Tr('personal.nav.readiness', 'Readiness'),
    Tr('personal.nav.readinessHint',
    'Readiness monitors and the inspection check'));
  SetCommand(actPageLive, Tr('personal.nav.live', 'Live data'),
    Tr('personal.nav.liveHint', 'Live sensor values with a chart'));
  SetCommand(actPageDashboard, Tr('personal.nav.dashboard', 'Dashboard'),
    Tr('personal.nav.dashboardHint', 'Gauges for the most important values'));
  SetCommand(actPageTests, Tr('personal.nav.tests', 'Monitor tests'),
    Tr('personal.nav.testsHint', 'On-board monitor test results'));
  SetCommand(actPageInfo, Tr('personal.nav.info', 'Vehicle info'),
    Tr('personal.nav.infoHint', 'VIN, calibration IDs and ECUs'));
  SetCommand(actPageFuel, Tr('personal.nav.fuel', 'Fuel economy'),
    Tr('personal.nav.fuelHint', 'Consumption, trip costs and CO2'));
  SetCommand(actPagePerformance, Tr('personal.nav.performance', 'Performance'),
    Tr('personal.nav.performanceHint', 'Acceleration and power runs'));
  SetCommand(actPageEV, Tr('personal.nav.ev', 'EV battery'),
    Tr('personal.nav.evHint', 'State of charge and battery health'));
  SetCommand(actPageAdapter, Tr('personal.nav.adapter', 'Adapter'),
    Tr('personal.nav.adapterHint', 'Choose and connect the adapter'));
  SetCommand(actPageRecordings, Tr('personal.nav.recordings', 'Recordings'),
    Tr('personal.nav.recordingsHint', 'Record and replay drives'));
  SetCommand(actPageConsole, Tr('personal.nav.console', 'Console'),
    Tr('personal.nav.consoleHint', 'Activity log and adapter terminal'));
  SetCommand(actPageSettings, Tr('personal.nav.settings', 'Settings'),
    Tr('personal.nav.settingsHint', 'Language, appearance, units and data'));
  SetCommand(actThemeLight, Tr('personal.cmd.themeLight', 'Light theme'),
    Tr('personal.cmd.themeLight', 'Light theme'));
  SetCommand(actThemeDark, Tr('personal.cmd.themeDark', 'Dark theme'),
    Tr('personal.cmd.themeDark', 'Dark theme'));
  SetCommand(actThemeWindows, Tr('personal.cmd.themeWindows', 'Follow Windows'),
    Tr('personal.cmd.themeWindowsHint', 'Follow the Windows app mode'));
  SetCommand(actDensityDesktop, Tr('personal.cmd.desktop', 'Desktop'),
    Tr('personal.cmd.desktopHint', 'Compact controls for mouse and keyboard'));
  SetCommand(actDensityTablet, Tr('personal.cmd.tablet', 'Tablet'),
    Tr('personal.cmd.tabletHint', 'Larger controls for touch screens'));
  SetCommand(actUnitsMetric, Tr('personal.cmd.metric', 'Metric units'),
    Tr('personal.cmd.metricHint', 'km, °C, kPa, litres'));
  SetCommand(actUnitsImperial, Tr('personal.cmd.imperial', 'Imperial units'),
    Tr('personal.cmd.imperialHint', 'miles, °F, psi, gallons'));
  SetCommand(actMenuBar, Tr('personal.cmd.menuBar', 'Menu bar'),
    Tr('personal.cmd.menuBarHint', 'Show the commands in a classic menu bar'));
  SetCommand(actRibbon, Tr('personal.cmd.ribbon', 'Ribbon'),
    Tr('personal.cmd.ribbonHint', 'Show the commands in a ribbon'));
  SetCommand(actAbout, Tr('personal.shell.about', 'About OBD-Studio Personal'),
    Tr('personal.shell.about', 'About OBD-Studio Personal'));
  SetCommand(actExit, Tr('personal.cmd.exit', 'Exit'),
    Tr('personal.cmd.exitHint', 'Close OBD-Studio Personal'));

  miFile.Caption := Tr('personal.menu.file', '&File');
  miVehicle.Caption := Tr('personal.menu.vehicle', '&Vehicle');
  miTests.Caption := Tr('personal.menu.tests', '&Tests');
  miDrive.Caption := Tr('personal.menu.drive', '&Drive');
  miTools.Caption := Tr('personal.menu.tools', 'T&ools');
  miView.Caption := Tr('personal.menu.view', 'Vie&w');
  miHelp.Caption := Tr('personal.menu.help', '&Help');

  // Tabs and groups in the order of the DFM.
  if rbnMain.Tabs.Count >= 4 then
  begin
    rbnMain.Tabs[0].Caption := Tr('personal.ribbon.home', 'Home');
    rbnMain.Tabs[1].Caption := Tr('personal.ribbon.testsDrive',
      'Tests and drive');
    rbnMain.Tabs[2].Caption := Tr('personal.ribbon.tools', 'Tools');
    rbnMain.Tabs[3].Caption := Tr('personal.ribbon.view', 'View');
  end;
  SetGroupCaption(0, 0, Tr('personal.ribbon.connection', 'Connection'));
  SetGroupCaption(0, 1, Tr('personal.nav.groupVehicle', 'Vehicle'));
  SetGroupCaption(1, 0, Tr('personal.nav.groupTests', 'Tests'));
  SetGroupCaption(1, 1, Tr('personal.nav.groupDrive', 'Drive'));
  SetGroupCaption(2, 0, Tr('personal.nav.groupTools', 'Tools'));
  SetGroupCaption(2, 1, Tr('personal.nav.settings', 'Settings'));
  SetGroupCaption(3, 0, Tr('personal.ribbon.theme', 'Theme'));
  SetGroupCaption(3, 1, Tr('personal.ribbon.density', 'Density'));
  SetGroupCaption(3, 2, Tr('personal.ribbon.units', 'Units'));
  SetGroupCaption(3, 3, Tr('personal.ribbon.commands', 'Commands'));
  SetGroupCaption(3, 4, Tr('personal.ribbon.help', 'Help'));
  rbnMain.SearchHint := Tr('personal.ribbon.search', 'Search commands  Alt+Q');
  TranslatePageCommands;
  // Top-level captions changed: the menu bar measures its items again.
  TitleBar.MenuChanged;
end;

procedure TfrmMain.TranslatePageCommands;

  procedure SetCommand(ACommand: TPersonalCommand; const ACaption,
    AHint: string);
  begin
    if FCommandActions[ACommand] <> nil then
    begin
      FCommandActions[ACommand].Caption := ACaption;
      FCommandActions[ACommand].Hint := AHint;
    end;
  end;

var
  I, J: Integer;
  Tab: TOBDRibbonTab;
begin
  SetCommand(pcScanVehicle, Tr('personal.cmd.scan', 'Scan vehicle'),
    Tr('personal.cmd.scanHint',
    'Quick scan of codes, readiness and key live values'));
  SetCommand(pcSaveVehicleReport, Tr('personal.cmd.healthReport',
    'Health report'), Tr('personal.cmd.healthReportHint',
    'Save a vehicle health report as text'));
  SetCommand(pcReadCodes, Tr('personal.cmd.readCodes', 'Read codes'),
    Tr('personal.cmd.readCodesHint',
    'Read the stored, pending and permanent trouble codes'));
  SetCommand(pcClearCodes, Tr('personal.cmd.clearCodes', 'Clear codes'),
    Tr('personal.cmd.clearCodesHint',
    'Erase the trouble codes and turn the MIL off (asks first)'));
  SetCommand(pcLookUpCode, Tr('personal.cmd.searchWeb', 'Search the web'),
    Tr('personal.cmd.searchWebHint', 'Search the web for the selected code'));
  SetCommand(pcSaveCodeReport, Tr('personal.cmd.saveCodeReport',
    'Save report'), Tr('personal.cmd.saveCodeReportHint',
    'Save the trouble-code report as text and HTML'));
  SetCommand(pcRefreshReadiness, Tr('personal.cmd.readReadiness',
    'Read readiness'), Tr('personal.cmd.readReadinessHint',
    'Read the readiness monitors again'));
  SetCommand(pcStartAdvisor, Tr('personal.cmd.startAdvisor', 'Start advisor'),
    Tr('personal.cmd.startAdvisorHint',
    'Drive-cycle advisor for the first incomplete monitor'));
  SetCommand(pcStopAdvisor, Tr('personal.cmd.stopAdvisor', 'Stop advisor'),
    Tr('personal.cmd.stopAdvisorHint', 'Stop the drive-cycle advisor'));
  SetCommand(pcRefreshSensors, Tr('personal.cmd.findSensors', 'Find sensors'),
    Tr('personal.cmd.findSensorsHint', 'Read the supported sensors again'));
  SetCommand(pcRecordLive, Tr('personal.cmd.recordCsv', 'Record CSV'),
    Tr('personal.cmd.recordCsvHint',
    'Record the selected sensors to a CSV file'));
  SetCommand(pcAddGauge, Tr('personal.cmd.addGauge', 'Add gauge'),
    Tr('personal.cmd.addGaugeHint', 'Add the chosen gauge to the dashboard'));
  SetCommand(pcEditDashboard, Tr('personal.cmd.editLayout', 'Edit layout'),
    Tr('personal.cmd.editLayoutHint', 'Move and resize the gauges'));
  SetCommand(pcSaveDashboard, Tr('personal.cmd.saveLayout', 'Save layout'),
    Tr('personal.cmd.saveLayoutHint', 'Save the dashboard layout'));
  SetCommand(pcResetDashboard, Tr('personal.cmd.defaultLayout',
    'Default layout'), Tr('personal.cmd.defaultLayoutHint',
    'Replace the layout with the default gauges (asks first)'));
  SetCommand(pcReadTests, Tr('personal.cmd.readTests', 'Read tests'),
    Tr('personal.cmd.readTestsHint',
    'Read the Mode 06 results and the in-use performance counters'));
  SetCommand(pcExportTests, Tr('personal.cmd.exportTests', 'Export CSV'),
    Tr('personal.cmd.exportTestsHint', 'Export the test results to a CSV file'));
  SetCommand(pcReadVehicleInfo, Tr('personal.cmd.readInfo', 'Read info'),
    Tr('personal.cmd.readInfoHint',
    'Read the VIN, calibration IDs and ECUs again'));
  SetCommand(pcCopyVehicleInfo, Tr('personal.cmd.copyInfo', 'Copy all'),
    Tr('personal.cmd.copyInfoHint',
    'Copy the vehicle information to the clipboard'));
  SetCommand(pcSaveVehicleInfo, Tr('personal.cmd.saveInfo', 'Save text'),
    Tr('personal.cmd.saveInfoHint', 'Save the vehicle information as text'));
  SetCommand(pcResetTrip, Tr('personal.cmd.resetTrip', 'Reset trip'),
    Tr('personal.cmd.resetTripHint',
    'Start a new trip for consumption, costs and CO2'));
  SetCommand(pcArmRun, Tr('personal.cmd.armRun', 'Arm run'),
    Tr('personal.cmd.armRunHint', 'Get ready: the run starts when you pull away'));
  SetCommand(pcCancelRun, Tr('personal.cmd.cancelRun', 'Cancel run'),
    Tr('personal.cmd.cancelRunHint', 'Cancel the armed or running run'));
  SetCommand(pcReadBattery, Tr('personal.cmd.readBattery', 'Read battery'),
    Tr('personal.cmd.readBatteryHint', 'Read the battery values again'));
  SetCommand(pcRefreshPorts, Tr('personal.cmd.refreshPorts', 'Refresh ports'),
    Tr('personal.cmd.refreshPortsHint',
    'Look for serial and USB ports again'));
  SetCommand(pcRecordSession, Tr('personal.cmd.recordSession',
    'Record session'), Tr('personal.cmd.recordSessionHint',
    'Record the whole session to a log you can replay'));
  SetCommand(pcRefreshRecordings, Tr('personal.cmd.refreshRecordings',
    'Refresh'), Tr('personal.cmd.refreshRecordingsHint',
    'Look for recordings again'));
  SetCommand(pcRenameRecording, Tr('personal.cmd.rename', 'Rename'),
    Tr('personal.cmd.renameHint', 'Rename the selected recording'));
  SetCommand(pcDeleteRecording, Tr('personal.cmd.delete', 'Delete'),
    Tr('personal.cmd.deleteHint',
    'Delete the selected recording (asks first)'));
  SetCommand(pcExportRecording, Tr('personal.cmd.exportCopy', 'Export copy'),
    Tr('personal.cmd.exportCopyHint', 'Save a copy of the selected recording'));
  SetCommand(pcShowRecording, Tr('personal.cmd.showFile', 'Show file'),
    Tr('personal.cmd.showFileHint',
    'Show the selected recording in Explorer'));
  SetCommand(pcClearConsole, Tr('personal.cmd.clearConsole', 'Clear'),
    Tr('personal.cmd.clearConsoleHint',
    'Clear the terminal, or read the activity log again'));
  SetCommand(pcSaveConsole, Tr('personal.cmd.saveLog', 'Save log'),
    Tr('personal.cmd.saveLogHint', 'Save the terminal or the activity log'));

  if FDiagnosisGroup <> nil then
    FDiagnosisGroup.Caption := Tr('personal.ribbon.diagnosis', 'Diagnosis');
  // A page tab and a page menu carry the name of their page.
  for I := 0 to High(FPageTabs) do
    if (FPageTabs[I] <> nil) and (I <= High(FPageActions)) and
      (FPageTabs[I].Tabs.Count > 0) then
    begin
      Tab := FPageTabs[I].Tabs[0];
      Tab.Caption := FPageActions[I].Caption;
      for J := 0 to Tab.Groups.Count - 1 do
        if Tab.Groups[J].Tag = 1 then
          Tab.Groups[J].Caption := Tr('personal.ribbon.share', 'Save and share')
        else
          Tab.Groups[J].Caption := Tr('personal.ribbon.actions', 'Actions');
    end;
  if (FPageMenu <> nil) and (FActive >= 0) and
    (FActive <= High(FPageActions)) then
    FPageMenu.Caption := FPageActions[FActive].Caption;
end;

{ Session ------------------------------------------------------------------- }

procedure TfrmMain.SessionStateChanged(Sender: TObject);
var
  I: Integer;
  Page: IPersonalPage;
begin
  case FSession.State of
    plsOnline:
      begin
        Log(TrFmt('personal.shell.logConnected', 'Connected: %s, %s',
          [FSession.AdapterName, FSession.ProtocolName]));
        if FSession.IsDemo then
          Notify(tkInfo, Tr('personal.shell.demoTitle', 'Demo car'),
            Tr('personal.shell.demoText',
            'You are looking at the built-in demo car. Connect an adapter ' +
            'on the Adapter page to read your own car.'))
        else
          Notify(tkSuccess, Tr('personal.shell.connectedTitle', 'Connected'),
            FSession.AdapterName);
      end;
    plsFailed:
      begin
        LogWarning(TrFmt('personal.shell.logFailed', 'Connection failed: %s',
          [FSession.LastError]));
        Notify(tkDanger, Tr('personal.shell.failedTitle', 'Not connected'),
          FSession.LastError);
      end;
    plsOffline:
      begin
        Log(Tr('personal.shell.logDisconnected', 'Disconnected'));
        FCodeCount := 0;
        FMIL := False;
      end;
  end;
  tmrBattery.Enabled := FSession.Online;
  SyncTitleButtons;
  SyncCommands;
  SyncStatus;
  if FSession.State <> plsConnecting then
    for I := 0 to High(FPages) do
      if Supports(FPages[I], IPersonalPage, Page) then
        Page.ConnectionChanged(FSession.Online);
end;

procedure TfrmMain.SessionStatus(Sender: TObject);
begin
  SyncStatus;
end;

procedure TfrmMain.tmrBatteryTimer(Sender: TObject);
begin
  FSession.RefreshBattery;
end;

procedure TfrmMain.SyncStatus;
var
  Link: TOBDStatusPanel;
begin
  Link := sbMain.Panels[SP_LINK];
  case FSession.State of
    plsOnline:
      begin
        Link.Text := Tr('personal.status.connected', 'Connected');
        Link.StatusKind := skSuccess;
      end;
    plsConnecting:
      begin
        Link.Text := Tr('personal.status.connecting', 'Connecting');
        Link.StatusKind := skWarning;
      end;
    plsFailed:
      begin
        Link.Text := Tr('personal.status.failed', 'Connection failed');
        Link.StatusKind := skDanger;
      end;
  else
    Link.Text := Tr('personal.status.offline', 'Not connected');
    Link.StatusKind := skNeutral;
  end;
  if FSession.Online then
  begin
    if FSession.IsDemo then
      sbMain.Panels[SP_ADAPTER].Text := Tr('personal.status.demo', 'Demo car')
    else
      sbMain.Panels[SP_ADAPTER].Text := FSession.AdapterName;
    sbMain.Panels[SP_PROTOCOL].Text := FSession.ProtocolName;
    if FSession.BatteryVolts > 0 then
      sbMain.Panels[SP_BATTERY].Text := FormatFloat('0.0', FSession.BatteryVolts) + ' V'
    else
      sbMain.Panels[SP_BATTERY].Text := '';
  end
  else
  begin
    sbMain.Panels[SP_ADAPTER].Text := '';
    sbMain.Panels[SP_PROTOCOL].Text := '';
    sbMain.Panels[SP_BATTERY].Text := '';
  end;
  sbMain.Panels[SP_ADAPTER].Visible := sbMain.Panels[SP_ADAPTER].Text <> '';
  sbMain.Panels[SP_PROTOCOL].Visible := sbMain.Panels[SP_PROTOCOL].Text <> '';
  sbMain.Panels[SP_BATTERY].Visible := sbMain.Panels[SP_BATTERY].Text <> '';

  if FSession.Online then
  begin
    sbMain.Panels[SP_CODES].Text := Format(TrPlural('personal.status.codes',
      FCodeCount, '%d code', '%d codes'), [FCodeCount]);
    if FMIL then
      sbMain.Panels[SP_CODES].Text := sbMain.Panels[SP_CODES].Text +
        '  ·  ' + Tr('personal.status.milOn', 'MIL on');
    sbMain.Panels[SP_CODES].Visible := True;
  end
  else
    sbMain.Panels[SP_CODES].Visible := False;

  if FSession.Online then
  begin
    TitleBar.StatusCaption := Link.Text;
    TitleBar.StatusKind := skSuccess;
  end
  else
  begin
    TitleBar.StatusCaption := Link.Text;
    TitleBar.StatusKind := Link.StatusKind;
  end;
  if FSession.VehicleVIN <> '' then
    TitleBar.Subtitle := FSession.VehicleVIN
  else
    TitleBar.Subtitle := '';
end;

{ Window -------------------------------------------------------------------- }

procedure TfrmMain.RestoreWindow;
var
  R: TRect;
begin
  if (FSettings.WindowLeft >= 0) and (FSettings.WindowTop >= 0) then
  begin
    R := Rect(FSettings.WindowLeft, FSettings.WindowTop,
      FSettings.WindowLeft + FSettings.WindowWidth,
      FSettings.WindowTop + FSettings.WindowHeight);
    // Only when the saved place is still on a monitor.
    if Screen.MonitorFromRect(R, mdNull) <> nil then
    begin
      Position := poDesigned;
      BoundsRect := R;
    end;
  end
  else
  begin
    Width := FSettings.WindowWidth;
    Height := FSettings.WindowHeight;
  end;
  if FSettings.WindowMaximized then
    WindowState := wsMaximized;
end;

procedure TfrmMain.StoreWindow;
var
  Placement: TWindowPlacement;
begin
  Placement.length := SizeOf(Placement);
  if GetWindowPlacement(Handle, @Placement) then
  begin
    FSettings.WindowLeft := Placement.rcNormalPosition.Left;
    FSettings.WindowTop := Placement.rcNormalPosition.Top;
    FSettings.WindowWidth := Placement.rcNormalPosition.Right -
      Placement.rcNormalPosition.Left;
    FSettings.WindowHeight := Placement.rcNormalPosition.Bottom -
      Placement.rcNormalPosition.Top;
  end;
  FSettings.WindowMaximized := WindowState = wsMaximized;
end;

{ Activity log -------------------------------------------------------------- }

procedure TfrmMain.WriteActivity(const ALevel, AText: string);
var
  Line, FileName: string;
begin
  Line := FormatDateTime('hh:nn:ss', Now) + '  ' + ALevel + '  ' + AText;
  FActivity.Add(Line);
  while FActivity.Count > ACTIVITY_MAX_LINES do
    FActivity.Delete(0);
  try
    FileName := TPath.Combine(FSettings.DataSubFolder('Logs'),
      'activity-' + FormatDateTime('yyyymmdd', Now) + '.log');
    TFile.AppendAllText(FileName, FormatDateTime('yyyy-mm-dd ', Now) + Line +
      sLineBreak, TEncoding.UTF8);
  except
    // The log file is a convenience; a full disk must not break the app.
  end;
end;

{ IPersonalHost ------------------------------------------------------------- }

function TfrmMain.Session: TPersonalSession;
begin
  Result := FSession;
end;

function TfrmMain.Settings: TPersonalSettings;
begin
  Result := FSettings;
end;

function TfrmMain.Connected: Boolean;
begin
  Result := (FSession <> nil) and FSession.Online;
end;

procedure TfrmMain.Notify(AKind: TOBDToastKind; const ATitle, AText: string);
begin
  OBDToasts.Show(AKind, ATitle, AText);
end;

procedure TfrmMain.Log(const AText: string);
begin
  WriteActivity('INFO', AText);
end;

procedure TfrmMain.LogWarning(const AText: string);
begin
  WriteActivity('WARN', AText);
end;

function TfrmMain.ActivityLog: TStrings;
begin
  Result := FActivity;
end;

procedure TfrmMain.SetProgress(const AText: string; APercent: Integer);
begin
  if APercent < 0 then
    sbMain.Panels[SP_PROGRESS].Visible := False
  else
  begin
    sbMain.Panels[SP_PROGRESS].Text := AText;
    if APercent > 100 then
      sbMain.Panels[SP_PROGRESS].Position := 100
    else
      sbMain.Panels[SP_PROGRESS].Position := APercent;
    sbMain.Panels[SP_PROGRESS].Visible := True;
  end;
end;

function TfrmMain.Confirm(AKind: TOBDDialogKind; const ATitle, AText,
  AConfirmCaption: string): Boolean;
begin
  dlgConfirm.Kind := AKind;
  dlgConfirm.Title := ATitle;
  dlgConfirm.Text := AText;
  dlgConfirm.Buttons := [mbYes, mbCancel];
  dlgConfirm.ButtonCaptions.Text := 'mbYes=' + AConfirmCaption + sLineBreak +
    'mbCancel=' + Tr('personal.common.cancel', 'Cancel');
  dlgConfirm.DefaultButton := mbCancel;
  if AKind = dkDanger then
    dlgConfirm.DangerButton := mbYes
  else
    dlgConfirm.DangerButton := mbNo;
  dlgConfirm.CheckBoxCaption := '';
  dlgConfirm.CheckBoxChecked := False;
  Result := dlgConfirm.Execute = mrYes;
end;

procedure TfrmMain.ShowPage(const AKey: string);
begin
  ActivatePage(PageIndex(AKey));
end;

procedure TfrmMain.SetBadge(const AKey: string; ACount: Integer);
var
  Item: TOBDSidebarItem;
begin
  Item := SidebarItemOf(PageIndex(AKey));
  if Item <> nil then
    Item.Badge := ACount;
end;

procedure TfrmMain.SetCodeSummary(ACount: Integer; AMIL: Boolean);
begin
  FCodeCount := ACount;
  FMIL := AMIL;
  SyncStatus;
end;

procedure TfrmMain.ToggleConnection;
begin
  case FSession.State of
    plsOnline, plsConnecting:
      FSession.Disconnect;
  else
    ShowPage(PAGE_ADAPTER);
  end;
end;

procedure TfrmMain.SettingsChanged;
begin
  ApplySettings;
  try
    FSettings.Save;
  except
    on E: Exception do
      LogWarning('Settings not saved: ' + E.Message);
  end;
end;

procedure TfrmMain.SetRecording(const AText: string);
begin
  sbMain.Panels[SP_RECORDING].Text := AText;
  sbMain.Panels[SP_RECORDING].Visible := AText <> '';
end;

end.
