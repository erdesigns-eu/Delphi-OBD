//------------------------------------------------------------------------------
//  untMain
//
//  Main form of OBD-Studio Personal: the shell around the pages.
//
//  Title bar (theme, density, help, connect), the sidebar with the page
//  groups of the mockups (Vehicle, Tests, Drive, Tools, Settings), the
//  status bar (link, adapter, protocol, battery, progress, codes and the
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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  ERD.I18n,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Theme,
  ERD.UI.TitleBar,
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
    sdbMain: TOBDSidebar;
    pnlPages: TPanel;
    sbMain: TOBDStatusBar;
    OBDTheme: TOBDTheme;
    OBDHintStyle: TOBDHintStyle;
    OBDToasts: TOBDToastManager;
    dlgConfirm: TOBDDialog;
    tmrBattery: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sdbMainChange(Sender: TObject);
    procedure TitleBarButtonClick(Sender: TObject; AButton: TOBDCaptionButton);
    procedure sbMainPanelClick(Sender: TObject; Panel: TOBDStatusPanel);
    procedure OBDThemeChange(Sender: TObject);
    procedure tmrBatteryTimer(Sender: TObject);
  strict private
    FSettings: TPersonalSettings;
    FSession: TPersonalSession;
    FPages: TArray<TControl>;
    FKeys: TArray<string>;
    FActive: Integer;
    FSyncing: Boolean;
    FActivity: TStringList;
    FCodeCount: Integer;
    FMIL: Boolean;
    procedure WMPersonalStarted(var Msg: TMessage); message WM_PERSONAL_STARTED;
    procedure AddPage(APage: TControl);
    procedure CreatePages;
    function PageIndex(const AKey: string): Integer;
    function SidebarItemOf(AIndex: Integer): TOBDSidebarItem;
    procedure ActivatePage(AIndex: Integer);
    procedure ApplyChrome;
    procedure ApplySettings;
    procedure ApplyLanguage;
    procedure TranslateShell;
    procedure SyncTitleButtons;
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

{ Start and stop ------------------------------------------------------------ }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Frames and dialogs created at run time pick this theme up as well.
  TOBDTheme.RegisterDefault(OBDTheme);
  FActivity := TStringList.Create;
  FSettings := TPersonalSettings.Create;
  FSettings.Load;

  // The PID and trouble code catalogues ship next to the program.
  TOBDServiceCatalog.Default.LoadDirectory(
    TPath.Combine(AppFolder, 'catalogs'));

  FSession := TPersonalSession.Create(Self);
  FSession.OnStateChanged := SessionStateChanged;
  FSession.OnStatus := SessionStatus;
  FSession.Inspection.Country := FSettings.InspectionCountry;

  FActive := -1;
  CreatePages;
  RestoreWindow;
  ApplySettings;
  SyncStatus;
  Log('OBD-Studio Personal started');
  // The welcome wizard and the automatic connect run once the window is
  // on screen.
  PostMessage(Handle, WM_PERSONAL_STARTED, 0, 0);
end;

procedure TfrmMain.WMPersonalStarted(var Msg: TMessage);
begin
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
  if FActive >= 0 then
    FSettings.LastPage := FKeys[FActive];
  StoreWindow;
  try
    FSettings.Save;
  except
    on E: Exception do
      // Closing must not fail because the settings folder is read-only.
      WriteActivity('WARN', 'Settings not saved: ' + E.Message);
  end;
  FSession.Disconnect;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  tmrBattery.Enabled := False;
  FSession.OnStateChanged := nil;
  FSession.OnStatus := nil;
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
      Button.Down := FSession.Online;
      if FSession.Online then
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
const
  // Sidebar captions by page index (the Tag of the items).
  KEYS: array[0..13] of string = ('overview', 'codes', 'readiness', 'live',
    'dashboard', 'tests', 'info', 'fuel', 'performance', 'ev', 'adapter',
    'recordings', 'console', 'settings');
  CAPTIONS: array[0..13] of string = ('Overview', 'Trouble codes',
    'Readiness', 'Live data', 'Dashboard', 'Monitor tests', 'Vehicle info',
    'Fuel economy', 'Performance', 'EV battery', 'Adapter', 'Recordings',
    'Console', 'Settings');
var
  I: Integer;
  Item: TOBDSidebarItem;
  Page: IPersonalPage;
begin
  for I := 0 to High(KEYS) do
  begin
    Item := SidebarItemOf(I);
    if Item <> nil then
      Item.Caption := Tr('personal.nav.' + KEYS[I], CAPTIONS[I]);
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
  Result := FSession.Online;
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
