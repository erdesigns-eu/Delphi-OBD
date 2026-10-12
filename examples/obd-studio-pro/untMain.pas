//------------------------------------------------------------------------------
//  untMain
//
//  Main form of the OBD Studio Pro example: the workshop functions an
//  OBD Studio Pro edition adds on top of the free OBD Studio example,
//  built from the 'OBD Studio Pro' palette page and the common
//  'OBD Studio' building blocks.
//
//    - The same chrome as the free example: TOBDTitleBar, TOBDRibbon with
//      a TOBDBackstage, TOBDSidebar, TOBDStatusBar and TOBDConnectionBar,
//      themed by one TOBDTheme (light / dark / Windows, desktop / tablet,
//      metric / imperial).
//    - Twelve pages, each a TFrame in its own unit (untPage*.pas), created
//      at run time in pnlPages: ECU network, service functions, coding,
//      flash, memory, radio code, keys and immobiliser, dyno, performance,
//      measurement, actuators and bus, and EV battery. A session log page
//      collects what the pages report.
//    - The pages only know untProPage: the form implements IProHost
//      (toasts, log, status bar progress, confirmation dialog, simulated
//      vehicle) and drives every page through IProPage.
//
//  The vehicle is the simulated Golf VII 1.6 TDI of the free example
//  (examples\obd-studio\untSimulator.pas), so both applications tell the
//  same story: a sticking EGR valve and P0401.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Types, System.UITypes, System.Actions, Vcl.ActnList, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Control, ERD.UI.Theme, ERD.UI.Hint,
  ERD.UI.Toast, ERD.UI.Dialogs, ERD.UI.TitleBar, ERD.UI.Ribbon,
  ERD.UI.Backstage, ERD.UI.Sidebar, ERD.UI.StatusBar, ERD.UI.Card,
  ERD.UI.Chips, ERD.UI.ConnectionBar, ERD.UI.LogViewer,
  untProPage, untSimulator;

type
  TfrmMain = class(TForm, IProHost)
    OBDTheme: TOBDTheme;
    OBDHintStyle: TOBDHintStyle;
    OBDToasts: TOBDToastManager;
    dlgConfirm: TOBDDialog;
    alMain: TActionList;
    actConnect: TAction;
    actDisconnect: TAction;
    actThemeLight: TAction;
    actThemeDark: TAction;
    actThemeWindows: TAction;
    actDensityDesktop: TAction;
    actDensityTablet: TAction;
    actUnitsMetric: TAction;
    actUnitsImperial: TAction;
    actAbout: TAction;
    tmrConnect: TTimer;
    tmrSimulation: TTimer;
    TitleBar: TOBDTitleBar;
    rbnMain: TOBDRibbon;
    sbMain: TOBDStatusBar;
    pnlBody: TPanel;
    sdbMain: TOBDSidebar;
    pnlPages: TPanel;
    cbConnection: TOBDConnectionBar;
    pgLog: TPanel;
    lvLog: TOBDLogViewer;
    bsMain: TOBDBackstage;
    cdAbout: TOBDCard;
    bnrAbout: TOBDBanner;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OBDThemeChange(Sender: TObject);
    procedure tmrConnectTimer(Sender: TObject);
    procedure tmrSimulationTimer(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actThemeExecute(Sender: TObject);
    procedure actDensityExecute(Sender: TObject);
    procedure actUnitsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure TitleBarButtonClick(Sender: TObject;
      AButton: TOBDCaptionButton);
    procedure rbnMainItemClick(Sender: TObject; AItem: TOBDRibbonItem);
    procedure sdbMainChange(Sender: TObject);
    procedure sbMainPanelClick(Sender: TObject; Panel: TOBDStatusPanel);
    procedure bsExitClick(Sender: TObject);
  private
    { Private declarations }
    FSim: TEngineSimulator;
    FPages: TArray<TControl>;
    FConnected: Boolean;
    FSyncing: Boolean;
    function IsDark: Boolean;
    procedure AddPage(APage: TControl);
    procedure CreatePages;
    procedure ShowPage(AIndex: Integer);
    procedure ApplyChrome;
    procedure SyncCommands;
    procedure PagesConnectionChanged;
    procedure ToastConnect(Sender: TObject);
  public
    { IProHost }
    function Simulator: TEngineSimulator;
    function Connected: Boolean;
    procedure Notify(AKind: TOBDToastKind; const ATitle, AText: string);
    procedure Log(const AText: string);
    procedure LogWarning(const AText: string);
    procedure SetProgress(const AText: string; APercent: Integer);
    function Confirm(AKind: TOBDDialogKind; const ATitle, AText,
      AConfirmCaption: string): Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  untPageEcuNetwork, untPageService, untPageCoding, untPageFlash,
  untPageMemory, untPageRadio, untPageSecurity, untPageDyno,
  untPagePerformance, untPageMeasurement, untPageBus, untPageEvBattery;

{$R *.dfm}

const
  VEHICLE_VIN = 'WVWZZZAUZGW123456';
  PROTOCOL_TEXT = 'ISO 15765-4 CAN (11 bit, 500 kbaud)';
  PROTOCOL_COMPACT = 'CAN 11/500';

  // Page indexes; the sidebar items carry them in Tag. The order is the
  // order of CreatePages.
  PAGE_ECU_NETWORK = 0;
  PAGE_LOG = 12;

  // Ribbon items with a Tag of PAGE_TAG + n open page n.
  PAGE_TAG = 100;

  // Status bar panels (see sbMain.Panels in the DFM).
  SP_LINK = 0;
  SP_PROTOCOL = 1;
  SP_PROGRESS = 2;
  SP_UNITS = 4;
  SP_THEME = 5;

  // Backstage items (see bsMain.Items in the DFM).
  BS_ABOUT = 0;

{ Start and stop ------------------------------------------------------------ }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Frames and controls created at run time (the pages, toasts and
  // dialogs) pick this theme up as well.
  TOBDTheme.RegisterDefault(OBDTheme);
  FSim := TEngineSimulator.Create;

  lvLog.Info('OBD Studio Pro started');
  CreatePages;
  ShowPage(PAGE_ECU_NETWORK);
  ApplyChrome;
  SyncCommands;
  tmrSimulation.Enabled := True;
  actConnect.Execute;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  tmrConnect.Enabled := False;
  tmrSimulation.Enabled := False;
  FreeAndNil(FSim);
end;

function TfrmMain.IsDark: Boolean;
begin
  Result := OBDIsDarkPalette(OBDTheme.Palette);
end;

{ Pages --------------------------------------------------------------------- }

procedure TfrmMain.AddPage(APage: TControl);
var
  Page: IProPage;
begin
  APage.Visible := False;
  APage.Parent := pnlPages;
  APage.Align := alClient;
  FPages := FPages + [APage];
  if Supports(APage, IProPage, Page) then
    Page.Init(Self);
end;

procedure TfrmMain.CreatePages;
begin
  // The order matches the Tag of the sidebar items and PAGE_TAG + n of
  // the ribbon items.
  AddPage(TfrmEcuNetwork.Create(Self));
  AddPage(TfrmService.Create(Self));
  AddPage(TfrmCoding.Create(Self));
  AddPage(TfrmFlash.Create(Self));
  AddPage(TfrmMemory.Create(Self));
  AddPage(TfrmRadio.Create(Self));
  AddPage(TfrmSecurity.Create(Self));
  AddPage(TfrmDyno.Create(Self));
  AddPage(TfrmPerformance.Create(Self));
  AddPage(TfrmMeasurement.Create(Self));
  AddPage(TfrmBus.Create(Self));
  AddPage(TfrmEvBattery.Create(Self));
  // The session log is designed on the form itself.
  AddPage(pgLog);
end;

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

procedure TfrmMain.PagesConnectionChanged;
var
  I: Integer;
  Page: IProPage;
begin
  for I := 0 to High(FPages) do
    if Supports(FPages[I], IProPage, Page) then
      Page.ConnectionChanged;
end;

{ Chrome -------------------------------------------------------------------- }

procedure TfrmMain.ApplyChrome;
var
  P: TOBDThemePalette;
  I: Integer;
begin
  // The OBD controls paint themselves; the plain VCL panels and the
  // page frames take the theme's background colour (the layout panels
  // inside the frames follow their frame through ParentColor).
  P := OBDTheme.Palette;
  Color := P.Background;
  pnlBody.Color := P.Background;
  pnlPages.Color := P.Background;
  for I := 0 to High(FPages) do
    if FPages[I] is TFrame then
    begin
      TFrame(FPages[I]).ParentBackground := False;
      TFrame(FPages[I]).Color := P.Background;
    end
    else if FPages[I] is TPanel then
      TPanel(FPages[I]).Color := P.Background;
  // The non-visual components have their own density.
  OBDToasts.Density := OBDTheme.Density;
  dlgConfirm.Density := OBDTheme.Density;
  OBDHintStyle.Density := OBDTheme.Density;
end;

procedure TfrmMain.OBDThemeChange(Sender: TObject);
begin
  ApplyChrome;
  SyncCommands;
end;

procedure TfrmMain.SyncCommands;
const
  UNIT_NAMES: array [TOBDUnitSystem] of string = ('Metric', 'Imperial');
var
  I: Integer;
begin
  // Ribbon items show their action's Checked state as Down.
  actThemeLight.Checked := OBDTheme.Mode = tmLight;
  actThemeDark.Checked := OBDTheme.Mode = tmDark;
  actThemeWindows.Checked := OBDTheme.Mode in [tmAuto, tmWindows];
  actDensityDesktop.Checked := OBDTheme.Density = dnDesktop;
  actDensityTablet.Checked := OBDTheme.Density = dnTablet;
  actUnitsMetric.Checked := OBDTheme.UnitSystem = usMetric;
  actUnitsImperial.Checked := OBDTheme.UnitSystem = usImperial;
  actConnect.Enabled := not FConnected and not tmrConnect.Enabled;
  actDisconnect.Enabled := FConnected or tmrConnect.Enabled;

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
    ShowPage(PAGE_LOG);
  end
  else if AButton.Name = 'help' then
    actAbout.Execute
  else if AButton.Name = 'connect' then
  begin
    if FConnected then
      actDisconnect.Execute
    else
      actConnect.Execute;
  end
  else if AButton.Name = 'log' then
    ShowPage(PAGE_LOG);
  SyncCommands;
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

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  bsMain.Show(BS_ABOUT);
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
  lvLog.Info('Connecting to the vehicle');
  tmrConnect.Enabled := True;
  SyncCommands;
end;

procedure TfrmMain.tmrConnectTimer(Sender: TObject);
begin
  tmrConnect.Enabled := False;
  FConnected := True;
  cbConnection.LinkState := lnkConnected;
  cbConnection.ProtocolText := PROTOCOL_COMPACT;
  cbConnection.VIN := VEHICLE_VIN;
  sbMain.Panels[SP_LINK].Text := 'Connected';
  sbMain.Panels[SP_LINK].StatusKind := skSuccess;
  sbMain.Panels[SP_PROTOCOL].Text := PROTOCOL_TEXT;
  TitleBar.StatusCaption := 'Connected';
  TitleBar.StatusKind := skSuccess;
  lvLog.Info('Connected: ' + PROTOCOL_TEXT + ', VIN ' + VEHICLE_VIN);
  OBDToasts.Show(tkSuccess, 'Connected to Golf VII 1.6 TDI', PROTOCOL_TEXT);
  PagesConnectionChanged;
  SyncCommands;
end;

procedure TfrmMain.actDisconnectExecute(Sender: TObject);
begin
  tmrConnect.Enabled := False;
  FConnected := False;
  cbConnection.LinkState := lnkDisconnected;
  cbConnection.ProtocolText := '';
  sbMain.Panels[SP_LINK].Text := 'Disconnected';
  sbMain.Panels[SP_LINK].StatusKind := skNeutral;
  sbMain.Panels[SP_PROTOCOL].Text := '';
  SetProgress('', -1);
  TitleBar.StatusCaption := 'Offline';
  TitleBar.StatusKind := skNeutral;
  lvLog.Info('Disconnected');
  OBDToasts.Show(tkInfo, 'Disconnected',
    'The adapter was closed. Values on screen are no longer updated.',
    'Connect', ToastConnect);
  PagesConnectionChanged;
  SyncCommands;
end;

procedure TfrmMain.ToastConnect(Sender: TObject);
begin
  actConnect.Execute;
end;

procedure TfrmMain.tmrSimulationTimer(Sender: TObject);
var
  I: Integer;
  Page: IProPage;
begin
  if FConnected then
    FSim.Step(tmrSimulation.Interval / 1000);
  // Every page ticks, also the hidden ones, so a flash or a key
  // learning procedure keeps running while another page is shown.
  for I := 0 to High(FPages) do
    if Supports(FPages[I], IProPage, Page) then
      Page.Tick;
end;

{ IProHost ------------------------------------------------------------------ }

function TfrmMain.Simulator: TEngineSimulator;
begin
  Result := FSim;
end;

function TfrmMain.Connected: Boolean;
begin
  Result := FConnected;
end;

procedure TfrmMain.Notify(AKind: TOBDToastKind; const ATitle, AText: string);
begin
  OBDToasts.Show(AKind, ATitle, AText);
end;

procedure TfrmMain.Log(const AText: string);
begin
  lvLog.Info(AText);
end;

procedure TfrmMain.LogWarning(const AText: string);
var
  I: Integer;
begin
  lvLog.Warn(AText);
  // The bell in the title bar counts the warnings until it is clicked.
  for I := 0 to TitleBar.Buttons.Count - 1 do
    if TitleBar.Buttons[I].Name = 'notifications' then
      TitleBar.Buttons[I].Badge := IntToStr(StrToIntDef(
        TitleBar.Buttons[I].Badge, 0) + 1);
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
  dlgConfirm.ButtonCaptions.Text := 'mbYes=' + AConfirmCaption;
  dlgConfirm.DefaultButton := mbCancel;
  if AKind = dkDanger then
    dlgConfirm.DangerButton := mbYes
  else
    dlgConfirm.DangerButton := mbNo;
  dlgConfirm.CheckBoxCaption := '';
  dlgConfirm.CheckBoxChecked := False;
  Result := dlgConfirm.Execute = mrYes;
end;

end.
