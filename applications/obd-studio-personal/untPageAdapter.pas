//------------------------------------------------------------------------------
//  untPageAdapter
//
//  Adapter page for OBD-Studio Personal. It lets the user choose how the
//  application talks to the vehicle, opens or closes the session and shows the
//  adapter identity, protocol and troubleshooting hints.
//------------------------------------------------------------------------------

unit untPageAdapter;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Win.Registry,
  System.Math,
  Vcl.Controls,
  Vcl.Forms,
  ERD.I18n,
  ERD.UI.Paint,
  ERD.UI.Dialogs,
  ERD.UI.Card,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.Segmented,
  ERD.UI.ScrollBox,
  ERD.UI.KeyValueList,
  ERD.UI.Memo,
  ERD.UI.Chips,
  ERD.Adapter.Types,
  untSettings,
  untSession,
  untPersonalPage;

type
  TfrmPageAdapter = class(TFrame, IPersonalPage, IPersonalCommands)
  strict private
    FHost: IPersonalHost;
    FLoading: Boolean;
    FScroll: TOBDScrollBox;
    FTitle: TOBDDialogText;
    FSubtitle: TOBDDialogText;
    FRefreshPortsButton: TOBDButton;
    FConnectButton: TOBDButton;
    FDisconnectButton: TOBDButton;
    FKindLabel: TOBDDialogText;
    FKindSegment: TOBDSegmented;
    FConnectionCard: TOBDCard;
    FPortLabel: TOBDDialogText;
    FPortCombo: TOBDComboBox;
    FBaudLabel: TOBDDialogText;
    FBaudCombo: TOBDComboBox;
    FHostLabel: TOBDDialogText;
    FHostEdit: TOBDEdit;
    FWiFiPortLabel: TOBDDialogText;
    FWiFiPortEdit: TOBDEdit;
    FBLELabel: TOBDDialogText;
    FBLEEdit: TOBDEdit;
    FAutoConnectCheck: TOBDCheckBox;
    FStateBanner: TOBDBanner;
    FIdentityCard: TOBDCard;
    FIdentityList: TOBDKeyValueList;
    FLogCard: TOBDCard;
    FLogMemo: TOBDMemo;
    procedure BuildUI;
    function NewText(AParent: TWinControl; AStyle: TOBDDialogTextStyle):
      TOBDDialogText;
    function NewCombo(AParent: TWinControl): TOBDComboBox;
    function NewEdit(AParent: TWinControl): TOBDEdit;
    function NewButton(AParent: TWinControl; AKind: TOBDButtonKind;
      AGlyph: TOBDGlyph): TOBDButton;
    procedure LayoutControls;
    function CurrentKind: TPersonalConnectionKind;
    procedure ConnectionKindChanged(Sender: TObject);
    procedure RefreshPortsClicked(Sender: TObject);
    procedure ConnectClicked(Sender: TObject);
    procedure DisconnectClicked(Sender: TObject);
    procedure AutoConnectChanged(Sender: TObject);
    procedure LoadSettingsToControls;
    procedure ApplyControlsToSettings;
    procedure EnumeratePorts;
    procedure SelectComboText(ACombo: TOBDComboBox; const AText: string);
    procedure UpdateConnectionControls;
    procedure UpdateLog;
    procedure UpdateState;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
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

function NoPortsText: string;
begin
  Result := Tr('personal.adapter.noPorts', '(no COM ports found)');
end;

function CapabilityName(ACapability: TOBDAdapterCapability): string;
begin
  case ACapability of
    acCAN:
      Result := Tr('personal.adapter.capCAN', 'CAN');
    acCANFD:
      Result := Tr('personal.adapter.capCANFD', 'CAN-FD');
    acISOTP:
      Result := Tr('personal.adapter.capISOTP', 'ISO-TP');
    acISOTPLongFrame:
      Result := Tr('personal.adapter.capLongISOTP', 'Long ISO-TP frames');
    acDoIP:
      Result := Tr('personal.adapter.capDoIP', 'Diagnostics over IP');
    acJ1939:
      Result := Tr('personal.adapter.capJ1939', 'SAE J1939');
    acKLine:
      Result := Tr('personal.adapter.capKLine', 'K-line');
    acVoltageMonitor:
      Result := Tr('personal.adapter.capVoltage', 'Battery voltage');
    acSecureOnboard:
      Result := Tr('personal.adapter.capSecureOnboard',
        'Secure on-board communication');
    acJ2534:
      Result := Tr('personal.adapter.capJ2534', 'SAE J2534');
    acJ2534v2:
      Result := Tr('personal.adapter.capJ2534v2', 'SAE J2534-2');
    acSTCommands:
      Result := Tr('personal.adapter.capSTCommands', 'OBDLink ST commands');
    acMessageFilters:
      Result := Tr('personal.adapter.capMessageFilters',
        'Adapter message filters');
    acProgrammableHeaders:
      Result := Tr('personal.adapter.capHeaders', 'Programmable headers');
    acAdapterMultiFrame:
      Result := Tr('personal.adapter.capMultiFrame',
        'Adapter multi-frame assembly');
  else
    Result := TrFmt('personal.adapter.capabilityGeneric', 'Capability %d',
      [Ord(ACapability)]);
  end;
end;

function LinkStateText(AState: TPersonalLinkState): string;
begin
  case AState of
    plsConnecting:
      Result := Tr('personal.adapter.stateConnecting', 'Connecting');
    plsOnline:
      Result := Tr('personal.adapter.stateOnline', 'Connected');
    plsFailed:
      Result := Tr('personal.adapter.stateFailed', 'Connection failed');
  else
    Result := Tr('personal.adapter.stateOffline', 'Not connected');
  end;
end;

function LinkStateBannerKind(AState: TPersonalLinkState): TOBDBannerKind;
begin
  case AState of
    plsConnecting:
      Result := bnWarning;
    plsOnline:
      Result := bnSuccess;
    plsFailed:
      Result := bnDanger;
  else
    Result := bnInfo;
  end;
end;

function LinkProgressText(AState: TPersonalLinkState): string;
begin
  case AState of
    plsConnecting:
      Result := Tr('personal.adapter.progressConnecting',
        'Opening the adapter, detecting the chip and finding the protocol');
    plsOnline:
      Result := Tr('personal.adapter.progressOnline', 'Ready');
    plsFailed:
      Result := Tr('personal.adapter.progressFailed',
        'Check the hints below and try again');
  else
    Result := Tr('personal.adapter.progressOffline',
      'Choose a connection and press Connect');
  end;
end;

{ TfrmPageAdapter }

constructor TfrmPageAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentBackground := False;
  BuildUI;
  Translate;
end;

function TfrmPageAdapter.NewText(AParent: TWinControl;
  AStyle: TOBDDialogTextStyle): TOBDDialogText;
begin
  Result := TOBDDialogText.Create(Self);
  Result.Parent := AParent;
  Result.Style := AStyle;
  Result.WordWrap := True;
end;

function TfrmPageAdapter.NewCombo(AParent: TWinControl): TOBDComboBox;
begin
  Result := TOBDComboBox.Create(Self);
  Result.Parent := AParent;
  Result.DropDownCount := 12;
end;

function TfrmPageAdapter.NewEdit(AParent: TWinControl): TOBDEdit;
begin
  Result := TOBDEdit.Create(Self);
  Result.Parent := AParent;
end;

function TfrmPageAdapter.NewButton(AParent: TWinControl;
  AKind: TOBDButtonKind; AGlyph: TOBDGlyph): TOBDButton;
begin
  Result := TOBDButton.Create(Self);
  Result.Parent := AParent;
  Result.Kind := AKind;
  Result.Glyph := AGlyph;
  Result.AutoSize := False;
end;

procedure TfrmPageAdapter.BuildUI;
begin
  Align := alClient;

  FScroll := TOBDScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.LineStep := 64;

  FTitle := NewText(FScroll, dtsHeading);
  FSubtitle := NewText(FScroll, dtsSubtle);
  FRefreshPortsButton := NewButton(FScroll, bkSecondary, glRefresh);
  FRefreshPortsButton.OnClick := RefreshPortsClicked;
  FConnectButton := NewButton(FScroll, bkPrimary, glConnect);
  FConnectButton.Default := True;
  FConnectButton.OnClick := ConnectClicked;
  FDisconnectButton := NewButton(FScroll, bkSecondary, glDisconnect);
  FDisconnectButton.OnClick := DisconnectClicked;

  FKindLabel := NewText(FScroll, dtsCaps);
  FKindSegment := TOBDSegmented.Create(Self);
  FKindSegment.Parent := FScroll;
  FKindSegment.AutoSize := False;
  FKindSegment.OnChange := ConnectionKindChanged;

  FConnectionCard := TOBDCard.Create(Self);
  FConnectionCard.Parent := FScroll;
  FConnectionCard.StatusEdge := ceAccent;

  FPortLabel := NewText(FConnectionCard, dtsCaps);
  FPortCombo := NewCombo(FConnectionCard);
  FBaudLabel := NewText(FConnectionCard, dtsCaps);
  FBaudCombo := NewCombo(FConnectionCard);
  FBaudCombo.Items.Add('9600');
  FBaudCombo.Items.Add('19200');
  FBaudCombo.Items.Add('38400');
  FBaudCombo.Items.Add('57600');
  FBaudCombo.Items.Add('115200');
  FBaudCombo.Items.Add('230400');
  FBaudCombo.Items.Add('460800');
  FBaudCombo.Items.Add('500000');
  FBaudCombo.Items.Add('921600');
  FBaudCombo.Items.Add('1000000');
  FBaudCombo.Items.Add('2000000');
  FHostLabel := NewText(FConnectionCard, dtsCaps);
  FHostEdit := NewEdit(FConnectionCard);
  FWiFiPortLabel := NewText(FConnectionCard, dtsCaps);
  FWiFiPortEdit := NewEdit(FConnectionCard);
  FWiFiPortEdit.NumbersOnly := True;
  FBLELabel := NewText(FConnectionCard, dtsCaps);
  FBLEEdit := NewEdit(FConnectionCard);
  FBLEEdit.Mono := True;
  FAutoConnectCheck := TOBDCheckBox.Create(Self);
  FAutoConnectCheck.Parent := FConnectionCard;
  FAutoConnectCheck.Style := csSwitch;
  FAutoConnectCheck.OnChange := AutoConnectChanged;

  FStateBanner := TOBDBanner.Create(Self);
  FStateBanner.Parent := FScroll;
  FStateBanner.AutoSize := False;

  FIdentityCard := TOBDCard.Create(Self);
  FIdentityCard.Parent := FScroll;
  FIdentityCard.StatusEdge := ceNeutral;
  FIdentityList := TOBDKeyValueList.Create(Self);
  FIdentityList.Parent := FIdentityCard;
  FIdentityList.KeyWidth := 132;
  FIdentityList.WordWrap := True;

  FLogCard := TOBDCard.Create(Self);
  FLogCard.Parent := FScroll;
  FLogMemo := TOBDMemo.Create(Self);
  FLogMemo.Parent := FLogCard;
  FLogMemo.ReadOnly := True;
  FLogMemo.Mono := True;
  FLogMemo.WordWrap := False;

  LayoutControls;
end;

procedure TfrmPageAdapter.Resize;
begin
  inherited Resize;
  LayoutControls;
end;

procedure TfrmPageAdapter.LayoutControls;
var
  ContentW, Gap, ButtonY, X, Y, LeftW, RightW, RightX, CardH, FieldW,
  LabelW, EditX, EditW, HalfW, BottomY: Integer;
  TwoColumns: Boolean;
begin
  if FScroll = nil then
    Exit;
  FScroll.SetBounds(0, 0, ClientWidth, ClientHeight);
  ContentW := Max(720, ClientWidth - 48);
  Gap := 16;
  TwoColumns := ContentW >= 920;

  FTitle.SetBounds(24, 18, ContentW, 28);
  X := 24 + ContentW;
  ButtonY := 22;
  FDisconnectButton.SetBounds(X - 116, ButtonY, 116, 34);
  X := FDisconnectButton.Left - 8;
  FConnectButton.SetBounds(X - 104, ButtonY, 104, 34);
  X := FConnectButton.Left - 8;
  FRefreshPortsButton.SetBounds(X - 116, ButtonY, 116, 34);
  FSubtitle.SetBounds(24, 46, Max(240, FRefreshPortsButton.Left - 40), 38);

  FKindLabel.SetBounds(24, 90, ContentW, 18);
  FKindSegment.SetBounds(24, 110, ContentW, 34);

  Y := 162;
  if TwoColumns then
  begin
    LeftW := 430;
    RightW := ContentW - LeftW - Gap;
    RightX := 24 + LeftW + Gap;
    CardH := 408;
    FConnectionCard.SetBounds(24, Y, LeftW, CardH);
    FStateBanner.SetBounds(RightX, Y, RightW, 72);
    FIdentityCard.SetBounds(RightX, Y + 88, RightW, 300);
    FLogCard.SetBounds(RightX, Y + 404, RightW, 250);
    BottomY := Y + 654;
  end
  else
  begin
    LeftW := ContentW;
    RightW := ContentW;
    RightX := 24;
    CardH := 408;
    FConnectionCard.SetBounds(24, Y, LeftW, CardH);
    FStateBanner.SetBounds(RightX, Y + CardH + Gap, RightW, 72);
    FIdentityCard.SetBounds(RightX, FStateBanner.Top + 88, RightW, 300);
    FLogCard.SetBounds(RightX, FIdentityCard.Top + FIdentityCard.Height + Gap,
      RightW, 250);
    BottomY := FLogCard.Top + FLogCard.Height;
  end;

  FieldW := LeftW - 32;
  LabelW := 126;
  EditX := 16 + LabelW;
  EditW := FieldW - LabelW;
  HalfW := (EditW - Gap) div 2;

  FPortLabel.SetBounds(16, 66, LabelW - 8, 18);
  FPortCombo.SetBounds(EditX, 62, EditW, 30);
  FBaudLabel.SetBounds(16, 108, LabelW - 8, 18);
  FBaudCombo.SetBounds(EditX, 104, EditW, 30);
  FHostLabel.SetBounds(16, 154, LabelW - 8, 18);
  FHostEdit.SetBounds(EditX, 150, EditW, 30);
  FWiFiPortLabel.SetBounds(16, 196, LabelW - 8, 18);
  FWiFiPortEdit.SetBounds(EditX, 192, HalfW, 30);
  FBLELabel.SetBounds(16, 242, LabelW - 8, 18);
  FBLEEdit.SetBounds(EditX, 238, EditW, 30);
  FAutoConnectCheck.SetBounds(16, 306, FieldW, 34);

  FIdentityList.SetBounds(16, 62, FIdentityCard.Width - 32,
    FIdentityCard.Height - 78);
  FLogMemo.SetBounds(16, 62, FLogCard.Width - 32, FLogCard.Height - 78);

  if BottomY > 0 then
    FScroll.Height := ClientHeight;
end;

function TfrmPageAdapter.PageKey: string;
begin
  Result := PAGE_ADAPTER;
end;

procedure TfrmPageAdapter.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  LoadSettingsToControls;
  UpdateState;
end;

procedure TfrmPageAdapter.Activated(AActive: Boolean);
begin
  if AActive then
  begin
    LoadSettingsToControls;
    UpdateState;
  end;
end;

procedure TfrmPageAdapter.ConnectionChanged(AConnected: Boolean);
begin
  UpdateState;
end;

procedure TfrmPageAdapter.UnitsChanged;
begin
  UpdateState;
end;

procedure TfrmPageAdapter.Translate;
var
  Index: Integer;
begin
  FTitle.Caption := Tr('personal.adapter.title', 'Adapter');
  FSubtitle.Caption := Tr('personal.adapter.subtitle',
    'Choose the connection, then connect to the vehicle or the demo car.');
  FRefreshPortsButton.Caption := Tr('personal.adapter.refreshPorts',
    'Refresh ports');
  FConnectButton.Caption := Tr('personal.adapter.connect', 'Connect');
  FDisconnectButton.Caption := Tr('personal.adapter.disconnect', 'Disconnect');
  FKindLabel.Caption := Tr('personal.adapter.connectionType',
    'Connection type');
  FConnectionCard.Title := Tr('personal.adapter.connectionTitle',
    'Connection settings');
  FPortLabel.Caption := Tr('personal.adapter.port', 'COM port');
  FBaudLabel.Caption := Tr('personal.adapter.baud', 'Baud rate');
  FHostLabel.Caption := Tr('personal.adapter.wifiHost', 'Wi-Fi host');
  FWiFiPortLabel.Caption := Tr('personal.adapter.wifiPort', 'Wi-Fi port');
  FBLELabel.Caption := Tr('personal.adapter.bleAddress', 'BLE address');
  FAutoConnectCheck.Caption := Tr('personal.adapter.autoConnect',
    'Connect automatically on start');
  FIdentityCard.Title := Tr('personal.adapter.identityTitle',
    'Adapter identity and capabilities');
  FLogCard.Title := Tr('personal.adapter.logTitle', 'Connection log');
  FLogMemo.TextHint := Tr('personal.adapter.logEmpty',
    'Connect to show adapter activity.');

  Index := FKindSegment.ItemIndex;
  FLoading := True;
  try
    FKindSegment.Items.BeginUpdate;
    try
      FKindSegment.Items.Clear;
      FKindSegment.Items.Add(Tr('personal.adapter.kindSerial',
        'USB / Bluetooth COM port'));
      FKindSegment.Items.Add(Tr('personal.adapter.kindWiFi', 'Wi-Fi adapter'));
      FKindSegment.Items.Add(Tr('personal.adapter.kindBLE', 'Bluetooth LE'));
      FKindSegment.Items.Add(Tr('personal.adapter.kindDemo', 'Demo car'));
    finally
      FKindSegment.Items.EndUpdate;
    end;
    if (Index >= 0) and (Index < FKindSegment.Items.Count) then
      FKindSegment.ItemIndex := Index
    else if FKindSegment.Items.Count > 0 then
      FKindSegment.ItemIndex := Ord(pckDemo);
  finally
    FLoading := False;
  end;

  UpdateConnectionControls;
  UpdateState;
  LayoutControls;
end;

function TfrmPageAdapter.CurrentKind: TPersonalConnectionKind;
begin
  if (FKindSegment.ItemIndex >= Ord(Low(TPersonalConnectionKind))) and
    (FKindSegment.ItemIndex <= Ord(High(TPersonalConnectionKind))) then
    Result := TPersonalConnectionKind(FKindSegment.ItemIndex)
  else
    Result := pckDemo;
end;

procedure TfrmPageAdapter.SelectComboText(ACombo: TOBDComboBox;
  const AText: string);
var
  Index: Integer;
begin
  Index := ACombo.Items.IndexOf(AText);
  if Index >= 0 then
    ACombo.ItemIndex := Index
  else if ACombo.Items.Count > 0 then
    ACombo.ItemIndex := 0;
end;

procedure TfrmPageAdapter.EnumeratePorts;
var
  Reg: TRegistry;
  Names: TStringList;
  Ports: TStringList;
  I: Integer;
  Port: string;
begin
  Reg := TRegistry.Create(KEY_READ);
  Names := TStringList.Create;
  Ports := TStringList.Create;
  try
    Ports.Sorted := True;
    Ports.Duplicates := dupIgnore;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
    try
      Reg.GetValueNames(Names);
      for I := 0 to Names.Count - 1 do
      begin
        Port := Trim(Reg.ReadString(Names[I]));
        if Port <> '' then
          Ports.Add(Port);
      end;
    finally
      Reg.CloseKey;
    end;
    FPortCombo.Items.Assign(Ports);
    if FPortCombo.Items.Count = 0 then
      FPortCombo.Items.Add(NoPortsText);
  finally
    Ports.Free;
    Names.Free;
    Reg.Free;
  end;
end;

procedure TfrmPageAdapter.LoadSettingsToControls;
var
  Settings: TPersonalSettings;
begin
  if FHost = nil then
    Exit;
  Settings := FHost.Settings;
  FLoading := True;
  try
    EnumeratePorts;
    if FKindSegment.Items.Count = 0 then
      Translate;
    FKindSegment.ItemIndex := Ord(Settings.ConnectionKind);
    if Settings.SerialPort <> '' then
    begin
      if FPortCombo.Items.IndexOf(Settings.SerialPort) < 0 then
        FPortCombo.Items.Insert(0, Settings.SerialPort);
      SelectComboText(FPortCombo, Settings.SerialPort);
    end
    else if FPortCombo.Items.Count > 0 then
      FPortCombo.ItemIndex := 0;
    SelectComboText(FBaudCombo, IntToStr(Settings.BaudRate));
    FHostEdit.Text := Settings.WiFiHost;
    FWiFiPortEdit.Text := IntToStr(Settings.WiFiPort);
    FBLEEdit.Text := Settings.BLEAddress;
    FAutoConnectCheck.Checked := Settings.AutoConnect;
  finally
    FLoading := False;
  end;
  UpdateConnectionControls;
end;

procedure TfrmPageAdapter.ApplyControlsToSettings;
var
  Settings: TPersonalSettings;
begin
  if FHost = nil then
    Exit;
  Settings := FHost.Settings;
  Settings.ConnectionKind := CurrentKind;
  if (FPortCombo.ItemIndex >= 0) and (FPortCombo.Text <> NoPortsText) then
    Settings.SerialPort := FPortCombo.Text;
  Settings.BaudRate := StrToIntDef(FBaudCombo.Text, Settings.BaudRate);
  Settings.WiFiHost := Trim(FHostEdit.Text);
  Settings.WiFiPort := StrToIntDef(FWiFiPortEdit.Text, Settings.WiFiPort);
  Settings.BLEAddress := Trim(FBLEEdit.Text);
  Settings.AutoConnect := FAutoConnectCheck.Checked;
end;

procedure TfrmPageAdapter.UpdateConnectionControls;
var
  Kind: TPersonalConnectionKind;
  SerialEnabled: Boolean;
  WiFiEnabled: Boolean;
  BLEEnabled: Boolean;
begin
  Kind := CurrentKind;
  SerialEnabled := Kind = pckSerial;
  WiFiEnabled := Kind = pckWiFi;
  BLEEnabled := Kind = pckBLE;
  FPortLabel.Enabled := SerialEnabled;
  FPortCombo.Enabled := SerialEnabled;
  FBaudLabel.Enabled := SerialEnabled;
  FBaudCombo.Enabled := SerialEnabled;
  FHostLabel.Enabled := WiFiEnabled;
  FHostEdit.Enabled := WiFiEnabled;
  FWiFiPortLabel.Enabled := WiFiEnabled;
  FWiFiPortEdit.Enabled := WiFiEnabled;
  FBLELabel.Enabled := BLEEnabled;
  FBLEEdit.Enabled := BLEEnabled;
end;

procedure TfrmPageAdapter.UpdateLog;
var
  Log: TStrings;
  I, Start: Integer;
begin
  FLogMemo.Lines.BeginUpdate;
  try
    FLogMemo.Clear;
    FLogMemo.Lines.Add(Tr('personal.adapter.tipsTitle',
      'Troubleshooting tips:'));
    FLogMemo.Lines.Add(Tr('personal.adapter.tipIgnition',
      'Turn the ignition on before connecting.'));
    FLogMemo.Lines.Add(Tr('personal.adapter.tipLeds',
      'Check the adapter LED: it should have power and activity.'));
    FLogMemo.Lines.Add(Tr('personal.adapter.tipPairing',
      'Pair Bluetooth adapters in Windows before selecting the COM port.'));
    FLogMemo.Lines.Add(Tr('personal.adapter.tipClone',
      'If a clone fails, try a lower baud rate or the demo car.'));
    if FHost <> nil then
    begin
      Log := FHost.ActivityLog;
      if (Log <> nil) and (Log.Count > 0) then
      begin
        FLogMemo.Lines.Add('');
        FLogMemo.Lines.Add(Tr('personal.adapter.recentActivity',
          'Recent activity:'));
        Start := Max(0, Log.Count - 200);
        for I := Start to Log.Count - 1 do
          FLogMemo.Lines.Add(Log[I]);
      end;
    end;
  finally
    FLogMemo.Lines.EndUpdate;
  end;
end;

procedure TfrmPageAdapter.UpdateState;
var
  State: TPersonalLinkState;
  Identity: TOBDAdapterIdentity;
  Battery: Double;
  LastError: string;
  Item: TOBDKeyValueItem;
  Cap: TOBDAdapterCapability;
  AddedCapability: Boolean;
  CategoryIdentity: string;
  CategoryCapabilities: string;
begin
  State := plsOffline;
  LastError := '';
  if FHost <> nil then
  begin
    State := FHost.Session.State;
    LastError := FHost.Session.LastError;
  end;

  FStateBanner.Kind := LinkStateBannerKind(State);
  FStateBanner.Title := LinkStateText(State);
  if (State = plsFailed) and (LastError <> '') then
    FStateBanner.Text := LastError
  else
    FStateBanner.Text := LinkProgressText(State);

  FConnectButton.Enabled := (FHost <> nil) and
    not(State in [plsConnecting, plsOnline]);
  FDisconnectButton.Enabled := (FHost <> nil) and
    (State in [plsConnecting, plsOnline]);
  FRefreshPortsButton.Enabled := True;

  CategoryIdentity := Tr('personal.adapter.categoryIdentity', 'Identity');
  CategoryCapabilities := Tr('personal.adapter.categoryCapabilities',
    'Capabilities');
  FIdentityList.BeginUpdate;
  try
    FIdentityList.Clear;
    if (FHost <> nil) and FHost.Session.Online then
    begin
      Identity := FHost.Session.Adapter.Identity;
      if Identity.DisplayName <> '' then
        FIdentityList.Add(CategoryIdentity,
          Tr('personal.adapter.adapterLabel', 'Adapter'), Identity.DisplayName)
      else
        FIdentityList.Add(CategoryIdentity,
          Tr('personal.adapter.adapterLabel', 'Adapter'),
          FHost.Session.AdapterName);
      FIdentityList.Add(CategoryIdentity,
        Tr('personal.adapter.firmwareLabel', 'Firmware'),
        Identity.FirmwareVersion, kvkMono);
      Item := FIdentityList.Add(CategoryIdentity,
        Tr('personal.adapter.cloneLabel', 'Clone check'), '', kvkTone);
      if Identity.IsClone then
      begin
        Item.Value := Tr('personal.adapter.cloneWarning',
          'Clone detected: some commands may be missing');
        Item.Tone := skDanger;
      end
      else
      begin
        Item.Value := Tr('personal.adapter.cloneOk',
          'No clone signature detected');
        Item.Tone := skSuccess;
      end;
      FIdentityList.Add(CategoryIdentity,
        Tr('personal.adapter.protocolLabel', 'Protocol'),
        FHost.Session.ProtocolName);
      Battery := FHost.Session.BatteryVolts;
      if Battery > 0 then
        FIdentityList.Add(CategoryIdentity,
          Tr('personal.adapter.batteryLabel', 'Battery'),
          FormatFloat('0.0 V', Battery), kvkMono)
      else
        FIdentityList.Add(CategoryIdentity,
          Tr('personal.adapter.batteryLabel', 'Battery'),
          Tr('personal.common.unknown', 'Unknown'));

      AddedCapability := False;
      for Cap := Low(TOBDAdapterCapability) to High(TOBDAdapterCapability) do
        if Cap in FHost.Session.Adapter.Capabilities then
        begin
          Item := FIdentityList.Add(CategoryCapabilities, CapabilityName(Cap),
            Tr('personal.adapter.supported', 'Supported'), kvkChip);
          Item.Tone := skSuccess;
          AddedCapability := True;
        end;
      if not AddedCapability then
        FIdentityList.Add(CategoryCapabilities,
          Tr('personal.adapter.capabilitiesLabel', 'Capabilities'),
          Tr('personal.adapter.capabilitiesNone',
          'No capabilities reported yet.'));
      FIdentityCard.StatusEdge := ceSuccess;
    end
    else
    begin
      FIdentityList.Add(CategoryIdentity,
        Tr('personal.adapter.adapterLabel', 'Adapter'),
        Tr('personal.adapter.identityEmpty',
        'Connect to show adapter identity.'));
      FIdentityList.Add(CategoryCapabilities,
        Tr('personal.adapter.capabilitiesLabel', 'Capabilities'),
        Tr('personal.adapter.capabilitiesEmpty',
        'Connect to show adapter capabilities.'));
      if State = plsFailed then
        FIdentityCard.StatusEdge := ceDanger
      else
        FIdentityCard.StatusEdge := ceNeutral;
    end;
  finally
    FIdentityList.EndUpdate;
  end;
  UpdateLog;
end;

procedure TfrmPageAdapter.ConnectionKindChanged(Sender: TObject);
begin
  if not FLoading then
    UpdateConnectionControls;
end;

procedure TfrmPageAdapter.RefreshPortsClicked(Sender: TObject);
var
  Saved: string;
begin
  Saved := FPortCombo.Text;
  EnumeratePorts;
  if Saved <> '' then
    SelectComboText(FPortCombo, Saved)
  else if FPortCombo.Items.Count > 0 then
    FPortCombo.ItemIndex := 0;
end;

procedure TfrmPageAdapter.ConnectClicked(Sender: TObject);
begin
  if FHost = nil then
    Exit;
  ApplyControlsToSettings;
  FHost.SettingsChanged;
  FHost.Session.Connect(FHost.Settings);
  UpdateState;
end;

procedure TfrmPageAdapter.DisconnectClicked(Sender: TObject);
begin
  if FHost <> nil then
  begin
    FHost.Session.Disconnect;
    UpdateState;
  end;
end;

procedure TfrmPageAdapter.AutoConnectChanged(Sender: TObject);
begin
  if FLoading or (FHost = nil) then
    Exit;
  ApplyControlsToSettings;
  FHost.SettingsChanged;
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageAdapter.Commands: TPersonalCommands;
begin
  Result := [pcRefreshPorts];
end;

function TfrmPageAdapter.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  Result := ACommand = pcRefreshPorts;
end;

function TfrmPageAdapter.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPageAdapter.ExecuteCommand(ACommand: TPersonalCommand);
begin
  if ACommand = pcRefreshPorts then
    RefreshPortsClicked(Self);
end;

end.
