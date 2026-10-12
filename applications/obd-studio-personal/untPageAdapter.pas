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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  ERD.I18n,
  ERD.Adapter.Types,
  untSettings,
  untSession,
  untPersonalPage;

type
  TfrmPageAdapter = class(TFrame, IPersonalPage)
  strict private
    FHost: IPersonalHost;
    FLoading: Boolean;
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblSubtitle: TLabel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnRefreshPorts: TButton;
    lblKind: TLabel;
    cmbKind: TComboBox;
    lblPort: TLabel;
    cmbPort: TComboBox;
    lblBaud: TLabel;
    cmbBaud: TComboBox;
    lblHost: TLabel;
    edtHost: TEdit;
    lblWiFiPort: TLabel;
    edtWiFiPort: TEdit;
    lblBLE: TLabel;
    edtBLE: TEdit;
    chkAutoConnect: TCheckBox;
    lblStateValue: TLabel;
    lblProgressValue: TLabel;
    lblErrorValue: TLabel;
    lblAdapterValue: TLabel;
    lblFirmwareValue: TLabel;
    lblCloneValue: TLabel;
    lblProtocolValue: TLabel;
    lblBatteryValue: TLabel;
    memCapabilities: TMemo;
    memTips: TMemo;
    procedure BuildUI;
    procedure HeaderResize(Sender: TObject);
    procedure ConnectionKindChanged(Sender: TObject);
    procedure RefreshPortsClicked(Sender: TObject);
    procedure ConnectClicked(Sender: TObject);
    procedure DisconnectClicked(Sender: TObject);
    procedure AutoConnectClicked(Sender: TObject);
    procedure LoadSettingsToControls;
    procedure ApplyControlsToSettings;
    procedure EnumeratePorts;
    procedure SelectComboText(ACombo: TComboBox; const AText: string);
    procedure UpdateConnectionControls;
    procedure UpdateState;
  public
    constructor Create(AOwner: TComponent); override;
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
  NO_PORTS_TEXT = '(no COM ports found)';

function CapabilityName(ACapability: TOBDAdapterCapability): string;
begin
  case ACapability of
    acCAN:
      Result := 'CAN';
    acCANFD:
      Result := 'CAN-FD';
    acISOTP:
      Result := 'ISO-TP';
    acISOTPLongFrame:
      Result := 'Long ISO-TP frames';
    acDoIP:
      Result := 'Diagnostics over IP';
    acJ1939:
      Result := 'SAE J1939';
    acKLine:
      Result := 'K-line';
    acVoltageMonitor:
      Result := 'Battery voltage';
    acSecureOnboard:
      Result := 'Secure on-board communication';
    acJ2534:
      Result := 'SAE J2534';
    acJ2534v2:
      Result := 'SAE J2534-2';
    acSTCommands:
      Result := 'OBDLink ST commands';
    acMessageFilters:
      Result := 'Adapter message filters';
    acProgrammableHeaders:
      Result := 'Programmable headers';
    acAdapterMultiFrame:
      Result := 'Adapter multi-frame assembly';
  else
    Result := 'Capability ' + IntToStr(Ord(ACapability));
  end;
end;

function CapabilitiesText(ACapabilities: TOBDAdapterCapabilities): string;
var
  Cap: TOBDAdapterCapability;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    for Cap := Low(TOBDAdapterCapability) to High(TOBDAdapterCapability) do
      if Cap in ACapabilities then
        Lines.Add('• ' + CapabilityName(Cap));
    if Lines.Count = 0 then
      Lines.Add('No capabilities reported yet. Connect and let the adapter ' +
        'identify itself.');
    Result := Lines.Text;
  finally
    Lines.Free;
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

{ TfrmPageAdapter }

constructor TfrmPageAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

procedure TfrmPageAdapter.BuildUI;
var
  Body: TPanel;
  RightPanel: TPanel;
  grpConnection: TGroupBox;
  grpState: TGroupBox;
  grpIdentity: TGroupBox;

  function AddLabel(AParent: TWinControl; ALeft, ATop: Integer;
    const ACaption: string): TLabel;
  begin
    Result := TLabel.Create(Self);
    Result.Parent := AParent;
    Result.Left := ALeft;
    Result.Top := ATop;
    Result.Caption := ACaption;
  end;

begin
  Align := alClient;
  ParentBackground := False;

  pnlHeader := TPanel.Create(Self);
  pnlHeader.Parent := Self;
  pnlHeader.Align := alTop;
  pnlHeader.Height := 76;
  pnlHeader.BevelOuter := bvNone;
  pnlHeader.ParentBackground := False;
  pnlHeader.OnResize := HeaderResize;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := pnlHeader;
  lblTitle.Left := 16;
  lblTitle.Top := 10;
  lblTitle.Font.Style := [fsBold];
  lblTitle.Font.Size := 13;

  lblSubtitle := TLabel.Create(Self);
  lblSubtitle.Parent := pnlHeader;
  lblSubtitle.Left := 16;
  lblSubtitle.Top := 38;
  lblSubtitle.Width := 520;

  btnDisconnect := TButton.Create(Self);
  btnDisconnect.Parent := pnlHeader;
  btnDisconnect.Top := 22;
  btnDisconnect.Width := 112;
  btnDisconnect.OnClick := DisconnectClicked;

  btnConnect := TButton.Create(Self);
  btnConnect.Parent := pnlHeader;
  btnConnect.Top := 22;
  btnConnect.Width := 104;
  btnConnect.Default := True;
  btnConnect.OnClick := ConnectClicked;

  btnRefreshPorts := TButton.Create(Self);
  btnRefreshPorts.Parent := pnlHeader;
  btnRefreshPorts.Top := 22;
  btnRefreshPorts.Width := 112;
  btnRefreshPorts.OnClick := RefreshPortsClicked;

  Body := TPanel.Create(Self);
  Body.Parent := Self;
  Body.Align := alClient;
  Body.BevelOuter := bvNone;
  Body.ParentBackground := False;

  grpConnection := TGroupBox.Create(Self);
  grpConnection.Parent := Body;
  grpConnection.Align := alLeft;
  grpConnection.Width := 430;
  grpConnection.Padding.SetBounds(12, 12, 12, 12);

  lblKind := AddLabel(grpConnection, 20, 36, 'Connection type');
  cmbKind := TComboBox.Create(Self);
  cmbKind.Parent := grpConnection;
  cmbKind.Left := 160;
  cmbKind.Top := 32;
  cmbKind.Width := 236;
  cmbKind.Style := csDropDownList;
  cmbKind.OnChange := ConnectionKindChanged;

  lblPort := AddLabel(grpConnection, 20, 76, 'COM port');
  cmbPort := TComboBox.Create(Self);
  cmbPort.Parent := grpConnection;
  cmbPort.Left := 160;
  cmbPort.Top := 72;
  cmbPort.Width := 236;
  cmbPort.Style := csDropDownList;

  lblBaud := AddLabel(grpConnection, 20, 116, 'Baud rate');
  cmbBaud := TComboBox.Create(Self);
  cmbBaud.Parent := grpConnection;
  cmbBaud.Left := 160;
  cmbBaud.Top := 112;
  cmbBaud.Width := 236;
  cmbBaud.Style := csDropDownList;
  cmbBaud.Items.Add('9600');
  cmbBaud.Items.Add('19200');
  cmbBaud.Items.Add('38400');
  cmbBaud.Items.Add('57600');
  cmbBaud.Items.Add('115200');
  cmbBaud.Items.Add('230400');
  cmbBaud.Items.Add('460800');
  cmbBaud.Items.Add('500000');
  cmbBaud.Items.Add('921600');
  cmbBaud.Items.Add('1000000');
  cmbBaud.Items.Add('2000000');

  lblHost := AddLabel(grpConnection, 20, 156, 'Wi-Fi host');
  edtHost := TEdit.Create(Self);
  edtHost.Parent := grpConnection;
  edtHost.Left := 160;
  edtHost.Top := 152;
  edtHost.Width := 236;

  lblWiFiPort := AddLabel(grpConnection, 20, 196, 'Wi-Fi port');
  edtWiFiPort := TEdit.Create(Self);
  edtWiFiPort.Parent := grpConnection;
  edtWiFiPort.Left := 160;
  edtWiFiPort.Top := 192;
  edtWiFiPort.Width := 236;

  lblBLE := AddLabel(grpConnection, 20, 236, 'BLE address');
  edtBLE := TEdit.Create(Self);
  edtBLE.Parent := grpConnection;
  edtBLE.Left := 160;
  edtBLE.Top := 232;
  edtBLE.Width := 236;

  chkAutoConnect := TCheckBox.Create(Self);
  chkAutoConnect.Parent := grpConnection;
  chkAutoConnect.Left := 20;
  chkAutoConnect.Top := 280;
  chkAutoConnect.Width := 376;
  chkAutoConnect.OnClick := AutoConnectClicked;

  RightPanel := TPanel.Create(Self);
  RightPanel.Parent := Body;
  RightPanel.Align := alClient;
  RightPanel.BevelOuter := bvNone;
  RightPanel.ParentBackground := False;

  grpState := TGroupBox.Create(Self);
  grpState.Parent := RightPanel;
  grpState.Align := alTop;
  grpState.Height := 168;
  grpState.Padding.SetBounds(12, 12, 12, 12);

  AddLabel(grpState, 20, 36, 'State');
  lblStateValue := AddLabel(grpState, 168, 36, '');
  lblStateValue.Font.Style := [fsBold];
  AddLabel(grpState, 20, 64, 'Progress');
  lblProgressValue := AddLabel(grpState, 168, 64, '');
  AddLabel(grpState, 20, 92, 'Last error');
  lblErrorValue := AddLabel(grpState, 168, 92, '');
  lblErrorValue.Font.Color := clRed;
  AddLabel(grpState, 20, 120, 'Protocol');
  lblProtocolValue := AddLabel(grpState, 168, 120, '');

  memTips := TMemo.Create(Self);
  memTips.Parent := RightPanel;
  memTips.Align := alBottom;
  memTips.Height := 136;
  memTips.ReadOnly := True;
  memTips.ScrollBars := ssVertical;

  grpIdentity := TGroupBox.Create(Self);
  grpIdentity.Parent := RightPanel;
  grpIdentity.Align := alClient;
  grpIdentity.Padding.SetBounds(12, 12, 12, 12);

  AddLabel(grpIdentity, 20, 36, 'Adapter');
  lblAdapterValue := AddLabel(grpIdentity, 168, 36, '');
  AddLabel(grpIdentity, 20, 64, 'Firmware');
  lblFirmwareValue := AddLabel(grpIdentity, 168, 64, '');
  AddLabel(grpIdentity, 20, 92, 'Clone check');
  lblCloneValue := AddLabel(grpIdentity, 168, 92, '');
  AddLabel(grpIdentity, 20, 120, 'Battery');
  lblBatteryValue := AddLabel(grpIdentity, 168, 120, '');

  memCapabilities := TMemo.Create(Self);
  memCapabilities.Parent := grpIdentity;
  memCapabilities.Left := 20;
  memCapabilities.Top := 156;
  memCapabilities.Width := 520;
  memCapabilities.Height := 180;
  memCapabilities.ReadOnly := True;
  memCapabilities.ScrollBars := ssVertical;

  Translate;
end;

procedure TfrmPageAdapter.HeaderResize(Sender: TObject);
begin
  btnDisconnect.Left := pnlHeader.Width - btnDisconnect.Width - 16;
  btnConnect.Left := btnDisconnect.Left - btnConnect.Width - 8;
  btnRefreshPorts.Left := btnConnect.Left - btnRefreshPorts.Width - 8;
  lblSubtitle.Width := btnRefreshPorts.Left - lblSubtitle.Left - 16;
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
  lblTitle.Caption := Tr('personal.adapter.title', 'Adapter');
  lblSubtitle.Caption := Tr('personal.adapter.subtitle',
    'Choose the connection, then connect to the vehicle or the demo car.');
  btnRefreshPorts.Caption := Tr('personal.adapter.refreshPorts', 'Refresh ports');
  btnConnect.Caption := Tr('personal.adapter.connect', 'Connect');
  btnDisconnect.Caption := Tr('personal.adapter.disconnect', 'Disconnect');
  chkAutoConnect.Caption := Tr('personal.adapter.autoConnect',
    'Connect automatically on start');
  memTips.Lines.Text := Tr('personal.adapter.tips',
    'Troubleshooting tips:'#13#10 +
    '• Turn the ignition on before connecting.'#13#10 +
    '• Check the adapter LED: it should have power and activity.'#13#10 +
    '• Pair Bluetooth adapters in Windows before selecting the COM port.'#13#10 +
    '• If a clone fails, try a lower baud rate or the demo car.');

  Index := cmbKind.ItemIndex;
  FLoading := True;
  try
    cmbKind.Items.BeginUpdate;
    try
      cmbKind.Items.Clear;
      cmbKind.Items.Add(Tr('personal.adapter.kindSerial',
        'USB / Bluetooth COM port'));
      cmbKind.Items.Add(Tr('personal.adapter.kindWiFi', 'Wi-Fi adapter'));
      cmbKind.Items.Add(Tr('personal.adapter.kindBLE', 'Bluetooth LE'));
      cmbKind.Items.Add(Tr('personal.adapter.kindDemo', 'Demo car'));
    finally
      cmbKind.Items.EndUpdate;
    end;
    if (Index >= 0) and (Index < cmbKind.Items.Count) then
      cmbKind.ItemIndex := Index;
  finally
    FLoading := False;
  end;
  UpdateConnectionControls;
  UpdateState;
end;

procedure TfrmPageAdapter.SelectComboText(ACombo: TComboBox;
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
    cmbPort.Items.Assign(Ports);
    if cmbPort.Items.Count = 0 then
      cmbPort.Items.Add(NO_PORTS_TEXT);
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
    if cmbKind.Items.Count = 0 then
      Translate;
    cmbKind.ItemIndex := Ord(Settings.ConnectionKind);
    if Settings.SerialPort <> '' then
    begin
      if cmbPort.Items.IndexOf(Settings.SerialPort) < 0 then
        cmbPort.Items.Insert(0, Settings.SerialPort);
      SelectComboText(cmbPort, Settings.SerialPort);
    end
    else if cmbPort.Items.Count > 0 then
      cmbPort.ItemIndex := 0;
    SelectComboText(cmbBaud, IntToStr(Settings.BaudRate));
    edtHost.Text := Settings.WiFiHost;
    edtWiFiPort.Text := IntToStr(Settings.WiFiPort);
    edtBLE.Text := Settings.BLEAddress;
    chkAutoConnect.Checked := Settings.AutoConnect;
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
  if (cmbKind.ItemIndex >= Ord(Low(TPersonalConnectionKind))) and
    (cmbKind.ItemIndex <= Ord(High(TPersonalConnectionKind))) then
    Settings.ConnectionKind := TPersonalConnectionKind(cmbKind.ItemIndex);
  if (cmbPort.ItemIndex >= 0) and (cmbPort.Text <> NO_PORTS_TEXT) then
    Settings.SerialPort := cmbPort.Text;
  Settings.BaudRate := StrToIntDef(cmbBaud.Text, Settings.BaudRate);
  Settings.WiFiHost := Trim(edtHost.Text);
  Settings.WiFiPort := StrToIntDef(edtWiFiPort.Text, Settings.WiFiPort);
  Settings.BLEAddress := Trim(edtBLE.Text);
  Settings.AutoConnect := chkAutoConnect.Checked;
end;

procedure TfrmPageAdapter.UpdateConnectionControls;
var
  Kind: TPersonalConnectionKind;
  SerialEnabled: Boolean;
  WiFiEnabled: Boolean;
  BLEEnabled: Boolean;
begin
  Kind := pckDemo;
  if (cmbKind.ItemIndex >= Ord(Low(TPersonalConnectionKind))) and
    (cmbKind.ItemIndex <= Ord(High(TPersonalConnectionKind))) then
    Kind := TPersonalConnectionKind(cmbKind.ItemIndex);
  SerialEnabled := Kind = pckSerial;
  WiFiEnabled := Kind = pckWiFi;
  BLEEnabled := Kind = pckBLE;
  lblPort.Enabled := SerialEnabled;
  cmbPort.Enabled := SerialEnabled;
  lblBaud.Enabled := SerialEnabled;
  cmbBaud.Enabled := SerialEnabled;
  lblHost.Enabled := WiFiEnabled;
  edtHost.Enabled := WiFiEnabled;
  lblWiFiPort.Enabled := WiFiEnabled;
  edtWiFiPort.Enabled := WiFiEnabled;
  lblBLE.Enabled := BLEEnabled;
  edtBLE.Enabled := BLEEnabled;
end;

procedure TfrmPageAdapter.UpdateState;
var
  Identity: TOBDAdapterIdentity;
  Battery: Double;
begin
  if FHost = nil then
    Exit;
  lblStateValue.Caption := LinkStateText(FHost.Session.State);
  case FHost.Session.State of
    plsConnecting:
      lblProgressValue.Caption := Tr('personal.adapter.progressConnecting',
        'Opening the adapter, detecting the chip and finding the protocol');
    plsOnline:
      lblProgressValue.Caption := Tr('personal.adapter.progressOnline',
        'Ready');
    plsFailed:
      lblProgressValue.Caption := Tr('personal.adapter.progressFailed',
        'Check the hints below and try again');
  else
    lblProgressValue.Caption := Tr('personal.adapter.progressOffline',
      'Choose a connection and press Connect');
  end;
  lblErrorValue.Caption := FHost.Session.LastError;
  lblProtocolValue.Caption := FHost.Session.ProtocolName;
  btnConnect.Enabled := not(FHost.Session.State in [plsConnecting, plsOnline]);
  btnDisconnect.Enabled := FHost.Session.State in [plsConnecting, plsOnline];

  if FHost.Session.Online then
  begin
    Identity := FHost.Session.Adapter.Identity;
    if Identity.DisplayName <> '' then
      lblAdapterValue.Caption := Identity.DisplayName
    else
      lblAdapterValue.Caption := FHost.Session.AdapterName;
    lblFirmwareValue.Caption := Identity.FirmwareVersion;
    if Identity.IsClone then
    begin
      lblCloneValue.Font.Color := clRed;
      lblCloneValue.Caption := Tr('personal.adapter.cloneWarning',
        'Clone detected: some commands may be missing');
    end
    else
    begin
      lblCloneValue.Font.Color := clGreen;
      lblCloneValue.Caption := Tr('personal.adapter.cloneOk',
        'No clone signature detected');
    end;
    Battery := FHost.Session.BatteryVolts;
    if Battery > 0 then
      lblBatteryValue.Caption := FormatFloat('0.0 V', Battery)
    else
      lblBatteryValue.Caption := Tr('personal.common.unknown', 'Unknown');
    memCapabilities.Lines.Text := CapabilitiesText(
      FHost.Session.Adapter.Capabilities);
  end
  else
  begin
    lblAdapterValue.Caption := '';
    lblFirmwareValue.Caption := '';
    lblCloneValue.Font.Color := clWindowText;
    lblCloneValue.Caption := '';
    lblBatteryValue.Caption := '';
    memCapabilities.Lines.Text := Tr('personal.adapter.capabilitiesEmpty',
      'Connect to show adapter capabilities.');
  end;
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
  Saved := cmbPort.Text;
  EnumeratePorts;
  if Saved <> '' then
    SelectComboText(cmbPort, Saved)
  else if cmbPort.Items.Count > 0 then
    cmbPort.ItemIndex := 0;
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

procedure TfrmPageAdapter.AutoConnectClicked(Sender: TObject);
begin
  if FLoading or (FHost = nil) then
    Exit;
  ApplyControlsToSettings;
  FHost.SettingsChanged;
end;

end.
