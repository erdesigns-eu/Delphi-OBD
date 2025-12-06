unit Examples.Advanced.AdvancedDashboard;

interface

uses
  System.Classes, System.SysUtils,
  System.StrUtils,
  Vcl.ComCtrls,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.StdCtrls,
  OBD.Adapter.Types, OBD.Connection.Component, OBD.Protocol, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.CircularGauge,
  OBD.Service.Types,
  Examples.ServiceDemo;

/// <summary>
///   Advanced dashboard example with diagnostics logging and live gauge binding.
/// </summary>
TAdvancedDashboardForm = class(TForm)
  Header: TOBDTouchHeader;
  Subheader: TOBDTouchSubheader;
  Statusbar: TOBDTouchStatusbar;
  Gauge: TOBDCircularGauge;
  ControlPanel: TPanel;
  ConnectButton: TButton;
  DisconnectButton: TButton;
  DiagnosticsMemo: TMemo;
  ConnectionComponent: TOBDConnectionComponent;
  ProtocolComponent: TOBDProtocolComponent;
  HeaderComponent: TOBDHeaderComponent;
  SubheaderComponent: TOBDSubheaderComponent;
  GaugeComponent: TOBDGaugeComponent;
  DiagnosticsPanel: TPanel;
  DiagnosticButtons: TPanel;
  MonitorStatusButton: TButton;
  FreezeFrameButton: TButton;
  StoredDTCButton: TButton;
  PendingDTCButton: TButton;
  PermanentDTCButton: TButton;
  VehicleInfoButton: TButton;
  ClearDTCButton: TButton;
  OxygenSensorButton: TButton;
  MonitorTestsButton: TButton;
  ControlSystemButton: TButton;
  DataPanel: TPanel;
  LiveDataGroup: TGroupBox;
  LiveDataList: TListView;
  FreezeFrameGroup: TGroupBox;
  FreezeFrameList: TListView;
  VehicleInfoGroup: TGroupBox;
  VINCaption: TLabel;
  VINValue: TLabel;
  DTCGroup: TGroupBox;
  DTCList: TListView;
private
  /// <summary>
  ///   Shared helper that issues and parses mode 01-0A requests for the demo.
  /// </summary>
  FServiceDemo: TOBDServiceDemo;
  /// <summary>
  ///   Connects the configured adapter and begins streaming data into the UI.
  /// </summary>
  procedure ConnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Disconnects the active adapter and stops updates.
  /// </summary>
  procedure DisconnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Resolves gauge values from incoming messages.
  /// </summary>
  procedure ResolveGaugeValue(Sender: TObject; const Messages: TArray<IOBDDataMessage>;
    out Value: Single; out Applied: Boolean);
  /// <summary>
  ///   Refreshes the diagnostics memo when the protocol component publishes updates.
  /// </summary>
  procedure DiagnosticsUpdated(Sender: TObject);
  /// <summary>
  ///   Requests mode 01 monitor status and supported PID data.
  /// </summary>
  procedure MonitorStatusButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 02 freeze-frame details for the first stored snapshot.
  /// </summary>
  procedure FreezeFrameButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 03 stored diagnostic trouble codes.
  /// </summary>
  procedure StoredDTCButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 07 pending diagnostic trouble codes.
  /// </summary>
  procedure PendingDTCButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 0A permanent diagnostic trouble codes.
  /// </summary>
  procedure PermanentDTCButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 09 vehicle identification details.
  /// </summary>
  procedure VehicleInfoButtonClick(Sender: TObject);
  /// <summary>
  ///   Sends a mode 04 clear-DTC command.
  /// </summary>
  procedure ClearDTCButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 05 oxygen sensor diagnostics.
  /// </summary>
  procedure OxygenSensorButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 06 onboard monitoring results.
  /// </summary>
  procedure MonitorTestsButtonClick(Sender: TObject);
  /// <summary>
  ///   Requests mode 08 control-system tests.
  /// </summary>
  procedure ControlSystemButtonClick(Sender: TObject);
  /// <summary>
  ///   Updates the live data list and gauge value when new mode 01 data arrives.
  /// </summary>
  procedure HandleLiveData(Sender: TObject; const Snapshot: TOBDService01Snapshot);
  /// <summary>
  ///   Renders the freeze-frame snapshot when mode 02 responses are parsed.
  /// </summary>
  procedure HandleFreezeFrame(Sender: TObject; const DTC: string;
    const Snapshot: TOBDService01Snapshot);
  /// <summary>
  ///   Displays stored, pending, or permanent DTCs when parsed by the helper.
  /// </summary>
  procedure HandleDTCs(Sender: TObject; const Kind: string;
    const Codes: TArray<TOBDServiceDiagnosticTroubleCode>);
  /// <summary>
  ///   Shows the VIN parsed from mode 09 vehicle information.
  /// </summary>
  procedure HandleVIN(Sender: TObject; const VIN: string);
  /// <summary>
  ///   Finds or creates a list item and sets its value text.
  /// </summary>
  procedure UpsertListItem(const ListView: TListView; const Name, Value: string);
  /// <summary>
  ///   Populates the live data list with the latest snapshot values.
  /// </summary>
  procedure UpdateLiveData(const Snapshot: TOBDService01Snapshot);
  /// <summary>
  ///   Populates freeze-frame readings for the referenced DTC.
  /// </summary>
  procedure UpdateFreezeFrame(const DTC: string; const Snapshot: TOBDService01Snapshot);
  /// <summary>
  ///   Replaces the DTC list items with the provided category and codes.
  /// </summary>
  procedure UpdateDTCList(const Kind: string; const Codes: TArray<TOBDServiceDiagnosticTroubleCode>);
  /// <summary>
  ///   Initializes the service demo helper and hooks protocol diagnostics.
  /// </summary>
  procedure FormCreate(Sender: TObject);
  /// <summary>
  ///   Releases helper resources and detaches event handlers.
  /// </summary>
  procedure FormDestroy(Sender: TObject);
end;

var
  AdvancedDashboardForm: TAdvancedDashboardForm;

implementation

{$R *.dfm}

procedure TAdvancedDashboardForm.ClearDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.ClearStoredDTCs;
end;

procedure TAdvancedDashboardForm.ConnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Connect;
end;

procedure TAdvancedDashboardForm.ControlSystemButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestControlSystemTest;
end;

procedure TAdvancedDashboardForm.HandleDTCs(Sender: TObject; const Kind: string;
  const Codes: TArray<TOBDServiceDiagnosticTroubleCode>);
begin
  TThread.Queue(nil,
    procedure
    begin
      UpdateDTCList(Kind, Codes);
    end);
end;

procedure TAdvancedDashboardForm.HandleFreezeFrame(Sender: TObject;
  const DTC: string; const Snapshot: TOBDService01Snapshot);
begin
  TThread.Queue(nil,
    procedure
    begin
      UpdateFreezeFrame(DTC, Snapshot);
    end);
end;

procedure TAdvancedDashboardForm.HandleLiveData(Sender: TObject;
  const Snapshot: TOBDService01Snapshot);
begin
  TThread.Queue(nil,
    procedure
    begin
      UpdateLiveData(Snapshot);
    end);
end;

procedure TAdvancedDashboardForm.HandleVIN(Sender: TObject; const VIN: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      VINValue.Caption := VIN;
    end);
end;

procedure TAdvancedDashboardForm.UpsertListItem(const ListView: TListView; const Name,
  Value: string);
var
  Item: TListItem;
begin
  Item := ListView.FindCaption(0, Name, False, True, False);
  if not Assigned(Item) then
  begin
    Item := ListView.Items.Add;
    Item.Caption := Name;
    Item.SubItems.Add(Value);
  end
  else
  begin
    if Item.SubItems.Count = 0 then
      Item.SubItems.Add(Value)
    else
      Item.SubItems[0] := Value;
  end;
end;

procedure TAdvancedDashboardForm.UpdateDTCList(const Kind: string;
  const Codes: TArray<TOBDServiceDiagnosticTroubleCode>);
var
  Index: Integer;
  Code: TOBDServiceDiagnosticTroubleCode;
  Item: TListItem;
begin
  DTCList.Items.BeginUpdate;
  try
    for Index := DTCList.Items.Count - 1 downto 0 do
      if SameText(DTCList.Items[Index].Caption, Kind) then
        DTCList.Items.Delete(Index);

    if Length(Codes) = 0 then
    begin
      Item := DTCList.Items.Add;
      Item.Caption := Kind;
      Item.SubItems.Add('None reported');
      Exit;
    end;

    for Code in Codes do
    begin
      Item := DTCList.Items.Add;
      Item.Caption := Kind;
      Item.SubItems.Add(Code.DTC);
    end;
  finally
    DTCList.Items.EndUpdate;
  end;
end;

procedure TAdvancedDashboardForm.UpdateFreezeFrame(const DTC: string;
  const Snapshot: TOBDService01Snapshot);
begin
  FreezeFrameList.Items.BeginUpdate;
  try
    FreezeFrameList.Items.Clear;
    UpsertListItem(FreezeFrameList, 'Stored DTC', DTC);
    UpsertListItem(FreezeFrameList, 'Engine RPM', Format('%d rpm', [Snapshot.EngineRPM]));
    UpsertListItem(FreezeFrameList, 'Vehicle Speed', Format('%d km/h', [Snapshot.VehicleSpeed]));
    UpsertListItem(FreezeFrameList, 'Intake Temp', Format('%d °C', [Snapshot.IntakeAirTemperature]));
    UpsertListItem(FreezeFrameList, 'Coolant Temp', Format('%d °C', [Snapshot.CoolantTemperature]));
    UpsertListItem(FreezeFrameList, 'Throttle', FormatFloat('0.0 %', Snapshot.ThrottlePosition));
    UpsertListItem(FreezeFrameList, 'Fuel Pressure', Format('%d kPa', [Snapshot.FuelPressure]));
    UpsertListItem(FreezeFrameList, 'Intake MAP', Format('%d kPa', [Snapshot.IntakeManifoldAbsolutePressure]));
    UpsertListItem(FreezeFrameList, 'MAF', FormatFloat('0.0 g/s', Snapshot.MassAirFlowRate));
  finally
    FreezeFrameList.Items.EndUpdate;
  end;
end;

procedure TAdvancedDashboardForm.UpdateLiveData(const Snapshot: TOBDService01Snapshot);
begin
  LiveDataList.Items.BeginUpdate;
  try
    UpsertListItem(LiveDataList, 'MIL', IfThen(Snapshot.MIL, 'On', 'Off'));
    UpsertListItem(LiveDataList, 'DTC Count', Snapshot.DTCCount.ToString);
    UpsertListItem(LiveDataList, 'Engine RPM', Format('%d rpm', [Snapshot.EngineRPM]));
    UpsertListItem(LiveDataList, 'Vehicle Speed', Format('%d km/h', [Snapshot.VehicleSpeed]));
    UpsertListItem(LiveDataList, 'Intake Temp', Format('%d °C', [Snapshot.IntakeAirTemperature]));
    UpsertListItem(LiveDataList, 'Coolant Temp', Format('%d °C', [Snapshot.CoolantTemperature]));
    UpsertListItem(LiveDataList, 'Throttle', FormatFloat('0.0 %', Snapshot.ThrottlePosition));
    UpsertListItem(LiveDataList, 'Fuel Pressure', Format('%d kPa', [Snapshot.FuelPressure]));
    UpsertListItem(LiveDataList, 'Intake MAP', Format('%d kPa', [Snapshot.IntakeManifoldAbsolutePressure]));
    UpsertListItem(LiveDataList, 'MAF', FormatFloat('0.0 g/s', Snapshot.MassAirFlowRate));
  finally
    LiveDataList.Items.EndUpdate;
  end;

  Gauge.Value := Snapshot.EngineRPM;
end;

procedure TAdvancedDashboardForm.DiagnosticsUpdated(Sender: TObject);
var
  Lines: TArray<string>;
  Line: string;
begin
  Lines := ProtocolComponent.DiagnosticsSnapshot;
  DiagnosticsMemo.Lines.BeginUpdate;
  try
    DiagnosticsMemo.Clear;
    for Line in Lines do
      DiagnosticsMemo.Lines.Add(Line);
  finally
    DiagnosticsMemo.Lines.EndUpdate;
  end;
end;

procedure TAdvancedDashboardForm.DisconnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Disconnect;
end;

procedure TAdvancedDashboardForm.FormCreate(Sender: TObject);
begin
  FServiceDemo := TOBDServiceDemo.Create(ConnectionComponent, ProtocolComponent, DiagnosticsMemo);
  FServiceDemo.Attach;
  FServiceDemo.OnLiveData := HandleLiveData;
  FServiceDemo.OnFreezeFrame := HandleFreezeFrame;
  FServiceDemo.OnDTCs := HandleDTCs;
  FServiceDemo.OnVIN := HandleVIN;
  LiveDataList.ViewStyle := vsReport;
  FreezeFrameList.ViewStyle := vsReport;
  DTCList.ViewStyle := vsReport;
  ProtocolComponent.OnDiagnostics := DiagnosticsUpdated;
end;

procedure TAdvancedDashboardForm.FormDestroy(Sender: TObject);
begin
  FServiceDemo.Free;
end;

procedure TAdvancedDashboardForm.FreezeFrameButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestFreezeFrame;
end;

procedure TAdvancedDashboardForm.MonitorStatusButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestMonitorStatus;
end;

procedure TAdvancedDashboardForm.MonitorTestsButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOnBoardMonitoring;
end;

procedure TAdvancedDashboardForm.OxygenSensorButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOxygenSensorTests;
end;

procedure TAdvancedDashboardForm.PendingDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPendingDTCs;
end;

procedure TAdvancedDashboardForm.PermanentDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPermanentDTCs;
end;

procedure TAdvancedDashboardForm.ResolveGaugeValue(Sender: TObject;
  const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean);
begin
  Applied := Length(Messages) > 0;
  if Applied then
    Value := Messages[High(Messages)].NumericValue
  else
    Value := 0;
end;

procedure TAdvancedDashboardForm.StoredDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestStoredDTCs;
end;

procedure TAdvancedDashboardForm.VehicleInfoButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestVehicleInformation;
end;

end.
