unit Examples.Bluetooth.BluetoothModule;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.StdCtrls,
  OBD.Adapter.Types, OBD.Connection.Component, OBD.Protocol, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.CircularGauge,
  Examples.ServiceDemo;

/// <summary>
///   Bluetooth dashboard example preconfigured with a MAC address and manager.
/// </summary>
TBluetoothDashboardForm = class(TForm)
  Header: TOBDTouchHeader;
  Subheader: TOBDTouchSubheader;
  Statusbar: TOBDTouchStatusbar;
  Gauge: TOBDCircularGauge;
  ControlPanel: TPanel;
  ConnectButton: TButton;
  DisconnectButton: TButton;
  DiagnosticsPanel: TPanel;
  DiagnosticsMemo: TMemo;
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
  ConnectionComponent: TOBDConnectionComponent;
  ProtocolComponent: TOBDProtocolComponent;
  HeaderComponent: TOBDHeaderComponent;
  SubheaderComponent: TOBDSubheaderComponent;
  GaugeComponent: TOBDGaugeComponent;
private
  /// <summary>
  ///   Shared helper that issues and parses mode 01-0A requests for the demo.
  /// </summary>
  FServiceDemo: TOBDServiceDemo;
  /// <summary>
  ///   Initializes the service demo helper and subscribes to protocol messages.
  /// </summary>
  procedure FormCreate(Sender: TObject);
  /// <summary>
  ///   Releases the helper and detaches protocol hooks.
  /// </summary>
  procedure FormDestroy(Sender: TObject);
  /// <summary>
  ///   Initiates a Bluetooth connection using the configured address and manager.
  /// </summary>
  procedure ConnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Disconnects the Bluetooth adapter when the user requests it.
  /// </summary>
  procedure DisconnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Converts protocol messages into gauge values for visualization.
  /// </summary>
  procedure ResolveGaugeValue(Sender: TObject; const Messages: TArray<IOBDDataMessage>;
    out Value: Single; out Applied: Boolean);
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
end;

var
  BluetoothDashboardForm: TBluetoothDashboardForm;

implementation

{$R *.dfm}

procedure TBluetoothDashboardForm.ClearDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.ClearStoredDTCs;
end;

procedure TBluetoothDashboardForm.ConnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Connect;
end;

procedure TBluetoothDashboardForm.ControlSystemButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestControlSystemTest;
end;

procedure TBluetoothDashboardForm.DisconnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Disconnect;
end;

procedure TBluetoothDashboardForm.FormCreate(Sender: TObject);
begin
  FServiceDemo := TOBDServiceDemo.Create(ConnectionComponent, ProtocolComponent, DiagnosticsMemo);
  FServiceDemo.Attach;
end;

procedure TBluetoothDashboardForm.FormDestroy(Sender: TObject);
begin
  FServiceDemo.Free;
end;

procedure TBluetoothDashboardForm.FreezeFrameButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestFreezeFrame;
end;

procedure TBluetoothDashboardForm.MonitorStatusButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestMonitorStatus;
end;

procedure TBluetoothDashboardForm.MonitorTestsButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOnBoardMonitoring;
end;

procedure TBluetoothDashboardForm.OxygenSensorButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOxygenSensorTests;
end;

procedure TBluetoothDashboardForm.PendingDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPendingDTCs;
end;

procedure TBluetoothDashboardForm.PermanentDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPermanentDTCs;
end;

procedure TBluetoothDashboardForm.ResolveGaugeValue(Sender: TObject;
  const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean);
begin
  Applied := Length(Messages) > 0;
  if Applied then
    Value := Messages[High(Messages)].NumericValue
  else
    Value := 0;
end;

procedure TBluetoothDashboardForm.StoredDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestStoredDTCs;
end;

procedure TBluetoothDashboardForm.VehicleInfoButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestVehicleInformation;
end;

end.
