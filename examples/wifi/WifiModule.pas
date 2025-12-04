unit Examples.Wifi.WifiModule;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.StdCtrls,
  OBD.Adapter.Types, OBD.Connection.Component, OBD.Protocol, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.CircularGauge,
  Examples.ServiceDemo;

/// <summary>
///   WiFi dashboard example configured with host and port settings.
/// </summary>
TWiFiDashboardForm = class(TForm)
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
  ///   Initiates a WiFi connection using the configured endpoint.
  /// </summary>
  procedure ConnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Disconnects the WiFi adapter when the user requests it.
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
  WiFiDashboardForm: TWiFiDashboardForm;

implementation

{$R *.dfm}

procedure TWiFiDashboardForm.ClearDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.ClearStoredDTCs;
end;

procedure TWiFiDashboardForm.ConnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Connect;
end;

procedure TWiFiDashboardForm.ControlSystemButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestControlSystemTest;
end;

procedure TWiFiDashboardForm.DisconnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Disconnect;
end;

procedure TWiFiDashboardForm.FormCreate(Sender: TObject);
begin
  FServiceDemo := TOBDServiceDemo.Create(ConnectionComponent, ProtocolComponent, DiagnosticsMemo);
  FServiceDemo.Attach;
end;

procedure TWiFiDashboardForm.FormDestroy(Sender: TObject);
begin
  FServiceDemo.Free;
end;

procedure TWiFiDashboardForm.FreezeFrameButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestFreezeFrame;
end;

procedure TWiFiDashboardForm.MonitorStatusButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestMonitorStatus;
end;

procedure TWiFiDashboardForm.MonitorTestsButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOnBoardMonitoring;
end;

procedure TWiFiDashboardForm.OxygenSensorButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOxygenSensorTests;
end;

procedure TWiFiDashboardForm.PendingDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPendingDTCs;
end;

procedure TWiFiDashboardForm.PermanentDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPermanentDTCs;
end;

procedure TWiFiDashboardForm.ResolveGaugeValue(Sender: TObject;
  const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean);
begin
  Applied := Length(Messages) > 0;
  if Applied then
    Value := Messages[High(Messages)].NumericValue
  else
    Value := 0;
end;

procedure TWiFiDashboardForm.StoredDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestStoredDTCs;
end;

procedure TWiFiDashboardForm.VehicleInfoButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestVehicleInformation;
end;

end.
