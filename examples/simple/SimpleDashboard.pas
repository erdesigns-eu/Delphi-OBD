unit Examples.Simple.SimpleDashboard;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls,
  OBD.Adapter.Types, OBD.Connection.Component, OBD.Protocol, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.CircularGauge,
  Examples.ServiceDemo;

/// <summary>
///   Simple dashboard showcasing connection, protocol, header, subheader, and
///   gauge bindings using the non-visual components.
/// </summary>
/// <remarks>
///   Drop-in example with connect/disconnect buttons and a live gauge resolver.
/// </remarks>
TSimpleDashboardForm = class(TForm)
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
  ///   Connects using the configured connection component when the button is
  ///   pressed.
  /// </summary>
  procedure ConnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Disconnects the active connection when requested from the UI.
  /// </summary>
  procedure DisconnectButtonClick(Sender: TObject);
  /// <summary>
  ///   Resolves a gauge value from the incoming protocol messages.
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
  SimpleDashboardForm: TSimpleDashboardForm;

implementation

{$R *.dfm}

procedure TSimpleDashboardForm.ClearDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.ClearStoredDTCs;
end;

procedure TSimpleDashboardForm.ConnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Connect;
end;

procedure TSimpleDashboardForm.ControlSystemButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestControlSystemTest;
end;

procedure TSimpleDashboardForm.DisconnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Disconnect;
end;

procedure TSimpleDashboardForm.FormCreate(Sender: TObject);
begin
  FServiceDemo := TOBDServiceDemo.Create(ConnectionComponent, ProtocolComponent, DiagnosticsMemo);
  FServiceDemo.Attach;
end;

procedure TSimpleDashboardForm.FormDestroy(Sender: TObject);
begin
  FServiceDemo.Free;
end;

procedure TSimpleDashboardForm.FreezeFrameButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestFreezeFrame;
end;

procedure TSimpleDashboardForm.MonitorStatusButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestMonitorStatus;
end;

procedure TSimpleDashboardForm.MonitorTestsButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOnBoardMonitoring;
end;

procedure TSimpleDashboardForm.OxygenSensorButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestOxygenSensorTests;
end;

procedure TSimpleDashboardForm.PendingDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPendingDTCs;
end;

procedure TSimpleDashboardForm.PermanentDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestPermanentDTCs;
end;

procedure TSimpleDashboardForm.ResolveGaugeValue(Sender: TObject;
  const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean);
begin
  Applied := Length(Messages) > 0;
  if Applied then
    Value := Messages[High(Messages)].NumericValue
  else
    Value := 0;
end;

procedure TSimpleDashboardForm.StoredDTCButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestStoredDTCs;
end;

procedure TSimpleDashboardForm.VehicleInfoButtonClick(Sender: TObject);
begin
  FServiceDemo.RequestVehicleInformation;
end;

end.
