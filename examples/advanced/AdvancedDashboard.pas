unit Examples.Advanced.AdvancedDashboard;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.StdCtrls,
  OBD.Adapter.Types, OBD.Connection.Component, OBD.Protocol, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.CircularGauge,
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
