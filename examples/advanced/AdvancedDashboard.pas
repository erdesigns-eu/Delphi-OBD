unit Examples.Advanced.AdvancedDashboard;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.StdCtrls,
  OBD.Adapter.Types, OBD.Connection.Component, OBD.Protocol, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.CircularGauge;

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
end;

var
  AdvancedDashboardForm: TAdvancedDashboardForm;

implementation

{$R *.dfm}

procedure TAdvancedDashboardForm.ConnectButtonClick(Sender: TObject);
begin
  ConnectionComponent.Connect;
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

procedure TAdvancedDashboardForm.ResolveGaugeValue(Sender: TObject;
  const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean);
begin
  Applied := Length(Messages) > 0;
  if Applied then
    Value := Messages[High(Messages)].NumericValue
  else
    Value := 0;
end;

end.
