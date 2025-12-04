unit Examples.Advanced.AdvancedDashboard;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.Threading,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls,
  OBD.Component.BindingHelpers,
  OBD.Connection.Component, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.CircularGauge;

type
  /// <summary>
  ///   Advanced demo form that exercises diagnostics, binding notifications,
  ///   and custom gauge value resolution while logging updates to a memo.
  /// </summary>
  TAdvancedDashboardForm = class(TForm)
  private
    /// <summary>
    ///   Memo used to display diagnostic messages from bindings and parsing.
    /// </summary>
    FLog: TMemo;
    /// <summary>
    ///   Connection component configured for WiFi adapters.
    /// </summary>
    FConnection: TOBDConnectionComponent;
    /// <summary>
    ///   Protocol component that parses inbound frames and exposes diagnostics.
    /// </summary>
    FProtocol: TOBDProtocolComponent;
    /// <summary>
    ///   Header controller that propagates connection state.
    /// </summary>
    FHeaderController: TOBDHeaderComponent;
    /// <summary>
    ///   Subheader controller that propagates connection and protocol captions.
    /// </summary>
    FSubheaderController: TOBDSubheaderComponent;
    /// <summary>
    ///   Gauge controller that resolves values from parsed messages.
    /// </summary>
    FGaugeController: TOBDGaugeComponent;
    /// <summary>
    ///   Skia-based header control displayed at the top of the form.
    /// </summary>
    FHeader: TOBDTouchHeader;
    /// <summary>
    ///   Skia-based subheader control displayed below the header.
    /// </summary>
    FSubheader: TOBDTouchSubheader;
    /// <summary>
    ///   Circular gauge visual hosted on the form.
    /// </summary>
    FGauge: TOBDCircularGauge;
    /// <summary>
    ///   Append a timestamped diagnostic line to the memo.
    /// </summary>
    procedure LogDiagnostic(const Text: string);
  public
    /// <summary>
    ///   Builds visuals, non-visual components, and attaches diagnostic hooks.
    /// </summary>
    procedure InitializeDashboard;
  end;

implementation

procedure TAdvancedDashboardForm.InitializeDashboard;
begin
  // Create a memo for diagnostics so binding events surface in the UI.
  FLog := TMemo.Create(Self);
  FLog.Parent := Self;
  FLog.Align := alBottom;
  FLog.Height := 120;

  // Construct visuals for the header, subheader, and gauge.
  FHeader := TOBDTouchHeader.Create(Self);
  FHeader.Parent := Self;
  FHeader.Align := alTop;

  FSubheader := TOBDTouchSubheader.Create(Self);
  FSubheader.Parent := Self;
  FSubheader.Align := alTop;

  FGauge := TOBDCircularGauge.Create(Self);
  FGauge.Parent := Self;
  FGauge.Align := alClient;

  // Configure a WiFi connection with diagnostic binding callbacks.
  FConnection := TOBDConnectionComponent.Create(Self);
  FConnection.ConnectionType := ctWiFi;
  FConnection.IPAddress := '192.168.0.10';
  FConnection.Port := 35000;

  // Parse incoming frames and enable diagnostic telemetry.
  FProtocol := TOBDProtocolComponent.Create(Self);
  FProtocol.ConnectionComponent := FConnection;
  FProtocol.AutoBindConnection := True;
  FProtocol.OnBindingNotification :=
    procedure(const Stage, Target: string; const Action: TOBDBindingAction; const ElapsedMs: Int64)
    begin
      LogDiagnostic(Format('Protocol binding %s %s in %d ms', [Stage, Target, ElapsedMs]));
    end;
  FProtocol.OnDiagnosticsUpdated :=
    procedure(Sender: TObject)
    begin
      LogDiagnostic(Format('Diagnostics buffer depth: %d', [Length(FProtocol.DiagnosticsSnapshot)]));
    end;

  // Apply connection state to the header with binding diagnostics.
  FHeaderController := TOBDHeaderComponent.Create(Self);
  FHeaderController.Header := FHeader;
  FHeaderController.ConnectionComponent := FConnection;
  FHeaderController.AutoBindConnection := True;
  FHeaderController.OnBindingNotification :=
    procedure(const Stage, Target: string; const Action: TOBDBindingAction; const ElapsedMs: Int64)
    begin
      LogDiagnostic(Format('Header binding %s %s in %d ms', [Stage, Target, ElapsedMs]));
    end;

  // Apply connection and protocol details to the subheader with diagnostics.
  FSubheaderController := TOBDSubheaderComponent.Create(Self);
  FSubheaderController.Subheader := FSubheader;
  FSubheaderController.ConnectionComponent := FConnection;
  FSubheaderController.ProtocolComponent := FProtocol;
  FSubheaderController.AutoBindConnection := True;
  FSubheaderController.AutoBindProtocol := True;
  FSubheaderController.OnBindingNotification :=
    procedure(const Stage, Target: string; const Action: TOBDBindingAction; const ElapsedMs: Int64)
    begin
      LogDiagnostic(Format('Subheader binding %s %s in %d ms', [Stage, Target, ElapsedMs]));
    end;

  // Resolve gauge values and report binding diagnostics.
  FGaugeController := TOBDGaugeComponent.Create(Self);
  FGaugeController.Gauge := FGauge;
  FGaugeController.ProtocolComponent := FProtocol;
  FGaugeController.AutoBindProtocol := True;
  FGaugeController.OnBindingNotification :=
    procedure(const Stage, Target: string; const Action: TOBDBindingAction; const ElapsedMs: Int64)
    begin
      LogDiagnostic(Format('Gauge binding %s %s in %d ms', [Stage, Target, ElapsedMs]));
    end;
  FGaugeController.OnResolveValue :=
    procedure(Sender: TObject; const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean)
    begin
      // Demo resolver: average all numeric values in the batch.
      if Length(Messages) = 0 then
      begin
        Applied := False;
        Value := 0;
        Exit;
      end;

      var Sum := 0.0;
      for var Msg in Messages do
        Sum := Sum + Msg.NumericValue;

      Value := Sum / Length(Messages);
      Applied := True;
    end;
end;

procedure TAdvancedDashboardForm.LogDiagnostic(const Text: string);
begin
  // Marshal diagnostics to the UI thread to keep logging thread-safe.
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FLog) then
        FLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Text);
    end);
end;

end.
