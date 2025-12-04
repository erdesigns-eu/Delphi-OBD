unit Examples.Simple.SimpleDashboard;

interface

uses
  System.Classes, Vcl.Forms, Vcl.Controls,
  OBD.Connection.Component, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.CircularGauge;

type
  /// <summary>
  ///   Minimal serial demo form that wires connection, protocol, header,
  ///   subheader, and gauge components together with defaults.
  /// </summary>
  TSimpleDashboardForm = class(TForm)
  private
    /// <summary>
    ///   Non-visual connection wrapper configured for the serial transport.
    /// </summary>
    FConnection: TOBDConnectionComponent;
    /// <summary>
    ///   Protocol parser that consumes connection data and emits parsed messages.
    /// </summary>
    FProtocol: TOBDProtocolComponent;
    /// <summary>
    ///   Controller that applies connection state to the header visual.
    /// </summary>
    FHeaderController: TOBDHeaderComponent;
    /// <summary>
    ///   Controller that applies connection and protocol details to the subheader visual.
    /// </summary>
    FSubheaderController: TOBDSubheaderComponent;
    /// <summary>
    ///   Controller that maps protocol messages into gauge values.
    /// </summary>
    FGaugeController: TOBDGaugeComponent;
    /// <summary>
    ///   Skia-based header visual displayed at the top of the form.
    /// </summary>
    FHeader: TOBDTouchHeader;
    /// <summary>
    ///   Skia-based subheader visual displayed beneath the header.
    /// </summary>
    FSubheader: TOBDTouchSubheader;
    /// <summary>
    ///   Circular gauge visual hosted on the form.
    /// </summary>
    FGauge: TOBDCircularGauge;
  public
    /// <summary>
    ///   Builds the form controls, non-visual components, and default bindings.
    /// </summary>
    procedure InitializeDashboard;
  end;

implementation

procedure TSimpleDashboardForm.InitializeDashboard;
begin
  // Create the visual controls first so they are ready for binding.
  FHeader := TOBDTouchHeader.Create(Self);
  FHeader.Parent := Self;
  FHeader.Align := alTop;

  FSubheader := TOBDTouchSubheader.Create(Self);
  FSubheader.Parent := Self;
  FSubheader.Align := alTop;

  FGauge := TOBDCircularGauge.Create(Self);
  FGauge.Parent := Self;
  FGauge.Align := alClient;

  // Create the non-visual components that manage connectivity and parsing.
  FConnection := TOBDConnectionComponent.Create(Self);
  FConnection.SerialPort := 'COM3';
  FConnection.SerialBaudRate := br115200;

  FProtocol := TOBDProtocolComponent.Create(Self);
  FProtocol.ConnectionComponent := FConnection;

  // Bind header and subheader visuals to the connection component for captions and indicators.
  FHeaderController := TOBDHeaderComponent.Create(Self);
  FHeaderController.Header := FHeader;
  FHeaderController.ConnectionComponent := FConnection;
  FHeaderController.AutoBindConnection := True;

  FSubheaderController := TOBDSubheaderComponent.Create(Self);
  FSubheaderController.Subheader := FSubheader;
  FSubheaderController.ConnectionComponent := FConnection;
  FSubheaderController.ProtocolComponent := FProtocol;
  FSubheaderController.AutoBindConnection := True;
  FSubheaderController.AutoBindProtocol := True;

  // Bind protocol output to the gauge for a simple live value preview.
  FGaugeController := TOBDGaugeComponent.Create(Self);
  FGaugeController.Gauge := FGauge;
  FGaugeController.ProtocolComponent := FProtocol;
  FGaugeController.AutoBindProtocol := True;
  FGaugeController.OnResolveValue :=
    procedure(Sender: TObject; const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean)
    begin
      // Sample resolver: apply the latest numeric payload when present.
      Applied := Length(Messages) > 0;
      if Applied then
        Value := Messages[High(Messages)].NumericValue
      else
        Value := 0;
    end;
end;

end.
