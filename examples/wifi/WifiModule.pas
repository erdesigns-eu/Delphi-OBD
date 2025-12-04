unit Examples.Wifi.WifiModule;

interface

uses
  System.Classes,
  OBD.Connection.Component, OBD.Protocol.Component,
  OBD.Subheader.Component, OBD.Gauge.Component;

type
  /// <summary>
  ///   Data module that connects to a WiFi adapter and broadcasts updates to UI controllers.
  /// </summary>
  TWiFiDemoModule = class(TDataModule)
  private
    /// <summary>
    ///   Connection component configured for TCP/IP adapters.
    /// </summary>
    FConnection: TOBDConnectionComponent;
    /// <summary>
    ///   Protocol component that parses incoming WiFi frames.
    /// </summary>
    FProtocol: TOBDProtocolComponent;
    /// <summary>
    ///   Gauge controller that consumes parsed messages for visualization.
    /// </summary>
    FGaugeController: TOBDGaugeComponent;
    /// <summary>
    ///   Subheader controller that reflects connection state and protocol captions.
    /// </summary>
    FSubheaderController: TOBDSubheaderComponent;
  public
    /// <summary>
    ///   Initialize the WiFi transport, protocol parser, and UI bindings.
    /// </summary>
    procedure InitializeComponents;
  end;

implementation

procedure TWiFiDemoModule.InitializeComponents;
begin
  // Configure the WiFi link with the adapter IP/port pair.
  FConnection := TOBDConnectionComponent.Create(Self);
  FConnection.ConnectionType := ctWiFi;
  FConnection.IPAddress := '192.168.0.10';
  FConnection.Port := 35000;

  // Attach the protocol parser to the WiFi connection.
  FProtocol := TOBDProtocolComponent.Create(Self);
  FProtocol.ConnectionComponent := FConnection;
  FProtocol.AutoBindConnection := True;

  // Bind parsed messages to a gauge via the controller.
  FGaugeController := TOBDGaugeComponent.Create(Self);
  FGaugeController.ProtocolComponent := FProtocol;
  FGaugeController.AutoBindProtocol := True;
  FGaugeController.OnResolveValue :=
    procedure(Sender: TObject; const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean)
    begin
      Applied := Length(Messages) > 0;
      if Applied then
        Value := Messages[High(Messages)].NumericValue
      else
        Value := 0;
    end;

  // Broadcast WiFi state and protocol captions to the subheader.
  FSubheaderController := TOBDSubheaderComponent.Create(Self);
  FSubheaderController.ConnectionComponent := FConnection;
  FSubheaderController.ProtocolComponent := FProtocol;
  FSubheaderController.AutoBindConnection := True;
  FSubheaderController.AutoBindProtocol := True;
end;

end.
