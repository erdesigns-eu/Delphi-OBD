unit Examples.Serial.SerialModule;

interface

uses
  System.Classes,
  OBD.Connection.Component, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component;

type
  /// <summary>
  ///   Data module that configures a serial transport with custom captions for UI bindings.
  /// </summary>
  TSerialDemoModule = class(TDataModule)
  private
    /// <summary>
    ///   Connection component configured for a wired serial adapter.
    /// </summary>
    FConnection: TOBDConnectionComponent;
    /// <summary>
    ///   Protocol component that consumes serial frames.
    /// </summary>
    FProtocol: TOBDProtocolComponent;
    /// <summary>
    ///   Header controller that injects human-friendly captions when connected.
    /// </summary>
    FHeaderController: TOBDHeaderComponent;
    /// <summary>
    ///   Subheader controller that forwards connection details.
    /// </summary>
    FSubheaderController: TOBDSubheaderComponent;
  public
    /// <summary>
    ///   Initialize the serial connection, parser, and UI controllers.
    /// </summary>
    procedure InitializeComponents;
  end;

implementation

procedure TSerialDemoModule.InitializeComponents;
begin
  // Create the serial connection with an explicit baud rate and port.
  FConnection := TOBDConnectionComponent.Create(Self);
  FConnection.ConnectionType := ctSerial;
  FConnection.SerialPort := 'COM5';
  FConnection.SerialBaudRate := br115200;

  // Hook the protocol to the connection for auto-parsing.
  FProtocol := TOBDProtocolComponent.Create(Self);
  FProtocol.ConnectionComponent := FConnection;
  FProtocol.AutoBindConnection := True;

  // Customize header captions via the resolver to surface the selected port.
  FHeaderController := TOBDHeaderComponent.Create(Self);
  FHeaderController.ConnectionComponent := FConnection;
  FHeaderController.AutoBindConnection := True;
  FHeaderController.AutoApplyBattery := False;
  FHeaderController.OnResolveState :=
    procedure(Sender: TObject; const Connected: Boolean; const ConnectionType: TOBDConnectionType;
      out Caption: string; out BatteryPercentage: Single; out ApplyCaption, ApplyBattery: Boolean)
    begin
      ApplyCaption := True;
      ApplyBattery := False;
      if Connected then
        Caption := Format('Serial link active on %s', [FConnection.SerialPort])
      else
        Caption := 'Serial link disconnected';
      BatteryPercentage := 0;
    end;

  // Forward connection and protocol bindings to the subheader for status labels.
  FSubheaderController := TOBDSubheaderComponent.Create(Self);
  FSubheaderController.ConnectionComponent := FConnection;
  FSubheaderController.ProtocolComponent := FProtocol;
  FSubheaderController.AutoBindConnection := True;
  FSubheaderController.AutoBindProtocol := True;
end;

end.
