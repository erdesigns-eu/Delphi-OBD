unit Examples.FTDI.FTDIModule;

interface

uses
  System.Classes,
  OBD.Connection.Component, OBD.Protocol.Component,
  OBD.Header.Component, OBD.Subheader.Component;

type
  /// <summary>
  ///   Data module demonstrating FTDI cable setup with guarded reconnects.
  /// </summary>
  TFTDIDemoModule = class(TDataModule)
  private
    /// <summary>
    ///   FTDI-backed connection component.
    /// </summary>
    FConnection: TOBDConnectionComponent;
    /// <summary>
    ///   Protocol component that parses FTDI frames.
    /// </summary>
    FProtocol: TOBDProtocolComponent;
    /// <summary>
    ///   Header controller that mirrors connection changes.
    /// </summary>
    FHeaderController: TOBDHeaderComponent;
    /// <summary>
    ///   Subheader controller that reflects FTDI connectivity.
    /// </summary>
    FSubheaderController: TOBDSubheaderComponent;
  public
    /// <summary>
    ///   Initialize the FTDI connection, parser, and UI controllers.
    /// </summary>
    procedure InitializeComponents;
    /// <summary>
    ///   Attempt a reconnect using the configured FTDI parameters.
    /// </summary>
    procedure AttemptReconnect;
  end;

implementation

procedure TFTDIDemoModule.InitializeComponents;
begin
  // Configure the FTDI link with serial number and baud rate.
  FConnection := TOBDConnectionComponent.Create(Self);
  FConnection.ConnectionType := ctFTDI;
  FConnection.SerialNumber := 'FT123456';
  FConnection.FTDIBaudRate := br115200;

  // Bind protocol parsing to the FTDI connection.
  FProtocol := TOBDProtocolComponent.Create(Self);
  FProtocol.ConnectionComponent := FConnection;
  FProtocol.AutoBindConnection := True;

  // Apply connection state to header and subheader visuals.
  FHeaderController := TOBDHeaderComponent.Create(Self);
  FHeaderController.ConnectionComponent := FConnection;
  FHeaderController.AutoBindConnection := True;

  FSubheaderController := TOBDSubheaderComponent.Create(Self);
  FSubheaderController.ConnectionComponent := FConnection;
  FSubheaderController.ProtocolComponent := FProtocol;
  FSubheaderController.AutoBindConnection := True;
  FSubheaderController.AutoBindProtocol := True;
end;

procedure TFTDIDemoModule.AttemptReconnect;
begin
  // Simplified reconnect that safely disconnects before attempting a reconnect.
  if FConnection.Connected then
    FConnection.Disconnect;
  FConnection.Connect;
end;

end.
