unit Examples.Bluetooth.BluetoothModule;

interface

uses
  System.Classes, System.Bluetooth,
  OBD.Connection.Component, OBD.Protocol.Component,
  OBD.Subheader.Component;

type
  /// <summary>
  ///   Data module that pairs a Bluetooth adapter with the protocol parser and subheader controller.
  /// </summary>
  TBluetoothDemoModule = class(TDataModule)
  private
    /// <summary>
    ///   Connection component configured for Bluetooth discovery.
    /// </summary>
    FConnection: TOBDConnectionComponent;
    /// <summary>
    ///   Protocol parser bound to the Bluetooth link.
    /// </summary>
    FProtocol: TOBDProtocolComponent;
    /// <summary>
    ///   Subheader controller that mirrors VCI and internet captions from the connection.
    /// </summary>
    FSubheaderController: TOBDSubheaderComponent;
  public
    /// <summary>
    ///   Initialize the Bluetooth connection settings and bind the controllers.
    /// </summary>
    procedure InitializeComponents;
  end;

implementation

procedure TBluetoothDemoModule.InitializeComponents;
begin
  // Configure the Bluetooth link with a manager and target address.
  FConnection := TOBDConnectionComponent.Create(Self);
  FConnection.ConnectionType := ctBluetooth;
  FConnection.BluetoothManager := TBluetoothManager.Current;
  FConnection.BluetoothAddress := '00:00:00:00:00:00';

  // Bind the protocol to the Bluetooth connection for automatic parsing.
  FProtocol := TOBDProtocolComponent.Create(Self);
  FProtocol.ConnectionComponent := FConnection;
  FProtocol.AutoBindConnection := True;

  // Present VCI and internet indicators based on the Bluetooth state.
  FSubheaderController := TOBDSubheaderComponent.Create(Self);
  FSubheaderController.ConnectionComponent := FConnection;
  FSubheaderController.ProtocolComponent := FProtocol;
  FSubheaderController.AutoBindConnection := True;
  FSubheaderController.AutoBindProtocol := True;
end;

end.
