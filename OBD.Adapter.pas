//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.pas
// CONTENTS       : Base OBD Adapter Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/03/2024
//------------------------------------------------------------------------------
unit OBD.Adapter;

interface

uses
  System.SysUtils, Winapi.Windows, System.Bluetooth,

  OBD.Connection, OBD.Connection.Serial, OBD.Connection.Bluetooth,
  OBD.Connection.Wifi, OBD.Connection.FTDI, OBD.Protocol;

//------------------------------------------------------------------------------
// OTHER
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter Status
  /// </summary>
  TOBDAdapterStatus = (asNotConnected, asAdapterConnected, asOBDConnected, asCarConnected);

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter (INTERFACE)
  /// </summary>
  IOBDAdapter = interface
    ['{212E2851-98C7-4A8E-8848-6347CD18CD7D}']
    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    function Connect: Boolean;
    /// <summary>
    ///   Disconnect the connected OBD Adapter
    /// </summary>
    function Disconnect: Boolean;
    /// <summary>
    ///   Get the connected status of the adapter
    /// </summary>
    function Connected: Boolean;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter Exception
  /// </summary>
  TOBDAdapterException = class(Exception);

  /// <summary>
  ///   OBD Adapter (CLASS)
  /// </summary>
  TOBDAdapter = class(TInterfacedObject, IOBDAdapter)
  private
    /// <summary>
    ///   Adapter status
    /// </summary>
    FStatus: TOBDAdapterStatus;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    function Connect: Boolean; virtual; abstract;
    /// <summary>
    ///   Disconnect the connected OBD Adapter
    /// </summary>
    function Disconnect: Boolean; virtual; abstract;
    /// <summary>
    ///   Get the connected status of the adapter
    /// </summary>
    function Connected: Boolean; virtual; abstract;

    /// <summary>
    ///   Adapter status
    /// </summary>
    property Status: TOBDAdapterStatus read FStatus write FStatus;
  end;

  /// <summary>
  ///   OBD Adapter Settings Base Class
  /// </summary>
  TOBDAdapterSettings = class
  private
    /// <summary>
    ///   Adapter
    /// </summary>
    FAdapter: IOBDAdapter;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Adapter: TOBDAdapter); virtual;

    /// <summary>
    ///   Adapter
    /// </summary>
    property Adapter: IOBDAdapter read FAdapter;
  end;

  /// <summary>
  ///   OBD Adapter Serial Settings
  /// </summary>
  TOBDAdapterSerial = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   COM port
    /// </summary>
    FCOMPort: string;
    /// <summary>
    ///   COM Baudrate
    /// </summary>
    FBaudRate: Integer;

    /// <summary>
    ///   Set COM port
    /// </summary>
    procedure SetCOMPort(Value: string);
    /// <summary>
    ///   Set COM Baudrate
    /// </summary>
    procedure SetBaudRate(Value: Integer);
  public
    /// <summary>
    ///   COM port
    /// </summary>
    property COMPort: string read FCOMPort write SetCOMPort;
    /// <summary>
    ///   COM Baudrate
    /// </summary>
    property BaudRate: Integer read FBaudRate write SetBaudRate;
  end;

  /// <summary>
  ///   OBD Adapter Bluetooth Settings
  /// </summary>
  TOBDAdapterBluetooth = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   Bluetooth manager
    /// </summary>
    FManager: TBluetoothManager;
    /// <summary>
    ///   Bluetooth address
    /// </summary>
    FAddress: string;

    /// <summary>
    ///   Set bluetooth manager
    /// </summary>
    procedure SetManager(Value: TBluetoothManager);
    /// <summary>
    ///   Set address
    /// </summary>
    procedure SetAddress(Value: string);
  public
    /// <summary>
    ///   Bluetooth manager
    /// </summary>
    property Manager: TBluetoothManager read FManager write SetManager;
    /// <summary>
    ///   Bluetooth address
    /// </summary>
    property Address: string read FAddress write SetAddress;
  end;

  /// <summary>
  ///   OBD Adapter Wifi Settings
  /// </summary>
  TOBDAdapterWifi = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   IP Address
    /// </summary>
    FIPAddress: string;
    /// <summary>
    ///   Port
    /// </summary>
    FPort: Integer;

    /// <summary>
    ///   Set IP address
    /// </summary>
    procedure SetIPAddress(Value: string);
    /// <summary>
    ///   Set port
    /// </summary>
    procedure SetPort(Value: Integer);
  public
    /// <summary>
    ///   IP Address
    /// </summary>
    property IPAddress: string read FIPAddress write SetIPAddress;
    /// <summary>
    ///   Port
    /// </summary>
    property Port: Integer read FPort write SetPort;
  end;

  /// <summary>
  ///   OBD Adapter FTDI (USB) Settings
  /// </summary>
  TOBDAdapterFTDI = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   Serial Number
    /// </summary>
    FSerialNumber: string;

    /// <summary>
    ///   Set Serial Number
    /// </summary>
    procedure SetSerialNumber(Value: string);
  public
    /// <summary>
    ///   Serial Number
    /// </summary>
    property SerialNumber: string read FSerialNumber write SetSerialNumber;
  end;

  /// <summary>
  ///   Base ELM OBD Adapter Class
  /// </summary>
  TELMAdapter = class(TOBDAdapter)
  private
    /// <summary>
    ///   Serial (COM PORT) settings
    /// </summary>
    FSerial: TOBDAdapterSerial;
    /// <summary>
    ///   Bluetooth settings
    /// </summary>
    FBluetooth: TOBDAdapterBluetooth;
    /// <summary>
    ///   Wifi settings
    /// </summary>
    FWifi: TOBDAdapterWifi;
    /// <summary>
    ///   FTDI settings
    /// </summary>
    FFTDI: TOBDAdapterFTDI;
    /// <summary>
    ///   Connection Type (Unknown/Serial/Bluetooth/Wifi/FTDI)
    /// </summary>
    FConnectionType: TOBDConnectionType;
    /// <summary>
    ///   Connection (COM/FTDI/Bluetooth/Wifi)
    /// </summary>
    FConnection: IOBDConnection;
    /// <summary>
    ///   Protocol (Legacy/Can)
    /// </summary>
    FProtocol: IOBDProtocol;

    /// <summary>
    ///   Set connection type
    /// </summary>
    procedure SetConnectionType(Value: TOBDConnectionType);
  protected
    /// <summary>
    ///   Initialize connection
    /// </summary>
    procedure Init; virtual; abstract;
    /// <summary>
    ///   OBD Connection
    /// </summary>
    property Connection: IOBDConnection read FConnection;
  public
    /// <summary>
    ///   Constructor
    /// <summary>
    constructor Create; override;
    /// <summary>
    ///   Destructor
    /// <summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    function Connect: Boolean; override;
    /// <summary>
    ///   Disconnect the connected OBD Adapter
    /// </summary>
    function Disconnect: Boolean; override;
    /// <summary>
    ///   Get the connected status of the adapter
    /// </summary>
    function Connected: Boolean; override;

    /// <summary>
    ///   Serial (COM PORT) settings
    /// </summary>
    property Serial: TOBDAdapterSerial read FSerial;
    /// <summary>
    ///   Bluetooth settings
    /// </summary>
    property Bluetooth: TOBDAdapterBluetooth read FBluetooth;
    /// <summary>
    ///   Wifi settings
    /// </summary>
    property Wifi: TOBDAdapterWifi read FWifi;
    /// <summary>
    ///   FTDI settings
    /// </summary>
    property FTDI: TOBDAdapterFTDI read FFTDI;
    /// <summary>
    ///   Connection Type (Unknown/Serial/Bluetooth/Wifi/FTDI)
    /// </summary>
    property ConnectionType: TOBDConnectionType read FConnectionType write SetConnectionType;
    /// <summary>
    ///   OBD Protocol
    /// </summary>
    property Protocol: IOBDProtocol read FProtocol write FProtocol;
  end;


implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDAdapter.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Initialize status
  FStatus := asNotConnected;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDAdapterSettings.Create(Adapter: TOBDAdapter);
begin
  // Call inherited constructor
  inherited Create;
  // Set Adapter
  FAdapter := Adapter;
end;

//------------------------------------------------------------------------------
// SET COM PORT
//------------------------------------------------------------------------------
procedure TOBDAdapterSerial.SetCOMPort(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set COM Port
  if FCOMPort <> Value then FCOMPort := Value;
end;

//------------------------------------------------------------------------------
// SET COM BAUDRATE
//------------------------------------------------------------------------------
procedure TOBDAdapterSerial.SetBaudRate(Value: Integer);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set COM Port
  if FBaudRate <> Value then FBaudRate := Value;
end;

//------------------------------------------------------------------------------
// SET BLUETOOTH MANAGER
//------------------------------------------------------------------------------
procedure TOBDAdapterBluetooth.SetManager(Value: TBluetoothManager);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Bluetooth manager
  if FManager <> Value then FManager := Value;
end;

//------------------------------------------------------------------------------
// SET ADDRESS
//------------------------------------------------------------------------------
procedure TOBDAdapterBluetooth.SetAddress(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Address
  if FAddress <> Value then FAddress := Value;
end;

//------------------------------------------------------------------------------
// SET IP ADDRESS
//------------------------------------------------------------------------------
procedure TOBDAdapterWifi.SetIPAddress(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set IP Address
  if FIPAddress <> Value then FIPAddress := Value;
end;

//------------------------------------------------------------------------------
// SET PORT
//------------------------------------------------------------------------------
procedure TOBDAdapterWifi.SetPort(Value: Integer);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Port
  if FPort <> Value then FPort := Value;
end;

//------------------------------------------------------------------------------
// SET SERIAL NUMBER
//------------------------------------------------------------------------------
procedure TOBDAdapterFTDI.SetSerialNumber(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Serial Number
  if FSerialNumber <> Value then FSerialNumber := Value;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TELMAdapter.Create;
begin
  // Call inherited constructor
  inherited Create;

  // Serial (COM PORT) settings
  FSerial := TOBDAdapterSerial.Create(Self);
  // Bluetooth settings
  FBluetooth := TOBDAdapterBluetooth.Create(Self);
  // Wifi settings
  FWifi := TOBDAdapterWifi.Create(Self);
  // FTDI settings
  FFTDI := TOBDAdapterFTDI.Create(Self);
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TELMAdapter.Destroy;
begin
  // Free serial settings
  FSerial.Free;
  // Free bluetooth settings
  FBluetooth.Free;
  // Free wifi settings
  FWifi.Free;
  // Free FTDI settings
  FFTDI.Free;

  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SET CONNECTION TYPE
//------------------------------------------------------------------------------
procedure TELMAdapter.SetConnectionType(Value: TOBDConnectionType);
begin
  // Exit here if we're connected
  if Connected then Exit;
  // Set connection type
  if Value <> FConnectionType then
  begin
    FConnectionType := Value;
  end;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TELMAdapter.Connect: Boolean;
var
  Params: TOBDConnectionParams;
begin
  // initialize result
  Result := False;
  // Exit here if we are already connected
  if Connected then Exit;
  // Make sure there is a connection method selected
  if FConnectionType = ctUnknown then Exit;

  // Setup the connection type
  Params.ConnectionType := FConnectionType;

  // Create the connection
  case FConnectionType of
    ctSerial:
    begin
      // Create the connection class
      FConnection := TSerialOBDConnection.Create;
      // Setup params
      Params.COMPort := ShortString(Serial.COMPort);
      Params.BaudRate := Serial.BaudRate;
    end;

    ctBluetooth:
    begin
      // Create the connection class
      FConnection := TBluetoothOBDConnection.Create;
      // Setup params
      Params.Manager := Bluetooth.Manager;
      Params.Address := ShortString(Bluetooth.Address);
    end;

    ctWiFi:
    begin
      // Create the connection class
      FConnection := TWifiOBDConnection.Create;
      // Setup params
      Params.IPAddress := ShortString(Wifi.IPAddress);
      Params.Port := Wifi.Port;
    end;

    ctFTDI:
    begin
      // Create the connection class
      FConnection := TFTDIOBDConnection.Create;
      // Setup params
      Params.SerialNumber := ShortString(FTDI.SerialNumber);
    end;
  end;

  // Try to connect and return success status
  Result := FConnection.Connect(Params);

  // Update the status
  if Result then FStatus := asAdapterConnected;
  // If we are connected, then init the adapter.
  if Result then Init;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
function TELMAdapter.Disconnect: Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we aren't connected
  if not Connected then Exit;
  // Disconnect the connection
  Result := FConnection.Disconnect;
  // Remove the connection
  if Result then FConnection := nil;
  // Update the status
  if Result then FStatus := asNotConnected;
end;

//------------------------------------------------------------------------------
// GET CONNECTED STATUS
//------------------------------------------------------------------------------
function TELMAdapter.Connected: Boolean;
begin
  Result := Assigned(FConnection) and FConnection.Connected;
end;

end.
