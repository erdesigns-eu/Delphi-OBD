//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.Component.pas
// CONTENTS       : Non-visual connection component wrappers
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : OpenAI Assistant
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/02/2025
//------------------------------------------------------------------------------
unit OBD.Connection.Component;

interface

uses
  System.Classes, System.SyncObjs, System.Bluetooth,
  OBD.Connection.Types, OBD.Connection, OBD.Connection.Serial,
  OBD.Connection.Bluetooth, OBD.Connection.Wifi, OBD.Connection.FTDI;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Event signature raised when the connection component changes state.
  /// </summary>
  TConnectionStateChangedEvent = procedure(Sender: TObject; const Connected: Boolean; const ConnectionType: TOBDConnectionType) of object;

  /// <summary>
  ///   Non-visual connection component that exposes OBD adapter connectivity
  ///   via published properties and thread-safe event hooks for design-time use.
  /// </summary>
  TOBDConnectionComponent = class(TComponent)
  private
    /// <summary>
    ///   Internal connection instance used to perform adapter operations.
    /// </summary>
    FConnection: IOBDConnection;
    /// <summary>
    ///   Selected connection type backing the published <c>ConnectionType</c> property.
    /// </summary>
    FConnectionType: TOBDConnectionType;
    /// <summary>
    ///   Serial COM port identifier used when <c>ConnectionType</c> equals <c>ctSerial</c>.
    /// </summary>
    FSerialPort: string;
    /// <summary>
    ///   Serial baud rate used when <c>ConnectionType</c> equals <c>ctSerial</c>.
    /// </summary>
    FSerialBaudRate: TBaudRate;
    /// <summary>
    ///   Bluetooth manager reference used to perform discovery when <c>ctBluetooth</c> is selected.
    /// </summary>
    FBluetoothManager: TBluetoothManager;
    /// <summary>
    ///   Bluetooth MAC address used when <c>ConnectionType</c> equals <c>ctBluetooth</c>.
    /// </summary>
    FBluetoothAddress: string;
    /// <summary>
    ///   IP address used when <c>ConnectionType</c> equals <c>ctWiFi</c>.
    /// </summary>
    FIPAddress: string;
    /// <summary>
    ///   TCP port used when <c>ConnectionType</c> equals <c>ctWiFi</c>.
    /// </summary>
    FPort: Integer;
    /// <summary>
    ///   FTDI serial number used when <c>ConnectionType</c> equals <c>ctFTDI</c>.
    /// </summary>
    FSerialNumber: string;
    /// <summary>
    ///   FTDI baud rate used when <c>ConnectionType</c> equals <c>ctFTDI</c>.
    /// </summary>
    FFTDIBaudRate: TBaudRate;
    /// <summary>
    ///   On-data-send event handler backing store for published event linkage.
    /// </summary>
    FOnDataSend: TDataSendEvent;
    /// <summary>
    ///   On-data-received event handler backing store for published event linkage.
    /// </summary>
    FOnDataReceived: TDataReceivedEvent;
    /// <summary>
    ///   On-error event handler backing store for published event linkage.
    /// </summary>
    FOnError: TErrorEvent;
    /// <summary>
    ///   Synchronization object guarding event assignment and connection swapping.
    /// </summary>
    FEventLock: TObject;
    /// <summary>
    ///   Event raised when the connection component toggles between connected and disconnected states.
    /// </summary>
    FOnConnectionStateChanged: TConnectionStateChangedEvent;
    /// <summary>
    ///   Build a connection instance matching the configured connection type.
    /// </summary>
    function BuildConnection: IOBDConnection;
    /// <summary>
    ///   Apply the component-level event handlers to the current connection instance.
    /// </summary>
    procedure ApplyEventHandlers;
    /// <summary>
    ///   Populate connection parameters for the configured connection type.
    /// </summary>
    function BuildParams: TOBDConnectionParams;
    /// <summary>
    ///   Event setter for published <c>OnDataSend</c> to keep handlers synced.
    /// </summary>
    procedure SetOnDataSend(const Value: TDataSendEvent);
    /// <summary>
    ///   Event setter for published <c>OnDataReceived</c> to keep handlers synced.
    /// </summary>
    procedure SetOnDataReceived(const Value: TDataReceivedEvent);
    /// <summary>
    ///   Event setter for published <c>OnError</c> to keep handlers synced.
    /// </summary>
    procedure SetOnError(const Value: TErrorEvent);
    /// <summary>
    ///   Setter for the published <c>ConnectionType</c> that resets stale connections.
    /// </summary>
    procedure SetConnectionType(const Value: TOBDConnectionType);
    /// <summary>
    ///   Notify listeners about connection state changes in a thread-safe manner.
    /// </summary>
    procedure NotifyConnectionStateChanged(const AConnected: Boolean);
    /// <summary>
    ///   Provides read-only access to the active connection instance.
    /// </summary>
    function GetConnectionInstance: IOBDConnection;
  public
    /// <summary>
    ///   Create a component instance and allocate synchronization primitives.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Tear down the component, disconnecting any active connection.
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///   Connect using the current published settings and propagate events.
    /// </summary>
    function Connect: Boolean;
    /// <summary>
    ///   Disconnect the active connection if present.
    /// </summary>
    function Disconnect: Boolean;
    /// <summary>
    ///   Indicates whether the wrapped connection is currently active.
    /// </summary>
    function Connected: Boolean;
    /// <summary>
    ///   Returns the active connection instance for consumers that need low-level access.
    /// </summary>
    property ConnectionInstance: IOBDConnection read GetConnectionInstance;
  published
    /// <summary>
    ///   Selected transport the component should instantiate when connecting.
    /// </summary>
    property ConnectionType: TOBDConnectionType read FConnectionType write SetConnectionType default ctSerial;
    /// <summary>
    ///   Serial COM port identifier used when <c>ConnectionType</c> equals <c>ctSerial</c>.
    /// </summary>
    property SerialPort: string read FSerialPort write FSerialPort;
    /// <summary>
    ///   Serial baud rate used when <c>ConnectionType</c> equals <c>ctSerial</c>.
    /// </summary>
    property SerialBaudRate: TBaudRate read FSerialBaudRate write FSerialBaudRate default br9600;
    /// <summary>
    ///   Bluetooth manager reference used when connecting via <c>ctBluetooth</c>.
    /// </summary>
    property BluetoothManager: TBluetoothManager read FBluetoothManager write FBluetoothManager;
    /// <summary>
    ///   Bluetooth MAC address used when connecting via <c>ctBluetooth</c>.
    /// </summary>
    property BluetoothAddress: string read FBluetoothAddress write FBluetoothAddress;
    /// <summary>
    ///   IP address used when connecting via <c>ctWiFi</c>.
    /// </summary>
    property IPAddress: string read FIPAddress write FIPAddress;
    /// <summary>
    ///   TCP port used when connecting via <c>ctWiFi</c>.
    /// </summary>
    property Port: Integer read FPort write FPort default 35000;
    /// <summary>
    ///   FTDI serial number used when connecting via <c>ctFTDI</c>.
    /// </summary>
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    /// <summary>
    ///   FTDI baud rate used when connecting via <c>ctFTDI</c>.
    /// </summary>
    property FTDIBaudRate: TBaudRate read FFTDIBaudRate write FFTDIBaudRate default br9600;
    /// <summary>
    ///   Event fired when data is transmitted by the active connection.
    /// </summary>
    property OnDataSend: TDataSendEvent read FOnDataSend write SetOnDataSend;
    /// <summary>
    ///   Event fired when data is received by the active connection.
    /// </summary>
    property OnDataReceived: TDataReceivedEvent read FOnDataReceived write SetOnDataReceived;
    /// <summary>
    ///   Event fired when the active connection reports an error.
    /// </summary>
    property OnError: TErrorEvent read FOnError write SetOnError;
    /// <summary>
    ///   Event fired when the connection toggles between connected and disconnected.
    /// </summary>
    property OnConnectionStateChanged: TConnectionStateChangedEvent read FOnConnectionStateChanged write FOnConnectionStateChanged;
  end;

implementation

uses
  System.SysUtils;

{ TOBDConnectionComponent }

procedure TOBDConnectionComponent.ApplyEventHandlers;
begin
  TMonitor.Enter(FEventLock);
  try
    if Assigned(FConnection) then
    begin
      FConnection.OnDataSend := FOnDataSend;
      FConnection.OnDataReceived := FOnDataReceived;
      FConnection.OnError := FOnError;
    end;
  finally
    TMonitor.Exit(FEventLock);
  end;
end;

function TOBDConnectionComponent.BuildConnection: IOBDConnection;
begin
  case FConnectionType of
    ctSerial:
      Result := TSerialOBDConnection.Create;
    ctBluetooth:
      Result := TBluetoothOBDConnection.Create;
    ctWiFi:
      Result := TWifiOBDConnection.Create;
    ctFTDI:
      Result := TFTDIOBDConnection.Create;
  else
    Result := nil;
  end;
end;

function TOBDConnectionComponent.BuildParams: TOBDConnectionParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ConnectionType := FConnectionType;
  case FConnectionType of
    ctSerial:
    begin
      Result.COMPort := ShortString(FSerialPort);
      Result.COMBaudRate := FSerialBaudRate;
    end;
    ctBluetooth:
    begin
      Result.Manager := FBluetoothManager;
      Result.Address := ShortString(FBluetoothAddress);
    end;
    ctWiFi:
    begin
      Result.IPAddress := ShortString(FIPAddress);
      Result.Port := FPort;
    end;
    ctFTDI:
    begin
      Result.SerialNumber := ShortString(FSerialNumber);
      Result.FTDIBaudRate := FFTDIBaudRate;
    end;
  end;
end;

procedure TOBDConnectionComponent.NotifyConnectionStateChanged(const AConnected: Boolean);
var
  Handler: TConnectionStateChangedEvent;
begin
  TMonitor.Enter(FEventLock);
  try
    Handler := FOnConnectionStateChanged;
  finally
    TMonitor.Exit(FEventLock);
  end;

  if Assigned(Handler) then
    Handler(Self, AConnected, FConnectionType);
end;

function TOBDConnectionComponent.GetConnectionInstance: IOBDConnection;
begin
  TMonitor.Enter(FEventLock);
  try
    Result := FConnection;
  finally
    TMonitor.Exit(FEventLock);
  end;
end;

function TOBDConnectionComponent.Connect: Boolean;
var
  Params: TOBDConnectionParams;
begin
  Result := False;
  TMonitor.Enter(FEventLock);
  try
    if FConnection = nil then
    begin
      FConnection := BuildConnection;
      ApplyEventHandlers;
    end;
    Params := BuildParams;
  finally
    TMonitor.Exit(FEventLock);
  end;
  if Assigned(FConnection) then
  begin
    if FConnection.Connected then
    begin
      FConnection.Disconnect;
      Result := FConnection.Connect(Params);
    end else
      Result := FConnection.Connect(Params);
    if Result then
      NotifyConnectionStateChanged(True);
  end;
end;

function TOBDConnectionComponent.Connected: Boolean;
begin
  if Assigned(FConnection) then
    Result := FConnection.Connected
  else
    Result := False;
end;

constructor TOBDConnectionComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventLock := TObject.Create;
  FConnectionType := ctSerial;
  FSerialBaudRate := br9600;
  FFTDIBaudRate := br9600;
  FPort := 35000;
end;

function TOBDConnectionComponent.Disconnect: Boolean;
begin
  Result := False;
  if Assigned(FConnection) then
  begin
    Result := FConnection.Disconnect;
    NotifyConnectionStateChanged(False);
  end;
end;

destructor TOBDConnectionComponent.Destroy;
begin
  Disconnect;
  TMonitor.Enter(FEventLock);
  try
    FConnection := nil;
  finally
    TMonitor.Exit(FEventLock);
  end;
  FEventLock.Free;
  inherited Destroy;
end;

procedure TOBDConnectionComponent.SetConnectionType(const Value: TOBDConnectionType);
begin
  if FConnectionType <> Value then
  begin
    TMonitor.Enter(FEventLock);
    try
      FConnectionType := Value;
      FConnection := nil;
    finally
      TMonitor.Exit(FEventLock);
    end;
  end;
end;

procedure TOBDConnectionComponent.SetOnDataReceived(const Value: TDataReceivedEvent);
begin
  TMonitor.Enter(FEventLock);
  try
    FOnDataReceived := Value;
    ApplyEventHandlers;
  finally
    TMonitor.Exit(FEventLock);
  end;
end;

procedure TOBDConnectionComponent.SetOnDataSend(const Value: TDataSendEvent);
begin
  TMonitor.Enter(FEventLock);
  try
    FOnDataSend := Value;
    ApplyEventHandlers;
  finally
    TMonitor.Exit(FEventLock);
  end;
end;

procedure TOBDConnectionComponent.SetOnError(const Value: TErrorEvent);
begin
  TMonitor.Enter(FEventLock);
  try
    FOnError := Value;
    ApplyEventHandlers;
  finally
    TMonitor.Exit(FEventLock);
  end;
end;

end.
