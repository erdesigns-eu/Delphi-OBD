//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.BLE.pas
// CONTENTS       : Bluetooth Low Energy OBD Connection Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 10, 11
// RELEASE DATE   : 09/12/2025
// NOTE           : Targets the GATT-over-BLE OBD-II adapter family (Vgate
//                  iCar Pro BLE, Veepeak BLE+, OBDLink CX, generic FFE0/FFE1
//                  ELM327 BLE clones, and Nordic-UART-style adapters).
//
//                  The transport drives `System.Bluetooth.TBluetoothLEManager`,
//                  discovers the configured GATT service, locates a write
//                  characteristic and a notify characteristic, subscribes to
//                  notifications, and exposes the same Connect/Disconnect/
//                  Write* surface as the classic-Bluetooth transport so it
//                  drops into existing call sites.
//
//                  Defaults match the most common ELM327 BLE adapters
//                  (service `FFE0`, characteristic `FFE1` for both write and
//                  notify). Override via `TOBDConnectionParams.ServiceUUID`,
//                  `WriteCharUUID`, `NotifyCharUUID` for vendor-specific
//                  layouts (e.g. Nordic UART NUS).
//------------------------------------------------------------------------------
unit OBD.Connection.BLE;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Bluetooth, System.Bluetooth.Components,

  OBD.Connection,
  OBD.Connection.Types,
  OBD.Connection.Constants;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default GATT service UUID — used by FFE0/FFE1 ELM327 BLE adapters.
  /// </summary>
  BLE_DEFAULT_SERVICE_UUID = '0000FFE0-0000-1000-8000-00805F9B34FB';
  /// <summary>
  ///   Default characteristic UUID for write (and, on these adapters, also
  ///   the notify channel).
  /// </summary>
  BLE_DEFAULT_WRITE_CHAR_UUID = '0000FFE1-0000-1000-8000-00805F9B34FB';
  /// <summary>
  ///   Default characteristic UUID for notifications.
  /// </summary>
  BLE_DEFAULT_NOTIFY_CHAR_UUID = '0000FFE1-0000-1000-8000-00805F9B34FB';

  /// <summary>
  ///   Nordic UART Service (NUS) — used by some modern BLE OBD adapters.
  ///   Set ServiceUUID/WriteCharUUID/NotifyCharUUID to these on connection
  ///   parameters to target NUS devices.
  /// </summary>
  BLE_NUS_SERVICE_UUID  = '6E400001-B5A3-F393-E0A9-E50E24DCCA9E';
  BLE_NUS_RX_CHAR_UUID  = '6E400002-B5A3-F393-E0A9-E50E24DCCA9E'; // write
  BLE_NUS_TX_CHAR_UUID  = '6E400003-B5A3-F393-E0A9-E50E24DCCA9E'; // notify

  /// <summary>
  ///   Discovery timeout (ms) for finding a BLE device by address.
  /// </summary>
  BLE_DISCOVERY_TIMEOUT_MS = 8000;

  /// <summary>
  ///   Connection error codes.
  /// </summary>
  BLE_ERROR_DEVICE_NOT_FOUND   = 1;
  BLE_ERROR_SERVICE_NOT_FOUND  = 2;
  BLE_ERROR_CHAR_NOT_FOUND     = 3;
  BLE_ERROR_NOTIFY_FAILED      = 4;
  BLE_ERROR_WRITE_FAILED       = 5;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Low-level BLE GATT wrapper. Mirrors the surface of `TBluetooth` from
  ///   `OBD.Connection.Bluetooth`, but talks GATT instead of RFCOMM.
  /// </summary>
  TBluetoothLE = class
  private
    /// <summary>
    ///   The owning manager — caller-supplied to allow sharing one manager
    ///   between multiple connections.
    /// </summary>
    FManager: TBluetoothLEManager;
    /// <summary>
    ///   Connected device.
    /// </summary>
    FDevice: TBluetoothLEDevice;
    /// <summary>
    ///   Resolved write characteristic.
    /// </summary>
    FWriteChar: TBluetoothGattCharacteristic;
    /// <summary>
    ///   Resolved notify characteristic.
    /// </summary>
    FNotifyChar: TBluetoothGattCharacteristic;
    /// <summary>
    ///   GUID forms of the configured UUIDs.
    /// </summary>
    FServiceGuid: TBluetoothUUID;
    FWriteCharGuid: TBluetoothUUID;
    FNotifyCharGuid: TBluetoothUUID;
    /// <summary>
    ///   Connected flag — true once notifications are subscribed.
    /// </summary>
    FConnected: Boolean;
    /// <summary>
    ///   Discovery latch — set by the manager's discovery callback when the
    ///   target device is observed.
    /// </summary>
    FDiscoveryEvent: THandle;
    /// <summary>
    ///   Address being looked for during discovery.
    /// </summary>
    FTargetAddress: string;
    /// <summary>
    ///   Discovered candidate (set on the manager thread).
    /// </summary>
    FDiscoveredDevice: TBluetoothLEDevice;
    /// <summary>
    ///   Outbound event handlers.
    /// </summary>
    FOnReceiveData: TDataReceivedEvent;
    FOnSendData: TDataSendEvent;
    FOnError: TErrorEvent;
    /// <summary>
    ///   Bluetooth manager event handlers.
    /// </summary>
    procedure HandleDiscoveryEnd(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
    procedure HandleCharacteristicRead(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
    /// <summary>
    ///   Find the requested service on the device.
    /// </summary>
    function FindService: TBluetoothGattService;
    /// <summary>
    ///   Find a characteristic by UUID inside a service.
    /// </summary>
    function FindCharacteristic(const Service: TBluetoothGattService;
      const Uuid: TBluetoothUUID): TBluetoothGattCharacteristic;
    /// <summary>
    ///   Subscribe to notifications on the notify characteristic.
    /// </summary>
    function SubscribeNotifications: Boolean;
    /// <summary>
    ///   Raise OnError with a code + message.
    /// </summary>
    procedure RaiseError(Code: Integer; const Msg: string);
  public
    /// <summary>
    ///   Constructor.
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor — disconnects if connected.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Discover and connect to a BLE OBD adapter.
    /// </summary>
    function Connect(Manager: TBluetoothLEManager; const Address: string;
      const ServiceUuid, WriteUuid, NotifyUuid: string): Boolean;
    /// <summary>
    ///   Disconnect and tear down.
    /// </summary>
    procedure Disconnect;

    /// <summary>
    ///   Raw byte send through the write characteristic.
    /// </summary>
    function SendData(DataPtr: Pointer; DataSize: Cardinal): Cardinal;
    /// <summary>
    ///   Send a single byte.
    /// </summary>
    function SendByte(Value: Byte): Boolean;
    /// <summary>
    ///   Send an AnsiChar.
    /// </summary>
    function SendChar(Value: AnsiChar): Boolean;
    /// <summary>
    ///   Send an AnsiString. The caller is responsible for trailing CR/LF
    ///   if the adapter requires it (ELM327 expects `\r`).
    /// </summary>
    function SendString(const S: AnsiString): Boolean;

    /// <summary>
    ///   Connected flag.
    /// </summary>
    property Connected: Boolean read FConnected;
    /// <summary>
    ///   On-receive callback — fired from the manager's notification thread.
    ///   Consumers that need UI-thread delivery must marshal themselves
    ///   (e.g. `TThread.Queue`).
    /// </summary>
    property OnReceiveData: TDataReceivedEvent read FOnReceiveData write FOnReceiveData;
    /// <summary>
    ///   On-send callback.
    /// </summary>
    property OnSendData: TDataSendEvent read FOnSendData write FOnSendData;
    /// <summary>
    ///   On-error callback.
    /// </summary>
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

  /// <summary>
  ///   BLE OBD Connection — concrete `TOBDConnection` subclass. Drop-in
  ///   replacement for `TBluetoothOBDConnection` for BLE adapters.
  /// </summary>
  TBluetoothLEOBDConnection = class(TOBDConnection)
  private
    FBluetoothLE: TBluetoothLE;
  protected
    function Connected: Boolean; override;
    procedure OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    procedure OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    procedure OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connect(const Params: TOBDConnectionParams): Boolean; override;
    function Disconnect: Boolean; override;
    function WriteATCommand(const ATCommand: string): Boolean; override;
    function WriteSTCommand(const STCommand: string): Boolean; override;
    function WriteOBDCommand(const OBDCommand: string): Boolean; override;
  end;

implementation

uses System.StrUtils, System.SyncObjs;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TBluetoothLE.Create;
begin
  inherited Create;
  FConnected := False;
  // Manual-reset event used as a discovery latch; set by HandleDiscoveryEnd
  // when the target address is matched.
  FDiscoveryEvent := CreateEvent(nil, True, False, nil);
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TBluetoothLE.Destroy;
begin
  if FConnected then Disconnect;
  if FDiscoveryEvent <> 0 then CloseHandle(FDiscoveryEvent);
  inherited;
end;

//------------------------------------------------------------------------------
// RAISE ERROR
//------------------------------------------------------------------------------
procedure TBluetoothLE.RaiseError(Code: Integer; const Msg: string);
begin
  if Assigned(FOnError) then FOnError(Self, Code, Msg);
end;

//------------------------------------------------------------------------------
// HANDLE DISCOVERY END
//------------------------------------------------------------------------------
procedure TBluetoothLE.HandleDiscoveryEnd(const Sender: TObject;
  const ADeviceList: TBluetoothLEDeviceList);
var
  I: Integer;
begin
  // Match the requested address against the discovered list and latch the
  // first hit — the discovery thread will be unblocked by SetEvent.
  for I := 0 to ADeviceList.Count - 1 do
  begin
    if SameText(ADeviceList[I].Address, FTargetAddress) then
    begin
      FDiscoveredDevice := ADeviceList[I];
      Break;
    end;
  end;
  SetEvent(FDiscoveryEvent);
end;

//------------------------------------------------------------------------------
// HANDLE CHARACTERISTIC READ (notification arrival)
//------------------------------------------------------------------------------
procedure TBluetoothLE.HandleCharacteristicRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
var
  Bytes: TBytes;
begin
  if AGattStatus <> TBluetoothGattStatus.Success then Exit;
  if not Assigned(ACharacteristic) then Exit;

  // Only forward the characteristic we subscribed to.
  if Assigned(FNotifyChar) and not ACharacteristic.UUID.Equals(FNotifyChar.UUID) then
    Exit;

  Bytes := ACharacteristic.Value;
  if Length(Bytes) = 0 then Exit;

  if Assigned(FOnReceiveData) then
    FOnReceiveData(Self, @Bytes[0], Length(Bytes));
end;

//------------------------------------------------------------------------------
// FIND SERVICE
//------------------------------------------------------------------------------
function TBluetoothLE.FindService: TBluetoothGattService;
var
  Services: TBluetoothGattServiceList;
  S: TBluetoothGattService;
begin
  Result := nil;
  if not Assigned(FDevice) then Exit;

  Services := FDevice.Services;
  if not Assigned(Services) then Exit;

  for S in Services do
    if S.UUID.Equals(FServiceGuid) then
      Exit(S);
end;

//------------------------------------------------------------------------------
// FIND CHARACTERISTIC
//------------------------------------------------------------------------------
function TBluetoothLE.FindCharacteristic(const Service: TBluetoothGattService;
  const Uuid: TBluetoothUUID): TBluetoothGattCharacteristic;
var
  C: TBluetoothGattCharacteristic;
begin
  Result := nil;
  if not Assigned(Service) then Exit;
  for C in Service.Characteristics do
    if C.UUID.Equals(Uuid) then
      Exit(C);
end;

//------------------------------------------------------------------------------
// SUBSCRIBE TO NOTIFICATIONS
//------------------------------------------------------------------------------
function TBluetoothLE.SubscribeNotifications: Boolean;
begin
  Result := False;
  if not Assigned(FDevice) or not Assigned(FNotifyChar) then Exit;
  // Ask the stack to enable CCCD notifications and start delivering values
  // through OnCharacteristicRead.
  Result := FDevice.SetCharacteristicNotification(FNotifyChar, True);
  if not Result then
    RaiseError(BLE_ERROR_NOTIFY_FAILED, 'Failed to enable notifications');
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TBluetoothLE.Connect(Manager: TBluetoothLEManager;
  const Address: string;
  const ServiceUuid, WriteUuid, NotifyUuid: string): Boolean;
var
  Service: TBluetoothGattService;
begin
  Result := False;
  if FConnected then Exit;
  if not Assigned(Manager) then
  begin
    RaiseError(BLE_ERROR_DEVICE_NOT_FOUND, 'BLE manager is nil');
    Exit;
  end;

  FManager := Manager;
  FTargetAddress := Address;
  FDiscoveredDevice := nil;
  FServiceGuid := TBluetoothUUID.Create(ServiceUuid);
  FWriteCharGuid := TBluetoothUUID.Create(WriteUuid);
  FNotifyCharGuid := TBluetoothUUID.Create(NotifyUuid);

  // Latch ready, hook discovery, kick off scan.
  ResetEvent(FDiscoveryEvent);
  FManager.OnDiscoveryEnd := HandleDiscoveryEnd;
  try
    FManager.StartDiscovery(BLE_DISCOVERY_TIMEOUT_MS);

    // Wait for HandleDiscoveryEnd to fire — fall through on timeout.
    if WaitForSingleObject(FDiscoveryEvent, BLE_DISCOVERY_TIMEOUT_MS + 1000)
       = WAIT_OBJECT_0 then
    begin
      FDevice := FDiscoveredDevice;
    end;
  finally
    FManager.OnDiscoveryEnd := nil;
  end;

  if not Assigned(FDevice) then
  begin
    RaiseError(BLE_ERROR_DEVICE_NOT_FOUND,
      Format('No BLE device with address "%s" found within %d ms',
        [Address, BLE_DISCOVERY_TIMEOUT_MS]));
    Exit;
  end;

  // Resolve service + characteristics.
  Service := FindService;
  if not Assigned(Service) then
  begin
    RaiseError(BLE_ERROR_SERVICE_NOT_FOUND,
      Format('GATT service %s not found on device', [ServiceUuid]));
    Exit;
  end;

  FWriteChar  := FindCharacteristic(Service, FWriteCharGuid);
  FNotifyChar := FindCharacteristic(Service, FNotifyCharGuid);
  if not Assigned(FWriteChar) or not Assigned(FNotifyChar) then
  begin
    RaiseError(BLE_ERROR_CHAR_NOT_FOUND,
      'Required write or notify characteristic not present on service');
    Exit;
  end;

  // Hook the read/notify pipe and arm notifications.
  FDevice.OnCharacteristicRead := HandleCharacteristicRead;
  if not SubscribeNotifications then Exit;

  FConnected := True;
  Result := True;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
procedure TBluetoothLE.Disconnect;
begin
  if not FConnected then Exit;
  try
    if Assigned(FDevice) and Assigned(FNotifyChar) then
      FDevice.SetCharacteristicNotification(FNotifyChar, False);
  except
    // Disconnect must be best-effort — never raise.
  end;
  // The device object is owned by the manager; we just drop our references.
  FNotifyChar := nil;
  FWriteChar := nil;
  FDevice := nil;
  FConnected := False;
end;

//------------------------------------------------------------------------------
// SEND DATA
//------------------------------------------------------------------------------
function TBluetoothLE.SendData(DataPtr: Pointer; DataSize: Cardinal): Cardinal;
var
  Buf: TBytes;
begin
  Result := 0;
  if not FConnected or not Assigned(FDevice) or not Assigned(FWriteChar) then Exit;
  if (DataPtr = nil) or (DataSize = 0) then Exit;

  SetLength(Buf, DataSize);
  Move(DataPtr^, Buf[0], DataSize);

  if Assigned(FOnSendData) then FOnSendData(Self, DataPtr, DataSize);

  if FDevice.WriteCharacteristic(FWriteChar, Buf) then
    Result := DataSize
  else
    RaiseError(BLE_ERROR_WRITE_FAILED, 'WriteCharacteristic returned false');
end;

//------------------------------------------------------------------------------
// SEND BYTE / CHAR / STRING
//------------------------------------------------------------------------------
function TBluetoothLE.SendByte(Value: Byte): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

function TBluetoothLE.SendChar(Value: AnsiChar): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

function TBluetoothLE.SendString(const S: AnsiString): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  if L = 0 then Exit(True);
  Result := SendData(PAnsiChar(S), L) = L;
end;

//==============================================================================
// TBluetoothLEOBDConnection
//==============================================================================

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TBluetoothLEOBDConnection.Create;
begin
  inherited;
  FBluetoothLE := TBluetoothLE.Create;
  FBluetoothLE.OnReceiveData := OnReceiveData;
  FBluetoothLE.OnSendData := OnSendData;
  FBluetoothLE.OnError := OnConnectionError;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TBluetoothLEOBDConnection.Destroy;
begin
  FBluetoothLE.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// CONNECTED
//------------------------------------------------------------------------------
function TBluetoothLEOBDConnection.Connected: Boolean;
begin
  Result := Assigned(FBluetoothLE) and FBluetoothLE.Connected;
end;

//------------------------------------------------------------------------------
// EVENT FORWARDERS
//------------------------------------------------------------------------------
procedure TBluetoothLEOBDConnection.OnReceiveData(Sender: TObject;
  DataPtr: Pointer; DataSize: DWORD);
begin
  InvokeDataReceived(DataPtr, DataSize);
end;

procedure TBluetoothLEOBDConnection.OnSendData(Sender: TObject;
  DataPtr: Pointer; DataSize: DWORD);
begin
  InvokeDataSend(DataPtr, DataSize);
end;

procedure TBluetoothLEOBDConnection.OnConnectionError(Sender: TObject;
  ErrorCode: Integer; ErrorMessage: string);
begin
  InvokeError(ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TBluetoothLEOBDConnection.Connect(
  const Params: TOBDConnectionParams): Boolean;
var
  Service, Write, Notify: string;
begin
  TMonitor.Enter(FConnectionLock);
  try
    Result := Connected;
    if Result then Exit;
    if Params.ConnectionType <> ctBluetoothLE then Exit;

    // Empty UUID strings fall back to FFE0/FFE1 defaults so a caller can
    // opt in just by setting LEAddress.
    Service := Params.ServiceUUID;
    if Service = '' then Service := BLE_DEFAULT_SERVICE_UUID;

    Write := Params.WriteCharUUID;
    if Write = '' then Write := BLE_DEFAULT_WRITE_CHAR_UUID;

    Notify := Params.NotifyCharUUID;
    if Notify = '' then Notify := BLE_DEFAULT_NOTIFY_CHAR_UUID;

    Result := FBluetoothLE.Connect(Params.LEManager, Params.LEAddress,
      Service, Write, Notify);
  finally
    TMonitor.Exit(FConnectionLock);
  end;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
function TBluetoothLEOBDConnection.Disconnect: Boolean;
begin
  TMonitor.Enter(FConnectionLock);
  try
    Result := Connected;
    if Result then
    begin
      FBluetoothLE.Disconnect;
      Result := Connected;
    end;
  finally
    TMonitor.Exit(FConnectionLock);
  end;
end;

//------------------------------------------------------------------------------
// WRITE COMMANDS
//------------------------------------------------------------------------------
function TBluetoothLEOBDConnection.WriteATCommand(
  const ATCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('AT', ATCommand) = 1, ATCommand,
    Format(IfThen(Pos(' ', ATCommand) > 0, 'AT %s', 'AT%s'), [ATCommand]));
  // ELM327 BLE adapters require a CR terminator just like the RFCOMM ones.
  Result := FBluetoothLE.SendString(AnsiString(S + #13));
end;

function TBluetoothLEOBDConnection.WriteSTCommand(
  const STCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('ST', STCommand) = 1, STCommand,
    Format(IfThen(Pos(' ', STCommand) > 0, 'ST %s', 'ST%s'), [STCommand]));
  Result := FBluetoothLE.SendString(AnsiString(S + #13));
end;

function TBluetoothLEOBDConnection.WriteOBDCommand(
  const OBDCommand: string): Boolean;
begin
  Result := FBluetoothLE.SendString(AnsiString(OBDCommand + #13));
end;

end.
