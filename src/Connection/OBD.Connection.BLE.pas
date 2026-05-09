//------------------------------------------------------------------------------
//  OBD.Connection.BLE
//
//  Bluetooth Low Energy (GATT) transport for ELM327-BLE clones and
//  similar adapters. Default profile FFE0 / FFE1 (write + notify on
//  the same characteristic). Service / characteristic UUIDs are
//  overridable via TOBDBLESettings.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//
//  Future work :
//    - Connection-parameter tuning (interval / latency) for chips that
//      support it.
//    - Pairing-required adapters (e.g. some Nordic UART variants) via
//      OnTransportError.
//------------------------------------------------------------------------------

unit OBD.Connection.BLE;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Bluetooth,
  System.Bluetooth.Components,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings;

type
  /// <summary>Bluetooth Low Energy transport.</summary>
  /// <remarks>
  ///   The BLE manager dispatches its events on the manager's own
  ///   thread; we forward them to the parent transport's callbacks
  ///   verbatim. The TOBDConnection component is responsible for
  ///   marshalling to the main thread.
  /// </remarks>
  TOBDBLETransport = class(TInterfacedObject, IOBDConnectionTransport)
  strict private
    FLock: TCriticalSection;
    FManager: TBluetoothLEManager;
    FDevice: TBluetoothLEDevice;
    FService: TBluetoothGattService;
    FWriteChar: TBluetoothGattCharacteristic;
    FNotifyChar: TBluetoothGattCharacteristic;
    FState: TOBDConnectionState;
    FOnDataReceived: TOBDBytesEvent;
    FOnStateChanged: TOBDStateEvent;
    FOnTransportError: TOBDTransportErrorEvent;
    procedure SetState(ANewState: TOBDConnectionState);
    procedure HandleBytes(const ABytes: TBytes);
    procedure HandleError(ACode: TOBDErrorCode; const AMessage: string);
    procedure HandleCharRead(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    function NormaliseUUID(const ARaw: string): TGUID;
  public
    /// <summary>Constructs an idle BLE transport.</summary>
    constructor Create;
    /// <summary>Disconnects if open and releases the lock.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connects to the configured BLE device, locates the GATT
    ///   service and characteristics, and subscribes to notifications.
    /// </summary>
    /// <param name="ASettings">Device address, service UUID, write
    /// and notify characteristic UUIDs.</param>
    /// <remarks>
    ///   The device must already be paired with the host OS.
    ///   Notifications fire on the BLE manager's worker thread.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c> or
    /// device address is empty.</exception>
    /// <exception cref="EOBDError">Manager unavailable, device not
    /// paired, service / characteristic missing, or notification
    /// subscribe failed.</exception>
    procedure Open(const ASettings: TOBDBLESettings);

    /// <summary>Disables notifications and disconnects.</summary>
    procedure Close;
    /// <summary>True when GATT links are alive.</summary>
    /// <returns><c>True</c> if state is <c>csOpen</c>.</returns>
    function IsOpen: Boolean;
    /// <summary>Current lifecycle state.</summary>
    /// <returns>State enum.</returns>
    function State: TOBDConnectionState;
    /// <summary>Writes bytes to the configured write
    /// characteristic.</summary>
    /// <param name="ABytes">Bytes to send. Empty returns 0.</param>
    /// <returns><c>Length(ABytes)</c> on success; 0 on transport
    /// error.</returns>
    /// <exception cref="EOBDNotConnected">Not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer;

    /// <summary>Internal accessor.</summary>
    function GetOnDataReceived: TOBDBytesEvent;
    /// <summary>Internal mutator.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnDataReceived(const AValue: TOBDBytesEvent);
    /// <summary>Internal accessor.</summary>
    function GetOnStateChanged: TOBDStateEvent;
    /// <summary>Internal mutator.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnStateChanged(const AValue: TOBDStateEvent);
    /// <summary>Internal accessor.</summary>
    function GetOnTransportError: TOBDTransportErrorEvent;
    /// <summary>Internal mutator.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnTransportError(const AValue: TOBDTransportErrorEvent);
  end;

implementation

{ ---- TOBDBLETransport -------------------------------------------------------- }

constructor TOBDBLETransport.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FState := csClosed;
end;

destructor TOBDBLETransport.Destroy;
begin
  Close;
  FLock.Free;
  inherited;
end;

procedure TOBDBLETransport.SetState(ANewState: TOBDConnectionState);
var
  Handler: TOBDStateEvent;
begin
  FLock.Enter;
  try
    if FState = ANewState then Exit;
    FState := ANewState;
    Handler := FOnStateChanged;
  finally
    FLock.Leave;
  end;
  if Assigned(Handler) then
    Handler(Self, ANewState);
end;

procedure TOBDBLETransport.HandleBytes(const ABytes: TBytes);
var
  Handler: TOBDBytesEvent;
begin
  Handler := FOnDataReceived;
  if Assigned(Handler) then
    Handler(Self, ABytes);
end;

procedure TOBDBLETransport.HandleError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Handler: TOBDTransportErrorEvent;
begin
  Handler := FOnTransportError;
  if Assigned(Handler) then
    Handler(Self, ACode, AMessage);
  SetState(csError);
end;

function TOBDBLETransport.NormaliseUUID(const ARaw: string): TGUID;
var
  S: string;
begin
  S := Trim(ARaw);
  if (S = '') then
    raise EOBDConfig.Create('UUID is empty');
  if (Length(S) = 4) or (Length(S) = 8) then
    // Short 16-bit / 32-bit form: expand to base BLE UUID.
    S := Format('0000%s-0000-1000-8000-00805F9B34FB',
      [S.PadLeft(8, '0')]);
  if (Length(S) > 0) and (S[1] <> '{') then
    S := '{' + S + '}';
  Result := StringToGUID(S);
end;

procedure TOBDBLETransport.HandleCharRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
begin
  if AGattStatus = TBluetoothGattStatus.Success then
  begin
    if Length(ACharacteristic.Value) > 0 then
      HandleBytes(ACharacteristic.Value);
  end
  else
    HandleError(oeIO, Format('GATT read failed (status %d)',
      [Ord(AGattStatus)]));
end;

procedure TOBDBLETransport.Open(const ASettings: TOBDBLESettings);
var
  ServiceGuid, WriteGuid, NotifyGuid: TGUID;
  Devices: TBluetoothLEDeviceList;
  D: TBluetoothLEDevice;
  Needle: string;
begin
  if ASettings = nil then
    raise EOBDConfig.Create('BLE settings are nil');
  if Trim(ASettings.DeviceAddress) = '' then
    raise EOBDConfig.Create('BLE device address is empty');

  ServiceGuid := NormaliseUUID(ASettings.ServiceUUID);
  WriteGuid   := NormaliseUUID(ASettings.WriteCharUUID);
  NotifyGuid  := NormaliseUUID(ASettings.NotifyCharUUID);

  SetState(csOpening);
  try
    FManager := TBluetoothLEManager.Current;
    if FManager = nil then
      raise EOBDError.Create('No BLE manager available on this host');
    Devices := FManager.GetPairedDevices;
    if Devices = nil then
      raise EOBDError.Create('No paired BLE devices');
    Needle := UpperCase(Trim(ASettings.DeviceAddress));
    FDevice := nil;
    for D in Devices do
      if (UpperCase(D.Address) = Needle) or
         SameText(D.DeviceName, ASettings.DeviceAddress) then
      begin
        FDevice := D;
        Break;
      end;
    if FDevice = nil then
      raise EOBDError.CreateFmt(
        'BLE device "%s" not found among paired devices',
        [ASettings.DeviceAddress]);

    FService := FDevice.GetService(ServiceGuid);
    if FService = nil then
      raise EOBDError.CreateFmt('BLE service %s not found',
        [GUIDToString(ServiceGuid)]);
    FWriteChar := FDevice.GetCharacteristic(FService, WriteGuid);
    if FWriteChar = nil then
      raise EOBDError.CreateFmt('Write characteristic %s not found',
        [GUIDToString(WriteGuid)]);
    FNotifyChar := FDevice.GetCharacteristic(FService, NotifyGuid);
    if FNotifyChar = nil then
      raise EOBDError.CreateFmt('Notify characteristic %s not found',
        [GUIDToString(NotifyGuid)]);

    FDevice.OnCharacteristicRead := HandleCharRead;
    if not FDevice.SetCharacteristicNotification(FNotifyChar, True) then
      raise EOBDError.Create(
        'Failed to enable notifications on the notify characteristic');
  except
    on E: Exception do
    begin
      FDevice := nil;
      SetState(csError);
      raise EOBDError.CreateFmt('BLE open failed: %s', [E.Message]);
    end;
  end;

  SetState(csOpen);
end;

procedure TOBDBLETransport.Close;
begin
  FLock.Enter;
  try
    if FState in [csClosed, csClosing] then Exit;
    SetState(csClosing);
    if Assigned(FDevice) and Assigned(FNotifyChar) then
      try
        FDevice.SetCharacteristicNotification(FNotifyChar, False);
      except
      end;
    FDevice := nil;
    FNotifyChar := nil;
    FWriteChar := nil;
    FService := nil;
  finally
    FLock.Leave;
  end;
  SetState(csClosed);
end;

function TOBDBLETransport.IsOpen: Boolean;
begin
  Result := State = csOpen;
end;

function TOBDBLETransport.State: TOBDConnectionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

function TOBDBLETransport.WriteBytes(const ABytes: TBytes): Integer;
begin
  if not IsOpen then
    raise EOBDNotConnected.Create('BLE transport is not open');
  if Length(ABytes) = 0 then
    Exit(0);
  try
    FWriteChar.SetValue(ABytes);
    if not FDevice.WriteCharacteristic(FWriteChar) then
    begin
      HandleError(oeIO, 'GATT write rejected');
      Exit(0);
    end;
    Result := Length(ABytes);
  except
    on E: Exception do
    begin
      HandleError(oeIO, E.Message);
      Result := 0;
    end;
  end;
end;

function TOBDBLETransport.GetOnDataReceived: TOBDBytesEvent;
begin Result := FOnDataReceived; end;
procedure TOBDBLETransport.SetOnDataReceived(const AValue: TOBDBytesEvent);
begin FOnDataReceived := AValue; end;
function TOBDBLETransport.GetOnStateChanged: TOBDStateEvent;
begin Result := FOnStateChanged; end;
procedure TOBDBLETransport.SetOnStateChanged(const AValue: TOBDStateEvent);
begin FOnStateChanged := AValue; end;
function TOBDBLETransport.GetOnTransportError: TOBDTransportErrorEvent;
begin Result := FOnTransportError; end;
procedure TOBDBLETransport.SetOnTransportError(
  const AValue: TOBDTransportErrorEvent);
begin FOnTransportError := AValue; end;

end.
