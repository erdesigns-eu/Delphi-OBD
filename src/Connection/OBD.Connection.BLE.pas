//------------------------------------------------------------------------------
//  OBD.Connection.BLE
//
//  Bluetooth Low Energy (GATT) transport for ELM327-BLE clones and
//  similar adapters. Default profile FFE0 / FFE1 (write + notify on
//  the same characteristic). Service / characteristic UUIDs are
//  overridable via TOBDBLESettings.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//    2026-05-09  ERD  Phase 2 follow-up: rebased onto TOBDBaseTransport
//                     and instrumented with step-progress events.
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
  OBD.Connection.Settings,
  OBD.Connection.Transport.Base;

type
  /// <summary>Bluetooth Low Energy transport.</summary>
  /// <remarks>
  ///   The BLE manager dispatches its events on the manager's own
  ///   thread; we forward them to the parent transport's callbacks.
  /// </remarks>
  TOBDBLETransport = class(TOBDBaseTransport)
  strict private
    FManager: TBluetoothLEManager;
    FDevice: TBluetoothLEDevice;
    FService: TBluetoothGattService;
    FWriteChar: TBluetoothGattCharacteristic;
    FNotifyChar: TBluetoothGattCharacteristic;
    procedure HandleCharRead(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    function NormaliseUUID(const ARaw: string): TGUID;
  public
    /// <summary>Constructs an idle BLE transport.</summary>
    constructor Create;
    /// <summary>Disconnects if open.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connects to the configured BLE device, locates GATT
    ///   characteristics, and subscribes to notifications.
    /// </summary>
    /// <param name="ASettings">Device address, service UUID, write
    /// and notify characteristic UUIDs.</param>
    /// <remarks>
    ///   Synchronous. Fires six step-progress events:
    ///   <c>1/6 Adapter check</c>, <c>2/6 Locating device</c>,
    ///   <c>3/6 Connecting</c>, <c>4/6 Discovering service</c>,
    ///   <c>5/6 Subscribing notifications</c>, <c>6/6 Ready</c>.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c> or
    /// device address is empty.</exception>
    /// <exception cref="EOBDError">Manager unavailable, device not
    /// paired, service / characteristic missing, or notification
    /// subscribe failed.</exception>
    procedure Open(const ASettings: TOBDBLESettings);

    /// <summary>Disables notifications and disconnects.</summary>
    procedure Close; override;
    /// <summary>Writes bytes to the configured write
    /// characteristic.</summary>
    /// <param name="ABytes">Bytes to send.</param>
    /// <returns><c>Length(ABytes)</c> on success; 0 on transport
    /// error.</returns>
    /// <exception cref="EOBDNotConnected">Not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer; override;
  end;

implementation

{ ---- TOBDBLETransport -------------------------------------------------------- }

constructor TOBDBLETransport.Create;
begin
  inherited;
end;

destructor TOBDBLETransport.Destroy;
begin
  Close;
  inherited;
end;

function TOBDBLETransport.NormaliseUUID(const ARaw: string): TGUID;
var
  S: string;
begin
  S := Trim(ARaw);
  if S = '' then
    raise EOBDConfig.Create('UUID is empty');
  if (Length(S) = 4) or (Length(S) = 8) then
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
      FireBytes(ACharacteristic.Value);
  end
  else
    FireError(oeIO, Format('GATT read failed (status %d)',
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
    FireProgress(1, 6, 'Adapter check', '');
    FManager := TBluetoothLEManager.Current;
    if FManager = nil then
      raise EOBDError.Create('No BLE manager available on this host');

    FireProgress(2, 6, 'Locating device', ASettings.DeviceAddress);
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

    FireProgress(3, 6, 'Connecting', '');
    FService := FDevice.GetService(ServiceGuid);
    if FService = nil then
      raise EOBDError.CreateFmt('BLE service %s not found',
        [GUIDToString(ServiceGuid)]);

    FireProgress(4, 6, 'Discovering service', ASettings.ServiceUUID);
    FWriteChar := FDevice.GetCharacteristic(FService, WriteGuid);
    if FWriteChar = nil then
      raise EOBDError.CreateFmt('Write characteristic %s not found',
        [GUIDToString(WriteGuid)]);
    FNotifyChar := FDevice.GetCharacteristic(FService, NotifyGuid);
    if FNotifyChar = nil then
      raise EOBDError.CreateFmt('Notify characteristic %s not found',
        [GUIDToString(NotifyGuid)]);

    FireProgress(5, 6, 'Subscribing notifications', ASettings.NotifyCharUUID);
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

  FireProgress(6, 6, 'Ready', '');
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
      FireError(oeIO, 'GATT write rejected');
      Exit(0);
    end;
    Result := Length(ABytes);
  except
    on E: Exception do
    begin
      FireError(oeIO, E.Message);
      Result := 0;
    end;
  end;
end;

end.
