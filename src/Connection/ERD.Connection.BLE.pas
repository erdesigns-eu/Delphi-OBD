// ------------------------------------------------------------------------------
// ERD.Connection.BLE
//
// Bluetooth Low Energy (GATT) transport for ELM327-BLE clones and
// similar adapters. Default profile FFE0 / FFE1 (write + notify on
// the same characteristic). Service / characteristic UUIDs are
// overridable via TOBDBLESettings.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-05-09  ERD  Initial implementation.
// 2026-05-09  ERD  Follow-up: rebased onto TOBDBaseTransport
// and instrumented with step-progress events.
// 2026-10-09  ERD  Use Delphi LE discovery and service characteristics.
//
// Future work :
// - Connection-parameter tuning (interval / latency) for chips that
// support it.
// - Pairing-required adapters (e.g. some Nordic UART variants) via
// OnTransportError.
// ------------------------------------------------------------------------------

unit ERD.Connection.BLE;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  System.Bluetooth,
  Winapi.Windows,
  ERD.Types,
  ERD.Connection.Types,
  ERD.Connection.Settings,
  ERD.Connection.Transport.Base;

type
  /// <summary>Device discovery completion callback used by the RTL manager.</summary>
  TOBDBLEDiscoveryEnd = procedure(const Sender: TObject;
    const ADevices: TBluetoothLEDeviceList) of object;
  /// <summary>Service discovery completion callback used by the RTL device.</summary>
  TOBDBLEServicesDiscovered = procedure(const Sender: TObject;
    const AServices: TBluetoothGattServiceList) of object;

  /// <summary>Characteristic read callback used by the RTL device.</summary>
  TOBDBLECharacteristicRead = procedure(const Sender: TObject;
    const ACharacteristic: TBluetoothGattCharacteristic;
    AGattStatus: TBluetoothGattStatus) of object;

  /// <summary>Bluetooth Low Energy transport.</summary>
  /// <remarks>
  /// The BLE manager dispatches its events on the manager's own
  /// thread; we forward them to the parent transport's callbacks.
  /// </remarks>
  TOBDBLETransport = class(TOBDBaseTransport)
  strict private
    FManager: TBluetoothLEManager;
    FDevice: TBluetoothLEDevice;
    FService: TBluetoothGattService;
    FWriteChar: TBluetoothGattCharacteristic;
    FNotifyChar: TBluetoothGattCharacteristic;
    FDiscoveryDone: TEvent;
    FPreviousCharRead: TOBDBLECharacteristicRead;
    FReadHandlerInstalled: Boolean;
    procedure HandleDiscoveryEnd(const Sender: TObject;
      const ADevices: TBluetoothLEDeviceList);
    procedure HandleServicesDiscovered(const Sender: TObject;
      const AServices: TBluetoothGattServiceList);
    procedure WaitForDiscovery(const ADeadline: UInt64);
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
    /// Connects to the configured BLE device, locates GATT
    /// characteristics, and subscribes to notifications.
    /// </summary>
    /// <param name="ASettings">Device address, service UUID, write
    /// and notify characteristic UUIDs.</param>
    /// <remarks>
    /// Synchronous. Fires six step-progress events:
    /// <c>1/6 Adapter check</c>, <c>2/6 Locating device</c>,
    /// <c>3/6 Connecting</c>, <c>4/6 Discovering service</c>,
    /// <c>5/6 Subscribing notifications</c>, <c>6/6 Ready</c>.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c> or
    /// device address is empty, or connect timeout is zero.</exception>
    /// <exception cref="EOBDError">Manager unavailable, device not
    /// discovered, service / characteristic missing, or notification
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
  FDiscoveryDone := TEvent.Create(nil, True, False, '');
end;

destructor TOBDBLETransport.Destroy;
begin
  Close;
  FDiscoveryDone.Free;
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
    S := Format('%s-0000-1000-8000-00805F9B34FB', [S.PadLeft(8, '0')]);
  if (Length(S) > 0) and (S[1] <> '{') then
    S := '{' + S + '}';
  Result := StringToGUID(S);
end;

procedure TOBDBLETransport.HandleDiscoveryEnd(const Sender: TObject;
  const ADevices: TBluetoothLEDeviceList);
begin
  FDiscoveryDone.SetEvent;
end;

procedure TOBDBLETransport.HandleServicesDiscovered(const Sender: TObject;
  const AServices: TBluetoothGattServiceList);
begin
  FDiscoveryDone.SetEvent;
end;

procedure TOBDBLETransport.WaitForDiscovery(const ADeadline: UInt64);
begin
  while FDiscoveryDone.WaitFor(10) <> wrSignaled do
  begin
    if GetTickCount64 >= ADeadline then
      raise EOBDError.Create('BLE discovery timed out');
    // RTL discovery can dispatch through the main thread. Do not block it
    // while servicing a synchronous Open call from a VCL application.
    if TThread.CurrentThread.ThreadID = MainThreadID then
      CheckSynchronize(0);
  end;
end;

procedure TOBDBLETransport.HandleCharRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
begin
  if (FState <> csOpen) or (ACharacteristic <> FNotifyChar) or
    (ACharacteristic = nil) then
    Exit;
  if AGattStatus = TBluetoothGattStatus.Success then
  begin
    if Length(ACharacteristic.Value) > 0 then
      FireBytes(ACharacteristic.Value);
  end
  else
    FireError(oeIO, Format('GATT read failed (status %d)', [Ord(AGattStatus)]));
end;

procedure TOBDBLETransport.Open(const ASettings: TOBDBLESettings);
var
  ServiceGuid, WriteGuid, NotifyGuid: TGUID;
  Devices: TBluetoothLEDeviceList;
  D: TBluetoothLEDevice;
  Needle: string;
  Characteristic: TBluetoothGattCharacteristic;
  PreviousDiscoveryEnd: TOBDBLEDiscoveryEnd;
  PreviousServicesDiscovered: TOBDBLEServicesDiscovered;
  Deadline: UInt64;
  ScanTime: Cardinal;
begin
  if ASettings = nil then
    raise EOBDConfig.Create('BLE settings are nil');
  if Trim(ASettings.DeviceAddress) = '' then
    raise EOBDConfig.Create('BLE device address is empty');
  if ASettings.ConnectTimeout = 0 then
    raise EOBDConfig.Create('BLE connect timeout must be greater than zero');
  Close;

  ServiceGuid := NormaliseUUID(ASettings.ServiceUUID);
  WriteGuid := NormaliseUUID(ASettings.WriteCharUUID);
  NotifyGuid := NormaliseUUID(ASettings.NotifyCharUUID);

  SetState(csOpening);
  try
    FireProgress(1, 6, 'Adapter check', '');
    FManager := TBluetoothLEManager.Current;
    if FManager = nil then
      raise EOBDError.Create('No BLE manager available on this host');

    FireProgress(2, 6, 'Locating device', ASettings.DeviceAddress);
    Deadline := GetTickCount64 + UInt64(ASettings.ConnectTimeout);
    ScanTime := ASettings.ConnectTimeout div 2;
    if ScanTime = 0 then
      ScanTime := 1;
    if ScanTime > 5000 then
      ScanTime := 5000;
    FDiscoveryDone.ResetEvent;
    PreviousDiscoveryEnd := FManager.OnDiscoveryEnd;
    FManager.OnDiscoveryEnd := HandleDiscoveryEnd;
    try
      FManager.StartDiscovery(ScanTime);
      try
        WaitForDiscovery(Deadline);
      except
        FManager.CancelDiscovery;
        raise;
      end;
    finally
      FManager.OnDiscoveryEnd := PreviousDiscoveryEnd;
    end;
    Devices := FManager.LastDiscoveredDevices;
    if Devices = nil then
      raise EOBDError.Create('No BLE devices discovered');
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
      raise EOBDError.CreateFmt
        ('BLE device "%s" not found among discovered devices',
        [ASettings.DeviceAddress]);

    FireProgress(3, 6, 'Connecting', '');
    FDiscoveryDone.ResetEvent;
    PreviousServicesDiscovered := FDevice.OnServicesDiscovered;
    FDevice.OnServicesDiscovered := HandleServicesDiscovered;
    try
      if not FDevice.DiscoverServices then
        raise EOBDError.Create('Failed to start BLE service discovery');
      WaitForDiscovery(Deadline);
    finally
      FDevice.OnServicesDiscovered := PreviousServicesDiscovered;
    end;
    FService := FDevice.GetService(ServiceGuid);
    if FService = nil then
      raise EOBDError.CreateFmt('BLE service %s not found',
        [GUIDToString(ServiceGuid)]);

    FireProgress(4, 6, 'Discovering service', ASettings.ServiceUUID);
    FWriteChar := nil;
    FNotifyChar := nil;
    for Characteristic in FService.Characteristics do
    begin
      if GUIDToString(Characteristic.UUID) = GUIDToString(WriteGuid) then
        FWriteChar := Characteristic;
      if GUIDToString(Characteristic.UUID) = GUIDToString(NotifyGuid) then
        FNotifyChar := Characteristic;
    end;
    if FWriteChar = nil then
      raise EOBDError.CreateFmt('Write characteristic %s not found',
        [GUIDToString(WriteGuid)]);
    if FNotifyChar = nil then
      raise EOBDError.CreateFmt('Notify characteristic %s not found',
        [GUIDToString(NotifyGuid)]);

    FireProgress(5, 6, 'Subscribing notifications', ASettings.NotifyCharUUID);
    FPreviousCharRead := FDevice.OnCharacteristicRead;
    FDevice.OnCharacteristicRead := HandleCharRead;
    FReadHandlerInstalled := True;
    if not FDevice.SetCharacteristicNotification(FNotifyChar, True) then
      raise EOBDError.Create
        ('Failed to enable notifications on the notify characteristic');
  except
    on E: Exception do
    begin
      Close;
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
    if FState in [csClosed, csClosing] then
      Exit;
    SetState(csClosing);
    if FReadHandlerInstalled and Assigned(FDevice) and Assigned(FNotifyChar)
    then
      try
        FDevice.SetCharacteristicNotification(FNotifyChar, False);
      except
      end;
    if FReadHandlerInstalled and Assigned(FDevice) then
      FDevice.OnCharacteristicRead := FPreviousCharRead;
    FReadHandlerInstalled := False;
    FPreviousCharRead := nil;
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
