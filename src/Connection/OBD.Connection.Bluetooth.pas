//------------------------------------------------------------------------------
//  OBD.Connection.Bluetooth
//
//  Bluetooth Classic (RFCOMM / SPP) transport via System.Bluetooth.
//
//  The Bluetooth shared <c>TBluetoothManager</c> is reference-counted
//  internally by the RTL; this transport claims a reference on Open
//  and releases it on Close. SPP service UUID defaults to the canonical
//  <c>00001101-0000-1000-8000-00805F9B34FB</c>; ELM327 BT clones use
//  the same UUID.
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
//    - Pairing / passkey events surfaced through OnTransportError so
//      apps can prompt without coupling to System.Bluetooth.
//------------------------------------------------------------------------------

unit OBD.Connection.Bluetooth;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Bluetooth,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings,
  OBD.Connection.Transport.Base;

type
  /// <summary>Read loop for the Bluetooth Classic socket.</summary>
  TOBDBluetoothReadThread = class(TThread)
  strict private
    FSocket: TBluetoothSocket;
    FOnBytes: TProc<TBytes>;
    FOnError: TProc<TOBDErrorCode, string>;
  protected
    /// <summary>Read loop. Exits on socket close or
    /// <c>Terminated</c>.</summary>
    procedure Execute; override;
  public
    /// <summary>Spawns the thread bound to a connected RFCOMM
    /// socket.</summary>
    /// <param name="ASocket">Connected <c>TBluetoothSocket</c>.</param>
    /// <param name="AOnBytes">Inbound-bytes callback. Required.</param>
    /// <param name="AOnError">Error callback. Optional.</param>
    constructor Create(ASocket: TBluetoothSocket;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>Bluetooth Classic / RFCOMM transport.</summary>
  TOBDBluetoothTransport = class(TOBDBaseTransport)
  strict private
    FManager: TBluetoothManager;
    FDevice: TBluetoothDevice;
    FSocket: TBluetoothSocket;
    FReader: TOBDBluetoothReadThread;
    function FindDevice(const AAddress: string): TBluetoothDevice;
  public
    /// <summary>Constructs an idle Bluetooth transport.</summary>
    constructor Create;
    /// <summary>Closes the socket if open.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Locates the paired device and opens an RFCOMM client socket
    ///   against the configured service UUID.
    /// </summary>
    /// <param name="ASettings">Device address + service UUID +
    /// connect timeout.</param>
    /// <remarks>
    ///   Synchronous. Fires five step-progress events:
    ///   <c>1/5 Adapter check</c>, <c>2/5 Locating device</c>,
    ///   <c>3/5 Creating socket</c>, <c>4/5 Connecting</c>,
    ///   <c>5/5 Ready</c>. The device must already be paired with
    ///   the host OS.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c>,
    /// device address empty, or service UUID empty.</exception>
    /// <exception cref="EOBDError">Adapter unavailable, device not
    /// paired, or RFCOMM connect refused.</exception>
    procedure Open(const ASettings: TOBDBluetoothSettings);

    /// <summary>Closes the socket and joins the read thread.</summary>
    procedure Close; override;
    /// <summary>Sends bytes through the RFCOMM socket.</summary>
    /// <param name="ABytes">Bytes to send.</param>
    /// <returns><c>Length(ABytes)</c> on success; 0 on transport
    /// error.</returns>
    /// <exception cref="EOBDNotConnected">Socket not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer; override;
  end;

implementation

{ ---- TOBDBluetoothReadThread ------------------------------------------------- }

constructor TOBDBluetoothReadThread.Create(ASocket: TBluetoothSocket;
  const AOnBytes: TProc<TBytes>;
  const AOnError: TProc<TOBDErrorCode, string>);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FSocket := ASocket;
  FOnBytes := AOnBytes;
  FOnError := AOnError;
end;

procedure TOBDBluetoothReadThread.Execute;
var
  Got: TBytes;
begin
  while not Terminated do
  begin
    try
      Got := FSocket.ReceiveData;
      if Length(Got) > 0 then
      begin
        if Assigned(FOnBytes) then
          FOnBytes(Got);
      end;
    except
      on E: Exception do
      begin
        if not Terminated and Assigned(FOnError) then
          FOnError(oeIO, E.Message);
        Break;
      end;
    end;
  end;
end;

{ ---- TOBDBluetoothTransport -------------------------------------------------- }

constructor TOBDBluetoothTransport.Create;
begin
  inherited;
end;

destructor TOBDBluetoothTransport.Destroy;
begin
  Close;
  inherited;
end;

function TOBDBluetoothTransport.FindDevice(
  const AAddress: string): TBluetoothDevice;
var
  Devices: TBluetoothDeviceList;
  D: TBluetoothDevice;
  Needle: string;
begin
  Result := nil;
  Devices := FManager.GetPairedDevices;
  if Devices = nil then Exit;
  Needle := UpperCase(Trim(AAddress));
  for D in Devices do
  begin
    if (UpperCase(D.Address) = Needle) or
       SameText(D.DeviceName, AAddress) then
      Exit(D);
  end;
end;

procedure TOBDBluetoothTransport.Open(const ASettings: TOBDBluetoothSettings);
var
  ServiceGUID: TGUID;
begin
  if ASettings = nil then
    raise EOBDConfig.Create('Bluetooth settings are nil');
  if Trim(ASettings.DeviceAddress) = '' then
    raise EOBDConfig.Create('Bluetooth device address is empty');
  if Trim(ASettings.ServiceUUID) = '' then
    raise EOBDConfig.Create('Bluetooth service UUID is empty');

  SetState(csOpening);
  try
    FireProgress(1, 5, 'Adapter check', '');
    FManager := TBluetoothManager.Current;
    if (FManager = nil) or not FManager.CurrentAdapter.Activated then
      raise EOBDError.Create('Bluetooth adapter is not available or activated');

    FireProgress(2, 5, 'Locating device', ASettings.DeviceAddress);
    FDevice := FindDevice(ASettings.DeviceAddress);
    if FDevice = nil then
      raise EOBDError.CreateFmt(
        'Bluetooth device "%s" is not paired with this host',
        [ASettings.DeviceAddress]);

    FireProgress(3, 5, 'Creating socket', ASettings.ServiceUUID);
    ServiceGUID := StringToGUID('{' + ASettings.ServiceUUID + '}');
    FSocket := FDevice.CreateClientSocket(ServiceGUID, False);
    if FSocket = nil then
      raise EOBDError.Create('Failed to create RFCOMM client socket');

    FireProgress(4, 5, 'Connecting', '');
    FSocket.Connect;
  except
    on E: Exception do
    begin
      FreeAndNil(FSocket);
      SetState(csError);
      raise EOBDError.CreateFmt('Bluetooth open failed: %s', [E.Message]);
    end;
  end;

  FReader := TOBDBluetoothReadThread.Create(FSocket,
    procedure(const Bytes: TBytes) begin FireBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin FireError(Code, Msg); end);

  FireProgress(5, 5, 'Ready', '');
  SetState(csOpen);
end;

procedure TOBDBluetoothTransport.Close;
var
  Local: TBluetoothSocket;
begin
  FLock.Enter;
  try
    if FState in [csClosed, csClosing] then Exit;
    SetState(csClosing);
    Local := FSocket;
    FSocket := nil;
  finally
    FLock.Leave;
  end;

  if Assigned(FReader) then
  begin
    FReader.Terminate;
    if Assigned(Local) then
      try Local.Close; except end;
    FReader.WaitFor;
    FreeAndNil(FReader);
  end;

  if Assigned(Local) then
    Local.Free;
  FDevice := nil;

  SetState(csClosed);
end;

function TOBDBluetoothTransport.WriteBytes(const ABytes: TBytes): Integer;
begin
  if not IsOpen then
    raise EOBDNotConnected.Create('Bluetooth transport is not open');
  if Length(ABytes) = 0 then
    Exit(0);
  try
    FSocket.SendData(ABytes);
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
