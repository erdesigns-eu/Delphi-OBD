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
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
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
  OBD.Connection.Settings;

type
  TOBDBluetoothTransport = class;

  /// <summary>Read loop for the Bluetooth Classic socket.</summary>
  /// <remarks>Owned by the parent transport.</remarks>
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
    /// <summary>Spawns the thread bound to a connected RFCOMM socket.</summary>
    /// <param name="ASocket">Connected <c>TBluetoothSocket</c>.</param>
    /// <param name="AOnBytes">Inbound-bytes callback. Required.</param>
    /// <param name="AOnError">Error callback. Optional.</param>
    constructor Create(ASocket: TBluetoothSocket;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>Bluetooth Classic / RFCOMM transport.</summary>
  TOBDBluetoothTransport = class(TInterfacedObject, IOBDConnectionTransport)
  strict private
    FLock: TCriticalSection;
    FManager: TBluetoothManager;
    FDevice: TBluetoothDevice;
    FSocket: TBluetoothSocket;
    FState: TOBDConnectionState;
    FReader: TOBDBluetoothReadThread;
    FOnDataReceived: TOBDBytesEvent;
    FOnStateChanged: TOBDStateEvent;
    FOnTransportError: TOBDTransportErrorEvent;
    procedure SetState(ANewState: TOBDConnectionState);
    procedure HandleBytes(const ABytes: TBytes);
    procedure HandleError(ACode: TOBDErrorCode; const AMessage: string);
    function FindDevice(const AAddress: string): TBluetoothDevice;
  public
    /// <summary>Constructs an idle Bluetooth transport.</summary>
    constructor Create;
    /// <summary>Closes the socket if open and releases the lock.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Locates the paired device by address or name, opens an RFCOMM
    ///   client socket against the configured service UUID, and starts
    ///   the read thread.
    /// </summary>
    /// <param name="ASettings">Device address + service UUID +
    /// connect timeout. <c>DeviceAddress</c> and <c>ServiceUUID</c>
    /// must be non-empty.</param>
    /// <remarks>
    ///   The device must already be paired with the host OS — pairing
    ///   is outside the scope of this package.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c>,
    /// device address empty, or service UUID empty.</exception>
    /// <exception cref="EOBDError">Adapter unavailable, device not
    /// paired, or RFCOMM connect refused.</exception>
    procedure Open(const ASettings: TOBDBluetoothSettings);

    /// <summary>Closes the socket and joins the read thread.</summary>
    procedure Close;
    /// <summary>True when the RFCOMM socket is open.</summary>
    /// <returns><c>True</c> if state is <c>csOpen</c>.</returns>
    function IsOpen: Boolean;
    /// <summary>Current lifecycle state.</summary>
    /// <returns>State enum.</returns>
    function State: TOBDConnectionState;
    /// <summary>Sends bytes through the RFCOMM socket.</summary>
    /// <param name="ABytes">Bytes to send.</param>
    /// <returns>Always <c>Length(ABytes)</c> on success; 0 on transport
    /// error.</returns>
    /// <exception cref="EOBDNotConnected">Socket not open.</exception>
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
      Got := FSocket.ReceiveData; // blocks
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
  FLock := TCriticalSection.Create;
  FState := csClosed;
end;

destructor TOBDBluetoothTransport.Destroy;
begin
  Close;
  FLock.Free;
  inherited;
end;

procedure TOBDBluetoothTransport.SetState(ANewState: TOBDConnectionState);
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

procedure TOBDBluetoothTransport.HandleBytes(const ABytes: TBytes);
var
  Handler: TOBDBytesEvent;
begin
  Handler := FOnDataReceived;
  if Assigned(Handler) then
    Handler(Self, ABytes);
end;

procedure TOBDBluetoothTransport.HandleError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Handler: TOBDTransportErrorEvent;
begin
  Handler := FOnTransportError;
  if Assigned(Handler) then
    Handler(Self, ACode, AMessage);
  SetState(csError);
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
    FManager := TBluetoothManager.Current;
    if (FManager = nil) or not FManager.CurrentAdapter.Activated then
      raise EOBDError.Create('Bluetooth adapter is not available or activated');
    FDevice := FindDevice(ASettings.DeviceAddress);
    if FDevice = nil then
      raise EOBDError.CreateFmt(
        'Bluetooth device "%s" is not paired with this host',
        [ASettings.DeviceAddress]);
    ServiceGUID := StringToGUID('{' + ASettings.ServiceUUID + '}');
    FSocket := FDevice.CreateClientSocket(ServiceGUID, False);
    if FSocket = nil then
      raise EOBDError.Create('Failed to create RFCOMM client socket');
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
    procedure(const Bytes: TBytes) begin HandleBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin HandleError(Code, Msg); end);

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
  FDevice := nil; // not owned

  SetState(csClosed);
end;

function TOBDBluetoothTransport.IsOpen: Boolean;
begin
  Result := State = csOpen;
end;

function TOBDBluetoothTransport.State: TOBDConnectionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
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
      HandleError(oeIO, E.Message);
      Result := 0;
    end;
  end;
end;

function TOBDBluetoothTransport.GetOnDataReceived: TOBDBytesEvent;
begin Result := FOnDataReceived; end;
procedure TOBDBluetoothTransport.SetOnDataReceived(const AValue: TOBDBytesEvent);
begin FOnDataReceived := AValue; end;
function TOBDBluetoothTransport.GetOnStateChanged: TOBDStateEvent;
begin Result := FOnStateChanged; end;
procedure TOBDBluetoothTransport.SetOnStateChanged(const AValue: TOBDStateEvent);
begin FOnStateChanged := AValue; end;
function TOBDBluetoothTransport.GetOnTransportError: TOBDTransportErrorEvent;
begin Result := FOnTransportError; end;
procedure TOBDBluetoothTransport.SetOnTransportError(
  const AValue: TOBDTransportErrorEvent);
begin FOnTransportError := AValue; end;

end.
