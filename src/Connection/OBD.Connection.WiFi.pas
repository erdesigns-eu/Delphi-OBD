//------------------------------------------------------------------------------
//  OBD.Connection.WiFi
//
//  TCP transport for Wi-Fi and Ethernet adapters (e.g. ELM327 Wi-Fi
//  clones at 192.168.0.10:35000, ESP-Link ECU bridges, DoIP TCP
//  channels).
//
//  Cross-platform via <c>System.Net.Socket</c> (available 10.3+).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//------------------------------------------------------------------------------

unit OBD.Connection.WiFi;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Net.Socket,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings;

type
  TOBDWiFiTransport = class;

  /// <summary>Read loop for the Wi-Fi transport.</summary>
  /// <remarks>Owned by the parent transport; joined inside
  /// <see cref="TOBDWiFiTransport.Close"/>.</remarks>
  TOBDWiFiReadThread = class(TThread)
  strict private
    FSocket: TSocket;
    FOnBytes: TProc<TBytes>;
    FOnError: TProc<TOBDErrorCode, string>;
  protected
    /// <summary>Read loop. Exits on socket close or
    /// <c>Terminated</c>.</summary>
    procedure Execute; override;
  public
    /// <summary>Spawns the thread bound to a connected TCP socket.</summary>
    /// <param name="ASocket">Connected <c>TSocket</c>. Owned by the
    /// caller.</param>
    /// <param name="AOnBytes">Callback for inbound bytes. Required.</param>
    /// <param name="AOnError">Callback for socket errors. Optional.</param>
    constructor Create(ASocket: TSocket;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>TCP transport (Wi-Fi / Ethernet).</summary>
  TOBDWiFiTransport = class(TInterfacedObject, IOBDConnectionTransport)
  strict private
    FLock: TCriticalSection;
    FSocket: TSocket;
    FState: TOBDConnectionState;
    FReader: TOBDWiFiReadThread;
    FOnDataReceived: TOBDBytesEvent;
    FOnStateChanged: TOBDStateEvent;
    FOnTransportError: TOBDTransportErrorEvent;
    procedure SetState(ANewState: TOBDConnectionState);
    procedure HandleBytes(const ABytes: TBytes);
    procedure HandleError(ACode: TOBDErrorCode; const AMessage: string);
  public
    /// <summary>Constructs an idle TCP transport.</summary>
    constructor Create;
    /// <summary>Closes the socket if open and releases the lock.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Resolves the host, opens a TCP connection to
    ///   <c>ASettings.Host:ASettings.Port</c>, and starts the read
    ///   thread.
    /// </summary>
    /// <param name="ASettings">Host, port, keep-alive flag, connect
    /// timeout. <c>Host</c> must be non-empty and <c>Port</c> non-zero.</param>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c>,
    /// <c>Host</c> is empty, or <c>Port</c> is zero.</exception>
    /// <exception cref="EOBDError">Connect failed (DNS, refused,
    /// timed out).</exception>
    procedure Open(const ASettings: TOBDWiFiSettings);

    /// <summary>Closes the socket and joins the read thread.</summary>
    procedure Close;
    /// <summary>True when the socket is connected.</summary>
    /// <returns><c>True</c> if state is <c>csOpen</c>.</returns>
    function IsOpen: Boolean;
    /// <summary>Current lifecycle state.</summary>
    /// <returns>State enum.</returns>
    function State: TOBDConnectionState;
    /// <summary>Sends bytes via <c>TSocket.Send</c>.</summary>
    /// <param name="ABytes">Bytes to send. Empty returns 0.</param>
    /// <returns>Number of bytes the OS accepted.</returns>
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

{ ---- TOBDWiFiReadThread ------------------------------------------------------ }

constructor TOBDWiFiReadThread.Create(ASocket: TSocket;
  const AOnBytes: TProc<TBytes>;
  const AOnError: TProc<TOBDErrorCode, string>);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FSocket := ASocket;
  FOnBytes := AOnBytes;
  FOnError := AOnError;
end;

procedure TOBDWiFiReadThread.Execute;
const
  ChunkSize = 1024;
var
  Buffer: TBytes;
  Got: Integer;
begin
  SetLength(Buffer, ChunkSize);
  while not Terminated do
  begin
    try
      // ReceiveLength reads up to Length(Buffer) bytes; non-blocking
      // to allow the Terminated check on socket close.
      Got := FSocket.Receive(Buffer, 0, Length(Buffer), [TSocketFlag.WAITALL]);
      if Got <= 0 then
      begin
        // Peer closed or socket shut down.
        if not Terminated and Assigned(FOnError) then
          FOnError(oeIO, 'Peer closed connection');
        Break;
      end;
      if Assigned(FOnBytes) then
        FOnBytes(Copy(Buffer, 0, Got));
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

{ ---- TOBDWiFiTransport ------------------------------------------------------- }

constructor TOBDWiFiTransport.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FState := csClosed;
end;

destructor TOBDWiFiTransport.Destroy;
begin
  Close;
  FLock.Free;
  inherited;
end;

procedure TOBDWiFiTransport.SetState(ANewState: TOBDConnectionState);
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

procedure TOBDWiFiTransport.HandleBytes(const ABytes: TBytes);
var
  Handler: TOBDBytesEvent;
begin
  Handler := FOnDataReceived;
  if Assigned(Handler) then
    Handler(Self, ABytes);
end;

procedure TOBDWiFiTransport.HandleError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Handler: TOBDTransportErrorEvent;
begin
  Handler := FOnTransportError;
  if Assigned(Handler) then
    Handler(Self, ACode, AMessage);
  SetState(csError);
end;

procedure TOBDWiFiTransport.Open(const ASettings: TOBDWiFiSettings);
var
  Endpoint: TNetEndpoint;
begin
  if ASettings = nil then
    raise EOBDConfig.Create('Wi-Fi settings are nil');
  if Trim(ASettings.Host) = '' then
    raise EOBDConfig.Create('Wi-Fi host is empty');
  if ASettings.Port = 0 then
    raise EOBDConfig.Create('Wi-Fi port is zero');

  SetState(csOpening);
  FSocket := TSocket.Create(TSocketType.TCP, TEncoding.ASCII);
  try
    Endpoint := TNetEndpoint.Create(
      TIPAddress.LookupName(ASettings.Host), ASettings.Port);
    FSocket.Connect(Endpoint);
    if ASettings.KeepAlive then
      FSocket.SetKeepAlive(True);
  except
    on E: Exception do
    begin
      FreeAndNil(FSocket);
      SetState(csError);
      raise EOBDError.CreateFmt('TCP connect to %s:%d failed: %s',
        [ASettings.Host, ASettings.Port, E.Message]);
    end;
  end;

  FReader := TOBDWiFiReadThread.Create(FSocket,
    procedure(const Bytes: TBytes) begin HandleBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin HandleError(Code, Msg); end);

  SetState(csOpen);
end;

procedure TOBDWiFiTransport.Close;
var
  Local: TSocket;
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

  SetState(csClosed);
end;

function TOBDWiFiTransport.IsOpen: Boolean;
begin
  Result := State = csOpen;
end;

function TOBDWiFiTransport.State: TOBDConnectionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

function TOBDWiFiTransport.WriteBytes(const ABytes: TBytes): Integer;
begin
  if not IsOpen then
    raise EOBDNotConnected.Create('Wi-Fi transport is not open');
  if Length(ABytes) = 0 then
    Exit(0);
  try
    Result := FSocket.Send(ABytes);
  except
    on E: Exception do
    begin
      HandleError(oeIO, E.Message);
      Result := 0;
    end;
  end;
end;

function TOBDWiFiTransport.GetOnDataReceived: TOBDBytesEvent;
begin Result := FOnDataReceived; end;
procedure TOBDWiFiTransport.SetOnDataReceived(const AValue: TOBDBytesEvent);
begin FOnDataReceived := AValue; end;
function TOBDWiFiTransport.GetOnStateChanged: TOBDStateEvent;
begin Result := FOnStateChanged; end;
procedure TOBDWiFiTransport.SetOnStateChanged(const AValue: TOBDStateEvent);
begin FOnStateChanged := AValue; end;
function TOBDWiFiTransport.GetOnTransportError: TOBDTransportErrorEvent;
begin Result := FOnTransportError; end;
procedure TOBDWiFiTransport.SetOnTransportError(
  const AValue: TOBDTransportErrorEvent);
begin FOnTransportError := AValue; end;

end.
