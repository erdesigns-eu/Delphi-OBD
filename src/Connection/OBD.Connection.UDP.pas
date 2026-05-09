//------------------------------------------------------------------------------
//  OBD.Connection.UDP
//
//  UDP transport. Primary use cases: DoIP UDP discovery (ISO 13400-2)
//  and broadcast vehicle-announcement messages.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//------------------------------------------------------------------------------

unit OBD.Connection.UDP;

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
  TOBDUDPTransport = class;

  /// <summary>UDP receive loop.</summary>
  /// <remarks>Owned by the parent transport.</remarks>
  TOBDUDPReadThread = class(TThread)
  strict private
    FSocket: TSocket;
    FOnBytes: TProc<TBytes>;
    FOnError: TProc<TOBDErrorCode, string>;
  protected
    /// <summary>Datagram receive loop.</summary>
    procedure Execute; override;
  public
    /// <summary>Spawns the thread bound to a UDP socket.</summary>
    /// <param name="ASocket">UDP <c>TSocket</c>. Owned by caller.</param>
    /// <param name="AOnBytes">Inbound-bytes callback. Required.</param>
    /// <param name="AOnError">Error callback. Optional.</param>
    constructor Create(ASocket: TSocket;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>UDP transport.</summary>
  /// <remarks>UDP is connectionless; "open" means "ready to send and
  /// receive datagrams."</remarks>
  TOBDUDPTransport = class(TInterfacedObject, IOBDConnectionTransport)
  strict private
    FLock: TCriticalSection;
    FSocket: TSocket;
    FRemote: TNetEndpoint;
    FState: TOBDConnectionState;
    FReader: TOBDUDPReadThread;
    FOnDataReceived: TOBDBytesEvent;
    FOnStateChanged: TOBDStateEvent;
    FOnTransportError: TOBDTransportErrorEvent;
    procedure SetState(ANewState: TOBDConnectionState);
    procedure HandleBytes(const ABytes: TBytes);
    procedure HandleError(ACode: TOBDErrorCode; const AMessage: string);
  public
    /// <summary>Constructs an idle UDP transport.</summary>
    constructor Create;
    /// <summary>Closes the socket and releases the lock.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Creates the UDP socket, optionally binds a local port, and
    ///   stores the remote endpoint for subsequent
    ///   <see cref="WriteBytes"/> calls.
    /// </summary>
    /// <param name="ASettings">Host, port, bind / broadcast flags.</param>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c>.</exception>
    /// <exception cref="EOBDError">Bind or DNS lookup failed.</exception>
    procedure Open(const ASettings: TOBDUDPSettings);

    /// <summary>Closes the socket and joins the read thread.</summary>
    procedure Close;
    /// <summary>True when the socket is bound.</summary>
    /// <returns><c>True</c> if state is <c>csOpen</c>.</returns>
    function IsOpen: Boolean;
    /// <summary>Current lifecycle state.</summary>
    /// <returns>State enum.</returns>
    function State: TOBDConnectionState;
    /// <summary>Sends a datagram to the configured remote endpoint.</summary>
    /// <param name="ABytes">Datagram payload.</param>
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

{ ---- TOBDUDPReadThread ------------------------------------------------------- }

constructor TOBDUDPReadThread.Create(ASocket: TSocket;
  const AOnBytes: TProc<TBytes>;
  const AOnError: TProc<TOBDErrorCode, string>);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FSocket := ASocket;
  FOnBytes := AOnBytes;
  FOnError := AOnError;
end;

procedure TOBDUDPReadThread.Execute;
const
  ChunkSize = 2048; // UDP datagram max for typical use
var
  Buffer: TBytes;
  Got: Integer;
  Origin: TNetEndpoint;
begin
  SetLength(Buffer, ChunkSize);
  while not Terminated do
  begin
    try
      Got := FSocket.ReceiveFrom(Buffer, Origin, [], ChunkSize);
      if Terminated then Break;
      if Got > 0 then
      begin
        if Assigned(FOnBytes) then
          FOnBytes(Copy(Buffer, 0, Got));
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

{ ---- TOBDUDPTransport -------------------------------------------------------- }

constructor TOBDUDPTransport.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FState := csClosed;
end;

destructor TOBDUDPTransport.Destroy;
begin
  Close;
  FLock.Free;
  inherited;
end;

procedure TOBDUDPTransport.SetState(ANewState: TOBDConnectionState);
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

procedure TOBDUDPTransport.HandleBytes(const ABytes: TBytes);
var
  Handler: TOBDBytesEvent;
begin
  Handler := FOnDataReceived;
  if Assigned(Handler) then
    Handler(Self, ABytes);
end;

procedure TOBDUDPTransport.HandleError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Handler: TOBDTransportErrorEvent;
begin
  Handler := FOnTransportError;
  if Assigned(Handler) then
    Handler(Self, ACode, AMessage);
  SetState(csError);
end;

procedure TOBDUDPTransport.Open(const ASettings: TOBDUDPSettings);
var
  Local: TNetEndpoint;
begin
  if ASettings = nil then
    raise EOBDConfig.Create('UDP settings are nil');

  SetState(csOpening);
  FSocket := TSocket.Create(TSocketType.UDP, TEncoding.ASCII);
  try
    if ASettings.BindLocal then
    begin
      Local := TNetEndpoint.Create(TIPAddress.Any.IPv4Address,
        ASettings.LocalPort);
      FSocket.Bind(Local);
    end;
    if ASettings.Broadcast then
      FSocket.SetSocketOpt(TSocketOption.Broadcast, 1);
    if Trim(ASettings.Host) <> '' then
      FRemote := TNetEndpoint.Create(
        TIPAddress.LookupName(ASettings.Host), ASettings.Port)
    else
      FRemote := TNetEndpoint.Create(TIPAddress.Any.IPv4Address,
        ASettings.Port);
  except
    on E: Exception do
    begin
      FreeAndNil(FSocket);
      SetState(csError);
      raise EOBDError.CreateFmt('UDP open failed: %s', [E.Message]);
    end;
  end;

  FReader := TOBDUDPReadThread.Create(FSocket,
    procedure(const Bytes: TBytes) begin HandleBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin HandleError(Code, Msg); end);

  SetState(csOpen);
end;

procedure TOBDUDPTransport.Close;
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

function TOBDUDPTransport.IsOpen: Boolean;
begin
  Result := State = csOpen;
end;

function TOBDUDPTransport.State: TOBDConnectionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

function TOBDUDPTransport.WriteBytes(const ABytes: TBytes): Integer;
begin
  if not IsOpen then
    raise EOBDNotConnected.Create('UDP transport is not open');
  if Length(ABytes) = 0 then
    Exit(0);
  try
    Result := FSocket.SendTo(FRemote, ABytes);
  except
    on E: Exception do
    begin
      HandleError(oeIO, E.Message);
      Result := 0;
    end;
  end;
end;

function TOBDUDPTransport.GetOnDataReceived: TOBDBytesEvent;
begin Result := FOnDataReceived; end;
procedure TOBDUDPTransport.SetOnDataReceived(const AValue: TOBDBytesEvent);
begin FOnDataReceived := AValue; end;
function TOBDUDPTransport.GetOnStateChanged: TOBDStateEvent;
begin Result := FOnStateChanged; end;
procedure TOBDUDPTransport.SetOnStateChanged(const AValue: TOBDStateEvent);
begin FOnStateChanged := AValue; end;
function TOBDUDPTransport.GetOnTransportError: TOBDTransportErrorEvent;
begin Result := FOnTransportError; end;
procedure TOBDUDPTransport.SetOnTransportError(
  const AValue: TOBDTransportErrorEvent);
begin FOnTransportError := AValue; end;

end.
