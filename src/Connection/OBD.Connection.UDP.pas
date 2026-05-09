//------------------------------------------------------------------------------
//  OBD.Connection.UDP
//
//  UDP transport. Primary use cases: DoIP UDP discovery (ISO 13400-2)
//  and broadcast vehicle-announcement messages.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//    2026-05-09  ERD  Phase 2 follow-up: rebased onto TOBDBaseTransport
//                     and instrumented with step-progress events.
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
  OBD.Connection.Settings,
  OBD.Connection.Transport.Base;

type
  /// <summary>UDP receive loop.</summary>
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
    /// <param name="ASocket">UDP <c>TSocket</c>.</param>
    /// <param name="AOnBytes">Inbound-bytes callback. Required.</param>
    /// <param name="AOnError">Error callback. Optional.</param>
    constructor Create(ASocket: TSocket;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>UDP transport. Connectionless; "open" means "ready to
  /// send and receive datagrams."</summary>
  TOBDUDPTransport = class(TOBDBaseTransport)
  strict private
    FSocket: TSocket;
    FRemote: TNetEndpoint;
    FReader: TOBDUDPReadThread;
  public
    /// <summary>Constructs an idle UDP transport.</summary>
    constructor Create;
    /// <summary>Closes the socket if open.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Creates the UDP socket, optionally binds a local port, and
    ///   stores the remote endpoint.
    /// </summary>
    /// <param name="ASettings">Host, port, bind / broadcast flags.</param>
    /// <remarks>
    ///   Synchronous. Fires two step-progress events:
    ///   <c>1/2 Binding</c>, <c>2/2 Ready</c>.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c>.</exception>
    /// <exception cref="EOBDError">Bind or DNS lookup failed.</exception>
    procedure Open(const ASettings: TOBDUDPSettings);

    /// <summary>Closes the socket and joins the read thread.</summary>
    procedure Close; override;
    /// <summary>Sends a datagram to the configured remote endpoint.</summary>
    /// <param name="ABytes">Datagram payload.</param>
    /// <returns>Bytes the OS accepted.</returns>
    /// <exception cref="EOBDNotConnected">Socket not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer; override;
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
  ChunkSize = 2048;
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
end;

destructor TOBDUDPTransport.Destroy;
begin
  Close;
  inherited;
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
    FireProgress(1, 2, 'Binding',
      Format('local-port=%d', [ASettings.LocalPort]));
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
    procedure(const Bytes: TBytes) begin FireBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin FireError(Code, Msg); end);

  FireProgress(2, 2, 'Ready', '');
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
      FireError(oeIO, E.Message);
      Result := 0;
    end;
  end;
end;

end.
