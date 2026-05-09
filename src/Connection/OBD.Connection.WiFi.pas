//------------------------------------------------------------------------------
//  OBD.Connection.WiFi
//
//  TCP transport for Wi-Fi and Ethernet adapters (e.g. ELM327 Wi-Fi
//  clones at 192.168.0.10:35000, ESP-Link ECU bridges, DoIP TCP
//  channels).
//
//  Cross-platform via <c>System.Net.Socket</c>.
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

unit OBD.Connection.WiFi;

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
  /// <summary>Read loop for the Wi-Fi transport.</summary>
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
    /// <param name="ASocket">Connected <c>TSocket</c>.</param>
    /// <param name="AOnBytes">Inbound-bytes callback. Required.</param>
    /// <param name="AOnError">Error callback. Optional.</param>
    constructor Create(ASocket: TSocket;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>TCP transport (Wi-Fi / Ethernet).</summary>
  TOBDWiFiTransport = class(TOBDBaseTransport)
  strict private
    FSocket: TSocket;
    FReader: TOBDWiFiReadThread;
  public
    /// <summary>Constructs an idle TCP transport.</summary>
    constructor Create;
    /// <summary>Closes the socket if open.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Resolves <c>ASettings.Host</c>, opens a TCP connection,
    ///   starts the read thread.
    /// </summary>
    /// <param name="ASettings">Host, port, keep-alive, connect
    /// timeout.</param>
    /// <remarks>
    ///   Synchronous. Fires three step-progress events:
    ///   <c>1/3 Resolving host</c>, <c>2/3 Connecting</c>,
    ///   <c>3/3 Ready</c>.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c>,
    /// <c>Host</c> empty, or <c>Port</c> zero.</exception>
    /// <exception cref="EOBDError">Connect failed.</exception>
    procedure Open(const ASettings: TOBDWiFiSettings);

    /// <summary>Closes the socket and joins the read thread.</summary>
    procedure Close; override;
    /// <summary>Sends bytes via <c>TSocket.Send</c>.</summary>
    /// <param name="ABytes">Bytes to send.</param>
    /// <returns>Number of bytes the OS accepted.</returns>
    /// <exception cref="EOBDNotConnected">Socket not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer; override;
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
      Got := FSocket.Receive(Buffer, 0, Length(Buffer), [TSocketFlag.WAITALL]);
      if Got <= 0 then
      begin
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
end;

destructor TOBDWiFiTransport.Destroy;
begin
  Close;
  inherited;
end;

procedure TOBDWiFiTransport.Open(const ASettings: TOBDWiFiSettings);
var
  Endpoint: TNetEndpoint;
  ResolvedAddr: TIPAddress;
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
    FireProgress(1, 3, 'Resolving host', ASettings.Host);
    ResolvedAddr := TIPAddress.LookupName(ASettings.Host);

    FireProgress(2, 3, 'Connecting',
      Format('%s:%d', [ASettings.Host, ASettings.Port]));
    Endpoint := TNetEndpoint.Create(ResolvedAddr, ASettings.Port);
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
    procedure(const Bytes: TBytes) begin FireBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin FireError(Code, Msg); end);

  FireProgress(3, 3, 'Ready', '');
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
      FireError(oeIO, E.Message);
      Result := 0;
    end;
  end;
end;

end.
