// ------------------------------------------------------------------------------
// ERD.Connection.WiFi
//
// TCP transport for Wi-Fi and Ethernet adapters (e.g. ELM327 Wi-Fi
// clones at 192.168.0.10:35000, ESP-Link ECU bridges, DoIP TCP
// channels).
//
// Cross-platform via <c>ERD.Compat.Socket</c>.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-05-09  ERD  Initial implementation.
// 2026-05-09  ERD  Follow-up: rebased onto TOBDBaseTransport
// and instrumented with step-progress events.
// 2026-10-09  ERD  Route native socket options through the shared wrapper.
// ------------------------------------------------------------------------------

unit ERD.Connection.WiFi;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}ERD.Compat.Functions, {$ENDIF}
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  ERD.Compat.Socket,
  ERD.Types,
  ERD.Connection.Types,
  ERD.Connection.Settings,
  ERD.Connection.Transport.Base;

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
    constructor Create(ASocket: TSocket; const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>TCP transport (Wi-Fi / Ethernet).</summary>
  TOBDWiFiTransport = class(TOBDBaseTransport, IOBDTimedStreamTransport)
  strict private
    FSocket: TSocket;
    FReader: TOBDWiFiReadThread;
  public
    /// <summary>Constructs an idle TCP transport.</summary>
    constructor Create;
    procedure SetWriteTimeout(ATimeoutMs: Cardinal);
    /// <summary>Closes the socket if open.</summary>
    destructor Destroy; override;

    /// <summary>
    /// Resolves <c>ASettings.Host</c>, opens a TCP connection,
    /// starts the read thread.
    /// </summary>
    /// <param name="ASettings">Host, port, keep-alive, connect
    /// timeout.</param>
    /// <remarks>
    /// Synchronous. Fires three step-progress events:
    /// <c>1/3 Resolving host</c>, <c>2/3 Connecting</c>,
    /// <c>3/3 Ready</c>.
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
  const AOnBytes: TProc<TBytes>; const AOnError: TProc<TOBDErrorCode, string>);
begin
  inherited Create(True);
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
      Got := FSocket.Receive(Buffer, 0, Length(Buffer), []);
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

  Close;
  SetState(csOpening);
  FSocket := TSocket.Create(TSocketType.TCP, TEncoding.ASCII);
  try
    FireProgress(1, 3, 'Resolving host', ASettings.Host);
    ResolvedAddr := TIPAddress.LookupName(ASettings.Host);

    FireProgress(2, 3, 'Connecting',
      Format('%s:%d', [ASettings.Host, ASettings.Port]));
    Endpoint := TNetEndpoint.Create(ResolvedAddr, ASettings.Port);
    FSocket.ConnectWithTimeout(Endpoint, ASettings.ConnectTimeout);
    SetWriteTimeout(5000);
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
    procedure(Bytes: TBytes)
    begin
      FireBytes(Bytes);
    end,
    procedure(Code: TOBDErrorCode; Msg: string)
    begin
      FireError(Code, Msg);
    end);
  FReader.Start;

  FireProgress(3, 3, 'Ready', '');
  SetState(csOpen);
end;

procedure TOBDWiFiTransport.Close;
var
  Local: TSocket;
begin
  FLock.Enter;
  try
    if FState in [csClosed, csClosing] then
      Exit;
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
      try
        Local.Close;
      except
      end;
    FReader.WaitFor;
    FreeAndNil(FReader);
  end;

  if Assigned(Local) then
    Local.Free;

  SetState(csClosed);
end;

procedure TOBDWiFiTransport.SetWriteTimeout(ATimeoutMs: Cardinal);
begin
  if FSocket = nil then
    raise EOBDNotConnected.Create('Wi-Fi socket is closed');
  if ATimeoutMs = 0 then
    ATimeoutMs := 1;
  FSocket.SetSendTimeout(ATimeoutMs);
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
