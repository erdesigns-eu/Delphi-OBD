//------------------------------------------------------------------------------
//  OBD.Adapter
//
//  TOBDAdapter — the second-tier non-visual component sitting between
//  TOBDConnection (transport) and TOBDProtocol (wire protocol). Handles:
//
//    - sending AT / ST / OBD commands and collecting their responses
//      against the ELM327 '>' prompt terminator,
//    - chip-family detection (Detect / DetectAsync),
//    - per-family initialisation (Init / InitAsync),
//    - capability resolution from the catalogue,
//    - event surface for response and identity changes.
//
//  Response collection runs on the connection's worker-thread raw hook
//  so sync calls do not deadlock when invoked from the main thread —
//  the producer (rx thread) is never the same as the consumer
//  (caller).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial: TOBDAdapter component, sync +
//                     async + progress for Detect / Init /
//                     WriteATCommand / WriteSTCommand /
//                     WriteOBDCommand.
//------------------------------------------------------------------------------

unit OBD.Adapter;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection,
  OBD.Adapter.Types,
  OBD.Adapter.Capabilities,
  OBD.Adapter.Detection,
  OBD.Adapter.Init;

const
  /// <summary>Default per-command timeout in milliseconds.</summary>
  OBD_ADAPTER_DEFAULT_TIMEOUT = 5000;

  /// <summary>ELM327 / OBDLink response terminator.</summary>
  OBD_ADAPTER_PROMPT = '>';

type
  /// <summary>
  ///   Non-visual adapter component bound to a <c>TOBDConnection</c>.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, point <c>Connection</c> at a configured
  ///   <c>TOBDConnection</c>, set <c>Family</c> to your adapter's
  ///   family (or leave <c>afELM327</c> and call <c>Detect</c>), then
  ///   call <c>Init</c> to run the family's standard initialisation.
  ///
  ///   <c>Detect</c>, <c>Init</c> and the various <c>Write*Command</c>
  ///   methods all ship in synchronous and asynchronous forms per the
  ///   dual-method rule (PLAN §3.7); the async forms fire
  ///   <c>OnReady</c> / <c>OnIdentityChanged</c> / <c>OnATResponse</c>
  ///   / <c>OnError</c> on the main thread.
  ///
  ///   Long-running ops fire <c>OnProgress</c> with named phases.
  /// </remarks>
  TOBDAdapter = class(TComponent, IOBDAdapterCommandSender)
  strict private
    FConnection: TOBDConnection;
    FFamily: TOBDAdapterFamily;
    FIdentity: TOBDAdapterIdentity;
    FCapabilities: TOBDAdapterCapabilities;
    FMaxIsoTpFrameBytes: Cardinal;
    FInitCommands: TStrings;
    FCommandTimeoutMs: Cardinal;
    FAutoSubscribed: Boolean;
    FInitialized: Boolean;
    FRunning: Boolean;

    // Response collector
    FRxLock: TCriticalSection;
    FRxBuffer: string;
    FRxComplete: TEvent;
    FCancelEvent: TEvent;
    FCurrentCommand: string;

    // Async worker
    FAsyncLock: TCriticalSection;
    FAsyncThread: TThread;

    // Events
    FOnReady: TNotifyEvent;
    FOnIdentityChanged: TOBDAdapterIdentityEvent;
    FOnATResponse: TOBDAdapterResponseEvent;
    FOnProgress: TOBDProgressEvent;
    FOnError: TOBDConnectionErrorEvent;

    procedure SetConnection(AValue: TOBDConnection);
    procedure SetInitCommands(AValue: TStrings);

    procedure HandleRawBytes(Sender: TObject; const ABytes: TBytes);

    function TerminatorIndex(const ABuffer: string): Integer;
    function ParseResponse(const ARaw: string;
      const ACommand: string; AElapsed: Cardinal): TOBDAdapterResponse;
    function BytesToWireString(const ABytes: TBytes): string;
    function StripLeadingEcho(const ABuffer, ACommand: string): string;

    procedure SubscribeIfNeeded;
    procedure UnsubscribeIfNeeded;
    procedure WaitForAsync;

    procedure FireProgress(AIndex, ACount: Cardinal;
      const AName, ADetail: string);
    procedure FireOnReady;
    procedure FireOnIdentityChanged(const AIdentity: TOBDAdapterIdentity);
    procedure FireOnATResponse(const AResponse: TOBDAdapterResponse);
    procedure FireOnError(ACode: TOBDErrorCode; const AMessage: string);

    function DoSendCommand(const ACommand: string;
      ATimeoutMs: Cardinal): TOBDAdapterResponse;
    procedure DoDetect;
    procedure DoInit;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Allocates internal state; does not auto-subscribe to
    /// the connection.</summary>
    /// <param name="AOwner">Standard component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Joins any in-flight async work and unsubscribes from
    /// the connection.</summary>
    destructor Destroy; override;

    // ---- IOBDAdapterCommandSender ----
    /// <summary>Sends a command and waits for the response.</summary>
    /// <param name="ACommand">Command verb (already formatted).</param>
    /// <param name="ATimeoutMs">Timeout in milliseconds.</param>
    /// <returns>Parsed response.</returns>
    /// <exception cref="EOBDNotConnected">Connection not active.</exception>
    /// <exception cref="EOBDAdapter">Timeout, or operation cancelled
    /// via <see cref="Close"/>.</exception>
    function SendCommand(const ACommand: string;
      ATimeoutMs: Cardinal): TOBDAdapterResponse;

    /// <summary>
    ///   Cancels every in-flight sync / async operation, joins the
    ///   async worker thread, and unsubscribes from the connection's
    ///   raw byte hook.
    /// </summary>
    /// <remarks>
    ///   Safe to call from any thread. Sync calls already in their
    ///   <c>WaitFor</c> loop wake within ~50 ms and raise
    ///   <see cref="EOBDAdapter"/>; async calls fire <c>OnError</c>
    ///   with <c>oeIO</c> and a "cancelled" message.
    ///
    ///   Equivalent to disconnecting the bound connection from the
    ///   adapter's perspective; the connection itself is not closed.
    ///   Set <c>Connection := nil</c> after this if you want to fully
    ///   detach.
    /// </remarks>
    procedure Close;

    /// <summary>
    ///   Identifies the adapter chip and populates <c>Identity</c> +
    ///   <c>Capabilities</c>.
    /// </summary>
    /// <remarks>Synchronous. Fires <c>OnProgress</c> for each of six
    /// phases (see <c>TOBDAdapterDetector</c>) and
    /// <c>OnIdentityChanged</c> on completion.</remarks>
    /// <exception cref="EOBDNotConnected">Connection not active.</exception>
    procedure Detect;

    /// <summary>Non-blocking <see cref="Detect"/>.</summary>
    /// <exception cref="EOBDConfig">Already in flight.</exception>
    procedure DetectAsync;

    /// <summary>
    ///   Runs the family's initialisation sequence, optionally
    ///   appending <c>InitCommands</c>.
    /// </summary>
    /// <remarks>Synchronous. Fires <c>OnProgress</c> per step and
    /// <c>OnReady</c> on completion.</remarks>
    /// <exception cref="EOBDNotConnected">Connection not active.</exception>
    /// <exception cref="EOBDAdapter">A required step failed.</exception>
    procedure Init;

    /// <summary>Non-blocking <see cref="Init"/>.</summary>
    /// <exception cref="EOBDConfig">Already in flight.</exception>
    procedure InitAsync;

    /// <summary>Sends an AT command and returns its parsed response.</summary>
    /// <param name="ACommand">AT command verb (without trailing CR).</param>
    /// <param name="ATimeoutMs">Timeout. <c>0</c> uses
    /// <c>CommandTimeoutMs</c>.</param>
    /// <returns>Parsed response.</returns>
    /// <exception cref="EOBDNotConnected">Connection not active.</exception>
    function WriteATCommand(const ACommand: string;
      ATimeoutMs: Cardinal = 0): TOBDAdapterResponse;

    /// <summary>Non-blocking variant. Result delivered via
    /// <c>OnATResponse</c>.</summary>
    /// <param name="ACommand">AT command verb.</param>
    /// <param name="ATimeoutMs">Timeout. <c>0</c> uses default.</param>
    /// <exception cref="EOBDConfig">Already in flight.</exception>
    procedure WriteATCommandAsync(const ACommand: string;
      ATimeoutMs: Cardinal = 0);

    /// <summary>Sends an OBDLink-extended ST command.</summary>
    /// <param name="ACommand">ST command verb.</param>
    /// <param name="ATimeoutMs">Timeout. <c>0</c> uses default.</param>
    /// <returns>Parsed response.</returns>
    /// <exception cref="EOBDNotConnected">Connection not active.</exception>
    /// <exception cref="EOBDUnsupported">Adapter lacks
    /// <c>acSTCommands</c>.</exception>
    function WriteSTCommand(const ACommand: string;
      ATimeoutMs: Cardinal = 0): TOBDAdapterResponse;

    /// <summary>Non-blocking variant. Result delivered via
    /// <c>OnATResponse</c>.</summary>
    /// <param name="ACommand">ST command verb.</param>
    /// <param name="ATimeoutMs">Timeout. <c>0</c> uses default.</param>
    /// <exception cref="EOBDConfig">Already in flight.</exception>
    /// <exception cref="EOBDUnsupported">Adapter lacks
    /// <c>acSTCommands</c>.</exception>
    procedure WriteSTCommandAsync(const ACommand: string;
      ATimeoutMs: Cardinal = 0);

    /// <summary>Sends an OBD hex command (e.g. <c>'01 0C'</c>).</summary>
    /// <param name="ACommand">OBD hex command, space-separated bytes.</param>
    /// <param name="ATimeoutMs">Timeout. <c>0</c> uses default.</param>
    /// <returns>Parsed response.</returns>
    /// <exception cref="EOBDNotConnected">Connection not active.</exception>
    function WriteOBDCommand(const ACommand: string;
      ATimeoutMs: Cardinal = 0): TOBDAdapterResponse;

    /// <summary>Non-blocking variant. Result delivered via
    /// <c>OnATResponse</c>.</summary>
    /// <param name="ACommand">OBD hex command.</param>
    /// <param name="ATimeoutMs">Timeout. <c>0</c> uses default.</param>
    /// <exception cref="EOBDConfig">Already in flight.</exception>
    procedure WriteOBDCommandAsync(const ACommand: string;
      ATimeoutMs: Cardinal = 0);

    /// <summary>Read-only identity populated by <c>Detect</c>.</summary>
    property Identity: TOBDAdapterIdentity read FIdentity;
    /// <summary>Read-only capability set populated by <c>Detect</c>
    /// from the registry.</summary>
    property Capabilities: TOBDAdapterCapabilities read FCapabilities;
    /// <summary>
    ///   Largest ISO-TP frame the adapter can carry (bytes).
    /// </summary>
    /// <remarks>
    ///   Populated from the capability registry alongside
    ///   <c>Capabilities</c> on a successful <c>Detect</c>. <c>0</c>
    ///   when not yet detected. The protocol layer (Phase 4b)
    ///   reads this to decide whether long-frame ISO-TP is available.
    /// </remarks>
    property MaxIsoTpFrameBytes: Cardinal read FMaxIsoTpFrameBytes;
  published
    /// <summary>Bound connection. Required before any I/O.</summary>
    property Connection: TOBDConnection read FConnection write SetConnection;
    /// <summary>Adapter family. Set explicitly or replaced by
    /// <c>Detect</c>.</summary>
    property Family: TOBDAdapterFamily read FFamily write FFamily
      default afELM327;
    /// <summary>User-supplied additional init commands appended to the
    /// family's built-in sequence as best-effort steps.</summary>
    property InitCommands: TStrings read FInitCommands write SetInitCommands;
    /// <summary>Default per-command timeout when caller passes
    /// <c>0</c>.</summary>
    property CommandTimeoutMs: Cardinal read FCommandTimeoutMs
      write FCommandTimeoutMs default OBD_ADAPTER_DEFAULT_TIMEOUT;

    /// <summary>Fires when <c>Init</c> completes successfully (main
    /// thread).</summary>
    property OnReady: TNotifyEvent read FOnReady write FOnReady;
    /// <summary>Fires after <c>Detect</c> populates
    /// <c>Identity</c> (main thread).</summary>
    property OnIdentityChanged: TOBDAdapterIdentityEvent
      read FOnIdentityChanged write FOnIdentityChanged;
    /// <summary>Fires for each command response (main thread).</summary>
    property OnATResponse: TOBDAdapterResponseEvent read FOnATResponse
      write FOnATResponse;
    /// <summary>Fires per phase during <c>Detect</c> / <c>Init</c>.</summary>
    /// <remarks>Detect: 6 phases (see <c>TOBDAdapterDetector</c>).
    /// Init: 1 phase per step in the resolved sequence.</remarks>
    property OnProgress: TOBDProgressEvent read FOnProgress write FOnProgress;
    /// <summary>Fires on transient adapter / I/O errors.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

const
  // Known ELM327 / OBDLink error keywords. Stored uppercase; matched
  // case-insensitively against the response.
  ERROR_KEYWORDS: array[0..11] of string = (
    'NO DATA',
    'BUS ERROR',
    'CAN ERROR',
    'BUS BUSY',
    'BUS INIT',
    'UNABLE TO CONNECT',
    'BUFFER FULL',
    'STOPPED',
    'FB ERROR',
    'DATA ERROR',
    'ERR',
    '?'   // generic invalid-command marker
  );

{ ---- TOBDAdapter ------------------------------------------------------------- }

constructor TOBDAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFamily := afELM327;
  FCommandTimeoutMs := OBD_ADAPTER_DEFAULT_TIMEOUT;
  FInitCommands := TStringList.Create;
  FIdentity := MakeAdapterIdentity;
  FRxLock := TCriticalSection.Create;
  FRxComplete := TEvent.Create(nil, True, False, '');
  FCancelEvent := TEvent.Create(nil, True, False, '');
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDAdapter.Destroy;
begin
  // Signal cancel so any in-flight sync caller wakes and exits with
  // EOBDAdapter rather than blocking the destructor on its WaitFor.
  if Assigned(FCancelEvent) then
    FCancelEvent.SetEvent;
  WaitForAsync;
  UnsubscribeIfNeeded;
  FAsyncLock.Free;
  FCancelEvent.Free;
  FRxComplete.Free;
  FRxLock.Free;
  FInitCommands.Free;
  inherited;
end;

procedure TOBDAdapter.Close;
begin
  FCancelEvent.SetEvent;
  WaitForAsync;
  UnsubscribeIfNeeded;
  FCancelEvent.ResetEvent;
end;

procedure TOBDAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TOBDAdapter.SetConnection(AValue: TOBDConnection);
begin
  if FConnection = AValue then Exit;
  UnsubscribeIfNeeded;
  if FConnection <> nil then
    FConnection.RemoveFreeNotification(Self);
  FConnection := AValue;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
end;

procedure TOBDAdapter.SetInitCommands(AValue: TStrings);
begin
  FInitCommands.Assign(AValue);
end;

procedure TOBDAdapter.SubscribeIfNeeded;
begin
  if FAutoSubscribed or (FConnection = nil) then Exit;
  FConnection.OnDataReceivedRaw := HandleRawBytes;
  FAutoSubscribed := True;
end;

procedure TOBDAdapter.UnsubscribeIfNeeded;
begin
  if FAutoSubscribed and (FConnection <> nil) then
    FConnection.OnDataReceivedRaw := nil;
  FAutoSubscribed := False;
end;

procedure TOBDAdapter.HandleRawBytes(Sender: TObject; const ABytes: TBytes);
var
  Chunk: string;
begin
  if Length(ABytes) = 0 then Exit;
  // 1:1 byte-to-Char preservation. ELM327 / OBDLink chips are 7-bit
  // clean by spec but clones may emit 0x80+ bytes; preserving them
  // lets a higher layer distinguish "junk byte 0xFE" from '?'.
  Chunk := BytesToWireString(ABytes);
  FRxLock.Enter;
  try
    FRxBuffer := FRxBuffer + Chunk;
    if TerminatorIndex(FRxBuffer) > 0 then
      FRxComplete.SetEvent;
  finally
    FRxLock.Leave;
  end;
end;

function TOBDAdapter.BytesToWireString(const ABytes: TBytes): string;
var
  I: Integer;
begin
  SetLength(Result, Length(ABytes));
  for I := 0 to High(ABytes) do
    Result[I + 1] := Char(ABytes[I]);
end;

function TOBDAdapter.StripLeadingEcho(const ABuffer,
  ACommand: string): string;
var
  Trimmed: string;
  CmdNorm: string;
  Idx, J: Integer;
  Len: Integer;
begin
  Result := ABuffer;
  CmdNorm := Trim(ACommand);
  if CmdNorm = '' then Exit;
  Trimmed := Result;

  // Drop leading whitespace / CR / LF before checking.
  J := 1;
  while (J <= Length(Trimmed)) and
        CharInSet(Trimmed[J], [#9, #10, #13, ' ']) do
    Inc(J);
  if J > 1 then
    Trimmed := Copy(Trimmed, J, MaxInt);

  // Echo present only when the buffer starts with the command
  // (case-insensitive) followed by a separator.
  Len := Length(CmdNorm);
  if (Length(Trimmed) >= Len) and
     SameText(Copy(Trimmed, 1, Len), CmdNorm) then
  begin
    Idx := Len + 1;
    // Drop the terminator(s) that follow the echoed command.
    while (Idx <= Length(Trimmed)) and
          CharInSet(Trimmed[Idx], [#9, #10, #13, ' ']) do
      Inc(Idx);
    Result := Copy(Trimmed, Idx, MaxInt);
  end;
end;

function TOBDAdapter.TerminatorIndex(const ABuffer: string): Integer;
begin
  // Position of the first '>' character.
  Result := Pos(OBD_ADAPTER_PROMPT, ABuffer);
end;

function TOBDAdapter.ParseResponse(const ARaw: string;
  const ACommand: string; AElapsed: Cardinal): TOBDAdapterResponse;
var
  Idx: Integer;
  Body: string;
  Lines: TArray<string>;
  I: Integer;
  Trimmed, Upper: string;
  Keyword: string;
begin
  Result.Command := ACommand;
  Result.Elapsed := AElapsed;
  Result.IsError := False;
  Result.ErrorKeyword := '';
  Result.Lines := nil;

  Idx := Pos(OBD_ADAPTER_PROMPT, ARaw);
  if Idx > 0 then
    Body := Copy(ARaw, 1, Idx - 1)
  else
    Body := ARaw;
  // Strip echo at the head of the buffer first — some clones emit
  // arbitrary whitespace / line endings around the echo.
  Body := StripLeadingEcho(Body, ACommand);
  Result.Raw := Body;

  Lines := SplitString(Body, #13#10);
  if Length(Lines) <= 1 then
    Lines := SplitString(Body, #13);
  if Length(Lines) <= 1 then
    Lines := SplitString(Body, #10);

  SetLength(Result.Lines, 0);
  for I := 0 to High(Lines) do
  begin
    Trimmed := Trim(Lines[I]);
    if Trimmed = '' then Continue;
    // Belt-and-braces: a trailing per-line echo (rare) is still dropped.
    if SameText(Trimmed, Trim(ACommand)) then Continue;
    SetLength(Result.Lines, Length(Result.Lines) + 1);
    Result.Lines[High(Result.Lines)] := Trimmed;
  end;

  // Error-keyword scan over the trimmed lines.
  for I := 0 to High(Result.Lines) do
  begin
    Upper := UpperCase(Result.Lines[I]);
    for Keyword in ERROR_KEYWORDS do
      if Pos(Keyword, Upper) > 0 then
      begin
        Result.IsError := True;
        Result.ErrorKeyword := Keyword;
        Exit;
      end;
  end;
end;

function TOBDAdapter.SendCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
begin
  Result := DoSendCommand(ACommand, ATimeoutMs);
end;

function TOBDAdapter.DoSendCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
var
  Sw: TStopwatch;
  Raw: string;
  Effective: Cardinal;
begin
  if (FConnection = nil) or not FConnection.Active then
    raise EOBDNotConnected.Create(
      'TOBDAdapter: Connection is not active');
  if Trim(ACommand) = '' then
    raise EOBDAdapter.Create('SendCommand: command is empty');

  SubscribeIfNeeded;

  Effective := ATimeoutMs;
  if Effective = 0 then Effective := FCommandTimeoutMs;

  // FCancelEvent's lifecycle is owned by Close / destructor — never
  // reset here. A cancel set between two commands of a Detect / Init
  // sequence must persist so the next SendCommand exits immediately.
  if FCancelEvent.WaitFor(0) = wrSignaled then
    raise EOBDAdapter.Create(
      'Adapter is cancelled (Close was called)');

  FRxLock.Enter;
  try
    FRxBuffer := '';
    FRxComplete.ResetEvent;
    FCurrentCommand := ACommand;
  finally
    FRxLock.Leave;
  end;

  Sw := TStopwatch.StartNew;
  FConnection.WriteString(ACommand + #13);

  // Poll-loop on FRxComplete + FCancelEvent so an external Close (or
  // destructor) can abort within ~50 ms instead of waiting out the
  // full timeout.
  while True do
  begin
    if FRxComplete.WaitFor(50) = wrSignaled then
      Break;
    if FCancelEvent.WaitFor(0) = wrSignaled then
      raise EOBDAdapter.CreateFmt(
        'Operation cancelled while waiting for response to "%s"',
        [ACommand]);
    if Cardinal(Sw.ElapsedMilliseconds) >= Effective then
    begin
      FireOnError(oeTimeout,
        Format('Timeout waiting for response to "%s"', [ACommand]));
      raise EOBDAdapter.CreateFmt(
        'Timeout waiting for response to "%s"', [ACommand]);
    end;
  end;

  FRxLock.Enter;
  try
    Raw := FRxBuffer;
  finally
    FRxLock.Leave;
  end;

  Result := ParseResponse(Raw, ACommand, Cardinal(Sw.ElapsedMilliseconds));
end;

procedure TOBDAdapter.WaitForAsync;
var
  Worker: TThread;
begin
  FAsyncLock.Enter;
  try
    Worker := FAsyncThread;
    FAsyncThread := nil;
  finally
    FAsyncLock.Leave;
  end;
  if Worker = nil then Exit;
  Worker.WaitFor;
  Worker.Free;
end;

procedure TOBDAdapter.FireProgress(AIndex, ACount: Cardinal;
  const AName, ADetail: string);
var
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then Exit;
  Step := TOBDProgressStep.MakeStep(AIndex, ACount, AName, ADetail);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self, Step)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnProgress) then
          FOnProgress(Self, Step);
      end);
end;

procedure TOBDAdapter.FireOnReady;
begin
  if not Assigned(FOnReady) then Exit;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnReady(Self)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnReady) then
          FOnReady(Self);
      end);
end;

procedure TOBDAdapter.FireOnIdentityChanged(
  const AIdentity: TOBDAdapterIdentity);
var
  Snapshot: TOBDAdapterIdentity;
begin
  Snapshot := AIdentity;
  if not Assigned(FOnIdentityChanged) then Exit;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnIdentityChanged(Self, Snapshot)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnIdentityChanged) then
          FOnIdentityChanged(Self, Snapshot);
      end);
end;

procedure TOBDAdapter.FireOnATResponse(const AResponse: TOBDAdapterResponse);
var
  Snapshot: TOBDAdapterResponse;
begin
  Snapshot := AResponse;
  if not Assigned(FOnATResponse) then Exit;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnATResponse(Self, Snapshot)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnATResponse) then
          FOnATResponse(Self, Snapshot);
      end);
end;

procedure TOBDAdapter.FireOnError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Code: TOBDErrorCode;
  Msg: string;
begin
  Code := ACode;
  Msg := AMessage;
  if not Assigned(FOnError) then Exit;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil,
      procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(FOnError) then
          FOnError(Self, Code, Msg, Handled);
      end);
end;

procedure TOBDAdapter.DoDetect;
var
  Identity: TOBDAdapterIdentity;
  Entry: TOBDAdapterCapabilityEntry;
  ProgressCb: TOBDDetectionProgress;
begin
  ProgressCb :=
    procedure(AIndex, ACount: Cardinal; const AName, ADetail: string)
    begin
      FireProgress(AIndex, ACount, AName, ADetail);
    end;

  TOBDAdapterDetector.Detect(Self as IOBDAdapterCommandSender, Identity,
    ProgressCb, FCommandTimeoutMs);

  // Capability lookup
  if TOBDAdapterCapabilityRegistry.Default.TryFind(Identity.AdapterKey, Entry) then
  begin
    FCapabilities := Entry.Capabilities;
    FMaxIsoTpFrameBytes := Entry.MaxIsoTpFrameBytes;
  end
  else
  begin
    FCapabilities := [];
    FMaxIsoTpFrameBytes := 0;
  end;

  FIdentity := Identity;
  FFamily := Identity.Family;
  FireOnIdentityChanged(Identity);
end;

procedure TOBDAdapter.DoInit;
var
  Sequence: TOBDInitSequence;
  ProgressCb: TOBDDetectionProgress;
begin
  // ResolvedSequence consults JSON-loaded overrides first and falls
  // back to the in-source built-ins.
  Sequence := TOBDAdapterInitializer.ResolvedSequence(FFamily);
  Sequence := TOBDAdapterInitializer.ExtendSequence(Sequence, FInitCommands);
  ProgressCb :=
    procedure(AIndex, ACount: Cardinal; const AName, ADetail: string)
    begin
      FireProgress(AIndex, ACount, AName, ADetail);
    end;
  TOBDAdapterInitializer.Run(Self as IOBDAdapterCommandSender,
    Sequence, ProgressCb, FCommandTimeoutMs);
  FInitialized := True;
  FireOnReady;
end;

procedure TOBDAdapter.Detect;
begin
  if (FConnection = nil) or not FConnection.Active then
    raise EOBDNotConnected.Create('TOBDAdapter.Detect: Connection inactive');
  WaitForAsync;
  SubscribeIfNeeded;
  DoDetect;
end;

procedure TOBDAdapter.DetectAsync;
var
  Self_: TOBDAdapter;
begin
  if (FConnection = nil) or not FConnection.Active then
    raise EOBDNotConnected.Create('TOBDAdapter.DetectAsync: Connection inactive');
  FAsyncLock.Enter;
  try
    if FAsyncThread <> nil then
      raise EOBDConfig.Create('Adapter operation already in flight');
  finally
    FAsyncLock.Leave;
  end;
  SubscribeIfNeeded;

  Self_ := Self;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    var
      Worker: TThread;
    begin
      try
        try
          Self_.DoDetect;
        except
          on E: Exception do
            Self_.FireOnError(oeIO, E.Message);
        end;
      finally
        TThread.Queue(nil,
          procedure
          begin
            Self_.FAsyncLock.Enter;
            try
              Worker := Self_.FAsyncThread;
              Self_.FAsyncThread := nil;
            finally
              Self_.FAsyncLock.Leave;
            end;
            if Worker <> nil then
            begin
              Worker.WaitFor;
              Worker.Free;
            end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

procedure TOBDAdapter.Init;
begin
  if (FConnection = nil) or not FConnection.Active then
    raise EOBDNotConnected.Create('TOBDAdapter.Init: Connection inactive');
  WaitForAsync;
  SubscribeIfNeeded;
  DoInit;
end;

procedure TOBDAdapter.InitAsync;
var
  Self_: TOBDAdapter;
begin
  if (FConnection = nil) or not FConnection.Active then
    raise EOBDNotConnected.Create('TOBDAdapter.InitAsync: Connection inactive');
  FAsyncLock.Enter;
  try
    if FAsyncThread <> nil then
      raise EOBDConfig.Create('Adapter operation already in flight');
  finally
    FAsyncLock.Leave;
  end;
  SubscribeIfNeeded;

  Self_ := Self;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    var
      Worker: TThread;
    begin
      try
        try
          Self_.DoInit;
        except
          on E: Exception do
            Self_.FireOnError(oeIO, E.Message);
        end;
      finally
        TThread.Queue(nil,
          procedure
          begin
            Self_.FAsyncLock.Enter;
            try
              Worker := Self_.FAsyncThread;
              Self_.FAsyncThread := nil;
            finally
              Self_.FAsyncLock.Leave;
            end;
            if Worker <> nil then
            begin
              Worker.WaitFor;
              Worker.Free;
            end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

function TOBDAdapter.WriteATCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
begin
  Result := DoSendCommand(ACommand, ATimeoutMs);
  FireOnATResponse(Result);
end;

procedure TOBDAdapter.WriteATCommandAsync(const ACommand: string;
  ATimeoutMs: Cardinal);
var
  Self_: TOBDAdapter;
  CmdCopy: string;
  TmoCopy: Cardinal;
begin
  if (FConnection = nil) or not FConnection.Active then
    raise EOBDNotConnected.Create('Connection inactive');
  FAsyncLock.Enter;
  try
    if FAsyncThread <> nil then
      raise EOBDConfig.Create('Adapter operation already in flight');
  finally
    FAsyncLock.Leave;
  end;

  Self_ := Self;
  CmdCopy := ACommand;
  TmoCopy := ATimeoutMs;
  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    var
      Resp: TOBDAdapterResponse;
      Worker: TThread;
    begin
      try
        try
          Resp := Self_.DoSendCommand(CmdCopy, TmoCopy);
          Self_.FireOnATResponse(Resp);
        except
          on E: Exception do
            Self_.FireOnError(oeIO, E.Message);
        end;
      finally
        TThread.Queue(nil,
          procedure
          begin
            Self_.FAsyncLock.Enter;
            try
              Worker := Self_.FAsyncThread;
              Self_.FAsyncThread := nil;
            finally
              Self_.FAsyncLock.Leave;
            end;
            if Worker <> nil then
            begin
              Worker.WaitFor;
              Worker.Free;
            end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

function TOBDAdapter.WriteSTCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
begin
  if not (acSTCommands in FCapabilities) then
    raise EOBDUnsupported.Create(
      'ST commands require an OBDLink-family adapter (acSTCommands)');
  Result := DoSendCommand(ACommand, ATimeoutMs);
  FireOnATResponse(Result);
end;

procedure TOBDAdapter.WriteSTCommandAsync(const ACommand: string;
  ATimeoutMs: Cardinal);
begin
  if not (acSTCommands in FCapabilities) then
    raise EOBDUnsupported.Create(
      'ST commands require an OBDLink-family adapter (acSTCommands)');
  WriteATCommandAsync(ACommand, ATimeoutMs);
end;

function TOBDAdapter.WriteOBDCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
begin
  Result := DoSendCommand(ACommand, ATimeoutMs);
  FireOnATResponse(Result);
end;

procedure TOBDAdapter.WriteOBDCommandAsync(const ACommand: string;
  ATimeoutMs: Cardinal);
begin
  WriteATCommandAsync(ACommand, ATimeoutMs);
end;

end.
