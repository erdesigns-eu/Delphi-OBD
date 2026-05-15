//------------------------------------------------------------------------------
//  OBD.OEM.Session.Runner
//
//  Executes a <see cref="TOBDSessionPlan"/> against a
//  <see cref="TOBDProtocol"/> + bound adapter and runs the
//  tester-present heartbeat thread.
//
//  Plan execution is sync — each step blocks on the protocol's
//  <c>Request</c> (UDS) or the adapter's <c>SendCommand</c> (AT)
//  call. When <c>ExpectedResponse</c> is set, the response bytes
//  must start with that prefix; when empty, any non-error reply
//  passes — that is what lets optional probes (Stellantis F198,
//  etc.) tolerate negative responses.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation against
//                     TOBDProtocol + TOBDAdapter.
//------------------------------------------------------------------------------

unit OBD.OEM.Session.Runner;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Adapter,
  OBD.Adapter.Types,
  OBD.Protocol,
  OBD.Protocol.Types,
  OBD.OEM.Session;

type
  /// <summary>Raised on runner mis-configuration.</summary>
  EOBDSessionRunnerError = class(Exception);

  /// <summary>One row in the runner's audit log.</summary>
  TOBDSessionStepResult = record
    /// <summary>The step that ran.</summary>
    Step: TOBDSessionStep;
    /// <summary>
    ///   Adapter response text (AT step) or hex-rendered UDS
    ///   response bytes (UDS step).
    /// </summary>
    Response: string;
    /// <summary>Decoded UDS response bytes (UDS step;
    /// empty for AT steps).</summary>
    ResponseBytes: TBytes;
    /// <summary>
    ///   <c>True</c> when the step matched its expected-response
    ///   prefix (or the prefix was empty and any non-error reply
    ///   was acceptable).
    /// </summary>
    Success: Boolean;
    /// <summary>Empty when <c>Success</c> is <c>True</c>.</summary>
    ErrorMessage: string;
    /// <summary>Wall-clock duration of the step (ms).</summary>
    DurationMs: Cardinal;
  end;

  /// <summary>End-to-end plan execution result.</summary>
  TOBDSessionRunResult = record
    /// <summary>Every step succeeded.</summary>
    Success: Boolean;
    /// <summary>Per-step audit rows (truncated at the first
    /// failure when <c>Success</c> is <c>False</c>).</summary>
    Steps: TArray<TOBDSessionStepResult>;
  end;

  /// <summary>
  ///   Background thread that fires the negotiator's
  ///   tester-present request at a fixed interval. Exits on
  ///   <c>Terminate</c> + on the first send failure (so a
  ///   dropped connection stops the heartbeat instead of
  ///   spinning).
  /// </summary>
  TOBDTesterPresentThread = class(TThread)
  strict private
    FProtocol: TOBDProtocol;
    FRequest: TBytes;
    FIntervalMs: Cardinal;
    FStopEvent: TEvent;
  protected
    procedure Execute; override;
  public
    /// <summary>Constructs and starts the heartbeat
    /// thread.</summary>
    /// <param name="AProtocol">Bound protocol.</param>
    /// <param name="ARequest">Tester-present bytes (typically
    /// <c>$3E $80</c>).</param>
    /// <param name="AIntervalMs">Heartbeat interval.</param>
    constructor Create(AProtocol: TOBDProtocol;
      const ARequest: TBytes; AIntervalMs: Cardinal);
    /// <summary>Frees state.</summary>
    destructor Destroy; override;
    /// <summary>Signals the thread to exit and blocks until it
    /// has joined.</summary>
    procedure StopGracefully;
  end;

  /// <summary>
  ///   Synchronous plan runner. Concurrency-safe — multiple
  ///   instances bound to different protocols are independent.
  /// </summary>
  TOBDSessionRunner = class
  strict private
    FProtocol: TOBDProtocol;
    FDefaultTimeoutMs: Cardinal;
    function ExecuteStep(
      const Step: TOBDSessionStep): TOBDSessionStepResult;
    function FormatHexBytes(const Bytes: TBytes): string;
    function HexResponseStartsWith(
      const ResponseBytes: TBytes;
      const Prefix: TBytes): Boolean;
  public
    /// <summary>Constructs the runner.</summary>
    /// <param name="AProtocol">Bound protocol (must have a
    /// connected adapter).</param>
    /// <exception cref="EOBDSessionRunnerError">
    ///   <c>AProtocol</c> is nil or has no bound adapter.
    /// </exception>
    constructor Create(AProtocol: TOBDProtocol);
    /// <summary>Per-step default timeout when the step itself
    /// doesn't specify one. Defaults to 5000 ms.</summary>
    property DefaultTimeoutMs: Cardinal
      read FDefaultTimeoutMs write FDefaultTimeoutMs;
    /// <summary>
    ///   Runs <c>Plan</c>'s steps in order. Stops on the first
    ///   failed step and reports it in the result. Does NOT
    ///   start the tester-present thread — call
    ///   <see cref="StartTesterPresent"/> after a successful
    ///   begin.
    /// </summary>
    /// <param name="Plan">Plan to execute.</param>
    function Execute(
      const Plan: TOBDSessionPlan): TOBDSessionRunResult;
    /// <summary>
    ///   Spawns a tester-present thread per the plan's heartbeat
    ///   spec. Returns <c>nil</c> when the plan has no
    ///   heartbeat. Caller owns the thread and must call
    ///   <c>StopGracefully</c> before <c>Free</c>.
    /// </summary>
    /// <param name="Plan">Plan whose heartbeat spec to read.</param>
    function StartTesterPresent(
      const Plan: TOBDSessionPlan): TOBDTesterPresentThread;
  end;

implementation

uses
  System.Diagnostics;

const
  DEFAULT_STEP_TIMEOUT_MS = 5000;

function BytesEqualPrefix(const Bytes, Prefix: TBytes): Boolean;
var
  I: Integer;
begin
  if Length(Prefix) = 0 then
    Exit(True);
  if Length(Bytes) < Length(Prefix) then
    Exit(False);
  for I := 0 to High(Prefix) do
    if Bytes[I] <> Prefix[I] then
      Exit(False);
  Result := True;
end;

{ TOBDTesterPresentThread }

constructor TOBDTesterPresentThread.Create(AProtocol: TOBDProtocol;
  const ARequest: TBytes; AIntervalMs: Cardinal);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FProtocol := AProtocol;
  FRequest := ARequest;
  FIntervalMs := AIntervalMs;
  FStopEvent := TEvent.Create(nil, True, False, '');
end;

destructor TOBDTesterPresentThread.Destroy;
begin
  FStopEvent.Free;
  inherited;
end;

procedure TOBDTesterPresentThread.StopGracefully;
begin
  Terminate;
  FStopEvent.SetEvent;
  WaitFor;
end;

procedure TOBDTesterPresentThread.Execute;
var
  SID: Byte;
  Body: TBytes;
begin
  if Length(FRequest) = 0 then
    Exit;
  SID := FRequest[0];
  Body := Copy(FRequest, 1, Length(FRequest) - 1);
  while not Terminated do
  begin
    if FStopEvent.WaitFor(FIntervalMs) <> wrTimeout then
      Break;
    if Terminated then
      Break;
    try
      // Sub-function 0x80 (suppress positive response) means the
      // ECU won't reply; the call will time out at the protocol
      // layer. We swallow the exception and keep the heartbeat
      // running until either Terminate or a non-timeout failure
      // (dropped adapter) breaks us out.
      FProtocol.Request(SID, Body, FIntervalMs * 2);
    except
      on E: Exception do
        if (Pos('timeout', LowerCase(E.Message)) = 0) and
           (Pos('time-out', LowerCase(E.Message)) = 0) then
          Break;
    end;
  end;
end;

{ TOBDSessionRunner }

constructor TOBDSessionRunner.Create(AProtocol: TOBDProtocol);
begin
  inherited Create;
  if AProtocol = nil then
    raise EOBDSessionRunnerError.Create(
      'TOBDSessionRunner requires a non-nil TOBDProtocol');
  if AProtocol.Adapter = nil then
    raise EOBDSessionRunnerError.Create(
      'TOBDSessionRunner: bound protocol has no adapter');
  FProtocol := AProtocol;
  FDefaultTimeoutMs := DEFAULT_STEP_TIMEOUT_MS;
end;

function TOBDSessionRunner.FormatHexBytes(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Bytes) do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(Bytes[I], 2);
  end;
end;

function TOBDSessionRunner.HexResponseStartsWith(
  const ResponseBytes: TBytes;
  const Prefix: TBytes): Boolean;
begin
  if Length(Prefix) = 0 then
    Exit(Length(ResponseBytes) > 0);
  Result := BytesEqualPrefix(ResponseBytes, Prefix);
end;

function TOBDSessionRunner.ExecuteStep(
  const Step: TOBDSessionStep): TOBDSessionStepResult;
var
  Watch: TStopwatch;
  Timeout: Cardinal;
  AdapterResp: TOBDAdapterResponse;
  UdsResp: TOBDResponse;
  SID: Byte;
  Body: TBytes;
begin
  Result := Default(TOBDSessionStepResult);
  Result.Step := Step;
  Watch := TStopwatch.StartNew;
  Timeout := Step.TimeoutMs;
  if Timeout = 0 then
    Timeout := FDefaultTimeoutMs;
  try
    case Step.Kind of
      sskATCommand:
        begin
          AdapterResp := FProtocol.Adapter.SendCommand(
            Step.AdapterCmd, Timeout);
          Result.Response := AdapterResp.Raw;
          // AT responses are text — they don't carry typed
          // expected-response prefixes the way UDS does. An empty
          // ExpectedResponse just requires a non-empty reply.
          if Length(Step.ExpectedResponse) = 0 then
            Result.Success := Trim(AdapterResp.Raw) <> ''
          else
            // Caller supplied raw bytes; check by ASCII text.
            Result.Success := Pos(
              TEncoding.ASCII.GetString(Step.ExpectedResponse),
              AdapterResp.Raw) > 0;
          if not Result.Success then
            Result.ErrorMessage :=
              'Adapter response did not match expected prefix';
        end;
      sskUDSRequest:
        begin
          if Length(Step.UDS) = 0 then
            raise EOBDSessionRunnerError.Create(
              'UDS step has empty payload');
          SID := Step.UDS[0];
          Body := Copy(Step.UDS, 1, Length(Step.UDS) - 1);
          UdsResp := FProtocol.Request(SID, Body, Timeout);
          // Reassemble the on-wire shape (SID + Data) for
          // expected-response prefix matching.
          SetLength(Result.ResponseBytes, 1 + Length(UdsResp.Data));
          Result.ResponseBytes[0] := UdsResp.ServiceID;
          if Length(UdsResp.Data) > 0 then
            Move(UdsResp.Data[0], Result.ResponseBytes[1],
              Length(UdsResp.Data));
          Result.Response := FormatHexBytes(Result.ResponseBytes);
          Result.Success := HexResponseStartsWith(
            Result.ResponseBytes, Step.ExpectedResponse);
          if not Result.Success then
            Result.ErrorMessage := Format(
              'Response did not match expected prefix (%s)',
              [FormatHexBytes(Step.ExpectedResponse)]);
        end;
    end;
  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  end;
  Watch.Stop;
  Result.DurationMs := Cardinal(Watch.ElapsedMilliseconds);
end;

function TOBDSessionRunner.Execute(
  const Plan: TOBDSessionPlan): TOBDSessionRunResult;
var
  StepResult: TOBDSessionStepResult;
  I: Integer;
begin
  Result := Default(TOBDSessionRunResult);
  Result.Success := True;
  SetLength(Result.Steps, Length(Plan.Steps));
  for I := 0 to High(Plan.Steps) do
  begin
    StepResult := ExecuteStep(Plan.Steps[I]);
    Result.Steps[I] := StepResult;
    if not StepResult.Success then
    begin
      Result.Success := False;
      // Truncate the audit to the steps actually run.
      SetLength(Result.Steps, I + 1);
      Exit;
    end;
  end;
end;

function TOBDSessionRunner.StartTesterPresent(
  const Plan: TOBDSessionPlan): TOBDTesterPresentThread;
begin
  if Plan.TesterPresentMs = 0 then
    Exit(nil);
  if Length(Plan.TesterPresentRequest) = 0 then
    Exit(nil);
  Result := TOBDTesterPresentThread.Create(
    FProtocol, Plan.TesterPresentRequest, Plan.TesterPresentMs);
end;

end.
