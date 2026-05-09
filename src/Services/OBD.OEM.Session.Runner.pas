//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Session.Runner.pas
// CONTENTS       : Executes a TOBDSessionPlan against a connection
//                  and runs the tester-present heartbeat thread.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Plan execution is async-first: each step is
//                  awaited via TOBDConnectionAsync's IOBDFuture<string>.
//                  When ExpectedResponse is set, the response hex
//                  string must start with that prefix (case-
//                  insensitive). When empty, any non-empty reply
//                  passes — that's what lets Stellantis' optional
//                  F198 probe tolerate FCA's negative response.
//------------------------------------------------------------------------------
unit OBD.OEM.Session.Runner;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  OBD.Async, OBD.Connection.Async, OBD.OEM.Session;

type
  EOBDSessionRunnerError = class(Exception);

  /// <summary>
  ///   One row in the audit log produced by the runner.
  /// </summary>
  TOBDSessionStepResult = record
    Step: TOBDSessionStep;
    /// <summary>
    ///   Raw response text received from the adapter.
    /// </summary>
    Response: string;
    /// <summary>
    ///   True if the step matched its expected-response prefix
    ///   (or the prefix was empty and any reply was acceptable).
    /// </summary>
    Success: Boolean;
    /// <summary>
    ///   Empty when <c>Success</c> is true.
    /// </summary>
    ErrorMessage: string;
    /// <summary>
    ///   Wall-clock duration of the step (ms).
    /// </summary>
    DurationMs: Cardinal;
  end;

  TOBDSessionRunResult = record
    Success: Boolean;
    Steps: TArray<TOBDSessionStepResult>;
    /// <summary>
    ///   Captured if any step raised; nil otherwise.
    /// </summary>
    Error: Exception;
  end;

  /// <summary>
  ///   Background thread that fires the negotiator's tester-present
  ///   request at a fixed interval. Exits on <c>Terminate</c> + on
  ///   the first send failure (so a dropped connection stops the
  ///   heartbeat instead of spinning).
  /// </summary>
  TOBDTesterPresentThread = class(TThread)
  strict private
    FConnection: TOBDConnectionAsync;
    FRequest: TBytes;
    FIntervalMs: Cardinal;
    FStopEvent: TEvent;
    FToken: IOBDCancellationToken;
    function FormatHexBytes(const Bytes: TBytes): string;
  protected
    procedure Execute; override;
  public
    constructor Create(AConnection: TOBDConnectionAsync;
      const ARequest: TBytes; AIntervalMs: Cardinal);
    destructor Destroy; override;
    procedure StopGracefully;
  end;

  /// <summary>
  ///   Sync façade: blocks until the plan is fully executed.
  ///   Concurrency-safe — multiple instances are independent.
  /// </summary>
  TOBDSessionRunner = class
  strict private
    FConnection: TOBDConnectionAsync;
    FDefaultTimeoutMs: Cardinal;
    function ExecuteStep(const Step: TOBDSessionStep): TOBDSessionStepResult;
    function FormatHexBytes(const Bytes: TBytes): string;
    function HexResponseStartsWith(const Response: string;
      const Prefix: TBytes): Boolean;
  public
    constructor Create(AConnection: TOBDConnectionAsync);
    /// <summary>
    ///   Per-step default timeout when the step itself doesn't
    ///   specify one. Defaults to 5000 ms.
    /// </summary>
    property DefaultTimeoutMs: Cardinal read FDefaultTimeoutMs write FDefaultTimeoutMs;
    /// <summary>
    ///   Run <c>Plan</c>'s steps in order. Stops on the first failed
    ///   step and reports it in the result. Does NOT start the
    ///   tester-present thread — the caller does that explicitly with
    ///   <c>StartTesterPresent</c> after a successful BeginSession.
    /// </summary>
    function Execute(const Plan: TOBDSessionPlan): TOBDSessionRunResult;
    /// <summary>
    ///   Spawn a tester-present thread per the plan's heartbeat spec.
    ///   Returns nil when the plan has no heartbeat (default session,
    ///   or the negotiator chose 0 ms). Caller owns and frees the
    ///   thread (call <c>StopGracefully</c> first).
    /// </summary>
    function StartTesterPresent(const Plan: TOBDSessionPlan): TOBDTesterPresentThread;
  end;

implementation

uses
  System.Diagnostics, System.Math;

const
  DEFAULT_STEP_TIMEOUT_MS = 5000;

//------------------------------------------------------------------------------
// BYTES EQUAL PREFIX
//------------------------------------------------------------------------------
function BytesEqualPrefix(const Bytes, Prefix: TBytes): Boolean;
var
  I: Integer;
begin
  if Length(Prefix) = 0 then Exit(True);
  if Length(Bytes) < Length(Prefix) then Exit(False);
  for I := 0 to High(Prefix) do
    if Bytes[I] <> Prefix[I] then Exit(False);
  Result := True;
end;

//------------------------------------------------------------------------------
// HEX CHARS TO BYTES
//------------------------------------------------------------------------------
function HexCharsToBytes(const HexChars: string): TBytes;
var
  Buf: string;
  I, J: Integer;
begin
  // Strips whitespace + non-hex characters and pairs the remaining
  // nibbles. Lets us match an ELM327 response like '50 03 00 32 01 F4'
  // against an expected prefix of $50 $03 without parsing the surrounding
  // protocol envelope.
  SetLength(Buf, Length(HexChars));
  J := 0;
  for I := 1 to Length(HexChars) do
    if CharInSet(HexChars[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
    begin
      Inc(J);
      Buf[J] := HexChars[I];
    end;
  SetLength(Buf, J);
  if Odd(J) then Buf := Copy(Buf, 1, J - 1);  // drop dangling nibble
  SetLength(Result, Length(Buf) div 2);
  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(Buf, I * 2 + 1, 2));
end;

//==============================================================================
// TOBDTesterPresentThread
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDTesterPresentThread.Create(AConnection: TOBDConnectionAsync;
  const ARequest: TBytes; AIntervalMs: Cardinal);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FConnection := AConnection;
  FRequest := ARequest;
  FIntervalMs := AIntervalMs;
  FStopEvent := TEvent.Create(nil, True, False, '');
  FToken := NewCancellationToken;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDTesterPresentThread.Destroy;
begin
  FStopEvent.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// STOP GRACEFULLY
//------------------------------------------------------------------------------
procedure TOBDTesterPresentThread.StopGracefully;
begin
  Terminate;
  FStopEvent.SetEvent;
  if Assigned(FToken) then FToken.Cancel;
  // Wait for graceful shutdown so the next session-control request
  // doesn't race against the heartbeat.
  WaitFor;
end;

//------------------------------------------------------------------------------
// FORMAT HEX BYTES
//------------------------------------------------------------------------------
function TOBDTesterPresentThread.FormatHexBytes(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Bytes) do
    Result := Result + IntToHex(Bytes[I], 2);
end;

//------------------------------------------------------------------------------
// EXECUTE
//------------------------------------------------------------------------------
procedure TOBDTesterPresentThread.Execute;
var
  HexCmd: string;
  Future: IOBDFuture<string>;
begin
  HexCmd := FormatHexBytes(FRequest);
  while not Terminated do
  begin
    if FStopEvent.WaitFor(FIntervalMs) <> wrTimeout then Break;
    if Terminated then Break;
    try
      Future := FConnection.OBDAsync(HexCmd, FIntervalMs * 2, FToken);
      // Don't actually wait for the response — 3E 80 suppresses the
      // positive reply, so we'd just time out. Fire-and-forget.
      Future := nil;
    except
      // Connection dropped — exit; the next BeginSession will re-arm.
      Break;
    end;
  end;
end;

//==============================================================================
// TOBDSessionRunner
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSessionRunner.Create(AConnection: TOBDConnectionAsync);
begin
  inherited Create;
  if not Assigned(AConnection) then
    raise EOBDSessionRunnerError.Create(
      'TOBDSessionRunner requires a non-nil TOBDConnectionAsync');
  FConnection := AConnection;
  FDefaultTimeoutMs := DEFAULT_STEP_TIMEOUT_MS;
end;

//------------------------------------------------------------------------------
// FORMAT HEX BYTES
//------------------------------------------------------------------------------
function TOBDSessionRunner.FormatHexBytes(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Bytes) do
    Result := Result + IntToHex(Bytes[I], 2);
end;

//------------------------------------------------------------------------------
// HEX RESPONSE STARTS WITH
//------------------------------------------------------------------------------
function TOBDSessionRunner.HexResponseStartsWith(const Response: string;
  const Prefix: TBytes): Boolean;
begin
  if Length(Prefix) = 0 then Exit(Trim(Response) <> '');
  Result := BytesEqualPrefix(HexCharsToBytes(Response), Prefix);
end;

//------------------------------------------------------------------------------
// EXECUTE STEP
//------------------------------------------------------------------------------
function TOBDSessionRunner.ExecuteStep(
  const Step: TOBDSessionStep): TOBDSessionStepResult;
var
  Watch: TStopwatch;
  Future: IOBDFuture<string>;
  Timeout: Cardinal;
begin
  Result := Default(TOBDSessionStepResult);
  Result.Step := Step;
  Watch := TStopwatch.StartNew;
  Timeout := Step.TimeoutMs;
  if Timeout = 0 then Timeout := FDefaultTimeoutMs;
  try
    case Step.Kind of
      sskATCommand:
        Future := FConnection.ATAsync(Step.AdapterCmd, Timeout);
      sskUDSRequest:
        Future := FConnection.OBDAsync(FormatHexBytes(Step.UDS), Timeout);
    end;
    Result.Response := Future.Await(Timeout + 500);
    Result.Success := HexResponseStartsWith(Result.Response, Step.ExpectedResponse);
    if not Result.Success then
      Result.ErrorMessage :=
        Format('Response did not match expected prefix (%s)',
          [FormatHexBytes(Step.ExpectedResponse)]);
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

//------------------------------------------------------------------------------
// EXECUTE
//------------------------------------------------------------------------------
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
      // Trim the result down to the steps we actually ran; later
      // entries are zeroed default records and would mislead the audit.
      SetLength(Result.Steps, I + 1);
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
// START TESTER PRESENT
//------------------------------------------------------------------------------
function TOBDSessionRunner.StartTesterPresent(
  const Plan: TOBDSessionPlan): TOBDTesterPresentThread;
begin
  if Plan.TesterPresentMs = 0 then Exit(nil);
  if Length(Plan.TesterPresentRequest) = 0 then Exit(nil);
  Result := TOBDTesterPresentThread.Create(
    FConnection, Plan.TesterPresentRequest, Plan.TesterPresentMs);
end;

end.
