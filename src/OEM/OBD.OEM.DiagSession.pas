//------------------------------------------------------------------------------
//  OBD.OEM.DiagSession
//
//  High-level diagnostic-session wrapper. Stitches the OEM
//  session negotiator, seed-key registry, DID catalogue and
//  RoutineControl framework into one stateful workflow with the
//  failure modes a tool builder actually cares about.
//
//  The wrapper owns the tester-present heartbeat thread so
//  callers do not have to remember Stop + Free lifecycles. Each
//  high-level call returns a boolean for the simple case and
//  exposes the last detailed error via <see cref="LastError"/>
//  when the boolean is <c>False</c>.
//
//  SecurityAccess is delegated to
//  <see cref="TOBDSecurityAccessClient"/> so the timing / NRC
//  / retry policy is shared with hosts that drive the dance
//  directly.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.DiagSession;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Protocol,
  OBD.Protocol.Types,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.Session.Runner,
  OBD.OEM.SeedKey,
  OBD.OEM.SecurityAccess,
  OBD.OEM.RoutineControl;

type
  /// <summary>Raised on construction errors. Runtime failures
  /// surface through the <c>Boolean</c> return + <c>LastError</c>
  /// rather than exceptions.</summary>
  EOBDDiagSessionError = class(Exception);

  /// <summary>High-level session state machine.</summary>
  TOBDDiagSessionState = (
    /// <summary>Default session, no heartbeat.</summary>
    dssIdle,
    /// <summary>Non-default session running with
    /// heartbeat.</summary>
    dssActive,
    /// <summary>Non-default + SecurityAccess passed.</summary>
    dssActiveUnlocked,
    /// <summary>Free has been called; reuse forbidden.</summary>
    dssClosed);

  /// <summary>Stateful diagnostic-session wrapper.</summary>
  TOBDDiagSession = class
  strict private
    FProtocol: TOBDProtocol;
    FOEM: IOBDOEMExtension;
    FRunner: TOBDSessionRunner;
    FSecurity: TOBDSecurityAccessClient;
    FHeartbeat: TOBDTesterPresentThread;
    FState: TOBDDiagSessionState;
    FActiveSession: TOBDSessionType;
    FActiveECU: Word;
    FUnlockedLevel: Byte;
    FLastError: string;
    FLock: TCriticalSection;
    procedure StopHeartbeat;
    function CheckResponse(const Resp: TOBDResponse;
      const Operation: string): Boolean;
  public
    /// <summary>Constructs a session bound to a protocol +
    /// OEM extension.</summary>
    /// <param name="AProtocol">Bound protocol (must have an
    /// active adapter).</param>
    /// <param name="AExtension">OEM extension supplying the
    /// negotiator, seed-key registry and DID decoder.</param>
    /// <exception cref="EOBDDiagSessionError"><c>AProtocol</c>
    /// or <c>AExtension</c> is nil.</exception>
    constructor Create(AProtocol: TOBDProtocol;
      const AExtension: IOBDOEMExtension);
    /// <summary>Frees state. Stops the heartbeat and attempts
    /// best-effort EndSession; never raises.</summary>
    destructor Destroy; override;

    /// <summary>Runs the OEM negotiator's begin-session plan
    /// and starts the tester-present heartbeat on success.
    /// Idempotent for the same session type / ECU.</summary>
    /// <param name="ASessionType">UDS session type.</param>
    /// <param name="AEcuAddress">UDS request address; 0 leaves
    /// the adapter header untouched.</param>
    function BeginSession(ASessionType: TOBDSessionType;
      AEcuAddress: Word = 0): Boolean;

    /// <summary>Runs the OEM negotiator's end-session plan,
    /// stops the heartbeat and returns to <c>dssIdle</c>. Safe
    /// from any state.</summary>
    function EndSession: Boolean;

    /// <summary>Drives the UDS SecurityAccess exchange via
    /// <see cref="TOBDSecurityAccessClient"/>. Uses the OEM
    /// extension's seed-key registry by default; pass
    /// <c>AAlgorithm</c> to override per-call (production
    /// callers plug their NDA algorithm in here).</summary>
    /// <param name="ALevel">Seed-request level
    /// (odd byte).</param>
    /// <param name="AAlgorithm">Optional per-call algorithm
    /// override.</param>
    function UnlockSecurityAccess(ALevel: Byte;
      const AAlgorithm: IOBDSeedKeyAlgorithm = nil): Boolean;

    /// <summary>UDS ReadDataByIdentifier — returns the raw
    /// payload (everything past <c>62 hi lo</c>). Negative
    /// replies populate <see cref="LastError"/> and return
    /// <c>False</c>.</summary>
    function ReadDID(ADID: Word;
      out APayload: TBytes): Boolean; overload;

    /// <summary>ReadDID + OEM <c>DecodeDID</c> in one
    /// call.</summary>
    function ReadDID(ADID: Word;
      out ADecoded: string): Boolean; overload;

    /// <summary>RoutineControl 31 01 with optional input
    /// data. Returns the status payload (everything past
    /// <c>71 01 RID</c>).</summary>
    function StartRoutine(ARID: Word;
      const AInputData: TBytes;
      out AStatus: TBytes): Boolean;

    /// <summary>RoutineControl 31 02 — stop the named
    /// routine.</summary>
    function StopRoutine(ARID: Word): Boolean;

    /// <summary>RoutineControl 31 03 — request the routine's
    /// results.</summary>
    function RequestRoutineResults(ARID: Word;
      out AStatus: TBytes): Boolean;

    /// <summary>Current state.</summary>
    function State: TOBDDiagSessionState;

    /// <summary>Most recent failure detail; cleared on each
    /// successful high-level call.</summary>
    property LastError: string read FLastError;
    /// <summary>The OEM extension this session was bound
    /// to.</summary>
    property OEM: IOBDOEMExtension read FOEM;
    /// <summary>Bound protocol (read-only).</summary>
    property Protocol: TOBDProtocol read FProtocol;
  end;

implementation

uses
  OBD.UDS.NRC;

constructor TOBDDiagSession.Create(AProtocol: TOBDProtocol;
  const AExtension: IOBDOEMExtension);
begin
  if AProtocol = nil then
    raise EOBDDiagSessionError.Create(
      'TOBDDiagSession requires a non-nil protocol');
  if AExtension = nil then
    raise EOBDDiagSessionError.Create(
      'TOBDDiagSession requires a non-nil OEM extension');
  inherited Create;
  FProtocol := AProtocol;
  FOEM := AExtension;
  FRunner := TOBDSessionRunner.Create(AProtocol);
  FSecurity := TOBDSecurityAccessClient.Create(
    AProtocol, AExtension.SeedKeyRegistry);
  FState := dssIdle;
  FActiveSession := sstDefault;
  FLock := TCriticalSection.Create;
end;

destructor TOBDDiagSession.Destroy;
begin
  StopHeartbeat;
  // Best-effort exit — never raise from the destructor; the
  // runner / protocol may already be gone.
  try
    EndSession;
  except
    // Swallow — destructor contract is "no throw".
  end;
  FSecurity.Free;
  FRunner.Free;
  FLock.Free;
  FState := dssClosed;
  FOEM := nil;
  inherited;
end;

procedure TOBDDiagSession.StopHeartbeat;
begin
  if FHeartbeat = nil then
    Exit;
  try
    FHeartbeat.StopGracefully;
  finally
    FreeAndNil(FHeartbeat);
  end;
end;

function TOBDDiagSession.CheckResponse(const Resp: TOBDResponse;
  const Operation: string): Boolean;
begin
  if Resp.IsNegative then
  begin
    FLastError := Format('%s rejected: %s',
      [Operation, FormatNRC(Resp.NRC)]);
    Exit(False);
  end;
  FLastError := '';
  Result := True;
end;

function TOBDDiagSession.BeginSession(
  ASessionType: TOBDSessionType;
  AEcuAddress: Word): Boolean;
var
  Plan: TOBDSessionPlan;
  RunResult: TOBDSessionRunResult;
begin
  FLock.Enter;
  try
    if (FState in [dssActive, dssActiveUnlocked]) and
       (FActiveSession = ASessionType) and
       (FActiveECU = AEcuAddress) then
      Exit(True);
    StopHeartbeat;

    Plan := FOEM.SessionNegotiator.BeginSessionPlan(
      ASessionType, AEcuAddress);
    RunResult := FRunner.Execute(Plan);
    if not RunResult.Success then
    begin
      if Length(RunResult.Steps) > 0 then
        FLastError :=
          RunResult.Steps[High(RunResult.Steps)].ErrorMessage
      else
        FLastError := 'BeginSession failed before any step ran';
      Exit(False);
    end;

    FActiveSession := ASessionType;
    FActiveECU := AEcuAddress;
    if ASessionType = sstDefault then
      FState := dssIdle
    else
      FState := dssActive;
    FUnlockedLevel := 0;
    FHeartbeat := FRunner.StartTesterPresent(Plan);
    FLastError := '';
    Result := True;
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.EndSession: Boolean;
var
  Plan: TOBDSessionPlan;
  RunResult: TOBDSessionRunResult;
begin
  FLock.Enter;
  try
    StopHeartbeat;
    if FState = dssIdle then
    begin
      FLastError := '';
      Exit(True);
    end;
    Plan := FOEM.SessionNegotiator.EndSessionPlan(FActiveECU);
    RunResult := FRunner.Execute(Plan);
    FState := dssIdle;
    FActiveSession := sstDefault;
    FUnlockedLevel := 0;
    Result := RunResult.Success;
    if not Result and (Length(RunResult.Steps) > 0) then
      FLastError :=
        RunResult.Steps[High(RunResult.Steps)].ErrorMessage
    else if Result then
      FLastError := '';
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.UnlockSecurityAccess(ALevel: Byte;
  const AAlgorithm: IOBDSeedKeyAlgorithm): Boolean;
var
  SARes: TOBDSecurityAccessResult;
  Registry: TOBDSeedKeyRegistry;
begin
  FLock.Enter;
  try
    if not (FState in [dssActive, dssActiveUnlocked]) then
    begin
      FLastError :=
        'UnlockSecurityAccess requires an active non-default session';
      Exit(False);
    end;
    if Assigned(AAlgorithm) then
    begin
      // Per-call override: stage the override into the registry
      // for the duration of the call so the SecurityAccess
      // client picks it up.
      Registry := FOEM.SeedKeyRegistry;
      Registry.RegisterAlgorithm(ALevel, AAlgorithm);
      try
        SARes := FSecurity.Unlock(ALevel);
      finally
        Registry.UnregisterAlgorithm(ALevel, AAlgorithm);
      end;
    end
    else
      SARes := FSecurity.Unlock(ALevel);

    case SARes.Outcome of
      saoUnlocked, saoAlreadyUnlocked:
        begin
          FUnlockedLevel := ALevel;
          FState := dssActiveUnlocked;
          FLastError := '';
          Exit(True);
        end;
    else
      FLastError := SARes.Message;
      Exit(False);
    end;
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.ReadDID(ADID: Word;
  out APayload: TBytes): Boolean;
var
  Resp: TOBDResponse;
begin
  FLock.Enter;
  try
    APayload := nil;
    try
      Resp := FProtocol.Request($22,
        TBytes.Create(Byte(ADID shr 8), Byte(ADID and $FF)));
    except
      on E: Exception do
      begin
        FLastError := Format('ReadDID 0x%.4X: %s',
          [ADID, E.Message]);
        Exit(False);
      end;
    end;
    if not CheckResponse(Resp,
        Format('ReadDID 0x%.4X', [ADID])) then
      Exit(False);
    // The protocol decoder strips the SID and echoed DID, so
    // Data is the payload directly.
    APayload := Resp.Data;
    Result := True;
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.ReadDID(ADID: Word;
  out ADecoded: string): Boolean;
var
  Bytes: TBytes;
begin
  ADecoded := '';
  Result := ReadDID(ADID, Bytes);
  if Result then
    ADecoded := FOEM.DecodeDID(ADID, Bytes);
end;

function TOBDDiagSession.StartRoutine(ARID: Word;
  const AInputData: TBytes; out AStatus: TBytes): Boolean;
var
  Request, ReplyBytes: TBytes;
  Resp: TOBDResponse;
begin
  FLock.Enter;
  try
    AStatus := nil;
    Request := BuildStartRoutine(ARID, AInputData);
    // BuildStartRoutine returns the full frame [31 01 hi lo …];
    // strip the SID for the protocol's typed Request call so
    // the response decoder still produces a typed result with
    // matching ServiceID.
    try
      Resp := FProtocol.Request(Request[0],
        Copy(Request, 1, Length(Request) - 1));
    except
      on E: Exception do
      begin
        FLastError := Format('StartRoutine 0x%.4X: %s',
          [ARID, E.Message]);
        Exit(False);
      end;
    end;
    if not CheckResponse(Resp,
        Format('StartRoutine 0x%.4X', [ARID])) then
      Exit(False);
    // ParseRoutineResponse expects the on-wire frame with the
    // positive-response SID — reassemble [71 SF hi lo data…].
    SetLength(ReplyBytes, 1 + Length(Resp.Data));
    ReplyBytes[0] := Resp.ServiceID;
    if Length(Resp.Data) > 0 then
      Move(Resp.Data[0], ReplyBytes[1], Length(Resp.Data));
    try
      AStatus := ParseRoutineResponse(ReplyBytes, rcStart, ARID);
      Result := True;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        Result := False;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.StopRoutine(ARID: Word): Boolean;
var
  Request, ReplyBytes: TBytes;
  Resp: TOBDResponse;
  Discard: TBytes;
begin
  FLock.Enter;
  try
    Request := BuildStopRoutine(ARID);
    try
      Resp := FProtocol.Request(Request[0],
        Copy(Request, 1, Length(Request) - 1));
    except
      on E: Exception do
      begin
        FLastError := Format('StopRoutine 0x%.4X: %s',
          [ARID, E.Message]);
        Exit(False);
      end;
    end;
    if not CheckResponse(Resp,
        Format('StopRoutine 0x%.4X', [ARID])) then
      Exit(False);
    SetLength(ReplyBytes, 1 + Length(Resp.Data));
    ReplyBytes[0] := Resp.ServiceID;
    if Length(Resp.Data) > 0 then
      Move(Resp.Data[0], ReplyBytes[1], Length(Resp.Data));
    try
      Discard := ParseRoutineResponse(ReplyBytes, rcStop, ARID);
      // Discard intentional — Stop has no payload of interest.
      Discard := nil;
      Result := True;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        Result := False;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.RequestRoutineResults(ARID: Word;
  out AStatus: TBytes): Boolean;
var
  Request, ReplyBytes: TBytes;
  Resp: TOBDResponse;
begin
  FLock.Enter;
  try
    AStatus := nil;
    Request := BuildRequestRoutineResults(ARID);
    try
      Resp := FProtocol.Request(Request[0],
        Copy(Request, 1, Length(Request) - 1));
    except
      on E: Exception do
      begin
        FLastError := Format('RequestRoutineResults 0x%.4X: %s',
          [ARID, E.Message]);
        Exit(False);
      end;
    end;
    if not CheckResponse(Resp,
        Format('RequestRoutineResults 0x%.4X', [ARID])) then
      Exit(False);
    SetLength(ReplyBytes, 1 + Length(Resp.Data));
    ReplyBytes[0] := Resp.ServiceID;
    if Length(Resp.Data) > 0 then
      Move(Resp.Data[0], ReplyBytes[1], Length(Resp.Data));
    try
      AStatus := ParseRoutineResponse(ReplyBytes,
        rcRequestResults, ARID);
      Result := True;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        Result := False;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.State: TOBDDiagSessionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

end.
