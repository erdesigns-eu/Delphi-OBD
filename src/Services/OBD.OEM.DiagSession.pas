//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.DiagSession.pas
// CONTENTS       : High-level diagnostic-session wrapper. Stitches
//                  the OEM session negotiator, seed-key registry,
//                  DID catalog, and RoutineControl framework into
//                  one stateful workflow with the failure modes a
//                  tool-builder actually cares about.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The wrapper owns the heartbeat thread, so callers
//                  don't have to remember Stop+Free lifecycles. Each
//                  high-level call returns a boolean for the simple
//                  case and exposes the last detailed error via
//                  <c>LastError</c> when the boolean is False.
//------------------------------------------------------------------------------
unit OBD.OEM.DiagSession;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  OBD.Connection.Async, OBD.OEM, OBD.OEM.Session, OBD.OEM.Session.Runner,
  OBD.OEM.SeedKey, OBD.OEM.RoutineControl;

type
  EOBDDiagSessionError = class(Exception);

  TOBDDiagSessionState = (
    dssIdle,                 // Default session, no heartbeat
    dssActive,               // Non-default session running with heartbeat
    dssActiveUnlocked,       // Non-default + SecurityAccess passed
    dssClosed                // Free has been called; reuse forbidden
  );

  TOBDDiagSession = class
  strict private
    FConnection: TOBDConnectionAsync;
    FOEM: IOBDOEMExtension;
    FRunner: TOBDSessionRunner;
    FHeartbeat: TOBDTesterPresentThread;
    FState: TOBDDiagSessionState;
    FActiveSession: TOBDSessionType;
    FActiveECU: Word;
    FUnlockedLevel: Byte;
    FLastError: string;
    FLock: TCriticalSection;
    procedure StopHeartbeat;
    function ResolveSeedKey(const Level: Byte;
      const Override_: IOBDSeedKeyAlgorithm): IOBDSeedKeyAlgorithm;
    function AwaitOBD(const HexCommand: string;
      const TimeoutMs: Cardinal = 5000): TBytes;
  public
    constructor Create(const Conn: TOBDConnectionAsync;
      const Ext: IOBDOEMExtension);
    destructor Destroy; override;

    /// <summary>Run the OEM negotiator's begin-session plan. Starts
    /// the tester-present heartbeat on success. Idempotent for the
    /// same session type — calling twice with the same args is a
    /// no-op.</summary>
    function BeginSession(const SessionType: TOBDSessionType;
      const ECUAddress: Word = 0): Boolean;

    /// <summary>Run the OEM negotiator's end-session plan. Stops the
    /// heartbeat, returns to <c>dssIdle</c>. Safe to call from any
    /// state.</summary>
    function EndSession: Boolean;

    /// <summary>UDS SecurityAccess: 27 LL → 67 LL SEED → 27 LL+1 KEY.
    /// Uses the OEM extension's seed-key registry by default; the
    /// optional <c>Algorithm</c> parameter overrides it (production
    /// users plug their NDA-protected algorithm in here).</summary>
    function UnlockSecurityAccess(const Level: Byte;
      const Algorithm: IOBDSeedKeyAlgorithm = nil): Boolean;

    /// <summary>UDS ReadDataByIdentifier (22 HiDID LoDID). Returns
    /// the raw payload (everything past <c>62 HiDID LoDID</c>).
    /// Negative replies set <c>LastError</c> and return False.</summary>
    function ReadDID(const DID: Word; out Payload: TBytes): Boolean; overload;

    /// <summary>Read + decode a DID via the OEM's <c>DecodeDID</c>.
    /// Useful for tool UIs that just want the human-readable string.</summary>
    function ReadDID(const DID: Word; out Decoded: string): Boolean; overload;

    /// <summary>Run RoutineControl 31 01 with optional input data.
    /// Returns the status payload (everything past <c>71 01 RID</c>).</summary>
    function StartRoutine(const RID: Word;
      const InputData: TBytes; out Status: TBytes): Boolean;

    /// <summary>RoutineControl 31 02 — stop the named routine.</summary>
    function StopRoutine(const RID: Word): Boolean;

    /// <summary>RoutineControl 31 03 — request the routine's results.</summary>
    function RequestRoutineResults(const RID: Word;
      out Status: TBytes): Boolean;

    /// <summary>State accessor.</summary>
    function State: TOBDDiagSessionState;

    /// <summary>Most recent failure detail; cleared on each successful
    /// high-level call.</summary>
    property LastError: string read FLastError;

    /// <summary>The OEM extension this session was bound to.</summary>
    property OEM: IOBDOEMExtension read FOEM;
  end;

implementation

uses
  OBD.OEM.Coding, OBD.Async;

constructor TOBDDiagSession.Create(const Conn: TOBDConnectionAsync;
  const Ext: IOBDOEMExtension);
begin
  if Conn = nil then
    raise EOBDDiagSessionError.Create('TOBDDiagSession requires a connection');
  if Ext = nil then
    raise EOBDDiagSessionError.Create('TOBDDiagSession requires an OEM extension');
  inherited Create;
  FConnection := Conn;
  FOEM := Ext;
  FRunner := TOBDSessionRunner.Create(Conn);
  FState := dssIdle;
  FActiveSession := sstDefault;
  FLock := TCriticalSection.Create;
end;

destructor TOBDDiagSession.Destroy;
begin
  StopHeartbeat;
  // Best-effort exit — never throw from the destructor; if the
  // connection is already gone the runner will swallow the error.
  try EndSession; except end;
  FRunner.Free;
  FLock.Free;
  FState := dssClosed;
  FOEM := nil;
  inherited;
end;

procedure TOBDDiagSession.StopHeartbeat;
begin
  if FHeartbeat = nil then Exit;
  try
    FHeartbeat.StopGracefully;
  finally
    FreeAndNil(FHeartbeat);
  end;
end;

function TOBDDiagSession.ResolveSeedKey(const Level: Byte;
  const Override_: IOBDSeedKeyAlgorithm): IOBDSeedKeyAlgorithm;
begin
  if Assigned(Override_) then Exit(Override_);
  Result := FOEM.SeedKeyRegistry.Find(Level);
end;

function TOBDDiagSession.AwaitOBD(const HexCommand: string;
  const TimeoutMs: Cardinal): TBytes;
var
  Future: IOBDFuture<string>;
  Reply: string;
begin
  Future := FConnection.OBDAsync(HexCommand, TimeoutMs);
  Reply := Future.Await(TimeoutMs + 500);
  Result := HexStringToBytes(Reply);
end;

function FormatBytes(const Bytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Bytes) do
  begin
    if I > 0 then Result := Result + ' ';
    Result := Result + IntToHex(Bytes[I], 2);
  end;
end;

//==============================================================================
//  BeginSession / EndSession
//==============================================================================
function TOBDDiagSession.BeginSession(const SessionType: TOBDSessionType;
  const ECUAddress: Word): Boolean;
var
  Plan: TOBDSessionPlan;
  RunResult: TOBDSessionRunResult;
begin
  FLock.Enter;
  try
    if (FState in [dssActive, dssActiveUnlocked]) and
       (FActiveSession = SessionType) and (FActiveECU = ECUAddress) then
      Exit(True);
    StopHeartbeat;

    Plan := FOEM.SessionNegotiator.BeginSessionPlan(SessionType, ECUAddress);
    RunResult := FRunner.Execute(Plan);
    if not RunResult.Success then
    begin
      if Length(RunResult.Steps) > 0 then
        FLastError := RunResult.Steps[High(RunResult.Steps)].ErrorMessage
      else
        FLastError := 'BeginSession failed before any step ran';
      Exit(False);
    end;

    FActiveSession := SessionType;
    FActiveECU := ECUAddress;
    if SessionType = sstDefault then
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
      FLastError := RunResult.Steps[High(RunResult.Steps)].ErrorMessage
    else if Result then
      FLastError := '';
  finally
    FLock.Leave;
  end;
end;

//==============================================================================
//  SecurityAccess
//==============================================================================
function TOBDDiagSession.UnlockSecurityAccess(const Level: Byte;
  const Algorithm: IOBDSeedKeyAlgorithm): Boolean;
var
  Algo: IOBDSeedKeyAlgorithm;
  SeedRequest, KeyRequest: TBytes;
  SeedReply, KeyReply: TBytes;
  Seed, Key: TBytes;
begin
  FLock.Enter;
  try
    if not (FState in [dssActive, dssActiveUnlocked]) then
    begin
      FLastError := 'UnlockSecurityAccess requires an active non-default session';
      Exit(False);
    end;
    Algo := ResolveSeedKey(Level, Algorithm);
    if Algo = nil then
    begin
      FLastError := Format(
        'No seed-key algorithm registered for level 0x%.2X', [Level]);
      Exit(False);
    end;

    SeedRequest := RequestSeedFrame(Level);
    SeedReply := AwaitOBD(FormatBytes(SeedRequest));
    if (Length(SeedReply) >= 3) and (SeedReply[0] = $7F) then
    begin
      FLastError := Format(
        'SecurityAccess seed request rejected with NRC 0x%.2X', [SeedReply[2]]);
      Exit(False);
    end;
    Seed := ExtractSeed(SeedReply, Level);
    Key := Algo.ComputeKey(Seed, Level);

    KeyRequest := SendKeyFrame(Level, Key);
    KeyReply := AwaitOBD(FormatBytes(KeyRequest));
    if (Length(KeyReply) >= 3) and (KeyReply[0] = $7F) then
    begin
      FLastError := Format(
        'SecurityAccess key send rejected with NRC 0x%.2X', [KeyReply[2]]);
      Exit(False);
    end;
    if (Length(KeyReply) < 2) or (KeyReply[0] <> $67) or
       (KeyReply[1] <> Level + 1) then
    begin
      FLastError := Format(
        'SecurityAccess key send returned unexpected reply: %s',
        [FormatBytes(KeyReply)]);
      Exit(False);
    end;

    FUnlockedLevel := Level;
    FState := dssActiveUnlocked;
    FLastError := '';
    Result := True;
  finally
    FLock.Leave;
  end;
end;

//==============================================================================
//  ReadDataByIdentifier
//==============================================================================
function TOBDDiagSession.ReadDID(const DID: Word; out Payload: TBytes): Boolean;
var
  Reply: TBytes;
  Request: TBytes;
begin
  FLock.Enter;
  try
    Payload := nil;
    Request := TBytes.Create($22, Byte(DID shr 8), Byte(DID and $FF));
    Reply := AwaitOBD(FormatBytes(Request));
    if (Length(Reply) >= 3) and (Reply[0] = $7F) then
    begin
      FLastError := Format(
        'ReadDID 0x%.4X rejected with NRC 0x%.2X', [DID, Reply[2]]);
      Exit(False);
    end;
    if (Length(Reply) < 3) or (Reply[0] <> $62) or
       (Reply[1] <> Byte(DID shr 8)) or (Reply[2] <> Byte(DID and $FF)) then
    begin
      FLastError := Format(
        'ReadDID 0x%.4X reply mismatched: %s',
        [DID, FormatBytes(Reply)]);
      Exit(False);
    end;
    Payload := Copy(Reply, 3, Length(Reply) - 3);
    FLastError := '';
    Result := True;
  finally
    FLock.Leave;
  end;
end;

function TOBDDiagSession.ReadDID(const DID: Word;
  out Decoded: string): Boolean;
var
  Bytes: TBytes;
begin
  Decoded := '';
  Result := ReadDID(DID, Bytes);
  if Result then
    Decoded := FOEM.DecodeDID(DID, Bytes);
end;

//==============================================================================
//  RoutineControl
//==============================================================================
function TOBDDiagSession.StartRoutine(const RID: Word;
  const InputData: TBytes; out Status: TBytes): Boolean;
var
  Request, Reply: TBytes;
begin
  FLock.Enter;
  try
    Status := nil;
    Request := BuildStartRoutine(RID, InputData);
    Reply := AwaitOBD(FormatBytes(Request));
    try
      Status := ParseRoutineResponse(Reply, rcStart, RID);
      FLastError := '';
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

function TOBDDiagSession.StopRoutine(const RID: Word): Boolean;
var
  Request, Reply, Discard: TBytes;
begin
  FLock.Enter;
  try
    Request := BuildStopRoutine(RID);
    Reply := AwaitOBD(FormatBytes(Request));
    try
      Discard := ParseRoutineResponse(Reply, rcStop, RID);
      Discard := nil; // intentionally unused
      FLastError := '';
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

function TOBDDiagSession.RequestRoutineResults(const RID: Word;
  out Status: TBytes): Boolean;
var
  Request, Reply: TBytes;
begin
  FLock.Enter;
  try
    Status := nil;
    Request := BuildRequestRoutineResults(RID);
    Reply := AwaitOBD(FormatBytes(Request));
    try
      Status := ParseRoutineResponse(Reply, rcRequestResults, RID);
      FLastError := '';
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
  try Result := FState; finally FLock.Leave; end;
end;

end.
