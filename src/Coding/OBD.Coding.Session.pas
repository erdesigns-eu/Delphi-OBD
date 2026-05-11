//------------------------------------------------------------------------------
//  OBD.Coding.Session
//
//  TOBDCodingSession — orchestrator that wraps a coding apply
//  into a hardware-recoverable transaction:
//
//    1. Snapshot — read current value(s)
//    2. Write — apply new value(s)
//    3. Verify — read back and compare
//    4. Rollback — on verify-fail or write-fail, write the
//                       snapshot back
//
//  Hosts feed the orchestrator a list of (DID, NewBytes) tuples.
//  Every step writes an audit-log entry when an
//  <see cref="AuditLog"/> is attached.
//
//  Like every write-side component in this package, the session
//  defaults <c>AutoExecute = False</c> and propagates the gate to
//  the underlying TOBDDataIdentifierIO.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.Session;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol,
  OBD.Coding.DataIdentifierIO,
  OBD.Coding.AuditLog,
  OBD.Coding.Diff;

type
  /// <summary>One coding step.</summary>
  TOBDCodingStep = record
    DID: Word;
    /// <summary>New value to write.</summary>
    NewValue: TBytes;
    /// <summary>Captured snapshot — populated by <c>Snapshot</c>.</summary>
    OldValue: TBytes;
  end;

  /// <summary>Outcome bucket for one session.</summary>
  TOBDCodingOutcome = (
    coNotStarted, coCommitted, coRolledBack, coFailed
  );

  /// <summary>Fires per step.</summary>
  TOBDCodingStepEvent = procedure(Sender: TObject;
    AIndex, ACount: Integer; const AStep: TOBDCodingStep) of object;

  /// <summary>Coding-session orchestrator.</summary>
  TOBDCodingSession = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FRollbackOnFail: Boolean;
    FVerifyAfterWrite: Boolean;
    FDryRun: Boolean;
    FAuditLog: TOBDCodingAuditLog;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOutcome: TOBDCodingOutcome;
    FOnStepWritten: TOBDCodingStepEvent;
    FOnStepVerified: TOBDCodingStepEvent;
    FOnRollback: TOBDCodingStepEvent;
    FOnComplete: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoApply(var ASteps: TArray<TOBDCodingStep>);
    procedure SnapshotStep(var AStep: TOBDCodingStep;
      AIO: TOBDDataIdentifierIO);
    procedure WriteStep(var AStep: TOBDCodingStep;
      AIO: TOBDDataIdentifierIO);
    procedure VerifyStep(var AStep: TOBDCodingStep;
      AIO: TOBDDataIdentifierIO);
    procedure RollbackStep(const AStep: TOBDCodingStep;
      AIO: TOBDDataIdentifierIO);
    procedure WriteAudit(AKind: TOBDAuditKind; ATarget: string;
      const ABefore, AAfter: TBytes; const AMessage: string);
    procedure FireStepWritten(AIndex, ACount: Integer;
      const AStep: TOBDCodingStep);
    procedure FireStepVerified(AIndex, ACount: Integer;
      const AStep: TOBDCodingStep);
    procedure FireRollback(AIndex, ACount: Integer;
      const AStep: TOBDCodingStep);
    procedure FireComplete;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure SetAuditLog(AValue: TOBDCodingAuditLog);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Runs the snapshot → write → verify → (rollback)
    /// transaction synchronously.</summary>
    /// <param name="ASteps">In/out: <c>OldValue</c> is populated
    /// during snapshot; the array is mutated in place.</param>
    procedure Apply(var ASteps: TArray<TOBDCodingStep>);
    /// <summary>Non-blocking <see cref="Apply"/>.</summary>
    procedure ApplyAsync(const ASteps: TArray<TOBDCodingStep>);

    /// <summary>Outcome of the last <c>Apply</c>.</summary>
    property Outcome: TOBDCodingOutcome read FOutcome;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property AuditLog: TOBDCodingAuditLog read FAuditLog write SetAuditLog;
    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    /// <summary>Rollback automatically when verify fails. Default
    /// <c>True</c>.</summary>
    property RollbackOnFail: Boolean read FRollbackOnFail
      write FRollbackOnFail default True;
    /// <summary>Run the read-back / compare step. Default
    /// <c>True</c>.</summary>
    property VerifyAfterWrite: Boolean read FVerifyAfterWrite
      write FVerifyAfterWrite default True;
    /// <summary>Dry-run mode. When <c>True</c>, the session
    /// snapshots, emits the audit-log trail with each entry's
    /// <c>kind</c> set to the appropriate stage, but does NOT
    /// touch the wire for write / verify / rollback. Useful for
    /// "review before commit" UIs. Default <c>False</c>.</summary>
    property DryRun: Boolean read FDryRun write FDryRun default False;

    property OnStepWritten: TOBDCodingStepEvent read FOnStepWritten
      write FOnStepWritten;
    property OnStepVerified: TOBDCodingStepEvent read FOnStepVerified
      write FOnStepVerified;
    property OnRollback: TOBDCodingStepEvent read FOnRollback
      write FOnRollback;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDCodingSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FRollbackOnFail := True;
  FVerifyAfterWrite := True;
end;

destructor TOBDCodingSession.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDCodingSession.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDCodingSession.SetAuditLog(AValue: TOBDCodingAuditLog);
begin
  if FAuditLog = AValue then Exit;
  if FAuditLog <> nil then FAuditLog.RemoveFreeNotification(Self);
  FAuditLog := AValue;
  if FAuditLog <> nil then FAuditLog.FreeNotification(Self);
end;

procedure TOBDCodingSession.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FProtocol then FProtocol := nil;
    if AComponent = FAuditLog then FAuditLog := nil;
  end;
end;

procedure TOBDCodingSession.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDCodingSession: apply already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDCodingSession.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDCodingSession.WriteAudit(AKind: TOBDAuditKind;
  ATarget: string; const ABefore, AAfter: TBytes; const AMessage: string);
var
  Entry: TOBDAuditEntry;
begin
  if FAuditLog = nil then Exit;
  Entry := Default(TOBDAuditEntry);
  Entry.Timestamp := Now;
  Entry.SessionID := FAuditLog.SessionID;
  Entry.Kind := AKind;
  Entry.Target := ATarget;
  Entry.Before := ABefore;
  Entry.After := AAfter;
  Entry.Message := AMessage;
  FAuditLog.Append(Entry);
end;

procedure TOBDCodingSession.SnapshotStep(var AStep: TOBDCodingStep;
  AIO: TOBDDataIdentifierIO);
begin
  AStep.OldValue := AIO.ReadOne(AStep.DID);
  WriteAudit(akSnapshot, Format('0x%4.4X', [AStep.DID]),
    AStep.OldValue, nil, '');
end;

procedure TOBDCodingSession.WriteStep(var AStep: TOBDCodingStep;
  AIO: TOBDDataIdentifierIO);
var
  Note: string;
begin
  if FDryRun then
  begin
    Note := 'dry-run';
  end
  else
  begin
    AIO.Write(AStep.DID, AStep.NewValue);
    Note := '';
  end;
  WriteAudit(akWrite, Format('0x%4.4X', [AStep.DID]),
    AStep.OldValue, AStep.NewValue, Note);
end;

procedure TOBDCodingSession.VerifyStep(var AStep: TOBDCodingStep;
  AIO: TOBDDataIdentifierIO);
var
  ReadBack: TBytes;
  Diff: TOBDCodingDiffResult;
begin
  if FDryRun then
  begin
    WriteAudit(akVerify, Format('0x%4.4X', [AStep.DID]),
      nil, AStep.NewValue, 'dry-run (verify skipped)');
    Exit;
  end;
  ReadBack := AIO.ReadOne(AStep.DID);
  Diff := TOBDCodingDiff.Compute(ReadBack, AStep.NewValue);
  if Length(Diff.Changes) <> 0 then
    raise EOBDProtocolErr.CreateFmt(
      'Coding verify failed for DID 0x%4.4X (%d byte(s) differ)',
      [AStep.DID, Length(Diff.Changes)]);
  WriteAudit(akVerify, Format('0x%4.4X', [AStep.DID]),
    nil, ReadBack, 'verified');
end;

procedure TOBDCodingSession.RollbackStep(const AStep: TOBDCodingStep;
  AIO: TOBDDataIdentifierIO);
begin
  if Length(AStep.OldValue) = 0 then Exit;
  if FDryRun then
  begin
    WriteAudit(akRollback, Format('0x%4.4X', [AStep.DID]),
      AStep.NewValue, AStep.OldValue, 'dry-run (no wire access)');
    Exit;
  end;
  AIO.Write(AStep.DID, AStep.OldValue);
  WriteAudit(akRollback, Format('0x%4.4X', [AStep.DID]),
    AStep.NewValue, AStep.OldValue, 'restored');
end;

procedure TOBDCodingSession.DoApply(var ASteps: TArray<TOBDCodingStep>);
var
  I, J: Integer;
  IO: TOBDDataIdentifierIO;
  WrittenSoFar: Integer;
  ApplyError: string;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDCodingSession: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDCodingSession: AutoExecute is False — set it explicitly');
  if Length(ASteps) = 0 then
    raise EOBDConfig.Create('TOBDCodingSession: empty step list');

  IO := TOBDDataIdentifierIO.Create(nil);
  WrittenSoFar := 0;
  ApplyError := '';
  try
    IO.Protocol := FProtocol;
    IO.AutoExecute := True;

    // 1. Snapshot every step first so a write-failure mid-batch
    //    can roll back even the steps we never reached.
    for I := 0 to High(ASteps) do
      SnapshotStep(ASteps[I], IO);

    try
      // 2. Write.
      for I := 0 to High(ASteps) do
      begin
        WriteStep(ASteps[I], IO);
        Inc(WrittenSoFar);
        FireStepWritten(I + 1, Length(ASteps), ASteps[I]);
      end;

      // 3. Verify.
      if FVerifyAfterWrite then
      begin
        for I := 0 to High(ASteps) do
        begin
          VerifyStep(ASteps[I], IO);
          FireStepVerified(I + 1, Length(ASteps), ASteps[I]);
        end;
      end;

      FOutcome := coCommitted;
      WriteAudit(akInfo, '', nil, nil, 'commit');
      FireComplete;
    except
      on E: Exception do
      begin
        ApplyError := E.Message;
        WriteAudit(akError, '', nil, nil, ApplyError);
        if FRollbackOnFail then
        begin
          FOutcome := coRolledBack;
          // Walk back over written steps in reverse.
          for J := WrittenSoFar - 1 downto 0 do
          begin
            try
              RollbackStep(ASteps[J], IO);
              FireRollback(J + 1, Length(ASteps), ASteps[J]);
            except
              on E2: Exception do
                WriteAudit(akError,
                  Format('rollback 0x%4.4X', [ASteps[J].DID]),
                  nil, nil, E2.Message);
            end;
          end;
        end
        else
          FOutcome := coFailed;
        raise;
      end;
    end;
  finally
    IO.Free;
  end;
end;

procedure TOBDCodingSession.Apply(var ASteps: TArray<TOBDCodingStep>);
begin
  FOutcome := coNotStarted;
  DoApply(ASteps);
end;

procedure TOBDCodingSession.ApplyAsync(const ASteps: TArray<TOBDCodingStep>);
var
  Self_: TOBDCodingSession;
  StepsCopy: TArray<TOBDCodingStep>;
begin
  GuardSingleAsync;
  Self_ := Self;
  SetLength(StepsCopy, Length(ASteps));
  for var I := 0 to High(ASteps) do StepsCopy[I] := ASteps[I];
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.FOutcome := coNotStarted;
          Self_.DoApply(StepsCopy);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDCodingSession.FireStepWritten(AIndex, ACount: Integer;
  const AStep: TOBDCodingStep);
var
  Self_: TOBDCodingSession; I, C: Integer; S: TOBDCodingStep;
begin
  if not Assigned(FOnStepWritten) then Exit;
  Self_ := Self; I := AIndex; C := ACount; S := AStep;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnStepWritten(Self_, I, C, S)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnStepWritten) then
        Self_.FOnStepWritten(Self_, I, C, S);
    end);
end;

procedure TOBDCodingSession.FireStepVerified(AIndex, ACount: Integer;
  const AStep: TOBDCodingStep);
var
  Self_: TOBDCodingSession; I, C: Integer; S: TOBDCodingStep;
begin
  if not Assigned(FOnStepVerified) then Exit;
  Self_ := Self; I := AIndex; C := ACount; S := AStep;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnStepVerified(Self_, I, C, S)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnStepVerified) then
        Self_.FOnStepVerified(Self_, I, C, S);
    end);
end;

procedure TOBDCodingSession.FireRollback(AIndex, ACount: Integer;
  const AStep: TOBDCodingStep);
var
  Self_: TOBDCodingSession; I, C: Integer; S: TOBDCodingStep;
begin
  if not Assigned(FOnRollback) then Exit;
  Self_ := Self; I := AIndex; C := ACount; S := AStep;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRollback(Self_, I, C, S)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnRollback) then
        Self_.FOnRollback(Self_, I, C, S);
    end);
end;

procedure TOBDCodingSession.FireComplete;
var
  Self_: TOBDCodingSession;
begin
  if not Assigned(FOnComplete) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnComplete(Self_)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnComplete) then Self_.FOnComplete(Self_);
    end);
end;

procedure TOBDCodingSession.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDCodingSession; Code: TOBDErrorCode; Msg: string;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
