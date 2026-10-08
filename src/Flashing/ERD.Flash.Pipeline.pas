//------------------------------------------------------------------------------
//  ERD.Flash.Pipeline
//
//  TOBDFlashPipeline — production-grade ECU reflash orchestrator.
//  Composes the flashing building blocks into one safe-by-default
//  end-to-end run:
//
//    1. fpPreflight — host-supplied checks (engine off,
//                            voltage floor, ambient temp, ignition,
//                            user confirmation)
//    2. fpVerifyImage — image hash / signature verification
//    3. fpEnterProgramming — host-supplied entry routine (session
//                            switch + security-access + erase)
//    4. fpTransfer — TOBDUDSTransfer
//    5. fpVerify — host-supplied verify routine
//    6. fpReset — ECUReset hardReset
//    7. fpFinalise — close audit log
//
//  SAFETY — BRICK RISK ----------------------------------------------------
//  This is the dangerous one. Defaults <c>AutoExecute = False</c>;
//  fires <c>OnConfirmExecute(var Allow)</c> on the main thread;
//  a missing handler with <c>AutoExecute = False</c> aborts the
//  flash with <c>EOBDConfig</c>.
//
//  A pipeline with no <c>VoltageGate</c> assigned logs a WARN-
//  severity audit-log entry at start-of-flash but proceeds — by
//  design, the package surfaces the safety primitive but does
//  not force it on you. Wire one. Read docs/flashing-safety.md
//  before integrating.
//  ------------------------------------------------------------------------
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Flash.Pipeline;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IF FPC_FULLVERSION >= 30301}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}

interface

uses
  ERD.Async.Task,
  ERD.Connection,
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  {$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  {$IFDEF FPC}DateUtils{$ELSE}System.DateUtils{$ENDIF},
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Protocol,
  ERD.Coding.AuditLog,
  ERD.UDS.Transfer,
  ERD.Flash.VoltageGate,
  ERD.Flash.Phases,
  ERD.Flash.Checkpoint;

type
  /// <summary>Argument record for <c>OnConfirmExecute</c>.</summary>
  TOBDFlashConfirmEvent = procedure(Sender: TObject;
    AAddress: UInt64; ASize: UInt32; var AAllow: Boolean) of object;

  /// <summary>Argument record for <c>OnPhaseChange</c>.</summary>
  TOBDFlashPhaseEvent = procedure(Sender: TObject;
    APhase: TOBDFlashPhase) of object;

  /// <summary>Argument record for <c>OnCheckResult</c>.</summary>
  TOBDFlashCheckResultEvent = procedure(Sender: TObject;
    const AResult: TOBDCheckResult) of object;

  /// <summary>Host-supplied step (entry / verify routines).</summary>
  TOBDFlashStepFunc = reference to procedure;

  /// <summary>
  ///   Orchestrator that drives a UDS reflash through the full
  ///   safety pipeline.
  /// </summary>
  TOBDFlashPipeline = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAuditLog: TOBDCodingAuditLog;
    FVoltageGate: TOBDVoltageGate;
    FTransfer: TOBDUDSTransfer;
    FChecks: TOBDFlashCheckList;
    FOwnedTask: TOBDOwnedTask;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FCheckpointFile: string;
    FTargetVendor, FTargetModule, FECUIdentity, FSessionID: string;
    FResuming: Boolean;
    FResumeInfo: TOBDFlashCheckpointInfo;
    FResumeValidator: TOBDResumeValidator;
    FResetAfterFlash: Boolean;
    FAddressFormatBytes: Byte;
    FLengthFormatBytes: Byte;
    FOnConfirmExecute: TOBDFlashConfirmEvent;
    FOnPhaseChange: TOBDFlashPhaseEvent;
    FOnCheckResult: TOBDFlashCheckResultEvent;
    FOnComplete: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    FEnterProgramming: TOBDFlashStepFunc;
    FVerifyStep: TOBDFlashStepFunc;
    FVoltageGateAborted: Integer;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure SetAuditLog(AValue: TOBDCodingAuditLog);
    procedure SetVoltageGate(AValue: TOBDVoltageGate);
    function GetChecks: TOBDFlashCheckList;
    procedure WriteAudit(AKind: TOBDAuditKind; const ATarget: string;
      const AMessage: string; const ABefore: TBytes = nil;
      const AAfter: TBytes = nil);
    procedure FirePhase(APhase: TOBDFlashPhase);
    procedure FireCheckResult(const AResult: TOBDCheckResult);
    procedure FireComplete;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    function FireConfirm(AAddress: UInt64; ASize: UInt32): Boolean;
    procedure RunChecks(APhase: TOBDFlashPhase);
    procedure HandleVoltageAbort(Sender: TObject;
      AVoltage: Double; AReason: string);
    procedure DoFlash(AAddress: UInt64; const AImage: TBytes);
    procedure WriteCheckpointSafe(AAddress: UInt64;
      const AImage: TBytes; const ACursor: TOBDTransferCursor);
    procedure HandleBeforeRequest(Sender: TObject);
    procedure HandleProgress(Sender: TObject;
      const ACursor: TOBDTransferCursor);
    function FCurrentImage: TBytes; // for the progress hook
    function FCurrentAddress: UInt64;
    var
      FSavedImage: TBytes;
      FSavedImageHash: TBytes;
      FSavedAddress: UInt64;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Runs the full pipeline. Fires
    /// <c>OnConfirmExecute</c> first (gated by
    /// <c>AutoExecute</c>); aborts on any <c>csError</c> check
    /// failure or voltage-gate abort.</summary>
    procedure Flash(AAddress: UInt64; const AImage: TBytes);
    /// <summary>Non-blocking <see cref="Flash"/>.</summary>
    /// <summary>Resume with full preflight and image checks, without restarting programming/download. Host confirms the ECU is still in the saved transfer.</summary>
    procedure ResumeFromCheckpoint(const AFileName: string; const AImage: TBytes;
      const ASessionID: string; const AConfirmECU: TOBDResumeValidator);
    procedure FlashAsync(AAddress: UInt64; const AImage: TBytes);

    /// <summary>The check list. Hosts populate it before
    /// <c>Flash</c> with their preflight / verify-image checks.</summary>
    property Checks: TOBDFlashCheckList read GetChecks;
    /// <summary>Called between fpEnterProgramming entry and
    /// fpTransfer. Hosts wire their session-switch + security-
    /// access + erase routine here.</summary>
    property OnEnterProgramming: TOBDFlashStepFunc
      read FEnterProgramming write FEnterProgramming;
    /// <summary>Called during fpVerify. Hosts wire their
    /// post-flash verify routine here.</summary>
    property OnVerifyRoutine: TOBDFlashStepFunc
      read FVerifyStep write FVerifyStep;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property AuditLog: TOBDCodingAuditLog read FAuditLog write SetAuditLog;
    property VoltageGate: TOBDVoltageGate read FVoltageGate
      write SetVoltageGate;
    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    /// <summary>Optional checkpoint file path. When non-empty, the
    /// pipeline writes a checkpoint after every accepted chunk so
    /// a host can resume after a crash.</summary>
    property TargetVendor: string read FTargetVendor write FTargetVendor;
    property TargetModule: string read FTargetModule write FTargetModule;
    property ECUIdentity: string read FECUIdentity write FECUIdentity;
    property CheckpointFile: string read FCheckpointFile
      write FCheckpointFile;
    /// <summary>Send ECUReset hardReset as the final step.
    /// Default <c>True</c>.</summary>
    property ResetAfterFlash: Boolean read FResetAfterFlash
      write FResetAfterFlash default True;
    property AddressFormatBytes: Byte read FAddressFormatBytes
      write FAddressFormatBytes default 4;
    property LengthFormatBytes: Byte read FLengthFormatBytes
      write FLengthFormatBytes default 4;

    /// <summary>Fires before any wire access (main thread).</summary>
    property OnConfirmExecute: TOBDFlashConfirmEvent
      read FOnConfirmExecute write FOnConfirmExecute;
    property OnPhaseChange: TOBDFlashPhaseEvent
      read FOnPhaseChange write FOnPhaseChange;
    property OnCheckResult: TOBDFlashCheckResultEvent
      read FOnCheckResult write FOnCheckResult;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  {$IFDEF FPC}TypInfo{$ELSE}System.TypInfo{$ENDIF},
  ERD.Protocol.UDS;

constructor TOBDFlashPipeline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FOwnedTask := TOBDOwnedTask.Create;
  FChecks := TOBDFlashCheckList.Create;
  FTransfer := TOBDUDSTransfer.Create(Self);
  FResetAfterFlash := True;
  FAddressFormatBytes := 4;
  FLengthFormatBytes := 4;
end;

destructor TOBDFlashPipeline.Destroy;
begin
  if FTransfer <> nil then FTransfer.Cancel;
  if FOwnedTask <> nil then FOwnedTask.Cancel;
  FChecks.Free;
  FreeAndNil(FOwnedTask);
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDFlashPipeline.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FOwnedTask <> nil then FOwnedTask.Quiesce;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDFlashPipeline.SetAuditLog(AValue: TOBDCodingAuditLog);
begin
  if FAuditLog = AValue then Exit;
  if FOwnedTask <> nil then FOwnedTask.Quiesce;
  if FAuditLog <> nil then FAuditLog.RemoveFreeNotification(Self);
  FAuditLog := AValue;
  if FAuditLog <> nil then FAuditLog.FreeNotification(Self);
end;

procedure TOBDFlashPipeline.SetVoltageGate(AValue: TOBDVoltageGate);
begin
  if FVoltageGate = AValue then Exit;
  if FOwnedTask <> nil then FOwnedTask.Quiesce;
  if FVoltageGate <> nil then FVoltageGate.RemoveFreeNotification(Self);
  FVoltageGate := AValue;
  if FVoltageGate <> nil then FVoltageGate.FreeNotification(Self);
end;

procedure TOBDFlashPipeline.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = FProtocol) or (AComponent = FAuditLog) or (AComponent = FVoltageGate) then
      if FOwnedTask <> nil then FOwnedTask.Quiesce;
    if AComponent = FProtocol     then FProtocol     := nil;
    if AComponent = FAuditLog     then FAuditLog     := nil;
    if AComponent = FVoltageGate  then FVoltageGate  := nil;
  end;
end;

procedure TOBDFlashPipeline.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('Async start requires the main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDFlashPipeline: flash already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDFlashPipeline.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDFlashPipeline.GetChecks: TOBDFlashCheckList;
begin
  Result := FChecks;
end;

procedure TOBDFlashPipeline.WriteAudit(AKind: TOBDAuditKind;
  const ATarget, AMessage: string;
  const ABefore, AAfter: TBytes);
var
  Entry: TOBDAuditEntry;
begin
  if FAuditLog = nil then Exit;
  Entry := Default(TOBDAuditEntry);
  Entry.Timestamp := Now;
  Entry.SessionID := FAuditLog.SessionID;
  Entry.Kind := AKind;
  Entry.Target := ATarget;
  Entry.Message := AMessage;
  Entry.Before := ABefore;
  Entry.After := AAfter;
  FAuditLog.Append(Entry);
end;

procedure TOBDFlashPipeline.FirePhase(APhase: TOBDFlashPhase);
var
  Self_: TOBDFlashPipeline;
  P: TOBDFlashPhase;
begin
  if not Assigned(FOnPhaseChange) then Exit;
  Self_ := Self; P := APhase;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnPhaseChange(Self_, P)
  else
    FOwnedTask.Post( procedure begin
      if Assigned(Self_.FOnPhaseChange) then
        Self_.FOnPhaseChange(Self_, P);
    end);
end;

procedure TOBDFlashPipeline.FireCheckResult(const AResult: TOBDCheckResult);
var
  Self_: TOBDFlashPipeline;
  R: TOBDCheckResult;
begin
  if not Assigned(FOnCheckResult) then Exit;
  Self_ := Self; R := AResult;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnCheckResult(Self_, R)
  else
    FOwnedTask.Post( procedure begin
      if Assigned(Self_.FOnCheckResult) then
        Self_.FOnCheckResult(Self_, R);
    end);
end;

procedure TOBDFlashPipeline.FireComplete;
var
  Self_: TOBDFlashPipeline;
begin
  if not Assigned(FOnComplete) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnComplete(Self_)
  else
    FOwnedTask.Post( procedure begin
      if Assigned(Self_.FOnComplete) then Self_.FOnComplete(Self_);
    end);
end;

procedure TOBDFlashPipeline.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDFlashPipeline; Code: TOBDErrorCode; Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    FOwnedTask.Post( procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

function TOBDFlashPipeline.FireConfirm(AAddress: UInt64;
  ASize: UInt32): Boolean;
var
  Allow: Boolean;
  Self_: TOBDFlashPipeline;
  Addr: UInt64;
  Sz: UInt32;
  Local: Boolean;
begin
  // No handler + AutoExecute=False → abort per PLAN §785.
  if not Assigned(FOnConfirmExecute) then
  begin
    Result := FAutoExecute;
    Exit;
  end;
  Self_ := Self; Addr := AAddress; Sz := ASize; Allow := False;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnConfirmExecute(Self_, Addr, Sz, Allow)
  else
  begin
    Local := False;
    FOwnedTask.Synchronize( procedure
      var L: Boolean;
      begin
        L := False;
        if Assigned(Self_.FOnConfirmExecute) then
          Self_.FOnConfirmExecute(Self_, Addr, Sz, L);
        Local := L;
      end);
    Allow := Local;
  end;
  Result := Allow;
end;

procedure TOBDFlashPipeline.RunChecks(APhase: TOBDFlashPhase);
var
  Err: TOBDCheckResult;
  Self_: TOBDFlashPipeline;
begin
  if TInterlocked.CompareExchange(FVoltageGateAborted, 0, 0) <> 0 then
    raise EOBDProtocolErr.Create('Flash aborted by voltage gate');
  Self_ := Self;
  if not FChecks.RunPhase(APhase, Err,
    procedure(R: TOBDCheckResult)
    begin
      Self_.FireCheckResult(R);
      Self_.WriteAudit(akInfo,
        Format('%s/%s', [
          GetEnumName(TypeInfo(TOBDFlashPhase), Ord(R.Phase)),
          R.Name]),
        R.Message);
    end) then
    raise EOBDProtocolErr.CreateFmt(
      'Pre-condition "%s" failed: %s', [Err.Name, Err.Message]);
end;

procedure TOBDFlashPipeline.HandleVoltageAbort(Sender: TObject;
  AVoltage: Double; AReason: string);
begin
  TInterlocked.Exchange(FVoltageGateAborted, 1);
  if FTransfer <> nil then FTransfer.Cancel;
  WriteAudit(akError, 'voltage-gate', AReason);
end;

function TOBDFlashPipeline.FCurrentImage: TBytes;
begin
  Result := FSavedImage;
end;

function TOBDFlashPipeline.FCurrentAddress: UInt64;
begin
  Result := FSavedAddress;
end;

procedure TOBDFlashPipeline.WriteCheckpointSafe(AAddress: UInt64;
  const AImage: TBytes; const ACursor: TOBDTransferCursor);
var
  Info: TOBDFlashCheckpointInfo;
begin
  if FCheckpointFile = '' then Exit;
  Info := Default(TOBDFlashCheckpointInfo);
  Info.SessionID := FSessionID;
  Info.Vendor := FTargetVendor;
  Info.Module := FTargetModule;
  Info.ECUIdentity := FECUIdentity;
  Info.ImageSha256 := FSavedImageHash;
  Info.Cursor := ACursor;
  try
    TOBDFlashCheckpoint.Save(FCheckpointFile, Info);
  except
    on E: Exception do
    begin
      WriteAudit(akError, 'checkpoint', 'Checkpoint write failed: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TOBDFlashPipeline.HandleBeforeRequest(Sender: TObject);
begin
  FOwnedTask.CheckCancelled;
  if TInterlocked.CompareExchange(FVoltageGateAborted, 0, 0) <> 0 then
    raise EOBDProtocolErr.Create('Flash aborted by voltage gate');
end;

procedure TOBDFlashPipeline.HandleProgress(Sender: TObject;
  const ACursor: TOBDTransferCursor);
begin
  if TInterlocked.CompareExchange(FVoltageGateAborted, 0, 0) <> 0 then
    FTransfer.Cancel;
  WriteCheckpointSafe(FCurrentAddress, FCurrentImage, ACursor);
end;

procedure TOBDFlashPipeline.DoFlash(AAddress: UInt64; const AImage: TBytes);
var
  Resp: TOBDResponse;
  SessionGUID: TGUID;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDFlashPipeline: Protocol not assigned');
  if Length(AImage) = 0 then
    raise EOBDConfig.Create('TOBDFlashPipeline: empty image');
  if not FAutoExecute then
  begin
    if not FireConfirm(AAddress, UInt32(Length(AImage))) then
      raise EOBDConfig.Create(
        'TOBDFlashPipeline: cancelled by OnConfirmExecute / AutoExecute');
  end;

  if not FResuming then
  begin
    if FAuditLog <> nil then FSessionID := FAuditLog.SessionID
    else begin CreateGUID(SessionGUID); FSessionID := GUIDToString(SessionGUID) end;
  end;
  FSavedImage := Copy(AImage);
  FSavedImageHash := TOBDFlashCheckpoint.ComputeImageHash(FSavedImage);
  FSavedAddress := AAddress;
  TInterlocked.Exchange(FVoltageGateAborted, 0);

  if FVoltageGate = nil then
    WriteAudit(akInfo, 'voltage-gate',
      'WARN: no VoltageGate assigned — proceeding without supply monitor')
  else
  begin
    FVoltageGate.OnAbortExecutingThread := HandleVoltageAbort;
    FVoltageGate.Start;
  end;

  WriteAudit(akInfo, 'flash',
    Format('start address=0x%x size=%d', [AAddress, Length(AImage)]));

  try
    FirePhase(fpPreflight);
    RunChecks(fpPreflight);

    FirePhase(fpVerifyImage);
    RunChecks(fpVerifyImage);

    FirePhase(fpEnterProgramming);
    RunChecks(fpEnterProgramming);
    if not FResuming and Assigned(FEnterProgramming) then FEnterProgramming();

    FTransfer.Protocol := FProtocol;
    FTransfer.AutoExecute := True;
    FTransfer.AddressFormatBytes := FAddressFormatBytes;
    FTransfer.LengthFormatBytes := FLengthFormatBytes;
    FTransfer.OnBeforeRequest := HandleBeforeRequest;
    FTransfer.OnAcceptedBlock := HandleProgress;

    FirePhase(fpTransfer);
    if FResuming then
    begin
      TOBDFlashCheckpoint.ValidateResume(FResumeInfo, FSavedImage, FSessionID,
        FTargetVendor, FTargetModule, FECUIdentity, FResumeValidator);
      FTransfer.Resume(FResumeInfo.Cursor, FSavedImage);
    end
    else FTransfer.Run(AAddress, FSavedImage);

    FirePhase(fpVerify);
    RunChecks(fpVerify);
    if Assigned(FVerifyStep) then FVerifyStep();

    if FResetAfterFlash then
    begin
      FirePhase(fpReset);
      Resp := FProtocol.Request($11, TBytes.Create($01));
      if Resp.IsNegative then
        raise EOBDProtocolErr.CreateFmt(
          'ECUReset hardReset negative: %s', [Resp.NRCText]);
    end;

    FirePhase(fpFinalise);
    RunChecks(fpFinalise);
    WriteAudit(akInfo, 'flash', 'commit');
    FireComplete;
  except
    on E: Exception do
    begin
      WriteAudit(akError, 'flash', E.Message);
      raise;
    end;
  end;
end;

procedure TOBDFlashPipeline.Flash(AAddress: UInt64; const AImage: TBytes);
begin
  try
    DoFlash(AAddress, AImage);
  finally
    if FVoltageGate <> nil then
    begin
      FVoltageGate.Stop;
      FVoltageGate.OnAbortExecutingThread := nil;
    end;
  end;
end;

procedure TOBDFlashPipeline.ResumeFromCheckpoint(const AFileName: string;
  const AImage: TBytes; const ASessionID: string; const AConfirmECU: TOBDResumeValidator);
begin
  if FAsyncInFlight or FResuming then
    raise EOBDConfig.Create('Flash pipeline already in flight');
  FResumeInfo := TOBDFlashCheckpoint.Load(AFileName);
  // Local checks must finish before preflight can perform any wire access.
  TOBDFlashCheckpoint.ValidateResumeLocal(FResumeInfo, AImage, ASessionID,
    FTargetVendor, FTargetModule, FECUIdentity);
  if not Assigned(AConfirmECU) then raise EOBDConfig.Create('Resume requires ECU state confirmation');
  FCheckpointFile := AFileName;
  FSessionID := ASessionID;
  FResumeValidator := AConfirmECU;
  FResuming := True;
  try Flash(FResumeInfo.Cursor.Address, AImage)
  finally FResuming := False; FResumeValidator := nil end;
end;

procedure TOBDFlashPipeline.FlashAsync(AAddress: UInt64;
  const AImage: TBytes);
var
  Self_: TOBDFlashPipeline;
  Addr: UInt64;
  Img: TBytes;
begin
  GuardSingleAsync;
  try
    Self_ := Self; Addr := AAddress;
    Img := Copy(AImage, 0, Length(AImage));
    FOwnedTask.Start(
      procedure
      begin
        try
          try
            Self_.Flash(Addr, Img);
          except
            on E: Exception do Self_.FireError(oeIO, E.Message);
          end;
        finally
          Self_.ReleaseAsync;
        end;
      end);
  except
    ReleaseAsync;
    raise;
  end;
end;

end.
