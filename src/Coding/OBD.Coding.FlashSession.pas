//------------------------------------------------------------------------------
//  OBD.Coding.FlashSession
//
//  TOBDFlashSession — high-level orchestrator that wraps the
//  classic UDS reflash choreography into a single call:
//
//    1. DiagnosticSessionControl  (0x10) → programmingSession (0x02)
//    2. SecurityAccess            (0x27) — via TOBDSecurityAccess
//    3. RoutineControl Erase      (0x31 0x01 ERASE_RID + memRange)
//    4. Flasher.Flash             (0x34/0x36/0x37)
//    5. RoutineControl CheckSum   (0x31 0x01 VERIFY_RID + checksum)
//    6. ECUReset                  (0x11 0x01 hardReset)
//
//  Every step is configurable by property; hosts can disable
//  individual steps (e.g. set <c>VerifyRoutineID = 0</c> to skip
//  the verify routine).
//
//  Like its underlying components, the orchestrator defaults
//  <c>AutoExecute</c> to <c>False</c> and propagates the gate to
//  every write-side child component before calling them.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 9.2 (Session control)
//    - ISO 14229-1:2020 § 9.3 (ECUReset)
//    - ISO 14229-1:2020 § 12.7 (RoutineControl)
//    - ISO 14229-1:2020 § 14.2 (RequestDownload)
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.Coding.FlashSession;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol,
  OBD.Coding.SecurityAccess,
  OBD.Coding.RoutineControl,
  OBD.Coding.Flasher;

const
  /// <summary>UDS DiagnosticSessionControl SID.</summary>
  UDS_SID_SESSION = $10;
  /// <summary>UDS ECUReset SID.</summary>
  UDS_SID_ECURESET = $11;
  /// <summary>Programming-session sub-function.</summary>
  UDS_SESSION_PROGRAMMING = $02;
  /// <summary>Hard-reset sub-function.</summary>
  UDS_RESET_HARD = $01;

type
  /// <summary>Argument record for <c>OnBeforeSession</c>.</summary>
  TOBDFlashSessionBeforeEvent = procedure(Sender: TObject;
    AAddress: UInt64; ASize: UInt32; var ACancel: Boolean) of object;

  /// <summary>
  ///   Composite session orchestrator. Holds (and lazily creates)
  ///   the child components — security access, routine control,
  ///   flasher — bound to the same protocol.
  /// </summary>
  TOBDFlashSession = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    // Choreography knobs.
    FSecurityLevel: Byte;
    FEraseRoutineID: Word;
    FVerifyRoutineID: Word;
    FResetAfterFlash: Boolean;
    FSessionAtStart: Boolean;
    // Child components.
    FSecurity: TOBDSecurityAccess;
    FRoutines: TOBDRoutineControl;
    FFlasher: TOBDFlasher;
    // Events.
    FSeedToKey: TOBDSeedToKeyFunc;
    FOnBeforeSession: TOBDFlashSessionBeforeEvent;
    FOnComplete: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    FOnProgress: TOBDProgressEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure EnsureChildren;
    procedure DoFlash(AAddress: UInt64; const AImage: TBytes;
      const AVerifyChecksum: TBytes);
    procedure SwitchSession(ASubFunction: Byte);
    procedure ResetEcu;
    function FireBeforeSession(AAddress: UInt64; ASize: UInt32): Boolean;
    procedure FireComplete;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure FireProgress(AIndex, ACount: Cardinal;
      const AName, ADetail: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Runs the full reflash choreography: programming session →
    ///   security access → erase → flash → verify → reset.
    /// </summary>
    /// <param name="AAddress">Target memory address.</param>
    /// <param name="AImage">Firmware bytes.</param>
    /// <param name="AVerifyChecksum">Optional checksum bytes
    /// passed as parameters to the verify routine. Empty array
    /// skips the verify call only when <c>VerifyRoutineID = 0</c>.</param>
    procedure Flash(AAddress: UInt64; const AImage: TBytes;
      const AVerifyChecksum: TBytes = nil);
    /// <summary>Non-blocking <see cref="Flash"/>.</summary>
    procedure FlashAsync(AAddress: UInt64; const AImage: TBytes;
      const AVerifyChecksum: TBytes = nil);

    /// <summary>Functional seed → key transform forwarded to the
    /// internal <see cref="TOBDSecurityAccess"/>.</summary>
    property SeedToKey: TOBDSeedToKeyFunc read FSeedToKey write FSeedToKey;

    /// <summary>The internal flasher component, exposed so a host
    /// can tune block sizes / retry budgets.</summary>
    property Flasher: TOBDFlasher read FFlasher;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate, propagated to every child.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    /// <summary>SecurityAccess level used during the unlock step
    /// (must be odd). Default <c>0x01</c>.</summary>
    property SecurityLevel: Byte read FSecurityLevel write FSecurityLevel
      default $01;
    /// <summary>RoutineID used for the erase step. <c>0</c>
    /// disables erase. Default <c>0xFF00</c> (vendor-typical).</summary>
    property EraseRoutineID: Word read FEraseRoutineID
      write FEraseRoutineID default $FF00;
    /// <summary>RoutineID used for the verify-checksum step.
    /// <c>0</c> disables verify. Default <c>0xFF01</c>.</summary>
    property VerifyRoutineID: Word read FVerifyRoutineID
      write FVerifyRoutineID default $FF01;
    /// <summary>Send <c>ECUReset hardReset</c> as the final
    /// step. Default <c>True</c>.</summary>
    property ResetAfterFlash: Boolean read FResetAfterFlash
      write FResetAfterFlash default True;
    /// <summary>Run a <c>0x10 0x02</c> programming-session switch
    /// at the start. Default <c>True</c>. Disable when the host
    /// already manages the session externally.</summary>
    property SessionAtStart: Boolean read FSessionAtStart
      write FSessionAtStart default True;

    property OnBeforeSession: TOBDFlashSessionBeforeEvent
      read FOnBeforeSession write FOnBeforeSession;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
    property OnProgress: TOBDProgressEvent read FOnProgress
      write FOnProgress;
  end;

implementation

constructor TOBDFlashSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAutoExecute := False;
  FSecurityLevel := $01;
  FEraseRoutineID := $FF00;
  FVerifyRoutineID := $FF01;
  FResetAfterFlash := True;
  FSessionAtStart := True;
end;

destructor TOBDFlashSession.Destroy;
begin
  FFlasher.Free;
  FRoutines.Free;
  FSecurity.Free;
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDFlashSession.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
  if FSecurity <> nil then FSecurity.Protocol := AValue;
  if FRoutines <> nil then FRoutines.Protocol := AValue;
  if FFlasher  <> nil then FFlasher.Protocol  := AValue;
end;

procedure TOBDFlashSession.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDFlashSession.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDFlashSession: flash already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDFlashSession.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDFlashSession.EnsureChildren;
begin
  if FSecurity = nil then
    FSecurity := TOBDSecurityAccess.Create(Self);
  if FRoutines = nil then
    FRoutines := TOBDRoutineControl.Create(Self);
  if FFlasher = nil then
    FFlasher := TOBDFlasher.Create(Self);
  FSecurity.Protocol := FProtocol;
  FRoutines.Protocol := FProtocol;
  FFlasher.Protocol  := FProtocol;
  FRoutines.AutoExecute := FAutoExecute;
  FFlasher.AutoExecute  := FAutoExecute;
  FSecurity.SeedToKey := FSeedToKey;
end;

procedure TOBDFlashSession.SwitchSession(ASubFunction: Byte);
var
  Resp: TOBDResponse;
begin
  Resp := FProtocol.Request(UDS_SID_SESSION, TBytes.Create(ASubFunction));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'DiagnosticSessionControl 0x%2.2X negative: %s',
      [ASubFunction, Resp.NRCText]);
end;

procedure TOBDFlashSession.ResetEcu;
var
  Resp: TOBDResponse;
begin
  Resp := FProtocol.Request(UDS_SID_ECURESET, TBytes.Create(UDS_RESET_HARD));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ECUReset hardReset negative: %s', [Resp.NRCText]);
end;

procedure TOBDFlashSession.DoFlash(AAddress: UInt64;
  const AImage: TBytes; const AVerifyChecksum: TBytes);
var
  TotalSteps, Step: Cardinal;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDFlashSession: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDFlashSession: AutoExecute is False — set it before flashing');
  if Length(AImage) = 0 then
    raise EOBDConfig.Create('TOBDFlashSession: empty image');
  if not FireBeforeSession(AAddress, UInt32(Length(AImage))) then
    raise EOBDConfig.Create(
      'TOBDFlashSession: cancelled by OnBeforeSession handler');

  EnsureChildren;

  // Walk the choreography. Step count is decided up-front so the
  // progress bar is monotonic.
  TotalSteps := 0;
  if FSessionAtStart    then Inc(TotalSteps);
  Inc(TotalSteps);                              // security access
  if FEraseRoutineID  <> 0 then Inc(TotalSteps); // erase
  Inc(TotalSteps);                              // flash
  if FVerifyRoutineID <> 0 then Inc(TotalSteps); // verify
  if FResetAfterFlash then Inc(TotalSteps);      // reset

  Step := 0;

  if FSessionAtStart then
  begin
    Inc(Step);
    FireProgress(Step, TotalSteps, 'Flash session', 'programming session');
    SwitchSession(UDS_SESSION_PROGRAMMING);
  end;

  Inc(Step);
  FireProgress(Step, TotalSteps, 'Flash session', 'security access');
  FSecurity.Unlock(FSecurityLevel);

  if FEraseRoutineID <> 0 then
  begin
    Inc(Step);
    FireProgress(Step, TotalSteps, 'Flash session',
      Format('erase RID 0x%4.4X', [FEraseRoutineID]));
    FRoutines.Start(FEraseRoutineID, nil);
  end;

  Inc(Step);
  FireProgress(Step, TotalSteps, 'Flash session', 'flashing image');
  FFlasher.Flash(AAddress, AImage);

  if FVerifyRoutineID <> 0 then
  begin
    Inc(Step);
    FireProgress(Step, TotalSteps, 'Flash session',
      Format('verify RID 0x%4.4X', [FVerifyRoutineID]));
    FRoutines.Start(FVerifyRoutineID, AVerifyChecksum);
  end;

  if FResetAfterFlash then
  begin
    Inc(Step);
    FireProgress(Step, TotalSteps, 'Flash session', 'ECU reset');
    ResetEcu;
  end;

  FireComplete;
end;

procedure TOBDFlashSession.Flash(AAddress: UInt64;
  const AImage: TBytes; const AVerifyChecksum: TBytes);
begin
  DoFlash(AAddress, AImage, AVerifyChecksum);
end;

procedure TOBDFlashSession.FlashAsync(AAddress: UInt64;
  const AImage: TBytes; const AVerifyChecksum: TBytes);
var
  Self_: TOBDFlashSession;
  Addr: UInt64;
  Img, Vc: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; Addr := AAddress;
  Img := Copy(AImage, 0, Length(AImage));
  Vc  := Copy(AVerifyChecksum, 0, Length(AVerifyChecksum));
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoFlash(Addr, Img, Vc);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

function TOBDFlashSession.FireBeforeSession(AAddress: UInt64;
  ASize: UInt32): Boolean;
var
  Cancel: Boolean;
  Self_: TOBDFlashSession;
  Addr: UInt64;
  Sz: UInt32;
  Local: Boolean;
begin
  if not Assigned(FOnBeforeSession) then Exit(True);
  Self_ := Self; Addr := AAddress; Sz := ASize; Cancel := False;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnBeforeSession(Self_, Addr, Sz, Cancel)
  else
  begin
    Local := False;
    TThread.Synchronize(nil, procedure
      var C: Boolean;
      begin
        C := False;
        if Assigned(Self_.FOnBeforeSession) then
          Self_.FOnBeforeSession(Self_, Addr, Sz, C);
        Local := C;
      end);
    Cancel := Local;
  end;
  Result := not Cancel;
end;

procedure TOBDFlashSession.FireComplete;
var
  Self_: TOBDFlashSession;
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

procedure TOBDFlashSession.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDFlashSession; Code: TOBDErrorCode; Msg: string;
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
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

procedure TOBDFlashSession.FireProgress(AIndex, ACount: Cardinal;
  const AName, ADetail: string);
var
  Self_: TOBDFlashSession;
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then Exit;
  Self_ := Self;
  Step := TOBDProgressStep.MakeStep(AIndex, ACount, AName, ADetail);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self_, Step)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnProgress) then Self_.FOnProgress(Self_, Step);
    end);
end;

end.
