//------------------------------------------------------------------------------
//  OBD.Service.DriveCycle
//
//  TOBDDriveCycleAdvisor - non-visual component that walks the
//  driver through the cycle(s) needed to clear specific OBD-II
//  readiness monitors. Two modes:
//
//    1. Static lookup: GetCycle(AMonitor) returns the drive
//       cycle steps from the catalogue. No protocol needed.
//
//    2. Live mode: Start polls Mode 01 PID 01 every
//       PollIntervalMs, fires OnReadiness with the current
//       monitor state, fires OnReady whenever a monitor
//       transitions to "complete", and walks the host through
//       the steps via OnStep events. AutoStop ends the session
//       when TargetMonitor is complete (or all monitors when
//       TargetMonitor isn't set).
//
//  Component sits on the "OBD Services" palette tab; depends on
//  TOBDProtocol for the bus read. TOBDOnBoardMonitor is
//  optional - if wired we'll trigger it after each readiness
//  poll so hosts can pull Mode 06 results too.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Service.DriveCycle;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Data.Bind.Components,
  OBD.Errors,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Protocol.Types,
  OBD.Protocol,
  OBD.Service.DriveCycle.Types,
  OBD.Service.DriveCycle.Catalog;

type
  /// <summary>Fired after each readiness poll with the full
  /// snapshot.</summary>
  TOBDDriveCycleReadinessEvent = procedure(Sender: TObject;
    const AReadiness: TArray<TOBDMonitorReadiness>) of object;

  /// <summary>Fired whenever a previously-incomplete monitor
  /// reports complete (transition Notin -> In Complete).</summary>
  TOBDDriveCycleReadyEvent = procedure(Sender: TObject;
    AMonitor: TOBDMonitor) of object;

  /// <summary>Fired when the live-mode walker advances to a
  /// new step. <c>AElapsedSec</c> is the seconds since
  /// <see cref="Start"/>; <c>ATotalSec</c> is the cycle's
  /// total estimated duration.</summary>
  TOBDDriveCycleStepEvent = procedure(Sender: TObject;
    const AStep: TOBDDriveCycleStep;
    AElapsedSec, ATotalSec: Cardinal) of object;

  /// <summary>Fired once when the target monitor completes
  /// (or every supported monitor, when no target was set).</summary>
  TOBDDriveCycleAllReadyEvent = procedure(Sender: TObject) of object;

  TOBDDriveCyclePollThread = class;

  TOBDDriveCycleAdvisor = class(TComponent)
  strict private
    FProtocol:        TOBDProtocol;
    FTargetMonitor:   TOBDMonitor;
    FHasTarget:       Boolean;
    FAutoStop:        Boolean;
    FPollIntervalMs:  Cardinal;
    FThread:          TOBDDriveCyclePollThread;
    FOnReadiness:     TOBDDriveCycleReadinessEvent;
    FOnReady:         TOBDDriveCycleReadyEvent;
    FOnStep:          TOBDDriveCycleStepEvent;
    FOnAllReady:      TOBDDriveCycleAllReadyEvent;
    FOnError:         TOBDConnectionErrorEvent;
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure SetTargetMonitor(AValue: TOBDMonitor);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Decodes Mode 01 PID 01 against
    /// <see cref="Protocol"/>. Inline rather than calling into
    /// VehicleHealth so the advisor stays usable on its own.
    /// </summary>
    function ReadReadinessRaw(out AError: string):
      TArray<TOBDMonitorReadiness>;
  public
    /// <summary>Pure decoder for a Mode 01 PID 01 response
    /// payload. Hosts that already have the 4 bytes (e.g. from
    /// a recorder replay) call this directly without going
    /// through the live protocol. <c>AError</c> populated when
    /// the payload is too short.</summary>
    class function DecodePID01(const AData: TBytes;
      out AError: string): TArray<TOBDMonitorReadiness>; static;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    /// <summary>One-shot readiness read. Returns the full set
    /// (one entry per monitor; <c>Supported = False</c> for
    /// monitors the ECU doesn't have).</summary>
    function ReadReadiness: TArray<TOBDMonitorReadiness>;

    /// <summary>Catalogue lookup - same as
    /// <see cref="TOBDDriveCycleCatalog.TryGetCycle"/>.
    /// Returns a default-zero record when unregistered.</summary>
    function GetCycle(AMonitor: TOBDMonitor): TOBDDriveCycle;

    /// <summary>Begin the live walker. Reads readiness, picks
    /// the cycle for <see cref="TargetMonitor"/>, and walks
    /// through its steps in real time. Idempotent - calling
    /// Start while running is a no-op.</summary>
    procedure Start;

    /// <summary>Stop the live walker. Joins the polling
    /// thread.</summary>
    procedure Stop;

    /// <summary>True when the live walker is running.</summary>
    function Running: Boolean;
  published
    /// <summary>Required - source of the bus reads.</summary>
    property Protocol: TOBDProtocol
      read FProtocol write SetProtocol;

    /// <summary>Monitor the live walker focuses on. Default
    /// <c>omCatalyst</c> - the monitor most hosts want
    /// cleared. <see cref="HasTarget"/> = False after
    /// <see cref="ClearTarget"/> means "advise on all
    /// supported monitors".</summary>
    property TargetMonitor: TOBDMonitor
      read FTargetMonitor write SetTargetMonitor default omCatalyst;

    /// <summary>When True (default) <see cref="Stop"/> is
    /// called automatically once the target monitor completes
    /// (or all supported monitors when no target).</summary>
    property AutoStop: Boolean
      read FAutoStop write FAutoStop default True;

    /// <summary>Live-mode poll interval in milliseconds.
    /// Default 5000 (5 s) - readiness flags only update once
    /// the ECU re-evaluates them, faster polling just hammers
    /// the bus.</summary>
    property PollIntervalMs: Cardinal
      read FPollIntervalMs write FPollIntervalMs default 5000;

    property OnReadiness: TOBDDriveCycleReadinessEvent
      read FOnReadiness write FOnReadiness;
    property OnReady: TOBDDriveCycleReadyEvent
      read FOnReady write FOnReady;
    property OnStep: TOBDDriveCycleStepEvent
      read FOnStep write FOnStep;
    property OnAllReady: TOBDDriveCycleAllReadyEvent
      read FOnAllReady write FOnAllReady;
    property OnError: TOBDConnectionErrorEvent
      read FOnError write FOnError;
  end;

  TOBDDriveCyclePollThread = class(TThread)
  strict private
    FAdvisor:      TOBDDriveCycleAdvisor;
    FStopEvent:    TEvent;
    FCycle:        TOBDDriveCycle;
    FPriorState:   array[TOBDMonitor] of Boolean;  // last "Complete" snapshot
    FPriorKnown:   array[TOBDMonitor] of Boolean;
    FStartedAt:    TDateTime;
    FCurrentStep:  Integer;
    FAccumulatedSec: Cardinal;
    procedure FireReadinessSync(const A: TArray<TOBDMonitorReadiness>);
    procedure FireReadySync(M: TOBDMonitor);
    procedure FireStepSync(const S: TOBDDriveCycleStep;
      Elapsed, Total: Cardinal);
    procedure FireAllReadySync;
    procedure FireErrorSync(C: TOBDErrorCode; const M: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AAdvisor: TOBDDriveCycleAdvisor);
    destructor  Destroy; override;
    procedure   Stop;
  end;

implementation

uses
  System.DateUtils;

{ TOBDDriveCycleAdvisor -------------------------------------------------------}

constructor TOBDDriveCycleAdvisor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargetMonitor  := omCatalyst;
  FHasTarget      := True;
  FAutoStop       := True;
  FPollIntervalMs := 5000;
end;

destructor TOBDDriveCycleAdvisor.Destroy;
begin
  Stop;
  inherited;
end;

procedure TOBDDriveCycleAdvisor.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDDriveCycleAdvisor.SetTargetMonitor(AValue: TOBDMonitor);
begin
  FTargetMonitor := AValue;
  FHasTarget := True;
end;

procedure TOBDDriveCycleAdvisor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

class function TOBDDriveCycleAdvisor.DecodePID01(const AData: TBytes;
  out AError: string): TArray<TOBDMonitorReadiness>;

  procedure Add(var ARes: TArray<TOBDMonitorReadiness>;
    AMon: TOBDMonitor; ASup, ACom: Boolean);
  var Idx: Integer;
  begin
    Idx := Length(ARes);
    SetLength(ARes, Idx + 1);
    ARes[Idx].Monitor   := AMon;
    ARes[Idx].Supported := ASup;
    ARes[Idx].Complete  := ACom;
  end;

var
  B, C, D:  Byte;
  IsDiesel: Boolean;
begin
  Result := nil;
  AError := '';
  if Length(AData) < 4 then
  begin
    AError := Format('PID 01 response too short (%d bytes)',
      [Length(AData)]);
    Exit;
  end;
  B := AData[1];
  C := AData[2];
  D := AData[3];
  IsDiesel := (B and $08) <> 0;

    // Continuous monitors. "complete" = bit clear.
    Add(Result, omMisfire,
        (B and $01) <> 0, (B and $10) = 0);
    Add(Result, omFuelSystem,
        (B and $02) <> 0, (B and $20) = 0);
    Add(Result, omComprehensive,
        (B and $04) <> 0, (B and $40) = 0);

    // Non-continuous - bit layout depends on engine type
    // (SAE J1979 Table A2). C carries supported, D carries
    // "not complete" (so bit clear = complete).
    if IsDiesel then
    begin
      Add(Result, omNMHCCatalyst,
          (C and $01) <> 0, (D and $01) = 0);
      Add(Result, omNOxAftertreatment,
          (C and $02) <> 0, (D and $02) = 0);
      Add(Result, omBoostPressureSystem,
          (C and $08) <> 0, (D and $08) = 0);
      Add(Result, omExhaustGasSensor,
          (C and $20) <> 0, (D and $20) = 0);
      Add(Result, omPMFilter,
          (C and $40) <> 0, (D and $40) = 0);
      Add(Result, omEGRorVVTSystem,
          (C and $80) <> 0, (D and $80) = 0);
    end
    else
    begin
      Add(Result, omCatalyst,
          (C and $01) <> 0, (D and $01) = 0);
      Add(Result, omHeatedCatalyst,
          (C and $02) <> 0, (D and $02) = 0);
      Add(Result, omEvaporativeSystem,
          (C and $04) <> 0, (D and $04) = 0);
      Add(Result, omSecondaryAirSystem,
          (C and $08) <> 0, (D and $08) = 0);
      Add(Result, omACRefrigerant,
          (C and $10) <> 0, (D and $10) = 0);
      Add(Result, omOxygenSensor,
          (C and $20) <> 0, (D and $20) = 0);
      Add(Result, omOxygenSensorHeater,
          (C and $40) <> 0, (D and $40) = 0);
      Add(Result, omEGRSystem,
          (C and $80) <> 0, (D and $80) = 0);
    end;
end;

function TOBDDriveCycleAdvisor.ReadReadinessRaw(out AError: string):
  TArray<TOBDMonitorReadiness>;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
begin
  Result := nil;
  AError := '';
  if FProtocol = nil then
  begin
    AError := 'TOBDDriveCycleAdvisor: Protocol not assigned';
    Exit;
  end;
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $01;
    Req.Data := TBytes.Create($01);
    Resp := FProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      AError := Format('NRC 0x%.2X - %s', [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result := DecodePID01(Resp.Data, AError);
  except
    on E: Exception do
      AError := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDDriveCycleAdvisor.ReadReadiness:
  TArray<TOBDMonitorReadiness>;
var Err: string;
begin
  Result := ReadReadinessRaw(Err);
  // LiveBindings refresh — readiness snapshots are state that
  // hosts often bind to a UI grid. Notify even on partial reads
  // so the grid shows whatever could be parsed.
  try TBindings.Notify(Self, ''); except end;
  if (Err <> '') and Assigned(FOnError) then
    FOnError(Self, oeIO, Err);
end;

function TOBDDriveCycleAdvisor.GetCycle(AMonitor: TOBDMonitor): TOBDDriveCycle;
begin
  TOBDDriveCycleCatalog.TryGetCycle(AMonitor, Result);
end;

procedure TOBDDriveCycleAdvisor.Start;
begin
  if FThread <> nil then Exit;
  if FProtocol = nil then
    raise EOBDConfig.Create(
      'TOBDDriveCycleAdvisor.Start: Protocol not assigned');
  FThread := TOBDDriveCyclePollThread.Create(Self);
end;

procedure TOBDDriveCycleAdvisor.Stop;
begin
  if FThread = nil then Exit;
  FThread.Stop;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

function TOBDDriveCycleAdvisor.Running: Boolean;
begin
  Result := FThread <> nil;
end;

{ TOBDDriveCyclePollThread ----------------------------------------------------}

constructor TOBDDriveCyclePollThread.Create(
  AAdvisor: TOBDDriveCycleAdvisor);
begin
  FAdvisor   := AAdvisor;
  FStopEvent := TEvent.Create(nil, True, False, '');
  inherited Create(False);
end;

destructor TOBDDriveCyclePollThread.Destroy;
begin
  FStopEvent.Free;
  inherited;
end;

procedure TOBDDriveCyclePollThread.Stop;
begin
  Terminate;
  FStopEvent.SetEvent;
end;

procedure TOBDDriveCyclePollThread.FireReadinessSync(
  const A: TArray<TOBDMonitorReadiness>);
begin
  Synchronize(procedure
  begin
    try TBindings.Notify(FAdvisor, ''); except end;
    if Assigned(FAdvisor.FOnReadiness) then
      FAdvisor.FOnReadiness(FAdvisor, A);
  end);
end;

procedure TOBDDriveCyclePollThread.FireReadySync(M: TOBDMonitor);
begin
  if Assigned(FAdvisor.FOnReady) then
    Synchronize(procedure
    begin
      FAdvisor.FOnReady(FAdvisor, M);
    end);
end;

procedure TOBDDriveCyclePollThread.FireStepSync(
  const S: TOBDDriveCycleStep; Elapsed, Total: Cardinal);
begin
  if Assigned(FAdvisor.FOnStep) then
    Synchronize(procedure
    begin
      FAdvisor.FOnStep(FAdvisor, S, Elapsed, Total);
    end);
end;

procedure TOBDDriveCyclePollThread.FireAllReadySync;
begin
  if Assigned(FAdvisor.FOnAllReady) then
    Synchronize(procedure
    begin
      FAdvisor.FOnAllReady(FAdvisor);
    end);
end;

procedure TOBDDriveCyclePollThread.FireErrorSync(C: TOBDErrorCode;
  const M: string);
begin
  if Assigned(FAdvisor.FOnError) then
    Synchronize(procedure
    begin
      FAdvisor.FOnError(FAdvisor, C, M);
    end);
end;

procedure TOBDDriveCyclePollThread.Execute;
var
  Snap:  TArray<TOBDMonitorReadiness>;
  R:     TOBDMonitorReadiness;
  Err:   string;
  TargetReady: Boolean;
  AnyIncomplete: Boolean;
  ElapsedSec:  Cardinal;
  M: TOBDMonitor;
begin
  // Pick the target's cycle once at start; falls back to a
  // walk over every supported monitor when HasTarget is False.
  if FAdvisor.FHasTarget then
    if not TOBDDriveCycleCatalog.TryGetCycle(
      FAdvisor.FTargetMonitor, FCycle) then
      FCycle := Default(TOBDDriveCycle);

  FStartedAt   := Now;
  FCurrentStep := 0;
  FAccumulatedSec := 0;

  // Fire the first step immediately so the host UI has
  // something to show.
  if Length(FCycle.Steps) > 0 then
    FireStepSync(FCycle.Steps[0], 0, FCycle.TotalSec);

  while not Terminated do
  begin
    Snap := FAdvisor.ReadReadinessRaw(Err);
    if Err <> '' then
      FireErrorSync(oeIO, Err)
    else
    begin
      FireReadinessSync(Snap);

      // Detect transitions Incomplete -> Complete per monitor.
      for R in Snap do
      begin
        if R.Supported and R.Complete then
        begin
          if not (FPriorKnown[R.Monitor] and FPriorState[R.Monitor]) then
            FireReadySync(R.Monitor);
        end;
        FPriorKnown[R.Monitor] := True;
        FPriorState[R.Monitor] := R.Complete;
      end;

      // Walk steps based on wall-clock elapsed.
      ElapsedSec := Round((Now - FStartedAt) * 86400);
      while (FCurrentStep < High(FCycle.Steps)) and
            (ElapsedSec >=
              FAccumulatedSec + FCycle.Steps[FCurrentStep].DurationSec) do
      begin
        FAccumulatedSec :=
          FAccumulatedSec + FCycle.Steps[FCurrentStep].DurationSec;
        Inc(FCurrentStep);
        FireStepSync(FCycle.Steps[FCurrentStep],
                     ElapsedSec, FCycle.TotalSec);
      end;

      // AutoStop logic.
      if FAdvisor.FAutoStop then
      begin
        if FAdvisor.FHasTarget then
        begin
          TargetReady := False;
          for R in Snap do
            if (R.Monitor = FAdvisor.FTargetMonitor) and
               R.Supported and R.Complete then
              TargetReady := True;
          if TargetReady then
          begin
            FireAllReadySync;
            Break;
          end;
        end
        else
        begin
          AnyIncomplete := False;
          for R in Snap do
            if R.Supported and not R.Complete then AnyIncomplete := True;
          if not AnyIncomplete then
          begin
            FireAllReadySync;
            Break;
          end;
        end;
      end;
    end;

    // Sleep until poll interval expires or Stop signaled.
    if FStopEvent.WaitFor(FAdvisor.FPollIntervalMs) = wrSignaled then
      Break;
  end;
  // Suppress unused-variable warning on M (compiler hint
  // path - not all branches touch it).
  M := omMisfire; if M = omMisfire then ;
end;

end.
