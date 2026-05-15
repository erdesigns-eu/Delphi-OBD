//------------------------------------------------------------------------------
//  OBD.OEM.SessionHelper
//
//  One-call wrapper around a service-routine execution. Given a
//  <see cref="TOBDServiceRoutine"/> and a bundle of callbacks
//  the helper:
//
//    1. Opens the required diagnostic session (e.g. extended /
//       programming).
//    2. Optionally consults the voltage gate when the routine
//       demands it (<c>srsBatteryMin12V5</c>).
//    3. Sends the UDS 0x31 RoutineControl request.
//    4. Reads the routine result via the optional read-result
//       callback.
//    5. Always attempts to close the session, capturing close
//       failure when the run had already succeeded.
//
//  The helper itself is transport-agnostic: hosts wire each
//  callback to their TOBDProtocol / TOBDDiagSession, tests inject
//  lambdas. The voltage source is a
//  <see cref="TOBDVoltageSourceFunc"/> matching the
//  <c>OBD.Flash.VoltageGate</c> component's source signature so a
//  caller can reuse the same function across both.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.SessionHelper;

interface

uses
  System.SysUtils,
  OBD.OEM.ServiceRoutines,
  OBD.Flash.VoltageGate,
  OBD.UDS.NRC;

type
  /// <summary>Raised on misuse (missing required
  /// callback / empty routine key).</summary>
  EOBDOEMSessionHelper = class(Exception);

  /// <summary>Stage at which a routine execution finished or
  /// aborted. On success the helper reports
  /// <c>reseSessionClose</c>; on failure it reports the failing
  /// stage.</summary>
  TOBDRoutineExecutionStage = (
    reseNotStarted,
    reseSessionOpen,
    reseVoltageGate,
    reseRoutineStart,
    reseRoutineWait,
    reseResultRead,
    reseSessionClose);

  /// <summary>End-to-end routine-execution result.</summary>
  TOBDRoutineExecutionResult = record
    /// <summary>Whole-run success.</summary>
    Success: Boolean;
    /// <summary>Routine key that was executed.</summary>
    RoutineKey: string;
    /// <summary>Stage the run reached or failed at.</summary>
    AbortStage: TOBDRoutineExecutionStage;
    /// <summary>NRC byte when a callback reported one; 0
    /// otherwise.</summary>
    NRC: Byte;
    /// <summary>Populated on failure with stage / NRC
    /// detail.</summary>
    ErrorMessage: string;
    /// <summary>Voltage sampled by the gate (0.0 when the gate
    /// was not consulted).</summary>
    MeasuredVolts: Double;
    /// <summary>ResultRead payload on success.</summary>
    ResultBytes: TBytes;
  end;

  /// <summary>Opens the diagnostic session at
  /// <c>SessionType</c> (e.g. <c>0x03</c> extended). Returns
  /// <c>True</c> on success; <c>NRC</c> carries the negative
  /// response byte on failure (0 if non-NRC error).</summary>
  TOBDSessionOpenCallback = reference to function(
    SessionType: Byte; out NRC: Byte): Boolean;

  /// <summary>Sends the pre-built UDS 0x31 RoutineControl
  /// frame. Returns <c>True</c> on positive response.</summary>
  TOBDRoutineStartCallback = reference to function(
    const Frame: TBytes; out NRC: Byte): Boolean;

  /// <summary>Reads the routine result via 0x31 sub-function
  /// 0x03. Returns <c>True</c> + <c>ResultBytes</c> on positive
  /// response.</summary>
  TOBDRoutineResultCallback = reference to function(RID: Word;
    out ResultBytes: TBytes; out NRC: Byte): Boolean;

  /// <summary>Closes the diagnostic session (back to
  /// default).</summary>
  TOBDSessionCloseCallback = reference to function: Boolean;

  /// <summary>Bundle of callbacks the helper needs. Production
  /// callers wire each to their TOBDDiagSession; tests inject
  /// lambdas. <c>ReadVoltage</c> is consulted only when the
  /// routine carries <c>srsBatteryMin12V5</c>.</summary>
  TOBDOEMSessionCallbacks = record
    OpenSession: TOBDSessionOpenCallback;
    StartRoutine: TOBDRoutineStartCallback;
    ReadResult: TOBDRoutineResultCallback;
    CloseSession: TOBDSessionCloseCallback;
    ReadVoltage: TOBDVoltageSourceFunc;
  end;

  /// <summary>Service-routine execution helper.</summary>
  TOBDOEMSessionHelper = class
  strict private
    FMinimumVoltage: Double;
    procedure SetFailure(var Res: TOBDRoutineExecutionResult;
      Stage: TOBDRoutineExecutionStage; NRC: Byte;
      const Msg: string);
    function ApplyVoltageGate(const Routine: TOBDServiceRoutine;
      const ReadVoltage: TOBDVoltageSourceFunc;
      var Res: TOBDRoutineExecutionResult): Boolean;
  public
    /// <summary>Constructs the helper. The default voltage
    /// threshold is 12.5 V (the
    /// <c>srsBatteryMin12V5</c> safety tag); change via
    /// <see cref="MinimumVoltage"/>.</summary>
    constructor Create;
    /// <summary>Voltage threshold the helper enforces when a
    /// routine is tagged <c>srsBatteryMin12V5</c>.</summary>
    property MinimumVoltage: Double
      read FMinimumVoltage write FMinimumVoltage;
    /// <summary>Executes <c>Routine</c> end-to-end using
    /// <c>Callbacks</c>.</summary>
    /// <param name="Routine">Routine to run.</param>
    /// <param name="Callbacks">Transport callbacks.</param>
    /// <exception cref="EOBDOEMSessionHelper">Routine key is
    /// empty, or a required callback is unassigned.</exception>
    function RunServiceRoutine(
      const Routine: TOBDServiceRoutine;
      const Callbacks: TOBDOEMSessionCallbacks):
      TOBDRoutineExecutionResult;
  end;

implementation

constructor TOBDOEMSessionHelper.Create;
begin
  inherited;
  FMinimumVoltage := 12.5;
end;

procedure TOBDOEMSessionHelper.SetFailure(
  var Res: TOBDRoutineExecutionResult;
  Stage: TOBDRoutineExecutionStage; NRC: Byte;
  const Msg: string);
begin
  Res.Success := False;
  Res.AbortStage := Stage;
  Res.NRC := NRC;
  if NRC <> 0 then
    Res.ErrorMessage := Msg + ' [' + FormatNRC(NRC) + ']'
  else
    Res.ErrorMessage := Msg;
end;

function TOBDOEMSessionHelper.ApplyVoltageGate(
  const Routine: TOBDServiceRoutine;
  const ReadVoltage: TOBDVoltageSourceFunc;
  var Res: TOBDRoutineExecutionResult): Boolean;
var
  V: Double;
begin
  if Routine.Safety <> srsBatteryMin12V5 then
    Exit(True);
  if not Assigned(ReadVoltage) then
  begin
    SetFailure(Res, reseVoltageGate, 0,
      Format('routine %s requires voltage check but no ' +
        'ReadVoltage callback supplied', [Routine.Key]));
    Exit(False);
  end;
  V := ReadVoltage();
  Res.MeasuredVolts := V;
  if V < FMinimumVoltage then
  begin
    SetFailure(Res, reseVoltageGate, 0,
      Format('voltage gate failed: %.2f V < %.2f V minimum',
        [V, FMinimumVoltage]));
    Exit(False);
  end;
  Result := True;
end;

function TOBDOEMSessionHelper.RunServiceRoutine(
  const Routine: TOBDServiceRoutine;
  const Callbacks: TOBDOEMSessionCallbacks):
  TOBDRoutineExecutionResult;
var
  Frame: TBytes;
  NRC: Byte;
  ResultBytes: TBytes;
begin
  Result := Default(TOBDRoutineExecutionResult);
  Result.RoutineKey := Routine.Key;
  Result.AbortStage := reseNotStarted;

  if Routine.Key = '' then
    raise EOBDOEMSessionHelper.Create('Routine has empty key');
  if not Assigned(Callbacks.OpenSession) then
    raise EOBDOEMSessionHelper.Create(
      'OpenSession callback required');
  if not Assigned(Callbacks.StartRoutine) then
    raise EOBDOEMSessionHelper.Create(
      'StartRoutine callback required');
  if not Assigned(Callbacks.CloseSession) then
    raise EOBDOEMSessionHelper.Create(
      'CloseSession callback required');

  // 1. Open the diagnostic session.
  NRC := 0;
  if not Callbacks.OpenSession(Routine.RequiredSessionType, NRC) then
  begin
    SetFailure(Result, reseSessionOpen, NRC,
      Format('failed to open diagnostic session 0x%.2x',
        [Routine.RequiredSessionType]));
    Exit;
  end;

  try
    // 2. Voltage gate (only when the routine demands it).
    if not ApplyVoltageGate(Routine, Callbacks.ReadVoltage,
                            Result) then
      Exit;

    // 3. Build and send the RoutineControl request.
    Frame := BuildRoutineControlFrame(Routine);
    NRC := 0;
    if not Callbacks.StartRoutine(Frame, NRC) then
    begin
      SetFailure(Result, reseRoutineStart, NRC,
        Format('routine %s start refused', [Routine.Key]));
      Exit;
    end;

    // 4. Read result. Some routines complete without an explicit
    // result-read; ReadResult is optional.
    if Assigned(Callbacks.ReadResult) then
    begin
      NRC := 0;
      if not Callbacks.ReadResult(Routine.RoutineIdentifier,
                                  ResultBytes, NRC) then
      begin
        SetFailure(Result, reseResultRead, NRC,
          Format('routine %s result read failed',
            [Routine.Key]));
        Exit;
      end;
      Result.ResultBytes := ResultBytes;
    end;

    Result.Success := True;
    Result.AbortStage := reseSessionClose;
  finally
    // 5. Always attempt to close. Close failure does not demote a
    // successful run, but is captured if we were already failing.
    if not Callbacks.CloseSession then
      if Result.Success then
      begin
        Result.Success := False;
        Result.AbortStage := reseSessionClose;
        Result.ErrorMessage :=
          'session close failed after successful routine';
      end;
  end;
end;

end.
