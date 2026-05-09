//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.SessionHelper.pas
// CONTENTS       : One-call wrapper for service-routine execution
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.SessionHelper;

interface

uses
  System.SysUtils,

  OBD.OEM.ServiceRoutines,
  OBD.ECU.Flashing.VoltageGate,
  OBD.UDS.NRC;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDOEMSessionHelper = class(Exception);

  /// <summary>Stage at which a routine execution finished or aborted.
  /// On success, the helper reports reseSessionClose; on failure it
  /// reports the stage that failed.</summary>
  TOBDRoutineExecutionStage = (
    reseNotStarted,
    reseSessionOpen,
    reseVoltageGate,
    reseRoutineStart,
    reseRoutineWait,
    reseResultRead,
    reseSessionClose
  );

  TOBDRoutineExecutionResult = record
    Success: Boolean;
    RoutineKey: string;
    AbortStage: TOBDRoutineExecutionStage;
    NRC: Byte;                  // 0 if no NRC was raised
    ErrorMessage: string;       // populated on failure
    MeasuredVolts: Single;      // 0 if voltage gate wasn't consulted
    ResultBytes: TBytes;        // ResultRead payload on success
  end;

  /// <summary>Open the diagnostic session at the given session-type byte
  /// (e.g. 0x03 = Extended). Return True on success; out-parameter NRC
  /// carries the negative-response byte on failure (0 if non-NRC error).</summary>
  TOBDSessionOpenCallback = reference to function(SessionType: Byte;
    out NRC: Byte): Boolean;

  /// <summary>Send the UDS 0x31 RoutineControl frame for the given
  /// routine. Frame is pre-built by BuildRoutineControlFrame. Return
  /// True on positive response.</summary>
  TOBDRoutineStartCallback = reference to function(const Frame: TBytes;
    out NRC: Byte): Boolean;

  /// <summary>Read the routine result via UDS 0x31 sub-function 0x03.
  /// Returns True + ResultBytes on positive response.</summary>
  TOBDRoutineResultCallback = reference to function(RID: Word;
    out ResultBytes: TBytes; out NRC: Byte): Boolean;

  /// <summary>Close the diagnostic session (return to default).</summary>
  TOBDSessionCloseCallback = reference to function: Boolean;

  /// <summary>Read the adapter battery voltage. Mirrors the
  /// TOBDVoltageReader signature from OBD.ECU.Flashing.VoltageGate so
  /// the gate can be reused as-is.</summary>
  TOBDOEMSessionVoltageReader = TOBDVoltageReader;

  /// <summary>Bundle of callbacks the helper needs. Production callers
  /// wire each to their TOBDDiagSession; tests inject lambdas.</summary>
  TOBDOEMSessionCallbacks = record
    OpenSession: TOBDSessionOpenCallback;
    StartRoutine: TOBDRoutineStartCallback;
    ReadResult: TOBDRoutineResultCallback;
    CloseSession: TOBDSessionCloseCallback;
    ReadVoltage: TOBDOEMSessionVoltageReader;  // optional; only consulted
                                               // when Routine.Safety = srsBatteryMin12V5
  end;

  TOBDOEMSessionHelper = class
  private
    FVoltageGate: TOBDProgrammingVoltageGate;
    FOwnsGate: Boolean;
    function ApplyVoltageGate(const Routine: TOBDServiceRoutine;
      const ReadVoltage: TOBDOEMSessionVoltageReader;
      var Res: TOBDRoutineExecutionResult): Boolean;
    procedure SetFailure(var Res: TOBDRoutineExecutionResult;
      Stage: TOBDRoutineExecutionStage; NRC: Byte; const Msg: string);
  public
    /// <summary>Construct with an optional pre-configured voltage gate.
    /// Pass nil to let the helper own a default gate (12.5 V threshold).
    /// </summary>
    constructor Create(VoltageGate: TOBDProgrammingVoltageGate = nil);
    destructor Destroy; override;

    function RunServiceRoutine(const Routine: TOBDServiceRoutine;
      const Callbacks: TOBDOEMSessionCallbacks): TOBDRoutineExecutionResult;

    /// <summary>Direct access to the configured voltage gate so callers
    /// can register OEM-specific thresholds (e.g. tesla -> 13.0 V).</summary>
    property VoltageGate: TOBDProgrammingVoltageGate read FVoltageGate;
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

constructor TOBDOEMSessionHelper.Create(VoltageGate: TOBDProgrammingVoltageGate);
begin
  inherited Create;
  if VoltageGate = nil then
  begin
    FVoltageGate := TOBDProgrammingVoltageGate.Create;
    FOwnsGate := True;
  end
  else
  begin
    FVoltageGate := VoltageGate;
    FOwnsGate := False;
  end;
end;

destructor TOBDOEMSessionHelper.Destroy;
begin
  if FOwnsGate then FVoltageGate.Free;
  inherited;
end;

procedure TOBDOEMSessionHelper.SetFailure(var Res: TOBDRoutineExecutionResult;
  Stage: TOBDRoutineExecutionStage; NRC: Byte; const Msg: string);
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
  const ReadVoltage: TOBDOEMSessionVoltageReader;
  var Res: TOBDRoutineExecutionResult): Boolean;
var
  GateResult: TOBDVoltageGateResult;
begin
  if Routine.Safety <> srsBatteryMin12V5 then
    Exit(True); // gate not required for this routine class
  if not Assigned(ReadVoltage) then
  begin
    SetFailure(Res, reseVoltageGate, 0,
      Format('routine %s requires voltage check but no ReadVoltage callback supplied',
        [Routine.Key]));
    Exit(False);
  end;
  GateResult := FVoltageGate.Check(ReadVoltage, '');
  Res.MeasuredVolts := GateResult.MeasuredVolts;
  if not GateResult.Passed then
  begin
    SetFailure(Res, reseVoltageGate, 0,
      'voltage gate failed: ' + GateResult.Reason);
    Exit(False);
  end;
  Result := True;
end;

function TOBDOEMSessionHelper.RunServiceRoutine(
  const Routine: TOBDServiceRoutine;
  const Callbacks: TOBDOEMSessionCallbacks): TOBDRoutineExecutionResult;
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
    raise EOBDOEMSessionHelper.Create('OpenSession callback required');
  if not Assigned(Callbacks.StartRoutine) then
    raise EOBDOEMSessionHelper.Create('StartRoutine callback required');
  if not Assigned(Callbacks.CloseSession) then
    raise EOBDOEMSessionHelper.Create('CloseSession callback required');

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
    if not ApplyVoltageGate(Routine, Callbacks.ReadVoltage, Result) then
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
          Format('routine %s result read failed', [Routine.Key]));
        Exit;
      end;
      Result.ResultBytes := ResultBytes;
    end;

    Result.Success := True;
    Result.AbortStage := reseSessionClose;
  finally
    // 5. Always attempt to close the session. Close failure does not
    // demote a successful run, but is captured if we were already
    // failing.
    if not Callbacks.CloseSession then
      if Result.Success then
      begin
        Result.Success := False;
        Result.AbortStage := reseSessionClose;
        Result.ErrorMessage := 'session close failed after successful routine';
      end;
  end;
end;

end.
