//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Session.pas
// CONTENTS       : OEM session negotiation — types, interface and
//                  ISO 14229 reference implementation
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Each OEM expresses its session-entry choreography
//                  as a *plan*: an ordered list of adapter (AT/ST) and
//                  UDS steps, plus a tester-present heartbeat spec.
//                  The plan is pure data — the runner in
//                  <c>OBD.OEM.Session.Runner</c> executes it against
//                  a connection. Keeps OBD.OEM free of async deps.
//------------------------------------------------------------------------------
unit OBD.OEM.Session;

interface

uses
  System.SysUtils, System.Classes;

type
  /// <summary>
  ///   ISO 14229-1 standard session subtypes (the byte that follows
  ///   SID 0x10) plus a small reserved range for OEM-specific session
  ///   subtypes that don't fit the standard four.
  /// </summary>
  TOBDSessionType = (
    sstDefault,                // 10 01
    sstProgramming,            // 10 02
    sstExtendedDiagnostic,     // 10 03
    sstSafetySystem,           // 10 04
    sstOEMSpecific1,           // 10 40 / 10 60 - vendor specific
    sstOEMSpecific2            // second vendor-specific slot
  );

  /// <summary>
  ///   One step in a session plan: an adapter command (AT/ST)
  ///   or a raw UDS frame.
  /// </summary>
  TOBDSessionStepKind = (sskATCommand, sskUDSRequest);

  TOBDSessionStep = record
    Kind: TOBDSessionStepKind;
    /// <summary>
    ///   Adapter command text, no leading "AT" prefix
    ///   (e.g. "SH 7E0", "CRA 7E8"). Only used when <c>Kind</c> = sskATCommand.
    /// </summary>
    AdapterCmd: string;
    /// <summary>
    ///   Raw UDS request bytes. Only used when <c>Kind</c> = sskUDSRequest.
    /// </summary>
    UDS: TBytes;
    /// <summary>
    ///   Optional expected response prefix; an empty array
    ///   means "any positive response is acceptable".
    /// </summary>
    ExpectedResponse: TBytes;
    /// <summary>
    ///   Per-step timeout (ms); 0 = use the runner's default.
    /// </summary>
    TimeoutMs: Cardinal;
    /// <summary>
    ///   Free-text label for logging / audit trails.
    /// </summary>
    Description: string;
  end;

  /// <summary>
  ///   A complete session-transition plan. Steps run in order; the
  ///   tester-present heartbeat starts after the final step settles
  ///   and continues until <c>EndSessionPlan</c> runs.
  /// </summary>
  TOBDSessionPlan = record
    Steps: TArray<TOBDSessionStep>;
    /// <summary>
    ///   Heartbeat interval after the plan completes;
    ///   0 = no heartbeat (default session).
    /// </summary>
    TesterPresentMs: Cardinal;
    /// <summary>
    ///   Bytes sent for tester-present (default ISO 14229: $3E $80
    ///   — sub-function "suppressPosRespMsgIndicationBit" set so the ECU
    ///   doesn't ACK every keep-alive).
    /// </summary>
    TesterPresentRequest: TBytes;
  end;

  /// <summary>
  ///   Each OEM extension returns one of these; it describes how the
  ///   manufacturer expects a tester to enter / exit each diagnostic
  ///   session. The framework's runner consumes the plan; the
  ///   negotiator itself doesn't talk to the connection.
  /// </summary>
  IOBDSessionNegotiator = interface
    ['{F4D5C8A1-3E7B-4D9F-8C2A-7B1E9F4D6C8A}']
    /// <summary>
    ///   Plan to enter <c>SessionType</c> on <c>ECUAddress</c>
    ///   (0 = use the connection's currently-active header).
    /// </summary>
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan;

    /// <summary>
    ///   Plan to leave the active session and return to default.
    ///   Always sends 10 01; OEMs that wrap that with extra teardown
    ///   (BMW, Mercedes) layer it on top.
    /// </summary>
    function EndSessionPlan(const ECUAddress: Word): TOBDSessionPlan;

    /// <summary>
    ///   True if this OEM expects a SecurityAccess (27 xx)
    ///   exchange immediately after entering the given session — flashing
    ///   programming sessions almost always do; extended diagnostic
    ///   sessions may or may not.
    /// </summary>
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean;

    /// <summary>
    ///   Tester-present interval (ms) the OEM expects for
    ///   non-default sessions. ISO 14229 default is 2000 ms; some OEMs
    ///   (Mercedes XENTRY, BMW E-Sys) recommend 1500 ms for older ECUs.
    /// </summary>
    function DefaultTesterPresentMs: Cardinal;

    /// <summary>
    ///   Display label for logs — e.g. "ISO 14229 standard",
    ///   "VAG TP 2.0", "BMW E-Sys".
    /// </summary>
    function DisplayName: string;
  end;

  /// <summary>
  ///   Pure ISO 14229-1 reference implementation. Used whenever an
  ///   OEM extension hasn't shipped its own negotiator. Subclasses
  ///   override the bits that differ — most OEMs only diverge on
  ///   <c>BeginSessionPlan</c>.
  /// </summary>
  TOBDStandardSessionNegotiator = class(TInterfacedObject, IOBDSessionNegotiator)
  protected
    /// <summary>
    ///   Helper for subclasses: prepend an "AT SH <hex>" step
    ///   when <c>ECUAddress &lt;&gt; 0</c>.
    /// </summary>
    function PrependHeaderStep(const Steps: TArray<TOBDSessionStep>;
      const ECUAddress: Word): TArray<TOBDSessionStep>;
  public
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan; virtual;
    function EndSessionPlan(const ECUAddress: Word): TOBDSessionPlan; virtual;
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean; virtual;
    function DefaultTesterPresentMs: Cardinal; virtual;
    function DisplayName: string; virtual;
  end;

/// <summary>
///   Build an adapter (AT/ST) step.
/// </summary>
function ATStep(const Cmd, Description: string): TOBDSessionStep; overload;
function ATStep(const Cmd, Description: string;
  TimeoutMs: Cardinal): TOBDSessionStep; overload;

/// <summary>
///   Build a UDS step from raw bytes.
/// </summary>
function UDSStep(const Bytes: TBytes; const Description: string): TOBDSessionStep; overload;
function UDSStep(const Bytes: TBytes; const ExpectedResponse: TBytes;
  const Description: string): TOBDSessionStep; overload;

/// <summary>
///   Encode <c>SessionType</c> as the byte that follows SID 0x10.
/// </summary>
function SessionTypeByte(SessionType: TOBDSessionType): Byte;

/// <summary>
///   Format a CAN-ID as the 3-hex-digit value AT SH expects.
/// </summary>
function FormatHeader(const ECUAddress: Word): string;

implementation

//------------------------------------------------------------------------------
// FORMAT HEADER
//------------------------------------------------------------------------------
function FormatHeader(const ECUAddress: Word): string;
begin
  // 11-bit IDs (the OBD-II range 0x000-0x7FF) need 3 hex chars; 29-bit
  // IDs need 8. The framework targets ISO 15765-4 11-bit by default;
  // 29-bit deployments can override the negotiator outright.
  if ECUAddress <= $7FF then
    Result := Format('%.3X', [ECUAddress])
  else
    Result := Format('%.8X', [ECUAddress]);
end;

//------------------------------------------------------------------------------
// SESSION TYPE BYTE
//------------------------------------------------------------------------------
function SessionTypeByte(SessionType: TOBDSessionType): Byte;
begin
  case SessionType of
    sstDefault:            Result := $01;
    sstProgramming:        Result := $02;
    sstExtendedDiagnostic: Result := $03;
    sstSafetySystem:       Result := $04;
    sstOEMSpecific1:       Result := $40;
    sstOEMSpecific2:       Result := $60;
  else
    Result := $03;
  end;
end;

//------------------------------------------------------------------------------
// ATSTEP
//------------------------------------------------------------------------------
function ATStep(const Cmd, Description: string): TOBDSessionStep;
begin
  Result := Default(TOBDSessionStep);
  Result.Kind := sskATCommand;
  Result.AdapterCmd := Cmd;
  Result.Description := Description;
end;

//------------------------------------------------------------------------------
// ATSTEP
//------------------------------------------------------------------------------
function ATStep(const Cmd, Description: string;
  TimeoutMs: Cardinal): TOBDSessionStep;
begin
  Result := ATStep(Cmd, Description);
  Result.TimeoutMs := TimeoutMs;
end;

//------------------------------------------------------------------------------
// UDSSTEP
//------------------------------------------------------------------------------
function UDSStep(const Bytes: TBytes; const Description: string): TOBDSessionStep;
begin
  Result := Default(TOBDSessionStep);
  Result.Kind := sskUDSRequest;
  Result.UDS := Bytes;
  Result.Description := Description;
end;

//------------------------------------------------------------------------------
// UDSSTEP
//------------------------------------------------------------------------------
function UDSStep(const Bytes: TBytes; const ExpectedResponse: TBytes;
  const Description: string): TOBDSessionStep;
begin
  Result := UDSStep(Bytes, Description);
  Result.ExpectedResponse := ExpectedResponse;
end;

//==============================================================================
// TOBDStandardSessionNegotiator
//==============================================================================

//------------------------------------------------------------------------------
// PREPEND HEADER STEP
//------------------------------------------------------------------------------
function TOBDStandardSessionNegotiator.PrependHeaderStep(
  const Steps: TArray<TOBDSessionStep>;
  const ECUAddress: Word): TArray<TOBDSessionStep>;
begin
  if ECUAddress = 0 then Exit(Steps);
  Result := [
    ATStep('SH ' + FormatHeader(ECUAddress),
           Format('Set request header to 0x%s', [FormatHeader(ECUAddress)]))
  ] + Steps;
end;

//------------------------------------------------------------------------------
// BEGIN SESSION PLAN
//------------------------------------------------------------------------------
function TOBDStandardSessionNegotiator.BeginSessionPlan(
  SessionType: TOBDSessionType;
  const ECUAddress: Word): TOBDSessionPlan;
var
  Sub: Byte;
begin
  Result := Default(TOBDSessionPlan);
  Sub := SessionTypeByte(SessionType);
  // Positive response to DiagnosticSessionControl is 50 <subfunction>.
  Result.Steps := PrependHeaderStep(
    [UDSStep(TBytes.Create($10, Sub), TBytes.Create($50, Sub),
       Format('DiagnosticSessionControl 0x%.2X', [Sub]))],
    ECUAddress);

  if SessionType = sstDefault then
    Result.TesterPresentMs := 0
  else
    Result.TesterPresentMs := DefaultTesterPresentMs;

  // ISO 14229: 3E 80 (suppress positive response).
  Result.TesterPresentRequest := TBytes.Create($3E, $80);
end;

//------------------------------------------------------------------------------
// END SESSION PLAN
//------------------------------------------------------------------------------
function TOBDStandardSessionNegotiator.EndSessionPlan(
  const ECUAddress: Word): TOBDSessionPlan;
begin
  Result := Default(TOBDSessionPlan);
  Result.Steps := PrependHeaderStep(
    [UDSStep(TBytes.Create($10, $01), TBytes.Create($50, $01),
       'Return to default session')],
    ECUAddress);
  Result.TesterPresentMs := 0;
  Result.TesterPresentRequest := TBytes.Create($3E, $80);
end;

//------------------------------------------------------------------------------
// REQUIRES SECURITY ACCESS
//------------------------------------------------------------------------------
function TOBDStandardSessionNegotiator.RequiresSecurityAccess(
  SessionType: TOBDSessionType): Boolean;
begin
  // Programming sessions almost always require security access; the
  // standard says nothing, so this is a sensible default that OEMs
  // can override.
  Result := SessionType = sstProgramming;
end;

//------------------------------------------------------------------------------
// DEFAULT TESTER PRESENT MS
//------------------------------------------------------------------------------
function TOBDStandardSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 2000;
end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDStandardSessionNegotiator.DisplayName: string;
begin
  Result := 'ISO 14229 standard';
end;

end.
