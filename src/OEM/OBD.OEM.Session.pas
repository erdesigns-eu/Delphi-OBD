//------------------------------------------------------------------------------
//  OBD.OEM.Session
//
//  OEM session-negotiation contract. Vendors express their
//  diagnostic-session entry/exit choreography as a
//  <see cref="TOBDSessionPlan"/> — an ordered list of adapter
//  (AT/ST) commands and raw UDS frames, plus the tester-present
//  heartbeat the OEM expects. Plans are pure data; the runner in
//  <c>OBD.OEM.Session.Runner</c> executes them against a live
//  connection so this unit stays free of async / transport deps.
//
//  Every <see cref="IOBDOEMExtension"/> publishes an
//  <see cref="IOBDSessionNegotiator"/>; vendors that follow plain
//  ISO 14229-1 inherit <see cref="TOBDStandardSessionNegotiator"/>
//  and override only what they need to (typically just
//  <c>BeginSessionPlan</c> to layer in their pre-trigger AT
//  commands or vendor-specific keep-alive cadence).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation (open/close shape).
//    2026-05-12  ERD  Plan/step model with tester-present heartbeat.
//------------------------------------------------------------------------------

unit OBD.OEM.Session;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>Raised on session-negotiation errors.</summary>
  EOBDSession = class(Exception);

  /// <summary>
  ///   ISO 14229-1 session sub-functions plus two reserved slots
  ///   for OEM-specific session types that don't fit the standard
  ///   four (BMW E-Sys, Mercedes XENTRY, etc.).
  /// </summary>
  TOBDSessionType = (
    /// <summary>Default diagnostic session (sub 0x01).</summary>
    sstDefault,
    /// <summary>Programming session (sub 0x02).</summary>
    sstProgramming,
    /// <summary>Extended diagnostic session (sub 0x03).</summary>
    sstExtendedDiagnostic,
    /// <summary>Safety-system diagnostic session (sub 0x04).</summary>
    sstSafetySystem,
    /// <summary>OEM-specific session slot 1 (default sub 0x40).</summary>
    sstOEMSpecific1,
    /// <summary>OEM-specific session slot 2 (default sub 0x60).</summary>
    sstOEMSpecific2);

  /// <summary>One step inside a <see cref="TOBDSessionPlan"/>.</summary>
  TOBDSessionStepKind = (
    /// <summary>Adapter (AT/ST) command.</summary>
    sskATCommand,
    /// <summary>Raw UDS request.</summary>
    sskUDSRequest);

  /// <summary>One step inside a session plan.</summary>
  TOBDSessionStep = record
    /// <summary>Step kind.</summary>
    Kind: TOBDSessionStepKind;
    /// <summary>
    ///   Adapter command without the leading <c>AT</c> prefix
    ///   (e.g. <c>SH 7E0</c>, <c>CRA 7E8</c>). Only used when
    ///   <c>Kind = sskATCommand</c>.
    /// </summary>
    AdapterCmd: string;
    /// <summary>
    ///   Raw UDS request bytes. Only used when
    ///   <c>Kind = sskUDSRequest</c>.
    /// </summary>
    UDS: TBytes;
    /// <summary>
    ///   Optional positive-response prefix; an empty array means
    ///   any positive response is acceptable.
    /// </summary>
    ExpectedResponse: TBytes;
    /// <summary>Per-step timeout in ms; 0 = runner default.</summary>
    TimeoutMs: Cardinal;
    /// <summary>Free-text label for logs / audit trails.</summary>
    Description: string;
  end;

  /// <summary>
  ///   Complete session-transition plan.
  /// </summary>
  /// <remarks>
  ///   <para><c>Steps</c> are executed in order; the tester-present
  ///   heartbeat starts after the final step settles and continues
  ///   until <see cref="IOBDSessionNegotiator.EndSessionPlan"/>
  ///   runs.</para>
  /// </remarks>
  TOBDSessionPlan = record
    /// <summary>Plan steps in execution order.</summary>
    Steps: TArray<TOBDSessionStep>;
    /// <summary>Tester-present interval (ms); 0 = no heartbeat.</summary>
    TesterPresentMs: Cardinal;
    /// <summary>
    ///   Bytes sent for tester-present; ISO 14229 default is
    ///   <c>$3E $80</c> (suppress positive response).
    /// </summary>
    TesterPresentRequest: TBytes;
  end;

  /// <summary>
  ///   Manufacturer session-negotiation contract.
  /// </summary>
  /// <remarks>
  ///   <para>Each OEM extension publishes one of these. The
  ///   negotiator only builds plans — it never talks to the
  ///   connection. The plan runner in
  ///   <c>OBD.OEM.Session.Runner</c> executes the plan against the
  ///   live transport.</para>
  /// </remarks>
  IOBDSessionNegotiator = interface
    ['{F4D5C8A1-3E7B-4D9F-8C2A-7B1E9F4D6C8A}']
    /// <summary>
    ///   Plan to enter <c>ASessionType</c> on <c>AEcuAddress</c>.
    /// </summary>
    /// <param name="ASessionType">Target session.</param>
    /// <param name="AEcuAddress">CAN-ID (0 = use current header).</param>
    function BeginSessionPlan(ASessionType: TOBDSessionType;
      const AEcuAddress: Word): TOBDSessionPlan;
    /// <summary>
    ///   Plan to leave the active session and return to default.
    /// </summary>
    /// <param name="AEcuAddress">CAN-ID (0 = use current header).</param>
    function EndSessionPlan(const AEcuAddress: Word): TOBDSessionPlan;
    /// <summary>
    ///   <c>True</c> if entering <c>ASessionType</c> requires a
    ///   SecurityAccess (SID 0x27) exchange.
    /// </summary>
    /// <param name="ASessionType">Target session.</param>
    function RequiresSecurityAccess(ASessionType: TOBDSessionType): Boolean;
    /// <summary>Tester-present interval (ms) for non-default
    /// sessions.</summary>
    function DefaultTesterPresentMs: Cardinal;
    /// <summary>Display label for logs.</summary>
    function DisplayName: string;
  end;

  /// <summary>
  ///   Pure ISO 14229-1 reference negotiator. Vendor negotiators
  ///   typically inherit from this and override only
  ///   <c>BeginSessionPlan</c> (to inject pre-trigger AT commands
  ///   or tweak tester-present) and <c>DisplayName</c>.
  /// </summary>
  TOBDStandardSessionNegotiator = class(TInterfacedObject,
    IOBDSessionNegotiator)
  protected
    /// <summary>
    ///   Helper: prepend an <c>AT SH &lt;hex&gt;</c> step when
    ///   <c>AEcuAddress &lt;&gt; 0</c>.
    /// </summary>
    /// <param name="ASteps">Inner steps.</param>
    /// <param name="AEcuAddress">CAN-ID (0 = no header step).</param>
    function PrependHeaderStep(const ASteps: TArray<TOBDSessionStep>;
      const AEcuAddress: Word): TArray<TOBDSessionStep>;
  public
    function BeginSessionPlan(ASessionType: TOBDSessionType;
      const AEcuAddress: Word): TOBDSessionPlan; virtual;
    function EndSessionPlan(
      const AEcuAddress: Word): TOBDSessionPlan; virtual;
    function RequiresSecurityAccess(
      ASessionType: TOBDSessionType): Boolean; virtual;
    function DefaultTesterPresentMs: Cardinal; virtual;
    function DisplayName: string; virtual;
  end;

const
  /// <summary>UDS DiagnosticSessionControl SID.</summary>
  UDS_SID_DIAGNOSTIC_SESSION_CONTROL = $10;
  /// <summary>UDS positive-response offset.</summary>
  UDS_POSITIVE_RESPONSE_OFFSET       = $40;
  /// <summary>UDS TesterPresent SID.</summary>
  UDS_SID_TESTER_PRESENT             = $3E;

/// <summary>Builds an adapter (AT/ST) step.</summary>
/// <param name="ACmd">Command body, no leading <c>AT</c>.</param>
/// <param name="ADescription">Log label.</param>
function ATStep(const ACmd, ADescription: string): TOBDSessionStep; overload;
/// <summary>Builds an adapter (AT/ST) step with an explicit
/// timeout.</summary>
/// <param name="ACmd">Command body, no leading <c>AT</c>.</param>
/// <param name="ADescription">Log label.</param>
/// <param name="ATimeoutMs">Per-step timeout in ms.</param>
function ATStep(const ACmd, ADescription: string;
  ATimeoutMs: Cardinal): TOBDSessionStep; overload;

/// <summary>Builds a UDS step.</summary>
/// <param name="ABytes">Raw UDS request bytes.</param>
/// <param name="ADescription">Log label.</param>
function UDSStep(const ABytes: TBytes;
  const ADescription: string): TOBDSessionStep; overload;
/// <summary>Builds a UDS step with an expected positive-response
/// prefix.</summary>
/// <param name="ABytes">Raw UDS request bytes.</param>
/// <param name="AExpectedResponse">Expected positive-response prefix.</param>
/// <param name="ADescription">Log label.</param>
function UDSStep(const ABytes, AExpectedResponse: TBytes;
  const ADescription: string): TOBDSessionStep; overload;

/// <summary>Encodes <c>ASessionType</c> as the byte that follows
/// SID 0x10.</summary>
/// <param name="ASessionType">Session.</param>
function SessionTypeByte(ASessionType: TOBDSessionType): Byte;

/// <summary>Formats a CAN-ID as 3-hex-digit (11-bit) or 8-hex-digit
/// (29-bit) text for <c>AT SH</c>.</summary>
/// <param name="AEcuAddress">CAN-ID.</param>
function FormatHeader(const AEcuAddress: Word): string;

implementation

function FormatHeader(const AEcuAddress: Word): string;
begin
  if AEcuAddress <= $7FF then
    Result := Format('%.3X', [AEcuAddress])
  else
    Result := Format('%.8X', [AEcuAddress]);
end;

function SessionTypeByte(ASessionType: TOBDSessionType): Byte;
begin
  case ASessionType of
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

function ATStep(const ACmd, ADescription: string): TOBDSessionStep;
begin
  Result := Default(TOBDSessionStep);
  Result.Kind := sskATCommand;
  Result.AdapterCmd := ACmd;
  Result.Description := ADescription;
end;

function ATStep(const ACmd, ADescription: string;
  ATimeoutMs: Cardinal): TOBDSessionStep;
begin
  Result := ATStep(ACmd, ADescription);
  Result.TimeoutMs := ATimeoutMs;
end;

function UDSStep(const ABytes: TBytes;
  const ADescription: string): TOBDSessionStep;
begin
  Result := Default(TOBDSessionStep);
  Result.Kind := sskUDSRequest;
  Result.UDS := ABytes;
  Result.Description := ADescription;
end;

function UDSStep(const ABytes, AExpectedResponse: TBytes;
  const ADescription: string): TOBDSessionStep;
begin
  Result := UDSStep(ABytes, ADescription);
  Result.ExpectedResponse := AExpectedResponse;
end;

{ TOBDStandardSessionNegotiator }

function TOBDStandardSessionNegotiator.PrependHeaderStep(
  const ASteps: TArray<TOBDSessionStep>;
  const AEcuAddress: Word): TArray<TOBDSessionStep>;
begin
  if AEcuAddress = 0 then
    Exit(ASteps);
  Result := [
    ATStep('SH ' + FormatHeader(AEcuAddress),
      Format('Set request header to 0x%s',
        [FormatHeader(AEcuAddress)]))
  ] + ASteps;
end;

function TOBDStandardSessionNegotiator.BeginSessionPlan(
  ASessionType: TOBDSessionType;
  const AEcuAddress: Word): TOBDSessionPlan;
var
  Sub: Byte;
begin
  Result := Default(TOBDSessionPlan);
  Sub := SessionTypeByte(ASessionType);
  Result.Steps := PrependHeaderStep(
    [UDSStep(
      TBytes.Create(UDS_SID_DIAGNOSTIC_SESSION_CONTROL, Sub),
      TBytes.Create(UDS_SID_DIAGNOSTIC_SESSION_CONTROL +
        UDS_POSITIVE_RESPONSE_OFFSET, Sub),
      Format('DiagnosticSessionControl 0x%.2X', [Sub]))],
    AEcuAddress);
  if ASessionType = sstDefault then
    Result.TesterPresentMs := 0
  else
    Result.TesterPresentMs := DefaultTesterPresentMs;
  Result.TesterPresentRequest := TBytes.Create(UDS_SID_TESTER_PRESENT, $80);
end;

function TOBDStandardSessionNegotiator.EndSessionPlan(
  const AEcuAddress: Word): TOBDSessionPlan;
begin
  Result := Default(TOBDSessionPlan);
  Result.Steps := PrependHeaderStep(
    [UDSStep(
      TBytes.Create(UDS_SID_DIAGNOSTIC_SESSION_CONTROL, $01),
      TBytes.Create(UDS_SID_DIAGNOSTIC_SESSION_CONTROL +
        UDS_POSITIVE_RESPONSE_OFFSET, $01),
      'Return to default session')],
    AEcuAddress);
  Result.TesterPresentMs := 0;
  Result.TesterPresentRequest := TBytes.Create(UDS_SID_TESTER_PRESENT, $80);
end;

function TOBDStandardSessionNegotiator.RequiresSecurityAccess(
  ASessionType: TOBDSessionType): Boolean;
begin
  Result := ASessionType = sstProgramming;
end;

function TOBDStandardSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 2000;
end;

function TOBDStandardSessionNegotiator.DisplayName: string;
begin
  Result := 'ISO 14229 standard';
end;

end.
