//------------------------------------------------------------------------------
//  OBD.OEM.Session
//
//  Session-negotiation contract for OEM diagnostic flows. Most
//  vendors follow the plain ISO 14229-1 §9.2 sequence (start
//  default session → security access → start extended /
//  programming session), but a non-trivial minority diverge —
//  pre-trigger handshakes, GoldenCheck-style supplier challenges,
//  StarCAN gateway routing, etc.
//
//  Every <see cref="IOBDOEMExtension"/> publishes a session
//  negotiator. The default <see cref="TOBDStandardSessionNegotiator"/>
//  walks the ISO 14229-1 baseline; vendors override
//  <c>OpenSession</c> / <c>CloseSession</c> /
//  <c>EnterProgrammingSession</c> with their own.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.OEM.Session.
//------------------------------------------------------------------------------

unit OBD.OEM.Session;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>Raised on session-negotiation errors.</summary>
  EOBDSession = class(Exception);

  /// <summary>UDS standard session sub-functions.</summary>
  TOBDSessionKind = (
    /// <summary>Default diagnostic session (sub 0x01).</summary>
    skDefault,
    /// <summary>Programming session (sub 0x02).</summary>
    skProgramming,
    /// <summary>Extended diagnostic session (sub 0x03).</summary>
    skExtended,
    /// <summary>Safety-system diagnostic session (sub 0x04).</summary>
    skSafetySystem
  );

  /// <summary>
  ///   Transport callback the negotiator uses to send raw UDS
  ///   service / body bytes and collect the response payload.
  ///   Hosts wire this to <c>TOBDProtocol.Request</c> or an
  ///   equivalent UDS-aware path.
  /// </summary>
  TOBDSessionRequest = reference to function(
    AServiceId: Byte; const ABody: TBytes): TBytes;

  /// <summary>
  ///   Session-negotiator contract.
  /// </summary>
  /// <remarks>
  ///   Each method receives a <c>TOBDSessionRequest</c> callback
  ///   so the negotiator stays transport-agnostic. Methods may
  ///   raise <see cref="EOBDSession"/> on a negative response or
  ///   protocol violation; on success they return the negotiated
  ///   session kind so the host knows which path it ended up in.
  /// </remarks>
  IOBDSessionNegotiator = interface
    ['{4E0B9F31-8B7C-43C2-8D5E-6F1A2C9E5D04}']
    /// <summary>Opens a diagnostic session.</summary>
    /// <param name="AKind">Target session.</param>
    /// <param name="ARequest">Transport callback.</param>
    /// <returns>The kind the ECU confirmed (may differ from the
    /// request when the ECU offers a different one).</returns>
    function OpenSession(AKind: TOBDSessionKind;
      const ARequest: TOBDSessionRequest): TOBDSessionKind;

    /// <summary>Closes the current session (drops back to
    /// <c>skDefault</c>).</summary>
    /// <param name="ARequest">Transport callback.</param>
    procedure CloseSession(const ARequest: TOBDSessionRequest);

    /// <summary>Convenience: open <c>skProgramming</c> after
    /// any vendor-specific pre-trigger handshake.</summary>
    /// <param name="ARequest">Transport callback.</param>
    /// <returns>The kind the ECU confirmed.</returns>
    function EnterProgrammingSession(
      const ARequest: TOBDSessionRequest): TOBDSessionKind;

    /// <summary>Returns the last negotiated session kind.</summary>
    function CurrentSession: TOBDSessionKind;
  end;

  /// <summary>
  ///   Plain ISO 14229-1 negotiator — issues
  ///   <c>10 &lt;sub&gt;</c> and verifies the
  ///   <c>50 &lt;sub&gt;</c> positive response.
  /// </summary>
  TOBDStandardSessionNegotiator = class(TInterfacedObject,
    IOBDSessionNegotiator)
  strict private
    FCurrent: TOBDSessionKind;
    function SubFunctionFor(AKind: TOBDSessionKind): Byte;
    function KindFor(ASubFunction: Byte): TOBDSessionKind;
  public
    /// <summary>Constructs the negotiator in
    /// <c>skDefault</c>.</summary>
    constructor Create;
    function OpenSession(AKind: TOBDSessionKind;
      const ARequest: TOBDSessionRequest): TOBDSessionKind;
    procedure CloseSession(const ARequest: TOBDSessionRequest);
    function EnterProgrammingSession(
      const ARequest: TOBDSessionRequest): TOBDSessionKind;
    function CurrentSession: TOBDSessionKind;
  end;

const
  /// <summary>UDS DiagnosticSessionControl SID.</summary>
  UDS_SID_DIAGNOSTIC_SESSION_CONTROL = $10;
  /// <summary>UDS positive-response offset.</summary>
  UDS_POSITIVE_RESPONSE_OFFSET       = $40;

implementation

constructor TOBDStandardSessionNegotiator.Create;
begin
  inherited;
  FCurrent := skDefault;
end;

function TOBDStandardSessionNegotiator.SubFunctionFor(
  AKind: TOBDSessionKind): Byte;
begin
  case AKind of
    skDefault:      Result := $01;
    skProgramming:  Result := $02;
    skExtended:     Result := $03;
    skSafetySystem: Result := $04;
  else
    Result := $01;
  end;
end;

function TOBDStandardSessionNegotiator.KindFor(
  ASubFunction: Byte): TOBDSessionKind;
begin
  case ASubFunction of
    $01: Result := skDefault;
    $02: Result := skProgramming;
    $03: Result := skExtended;
    $04: Result := skSafetySystem;
  else
    Result := skDefault;
  end;
end;

function TOBDStandardSessionNegotiator.OpenSession(AKind: TOBDSessionKind;
  const ARequest: TOBDSessionRequest): TOBDSessionKind;
var
  Req: TBytes;
  Resp: TBytes;
  EchoSub: Byte;
begin
  if not Assigned(ARequest) then
    raise EOBDSession.Create(
      'TOBDStandardSessionNegotiator.OpenSession: nil request callback');
  SetLength(Req, 1);
  Req[0] := SubFunctionFor(AKind);
  Resp := ARequest(UDS_SID_DIAGNOSTIC_SESSION_CONTROL, Req);
  if Length(Resp) < 1 then
    raise EOBDSession.CreateFmt(
      'DiagnosticSessionControl: short response (%d bytes)',
      [Length(Resp)]);
  EchoSub := Resp[0];
  FCurrent := KindFor(EchoSub);
  Result := FCurrent;
end;

procedure TOBDStandardSessionNegotiator.CloseSession(
  const ARequest: TOBDSessionRequest);
begin
  OpenSession(skDefault, ARequest);
end;

function TOBDStandardSessionNegotiator.EnterProgrammingSession(
  const ARequest: TOBDSessionRequest): TOBDSessionKind;
begin
  Result := OpenSession(skProgramming, ARequest);
end;

function TOBDStandardSessionNegotiator.CurrentSession: TOBDSessionKind;
begin
  Result := FCurrent;
end;

end.
