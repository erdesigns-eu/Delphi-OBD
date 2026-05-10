//------------------------------------------------------------------------------
//  OBD.Adapter.Detection
//
//  Chip-family detection routine. Sends a small fixed sequence of AT
//  / ST identification commands to whatever implements
//  IOBDAdapterCommandSender, parses the responses, and produces a
//  populated TOBDAdapterIdentity.
//
//  The detector is decoupled from TOBDAdapter so it can be exercised
//  in unit tests with a scripted command-sender that does not need a
//  real connection.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation: ATZ → ATE0 → ATI → AT@1 → AT@2
//                     → optional STI sequence with clone-signature
//                     heuristic.
//------------------------------------------------------------------------------

unit OBD.Adapter.Detection;

interface

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  OBD.Types,
  OBD.Adapter.Types;

type
  /// <summary>
  ///   Minimal contract a detection consumer implements. Sends a
  ///   command to the adapter and returns the parsed response.
  /// </summary>
  /// <remarks>
  ///   Implemented by <c>TOBDAdapter</c> (production) and by test
  ///   doubles (unit tests).
  /// </remarks>
  IOBDAdapterCommandSender = interface
    ['{2A7F4C0E-3D5E-4F1A-9C7B-8D6E5F4A3B2C}']
    /// <summary>Sends a command and waits for the response.</summary>
    /// <param name="ACommand">Command verb (already formatted).</param>
    /// <param name="ATimeoutMs">Timeout in milliseconds.</param>
    /// <returns>Parsed response.</returns>
    function SendCommand(const ACommand: string;
      ATimeoutMs: Cardinal): TOBDAdapterResponse;
  end;

  /// <summary>
  ///   Optional progress reporter used by the detector. The adapter
  ///   wires its own <c>FireProgress</c> here so detection phases
  ///   surface on <c>OnProgress</c>.
  /// </summary>
  TOBDDetectionProgress = reference to procedure(AIndex, ACount: Cardinal;
    const AName, ADetail: string);

  /// <summary>
  ///   Stateless chip-family detector.
  /// </summary>
  /// <remarks>
  ///   Detection sequence (six phases):
  ///   <list type="number">
  ///     <item><c>1/6 Resetting</c> — sends <c>ATZ</c>.</item>
  ///     <item><c>2/6 Echo off</c> — sends <c>ATE0</c>.</item>
  ///     <item><c>3/6 Reading ATI</c> — primary identity string.</item>
  ///     <item><c>4/6 Reading AT@1</c> — device description.</item>
  ///     <item><c>5/6 Reading AT@2</c> — device identifier.</item>
  ///     <item><c>6/6 Reading STI</c> — OBDLink-specific (skipped for
  ///         ELM327-only chips).</item>
  ///   </list>
  /// </remarks>
  TOBDAdapterDetector = class
  public
    /// <summary>
    ///   Runs the detection sequence and populates an identity
    ///   record.
    /// </summary>
    /// <param name="ASender">Command sender. Must not be
    /// <c>nil</c>.</param>
    /// <param name="AIdentity">Output identity. Always populated;
    /// fields the chip did not report stay empty.</param>
    /// <param name="AOnProgress">Optional progress callback. May be
    /// <c>nil</c>.</param>
    /// <param name="ATimeoutMs">Per-command timeout in
    /// milliseconds. Default 5000.</param>
    /// <returns>True when the <c>ATI</c> step produced a recognisable
    /// signature (ELM327 / OBDLink / STN…). False when <c>ATI</c>
    /// returned text the parser does not recognise; <c>AIdentity</c>
    /// still carries the raw response and the family stays at the
    /// fallback <c>afELM327</c>.</returns>
    /// <remarks>
    ///   Phases <c>4/6 AT@1</c> and <c>5/6 AT@2</c> are
    ///   <b>best-effort</b>: clones often respond <c>?</c> or time
    ///   out, in which case the corresponding identity field stays
    ///   empty. The <see cref="LooksLikeClone"/> heuristic uses
    ///   empty description / identifier on a v1.5 chip as a clone
    ///   signal.
    ///
    ///   Phase <c>6/6 STI</c> is skipped on non-OBDLink chips to
    ///   avoid the <c>?</c> noise from ELM327-only adapters.
    /// </remarks>
    /// <exception cref="EOBDAdapter"><c>ASender</c> is <c>nil</c>.</exception>
    class function Detect(const ASender: IOBDAdapterCommandSender;
      out AIdentity: TOBDAdapterIdentity;
      const AOnProgress: TOBDDetectionProgress = nil;
      ATimeoutMs: Cardinal = 5000): Boolean; static;

    /// <summary>
    ///   Parses the verbatim text of an <c>ATI</c> response into
    ///   family + firmware version.
    /// </summary>
    /// <param name="AInfoLine">Raw <c>ATI</c> response (one line).</param>
    /// <param name="AFamily">Output family.</param>
    /// <param name="AVersion">Output version string.</param>
    /// <returns>True when a known signature was matched.</returns>
    class function ParseInfoLine(const AInfoLine: string;
      out AFamily: TOBDAdapterFamily;
      out AVersion: string): Boolean; static;

    /// <summary>
    ///   Heuristic clone detector. Returns True if the assembled
    ///   identity has signs of being a clone (1.5 firmware reporting
    ///   newer features, identifier string is a vendor-spoof, …).
    /// </summary>
    /// <param name="AIdentity">Identity to inspect.</param>
    /// <returns>True when likely a clone.</returns>
    class function LooksLikeClone(
      const AIdentity: TOBDAdapterIdentity): Boolean; static;
  end;

implementation

{ ---- helpers ----------------------------------------------------------------- }

function FirstNonEmptyLine(const ALines: TArray<string>): string;
var
  I: Integer;
  Trimmed: string;
begin
  for I := 0 to High(ALines) do
  begin
    Trimmed := Trim(ALines[I]);
    if Trimmed <> '' then
      Exit(Trimmed);
  end;
  Result := '';
end;

procedure ReportProgress(const ACallback: TOBDDetectionProgress;
  AIndex, ACount: Cardinal; const AName, ADetail: string);
begin
  if Assigned(ACallback) then
    ACallback(AIndex, ACount, AName, ADetail);
end;

{ ---- TOBDAdapterDetector ----------------------------------------------------- }

class function TOBDAdapterDetector.ParseInfoLine(const AInfoLine: string;
  out AFamily: TOBDAdapterFamily; out AVersion: string): Boolean;
var
  Upper: string;
  M: TMatch;
begin
  Upper := UpperCase(AInfoLine);
  AFamily := afELM327;
  AVersion := '';
  Result := False;

  if Pos('OBDLINK', Upper) > 0 then
  begin
    AFamily := afOBDLink;
    M := TRegEx.Match(AInfoLine, '(\d+\.\d+(?:\.\d+)?)');
    if M.Success then
      AVersion := M.Value;
    Result := True;
    Exit;
  end;

  if Pos('ELM327', Upper) > 0 then
  begin
    AFamily := afELM327;
    M := TRegEx.Match(AInfoLine, 'v?(\d+\.\d+)', [TRegExOption.roIgnoreCase]);
    if M.Success and (M.Groups.Count > 1) then
      AVersion := M.Groups[1].Value;
    Result := True;
    Exit;
  end;

  if Pos('STN', Upper) > 0 then
  begin
    // STN1110 / STN2120 — OBDLink chip family
    AFamily := afOBDLink;
    M := TRegEx.Match(AInfoLine, '(\d+\.\d+(?:\.\d+)?)');
    if M.Success then
      AVersion := M.Value;
    Result := True;
    Exit;
  end;

  // Unrecognised; not a hard failure — caller may treat as a generic
  // ELM-compatible chip with empty version.
end;

class function TOBDAdapterDetector.LooksLikeClone(
  const AIdentity: TOBDAdapterIdentity): Boolean;
var
  V: string;
  Identifier: string;
  ContainsClone: Boolean;
begin
  Identifier := UpperCase(AIdentity.DeviceIdentifier);
  ContainsClone := (Pos('CLONE', Identifier) > 0) or
                   (Pos('SCANTOOL', UpperCase(AIdentity.Description)) > 0) and
                   (AIdentity.Family = afELM327);

  V := AIdentity.FirmwareVersion;
  // Genuine ELM327 firmware caps at 2.x; 1.5 / 1.5a clones are
  // common. The 1.5 figure alone is a hint, not proof — we couple it
  // with absent AT@1 / AT@2 fields and identifier heuristics.
  if (AIdentity.Family = afELM327) and (V = '1.5') and
     ((AIdentity.Description = '') or (AIdentity.DeviceIdentifier = '')) then
    Exit(True);

  Result := ContainsClone;
end;

class function TOBDAdapterDetector.Detect(
  const ASender: IOBDAdapterCommandSender;
  out AIdentity: TOBDAdapterIdentity;
  const AOnProgress: TOBDDetectionProgress;
  ATimeoutMs: Cardinal): Boolean;
var
  Resp: TOBDAdapterResponse;
  InfoLine: string;
  ATIRecognised: Boolean;
begin
  if ASender = nil then
    raise EOBDAdapter.Create('Detect: command sender is nil');

  AIdentity := MakeAdapterIdentity;
  ATIRecognised := False;

  // 1/6 Reset
  ReportProgress(AOnProgress, 1, 6, 'Resetting', 'ATZ');
  try
    ASender.SendCommand('ATZ', ATimeoutMs);
  except
    // Fall through; some adapters drop the first command after a power-up.
  end;

  // 2/6 Echo off — ensures clean ATI parsing
  ReportProgress(AOnProgress, 2, 6, 'Echo off', 'ATE0');
  try
    ASender.SendCommand('ATE0', ATimeoutMs);
  except
  end;

  // 3/6 Identify chip
  ReportProgress(AOnProgress, 3, 6, 'Reading ATI', '');
  Resp := ASender.SendCommand('ATI', ATimeoutMs);
  AIdentity.InfoResponse := Trim(Resp.Raw);
  InfoLine := FirstNonEmptyLine(Resp.Lines);
  if InfoLine = '' then
    InfoLine := AIdentity.InfoResponse;
  ATIRecognised := ParseInfoLine(InfoLine,
    AIdentity.Family, AIdentity.FirmwareVersion);

  // 4/6 Description.
  //
  // AT@1 is best-effort by design. Many ELM327 clones answer '?'
  // (treated by the parser as an error response, then by SendCommand
  // as a successful exchange — but with IsError = True). Some chips
  // raise on the request (timeout, bus glitch). We swallow either
  // outcome and leave Description empty; the LooksLikeClone heuristic
  // uses an empty Description as one of its signals.
  ReportProgress(AOnProgress, 4, 6, 'Reading AT@1', '');
  try
    Resp := ASender.SendCommand('AT@1', ATimeoutMs);
    if not Resp.IsError then
      AIdentity.Description := Trim(FirstNonEmptyLine(Resp.Lines));
  except
    // Swallow on purpose — see the AT@1 contract above.
  end;

  // 5/6 Identifier. Same swallow-and-continue contract as AT@1.
  ReportProgress(AOnProgress, 5, 6, 'Reading AT@2', '');
  try
    Resp := ASender.SendCommand('AT@2', ATimeoutMs);
    if not Resp.IsError then
      AIdentity.DeviceIdentifier := Trim(FirstNonEmptyLine(Resp.Lines));
  except
    // Swallow on purpose — see the AT@1 contract above.
  end;

  // 6/6 STI for OBDLink — skip for non-OBDLink families to avoid the
  // '?' invalid-command response on ELM327 chips.
  ReportProgress(AOnProgress, 6, 6, 'Reading STI', '');
  if AIdentity.Family = afOBDLink then
  begin
    try
      Resp := ASender.SendCommand('STI', ATimeoutMs);
      AIdentity.STInfo := Trim(FirstNonEmptyLine(Resp.Lines));
    except
    end;
  end;

  // Adapter key — best-effort canonicalisation.
  case AIdentity.Family of
    afELM327: AIdentity.AdapterKey := 'elm327';
    afOBDLink:
      begin
        if Pos('MX', UpperCase(AIdentity.STInfo + AIdentity.Description)) > 0 then
          AIdentity.AdapterKey := 'obdlink_mx'
        else if Pos('EX', UpperCase(AIdentity.STInfo + AIdentity.Description)) > 0 then
          AIdentity.AdapterKey := 'obdlink_ex'
        else if Pos('CX', UpperCase(AIdentity.STInfo + AIdentity.Description)) > 0 then
          AIdentity.AdapterKey := 'obdlink_cx'
        else
          AIdentity.AdapterKey := 'obdlink_mx';
      end;
    afJ2534:  AIdentity.AdapterKey := 'j2534';
    afDoIP:   AIdentity.AdapterKey := 'doip';
  end;

  // Display name — Description if present, else InfoResponse, else key.
  if AIdentity.Description <> '' then
    AIdentity.DisplayName := AIdentity.Description
  else if AIdentity.InfoResponse <> '' then
    AIdentity.DisplayName := AIdentity.InfoResponse
  else
    AIdentity.DisplayName := AIdentity.AdapterKey;

  AIdentity.IsClone := LooksLikeClone(AIdentity);
  Result := ATIRecognised;
end;

end.
