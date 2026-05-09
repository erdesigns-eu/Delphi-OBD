//------------------------------------------------------------------------------
//  OBD.Adapter.Types
//
//  Adapter-layer types: capability bits, identity record, command kind
//  enum, command record, command-result record, event signatures.
//
//  Has no dependencies beyond OBD.Types and the System RTL. The
//  TOBDAdapterFamily enum lives in OBD.Types because it is shared with
//  the connection layer; everything else specific to the adapter is
//  here.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ELM327 datasheet (Elm Electronics)
//    - OBDLink ST command reference
//    - SAE J2534 PassThru spec
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial: capability set, identity record,
//                     command kind enum, command record, AT/OBD-frame
//                     events.
//------------------------------------------------------------------------------

unit OBD.Adapter.Types;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types;

type
  /// <summary>
  ///   Capabilities a chip / adapter may declare.
  /// </summary>
  /// <remarks>
  ///   Stable enum order — values appear in JSON catalogues. Append at
  ///   the end; never reorder existing members.
  /// </remarks>
  TOBDAdapterCapability = (
    /// <summary>11/29-bit CAN at standard rates (250 / 500 kbps).</summary>
    acCAN,
    /// <summary>CAN-FD support.</summary>
    acCANFD,
    /// <summary>ISO 15765-2 transport protocol.</summary>
    acISOTP,
    /// <summary>Long-frame ISO-TP (e.g. CAN-FD, DoIP > 7 bytes).</summary>
    acISOTPLongFrame,
    /// <summary>Diagnostics over IP (ISO 13400).</summary>
    acDoIP,
    /// <summary>SAE J1939 (heavy-duty CAN).</summary>
    acJ1939,
    /// <summary>Pre-CAN K-line / L-line (ISO 9141, ISO 14230).</summary>
    acKLine,
    /// <summary>Battery-voltage measurement command (ATRV / equivalent).</summary>
    acVoltageMonitor,
    /// <summary>AUTOSAR Secure On-Board Communication.</summary>
    acSecureOnboard,
    /// <summary>SAE J2534 PassThru API.</summary>
    acJ2534,
    /// <summary>SAE J2534 PassThru v2 (CAN-FD-aware).</summary>
    acJ2534v2,
    /// <summary>OBDLink ST commands (extended OBDLink command set).</summary>
    acSTCommands,
    /// <summary>Adapter-side message filters / masks.</summary>
    acMessageFilters,
    /// <summary>Programmable headers (ATSH / equivalent).</summary>
    acProgrammableHeaders,
    /// <summary>Adapter-side multi-frame ISO-TP assembly.</summary>
    acAdapterMultiFrame
  );

  /// <summary>Set of <see cref="TOBDAdapterCapability"/>.</summary>
  TOBDAdapterCapabilities = set of TOBDAdapterCapability;

  /// <summary>
  ///   Snapshot of an adapter's identity, populated by
  ///   <c>TOBDAdapter.Detect</c>.
  /// </summary>
  /// <remarks>
  ///   Strings are normalised: trimmed, single-line, no leading
  ///   <c>></c> prompt. Fields that the adapter did not report are
  ///   empty.
  /// </remarks>
  TOBDAdapterIdentity = record
    /// <summary>Detected family (or <c>afELM327</c> as a fallback).</summary>
    Family: TOBDAdapterFamily;
    /// <summary>Catalogue key for this adapter (e.g. <c>'elm327'</c>,
    /// <c>'obdlink_mx'</c>, <c>'obdlink_ex'</c>). Lower-case, stable
    /// across releases.</summary>
    AdapterKey: string;
    /// <summary>Display name (e.g. <c>'OBDLink MX+'</c>).</summary>
    DisplayName: string;
    /// <summary>Firmware version string as reported by the chip
    /// (e.g. <c>'v1.5'</c>, <c>'4.2.0'</c>).</summary>
    FirmwareVersion: string;
    /// <summary>Verbatim response to <c>ATI</c> / <c>STI</c>.</summary>
    InfoResponse: string;
    /// <summary>Verbatim response to <c>AT@1</c> (description).</summary>
    Description: string;
    /// <summary>Verbatim response to <c>AT@2</c> (identifier — clones
    /// fill this with arbitrary text).</summary>
    DeviceIdentifier: string;
    /// <summary>Verbatim response to <c>STI</c> (OBDLink) or empty for
    /// other families.</summary>
    STInfo: string;
    /// <summary>Whether the chip advertises a known clone signature.</summary>
    IsClone: Boolean;
  end;

  /// <summary>
  ///   Kind of command sent to the adapter.
  /// </summary>
  TOBDAdapterCommandKind = (
    /// <summary>ELM327-style <c>AT</c> command (e.g. <c>ATZ</c>,
    /// <c>ATSP0</c>).</summary>
    ckAT,
    /// <summary>OBDLink-style <c>ST</c> command (e.g. <c>STI</c>,
    /// <c>STFMR</c>).</summary>
    ckST,
    /// <summary>OBD-II hex command (e.g. <c>'01 0C'</c>).</summary>
    ckOBD,
    /// <summary>Raw bytes sent verbatim, no prefix or framing.</summary>
    ckRaw
  );

  /// <summary>
  ///   Static description of a command in the adapter command catalogue.
  /// </summary>
  TOBDAdapterCommand = record
    /// <summary>Command kind — selects whether the verb is sent
    /// raw, with the AT/ST prefix already in <c>Verb</c>.</summary>
    Kind: TOBDAdapterCommandKind;
    /// <summary>Verb / template (e.g. <c>'ATZ'</c>, <c>'AT SP %d'</c>,
    /// <c>'STI'</c>).</summary>
    Verb: string;
    /// <summary>Brief description.</summary>
    Description: string;
    /// <summary>Capability required for this command. Empty set means
    /// "always available".</summary>
    RequiredCapabilities: TOBDAdapterCapabilities;
  end;

  /// <summary>
  ///   Result of a single command exchange.
  /// </summary>
  TOBDAdapterResponse = record
    /// <summary>The command that was sent (after parameter
    /// substitution).</summary>
    Command: string;
    /// <summary>Raw response text up to (but not including) the
    /// <c>></c> prompt. CRLFs are preserved.</summary>
    Raw: string;
    /// <summary>Lines extracted from <c>Raw</c> with leading / trailing
    /// whitespace and the trailing <c>></c> stripped.</summary>
    Lines: TArray<string>;
    /// <summary>Round-trip time in milliseconds.</summary>
    Elapsed: Cardinal;
    /// <summary>Whether the response begins with a known ELM327 error
    /// keyword (<c>NO DATA</c>, <c>BUS ERROR</c>, <c>UNABLE TO CONNECT</c>,
    /// <c>?</c>, …).</summary>
    IsError: Boolean;
    /// <summary>Single-line error keyword when <c>IsError</c> is True;
    /// otherwise empty.</summary>
    ErrorKeyword: string;
  end;

  /// <summary>
  ///   Event raised when a command response arrives.
  /// </summary>
  /// <param name="Sender">Adapter instance.</param>
  /// <param name="AResponse">Parsed response.</param>
  /// <remarks>Fires on the main thread.</remarks>
  TOBDAdapterResponseEvent = procedure(Sender: TObject;
    const AResponse: TOBDAdapterResponse) of object;

  /// <summary>
  ///   Event raised when adapter identity has been (re-)detected.
  /// </summary>
  /// <param name="Sender">Adapter instance.</param>
  /// <param name="AIdentity">Newly resolved identity.</param>
  /// <remarks>Fires on the main thread.</remarks>
  TOBDAdapterIdentityEvent = procedure(Sender: TObject;
    const AIdentity: TOBDAdapterIdentity) of object;

  /// <summary>
  ///   Adapter-level configuration error (e.g. command sent without an
  ///   active connection, command requires an unsupported capability).
  /// </summary>
  EOBDAdapter = class(EOBDError);

/// <summary>
///   Returns the canonical adapter-key for a family enum value
///   (e.g. <c>'elm327'</c> for <c>afELM327</c>).
/// </summary>
/// <param name="AFamily">Family enum value.</param>
/// <returns>Canonical lower-case identifier.</returns>
function AdapterFamilyKey(AFamily: TOBDAdapterFamily): string;

/// <summary>
///   Parses a capability key (case-insensitive) to its enum value.
/// </summary>
/// <param name="AKey">Capability key as it appears in JSON
/// (<c>'CAN'</c>, <c>'ISO-TP'</c>, <c>'KLine'</c>, …).</param>
/// <param name="ACap">Output enum value, valid only when the function
/// returns True.</param>
/// <returns>True when a capability matches.</returns>
function TryParseCapability(const AKey: string;
  out ACap: TOBDAdapterCapability): Boolean;

/// <summary>
///   Returns an empty <see cref="TOBDAdapterIdentity"/>.
/// </summary>
/// <returns>Identity record with <c>Family = afELM327</c> and all
/// strings empty.</returns>
function MakeAdapterIdentity: TOBDAdapterIdentity;

implementation

function AdapterFamilyKey(AFamily: TOBDAdapterFamily): string;
begin
  case AFamily of
    afELM327:  Result := 'elm327';
    afOBDLink: Result := 'obdlink';
    afJ2534:   Result := 'j2534';
    afDoIP:    Result := 'doip';
  else
    Result := 'unknown';
  end;
end;

function TryParseCapability(const AKey: string;
  out ACap: TOBDAdapterCapability): Boolean;
var
  Norm: string;
begin
  Norm := UpperCase(StringReplace(StringReplace(Trim(AKey),
    '-', '', [rfReplaceAll]), '_', '', [rfReplaceAll]));
  Result := True;
  if      Norm = 'CAN'                  then ACap := acCAN
  else if Norm = 'CANFD'                then ACap := acCANFD
  else if Norm = 'ISOTP'                then ACap := acISOTP
  else if (Norm = 'ISOTPLF') or (Norm = 'ISOTPLONGFRAME')
                                        then ACap := acISOTPLongFrame
  else if Norm = 'DOIP'                 then ACap := acDoIP
  else if Norm = 'J1939'                then ACap := acJ1939
  else if (Norm = 'KLINE') or (Norm = 'KL')
                                        then ACap := acKLine
  else if (Norm = 'VOLTAGEMONITOR') or (Norm = 'BATTERYVOLTAGE')
                                        then ACap := acVoltageMonitor
  else if Norm = 'SECUREONBOARD'        then ACap := acSecureOnboard
  else if Norm = 'J2534'                then ACap := acJ2534
  else if Norm = 'J2534V2'              then ACap := acJ2534v2
  else if (Norm = 'STCOMMANDS') or (Norm = 'ST')
                                        then ACap := acSTCommands
  else if Norm = 'MESSAGEFILTERS'       then ACap := acMessageFilters
  else if Norm = 'PROGRAMMABLEHEADERS'  then ACap := acProgrammableHeaders
  else if Norm = 'ADAPTERMULTIFRAME'    then ACap := acAdapterMultiFrame
  else Result := False;
end;

function MakeAdapterIdentity: TOBDAdapterIdentity;
begin
  Result.Family := afELM327;
  Result.AdapterKey := '';
  Result.DisplayName := '';
  Result.FirmwareVersion := '';
  Result.InfoResponse := '';
  Result.Description := '';
  Result.DeviceIdentifier := '';
  Result.STInfo := '';
  Result.IsClone := False;
end;

end.
