//------------------------------------------------------------------------------
//  OBD.Types
//
//  Foundational types for the Delphi-OBD package.
//
//  Defines the enums, value carriers, and exception hierarchy that are
//  used by every other layer. Has no dependencies beyond the System RTL.
//
//  Author      : ERDesigns
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 15031-5 (OBD-II services 01..0A)
//    - ISO 14229-1 (UDS)
//    - ISO 14230   (KWP2000)
//    - ISO 15765-4 (CAN-TP for OBD)
//    - SAE J1939   (heavy-duty diagnostics)
//    - ISO 13400   (DoIP)
//
//  History     :
//    2026-05-09  ERD  Initial Phase 1 types: enums, TOBDValue, exception
//                     hierarchy.
//
//  Future work :
//    - Extend TOBDProtocolID once Phase 4 lands FlexRay / MOST flavours.
//    - Add SecOC freshness types when Phase 4 wires SecOC.
//------------------------------------------------------------------------------

unit OBD.Types;

interface

uses
  System.SysUtils,
  System.Variants;

type
  /// <summary>
  ///   Physical transport between the host application and an OBD adapter.
  /// </summary>
  /// <remarks>
  ///   Stable enum order. Values are streamed in DFM and JSON catalogues;
  ///   never reorder or remove members.
  /// </remarks>
  TOBDTransport = (
    /// <summary>RS-232 / virtual serial port over USB.</summary>
    otSerial,
    /// <summary>Bluetooth Classic (RFCOMM).</summary>
    otBluetooth,
    /// <summary>Bluetooth Low Energy (GATT).</summary>
    otBLE,
    /// <summary>TCP/IP over Wi-Fi (e.g. ELM327 Wi-Fi clones).</summary>
    otWiFi,
    /// <summary>UDP — primarily for DoIP and discovery.</summary>
    otUDP,
    /// <summary>Direct FTDI D2XX driver (skips the COM stack).</summary>
    otFTDI
  );

  /// <summary>
  ///   Family of an OBD adapter / interface chip.
  /// </summary>
  /// <remarks>
  ///   Detected automatically by <c>TOBDAdapter</c> when the adapter
  ///   identifies itself; can also be set manually for offline projects.
  /// </remarks>
  TOBDAdapterFamily = (
    /// <summary>ELM327 (Elm Electronics) and clones.</summary>
    afELM327,
    /// <summary>OBDLink (ScanTool / OBDSolutions, ST-extended).</summary>
    afOBDLink,
    /// <summary>SAE J2534 PassThru tool.</summary>
    afJ2534,
    /// <summary>DoIP gateway (no chip — the protocol is the adapter).</summary>
    afDoIP
  );

  /// <summary>
  ///   Wire protocol used between the OBD adapter and the vehicle bus.
  /// </summary>
  /// <remarks>
  ///   The numeric values match the <c>AT TP nn</c> / <c>AT SP nn</c>
  ///   protocol numbers used by ELM327. New entries are appended after
  ///   the existing last value and never inserted in the middle.
  /// </remarks>
  TOBDProtocolID = (
    /// <summary>Auto-detect — the adapter chooses.</summary>
    pidAuto,
    /// <summary>SAE J1850 PWM (Ford).</summary>
    pidJ1850PWM,
    /// <summary>SAE J1850 VPW (GM).</summary>
    pidJ1850VPW,
    /// <summary>ISO 9141-2 (Asian / European pre-CAN).</summary>
    pidISO9141_2,
    /// <summary>ISO 14230-4 KWP, 5-baud init.</summary>
    pidISO14230_4_KWP_5BAUD,
    /// <summary>ISO 14230-4 KWP, fast init.</summary>
    pidISO14230_4_KWP_FAST,
    /// <summary>ISO 15765-4 CAN, 11-bit ID, 500 kbps.</summary>
    pidISO15765_4_CAN_11_500,
    /// <summary>ISO 15765-4 CAN, 29-bit ID, 500 kbps.</summary>
    pidISO15765_4_CAN_29_500,
    /// <summary>ISO 15765-4 CAN, 11-bit ID, 250 kbps.</summary>
    pidISO15765_4_CAN_11_250,
    /// <summary>ISO 15765-4 CAN, 29-bit ID, 250 kbps.</summary>
    pidISO15765_4_CAN_29_250,
    /// <summary>SAE J1939 (heavy-duty CAN).</summary>
    pidSAE_J1939,
    /// <summary>User-defined CAN, 11-bit ID, 125 kbps.</summary>
    pidUserCAN_11_125,
    /// <summary>User-defined CAN, 29-bit ID, 50 kbps.</summary>
    pidUserCAN_29_50
  );

  /// <summary>
  ///   Transient error condition surfaced via the <c>OnError</c> event on
  ///   any component.
  /// </summary>
  /// <remarks>
  ///   These never raise an exception. Exceptions are reserved for
  ///   programmer / configuration errors (see <see cref="EOBDError"/>).
  /// </remarks>
  TOBDErrorCode = (
    /// <summary>No error. Sentinel value, not normally fired.</summary>
    oeNone,
    /// <summary>Operation timed out waiting for a response.</summary>
    oeTimeout,
    /// <summary>Adapter / ECU returned <c>NO DATA</c>.</summary>
    oeNoData,
    /// <summary>Adapter buffer overflow.</summary>
    oeBufferFull,
    /// <summary>Adapter reported a CAN / wire bus error.</summary>
    oeBusError,
    /// <summary>The requested PID is not supported by the ECU.</summary>
    oeUnsupportedPID,
    /// <summary>UDS Negative Response Code (NRC) received.</summary>
    oeNRC,
    /// <summary>Wire protocol mismatch (e.g. CAN frame on a non-CAN bus).</summary>
    oeProtocolMismatch,
    /// <summary>Frame failed checksum / CRC validation.</summary>
    oeChecksumFailed,
    /// <summary>Multi-frame reassembly failed (ISO-TP).</summary>
    oeReassemblyFailed,
    /// <summary>Adapter chip reported an internal failure.</summary>
    oeAdapterFault,
    /// <summary>Unsolicited / unexpected frame received.</summary>
    oeUnexpectedFrame,
    /// <summary>Catch-all for transient I/O glitches.</summary>
    oeIO
  );

  /// <summary>
  ///   Discriminator for the polymorphic value carried by <see cref="TOBDValue"/>.
  /// </summary>
  TOBDValueKind = (
    /// <summary>No value present (e.g. handler invoked with empty data).</summary>
    vkEmpty,
    /// <summary>Signed 64-bit integer in <c>AsInteger</c>.</summary>
    vkInteger,
    /// <summary>IEEE-754 double in <c>AsFloat</c>.</summary>
    vkFloat,
    /// <summary>UTF-8 string in <c>AsString</c> (e.g. VIN).</summary>
    vkString,
    /// <summary>Boolean in <c>AsBoolean</c>.</summary>
    vkBoolean,
    /// <summary>Bit field packed into <c>AsBitField</c>.</summary>
    vkBitField,
    /// <summary>Raw bytes only — no scaled interpretation.</summary>
    vkRawOnly
  );

  /// <summary>
  ///   Polymorphic value produced by a decoder when an ECU response is
  ///   parsed.
  /// </summary>
  /// <remarks>
  ///   The contract is "raw bytes plus one decoded view". The decoded
  ///   view is determined by <c>Kind</c>; consumers should switch on
  ///   <c>Kind</c> rather than try every <c>AsXxx</c> field.
  ///
  ///   Values are passed by value in events. Do not retain a pointer to
  ///   <c>Raw</c> beyond the scope of the handler — copy if needed.
  ///
  ///   <c>UnitName</c> uses SI units where applicable
  ///   (<c>°C</c>, <c>kPa</c>, <c>%</c>, <c>rpm</c>, <c>km/h</c>) and is
  ///   empty for unitless values.
  /// </remarks>
  TOBDValue = record
    /// <summary>Type of the decoded view; selects which <c>AsXxx</c>
    /// field is meaningful.</summary>
    Kind: TOBDValueKind;
    /// <summary>Raw bytes returned by the ECU for this identifier.</summary>
    Raw: TBytes;
    /// <summary>Integer view (<c>Kind = vkInteger</c>).</summary>
    AsInteger: Int64;
    /// <summary>Float view (<c>Kind = vkFloat</c>).</summary>
    AsFloat: Double;
    /// <summary>String view (<c>Kind = vkString</c>).</summary>
    AsString: string;
    /// <summary>Boolean view (<c>Kind = vkBoolean</c>).</summary>
    AsBoolean: Boolean;
    /// <summary>Bit field view (<c>Kind = vkBitField</c>).</summary>
    AsBitField: UInt64;
    /// <summary>SI unit name (<c>'°C'</c>, <c>'kPa'</c>, <c>'%'</c>, …).
    /// Empty for unitless values.</summary>
    UnitName: string;
    /// <summary>Symbol name as defined in the catalogue (e.g.
    /// <c>'Engine RPM'</c>).</summary>
    Name: string;
    /// <summary>Local time the value was decoded.</summary>
    Timestamp: TDateTime;
  end;

  /// <summary>
  ///   Static description of a single PID / DID / DM / DTC entry as
  ///   loaded from a JSON catalogue.
  /// </summary>
  /// <remarks>
  ///   Decoders consult this descriptor to scale raw bytes into a
  ///   <see cref="TOBDValue"/>. <c>DecoderName</c> is the registry key in
  ///   <c>OBD.Decoders</c>; <c>Scale</c>, <c>Offset</c>, <c>Min</c>,
  ///   <c>Max</c> tune linear / clamped decoders.
  /// </remarks>
  TOBDPIDDescriptor = record
    /// <summary>Numeric identifier of this entry within its mode/service.
    /// For OBD-II Mode 01 this is the PID; for UDS this is the DID.</summary>
    ID: Cardinal;
    /// <summary>Human-readable name (<c>'Engine RPM'</c>).</summary>
    Name: string;
    /// <summary>Number of payload bytes returned by the ECU
    /// (excluding the SID/PID echo).</summary>
    Length: Byte;
    /// <summary>Engineering unit of the decoded value (<c>'rpm'</c>,
    /// <c>'%'</c>, <c>'°C'</c>, …). Empty for unitless / strings.</summary>
    UnitName: string;
    /// <summary>Decoder registry key (<c>'linear'</c>, <c>'percentage'</c>,
    /// <c>'temperature'</c>, <c>'fueltrim'</c>, <c>'ascii'</c>, …).</summary>
    DecoderName: string;
    /// <summary>Linear scaling multiplier (decoder-dependent).</summary>
    Scale: Double;
    /// <summary>Linear scaling offset (decoder-dependent).</summary>
    Offset: Double;
    /// <summary>Lower clamp / range minimum, in scaled units.</summary>
    Min: Double;
    /// <summary>Upper clamp / range maximum, in scaled units.</summary>
    Max: Double;
    /// <summary>Optional human-readable explanation.</summary>
    Description: string;
  end;

  /// <summary>
  ///   Base exception for all programmer / configuration errors raised
  ///   by the Delphi-OBD package.
  /// </summary>
  /// <remarks>
  ///   Transient I/O issues (timeouts, NO-DATA, NRCs, bus errors) do
  ///   <b>not</b> raise — they fire <c>OnError</c>. Only catch
  ///   <see cref="EOBDError"/> if you want the bucket of misuse + assertion
  ///   failures.
  /// </remarks>
  EOBDError = class(Exception);

  /// <summary>
  ///   A configuration property is missing, contradictory, or invalid.
  /// </summary>
  /// <remarks>
  ///   Examples: setting <c>Active := True</c> on <c>TOBDConnection</c>
  ///   without choosing a transport; passing an empty PID list to a
  ///   one-shot read; <c>AutoExecute = False</c> with no
  ///   <c>OnConfirmExecute</c> handler.
  /// </remarks>
  EOBDConfig = class(EOBDError);

  /// <summary>
  ///   Operation requires an active connection but the target is
  ///   <c>nil</c> or inactive.
  /// </summary>
  EOBDNotConnected = class(EOBDError);

  /// <summary>
  ///   The wire frame failed to parse and there is no reasonable way to
  ///   continue. Indicates a programmer error in the layer below — log,
  ///   capture the frame, and report the bug.
  /// </summary>
  EOBDProtocol = class(EOBDError);

  /// <summary>
  ///   The requested feature is not supported by the bound adapter,
  ///   protocol, or ECU.
  /// </summary>
  /// <remarks>
  ///   Different from <c>oeUnsupportedPID</c> — that is a transient
  ///   per-PID condition the user can recover from. <c>EOBDUnsupported</c>
  ///   is structural ("you cannot do UDS on a J1850 PWM bus").
  /// </remarks>
  EOBDUnsupported = class(EOBDError);

  /// <summary>
  ///   Internal invariant violation. If you see this in production,
  ///   please file a bug report with the captured log.
  /// </summary>
  EOBDInternal = class(EOBDError);

/// <summary>
///   Returns an empty <see cref="TOBDValue"/> with <c>Kind = vkEmpty</c>
///   and <c>Timestamp</c> set to <c>Now</c>.
/// </summary>
/// <remarks>
///   Use this as a starting point when constructing a value from a
///   decoder; populate <c>Kind</c>, the relevant <c>AsXxx</c> field,
///   <c>Raw</c>, <c>UnitName</c>, and <c>Name</c> before returning.
/// </remarks>
function MakeOBDValue: TOBDValue;

/// <summary>
///   Returns the canonical lower-case decoder registry name for a
///   descriptor. Trims whitespace and folds case.
/// </summary>
/// <param name="ADescriptor">Descriptor whose decoder name to canonicalise.</param>
/// <returns>Lower-case trimmed decoder name; <c>'raw'</c> if absent.</returns>
function CanonicalDecoderName(const ADescriptor: TOBDPIDDescriptor): string;

implementation

function MakeOBDValue: TOBDValue;
begin
  Result.Kind := vkEmpty;
  Result.Raw := nil;
  Result.AsInteger := 0;
  Result.AsFloat := 0.0;
  Result.AsString := '';
  Result.AsBoolean := False;
  Result.AsBitField := 0;
  Result.UnitName := '';
  Result.Name := '';
  Result.Timestamp := Now;
end;

function CanonicalDecoderName(const ADescriptor: TOBDPIDDescriptor): string;
begin
  Result := LowerCase(Trim(ADescriptor.DecoderName));
  if Result = '' then
    Result := 'raw';
end;

end.
