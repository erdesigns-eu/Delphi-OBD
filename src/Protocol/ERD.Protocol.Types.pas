// ------------------------------------------------------------------------------
// ERD.Protocol.Types
//
// Foundational protocol-layer types: request / response records,
// service-tag enums, error categories, frame kinds, NRC text loader
// hook. Has no dependencies beyond ERD.Types and ERD.Catalog.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - ISO 15031-5 (OBD-II services)
// - ISO 14229-1 (UDS service set + NRC catalogue)
// - ISO 14230-3 (KWP2000 services)
// - SAE J1939-73 (Diagnostics Messages)
//
// History     :
// 2026-05-09  ERD  Initial implementation.
// 2026-10-12  ERD  Add TOBDEcuMessage, TOBDELMHeaderMode, TOBDResponse.Messages.
// ------------------------------------------------------------------------------

unit ERD.Protocol.Types;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  ERD.Types;

type
  /// <summary>
  /// Application-layer protocol the request will be encoded in.
  /// </summary>
  /// <remarks>
  /// Distinct from <see cref="TOBDProtocolID"/> in
  /// <c>ERD.Types</c>, which is the wire-bus protocol (CAN /
  /// J1850 / etc.). One bus protocol can carry multiple
  /// application protocols — e.g. ISO 15765 carries OBD-II Mode
  /// 01–0A *and* UDS *and* WWH-ERD.
  /// </remarks>
  TOBDApplicationProtocol = (
    /// <summary>OBD-II Modes 01–0A (ISO 15031-5).</summary>
    apOBD2,
    /// <summary>UDS (ISO 14229).</summary>
    apUDS,
    /// <summary>KWP2000 (ISO 14230).</summary>
    apKWP2000,
    /// <summary>SAE J1939 (heavy-duty diagnostics messages).</summary>
    apJ1939,
    /// <summary>WWH-OBD (GTR No. 5; UDS-shaped).</summary>
    apWWHOBD,
    /// <summary>Diagnostics over IP (ISO 13400).</summary>
    apDoIP);

  /// <summary>
  /// Frame kind in an ISO-TP / multi-frame transport.
  /// </summary>
  TOBDFrameKind = (
    /// <summary>Single Frame (ISO-TP SF, payload &lt;= 7 bytes on
    /// classic CAN).</summary>
    fkSingle,
    /// <summary>First Frame of a multi-frame message.</summary>
    fkFirst,
    /// <summary>Consecutive Frame.</summary>
    fkConsecutive,
    /// <summary>Flow Control frame.</summary>
    fkFlowControl,
    /// <summary>Frame outside the ISO-TP categorisation
    /// (e.g. raw legacy line).</summary>
    fkRaw);

  /// <summary>
  /// A single low-level frame as observed on the wire.
  /// </summary>
  /// <remarks>
  /// For an ELM327-driven adapter this is constructed by parsing
  /// the chip's hex-text response. For raw-CAN adapters (J2534)
  /// it is constructed directly from the bus driver.
  /// </remarks>
  TOBDFrame = record
    /// <summary>CAN identifier (11 or 29 bit) or 0 for non-CAN
    /// transports.</summary>
    Id: Cardinal;
    /// <summary>True when <c>Id</c> is a 29-bit identifier.</summary>
    IsExtendedId: Boolean;
    /// <summary>Frame payload bytes (without the CAN ID).</summary>
    Payload: TBytes;
    /// <summary>Categorised frame kind (ISO-TP discriminator if
    /// applicable).</summary>
    Kind: TOBDFrameKind;
    /// <summary>Local capture timestamp.</summary>
    Timestamp: TDateTime;
  end;

  /// <summary>
  /// Application-level request to send.
  /// </summary>
  /// <remarks>
  /// <c>ServiceID</c> is the leading byte (e.g. 0x01 for OBD-II
  /// current data, 0x22 for UDS ReadDataByIdentifier). <c>Data</c>
  /// is the body that follows (PID, DID, sub-function, …). The
  /// protocol layer prepends <c>ServiceID</c> and any wire-level
  /// header bytes (CAN ID, KWP format byte, …) before transmit.
  /// </remarks>
  TOBDRequest = record
    /// <summary>Application protocol used to encode this request.</summary>
    Protocol: TOBDApplicationProtocol;
    /// <summary>Leading service / SID byte.</summary>
    ServiceID: Byte;
    /// <summary>Body bytes after the service ID.</summary>
    Data: TBytes;
    /// <summary>Optional override of the adapter-managed header /
    /// CAN ID. Empty string lets the adapter use its current
    /// setting.</summary>
    HeaderOverride: string;
    /// <summary>CAN receive filter for a routed request; empty clears filtering.</summary>
    ResponseHeaderOverride: string;
    /// <summary>Enable ISO-TP extended addressing for this routed request.</summary>
    UseExtendedAddressing: Boolean;
    /// <summary>Extended destination byte sent before the ISO-TP PCI.</summary>
    ExtendedTarget: Byte;
    /// <summary>Extended tester byte expected before the reply PCI.</summary>
    ExtendedTester: Byte;
    /// <summary>Per-request timeout in milliseconds. <c>0</c> uses
    /// the protocol component's default.</summary>
    TimeoutMs: Cardinal;
  end;

  /// <summary>
  /// How the adapter prints the header in front of each reply line.
  /// </summary>
  /// <remarks>
  /// Matches the ELM327 <c>ATH0</c> / <c>ATH1</c> setting. <c>ehAuto</c>
  /// recognises 11-bit CAN headers (an odd number of hex digits, e.g.
  /// <c>7E8 06 41 0C …</c>) and 29-bit OBD headers (<c>18DA…</c> /
  /// <c>18DB…</c>); legacy J1850 / ISO 9141 / KWP headers cannot be told
  /// apart from data and need <c>ehLegacy</c>.
  /// </remarks>
  TOBDELMHeaderMode = (
    /// <summary>Detect CAN headers per line; otherwise headers off.</summary>
    ehAuto,
    /// <summary>Headers off (<c>ATH0</c>, the library default).</summary>
    ehOff,
    /// <summary>11-bit CAN identifiers (3 hex digits).</summary>
    ehCAN11,
    /// <summary>29-bit CAN identifiers (8 hex digits).</summary>
    ehCAN29,
    /// <summary>Legacy 3-byte header plus trailing checksum byte
    /// (SAE J1850, ISO 9141-2, ISO 14230-4).</summary>
    ehLegacy);

  /// <summary>
  /// One complete message from one ECU, reassembled from the
  /// adapter's text output.
  /// </summary>
  /// <remarks>
  /// A functional OBD request (<c>7DF</c>) can be answered by several
  /// ECUs; each answer is a separate message. Multi-frame ISO-TP
  /// answers are joined and trimmed to their announced length.
  /// </remarks>
  TOBDEcuMessage = record
    /// <summary>Header as printed by the adapter (<c>'7E8'</c>,
    /// <c>'18DAF110'</c>, <c>'486B10'</c>). Empty with headers off.</summary>
    Header: string;
    /// <summary>Numeric CAN identifier (or the legacy source address);
    /// 0 with headers off.</summary>
    CanId: Cardinal;
    /// <summary>True for a 29-bit CAN identifier.</summary>
    IsExtendedId: Boolean;
    /// <summary>Full message payload, service ID first.</summary>
    Data: TBytes;
    /// <summary>False when a multi-frame message ended before its
    /// announced length was received.</summary>
    Complete: Boolean;
  end;

  /// <summary>
  /// Decoded response to a request.
  /// </summary>
  TOBDResponse = record
    /// <summary>Request that produced this response.</summary>
    Request: TOBDRequest;
    /// <summary>Service ID echoed back by the ECU (request SID + 0x40
    /// for positive response on UDS / OBD-II).</summary>
    ServiceID: Byte;
    /// <summary>Application payload (excludes the service ID) of the
    /// primary message: the first positive answer, else the first
    /// negative answer. See <c>Messages</c> for every ECU's answer.</summary>
    Data: TBytes;
    /// <summary>Every ECU message in the reply, in arrival order.
    /// "Response pending" (<c>7F xx 78</c>) messages are left out.</summary>
    Messages: TArray<TOBDEcuMessage>;
    /// <summary>Frames seen on the wire that contributed to this
    /// response. Useful for diagnostics / logging.</summary>
    Frames: TArray<TOBDFrame>;
    /// <summary>Round-trip elapsed milliseconds.</summary>
    Elapsed: Cardinal;
    /// <summary>True when the response is a UDS / KWP negative
    /// response (<c>0x7F service NRC</c>).</summary>
    IsNegative: Boolean;
    /// <summary>NRC byte when <c>IsNegative</c> is True.</summary>
    NRC: Byte;
    /// <summary>Resolved NRC text (looked up via <c>ERD.Catalog</c>);
    /// empty when <c>IsNegative</c> is False.</summary>
    NRCText: string;
  end;

  /// <summary>
  /// Event raised for every frame observed on the wire.
  /// </summary>
  /// <param name="Sender">Protocol component instance.</param>
  /// <param name="AFrame">Captured frame.</param>
  /// <remarks>Fires on the main thread.</remarks>
  TOBDProtocolFrameEvent = procedure(Sender: TObject; const AFrame: TOBDFrame)
    of object;

  /// <summary>
  /// Event raised for every successful response.
  /// </summary>
  /// <param name="Sender">Protocol component instance.</param>
  /// <param name="AResponse">Decoded response.</param>
  /// <remarks>Fires on the main thread.</remarks>
  TOBDProtocolResponseEvent = procedure(Sender: TObject;
    const AResponse: TOBDResponse) of object;

  /// <summary>
  /// Negative-response (NRC) callback for <c>OnNRC</c>.
  /// </summary>
  /// <param name="Sender">Protocol component instance.</param>
  /// <param name="ARequest">The request that triggered the NRC.</param>
  /// <param name="ANRC">Negative-response code byte.</param>
  /// <param name="AText">Resolved NRC text.</param>
  TOBDProtocolNRCEvent = procedure(Sender: TObject; const ARequest: TOBDRequest;
    ANRC: Byte; const AText: string) of object;

  /// <summary>Multi-listener variant of the protocol error event.
  /// Listeners observe transient errors; they cannot absorb them
  /// (the <c>var AHandled</c> contract belongs to the single
  /// <c>OnError</c> handler).</summary>
  TOBDProtocolErrorListenerEvent = procedure(Sender: TObject;
    ACode: TOBDErrorCode; const AMessage: string) of object;

  /// <summary>
  /// Multi-cast listener record for
  /// <c>TOBDProtocol.AddListener</c>. Set the fields you care
  /// about; leave the rest <c>nil</c>. The protocol fans every
  /// event out to all registered listeners on the main thread,
  /// alongside (not instead of) the single-cast OnXxx
  /// properties — so a host can wire its own OnFrame handler
  /// AND drop in a TOBDRecorder without either clobbering the
  /// other.
  /// </summary>
  TOBDProtocolListener = record
    OnFrame: TOBDProtocolFrameEvent;
    OnResponse: TOBDProtocolResponseEvent;
    OnNRC: TOBDProtocolNRCEvent;
    OnError: TOBDProtocolErrorListenerEvent;
  end;

  /// <summary>
  /// Programmer / configuration error raised by the protocol layer.
  /// </summary>
  EOBDProtocolErr = class(EOBDError);

  /// <summary>
  /// Returns an empty <see cref="TOBDRequest"/> with sensible defaults.
  /// </summary>
  /// <returns>Fresh request record.</returns>
function MakeOBDRequest: TOBDRequest;

/// <summary>
/// Returns an empty <see cref="TOBDResponse"/>.
/// </summary>
/// <returns>Fresh response record.</returns>
function MakeOBDResponse: TOBDResponse;

/// <summary>
/// Format raw bytes as a space-separated upper-case hex string
/// (e.g. <c>'01 0C'</c>). Used by the protocol layer to assemble
/// commands the ELM327 / OBDLink chip expects.
/// </summary>
/// <param name="ABytes">Bytes to format.</param>
/// <returns>Space-separated hex string. Empty for an empty input.</returns>
function BytesToHex(const ABytes: TBytes): string;

/// <summary>
/// Parse a space-separated hex string back into bytes. Tolerates
/// any whitespace (CR/LF/Tab) and ignores hex-only token cruft;
/// any non-hex character interrupts the parse.
/// </summary>
/// <param name="AText">Hex-and-whitespace text.</param>
/// <returns>Bytes. Empty when no recognisable hex byte was found.</returns>
function HexToBytes(const AText: string): TBytes;

/// <summary>
/// The positive answers of a response, one per ECU message.
/// </summary>
/// <param name="AResponse">Decoded response.</param>
/// <param name="APositiveSID">Positive-response service ID (request
/// SID + 0x40).</param>
/// <returns>Messages from <c>AResponse.Messages</c> that start with
/// <c>APositiveSID</c>. A response built by hand (empty
/// <c>Messages</c>) yields one message made of <c>ServiceID</c> +
/// <c>Data</c>, so callers work with mocks and replays too.</returns>
function OBDPositiveMessages(const AResponse: TOBDResponse;
  APositiveSID: Byte): TArray<TOBDEcuMessage>;

implementation

function MakeOBDRequest: TOBDRequest;
begin
  Result.Protocol := apOBD2;
  Result.ServiceID := 0;
  Result.Data := nil;
  Result.HeaderOverride := '';
  Result.ResponseHeaderOverride := '';
  Result.UseExtendedAddressing := False;
  Result.ExtendedTarget := 0;
  Result.ExtendedTester := 0;
  Result.TimeoutMs := 0;
end;

function MakeOBDResponse: TOBDResponse;
begin
  Result.Request := MakeOBDRequest;
  Result.ServiceID := 0;
  Result.Data := nil;
  Result.Messages := nil;
  Result.Frames := nil;
  Result.Elapsed := 0;
  Result.IsNegative := False;
  Result.NRC := 0;
  Result.NRCText := '';
end;

function OBDPositiveMessages(const AResponse: TOBDResponse;
  APositiveSID: Byte): TArray<TOBDEcuMessage>;
var
  I, N: Integer;
begin
  if Length(AResponse.Messages) = 0 then
  begin
    if AResponse.IsNegative or (AResponse.ServiceID <> APositiveSID) then
      Exit(nil);
    SetLength(Result, 1);
    Result[0].Header := '';
    Result[0].CanId := 0;
    Result[0].IsExtendedId := False;
    Result[0].Complete := True;
    SetLength(Result[0].Data, 1 + Length(AResponse.Data));
    Result[0].Data[0] := APositiveSID;
    if Length(AResponse.Data) > 0 then
      Move(AResponse.Data[0], Result[0].Data[1], Length(AResponse.Data));
    Exit;
  end;
  SetLength(Result, Length(AResponse.Messages));
  N := 0;
  for I := 0 to High(AResponse.Messages) do
    if (Length(AResponse.Messages[I].Data) > 0) and
      (AResponse.Messages[I].Data[0] = APositiveSID) then
    begin
      Result[N] := AResponse.Messages[I];
      Inc(N);
    end;
  SetLength(Result, N);
end;

function BytesToHex(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ABytes) do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(ABytes[I], 2);
  end;
end;

function IsHexDigit(C: Char): Boolean;
begin
  Result := CharInSet(C, ['0' .. '9', 'A' .. 'F', 'a' .. 'f']);
end;

function HexNibble(C: Char): Byte;
begin
  case C of
    '0' .. '9':
      Result := Ord(C) - Ord('0');
    'A' .. 'F':
      Result := Ord(C) - Ord('A') + 10;
    'a' .. 'f':
      Result := Ord(C) - Ord('a') + 10;
  else
    Result := 0;
  end;
end;

function HexToBytes(const AText: string): TBytes;
var
  I, Hi, Lo: Integer;
begin
  SetLength(Result, 0);
  I := 1;
  while I <= Length(AText) do
  begin
    if not IsHexDigit(AText[I]) then
    begin
      Inc(I);
      Continue;
    end;
    Hi := HexNibble(AText[I]);
    Inc(I);
    if (I > Length(AText)) or not IsHexDigit(AText[I]) then
      Break;
    Lo := HexNibble(AText[I]);
    Inc(I);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := (Hi shl 4) or Lo;
  end;
end;

end.
