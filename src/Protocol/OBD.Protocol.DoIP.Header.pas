//------------------------------------------------------------------------------
//  OBD.Protocol.DoIP.Header
//
//  ISO 13400-2 DoIP frame header (8 bytes):
//
//    Byte 0      Protocol version (0x02 = ISO 13400-2:2012,
//                                  0x03 = ISO 13400-2:2019)
//    Byte 1      Inverse protocol version (~ProtocolVersion)
//    Byte 2..3   Payload type (big-endian)
//    Byte 4..7   Payload length (big-endian)
//    Byte 8..n   Payload bytes
//
//  This unit owns the header record, payload-type enum, encoder /
//  decoder, and Generic-NACK reasons. Per-payload encoders live in
//  OBD.Protocol.DoIP.Messages.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 13400-2:2019 §6 (Generic header structure)
//    - ISO 13400-2:2019 Table 17 (Payload-type values)
//    - ISO 13400-2:2019 §7.1.5 (Generic header NACK codes)
//
//  History     :
//    2026-05-09  ERD  Phase 4d initial.
//------------------------------------------------------------------------------

unit OBD.Protocol.DoIP.Header;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.Types;

const
  /// <summary>DoIP fixed protocol version (2019 edition).</summary>
  DOIP_PROTOCOL_VERSION_2019 = $03;
  /// <summary>DoIP fixed protocol version (2012 edition).</summary>
  DOIP_PROTOCOL_VERSION_2012 = $02;
  /// <summary>Header length in bytes.</summary>
  DOIP_HEADER_LENGTH = 8;

  // ---- Payload type values (ISO 13400-2 Table 17) ----
  DOIP_PT_GenericNACK                    = $0000;
  DOIP_PT_VehicleIDRequest               = $0001;
  DOIP_PT_VehicleIDRequestEID            = $0002;
  DOIP_PT_VehicleIDRequestVIN            = $0003;
  DOIP_PT_VehicleAnnouncement            = $0004;
  DOIP_PT_RoutingActivationRequest       = $0005;
  DOIP_PT_RoutingActivationResponse      = $0006;
  DOIP_PT_AliveCheckRequest              = $0007;
  DOIP_PT_AliveCheckResponse             = $0008;
  DOIP_PT_EntityStatusRequest            = $4001;
  DOIP_PT_EntityStatusResponse           = $4002;
  DOIP_PT_PowerModeInfoRequest           = $4003;
  DOIP_PT_PowerModeInfoResponse          = $4004;
  DOIP_PT_DiagnosticMessage              = $8001;
  DOIP_PT_DiagnosticMessagePosAck        = $8002;
  DOIP_PT_DiagnosticMessageNegAck        = $8003;

  // ---- Generic header NACK codes (ISO 13400-2 Table 18) ----
  DOIP_NACK_IncorrectPattern    = $00;
  DOIP_NACK_UnknownPayloadType  = $01;
  DOIP_NACK_MessageTooLarge     = $02;
  DOIP_NACK_OutOfMemory         = $03;
  DOIP_NACK_InvalidPayloadLen   = $04;

  // ---- Standard TCP / UDP ports (ISO 13400-2 §5.6) ----
  DOIP_TCP_DATA_PORT            = 13400;
  DOIP_UDP_DISCOVERY_PORT       = 13400;
  DOIP_TLS_DATA_PORT            = 3496;

type
  /// <summary>
  ///   Decoded DoIP header.
  /// </summary>
  TOBDDoIPHeader = record
    /// <summary>Protocol version byte (0x02 or 0x03).</summary>
    ProtocolVersion: Byte;
    /// <summary>Inverse-protocol-version byte.</summary>
    InverseProtocolVersion: Byte;
    /// <summary>Payload type (big-endian on the wire).</summary>
    PayloadType: Word;
    /// <summary>Payload length (big-endian on the wire).</summary>
    PayloadLength: Cardinal;
  end;

/// <summary>
///   Returns a default DoIP header for the 2019 edition with the
///   supplied payload type and length.
/// </summary>
/// <param name="APayloadType">Payload type code.</param>
/// <param name="APayloadLength">Payload byte count (excludes the
/// header itself).</param>
/// <returns>Initialised header record.</returns>
function MakeDoIPHeader(APayloadType: Word;
  APayloadLength: Cardinal): TOBDDoIPHeader;

/// <summary>
///   Encodes a DoIP header to its 8-byte wire form.
/// </summary>
/// <param name="AHeader">Header to encode.</param>
/// <returns>8 bytes ready to prepend to the payload.</returns>
function EncodeDoIPHeader(const AHeader: TOBDDoIPHeader): TBytes;

/// <summary>
///   Decodes a DoIP header from its 8-byte wire form.
/// </summary>
/// <param name="ABytes">Buffer containing at least 8 bytes.</param>
/// <param name="AHeader">Output header.</param>
/// <returns>True on success, False when buffer too short or the
/// inverse-version check fails.</returns>
function DecodeDoIPHeader(const ABytes: TBytes;
  out AHeader: TOBDDoIPHeader): Boolean;

/// <summary>
///   Validates a DoIP header. Returns the Generic-NACK code that
///   should be sent back when invalid; <c>$FF</c> means valid.
/// </summary>
/// <param name="AHeader">Header to validate.</param>
/// <param name="AMaxPayload">Implementation maximum payload length.</param>
/// <returns>Generic-NACK code (0x00..0x04) or <c>$FF</c> when ok.</returns>
function ValidateDoIPHeader(const AHeader: TOBDDoIPHeader;
  AMaxPayload: Cardinal = $00FFFFFF): Byte;

/// <summary>
///   Returns a human-readable description of a payload-type code.
///   For unknown codes returns <c>'PayloadType 0xNNNN'</c>.
/// </summary>
function DoIPPayloadTypeName(AType: Word): string;

implementation

function MakeDoIPHeader(APayloadType: Word;
  APayloadLength: Cardinal): TOBDDoIPHeader;
begin
  Result.ProtocolVersion := DOIP_PROTOCOL_VERSION_2019;
  Result.InverseProtocolVersion := Byte(not DOIP_PROTOCOL_VERSION_2019);
  Result.PayloadType := APayloadType;
  Result.PayloadLength := APayloadLength;
end;

function EncodeDoIPHeader(const AHeader: TOBDDoIPHeader): TBytes;
begin
  SetLength(Result, DOIP_HEADER_LENGTH);
  Result[0] := AHeader.ProtocolVersion;
  Result[1] := AHeader.InverseProtocolVersion;
  Result[2] := Byte((AHeader.PayloadType shr 8) and $FF);
  Result[3] := Byte(AHeader.PayloadType and $FF);
  Result[4] := Byte((AHeader.PayloadLength shr 24) and $FF);
  Result[5] := Byte((AHeader.PayloadLength shr 16) and $FF);
  Result[6] := Byte((AHeader.PayloadLength shr 8) and $FF);
  Result[7] := Byte(AHeader.PayloadLength and $FF);
end;

function DecodeDoIPHeader(const ABytes: TBytes;
  out AHeader: TOBDDoIPHeader): Boolean;
begin
  Result := False;
  if Length(ABytes) < DOIP_HEADER_LENGTH then Exit;
  AHeader.ProtocolVersion := ABytes[0];
  AHeader.InverseProtocolVersion := ABytes[1];
  if AHeader.ProtocolVersion <> Byte(not AHeader.InverseProtocolVersion) then
    Exit;
  AHeader.PayloadType :=
    (Word(ABytes[2]) shl 8) or Word(ABytes[3]);
  AHeader.PayloadLength :=
    (Cardinal(ABytes[4]) shl 24) or
    (Cardinal(ABytes[5]) shl 16) or
    (Cardinal(ABytes[6]) shl 8) or
    Cardinal(ABytes[7]);
  Result := True;
end;

function ValidateDoIPHeader(const AHeader: TOBDDoIPHeader;
  AMaxPayload: Cardinal): Byte;
begin
  if AHeader.ProtocolVersion <> Byte(not AHeader.InverseProtocolVersion) then
    Exit(DOIP_NACK_IncorrectPattern);
  case AHeader.PayloadType of
    DOIP_PT_GenericNACK,
    DOIP_PT_VehicleIDRequest, DOIP_PT_VehicleIDRequestEID,
    DOIP_PT_VehicleIDRequestVIN, DOIP_PT_VehicleAnnouncement,
    DOIP_PT_RoutingActivationRequest, DOIP_PT_RoutingActivationResponse,
    DOIP_PT_AliveCheckRequest, DOIP_PT_AliveCheckResponse,
    DOIP_PT_EntityStatusRequest, DOIP_PT_EntityStatusResponse,
    DOIP_PT_PowerModeInfoRequest, DOIP_PT_PowerModeInfoResponse,
    DOIP_PT_DiagnosticMessage, DOIP_PT_DiagnosticMessagePosAck,
    DOIP_PT_DiagnosticMessageNegAck:
      ; // recognised
  else
    Exit(DOIP_NACK_UnknownPayloadType);
  end;
  if AHeader.PayloadLength > AMaxPayload then
    Exit(DOIP_NACK_MessageTooLarge);
  Result := $FF; // valid
end;

function DoIPPayloadTypeName(AType: Word): string;
begin
  case AType of
    DOIP_PT_GenericNACK:               Result := 'Generic NACK';
    DOIP_PT_VehicleIDRequest:          Result := 'Vehicle ID Request';
    DOIP_PT_VehicleIDRequestEID:       Result := 'Vehicle ID Request (EID)';
    DOIP_PT_VehicleIDRequestVIN:       Result := 'Vehicle ID Request (VIN)';
    DOIP_PT_VehicleAnnouncement:       Result := 'Vehicle Announcement / ID Response';
    DOIP_PT_RoutingActivationRequest:  Result := 'Routing Activation Request';
    DOIP_PT_RoutingActivationResponse: Result := 'Routing Activation Response';
    DOIP_PT_AliveCheckRequest:         Result := 'Alive Check Request';
    DOIP_PT_AliveCheckResponse:        Result := 'Alive Check Response';
    DOIP_PT_EntityStatusRequest:       Result := 'Entity Status Request';
    DOIP_PT_EntityStatusResponse:      Result := 'Entity Status Response';
    DOIP_PT_PowerModeInfoRequest:      Result := 'Diagnostic Power Mode Info Request';
    DOIP_PT_PowerModeInfoResponse:     Result := 'Diagnostic Power Mode Info Response';
    DOIP_PT_DiagnosticMessage:         Result := 'Diagnostic Message';
    DOIP_PT_DiagnosticMessagePosAck:   Result := 'Diagnostic Message Positive ACK';
    DOIP_PT_DiagnosticMessageNegAck:   Result := 'Diagnostic Message Negative ACK';
  else
    Result := Format('PayloadType 0x%4.4X', [AType]);
  end;
end;

end.
