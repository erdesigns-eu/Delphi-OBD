//------------------------------------------------------------------------------
//  OBD.Protocol.DoIP.Messages
//
//  Per-payload-type encoders and decoders. Each DoIP payload type
//  defined in ISO 13400-2 Table 17 has a record + Encode + Decode.
//  The header itself is in OBD.Protocol.DoIP.Header.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 13400-2:2019 §7 (Payload formats)
//    - ISO 13400-2:2019 Tables 19..40 (per-payload field layouts)
//
//  History     :
//    2026-05-09  ERD  Phase 4d initial. All 15 standard payload
//                     types covered.
//------------------------------------------------------------------------------

unit OBD.Protocol.DoIP.Messages;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.DoIP.Header;

const
  // ---- Routing-activation request types (Table 21) ----
  DOIP_RA_TYPE_DEFAULT          = $00;
  DOIP_RA_TYPE_WWHOBD           = $01;
  DOIP_RA_TYPE_CENTRAL_SECURITY = $E0;

  // ---- Routing-activation response codes (Table 23) ----
  DOIP_RA_RESP_DeniedUnknownSA              = $00;
  DOIP_RA_RESP_DeniedAllSocketsRegistered   = $01;
  DOIP_RA_RESP_DeniedSAMismatch             = $02;
  DOIP_RA_RESP_DeniedSAAlreadyActive        = $03;
  DOIP_RA_RESP_DeniedAuthMissing            = $04;
  DOIP_RA_RESP_DeniedConfirmationRejected   = $05;
  DOIP_RA_RESP_DeniedUnsupportedType        = $06;
  DOIP_RA_RESP_DeniedTLSRequired            = $07;
  DOIP_RA_RESP_Activated                    = $10;
  DOIP_RA_RESP_PendingConfirmation          = $11;

  // ---- Diagnostic-message NACK codes (Table 39) ----
  DOIP_DM_NACK_InvalidSourceAddress  = $02;
  DOIP_DM_NACK_UnknownTargetAddress  = $03;
  DOIP_DM_NACK_DiagnosticTooLarge    = $04;
  DOIP_DM_NACK_OutOfMemory           = $05;
  DOIP_DM_NACK_TargetUnreachable     = $06;
  DOIP_DM_NACK_UnknownNetwork        = $07;
  DOIP_DM_NACK_TransportProtocolErr  = $08;

  // ---- Power-mode codes (Table 36) ----
  DOIP_POWER_NotReady     = $00;
  DOIP_POWER_Ready        = $01;
  DOIP_POWER_NotSupported = $02;

  // ---- Node types (Table 33) ----
  DOIP_NODE_TYPE_GATEWAY = $00;
  DOIP_NODE_TYPE_NODE    = $01;

  // ---- "Further action required" (Table 19) ----
  DOIP_FAR_NoFurtherAction       = $00;
  DOIP_FAR_RoutingActivationReqd = $10;

type
  /// <summary>Routing-activation request (payload 0x0005).</summary>
  TOBDDoIPRoutingActivationRequest = record
    /// <summary>Tester logical address (e.g. 0x0E80..0x0EFF).</summary>
    SourceAddress: Word;
    /// <summary>Activation type (0x00 default, 0x01 WWH-OBD, 0xE0
    /// central security).</summary>
    ActivationType: Byte;
    /// <summary>Reserved-by-ISO bytes (4 bytes; standard is all
    /// zeros).</summary>
    ReservedISO: Cardinal;
    /// <summary>Optional OEM-specific 4 bytes. Set
    /// <c>HasOEMData = False</c> to omit.</summary>
    OEMData: Cardinal;
    /// <summary>True when <c>OEMData</c> is meaningful.</summary>
    HasOEMData: Boolean;
  end;

  /// <summary>Routing-activation response (payload 0x0006).</summary>
  TOBDDoIPRoutingActivationResponse = record
    /// <summary>Tester logical address.</summary>
    TesterAddress: Word;
    /// <summary>DoIP entity logical address.</summary>
    EntityAddress: Word;
    /// <summary>Routing activation response code (one of
    /// <c>DOIP_RA_RESP_*</c>).</summary>
    ResponseCode: Byte;
    /// <summary>Reserved-by-ISO bytes (4 bytes).</summary>
    ReservedISO: Cardinal;
    /// <summary>Optional OEM-specific 4 bytes.</summary>
    OEMData: Cardinal;
    /// <summary>True when <c>OEMData</c> is meaningful.</summary>
    HasOEMData: Boolean;
  end;

  /// <summary>Vehicle announcement / ID response (payload 0x0004).</summary>
  TOBDDoIPVehicleAnnouncement = record
    /// <summary>17-byte ASCII VIN.</summary>
    VIN: string;
    /// <summary>Logical address of the responding entity.</summary>
    LogicalAddress: Word;
    /// <summary>Entity ID (6 bytes; typically MAC of the gateway).</summary>
    EID: array[0..5] of Byte;
    /// <summary>Group ID (6 bytes).</summary>
    GID: array[0..5] of Byte;
    /// <summary>Further-action-required code (Table 19).</summary>
    FurtherAction: Byte;
    /// <summary>GID sync status (protocol v0x03+; 0 = sync,
    /// 0x10 = sync incomplete).</summary>
    GIDSyncStatus: Byte;
    /// <summary>True when <c>GIDSyncStatus</c> is present (protocol
    /// version &gt;= 3).</summary>
    HasGIDSyncStatus: Boolean;
  end;

  /// <summary>Vehicle ID request (EID-specific) payload 0x0002.</summary>
  TOBDDoIPVehicleIDRequestEID = record
    /// <summary>Entity ID to look up (6 bytes).</summary>
    EID: array[0..5] of Byte;
  end;

  /// <summary>Vehicle ID request (VIN-specific) payload 0x0003.</summary>
  TOBDDoIPVehicleIDRequestVIN = record
    /// <summary>17-character VIN.</summary>
    VIN: string;
  end;

  /// <summary>Diagnostic message envelope (payload 0x8001).</summary>
  TOBDDoIPDiagnosticMessage = record
    /// <summary>Tester source address.</summary>
    SourceAddress: Word;
    /// <summary>ECU target address.</summary>
    TargetAddress: Word;
    /// <summary>UDS / OBD-II application bytes.</summary>
    UserData: TBytes;
  end;

  /// <summary>Diagnostic message ACK (payload 0x8002 / 0x8003).</summary>
  /// <remarks>Same shape covers both positive and negative; the
  /// payload-type tells them apart.</remarks>
  TOBDDoIPDiagnosticAck = record
    /// <summary>Tester source address (echoed).</summary>
    SourceAddress: Word;
    /// <summary>ECU target address (echoed).</summary>
    TargetAddress: Word;
    /// <summary>ACK / NACK code byte.</summary>
    AckCode: Byte;
    /// <summary>Optional echo of the previous diagnostic message.
    /// May be empty.</summary>
    PreviousData: TBytes;
  end;

  /// <summary>Alive-check response (payload 0x0008).</summary>
  TOBDDoIPAliveCheckResponse = record
    /// <summary>Source address that owns the TCP_DATA socket.</summary>
    SourceAddress: Word;
  end;

  /// <summary>Entity status response (payload 0x4002).</summary>
  TOBDDoIPEntityStatusResponse = record
    /// <summary>Node type (0x00 gateway, 0x01 node).</summary>
    NodeType: Byte;
    /// <summary>Maximum number of concurrent TCP_DATA sockets.</summary>
    MaxConcurrentSockets: Byte;
    /// <summary>Currently open TCP_DATA sockets.</summary>
    CurrentlyOpenSockets: Byte;
    /// <summary>Maximum data size per diagnostic message (4 bytes).
    /// Zero when not advertised.</summary>
    MaxDataSize: Cardinal;
    /// <summary>True when the entity advertises a max-data-size
    /// field.</summary>
    HasMaxDataSize: Boolean;
  end;

  /// <summary>Diagnostic power mode response (payload 0x4004).</summary>
  TOBDDoIPPowerModeResponse = record
    /// <summary>Power-mode code (0x00 not ready, 0x01 ready,
    /// 0x02 not supported).</summary>
    PowerMode: Byte;
  end;

  /// <summary>Stateless DoIP message encoder / decoder.</summary>
  TOBDDoIPCodec = class
  public
    // ---- Routing activation ----
    /// <summary>Encodes routing-activation request (payload type
    /// 0x0005).</summary>
    /// <param name="ARequest">Request record.</param>
    /// <returns>Wire bytes (header + payload).</returns>
    class function EncodeRoutingActivationRequest(
      const ARequest: TOBDDoIPRoutingActivationRequest): TBytes; static;
    /// <summary>Decodes a routing-activation response payload
    /// (without the 8-byte header).</summary>
    class function DecodeRoutingActivationResponse(
      const APayload: TBytes;
      out AResponse: TOBDDoIPRoutingActivationResponse): Boolean; static;

    // ---- Vehicle ID ----
    /// <summary>Encodes a generic vehicle ID request
    /// (payload 0x0001, no body).</summary>
    class function EncodeVehicleIDRequest: TBytes; static;
    /// <summary>Encodes a vehicle ID request by EID
    /// (payload 0x0002).</summary>
    class function EncodeVehicleIDRequestEID(
      const ARequest: TOBDDoIPVehicleIDRequestEID): TBytes; static;
    /// <summary>Encodes a vehicle ID request by VIN
    /// (payload 0x0003).</summary>
    class function EncodeVehicleIDRequestVIN(
      const ARequest: TOBDDoIPVehicleIDRequestVIN): TBytes; static;
    /// <summary>Decodes a vehicle announcement (payload 0x0004).</summary>
    class function DecodeVehicleAnnouncement(const APayload: TBytes;
      out AResponse: TOBDDoIPVehicleAnnouncement): Boolean; static;

    // ---- Alive check ----
    /// <summary>Encodes an alive-check request (payload 0x0007, no
    /// body).</summary>
    class function EncodeAliveCheckRequest: TBytes; static;
    /// <summary>Decodes an alive-check response (payload 0x0008).</summary>
    class function DecodeAliveCheckResponse(const APayload: TBytes;
      out AResponse: TOBDDoIPAliveCheckResponse): Boolean; static;

    // ---- Entity status ----
    /// <summary>Encodes an entity-status request (payload 0x4001,
    /// no body).</summary>
    class function EncodeEntityStatusRequest: TBytes; static;
    /// <summary>Decodes an entity-status response
    /// (payload 0x4002).</summary>
    class function DecodeEntityStatusResponse(const APayload: TBytes;
      out AResponse: TOBDDoIPEntityStatusResponse): Boolean; static;

    // ---- Power mode ----
    /// <summary>Encodes a power-mode-info request (payload 0x4003,
    /// no body).</summary>
    class function EncodePowerModeRequest: TBytes; static;
    /// <summary>Decodes a power-mode-info response
    /// (payload 0x4004).</summary>
    class function DecodePowerModeResponse(const APayload: TBytes;
      out AResponse: TOBDDoIPPowerModeResponse): Boolean; static;

    // ---- Diagnostic message ----
    /// <summary>Encodes a diagnostic message (payload 0x8001).</summary>
    class function EncodeDiagnosticMessage(
      const AMessage: TOBDDoIPDiagnosticMessage): TBytes; static;
    /// <summary>Decodes a diagnostic message (payload 0x8001).</summary>
    class function DecodeDiagnosticMessage(const APayload: TBytes;
      out AMessage: TOBDDoIPDiagnosticMessage): Boolean; static;
    /// <summary>Decodes a diagnostic-message ACK
    /// (payload 0x8002 or 0x8003).</summary>
    class function DecodeDiagnosticAck(const APayload: TBytes;
      out AAck: TOBDDoIPDiagnosticAck): Boolean; static;

    // ---- Generic NACK ----
    /// <summary>Encodes a Generic NACK (payload 0x0000) with the
    /// supplied 1-byte reason.</summary>
    class function EncodeGenericNACK(AReason: Byte): TBytes; static;

    // ---- Helpers ----
    /// <summary>Builds a header + payload byte buffer.</summary>
    /// <param name="APayloadType">Payload-type code.</param>
    /// <param name="APayload">Payload bytes (may be empty).</param>
    /// <returns>Wire bytes ready to send.</returns>
    class function PackMessage(APayloadType: Word;
      const APayload: TBytes): TBytes; static;

    /// <summary>Splits a buffer into header + payload. Returns False
    /// when the buffer is incomplete or the header fails validation.</summary>
    /// <param name="ABuffer">Bytes received off the wire.</param>
    /// <param name="AHeader">Output header.</param>
    /// <param name="APayload">Output payload (length per the
    /// header).</param>
    /// <returns>True when a complete message is present.</returns>
    class function UnpackMessage(const ABuffer: TBytes;
      out AHeader: TOBDDoIPHeader;
      out APayload: TBytes): Boolean; static;
  end;

implementation

{ ---- byte helpers ------------------------------------------------------------ }

procedure WriteWordBE(var ABytes: TBytes; AOffset: Integer; AValue: Word);
begin
  ABytes[AOffset] := Byte((AValue shr 8) and $FF);
  ABytes[AOffset + 1] := Byte(AValue and $FF);
end;

procedure WriteCardinalBE(var ABytes: TBytes; AOffset: Integer;
  AValue: Cardinal);
begin
  ABytes[AOffset]     := Byte((AValue shr 24) and $FF);
  ABytes[AOffset + 1] := Byte((AValue shr 16) and $FF);
  ABytes[AOffset + 2] := Byte((AValue shr 8) and $FF);
  ABytes[AOffset + 3] := Byte(AValue and $FF);
end;

function ReadWordBE(const ABytes: TBytes; AOffset: Integer): Word;
begin
  Result := (Word(ABytes[AOffset]) shl 8) or Word(ABytes[AOffset + 1]);
end;

function ReadCardinalBE(const ABytes: TBytes; AOffset: Integer): Cardinal;
begin
  Result := (Cardinal(ABytes[AOffset]) shl 24) or
            (Cardinal(ABytes[AOffset + 1]) shl 16) or
            (Cardinal(ABytes[AOffset + 2]) shl 8) or
            Cardinal(ABytes[AOffset + 3]);
end;

procedure CopyToASCIIBytes(const AText: string; var ABytes: TBytes;
  AOffset, ALength: Integer);
var
  Encoded: TBytes;
  CopyLen: Integer;
begin
  Encoded := TEncoding.ASCII.GetBytes(AText);
  CopyLen := Length(Encoded);
  if CopyLen > ALength then
    CopyLen := ALength;
  if CopyLen > 0 then
    Move(Encoded[0], ABytes[AOffset], CopyLen);
  // Pad with 0x00 if shorter.
  if CopyLen < ALength then
    FillChar(ABytes[AOffset + CopyLen], ALength - CopyLen, 0);
end;

{ ---- TOBDDoIPCodec ----------------------------------------------------------- }

class function TOBDDoIPCodec.PackMessage(APayloadType: Word;
  const APayload: TBytes): TBytes;
var
  Header: TOBDDoIPHeader;
  HeaderBytes: TBytes;
  TotalLen: Integer;
begin
  Header := MakeDoIPHeader(APayloadType, Cardinal(Length(APayload)));
  HeaderBytes := EncodeDoIPHeader(Header);
  TotalLen := Length(HeaderBytes) + Length(APayload);
  SetLength(Result, TotalLen);
  Move(HeaderBytes[0], Result[0], Length(HeaderBytes));
  if Length(APayload) > 0 then
    Move(APayload[0], Result[Length(HeaderBytes)], Length(APayload));
end;

class function TOBDDoIPCodec.UnpackMessage(const ABuffer: TBytes;
  out AHeader: TOBDDoIPHeader; out APayload: TBytes): Boolean;
begin
  Result := False;
  if not DecodeDoIPHeader(ABuffer, AHeader) then Exit;
  if Cardinal(Length(ABuffer) - DOIP_HEADER_LENGTH) < AHeader.PayloadLength then
    Exit; // incomplete
  SetLength(APayload, AHeader.PayloadLength);
  if AHeader.PayloadLength > 0 then
    Move(ABuffer[DOIP_HEADER_LENGTH], APayload[0], AHeader.PayloadLength);
  Result := True;
end;

class function TOBDDoIPCodec.EncodeRoutingActivationRequest(
  const ARequest: TOBDDoIPRoutingActivationRequest): TBytes;
var
  Body: TBytes;
  BodyLen: Integer;
begin
  if ARequest.HasOEMData then BodyLen := 11 else BodyLen := 7;
  SetLength(Body, BodyLen);
  WriteWordBE(Body, 0, ARequest.SourceAddress);
  Body[2] := ARequest.ActivationType;
  WriteCardinalBE(Body, 3, ARequest.ReservedISO);
  if ARequest.HasOEMData then
    WriteCardinalBE(Body, 7, ARequest.OEMData);
  Result := PackMessage(DOIP_PT_RoutingActivationRequest, Body);
end;

class function TOBDDoIPCodec.DecodeRoutingActivationResponse(
  const APayload: TBytes;
  out AResponse: TOBDDoIPRoutingActivationResponse): Boolean;
begin
  Result := False;
  AResponse := Default(TOBDDoIPRoutingActivationResponse);
  if Length(APayload) < 9 then Exit;
  AResponse.TesterAddress := ReadWordBE(APayload, 0);
  AResponse.EntityAddress := ReadWordBE(APayload, 2);
  AResponse.ResponseCode := APayload[4];
  AResponse.ReservedISO := ReadCardinalBE(APayload, 5);
  if Length(APayload) >= 13 then
  begin
    AResponse.OEMData := ReadCardinalBE(APayload, 9);
    AResponse.HasOEMData := True;
  end;
  Result := True;
end;

class function TOBDDoIPCodec.EncodeVehicleIDRequest: TBytes;
begin
  Result := PackMessage(DOIP_PT_VehicleIDRequest, nil);
end;

class function TOBDDoIPCodec.EncodeVehicleIDRequestEID(
  const ARequest: TOBDDoIPVehicleIDRequestEID): TBytes;
var
  Body: TBytes;
  I: Integer;
begin
  SetLength(Body, 6);
  for I := 0 to 5 do
    Body[I] := ARequest.EID[I];
  Result := PackMessage(DOIP_PT_VehicleIDRequestEID, Body);
end;

class function TOBDDoIPCodec.EncodeVehicleIDRequestVIN(
  const ARequest: TOBDDoIPVehicleIDRequestVIN): TBytes;
var
  Body: TBytes;
begin
  SetLength(Body, 17);
  CopyToASCIIBytes(ARequest.VIN, Body, 0, 17);
  Result := PackMessage(DOIP_PT_VehicleIDRequestVIN, Body);
end;

class function TOBDDoIPCodec.DecodeVehicleAnnouncement(
  const APayload: TBytes;
  out AResponse: TOBDDoIPVehicleAnnouncement): Boolean;
var
  I: Integer;
  VinBytes: TBytes;
begin
  Result := False;
  AResponse := Default(TOBDDoIPVehicleAnnouncement);
  if Length(APayload) < 32 then Exit;
  SetLength(VinBytes, 17);
  Move(APayload[0], VinBytes[0], 17);
  AResponse.VIN := TEncoding.ASCII.GetString(VinBytes);
  AResponse.LogicalAddress := ReadWordBE(APayload, 17);
  for I := 0 to 5 do
    AResponse.EID[I] := APayload[19 + I];
  for I := 0 to 5 do
    AResponse.GID[I] := APayload[25 + I];
  AResponse.FurtherAction := APayload[31];
  if Length(APayload) >= 33 then
  begin
    AResponse.GIDSyncStatus := APayload[32];
    AResponse.HasGIDSyncStatus := True;
  end;
  Result := True;
end;

class function TOBDDoIPCodec.EncodeAliveCheckRequest: TBytes;
begin
  Result := PackMessage(DOIP_PT_AliveCheckRequest, nil);
end;

class function TOBDDoIPCodec.DecodeAliveCheckResponse(
  const APayload: TBytes;
  out AResponse: TOBDDoIPAliveCheckResponse): Boolean;
begin
  Result := False;
  AResponse := Default(TOBDDoIPAliveCheckResponse);
  if Length(APayload) < 2 then Exit;
  AResponse.SourceAddress := ReadWordBE(APayload, 0);
  Result := True;
end;

class function TOBDDoIPCodec.EncodeEntityStatusRequest: TBytes;
begin
  Result := PackMessage(DOIP_PT_EntityStatusRequest, nil);
end;

class function TOBDDoIPCodec.DecodeEntityStatusResponse(
  const APayload: TBytes;
  out AResponse: TOBDDoIPEntityStatusResponse): Boolean;
begin
  Result := False;
  AResponse := Default(TOBDDoIPEntityStatusResponse);
  if Length(APayload) < 3 then Exit;
  AResponse.NodeType := APayload[0];
  AResponse.MaxConcurrentSockets := APayload[1];
  AResponse.CurrentlyOpenSockets := APayload[2];
  if Length(APayload) >= 7 then
  begin
    AResponse.MaxDataSize := ReadCardinalBE(APayload, 3);
    AResponse.HasMaxDataSize := True;
  end;
  Result := True;
end;

class function TOBDDoIPCodec.EncodePowerModeRequest: TBytes;
begin
  Result := PackMessage(DOIP_PT_PowerModeInfoRequest, nil);
end;

class function TOBDDoIPCodec.DecodePowerModeResponse(
  const APayload: TBytes;
  out AResponse: TOBDDoIPPowerModeResponse): Boolean;
begin
  Result := False;
  AResponse := Default(TOBDDoIPPowerModeResponse);
  if Length(APayload) < 1 then Exit;
  AResponse.PowerMode := APayload[0];
  Result := True;
end;

class function TOBDDoIPCodec.EncodeDiagnosticMessage(
  const AMessage: TOBDDoIPDiagnosticMessage): TBytes;
var
  Body: TBytes;
  UserLen: Integer;
begin
  UserLen := Length(AMessage.UserData);
  SetLength(Body, 4 + UserLen);
  WriteWordBE(Body, 0, AMessage.SourceAddress);
  WriteWordBE(Body, 2, AMessage.TargetAddress);
  if UserLen > 0 then
    Move(AMessage.UserData[0], Body[4], UserLen);
  Result := PackMessage(DOIP_PT_DiagnosticMessage, Body);
end;

class function TOBDDoIPCodec.DecodeDiagnosticMessage(
  const APayload: TBytes;
  out AMessage: TOBDDoIPDiagnosticMessage): Boolean;
begin
  Result := False;
  AMessage := Default(TOBDDoIPDiagnosticMessage);
  if Length(APayload) < 4 then Exit;
  AMessage.SourceAddress := ReadWordBE(APayload, 0);
  AMessage.TargetAddress := ReadWordBE(APayload, 2);
  if Length(APayload) > 4 then
  begin
    SetLength(AMessage.UserData, Length(APayload) - 4);
    Move(APayload[4], AMessage.UserData[0], Length(APayload) - 4);
  end;
  Result := True;
end;

class function TOBDDoIPCodec.DecodeDiagnosticAck(const APayload: TBytes;
  out AAck: TOBDDoIPDiagnosticAck): Boolean;
begin
  Result := False;
  AAck := Default(TOBDDoIPDiagnosticAck);
  if Length(APayload) < 5 then Exit;
  AAck.SourceAddress := ReadWordBE(APayload, 0);
  AAck.TargetAddress := ReadWordBE(APayload, 2);
  AAck.AckCode := APayload[4];
  if Length(APayload) > 5 then
  begin
    SetLength(AAck.PreviousData, Length(APayload) - 5);
    Move(APayload[5], AAck.PreviousData[0], Length(APayload) - 5);
  end;
  Result := True;
end;

class function TOBDDoIPCodec.EncodeGenericNACK(AReason: Byte): TBytes;
var
  Body: TBytes;
begin
  SetLength(Body, 1);
  Body[0] := AReason;
  Result := PackMessage(DOIP_PT_GenericNACK, Body);
end;

end.
