//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.DoIP.Discovery.pas
// CONTENTS       : ISO 13400-2 UDP discovery and AliveCheck
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Protocol.DoIP.Discovery;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  DOIP_UDP_PORT_DISCOVERY = 13400;
  DOIP_PROTOCOL_VERSION_2012 = $02;
  DOIP_PROTOCOL_VERSION_2019 = $03;

  DOIP_PT_HEADER_NACK             = $0000;
  DOIP_PT_VEHICLE_IDENT_REQ       = $0001;
  DOIP_PT_VEHICLE_IDENT_REQ_EID   = $0002;
  DOIP_PT_VEHICLE_IDENT_REQ_VIN   = $0003;
  DOIP_PT_VEHICLE_ANNOUNCE        = $0004;  // also identification response
  DOIP_PT_ALIVE_CHECK_REQUEST     = $0007;
  DOIP_PT_ALIVE_CHECK_RESPONSE    = $0008;

  // ISO 13400-2 §6 NACK codes (the ones the discovery layer can emit)
  DOIP_NACK_INCORRECT_PATTERN     = $00;
  DOIP_NACK_UNKNOWN_PAYLOAD_TYPE  = $01;
  DOIP_NACK_MESSAGE_TOO_LARGE     = $02;
  DOIP_NACK_OUT_OF_MEMORY         = $03;
  DOIP_NACK_INVALID_PAYLOAD       = $04;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDDoIPDiscovery = class(Exception);

  /// <summary>
  ///   One DoIP frame as carried over UDP. Header (8 bytes) +
  ///   payload bytes. Build via the helper functions; parse via
  ///   ParseDoIPHeader / ParseVehicleAnnouncement.
  /// </summary>
  TDoIPFrame = record
    /// <summary>
    ///   Protocol version.
    /// </summary>
    ProtocolVersion: Byte;
    /// <summary>
    ///   Inverse protocol version.
    /// </summary>
    InverseProtocolVersion: Byte;
    /// <summary>
    ///   Payload type.
    /// </summary>
    PayloadType: Word;
    /// <summary>
    ///   Payload.
    /// </summary>
    Payload: TBytes;
  end;

  /// <summary>
  ///   Decoded Vehicle Announcement / Identification Response
  ///   payload (ISO 13400-2 §5.5.1). All multi-byte fields are big-
  ///   endian on the wire; we expose them in host order.
  /// </summary>
  TDoIPVehicleAnnouncement = record
    VIN: string;                   // 17 ASCII characters
    LogicalAddress: Word;          // 2 bytes
    EID: TBytes;                   // 6 bytes (typically MAC of the gateway)
    GID: TBytes;                   // 6 bytes
    FurtherActionRequired: Byte;   // 0x00 = none; 0x10 = central security
    SyncStatus: Byte;              // optional in 2012; mandatory in 2019
    HasSyncStatus: Boolean;        // true when payload included it
  end;

/// <summary>
///   Build a DoIP UDP frame: 4-byte header + 4-byte payload-
///   length + payload. The protocol version byte is followed by its
///   bitwise NOT for header validation.
/// </summary>
function BuildDoIPFrame(PayloadType: Word; const Payload: TBytes;
  ProtocolVersion: Byte = DOIP_PROTOCOL_VERSION_2019): TBytes;

/// <summary>
///   Builder shortcut for a Vehicle Identification Request
///   (no EID / no VIN). The payload is empty.
/// </summary>
function BuildVehicleIdentRequest(
  ProtocolVersion: Byte = DOIP_PROTOCOL_VERSION_2019): TBytes;

/// <summary>
///   Builder shortcut for VIN-targeted discovery.
/// </summary>
function BuildVehicleIdentRequestVIN(const VIN: string;
  ProtocolVersion: Byte = DOIP_PROTOCOL_VERSION_2019): TBytes;

/// <summary>
///   Builder shortcut for EID-targeted discovery (6 bytes).
/// </summary>
function BuildVehicleIdentRequestEID(const EID: TBytes;
  ProtocolVersion: Byte = DOIP_PROTOCOL_VERSION_2019): TBytes;

/// <summary>
///   AliveCheck request (empty payload).
/// </summary>
function BuildAliveCheckRequest(
  ProtocolVersion: Byte = DOIP_PROTOCOL_VERSION_2019): TBytes;

/// <summary>
///   AliveCheck response — payload carries the gateway's
///   2-byte logical source address.
/// </summary>
function BuildAliveCheckResponse(SourceAddress: Word;
  ProtocolVersion: Byte = DOIP_PROTOCOL_VERSION_2019): TBytes;

/// <summary>
///   Parse the 8-byte DoIP header. Verifies the
///   protocol-version / inverse pairing and the declared payload-length.
///   Raises EOBDDoIPDiscovery on malformed input.
/// </summary>
function ParseDoIPHeader(const Bytes: TBytes): TDoIPFrame;

/// <summary>
///   Parse a Vehicle Announcement / Identification Response
///   payload (ISO 13400-2 §5.5.1).
/// </summary>
function ParseVehicleAnnouncement(const Frame: TDoIPFrame):
  TDoIPVehicleAnnouncement;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// BUILD DO IPFRAME
//------------------------------------------------------------------------------
function BuildDoIPFrame(PayloadType: Word; const Payload: TBytes;
  ProtocolVersion: Byte): TBytes;
var
  Out_: TBytes;
  PayloadLen: UInt32;
begin
  PayloadLen := UInt32(Length(Payload));
  // Allocate Out_
  SetLength(Out_, 8 + Length(Payload));
  Out_[0] := ProtocolVersion;
  Out_[1] := Byte(not ProtocolVersion);
  Out_[2] := Byte(PayloadType shr 8);
  Out_[3] := Byte(PayloadType and $FF);
  Out_[4] := Byte(PayloadLen shr 24);
  Out_[5] := Byte(PayloadLen shr 16);
  Out_[6] := Byte(PayloadLen shr 8);
  Out_[7] := Byte(PayloadLen);
  if Length(Payload) > 0 then
    Move(Payload[0], Out_[8], Length(Payload));
  Result := Out_;
end;

//------------------------------------------------------------------------------
// BUILD VEHICLE IDENT REQUEST
//------------------------------------------------------------------------------
function BuildVehicleIdentRequest(ProtocolVersion: Byte): TBytes;
begin
  Result := BuildDoIPFrame(DOIP_PT_VEHICLE_IDENT_REQ, nil, ProtocolVersion);
end;

//------------------------------------------------------------------------------
// BUILD VEHICLE IDENT REQUEST VIN
//------------------------------------------------------------------------------
function BuildVehicleIdentRequestVIN(const VIN: string;
  ProtocolVersion: Byte): TBytes;
var
  Payload: TBytes;
  I: Integer;
begin
  if Length(VIN) <> 17 then
    raise EOBDDoIPDiscovery.CreateFmt(
      'VIN must be exactly 17 characters, got %d', [Length(VIN)]);
  // Allocate Payload
  SetLength(Payload, 17);
  for I := 0 to 16 do
    Payload[I] := Byte(Ord(VIN[I + 1]));
  Result := BuildDoIPFrame(DOIP_PT_VEHICLE_IDENT_REQ_VIN, Payload,
    ProtocolVersion);
end;

//------------------------------------------------------------------------------
// BUILD VEHICLE IDENT REQUEST EID
//------------------------------------------------------------------------------
function BuildVehicleIdentRequestEID(const EID: TBytes;
  ProtocolVersion: Byte): TBytes;
begin
  if Length(EID) <> 6 then
    raise EOBDDoIPDiscovery.CreateFmt(
      'EID must be exactly 6 bytes, got %d', [Length(EID)]);
  Result := BuildDoIPFrame(DOIP_PT_VEHICLE_IDENT_REQ_EID, EID,
    ProtocolVersion);
end;

//------------------------------------------------------------------------------
// BUILD ALIVE CHECK REQUEST
//------------------------------------------------------------------------------
function BuildAliveCheckRequest(ProtocolVersion: Byte): TBytes;
begin
  Result := BuildDoIPFrame(DOIP_PT_ALIVE_CHECK_REQUEST, nil, ProtocolVersion);
end;

//------------------------------------------------------------------------------
// BUILD ALIVE CHECK RESPONSE
//------------------------------------------------------------------------------
function BuildAliveCheckResponse(SourceAddress: Word;
  ProtocolVersion: Byte): TBytes;
var
  Payload: TBytes;
begin
  // Allocate Payload
  SetLength(Payload, 2);
  Payload[0] := Byte(SourceAddress shr 8);
  Payload[1] := Byte(SourceAddress and $FF);
  Result := BuildDoIPFrame(DOIP_PT_ALIVE_CHECK_RESPONSE, Payload,
    ProtocolVersion);
end;

//------------------------------------------------------------------------------
// PARSE DO IPHEADER
//------------------------------------------------------------------------------
function ParseDoIPHeader(const Bytes: TBytes): TDoIPFrame;
var
  PayloadLen: UInt32;
begin
  if Length(Bytes) < 8 then
    raise EOBDDoIPDiscovery.Create(
      'DoIP frame shorter than 8-byte header');
  Result.ProtocolVersion := Bytes[0];
  Result.InverseProtocolVersion := Bytes[1];
  if Bytes[1] <> Byte(not Bytes[0]) then
    raise EOBDDoIPDiscovery.CreateFmt(
      'DoIP header inverse mismatch: 0x%.2x / 0x%.2x',
      [Bytes[0], Bytes[1]]);
  Result.PayloadType := (UInt32(Bytes[2]) shl 8) or Bytes[3];
  PayloadLen := (UInt32(Bytes[4]) shl 24) or (UInt32(Bytes[5]) shl 16)
             or (UInt32(Bytes[6]) shl 8)  or  UInt32(Bytes[7]);
  if 8 + PayloadLen > UInt32(Length(Bytes)) then
    raise EOBDDoIPDiscovery.CreateFmt(
      'DoIP payload truncated: declared %d, actual %d',
      [PayloadLen, Length(Bytes) - 8]);
  // Allocate Result.Payload
  SetLength(Result.Payload, PayloadLen);
  if PayloadLen > 0 then
    Move(Bytes[8], Result.Payload[0], PayloadLen);
end;

//------------------------------------------------------------------------------
// PARSE VEHICLE ANNOUNCEMENT
//------------------------------------------------------------------------------
function ParseVehicleAnnouncement(const Frame: TDoIPFrame):
  TDoIPVehicleAnnouncement;
var
  P: TBytes;
  I: Integer;
begin
  if (Frame.PayloadType <> DOIP_PT_VEHICLE_ANNOUNCE) then
    raise EOBDDoIPDiscovery.Create(
      'Frame is not a Vehicle Announcement / Identification Response');
  P := Frame.Payload;
  if Length(P) < 32 then
    raise EOBDDoIPDiscovery.CreateFmt(
      'Vehicle Announcement payload too short: %d (expected >= 32)',
      [Length(P)]);

  Result := Default(TDoIPVehicleAnnouncement);

  // VIN — 17 ASCII bytes, offset 0.
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do
    Result.VIN[I + 1] := Char(P[I]);

  // Logical address — uint16 BE at offset 17.
  Result.LogicalAddress := (UInt32(P[17]) shl 8) or P[18];

  // EID — 6 bytes at offset 19.
  SetLength(Result.EID, 6);
  Move(P[19], Result.EID[0], 6);

  // GID — 6 bytes at offset 25.
  SetLength(Result.GID, 6);
  Move(P[25], Result.GID[0], 6);

  // Further action required — uint8 at offset 31.
  Result.FurtherActionRequired := P[31];

  // Sync status — uint8 at offset 32, optional in 2012, mandatory in 2019.
  if Length(P) >= 33 then
  begin
    Result.SyncStatus := P[32];
    Result.HasSyncStatus := True;
  end;
end;

end.
