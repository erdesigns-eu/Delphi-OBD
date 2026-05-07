//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.DoIP.pas
// CONTENTS       : ISO 13400-2 (DoIP) frame builders and parsers —
//                  vehicle announcements, routing activation, alive
//                  checks, diagnostic message wrapping.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : DoIP is the Ethernet transport for UDS. Modern
//                  cars (post-2018) carry the diagnostic session
//                  over TCP/UDP on port 13400 instead of CAN. Each
//                  message starts with an 8-byte header:
//                    Version (1) | InvVersion (1) | PayloadType (2)
//                    PayloadLength (4)
//                  Followed by the type-specific payload. This unit
//                  ships the encoders / decoders for the message
//                  types a UDS client actually needs; pair with
//                  the existing TCP/UDP transports under
//                  `OBD.Connection.UDP` and `OBD.Connection.Wifi`.
//------------------------------------------------------------------------------
unit OBD.OEM.DoIP;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

type
  EOBDDoIPError = class(Exception);

  /// <summary>ISO 13400-2 protocol versions used in the wild.</summary>
  TOBDDoIPVersion = (
    dpv2010 = $01,    // 2010 edition
    dpv2012 = $02,    // 2012 edition (most current vehicles)
    dpvDefault = $02
  );

  /// <summary>Common DoIP payload type codes (ISO 13400-2 §5.4).</summary>
  TOBDDoIPPayloadType = (
    dptGenericNack          = $0000,
    dptVehicleIdentRequest  = $0001,
    dptVehicleIdentRequestEid = $0002,
    dptVehicleIdentRequestVin = $0003,
    dptVehicleAnnouncement  = $0004,
    dptRoutingActivationRequest = $0005,
    dptRoutingActivationResponse = $0006,
    dptAliveCheckRequest    = $0007,
    dptAliveCheckResponse   = $0008,
    dptEntityStatusRequest  = $4001,
    dptEntityStatusResponse = $4002,
    dptDiagPowerModeRequest = $4003,
    dptDiagPowerModeResponse = $4004,
    dptDiagnosticMessage    = $8001,
    dptDiagnosticMessageAck = $8002,
    dptDiagnosticMessageNack = $8003
  );

  /// <summary>RoutingActivation request types (ISO 13400-2 §7.4.4.2).</summary>
  TOBDDoIPActivationType = (
    datDefault           = $00,
    datWWHOBD            = $01,
    datCentralSecurity   = $02,
    datOEMSpecific       = $E0
  );

  /// <summary>RoutingActivation response codes (ISO 13400-2 §7.4.5.4).</summary>
  TOBDDoIPRoutingResponse = (
    drrUnknownSourceAddress  = $00,
    drrAllSocketsRegistered  = $01,
    drrSourceAddressMismatch = $02,
    drrSocketAlreadyAssigned = $03,
    drrMissingAuthentication = $04,
    drrRejectedConfirmation  = $05,
    drrUnsupportedActivation = $06,
    drrSuccess               = $10,
    drrSuccessVehicleConfirm = $11
  );

  TOBDDoIPHeader = record
    Version: Byte;
    InvVersion: Byte;
    PayloadType: Word;
    PayloadLength: Cardinal;
  end;

  /// <summary>Decoded RoutingActivationResponse.</summary>
  TOBDDoIPRoutingActivation = record
    TesterLogicalAddress: Word;
    EntityLogicalAddress: Word;
    ResponseCode: TOBDDoIPRoutingResponse;
    Reserved: Cardinal;
    OEMSpecific: Cardinal;
  end;

  /// <summary>Decoded VehicleAnnouncement / VehicleIdentResponse.</summary>
  TOBDDoIPVehicleAnnouncement = record
    VIN: string;                  // 17 ASCII
    LogicalAddress: Word;
    EID: TBytes;                  // 6 bytes (MAC-like)
    GID: TBytes;                  // 6 bytes (group ID)
    FurtherActionRequired: Byte;
    SyncStatus: Byte;             // optional in v2012
  end;

  /// <summary>Decoded DiagnosticMessage payload (just the wrapper —
  /// the inner UDS bytes go to the runner / DiagSession).</summary>
  TOBDDoIPDiagnosticMessage = record
    SourceAddress: Word;
    TargetAddress: Word;
    UserData: TBytes;
  end;

//==============================================================================
//  Header helpers
//==============================================================================
function BuildDoIPHeader(const Version: TOBDDoIPVersion;
  const PayloadType: TOBDDoIPPayloadType;
  const PayloadLength: Cardinal): TBytes;

/// <summary>Parse the 8-byte header. Throws on the protocol-version
/// inversion check (Version XOR InvVersion must equal 0xFF).</summary>
function ParseDoIPHeader(const Bytes: TBytes; out Header: TOBDDoIPHeader): Integer;

//==============================================================================
//  Routing activation
//==============================================================================
function BuildRoutingActivationRequest(
  const TesterAddress: Word;
  const ActivationType: TOBDDoIPActivationType = datDefault;
  const OEMSpecific: Cardinal = 0;
  const Version: TOBDDoIPVersion = dpvDefault): TBytes;

function ParseRoutingActivationResponse(
  const Bytes: TBytes;
  out Activation: TOBDDoIPRoutingActivation): Boolean;

//==============================================================================
//  Vehicle announcement / VIN ident
//==============================================================================
function BuildVehicleIdentRequest(
  const Version: TOBDDoIPVersion = dpvDefault): TBytes;

function BuildVehicleIdentRequestByVIN(const VIN: string;
  const Version: TOBDDoIPVersion = dpvDefault): TBytes;

function ParseVehicleAnnouncement(
  const Bytes: TBytes;
  out Announcement: TOBDDoIPVehicleAnnouncement): Boolean;

//==============================================================================
//  Alive check
//==============================================================================
function BuildAliveCheckRequest(
  const Version: TOBDDoIPVersion = dpvDefault): TBytes;

function BuildAliveCheckResponse(const TesterAddress: Word;
  const Version: TOBDDoIPVersion = dpvDefault): TBytes;

//==============================================================================
//  Diagnostic message wrapping
//==============================================================================
function BuildDiagnosticMessage(const SourceAddress, TargetAddress: Word;
  const UserData: TBytes;
  const Version: TOBDDoIPVersion = dpvDefault): TBytes;

function ParseDiagnosticMessage(const Bytes: TBytes;
  out Msg: TOBDDoIPDiagnosticMessage): Boolean;

implementation

const
  DOIP_HEADER_SIZE = 8;

//==============================================================================
//  Endianness + header helpers (ISO 13400 is big-endian)
//==============================================================================
procedure WriteUInt16BE(var Buf: TBytes; const Offset: Integer; const Value: Word);
begin
  Buf[Offset]     := Byte(Value shr 8);
  Buf[Offset + 1] := Byte(Value and $FF);
end;

procedure WriteUInt32BE(var Buf: TBytes; const Offset: Integer; const Value: Cardinal);
begin
  Buf[Offset]     := Byte(Value shr 24);
  Buf[Offset + 1] := Byte((Value shr 16) and $FF);
  Buf[Offset + 2] := Byte((Value shr 8) and $FF);
  Buf[Offset + 3] := Byte(Value and $FF);
end;

function ReadUInt16BE(const Buf: TBytes; const Offset: Integer): Word;
begin
  Result := (Word(Buf[Offset]) shl 8) or Buf[Offset + 1];
end;

function ReadUInt32BE(const Buf: TBytes; const Offset: Integer): Cardinal;
begin
  Result := (Cardinal(Buf[Offset]) shl 24) or
            (Cardinal(Buf[Offset + 1]) shl 16) or
            (Cardinal(Buf[Offset + 2]) shl 8) or
             Cardinal(Buf[Offset + 3]);
end;

function BuildDoIPHeader(const Version: TOBDDoIPVersion;
  const PayloadType: TOBDDoIPPayloadType;
  const PayloadLength: Cardinal): TBytes;
begin
  SetLength(Result, DOIP_HEADER_SIZE);
  Result[0] := Byte(Version);
  Result[1] := Byte(not Byte(Version));
  WriteUInt16BE(Result, 2, Word(PayloadType));
  WriteUInt32BE(Result, 4, PayloadLength);
end;

function ParseDoIPHeader(const Bytes: TBytes;
  out Header: TOBDDoIPHeader): Integer;
begin
  if Length(Bytes) < DOIP_HEADER_SIZE then
    raise EOBDDoIPError.CreateFmt(
      'DoIP message too short (%d bytes, need %d for header)',
      [Length(Bytes), DOIP_HEADER_SIZE]);
  Header.Version := Bytes[0];
  Header.InvVersion := Bytes[1];
  if (Header.Version xor Header.InvVersion) <> $FF then
    raise EOBDDoIPError.CreateFmt(
      'DoIP version inversion check failed: 0x%.2X / 0x%.2X',
      [Header.Version, Header.InvVersion]);
  Header.PayloadType := ReadUInt16BE(Bytes, 2);
  Header.PayloadLength := ReadUInt32BE(Bytes, 4);
  Result := DOIP_HEADER_SIZE;
end;

function BytesAppend(const A, B: TBytes): TBytes;
begin
  SetLength(Result, Length(A) + Length(B));
  if Length(A) > 0 then Move(A[0], Result[0], Length(A));
  if Length(B) > 0 then Move(B[0], Result[Length(A)], Length(B));
end;

//==============================================================================
//  Routing activation
//==============================================================================
function BuildRoutingActivationRequest(
  const TesterAddress: Word;
  const ActivationType: TOBDDoIPActivationType;
  const OEMSpecific: Cardinal;
  const Version: TOBDDoIPVersion): TBytes;
const
  PAYLOAD_LEN = 11;     // 2 source + 1 type + 4 reserved + 4 OEM
var
  Header, Payload: TBytes;
begin
  SetLength(Payload, PAYLOAD_LEN);
  WriteUInt16BE(Payload, 0, TesterAddress);
  Payload[2] := Byte(ActivationType);
  WriteUInt32BE(Payload, 3, 0);            // reserved
  WriteUInt32BE(Payload, 7, OEMSpecific);
  Header := BuildDoIPHeader(Version, dptRoutingActivationRequest, PAYLOAD_LEN);
  Result := BytesAppend(Header, Payload);
end;

function ParseRoutingActivationResponse(const Bytes: TBytes;
  out Activation: TOBDDoIPRoutingActivation): Boolean;
var
  Header: TOBDDoIPHeader;
  Cursor: Integer;
begin
  Activation := Default(TOBDDoIPRoutingActivation);
  Cursor := ParseDoIPHeader(Bytes, Header);
  if Header.PayloadType <> Word(dptRoutingActivationResponse) then
    Exit(False);
  // Minimum length is 9 (2+2+1+4); v2012 adds a 4-byte OEM-specific
  // tail for 13.
  if (Header.PayloadLength < 9) or
     (Length(Bytes) < Cursor + Integer(Header.PayloadLength)) then
    raise EOBDDoIPError.CreateFmt(
      'Routing activation response truncated (payload %d bytes)',
      [Header.PayloadLength]);
  Activation.TesterLogicalAddress := ReadUInt16BE(Bytes, Cursor);
  Activation.EntityLogicalAddress := ReadUInt16BE(Bytes, Cursor + 2);
  Activation.ResponseCode := TOBDDoIPRoutingResponse(Bytes[Cursor + 4]);
  Activation.Reserved := ReadUInt32BE(Bytes, Cursor + 5);
  if Header.PayloadLength >= 13 then
    Activation.OEMSpecific := ReadUInt32BE(Bytes, Cursor + 9);
  Result := True;
end;

//==============================================================================
//  Vehicle announcement / ident
//==============================================================================
function BuildVehicleIdentRequest(const Version: TOBDDoIPVersion): TBytes;
begin
  // No payload — broadcast on UDP/13400.
  Result := BuildDoIPHeader(Version, dptVehicleIdentRequest, 0);
end;

function BuildVehicleIdentRequestByVIN(const VIN: string;
  const Version: TOBDDoIPVersion): TBytes;
var
  Header, Payload: TBytes;
  VinBytes: TBytes;
  I: Integer;
begin
  if Length(VIN) <> 17 then
    raise EOBDDoIPError.CreateFmt(
      'VehicleIdentRequestByVIN expects a 17-character VIN, got %d',
      [Length(VIN)]);
  VinBytes := TEncoding.ASCII.GetBytes(VIN);
  SetLength(Payload, 17);
  for I := 0 to 16 do Payload[I] := VinBytes[I];
  Header := BuildDoIPHeader(Version, dptVehicleIdentRequestVin, 17);
  Result := BytesAppend(Header, Payload);
end;

function ParseVehicleAnnouncement(const Bytes: TBytes;
  out Announcement: TOBDDoIPVehicleAnnouncement): Boolean;
var
  Header: TOBDDoIPHeader;
  Cursor: Integer;
  VinSlice: TBytes;
begin
  Announcement := Default(TOBDDoIPVehicleAnnouncement);
  Cursor := ParseDoIPHeader(Bytes, Header);
  if Header.PayloadType <> Word(dptVehicleAnnouncement) then Exit(False);
  // Minimum length: 17 (VIN) + 2 (logical address) + 6 (EID) + 6 (GID)
  // + 1 (FurtherActionRequired) = 32.
  if Header.PayloadLength < 32 then
    raise EOBDDoIPError.CreateFmt(
      'VehicleAnnouncement payload too short (%d)', [Header.PayloadLength]);
  if Length(Bytes) < Cursor + Integer(Header.PayloadLength) then
    raise EOBDDoIPError.Create('VehicleAnnouncement truncated');

  SetLength(VinSlice, 17);
  Move(Bytes[Cursor], VinSlice[0], 17);
  Announcement.VIN := TEncoding.ASCII.GetString(VinSlice);
  Announcement.LogicalAddress := ReadUInt16BE(Bytes, Cursor + 17);
  SetLength(Announcement.EID, 6);
  Move(Bytes[Cursor + 19], Announcement.EID[0], 6);
  SetLength(Announcement.GID, 6);
  Move(Bytes[Cursor + 25], Announcement.GID[0], 6);
  Announcement.FurtherActionRequired := Bytes[Cursor + 31];
  if Header.PayloadLength >= 33 then
    Announcement.SyncStatus := Bytes[Cursor + 32];
  Result := True;
end;

//==============================================================================
//  Alive check
//==============================================================================
function BuildAliveCheckRequest(const Version: TOBDDoIPVersion): TBytes;
begin
  Result := BuildDoIPHeader(Version, dptAliveCheckRequest, 0);
end;

function BuildAliveCheckResponse(const TesterAddress: Word;
  const Version: TOBDDoIPVersion): TBytes;
var
  Header, Payload: TBytes;
begin
  SetLength(Payload, 2);
  WriteUInt16BE(Payload, 0, TesterAddress);
  Header := BuildDoIPHeader(Version, dptAliveCheckResponse, 2);
  Result := BytesAppend(Header, Payload);
end;

//==============================================================================
//  Diagnostic message
//==============================================================================
function BuildDiagnosticMessage(const SourceAddress, TargetAddress: Word;
  const UserData: TBytes; const Version: TOBDDoIPVersion): TBytes;
var
  Header, Payload: TBytes;
begin
  if Length(UserData) = 0 then
    raise EOBDDoIPError.Create('DiagnosticMessage UserData must not be empty');
  SetLength(Payload, 4 + Length(UserData));
  WriteUInt16BE(Payload, 0, SourceAddress);
  WriteUInt16BE(Payload, 2, TargetAddress);
  Move(UserData[0], Payload[4], Length(UserData));
  Header := BuildDoIPHeader(Version, dptDiagnosticMessage,
    Cardinal(Length(Payload)));
  Result := BytesAppend(Header, Payload);
end;

function ParseDiagnosticMessage(const Bytes: TBytes;
  out Msg: TOBDDoIPDiagnosticMessage): Boolean;
var
  Header: TOBDDoIPHeader;
  Cursor, UserLen: Integer;
begin
  Msg := Default(TOBDDoIPDiagnosticMessage);
  Cursor := ParseDoIPHeader(Bytes, Header);
  if Header.PayloadType <> Word(dptDiagnosticMessage) then Exit(False);
  if (Header.PayloadLength < 5) or
     (Length(Bytes) < Cursor + Integer(Header.PayloadLength)) then
    raise EOBDDoIPError.CreateFmt(
      'DiagnosticMessage payload too short (%d)', [Header.PayloadLength]);
  Msg.SourceAddress := ReadUInt16BE(Bytes, Cursor);
  Msg.TargetAddress := ReadUInt16BE(Bytes, Cursor + 2);
  UserLen := Integer(Header.PayloadLength) - 4;
  SetLength(Msg.UserData, UserLen);
  if UserLen > 0 then
    Move(Bytes[Cursor + 4], Msg.UserData[0], UserLen);
  Result := True;
end;

end.
