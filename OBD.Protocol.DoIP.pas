//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.DoIP.pas
// CONTENTS       : Diagnostics over IP (DoIP) Protocol - ISO 13400
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.DoIP;

interface

uses
  System.SysUtils, System.Classes,
  OBD.Protocol, OBD.Protocol.Types;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   DoIP Protocol Version (ISO 13400)
  /// </summary>
  DOIP_PROTOCOL_VERSION = $02;
  
  /// <summary>
  ///   Inverse Protocol Version (bitwise NOT of version)
  /// </summary>
  DOIP_INVERSE_PROTOCOL_VERSION = $FD;
  
  /// <summary>
  ///   DoIP Generic Header NACK
  /// </summary>
  DOIP_PAYLOAD_TYPE_GENERIC_HEADER_NACK = $0000;
  
  /// <summary>
  ///   Vehicle identification request
  /// </summary>
  DOIP_PAYLOAD_TYPE_VEHICLE_IDENTIFICATION_REQUEST = $0001;
  
  /// <summary>
  ///   Vehicle identification request with EID
  /// </summary>
  DOIP_PAYLOAD_TYPE_VEHICLE_IDENTIFICATION_REQUEST_EID = $0002;
  
  /// <summary>
  ///   Vehicle identification request with VIN
  /// </summary>
  DOIP_PAYLOAD_TYPE_VEHICLE_IDENTIFICATION_REQUEST_VIN = $0003;
  
  /// <summary>
  ///   Vehicle announcement message/vehicle identification response
  /// </summary>
  DOIP_PAYLOAD_TYPE_VEHICLE_ANNOUNCEMENT = $0004;
  
  /// <summary>
  ///   Routing activation request
  /// </summary>
  DOIP_PAYLOAD_TYPE_ROUTING_ACTIVATION_REQUEST = $0005;
  
  /// <summary>
  ///   Routing activation response
  /// </summary>
  DOIP_PAYLOAD_TYPE_ROUTING_ACTIVATION_RESPONSE = $0006;
  
  /// <summary>
  ///   Alive check request
  /// </summary>
  DOIP_PAYLOAD_TYPE_ALIVE_CHECK_REQUEST = $0007;
  
  /// <summary>
  ///   Alive check response
  /// </summary>
  DOIP_PAYLOAD_TYPE_ALIVE_CHECK_RESPONSE = $0008;
  
  /// <summary>
  ///   DoIP entity status request
  /// </summary>
  DOIP_PAYLOAD_TYPE_ENTITY_STATUS_REQUEST = $4001;
  
  /// <summary>
  ///   DoIP entity status response
  /// </summary>
  DOIP_PAYLOAD_TYPE_ENTITY_STATUS_RESPONSE = $4002;
  
  /// <summary>
  ///   Diagnostic power mode information request
  /// </summary>
  DOIP_PAYLOAD_TYPE_POWER_MODE_REQUEST = $4003;
  
  /// <summary>
  ///   Diagnostic power mode information response
  /// </summary>
  DOIP_PAYLOAD_TYPE_POWER_MODE_RESPONSE = $4004;
  
  /// <summary>
  ///   Diagnostic message (UDS request/response)
  /// </summary>
  DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE = $8001;
  
  /// <summary>
  ///   Diagnostic message positive acknowledgement
  /// </summary>
  DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE_ACK = $8002;
  
  /// <summary>
  ///   Diagnostic message negative acknowledgement
  /// </summary>
  DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE_NACK = $8003;
  
  /// <summary>
  ///   DoIP Header Size (8 bytes)
  /// </summary>
  DOIP_HEADER_SIZE = 8;
  
  /// <summary>
  ///   Default tester source address (commonly used)
  /// </summary>
  DOIP_DEFAULT_TESTER_ADDRESS = $0E80;
  
  /// <summary>
  ///   BMW Gateway default address
  /// </summary>
  DOIP_BMW_GATEWAY_ADDRESS = $1000;
  
  /// <summary>
  ///   Routing activation type: Default
  /// </summary>
  DOIP_ROUTING_ACTIVATION_TYPE_DEFAULT = $00;
  
  /// <summary>
  ///   Routing activation type: WWH-OBD
  /// </summary>
  DOIP_ROUTING_ACTIVATION_TYPE_WWH_OBD = $01;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   DoIP Protocol Header (8 bytes)
  /// </summary>
  TDoIPHeader = packed record
    ProtocolVersion: Byte;          // 0x02
    InverseProtocolVersion: Byte;   // 0xFD
    PayloadType: Word;              // Big-endian
    PayloadLength: Cardinal;        // Big-endian
  end;

  /// <summary>
  ///   DoIP Diagnostic Message structure
  /// </summary>
  TDoIPDiagnosticMessage = record
    SourceAddress: Word;     // Tester address (e.g., 0x0E80)
    TargetAddress: Word;     // ECU address (e.g., 0x1000)
    UserData: TBytes;        // UDS request/response data
  end;

  /// <summary>
  ///   DoIP Routing Activation Response Code
  /// </summary>
  TDoIPRoutingActivationResponseCode = (
    rcUnknown = $00,
    rcSuccess = $10,
    rcUnsupportedActivationType = $06,
    rcConfirmationRequired = $01,
    rcSocketAlreadyRegistered = $02,
    rcDifferentSourceAddress = $03,
    rcAlreadyRegisteredAndActive = $04,
    rcAuthenticationMissing = $05,
    rcRejectedConfirmation = $07,
    rcUnsupportedVersion = $08
  );

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Diagnostics over IP (DoIP) Protocol Handler - ISO 13400
  /// </summary>
  TDoIPProtocol = class(TOBDProtocol)
  private
    FSourceAddress: Word;
    FTargetAddress: Word;
    FRoutingActivated: Boolean;
    
    function SwapWord(Value: Word): Word;
    function SwapCardinal(Value: Cardinal): Cardinal;
  protected
    function GetName: string; override;
    function GetDisplayName: string; override;
    function GetELMID: string; override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Lines: TStrings; AllowLongMessages: Boolean); override;
    
    /// <summary>
    ///   Build DoIP header
    /// </summary>
    function BuildDoIPHeader(PayloadType: Word; PayloadLength: Cardinal): TBytes;
    
    /// <summary>
    ///   Parse DoIP header from received data
    /// </summary>
    function ParseDoIPHeader(const Data: TBytes; out Header: TDoIPHeader): Boolean;
    
    /// <summary>
    ///   Build routing activation request
    /// </summary>
    function BuildRoutingActivationRequest(ActivationType: Byte = DOIP_ROUTING_ACTIVATION_TYPE_DEFAULT): TBytes;
    
    /// <summary>
    ///   Parse routing activation response
    /// </summary>
    function ParseRoutingActivationResponse(const Data: TBytes; out ResponseCode: TDoIPRoutingActivationResponseCode): Boolean;
    
    /// <summary>
    ///   Build diagnostic message (UDS request)
    /// </summary>
    function BuildDiagnosticMessage(const UDSData: TBytes): TBytes;
    
    /// <summary>
    ///   Parse diagnostic message (UDS response)
    /// </summary>
    function ParseDiagnosticMessage(const Data: TBytes; out Msg: TDoIPDiagnosticMessage): Boolean;
    
    /// <summary>
    ///   Build alive check response
    /// </summary>
    function BuildAliveCheckResponse: TBytes;
    
    /// <summary>
    ///   Parse a Data Frame (not used in DoIP)
    /// </summary>
    function ParseFrame(Frame: IOBDDataFrame): Boolean; override;
    
    /// <summary>
    ///   Parse a Data Message (not used in DoIP)
    /// </summary>
    function ParseMessage(Msg: IOBDDataMessage): Boolean; override;
    
    /// <summary>
    ///   Tester source address (default: 0x0E80)
    /// </summary>
    property SourceAddress: Word read FSourceAddress write FSourceAddress;
    
    /// <summary>
    ///   Target ECU address (e.g., 0x1000 for BMW gateway)
    /// </summary>
    property TargetAddress: Word read FTargetAddress write FTargetAddress;
    
    /// <summary>
    ///   Routing activation status
    /// </summary>
    property RoutingActivated: Boolean read FRoutingActivated write FRoutingActivated;
  end;

implementation

//------------------------------------------------------------------------------
// SWAP WORD (BIG-ENDIAN <-> LITTLE-ENDIAN)
//------------------------------------------------------------------------------
function TDoIPProtocol.SwapWord(Value: Word): Word;
begin
  Result := (Value shr 8) or (Value shl 8);
end;

//------------------------------------------------------------------------------
// SWAP CARDINAL (BIG-ENDIAN <-> LITTLE-ENDIAN)
//------------------------------------------------------------------------------
function TDoIPProtocol.SwapCardinal(Value: Cardinal): Cardinal;
begin
  Result := ((Value and $FF) shl 24) or 
            ((Value and $FF00) shl 8) or 
            ((Value and $FF0000) shr 8) or 
            ((Value and $FF000000) shr 24);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TDoIPProtocol.Create(Lines: TStrings; AllowLongMessages: Boolean);
begin
  inherited Create(Lines, AllowLongMessages);
  FSourceAddress := DOIP_DEFAULT_TESTER_ADDRESS;
  FTargetAddress := DOIP_BMW_GATEWAY_ADDRESS;
  FRoutingActivated := False;
end;

//------------------------------------------------------------------------------
// GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TDoIPProtocol.GetName: string;
begin
  Result := 'DoIP (ISO 13400)';
end;

//------------------------------------------------------------------------------
// GET PROTOCOL DISPLAY NAME
//------------------------------------------------------------------------------
function TDoIPProtocol.GetDisplayName: string;
begin
  Result := 'Diagnostics over IP (Ethernet/UDP)';
end;

//------------------------------------------------------------------------------
// GET ELM ID (NOT APPLICABLE FOR DoIP)
//------------------------------------------------------------------------------
function TDoIPProtocol.GetELMID: string;
begin
  Result := 'N/A';
end;

//------------------------------------------------------------------------------
// BUILD DOIP HEADER
//------------------------------------------------------------------------------
function TDoIPProtocol.BuildDoIPHeader(PayloadType: Word; PayloadLength: Cardinal): TBytes;
var
  Header: TDoIPHeader;
begin
  Header.ProtocolVersion := DOIP_PROTOCOL_VERSION;
  Header.InverseProtocolVersion := DOIP_INVERSE_PROTOCOL_VERSION;
  Header.PayloadType := SwapWord(PayloadType);  // Convert to big-endian
  Header.PayloadLength := SwapCardinal(PayloadLength);  // Convert to big-endian
  
  SetLength(Result, DOIP_HEADER_SIZE);
  Move(Header, Result[0], DOIP_HEADER_SIZE);
end;

//------------------------------------------------------------------------------
// PARSE DOIP HEADER
//------------------------------------------------------------------------------
function TDoIPProtocol.ParseDoIPHeader(const Data: TBytes; out Header: TDoIPHeader): Boolean;
begin
  Result := False;
  
  if Length(Data) < DOIP_HEADER_SIZE then Exit;
  
  Move(Data[0], Header, DOIP_HEADER_SIZE);
  
  // Validate protocol version
  if (Header.ProtocolVersion <> DOIP_PROTOCOL_VERSION) or
     (Header.InverseProtocolVersion <> DOIP_INVERSE_PROTOCOL_VERSION) then
    Exit;
  
  // Convert from big-endian to host byte order
  Header.PayloadType := SwapWord(Header.PayloadType);
  Header.PayloadLength := SwapCardinal(Header.PayloadLength);
  
  Result := True;
end;

//------------------------------------------------------------------------------
// BUILD ROUTING ACTIVATION REQUEST
//------------------------------------------------------------------------------
function TDoIPProtocol.BuildRoutingActivationRequest(ActivationType: Byte = DOIP_ROUTING_ACTIVATION_TYPE_DEFAULT): TBytes;
var
  Header: TBytes;
  Payload: TBytes;
begin
  // Payload: Source Address (2) + Activation Type (1) + Reserved (4)
  SetLength(Payload, 7);
  
  // Source address (big-endian)
  Payload[0] := Hi(FSourceAddress);
  Payload[1] := Lo(FSourceAddress);
  
  // Activation type
  Payload[2] := ActivationType;
  
  // Reserved bytes (all zeros)
  Payload[3] := $00;
  Payload[4] := $00;
  Payload[5] := $00;
  Payload[6] := $00;
  
  // Build header
  Header := BuildDoIPHeader(DOIP_PAYLOAD_TYPE_ROUTING_ACTIVATION_REQUEST, Length(Payload));
  
  // Combine header and payload
  Result := Header + Payload;
end;

//------------------------------------------------------------------------------
// PARSE ROUTING ACTIVATION RESPONSE
//------------------------------------------------------------------------------
function TDoIPProtocol.ParseRoutingActivationResponse(const Data: TBytes; out ResponseCode: TDoIPRoutingActivationResponseCode): Boolean;
var
  Header: TDoIPHeader;
  PayloadOffset: Integer;
begin
  Result := False;
  ResponseCode := rcUnknown;
  
  if not ParseDoIPHeader(Data, Header) then Exit;
  
  if Header.PayloadType <> DOIP_PAYLOAD_TYPE_ROUTING_ACTIVATION_RESPONSE then Exit;
  
  PayloadOffset := DOIP_HEADER_SIZE;
  
  // Payload: Tester Address (2) + Entity Address (2) + Response Code (1) + Reserved (4)
  if Length(Data) < PayloadOffset + 9 then Exit;
  
  // Get response code
  case Data[PayloadOffset + 4] of
    $10: ResponseCode := rcSuccess;
    $06: ResponseCode := rcUnsupportedActivationType;
    $01: ResponseCode := rcConfirmationRequired;
    $02: ResponseCode := rcSocketAlreadyRegistered;
    $03: ResponseCode := rcDifferentSourceAddress;
    $04: ResponseCode := rcAlreadyRegisteredAndActive;
    $05: ResponseCode := rcAuthenticationMissing;
    $07: ResponseCode := rcRejectedConfirmation;
    $08: ResponseCode := rcUnsupportedVersion;
  else
    ResponseCode := rcUnknown;
  end;
  
  FRoutingActivated := (ResponseCode = rcSuccess);
  Result := True;
end;

//------------------------------------------------------------------------------
// BUILD DIAGNOSTIC MESSAGE
//------------------------------------------------------------------------------
function TDoIPProtocol.BuildDiagnosticMessage(const UDSData: TBytes): TBytes;
var
  Header: TBytes;
  Payload: TBytes;
  PayloadLength: Cardinal;
begin
  // Payload: Source Address (2) + Target Address (2) + User Data (variable)
  PayloadLength := 4 + Length(UDSData);
  SetLength(Payload, PayloadLength);
  
  // Source address (big-endian)
  Payload[0] := Hi(FSourceAddress);
  Payload[1] := Lo(FSourceAddress);
  
  // Target address (big-endian)
  Payload[2] := Hi(FTargetAddress);
  Payload[3] := Lo(FTargetAddress);
  
  // User data (UDS request)
  if Length(UDSData) > 0 then
    Move(UDSData[0], Payload[4], Length(UDSData));
  
  // Build header
  Header := BuildDoIPHeader(DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE, PayloadLength);
  
  // Combine header and payload
  Result := Header + Payload;
end;

//------------------------------------------------------------------------------
// PARSE DIAGNOSTIC MESSAGE
//------------------------------------------------------------------------------
function TDoIPProtocol.ParseDiagnosticMessage(const Data: TBytes; out Msg: TDoIPDiagnosticMessage): Boolean;
var
  Header: TDoIPHeader;
  PayloadOffset: Integer;
  UserDataLength: Integer;
begin
  Result := False;
  
  if not ParseDoIPHeader(Data, Header) then Exit;
  
  if Header.PayloadType <> DOIP_PAYLOAD_TYPE_DIAGNOSTIC_MESSAGE then Exit;
  
  PayloadOffset := DOIP_HEADER_SIZE;
  
  // Minimum payload: Source Address (2) + Target Address (2) = 4 bytes
  if Length(Data) < PayloadOffset + 4 then Exit;
  
  // Parse source and target addresses (big-endian)
  Msg.SourceAddress := (Data[PayloadOffset] shl 8) or Data[PayloadOffset + 1];
  Msg.TargetAddress := (Data[PayloadOffset + 2] shl 8) or Data[PayloadOffset + 3];
  
  // Extract user data (UDS response)
  UserDataLength := Length(Data) - PayloadOffset - 4;
  if UserDataLength > 0 then
  begin
    SetLength(Msg.UserData, UserDataLength);
    Move(Data[PayloadOffset + 4], Msg.UserData[0], UserDataLength);
  end
  else
    SetLength(Msg.UserData, 0);
  
  Result := True;
end;

//------------------------------------------------------------------------------
// BUILD ALIVE CHECK RESPONSE
//------------------------------------------------------------------------------
function TDoIPProtocol.BuildAliveCheckResponse: TBytes;
var
  Header: TBytes;
  Payload: TBytes;
begin
  // Payload: Source Address (2)
  SetLength(Payload, 2);
  Payload[0] := Hi(FSourceAddress);
  Payload[1] := Lo(FSourceAddress);
  
  // Build header
  Header := BuildDoIPHeader(DOIP_PAYLOAD_TYPE_ALIVE_CHECK_RESPONSE, Length(Payload));
  
  // Combine header and payload
  Result := Header + Payload;
end;

//------------------------------------------------------------------------------
// PARSE FRAME (NOT USED IN DOIP - USES UDS DIRECTLY)
//------------------------------------------------------------------------------
function TDoIPProtocol.ParseFrame(Frame: IOBDDataFrame): Boolean;
begin
  // DoIP doesn't use frame-based parsing like CAN/Legacy protocols
  Result := False;
end;

//------------------------------------------------------------------------------
// PARSE MESSAGE (NOT USED IN DOIP - USES UDS DIRECTLY)
//------------------------------------------------------------------------------
function TDoIPProtocol.ParseMessage(Msg: IOBDDataMessage): Boolean;
begin
  // DoIP doesn't use message-based parsing like CAN/Legacy protocols
  Result := False;
end;

end.
