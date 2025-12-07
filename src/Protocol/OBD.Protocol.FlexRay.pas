//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.FlexRay.pas
// CONTENTS       : FlexRay Protocol Support for High-Speed Applications
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.FlexRay;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  OBD.Protocol, OBD.Protocol.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   FlexRay Channel
  /// </summary>
  TFlexRayChannel = (
    frChannelA,      // Channel A
    frChannelB,      // Channel B (redundant)
    frChannelAB      // Both channels (for fault-tolerant operation)
  );

  /// <summary>
  ///   FlexRay Slot Mode
  /// </summary>
  TFlexRaySlotMode = (
    frSingleSlot,    // Single slot transmission
    frAllSlots       // All slots in cycle
  );

  /// <summary>
  ///   FlexRay Frame structure
  /// </summary>
  TFlexRayFrame = record
    /// <summary>
    ///   Frame ID (1-2047)
    /// </summary>
    FrameID: Word;
    /// <summary>
    ///   Cycle count (0-63)
    /// </summary>
    Cycle: Byte;
    /// <summary>
    ///   Channel
    /// </summary>
    Channel: TFlexRayChannel;
    /// <summary>
    ///   Payload data (0-254 bytes)
    /// </summary>
    Data: TBytes;
    /// <summary>
    ///   Header CRC
    /// </summary>
    HeaderCRC: Word;
    /// <summary>
    ///   Frame CRC
    /// </summary>
    FrameCRC: array[0..2] of Byte;
    /// <summary>
    ///   Is frame valid?
    /// </summary>
    Valid: Boolean;
    /// <summary>
    ///   Is startup frame?
    /// </summary>
    IsStartup: Boolean;
    /// <summary>
    ///   Is sync frame?
    /// </summary>
    IsSync: Boolean;
    /// <summary>
    ///   Payload length in words (16-bit)
    /// </summary>
    PayloadLength: Byte;
  end;

  /// <summary>
  ///   FlexRay Cluster Configuration
  /// </summary>
  TFlexRayClusterConfig = record
    /// <summary>
    ///   Cycle length in microseconds (default: 5000µs = 5ms)
    /// </summary>
    CycleLengthUS: Cardinal;
    /// <summary>
    ///   Number of static slots
    /// </summary>
    StaticSlots: Word;
    /// <summary>
    ///   Static slot length in macroticks
    /// </summary>
    StaticSlotLength: Word;
    /// <summary>
    ///   Dynamic slot length in macroticks
    /// </summary>
    DynamicSlotLength: Word;
    /// <summary>
    ///   Bit rate (2.5, 5, or 10 Mbps)
    /// </summary>
    BitRateMbps: Single;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   FlexRay Protocol Implementation
  ///   High-speed, deterministic, fault-tolerant automotive network
  /// </summary>
  TFlexRayProtocol = class(TOBDProtocol)
  private
    FClusterConfig: TFlexRayClusterConfig;
    FCurrentCycle: Byte;
    
    /// <summary>
    ///   Calculate header CRC
    /// </summary>
    function CalculateHeaderCRC(FrameID: Word; PayloadLength: Byte; 
      HeaderCRC: Word; Cycle: Byte): Word;
    
    /// <summary>
    ///   Calculate frame CRC
    /// </summary>
    function CalculateFrameCRC(const Header: TBytes; const Payload: TBytes): TBytes;
    
    /// <summary>
    ///   Encode frame header
    /// </summary>
    function EncodeHeader(FrameID: Word; PayloadLength: Byte; Cycle: Byte;
      IsStartup: Boolean; IsSync: Boolean): TBytes;
  protected
    function GetName: string; override;
    function GetDisplayName: string; override;
    function GetELMID: string; override;
  public
    constructor Create;
    
    /// <summary>
    ///   Parse FlexRay frame from raw data
    /// </summary>
    function ParseFrame(const RawData: TBytes): TFlexRayFrame;
    
    /// <summary>
    ///   Create FlexRay frame
    /// </summary>
    function CreateFrame(FrameID: Word; const Data: TBytes; 
      Channel: TFlexRayChannel; IsStartup: Boolean = False; 
      IsSync: Boolean = False): TBytes;
    
    /// <summary>
    ///   Send static segment frame (deterministic)
    /// </summary>
    function SendStaticFrame(FrameID: Word; const Data: TBytes; 
      Channel: TFlexRayChannel): TBytes;
    
    /// <summary>
    ///   Send dynamic segment frame (event-triggered)
    /// </summary>
    function SendDynamicFrame(FrameID: Word; const Data: TBytes; 
      Channel: TFlexRayChannel): TBytes;
    
    /// <summary>
    ///   Configure cluster parameters
    /// </summary>
    procedure ConfigureCluster(CycleLengthUS: Cardinal; StaticSlots: Word;
      StaticSlotLength: Word; DynamicSlotLength: Word; BitRateMbps: Single);
    
    /// <summary>
    ///   Start communication (wakeup)
    /// </summary>
    function StartCommunication(Channel: TFlexRayChannel): TBytes;
    
    /// <summary>
    ///   Read diagnostic data via FlexRay
    /// </summary>
    function ReadDiagnosticData(FrameID: Word; ServiceID: Byte; 
      const Parameters: TBytes): TBytes;
    
    /// <summary>
    ///   Write diagnostic data via FlexRay
    /// </summary>
    function WriteDiagnosticData(FrameID: Word; ServiceID: Byte; 
      const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Cluster configuration
    /// </summary>
    property ClusterConfig: TFlexRayClusterConfig read FClusterConfig write FClusterConfig;
    
    /// <summary>
    ///   Current cycle counter
    /// </summary>
    property CurrentCycle: Byte read FCurrentCycle;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TFlexRayProtocol.Create;
begin
  inherited Create;
  
  // Default cluster configuration
  FClusterConfig.CycleLengthUS := 5000;      // 5ms cycle
  FClusterConfig.StaticSlots := 60;
  FClusterConfig.StaticSlotLength := 50;
  FClusterConfig.DynamicSlotLength := 10;
  FClusterConfig.BitRateMbps := 10.0;        // 10 Mbps
  
  FCurrentCycle := 0;
end;

//------------------------------------------------------------------------------
// GET NAME
//------------------------------------------------------------------------------
function TFlexRayProtocol.GetName: string;
begin
  Result := 'FlexRay';
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TFlexRayProtocol.GetDisplayName: string;
begin
  Result := Format('FlexRay @ %.1f Mbps', [FClusterConfig.BitRateMbps]);
end;

//------------------------------------------------------------------------------
// GET ELM ID
//------------------------------------------------------------------------------
function TFlexRayProtocol.GetELMID: string;
begin
  Result := 'F'; // FlexRay protocol identifier
end;

//------------------------------------------------------------------------------
// CALCULATE HEADER CRC
//------------------------------------------------------------------------------
function TFlexRayProtocol.CalculateHeaderCRC(FrameID: Word; PayloadLength: Byte; 
  HeaderCRC: Word; Cycle: Byte): Word;
const
  CRC_POLY = $0385; // FlexRay header CRC polynomial
var
  I, Bit: Integer;
  Data: Cardinal;
  CRC: Word;
begin
  // Combine header fields
  Data := (Cardinal(FrameID) shl 20) or (Cardinal(PayloadLength) shl 13) or 
          (Cardinal(HeaderCRC) shl 7) or Cardinal(Cycle);
  
  CRC := $001A; // Initial value
  
  for I := 0 to 19 do // 20 bits of data
  begin
    Bit := (Data shr (19 - I)) and 1;
    if ((CRC shr 10) and 1) <> Bit then
      CRC := ((CRC shl 1) xor CRC_POLY) and $7FF
    else
      CRC := (CRC shl 1) and $7FF;
  end;
  
  Result := CRC;
end;

//------------------------------------------------------------------------------
// CALCULATE FRAME CRC
//------------------------------------------------------------------------------
function TFlexRayProtocol.CalculateFrameCRC(const Header: TBytes; const Payload: TBytes): TBytes;
const
  CRC_POLY = $5D6DCB; // FlexRay frame CRC polynomial (24-bit)
var
  I, J: Integer;
  CRC: Cardinal;
  Data: Byte;
begin
  SetLength(Result, 3);
  CRC := $FEDC00; // Initial value
  
  // Process header
  for I := 0 to High(Header) do
  begin
    Data := Header[I];
    for J := 0 to 7 do
    begin
      if ((CRC shr 23) and 1) <> ((Data shr (7 - J)) and 1) then
        CRC := ((CRC shl 1) xor CRC_POLY) and $FFFFFF
      else
        CRC := (CRC shl 1) and $FFFFFF;
    end;
  end;
  
  // Process payload
  for I := 0 to High(Payload) do
  begin
    Data := Payload[I];
    for J := 0 to 7 do
    begin
      if ((CRC shr 23) and 1) <> ((Data shr (7 - J)) and 1) then
        CRC := ((CRC shl 1) xor CRC_POLY) and $FFFFFF
      else
        CRC := (CRC shl 1) and $FFFFFF;
    end;
  end;
  
  Result[0] := (CRC shr 16) and $FF;
  Result[1] := (CRC shr 8) and $FF;
  Result[2] := CRC and $FF;
end;

//------------------------------------------------------------------------------
// ENCODE HEADER
//------------------------------------------------------------------------------
function TFlexRayProtocol.EncodeHeader(FrameID: Word; PayloadLength: Byte; Cycle: Byte;
  IsStartup: Boolean; IsSync: Boolean): TBytes;
var
  Header: Word;
  HeaderCRC: Word;
  Flags: Byte;
begin
  SetLength(Result, 5);
  
  // Build header word
  Flags := 0;
  if IsStartup then Flags := Flags or $20;
  if IsSync then Flags := Flags or $10;
  
  Header := (FrameID and $7FF) shl 5; // Frame ID (11 bits)
  Header := Header or (PayloadLength and $7F); // Payload length (7 bits)
  
  // Calculate header CRC
  HeaderCRC := CalculateHeaderCRC(FrameID, PayloadLength, 0, Cycle);
  
  // Pack header
  Result[0] := (Header shr 8) and $FF;
  Result[1] := Header and $FF;
  Result[2] := ((HeaderCRC shr 3) and $FF) or Flags;
  Result[3] := ((HeaderCRC and $07) shl 5) or (Cycle and $3F);
  Result[4] := 0; // Reserved
end;

//------------------------------------------------------------------------------
// PARSE FRAME
//------------------------------------------------------------------------------
function TFlexRayProtocol.ParseFrame(const RawData: TBytes): TFlexRayFrame;
var
  Header: Word;
  PayloadLengthWords: Byte;
  PayloadLengthBytes: Integer;
begin
  Result.Valid := False;
  
  if Length(RawData) < 8 then // Minimum: 5 bytes header + 3 bytes CRC
    Exit;
  
  // Parse header
  Header := (Word(RawData[0]) shl 8) or RawData[1];
  Result.FrameID := (Header shr 5) and $7FF;
  PayloadLengthWords := Header and $7F;
  PayloadLengthBytes := PayloadLengthWords * 2;
  Result.PayloadLength := PayloadLengthWords;
  
  Result.HeaderCRC := ((Word(RawData[2] and $1F) shl 6) or ((RawData[3] shr 2) and $3F));
  Result.IsStartup := (RawData[2] and $20) <> 0;
  Result.IsSync := (RawData[2] and $10) <> 0;
  Result.Cycle := RawData[3] and $3F;
  
  // Extract payload
  if PayloadLengthBytes > 0 then
  begin
    SetLength(Result.Data, PayloadLengthBytes);
    Move(RawData[5], Result.Data[0], PayloadLengthBytes);
  end;
  
  // Extract frame CRC
  if Length(RawData) >= (5 + PayloadLengthBytes + 3) then
  begin
    Result.FrameCRC[0] := RawData[5 + PayloadLengthBytes];
    Result.FrameCRC[1] := RawData[5 + PayloadLengthBytes + 1];
    Result.FrameCRC[2] := RawData[5 + PayloadLengthBytes + 2];
    Result.Valid := True;
  end;
end;

//------------------------------------------------------------------------------
// CREATE FRAME
//------------------------------------------------------------------------------
function TFlexRayProtocol.CreateFrame(FrameID: Word; const Data: TBytes; 
  Channel: TFlexRayChannel; IsStartup: Boolean; IsSync: Boolean): TBytes;
var
  Header: TBytes;
  PayloadLengthWords: Byte;
  FrameCRC: TBytes;
  TotalLen: Integer;
begin
  PayloadLengthWords := (Length(Data) + 1) div 2; // Round up to words
  
  // Encode header
  Header := EncodeHeader(FrameID, PayloadLengthWords, FCurrentCycle, IsStartup, IsSync);
  
  // Calculate frame CRC
  FrameCRC := CalculateFrameCRC(Header, Data);
  
  // Build complete frame
  TotalLen := Length(Header) + Length(Data) + Length(FrameCRC);
  SetLength(Result, TotalLen);
  
  Move(Header[0], Result[0], Length(Header));
  if Length(Data) > 0 then
    Move(Data[0], Result[Length(Header)], Length(Data));
  Move(FrameCRC[0], Result[Length(Header) + Length(Data)], Length(FrameCRC));
  
  // Increment cycle counter
  FCurrentCycle := (FCurrentCycle + 1) mod 64;
end;

//------------------------------------------------------------------------------
// SEND STATIC FRAME
//------------------------------------------------------------------------------
function TFlexRayProtocol.SendStaticFrame(FrameID: Word; const Data: TBytes; 
  Channel: TFlexRayChannel): TBytes;
begin
  // Static frames use lower frame IDs and are sent deterministically
  if FrameID > FClusterConfig.StaticSlots then
    raise Exception.Create('Frame ID exceeds static slot range');
  
  Result := CreateFrame(FrameID, Data, Channel, False, False);
end;

//------------------------------------------------------------------------------
// SEND DYNAMIC FRAME
//------------------------------------------------------------------------------
function TFlexRayProtocol.SendDynamicFrame(FrameID: Word; const Data: TBytes; 
  Channel: TFlexRayChannel): TBytes;
begin
  // Dynamic frames use higher frame IDs and are event-triggered
  if FrameID <= FClusterConfig.StaticSlots then
    raise Exception.Create('Frame ID must be in dynamic segment range');
  
  Result := CreateFrame(FrameID, Data, Channel, False, False);
end;

//------------------------------------------------------------------------------
// CONFIGURE CLUSTER
//------------------------------------------------------------------------------
procedure TFlexRayProtocol.ConfigureCluster(CycleLengthUS: Cardinal; StaticSlots: Word;
  StaticSlotLength: Word; DynamicSlotLength: Word; BitRateMbps: Single);
begin
  FClusterConfig.CycleLengthUS := CycleLengthUS;
  FClusterConfig.StaticSlots := StaticSlots;
  FClusterConfig.StaticSlotLength := StaticSlotLength;
  FClusterConfig.DynamicSlotLength := DynamicSlotLength;
  FClusterConfig.BitRateMbps := BitRateMbps;
end;

//------------------------------------------------------------------------------
// START COMMUNICATION
//------------------------------------------------------------------------------
function TFlexRayProtocol.StartCommunication(Channel: TFlexRayChannel): TBytes;
var
  WakeupData: TBytes;
begin
  SetLength(WakeupData, 1);
  WakeupData[0] := $01; // Wakeup pattern
  
  // Create startup frame
  Result := CreateFrame(1, WakeupData, Channel, True, True);
end;

//------------------------------------------------------------------------------
// READ DIAGNOSTIC DATA
//------------------------------------------------------------------------------
function TFlexRayProtocol.ReadDiagnosticData(FrameID: Word; ServiceID: Byte; 
  const Parameters: TBytes): TBytes;
var
  DiagData: TBytes;
begin
  SetLength(DiagData, 1 + Length(Parameters));
  DiagData[0] := ServiceID;
  if Length(Parameters) > 0 then
    Move(Parameters[0], DiagData[1], Length(Parameters));
  
  Result := CreateFrame(FrameID, DiagData, frChannelA, False, False);
end;

//------------------------------------------------------------------------------
// WRITE DIAGNOSTIC DATA
//------------------------------------------------------------------------------
function TFlexRayProtocol.WriteDiagnosticData(FrameID: Word; ServiceID: Byte; 
  const Data: TBytes): TBytes;
var
  DiagData: TBytes;
begin
  SetLength(DiagData, 1 + Length(Data));
  DiagData[0] := ServiceID;
  if Length(Data) > 0 then
    Move(Data[0], DiagData[1], Length(Data));
  
  Result := CreateFrame(FrameID, DiagData, frChannelA, False, False);
end;

end.
