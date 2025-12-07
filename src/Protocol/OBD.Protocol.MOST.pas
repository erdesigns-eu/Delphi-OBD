//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.MOST.pas
// CONTENTS       : MOST (Media Oriented Systems Transport) Protocol Support
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.MOST;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  OBD.Protocol, OBD.Protocol.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   MOST Protocol Version
  /// </summary>
  TMOSTVersion = (
    most25,     // MOST25 @ 25 Mbps
    most50,     // MOST50 @ 50 Mbps
    most150     // MOST150 @ 150 Mbps
  );

  /// <summary>
  ///   MOST Message Type
  /// </summary>
  TMOSTMessageType = (
    mostControl,        // Control message (low bandwidth, high priority)
    mostAsync,          // Asynchronous data (packet-based)
    mostStream,         // Synchronous streaming (audio/video)
    mostIsochronous     // Isochronous data
  );

  /// <summary>
  ///   MOST Function Catalog (FBlock IDs)
  /// </summary>
  TMOSTFBlock = (
    fbNetworkMaster    = $00,
    fbAudioDiskPlayer  = $11,
    fbAmFmTuner        = $21,
    fbVideoPlayer      = $31,
    fbTelephone        = $41,
    fbNavigation       = $51,
    fbDiagnostics      = $70,
    fbSystemTest       = $FF
  );

  /// <summary>
  ///   MOST Control Message structure
  /// </summary>
  TMOSTControlMessage = record
    /// <summary>
    ///   Source address (12-bit)
    /// </summary>
    SourceAddr: Word;
    /// <summary>
    ///   Target address (12-bit, $FFF = broadcast)
    /// </summary>
    TargetAddr: Word;
    /// <summary>
    ///   Function Block ID
    /// </summary>
    FBlockID: Byte;
    /// <summary>
    ///   Instance ID
    /// </summary>
    InstanceID: Byte;
    /// <summary>
    ///   Function ID
    /// </summary>
    FunctionID: Word;
    /// <summary>
    ///   Operation type (Get, Set, Status, etc.)
    /// </summary>
    OpType: Byte;
    /// <summary>
    ///   Message data
    /// </summary>
    Data: TBytes;
    /// <summary>
    ///   Message type
    /// </summary>
    MsgType: TMOSTMessageType;
  end;

  /// <summary>
  ///   MOST Operation Types
  /// </summary>
  TMOSTOpType = (
    mostOpGet       = $00,  // Get property value
    mostOpStatus    = $01,  // Status response
    mostOpSet       = $02,  // Set property value
    mostOpSetGet    = $03,  // Set and get
    mostOpIncrement = $04,  // Increment value
    mostOpDecrement = $05,  // Decrement value
    mostOpError     = $0F   // Error response
  );

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   MOST Protocol Implementation
  ///   Media Oriented Systems Transport for automotive infotainment
  /// </summary>
  TMOSTProtocol = class(TOBDProtocol)
  private
    FVersion: TMOSTVersion;
    FNodeAddress: Word;
    FBitRate: Integer;
    
    /// <summary>
    ///   Build control message
    /// </summary>
    function BuildControlMessage(const Msg: TMOSTControlMessage): TBytes;
    
    /// <summary>
    ///   Parse control message
    /// </summary>
    function ParseControlMessage(const RawData: TBytes): TMOSTControlMessage;
  protected
    function GetName: string; override;
    function GetDisplayName: string; override;
    function GetELMID: string; override;
  public
    constructor Create(AVersion: TMOSTVersion = most150);
    
    /// <summary>
    ///   Send control message
    /// </summary>
    function SendControlMessage(TargetAddr: Word; FBlockID: Byte; 
      FunctionID: Word; OpType: Byte; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Get property value
    /// </summary>
    function GetProperty(TargetAddr: Word; FBlockID: Byte; 
      FunctionID: Word): TBytes;
    
    /// <summary>
    ///   Set property value
    /// </summary>
    function SetProperty(TargetAddr: Word; FBlockID: Byte; 
      FunctionID: Word; const Value: TBytes): TBytes;
    
    /// <summary>
    ///   Send diagnostic request
    /// </summary>
    function SendDiagnosticRequest(ServiceID: Byte; const Parameters: TBytes): TBytes;
    
    /// <summary>
    ///   Read fault codes
    /// </summary>
    function ReadFaultCodes: TBytes;
    
    /// <summary>
    ///   Clear fault codes
    /// </summary>
    function ClearFaultCodes: TBytes;
    
    /// <summary>
    ///   Read ECU information
    /// </summary>
    function ReadECUInfo(InfoType: Byte): TBytes;
    
    /// <summary>
    ///   Request streaming channel
    /// </summary>
    function RequestStreamingChannel(SourceAddr: Word; ChannelID: Byte;
      Bandwidth: Word): TBytes;
    
    /// <summary>
    ///   Release streaming channel
    /// </summary>
    function ReleaseStreamingChannel(ChannelID: Byte): TBytes;
    
    /// <summary>
    ///   Network configuration
    /// </summary>
    function ConfigureNetwork: TBytes;
    
    /// <summary>
    ///   MOST version
    /// </summary>
    property Version: TMOSTVersion read FVersion write FVersion;
    
    /// <summary>
    ///   Node address
    /// </summary>
    property NodeAddress: Word read FNodeAddress write FNodeAddress;
    
    /// <summary>
    ///   Bit rate in Mbps
    /// </summary>
    property BitRate: Integer read FBitRate;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TMOSTProtocol.Create(AVersion: TMOSTVersion);
begin
  inherited Create;
  FVersion := AVersion;
  FNodeAddress := $100; // Default node address
  
  case FVersion of
    most25:  FBitRate := 25;
    most50:  FBitRate := 50;
    most150: FBitRate := 150;
  end;
end;

//------------------------------------------------------------------------------
// GET NAME
//------------------------------------------------------------------------------
function TMOSTProtocol.GetName: string;
begin
  case FVersion of
    most25:  Result := 'MOST25';
    most50:  Result := 'MOST50';
    most150: Result := 'MOST150';
  else
    Result := 'MOST';
  end;
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TMOSTProtocol.GetDisplayName: string;
begin
  Result := Format('%s (Media Oriented Systems Transport @ %d Mbps)', 
    [GetName, FBitRate]);
end;

//------------------------------------------------------------------------------
// GET ELM ID
//------------------------------------------------------------------------------
function TMOSTProtocol.GetELMID: string;
begin
  Result := 'M'; // MOST protocol identifier
end;

//------------------------------------------------------------------------------
// BUILD CONTROL MESSAGE
//------------------------------------------------------------------------------
function TMOSTProtocol.BuildControlMessage(const Msg: TMOSTControlMessage): TBytes;
var
  Len: Integer;
  Idx: Integer;
begin
  // MOST control message format:
  // [TargetAddr_H] [TargetAddr_L] [SourceAddr_H] [SourceAddr_L]
  // [FBlockID] [InstanceID] [FunctionID_H] [FunctionID_L]
  // [OpType] [Length] [Data...]
  
  Len := 10 + Length(Msg.Data);
  SetLength(Result, Len);
  
  // Addresses (12-bit, stored in 16-bit words)
  Result[0] := (Msg.TargetAddr shr 8) and $FF;
  Result[1] := Msg.TargetAddr and $FF;
  Result[2] := (Msg.SourceAddr shr 8) and $FF;
  Result[3] := Msg.SourceAddr and $FF;
  
  // Function catalog
  Result[4] := Msg.FBlockID;
  Result[5] := Msg.InstanceID;
  Result[6] := (Msg.FunctionID shr 8) and $FF;
  Result[7] := Msg.FunctionID and $FF;
  
  // Operation and data
  Result[8] := Msg.OpType;
  Result[9] := Length(Msg.Data);
  
  if Length(Msg.Data) > 0 then
    Move(Msg.Data[0], Result[10], Length(Msg.Data));
end;

//------------------------------------------------------------------------------
// PARSE CONTROL MESSAGE
//------------------------------------------------------------------------------
function TMOSTProtocol.ParseControlMessage(const RawData: TBytes): TMOSTControlMessage;
var
  DataLen: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  if Length(RawData) < 10 then
    Exit;
  
  Result.TargetAddr := (Word(RawData[0]) shl 8) or RawData[1];
  Result.SourceAddr := (Word(RawData[2]) shl 8) or RawData[3];
  Result.FBlockID := RawData[4];
  Result.InstanceID := RawData[5];
  Result.FunctionID := (Word(RawData[6]) shl 8) or RawData[7];
  Result.OpType := RawData[8];
  DataLen := RawData[9];
  
  if (DataLen > 0) and (Length(RawData) >= (10 + DataLen)) then
  begin
    SetLength(Result.Data, DataLen);
    Move(RawData[10], Result.Data[0], DataLen);
  end;
  
  Result.MsgType := mostControl;
end;

//------------------------------------------------------------------------------
// SEND CONTROL MESSAGE
//------------------------------------------------------------------------------
function TMOSTProtocol.SendControlMessage(TargetAddr: Word; FBlockID: Byte; 
  FunctionID: Word; OpType: Byte; const Data: TBytes): TBytes;
var
  Msg: TMOSTControlMessage;
begin
  Msg.SourceAddr := FNodeAddress;
  Msg.TargetAddr := TargetAddr;
  Msg.FBlockID := FBlockID;
  Msg.InstanceID := 0;
  Msg.FunctionID := FunctionID;
  Msg.OpType := OpType;
  Msg.Data := Data;
  Msg.MsgType := mostControl;
  
  Result := BuildControlMessage(Msg);
end;

//------------------------------------------------------------------------------
// GET PROPERTY
//------------------------------------------------------------------------------
function TMOSTProtocol.GetProperty(TargetAddr: Word; FBlockID: Byte; 
  FunctionID: Word): TBytes;
begin
  Result := SendControlMessage(TargetAddr, FBlockID, FunctionID, 
    Byte(mostOpGet), nil);
end;

//------------------------------------------------------------------------------
// SET PROPERTY
//------------------------------------------------------------------------------
function TMOSTProtocol.SetProperty(TargetAddr: Word; FBlockID: Byte; 
  FunctionID: Word; const Value: TBytes): TBytes;
begin
  Result := SendControlMessage(TargetAddr, FBlockID, FunctionID, 
    Byte(mostOpSet), Value);
end;

//------------------------------------------------------------------------------
// SEND DIAGNOSTIC REQUEST
//------------------------------------------------------------------------------
function TMOSTProtocol.SendDiagnosticRequest(ServiceID: Byte; const Parameters: TBytes): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1 + Length(Parameters));
  Data[0] := ServiceID;
  if Length(Parameters) > 0 then
    Move(Parameters[0], Data[1], Length(Parameters));
  
  Result := SendControlMessage($FFF, Byte(fbDiagnostics), $0100, 
    Byte(mostOpSet), Data);
end;

//------------------------------------------------------------------------------
// READ FAULT CODES
//------------------------------------------------------------------------------
function TMOSTProtocol.ReadFaultCodes: TBytes;
begin
  Result := SendDiagnosticRequest($19, nil); // UDS Read DTC Information
end;

//------------------------------------------------------------------------------
// CLEAR FAULT CODES
//------------------------------------------------------------------------------
function TMOSTProtocol.ClearFaultCodes: TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 3);
  Data[0] := $FF;
  Data[1] := $FF;
  Data[2] := $FF;
  Result := SendDiagnosticRequest($14, Data); // UDS Clear DTC
end;

//------------------------------------------------------------------------------
// READ ECU INFO
//------------------------------------------------------------------------------
function TMOSTProtocol.ReadECUInfo(InfoType: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := InfoType;
  Result := SendDiagnosticRequest($22, Data); // UDS Read Data By Identifier
end;

//------------------------------------------------------------------------------
// REQUEST STREAMING CHANNEL
//------------------------------------------------------------------------------
function TMOSTProtocol.RequestStreamingChannel(SourceAddr: Word; ChannelID: Byte;
  Bandwidth: Word): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 5);
  Data[0] := (SourceAddr shr 8) and $FF;
  Data[1] := SourceAddr and $FF;
  Data[2] := ChannelID;
  Data[3] := (Bandwidth shr 8) and $FF;
  Data[4] := Bandwidth and $FF;
  
  Result := SendControlMessage(Byte(fbNetworkMaster), 0, $0200, 
    Byte(mostOpSet), Data);
end;

//------------------------------------------------------------------------------
// RELEASE STREAMING CHANNEL
//------------------------------------------------------------------------------
function TMOSTProtocol.ReleaseStreamingChannel(ChannelID: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := ChannelID;
  
  Result := SendControlMessage(Byte(fbNetworkMaster), 0, $0201, 
    Byte(mostOpSet), Data);
end;

//------------------------------------------------------------------------------
// CONFIGURE NETWORK
//------------------------------------------------------------------------------
function TMOSTProtocol.ConfigureNetwork: TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 4);
  Data[0] := Byte(FVersion);
  Data[1] := (FNodeAddress shr 8) and $FF;
  Data[2] := FNodeAddress and $FF;
  Data[3] := $01; // Network configuration flags
  
  Result := SendControlMessage(Byte(fbNetworkMaster), 0, $0100, 
    Byte(mostOpSet), Data);
end;

end.
