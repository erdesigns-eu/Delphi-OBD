//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.J1939.pas
// CONTENTS       : SAE J1939 Protocol Support for Heavy-Duty Vehicles
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.Protocol.J1939;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.Logger;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   J1939 Priority levels (0 = highest, 7 = lowest)
  /// </summary>
  TJ1939Priority = 0..7;

  /// <summary>
  ///   J1939 PGN (Parameter Group Number) - 18-bit identifier
  /// </summary>
  TJ1939PGN = record
    Value: Cardinal;
    function ToString: string;
    class function FromValue(APgn: Cardinal): TJ1939PGN; static;
  end;

  /// <summary>
  ///   J1939 Source Address (0-253, 254 = null, 255 = global)
  /// </summary>
  TJ1939Address = 0..255;

  /// <summary>
  ///   J1939 Message structure
  /// </summary>
  TJ1939Message = record
    /// <summary>
    ///   Message priority (0-7)
    /// </summary>
    Priority: TJ1939Priority;
    /// <summary>
    ///   Parameter Group Number
    /// </summary>
    PGN: TJ1939PGN;
    /// <summary>
    ///   Source address
    /// </summary>
    SourceAddress: TJ1939Address;
    /// <summary>
    ///   Destination address (255 for broadcast)
    /// </summary>
    DestinationAddress: TJ1939Address;
    /// <summary>
    ///   Message data bytes
    /// </summary>
    Data: TBytes;
    /// <summary>
    ///   Format message as string for logging
    /// </summary>
    function ToString: string;
  end;

//------------------------------------------------------------------------------
// COMMON J1939 PGNS
//------------------------------------------------------------------------------
const
  // Diagnostic PGNs
  J1939_PGN_DM1_ACTIVE_DTCS          = $FECA; // 65226 - Active diagnostic trouble codes
  J1939_PGN_DM2_PREVIOUSLY_ACTIVE    = $FECB; // 65227 - Previously active DTCs
  J1939_PGN_DM3_CLEAR_DTCS           = $FECC; // 65228 - Clear/reset DTCs
  J1939_PGN_DM4_FREEZE_FRAME         = $FECD; // 65229 - Freeze frame data
  J1939_PGN_DM5_DIAGNOSTIC_READINESS = $FECE; // 65230 - Diagnostic readiness
  
  // Engine PGNs
  J1939_PGN_ENGINE_TEMP              = $FEEE; // 65262 - Engine temperature
  J1939_PGN_ENGINE_FLUID_LEVEL       = $FEEF; // 65263 - Engine fluid level/pressure
  J1939_PGN_VEHICLE_ELECTRICAL_POWER = $FEF2; // 65266 - Electrical power
  J1939_PGN_ENGINE_HOURS             = $FEE5; // 65253 - Engine hours
  J1939_PGN_ENGINE_RPM_SPEED         = $F004; // 61444 - Engine speed and RPM
  
  // Vehicle PGNs
  J1939_PGN_VEHICLE_DISTANCE         = $FEC1; // 65217 - High resolution vehicle distance
  J1939_PGN_VEHICLE_SPEED            = $FEF1; // 65265 - Cruise control/vehicle speed
  J1939_PGN_FUEL_ECONOMY             = $FEF2; // 65266 - Fuel economy

  // Address claims
  J1939_PGN_ADDRESS_CLAIMED          = $EE00; // 60928 - Address claim message
  J1939_PGN_REQUEST                  = $EA00; // 59904 - Request PGN

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   J1939 Protocol Handler
  /// </summary>
  TOBDProtocolJ1939 = class
  private
    /// <summary>
    ///   Our device address on the J1939 bus
    /// </summary>
    FSourceAddress: TJ1939Address;
    /// <summary>
    ///   Active PGN filters
    /// </summary>
    FPGNFilters: TList<Cardinal>;
    /// <summary>
    ///   Transport protocol buffer for multi-frame messages
    /// </summary>
    FTransportBuffer: TDictionary<TJ1939Address, TBytes>;

    /// <summary>
    ///   Parse CAN ID to extract J1939 components
    /// </summary>
    procedure ParseCANID(const CANID: Cardinal; out Priority: TJ1939Priority;
      out PGN: TJ1939PGN; out SourceAddr: TJ1939Address);
    /// <summary>
    ///   Build CAN ID from J1939 components
    /// </summary>
    function BuildCANID(const Priority: TJ1939Priority; const PGN: TJ1939PGN;
      const SourceAddr: TJ1939Address): Cardinal;

  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(ASourceAddress: TJ1939Address = 249);
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Add PGN filter
    /// </summary>
    procedure AddPGNFilter(const PGN: Cardinal);
    /// <summary>
    ///   Remove PGN filter
    /// </summary>
    procedure RemovePGNFilter(const PGN: Cardinal);
    /// <summary>
    ///   Clear all PGN filters
    /// </summary>
    procedure ClearPGNFilters;

    /// <summary>
    ///   Request a PGN from target address
    /// </summary>
    function RequestPGN(const PGN: TJ1939PGN; const DestAddr: TJ1939Address = 255): TJ1939Message;
    /// <summary>
    ///   Send J1939 message
    /// </summary>
    function SendMessage(const Msg: TJ1939Message): Boolean;
    /// <summary>
    ///   Parse received CAN frame as J1939 message
    /// </summary>
    function ParseMessage(const CANID: Cardinal; const Data: TBytes): TJ1939Message;

    /// <summary>
    ///   Request active DTCs (DM1)
    /// </summary>
    function RequestActiveDTCs: TJ1939Message;
    /// <summary>
    ///   Request freeze frame data (DM4)
    /// </summary>
    function RequestFreezeFrame: TJ1939Message;
    /// <summary>
    ///   Clear DTCs (DM3)
    /// </summary>
    function ClearDTCs: TJ1939Message;

    /// <summary>
    ///   Source address on J1939 bus
    /// </summary>
    property SourceAddress: TJ1939Address read FSourceAddress write FSourceAddress;
  end;

implementation

//------------------------------------------------------------------------------
// TJ1939PGN - TO STRING
//------------------------------------------------------------------------------
function TJ1939PGN.ToString: string;
begin
  Result := Format('$%X (%d)', [Value, Value]);
end;

//------------------------------------------------------------------------------
// TJ1939PGN - FROM VALUE
//------------------------------------------------------------------------------
class function TJ1939PGN.FromValue(APgn: Cardinal): TJ1939PGN;
begin
  Result.Value := APgn and $3FFFF; // 18-bit PGN
end;

//------------------------------------------------------------------------------
// TJ1939Message - TO STRING
//------------------------------------------------------------------------------
function TJ1939Message.ToString: string;
begin
  Result := Format('Priority=%d PGN=%s SA=%d DA=%d DataLen=%d',
    [Priority, PGN.ToString, SourceAddress, DestinationAddress, Length(Data)]);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDProtocolJ1939.Create(ASourceAddress: TJ1939Address);
begin
  inherited Create;
  FSourceAddress := ASourceAddress;
  FPGNFilters := TList<Cardinal>.Create;
  FTransportBuffer := TDictionary<TJ1939Address, TBytes>.Create;

  if Assigned(GlobalLogger) then
    GlobalLogger.Info('J1939 protocol initialized with source address %d', [ASourceAddress]);
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDProtocolJ1939.Destroy;
begin
  FPGNFilters.Free;
  FTransportBuffer.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// PARSE CAN ID
//------------------------------------------------------------------------------
procedure TOBDProtocolJ1939.ParseCANID(const CANID: Cardinal; out Priority: TJ1939Priority;
  out PGN: TJ1939PGN; out SourceAddr: TJ1939Address);
begin
  // J1939 29-bit CAN ID structure:
  // Bits 26-28: Priority (3 bits)
  // Bits 8-25:  PGN (18 bits)
  // Bits 0-7:   Source Address (8 bits)
  
  Priority := (CANID shr 26) and $07;
  PGN.Value := (CANID shr 8) and $3FFFF;
  SourceAddr := CANID and $FF;
end;

//------------------------------------------------------------------------------
// BUILD CAN ID
//------------------------------------------------------------------------------
function TOBDProtocolJ1939.BuildCANID(const Priority: TJ1939Priority;
  const PGN: TJ1939PGN; const SourceAddr: TJ1939Address): Cardinal;
begin
  Result := ((Priority and $07) shl 26) or
            ((PGN.Value and $3FFFF) shl 8) or
            (SourceAddr and $FF);
end;

//------------------------------------------------------------------------------
// ADD PGN FILTER
//------------------------------------------------------------------------------
procedure TOBDProtocolJ1939.AddPGNFilter(const PGN: Cardinal);
begin
  if not FPGNFilters.Contains(PGN) then
  begin
    FPGNFilters.Add(PGN);
    if Assigned(GlobalLogger) then
      GlobalLogger.Debug('Added J1939 PGN filter: $%X', [PGN]);
  end;
end;

//------------------------------------------------------------------------------
// REMOVE PGN FILTER
//------------------------------------------------------------------------------
procedure TOBDProtocolJ1939.RemovePGNFilter(const PGN: Cardinal);
begin
  FPGNFilters.Remove(PGN);
end;

//------------------------------------------------------------------------------
// CLEAR PGN FILTERS
//------------------------------------------------------------------------------
procedure TOBDProtocolJ1939.ClearPGNFilters;
begin
  FPGNFilters.Clear;
  if Assigned(GlobalLogger) then
    GlobalLogger.Debug('Cleared all J1939 PGN filters');
end;

//------------------------------------------------------------------------------
// REQUEST PGN
//------------------------------------------------------------------------------
function TOBDProtocolJ1939.RequestPGN(const PGN: TJ1939PGN;
  const DestAddr: TJ1939Address): TJ1939Message;
begin
  Result.Priority := 6; // Standard request priority
  Result.PGN := TJ1939PGN.FromValue(J1939_PGN_REQUEST);
  Result.SourceAddress := FSourceAddress;
  Result.DestinationAddress := DestAddr;
  
  // Request data: PGN in little-endian format
  SetLength(Result.Data, 3);
  Result.Data[0] := PGN.Value and $FF;
  Result.Data[1] := (PGN.Value shr 8) and $FF;
  Result.Data[2] := (PGN.Value shr 16) and $FF;

  if Assigned(GlobalLogger) then
    GlobalLogger.Debug('Requesting J1939 PGN %s from address %d', [PGN.ToString, DestAddr]);
end;

//------------------------------------------------------------------------------
// SEND MESSAGE
//------------------------------------------------------------------------------
function TOBDProtocolJ1939.SendMessage(const Msg: TJ1939Message): Boolean;
var
  CANID: Cardinal;
begin
  Result := False;
  
  CANID := BuildCANID(Msg.Priority, Msg.PGN, Msg.SourceAddress);
  
  if Assigned(GlobalLogger) then
    GlobalLogger.Debug('Sending J1939 message: %s CANID=$%X', [Msg.ToString, CANID]);
  
  // Actual sending would be done through connection layer
  Result := True;
end;

//------------------------------------------------------------------------------
// PARSE MESSAGE
//------------------------------------------------------------------------------
function TOBDProtocolJ1939.ParseMessage(const CANID: Cardinal; const Data: TBytes): TJ1939Message;
begin
  ParseCANID(CANID, Result.Priority, Result.PGN, Result.SourceAddress);
  Result.DestinationAddress := 255; // Will be parsed from PGN if applicable
  Result.Data := Copy(Data, 0, Length(Data));

  if Assigned(GlobalLogger) then
    GlobalLogger.Debug('Parsed J1939 message: %s', [Result.ToString]);
end;

//------------------------------------------------------------------------------
// REQUEST ACTIVE DTCS
//------------------------------------------------------------------------------
function TOBDProtocolJ1939.RequestActiveDTCs: TJ1939Message;
begin
  Result := RequestPGN(TJ1939PGN.FromValue(J1939_PGN_DM1_ACTIVE_DTCS), 255);
end;

//------------------------------------------------------------------------------
// REQUEST FREEZE FRAME
//------------------------------------------------------------------------------
function TOBDProtocolJ1939.RequestFreezeFrame: TJ1939Message;
begin
  Result := RequestPGN(TJ1939PGN.FromValue(J1939_PGN_DM4_FREEZE_FRAME), 255);
end;

//------------------------------------------------------------------------------
// CLEAR DTCS
//------------------------------------------------------------------------------
function TOBDProtocolJ1939.ClearDTCs: TJ1939Message;
begin
  Result.Priority := 6;
  Result.PGN := TJ1939PGN.FromValue(J1939_PGN_DM3_CLEAR_DTCS);
  Result.SourceAddress := FSourceAddress;
  Result.DestinationAddress := 255; // Broadcast
  SetLength(Result.Data, 0); // No data required

  if Assigned(GlobalLogger) then
    GlobalLogger.Info('Sending J1939 clear DTCs command (DM3)');
end;

end.
