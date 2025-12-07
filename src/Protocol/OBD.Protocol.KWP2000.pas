//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.KWP2000.pas
// CONTENTS       : KWP2000 (ISO 14230) Extended Protocol Support
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.KWP2000;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  OBD.Protocol, OBD.Protocol.Types, OBD.Protocol.Legacy;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   KWP2000 Service IDs (ISO 14230-3)
  /// </summary>
  TKWP2000ServiceID = (
    kwpStartDiagnosticSession      = $10,
    kwpECUReset                    = $11,
    kwpReadFreezeFrameData         = $12,
    kwpReadDiagnosticTroubleCodes  = $13,
    kwpClearDiagnosticInformation  = $14,
    kwpReadStatusOfDTC             = $17,
    kwpReadDTCByStatus             = $18,
    kwpReadECUIdentification       = $1A,
    kwpStopDiagnosticSession       = $20,
    kwpReadDataByLocalIdentifier   = $21,
    kwpReadDataByIdentifier        = $22,
    kwpReadMemoryByAddress         = $23,
    kwpSetDataRates                = $26,
    kwpSecurityAccess              = $27,
    kwpDynamicallyDefineLocalID    = $2C,
    kwpWriteDataByIdentifier       = $2E,
    kwpInputOutputControlByID      = $30,
    kwpStartRoutineByLocalID       = $31,
    kwpStopRoutineByLocalID        = $32,
    kwpRequestRoutineResultsByID   = $33,
    kwpRequestDownload              = $34,
    kwpRequestUpload                = $35,
    kwpTransferData                 = $36,
    kwpRequestTransferExit          = $37,
    kwpWriteDataByLocalIdentifier   = $3B,
    kwpWriteMemoryByAddress         = $3D,
    kwpTesterPresent                = $3E
  );

  /// <summary>
  ///   KWP2000 Diagnostic Session Types
  /// </summary>
  TKWP2000SessionType = (
    kwpDefaultSession         = $81,
    kwpProgrammingSession     = $85,
    kwpExtendedSession        = $87
  );

  /// <summary>
  ///   KWP2000 Security Access Levels
  /// </summary>
  TKWP2000SecurityLevel = (
    kwpSecuritySeed           = $01,  // Request seed
    kwpSecurityKey            = $02   // Send key
  );

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Enhanced KWP2000 Protocol (ISO 14230)
  ///   Supports full diagnostic services, security access, and ECU flashing
  /// </summary>
  TKWP2000Protocol = class(TISO_14230_4_FAST_OBDProtocol)
  private
    FCurrentSession: TKWP2000SessionType;
    FSecurityUnlocked: Boolean;
    
    /// <summary>
    ///   Calculate checksum for KWP2000 message
    /// </summary>
    function CalculateChecksum(const Data: TBytes): Byte;
    
    /// <summary>
    ///   Build KWP2000 message with header and checksum
    /// </summary>
    function BuildMessage(ServiceID: Byte; const Data: TBytes): TBytes;
  public
    constructor Create;
    
    /// <summary>
    ///   Start diagnostic session
    /// </summary>
    function StartDiagnosticSession(SessionType: TKWP2000SessionType): TBytes;
    
    /// <summary>
    ///   Request security seed (first step of security access)
    /// </summary>
    function RequestSecuritySeed(Level: Byte = $01): TBytes;
    
    /// <summary>
    ///   Send security key (second step of security access)
    /// </summary>
    function SendSecurityKey(const Key: TBytes): TBytes;
    
    /// <summary>
    ///   Read ECU identification
    /// </summary>
    function ReadECUIdentification(IdentType: Byte = $87): TBytes;
    
    /// <summary>
    ///   Read data by identifier
    /// </summary>
    function ReadDataByIdentifier(Identifier: Word): TBytes;
    
    /// <summary>
    ///   Write data by identifier
    /// </summary>
    function WriteDataByIdentifier(Identifier: Word; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Read diagnostic trouble codes
    /// </summary>
    function ReadDiagnosticTroubleCodes: TBytes;
    
    /// <summary>
    ///   Clear diagnostic information
    /// </summary>
    function ClearDiagnosticInformation(GroupOfDTC: Cardinal = $FFFFFF): TBytes;
    
    /// <summary>
    ///   ECU Reset
    /// </summary>
    function ECUReset(ResetType: Byte = $01): TBytes;
    
    /// <summary>
    ///   Tester Present (keep-alive)
    /// </summary>
    function TesterPresent: TBytes;
    
    /// <summary>
    ///   Request download (for ECU flashing)
    /// </summary>
    function RequestDownload(MemoryAddress: Cardinal; MemorySize: Cardinal): TBytes;
    
    /// <summary>
    ///   Transfer data (ECU flashing data block)
    /// </summary>
    function TransferData(BlockSequence: Byte; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Request transfer exit (complete ECU flashing)
    /// </summary>
    function RequestTransferExit: TBytes;
    
    /// <summary>
    ///   Current diagnostic session
    /// </summary>
    property CurrentSession: TKWP2000SessionType read FCurrentSession;
    
    /// <summary>
    ///   Security access status
    /// </summary>
    property SecurityUnlocked: Boolean read FSecurityUnlocked;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TKWP2000Protocol.Create;
begin
  inherited Create;
  FCurrentSession := kwpDefaultSession;
  FSecurityUnlocked := False;
end;

//------------------------------------------------------------------------------
// CALCULATE CHECKSUM
//------------------------------------------------------------------------------
function TKWP2000Protocol.CalculateChecksum(const Data: TBytes): Byte;
var
  I: Integer;
  Sum: Integer;
begin
  Sum := 0;
  for I := 0 to High(Data) do
    Sum := Sum + Data[I];
  Result := Byte(Sum and $FF);
end;

//------------------------------------------------------------------------------
// BUILD MESSAGE
//------------------------------------------------------------------------------
function TKWP2000Protocol.BuildMessage(ServiceID: Byte; const Data: TBytes): TBytes;
var
  Len, I: Integer;
begin
  Len := 3 + Length(Data) + 1; // Format + Target + Source + Data + Checksum
  SetLength(Result, Len);
  
  Result[0] := $80;  // Format byte (physical addressing)
  Result[1] := $10;  // Target address (ECU)
  Result[2] := $F1;  // Source address (Tester)
  Result[3] := ServiceID;
  
  if Length(Data) > 0 then
    for I := 0 to High(Data) do
      Result[4 + I] := Data[I];
  
  Result[High(Result)] := CalculateChecksum(Result);
end;

//------------------------------------------------------------------------------
// START DIAGNOSTIC SESSION
//------------------------------------------------------------------------------
function TKWP2000Protocol.StartDiagnosticSession(SessionType: TKWP2000SessionType): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := Byte(SessionType);
  Result := BuildMessage(Byte(kwpStartDiagnosticSession), Data);
  FCurrentSession := SessionType;
end;

//------------------------------------------------------------------------------
// REQUEST SECURITY SEED
//------------------------------------------------------------------------------
function TKWP2000Protocol.RequestSecuritySeed(Level: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := Level;
  Result := BuildMessage(Byte(kwpSecurityAccess), Data);
end;

//------------------------------------------------------------------------------
// SEND SECURITY KEY
//------------------------------------------------------------------------------
function TKWP2000Protocol.SendSecurityKey(const Key: TBytes): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1 + Length(Key));
  Data[0] := $02;  // Security key sub-function
  if Length(Key) > 0 then
    Move(Key[0], Data[1], Length(Key));
  Result := BuildMessage(Byte(kwpSecurityAccess), Data);
end;

//------------------------------------------------------------------------------
// READ ECU IDENTIFICATION
//------------------------------------------------------------------------------
function TKWP2000Protocol.ReadECUIdentification(IdentType: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := IdentType;
  Result := BuildMessage(Byte(kwpReadECUIdentification), Data);
end;

//------------------------------------------------------------------------------
// READ DATA BY IDENTIFIER
//------------------------------------------------------------------------------
function TKWP2000Protocol.ReadDataByIdentifier(Identifier: Word): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 2);
  Data[0] := Hi(Identifier);
  Data[1] := Lo(Identifier);
  Result := BuildMessage(Byte(kwpReadDataByIdentifier), Data);
end;

//------------------------------------------------------------------------------
// WRITE DATA BY IDENTIFIER
//------------------------------------------------------------------------------
function TKWP2000Protocol.WriteDataByIdentifier(Identifier: Word; const Data: TBytes): TBytes;
var
  MsgData: TBytes;
begin
  SetLength(MsgData, 2 + Length(Data));
  MsgData[0] := Hi(Identifier);
  MsgData[1] := Lo(Identifier);
  if Length(Data) > 0 then
    Move(Data[0], MsgData[2], Length(Data));
  Result := BuildMessage(Byte(kwpWriteDataByIdentifier), MsgData);
end;

//------------------------------------------------------------------------------
// READ DIAGNOSTIC TROUBLE CODES
//------------------------------------------------------------------------------
function TKWP2000Protocol.ReadDiagnosticTroubleCodes: TBytes;
begin
  Result := BuildMessage(Byte(kwpReadDiagnosticTroubleCodes), nil);
end;

//------------------------------------------------------------------------------
// CLEAR DIAGNOSTIC INFORMATION
//------------------------------------------------------------------------------
function TKWP2000Protocol.ClearDiagnosticInformation(GroupOfDTC: Cardinal): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 3);
  Data[0] := (GroupOfDTC shr 16) and $FF;
  Data[1] := (GroupOfDTC shr 8) and $FF;
  Data[2] := GroupOfDTC and $FF;
  Result := BuildMessage(Byte(kwpClearDiagnosticInformation), Data);
end;

//------------------------------------------------------------------------------
// ECU RESET
//------------------------------------------------------------------------------
function TKWP2000Protocol.ECUReset(ResetType: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := ResetType;
  Result := BuildMessage(Byte(kwpECUReset), Data);
end;

//------------------------------------------------------------------------------
// TESTER PRESENT
//------------------------------------------------------------------------------
function TKWP2000Protocol.TesterPresent: TBytes;
begin
  Result := BuildMessage(Byte(kwpTesterPresent), nil);
end;

//------------------------------------------------------------------------------
// REQUEST DOWNLOAD
//------------------------------------------------------------------------------
function TKWP2000Protocol.RequestDownload(MemoryAddress: Cardinal; MemorySize: Cardinal): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 8);
  Data[0] := (MemoryAddress shr 24) and $FF;
  Data[1] := (MemoryAddress shr 16) and $FF;
  Data[2] := (MemoryAddress shr 8) and $FF;
  Data[3] := MemoryAddress and $FF;
  Data[4] := (MemorySize shr 24) and $FF;
  Data[5] := (MemorySize shr 16) and $FF;
  Data[6] := (MemorySize shr 8) and $FF;
  Data[7] := MemorySize and $FF;
  Result := BuildMessage(Byte(kwpRequestDownload), Data);
end;

//------------------------------------------------------------------------------
// TRANSFER DATA
//------------------------------------------------------------------------------
function TKWP2000Protocol.TransferData(BlockSequence: Byte; const Data: TBytes): TBytes;
var
  MsgData: TBytes;
begin
  SetLength(MsgData, 1 + Length(Data));
  MsgData[0] := BlockSequence;
  if Length(Data) > 0 then
    Move(Data[0], MsgData[1], Length(Data));
  Result := BuildMessage(Byte(kwpTransferData), MsgData);
end;

//------------------------------------------------------------------------------
// REQUEST TRANSFER EXIT
//------------------------------------------------------------------------------
function TKWP2000Protocol.RequestTransferExit: TBytes;
begin
  Result := BuildMessage(Byte(kwpRequestTransferExit), nil);
end;

end.
