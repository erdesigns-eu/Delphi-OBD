//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.UDS.pas
// CONTENTS       : UDS (ISO 14229) Protocol Support
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.UDS;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  OBD.Protocol, OBD.Protocol.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   UDS Service IDs (ISO 14229-1)
  /// </summary>
  TUDSServiceID = (
    udsNone                              = $00,
    udsDiagnosticSessionControl          = $10,
    udsECUReset                          = $11,
    udsSecurityAccess                    = $27,
    udsCommunicationControl              = $28,
    udsTesterPresent                     = $3E,
    udsAccessTimingParameter             = $83,
    udsSecuredDataTransmission           = $84,
    udsControlDTCSetting                 = $85,
    udsResponseOnEvent                   = $86,
    udsLinkControl                       = $87,
    udsReadDataByIdentifier              = $22,
    udsReadMemoryByAddress               = $23,
    udsReadScalingDataByIdentifier       = $24,
    udsReadDataByPeriodicIdentifier      = $2A,
    udsDynamicallyDefineDataIdentifier   = $2C,
    udsWriteDataByIdentifier             = $2E,
    udsWriteMemoryByAddress              = $3D,
    udsClearDiagnosticInformation        = $14,
    udsReadDTCInformation                = $19,
    udsInputOutputControlByIdentifier    = $2F,
    udsRoutineControl                    = $31,
    udsRequestDownload                   = $34,
    udsRequestUpload                     = $35,
    udsTransferData                      = $36,
    udsRequestTransferExit               = $37
  );

  /// <summary>
  ///   UDS Diagnostic Session Types
  /// </summary>
  TUDSSessionType = (
    udsDefaultSession         = $01,
    udsProgrammingSession     = $02,
    udsExtendedDiagnostic     = $03,
    udsSafetySystemSession    = $04
  );

  /// <summary>
  ///   UDS ECU Reset Types
  /// </summary>
  TUDSResetType = (
    udsHardReset              = $01,
    udsKeyOffOnReset          = $02,
    udsSoftReset              = $03,
    udsEnableRapidPowerShutdown = $04,
    udsDisableRapidPowerShutdown = $05
  );

  /// <summary>
  ///   UDS Response Codes
  /// </summary>
  TUDSResponseCode = (
    udsPositiveResponse                = $00,
    udsGeneralReject                   = $10,
    udsServiceNotSupported             = $11,
    udsSubFunctionNotSupported         = $12,
    udsIncorrectMessageLength          = $13,
    udsResponseTooLong                 = $14,
    udsBusyRepeatRequest               = $21,
    udsConditionsNotCorrect            = $22,
    udsRequestSequenceError            = $24,
    udsNoResponseFromSubnetComponent   = $25,
    udsFailurePreventsExecution        = $26,
    udsRequestOutOfRange               = $31,
    udsSecurityAccessDenied            = $33,
    udsInvalidKey                      = $35,
    udsExceededNumberOfAttempts        = $36,
    udsRequiredTimeDelayNotExpired     = $37,
    udsUploadDownloadNotAccepted       = $70,
    udsTransferDataSuspended           = $71,
    udsGeneralProgrammingFailure       = $72,
    udsWrongBlockSequenceCounter       = $73,
    udsResponsePending                 = $78,
    udsSubFunctionNotSupportedInActiveSession = $7E,
    udsServiceNotSupportedInActiveSession = $7F
  );

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   UDS Protocol (ISO 14229)
  ///   Unified Diagnostic Services with security and authentication
  /// </summary>
  TUDSProtocol = class(TOBDProtocol)
  private
    FCurrentSession: TUDSSessionType;
    FSecurityLevel: Byte;
    FSecurityUnlocked: Boolean;
    
    /// <summary>
    ///   Build UDS request message
    /// </summary>
    function BuildRequest(ServiceID: Byte; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Parse UDS response
    /// </summary>
    function ParseResponse(const Response: TBytes; var ServiceID: Byte; var Data: TBytes): TUDSResponseCode;
  protected
    function GetName: string; override;
    function GetDisplayName: string; override;
    function GetELMID: string; override;
  public
    constructor Create;
    
    /// <summary>
    ///   Diagnostic Session Control ($10)
    /// </summary>
    function DiagnosticSessionControl(SessionType: TUDSSessionType): TBytes;
    
    /// <summary>
    ///   ECU Reset ($11)
    /// </summary>
    function ECUReset(ResetType: TUDSResetType): TBytes;
    
    /// <summary>
    ///   Security Access - Request Seed ($27, sub $01/$03/$05...)
    /// </summary>
    function SecurityAccessRequestSeed(Level: Byte = $01): TBytes;
    
    /// <summary>
    ///   Security Access - Send Key ($27, sub $02/$04/$06...)
    /// </summary>
    function SecurityAccessSendKey(Level: Byte; const Key: TBytes): TBytes;
    
    /// <summary>
    ///   Tester Present ($3E)
    /// </summary>
    function TesterPresent(SuppressResponse: Boolean = False): TBytes;
    
    /// <summary>
    ///   Read Data By Identifier ($22)
    /// </summary>
    function ReadDataByIdentifier(const Identifiers: array of Word): TBytes;
    
    /// <summary>
    ///   Write Data By Identifier ($2E)
    /// </summary>
    function WriteDataByIdentifier(Identifier: Word; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Read Memory By Address ($23)
    /// </summary>
    function ReadMemoryByAddress(Address: Cardinal; Size: Word): TBytes;
    
    /// <summary>
    ///   Write Memory By Address ($3D)
    /// </summary>
    function WriteMemoryByAddress(Address: Cardinal; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Clear Diagnostic Information ($14)
    /// </summary>
    function ClearDiagnosticInformation(GroupOfDTC: Cardinal = $FFFFFF): TBytes;
    
    /// <summary>
    ///   Read DTC Information ($19)
    /// </summary>
    function ReadDTCInformation(SubFunction: Byte; const Data: TBytes = nil): TBytes;
    
    /// <summary>
    ///   Routine Control ($31)
    /// </summary>
    function RoutineControl(RoutineType: Byte; Identifier: Word; const Data: TBytes = nil): TBytes;
    
    /// <summary>
    ///   Request Download ($34) - for ECU flashing
    /// </summary>
    function RequestDownload(MemoryAddress: Cardinal; MemorySize: Cardinal; 
      DataFormatIdentifier: Byte = $00): TBytes;
    
    /// <summary>
    ///   Request Upload ($35)
    /// </summary>
    function RequestUpload(MemoryAddress: Cardinal; MemorySize: Cardinal;
      DataFormatIdentifier: Byte = $00): TBytes;
    
    /// <summary>
    ///   Transfer Data ($36)
    /// </summary>
    function TransferData(BlockSequenceCounter: Byte; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Request Transfer Exit ($37)
    /// </summary>
    function RequestTransferExit(const Data: TBytes = nil): TBytes;
    
    /// <summary>
    ///   Control DTC Setting ($85)
    /// </summary>
    function ControlDTCSetting(SettingType: Byte): TBytes;
    
    /// <summary>
    ///   Current diagnostic session
    /// </summary>
    property CurrentSession: TUDSSessionType read FCurrentSession;
    
    /// <summary>
    ///   Security level
    /// </summary>
    property SecurityLevel: Byte read FSecurityLevel;
    
    /// <summary>
    ///   Security unlocked status
    /// </summary>
    property SecurityUnlocked: Boolean read FSecurityUnlocked;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TUDSProtocol.Create;
begin
  inherited Create;
  FCurrentSession := udsDefaultSession;
  FSecurityLevel := 0;
  FSecurityUnlocked := False;
end;

//------------------------------------------------------------------------------
// GET NAME
//------------------------------------------------------------------------------
function TUDSProtocol.GetName: string;
begin
  Result := 'UDS (ISO 14229)';
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TUDSProtocol.GetDisplayName: string;
begin
  Result := 'Unified Diagnostic Services (ISO 14229-1)';
end;

//------------------------------------------------------------------------------
// GET ELM ID
//------------------------------------------------------------------------------
function TUDSProtocol.GetELMID: string;
begin
  Result := 'C'; // CAN 11-bit, 500 kbaud (typical for UDS)
end;

//------------------------------------------------------------------------------
// BUILD REQUEST
//------------------------------------------------------------------------------
function TUDSProtocol.BuildRequest(ServiceID: Byte; const Data: TBytes): TBytes;
var
  Len: Integer;
begin
  Len := 1 + Length(Data);
  SetLength(Result, Len);
  Result[0] := ServiceID;
  if Length(Data) > 0 then
    Move(Data[0], Result[1], Length(Data));
end;

//------------------------------------------------------------------------------
// PARSE RESPONSE
//------------------------------------------------------------------------------
function TUDSProtocol.ParseResponse(const Response: TBytes; var ServiceID: Byte; var Data: TBytes): TUDSResponseCode;
begin
  Result := udsGeneralReject;
  ServiceID := 0;
  SetLength(Data, 0);
  
  if Length(Response) < 2 then
    Exit;
  
  // Check for negative response
  if Response[0] = $7F then
  begin
    if Length(Response) >= 3 then
    begin
      ServiceID := Response[1];
      Result := TUDSResponseCode(Response[2]);
    end;
    Exit;
  end;
  
  // Positive response
  ServiceID := Response[0] - $40; // Remove positive response offset
  Result := udsPositiveResponse;
  
  if Length(Response) > 1 then
  begin
    SetLength(Data, Length(Response) - 1);
    Move(Response[1], Data[0], Length(Data));
  end;
end;

//------------------------------------------------------------------------------
// DIAGNOSTIC SESSION CONTROL
//------------------------------------------------------------------------------
function TUDSProtocol.DiagnosticSessionControl(SessionType: TUDSSessionType): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := Byte(SessionType);
  Result := BuildRequest(Byte(udsDiagnosticSessionControl), Data);
  FCurrentSession := SessionType;
end;

//------------------------------------------------------------------------------
// ECU RESET
//------------------------------------------------------------------------------
function TUDSProtocol.ECUReset(ResetType: TUDSResetType): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := Byte(ResetType);
  Result := BuildRequest(Byte(udsECUReset), Data);
end;

//------------------------------------------------------------------------------
// SECURITY ACCESS REQUEST SEED
//------------------------------------------------------------------------------
function TUDSProtocol.SecurityAccessRequestSeed(Level: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := Level; // Odd numbers for seed request (01, 03, 05, etc.)
  Result := BuildRequest(Byte(udsSecurityAccess), Data);
  FSecurityLevel := Level;
end;

//------------------------------------------------------------------------------
// SECURITY ACCESS SEND KEY
//------------------------------------------------------------------------------
function TUDSProtocol.SecurityAccessSendKey(Level: Byte; const Key: TBytes): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1 + Length(Key));
  Data[0] := Level; // Even numbers for key send (02, 04, 06, etc.)
  if Length(Key) > 0 then
    Move(Key[0], Data[1], Length(Key));
  Result := BuildRequest(Byte(udsSecurityAccess), Data);
end;

//------------------------------------------------------------------------------
// TESTER PRESENT
//------------------------------------------------------------------------------
function TUDSProtocol.TesterPresent(SuppressResponse: Boolean): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  if SuppressResponse then
    Data[0] := $80  // Suppress positive response
  else
    Data[0] := $00;
  Result := BuildRequest(Byte(udsTesterPresent), Data);
end;

//------------------------------------------------------------------------------
// READ DATA BY IDENTIFIER
//------------------------------------------------------------------------------
function TUDSProtocol.ReadDataByIdentifier(const Identifiers: array of Word): TBytes;
var
  Data: TBytes;
  I: Integer;
begin
  SetLength(Data, Length(Identifiers) * 2);
  for I := 0 to High(Identifiers) do
  begin
    Data[I * 2] := Hi(Identifiers[I]);
    Data[I * 2 + 1] := Lo(Identifiers[I]);
  end;
  Result := BuildRequest(Byte(udsReadDataByIdentifier), Data);
end;

//------------------------------------------------------------------------------
// WRITE DATA BY IDENTIFIER
//------------------------------------------------------------------------------
function TUDSProtocol.WriteDataByIdentifier(Identifier: Word; const Data: TBytes): TBytes;
var
  ReqData: TBytes;
begin
  SetLength(ReqData, 2 + Length(Data));
  ReqData[0] := Hi(Identifier);
  ReqData[1] := Lo(Identifier);
  if Length(Data) > 0 then
    Move(Data[0], ReqData[2], Length(Data));
  Result := BuildRequest(Byte(udsWriteDataByIdentifier), ReqData);
end;

//------------------------------------------------------------------------------
// READ MEMORY BY ADDRESS
//------------------------------------------------------------------------------
function TUDSProtocol.ReadMemoryByAddress(Address: Cardinal; Size: Word): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 7); // addressAndLengthFormatIdentifier + 4 bytes address + 2 bytes size
  Data[0] := $44; // 4-byte address, 4-byte length
  Data[1] := (Address shr 24) and $FF;
  Data[2] := (Address shr 16) and $FF;
  Data[3] := (Address shr 8) and $FF;
  Data[4] := Address and $FF;
  Data[5] := Hi(Size);
  Data[6] := Lo(Size);
  Result := BuildRequest(Byte(udsReadMemoryByAddress), Data);
end;

//------------------------------------------------------------------------------
// WRITE MEMORY BY ADDRESS
//------------------------------------------------------------------------------
function TUDSProtocol.WriteMemoryByAddress(Address: Cardinal; const Data: TBytes): TBytes;
var
  ReqData: TBytes;
  Size: Word;
begin
  Size := Length(Data);
  SetLength(ReqData, 7 + Size);
  ReqData[0] := $44; // 4-byte address, 4-byte length
  ReqData[1] := (Address shr 24) and $FF;
  ReqData[2] := (Address shr 16) and $FF;
  ReqData[3] := (Address shr 8) and $FF;
  ReqData[4] := Address and $FF;
  ReqData[5] := Hi(Size);
  ReqData[6] := Lo(Size);
  if Size > 0 then
    Move(Data[0], ReqData[7], Size);
  Result := BuildRequest(Byte(udsWriteMemoryByAddress), ReqData);
end;

//------------------------------------------------------------------------------
// CLEAR DIAGNOSTIC INFORMATION
//------------------------------------------------------------------------------
function TUDSProtocol.ClearDiagnosticInformation(GroupOfDTC: Cardinal): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 3);
  Data[0] := (GroupOfDTC shr 16) and $FF;
  Data[1] := (GroupOfDTC shr 8) and $FF;
  Data[2] := GroupOfDTC and $FF;
  Result := BuildRequest(Byte(udsClearDiagnosticInformation), Data);
end;

//------------------------------------------------------------------------------
// READ DTC INFORMATION
//------------------------------------------------------------------------------
function TUDSProtocol.ReadDTCInformation(SubFunction: Byte; const Data: TBytes): TBytes;
var
  ReqData: TBytes;
begin
  SetLength(ReqData, 1 + Length(Data));
  ReqData[0] := SubFunction;
  if Length(Data) > 0 then
    Move(Data[0], ReqData[1], Length(Data));
  Result := BuildRequest(Byte(udsReadDTCInformation), ReqData);
end;

//------------------------------------------------------------------------------
// ROUTINE CONTROL
//------------------------------------------------------------------------------
function TUDSProtocol.RoutineControl(RoutineType: Byte; Identifier: Word; const Data: TBytes): TBytes;
var
  ReqData: TBytes;
begin
  SetLength(ReqData, 3 + Length(Data));
  ReqData[0] := RoutineType; // $01=Start, $02=Stop, $03=Request Results
  ReqData[1] := Hi(Identifier);
  ReqData[2] := Lo(Identifier);
  if Length(Data) > 0 then
    Move(Data[0], ReqData[3], Length(Data));
  Result := BuildRequest(Byte(udsRoutineControl), ReqData);
end;

//------------------------------------------------------------------------------
// REQUEST DOWNLOAD
//------------------------------------------------------------------------------
function TUDSProtocol.RequestDownload(MemoryAddress: Cardinal; MemorySize: Cardinal; 
  DataFormatIdentifier: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 10);
  Data[0] := DataFormatIdentifier;
  Data[1] := $44; // addressAndLengthFormatIdentifier: 4 bytes each
  Data[2] := (MemoryAddress shr 24) and $FF;
  Data[3] := (MemoryAddress shr 16) and $FF;
  Data[4] := (MemoryAddress shr 8) and $FF;
  Data[5] := MemoryAddress and $FF;
  Data[6] := (MemorySize shr 24) and $FF;
  Data[7] := (MemorySize shr 16) and $FF;
  Data[8] := (MemorySize shr 8) and $FF;
  Data[9] := MemorySize and $FF;
  Result := BuildRequest(Byte(udsRequestDownload), Data);
end;

//------------------------------------------------------------------------------
// REQUEST UPLOAD
//------------------------------------------------------------------------------
function TUDSProtocol.RequestUpload(MemoryAddress: Cardinal; MemorySize: Cardinal;
  DataFormatIdentifier: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 10);
  Data[0] := DataFormatIdentifier;
  Data[1] := $44; // addressAndLengthFormatIdentifier: 4 bytes each
  Data[2] := (MemoryAddress shr 24) and $FF;
  Data[3] := (MemoryAddress shr 16) and $FF;
  Data[4] := (MemoryAddress shr 8) and $FF;
  Data[5] := MemoryAddress and $FF;
  Data[6] := (MemorySize shr 24) and $FF;
  Data[7] := (MemorySize shr 16) and $FF;
  Data[8] := (MemorySize shr 8) and $FF;
  Data[9] := MemorySize and $FF;
  Result := BuildRequest(Byte(udsRequestUpload), Data);
end;

//------------------------------------------------------------------------------
// TRANSFER DATA
//------------------------------------------------------------------------------
function TUDSProtocol.TransferData(BlockSequenceCounter: Byte; const Data: TBytes): TBytes;
var
  ReqData: TBytes;
begin
  SetLength(ReqData, 1 + Length(Data));
  ReqData[0] := BlockSequenceCounter;
  if Length(Data) > 0 then
    Move(Data[0], ReqData[1], Length(Data));
  Result := BuildRequest(Byte(udsTransferData), ReqData);
end;

//------------------------------------------------------------------------------
// REQUEST TRANSFER EXIT
//------------------------------------------------------------------------------
function TUDSProtocol.RequestTransferExit(const Data: TBytes): TBytes;
begin
  Result := BuildRequest(Byte(udsRequestTransferExit), Data);
end;

//------------------------------------------------------------------------------
// CONTROL DTC SETTING
//------------------------------------------------------------------------------
function TUDSProtocol.ControlDTCSetting(SettingType: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := SettingType; // $01=On, $02=Off
  Result := BuildRequest(Byte(udsControlDTCSetting), Data);
end;

end.
