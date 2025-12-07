//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.LIN.pas
// CONTENTS       : LIN (Local Interconnect Network) Protocol Support
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.LIN;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  OBD.Protocol, OBD.Protocol.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   LIN Protocol Version
  /// </summary>
  TLINVersion = (
    linVersion1_3,  // LIN 1.3
    linVersion2_0,  // LIN 2.0
    linVersion2_1,  // LIN 2.1
    linVersion2_2   // LIN 2.2A
  );

  /// <summary>
  ///   LIN Frame Type
  /// </summary>
  TLINFrameType = (
    linUnconditional,    // Standard frame
    linEventTriggered,   // Event-triggered frame
    linSporadic,         // Sporadic frame
    linDiagnostic        // Diagnostic frame (Master/Slave)
  );

  /// <summary>
  ///   LIN Checksum Type
  /// </summary>
  TLINChecksumType = (
    linClassic,    // LIN 1.x classic checksum (data only)
    linEnhanced    // LIN 2.x enhanced checksum (ID + data)
  );

  /// <summary>
  ///   LIN Frame structure
  /// </summary>
  TLINFrame = record
    /// <summary>
    ///   Frame ID (0-63, protected identifier with parity)
    /// </summary>
    FrameID: Byte;
    /// <summary>
    ///   Frame data (1-8 bytes)
    /// </summary>
    Data: TBytes;
    /// <summary>
    ///   Checksum byte
    /// </summary>
    Checksum: Byte;
    /// <summary>
    ///   Checksum type
    /// </summary>
    ChecksumType: TLINChecksumType;
    /// <summary>
    ///   Frame type
    /// </summary>
    FrameType: TLINFrameType;
    /// <summary>
    ///   Is frame valid?
    /// </summary>
    Valid: Boolean;
  end;

  /// <summary>
  ///   LIN Diagnostic Frame IDs
  /// </summary>
  TLINDiagnosticFrameID = (
    linMasterRequest  = $3C,  // 60 - Master request frame
    linSlaveResponse  = $3D   // 61 - Slave response frame
  );

  /// <summary>
  ///   LIN Diagnostic Service IDs (subset of UDS/KWP2000)
  /// </summary>
  TLINDiagnosticService = (
    linReadByIdentifier          = $22,
    linWriteByIdentifier         = $2E,
    linSessionControl            = $10,
    linReadDataByIdentifier      = $22,
    linSecurityAccess            = $27,
    linCommunicationControl      = $28,
    linTesterPresent             = $3E,
    linControlDTCSetting         = $85,
    linReadDTCInformation        = $19,
    linClearDiagnosticInfo       = $14
  );

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   LIN Protocol Implementation
  ///   Local Interconnect Network for low-cost automotive applications
  /// </summary>
  TLINProtocol = class(TOBDProtocol)
  private
    FVersion: TLINVersion;
    FBaudRate: Integer;
    
    /// <summary>
    ///   Calculate parity bits for protected identifier
    /// </summary>
    function CalculateParity(FrameID: Byte): Byte;
    
    /// <summary>
    ///   Calculate checksum (classic or enhanced)
    /// </summary>
    function CalculateChecksum(FrameID: Byte; const Data: TBytes; 
      ChecksumType: TLINChecksumType): Byte;
    
    /// <summary>
    ///   Build LIN frame with sync, break, and checksum
    /// </summary>
    function BuildFrame(FrameID: Byte; const Data: TBytes; 
      ChecksumType: TLINChecksumType): TBytes;
  protected
    function GetName: string; override;
    function GetDisplayName: string; override;
    function GetELMID: string; override;
  public
    constructor Create(AVersion: TLINVersion = linVersion2_2; ABaudRate: Integer = 19200);
    
    /// <summary>
    ///   Parse LIN frame from raw bytes
    /// </summary>
    function ParseFrame(const RawData: TBytes): TLINFrame;
    
    /// <summary>
    ///   Create unconditional frame
    /// </summary>
    function CreateUnconditionalFrame(FrameID: Byte; const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Create diagnostic request (master to slave)
    /// </summary>
    function CreateDiagnosticRequest(NAD: Byte; ServiceID: Byte; 
      const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Parse diagnostic response (slave to master)
    /// </summary>
    function ParseDiagnosticResponse(const Frame: TLINFrame; 
      var NAD: Byte; var ServiceID: Byte; var Data: TBytes): Boolean;
    
    /// <summary>
    ///   Read data by identifier (LIN diagnostic)
    /// </summary>
    function ReadDataByIdentifier(NAD: Byte; Identifier: Word): TBytes;
    
    /// <summary>
    ///   Write data by identifier (LIN diagnostic)
    /// </summary>
    function WriteDataByIdentifier(NAD: Byte; Identifier: Word; 
      const Data: TBytes): TBytes;
    
    /// <summary>
    ///   Session control
    /// </summary>
    function SessionControl(NAD: Byte; SessionType: Byte): TBytes;
    
    /// <summary>
    ///   Assign frame ID to node (configuration)
    /// </summary>
    function AssignFrameID(NAD: Byte; SupplierID: Word; FunctionID: Word; 
      NewFrameID: Byte): TBytes;
    
    /// <summary>
    ///   Read by identifier (node configuration)
    /// </summary>
    function ReadByIdentifier(NAD: Byte; Identifier: Byte): TBytes;
    
    /// <summary>
    ///   LIN protocol version
    /// </summary>
    property Version: TLINVersion read FVersion write FVersion;
    
    /// <summary>
    ///   Baud rate (typically 9600, 19200, or 20000 bps)
    /// </summary>
    property BaudRate: Integer read FBaudRate write FBaudRate;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TLINProtocol.Create(AVersion: TLINVersion; ABaudRate: Integer);
begin
  inherited Create;
  FVersion := AVersion;
  FBaudRate := ABaudRate;
end;

//------------------------------------------------------------------------------
// GET NAME
//------------------------------------------------------------------------------
function TLINProtocol.GetName: string;
begin
  case FVersion of
    linVersion1_3: Result := 'LIN 1.3';
    linVersion2_0: Result := 'LIN 2.0';
    linVersion2_1: Result := 'LIN 2.1';
    linVersion2_2: Result := 'LIN 2.2A';
  else
    Result := 'LIN';
  end;
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TLINProtocol.GetDisplayName: string;
begin
  Result := Format('%s (Local Interconnect Network @ %d bps)', [GetName, FBaudRate]);
end;

//------------------------------------------------------------------------------
// GET ELM ID
//------------------------------------------------------------------------------
function TLINProtocol.GetELMID: string;
begin
  Result := 'L'; // LIN protocol identifier
end;

//------------------------------------------------------------------------------
// CALCULATE PARITY
//------------------------------------------------------------------------------
function TLINProtocol.CalculateParity(FrameID: Byte): Byte;
var
  ID: Byte;
  P0, P1: Byte;
begin
  ID := FrameID and $3F; // Use only 6 bits
  
  // P0 = ID0 XOR ID1 XOR ID2 XOR ID4
  P0 := ((ID shr 0) and 1) xor ((ID shr 1) and 1) xor 
        ((ID shr 2) and 1) xor ((ID shr 4) and 1);
  
  // P1 = NOT (ID1 XOR ID3 XOR ID4 XOR ID5)
  P1 := not (((ID shr 1) and 1) xor ((ID shr 3) and 1) xor 
             ((ID shr 4) and 1) xor ((ID shr 5) and 1)) and 1;
  
  // Protected ID = ID[5:0] + P1 + P0
  Result := ID or (P0 shl 6) or (P1 shl 7);
end;

//------------------------------------------------------------------------------
// CALCULATE CHECKSUM
//------------------------------------------------------------------------------
function TLINProtocol.CalculateChecksum(FrameID: Byte; const Data: TBytes; 
  ChecksumType: TLINChecksumType): Byte;
var
  I: Integer;
  Sum: Cardinal;
begin
  Sum := 0;
  
  // Enhanced checksum includes protected ID
  if ChecksumType = linEnhanced then
    Sum := CalculateParity(FrameID);
  
  // Add all data bytes
  for I := 0 to High(Data) do
  begin
    Sum := Sum + Data[I];
    if Sum > 255 then
      Sum := Sum - 255; // Carry handling
  end;
  
  // Invert to get checksum
  Result := Byte(not Sum);
end;

//------------------------------------------------------------------------------
// BUILD FRAME
//------------------------------------------------------------------------------
function TLINProtocol.BuildFrame(FrameID: Byte; const Data: TBytes; 
  ChecksumType: TLINChecksumType): TBytes;
var
  ProtectedID: Byte;
  Checksum: Byte;
  Len: Integer;
begin
  ProtectedID := CalculateParity(FrameID);
  Checksum := CalculateChecksum(FrameID, Data, ChecksumType);
  
  Len := 1 + Length(Data) + 1; // ID + Data + Checksum
  SetLength(Result, Len);
  
  Result[0] := ProtectedID;
  if Length(Data) > 0 then
    Move(Data[0], Result[1], Length(Data));
  Result[High(Result)] := Checksum;
end;

//------------------------------------------------------------------------------
// PARSE FRAME
//------------------------------------------------------------------------------
function TLINProtocol.ParseFrame(const RawData: TBytes): TLINFrame;
var
  DataLen: Integer;
  CalcChecksum: Byte;
begin
  Result.Valid := False;
  
  if Length(RawData) < 2 then
    Exit;
  
  Result.FrameID := RawData[0] and $3F; // Remove parity bits
  DataLen := Length(RawData) - 2;
  
  if DataLen > 0 then
  begin
    SetLength(Result.Data, DataLen);
    Move(RawData[1], Result.Data[0], DataLen);
  end;
  
  Result.Checksum := RawData[High(RawData)];
  
  // Determine checksum type based on version
  if FVersion = linVersion1_3 then
    Result.ChecksumType := linClassic
  else
    Result.ChecksumType := linEnhanced;
  
  // Validate checksum
  CalcChecksum := CalculateChecksum(Result.FrameID, Result.Data, Result.ChecksumType);
  Result.Valid := (CalcChecksum = Result.Checksum);
  
  // Determine frame type based on ID
  if Result.FrameID = Byte(linMasterRequest) then
    Result.FrameType := linDiagnostic
  else if Result.FrameID = Byte(linSlaveResponse) then
    Result.FrameType := linDiagnostic
  else
    Result.FrameType := linUnconditional;
end;

//------------------------------------------------------------------------------
// CREATE UNCONDITIONAL FRAME
//------------------------------------------------------------------------------
function TLINProtocol.CreateUnconditionalFrame(FrameID: Byte; const Data: TBytes): TBytes;
var
  ChecksumType: TLINChecksumType;
begin
  if FVersion = linVersion1_3 then
    ChecksumType := linClassic
  else
    ChecksumType := linEnhanced;
    
  Result := BuildFrame(FrameID, Data, ChecksumType);
end;

//------------------------------------------------------------------------------
// CREATE DIAGNOSTIC REQUEST
//------------------------------------------------------------------------------
function TLINProtocol.CreateDiagnosticRequest(NAD: Byte; ServiceID: Byte; 
  const Data: TBytes): TBytes;
var
  DiagData: TBytes;
  Len: Integer;
begin
  Len := 2 + Length(Data);
  SetLength(DiagData, 8); // LIN diagnostic frames are always 8 bytes
  
  DiagData[0] := NAD;      // Node Address for Diagnostic
  DiagData[1] := $06;      // PCI (Single frame, 6 data bytes max)
  DiagData[2] := ServiceID;
  
  if Length(Data) > 0 then
    Move(Data[0], DiagData[3], Min(Length(Data), 5));
  
  // Pad with $FF
  FillChar(DiagData[3 + Length(Data)], 8 - (3 + Length(Data)), $FF);
  
  Result := BuildFrame(Byte(linMasterRequest), DiagData, linEnhanced);
end;

//------------------------------------------------------------------------------
// PARSE DIAGNOSTIC RESPONSE
//------------------------------------------------------------------------------
function TLINProtocol.ParseDiagnosticResponse(const Frame: TLINFrame; 
  var NAD: Byte; var ServiceID: Byte; var Data: TBytes): Boolean;
var
  DataLen: Integer;
begin
  Result := False;
  
  if (Length(Frame.Data) < 3) or (Frame.FrameID <> Byte(linSlaveResponse)) then
    Exit;
  
  NAD := Frame.Data[0];
  DataLen := Frame.Data[1] and $0F; // PCI contains length
  
  if DataLen > 0 then
  begin
    ServiceID := Frame.Data[2];
    if DataLen > 1 then
    begin
      SetLength(Data, DataLen - 1);
      Move(Frame.Data[3], Data[0], DataLen - 1);
    end;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------
// READ DATA BY IDENTIFIER
//------------------------------------------------------------------------------
function TLINProtocol.ReadDataByIdentifier(NAD: Byte; Identifier: Word): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 2);
  Data[0] := Hi(Identifier);
  Data[1] := Lo(Identifier);
  Result := CreateDiagnosticRequest(NAD, Byte(linReadDataByIdentifier), Data);
end;

//------------------------------------------------------------------------------
// WRITE DATA BY IDENTIFIER
//------------------------------------------------------------------------------
function TLINProtocol.WriteDataByIdentifier(NAD: Byte; Identifier: Word; 
  const Data: TBytes): TBytes;
var
  ReqData: TBytes;
begin
  SetLength(ReqData, 2 + Length(Data));
  ReqData[0] := Hi(Identifier);
  ReqData[1] := Lo(Identifier);
  if Length(Data) > 0 then
    Move(Data[0], ReqData[2], Length(Data));
  Result := CreateDiagnosticRequest(NAD, Byte(linWriteByIdentifier), ReqData);
end;

//------------------------------------------------------------------------------
// SESSION CONTROL
//------------------------------------------------------------------------------
function TLINProtocol.SessionControl(NAD: Byte; SessionType: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := SessionType;
  Result := CreateDiagnosticRequest(NAD, Byte(linSessionControl), Data);
end;

//------------------------------------------------------------------------------
// ASSIGN FRAME ID
//------------------------------------------------------------------------------
function TLINProtocol.AssignFrameID(NAD: Byte; SupplierID: Word; FunctionID: Word; 
  NewFrameID: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 5);
  Data[0] := Hi(SupplierID);
  Data[1] := Lo(SupplierID);
  Data[2] := Hi(FunctionID);
  Data[3] := Lo(FunctionID);
  Data[4] := NewFrameID;
  Result := CreateDiagnosticRequest(NAD, $B1, Data); // Assign frame ID service
end;

//------------------------------------------------------------------------------
// READ BY IDENTIFIER
//------------------------------------------------------------------------------
function TLINProtocol.ReadByIdentifier(NAD: Byte; Identifier: Byte): TBytes;
var
  Data: TBytes;
begin
  SetLength(Data, 1);
  Data[0] := Identifier;
  Result := CreateDiagnosticRequest(NAD, $B2, Data); // Read by identifier service
end;

end.
