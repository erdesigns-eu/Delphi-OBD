//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.ECUSecurity.pas
// CONTENTS       : ECU Security and Flashing Protocol Support
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.ECUSecurity;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,
  OBD.Protocol, OBD.Protocol.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Security Algorithm Type
  /// </summary>
  TSecurityAlgorithm = (
    saSeed Key,              // Simple seed/key
    saRSA,                   // RSA asymmetric
    saAES,                   // AES symmetric
    saChallenge Response,    // Challenge/response
    saHMAC,                  // HMAC-based
    saCustomOEM              // OEM-specific algorithm
  );

  /// <summary>
  ///   ECU Security Level
  /// </summary>
  TECUSecurityLevel = (
    slNone         = $00,
    slDiagnostic   = $01,
    slProgramming  = $03,
    slDeveloper    = $05,
    slManufacturer = $07
  );

  /// <summary>
  ///   Flash Memory Type
  /// </summary>
  TFlashMemoryType = (
    fmtInternal,      // Internal flash
    fmtExternal,      // External flash (SPI/I2C)
    fmtEEPROM,        // EEPROM
    fmtOTP            // One-time programmable
  );

  /// <summary>
  ///   Flash Block Info
  /// </summary>
  TFlashBlockInfo = record
    Address: Cardinal;
    Size: Cardinal;
    MemoryType: TFlashMemoryType;
    Erasable: Boolean;
    Writable: Boolean;
    Protected: Boolean;
  end;

  /// <summary>
  ///   Security Access State
  /// </summary>
  TSecurityAccessState = record
    Level: TECUSecurityLevel;
    Unlocked: Boolean;
    AttemptsRemaining: Integer;
    TimeoutSeconds: Integer;
    Algorithm: TSecurityAlgorithm;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   ECU Security and Flashing Protocol
  ///   Advanced security access and firmware flashing operations
  /// </summary>
  TECUSecurityProtocol = class(TOBDProtocol)
  private
    FSecurityState: TSecurityAccessState;
    FFlashInProgress: Boolean;
    
    /// <summary>
    ///   Calculate seed/key response (Manufacturer-specific algorithms)
    /// </summary>
    function CalculateSeedKeyResponse(const Seed: TBytes; 
      Algorithm: TSecurityAlgorithm): TBytes;
    
    /// <summary>
    ///   Generate RSA key pair
    /// </summary>
    procedure GenerateRSAKeyPair(var PublicKey, PrivateKey: TBytes);
    
    /// <summary>
    ///   Calculate CRC for flash block
    /// </summary>
    function CalculateFlashCRC(const Data: TBytes): Cardinal;
    
    /// <summary>
    ///   Verify flash signature
    /// </summary>
    function VerifyFlashSignature(const Data: TBytes; const Signature: TBytes): Boolean;
  protected
    function GetName: string; override;
    function GetDisplayName: string; override;
    function GetELMID: string; override;
  public
    constructor Create;
    
    /// <summary>
    ///   Request security seed
    /// </summary>
    function RequestSeed(Level: TECUSecurityLevel): TBytes;
    
    /// <summary>
    ///   Send security key
    /// </summary>
    function SendKey(Level: TECUSecurityLevel; const Key: TBytes): Boolean;
    
    /// <summary>
    ///   Unlock security with seed/key
    /// </summary>
    function UnlockSecurity(Level: TECUSecurityLevel; 
      Algorithm: TSecurityAlgorithm): Boolean;
    
    /// <summary>
    ///   Read security status
    /// </summary>
    function ReadSecurityStatus: TSecurityAccessState;
    
    /// <summary>
    ///   Reset security (requires power cycle or timeout)
    /// </summary>
    function ResetSecurity: Boolean;
    
    /// <summary>
    ///   Read ECU software version
    /// </summary>
    function ReadSoftwareVersion: string;
    
    /// <summary>
    ///   Read ECU hardware version
    /// </summary>
    function ReadHardwareVersion: string;
    
    /// <summary>
    ///   Read ECU serial number
    /// </summary>
    function ReadSerialNumber: string;
    
    /// <summary>
    ///   Read flash memory layout
    /// </summary>
    function ReadFlashLayout: TArray<TFlashBlockInfo>;
    
    /// <summary>
    ///   Erase flash block
    /// </summary>
    function EraseFlashBlock(Address: Cardinal; Size: Cardinal): Boolean;
    
    /// <summary>
    ///   Erase entire flash memory
    /// </summary>
    function EraseFlashMemory: Boolean;
    
    /// <summary>
    ///   Write flash block
    /// </summary>
    function WriteFlashBlock(Address: Cardinal; const Data: TBytes): Boolean;
    
    /// <summary>
    ///   Read flash block
    /// </summary>
    function ReadFlashBlock(Address: Cardinal; Size: Cardinal): TBytes;
    
    /// <summary>
    ///   Verify flash block
    /// </summary>
    function VerifyFlashBlock(Address: Cardinal; const Data: TBytes): Boolean;
    
    /// <summary>
    ///   Flash firmware file (with progress callback)
    /// </summary>
    function FlashFirmware(const FileName: string; 
      ProgressCallback: TProc<Integer>): Boolean;
    
    /// <summary>
    ///   Checksum flash memory
    /// </summary>
    function ChecksumFlashMemory(Address: Cardinal; Size: Cardinal): Cardinal;
    
    /// <summary>
    ///   Set ECU programming voltage
    /// </summary>
    function SetProgrammingVoltage(Voltage: Single): Boolean;
    
    /// <summary>
    ///   Reset ECU
    /// </summary>
    function ResetECU(HardReset: Boolean = False): Boolean;
    
    /// <summary>
    ///   Read ECU identification
    /// </summary>
    function ReadECUIdentification: TStringList;
    
    /// <summary>
    ///   Read supported security levels
    /// </summary>
    function ReadSupportedSecurityLevels: TArray<TECUSecurityLevel>;
    
    /// <summary>
    ///   Enable/disable programming mode
    /// </summary>
    function SetProgrammingMode(Enable: Boolean): Boolean;
    
    /// <summary>
    ///   Validate firmware image
    /// </summary>
    function ValidateFirmwareImage(const FileName: string): Boolean;
    
    /// <summary>
    ///   Backup current firmware
    /// </summary>
    function BackupFirmware(const FileName: string): Boolean;
    
    /// <summary>
    ///   Security state
    /// </summary>
    property SecurityState: TSecurityAccessState read FSecurityState;
    
    /// <summary>
    ///   Is flash operation in progress?
    /// </summary>
    property FlashInProgress: Boolean read FFlashInProgress;
  end;

implementation

uses System.IOUtils;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TECUSecurityProtocol.Create;
begin
  inherited Create;
  FSecurityState.Level := slNone;
  FSecurityState.Unlocked := False;
  FSecurityState.AttemptsRemaining := 3;
  FFlashInProgress := False;
end;

//------------------------------------------------------------------------------
// GET NAME
//------------------------------------------------------------------------------
function TECUSecurityProtocol.GetName: string;
begin
  Result := 'ECU Security & Flashing';
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TECUSecurityProtocol.GetDisplayName: string;
begin
  Result := 'ECU Security Access and Firmware Flashing Protocol';
end;

//------------------------------------------------------------------------------
// GET ELM ID
//------------------------------------------------------------------------------
function TECUSecurityProtocol.GetELMID: string;
begin
  Result := 'S'; // Security protocol identifier
end;

//------------------------------------------------------------------------------
// CALCULATE SEED KEY RESPONSE
//------------------------------------------------------------------------------
function TECUSecurityProtocol.CalculateSeedKeyResponse(const Seed: TBytes; 
  Algorithm: TSecurityAlgorithm): TBytes;
var
  I: Integer;
  Key: Cardinal;
begin
  // Simplified seed/key algorithms - Real implementations are manufacturer-specific
  
  case Algorithm of
    saSeedKey:
    begin
      // Simple XOR-based algorithm (example)
      SetLength(Result, Length(Seed));
      Key := $A5A5A5A5;
      for I := 0 to High(Seed) do
        Result[I] := Seed[I] xor ((Key shr (8 * (I mod 4))) and $FF);
    end;
    
    saChallenge Response:
    begin
      // Challenge/response (example)
      SetLength(Result, 4);
      Key := 0;
      for I := 0 to High(Seed) do
        Key := Key + Cardinal(Seed[I]) * (I + 1);
      Result[0] := (Key shr 24) and $FF;
      Result[1] := (Key shr 16) and $FF;
      Result[2] := (Key shr 8) and $FF;
      Result[3] := Key and $FF;
    end;
    
  else
    SetLength(Result, 0);
  end;
end;

//------------------------------------------------------------------------------
// GENERATE RSA KEY PAIR
//------------------------------------------------------------------------------
procedure TECUSecurityProtocol.GenerateRSAKeyPair(var PublicKey, PrivateKey: TBytes);
begin
  // Placeholder - real implementation would use crypto library
  SetLength(PublicKey, 128);
  SetLength(PrivateKey, 128);
end;

//------------------------------------------------------------------------------
// CALCULATE FLASH CRC
//------------------------------------------------------------------------------
function TECUSecurityProtocol.CalculateFlashCRC(const Data: TBytes): Cardinal;
var
  I: Integer;
  CRC: Cardinal;
begin
  CRC := $FFFFFFFF;
  for I := 0 to High(Data) do
  begin
    CRC := CRC xor Cardinal(Data[I]);
    CRC := (CRC shr 1) xor (-(CRC and 1) and $EDB88320);
  end;
  Result := not CRC;
end;

//------------------------------------------------------------------------------
// VERIFY FLASH SIGNATURE
//------------------------------------------------------------------------------
function TECUSecurityProtocol.VerifyFlashSignature(const Data: TBytes; const Signature: TBytes): Boolean;
begin
  // Placeholder - real implementation would verify RSA/ECDSA signature
  Result := Length(Signature) > 0;
end;

//------------------------------------------------------------------------------
// REQUEST SEED
//------------------------------------------------------------------------------
function TECUSecurityProtocol.RequestSeed(Level: TECUSecurityLevel): TBytes;
var
  Command: TBytes;
begin
  SetLength(Command, 2);
  Command[0] := $27; // Security Access service
  Command[1] := Byte(Level); // Odd number for seed request
  
  // In real implementation, send command and return seed
  SetLength(Result, 4);
  Result[0] := $12;
  Result[1] := $34;
  Result[2] := $56;
  Result[3] := $78;
end;

//------------------------------------------------------------------------------
// SEND KEY
//------------------------------------------------------------------------------
function TECUSecurityProtocol.SendKey(Level: TECUSecurityLevel; const Key: TBytes): Boolean;
var
  Command: TBytes;
begin
  SetLength(Command, 2 + Length(Key));
  Command[0] := $27; // Security Access service
  Command[1] := Byte(Level) + 1; // Even number for key send
  
  if Length(Key) > 0 then
    Move(Key[0], Command[2], Length(Key));
  
  // In real implementation, send command and check response
  Result := True;
  if Result then
  begin
    FSecurityState.Level := Level;
    FSecurityState.Unlocked := True;
  end;
end;

//------------------------------------------------------------------------------
// UNLOCK SECURITY
//------------------------------------------------------------------------------
function TECUSecurityProtocol.UnlockSecurity(Level: TECUSecurityLevel; 
  Algorithm: TSecurityAlgorithm): Boolean;
var
  Seed, Key: TBytes;
begin
  Seed := RequestSeed(Level);
  if Length(Seed) = 0 then
    Exit(False);
  
  Key := CalculateSeedKeyResponse(Seed, Algorithm);
  Result := SendKey(Level, Key);
end;

//------------------------------------------------------------------------------
// READ SECURITY STATUS
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadSecurityStatus: TSecurityAccessState;
begin
  Result := FSecurityState;
end;

//------------------------------------------------------------------------------
// RESET SECURITY
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ResetSecurity: Boolean;
begin
  FSecurityState.Level := slNone;
  FSecurityState.Unlocked := False;
  FSecurityState.AttemptsRemaining := 3;
  Result := True;
end;

//------------------------------------------------------------------------------
// READ SOFTWARE VERSION
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadSoftwareVersion: string;
begin
  Result := '1.0.0'; // Placeholder
end;

//------------------------------------------------------------------------------
// READ HARDWARE VERSION
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadHardwareVersion: string;
begin
  Result := 'HW v1.0'; // Placeholder
end;

//------------------------------------------------------------------------------
// READ SERIAL NUMBER
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadSerialNumber: string;
begin
  Result := 'SN123456789'; // Placeholder
end;

//------------------------------------------------------------------------------
// READ FLASH LAYOUT
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadFlashLayout: TArray<TFlashBlockInfo>;
var
  Block: TFlashBlockInfo;
begin
  SetLength(Result, 1);
  Block.Address := $00000000;
  Block.Size := $00040000; // 256KB
  Block.MemoryType := fmtInternal;
  Block.Erasable := True;
  Block.Writable := True;
  Block.Protected := False;
  Result[0] := Block;
end;

//------------------------------------------------------------------------------
// ERASE FLASH BLOCK
//------------------------------------------------------------------------------
function TECUSecurityProtocol.EraseFlashBlock(Address: Cardinal; Size: Cardinal): Boolean;
begin
  if not FSecurityState.Unlocked then
    Exit(False);
  
  // In real implementation, send erase command
  Result := True;
end;

//------------------------------------------------------------------------------
// ERASE FLASH MEMORY
//------------------------------------------------------------------------------
function TECUSecurityProtocol.EraseFlashMemory: Boolean;
begin
  if not FSecurityState.Unlocked then
    Exit(False);
  
  // In real implementation, erase entire flash
  Result := True;
end;

//------------------------------------------------------------------------------
// WRITE FLASH BLOCK
//------------------------------------------------------------------------------
function TECUSecurityProtocol.WriteFlashBlock(Address: Cardinal; const Data: TBytes): Boolean;
begin
  if not FSecurityState.Unlocked then
    Exit(False);
  
  // In real implementation, write flash data
  Result := True;
end;

//------------------------------------------------------------------------------
// READ FLASH BLOCK
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadFlashBlock(Address: Cardinal; Size: Cardinal): TBytes;
begin
  SetLength(Result, Size);
  // In real implementation, read flash data
end;

//------------------------------------------------------------------------------
// VERIFY FLASH BLOCK
//------------------------------------------------------------------------------
function TECUSecurityProtocol.VerifyFlashBlock(Address: Cardinal; const Data: TBytes): Boolean;
var
  ReadData: TBytes;
begin
  ReadData := ReadFlashBlock(Address, Length(Data));
  Result := CompareMem(@Data[0], @ReadData[0], Length(Data));
end;

//------------------------------------------------------------------------------
// FLASH FIRMWARE
//------------------------------------------------------------------------------
function TECUSecurityProtocol.FlashFirmware(const FileName: string; 
  ProgressCallback: TProc<Integer>): Boolean;
var
  FileStream: TFileStream;
  BlockSize: Integer;
  Address: Cardinal;
  Buffer: TBytes;
  BytesRead, TotalBytes: Integer;
  Progress: Integer;
begin
  Result := False;
  
  if not FSecurityState.Unlocked then
    Exit;
  
  if not FileExists(FileName) then
    Exit;
  
  FFlashInProgress := True;
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      TotalBytes := FileStream.Size;
      BlockSize := 256; // 256 bytes per block
      Address := 0;
      
      SetLength(Buffer, BlockSize);
      
      while FileStream.Position < FileStream.Size do
      begin
        BytesRead := FileStream.Read(Buffer[0], BlockSize);
        
        if not WriteFlashBlock(Address, Buffer) then
          Exit;
        
        Inc(Address, BytesRead);
        
        if Assigned(ProgressCallback) then
        begin
          Progress := Round((FileStream.Position / TotalBytes) * 100);
          ProgressCallback(Progress);
        end;
      end;
      
      Result := True;
    finally
      FileStream.Free;
    end;
  finally
    FFlashInProgress := False;
  end;
end;

//------------------------------------------------------------------------------
// CHECKSUM FLASH MEMORY
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ChecksumFlashMemory(Address: Cardinal; Size: Cardinal): Cardinal;
var
  Data: TBytes;
begin
  Data := ReadFlashBlock(Address, Size);
  Result := CalculateFlashCRC(Data);
end;

//------------------------------------------------------------------------------
// SET PROGRAMMING VOLTAGE
//------------------------------------------------------------------------------
function TECUSecurityProtocol.SetProgrammingVoltage(Voltage: Single): Boolean;
begin
  // In real implementation, set voltage via J2534 or similar
  Result := (Voltage >= 10.0) and (Voltage <= 14.0);
end;

//------------------------------------------------------------------------------
// RESET ECU
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ResetECU(HardReset: Boolean): Boolean;
begin
  // In real implementation, send ECU reset command
  Result := True;
  if Result then
    ResetSecurity;
end;

//------------------------------------------------------------------------------
// READ ECU IDENTIFICATION
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadECUIdentification: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('Manufacturer: Example OEM');
  Result.Add('Part Number: 12345-67890');
  Result.Add('Software: v1.0.0');
  Result.Add('Hardware: v1.0');
end;

//------------------------------------------------------------------------------
// READ SUPPORTED SECURITY LEVELS
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ReadSupportedSecurityLevels: TArray<TECUSecurityLevel>;
begin
  SetLength(Result, 2);
  Result[0] := slDiagnostic;
  Result[1] := slProgramming;
end;

//------------------------------------------------------------------------------
// SET PROGRAMMING MODE
//------------------------------------------------------------------------------
function TECUSecurityProtocol.SetProgrammingMode(Enable: Boolean): Boolean;
begin
  // In real implementation, enter/exit programming session
  Result := True;
end;

//------------------------------------------------------------------------------
// VALIDATE FIRMWARE IMAGE
//------------------------------------------------------------------------------
function TECUSecurityProtocol.ValidateFirmwareImage(const FileName: string): Boolean;
begin
  if not FileExists(FileName) then
    Exit(False);
  
  // In real implementation, validate firmware signature and checksum
  Result := True;
end;

//------------------------------------------------------------------------------
// BACKUP FIRMWARE
//------------------------------------------------------------------------------
function TECUSecurityProtocol.BackupFirmware(const FileName: string): Boolean;
var
  FileStream: TFileStream;
  Data: TBytes;
  Layout: TArray<TFlashBlockInfo>;
  I: Integer;
begin
  Result := False;
  
  if not FSecurityState.Unlocked then
    Exit;
  
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    Layout := ReadFlashLayout;
    
    for I := 0 to High(Layout) do
    begin
      Data := ReadFlashBlock(Layout[I].Address, Layout[I].Size);
      FileStream.WriteBuffer(Data[0], Length(Data));
    end;
    
    Result := True;
  finally
    FileStream.Free;
  end;
end;

end.
