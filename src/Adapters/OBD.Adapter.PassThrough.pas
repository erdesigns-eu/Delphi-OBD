//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.PassThrough.pas
// CONTENTS       : J2534 Pass-Through Adapter Support
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.Adapter.PassThrough;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Win.Registry,
  OBD.Adapter, OBD.Adapter.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   J2534 Protocol IDs
  /// </summary>
  TJ2534Protocol = (
    j2534_J1850VPW      = $01,
    j2534_J1850PWM      = $02,
    j2534_ISO9141       = $03,
    j2534_ISO14230      = $04,
    j2534_CAN           = $05,
    j2534_ISO15765      = $06,
    j2534_SCIAEngine    = $07,
    j2534_SCIATrans     = $08,
    j2534_SCIBEngine    = $09,
    j2534_SCIBTrans     = $0A
  );

  /// <summary>
  ///   J2534 Pass-Through Device Info
  /// </summary>
  TJ2534DeviceInfo = record
    Name: string;
    Vendor: string;
    LibraryPath: string;
    CANSupport: Boolean;
    ISO15765Support: Boolean;
    J1850Support: Boolean;
    ISO9141Support: Boolean;
    ISO14230Support: Boolean;
  end;

  /// <summary>
  ///   J2534 Function Pointers
  /// </summary>
  TPassThroughOpen = function(pName: PAnsiChar; var DeviceID: Cardinal): Integer; stdcall;
  TPassThroughClose = function(DeviceID: Cardinal): Integer; stdcall;
  TPassThroughConnect = function(DeviceID: Cardinal; ProtocolID: Cardinal; 
    Flags: Cardinal; Baudrate: Cardinal; var ChannelID: Cardinal): Integer; stdcall;
  TPassThroughDisconnect = function(ChannelID: Cardinal): Integer; stdcall;
  TPassThroughReadMsgs = function(ChannelID: Cardinal; pMsg: Pointer; 
    var NumMsgs: Cardinal; Timeout: Cardinal): Integer; stdcall;
  TPassThroughWriteMsgs = function(ChannelID: Cardinal; pMsg: Pointer; 
    var NumMsgs: Cardinal; Timeout: Cardinal): Integer; stdcall;
  TPassThroughStartPeriodicMsg = function(ChannelID: Cardinal; pMsg: Pointer;
    var MsgID: Cardinal; TimeInterval: Cardinal): Integer; stdcall;
  TPassThroughStopPeriodicMsg = function(ChannelID: Cardinal; MsgID: Cardinal): Integer; stdcall;
  TPassThroughStartMsgFilter = function(ChannelID: Cardinal; FilterType: Cardinal;
    pMaskMsg: Pointer; pPatternMsg: Pointer; pFlowControlMsg: Pointer;
    var FilterID: Cardinal): Integer; stdcall;
  TPassThroughStopMsgFilter = function(ChannelID: Cardinal; FilterID: Cardinal): Integer; stdcall;
  TPassThroughSetProgrammingVoltage = function(DeviceID: Cardinal; 
    PinNumber: Cardinal; Voltage: Cardinal): Integer; stdcall;
  TPassThroughReadVersion = function(DeviceID: Cardinal; pFirmwareVersion: PAnsiChar;
    pDllVersion: PAnsiChar; pApiVersion: PAnsiChar): Integer; stdcall;
  TPassThroughGetLastError = function(pErrorDescription: PAnsiChar): Integer; stdcall;
  TPassThroughIoctl = function(HandleID: Cardinal; IoctlID: Cardinal; 
    pInput: Pointer; pOutput: Pointer): Integer; stdcall;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   J2534 Pass-Through Adapter
  ///   Provides direct vehicle communication through SAE J2534 interface
  /// </summary>
  TJ2534PassThroughAdapter = class(TOBDAdapter)
  private
    FLibraryHandle: THandle;
    FDeviceID: Cardinal;
    FChannelID: Cardinal;
    FDeviceInfo: TJ2534DeviceInfo;
    
    // J2534 Function Pointers
    FPassThroughOpen: TPassThroughOpen;
    FPassThroughClose: TPassThroughClose;
    FPassThroughConnect: TPassThroughConnect;
    FPassThroughDisconnect: TPassThroughDisconnect;
    FPassThroughReadMsgs: TPassThroughReadMsgs;
    FPassThroughWriteMsgs: TPassThroughWriteMsgs;
    FPassThroughStartPeriodicMsg: TPassThroughStartPeriodicMsg;
    FPassThroughStopPeriodicMsg: TPassThroughStopPeriodicMsg;
    FPassThroughStartMsgFilter: TPassThroughStartMsgFilter;
    FPassThroughStopMsgFilter: TPassThroughStopMsgFilter;
    FPassThroughSetProgrammingVoltage: TPassThroughSetProgrammingVoltage;
    FPassThroughReadVersion: TPassThroughReadVersion;
    FPassThroughGetLastError: TPassThroughGetLastError;
    FPassThroughIoctl: TPassThroughIoctl;
    
    /// <summary>
    ///   Load J2534 library
    /// </summary>
    function LoadLibrary(const LibPath: string): Boolean;
    
    /// <summary>
    ///   Unload J2534 library
    /// </summary>
    procedure UnloadLibrary;
    
    /// <summary>
    ///   Load function pointers
    /// </summary>
    function LoadFunctions: Boolean;
  public
    constructor Create(const DeviceInfo: TJ2534DeviceInfo);
    destructor Destroy; override;
    
    /// <summary>
    ///   Connect to pass-through device
    /// </summary>
    function Connect: Boolean; override;
    
    /// <summary>
    ///   Disconnect from pass-through device
    /// </summary>
    function Disconnect: Boolean; override;
    
    /// <summary>
    ///   Is connected?
    /// </summary>
    function Connected: Boolean; override;
    
    /// <summary>
    ///   Open channel with specific protocol
    /// </summary>
    function OpenChannel(Protocol: TJ2534Protocol; BaudRate: Cardinal;
      Flags: Cardinal = 0): Boolean;
    
    /// <summary>
    ///   Close current channel
    /// </summary>
    function CloseChannel: Boolean;
    
    /// <summary>
    ///   Read messages from vehicle
    /// </summary>
    function ReadMessages(var Messages: TBytes; Timeout: Cardinal = 100): Integer;
    
    /// <summary>
    ///   Write messages to vehicle
    /// </summary>
    function WriteMessages(const Messages: TBytes): Boolean;
    
    /// <summary>
    ///   Start periodic message transmission
    /// </summary>
    function StartPeriodicMessage(const Message: TBytes; 
      IntervalMs: Cardinal): Cardinal;
    
    /// <summary>
    ///   Stop periodic message
    /// </summary>
    function StopPeriodicMessage(MessageID: Cardinal): Boolean;
    
    /// <summary>
    ///   Set programming voltage (for ECU flashing)
    /// </summary>
    function SetProgrammingVoltage(PinNumber: Cardinal; Voltage: Cardinal): Boolean;
    
    /// <summary>
    ///   Read device version information
    /// </summary>
    function ReadVersion(var Firmware, DLL, API: string): Boolean;
    
    /// <summary>
    ///   Get last error description
    /// </summary>
    function GetLastError: string;
    
    /// <summary>
    ///   Device information
    /// </summary>
    property DeviceInfo: TJ2534DeviceInfo read FDeviceInfo;
  end;

  /// <summary>
  ///   J2534 Device Registry Scanner
  ///   Scans Windows Registry for installed J2534 pass-through devices
  /// </summary>
  TJ2534DeviceScanner = class
  public
    /// <summary>
    ///   Scan registry for J2534 devices
    /// </summary>
    class function ScanDevices: TArray<TJ2534DeviceInfo>;
    
    /// <summary>
    ///   Get device info by name
    /// </summary>
    class function GetDeviceByName(const Name: string): TJ2534DeviceInfo;
  end;

implementation

uses System.StrUtils;

const
  J2534_REGISTRY_PATH = 'SOFTWARE\PassThruSupport.04.04';
  
  STATUS_NOERROR = $00;
  ERR_DEVICE_NOT_CONNECTED = $08;
  ERR_FAILED = $06;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TJ2534PassThroughAdapter.Create(const DeviceInfo: TJ2534DeviceInfo);
begin
  inherited Create;
  FDeviceInfo := DeviceInfo;
  FLibraryHandle := 0;
  FDeviceID := 0;
  FChannelID := 0;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TJ2534PassThroughAdapter.Destroy;
begin
  if Connected then
    Disconnect;
  UnloadLibrary;
  inherited;
end;

//------------------------------------------------------------------------------
// LOAD LIBRARY
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.LoadLibrary(const LibPath: string): Boolean;
begin
  Result := False;
  
  if not FileExists(LibPath) then
    Exit;
  
  FLibraryHandle := Winapi.Windows.LoadLibrary(PChar(LibPath));
  Result := (FLibraryHandle <> 0) and LoadFunctions;
end;

//------------------------------------------------------------------------------
// UNLOAD LIBRARY
//------------------------------------------------------------------------------
procedure TJ2534PassThroughAdapter.UnloadLibrary;
begin
  if FLibraryHandle <> 0 then
  begin
    FreeLibrary(FLibraryHandle);
    FLibraryHandle := 0;
  end;
end;

//------------------------------------------------------------------------------
// LOAD FUNCTIONS
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.LoadFunctions: Boolean;
begin
  Result := False;
  
  @FPassThroughOpen := GetProcAddress(FLibraryHandle, 'PassThruOpen');
  @FPassThroughClose := GetProcAddress(FLibraryHandle, 'PassThruClose');
  @FPassThroughConnect := GetProcAddress(FLibraryHandle, 'PassThruConnect');
  @FPassThroughDisconnect := GetProcAddress(FLibraryHandle, 'PassThruDisconnect');
  @FPassThroughReadMsgs := GetProcAddress(FLibraryHandle, 'PassThruReadMsgs');
  @FPassThroughWriteMsgs := GetProcAddress(FLibraryHandle, 'PassThruWriteMsgs');
  @FPassThroughStartPeriodicMsg := GetProcAddress(FLibraryHandle, 'PassThruStartPeriodicMsg');
  @FPassThroughStopPeriodicMsg := GetProcAddress(FLibraryHandle, 'PassThruStopPeriodicMsg');
  @FPassThroughStartMsgFilter := GetProcAddress(FLibraryHandle, 'PassThruStartMsgFilter');
  @FPassThroughStopMsgFilter := GetProcAddress(FLibraryHandle, 'PassThruStopMsgFilter');
  @FPassThroughSetProgrammingVoltage := GetProcAddress(FLibraryHandle, 'PassThruSetProgrammingVoltage');
  @FPassThroughReadVersion := GetProcAddress(FLibraryHandle, 'PassThruReadVersion');
  @FPassThroughGetLastError := GetProcAddress(FLibraryHandle, 'PassThruGetLastError');
  @FPassThroughIoctl := GetProcAddress(FLibraryHandle, 'PassThruIoctl');
  
  Result := Assigned(FPassThroughOpen) and Assigned(FPassThroughClose) and
            Assigned(FPassThroughConnect) and Assigned(FPassThroughDisconnect);
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.Connect: Boolean;
var
  DeviceName: AnsiString;
begin
  Result := False;
  
  if not LoadLibrary(FDeviceInfo.LibraryPath) then
    Exit;
  
  DeviceName := AnsiString(FDeviceInfo.Name);
  Result := FPassThroughOpen(PAnsiChar(DeviceName), FDeviceID) = STATUS_NOERROR;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.Disconnect: Boolean;
begin
  Result := False;
  
  if FChannelID <> 0 then
    CloseChannel;
  
  if FDeviceID <> 0 then
  begin
    Result := FPassThroughClose(FDeviceID) = STATUS_NOERROR;
    FDeviceID := 0;
  end;
  
  UnloadLibrary;
end;

//------------------------------------------------------------------------------
// CONNECTED
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.Connected: Boolean;
begin
  Result := (FDeviceID <> 0);
end;

//------------------------------------------------------------------------------
// OPEN CHANNEL
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.OpenChannel(Protocol: TJ2534Protocol; 
  BaudRate: Cardinal; Flags: Cardinal): Boolean;
begin
  Result := FPassThroughConnect(FDeviceID, Cardinal(Protocol), Flags, 
    BaudRate, FChannelID) = STATUS_NOERROR;
end;

//------------------------------------------------------------------------------
// CLOSE CHANNEL
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.CloseChannel: Boolean;
begin
  Result := False;
  
  if FChannelID <> 0 then
  begin
    Result := FPassThroughDisconnect(FChannelID) = STATUS_NOERROR;
    FChannelID := 0;
  end;
end;

//------------------------------------------------------------------------------
// READ MESSAGES
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.ReadMessages(var Messages: TBytes; 
  Timeout: Cardinal): Integer;
var
  NumMsgs: Cardinal;
  Buffer: array[0..4095] of Byte;
begin
  Result := 0;
  NumMsgs := 1;
  
  if FPassThroughReadMsgs(FChannelID, @Buffer, NumMsgs, Timeout) = STATUS_NOERROR then
  begin
    Result := Integer(NumMsgs);
    if NumMsgs > 0 then
    begin
      SetLength(Messages, NumMsgs * SizeOf(Buffer));
      Move(Buffer, Messages[0], Length(Messages));
    end;
  end;
end;

//------------------------------------------------------------------------------
// WRITE MESSAGES
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.WriteMessages(const Messages: TBytes): Boolean;
var
  NumMsgs: Cardinal;
begin
  NumMsgs := 1;
  Result := FPassThroughWriteMsgs(FChannelID, @Messages[0], NumMsgs, 1000) = STATUS_NOERROR;
end;

//------------------------------------------------------------------------------
// START PERIODIC MESSAGE
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.StartPeriodicMessage(const Message: TBytes; 
  IntervalMs: Cardinal): Cardinal;
begin
  Result := 0;
  FPassThroughStartPeriodicMsg(FChannelID, @Message[0], Result, IntervalMs);
end;

//------------------------------------------------------------------------------
// STOP PERIODIC MESSAGE
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.StopPeriodicMessage(MessageID: Cardinal): Boolean;
begin
  Result := FPassThroughStopPeriodicMsg(FChannelID, MessageID) = STATUS_NOERROR;
end;

//------------------------------------------------------------------------------
// SET PROGRAMMING VOLTAGE
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.SetProgrammingVoltage(PinNumber: Cardinal; 
  Voltage: Cardinal): Boolean;
begin
  Result := FPassThroughSetProgrammingVoltage(FDeviceID, PinNumber, Voltage) = STATUS_NOERROR;
end;

//------------------------------------------------------------------------------
// READ VERSION
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.ReadVersion(var Firmware, DLL, API: string): Boolean;
var
  FirmwareBuf, DLLBuf, APIBuf: array[0..255] of AnsiChar;
begin
  Result := FPassThroughReadVersion(FDeviceID, @FirmwareBuf, @DLLBuf, @APIBuf) = STATUS_NOERROR;
  
  if Result then
  begin
    Firmware := string(FirmwareBuf);
    DLL := string(DLLBuf);
    API := string(APIBuf);
  end;
end;

//------------------------------------------------------------------------------
// GET LAST ERROR
//------------------------------------------------------------------------------
function TJ2534PassThroughAdapter.GetLastError: string;
var
  ErrorBuf: array[0..255] of AnsiChar;
begin
  if Assigned(FPassThroughGetLastError) then
  begin
    FPassThroughGetLastError(@ErrorBuf);
    Result := string(ErrorBuf);
  end
  else
    Result := 'Unknown error';
end;

//------------------------------------------------------------------------------
// SCAN DEVICES
//------------------------------------------------------------------------------
class function TJ2534DeviceScanner.ScanDevices: TArray<TJ2534DeviceInfo>;
var
  Reg: TRegistry;
  DeviceList: TStringList;
  I: Integer;
  DeviceInfo: TJ2534DeviceInfo;
begin
  SetLength(Result, 0);
  Reg := TRegistry.Create(KEY_READ);
  DeviceList := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    
    if Reg.OpenKeyReadOnly(J2534_REGISTRY_PATH) then
    begin
      Reg.GetKeyNames(DeviceList);
      
      for I := 0 to DeviceList.Count - 1 do
      begin
        if Reg.OpenKeyReadOnly(J2534_REGISTRY_PATH + '\' + DeviceList[I]) then
        begin
          DeviceInfo.Name := DeviceList[I];
          DeviceInfo.Vendor := Reg.ReadString('Vendor');
          DeviceInfo.LibraryPath := Reg.ReadString('FunctionLibrary');
          DeviceInfo.CANSupport := Reg.ValueExists('CAN');
          DeviceInfo.ISO15765Support := Reg.ValueExists('ISO15765');
          DeviceInfo.J1850Support := Reg.ValueExists('J1850PWM') or Reg.ValueExists('J1850VPW');
          DeviceInfo.ISO9141Support := Reg.ValueExists('ISO9141');
          DeviceInfo.ISO14230Support := Reg.ValueExists('ISO14230');
          
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := DeviceInfo;
          
          Reg.CloseKey;
        end;
      end;
    end;
  finally
    DeviceList.Free;
    Reg.Free;
  end;
end;

//------------------------------------------------------------------------------
// GET DEVICE BY NAME
//------------------------------------------------------------------------------
class function TJ2534DeviceScanner.GetDeviceByName(const Name: string): TJ2534DeviceInfo;
var
  Devices: TArray<TJ2534DeviceInfo>;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Devices := ScanDevices;
  
  for I := 0 to High(Devices) do
  begin
    if SameText(Devices[I].Name, Name) then
    begin
      Result := Devices[I];
      Break;
    end;
  end;
end;

end.
