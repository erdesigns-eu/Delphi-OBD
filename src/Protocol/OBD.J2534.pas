//------------------------------------------------------------------------------
//  OBD.J2534
//
//  SAE J2534 PassThru host stack. Loads a vendor PassThru DLL
//  at runtime, enumerates devices, opens / connects channels,
//  reads / writes messages, and maps J2534 error codes to
//  Delphi exceptions.
//
//  Supports the union of J2534 v04.04 and v05.00 entry points:
//
//    v04.04 (universally implemented):
//      PassThruOpen, PassThruClose, PassThruConnect,
//      PassThruDisconnect, PassThruReadMsgs, PassThruWriteMsgs,
//      PassThruStartPeriodicMsg, PassThruStopPeriodicMsg,
//      PassThruStartMsgFilter, PassThruStopMsgFilter,
//      PassThruSetProgrammingVoltage, PassThruReadVersion,
//      PassThruGetLastError, PassThruIoctl
//
//    v05.00 additions (best-effort - missing symbols silently
//    disabled at load time, host queries with HasV05):
//      PassThruScanForDevices, PassThruGetNextDevice,
//      PassThruLogicalConnect, PassThruLogicalDisconnect,
//      PassThruSelect, PassThruQueueMsgs
//
//  Device discovery:
//    Windows: enumerate HKLM\Software\PassThruSupport.04.04 and
//             HKLM\Software\WOW6432Node\PassThruSupport.04.04 to
//             find each installed vendor's DLL path.
//    POSIX  : scan a configurable list of directories for *.so
//             files matching the J2534 ABI (best-effort - no
//             standard registry equivalent on Linux / macOS).
//
//  Threading:
//    The DLL functions are blocking. The host should drive each
//    channel from a single thread or wrap the calls in an
//    application-level mutex.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial v04.04 + v05.00 implementation.
//------------------------------------------------------------------------------

unit OBD.J2534;

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'OBD.J2534 currently supports Windows only - POSIX support is a follow-up.'}
{$ENDIF}
{$DEFINE J2534_REGISTRY}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,
  System.Generics.Collections,
  Winapi.Windows,
  System.Win.Registry;

type
  EOBDJ2534Error = class(Exception)
  strict private
    FErrorCode: Integer;
  public
    constructor Create(AErrorCode: Integer; const AMsg: string);
    property ErrorCode: Integer read FErrorCode;
  end;

  /// <summary>One installed PassThru DLL discovered on the
  /// host.</summary>
  TJ2534DeviceInfo = record
    VendorName: string;
    DllPath:    string;
    /// <summary>Free-form description from the registry / file
    /// metadata.</summary>
    Description: string;
  end;

  /// <summary>One J2534 protocol selector. The numeric values
  /// match the SAE J2534 spec.</summary>
  TJ2534Protocol = (
    jpJ1850VPW         = 1,
    jpJ1850PWM         = 2,
    jpISO9141          = 3,
    jpISO14230         = 4,
    jpCAN              = 5,
    jpISO15765         = 6,
    jpSCI_A_ENGINE     = 7,
    jpSCI_A_TRANS      = 8,
    jpSCI_B_ENGINE     = 9,
    jpSCI_B_TRANS      = 10,
    // v05 additions:
    jpSWCAN_PS         = $00008000,
    jpSWCAN_ISO15765   = $00009000
  );

  /// <summary>One J2534 message. Mirrors PASSTHRU_MSG.</summary>
  TJ2534Msg = packed record
    ProtocolID:    Cardinal;
    RxStatus:      Cardinal;
    TxFlags:       Cardinal;
    Timestamp:     Cardinal;
    DataSize:      Cardinal;
    ExtraDataIdx:  Cardinal;
    Data:          array[0..4127] of Byte;
  end;
  PJ2534Msg = ^TJ2534Msg;

  /// <summary>v05.00 SDEVICE structure for ScanForDevices /
  /// GetNextDevice.</summary>
  TJ2534SDevice = packed record
    DeviceName: array[0..79] of AnsiChar;
    DeviceAvailable: Cardinal;
    DeviceDLLFWStatus: Cardinal;
    DeviceConnectMedia: Cardinal;
    DeviceConnectSpeed: Cardinal;
    DeviceSignalQuality: Cardinal;
    DeviceSignalStrength: Cardinal;
  end;
  PJ2534SDevice = ^TJ2534SDevice;

  TJ2534DeviceId  = Cardinal;
  TJ2534ChannelId = Cardinal;
  TJ2534FilterId  = Cardinal;

  // ---- DLL function pointer types (subset shown - full set
  // declared in TJ2534Driver) ---------------------------------
  TPassThruOpen          = function(pName: PAnsiChar;
    var pDeviceID: TJ2534DeviceId): Integer; cdecl;
  TPassThruClose         = function(DeviceID: TJ2534DeviceId): Integer; cdecl;
  TPassThruConnect       = function(DeviceID: TJ2534DeviceId;
    ProtocolID, Flags, BaudRate: Cardinal;
    var pChannelID: TJ2534ChannelId): Integer; cdecl;
  TPassThruDisconnect    = function(ChannelID: TJ2534ChannelId): Integer; cdecl;
  TPassThruReadMsgs      = function(ChannelID: TJ2534ChannelId;
    pMsg: PJ2534Msg; var pNumMsgs: Cardinal;
    Timeout: Cardinal): Integer; cdecl;
  TPassThruWriteMsgs     = function(ChannelID: TJ2534ChannelId;
    pMsg: PJ2534Msg; var pNumMsgs: Cardinal;
    Timeout: Cardinal): Integer; cdecl;
  TPassThruStartPeriodicMsg = function(ChannelID: TJ2534ChannelId;
    pMsg: PJ2534Msg; var pMsgID: Cardinal;
    TimeInterval: Cardinal): Integer; cdecl;
  TPassThruStopPeriodicMsg  = function(ChannelID: TJ2534ChannelId;
    MsgID: Cardinal): Integer; cdecl;
  TPassThruStartMsgFilter   = function(ChannelID: TJ2534ChannelId;
    FilterType: Cardinal;
    pMaskMsg, pPatternMsg, pFlowControlMsg: PJ2534Msg;
    var pFilterID: TJ2534FilterId): Integer; cdecl;
  TPassThruStopMsgFilter    = function(ChannelID: TJ2534ChannelId;
    FilterID: TJ2534FilterId): Integer; cdecl;
  TPassThruSetProgrammingVoltage = function(DeviceID: TJ2534DeviceId;
    PinNumber, Voltage: Cardinal): Integer; cdecl;
  TPassThruReadVersion   = function(DeviceID: TJ2534DeviceId;
    pFirmwareVersion, pDLLVersion, pAPIVersion: PAnsiChar): Integer; cdecl;
  TPassThruGetLastError  = function(pErrorDescription: PAnsiChar): Integer; cdecl;
  TPassThruIoctl         = function(ChannelID: TJ2534ChannelId;
    IoctlID: Cardinal; pInput, pOutput: Pointer): Integer; cdecl;
  // v05 additions
  TPassThruScanForDevices  = function(var pDeviceCount: Cardinal): Integer; cdecl;
  TPassThruGetNextDevice   = function(pDevice: PJ2534SDevice): Integer; cdecl;
  TPassThruLogicalConnect  = function(PhysicalChannelID: TJ2534ChannelId;
    ProtocolID, Flags: Cardinal; pDescriptor: Pointer;
    var pChannelID: TJ2534ChannelId): Integer; cdecl;
  TPassThruLogicalDisconnect = function(ChannelID: TJ2534ChannelId): Integer; cdecl;
  TPassThruSelect          = function(pSChannelSet: Pointer;
    SelectType: Cardinal; Timeout: Cardinal): Integer; cdecl;
  TPassThruQueueMsgs       = function(ChannelID: TJ2534ChannelId;
    pMsg: PJ2534Msg; var pNumMsgs: Cardinal): Integer; cdecl;

  /// <summary>Wraps a vendor PassThru DLL. One instance per
  /// loaded DLL; multiple devices per instance via
  /// TJ2534Device.</summary>
  TJ2534Driver = class
  strict private
    FDllPath: string;
    FHandle:  THandle;
    FOpen:    TPassThruOpen;
    FClose:   TPassThruClose;
    FConnect: TPassThruConnect;
    FDisconnect: TPassThruDisconnect;
    FReadMsgs:   TPassThruReadMsgs;
    FWriteMsgs:  TPassThruWriteMsgs;
    FStartPeriodic: TPassThruStartPeriodicMsg;
    FStopPeriodic:  TPassThruStopPeriodicMsg;
    FStartFilter:   TPassThruStartMsgFilter;
    FStopFilter:    TPassThruStopMsgFilter;
    FSetProgVoltage: TPassThruSetProgrammingVoltage;
    FReadVersion:    TPassThruReadVersion;
    FGetLastError:   TPassThruGetLastError;
    FIoctl:          TPassThruIoctl;
    FScanForDevices: TPassThruScanForDevices;
    FGetNextDevice:  TPassThruGetNextDevice;
    FLogicalConnect: TPassThruLogicalConnect;
    FLogicalDisconnect: TPassThruLogicalDisconnect;
    FSelect:         TPassThruSelect;
    FQueueMsgs:      TPassThruQueueMsgs;
    FHasV05:         Boolean;
    function  Resolve(const AName: string;
      ARequired: Boolean = True): Pointer;
    procedure CheckError(ARC: Integer; const AContext: string);
  public
    constructor Create(const ADllPath: string);
    destructor  Destroy; override;

    /// <summary>True when the loaded DLL exports at least one
    /// v05.00 entry point.</summary>
    property HasV05: Boolean read FHasV05;
    property DllPath: string read FDllPath;

    // Thin wrappers that map to the DLL exports and translate
    // errors. Hosts usually use TJ2534Channel rather than these.
    function Open(const APortName: string): TJ2534DeviceId;
    procedure Close(ADeviceId: TJ2534DeviceId);
    function Connect(ADeviceId: TJ2534DeviceId;
      AProtocol: TJ2534Protocol; AFlags, ABaud: Cardinal): TJ2534ChannelId;
    procedure Disconnect(AChannelId: TJ2534ChannelId);
    function ReadMsgs(AChannelId: TJ2534ChannelId;
      AMsgs: PJ2534Msg; var ANumMsgs: Cardinal;
      ATimeoutMs: Cardinal): Boolean;
    procedure WriteMsgs(AChannelId: TJ2534ChannelId;
      AMsgs: PJ2534Msg; var ANumMsgs: Cardinal;
      ATimeoutMs: Cardinal);
    function StartFilter(AChannelId: TJ2534ChannelId;
      AFilterType: Cardinal;
      AMaskMsg, APatternMsg, AFlowControlMsg: PJ2534Msg): TJ2534FilterId;
    procedure StopFilter(AChannelId: TJ2534ChannelId;
      AFilterId: TJ2534FilterId);
    procedure SetProgrammingVoltage(ADeviceId: TJ2534DeviceId;
      APin: Cardinal; AVolts: Cardinal);
    function GetLastErrorText: string;
    procedure Ioctl(AChannelId: TJ2534ChannelId; AIoctlId: Cardinal;
      AInput, AOutput: Pointer);

    /// <summary>v05 only. Scans for dynamically discoverable
    /// devices on the bus (USB-CAN, BT, etc) and returns their
    /// SDEVICE descriptors.</summary>
    function ScanForDevices: TArray<TJ2534SDevice>;
  end;

  /// <summary>One open J2534 channel; helper around the raw
  /// driver functions that owns the device + channel lifetime.</summary>
  TJ2534Channel = class
  strict private
    FDriver:    TJ2534Driver;
    FOwnsDriver: Boolean;
    FDeviceId:  TJ2534DeviceId;
    FChannelId: TJ2534ChannelId;
    FProtocol:  TJ2534Protocol;
    FFilters:   TList<TJ2534FilterId>;
  public
    constructor Open(ADriver: TJ2534Driver;
      AProtocol: TJ2534Protocol;
      ABaud: Cardinal;
      AFlags: Cardinal = 0;
      const APortName: string = '');
    destructor Destroy; override;

    /// <summary>Sends one message; returns when the DLL
    /// reports it queued.</summary>
    procedure WriteMsg(const AData: TBytes;
      ATxFlags: Cardinal = 0; ATimeoutMs: Cardinal = 1000);

    /// <summary>Reads up to one message. Returns @True with
    /// AData populated, or @False on timeout.</summary>
    function TryReadMsg(out AData: TBytes;
      ATimeoutMs: Cardinal = 1000): Boolean;

    /// <summary>Installs a pass-filter for AId. Stops the
    /// filter at channel close.</summary>
    procedure InstallPassFilter(AId: Cardinal;
      AExtended: Boolean = False);

    property Driver:    TJ2534Driver  read FDriver;
    property Protocol:  TJ2534Protocol read FProtocol;
    property ChannelId: TJ2534ChannelId read FChannelId;
    property DeviceId:  TJ2534DeviceId  read FDeviceId;
  end;

/// <summary>Scans the host for installed PassThru DLLs.
/// Windows uses the registry; POSIX scans the supplied
/// directories.</summary>
function EnumeratePassThruDevices(
  const AExtraDirs: TArray<string> = nil): TArray<TJ2534DeviceInfo>;

const
  // J2534 error codes (subset)
  J2534_STATUS_NOERROR              = $00;
  J2534_ERR_NOT_SUPPORTED           = $01;
  J2534_ERR_INVALID_CHANNEL_ID      = $02;
  J2534_ERR_INVALID_PROTOCOL_ID     = $03;
  J2534_ERR_NULL_PARAMETER          = $04;
  J2534_ERR_INVALID_IOCTL_VALUE     = $05;
  J2534_ERR_INVALID_FLAGS           = $06;
  J2534_ERR_FAILED                  = $07;
  J2534_ERR_DEVICE_NOT_CONNECTED    = $08;
  J2534_ERR_TIMEOUT                 = $09;
  J2534_ERR_INVALID_MSG             = $0A;
  J2534_ERR_INVALID_TIME_INTERVAL   = $0B;
  J2534_ERR_EXCEEDED_LIMIT          = $0C;
  J2534_ERR_INVALID_MSG_ID          = $0D;
  J2534_ERR_DEVICE_IN_USE           = $0E;
  J2534_ERR_INVALID_IOCTL_ID        = $0F;
  J2534_ERR_BUFFER_EMPTY            = $10;
  J2534_ERR_BUFFER_FULL             = $11;
  J2534_ERR_BUFFER_OVERFLOW         = $12;
  J2534_ERR_PIN_INVALID             = $13;
  J2534_ERR_CHANNEL_IN_USE          = $14;
  J2534_ERR_MSG_PROTOCOL_ID         = $15;
  J2534_ERR_INVALID_FILTER_ID       = $16;
  J2534_ERR_NO_FLOW_CONTROL         = $17;
  J2534_ERR_NOT_UNIQUE              = $18;
  J2534_ERR_INVALID_BAUDRATE        = $19;
  J2534_ERR_INVALID_DEVICE_ID       = $1A;

  // Filter types
  J2534_FILTER_PASS    = 1;
  J2534_FILTER_BLOCK   = 2;
  J2534_FILTER_FLOW_CONTROL = 3;

  // TX flag bits
  J2534_TX_ISO15765_FRAME_PAD = $00000040;
  J2534_TX_ISO15765_CAN_29BIT = $00000100;

implementation

{ EOBDJ2534Error --------------------------------------------------------------}

constructor EOBDJ2534Error.Create(AErrorCode: Integer; const AMsg: string);
begin
  inherited Create(AMsg);
  FErrorCode := AErrorCode;
end;

{ TJ2534Driver ----------------------------------------------------------------}

constructor TJ2534Driver.Create(const ADllPath: string);
begin
  inherited Create;
  FDllPath := ADllPath;
  FHandle  := LoadLibrary(PChar(ADllPath));
  if FHandle = 0 then
    raise EOBDJ2534Error.Create(-1,
      'TJ2534Driver: could not load PassThru DLL "' + ADllPath + '"');

  FOpen          := Resolve('PassThruOpen');
  FClose         := Resolve('PassThruClose');
  FConnect       := Resolve('PassThruConnect');
  FDisconnect    := Resolve('PassThruDisconnect');
  FReadMsgs      := Resolve('PassThruReadMsgs');
  FWriteMsgs     := Resolve('PassThruWriteMsgs');
  FStartPeriodic := Resolve('PassThruStartPeriodicMsg', False);
  FStopPeriodic  := Resolve('PassThruStopPeriodicMsg', False);
  FStartFilter   := Resolve('PassThruStartMsgFilter');
  FStopFilter    := Resolve('PassThruStopMsgFilter');
  FSetProgVoltage := Resolve('PassThruSetProgrammingVoltage', False);
  FReadVersion   := Resolve('PassThruReadVersion', False);
  FGetLastError  := Resolve('PassThruGetLastError');
  FIoctl         := Resolve('PassThruIoctl', False);

  FScanForDevices := Resolve('PassThruScanForDevices', False);
  FGetNextDevice  := Resolve('PassThruGetNextDevice',  False);
  FLogicalConnect := Resolve('PassThruLogicalConnect', False);
  FLogicalDisconnect := Resolve('PassThruLogicalDisconnect', False);
  FSelect         := Resolve('PassThruSelect',         False);
  FQueueMsgs      := Resolve('PassThruQueueMsgs',      False);
  FHasV05 := Assigned(FScanForDevices) or
             Assigned(FLogicalConnect);
end;

destructor TJ2534Driver.Destroy;
begin
  if FHandle <> 0 then
    FreeLibrary(FHandle);
  inherited;
end;

function TJ2534Driver.Resolve(const AName: string;
  ARequired: Boolean): Pointer;
begin
  Result := GetProcAddress(FHandle, PChar(AName));
  if (Result = nil) and ARequired then
    raise EOBDJ2534Error.Create(-1,
      'TJ2534Driver: required export "' + AName + '" missing in DLL');
end;

procedure TJ2534Driver.CheckError(ARC: Integer; const AContext: string);
var Msg: string;
begin
  if ARC = J2534_STATUS_NOERROR then Exit;
  Msg := AContext + ' failed (J2534 rc=' + IntToStr(ARC) + ')';
  if Assigned(FGetLastError) then
    Msg := Msg + ': ' + GetLastErrorText;
  raise EOBDJ2534Error.Create(ARC, Msg);
end;

function TJ2534Driver.GetLastErrorText: string;
var Buf: array[0..255] of AnsiChar;
begin
  if not Assigned(FGetLastError) then Exit('');
  FillChar(Buf, SizeOf(Buf), 0);
  if FGetLastError(@Buf[0]) = J2534_STATUS_NOERROR then
    Result := string(AnsiString(Buf))
  else
    Result := '';
end;

function TJ2534Driver.Open(const APortName: string): TJ2534DeviceId;
var
  Name: AnsiString;
  P: PAnsiChar;
begin
  Name := AnsiString(APortName);
  if Name = '' then P := nil else P := PAnsiChar(Name);
  CheckError(FOpen(P, Result), 'PassThruOpen');
end;

procedure TJ2534Driver.Close(ADeviceId: TJ2534DeviceId);
begin
  CheckError(FClose(ADeviceId), 'PassThruClose');
end;

function TJ2534Driver.Connect(ADeviceId: TJ2534DeviceId;
  AProtocol: TJ2534Protocol; AFlags, ABaud: Cardinal): TJ2534ChannelId;
begin
  CheckError(FConnect(ADeviceId, Cardinal(AProtocol), AFlags, ABaud, Result),
    'PassThruConnect');
end;

procedure TJ2534Driver.Disconnect(AChannelId: TJ2534ChannelId);
begin
  CheckError(FDisconnect(AChannelId), 'PassThruDisconnect');
end;

function TJ2534Driver.ReadMsgs(AChannelId: TJ2534ChannelId;
  AMsgs: PJ2534Msg; var ANumMsgs: Cardinal;
  ATimeoutMs: Cardinal): Boolean;
var RC: Integer;
begin
  RC := FReadMsgs(AChannelId, AMsgs, ANumMsgs, ATimeoutMs);
  if RC = J2534_ERR_BUFFER_EMPTY then Exit(False);
  CheckError(RC, 'PassThruReadMsgs');
  Result := True;
end;

procedure TJ2534Driver.WriteMsgs(AChannelId: TJ2534ChannelId;
  AMsgs: PJ2534Msg; var ANumMsgs: Cardinal;
  ATimeoutMs: Cardinal);
begin
  CheckError(FWriteMsgs(AChannelId, AMsgs, ANumMsgs, ATimeoutMs),
    'PassThruWriteMsgs');
end;

function TJ2534Driver.StartFilter(AChannelId: TJ2534ChannelId;
  AFilterType: Cardinal;
  AMaskMsg, APatternMsg, AFlowControlMsg: PJ2534Msg): TJ2534FilterId;
begin
  CheckError(FStartFilter(AChannelId, AFilterType,
    AMaskMsg, APatternMsg, AFlowControlMsg, Result),
    'PassThruStartMsgFilter');
end;

procedure TJ2534Driver.StopFilter(AChannelId: TJ2534ChannelId;
  AFilterId: TJ2534FilterId);
begin
  CheckError(FStopFilter(AChannelId, AFilterId), 'PassThruStopMsgFilter');
end;

procedure TJ2534Driver.SetProgrammingVoltage(ADeviceId: TJ2534DeviceId;
  APin: Cardinal; AVolts: Cardinal);
begin
  if not Assigned(FSetProgVoltage) then
    raise EOBDJ2534Error.Create(J2534_ERR_NOT_SUPPORTED,
      'TJ2534Driver: PassThruSetProgrammingVoltage not exported');
  CheckError(FSetProgVoltage(ADeviceId, APin, AVolts),
    'PassThruSetProgrammingVoltage');
end;

procedure TJ2534Driver.Ioctl(AChannelId: TJ2534ChannelId;
  AIoctlId: Cardinal; AInput, AOutput: Pointer);
begin
  if not Assigned(FIoctl) then
    raise EOBDJ2534Error.Create(J2534_ERR_NOT_SUPPORTED,
      'TJ2534Driver: PassThruIoctl not exported');
  CheckError(FIoctl(AChannelId, AIoctlId, AInput, AOutput),
    'PassThruIoctl');
end;

function TJ2534Driver.ScanForDevices: TArray<TJ2534SDevice>;
var
  Count: Cardinal;
  Acc:   TList<TJ2534SDevice>;
  Dev:   TJ2534SDevice;
  I:     Integer;
begin
  if not Assigned(FScanForDevices) then
    raise EOBDJ2534Error.Create(J2534_ERR_NOT_SUPPORTED,
      'TJ2534Driver: ScanForDevices not supported (v04 DLL)');
  Count := 0;
  CheckError(FScanForDevices(Count), 'PassThruScanForDevices');
  Acc := TList<TJ2534SDevice>.Create;
  try
    for I := 0 to Integer(Count) - 1 do
    begin
      FillChar(Dev, SizeOf(Dev), 0);
      CheckError(FGetNextDevice(@Dev), 'PassThruGetNextDevice');
      Acc.Add(Dev);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

{ TJ2534Channel ---------------------------------------------------------------}

constructor TJ2534Channel.Open(ADriver: TJ2534Driver;
  AProtocol: TJ2534Protocol; ABaud: Cardinal; AFlags: Cardinal;
  const APortName: string);
begin
  inherited Create;
  FDriver    := ADriver;
  FProtocol  := AProtocol;
  FFilters   := TList<TJ2534FilterId>.Create;
  FDeviceId  := FDriver.Open(APortName);
  FChannelId := FDriver.Connect(FDeviceId, AProtocol, AFlags, ABaud);
end;

destructor TJ2534Channel.Destroy;
var Id: TJ2534FilterId;
begin
  if Assigned(FFilters) then
  begin
    for Id in FFilters do
      try FDriver.StopFilter(FChannelId, Id); except end;
    FFilters.Free;
  end;
  if FChannelId <> 0 then
    try FDriver.Disconnect(FChannelId); except end;
  if FDeviceId <> 0 then
    try FDriver.Close(FDeviceId); except end;
  inherited;
end;

procedure TJ2534Channel.WriteMsg(const AData: TBytes;
  ATxFlags: Cardinal; ATimeoutMs: Cardinal);
var
  M: TJ2534Msg;
  N: Cardinal;
begin
  if Length(AData) > Length(M.Data) then
    raise EOBDJ2534Error.Create(J2534_ERR_INVALID_MSG,
      'TJ2534Channel.WriteMsg: payload exceeds 4128 bytes');
  FillChar(M, SizeOf(M), 0);
  M.ProtocolID := Cardinal(FProtocol);
  M.TxFlags    := ATxFlags;
  M.DataSize   := Cardinal(Length(AData));
  if Length(AData) > 0 then
    Move(AData[0], M.Data[0], Length(AData));
  N := 1;
  FDriver.WriteMsgs(FChannelId, @M, N, ATimeoutMs);
end;

function TJ2534Channel.TryReadMsg(out AData: TBytes;
  ATimeoutMs: Cardinal): Boolean;
var
  M: TJ2534Msg;
  N: Cardinal;
begin
  AData := nil;
  N := 1;
  Result := FDriver.ReadMsgs(FChannelId, @M, N, ATimeoutMs);
  if not Result then Exit;
  if N = 0 then Exit(False);
  SetLength(AData, M.DataSize);
  if M.DataSize > 0 then
    Move(M.Data[0], AData[0], M.DataSize);
end;

procedure TJ2534Channel.InstallPassFilter(AId: Cardinal;
  AExtended: Boolean);
var
  Mask, Pattern: TJ2534Msg;
  Fid: TJ2534FilterId;
  IdSize: Integer;
  TxFlags: Cardinal;
begin
  if AExtended then IdSize := 4 else IdSize := 4;  // same wire shape
  TxFlags := 0;
  if AExtended then TxFlags := TxFlags or J2534_TX_ISO15765_CAN_29BIT;

  FillChar(Mask, SizeOf(Mask), 0);
  Mask.ProtocolID := Cardinal(FProtocol);
  Mask.TxFlags    := TxFlags;
  Mask.DataSize   := IdSize;
  Mask.Data[0]    := $FF;
  Mask.Data[1]    := $FF;
  Mask.Data[2]    := $FF;
  Mask.Data[3]    := $FF;

  FillChar(Pattern, SizeOf(Pattern), 0);
  Pattern.ProtocolID := Cardinal(FProtocol);
  Pattern.TxFlags    := TxFlags;
  Pattern.DataSize   := IdSize;
  Pattern.Data[0]    := Byte(AId shr 24);
  Pattern.Data[1]    := Byte(AId shr 16);
  Pattern.Data[2]    := Byte(AId shr 8);
  Pattern.Data[3]    := Byte(AId);

  Fid := FDriver.StartFilter(FChannelId, J2534_FILTER_PASS,
    @Mask, @Pattern, nil);
  FFilters.Add(Fid);
end;

{ Device discovery ------------------------------------------------------------}

{$IFDEF J2534_REGISTRY}
procedure ScanRegistry(AReg: TRegistry; const ARoot: string;
  AOut: TList<TJ2534DeviceInfo>);
var
  Names: TStringList;
  Sub: string;
  Info: TJ2534DeviceInfo;
begin
  if not AReg.OpenKeyReadOnly(ARoot) then Exit;
  Names := TStringList.Create;
  try
    AReg.GetKeyNames(Names);
    AReg.CloseKey;
    for Sub in Names do
    begin
      if AReg.OpenKeyReadOnly(ARoot + '\' + Sub) then
      try
        Info := Default(TJ2534DeviceInfo);
        Info.VendorName  := AReg.ReadString('Vendor');
        Info.DllPath     := AReg.ReadString('FunctionLibrary');
        try
          Info.Description := AReg.ReadString('Name');
        except
          Info.Description := Sub;
        end;
        if Info.DllPath <> '' then
          AOut.Add(Info);
      finally
        AReg.CloseKey;
      end;
    end;
  finally
    Names.Free;
  end;
end;
{$ENDIF}

function EnumeratePassThruDevices(
  const AExtraDirs: TArray<string>): TArray<TJ2534DeviceInfo>;
var
  Acc: TList<TJ2534DeviceInfo>;
  Reg: TRegistry;
  Dir, F: string;
  Files: TStringDynArray;
  Info: TJ2534DeviceInfo;
begin
  Acc := TList<TJ2534DeviceInfo>.Create;
  try
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      ScanRegistry(Reg, 'SOFTWARE\PassThruSupport.04.04', Acc);
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.Access  := KEY_READ or KEY_WOW64_32KEY;
      ScanRegistry(Reg,
        'SOFTWARE\WOW6432Node\PassThruSupport.04.04', Acc);
    finally
      Reg.Free;
    end;
    // AExtraDirs: scan additional folders for raw DLLs (e.g.
    // when a vendor ships a portable build that wasn't
    // registered).
    for Dir in AExtraDirs do
    begin
      if not TDirectory.Exists(Dir) then Continue;
      Files := TDirectory.GetFiles(Dir, '*.dll');
      for F in Files do
      begin
        Info := Default(TJ2534DeviceInfo);
        Info.VendorName  := 'Unknown';
        Info.DllPath     := F;
        Info.Description := TPath.GetFileNameWithoutExtension(F);
        Acc.Add(Info);
      end;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

end.
