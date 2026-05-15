//------------------------------------------------------------------------------
//  OBD.J2534.Components
//
//  Non-visual TComponent wrappers around TJ2534Driver and
//  TJ2534Channel. Drop on a form, set DllPath in the Object
//  Inspector, optionally pick a Protocol + BaudRate, call
//  Open. The components own the underlying driver / channel
//  lifecycle.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.J2534.Components;

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'OBD.J2534.Components is Windows-only.'}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.J2534;

type
  /// <summary>Non-visual wrapper around <see cref="TJ2534Driver"/>.
  /// Drop on a form, set <see cref="DllPath"/> (or pick from
  /// the EnumeratedDevices list), call <see cref="Open"/>.</summary>
  TOBDJ2534Device = class(TComponent)
  strict private
    FDllPath: string;
    FDriver:  TJ2534Driver;
    FAutoOpenOnLoad: Boolean;
  protected
    procedure Loaded; override;
  public
    destructor Destroy; override;

    /// <summary>Loads the DLL specified by
    /// <see cref="DllPath"/>. Idempotent - second call is a
    /// no-op.</summary>
    procedure Open;

    /// <summary>Unloads the DLL and disposes the driver.</summary>
    procedure Close;

    /// <summary>True after a successful <see cref="Open"/>.</summary>
    function IsOpen: Boolean;

    /// <summary>Pretty-printed list of every PassThru DLL
    /// registered on the host (Windows registry under
    /// HKLM\Software\PassThruSupport.04.04). Useful for
    /// populating a vendor-picker combo at design time.</summary>
    function EnumeratedDevices: TArray<TJ2534DeviceInfo>;

    /// <summary>True iff the loaded DLL implements at least
    /// one J2534 v05.00 entry point.</summary>
    function HasV05: Boolean;

    /// <summary>Direct access to the wrapped driver - hosts
    /// that need a method we haven't surfaced.</summary>
    property Driver: TJ2534Driver read FDriver;
  published
    /// <summary>Path to the vendor PassThru DLL.</summary>
    property DllPath: string read FDllPath write FDllPath;

    /// <summary>When True, the IDE Loaded handler attempts to
    /// open the DLL right after streaming the .dfm. Off by
    /// default - hosts typically open on demand.</summary>
    property AutoOpenOnLoad: Boolean
      read FAutoOpenOnLoad write FAutoOpenOnLoad default False;
  end;

  /// <summary>Non-visual wrapper around <see cref="TJ2534Channel"/>.
  /// Wire <see cref="Device"/> + pick <see cref="Protocol"/> /
  /// <see cref="BaudRate"/>, call <see cref="Open"/>.</summary>
  TOBDJ2534Channel = class(TComponent)
  strict private
    FDevice:    TOBDJ2534Device;
    FProtocol:  TJ2534Protocol;
    FBaudRate:  Cardinal;
    FFlags:     Cardinal;
    FPortName:  string;
    FChannel:   TJ2534Channel;
    procedure SetDevice(AValue: TOBDJ2534Device);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    function  IsOpen: Boolean;

    procedure WriteMsg(const AData: TBytes;
      ATxFlags: Cardinal = 0; ATimeoutMs: Cardinal = 1000);
    function  TryReadMsg(out AData: TBytes;
      ATimeoutMs: Cardinal = 1000): Boolean;
    procedure InstallPassFilter(AId: Cardinal;
      AExtended: Boolean = False);

    property Channel: TJ2534Channel read FChannel;
  published
    /// <summary>Reference to the device that owns this
    /// channel. Required.</summary>
    property Device: TOBDJ2534Device
      read FDevice write SetDevice;

    /// <summary>J2534 protocol id. Default
    /// <c>jpISO15765</c>.</summary>
    property Protocol: TJ2534Protocol
      read FProtocol write FProtocol default jpISO15765;

    /// <summary>Channel baud rate. 500_000 for CAN, 10_400 for
    /// ISO 9141, etc.</summary>
    property BaudRate: Cardinal
      read FBaudRate write FBaudRate default 500000;

    /// <summary>Per-channel flags passed to PassThruConnect.
    /// 0 by default; set bits for CAN_29BIT_ID etc.</summary>
    property Flags: Cardinal
      read FFlags write FFlags default 0;

    /// <summary>Port name passed to PassThruOpen. Empty
    /// (default) lets the DLL pick the device.</summary>
    property PortName: string
      read FPortName write FPortName;
  end;

implementation

{ TOBDJ2534Device -------------------------------------------------------------}

destructor TOBDJ2534Device.Destroy;
begin
  Close;
  inherited;
end;

procedure TOBDJ2534Device.Loaded;
begin
  inherited;
  if FAutoOpenOnLoad and (FDllPath <> '') then
    try Open; except end;
end;

procedure TOBDJ2534Device.Open;
begin
  if FDriver <> nil then Exit;
  if FDllPath = '' then
    raise EOBDJ2534Error.Create(-1,
      'TOBDJ2534Device: DllPath is not set');
  FDriver := TJ2534Driver.Create(FDllPath);
end;

procedure TOBDJ2534Device.Close;
begin
  FreeAndNil(FDriver);
end;

function TOBDJ2534Device.IsOpen: Boolean;
begin
  Result := FDriver <> nil;
end;

function TOBDJ2534Device.EnumeratedDevices: TArray<TJ2534DeviceInfo>;
begin
  Result := EnumeratePassThruDevices;
end;

function TOBDJ2534Device.HasV05: Boolean;
begin
  Result := IsOpen and FDriver.HasV05;
end;

{ TOBDJ2534Channel ------------------------------------------------------------}

constructor TOBDJ2534Channel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProtocol := jpISO15765;
  FBaudRate := 500000;
  FFlags    := 0;
end;

destructor TOBDJ2534Channel.Destroy;
begin
  Close;
  inherited;
end;

procedure TOBDJ2534Channel.SetDevice(AValue: TOBDJ2534Device);
begin
  if FDevice = AValue then Exit;
  if FDevice <> nil then FDevice.RemoveFreeNotification(Self);
  FDevice := AValue;
  if FDevice <> nil then FDevice.FreeNotification(Self);
end;

procedure TOBDJ2534Channel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDevice) then
  begin
    Close;
    FDevice := nil;
  end;
end;

procedure TOBDJ2534Channel.Open;
begin
  if FChannel <> nil then Exit;
  if FDevice = nil then
    raise EOBDJ2534Error.Create(-1,
      'TOBDJ2534Channel: Device not assigned');
  FDevice.Open;  // idempotent
  FChannel := TJ2534Channel.Open(FDevice.Driver, FProtocol,
                                  FBaudRate, FFlags, FPortName);
end;

procedure TOBDJ2534Channel.Close;
begin
  FreeAndNil(FChannel);
end;

function TOBDJ2534Channel.IsOpen: Boolean;
begin
  Result := FChannel <> nil;
end;

procedure TOBDJ2534Channel.WriteMsg(const AData: TBytes;
  ATxFlags: Cardinal; ATimeoutMs: Cardinal);
begin
  Open;
  FChannel.WriteMsg(AData, ATxFlags, ATimeoutMs);
end;

function TOBDJ2534Channel.TryReadMsg(out AData: TBytes;
  ATimeoutMs: Cardinal): Boolean;
begin
  Open;
  Result := FChannel.TryReadMsg(AData, ATimeoutMs);
end;

procedure TOBDJ2534Channel.InstallPassFilter(AId: Cardinal;
  AExtended: Boolean);
begin
  Open;
  FChannel.InstallPassFilter(AId, AExtended);
end;

end.
