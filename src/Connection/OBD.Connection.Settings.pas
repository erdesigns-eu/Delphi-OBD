//------------------------------------------------------------------------------
//  OBD.Connection.Settings
//
//  Per-transport TPersistent sub-objects exposed by the TOBDConnection
//  component. Each sub-object is shown conditionally in the Object
//  Inspector based on the parent's Transport enum (see PLAN §3.4).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial: Serial, Bluetooth, BLE, Wi-Fi,
//                     UDP, FTDI settings.
//------------------------------------------------------------------------------

unit OBD.Connection.Settings;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Connection.Types;

type
  /// <summary>Settings for a Win32 / virtual serial port.</summary>
  TOBDSerialSettings = class(TOBDTransportSettings)
  strict private
    FPort: string;
    FBaudRate: TOBDBaudRate;
    FDataBits: Byte;
    FParity: TOBDParity;
    FStopBits: TOBDStopBits;
    FFlowControl: TOBDFlowControl;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
  public
    /// <summary>Creates the settings sub-object with default values.</summary>
    constructor Create;
    /// <summary>Copies all published properties from another instance
    /// of the same class. Falls through to <c>inherited</c> for any
    /// other source type.</summary>
    /// <param name="Source">Source instance.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Device name (<c>'COM3'</c>, <c>'\\.\COM12'</c>, or a
    /// platform path).</summary>
    property Port: string read FPort write FPort;
    /// <summary>Baud rate.</summary>
    property BaudRate: TOBDBaudRate read FBaudRate write FBaudRate
      default br38400;
    /// <summary>Data bits (5–8).</summary>
    property DataBits: Byte read FDataBits write FDataBits default 8;
    /// <summary>Parity.</summary>
    property Parity: TOBDParity read FParity write FParity default paNone;
    /// <summary>Stop bits.</summary>
    property StopBits: TOBDStopBits read FStopBits write FStopBits default sb1;
    /// <summary>Flow control.</summary>
    property FlowControl: TOBDFlowControl read FFlowControl
      write FFlowControl default fcNone;
    /// <summary>Read timeout (milliseconds).</summary>
    property ReadTimeout: Cardinal read FReadTimeout write FReadTimeout
      default 1000;
    /// <summary>Write timeout (milliseconds).</summary>
    property WriteTimeout: Cardinal read FWriteTimeout write FWriteTimeout
      default 1000;
  end;

  /// <summary>Settings for Bluetooth Classic (RFCOMM) / SPP.</summary>
  TOBDBluetoothSettings = class(TOBDTransportSettings)
  strict private
    FDeviceAddress: string;
    FServiceUUID: string;
    FConnectTimeout: Cardinal;
  public
    /// <summary>Creates the settings sub-object with default values.</summary>
    constructor Create;
    /// <summary>Copies all published properties from another instance
    /// of the same class. Falls through to <c>inherited</c> for any
    /// other source type.</summary>
    /// <param name="Source">Source instance.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Bluetooth MAC address of the adapter
    /// (<c>'00:1D:A5:00:12:34'</c>) or device name. Empty enables
    /// discovery on connect.</summary>
    property DeviceAddress: string read FDeviceAddress write FDeviceAddress;
    /// <summary>Target service UUID. Default
    /// <c>00001101-0000-1000-8000-00805F9B34FB</c> (Serial Port
    /// Profile).</summary>
    property ServiceUUID: string read FServiceUUID write FServiceUUID;
    /// <summary>Connect timeout in milliseconds.</summary>
    property ConnectTimeout: Cardinal read FConnectTimeout
      write FConnectTimeout default 10000;
  end;

  /// <summary>Settings for Bluetooth Low Energy (GATT).</summary>
  TOBDBLESettings = class(TOBDTransportSettings)
  strict private
    FDeviceAddress: string;
    FServiceUUID: string;
    FWriteCharUUID: string;
    FNotifyCharUUID: string;
    FConnectTimeout: Cardinal;
  public
    /// <summary>Creates the settings sub-object with default values.</summary>
    constructor Create;
    /// <summary>Copies all published properties from another instance
    /// of the same class. Falls through to <c>inherited</c> for any
    /// other source type.</summary>
    /// <param name="Source">Source instance.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>BLE MAC or device name. Empty enables scanning on
    /// connect.</summary>
    property DeviceAddress: string read FDeviceAddress write FDeviceAddress;
    /// <summary>Custom service UUID. Default <c>FFE0</c> (the de-facto
    /// ELM327-BLE clone profile).</summary>
    property ServiceUUID: string read FServiceUUID write FServiceUUID;
    /// <summary>Characteristic to write commands to. Default
    /// <c>FFE1</c>.</summary>
    property WriteCharUUID: string read FWriteCharUUID write FWriteCharUUID;
    /// <summary>Characteristic to subscribe to for notifications.
    /// Default <c>FFE1</c>.</summary>
    property NotifyCharUUID: string read FNotifyCharUUID write FNotifyCharUUID;
    /// <summary>Connect timeout in milliseconds.</summary>
    property ConnectTimeout: Cardinal read FConnectTimeout
      write FConnectTimeout default 15000;
  end;

  /// <summary>Settings for a TCP transport.</summary>
  TOBDWiFiSettings = class(TOBDTransportSettings)
  strict private
    FHost: string;
    FPort: Word;
    FConnectTimeout: Cardinal;
    FKeepAlive: Boolean;
  public
    /// <summary>Creates the settings sub-object with default values.</summary>
    constructor Create;
    /// <summary>Copies all published properties from another instance
    /// of the same class. Falls through to <c>inherited</c> for any
    /// other source type.</summary>
    /// <param name="Source">Source instance.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Host name or IP address (<c>'192.168.0.10'</c>,
    /// <c>'elm327.local'</c>).</summary>
    property Host: string read FHost write FHost;
    /// <summary>TCP port. Default 35000 (common ELM327 Wi-Fi clone).</summary>
    property Port: Word read FPort write FPort default 35000;
    /// <summary>Connect timeout in milliseconds.</summary>
    property ConnectTimeout: Cardinal read FConnectTimeout
      write FConnectTimeout default 5000;
    /// <summary>Enable TCP keepalive at the socket layer.</summary>
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive default True;
  end;

  /// <summary>Settings for a UDP transport (used primarily by DoIP).</summary>
  TOBDUDPSettings = class(TOBDTransportSettings)
  strict private
    FHost: string;
    FPort: Word;
    FBindLocal: Boolean;
    FLocalPort: Word;
    FBroadcast: Boolean;
  public
    /// <summary>Creates the settings sub-object with default values.</summary>
    constructor Create;
    /// <summary>Copies all published properties from another instance
    /// of the same class. Falls through to <c>inherited</c> for any
    /// other source type.</summary>
    /// <param name="Source">Source instance.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Remote host or broadcast address.</summary>
    property Host: string read FHost write FHost;
    /// <summary>Remote port.</summary>
    property Port: Word read FPort write FPort default 13400;
    /// <summary>Bind a specific local port. False uses an ephemeral
    /// port.</summary>
    property BindLocal: Boolean read FBindLocal write FBindLocal default False;
    /// <summary>Local bind port when <c>BindLocal</c> is True.</summary>
    property LocalPort: Word read FLocalPort write FLocalPort default 0;
    /// <summary>Allow broadcast sends (DoIP vehicle announcement).</summary>
    property Broadcast: Boolean read FBroadcast write FBroadcast default False;
  end;

  /// <summary>Settings for an FTDI USB-serial adapter via D2XX.</summary>
  /// <remarks>
  ///   The D2XX driver bypasses the COM stack — useful on Windows where
  ///   COM-port enumeration is unreliable for FT232 family chips.
  /// </remarks>
  TOBDFTDISettings = class(TOBDTransportSettings)
  strict private
    FSerialNumber: string;
    FDescription: string;
    FDeviceIndex: Integer;
    FBaudRate: TOBDBaudRate;
    FLatencyTimer: Byte;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
  public
    /// <summary>Creates the settings sub-object with default values.</summary>
    constructor Create;
    /// <summary>Copies all published properties from another instance
    /// of the same class. Falls through to <c>inherited</c> for any
    /// other source type.</summary>
    /// <param name="Source">Source instance.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>FTDI serial number (e.g. <c>'FT123ABC'</c>). Takes
    /// precedence over <c>DeviceIndex</c> when non-empty.</summary>
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    /// <summary>FTDI device description string. Optional secondary
    /// matcher.</summary>
    property Description: string read FDescription write FDescription;
    /// <summary>Zero-based index when serial number is empty. -1 means
    /// 'first device found'.</summary>
    property DeviceIndex: Integer read FDeviceIndex write FDeviceIndex
      default -1;
    /// <summary>Baud rate.</summary>
    property BaudRate: TOBDBaudRate read FBaudRate write FBaudRate
      default br115200;
    /// <summary>FTDI latency timer (1–255 ms). Lower = lower latency,
    /// higher CPU.</summary>
    property LatencyTimer: Byte read FLatencyTimer write FLatencyTimer
      default 16;
    /// <summary>Read timeout (milliseconds).</summary>
    property ReadTimeout: Cardinal read FReadTimeout write FReadTimeout
      default 1000;
    /// <summary>Write timeout (milliseconds).</summary>
    property WriteTimeout: Cardinal read FWriteTimeout write FWriteTimeout
      default 1000;
  end;

implementation

{ ---- TOBDSerialSettings ------------------------------------------------------ }

constructor TOBDSerialSettings.Create;
begin
  inherited;
  FBaudRate := br38400;
  FDataBits := 8;
  FParity := paNone;
  FStopBits := sb1;
  FFlowControl := fcNone;
  FReadTimeout := 1000;
  FWriteTimeout := 1000;
end;

procedure TOBDSerialSettings.Assign(Source: TPersistent);
var
  S: TOBDSerialSettings;
begin
  if Source is TOBDSerialSettings then
  begin
    S := TOBDSerialSettings(Source);
    FPort := S.FPort;
    FBaudRate := S.FBaudRate;
    FDataBits := S.FDataBits;
    FParity := S.FParity;
    FStopBits := S.FStopBits;
    FFlowControl := S.FFlowControl;
    FReadTimeout := S.FReadTimeout;
    FWriteTimeout := S.FWriteTimeout;
  end
  else
    inherited Assign(Source);
end;

{ ---- TOBDBluetoothSettings --------------------------------------------------- }

constructor TOBDBluetoothSettings.Create;
begin
  inherited;
  FServiceUUID := '00001101-0000-1000-8000-00805F9B34FB'; // SPP
  FConnectTimeout := 10000;
end;

procedure TOBDBluetoothSettings.Assign(Source: TPersistent);
var
  S: TOBDBluetoothSettings;
begin
  if Source is TOBDBluetoothSettings then
  begin
    S := TOBDBluetoothSettings(Source);
    FDeviceAddress := S.FDeviceAddress;
    FServiceUUID := S.FServiceUUID;
    FConnectTimeout := S.FConnectTimeout;
  end
  else
    inherited Assign(Source);
end;

{ ---- TOBDBLESettings --------------------------------------------------------- }

constructor TOBDBLESettings.Create;
begin
  inherited;
  // Common ELM327-BLE clone profile (FFE0 / FFE1).
  FServiceUUID    := '0000FFE0-0000-1000-8000-00805F9B34FB';
  FWriteCharUUID  := '0000FFE1-0000-1000-8000-00805F9B34FB';
  FNotifyCharUUID := '0000FFE1-0000-1000-8000-00805F9B34FB';
  FConnectTimeout := 15000;
end;

procedure TOBDBLESettings.Assign(Source: TPersistent);
var
  S: TOBDBLESettings;
begin
  if Source is TOBDBLESettings then
  begin
    S := TOBDBLESettings(Source);
    FDeviceAddress := S.FDeviceAddress;
    FServiceUUID := S.FServiceUUID;
    FWriteCharUUID := S.FWriteCharUUID;
    FNotifyCharUUID := S.FNotifyCharUUID;
    FConnectTimeout := S.FConnectTimeout;
  end
  else
    inherited Assign(Source);
end;

{ ---- TOBDWiFiSettings -------------------------------------------------------- }

constructor TOBDWiFiSettings.Create;
begin
  inherited;
  FPort := 35000;
  FConnectTimeout := 5000;
  FKeepAlive := True;
end;

procedure TOBDWiFiSettings.Assign(Source: TPersistent);
var
  S: TOBDWiFiSettings;
begin
  if Source is TOBDWiFiSettings then
  begin
    S := TOBDWiFiSettings(Source);
    FHost := S.FHost;
    FPort := S.FPort;
    FConnectTimeout := S.FConnectTimeout;
    FKeepAlive := S.FKeepAlive;
  end
  else
    inherited Assign(Source);
end;

{ ---- TOBDUDPSettings --------------------------------------------------------- }

constructor TOBDUDPSettings.Create;
begin
  inherited;
  FPort := 13400; // DoIP UDP discovery port (ISO 13400-2)
end;

procedure TOBDUDPSettings.Assign(Source: TPersistent);
var
  S: TOBDUDPSettings;
begin
  if Source is TOBDUDPSettings then
  begin
    S := TOBDUDPSettings(Source);
    FHost := S.FHost;
    FPort := S.FPort;
    FBindLocal := S.FBindLocal;
    FLocalPort := S.FLocalPort;
    FBroadcast := S.FBroadcast;
  end
  else
    inherited Assign(Source);
end;

{ ---- TOBDFTDISettings -------------------------------------------------------- }

constructor TOBDFTDISettings.Create;
begin
  inherited;
  FDeviceIndex := -1;
  FBaudRate := br115200;
  FLatencyTimer := 16;
  FReadTimeout := 1000;
  FWriteTimeout := 1000;
end;

procedure TOBDFTDISettings.Assign(Source: TPersistent);
var
  S: TOBDFTDISettings;
begin
  if Source is TOBDFTDISettings then
  begin
    S := TOBDFTDISettings(Source);
    FSerialNumber := S.FSerialNumber;
    FDescription := S.FDescription;
    FDeviceIndex := S.FDeviceIndex;
    FBaudRate := S.FBaudRate;
    FLatencyTimer := S.FLatencyTimer;
    FReadTimeout := S.FReadTimeout;
    FWriteTimeout := S.FWriteTimeout;
  end
  else
    inherited Assign(Source);
end;

end.
