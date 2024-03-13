//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.Enumerator.pas
// CONTENTS       : OBD Adapter Enumerator Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 13/03/2024
//------------------------------------------------------------------------------
unit OBD.Adapter.Enumerator;

interface

uses
  System.Classes, System.SysUtils, System.Bluetooth, System.Win.Registry, Winapi.Windows,

  OBD.Adapter.Types, OBD.Adapter.Constants, OBD.Connection.Constants, OBD.Connection.Types;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter Enum (INTERFACE)
  /// </summary>
  IOBDAdapterEnum = interface
    ['{160D49F7-B17A-42C7-9236-E37A431635F5}']
  end;

  /// <summary>
  ///   OBD Adapter Enumerator (INTERFACE)
  /// </summary>
  IOBDAdapterEnumerator = interface
    ['{6943A0ED-9057-4F95-9A05-142B755E9A1D}']
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter Enum (CLASS)
  /// </summary>
  TOBDAdapterEnum = class(TInterfacedObject, IOBDAdapterEnum)
  end;

  /// <summary>
  ///   OBD Adapter Enum - SERIAL (COM) PORT
  /// </summary>
  TSerialOBDAdapterEnum = class(TOBDAdapterEnum)
  private
    /// <summary>
    ///   COM Port (e.g. COM1, COM2, ..)
    /// </summary>
    FCOMPort: string;
    /// <summary>
    ///   Port friendlyname (e.g. USB Serial Port)
    /// </summary>
    FFriendlyName: string;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(COMPort: string; FriendlyName: string); virtual;

    /// <summary>
    ///   COM Port (e.g. COM1, COM2, ..)
    /// </summary>
    property COMPort: string read FCOMPort;
    /// <summary>
    ///   Port friendlyname (e.g. USB Serial Port)
    /// </summary>
    property FriendlyName: string read FFriendlyName;
  end;

  /// <summary>
  ///   OBD Adapter Enum - FTDI (USB)
  /// </summary>
  TFTDIOBDAdapterEnum = class(TOBDAdapterEnum)
  private
    /// <summary>
    ///   Serial Number
    /// </summary>
    FSerialNumber: string;
    /// <summary>
    ///   FTDI friendlyname (e.g. ELMScan 5 Compact)
    /// </summary>
    FFriendlyName: string;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(SerialNumber: string; FriendlyName: string); virtual;

    /// <summary>
    ///   Serial Number
    /// </summary>
    property SerialNumber: string read FSerialNumber;
    /// <summary>
    ///   FTDI friendlyname (e.g. ELMScan 5 Compact)
    /// </summary>
    property FriendlyName: string read FFriendlyName;
  end;

  /// <summary>
  ///   OBD Adapter Enum - BLUETOOTH
  /// </summary>
  TBluetoothOBDAdapterEnum = class(TOBDAdapterEnum)
  private
    /// <summary>
    ///   Device address
    /// </summary>
    FAddress: string;
    /// <summary>
    ///   FTDI friendlyname (e.g. OBDLink MX+)
    /// </summary>
    FFriendlyName: string;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Address: string; FriendlyName: string); virtual;

    /// <summary>
    ///   Device address
    /// </summary>
    property Address: string read FAddress;
    /// <summary>
    ///   FTDI friendlyname (e.g. OBDLink MX+)
    /// </summary>
    property FriendlyName: string read FFriendlyName;
  end;

  /// <summary>
  ///   OBD Adapter Enumerator (CLASS)
  /// </summary>
  TOBDAdapterEnumerator = class(TInterfacedObject, IOBDAdapterEnumerator)
  private
    /// <summary>
    ///   Bluetooth manager
    /// </summary>
    FBluetoothManager: TBluetoothManager;
    /// <summary>
    ///   Serial (COM) port adapters
    /// </summary>
    FSerialAdapters: TArray<TSerialOBDAdapterEnum>;
    /// <summary>
    ///   FTDI (USB) adapters
    /// </summary>
    FFTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
    /// <summary>
    ///   Bluetooth adapters
    /// </summary>
    FBluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;
  protected
    /// <summary>
    ///   Enumerate serial (COM) ports
    /// </summary>
    function EnumerateSerial: Boolean; virtual;
    /// <summary>
    ///   Enumertate FTDI devices
    /// </summary>
    function EnumerateFTDI: Boolean; virtual;
    /// <summary>
    ///   Enumerate bluetooth devices
    /// </summary>
    function EnumerateBluetooth: Boolean; virtual;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Refresh the lists
    /// </summary>
    function Refresh: Boolean;

    /// <summary>
    ///   Serial (COM) port adapters
    /// </summary>
    property SerialAdapters: TArray<TSerialOBDAdapterEnum> read FSerialAdapters;
    /// <summary>
    ///   FTDI (USB) adapters
    /// </summary>
    property FTDIAdapters: TArray<TFTDIOBDAdapterEnum> read FFTDIAdapters;
    /// <summary>
    ///   Bluetooth adapters
    /// </summary>
    property BluetoothAdapters: TArray<TBluetoothOBDAdapterEnum> read FBluetoothAdapters;
    /// <summary>
    ///   Bluetooth manager (Used for discovery and connecting)
    /// </summary>
    property BluetoothManager: TBluetoothManager read FBluetoothManager;
  end;

implementation

//------------------------------------------------------------------------------
// SERIAL (COM) PORT
//------------------------------------------------------------------------------
function SetupDiGetClassDevs(const ClassGuid: PGUID; Enumerator: PChar; hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall; external SETUP_API_DLL name 'SetupDiGetClassDevsA';
function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO; MemberIndex: DWORD; var DeviceInfoData: SP_DEVINFO_DATA): BOOL; stdcall; external SETUP_API_DLL name 'SetupDiEnumDeviceInfo';
function SetupDiGetDeviceRegistryProperty(DeviceInfoSet: HDEVINFO; const DeviceInfoData: SP_DEVINFO_DATA; PropertyReg: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall; external SETUP_API_DLL name 'SetupDiGetDeviceRegistryPropertyA';
function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): BOOL; stdcall; external SETUP_API_DLL name 'SetupDiDestroyDeviceInfoList';

//------------------------------------------------------------------------------
// FTDI
//------------------------------------------------------------------------------
function FT_CreateDeviceInfoList(NumDevs: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_CreateDeviceInfoList';
function FT_GetDeviceInfoList(pFT_Device_Info_List: Pointer; NumDevs: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_GetDeviceInfoList';

//------------------------------------------------------------------------------
// CONSTRUCTOR (SERIAL)
//------------------------------------------------------------------------------
constructor TSerialOBDAdapterEnum.Create(COMPort: string; FriendlyName: string);
begin
  // Call inherited constructor
  inherited Create;
  // Set COM port
  FCOMPort := COMPort;
  // Set friendly name
  FFriendlyName := FriendlyName;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR (FTDI)
//------------------------------------------------------------------------------
constructor TFTDIOBDAdapterEnum.Create(SerialNumber: string; FriendlyName: string);
begin
  // Call inherited constructor
  inherited Create;
  // Set the serial number
  FSerialNumber := SerialNumber;
  // Set friendly name
  FFriendlyName := FriendlyName;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR (BLUETOOTH)
//------------------------------------------------------------------------------
constructor TBluetoothOBDAdapterEnum.Create(Address: string; FriendlyName: string);
begin
  // Call inherited constructor
  inherited Create;
  // Set the device address
  FAddress := Address;
  // Set friendly name
  FFriendlyName := FriendlyName;
end;

//------------------------------------------------------------------------------
// ENUMERATE SERIAL (COM) PORTS
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.EnumerateSerial: Boolean;

  function ExtractCOMPort(const S: string): string;
  var
    StartPos, EndPos: Integer;
  begin
    // initialize result
    Result := '';
    // Find the start position
    StartPos := Pos('(COM', S);
    // Find the end position
    EndPos := Pos(')', S, StartPos);
    // Make sure we found the start and end positions
    if (StartPos > 0) and (EndPos > StartPos) then
    begin
      // Extract the COM port, not including the parentheses themselves
      Result := Copy(S, StartPos + 1, EndPos - StartPos - 1);
    end;
  end;

var
  DeviceInfoSet: HDEVINFO;
  DeviceInfoData: SP_DEVINFO_DATA;
  I: Integer;
  S, P, F: string;
  RequiredSize: DWORD;
  RegDataType: DWORD;
  Buffer: array[0..1023] of Byte;

begin
  // initialize result
  Result := False;
  // Clear list
  SetLength(FSerialAdapters, 0);

  // Get device information set
  DeviceInfoSet := SetupDiGetClassDevs(@GUID_DEVINTERFACE_COMPORT, nil, 0, DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  // Exit here if we fail to get the device information set
  if DeviceInfoSet = Pointer(INVALID_HANDLE_VALUE) then Exit;

  // Try to populate the list
  try
    I := 0;
    FillChar(DeviceInfoData, SizeOf(DeviceInfoData), 0);
    DeviceInfoData.cbSize := SizeOf(DeviceInfoData);
    while SetupDiEnumDeviceInfo(DeviceInfoSet, I, DeviceInfoData) do
    begin
      Inc(I);
      // Fill the buffer with zero's
      FillChar(Buffer, SizeOf(Buffer), 0);
      // Try to get the device info
      if SetupDiGetDeviceRegistryProperty(DeviceInfoSet, DeviceInfoData, SPDRP_FRIENDLYNAME, @RegDataType, @Buffer[0], SizeOf(Buffer), @RequiredSize) then
      begin
        // Set the length of the list
        SetLength(FSerialAdapters, Length(FSerialAdapters) + 1);
        // Copy the buffer to a string
        S := String(PAnsiChar(@Buffer[0]));
        // Extract the COM port
        P := ExtractCOMPort(S);
        // Extract the friendlyname
        F := Copy(S, 1, LastDelimiter('(', S) - 1);
        // Create adapter enum
        FSerialAdapters[I -1] := TSerialOBDAdapterEnum.Create(P, F);
      end;
    end;
  finally
    SetupDiDestroyDeviceInfoList(DeviceInfoSet);
  end;

  // If we make it here, we succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ENUMERATE FTDI ADAPTERS
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.EnumerateFTDI: Boolean;
var
  DeviceCount: DWORD;
  DeviceInfoList: array of FTDIDeviceNode;
  I: Integer;
begin
  // initialize result
  Result := False;
  // Clear list
  SetLength(FFTDIAdapters, 0);
  // Get the FTDI device count
  if FT_CreateDeviceInfoList(@DeviceCount) <> FT_OK then Exit;
  // Load FTDI device information
  if DeviceCount > 0 then
  begin
    // Set length for list containing FTDI devices information
    SetLength(DeviceInfoList, DeviceCount);
    // Try to populate the list
    if FT_GetDeviceInfoList(@DeviceInfoList[0], @DeviceCount) = FT_OK then
    begin
      // Set length of the list
      SetLength(FFTDIAdapters, DeviceCount);
      // Add FTDI devices to the list
      for I := 0 to DeviceCount - 1 do
      FFTDIAdapters[I] := TFTDIOBDAdapterEnum.Create(String(DeviceInfoList[I].SerialNumber), String(DeviceInfoList[I].Description));
      // If we make it here, we succeeded
      Result := True;
    end;
  end;
end;

//------------------------------------------------------------------------------
// ENUMERATE BLUETOOTH ADAPTERS
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.EnumerateBluetooth: Boolean;
var
  PairedDevices: TBluetoothDeviceList;
  I: Integer;
begin
  // Clear list
  SetLength(FBluetoothAdapters, 0);
  try
    // Get the list of paired devices
    PairedDevices := BluetoothManager.LastPairedDevices;
    // Set the length of the list
    SetLength(FBluetoothAdapters, PairedDevices.Count);
    // Add devices to the list
    for I := 0 to PairedDevices.Count -1 do
    FBluetoothAdapters[I] := TBluetoothOBDAdapterEnum.Create(PairedDevices[I].Address, PairedDevices[I].DeviceName);
    // If we make it here, we succeeded
    Result := True;
  except
    // Some error occured, probably due to no Bluetooth being available
    // on this computer or Bluetooth is not enabled.
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDAdapterEnumerator.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Load the bluetooth manager
  FBluetoothManager := TBluetoothManager.Current;
end;

//------------------------------------------------------------------------------
// REFRESH
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.Refresh: Boolean;
var
  S, F, B: Boolean;
begin
  // Refresh Serial (COM) Ports
  S := EnumerateSerial;
  // Refresh the FTDI devices
  F := EnumerateFTDI;
  // Refresh the Bluetooth devices
  B := EnumerateBluetooth;
  // Return true if enumerating all lists succeeded
  Result := S and F and B;
end;

end.
