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
  System.Classes, System.SysUtils, System.Bluetooth, System.Win.Registry,
  System.SyncObjs, System.Threading, Winapi.Windows, WinApi.Messages,
  Vcl.Forms, System.Generics.Defaults, System.Generics.Collections,

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
    /// <summary>
    ///   Starts a background refresh of all adapter lists and returns the running task reference.
    /// </summary>
    function BeginRefreshAsync: ITask;
    /// <summary>
    ///   Retrieves a thread-safe snapshot of serial (COM) adapters.
    /// </summary>
    function SnapshotSerialAdapters: TArray<TSerialOBDAdapterEnum>;
    /// <summary>
    ///   Retrieves a thread-safe snapshot of FTDI adapters.
    /// </summary>
    function SnapshotFTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
    /// <summary>
    ///   Retrieves a thread-safe snapshot of Bluetooth adapters.
    /// </summary>
    function SnapshotBluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;
    /// <summary>
    ///   Performs a synchronous refresh of all adapter lists.
    /// </summary>
    function Refresh: Boolean;
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
  ///   Comparer for sorting Serial (COM) ports
  /// </summary>
  TSerialOBDAdapterEnumComparer = class(TInterfacedObject, IComparer<TSerialOBDAdapterEnum>)
  public
    function Compare(const Left, Right: TSerialOBDAdapterEnum): Integer;
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
    ///   Handle for a hidden window. We need this to be able to
    ///   listen to usb device insert/removal events.
    /// </summary>
    FUSBHandle: HWND;
    /// <summary>
    ///   Event when new USB devices is added
    /// </summary>
    FOnUSBArrival: TNotifyEvent;
    /// <summary>
    ///   Event when USB device is removed
    /// </summary>
    FOnUSBRemoval: TNotifyEvent;
    /// <summary>
    ///   Event for both arrival/removal
    /// </summary>
    FOnUSBChanged: TNotifyEvent;

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
    /// <summary>
    ///   Lock object guarding adapter lists and refresh task state.
    /// </summary>
    FAdaptersLock: TObject;
    /// <summary>
    ///   Background refresh task reference.
    /// </summary>
    FRefreshTask: ITask;
    /// <summary>
    ///   Cancellation event for the refresh task.
    /// </summary>
    FRefreshCancelEvent: TEvent;
    
    /// <summary>
    ///   FTDI DLL Library Handle
    /// </summary>
    FFTDILibraryHandle: THandle;
    /// <summary>
    ///   FTDI Function Pointers
    /// </summary>
    FFT_CreateDeviceInfoList: function(NumDevs: Pointer): FT_Result; stdcall;
    FFT_GetDeviceInfoList: function(pFT_Device_Info_List: Pointer; NumDevs: Pointer): FT_Result; stdcall;
    
    /// <summary>
    ///   Load FTDI DLL dynamically
    /// </summary>
    function LoadFTDILibrary: Boolean;
    /// <summary>
    ///   Unload FTDI DLL
    /// </summary>
    procedure UnloadFTDILibrary;

    /// <summary>
    ///   Hidden window message handler
    /// </summary>
    procedure USBWndProc(var Msg: TMessage);
    /// <summary>
    ///   Copies a serial adapter array under lock for external consumers.
    /// </summary>
    function GetSerialAdapters: TArray<TSerialOBDAdapterEnum>;
    /// <summary>
    ///   Copies an FTDI adapter array under lock for external consumers.
    /// </summary>
    function GetFTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
    /// <summary>
    ///   Copies a Bluetooth adapter array under lock for external consumers.
    /// </summary>
    function GetBluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;
  protected
    /// <summary>
    ///   Is bluetooth available on the system?
    /// </summary>
    function BluetoothAvailable: Boolean;
    /// <summary>
    ///   Is bluetooth enabled on the system?
    /// </summary>
    function BluetoothEnabled: Boolean;
    /// <summary>
    ///   Enumerate serial (COM) ports
    /// </summary>
    function EnumerateSerial(out Adapters: TArray<TSerialOBDAdapterEnum>): Boolean; virtual;
    /// <summary>
    ///   Enumertate FTDI devices
    /// </summary>
    function EnumerateFTDI(out Adapters: TArray<TFTDIOBDAdapterEnum>): Boolean; virtual;
    /// <summary>
    ///   Enumerate bluetooth devices
    /// </summary>
    function EnumerateBluetooth(out Adapters: TArray<TBluetoothOBDAdapterEnum>): Boolean; virtual;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Refresh the lists
    /// </summary>
    function Refresh: Boolean;
    /// <summary>
    ///   Starts a background refresh of all adapter lists and returns the running task reference.
    /// </summary>
    function BeginRefreshAsync: ITask;
    /// <summary>
    ///   Retrieves a thread-safe snapshot of serial (COM) adapters.
    /// </summary>
    function SnapshotSerialAdapters: TArray<TSerialOBDAdapterEnum>;
    /// <summary>
    ///   Retrieves a thread-safe snapshot of FTDI adapters.
    /// </summary>
    function SnapshotFTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
    /// <summary>
    ///   Retrieves a thread-safe snapshot of Bluetooth adapters.
    /// </summary>
    function SnapshotBluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;

    /// <summary>
    ///   Event when new USB devices is added
    /// </summary>
    property OnUSBArrival: TNotifyEvent read FOnUSBArrival write FOnUSBArrival;
    /// <summary>
    ///   Event when USB device is removed
    /// </summary>
    property OnUSBRemoval: TNotifyEvent read FOnUSBRemoval write FOnUSBRemoval;
    /// <summary>
    ///   Event for both arrival/removal
    /// </summary>
    property OnUSBChanged: TNotifyEvent read FOnUSBChanged write FOnUSBChanged;

    /// <summary>
    ///   Serial (COM) port adapters
    /// </summary>
    property SerialAdapters: TArray<TSerialOBDAdapterEnum> read GetSerialAdapters;
    /// <summary>
    ///   FTDI (USB) adapters
    /// </summary>
    property FTDIAdapters: TArray<TFTDIOBDAdapterEnum> read GetFTDIAdapters;
    /// <summary>
    ///   Bluetooth adapters
    /// </summary>
    property BluetoothAdapters: TArray<TBluetoothOBDAdapterEnum> read GetBluetoothAdapters;
    /// <summary>
    ///   Bluetooth manager (Used for discovery and connecting)
    /// </summary>
    property BluetoothManager: TBluetoothManager read FBluetoothManager;
  end;

implementation

uses System.Math;

//------------------------------------------------------------------------------
// SERIAL
//------------------------------------------------------------------------------
function SetupDiGetClassDevs(const ClassGuid: PGUID; Enumerator: PChar; hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall; external SETUP_API_DLL name 'SetupDiGetClassDevsA';
function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO; MemberIndex: DWORD; var DeviceInfoData: SP_DEVINFO_DATA): BOOL; stdcall; external SETUP_API_DLL name 'SetupDiEnumDeviceInfo';
function SetupDiGetDeviceRegistryProperty(DeviceInfoSet: HDEVINFO; const DeviceInfoData: SP_DEVINFO_DATA; PropertyReg: DWORD; PropertyRegDataType: PDWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall; external SETUP_API_DLL name 'SetupDiGetDeviceRegistryPropertyA';
function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): BOOL; stdcall; external SETUP_API_DLL name 'SetupDiDestroyDeviceInfoList';

//------------------------------------------------------------------------------
// LOAD FTDI LIBRARY DYNAMICALLY
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.LoadFTDILibrary: Boolean;
begin
  Result := False;
  
  // Already loaded
  if FFTDILibraryHandle <> 0 then
    Exit(True);
  
  // Try to load the FTDI DLL
  FFTDILibraryHandle := Winapi.Windows.LoadLibrary(FTDI_DLL);
  if FFTDILibraryHandle = 0 then
    Exit;
  
  // Load function pointers
  @FFT_CreateDeviceInfoList := GetProcAddress(FFTDILibraryHandle, 'FT_CreateDeviceInfoList');
  @FFT_GetDeviceInfoList := GetProcAddress(FFTDILibraryHandle, 'FT_GetDeviceInfoList');
  
  // Check if all required functions were loaded
  Result := Assigned(FFT_CreateDeviceInfoList) and Assigned(FFT_GetDeviceInfoList);
  
  // If not all functions loaded, free the library
  if not Result then
  begin
    FreeLibrary(FFTDILibraryHandle);
    FFTDILibraryHandle := 0;
  end;
end;

//------------------------------------------------------------------------------
// UNLOAD FTDI LIBRARY
//------------------------------------------------------------------------------
procedure TOBDAdapterEnumerator.UnloadFTDILibrary;
begin
  if FFTDILibraryHandle <> 0 then
  begin
    FreeLibrary(FFTDILibraryHandle);
    FFTDILibraryHandle := 0;
    FFT_CreateDeviceInfoList := nil;
    FFT_GetDeviceInfoList := nil;
  end;
end;

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
// COMPARE SERIAL PORTS
//------------------------------------------------------------------------------
function TSerialOBDAdapterEnumComparer.Compare(const Left, Right: TSerialOBDAdapterEnum): Integer;

  function ExtractNumber(const Port: string): Integer;
  var
    I: Integer;
    S: string;
  begin
    // initialize string for holding the numbers
    S := '';
    // Loop over characters
    for I := 1 to Length(Port) do
    // If we have a number, add it to the result string
    if CharInSet(Port[I], ['0'..'9']) then S := S + Port[I];
    // Try to convert to a number, or default to zero
    Result := StrToIntDef(S, 0);
  end;

var
  LeftNum, RightNum: Integer;
begin
  // Try to extract the numeric part
  LeftNum := ExtractNumber(Left.COMPort);
  // Try to extract the numeric part
  RightNum := ExtractNumber(Right.COMPort);
  // Compare the numbers
  Result := CompareValue(LeftNum, RightNum);
  // If numerical parts are equal, compare the full port name to maintain consistent ordering
  if Result = 0 then Result := CompareStr(Left.COMPort, Right.COMPort);
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
// USB WND PROC
//------------------------------------------------------------------------------
procedure TOBDAdapterEnumerator.USBWndProc(var Msg: TMessage);
var
  DeviceType: Integer;
  Data: PDevBroadcastHdr;
begin
  // Only handle device change messages
  if (Msg.Msg = WM_DEVICECHANGE) then
  begin
    try
      // Handle device arrival and removal
      if (Msg.wParam = DBT_DEVICEARRIVAL) or (Msg.wParam = DBT_DEVICEREMOVECOMPLETE) then
      begin
        // Get Data
        Data := PDevBroadcastHdr(Msg.lParam);
        // Get device type
        DeviceType := Data^.dbch_devicetype;
        // Make sure we are handling events for the right device type (USB) only
        if (DeviceType = DBT_DEVTYP_DEVICEINTERFACE) then
        begin
          // Arrival
          if Msg.wParam = DBT_DEVICEARRIVAL then
          begin
            if Assigned(OnUSBArrival) then OnUSBArrival(Self);
          end else
          // Removal
          begin
            if Assigned(OnUSBRemoval) then OnUSBRemoval(Self);
          end;
          // Both
          if Assigned(OnUSBChanged) then OnUSBChanged(Self);
        end;
      end;
    except
      // When an error occurs, let the application handle the exception
      Application.HandleException(Self);
    end;
  end else
  // Let Windows handle other messages.
  Msg.Result := DefWindowProc(FUSBHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

//------------------------------------------------------------------------------
// SERIAL SNAPSHOT ACCESSOR
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.GetSerialAdapters: TArray<TSerialOBDAdapterEnum>;
begin
  // Guard access to the shared serial adapter list
  TMonitor.Enter(FAdaptersLock);
  try
    // Copy to isolate callers from concurrent mutations
    Result := Copy(FSerialAdapters, 0, Length(FSerialAdapters));
  finally
    // Release the lock
    TMonitor.Exit(FAdaptersLock);
  end;
end;

//------------------------------------------------------------------------------
// FTDI SNAPSHOT ACCESSOR
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.GetFTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
begin
  // Guard access to the shared FTDI adapter list
  TMonitor.Enter(FAdaptersLock);
  try
    // Copy to isolate callers from concurrent mutations
    Result := Copy(FFTDIAdapters, 0, Length(FFTDIAdapters));
  finally
    // Release the lock
    TMonitor.Exit(FAdaptersLock);
  end;
end;

//------------------------------------------------------------------------------
// BLUETOOTH SNAPSHOT ACCESSOR
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.GetBluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;
begin
  // Guard access to the shared Bluetooth adapter list
  TMonitor.Enter(FAdaptersLock);
  try
    // Copy to isolate callers from concurrent mutations
    Result := Copy(FBluetoothAdapters, 0, Length(FBluetoothAdapters));
  finally
    // Release the lock
    TMonitor.Exit(FAdaptersLock);
  end;
end;

//------------------------------------------------------------------------------
// IS BLUETOOTH AVAILABLE
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.BluetoothAvailable: Boolean;
begin
  try
    // Try to get the current adapter
    Result := Assigned(FBluetoothManager.CurrentAdapter);
  except
    // If we get a exception, return false
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// IS BLUETOOTH ENABLED
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.BluetoothEnabled: Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if there is no bluetooth available
  if not BluetoothAvailable then Exit;
  // Check if adapter is on
  Result := FBluetoothManager.CurrentAdapter.State = TBluetoothAdapterState.On;
end;

//------------------------------------------------------------------------------
// ENUMERATE SERIAL (COM) PORTS
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.EnumerateSerial(out Adapters: TArray<TSerialOBDAdapterEnum>): Boolean;

  function ExtractCOMPort(const S: string): string;
  var
    StartPos, EndPos: Integer;
  begin
    // initialize result
    Result := '';
    // Find the start position
    StartPos :=  LastDelimiter('(', S);
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
  Comparer: IComparer<TSerialOBDAdapterEnum>;
begin
  // initialize result
  Result := False;
  // Clear list
  SetLength(Adapters, 0);

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
        SetLength(Adapters, Length(Adapters) + 1);
        // Copy the buffer to a string
        S := String(PAnsiChar(@Buffer[0]));
        // Extract the COM port
        P := ExtractCOMPort(S);
        // Extract the friendlyname
        F := Copy(S, 1, LastDelimiter('(', S) - 1);
        // Create adapter enum
        Adapters[I -1] := TSerialOBDAdapterEnum.Create(P, F);
      end;
    end;
  finally
    SetupDiDestroyDeviceInfoList(DeviceInfoSet);
  end;

  // Create an instance of the comparer
  Comparer := TSerialOBDAdapterEnumComparer.Create;
  // Sort the ports ascending by port number
  TArray.Sort<TSerialOBDAdapterEnum>(Adapters, Comparer);

  // If we make it here, we succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ENUMERATE FTDI ADAPTERS
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.EnumerateFTDI(out Adapters: TArray<TFTDIOBDAdapterEnum>): Boolean;
var
  DeviceCount: DWORD;
  DeviceInfoList: array of FTDIDeviceNode;
  I: Integer;
begin
  // initialize result
  Result := False;
  // Clear list
  SetLength(Adapters, 0);
  
  // Load FTDI library if not already loaded
  if not LoadFTDILibrary then
    Exit; // FTDI library not available, silently return empty list
  
  // Get the FTDI device count using function pointer
  if not Assigned(FFT_CreateDeviceInfoList) or (FFT_CreateDeviceInfoList(@DeviceCount) <> FT_OK) then
    Exit;
    
  // Load FTDI device information
  if DeviceCount > 0 then
  begin
    // Set length for list containing FTDI devices information
    SetLength(DeviceInfoList, DeviceCount);
    // Try to populate the list using function pointer
    if Assigned(FFT_GetDeviceInfoList) and (FFT_GetDeviceInfoList(@DeviceInfoList[0], @DeviceCount) = FT_OK) then
    begin
      // Set length of the list
      SetLength(Adapters, DeviceCount);
      // Add FTDI devices to the list
      for I := 0 to DeviceCount - 1 do
        Adapters[I] := TFTDIOBDAdapterEnum.Create(String(DeviceInfoList[I].SerialNumber), String(DeviceInfoList[I].Description));
      // If we make it here, we succeeded
      Result := True;
    end;
  end;
end;

//------------------------------------------------------------------------------
// ENUMERATE BLUETOOTH ADAPTERS
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.EnumerateBluetooth(out Adapters: TArray<TBluetoothOBDAdapterEnum>): Boolean;
var
  PairedDevices: TBluetoothDeviceList;
  I: Integer;
begin
  // initialize result
  Result := False;
  // Clear list
  SetLength(Adapters, 0);
  if BluetoothAvailable and BluetoothEnabled then
  try
    // Get the list of paired devices
    PairedDevices := BluetoothManager.LastPairedDevices;
    // Set the length of the list
    SetLength(Adapters, PairedDevices.Count);
    // Add devices to the list
    for I := 0 to PairedDevices.Count -1 do
      Adapters[I] := TBluetoothOBDAdapterEnum.Create(PairedDevices[I].Address, PairedDevices[I].DeviceName);
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
var
  DBI: DEV_BROADCAST_DEVICEINTERFACE;
  Size: Integer;
begin
  // Call inherited constructor
  inherited Create;
  // Initialize FTDI library handle
  FFTDILibraryHandle := 0;
  // Create lock for guarding adapter state
  FAdaptersLock := TObject.Create;
  // Create cancellation event for background refresh
  FRefreshCancelEvent := TEvent.Create(nil, True, False, '');
  // Load the bluetooth manager
  FBluetoothManager := TBluetoothManager.Current;
  // Create the hidden window for USB arrival/removal
  FUSBHandle := AllocateHWnd(USBWndProc);
  // Register the USB listener
  Size := SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
  ZeroMemory(@DBI, Size);
  DBI.dbcc_size := Size;
  DBI.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  DBI.dbcc_reserved := 0;
  DBI.dbcc_classguid  := GUID_DEVINTERFACE_USB_DEVICE;
  DBI.dbcc_name := 0;
  RegisterDeviceNotification(FUSBHandle, @DBI, DEVICE_NOTIFY_WINDOW_HANDLE);
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDAdapterEnumerator.Destroy;
begin
  // Cancel any running refresh task
  if Assigned(FRefreshTask) then
  begin
    FRefreshCancelEvent.SetEvent;
    FRefreshTask.Wait;
  end;
  // Unload FTDI library if loaded
  UnloadFTDILibrary;
  // Release synchronization primitives
  FRefreshCancelEvent.Free;
  TObject.Free(FAdaptersLock);
  // Free hidden window
  DeallocateHWnd(FUSBHandle);
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// REFRESH
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.Refresh: Boolean;
var
  SerialAdapters: TArray<TSerialOBDAdapterEnum>;
  FTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
  BluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;
  S, F, B: Boolean;
begin
  // Refresh Serial (COM) Ports into a local buffer
  S := EnumerateSerial(SerialAdapters);
  // Refresh the FTDI devices into a local buffer
  F := EnumerateFTDI(FTDIAdapters);
  // Refresh the Bluetooth devices into a local buffer
  B := EnumerateBluetooth(BluetoothAdapters);
  // Publish results under lock to prevent races with consumers
  TMonitor.Enter(FAdaptersLock);
  try
    FSerialAdapters := SerialAdapters;
    FFTDIAdapters := FTDIAdapters;
    FBluetoothAdapters := BluetoothAdapters;
  finally
    TMonitor.Exit(FAdaptersLock);
  end;
  // Return true if enumerating all lists succeeded
  Result := S and F and B;
end;

//------------------------------------------------------------------------------
// BEGIN ASYNC REFRESH
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.BeginRefreshAsync: ITask;
begin
  // Ensure only one refresh task runs at a time
  TMonitor.Enter(FAdaptersLock);
  try
    // Cancel and wait for an existing task when present
    if Assigned(FRefreshTask) and (FRefreshTask.Status = TTaskStatus.Running) then
    begin
      FRefreshCancelEvent.SetEvent;
      FRefreshTask.Wait;
    end;
    // Reset cancellation for the new run
    FRefreshCancelEvent.ResetEvent;
    // Launch a background refresh
    FRefreshTask := TTask.Run(
      procedure
      var
        SerialAdapters: TArray<TSerialOBDAdapterEnum>;
        FTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
        BluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;
        S, F, B: Boolean;
      begin
        // Gather adapter lists without holding locks
        S := EnumerateSerial(SerialAdapters);
        // Stop early if cancellation is requested
        if FRefreshCancelEvent.WaitFor(0) = wrSignaled then Exit;
        F := EnumerateFTDI(FTDIAdapters);
        if FRefreshCancelEvent.WaitFor(0) = wrSignaled then Exit;
        B := EnumerateBluetooth(BluetoothAdapters);
        if FRefreshCancelEvent.WaitFor(0) = wrSignaled then Exit;
        // Publish results under lock when no cancellation is pending
        TMonitor.Enter(FAdaptersLock);
        try
          if FRefreshCancelEvent.WaitFor(0) = wrSignaled then Exit;
          FSerialAdapters := SerialAdapters;
          FFTDIAdapters := FTDIAdapters;
          FBluetoothAdapters := BluetoothAdapters;
        finally
          TMonitor.Exit(FAdaptersLock);
        end;
        // Optionally signal failure by setting cancellation when any refresh failed
        if not (S and F and B) then
          FRefreshCancelEvent.SetEvent;
      end);
    Result := FRefreshTask;
  finally
    TMonitor.Exit(FAdaptersLock);
  end;
end;

//------------------------------------------------------------------------------
// SNAPSHOT ADAPTER ACCESSORS
//------------------------------------------------------------------------------
function TOBDAdapterEnumerator.SnapshotSerialAdapters: TArray<TSerialOBDAdapterEnum>;
begin
  // Provide a copy of the cached serial adapters
  Result := GetSerialAdapters;
end;

function TOBDAdapterEnumerator.SnapshotFTDIAdapters: TArray<TFTDIOBDAdapterEnum>;
begin
  // Provide a copy of the cached FTDI adapters
  Result := GetFTDIAdapters;
end;

function TOBDAdapterEnumerator.SnapshotBluetoothAdapters: TArray<TBluetoothOBDAdapterEnum>;
begin
  // Provide a copy of the cached Bluetooth adapters
  Result := GetBluetoothAdapters;
end;

end.
