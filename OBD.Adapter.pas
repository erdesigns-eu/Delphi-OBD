//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.pas
// CONTENTS       : Base OBD Adapter Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/03/2024
//------------------------------------------------------------------------------
unit OBD.Adapter;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows, System.Bluetooth,

  OBD.Connection, OBD.Connection.Types, OBD.Connection.Serial,
  OBD.Connection.Bluetooth, OBD.Connection.Wifi, OBD.Connection.FTDI,
  OBD.Protocol, OBD.Protocol.Types, OBD.Protocol.Legacy, OBD.Protocol.CAN,
  OBD.Adapter.Types, OBD.Adapter.Constants, OBD.Adapter.ATCommands;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter (INTERFACE)
  /// </summary>
  IOBDAdapter = interface
    ['{212E2851-98C7-4A8E-8848-6347CD18CD7D}']
    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    function Connect: Boolean;
    /// <summary>
    ///   Disconnect the connected OBD Adapter
    /// </summary>
    function Disconnect: Boolean;
    /// <summary>
    ///   Get the connected status of the adapter
    /// </summary>
    function Connected: Boolean;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter Exception
  /// </summary>
  TOBDAdapterException = class(Exception);

  /// <summary>
  ///   OBD Adapter (CLASS)
  /// </summary>
  TOBDAdapter = class(TInterfacedObject, IOBDAdapter)
  private
    /// <summary>
    ///   Adapter status
    /// </summary>
    FStatus: TOBDAdapterStatus;
    /// <summary>
    ///   Receive Data Messages Event
    /// </summary>
    FOnReceiveDataMessages: TReceiveDataMessagesEvent;
    /// <summary>
    ///   Receive Data Event
    /// </summary>
    FOnReceiveData: TReceiveDataEvent;
    /// <summary>
    ///   Status Change Event
    /// </summary>
    FOnStatusChange: TAdapterStatusChangeEvent;
    /// <summary>
    ///   Connection Change Event
    /// </summary>
    FOnConnectionChange: TAdapterConnectionChangeEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    function Connect: Boolean; virtual; abstract;
    /// <summary>
    ///   Disconnect the connected OBD Adapter
    /// </summary>
    function Disconnect: Boolean; virtual; abstract;
    /// <summary>
    ///   Get the connected status of the adapter
    /// </summary>
    function Connected: Boolean; virtual; abstract;

    /// <summary>
    ///   Adapter status
    /// </summary>
    property Status: TOBDAdapterStatus read FStatus write FStatus;

    /// <summary>
    ///   Receive Data Message Event
    /// </summary>
    property OnReceiveDataMessages: TReceiveDataMessagesEvent read FOnReceiveDataMessages write FOnReceiveDataMessages;
    /// <summary>
    ///   Receive Data Event
    /// </summary>
    property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    /// <summary>
    ///   Status Change Event
    /// </summary>
    property OnStatusChange: TAdapterStatusChangeEvent read FOnStatusChange write FOnStatusChange;
    /// <summary>
    ///   Connection Change Event
    /// </summary>
    property OnConnectionChange: TAdapterConnectionChangeEvent read FOnConnectionChange write FOnConnectionChange;
  end;

  /// <summary>
  ///   OBD Adapter Settings Base Class
  /// </summary>
  TOBDAdapterSettings = class
  private
    /// <summary>
    ///   Adapter
    /// </summary>
    FAdapter: IOBDAdapter;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Adapter: TOBDAdapter); virtual;

    /// <summary>
    ///   Adapter
    /// </summary>
    property Adapter: IOBDAdapter read FAdapter;
  end;

  /// <summary>
  ///   OBD Adapter Serial Settings
  /// </summary>
  TOBDAdapterSerial = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   COM port
    /// </summary>
    FCOMPort: string;
    /// <summary>
    ///   COM Baudrate
    /// </summary>
    FBaudRate: TBaudRate;

    /// <summary>
    ///   Set COM port
    /// </summary>
    procedure SetCOMPort(Value: string);
    /// <summary>
    ///   Set COM Baudrate
    /// </summary>
    procedure SetBaudRate(Value: TBaudRate);
  public
    /// <summary>
    ///   COM port
    /// </summary>
    property COMPort: string read FCOMPort write SetCOMPort;
    /// <summary>
    ///   COM Baudrate
    /// </summary>
    property BaudRate: TBaudRate read FBaudRate write SetBaudRate;
  end;

  /// <summary>
  ///   OBD Adapter Bluetooth Settings
  /// </summary>
  TOBDAdapterBluetooth = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   Bluetooth manager
    /// </summary>
    FManager: TBluetoothManager;
    /// <summary>
    ///   Bluetooth address
    /// </summary>
    FAddress: string;

    /// <summary>
    ///   Set bluetooth manager
    /// </summary>
    procedure SetManager(Value: TBluetoothManager);
    /// <summary>
    ///   Set address
    /// </summary>
    procedure SetAddress(Value: string);
  public
    /// <summary>
    ///   Bluetooth manager
    /// </summary>
    property Manager: TBluetoothManager read FManager write SetManager;
    /// <summary>
    ///   Bluetooth address
    /// </summary>
    property Address: string read FAddress write SetAddress;
  end;

  /// <summary>
  ///   OBD Adapter Wifi Settings
  /// </summary>
  TOBDAdapterWifi = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   IP Address
    /// </summary>
    FIPAddress: string;
    /// <summary>
    ///   Port
    /// </summary>
    FPort: Integer;

    /// <summary>
    ///   Set IP address
    /// </summary>
    procedure SetIPAddress(Value: string);
    /// <summary>
    ///   Set port
    /// </summary>
    procedure SetPort(Value: Integer);
  public
    /// <summary>
    ///   IP Address
    /// </summary>
    property IPAddress: string read FIPAddress write SetIPAddress;
    /// <summary>
    ///   Port
    /// </summary>
    property Port: Integer read FPort write SetPort;
  end;

  /// <summary>
  ///   OBD Adapter FTDI (USB) Settings
  /// </summary>
  TOBDAdapterFTDI = class(TOBDAdapterSettings)
  private
    /// <summary>
    ///   Serial Number
    /// </summary>
    FSerialNumber: string;

    /// <summary>
    ///   Set Serial Number
    /// </summary>
    procedure SetSerialNumber(Value: string);
  public
    /// <summary>
    ///   Serial Number
    /// </summary>
    property SerialNumber: string read FSerialNumber write SetSerialNumber;
  end;

  /// <summary>
  ///   Base ELM OBD Adapter Class
  /// </summary>
  TELMAdapter = class(TOBDAdapter)
  private type
    TSyncCommandType = (ctATCommand, ctSTCommand, ctOBDCommand);
  private
    /// <summary>
    ///   Flag indicating we are waiting for a SYNC response.
    ///   This is used for the WriteCommandSync function.
    /// </summary>
    FSyncResponseWaiting: Boolean;
    /// <summary>
    ///   Response data.
    ///   This is used for the WriteCommandSync function.
    /// </summary>
    FSyncResponseData: AnsiString;
  private
    /// <summary>
    ///   Incoming data buffer for accumulating the incoming data
    ///   until we get the '>' character.
    /// </summary>
    FDataBuffer: AnsiString;
    /// <summary>
    ///   List containing received data
    /// </summary>
    FDataLines: TStringList;
    /// <summary>
    ///   Serial (COM PORT) settings
    /// </summary>
    FSerial: TOBDAdapterSerial;
    /// <summary>
    ///   Bluetooth settings
    /// </summary>
    FBluetooth: TOBDAdapterBluetooth;
    /// <summary>
    ///   Wifi settings
    /// </summary>
    FWifi: TOBDAdapterWifi;
    /// <summary>
    ///   FTDI settings
    /// </summary>
    FFTDI: TOBDAdapterFTDI;
    /// <summary>
    ///   Connection Type (Unknown/Serial/Bluetooth/Wifi/FTDI)
    /// </summary>
    FConnectionType: TOBDConnectionType;
    /// <summary>
    ///   Connection (COM/FTDI/Bluetooth/Wifi)
    /// </summary>
    FConnection: IOBDConnection;
    /// <summary>
    ///   Protocol (Legacy/Can)
    /// </summary>
    FProtocol: IOBDProtocol;
    /// <summary>
    ///   ELM Protocol
    /// </summary>
    FELMProtocol: TELMProtocol;
    /// <summary>
    ///   Initialization timeout
    /// </summary>
    FInitializationTimeout: Integer;
    /// <summary>
    ///   Allow long messages (> 7 bytes)
    /// </summary>
    FAllowLongMessages: Boolean;

    /// <summary>
    ///   Adapter Identifier (e.g. ELM327 v1.2, v1.3, v1.4b, v1.5, v2.1)
    /// </summary>
    FIdentifier: string;
    /// <summary>
    ///   Device Descriptor (e.g. OBDII to RS232 Interpreter)
    /// </summary>
    FDeviceDescriptor: string;
    /// <summary>
    ///   Device Identifier (e.g. SCANTOOL.NET)
    /// </summary>
    FDeviceIdentifier: string;
    /// <summary>
    ///   Voltage present at pin 2 (e.g. 12.3V)
    /// </summary>
    FVoltage: string;
    /// <summary>
    ///   Protocol descriptor from ELM
    /// </summary>
    FProtocolDescriptor: string;

    /// <summary>
    ///   Adapter error event
    /// </summary>
    FOnAdapterError: TELMAdapterErrorEvent;
    /// <summary>
    ///   OBDLink adapter error event
    /// </summary>
    FOnOBDLinkError: TOBDLinkAdapterErrorEvent;

    /// <summary>
    ///   Set connection type
    /// </summary>
    procedure SetConnectionType(Value: TOBDConnectionType);
    /// <summary>
    ///   Set ELM protocol
    /// </summary>
    procedure SetELMProtocol(Value: TELMProtocol);
    /// <summary>
    ///   Set allow long messages
    /// </summary>
    procedure SetAllowLongMessages(Value: Boolean);
    /// <summary>
    ///   Handle data received event
    /// </summary>
    procedure HandleDataReceived(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
  protected
    /// <summary>
    ///   Write command to the adapter and return the response. (SYNC)
    /// </summary>
    function WriteCommandSync(const CommandType: TSyncCommandType; const Command: string; const Timeout: Integer = 5000): string; virtual;
    /// <summary>
    ///   Write command to the adapter. (SYNC)
    ///   And return true if the adapter acknowledges.
    /// </summary>
    function WriteCommandSyncExpectOK(const CommandType: TSyncCommandType; const Command: string; const Timeout: Integer = 5000): Boolean; virtual;
    /// <summary>
    ///   Initialize connection
    /// </summary>
    function Init: Boolean; virtual;
    /// <summary>
    ///   OBD Connection
    /// </summary>
    property Connection: IOBDConnection read FConnection;
  public
    /// <summary>
    ///   Constructor
    /// <summary>
    constructor Create; override;
    /// <summary>
    ///   Destructor
    /// <summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    function Connect: Boolean; override;
    /// <summary>
    ///   Disconnect the connected OBD Adapter
    /// </summary>
    function Disconnect: Boolean; override;
    /// <summary>
    ///   Get the connected status of the adapter
    /// </summary>
    function Connected: Boolean; override;

    /// <summary>
    ///   Set the ELM Adapter protocol
    /// </summary>
    function SetProtocol(Value: TELMProtocol): Boolean; virtual;
    /// <summary>
    ///   Try the ELM Adapter protocol
    /// </summary>
    function TryProtocol(Value: TELMProtocol): Boolean; virtual;
    /// <summary>
    ///   Test the protocol by asking for data
    /// </summary>
    function TestProtocol: Boolean; virtual;

    /// <summary>
    ///   Serial (COM PORT) settings
    /// </summary>
    property Serial: TOBDAdapterSerial read FSerial;
    /// <summary>
    ///   Bluetooth settings
    /// </summary>
    property Bluetooth: TOBDAdapterBluetooth read FBluetooth;
    /// <summary>
    ///   Wifi settings
    /// </summary>
    property Wifi: TOBDAdapterWifi read FWifi;
    /// <summary>
    ///   FTDI settings
    /// </summary>
    property FTDI: TOBDAdapterFTDI read FFTDI;
    /// <summary>
    ///   Connection Type (Unknown/Serial/Bluetooth/Wifi/FTDI)
    /// </summary>
    property ConnectionType: TOBDConnectionType read FConnectionType write SetConnectionType;
    /// <summary>
    ///   OBD Protocol
    /// </summary>
    property Protocol: IOBDProtocol read FProtocol write FProtocol;
    /// <summary>
    ///   ELM Protocol
    /// </summary>
    property ELMProtocol: TELMProtocol read FELMProtocol write SetELMProtocol;
    /// <summary>
    ///   Initialization timeout
    /// </summary>
    property InitializationTimeout: Integer read FInitializationTimeout write FInitializationTimeout;
    /// <summary>
    ///   Allow long messages (> 7 bytes)
    /// </summary>
    property AllowLongMessages: Boolean read FAllowLongMessages write SetAllowLongMessages;

    /// <summary>
    ///   Adapter identifier (e.g. ELM327 v1.2, v1.3, v1.4b, v1.5, v2.1)
    /// </summary>
    property Identifier: string read FIdentifier;
    /// <summary>
    ///   Device Descriptor (e.g. OBDII to RS232 Interpreter)
    /// </summary>
    property DeviceDescriptor: string read FDeviceDescriptor;
    /// <summary>
    ///   Device Identifier (e.g. SCANTOOL.NET)
    /// </summary>
    property DeviceIdentifier: string read FDeviceIdentifier;
    /// <summary>
    ///   Voltage present at pin 2 (e.g. 12.3V)
    /// </summary>
    property Voltage: string read FVoltage;
    /// <summary>
    ///   Protocol descriptor from ELM
    /// </summary>
    property ProtocolDescriptor: string read FProtocolDescriptor;

    /// <summary>
    ///   Adapter error event
    /// </summary>
    property OnAdapterError: TELMAdapterErrorEvent read FOnAdapterError write FOnAdapterError;
    /// <summary>
    ///   OBDLink adapter error event
    /// </summary>
    property OnOBDLinkError: TOBDLinkAdapterErrorEvent read FOnOBDLinkError write FOnOBDLinkError;
  end;


implementation

uses System.StrUtils, Vcl.Forms;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDAdapter.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Initialize status
  FStatus := asNotConnected;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDAdapterSettings.Create(Adapter: TOBDAdapter);
begin
  // Call inherited constructor
  inherited Create;
  // Set Adapter
  FAdapter := Adapter;
end;

//------------------------------------------------------------------------------
// SET COM PORT
//------------------------------------------------------------------------------
procedure TOBDAdapterSerial.SetCOMPort(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set COM Port
  if FCOMPort <> Value then FCOMPort := Value;
end;

//------------------------------------------------------------------------------
// SET COM BAUDRATE
//------------------------------------------------------------------------------
procedure TOBDAdapterSerial.SetBaudRate(Value: TBaudRate);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set COM Port
  if FBaudRate <> Value then FBaudRate := Value;
end;

//------------------------------------------------------------------------------
// SET BLUETOOTH MANAGER
//------------------------------------------------------------------------------
procedure TOBDAdapterBluetooth.SetManager(Value: TBluetoothManager);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Bluetooth manager
  if FManager <> Value then FManager := Value;
end;

//------------------------------------------------------------------------------
// SET ADDRESS
//------------------------------------------------------------------------------
procedure TOBDAdapterBluetooth.SetAddress(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Address
  if FAddress <> Value then FAddress := Value;
end;

//------------------------------------------------------------------------------
// SET IP ADDRESS
//------------------------------------------------------------------------------
procedure TOBDAdapterWifi.SetIPAddress(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set IP Address
  if FIPAddress <> Value then FIPAddress := Value;
end;

//------------------------------------------------------------------------------
// SET PORT
//------------------------------------------------------------------------------
procedure TOBDAdapterWifi.SetPort(Value: Integer);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Port
  if FPort <> Value then FPort := Value;
end;

//------------------------------------------------------------------------------
// SET SERIAL NUMBER
//------------------------------------------------------------------------------
procedure TOBDAdapterFTDI.SetSerialNumber(Value: string);
begin
  // Exit here if we're connected
  if Adapter.Connected then Exit;
  // Set Serial Number
  if FSerialNumber <> Value then FSerialNumber := Value;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TELMAdapter.Create;
begin
  // Call inherited constructor
  inherited Create;

  // Serial (COM PORT) settings
  FSerial := TOBDAdapterSerial.Create(Self);
  // Bluetooth settings
  FBluetooth := TOBDAdapterBluetooth.Create(Self);
  // Wifi settings
  FWifi := TOBDAdapterWifi.Create(Self);
  // FTDI settings
  FFTDI := TOBDAdapterFTDI.Create(Self);

  // Create stringlist for holding incoming data
  FDataLines := TStringList.Create;
  // Set initial initialization timeout
  FInitializationTimeout := 5000;
  // Set initial allow long messages
  FAllowLongMessages := False;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TELMAdapter.Destroy;
begin
  // Free serial settings
  FSerial.Free;
  // Free bluetooth settings
  FBluetooth.Free;
  // Free wifi settings
  FWifi.Free;
  // Free FTDI settings
  FFTDI.Free;

  // Free stringlist
  FDataLines.Free;

  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SET CONNECTION TYPE
//------------------------------------------------------------------------------
procedure TELMAdapter.SetConnectionType(Value: TOBDConnectionType);
begin
  // Exit here if we're connected
  if Connected then Exit;
  // Set connection type
  if Value <> FConnectionType then
  begin
    FConnectionType := Value;
  end;
end;

//------------------------------------------------------------------------------
// SET ELM PROTOCOL
//------------------------------------------------------------------------------
procedure TELMAdapter.SetELMProtocol(Value: TELMProtocol);
begin
  // Exit here if we're connected
  // We dont want users to change this while connected, then the try protocol
  // or set protocol methods should be used, and the ELM protocol will be
  // updated if successfull.
  if Connected then Exit;
  // Set ELM Protocol
  if Value <> FELMProtocol then
  begin
    FELMProtocol := Value;
  end;
end;

//------------------------------------------------------------------------------
// SET ALLOW LONG MESSAGES
//------------------------------------------------------------------------------
procedure TELMAdapter.SetAllowLongMessages(Value: Boolean);
begin
  FAllowLongMessages := Value;
  if Assigned(FProtocol) then
  begin
    (FProtocol as TOBDProtocol).AllowLongMesssages := Value;
  end;
end;

//------------------------------------------------------------------------------
// HANDLE DATA RECEIVED EVENT
//------------------------------------------------------------------------------
procedure TELMAdapter.HandleDataReceived(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
var
  S: AnsiString;
  T: AnsiString;
  E: TELMAdapterError;
  O: TOBDLinkAdapterError;

  function HasELMError: Boolean;
  begin
    // Initialize result
    Result := False;
    // Check for unsupported command
    if Pos(ELM_UNSUPPORTED_COMMAND, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeUnsupportedCommand;
    end else
    // Check for no data error
    if Pos(ELM_NO_DATA, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeNoData;
    end else
    if Pos(ELM_DATA_ERROR, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeDataError;
    end;
    // Check for bus init error
    if Pos(ELM_BUS_INIT_ERROR, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeBusInit;
    end else
    // Check for bus busy
    if Pos(ELM_BUS_BUSY, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeBusBusy;
    end else
    // Check for bus error
    if Pos(ELM_BUS_ERROR, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeBusError;
    end else
    // Check for can error
    if Pos(ELM_CAN_ERROR, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeCanError;
    end else
    // Check for unable to connect error
    if Pos(ELM_UNABLE_TO_CONNECT, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeUnableToConnect;
    end else
    // Check for other errors
    if Pos(ELM_ERROR, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeError;
    end else
    // Check for stopped message
    if Pos(ELM_STOPPED, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeStopped;
    end else
    // Check for buffer full error
    if Pos(ELM_BUFFER_FULL, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      E := aeBufferFull;
    end;
  end;

  function HasOBDLinkError: Boolean;
  begin
    // Initialize result
    Result := False;
    // Check for ACT Alert
    if Pos(OBDLINK_ACT_ALERT, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeActAlert;
    end else
    // Check for FB Error
    if Pos(OBDLINK_FB_ERROR, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeFbError;
    end else
    // Check for FC RX Timeout
    if Pos(OBDLINK_FC_RX_TIMEOUT, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeFcRxTimeout;
    end else
    // Check for LP Alert
    if Pos(OBDLINK_LP_ALERT, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeLpAlert;
    end else
    // Check for LV Reset
    if Pos(OBDLINK_LV_RESET, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeLvReset;
    end else
    // Check for out of memory
    if Pos(OBDLINK_OUT_OF_MEMORY, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeOutOfMemory;
    end else
    // Check for RX Error
    if Pos(OBDLINK_RX_ERROR, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeRxError;
    end else
    // Check for UART RX Overflow
    if Pos(OBDLINK_UART_RX_OVERFLOW, String(FDataBuffer)) > 0 then
    begin
      Result := True;
      O := aeUartRxOverflow;
    end;
  end;

var
  ParsedMessages: TArray<IOBDDataMessage>;
begin
  // Allocate memory for the data
  S := AnsiString(StringOfChar(' ', DataSize));
  // Move the data to the buffer
  Move(DataPtr^, PAnsiChar(S)^, DataSize);
  // Accumulate the incoming data until we find the '>' character.
  FDataBuffer := FDataBuffer + S;
  // Check for the '>' character
  if Pos(ELM_MESSAGE_DELIMITER, String(FDataBuffer)) > 0 then
  begin
    // Remove the delimiter
    T := Copy(FDataBuffer, 1, Length(FDataBuffer) -  Length(ELM_MESSAGE_DELIMITER));
    // Check for ELM errors, and notify if there is
    if HasELMError and Assigned(OnAdapterError) then OnAdapterError(Self, E);
    // Check for OBDLink errors, and notify if there is
    if HasOBDLinkError and Assigned(OnOBDLinkError) then OnOBDLinkError(Self, O);
    // Notify we received data
    if Assigned(OnReceiveData) then OnReceiveData(Self, T);

    // Are we waiting for a SYNC response?
    if FSyncResponseWaiting then
    begin
      // Assign the SYNC response data
      FSyncResponseData := T;
      // Reset flag
      FSyncResponseWaiting := False;
    end;

    // If the protocol is assigned, then send the data to the protocol for parsing
    if Assigned(Protocol) then
    begin
      // Clear the list
      FDataLines.Clear;
      // Assign the data
      FDataLines.Text := String(T);
      // Send the data to the protocol for parsing
      ParsedMessages := Protocol.Invoke(FDataLines);
      // Notify we have parsed messages
      if Assigned(OnReceiveDataMessages) then OnReceiveDataMessages(Self, ParsedMessages);
    end;

    // Clear buffer for next data
    FDataBuffer := '';
  end;
end;


//------------------------------------------------------------------------------
// WRITE COMMAND AND RETURN RESPONSE (SYNC)
//------------------------------------------------------------------------------
function TELMAdapter.WriteCommandSync(const CommandType: TSyncCommandType; const Command: string; const Timeout: Integer = 5000): string;
var
  StartTime, Elapsed: Cardinal;
  DidTimeOut: Boolean;
begin
  // initialize result
  Result := '';
  // Exit here if we're not connected
  if not Connected then Exit;

  // Clear the response data
  FSyncResponseData := '';
  // Update flag to indicate we are waiting for SYNC data
  FSyncResponseWaiting := True;

  // Write command based on type of command
  case CommandType of
    ctATCommand  : FConnection.WriteATCommand(IfThen(Pos(ELM_COMMAND_TERMINATOR, Command) = 0, Command + ELM_COMMAND_TERMINATOR, Command));
    ctSTCommand  : FConnection.WriteSTCommand(IfThen(Pos(ELM_COMMAND_TERMINATOR, Command) = 0, Command + ELM_COMMAND_TERMINATOR, Command));
    ctOBDCommand : FConnection.WriteOBDCommand(IfThen(Pos(ELM_COMMAND_TERMINATOR, Command) = 0, Command + ELM_COMMAND_TERMINATOR, Command));
  end;

  // Get the start time for timeout tracking
  StartTime := GetTickCount;
  // Reset did timeout flag
  DidTimeOut := False;

  // Wait for the response with a loop
  repeat
    // Calculate elapsed time to handle timeout
    Elapsed := GetTickCount - StartTime;
    // Exit the loop if the response has been received
    if FSyncResponseWaiting = False then Break;
    // Handle timeout (exit loop if exceeded)
    if Integer(Elapsed) >= Timeout then
    begin
      DidTimeOut := True;
      Break;
    end;
    // Sleep a bit to reduce CPU usage
    Sleep(100);
    // Optionally, process messages to keep the UI responsive
    Application.ProcessMessages;
  until False;

  // If the loop didnt timeout, return the response data
  if not DidTimeOut then Result := String(FSyncResponseData);
end;

//------------------------------------------------------------------------------
// WRITE COMMAND (SYNC) AND RETURN TRUE IF ADAPTER ACKNOWLEDGES
//------------------------------------------------------------------------------
function TELMAdapter.WriteCommandSyncExpectOK(const CommandType: TSyncCommandType; const Command: string; const Timeout: Integer = 5000): Boolean;
var
  S: string;
begin
  // Write command sync
  S := WriteCommandSync(CommandType, Command, Timeout);
  // Check if the adapter returned with OK
  Result := EndsText(ELM_ACKNOWLEDGE, Trim(S));
end;

//------------------------------------------------------------------------------
// INIT CONNECTION
//------------------------------------------------------------------------------
function TELMAdapter.Init: Boolean;
var
  S: string;
  P: Integer;
  L: TStringList;
begin
  // Initialize result
  Result := False;

  // First send a Cariage Return character to "wake-up" the adapter
  WriteCommandSync(ctOBDCommand, '', FInitializationTimeout);

  // Send a '?' character, if the ELM adapter responds with '?' we know we
  // are using the correct baudrate.
  S := WriteCommandSync(ctOBDCommand, ELM_UNSUPPORTED_COMMAND, FInitializationTimeout);
  if not EndsText(ELM_UNSUPPORTED_COMMAND, Trim(S)) then Exit;

  // Lets send a reset:
  S := WriteCommandSync(ctATCommand, FormatATCommand(RESET_ALL, []), FInitializationTimeout);
  if Length(Trim(S)) = 0 then Exit;

  // Then turn echo off
  if not WriteCommandSyncExpectOK(ctATCommand, FormatATCommand(ECHO_OFF, []), FInitializationTimeout) then Exit;

  // Then turn linefeeds on (should be on by default) because we use it in the
  // protocols for parsing.
  if not WriteCommandSyncExpectOK(ctATCommand, FormatATCommand(LINEFEEDS_ON, []), FInitializationTimeout) then Exit;

  // Then turn off spaces, because we are removing them anyway.
  if not WriteCommandSyncExpectOK(ctATCommand, FormatATCommand(SPACES_OFF, []), FInitializationTimeout) then Exit;

  // Turn on headers
  if not WriteCommandSyncExpectOK(ctATCommand, FormatATCommand(HEADERS_ON, []), FInitializationTimeout) then Exit;

  // Lets get the identifier
  FIdentifier := Trim(WriteCommandSync(ctATCommand, FormatATCommand(PRINT_ID, []), FInitializationTimeout));

  // Then lets get the voltage
  FVoltage := Trim(WriteCommandSync(ctATCommand, FormatATCommand(READ_VOLTAGE, []), FInitializationTimeout));

  // Then get the device descriptor
  FDeviceDescriptor := Trim(WriteCommandSync(ctATCommand, FormatATCommand(DISPLAY_DEVICE_DESCRIPTOR, []), FInitializationTimeout));

  // And then the device identifier
  FDeviceIdentifier := Trim(WriteCommandSync(ctATCommand, FormatATCommand(DISPLAY_DEVICE_IDENTIFIER, []), FInitializationTimeout));

  // Try to set the protocol
  if not SetProtocol(ELMProtocol) then Exit;

  // Update the status
  FStatus := asCarConnected;
  // Notify the status change
  if Assigned(OnStatusChange) then OnStatusChange(Self, FStatus);

  // Get protocol index number
  S := UpperCase(WriteCommandSync(ctATCommand, FormatATCommand(DESCRIBE_PROTOCOL_NUMBER, []), FInitializationTimeout));
  if StartsText('A', S) then S := Copy(S, 2, Length(S) - 1);
  // Convert hex to dec, if this fails set the protocol to auto
  if not TryStrToInt('$' + Trim(S), P) then P := 0;
  // Update the current protocol
  FELMProtocol := TELMProtocol(P);

  // Get the protocol description
  FProtocolDescriptor := WriteCommandSync(ctATCommand, FormatATCommand(DESCRIBE_CURRENT_PROTOCOL, []), FInitializationTimeout);

  // Create temp list for storing 0100 lines
  L := TStringList.Create;
  try
    // Get initial data (0100)
    L.Text := WriteCommandSync(ctOBDCommand, '0100');
    // Create the protocol object (if not set to auto)
    case FELMProtocol of
      epAutomatic          : FProtocol := nil;
      epSAE_J1850_PWM      : FProtocol := TSAE_J1850_PWM_OBDProtocol.Create(L, AllowLongMessages);
      epSAE_J1850_VPW      : FProtocol := TSAE_J1850_VPW_OBDProtocol.Create(L, AllowLongMessages);
      epISO_9141_2         : FProtocol := TISO_9141_2_OBDProtocol.Create(L, AllowLongMessages);
      epISO_14230_4_SLOW   : FProtocol := TISO_14230_4_5BAUD_OBDProtocol.Create(L, AllowLongMessages);
      epISO_14230_4_FAST   : FProtocol := TISO_14230_4_FAST_OBDProtocol.Create(L, AllowLongMessages);
      epISO_15765_4_11_500 : FProtocol := TISO_15765_4_11BIT_500K_OBDProtocol.Create(L, AllowLongMessages);
      epISO_15765_4_29_500 : FProtocol := TISO_15765_4_29BIT_500K_OBDProtocol.Create(L, AllowLongMessages);
      epISO_15765_4_11_250 : FProtocol := TISO_15765_4_11BIT_250K_OBDProtocol.Create(L, AllowLongMessages);
      epISO_15765_4_29_250 : FProtocol := TISO_15765_4_29BIT_250K_OBDProtocol.Create(L, AllowLongMessages);
      epSAE_J1939_29_250   : FProtocol := TSAE_J1939_250K_OBDProtocol.Create(L, AllowLongMessages);
      epSAE_J1939_29_500   : FProtocol := TSAE_J1939_500K_OBDProtocol.Create(L, AllowLongMessages);
    end;
  finally
    L.Free;
  end;

  // Check if we created the protocol
  if Assigned(FProtocol) then
  begin
    // Update the status
    FStatus := asOBDConnected;
    // Notify the status change
    if Assigned(OnStatusChange) then OnStatusChange(Self, FStatus);
  end;

  // If we get till here, everything went fine
  Result := True;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TELMAdapter.Connect: Boolean;
var
  Params: TOBDConnectionParams;
begin
  // initialize result
  Result := False;
  // Exit here if we are already connected
  if Connected then Exit;
  // Make sure there is a connection method selected
  if FConnectionType = ctUnknown then Exit;

  // Setup the connection type
  Params.ConnectionType := FConnectionType;

  // Create the connection
  case FConnectionType of
    ctSerial:
    begin
      // Create the connection class
      FConnection := TSerialOBDConnection.Create;
      // Setup params
      Params.COMPort := ShortString(Serial.COMPort);
      Params.BaudRate := Serial.BaudRate;
    end;

    ctBluetooth:
    begin
      // Create the connection class
      FConnection := TBluetoothOBDConnection.Create;
      // Setup params
      Params.Manager := Bluetooth.Manager;
      Params.Address := ShortString(Bluetooth.Address);
    end;

    ctWiFi:
    begin
      // Create the connection class
      FConnection := TWifiOBDConnection.Create;
      // Setup params
      Params.IPAddress := ShortString(Wifi.IPAddress);
      Params.Port := Wifi.Port;
    end;

    ctFTDI:
    begin
      // Create the connection class
      FConnection := TFTDIOBDConnection.Create;
      // Setup params
      Params.SerialNumber := ShortString(FTDI.SerialNumber);
    end;
  end;

  // Try to connect and return success status
  Result := FConnection.Connect(Params);

  // Exit here if we're not connected
  if not Result then
  begin
    // Remove connection
    FConnection := nil;
    // Exit here
    Exit;
  end;

  // Notify we connected
  if Assigned(OnConnectionChange) then OnConnectionChange(Self, True);
  // Clear input buffer
  FDataBuffer := '';

  // Update the status
  FStatus := asAdapterConnected;
  // Notify the status change
  if Assigned(OnStatusChange) then OnStatusChange(Self, FStatus);

  // If we succeeded until here, lets assign the data received handler
  FConnection.OnDataReceived := HandleDataReceived;
  // Initialize the adapter.
  Result := Init;

  // If the initialization failed, disconnect
  if not Result then Disconnect;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
function TELMAdapter.Disconnect: Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we aren't connected
  if not Connected then Exit;

  // Send close protocol
  WriteCommandSync(ctATCommand, FormatATCommand(PROTOCOL_CLOSE, []));
  // Send low power
  // WriteCommandSync(ctATCommand, FormatATCommand(LOW_POWER_MODE, []));
  // Reset adapter
  WriteCommandSync(ctATCommand, FormatATCommand(RESET_ALL, []));

  // Disconnect the connection
  Result := FConnection.Disconnect;
  // Exit here if the disconnection failed
  if not Result then Exit;

  // Clear the identifier
  FIdentifier := '';
  // Clear the voltage
  FVoltage := '';
  // Clear the device descriptor
  FDeviceDescriptor := '';
  // Clear the device identifier
  FDeviceIdentifier := '';
  // Clear the protocol description
  FProtocolDescriptor := '';

  // Remove the connection
  FConnection := nil;
  // Update the status
  FStatus := asNotConnected;
  // Notify the status change
  if Assigned(OnStatusChange) then OnStatusChange(Self, FStatus);
  // Notify we disconnected
  if Assigned(OnConnectionChange) then OnConnectionChange(Self, False);
  // Clear input buffer
  FDataBuffer := '';
end;

//------------------------------------------------------------------------------
// GET CONNECTED STATUS
//------------------------------------------------------------------------------
function TELMAdapter.Connected: Boolean;
begin
  Result := Assigned(FConnection) and FConnection.Connected;
end;

//------------------------------------------------------------------------------
// SET ELM ADAPTER PROTOCOL
//------------------------------------------------------------------------------
function TELMAdapter.SetProtocol(Value: TELMProtocol): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we aren't connected
  if not Connected then Exit;

  // Set protocol
  if not WriteCommandSyncExpectOK(ctATCommand, FormatATCommand(SET_PROTOCOL, [IntToHex(Ord(ELMProtocol), 1)])) then Exit;
  // Test the protocol and return the result
  Result := TestProtocol;
end;

//------------------------------------------------------------------------------
// TRY ELM ADAPTER PROTOCOL
//------------------------------------------------------------------------------
function TELMAdapter.TryProtocol(Value: TELMProtocol): Boolean;
begin
  // initialize result
  Result := False;
  // Exit here if we aren't connected
  if not Connected then Exit;

  // Try protocol
  if not WriteCommandSyncExpectOK(ctATCommand, FormatATCommand(TRY_PROTOCOL, [IntToHex(Ord(ELMProtocol), 1)])) then Exit;
  // Test the protocol and return the result
  Result := TestProtocol;
end;

//------------------------------------------------------------------------------
// TEST ELM ADAPTER PROTOCOL
//------------------------------------------------------------------------------
function TELMAdapter.TestProtocol: Boolean;
var
  S, E: string;
begin
  // initialize result
  Result := False;
  // Exit here if we aren't connected
  if not Connected then Exit;
  // Request data
  S := WriteCommandSync(ctOBDCommand, '0100');
  // Check if we received a response, otherwise we exit here
  if Length(Trim(S)) = 0 then Exit;
  // Test if the response contains any of these error messages
  for E in [ELM_NO_DATA, ELM_NO_RESPONSE, ELM_DATA_ERROR, ELM_BUS_INIT_ERROR, ELM_BUS_ERROR, ELM_CAN_ERROR, ELM_UNABLE_TO_CONNECT, ELM_CONNECTION_FAILED] do
  begin
    // If we found the message, exit here
    if Pos(E, S) > 0 then Exit;
  end;
  // If we make it till here, then the protocol test succeeded
  Result := True;
end;

end.
