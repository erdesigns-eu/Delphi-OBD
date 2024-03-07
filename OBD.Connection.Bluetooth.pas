//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.Bluetooth.pas
// CONTENTS       : Bluetooth OBD Connection Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/02/2024
//------------------------------------------------------------------------------
unit OBD.Connection.Bluetooth;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Bluetooth,

  OBD.Connection,
  OBD.Connection.Types,
  OBD.Connection.Constants;

type
  /// <summary>
  ///   Bluetooth Thread for events
  /// </summary>
  TBluetoothThread = class(TThread)
  private
    /// <summary>
    ///   Bluetooth socket
    /// </summary
    FBluetoothSocket: TBluetoothSocket;
    /// <summary>
    ///   Window Handle to notify owner class
    /// </summary
    FWindowHandle: HWND;
  protected
    /// <summary>
    ///   Execute thread
    /// </summary
    procedure Execute; override;
  public
    /// <summary>
    ///   Constructor
    /// </summary
    constructor Create(CreateSuspended: Boolean; Socket: TBluetoothSocket; WindowHandle: HWND);
  end;

  /// <summary>
  ///   Bluetooth
  /// </summary>
  TBluetooth = class
  private
    /// <summary>
    ///   Bluetooth socket
    /// </summary
    FBluetoothSocket: TBluetoothSocket;
    /// <summary>
    ///   Connected flag
    /// </summary>
    FConnected: Boolean;
    /// <summary>
    ///   This is used for the thread
    /// </summary>
    FNotifyWnd: HWND;
    /// <summary>
    ///   Thread for listening to Bluetooth events
    /// </summary>
    FEventThread: TThread;
    /// <summary>
    ///   Event to emit on data reception (asynchronous)
    /// </summary>
    FOnReceiveData: TDataReceivedEvent;
    /// <summary>
    ///   Event to emit on data send (asynchronous)
    /// </summary>
    FOnSendData: TDataSendEvent;
    /// <summary>
    ///   Event to emit when an error occurs
    /// </summary>
    FOnError: TErrorEvent;
    /// <summary>
    ///   Input Buffer
    /// </summary>
    FInputBuffer: Array[0..BT_In_Buffer_Length] of Byte;
  protected
    /// <summary>
    ///   Thread proc: fetches received data from the connection and calls the
    ///   apropriate receive callbacks if necessary
    /// </summary>
    procedure ThreadWndProc(var msg: TMessage);
    /// <summary>
    ///   Callback when thread terminates
    /// </summary>
    /// <param name="Sender">
    ///   Thread calling this eventhandler
    /// </param>
    procedure EventThreadTerminate(Sender: TObject);
  public
    /// <summary>
    ///   Constructor
    /// <summary>
    constructor Create; virtual;
    /// <summary>
    ///   Close existing connection and free internal ressources
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Opens the Bluetooth connection. Returns false if something goes wrong
    /// </summary>
    function Connect(Manager: TBluetoothManager; Address: AnsiString): Boolean;
    /// <summary>
    ///   Closes the Bluetooth connection and releases control of it
    /// </summary>
    procedure Disconnect;

    /// <summary>
    ///   Sends binary data
    /// </summary>
    /// <param name="DataPtr">
    ///   Pointer to the memory containing the data to send
    /// </param>
    /// <param name="DataSize">
    ///   Number of bytes to send
    /// </param>
    /// <returns>
    ///   Number of bytes sent
    /// </returns>
    function SendData(DataPtr: pointer; DataSize: DWORD): DWORD;
    /// <summary>
    ///   Sends a byte. Returns true if the byte has been sent
    /// </summary>
    /// <param name="Value">
    ///   Byte to send
    /// </param>
    function SendByte(Value: byte): Boolean;
    /// <summary>
    ///   Sends a AnsiChar. Returns true if the AnsiChar has been sent
    /// </summary>
    /// <param name="Value">
    ///   Char to send
    /// </param>
    function SendChar(Value: AnsiChar): Boolean;
    /// <summary>
    ///   Sends a Pascal Ansistring (NULL terminated)
    /// </summary>
    /// <param name="s">
    ///   string to send
    /// </param>
    function SendString(S: Ansistring): Boolean;
    /// <summary>
    //    Sends a C-style Ansi string (NULL terminated)
    /// </summary>
    /// <param name="s">
    ///   string to send
    /// </param>
    function SendCString(S: PAnsiChar): Boolean;

    /// <summary>
    ///   Connected state
    /// </summary>
    property Connected: Boolean read FConnected;
    /// <summary>
    ///   Event to emit when there is data available (input buffer has data)
    ///   (called only if PacketSize <= 0)
    /// </summary>
    property OnReceiveData: TDataReceivedEvent read FOnReceiveData write FOnReceiveData;
    /// <summary>
    ///   Event to emit when data is send
    /// </summary>
    property OnSendData: TDataSendEvent read FOnSendData write FOnSendData;
    /// <summary>
    ///   Event to emit an error occurs
    /// </summary>
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

  /// <summary>
  ///   Bluetooth OBD Connection
  /// </summary>
  TBluetoothOBDConnection = class(TOBDConnection)
  private
    /// <summary>
    ///    Bluetooth class instance
    /// </summary>
    FBluetooth: TBluetooth;
  protected
    /// <summary>
    ///    Returns true if the adapter is connected
    /// </summary>
    function Connected: Boolean; override;
    /// <summary>
    ///    Bluetooth Receive Data event handler
    /// </summary>
    procedure OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    Bluetooth Send Data event handler
    /// </summary>
    procedure OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    Bluetooth Error event handler
    /// </summary>
    procedure OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
  public
    /// <summary>
    ///   Constructor: Allocate resources needed for the OBD adapter.
    /// <summary>
    constructor Create; override;
    /// <summary>
    ///   Destructor: Free internal allocated resources
    /// <summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connect to a OBD adapter (ELM327, OBDLink, ..)
    /// </summary>
    /// <param name="Params">
    ///   TOBDInterfaceConnectionParams Record with parameters for connecting
    ///   to the OBD adapter over Serial (COM) Port, Bluetooth, WiFi and FTDI.
    /// </param>
    function Connect(const Params: TOBDConnectionParams): Boolean; override;
    /// <summary>
    ///   Disconnect the connected OBD adapter
    /// </summary>
    function Disconnect: Boolean; override;
    /// <summary>
    ///   Write AT Command
    /// </summary>
    /// <param name="ATCommand">
    ///   The AT command string (AT Z, AT SP 0, AT R, ..)
    /// </param>
    function WriteATCommand(const ATCommand: string): Boolean; override;
    /// <summary>
    ///   Write ST Command
    /// </summary>
    /// <param name="STCommand">
    ///   The AT command string (STDI, STI, STFMR, ..)
    /// </param>
    function WriteSTCommand(const STCommand: string): Boolean; override;
    /// <summary>
    ///   Write OBD Command
    /// </summary>
    /// <param name="OBDCommand">
    ///   The OBD command string (01 00, 01 1C, ..)
    /// </param>
    function WriteOBDCommand(const OBDCommand: string): Boolean; override;
  end;

implementation

uses System.StrUtils;

//------------------------------------------------------------------------------
// THREAD EXECUTE
//------------------------------------------------------------------------------
procedure TBluetoothThread.Execute;
var
  ReceivedData: TBytes;
begin
  while not Terminated do
  begin
    if FBluetoothSocket.Connected then
    begin
      ReceivedData := FBluetoothSocket.ReceiveData;
      if Length(ReceivedData) > 0 then
      begin
        PostMessage(FWindowHandle, WM_BLUETOOTH_EVENT, WPARAM(Length(ReceivedData)), LPARAM(@ReceivedData[0]));
      end;
    end;
    Sleep(10);
  end;
end;

//------------------------------------------------------------------------------
// THREAD CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TBluetoothThread.Create(CreateSuspended: Boolean; Socket: TBluetoothSocket; WindowHandle: HWND);
begin
  // Inherited constructor
  inherited Create(CreateSuspended);
  // Set bluetooth socket
  FBluetoothSocket := Socket;
  // Set window handle
  FWindowHandle := WindowHandle;
end;

//------------------------------------------------------------------------------
// FTDI PORT THREAD PROC
//------------------------------------------------------------------------------
procedure TBluetooth.ThreadWndProc(var msg: TMessage);
var
  DataSize: Integer;
begin
  // Data received notification
  if (Msg.Msg = WM_BLUETOOTH_EVENT) and Connected then
  begin
    // Get data length from message
    DataSize := Msg.WParam;

    // Copy the received data from LPARAM to the temporary buffer
    if DataSize > 0 then
    begin
      // Limit to max length of buffer
      if DataSize > BT_In_Buffer_Size then DataSize := BT_In_Buffer_Size;
      // Move the data to the input buffer
      Move(Pointer(Msg.LParam)^, FInputBuffer[0], DataSize);
      // If read is successfull emit event that we have new data
      if Assigned(OnReceiveData) then OnReceiveData(Self, @FInputBuffer, DataSize);
    end;
  end else
  // Let Windows handle other messages.
  Msg.Result := DefWindowProc(FNotifyWnd, Msg.Msg, Msg.wParam, Msg.lParam);
end;

//------------------------------------------------------------------------------
// FTDI THREAD TERMINATE HANDLER
//------------------------------------------------------------------------------
procedure TBluetooth.EventThreadTerminate(Sender: TObject);
begin
  FEventThread := nil;
end;

//------------------------------------------------------------------------------
// COMPONENT CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TBluetooth.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Allocate a Window HANDLE to catch the Bluetooth Thread Notification Message
  FNotifyWnd := AllocateHWnd(ThreadWndProc);
end;

//------------------------------------------------------------------------------
// COMPONENT DESTRUCTOR
//------------------------------------------------------------------------------
destructor TBluetooth.Destroy;
begin
  // Disconnect if there is still an open connection
  if Connected then Disconnect;
  // Release the thread's Window HANDLE
  DeallocateHWnd(FNotifyWnd);
  //
  if FEventThread <> nil then
  begin
    FEventThread.Terminate;
    FEventThread.WaitFor;
    FreeAndNil(FEventThread);
  end;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TBluetooth.Connect(Manager: TBluetoothManager; Address: AnsiString): Boolean;
var
  PairedDevices: TBluetoothDeviceList;
  I: Integer;
begin
  Result := Connected;
  // Exit here when we're already connected
  if Result then Exit;
  // Get paired devices
  PairedDevices := Manager.GetPairedDevices;
  // Try to connect to the bluetooth device
  for I := 0 to PairedDevices.Count - 1 do
  begin
    // If this device matches the address
    if CompareText(PairedDevices[I].Address, String(Address)) = 0 then
    begin
      // Create a client socket
      FBluetoothSocket := PairedDevices[I].CreateClientSocket(TBluetoothUUID.Create(BT_SERVICE_GUID), True);
      if Assigned(FBluetoothSocket) then
      begin
        // Connect the socket
        FBluetoothSocket.Connect;
        // Create the thread for listening to incoming data
        FEventThread := TBluetoothThread.Create(True, FBluetoothSocket, FNotifyWnd);
        FEventThread.OnTerminate := EventThreadTerminate;
        FEventThread.Start;
        // Set connected flag
        FConnected := True;
        Result := True;
        // Break the loop because we find and connected the device
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
procedure TBluetooth.Disconnect;
begin
  if Connected and Assigned(FEventThread) then
  begin
    // Terminate thread
    FEventThread.Terminate;
    // Close bluetooth socket
    FBluetoothSocket.Close;
    // Free thread
    FreeAndNil(FEventThread);
    // Free bluetooth socket
    FreeAndNil(FBluetoothSocket);
    // Update connected flag
    FConnected := False;
  end;
end;

//------------------------------------------------------------------------------
// SEND DATA
//------------------------------------------------------------------------------
function TBluetooth.SendData(DataPtr: Pointer; DataSize: Cardinal): Cardinal;
begin
  Result := 0;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Send data
  if Assigned(OnSendData) then OnSendData(Self, DataPtr, DataSize);
  if Assigned(FBluetoothSocket) and FBluetoothSocket.Connected then
    FBluetoothSocket.SendData(TBytes(DataPtr));
end;

//------------------------------------------------------------------------------
// SEND BYTE
//------------------------------------------------------------------------------
function TBluetooth.SendByte(Value: Byte): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI CHARACTER
//------------------------------------------------------------------------------
function TBluetooth.SendChar(Value: AnsiChar): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI string
//------------------------------------------------------------------------------
function TBluetooth.SendString(S: Ansistring): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(PAnsiChar(S), L) = L;
end;

//------------------------------------------------------------------------------
// SEND C-STYLE ANSI string
//------------------------------------------------------------------------------
function TBluetooth.SendCString(S: PAnsiChar): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(S, L) = L;
end;

//------------------------------------------------------------------------------
// GET INTERFACE CONNECTED STATE
//------------------------------------------------------------------------------
function TBluetoothOBDConnection.Connected: Boolean;
begin
  Result := FBluetooth.Connected;
end;

//------------------------------------------------------------------------------
// BLUETOOTH RECEIVE DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TBluetoothOBDConnection.OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  if Assigned(OnDataReceived) then OnDataReceived(Self, DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// BLUETOOTH SEND DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TBluetoothOBDConnection.OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  if Assigned(OnDataSend) then OnDataSend(Self, DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// BLUETOOTH ERROR EVENT HANDLER
//------------------------------------------------------------------------------
procedure TBluetoothOBDConnection.OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
begin
  if Assigned(OnError) then OnError(Self, ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------
// BLUETOOTH OBD CONNECTION CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TBluetoothOBDConnection.Create;
begin
  // Inherited constructor
  inherited;
  // Create Bluetooth Class instance
  FBluetooth := TBluetooth.Create;
  FBluetooth.OnReceiveData := OnReceiveData;
  FBluetooth.OnSendData := OnSendData;
  FBluetooth.OnError := OnConnectionError;
end;

//------------------------------------------------------------------------------
// BLUETOOTH OBD CONNECTION DESTRUCTOR
//------------------------------------------------------------------------------
destructor TBluetoothOBDConnection.Destroy;
begin
  // Free the Bluetooth Class instance
  FBluetooth.Free;
  // Inherited destructor
  inherited;
end;

//------------------------------------------------------------------------------
// CONNECT TO OBD CONNECTION
//------------------------------------------------------------------------------
function TBluetoothOBDConnection.Connect(const Params: TOBDConnectionParams): Boolean;
begin
  Result := Connected;
  // Exit here is we're already connected
  if Result then Exit;
  // Exit here if the connection type is incorrect
  if Params.ConnectionType <> ctBluetooth then Exit;
  // Connect to the Wifi (TCP) Socket
  FBluetooth.Connect(Params.Manager, AnsiString(Params.SerialNumber));
end;

//------------------------------------------------------------------------------
// DISCONNECT THE CONNECTED OBD CONNECTION
//------------------------------------------------------------------------------
function TBluetoothOBDConnection.Disconnect: Boolean;
begin
  Result := Connected;
  if Result then
  begin
    FBluetooth.Disconnect;
    Result := Connected;
  end;
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND
//------------------------------------------------------------------------------
function TBluetoothOBDConnection.WriteATCommand(const ATCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('AT', ATCommand) = 1, ATCommand, Format(IfThen(Pos(' ', ATCommand) > 0, 'AT %s', 'AT%s'), [ATCommand]));
  Result := FBluetooth.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE ST COMMAND
//------------------------------------------------------------------------------
function TBluetoothOBDConnection.WriteSTCommand(const STCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('ST', STCommand) = 1, STCommand, Format(IfThen(Pos(' ', STCommand) > 0, 'ST %s', 'ST%s'), [STCommand]));
  Result := FBluetooth.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE OBD COMMAND
//------------------------------------------------------------------------------
function TBluetoothOBDConnection.WriteOBDCommand(const OBDCommand: string): Boolean;
begin
  Result := FBluetooth.Sendstring(AnsiString(OBDCommand));
end;

end.
