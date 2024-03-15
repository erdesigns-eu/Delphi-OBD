//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.FTDI.pas
// CONTENTS       : FTDI (USB) OBD Connection Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/02/2024
//------------------------------------------------------------------------------
unit OBD.Connection.FTDI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,

  OBD.Connection,
  OBD.Connection.Types,
  OBD.Connection.Constants;

type
  /// <summary>
  ///   FTDI (USB) Thread for events
  /// </summary>
  TFTDIThread = class(TThread)
  private
    /// <summary>
    ///   Windows Event Handle
    /// </summary
    FEventHandle: THandle;
    /// <summary>
    ///   Window Handle to notify owner class
    /// </summary
    FWindowHandle: HWND;
    /// <summary>
    ///   FTDI Handle
    /// </summary
    FFTDIHandle: DWORD;
  protected
    /// <summary>
    ///   Execute thread
    /// </summary
    procedure Execute; override;
  public
    /// <summary>
    ///   Constructor
    /// </summary
    constructor Create(CreateSuspended: Boolean; EventHandle: THandle; WindowHandle: HWND; FTDIHandle: DWORD);
  end;

  /// <summary>
  ///   FTDI (USB)
  /// </summary>
  TFTDI = class
  private
    /// <summary>
    ///   FTDI Port Handle
    /// </summary>
    FFTDIHandle: DWORD;
    /// <summary>
    ///   This is used for the thread
    /// </summary>
    FNotifyWnd: HWND;
    /// <summary>
    ///   This is used for the thread
    /// </summary>
    FEventHandle: THandle;
    /// <summary>
    ///   Thread for listening to FTDI events
    /// </summary>
    FEventThread: TThread;
    /// <summary>
    ///   Read Timeout
    /// </summary>
    FReadTimeout: DWORD;
    /// <summary>
    ///   Write Timeout
    /// </summary>
    FWriteTimeout: DWORD;
    /// <summary>
    ///   COM Port speed (brXXX)
    /// </summary>
    FBaudRate: TBaudRate;
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
    FInputBuffer: Array[0..FT_In_Buffer_Length] of Byte;
    /// <summary>
    ///   Set Read Timeout
    /// </summary>
    procedure SetReadTimeout(Value: DWORD);
    /// <summary>
    ///   Set Write Timeout
    /// </summary>
    procedure SetWriteTimeout(Value: DWORD);
    /// <summary>
    ///   Selects the baud rate
    /// </summary>
    /// <param name="Baudrate">
    ///   Changes the baudrate to one of the fixed baudrates of this enum.
    ///   Be aware to not exceed the maximum baudrate supported by the equipment used.
    /// </param>
    procedure SetBaudRate(Value: TBaudRate);
  protected
    /// <summary>
    ///    Returns true if FTDI port has been opened
    /// </summary>
    function Connected: Boolean;
    /// <summary>
    ///   Applies current settings like baudrate and flow control to the open FTDI Port
    /// </summary>
    /// <returns>
    ///   false if calls to activate these features failed.
    /// </returns>
    function ApplyFTDISettings: Boolean;
    /// <summary>
    ///   Thread proc: fetches received data from the port and calls the
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
    ///   Delivers the Windows API baudrate constant value for a given TBaudrate
    ///   enumeration value.
    /// </summary>
    /// <param name="bRate">
    ///   Enumeration value for which to return the Windows API constant value
    /// </param>
    /// <returns>
    ///   Windows API constant value or 0 if bRate = brCustom
    /// </returns>
    function BaudRateOf(Rate: TBaudRate): DWORD;

    /// <summary>
    ///   Opens the FTDI port. Returns false if something goes wrong
    /// </summary>
    function Connect(SerialNumber: AnsiString): Boolean;
    /// <summary>
    ///   Closes the FTDI port and releases control of it
    /// </summary>
    procedure Disconnect;
    /// <summary>
    ///   Reset FTDI device
    /// </summary>
    function Reset: Boolean;

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
    ///   Read Time-Out
    /// </summary>
    property ReadTimeout: DWORD read FReadTimeout write SetReadTimeout;
    /// <summary>
    ///   Write Time-Out
    /// </summary>
    property WriteTimeout: DWORD read FWriteTimeout write SetWriteTimeout;

    /// <summary>
    //    Speed (Baud Rate) in form of an enumberation value
    /// </summary>
    property BaudRate: TBaudRate read FBaudRate write SetBaudRate default br38400;
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
  ///   FTDI (USB) OBD Connection
  /// </summary>
  TFTDIOBDConnection = class(TOBDConnection)
  private
    /// <summary>
    ///    FTDI Port class instance
    /// </summary>
    FFTDI: TFTDI;
  protected
    /// <summary>
    ///    Returns true if the adapter is connected
    /// </summary>
    function Connected: Boolean; override;
    /// <summary>
    ///    FTDI Port Receive Data event handler
    /// </summary>
    procedure OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    FTDI Port Send Data event handler
    /// </summary>
    procedure OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    FTDI Port Error event handler
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
    ///   TOBDConnectionParams Record with parameters for connecting
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

uses System.Math, System.StrUtils;

//------------------------------------------------------------------------------
// EXTERNAL - FTD2XX.DLL
//------------------------------------------------------------------------------
function FT_GetStatus(ftHandle: DWORD; RxBytes, TxBytes, EventStatus: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_GetStatus';
function FT_SetTimeouts(ftHandle: DWORD; ReadTimeout, WriteTimeout: DWORD): FT_Result; stdcall; external FTDI_DLL name 'FT_SetTimeouts';
function FT_GetModemStatus(ftHandle: DWORD; ModemStatus: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_GetModemStatus';
function FT_GetQueueStatus(ftHandle: DWORD; RxBytes: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_GetQueueStatus';
function FT_Read(ftHandle: DWORD; FTInBuf: Pointer; BufferSize: LongInt; ResultPtr: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_Read';
function FT_Write(ftHandle: DWORD; FTOutBuf: Pointer; BufferSize: LongInt; ResultPtr: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_Write';
function FT_SetEventNotification(ftHandle: DWORD; EventMask: DWORD; pvArgs: DWORD): FT_Result; stdcall; external FTDI_DLL name 'FT_SetEventNotification';
function FT_OpenEx(pvArg1: Pointer; dwFlags: DWORD; ftHandle: Pointer): FT_Result; stdcall; external FTDI_DLL name 'FT_OpenEx';
function FT_Close(ftHandle: DWORD): FT_Result; stdcall; external FTDI_DLL name 'FT_Close';
function FT_ResetDevice(ftHandle: DWORD): FT_Result; stdcall; external FTDI_DLL name 'FT_ResetDevice';
function FT_SetBaudRate(ftHandle: DWORD; BaudRate: DWORD): FT_Result; stdcall; external FTDI_DLL name 'FT_SetBaudRate';
function FT_SetDataCharacteristics(ftHandle: DWORD; WordLength, StopBits, Parity: Byte): FT_Result; stdcall; external FTDI_DLL name 'FT_SetDataCharacteristics';
function FT_SetFlowControl(ftHandle: DWORD; FlowControl: Word; XonChar, XoffChar: Byte): FT_Result; stdcall; external FTDI_DLL name 'FT_SetFlowControl';

//------------------------------------------------------------------------------
// THREAD EXECUTE
//------------------------------------------------------------------------------
procedure TFTDIThread.Execute;
var
  WaitResult: DWORD;
  Status: FT_Result;
  EventStatus: DWORD;
  RXBytes: DWORD;
  TXBytes: DWORD;
begin
  while not Terminated do
  begin
    WaitResult := WaitForSingleObject(FEventHandle, INFINITE);
    if WaitResult = WAIT_OBJECT_0 then
    begin
      Status := FT_GetStatus(FFTDIHandle, @RxBytes, @TxBytes, @EventStatus);
      if Status = FT_OK then
      begin
        // Notify FTDI class there is a modem event
        if (EventStatus and FT_EVENT_MODEM_STATUS) <> 0 then
          PostMessage(FWindowHandle, WM_FTDI_MODEM_STATUS, 0, 0);
        // Notify FTDI class there is data to be read
        if RxBytes > 0 then
          PostMessage(FWindowHandle, WM_FTDI_RX_CHAR, 0, 0);
      end;
    end else Break;
  end;
end;

//------------------------------------------------------------------------------
// THREAD CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TFTDIThread.Create(CreateSuspended: Boolean; EventHandle: NativeUInt; WindowHandle: HWND; FTDIHandle: DWORD);
begin
  // Inherited constructor
  inherited Create(CreateSuspended);
  // Set event handle
  FEventHandle := EventHandle;
  // Set (FTDI Class Instance) window handle
  FWindowHandle := WindowHandle;
  // Set FTDI Handle
  FFTDIHandle := FTDIHandle;
end;

//------------------------------------------------------------------------------
// SET READ TIMEOUT
//------------------------------------------------------------------------------
procedure TFTDI.SetReadTimeout(Value: DWORD);
begin
  if Value <> FReadTimeout then
  begin
    FReadTimeout := Value;
    if Connected then ApplyFTDISettings;
  end;
end;

//------------------------------------------------------------------------------
// SET WRITE TIMEOUT
//------------------------------------------------------------------------------
procedure TFTDI.SetWriteTimeout(Value: DWORD);
begin
  if Value <> FWriteTimeout then
  begin
    FWriteTimeout := Value;
    if Connected then ApplyFTDISettings;
  end;
end;

//------------------------------------------------------------------------------
// SET THE FTDI (COM) BAUD RATE
//------------------------------------------------------------------------------
procedure TFTDI.SetBaudRate(Value: TBaudRate);
begin
  // Set new COM PORT Baud Rate
  FBaudRate := Value;
  // Apply changes
  if Connected then ApplyFTDISettings;
end;

//------------------------------------------------------------------------------
// GET CONNECTED STATUS
//------------------------------------------------------------------------------
function TFTDI.Connected: Boolean;
begin
  Result := FFTDIHandle <> INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------
// APPLY FTDI SETTINGS
//------------------------------------------------------------------------------
function TFTDI.ApplyFTDISettings: Boolean;
var
  Status: FT_Result;
begin
  // Default result to false to indicate failure unless all settings are applied successfully
  Result := False;

  // Exit here if we're not connected
  if not Connected then Exit;

  // Set the READ/WRITE timeouts
  Status := FT_SetTimeouts(FFTDIHandle, FReadTimeout, FWriteTimeout);
  if Status <> FT_OK then Exit;

  // Set the baud rate
  Status := FT_SetBaudRate(FFTDIHandle, BaudRateOf(BaudRate));
  if Status <> FT_OK then Exit;

  // Set data characteristics - 8 data bits, 1 stop bit, no parity
  Status := FT_SetDataCharacteristics(FFTDIHandle, FT_BITS_8, FT_STOP_BITS_1, FT_PARITY_NONE);
  if Status <> FT_OK then Exit;

  // Disable flow control
  Status := FT_SetFlowControl(FFTDIHandle, FT_FLOW_NONE, 0, 0);
  if Status <> FT_OK then Exit;

  // If we make it until here, applying the settings succeeded
  Result := True;
end;


//------------------------------------------------------------------------------
// FTDI PORT THREAD PROC
//------------------------------------------------------------------------------
procedure TFTDI.ThreadWndProc(var msg: TMessage);
var
  RXBytes: DWORD;
  RXBytesReceived: DWORD;
  ModemStatus: DWORD;
  Status: FT_Result;
begin
  // Modem status change notification
  if (Msg.Msg = WM_FTDI_MODEM_STATUS) and Connected then
  begin
    Status := FT_GetModemStatus(FFTDIHandle, @ModemStatus);
    if (Status = FT_OK) then
    begin
      if (Status and $00000010) <> 0 then
      begin
        // Clear to send is high - READY
      end
      else
      begin
        // Clear to send is low - STOP/PAUSE
      end;
      if (Status and $00000020) <> 0 then
      begin
        // Data set ready is high - READY
      end
      else
      begin
        // Data set ready is low - STOP/PAUSE (Offline/HW Error)
      end;
    end;
  end else
  // RX Data notification
  if (Msg.Msg = WM_FTDI_RX_CHAR) and Connected then
  begin
    // Check how many bytes are waiting to be read
    Status := FT_GetQueueStatus(FFTDIHandle, @RxBytes);
    if (Status = FT_OK) and (RXBytes > 0) then
    begin
      // Make sure we do not exceed the buffer size
      if RXBytes > FT_In_Buffer_Size then RXBytes := FT_In_Buffer_Size;
      // Read the data out of the device
      Status := FT_Read(FFTDIHandle, @FInputBuffer, RxBytes, @RXBytesReceived);
      // If read is successfull emit event that we have new data
      if (Status = FT_OK) and Assigned(OnReceiveData) then
        OnReceiveData(Self, @FInputBuffer, RXBytesReceived);
    end;
  end else
  // Let Windows handle other messages.
  Msg.Result := DefWindowProc(FNotifyWnd, Msg.Msg, Msg.wParam, Msg.lParam);
end;

//------------------------------------------------------------------------------
// FTDI THREAD TERMINATE HANDLER
//------------------------------------------------------------------------------
procedure TFTDI.EventThreadTerminate(Sender: TObject);
begin
  FEventThread := nil;
end;

//------------------------------------------------------------------------------
// COMPONENT CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TFTDI.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Initialize Handle with an invalid handle value (Disconnected)
  FFTDIHandle := INVALID_HANDLE_VALUE;
  // Allocate a Window HANDLE to catch the FTDI Thread Notification Messages (WM_FTDI_RX_CHAR, WM_FTDI_MODEM_STATUS)
  FNotifyWnd := AllocateHWnd(ThreadWndProc);
  // Create the event handle (create a hidden window)
  FEventHandle := CreateEvent(nil, False, False, nil);
  // Set default baudrate
  FBaudrate := br38400;
end;

//------------------------------------------------------------------------------
// COMPONENT DESTRUCTOR
//------------------------------------------------------------------------------
destructor TFTDI.Destroy;
begin
  // Ensure connection is closed and resources are released
  Disconnect;
  // Release the thread's Window HANDLE
  DeallocateHWnd(FNotifyWnd);
  // Close the event handle (necessary for cleanup)
  if FEventHandle <> 0 then
  begin
    CloseHandle(FEventHandle);
    FEventHandle := 0;
  end;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// GET BAUDRATE VALUE FROM BAUDRATE TYPE
//------------------------------------------------------------------------------
function TFTDI.BaudRateOf(Rate: TBaudRate): Cardinal;
begin
  Result := Win32BaudRates[Rate];
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TFTDI.Connect(SerialNumber: AnsiString): Boolean;
var
  SerialNumberBuffer: array [1..50] of AnsiChar;

  procedure SetSerialNumberBuffer;
  var
    I, L: Integer;
  begin
    // Initialize the buffer to zeros
    FillChar(SerialNumberBuffer, SizeOf(SerialNumberBuffer), #0);
    // Leave space for null terminator
    L := Min(Length(SerialNumber), SizeOf(SerialNumberBuffer) - 2);
    for I := 1 to L do SerialNumberBuffer[I] := SerialNumber[I];
  end;

  function SetupEventNotification: Boolean;
  var
    Status: FT_Result;
    EventMask: DWORD;
  begin
    // Define the events we want to be notified about
    EventMask := FT_EVENT_RXCHAR or FT_EVENT_MODEM_STATUS;
    // Call FT_SetEventNotification to set up event notification
    Status := FT_SetEventNotification(FFTDIHandle, EventMask, FEventHandle);
    // Return success status
    Result := Status = FT_OK;
  end;

begin
  // initialize result
  Result := False;
  // Exit here when we're already connected
  if Connected then Exit;
  // Fill the serial number buffer with the provided serial number
  SetSerialNumberBuffer;
  // Attempt to open the FTDI device by its serial number
  if FT_OpenEx(@SerialNumberBuffer, FT_OPEN_BY_SERIAL_NUMBER, @FFTDIHandle) = FT_OK then
  begin
    // Successfully opened the device, now set up event notification
    if SetupEventNotification then
    begin
      // Successfully set up event notifications, now proceed to create the event listening thread
      FEventThread := TFTDIThread.Create(False, FEventHandle, FNotifyWnd, FFTDIHandle);
      FEventThread.OnTerminate := EventThreadTerminate;
      // Apply FTDI device settings
      ApplyFTDISettings;
      // Connection and setup successfu
      Result := True;
    end
    else
    begin
      // Failed to set up event notification, close the FTDI handle
      FT_Close(FFTDIHandle);
    end;
  end;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
procedure TFTDI.Disconnect;
begin
  if Connected then
  begin
    // Signal the event to ensure the thread is not stuck waiting
    SetEvent(FEventHandle);
    // Ensure the thread is terminated
    if Assigned(FEventThread) then
    begin
      // Signal termination
      FEventThread.Terminate;
      // Wait for the thread to finish
      FEventThread.WaitFor;
      // Free the thread
      FreeAndNil(FEventThread);
    end;
    // Close the FTDI device handle
    FT_Close(FFTDIHandle);
    // Reset the handle
    FFTDIHandle := INVALID_HANDLE_VALUE;
  end;
end;

//------------------------------------------------------------------------------
// RESET DEVICE
//------------------------------------------------------------------------------
function TFTDI.Reset: Boolean;
begin
  Result := False;
  // Exit here if we're not connected
  if not Connected then Exit;
  // Reset Device
  Result := FT_ResetDevice(FFTDIHandle) = FT_OK;
end;

//------------------------------------------------------------------------------
// SEND DATA
//------------------------------------------------------------------------------
function TFTDI.SendData(DataPtr: Pointer; DataSize: Cardinal): Cardinal;
var
  BytesWritten: DWORD;
begin
  Result := 0;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Send data
  if Assigned(OnSendData) then OnSendData(Self, DataPtr, DataSize);
  if (FT_Write(FFTDIHandle, DataPtr, DataSize, @BytesWritten) = FT_OK) then
  begin
    Result := BytesWritten;
    if (BytesWritten <> DataSize) then
    begin
      // TODO: Implement better error handling
      // Not all bytes are written - handle error/exception
      if Assigned(OnError) then OnError(Self, 0, Format('Written bytes differs from DataSize: (%d bytes) - (%d bytes)', [BytesWritten, DataSize]));
    end;
  end;
end;

//------------------------------------------------------------------------------
// SEND BYTE
//------------------------------------------------------------------------------
function TFTDI.SendByte(Value: Byte): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI CHARACTER
//------------------------------------------------------------------------------
function TFTDI.SendChar(Value: AnsiChar): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI string
//------------------------------------------------------------------------------
function TFTDI.SendString(S: Ansistring): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(PAnsiChar(S), L) = L;
end;

//------------------------------------------------------------------------------
// SEND C-STYLE ANSI string
//------------------------------------------------------------------------------
function TFTDI.SendCString(S: PAnsiChar): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(S, L) = L;
end;

//------------------------------------------------------------------------------
// FTDI OBD INTERFACE CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TFTDIOBDConnection.Create;
begin
  // Inherited constructor
  inherited;
  // Create FTDI Class instance
  FFTDI := TFTDI.Create;
  FFTDI.OnReceiveData := OnReceiveData;
  FFTDI.OnSendData := OnSendData;
  FFTDI.OnError := OnConnectionError;
end;

//------------------------------------------------------------------------------
// FTDI OBD INTERFACE DESTRUCTOR
//------------------------------------------------------------------------------
destructor TFTDIOBDConnection.Destroy;
begin
  // Free the FTDI Class instance
  FFTDI.Free;
  // Inherited destructor
  inherited;
end;

//------------------------------------------------------------------------------
// GET INTERFACE CONNECTED STATE
//------------------------------------------------------------------------------
function TFTDIOBDConnection.Connected: Boolean;
begin
  Result := FFTDI.Connected;
end;

//------------------------------------------------------------------------------
// FTDI PORT RECEIVE DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TFTDIOBDConnection.OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  if Assigned(OnDataReceived) then OnDataReceived(Self, DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// FTDI PORT SEND DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TFTDIOBDConnection.OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  if Assigned(OnDataSend) then OnDataSend(Self, DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// FTDI PORT ERROR EVENT HANDLER
//------------------------------------------------------------------------------
procedure TFTDIOBDConnection.OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
begin
  if Assigned(OnError) then OnError(Self, ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------
// CONNECT TO OBD CONNECTION
//------------------------------------------------------------------------------
function TFTDIOBDConnection.Connect(const Params: TOBDConnectionParams): Boolean;
begin
  Result := Connected;
  // Exit here is we're already connected
  if Result then Exit;
  // Exit here if the connection type is incorrect
  if Params.ConnectionType <> ctFTDI then Exit;
  // Connect to the FTDI port
  FFTDI.BaudRate := Params.FTDIBaudRate;
  Result := FFTDI.Connect(Params.SerialNumber);
end;

//------------------------------------------------------------------------------
// DISCONNECT THE CONNECTED OBD CONNECTION
//------------------------------------------------------------------------------
function TFTDIOBDConnection.Disconnect: Boolean;
begin
  Result := Connected;
  if Result then
  begin
    FFTDI.Disconnect;
    Result := Connected;
  end;
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND
//------------------------------------------------------------------------------
function TFTDIOBDConnection.WriteATCommand(const ATCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('AT', ATCommand) = 1, ATCommand, Format(IfThen(Pos(' ', ATCommand) > 0, 'AT %s', 'AT%s'), [ATCommand]));
  Result := FFTDI.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE ST COMMAND
//------------------------------------------------------------------------------
function TFTDIOBDConnection.WriteSTCommand(const STCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('ST', STCommand) = 1, STCommand, Format(IfThen(Pos(' ', STCommand) > 0, 'ST %s', 'ST%s'), [STCommand]));
  Result := FFTDI.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE OBD COMMAND
//------------------------------------------------------------------------------
function TFTDIOBDConnection.WriteOBDCommand(const OBDCommand: string): Boolean;
begin
  Result := FFTDI.Sendstring(AnsiString(OBDCommand));
end;

end.

