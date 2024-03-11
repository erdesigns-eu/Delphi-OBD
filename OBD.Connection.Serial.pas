//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.Serial.pas
// CONTENTS       : Serial (COM Port) OBD Connection Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/02/2024
//------------------------------------------------------------------------------
unit OBD.Connection.Serial;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,

  OBD.Connection,
  OBD.Connection.Types,
  OBD.Connection.Constants;

type
  /// <summary>
  ///   Serial (COM Port)
  /// </summary>
  TSerialPort = class
  private
    /// <summary>
    ///   Device Handle (File Handle)
    /// </summary>
    FHandle: THandle;
    /// <summary>
    ///   # of the COM port to use, or pnCustom to use custom port name
    /// </summary>
    FPort: string;
    /// <summary>
    ///   COM Port speed (brXXX)
    /// </summary>
    FBaudRate: TBaudRate;
    /// <summary>
    ///   Baud rate (actual numeric value)
    /// </summary>
    FBaudRateValue: DWORD;
    /// <summary>
    ///   Data bits size (dbXXX)
    /// </summary>
    FDataBits: TDataBits;
    /// <summary>
    ///   How many stop bits to use (sbXXX)
    /// </summary>
    FStopBits: TStopBits;
    /// <summary>
    ///   Type of parity to use (ptXXX)
    /// </summary>
    FParity: TParity;
    /// <summary>
    ///   Type of hw handshaking (hw flow control) to use (hfXXX)
    /// </summary>
    FHwFlow: THwFlowControl;
    /// <summary>
    ///   Type of sw handshaking (sw flow control) to use (sFXXX)
    /// </summary>
    FSwFlow: TSwFlowControl;
    /// <summary>
    ///   Size of the input buffer
    /// </summary>
    FInBufSize: DWORD;
    /// <summary>
    ///   Size of the output buffer
    /// </summary>
    FOutBufSize: DWORD;
    /// <summary>
    ///   Size of a data packet
    /// </summary>
    FPacketSize: Smallint;
    /// <summary>
    ///   ms to wait for a complete packet (<=0 = disabled)
    /// </summary>
    FPacketTimeout: Integer;
    /// <summary>
    ///   What to do with incomplete packets (pmXXX)
    /// </summary>
    FPacketMode: TPacketMode;
    /// <summary>
    ///   Event to raise on data reception (asynchronous)
    /// </summary>
    FOnReceiveData: TDataReceivedEvent;
    /// <summary>
    ///   Event to raise on packet reception (asynchronous)
    /// </summary>
    FOnReceivePacket: TPacketReceivedEvent;
    /// <summary>
    ///   Event to raise on data send (asynchronous)
    /// </summary>
    FOnSendData: TDataSendEvent;
    /// <summary>
    ///   Event to emit when an error occurs
    /// </summary>
    FOnError: TErrorEvent;
    /// <summary>
    ///   ms of delay between COM port pollings
    /// </summary>
    FPollingDelay: Word;
    /// <summary>
    ///   Specifies if the DTR line must be enabled/disabled on connect
    /// </summary>
    FEnableDTROnOpen: Boolean;
    /// <summary>
    ///   Output timeout - milliseconds
    /// </summary>
    FOutputTimeout: Word;
    /// <summary>
    ///   Timeout for ReadData
    /// </summary>
    FInputTimeout: DWORD;
    /// <summary>
    ///   Set to TRUE to prevent hangs when no device connected or device is OFF
    /// </summary>
    FCkLineStatus: Boolean;
    /// <summary>
    ///   This is used for the timer
    /// </summary>
    FNotifyWnd: HWND;
    /// <summary>
    ///   Temporary buffer (RX) - used internally
    /// </summary>
    FTempInBuffer: Pointer;
    /// <summary>
    ///   Time of the first byte of current RX packet
    /// </summary>
    FFirstByteOfPacketTime: DWORD;
    /// <summary>
    ///   Number of RX polling timer pauses
    /// </summary>
    FRXPollingPauses: Integer;
    /// <summary>
    ///   Flag used to guard against nested timer events which can be caused
    ///   if somebody calls something in such an event which queries/polls
    ///   Windows messages, like a call to MessageDlg.
    /// </summary>
    FIsInOnTimer: Boolean;

    /// <summary>
    ///   Sets the COM port handle
    /// </summary>
    /// <param name="Value">
    ///   File handle to the port
    /// </param>
    procedure SetHandle(Value: THandle);
    /// <summary>
    ///   Selects the COM port to use
    /// </summary>
    /// <param name="Value">
    ///   Number from the predefined enum of the port to use
    /// </param>
    procedure SetPort(Value: string);
    /// <summary>
    ///   Selects the baud rate
    /// </summary>
    /// <param name="Baudrate">
    ///   Changes the baudrate to one of the fixed baudrates of this enum.
    ///   Be aware to not exceed the maximum baudrate supported by the equipment used.
    /// </param>
    procedure SetBaudRate(Value: TBaudRate);
    /// <summary>
    ///   Selects the baud rate (actual baud rate value)
    /// </summary>
    /// <param name="Value">
    ///   Freely defines a baudrate. Be aware to not exceed the maximum baudrate
    ///   supported by the equipment used.
    /// </param>
    procedure SetBaudRateValue(Value: DWORD);
    /// <summary>
    ///   Selects the number of data bits
    /// </summary>
    /// <param name="Value">
    ///   New number of data bits per byte. Be aware that not all ports support
    ///   all values defined but we can't detect this.
    /// </param>
    procedure SetDataBits(Value: TDataBits);
    /// <summary>
    ///   Selects the number of stop bits
    /// </summary>
    /// <param name="Value">
    ///   New number of stopbits. Be aware that not all ports support all values
    ///   defined but we can't detect this. Especially 1.5 is often unsupported.
    /// </param>
    procedure SetStopBits(Value: TStopBits);
    /// <summary>
    ///   Selects the kind of parity
    /// </summary>
    /// <param name="Value">
    ///   New parity value. Be aware that not all ports support all values
    ///   defined but we can't detect this.
    /// </param>
    procedure SetParity(Value: TParity);
    /// <summary>
    ///   Selects the kind of hardware flow control
    /// </summary>
    /// <param name="Value">
    ///   New value for the hardware flow control mode to use.
    /// </param>
    procedure SetHwFlowControl(Value: THwFlowControl);
    /// <summary>
    ///   Selects the kind of software flow control
    /// </summary>
    /// <param name="Value">
    ///   New value for the software flow control mode to use.
    /// </param>
    procedure SetSwFlowControl(Value: TSwFlowControl);
    /// <summary>
    ///   Sets the RX buffer size
    /// </summary>
    /// <param name="Value">
    ///   New receive buffer size in byte. Restricted to MinRXBufferSize and
    ///   MaxRXBufferSize.
    /// </param>
    procedure SetInBufSize(Value: DWORD);
    /// <summary>
    ///   Sets the TX buffer size
    /// </summary>
    /// <param name="Value">
    ///   New receive buffer size in byte. Restricted to MinTBufferSize and
    ///   MaxTBufferSize.
    /// </param>
    procedure SetOutBufSize(Value: DWORD);
    /// <summary>
    ///   Sets the size of incoming packets
    /// </summary>
    /// <param name="Value">
    ///   Size of incomming packets in byte. If ther Receive buffer size is smaller
    ///   than the packet size specified here it will be automatically increased
    ///   to the size specified here.
    /// </param>
    procedure SetPacketSize(Value: Smallint);
    /// <summary>
    ///   Sets the timeout for incoming packets
    /// </summary>
    /// <param name="Value">
    ///   Time in ms to wait for arrival of incomming packets before assuming a
    ///   timeout. It cannot be less than polling delay + some extra ms so it is
    ///   adjusted to that if value given is less than polling delay.
    /// </param>
    procedure SetPacketTimeout(Value: Integer);
    /// <summary>
    ///   Sets the delay between polling checks
    /// </summary>
    /// <param name="Value">
    ///   New polling interval in ms. Be aware that accurancy is a bit limited
    ///   because of the use of the standard Windows timer.
    /// </param>
    procedure SetPollingDelay(Value: Word);
    /// <summary>
    ///   Applies current settings like baudrate and flow control to the open COM port
    /// </summary>
    /// <returns>
    ///   false if WInAPI call to activate these features failed.
    /// </returns>
    function ApplyCOMSettings: Boolean;
    /// <summary>
    ///   Polling proc: fetches received data from the port and calls the
    ///   apropriate receive callbacks if necessary
    /// </summary>
    procedure TimerWndProc(var msg: TMessage);
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
    //    Opens the COM port. Returns false if something goes wrong.
    /// </summary>
    function Connect: Boolean;
    /// <summary>
    ///   Closes the COM port and releases control of it
    /// </summary>
    procedure Disconnect;
    /// <summary>
    ///    Returns true if COM port has been opened
    /// </summary>
    function Connected: Boolean;
    /// <summary>
    ///   Returns the current state of CTS, DSR, RING and RLSD (CD) lines.
    ///   The function fails if the hardware does not support the control-register
    ///   values (that is, returned set is always empty).
    /// </summary>
    function GetLineStatus: TLineStatusSet;
    /// <summary>
    ///   Returns true if polling has not been paused
    /// </summary>
    function IsPolling: Boolean;
    /// <summary>
    ///   Pauses polling
    /// </summary>
    procedure PausePolling;
    /// <summary>
    ///   Re-starts polling (after pause)
    /// </summary>
    procedure ContinuePolling;
    /// <summary>
    ///   Flushes the RX/TX buffers
    /// </summary>
    /// <param name="inBuf">
    ///   when true the receive buffer is cleared
    /// </param>
    /// <param name="outBuf">
    ///   when true the transmit buffer is cleared
    /// </param>
    function FlushBuffers(inBuf, outBuf: Boolean): Boolean;
    /// <summary>
    ///   Returns number of received bytes in the RX buffer
    /// </summary>
    function CountRX: Integer;
    /// <summary>
    ///   Returns the output buffer free space or 65535 if not connected
    /// </summary>
    function OutFreeSpace: Word;
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
    ///   Sends binary data. Returns number of bytes sent. Timeout overrides
    ///   the value specifiend in the OutputTimeout property
    /// </summary>
    /// <param name="DataPtr">
    ///   Pointer to the memory containing the data to send
    /// </param>
    /// <param name="DataSize">
    ///   Number of bytes to send
    /// </param>
    /// <param name="Timeout">
    ///   Timeout in ms. If this time elapsed and not all data could be sent
    ///   further sending will be stoped.
    /// </param>
    /// <returns>
    ///   Number of bytes sent
    /// </returns>
    function SendDataEx(DataPtr: PAnsiChar; DataSize, Timeout: DWORD): DWORD;
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
    function Sendstring(S: Ansistring): Boolean;
    /// <summary>
    //    Sends a C-style Ansi string (NULL terminated)
    /// </summary>
    /// <param name="s">
    ///   string to send
    /// </param>
    function SendCstring(S: PAnsiChar): Boolean;
    /// <summary>
    //    Reads binary data. Returns number of bytes read
    /// </summary>
    /// <param name="DataPtr">
    ///   Pointer to the memory where the read out data shall be stored.
    /// </param>
    /// <param name="MaxDataSize">
    ///   Maximum available memory space in byte.
    /// </param>
    /// <returns>
    ///   Number of bytes read, maximum MaxDataSize
    /// </returns>
    function ReadData(DataPtr: PAnsiChar; MaxDataSize: DWORD): DWORD;
    /// <summary>
    //    Reads a byte. Returns true if the byte has been read
    /// </summary>
    /// <param name="Value">
    ///   Byte variable into which the byte read out shall be written
    /// </param>
    function ReadByte(var Value: Byte): Boolean;
    /// <summary>
    //    Reads a AnsiChar. Returns true if AnsiChar has been read
    /// </summary>
    /// <param name="Value">
    ///   AnsiChar variable into which the byte read out shall be written
    /// </param>
    function ReadChar(var Value: AnsiChar): Boolean;
    /// <summary>
    ///   Set DTR line high (onOff=TRUE) or low (onOff=FALSE).
    ///   You must not use HW handshaking.
    /// </summary>
    /// <param name="onOff">
    ///   true for setting Data Terminal Ready line to high, false for low
    /// </param>
    procedure ToggleDTR(onOff: Boolean);
    /// <summary>
    ///   Set RTS line high (onOff=TRUE) or low (onOff=FALSE).
    ///   You must not use HW handshaking.
    /// </summary>
    /// <param name="onOff">
    ///   true for setting Request To Send line to high, false for low
    /// </param>
    procedure ToggleRTS(onOff: Boolean);
    /// <summary>
    ///   Returns the maximum size the receive buffer can be set to in byte
    /// </summary>
    function GetMaxRXBufferSize: DWORD;
    /// <summary>
    ///   Returns the maximum size the transmit buffer can be set to in byte
    /// </summary>
    function GetMaxTXBufferSize: DWORD;
    /// <summary>
    ///   Returns the maximum size the receive buffer can be set to in byte
    /// </summary>
    function GetMinRXBufferSize: DWORD;
    /// <summary>
    ///   Returns the maximum size the transmit buffer can be set to in byte
    /// </summary>
    function GetMinTXBufferSize: DWORD;
    /// <summary>
    ///   Puts the port into "break" state, means starts to send a break signal.
    ///   It does not flush any buffers and keeps sending this signal until
    ///   ClearCommBreak is being called.
    /// </summary>
    /// <returns>
    ///   true for success, false in case of failure
    /// </returns>
    function SetCommBreak: Boolean;
    /// <summary>
    ///   Stops sending a break signal started with SetCommBreak
    /// </summary>
    /// <returns>
    ///   true for success, false in case of failure
    /// </returns>
    function ClearCommBreak: Boolean;
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
    ///   Calculates the time in ms it takes to receive a certain number of bytes at
    ///   a certain baudrate. The calculation is based on the current serial settings:
    ///   baudrate, databits per byte, number of stoppbits and parity.
    /// </summary>
    /// <param name="DataSize">
    ///   Number of bytes to send or receive
    /// </param>
    /// <returns>
    ///   Time in ms it takes to receive or send this amount of data
    /// </returns>
    function DelayForRX(DataSize: DWORD): DWORD;

    /// <summary>
    ///   Handle of the COM port (for TAPI...) [read/write]
    /// </summary>
    property Handle: THandle read FHandle write SetHandle;
    /// <summary>
    ///   Number of the COM Port to use (or pnCustom for port by name)
    /// </summary>
    property Port: string read FPort write SetPort;
    /// <summary>
    //    Speed (Baud Rate) in form of an enumberation value
    /// </summary>
    property BaudRate: TBaudRate read FBaudRate write SetBaudRate default br9600;
    /// <summary>
    ///   Speed (Actual Baud Rate value) if not an enumeration value is used
    /// </summary>
    property BaudRateValue: DWORD read FBaudRateValue write SetBaudRateValue default 9600;
    /// <summary>
    ///   Data bits to use (5..8, for the 8250 the use of 5 data bits with 2 stop
    ///   bits is an invalid combination, as is 6, 7, or 8 data bits with 1.5 stop
    ///   bits)
    /// </summary>
    property DataBits: TDataBits read FDataBits write SetDataBits default db8BITS;
    /// <summary>
    ///  Stop bits to use (1, 1.5, 2). Be aware that not all ports support 1.5
    /// </summary>
    property StopBits: TStopBits read FStopBits write SetStopBits default sb1BITS;
    /// <summary>
    ///   Kind of Parity to use (none, odd, even, mark, space)
    /// </summary>
    property Parity: TParity read FParity write SetParity default ptNONE;
    /// <summary>
    ///   Kind of Hardware Flow Control to use:
    ///   hfNONE          none
    ///   hfNONERTSON     no flow control but keep RTS line on
    ///   hfRTSCTS        Request-To-Send/Clear-To-Send
    /// </summary>
    property HwFlow: THwFlowControl read FHwFlow write SetHwFlowControl default hfNONERTSON;
    /// <summary>
    ///   Kind of Software Flow Control to use:
    ///   sfNONE          none
    ///   sfXONXOFF       XON/XOFF
    /// </summary>
    property SwFlow: TSwFlowControl read FSwFlow write SetSwFlowControl default sfNONE;
    /// <summary>
    ///   Input buffer size in byte (suggested - driver might ignore this setting !)
    /// <summary>
    property InBufSize: DWORD read FInBufSize write SetInBufSize default DEFAULT_BUFFER_SIZE;
    /// <summary>
    ///   Output buffer size in byte (suggested - driver usually ignores this setting !)
    /// </summary>
    property OutBufSize: DWORD read FOutBufSize write SetOutBufSize default DEFAULT_BUFFER_SIZE;
    /// <summary>
    ///   RX packet size (this value must be less than InBufSize)
    ///   A value <= 0 means "no packet mode" (i.e. standard mode enabled)
    /// </summary>
    property PacketSize: smallint read FPacketSize write SetPacketSize default -1;
    /// <summary>
    ///   Timeout (ms) for a complete packet (in RX)
    /// </summary>
    property PacketTimeout: integer read FPacketTimeout write SetPacketTimeout default -1;
    /// <summary>
    ///   What to do with incomplete packets (in RX)
    /// </summary>
    property PacketMode: TPacketMode read FPacketMode write FPacketMode default pmDiscard;
    /// <summary>
    ///   ms of delay between COM port pollings. Since they are handled by
    ///   standard Windows timer accurancy is not overly high
    /// </summary>
    property PollingDelay: word read FPollingDelay write SetPollingDelay default DEFAULT_POLLING_DELAY;
    /// <summary>
    ///   Set to TRUE to enable DTR line on connect and to leave it on until disconnect.
    ///   Set to FALSE to disable DTR line on connect.
    /// </summary>
    property EnableDTROnOpen: Boolean read FEnableDTROnOpen write FEnableDTROnOpen default true;
    /// <summary>
    ///   Output timeout (milliseconds)
    /// </summary>
    property OutputTimeout: word read FOutputTimeOut write FOutputTimeout default DEFAULT_OUTPUT_TIMEOUT;
    /// <summary>
    ///   Input timeout (milliseconds)
    /// </summary>
    property InputTimeout: DWORD read FInputTimeOut write FInputTimeout default DEFAULT_INPUT_TIMEOUT;
    /// <summary>
    ///   Set to TRUE to prevent hangs when no device connected or device is OFF
    /// </summary>
    property CheckLineStatus: Boolean read FCkLineStatus write FCkLineStatus default False;
    /// <summary>
    ///   Event to raise when there is data available (input buffer has data)
    ///   (called only if PacketSize <= 0)
    /// </summary>
    property OnReceiveData: TDataReceivedEvent read FOnReceiveData write FOnReceiveData;
    /// <summary>
    ///   Event to raise when there is data packet available (called only if PacketSize > 0)
    /// </summary>
    property OnReceivePacket: TPacketReceivedEvent read FOnReceivePacket write FOnReceivePacket;
    /// <summary>
    ///   Event to raise when data is send
    /// </summary>
    property OnSendData: TDataSendEvent read FOnSendData write FOnSendData;
    /// <summary>
    ///   Event to emit an error occurs
    /// </summary>
    property OnError: TErrorEvent read FOnError write FOnError;
    /// <summary>
    ///   Returns the maximum size the receive buffer can be set to in byte
    /// </summary>
    property MaxRXBufferSize: DWORD read GetMaxRXBufferSize;
    /// <summary>
    ///   Returns the maximum size the transmit buffer can be set to in byte
    /// </summary>
    property MaxTXBufferSize: DWORD read GetMaxTXBufferSize;
    /// <summary>
    ///   Returns the maximum size the receive buffer can be set to in byte
    /// </summary>
    property MinRXBufferSize: DWORD read GetMinRXBufferSize;
    /// <summary>
    ///   Returns the maximum size the transmit buffer can be set to in byte
    /// </summary>
    property MinTXBufferSize: DWORD read GetMinTXBufferSize;
  end;

  /// <summary>
  ///   Serial (COM Port) OBD Connection
  /// </summary>
  TSerialOBDConnection = class(TOBDConnection)
  private
    /// <summary>
    ///    Serial Port class instance
    /// </summary>
    FSerialPort: TSerialPort;
  protected
    /// <summary>
    ///    Returns true if the adapter is connected
    /// </summary>
    function Connected: Boolean; override;
    /// <summary>
    ///    Serial Port Receive Data event handler
    /// </summary>
    procedure OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    Serial Port Send Data event handler
    /// </summary>
    procedure OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    Serial Port Error event handler
    /// </summary>
    procedure OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
  public
    /// <summary>
    ///   Constructor: Allocate resources needed for the OBD connection.
    /// <summary>
    constructor Create; override;
    /// <summary>
    ///   Destructor: Free internal allocated resources
    /// <summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    /// <param name="Params">
    ///   TOBDConnectionParams Record with parameters for connecting
    ///   to the OBD adapter over Serial (COM) Port, Bluetooth, WiFi and FTDI.
    /// </param>
    function Connect(const Params: TOBDConnectionParams): Boolean; override;
    /// <summary>
    ///   Disconnect the connected OBD Adapter
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
// CONNECT TO EXTRERNAL OPENED COM PORT
// NOTE: Setting to INVALID_PORT_HANDLE acts as Disconnect.
//------------------------------------------------------------------------------
procedure TSerialPort.SetHandle(Value: NativeUInt);
begin
  // If same COM port then do nothing
  if FHandle = Value then Exit;
  // If value is RELEASE_NOCLOSE_PORT then stop controlling the COM port
  // without closing in
  if Value = RELEASE_NOCLOSE_PORT then
  begin
    // Stop the timer
    if Connected then KillTimer(FNotifyWnd, 1);
    // No more connected
    FHandle := INVALID_HANDLE_VALUE;
  end else
  begin
    // Disconnect
    Disconnect;
    // If Value is INVALID_HANDLE_VALUE then exit now
    if Value = INVALID_HANDLE_VALUE then Exit;
    // Set COM port handle
    FHandle := Value;
    // Start the timer (used for polling)
    SetTimer(FNotifyWnd, 1, FPollingDelay, nil);
  end;
end;

//------------------------------------------------------------------------------
// SET THE COM PORT
//------------------------------------------------------------------------------
procedure TSerialPort.SetPort(Value: string);
begin
  // Exit when already connected
  if Connected then Exit;
  // Change COM port
  FPort := Value;
end;

//------------------------------------------------------------------------------
// SET THE COM PORT BAUD RATE
//------------------------------------------------------------------------------
procedure TSerialPort.SetBaudRate(Value: TBaudRate);
begin
  // Set new COM PORT Baud Rate
  FBaudRate := Value;
  // Set the Baud Rate Value
  if FBaudRate <> brCustom then FBaudRateValue := BaudRateOf(FBaudRate);
  // Apply changes
  if Connected then ApplyCOMSettings;
end;

//------------------------------------------------------------------------------
// SET THE COM PORT BAUD RATE VALUE
//------------------------------------------------------------------------------
procedure TSerialPort.SetBaudRateValue(Value: Cardinal);
begin
  // Set the COM PORT Baud Rate to custom
  FBaudRate := brCustom;
  // Set the COM PORT Baud Rate Value
  FBaudRateValue := Value;
  // Apply changes
  if Connected then ApplyCOMSettings;
end;

//------------------------------------------------------------------------------
// SET THE COM PORT DATA BITS
//------------------------------------------------------------------------------
procedure TSerialPort.SetDataBits(Value: TDataBits);
begin
  // Set the COM PORT Data Bits
  FDataBits := Value;
  // Apply changes
  if Connected then ApplyCOMSettings;
end;

//------------------------------------------------------------------------------
// SET THE COM PORT STOP BITS
//------------------------------------------------------------------------------
procedure TSerialPort.SetStopBits(Value: TStopBits);
begin
  // Set the COM PORT Stop Bits
  FStopBits := Value;
  // Apply changes
  if Connected then ApplyCOMSettings;
end;

//------------------------------------------------------------------------------
// SET THE COM PORT PARITY
//------------------------------------------------------------------------------
procedure TSerialPort.SetParity(Value: TParity);
begin
  // Set the COM PORT parity
  FParity := Value;
  // Apply changes
  if Connected then ApplyCOMSettings;
end;

//------------------------------------------------------------------------------
// SET THE HARDWARE FLOW CONTROL KIND
//------------------------------------------------------------------------------
procedure TSerialPort.SetHwFlowControl(Value: THwFlowControl);
begin
  // Set the COM PORT Hardware Flow control
  FHwFlow := Value;
  // Apply changes
  if Connected then ApplyCOMSettings;
end;

//------------------------------------------------------------------------------
// SET THE SOFTWARE FLOW CONTROL KIND
//------------------------------------------------------------------------------
procedure TSerialPort.SetSwFlowControl(Value: TSwFlowControl);
begin
  // Set the COM PORT Software Flow control
  FSwFlow := Value;
  // Apply changes
  if Connected then ApplyCOMSettings;
end;

//------------------------------------------------------------------------------
// SET THE COM PORT RX BUFFER SIZE
//------------------------------------------------------------------------------
procedure TSerialPort.SetInBufSize(Value: Cardinal);
begin
  // Exit when already connected
  if Connected then Exit;
  // Free the temporary input buffer
  FreeMem(FTempInBuffer, FInBufSize);
  // Set new input buffer size
  if Value > cMaxRXBufferSize then Value := cMaxRXBufferSize;
  if Value < cMinRXBufferSize then Value := cMinRXBufferSize;
  FInBufSize := Value;
  // Allocate the temporary input buffer
  FTempInBuffer := AllocMem(FInBufSize);
  // Adjust the RX packet size
  SetPacketSize(FPacketSize);
end;

//------------------------------------------------------------------------------
// SET THE COM PORT TX BUFFER SIZE
//------------------------------------------------------------------------------
procedure TSerialPort.SetOutBufSize(Value: Cardinal);
begin
  // Exit when already connected
  if Connected then Exit;
  // Set new output buffer size
  if Value > cMaxTXBufferSize then Value := cMaxTXBufferSize;
  if Value < cMinTXBufferSize then Value := cMinTXBufferSize;
  FOutBufSize := Value;
end;

//------------------------------------------------------------------------------
// SET THE INCOMING PACKET SIZE
//------------------------------------------------------------------------------
procedure TSerialPort.SetPacketSize(Value: SmallInt);
begin
  // PacketSize <= 0 if data isn't to be "packetized"
  if Value <= 0 then FPacketSize := -1 else
  // If the PacketSize if greater than then RX buffer size then
  // increase the RX buffer size
  if DWORD(Value) > FInBufSize then
  begin
    FPacketSize := Value;
    SetInBufSize(FPacketSize);
  end;
end;

//------------------------------------------------------------------------------
// SET THE INCOMING PACKET TIMEOUT
//------------------------------------------------------------------------------
procedure TSerialPort.SetPacketTimeout(Value: Integer);
begin
  // PacketTimeout <= 0 if packet timeout is to be disabled
  if Value < 1 then FPacketTimeout := -1 else
  // PacketTimeout cannot be less than polling delay + some extra ms
  if Value < FPollingDelay then FPacketTimeout := FPollingDelay + (FPollingDelay * 40) div 100;
end;

//------------------------------------------------------------------------------
// SET THE DELAY BETWEEN POLLING
//------------------------------------------------------------------------------
procedure TSerialPort.SetPollingDelay(Value: Word);
begin
  // Set the value to minimum polling delay if lower
  if Value < MIN_POLLING_DELAY then Value := MIN_POLLING_DELAY;
  // Only update if the new delay is not the same
  if Value <> FPollingDelay then
  begin
    // Stop the timer
    if Connected then KillTimer(FNotifyWnd, 1);
    // Store new delay value
    FPollingDelay := Value;
    // Restart the timer
    if Connected then SetTimer(FNotifyWnd, 1, FPollingDelay, nil);
    // Adjust the packet timeout
    SetPacketTimeout(FPacketTimeout);
  end;
end;

//------------------------------------------------------------------------------
// APPLY COM PORT SETTINGS - SETUP DCB (DEVICE CONTROL BLOCK) FIELDS
//------------------------------------------------------------------------------
function TSerialPort.ApplyCOMSettings: Boolean;
var
  DCB: TDCB;
begin
  // Default result
  Result := False;
  // Exit when already connected
  if not Connected then Exit;
  // Fill with zero's (clear all)
  FillChar(DCB, sizeof(DCB), 0);
  // DCB structure size
  DCB.DCBLength := sizeof(DCB);
  // Baud rate
  DCB.BaudRate := FBaudRateValue;
  // Set fBinary: Win32 does not support non binary mode transfers (also disable EOF check)
  DCB.Flags := DCB_Binary;
  // Enables the DTR line when the device is opened and leaves it on
  if EnableDTROnOpen then DCB.Flags := DCB.Flags or DCB_DtrControlEnable;
  // Kind of HW Flow Control to use
  case FHwFlow of
    // No HW Flow Control
    hfNONE: ;
    // No HW Flow Control but set RTS High and leave it High
    hfNONERTSON: DCB.Flags := DCB.Flags or DCB_RtsControlEnable;
    // RTS/CTS (request-to-send/clear-to-send) Flow Control
    hfRTSCTS: DCB.Flags := DCB.Flags or DCB_OutxCtsFlow or DCB_RtsControlHandshake;
  end;
  // Kind of SW Flow Control to use
  case FSwFlow of
    // No SW Flow Control
    sfNONE: ;
    // XON/XOFF SW Flow Control
    sfXONXOFF: DCB.Flags := DCB.Flags or DCB_OutX or DCB_InX;
  end;
  // Set XONLim: specifies the minimum number of bytes allowed in the input
  // buffer before the XON character is sent (or CTS is set).
  if FInBufSize div 4 > MIN_XONLIM then DCB.XONLim := MIN_XONLIM else DCB.XONLim := FInBufSize div 4;
  // Specifies the maximum number of bytes allowed in the input buffer before
  // the XOFF character is sent (or CTS is set low). The maximum number of bytes
  // allowed is calculated by subtracting this value from the size in bytes of
  // the input buffer.
  DCB.XOFFLim := DCB.XONLim;
  // How many data bits to use
  DCB.ByteSize := 5 + ORD(FDataBits);
  // Kind of parity to use
  DCB.Parity := ORD(FParity);
  // How many stop bits to use
  DCB.StopBits := ORD(FStopbits);
  // XON ASCII AnsiChar
  DCB.XONChar := XON_CHAR;
  // XOFF ASCII AnsiChar
  dcb.XOFFChar := XOFF_CHAR;
  // Apply new settings
  Result := SetCommState(FHandle, dcb);
  if not Result then Exit;
  // Flush buffers
  Result := FlushBuffers(true, true);
  if not Result then Exit;
  // Setup buffers size
  Result := SetupComm(FHandle, FInBufSize, FOutBufSize);
end;

//------------------------------------------------------------------------------
// COM PORT POLLING PROC
//------------------------------------------------------------------------------
procedure TSerialPort.TimerWndProc(var Msg: TMessage);
var
  BytesRead, BytesToRead, BytesToReadBuf, Dummy: DWORD;
  ComStat: TComStat;
  S: string;
begin
  if (Msg.Msg = WM_TIMER) and Connected and (not FIsInOnTimer) then
  begin
    try
      // Set flag to indicate that we are already in the OnTimer PROC.
      FIsInOnTimer := True;
      // Exit when RX polling has been paused
      if FRXPollingPauses > 0 then Exit;
      // Clear COMM Errors before we proceed
      ClearCommError(FHandle, Dummy, @ComStat);
      // If PacketSize is > 0 then emit the OnReceiveData event only if the RX
      // buffer has at least PacketSize bytes in it.
      if FPacketSize > 0 then
      begin
        // Ensure we received a complete packet
        if DWORD(ComStat.cbInQue) >= DWORD(FPacketSize) then
        begin
          repeat
            BytesRead := 0;
            // Read Bytes
            if ReadFile(FHandle, FTempInBuffer^, FPacketSize, BytesRead, nil) then
            // Emit event
            if (BytesRead <> 0) and Assigned(FOnReceivePacket) then FOnReceivePacket(Self, FTempInBuffer, BytesRead);
            // Adjust time
            FFirstByteOfPacketTime := FFirstByteOfPacketTime + DelayForRX(FPacketSize);
            // Update IN QUE size
            ComStat.cbInQue := ComStat.cbInQue - WORD(FPacketSize);
            // Update first byte of packet
            if ComStat.cbInQue = 0 then FFirstByteOfPacketTime := DWORD(-1);
          until DWORD(comStat.cbInQue) < DWORD(FPacketSize);
          // Done!
          Exit;
        end;

        // Handle packet timeouts
        if (FPacketTimeout > 0) and (FFirstByteOfPacketTime <> DWORD(-1)) and (GetTickCount - FFirstByteOfPacketTime > DWORD(FPacketTimeout)) then
        begin
          BytesRead := 0;
          // Read the "incomplete" packet
          if ReadFile(FHandle, FTempInBuffer^, comStat.cbInQue, BytesRead, nil) then
          // If PacketMode is not pmDiscard then emit the packet
          if (FPacketMode <> pmDiscard) and (BytesRead <> 0) and Assigned(FOnReceivePacket) then FOnReceivePacket(Self, FTempInBuffer, BytesRead);
          // Restart waiting for a packet
          FFirstByteOfPacketTime := DWORD(-1);
          // Done!
          Exit;
        end;

        // Update the start time
        if (comStat.cbInQue > 0) and (FFirstByteOfPacketTime = DWORD(-1)) then FFirstByteOfPacketTime := GetTickCount;
        // Done!
        Exit;
      end;

      // Standard data handling
      BytesRead   := 0;
      BytesToRead := ComStat.cbInQue;
      while (BytesToRead > 0) do
      begin
        // Set buffer size
        BytesToReadBuf := BytesToRead;
        // Ensure the bytes to read are not > than the Input Buffer size
        if (BytesToReadBuf > FInBufSize) then BytesToReadBuf := FInBufSize;
        // Read Bytes
        if ReadFile(FHandle, FTempInBuffer^, BytesToReadBuf, BytesRead, nil) then
        begin
          if (BytesRead <> 0) and Assigned(FOnReceiveData) then
            FOnReceiveData(Self, FTempInBuffer, BytesRead);
        end else break;
        // Update bytes to read
        Dec(BytesToRead, BytesRead);
      end;
    finally
      // Set flag to indicate that we are can use the OnTimer PROC again.
      FIsInOnTimer := False;
    end;
  end
  // Let Windows handle other messages.
  else Msg.Result := DefWindowProc(FNotifyWnd, Msg.Msg, Msg.wParam, Msg.lParam);
end;

//------------------------------------------------------------------------------
// COMPONENT CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TSerialPort.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Initialize Handle with an invalid handle value (Disconnected)
  FHandle := INVALID_HANDLE_VALUE;
  // Set the COMPORT PORT
  FPort     := 'COM1'; (* \\.\COM1 *)
  // Set the COMPORT Baud Rate
  FBaudRate      := br9600;
  FBaudRateValue := BaudRateOf(br9600);
  // Set the COMPORT Data Bits to 8
  FDataBits  := db8BITS;
  // Set the COMPORT Stop Bits to 1
  FStopBits := sb1BITS;
  // Set the COMPORT Parity to none
  FParity := ptNONE;
  // No Hardware Flow Control
  FHwFlow := hfNONE;
  // No Software Flow Control
  FSwFlow := sfNONE;
  // Input and output buffer of 2048 bytes
  FInBufSize  := DEFAULT_BUFFER_SIZE;
  FOutBufSize := DEFAULT_BUFFER_SIZE;
  // No packets by default
  FPacketSize := -1;
  // Packet Timeout disabled by default
  FPacketTimeout := -1;
  // Discard incomplete packets
  FPacketMode := pmDiscard;
  // Set Polling Delay
  FPollingDelay := DEFAULT_POLLING_DELAY;
  // Set Output Time-Out
  FOutputTimeout := DEFAULT_OUTPUT_TIMEOUT;
  // Set Input Time-Out
  FInputTimeout := DEFAULT_INPUT_TIMEOUT;
  // DTR High on connect
  FEnableDTROnOpen := True;
  // Initialize time of first Packet Byte to -1
  FFirstByteOfPacketTime := DWORD(-1);
  // Set Check Line Status
  FCkLineStatus := False;
  // Initialize OnTimer PROC flag
  FIsInOnTimer := False;
  // Initialize number of RX polling timer pauses to zero (not paused)
  FRXPollingPauses := 0;
  // Allocate memory for the Input Buffer
  FTempInBuffer := AllocMem(FInBufSize);
  // Allocate a Window HANDLE to catch Timer's Notification Messages (WM_TIMER)
  FNotifyWnd := AllocateHWnd(TimerWndProc);
end;

//------------------------------------------------------------------------------
// COMPONENT DESTRUCTOR
//------------------------------------------------------------------------------
destructor TSerialPort.Destroy;
begin
  // Disconnect if there is still an open connection
  if Connected then Disconnect;
  // Free the memory for the Input Buffer
  FreeMem(FTempInBuffer, FInBufSize);
  // Release the timer's Window HANDLE
  DeallocateHWnd(FNotifyWnd);
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TSerialPort.Connect: Boolean;
var
  TimeOuts: TCommTimeouts;
  PortName: string;
const
  ShareMode          = DWORD(0);
  SecutiryAttributes = nil;
  TemplateFile       = 0;
  TimerFunc          = nil; // We are using Window Messages instead
begin
  Result := Connected;
  // Exit here when we're already connected
  if Result then Exit;
  // Open the COM port
  PortName := IfThen(Pos('\\.\', FPort) > 0, FPort, Format('\\.\%s', [FPort]));
  FHandle := CreateFile(PWideChar(PortName), GENERIC_READ or GENERIC_WRITE, ShareMode, SecutiryAttributes, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, TemplateFile);
  // Update the result
  Result := Connected;
  // Exit here when the connection failed
  if not Result then Exit;
  // Apply COM PORT settings
  Result := ApplyCOMSettings;
  // When applying settings failed, disconnect and Exit here
  if not Result then
  begin
    Disconnect;
    Exit;
  end;
  // Set ReadIntervalTimeout: Specifies the maximum time in milliseconds allowed
  // to elapse between the arrival of two characters on the communications line.
  // We disable timeouts because we are polling the COM PORT!
  TimeOuts.ReadIntervalTimeout := 1;
  // Set ReadTotalTimeoutMultiplier: Specifies the multiplie, in milliseconds
  // used to calculate the total time-out period for read operations.
  TimeOuts.ReadTotalTimeoutMultiplier := 0;
  // Set ReadTotalTimeoutConstant: Specifies the constant in milliseconds
  // used to calculate the total time-out period for read operations.
  TimeOuts.ReadTotalTimeoutConstant := 1;
  // Set WriteTotalTimeoutMultiplier: Specifies the multiplier in milliseconds
  // used to calculate the total time-out period for write operations.
  TimeOuts.WriteTotalTimeoutMultiplier := 0;
  // Set WriteTotalTimeoutConstant: Specifies the constant in milliseconds
  // used to calculate the total time-out period for write operations.
  TimeOuts.WriteTotalTimeoutConstant := 10;
  // Apply timeouts
  SetCommTimeOuts(FHandle, TimeOuts);
  // Start the timer (used for polling)
  SetTimer(FNotifyWnd, 1, FPollingDelay, TimerFunc);
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
procedure TSerialPort.Disconnect;
begin
  if Connected then
  begin
    // Stop the timer (used for polling)
    KillTimer(FNotifyWnd, 1);
    // Release the COM PORT
    CloseHandle(FHandle);
    // Update handle to indicate we are not connected
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

//------------------------------------------------------------------------------
// GET CONNECTED STATUS
//------------------------------------------------------------------------------
function TSerialPort.Connected: Boolean;
begin
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

//------------------------------------------------------------------------------
// GET CTS, DSR, RING AND RLSD (CD) SIGNALS STATUS
//------------------------------------------------------------------------------
function TSerialPort.GetLineStatus: TLineStatusSet;
var
  DWS: DWORD;
begin
  Result := [];
  // Retrieve modem control-register values.
  // The function fails if the hardware does not support the control-register values.
  if (not Connected) or (not GetCommModemStatus(FHandle, DWS)) then Exit;
  if (DWS and MS_CTS_ON)  <> 0 then Result := Result + [lsClearToSend];
  if (DWS and MS_DSR_ON)  <> 0 then Result := Result + [lsDataSetReady];
  if (DWS and MS_RING_ON) <> 0 then Result := Result + [lsRING];
  if (DWS and MS_RLSD_ON) <> 0 then Result := Result + [lsCarrierDetect];
end;

//------------------------------------------------------------------------------
// GET IS POLLING STATUS
//------------------------------------------------------------------------------
function TSerialPort.IsPolling: Boolean;
begin
  // Returns True if polling has not been paused
  Result := FRXPollingPauses <= 0;
end;

//------------------------------------------------------------------------------
// PAUSE POLLING
//------------------------------------------------------------------------------
procedure TSerialPort.PausePolling;
begin
  Inc(FRXPollingPauses);
end;

//------------------------------------------------------------------------------
// CONTINUE POLLING
//------------------------------------------------------------------------------
procedure TSerialPort.ContinuePolling;
begin
  Dec(FRXPollingPauses);
end;

//------------------------------------------------------------------------------
// FLUSH RX/TX BUFFERS
//------------------------------------------------------------------------------
function TSerialPort.FlushBuffers(inBuf: Boolean; outBuf: Boolean): Boolean;
var
  Flags: DWORD;
begin
  Result := False;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Initialize flags
  Flags := 0;
  // Flush the RX Data Buffer;
  if outBuf then Flags := Flags or PURGE_TXABORT or PURGE_TXCLEAR;
  // Flush the TX Data Buffer
  if inBuf then Flags := Flags or PURGE_RXABORT or PURGE_RXCLEAR;
  // Purge COM PORT
  Result := PurgeComm(FHandle, Flags);
  // Used for the RX Packets
  if Result then FFirstByteOfPacketTime := DWORD(-1);
end;

//------------------------------------------------------------------------------
// GET NUMBER OF RECEIVED BYTES IN RX BUFFER
//------------------------------------------------------------------------------
function TSerialPort.CountRX: Integer;
var
  ComStat: TComStat;
  Errors: DWORD;
begin
  // Initialize to MAX RX BUFFER SIZE
  Result := cMaxRXBufferSize;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Get count
  ClearCommError(FHandle, Errors, @ComStat);
  Result := ComStat.cbInQue;
end;

//------------------------------------------------------------------------------
// GET OUTPUT BUFFER FREE SPACE
//------------------------------------------------------------------------------
function TSerialPort.OutFreeSpace: Word;
var
  ComStat: TComStat;
  Errors: DWORD;
begin
  if not Connected then Result := cMaxRXBufferSize else
  begin
    ClearCommError(FHandle, Errors, @ComStat);
    Result := FOutBufSize - ComStat.cbOutQue;
  end;
end;

//------------------------------------------------------------------------------
// SEND DATA
//------------------------------------------------------------------------------
function TSerialPort.SendData(DataPtr: Pointer; DataSize: Cardinal): Cardinal;
begin
  if Assigned(OnSendData) then OnSendData(Self, DataPtr, DataSize);
  Result := SendDataEx(DataPtr, DataSize, FOutputTimeout);
end;

//------------------------------------------------------------------------------
// SEND DATA (EX)
//------------------------------------------------------------------------------
function TSerialPort.SendDataEx(DataPtr: PAnsiChar; DataSize: Cardinal; Timeout: Cardinal): Cardinal;
var
  BytesToSend, BytesSent, Tick: DWORD;
begin
  Result := 0;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Get current tick count
  Tick := GetTickCount;
  // Loop until all data is sent or a timeout occurred
  while DataSize > 0 do
  begin
    // Get TX buffer free space
    BytesToSend := OutFreeSpace;
    // Continue if we have bytes to send
    if BytesToSend > 0 then
    begin
      // Check signals
      if FCkLineStatus and (GetLineStatus = []) then Exit;
      // Don't send more bytes than we actually have to send
      if BytesToSend > DataSize then BytesToSend := DataSize;
      // Send the bytes
      WriteFile(FHandle, DataPtr^, BytesToSend, BytesSent, nil);
      BytesSent := Abs(BytesSent);
      if BytesSent > 0 then
      begin
        // Update number of bytes sent
        Result := Result + BytesSent;
        // Decrease the number of bytes to send
        DataSize := DataSize - BytesSent;
        // Increment data pointer
        DataPtr := DataPtr + BytesSent;
        // Get current tick count
        Tick := GetTickCount;
        // Continue. This skips the time check below (don't stop transmitting if the Timeout is set too low)
        Continue;
      end;
    end;
    // Exit here when we are waiting too long and the Buffer is full.
    if DWORD(GetTickCount - Tick) > Timeout then Exit;
  end;
end;

//------------------------------------------------------------------------------
// SEND BYTE
//------------------------------------------------------------------------------
function TSerialPort.SendByte(Value: Byte): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI CHARACTER
//------------------------------------------------------------------------------
function TSerialPort.SendChar(Value: AnsiChar): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI string
//------------------------------------------------------------------------------
function TSerialPort.SendString(S: Ansistring): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(PAnsiChar(S), L) = L;
end;

//------------------------------------------------------------------------------
// SEND C-STYLE ANSI string
//------------------------------------------------------------------------------
function TSerialPort.SendCString(S: PAnsiChar): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(S, L) = L;
end;

//------------------------------------------------------------------------------
// READ DATA
//------------------------------------------------------------------------------
function TSerialPort.ReadData(DataPtr: PAnsiChar; MaxDataSize: Cardinal): Cardinal;
var
  BytesToRead, BytesRead, Tick: DWORD;
begin
  Result := 0;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Pause polling
  PausePolling;
  // Current tick count
  Tick := GetTickCount;
  // Loop until all requested data is read or a timeout occurred
  while MaxDataSize > 0 do
  begin
    // Get data bytes count in RX buffer
    BytesToRead := CountRX;
    if BytesToRead > 0 then
    begin
      // Don't read more bytes than we actually have to read
      if BytesToRead > MaxDataSize then BytesToRead := MaxDataSize;
      // Read Bytes
      ReadFile(FHandle, DataPtr^, BytesToRead, BytesRead, nil);
      // Update number of bytes read
      Result := Result + BytesRead;
      // Decrease the count of bytes to read
      MaxDataSize := MaxDataSize - BytesRead;
      // Increase data pointer
      DataPtr := DataPtr + BytesRead;
      // Get current tick count
      Tick := GetTickCount;
      // Continue. This skips the time check below (don't stop reading if the Timeout is set too low)
      Continue;
    end;
    // Exit here when we are waiting too long and the Buffer is empty.
    if (GetTickCount - Tick) > FInputTimeout then Break;
  end;
  // Continue polling
  ContinuePolling;
end;

//------------------------------------------------------------------------------
// READ BYTE
//------------------------------------------------------------------------------
function TSerialPort.ReadByte(var Value: Byte): Boolean;
begin
  Result := ReadData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// READ ANSI CHARACTER
//------------------------------------------------------------------------------
function TSerialPort.ReadChar(var Value: AnsiChar): Boolean;
begin
  Result := ReadData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SET DTR LINE HIGH (onOff = TRUE) OR LOW (onOff = FALSE).
// NOTE: You must not use HW handshaking.
//------------------------------------------------------------------------------
procedure TSerialPort.ToggleDTR(onOff: Boolean);
const
  Funcs: array[Boolean] of Integer = (CLRDTR, SETDTR);
begin
  if Connected then EscapeCommFunction(FHandle, Funcs[onOff]);
end;

//------------------------------------------------------------------------------
// SET RTS LINE HIGH (onOff = TRUE) OR LOW (onOff = FALSE).
// NOTE: You must not use HW handshaking.
//------------------------------------------------------------------------------
procedure TSerialPort.ToggleRTS(onOff: Boolean);
const
  Funcs: array[Boolean] of Integer = (CLRRTS, SETRTS);
begin
  if Connected then EscapeCommFunction(FHandle, Funcs[onOff]);
end;

//------------------------------------------------------------------------------
// GET MAX RX BUFFER SIZE
//------------------------------------------------------------------------------
function TSerialPort.GetMaxRXBufferSize: Cardinal;
begin
  Result := cMaxRXBufferSize;
end;

//------------------------------------------------------------------------------
// GET MAX TX BUFFER SIZE
//------------------------------------------------------------------------------
function TSerialPort.GetMaxTXBufferSize: Cardinal;
begin
  Result := cMaxTXBufferSize;
end;

//------------------------------------------------------------------------------
// GET MIN RX BUFFER SIZE
//------------------------------------------------------------------------------
function TSerialPort.GetMinRXBufferSize: Cardinal;
begin
  Result := cMinRXBufferSize;
end;

//------------------------------------------------------------------------------
// GET MIN TX BUFFER SIZE
//------------------------------------------------------------------------------
function TSerialPort.GetMinTXBufferSize: Cardinal;
begin
  Result := cMinTXBufferSize;
end;

//------------------------------------------------------------------------------
// SET COMM BREAK
//------------------------------------------------------------------------------
function TSerialPort.SetCommBreak: Boolean;
begin
  Result := False;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Set COMM BREAK
  Result := Winapi.Windows.SetCommBreak(FHandle);
end;

//------------------------------------------------------------------------------
// CLEAR COMM BREAK
//------------------------------------------------------------------------------
function TSerialPort.ClearCommBreak: Boolean;
begin
  Result := False;
// Exit here when we're not connected
  if not Connected then Exit;
  // Clear COMM BREAK
  Result := Winapi.Windows.ClearCommBreak(FHandle);
end;

//------------------------------------------------------------------------------
// GET BAUDRATE VALUE FROM BAUDRATE TYPE
//------------------------------------------------------------------------------
function TSerialPort.BaudRateOf(Rate: TBaudRate): Cardinal;
begin
  if Rate = brCustom then Result := 0 else Result := Win32BaudRates[Rate];
end;

//------------------------------------------------------------------------------
// GET RX DELAY
//------------------------------------------------------------------------------
function TSerialPort.DelayForRX(DataSize: Cardinal): Cardinal;
var
  BitsForByte: Single;
begin
  BitsForByte := 10;
  case FStopBits of
    sb1HALFBITS : BitsForByte := BitsForByte + 1.5;
    sb2BITS     : BitsForByte := BitsForByte + 2;
  end;
  if FParity <> TParity.ptNONE then BitsForByte := BitsForByte + 1;
  case DataBits of
    db5BITS: BitsForByte := BitsForByte - 3;
    db6BITS: BitsForByte := BitsForByte - 2;
    db7BITS: BitsForByte := BitsForByte - 1;
  end;
  Result := Round(DataSize / (FBaudRateValue / BitsForByte) * 1000);
end;

//------------------------------------------------------------------------------
// SERIAL OBD CONNECTION CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TSerialOBDConnection.Create;
begin
  // Inherited create
  inherited;

  // Create Serial Port class instance
  FSerialPort := TSerialPort.Create;
  FSerialPort.OnReceiveData := OnReceiveData;
  FSerialPort.OnSendData := OnSendData;
  FSerialPort.OnError := OnConnectionError;
end;

//------------------------------------------------------------------------------
// SERIAL OBD CONNECTION DESTRUCTOR
//------------------------------------------------------------------------------
destructor TSerialOBDConnection.Destroy;
begin
  // Free Serial Port instance
  FSerialPort.Free;

  // Inherited destroy
  inherited;
end;

//------------------------------------------------------------------------------
// GET CONNECTED STATE
//------------------------------------------------------------------------------
function TSerialOBDConnection.Connected: Boolean;
begin
  Result := FSerialPort.Connected;
end;

//------------------------------------------------------------------------------
// SERIAL PORT RECEIVE DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TSerialOBDConnection.OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  if Assigned(OnDataReceived) then OnDataReceived(Self, DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// SERIAL PORT SEND DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TSerialOBDConnection.OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  if Assigned(OnDataSend) then OnDataSend(Self, DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// SERIAL PORT ERROR EVENT HANDLER
//------------------------------------------------------------------------------
procedure TSerialOBDConnection.OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
begin
  if Assigned(OnError) then OnError(Self, ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------
// CONNECT TO OBD INTERFACE
//------------------------------------------------------------------------------
function TSerialOBDConnection.Connect(const Params: TOBDConnectionParams): Boolean;
begin
  Result := FSerialPort.Connected;
  // Exit here if we're already connected
  if Result then Exit;
  // Exit here if the connection type is incorrect
  if Params.ConnectionType <> ctSerial then Exit;
  // Set the port
  FSerialPort.Port := String(Params.COMPort);
  // Set the baudrate
  FSerialPort.BaudRate := Params.BaudRate;
  // Connect to the serial port
  Result := FSerialPort.Connect;
end;

//------------------------------------------------------------------------------
// DISCONNECT THE CONNECTED OBD INTERFACE
//------------------------------------------------------------------------------
function TSerialOBDConnection.Disconnect: Boolean;
begin
  Result := False;
  if Connected then
  begin
    FSerialPort.Disconnect;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND
//------------------------------------------------------------------------------
function TSerialOBDConnection.WriteATCommand(const ATCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('AT', ATCommand) = 1, ATCommand, Format(IfThen(Pos(' ', ATCommand) > 0, 'AT %s', 'AT%s'), [ATCommand]));
  Result := FSerialPort.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE ST COMMAND
//------------------------------------------------------------------------------
function TSerialOBDConnection.WriteSTCommand(const STCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('ST', STCommand) = 1, STCommand, Format(IfThen(Pos(' ', STCommand) > 0, 'ST %s', 'ST%s'), [STCommand]));
  Result := FSerialPort.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE OBD COMMAND
//------------------------------------------------------------------------------
function TSerialOBDConnection.WriteOBDCommand(const OBDCommand: string): Boolean;
begin
  Result := FSerialPort.Sendstring(AnsiString(OBDCommand));
end;

end.
