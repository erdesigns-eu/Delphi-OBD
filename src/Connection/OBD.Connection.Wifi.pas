//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.Wifi.pas
// CONTENTS       : WIFI OBD Connection Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/02/2024
//------------------------------------------------------------------------------
unit OBD.Connection.Wifi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Win.ScktComp,
  Vcl.Forms,

  OBD.Connection,
  OBD.Connection.Types,
  OBD.Connection.Constants;

type
  /// <summary>
  ///   WIFI
  /// </summary>
  TWifi = class
  private
    /// <summary>
    ///    TCP Socket
    /// </summary>
    FTCPSocket: TClientSocket;
    /// <summary>
    ///    Connected flag
    /// </summary>
    FConnected: Boolean;
    /// <summary>
    ///    Connection timeout
    /// </summary>
    FConnectionTimeout: Integer;
    /// <summary>
    ///   Event to emit on data reception
    /// </summary>
    FOnReceiveData: TDataReceivedEvent;
    /// <summary>
    ///   Event to emit on data send
    /// </summary>
    FOnSendData: TDataSendEvent;
    /// <summary>
    ///   Event to emit when an error occurs
    /// </summary>
    FOnError: TErrorEvent;
  protected
    /// <summary>
    ///    Socket connect event handler
    /// </summary>
    procedure OnSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
    /// <summary>
    ///    Socket disconnect event handler
    /// </summary>
    procedure OnSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);

    /// <summary>
    ///    Socket read event handler
    /// </summary>
    procedure OnSocketRead(Sender: TObject; Socket: TCustomWinSocket);
    /// <summary>
    ///    Socket error event handler
    /// </summary>
    procedure OnSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: System.Win.ScktComp.TErrorEvent; var ErrorCode: Integer);

    /// <summary>
    ///    Returns true if TCP socket has been opened
    /// </summary>
    function Connected: Boolean;
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
    ///   Opens the TCP socket. Returns false if something goes wrong
    /// </summary>
    function Connect(Host: string; Port: Integer): Boolean;
    /// <summary>
    ///   Closes the TCP socket and releases control of it
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
    function SendString(const S: Ansistring): Boolean;
    /// <summary>
    //    Sends a C-style Ansi string (NULL terminated)
    /// </summary>
    /// <param name="s">
    ///   string to send
    /// </param>
    function SendCString(S: PAnsiChar): Boolean;

    /// <summary>
    ///    Connection timeout
    /// </summary>
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
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
  ///   WIFI OBD Connection
  /// </summary>
  TWifiOBDConnection = class(TOBDConnection)
  private
    /// <summary>
    ///    Wifi (TCP Socket) class instance
    /// </summary>
    FWifi: TWifi;
  protected
    /// <summary>
    ///    Returns true if the adapter is connected
    /// </summary>
    function Connected: Boolean; override;
    /// <summary>
    ///    Wifi (TCP Socket) Receive Data event handler
    /// </summary>
    procedure OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    Wifi (TCP Socket) Send Data event handler
    /// </summary>
    procedure OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: DWORD);
    /// <summary>
    ///    Wifi (TCP Socket) Error event handler
    /// </summary>
    procedure OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
  public
    /// <summary>
    ///   Constructor: Allocate resources needed for the OBD Interface.
    /// <summary>
    constructor Create; override;
    /// <summary>
    ///   Destructor: Free internal allocated resources
    /// <summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connect to a OBD Interface (ELM327, OBDLink, ..)
    /// </summary>
    /// <param name="Params">
    ///   TOBDConnectionParams Record with parameters for connecting
    ///   to the OBD Interface over Serial (COM) Port, Bluetooth, WiFi and FTDI.
    /// </param>
    function Connect(const Params: TOBDConnectionParams): Boolean; override;
    /// <summary>
    ///   Disconnect the connected OBD Interface
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
// SOCKET CONNECTED EVENT HANDLER
//------------------------------------------------------------------------------
procedure TWifi.OnSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnected := True;
end;

//------------------------------------------------------------------------------
// SOCKET DISCONNECTED EVENT HANDLER
//------------------------------------------------------------------------------
procedure TWifi.OnSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnected := False;
end;

//------------------------------------------------------------------------------
// SOCKET READ EVENT HANDLER
//------------------------------------------------------------------------------
procedure TWifi.OnSocketRead(Sender: TObject; Socket: TCustomWinSocket);
var
  Buffer: array[0..1023] of Byte;
  BytesRead: Integer;
begin
  BytesRead := Socket.ReceiveBuf(Buffer, SizeOf(Buffer));
  if BytesRead > 0 then
  begin
    if Assigned(FOnReceiveData) then FOnReceiveData(Self, @Buffer, BytesRead);
  end;
end;

//------------------------------------------------------------------------------
// SOCKET ERROR EVENT HANDLER
//------------------------------------------------------------------------------
procedure TWifi.OnSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: System.Win.ScktComp.TErrorEvent; var ErrorCode: Integer);
var
  ErrorMessage: string;
begin
  case ErrorEvent of
    eeGeneral    : ErrorMessage := 'General';
    eeSend       : ErrorMessage := 'Send';
    eeReceive    : ErrorMessage := 'Receive';
    eeConnect    : ErrorMessage := 'Connect';
    eeDisconnect : ErrorMessage := 'Disconnect';
    eeAccept     : ErrorMessage := 'Accept';
    eeLookup     : ErrorMessage := 'Lookup';
  end;
  if Assigned(OnError) then OnError(Self, ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------
// GET CONNECTED STATUS
//------------------------------------------------------------------------------
function TWifi.Connected: Boolean;
begin
  Result := FTCPSocket.Active and FConnected;
end;

//------------------------------------------------------------------------------
// COMPONENT CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TWifi.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create TCP socket
  FTCPSocket := TClientSocket.Create(nil);
  FTCPSocket.OnConnect := OnSocketConnect;
  FTCPSocket.OnDisconnect := OnSocketDisconnect;
  FTCPSocket.OnRead := OnSocketRead;
  FTCPSocket.OnError := OnSocketError;
  // Set default connection timeout
  FConnectionTimeout := 5000;
end;

//------------------------------------------------------------------------------
// COMPONENT DESTRUCTOR
//------------------------------------------------------------------------------
destructor TWifi.Destroy;
begin
  // Disconnect if there is still an open connection
  if Connected then Disconnect;
  // Free TCP socket
  FTCPSocket.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TWifi.Connect(Host: string; Port: Integer): Boolean;
var
  StartTime, Elapsed: Cardinal;
  DidTimeOut: Boolean;
begin
  // initialize result
  Result := False;
  // Exit here when we're already connected
  if Connected then Exit;
  // Set host and port
  FTCPSocket.Host := Host;
  FTCPSocket.Port := Port;
  // Set to non-blocking (ASYNC) communication
  FTCPSocket.ClientType := ctNonBlocking;

  // Try to open the socket
  try
    FTCPSocket.Active := True;
  except
    // If we encounter an error, like when the ip-address is not correct
    // the port is not correct, or any other kind - exit here.
    Exit;
  end;

  // Get the start time for timeout tracking
  StartTime := GetTickCount;
  // Reset did timeout flag
  DidTimeOut := False;

  // Wait for the connection with a loop
  repeat
    // Calculate elapsed time to handle timeout
    Elapsed := GetTickCount - StartTime;
    // Exit the loop if the response has been received
    if FTCPSocket.Active then Break;
    // Handle timeout (exit loop if exceeded)
    if Integer(Elapsed) >= ConnectionTimeout then
    begin
      DidTimeOut := True;
      Break;
    end;
    // Sleep a bit to reduce CPU usage
    Sleep(100);
    // Optionally, process messages to keep the UI responsive
    Application.ProcessMessages;
  until False;

  // If the loop didnt timeout, then we have successfully connected
  if not DidTimeOut then Result := True;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
procedure TWifi.Disconnect;
begin
  if Connected then
  begin
    FTCPSocket.Active := False;
  end;
end;

//------------------------------------------------------------------------------
// SEND DATA
//------------------------------------------------------------------------------
function TWifi.SendData(DataPtr: Pointer; DataSize: Cardinal): Cardinal;
var
  DataString: AnsiString;
begin
  Result := 0;
  // Exit here when we're not connected
  if not Connected then Exit;
  // Convert pointer data to AnsiString
  SetString(DataString, PAnsiChar(DataPtr), DataSize);
  // Optionally, handle OnSendData event if needed
  if Assigned(OnSendData) then OnSendData(Self, DataPtr, DataSize);
  // Send data as string
  Result := FTCPSocket.Socket.SendText(DataString);
end;

//------------------------------------------------------------------------------
// SEND BYTE
//------------------------------------------------------------------------------
function TWifi.SendByte(Value: Byte): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI CHARACTER
//------------------------------------------------------------------------------
function TWifi.SendChar(Value: AnsiChar): Boolean;
begin
  Result := SendData(@Value, 1) = 1;
end;

//------------------------------------------------------------------------------
// SEND ANSI string
//------------------------------------------------------------------------------
function TWifi.SendString(S: Ansistring): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(PAnsiChar(S), L) = L;
end;

//------------------------------------------------------------------------------
// SEND C-STYLE ANSI string
//------------------------------------------------------------------------------
function TWifi.SendCString(S: PAnsiChar): Boolean;
var
  L: DWORD;
begin
  L := Length(S);
  Result := SendData(S, L) = L;
end;

//------------------------------------------------------------------------------
// GET CONNECTED STATE
//------------------------------------------------------------------------------
function TWifiOBDConnection.Connected: Boolean;
begin
  Result := FWifi.Connected;
end;

//------------------------------------------------------------------------------
// WIFI (TCP SOCKET) RECEIVE DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TWifiOBDConnection.OnReceiveData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  InvokeDataReceived(DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// WIFI (TCP SOCKET) SEND DATA EVENT HANDLER
//------------------------------------------------------------------------------
procedure TWifiOBDConnection.OnSendData(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal);
begin
  InvokeDataSend(DataPtr, DataSize);
end;

//------------------------------------------------------------------------------
// WIFI (TCP SOCKET) ERROR EVENT HANDLER
//------------------------------------------------------------------------------
procedure TWifiOBDConnection.OnConnectionError(Sender: TObject; ErrorCode: Integer; ErrorMessage: string);
begin
  InvokeError(ErrorCode, ErrorMessage);
end;

//------------------------------------------------------------------------------
// WIFI OBD CONNECTION CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TWifiOBDConnection.Create;
begin
  // Inherited constructor
  inherited;
  // Create WIFI Class instance
  FWifi := TWifi.Create;
  FWifi.OnReceiveData := OnReceiveData;
  FWifi.OnSendData := OnSendData;
  FWifi.OnError := OnConnectionError;
end;

//------------------------------------------------------------------------------
// WIFI OBD CONNECTION DESTRUCTOR
//------------------------------------------------------------------------------
destructor TWifiOBDConnection.Destroy;
begin
  // Free the Wifi Class instance
  FWifi.Free;
  // Inherited destructor
  inherited;
end;

//------------------------------------------------------------------------------
// CONNECT TO OBD ADAPTER
//------------------------------------------------------------------------------
function TWifiOBDConnection.Connect(const Params: TOBDConnectionParams): Boolean;
begin
  TMonitor.Enter(FConnectionLock);
  try
    Result := Connected;
    // Exit here is we're already connected
    if Result then Exit;
    // Exit here if the connection type is incorrect
    if Params.ConnectionType <> ctWiFi then Exit;
    // Connect to the Wifi (TCP) Socket
    Result := FWifi.Connect(string(Params.IPAddress), Params.Port);
  finally
    TMonitor.Exit(FConnectionLock);
  end;
end;

//------------------------------------------------------------------------------
// DISCONNECT THE CONNECTED OBD INTERFACE
//------------------------------------------------------------------------------
function TWifiOBDConnection.Disconnect: Boolean;
begin
  TMonitor.Enter(FConnectionLock);
  try
    Result := Connected;
    if Result then
    begin
      FWifi.Disconnect;
      Result := Connected;
    end;
  finally
    TMonitor.Exit(FConnectionLock);
  end;
end;

//------------------------------------------------------------------------------
// WRITE AT COMMAND
//------------------------------------------------------------------------------
function TWifiOBDConnection.WriteATCommand(const ATCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('AT', ATCommand) = 1, ATCommand, Format(IfThen(Pos(' ', ATCommand) > 0, 'AT %s', 'AT%s'), [ATCommand]));
  Result := FWifi.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE ST COMMAND
//------------------------------------------------------------------------------
function TWifiOBDConnection.WriteSTCommand(const STCommand: string): Boolean;
var
  S: string;
begin
  S := IfThen(Pos('ST', STCommand) = 1, STCommand, Format(IfThen(Pos(' ', STCommand) > 0, 'ST %s', 'ST%s'), [STCommand]));
  Result := FWifi.Sendstring(AnsiString(S));
end;

//------------------------------------------------------------------------------
// WRITE OBD COMMAND
//------------------------------------------------------------------------------
function TWifiOBDConnection.WriteOBDCommand(const OBDCommand: string): Boolean;
begin
  Result := FWifi.Sendstring(AnsiString(OBDCommand));
end;

end.
