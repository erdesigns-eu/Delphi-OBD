//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.UDP.pas
// CONTENTS       : UDP/Ethernet Connection for Diagnostics over IP (DoIP)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2024
//------------------------------------------------------------------------------
unit OBD.Connection.UDP;

interface

uses
  System.SysUtils, System.Classes, WinApi.Windows, WinApi.WinSock2,
  OBD.Connection, OBD.Connection.Types;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   UDP Connection for Diagnostics over IP (DoIP) - ISO 13400
  ///   Used for BMW ENET cables and other automotive Ethernet diagnostics
  /// </summary>
  TUDPConnection = class(TInterfacedObject, IOBDConnection)
  private
    FSocket: TSocket;
    FConnected: Boolean;
    FHost: string;
    FPort: Word;
    FLocalPort: Word;
    FReceiveTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveBuffer: array[0..8192] of Byte;
    FOnDataReceived: TDataReceivedEvent;
    FOnDataSend: TDataSendEvent;
    FOnError: TErrorEvent;
    
    procedure InitWinSock;
    procedure CleanupWinSock;
    function GetRemoteAddress: TSockAddr;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    /// <param name="AHost">
    ///   Target host IP address (e.g., '192.168.0.10')
    /// </param>
    /// <param name="APort">
    ///   Target port (default 13400 for DoIP)
    /// </param>
    constructor Create(const AHost: string; APort: Word = 13400); 
    
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;
    
    /// <summary>
    ///   Connect to the remote DoIP endpoint
    /// </summary>
    function Connect: Boolean;
    
    /// <summary>
    ///   Disconnect from the remote DoIP endpoint
    /// </summary>
    function Disconnect: Boolean;
    
    /// <summary>
    ///   Check if connected
    /// </summary>
    function Connected: Boolean;
    
    /// <summary>
    ///   Send data over UDP
    /// </summary>
    function SendData(const Data: TBytes): Boolean;
    
    /// <summary>
    ///   Receive data from UDP socket with timeout
    /// </summary>
    function ReceiveData(var Buffer: TBytes; Timeout: Integer = 1000): Integer;
    
    /// <summary>
    ///   Target host IP address
    /// </summary>
    property Host: string read FHost write FHost;
    
    /// <summary>
    ///   Target port (DoIP default: 13400)
    /// </summary>
    property Port: Word read FPort write FPort;
    
    /// <summary>
    ///   Local UDP port (0 = auto-assign)
    /// </summary>
    property LocalPort: Word read FLocalPort write FLocalPort;
    
    /// <summary>
    ///   Receive timeout in milliseconds
    /// </summary>
    property ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout;
    
    /// <summary>
    ///   Send timeout in milliseconds
    /// </summary>
    property SendTimeout: Integer read FSendTimeout write FSendTimeout;
    
    /// <summary>
    ///   Data received event
    /// </summary>
    property OnDataReceived: TDataReceivedEvent read FOnDataReceived write FOnDataReceived;
    
    /// <summary>
    ///   Data send event
    /// </summary>
    property OnDataSend: TDataSendEvent read FOnDataSend write FOnDataSend;
    
    /// <summary>
    ///   Error event
    /// </summary>
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

implementation

//------------------------------------------------------------------------------
// INIT WINSOCK
//------------------------------------------------------------------------------
procedure TUDPConnection.InitWinSock;
var
  WSAData: TWSAData;
begin
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
    raise Exception.Create('Failed to initialize WinSock');
end;

//------------------------------------------------------------------------------
// CLEANUP WINSOCK
//------------------------------------------------------------------------------
procedure TUDPConnection.CleanupWinSock;
begin
  WSACleanup;
end;

//------------------------------------------------------------------------------
// GET REMOTE ADDRESS
//------------------------------------------------------------------------------
function TUDPConnection.GetRemoteAddress: TSockAddr;
var
  HostEnt: PHostEnt;
  Addr: u_long;
begin
  // Try to resolve as IP address first
  Addr := inet_addr(PAnsiChar(AnsiString(FHost)));
  
  if Addr = INADDR_NONE then
  begin
    // Try to resolve as hostname
    HostEnt := gethostbyname(PAnsiChar(AnsiString(FHost)));
    if HostEnt = nil then
      raise Exception.CreateFmt('Cannot resolve host: %s', [FHost]);
    Addr := PInAddr(HostEnt^.h_addr_list^)^.S_addr;
  end;
  
  FillChar(Result, SizeOf(Result), 0);
  Result.sin_family := AF_INET;
  Result.sin_addr.S_addr := Addr;
  Result.sin_port := htons(FPort);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TUDPConnection.Create(const AHost: string; APort: Word = 13400);
begin
  inherited Create;
  FHost := AHost;
  FPort := APort;
  FLocalPort := 0; // Auto-assign
  FReceiveTimeout := 5000;  // 5 seconds default
  FSendTimeout := 5000;     // 5 seconds default
  FConnected := False;
  FSocket := INVALID_SOCKET;
  
  InitWinSock;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TUDPConnection.Destroy;
begin
  Disconnect;
  CleanupWinSock;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// CONNECT
//------------------------------------------------------------------------------
function TUDPConnection.Connect: Boolean;
var
  LocalAddr: TSockAddr;
  TimeoutVal: Integer;
begin
  Result := False;
  
  try
    // Create UDP socket
    FSocket := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if FSocket = INVALID_SOCKET then
    begin
      if Assigned(FOnError) then
        FOnError(Self, WSAGetLastError, 'Failed to create UDP socket');
      Exit;
    end;
    
    // Bind to local port if specified
    if FLocalPort > 0 then
    begin
      FillChar(LocalAddr, SizeOf(LocalAddr), 0);
      LocalAddr.sin_family := AF_INET;
      LocalAddr.sin_addr.S_addr := INADDR_ANY;
      LocalAddr.sin_port := htons(FLocalPort);
      
      if bind(FSocket, @LocalAddr, SizeOf(LocalAddr)) = SOCKET_ERROR then
      begin
        if Assigned(FOnError) then
          FOnError(Self, WSAGetLastError, 'Failed to bind to local port');
        closesocket(FSocket);
        FSocket := INVALID_SOCKET;
        Exit;
      end;
    end;
    
    // Set receive timeout
    TimeoutVal := FReceiveTimeout;
    setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @TimeoutVal, SizeOf(TimeoutVal));
    
    // Set send timeout
    TimeoutVal := FSendTimeout;
    setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @TimeoutVal, SizeOf(TimeoutVal));
    
    FConnected := True;
    Result := True;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, 0, E.Message);
    end;
  end;
end;

//------------------------------------------------------------------------------
// DISCONNECT
//------------------------------------------------------------------------------
function TUDPConnection.Disconnect: Boolean;
begin
  Result := False;
  
  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    Result := True;
  end;
  
  FConnected := False;
end;

//------------------------------------------------------------------------------
// CONNECTED
//------------------------------------------------------------------------------
function TUDPConnection.Connected: Boolean;
begin
  Result := FConnected and (FSocket <> INVALID_SOCKET);
end;

//------------------------------------------------------------------------------
// SEND DATA
//------------------------------------------------------------------------------
function TUDPConnection.SendData(const Data: TBytes): Boolean;
var
  RemoteAddr: TSockAddr;
  BytesSent: Integer;
begin
  Result := False;
  
  if not Connected then Exit;
  if Length(Data) = 0 then Exit;
  
  try
    RemoteAddr := GetRemoteAddress;
    BytesSent := sendto(FSocket, Data[0], Length(Data), 0, @RemoteAddr, SizeOf(RemoteAddr));
    
    if BytesSent = SOCKET_ERROR then
    begin
      if Assigned(FOnError) then
        FOnError(Self, WSAGetLastError, 'Failed to send data');
      Exit;
    end;
    
    Result := BytesSent = Length(Data);
    
    if Result and Assigned(FOnDataSend) then
      FOnDataSend(Self, @Data[0], BytesSent);
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, 0, E.Message);
    end;
  end;
end;

//------------------------------------------------------------------------------
// RECEIVE DATA
//------------------------------------------------------------------------------
function TUDPConnection.ReceiveData(var Buffer: TBytes; Timeout: Integer = 1000): Integer;
var
  BytesReceived: Integer;
  FromAddr: TSockAddr;
  FromLen: Integer;
  OldTimeout: Integer;
begin
  Result := 0;
  
  if not Connected then Exit;
  
  try
    // Set temporary timeout if different from default
    if Timeout <> FReceiveTimeout then
    begin
      OldTimeout := FReceiveTimeout;
      setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
    end;
    
    FromLen := SizeOf(FromAddr);
    BytesReceived := recvfrom(FSocket, FReceiveBuffer[0], SizeOf(FReceiveBuffer), 0, @FromAddr, @FromLen);
    
    // Restore original timeout if it was changed
    if Timeout <> FReceiveTimeout then
      setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @OldTimeout, SizeOf(OldTimeout));
    
    if BytesReceived = SOCKET_ERROR then
    begin
      if WSAGetLastError <> WSAETIMEDOUT then
      begin
        if Assigned(FOnError) then
          FOnError(Self, WSAGetLastError, 'Failed to receive data');
      end;
      Exit;
    end;
    
    if BytesReceived > 0 then
    begin
      SetLength(Buffer, BytesReceived);
      Move(FReceiveBuffer[0], Buffer[0], BytesReceived);
      Result := BytesReceived;
      
      if Assigned(FOnDataReceived) then
        FOnDataReceived(Self, @Buffer[0], BytesReceived);
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, 0, E.Message);
    end;
  end;
end;

end.
