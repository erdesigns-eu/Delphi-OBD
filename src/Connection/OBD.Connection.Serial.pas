//------------------------------------------------------------------------------
//  OBD.Connection.Serial
//
//  Win32 serial-port transport. Opens a COM port via CreateFile, drives
//  configuration through SetCommState / SetCommTimeouts, runs a worker
//  thread that reads in non-overlapped 256-byte chunks and pushes them
//  to the receive callback.
//
//  Cross-platform note: this unit is Windows-only. POSIX transport
//  (termios via /dev/tty*) is a follow-up; the IOBDConnectionTransport
//  contract is platform-neutral so a POSIX implementation slots in
//  later without changing the public API.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - MS Communication Resources docs
//      (https://learn.microsoft.com/windows/win32/devio/communications-resources)
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial Win32 implementation.
//
//  Future work :
//    - POSIX backend (Linux / macOS) using termios.
//    - Dynamic enumeration of available ports for design-time
//      property editor (Phase 11).
//------------------------------------------------------------------------------

unit OBD.Connection.Serial;

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'OBD.Connection.Serial currently supports Windows only.'}
{$ENDIF}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings;

type
  /// <summary>
  ///   Worker thread that loops on <c>ReadFile</c> and pushes bytes
  ///   into the parent transport's receive callback.
  /// </summary>
  /// <remarks>
  ///   Lifetime is owned by the parent transport. Terminated and
  ///   joined inside <see cref="TOBDSerialTransport.Close"/>.
  ///   <c>FreeOnTerminate</c> is forced False so the parent can wait
  ///   on the join cleanly.
  /// </remarks>
  TOBDSerialReadThread = class(TThread)
  strict private
    FHandle: THandle;
    FOnBytes: TProc<TBytes>;
    FOnError: TProc<TOBDErrorCode, string>;
  protected
    /// <summary>Read loop. Exits on <c>Terminated</c> or unrecoverable
    /// error.</summary>
    procedure Execute; override;
  public
    /// <summary>Spawns the thread (running) bound to a serial handle.</summary>
    /// <param name="AHandle">Win32 file handle of the open COM port.
    /// Owned by the caller; not closed by this thread.</param>
    /// <param name="AOnBytes">Callback for each chunk of bytes read.
    /// Required.</param>
    /// <param name="AOnError">Callback for unrecoverable I/O errors.
    /// Optional.</param>
    constructor Create(AHandle: THandle;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>
  ///   Win32 serial-port transport.
  /// </summary>
  TOBDSerialTransport = class(TInterfacedObject, IOBDConnectionTransport)
  strict private
    FLock: TCriticalSection;
    FHandle: THandle;
    FState: TOBDConnectionState;
    FReader: TOBDSerialReadThread;
    FOnDataReceived: TOBDBytesEvent;
    FOnStateChanged: TOBDStateEvent;
    FOnTransportError: TOBDTransportErrorEvent;
    procedure SetState(ANewState: TOBDConnectionState);
    procedure HandleBytes(const ABytes: TBytes);
    procedure HandleError(ACode: TOBDErrorCode; const AMessage: string);
    procedure ApplySettings(const ASettings: TOBDSerialSettings);
  public
    /// <summary>Constructs an idle serial transport.</summary>
    constructor Create;
    /// <summary>Closes the port (if open) and releases the lock.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Opens the configured COM port and starts the read thread.
    /// </summary>
    /// <param name="ASettings">Port name, baud rate, framing and
    /// timeouts. Must not be <c>nil</c>; <c>Port</c> must be non-empty.</param>
    /// <remarks>
    ///   Synchronous — returns once the port is open and the read
    ///   thread is running. Use <see cref="TOBDConnection.RetryPolicy"/>
    ///   for transient-failure retry.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c> or
    /// <c>Port</c> is empty.</exception>
    /// <exception cref="EOBDError"><c>CreateFile</c> /
    /// <c>SetCommState</c> / <c>SetCommTimeouts</c> failed; the
    /// underlying Win32 error is included in the message.</exception>
    procedure Open(const ASettings: TOBDSerialSettings);

    /// <summary>Closes the port and joins the read thread.</summary>
    procedure Close;
    /// <summary>True when the port is open and ready for I/O.</summary>
    /// <returns><c>True</c> if state is <c>csOpen</c>.</returns>
    function IsOpen: Boolean;
    /// <summary>Current lifecycle state.</summary>
    /// <returns>State enum.</returns>
    function State: TOBDConnectionState;
    /// <summary>Writes bytes through <c>WriteFile</c>.</summary>
    /// <param name="ABytes">Bytes to send. Empty returns 0.</param>
    /// <returns>Number of bytes accepted by the OS.</returns>
    /// <exception cref="EOBDNotConnected">Port is not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer;

    /// <summary>Internal accessor.</summary>
    function GetOnDataReceived: TOBDBytesEvent;
    /// <summary>Internal mutator.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnDataReceived(const AValue: TOBDBytesEvent);
    /// <summary>Internal accessor.</summary>
    function GetOnStateChanged: TOBDStateEvent;
    /// <summary>Internal mutator.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnStateChanged(const AValue: TOBDStateEvent);
    /// <summary>Internal accessor.</summary>
    function GetOnTransportError: TOBDTransportErrorEvent;
    /// <summary>Internal mutator.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnTransportError(const AValue: TOBDTransportErrorEvent);
  end;

implementation

{ ---- helpers ----------------------------------------------------------------- }

function StopBitsToWin(AValue: TOBDStopBits): Byte;
begin
  case AValue of
    sb1:   Result := ONESTOPBIT;
    sb1_5: Result := ONE5STOPBITS;
    sb2:   Result := TWOSTOPBITS;
  else
    Result := ONESTOPBIT;
  end;
end;

function ParityToWin(AValue: TOBDParity): Byte;
begin
  case AValue of
    paNone:  Result := NOPARITY;
    paOdd:   Result := ODDPARITY;
    paEven:  Result := EVENPARITY;
    paMark:  Result := MARKPARITY;
    paSpace: Result := SPACEPARITY;
  else
    Result := NOPARITY;
  end;
end;

function PortPath(const APort: string): string;
begin
  // CreateFile requires the \\.\COMx form for COM10 and above.
  if (Length(APort) >= 4) and (UpperCase(Copy(APort, 1, 3)) = 'COM') and
     (Length(APort) > 4) then
    Result := '\\.\' + APort
  else
    Result := APort;
end;

{ ---- TOBDSerialReadThread ---------------------------------------------------- }

constructor TOBDSerialReadThread.Create(AHandle: THandle;
  const AOnBytes: TProc<TBytes>;
  const AOnError: TProc<TOBDErrorCode, string>);
begin
  inherited Create(False); // start running
  FreeOnTerminate := False;
  FHandle := AHandle;
  FOnBytes := AOnBytes;
  FOnError := AOnError;
end;

procedure TOBDSerialReadThread.Execute;
const
  ChunkSize = 256;
var
  Buf: array[0..ChunkSize - 1] of Byte;
  Read: DWORD;
  Slice: TBytes;
  ErrCode: DWORD;
begin
  while not Terminated do
  begin
    Read := 0;
    if not ReadFile(FHandle, Buf, ChunkSize, Read, nil) then
    begin
      ErrCode := GetLastError;
      // ERROR_OPERATION_ABORTED happens when the handle is closed; treat as EOL.
      if (ErrCode = ERROR_OPERATION_ABORTED) or
         (ErrCode = ERROR_INVALID_HANDLE) then
        Break;
      if Assigned(FOnError) then
        FOnError(oeIO, SysErrorMessage(ErrCode));
      Break;
    end;
    if Read > 0 then
    begin
      SetLength(Slice, Read);
      Move(Buf, Slice[0], Read);
      if Assigned(FOnBytes) then
        FOnBytes(Slice);
    end;
    // ReadFile with COMMTIMEOUTS will return 0 bytes when the read
    // interval expires; loop again and check Terminated.
  end;
end;

{ ---- TOBDSerialTransport ----------------------------------------------------- }

constructor TOBDSerialTransport.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FHandle := INVALID_HANDLE_VALUE;
  FState := csClosed;
end;

destructor TOBDSerialTransport.Destroy;
begin
  Close;
  FLock.Free;
  inherited;
end;

procedure TOBDSerialTransport.SetState(ANewState: TOBDConnectionState);
var
  Handler: TOBDStateEvent;
begin
  FLock.Enter;
  try
    if FState = ANewState then Exit;
    FState := ANewState;
    Handler := FOnStateChanged;
  finally
    FLock.Leave;
  end;
  if Assigned(Handler) then
    Handler(Self, ANewState);
end;

procedure TOBDSerialTransport.HandleBytes(const ABytes: TBytes);
var
  Handler: TOBDBytesEvent;
begin
  Handler := FOnDataReceived;
  if Assigned(Handler) then
    Handler(Self, ABytes);
end;

procedure TOBDSerialTransport.HandleError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Handler: TOBDTransportErrorEvent;
begin
  Handler := FOnTransportError;
  if Assigned(Handler) then
    Handler(Self, ACode, AMessage);
  SetState(csError);
end;

procedure TOBDSerialTransport.ApplySettings(
  const ASettings: TOBDSerialSettings);
var
  DCB: TDCB;
  Timeouts: TCommTimeouts;
begin
  FillChar(DCB, SizeOf(DCB), 0);
  DCB.DCBlength := SizeOf(DCB);
  if not GetCommState(FHandle, DCB) then
    RaiseLastOSError;

  DCB.BaudRate := OBDBaudRateValue(ASettings.BaudRate);
  DCB.ByteSize := ASettings.DataBits;
  DCB.Parity   := ParityToWin(ASettings.Parity);
  DCB.StopBits := StopBitsToWin(ASettings.StopBits);

  // Reset all flag bits we manage so each Open is deterministic.
  DCB.Flags := 0;
  DCB.Flags := DCB.Flags or $00000001;          // fBinary
  if ASettings.FlowControl = fcHardware then
    DCB.Flags := DCB.Flags or $00000004        // fOutxCtsFlow
                            or (1 shl 12);     // fRtsControl = HANDSHAKE
  if ASettings.FlowControl = fcSoftware then
    DCB.Flags := DCB.Flags or $00000100        // fOutX
                            or $00000200;      // fInX
  DCB.XonChar  := AnsiChar(17);
  DCB.XoffChar := AnsiChar(19);

  if not SetCommState(FHandle, DCB) then
    RaiseLastOSError;

  Timeouts.ReadIntervalTimeout         := MAXDWORD;
  Timeouts.ReadTotalTimeoutMultiplier  := 0;
  Timeouts.ReadTotalTimeoutConstant    := ASettings.ReadTimeout;
  Timeouts.WriteTotalTimeoutMultiplier := 0;
  Timeouts.WriteTotalTimeoutConstant   := ASettings.WriteTimeout;
  if not SetCommTimeouts(FHandle, Timeouts) then
    RaiseLastOSError;

  SetupComm(FHandle, 4096, 4096);
  PurgeComm(FHandle, PURGE_RXCLEAR or PURGE_TXCLEAR);
end;

procedure TOBDSerialTransport.Open(const ASettings: TOBDSerialSettings);
var
  Path: string;
begin
  if ASettings = nil then
    raise EOBDConfig.Create('Serial settings are nil');
  if Trim(ASettings.Port) = '' then
    raise EOBDConfig.Create('Serial port name is empty');

  SetState(csOpening);
  Path := PortPath(ASettings.Port);
  FHandle := CreateFile(PChar(Path), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, 0, 0);
  if FHandle = INVALID_HANDLE_VALUE then
  begin
    SetState(csError);
    raise EOBDError.CreateFmt('Cannot open %s: %s',
      [Path, SysErrorMessage(GetLastError)]);
  end;

  try
    ApplySettings(ASettings);
  except
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
    SetState(csError);
    raise;
  end;

  FReader := TOBDSerialReadThread.Create(FHandle,
    procedure(const Bytes: TBytes)
    begin
      HandleBytes(Bytes);
    end,
    procedure(Code: TOBDErrorCode; const Msg: string)
    begin
      HandleError(Code, Msg);
    end);

  SetState(csOpen);
end;

procedure TOBDSerialTransport.Close;
var
  H: THandle;
begin
  FLock.Enter;
  try
    if FState in [csClosed, csClosing] then Exit;
    SetState(csClosing);
    H := FHandle;
    FHandle := INVALID_HANDLE_VALUE;
  finally
    FLock.Leave;
  end;

  if Assigned(FReader) then
  begin
    FReader.Terminate;
    if H <> INVALID_HANDLE_VALUE then
      CancelIoEx(H, nil);
    FReader.WaitFor;
    FreeAndNil(FReader);
  end;

  if H <> INVALID_HANDLE_VALUE then
    CloseHandle(H);

  SetState(csClosed);
end;

function TOBDSerialTransport.IsOpen: Boolean;
begin
  Result := State = csOpen;
end;

function TOBDSerialTransport.State: TOBDConnectionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

function TOBDSerialTransport.WriteBytes(const ABytes: TBytes): Integer;
var
  Written: DWORD;
begin
  if not IsOpen then
    raise EOBDNotConnected.Create('Serial transport is not open');
  if Length(ABytes) = 0 then
    Exit(0);
  if not WriteFile(FHandle, ABytes[0], Length(ABytes), Written, nil) then
  begin
    HandleError(oeIO, SysErrorMessage(GetLastError));
    Exit(0);
  end;
  Result := Integer(Written);
end;

function TOBDSerialTransport.GetOnDataReceived: TOBDBytesEvent;
begin
  Result := FOnDataReceived;
end;

procedure TOBDSerialTransport.SetOnDataReceived(const AValue: TOBDBytesEvent);
begin
  FOnDataReceived := AValue;
end;

function TOBDSerialTransport.GetOnStateChanged: TOBDStateEvent;
begin
  Result := FOnStateChanged;
end;

procedure TOBDSerialTransport.SetOnStateChanged(const AValue: TOBDStateEvent);
begin
  FOnStateChanged := AValue;
end;

function TOBDSerialTransport.GetOnTransportError: TOBDTransportErrorEvent;
begin
  Result := FOnTransportError;
end;

procedure TOBDSerialTransport.SetOnTransportError(
  const AValue: TOBDTransportErrorEvent);
begin
  FOnTransportError := AValue;
end;

end.
