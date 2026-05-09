//------------------------------------------------------------------------------
//  OBD.Connection
//
//  TOBDConnection — the single non-visual component a user drops on a
//  form to talk to an OBD adapter over any of the supported transports.
//
//  Architecture: one component, enum-driven. Setting Transport selects
//  the active sub-settings sub-object; Active := True instantiates the
//  matching IOBDConnectionTransport implementation, wires its callbacks
//  to the component, and marshals events to the main thread.
//
//  Bound consumers (TOBDAdapter — Phase 3) read bytes via OnDataReceived
//  and write via WriteBytes / WriteString. They do not see the transport
//  layer directly.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//------------------------------------------------------------------------------

unit OBD.Connection;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings,
  OBD.Connection.Retry;

type
  /// <summary>
  ///   Event surface for transient (non-fatal) errors.
  /// </summary>
  /// <param name="Sender">The TOBDConnection instance.</param>
  /// <param name="ACode">Coded error.</param>
  /// <param name="AMessage">Human-readable message.</param>
  /// <param name="AHandled">Set True to suppress the default behaviour
  /// (state transition to <c>csError</c>). Defaults to False.</param>
  TOBDConnectionErrorEvent = procedure(Sender: TObject;
    ACode: TOBDErrorCode; const AMessage: string;
    var AHandled: Boolean) of object;

  /// <summary>
  ///   Headless OBD-II / diagnostics connection component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, set <c>Transport</c>, populate the matching
  ///   sub-settings sub-object, wire <c>OnDataReceived</c>, then set
  ///   <c>Active := True</c>. Threading is internal; events fire on the
  ///   main thread.
  /// </remarks>
  TOBDConnection = class(TComponent)
  strict private
    // configuration
    FTransport: TOBDTransport;
    FSerialSettings: TOBDSerialSettings;
    FBluetoothSettings: TOBDBluetoothSettings;
    FBLESettings: TOBDBLESettings;
    FWiFiSettings: TOBDWiFiSettings;
    FUDPSettings: TOBDUDPSettings;
    FFTDISettings: TOBDFTDISettings;
    FRetryPolicy: TOBDRetryPolicy;

    // runtime
    FActive: Boolean;
    FTransportImpl: IOBDConnectionTransport;
    FState: TOBDConnectionState;

    // async open
    FAsyncOpenLock: TCriticalSection;
    FAsyncOpenThread: TThread;
    FAsyncOpenCancelled: Boolean;

    // events
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnDataReceived: TOBDBytesEvent;
    FOnStateChanged: TOBDStateEvent;
    FOnError: TOBDConnectionErrorEvent;

    procedure SetActive(AValue: Boolean);
    procedure SetTransport(AValue: TOBDTransport);
    procedure SetSerialSettings(AValue: TOBDSerialSettings);
    procedure SetBluetoothSettings(AValue: TOBDBluetoothSettings);
    procedure SetBLESettings(AValue: TOBDBLESettings);
    procedure SetWiFiSettings(AValue: TOBDWiFiSettings);
    procedure SetUDPSettings(AValue: TOBDUDPSettings);
    procedure SetFTDISettings(AValue: TOBDFTDISettings);
    procedure SetRetryPolicy(AValue: TOBDRetryPolicy);

    procedure DoOpen;
    procedure DoClose;

    procedure FireOnConnect;
    procedure FireOnDisconnect;
    procedure FireOnError(ACode: TOBDErrorCode; const AMessage: string);
    procedure WaitForAsyncOpen;

    procedure HandleTransportBytes(Sender: TObject; const ABytes: TBytes);
    procedure HandleTransportState(Sender: TObject;
      NewState: TOBDConnectionState);
    procedure HandleTransportError(Sender: TObject; ACode: TOBDErrorCode;
      const AMessage: string);
  public
    /// <summary>Allocates default sub-objects and initialises the
    /// component.</summary>
    /// <param name="AOwner">Standard <c>TComponent</c> owner. May be
    /// <c>nil</c> for code-only use.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Closes the active transport (if any) and frees all
    /// sub-objects. Does not propagate exceptions.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Synchronous open. Equivalent to <c>Active := True</c>.
    /// </summary>
    /// <remarks>
    ///   Honours <c>RetryPolicy</c>: on transient open failure, sleeps
    ///   per the policy and retries up to <c>RetryPolicy.MaxAttempts</c>
    ///   times before re-raising the last exception.
    /// </remarks>
    /// <exception cref="EOBDConfig">Settings missing for the selected
    /// transport.</exception>
    /// <exception cref="EOBDError">Underlying transport refused to
    /// open after all retries.</exception>
    /// <exception cref="EOBDUnsupported">Selected transport not
    /// available on this platform (e.g. Serial / FTDI on POSIX).</exception>
    procedure Open;

    /// <summary>
    ///   Non-blocking open. Returns immediately; the actual transport
    ///   open runs on a background worker.
    /// </summary>
    /// <remarks>
    ///   On success, fires <c>OnConnect</c> on the main thread. On
    ///   failure, fires <c>OnError</c> on the main thread with a coded
    ///   error and a human-readable message. Honours
    ///   <c>RetryPolicy</c> the same way <see cref="Open"/> does.
    ///
    ///   Use this when you do not want to block the UI thread on a
    ///   slow transport (Wi-Fi DNS, Bluetooth handshake, BLE GATT
    ///   discovery). The synchronous <see cref="Open"/> method remains
    ///   available for code-driven use where blocking is acceptable.
    ///
    ///   Only one async open may be in flight at a time. Calling
    ///   <c>OpenAsync</c> while another is in progress raises. Calling
    ///   <c>Close</c> during an in-flight async open marks it
    ///   cancelled, waits for the worker to exit, then closes
    ///   normally.
    /// </remarks>
    /// <exception cref="EOBDConfig">Already active or another async
    /// open is already in progress.</exception>
    procedure OpenAsync;

    /// <summary>
    ///   Synchronous close. Equivalent to <c>Active := False</c>.
    /// </summary>
    /// <remarks>
    ///   No-op when not active. Joins the transport's worker thread
    ///   before returning. If an async open is in flight it is
    ///   cancelled and joined first.
    /// </remarks>
    procedure Close;

    /// <summary>
    ///   Non-blocking close. Returns immediately; the actual transport
    ///   shutdown runs on a background worker.
    /// </summary>
    /// <remarks>
    ///   Fires <c>OnDisconnect</c> on the main thread once the
    ///   transport has fully closed. Use this when a transport's
    ///   close path can take noticeable time (Bluetooth in particular)
    ///   and you do not want to block the UI thread.
    ///
    ///   Calling <c>CloseAsync</c> when the connection is already
    ///   closed is a no-op (no event fires).
    /// </remarks>
    procedure CloseAsync;

    /// <summary>Sends raw bytes through the active transport.</summary>
    /// <param name="ABytes">Bytes to send. Empty returns 0.</param>
    /// <returns>Number of bytes accepted by the transport.</returns>
    /// <exception cref="EOBDNotConnected">Connection is not active.</exception>
    function WriteBytes(const ABytes: TBytes): Integer;

    /// <summary>
    ///   Sends an ASCII string through the active transport. The
    ///   string is encoded as ASCII bytes and forwarded to
    ///   <see cref="WriteBytes"/>.
    /// </summary>
    /// <param name="AText">Text to send.</param>
    /// <returns>Number of bytes accepted by the transport.</returns>
    /// <exception cref="EOBDNotConnected">Connection is not active.</exception>
    function WriteString(const AText: string): Integer;

    /// <summary>Current lifecycle state.</summary>
    /// <returns><c>csClosed</c> when no transport is bound; otherwise
    /// the transport's own state.</returns>
    function State: TOBDConnectionState;

    /// <summary>The currently active transport implementation, or nil
    /// when not active.</summary>
    /// <remarks>For low-level callers (e.g. tests, the async wrapper);
    /// most components should use the public surface above.</remarks>
    property TransportImpl: IOBDConnectionTransport read FTransportImpl;
  published
    /// <summary>Open / close. Default False.</summary>
    property Active: Boolean read FActive write SetActive default False;
    /// <summary>Selects which transport will be used when
    /// <c>Active := True</c>. Default <c>otSerial</c>.</summary>
    property Transport: TOBDTransport read FTransport write SetTransport
      default otSerial;
    /// <summary>Serial-port tuneables (used when
    /// <c>Transport = otSerial</c>).</summary>
    property SerialSettings: TOBDSerialSettings read FSerialSettings
      write SetSerialSettings;
    /// <summary>Bluetooth Classic tuneables.</summary>
    property BluetoothSettings: TOBDBluetoothSettings
      read FBluetoothSettings write SetBluetoothSettings;
    /// <summary>Bluetooth LE tuneables.</summary>
    property BLESettings: TOBDBLESettings read FBLESettings write SetBLESettings;
    /// <summary>Wi-Fi (TCP) tuneables.</summary>
    property WiFiSettings: TOBDWiFiSettings read FWiFiSettings write SetWiFiSettings;
    /// <summary>UDP tuneables (DoIP discovery, broadcast).</summary>
    property UDPSettings: TOBDUDPSettings read FUDPSettings write SetUDPSettings;
    /// <summary>FTDI D2XX tuneables.</summary>
    property FTDISettings: TOBDFTDISettings read FFTDISettings write SetFTDISettings;
    /// <summary>Retry policy for transient open failures.</summary>
    property RetryPolicy: TOBDRetryPolicy read FRetryPolicy write SetRetryPolicy;

    /// <summary>Fires after a successful open.</summary>
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    /// <summary>Fires after the transport has fully closed.</summary>
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    /// <summary>Fires when bytes arrive from the wire (main thread).</summary>
    property OnDataReceived: TOBDBytesEvent read FOnDataReceived
      write FOnDataReceived;
    /// <summary>Fires on every state transition (main thread).</summary>
    property OnStateChanged: TOBDStateEvent read FOnStateChanged
      write FOnStateChanged;
    /// <summary>Fires on transient I/O errors.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  OBD.Connection.Mock,
{$IFDEF MSWINDOWS}
  OBD.Connection.Serial,
  OBD.Connection.FTDI,
{$ENDIF}
  OBD.Connection.WiFi,
  OBD.Connection.UDP,
  OBD.Connection.Bluetooth,
  OBD.Connection.BLE;

{ ---- TOBDConnection ---------------------------------------------------------- }

constructor TOBDConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncOpenLock := TCriticalSection.Create;
  FSerialSettings := TOBDSerialSettings.Create;
  FBluetoothSettings := TOBDBluetoothSettings.Create;
  FBLESettings := TOBDBLESettings.Create;
  FWiFiSettings := TOBDWiFiSettings.Create;
  FUDPSettings := TOBDUDPSettings.Create;
  FFTDISettings := TOBDFTDISettings.Create;
  FRetryPolicy := TOBDRetryPolicy.Create;
  FTransport := otSerial;
  FState := csClosed;
end;

destructor TOBDConnection.Destroy;
begin
  // Cancel and wait for any in-flight OpenAsync before tearing down.
  WaitForAsyncOpen;
  if FActive then
  try
    DoClose;
  except
    // Destructor must not propagate.
  end;
  FSerialSettings.Free;
  FBluetoothSettings.Free;
  FBLESettings.Free;
  FWiFiSettings.Free;
  FUDPSettings.Free;
  FFTDISettings.Free;
  FRetryPolicy.Free;
  FAsyncOpenLock.Free;
  inherited;
end;

procedure TOBDConnection.SetSerialSettings(AValue: TOBDSerialSettings);
begin FSerialSettings.Assign(AValue); end;
procedure TOBDConnection.SetBluetoothSettings(AValue: TOBDBluetoothSettings);
begin FBluetoothSettings.Assign(AValue); end;
procedure TOBDConnection.SetBLESettings(AValue: TOBDBLESettings);
begin FBLESettings.Assign(AValue); end;
procedure TOBDConnection.SetWiFiSettings(AValue: TOBDWiFiSettings);
begin FWiFiSettings.Assign(AValue); end;
procedure TOBDConnection.SetUDPSettings(AValue: TOBDUDPSettings);
begin FUDPSettings.Assign(AValue); end;
procedure TOBDConnection.SetFTDISettings(AValue: TOBDFTDISettings);
begin FFTDISettings.Assign(AValue); end;
procedure TOBDConnection.SetRetryPolicy(AValue: TOBDRetryPolicy);
begin FRetryPolicy.Assign(AValue); end;

procedure TOBDConnection.SetTransport(AValue: TOBDTransport);
begin
  if FTransport = AValue then Exit;
  if FActive then
    raise EOBDConfig.Create(
      'Transport cannot be changed while Active = True. Close first.');
  FTransport := AValue;
end;

procedure TOBDConnection.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  if AValue then DoOpen else DoClose;
end;

procedure TOBDConnection.DoOpen;
var
  Attempt: Cardinal;
  Delay: Cardinal;
  LastError: Exception;
{$IFDEF MSWINDOWS}
  SerialT: TOBDSerialTransport;
  FTDIT: TOBDFTDITransport;
{$ENDIF}
  WiFiT: TOBDWiFiTransport;
  UDPT: TOBDUDPTransport;
  BTT: TOBDBluetoothTransport;
  BLET: TOBDBLETransport;
begin
  Attempt := 0;
  LastError := nil;
  repeat
    Inc(Attempt);
    try
      case FTransport of
{$IFDEF MSWINDOWS}
        otSerial:
          begin
            SerialT := TOBDSerialTransport.Create;
            FTransportImpl := SerialT;
            SerialT.OnDataReceived := HandleTransportBytes;
            SerialT.OnStateChanged := HandleTransportState;
            SerialT.OnTransportError := HandleTransportError;
            SerialT.Open(FSerialSettings);
          end;
        otFTDI:
          begin
            FTDIT := TOBDFTDITransport.Create;
            FTransportImpl := FTDIT;
            FTDIT.OnDataReceived := HandleTransportBytes;
            FTDIT.OnStateChanged := HandleTransportState;
            FTDIT.OnTransportError := HandleTransportError;
            FTDIT.Open(FFTDISettings);
          end;
{$ELSE}
        otSerial, otFTDI:
          raise EOBDUnsupported.Create(
            'Serial / FTDI transports are Windows-only in v1');
{$ENDIF}
        otBluetooth:
          begin
            BTT := TOBDBluetoothTransport.Create;
            FTransportImpl := BTT;
            BTT.OnDataReceived := HandleTransportBytes;
            BTT.OnStateChanged := HandleTransportState;
            BTT.OnTransportError := HandleTransportError;
            BTT.Open(FBluetoothSettings);
          end;
        otBLE:
          begin
            BLET := TOBDBLETransport.Create;
            FTransportImpl := BLET;
            BLET.OnDataReceived := HandleTransportBytes;
            BLET.OnStateChanged := HandleTransportState;
            BLET.OnTransportError := HandleTransportError;
            BLET.Open(FBLESettings);
          end;
        otWiFi:
          begin
            WiFiT := TOBDWiFiTransport.Create;
            FTransportImpl := WiFiT;
            WiFiT.OnDataReceived := HandleTransportBytes;
            WiFiT.OnStateChanged := HandleTransportState;
            WiFiT.OnTransportError := HandleTransportError;
            WiFiT.Open(FWiFiSettings);
          end;
        otUDP:
          begin
            UDPT := TOBDUDPTransport.Create;
            FTransportImpl := UDPT;
            UDPT.OnDataReceived := HandleTransportBytes;
            UDPT.OnStateChanged := HandleTransportState;
            UDPT.OnTransportError := HandleTransportError;
            UDPT.Open(FUDPSettings);
          end;
      else
        raise EOBDConfig.CreateFmt('Unknown transport %d', [Ord(FTransport)]);
      end;

      // Success
      FActive := True;
      FireOnConnect;
      Exit;

    except
      on E: Exception do
      begin
        FTransportImpl := nil;
        FreeAndNil(LastError);
        LastError := Exception(AcquireExceptionObject);
      end;
    end;

    // Retry?
    if FAsyncOpenCancelled or
       (not FRetryPolicy.Enabled) or
       (Attempt >= FRetryPolicy.MaxAttempts) then
      Break;
    Delay := FRetryPolicy.DelayForAttempt(Attempt);
    if Delay > 0 then
      TThread.Sleep(Delay);
  until False;

  if LastError <> nil then
    raise LastError;
  raise EOBDError.Create('Connection open failed (no specific error captured)');
end;

procedure TOBDConnection.FireOnConnect;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnConnect) then
      FOnConnect(Self);
  end
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnConnect) then
          FOnConnect(Self);
      end);
end;

procedure TOBDConnection.FireOnDisconnect;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnDisconnect) then
      FOnDisconnect(Self);
  end
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnDisconnect) then
          FOnDisconnect(Self);
      end);
end;

procedure TOBDConnection.FireOnError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  CodeCopy: TOBDErrorCode;
  MsgCopy: string;
begin
  CodeCopy := ACode;
  MsgCopy := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    if Assigned(FOnError) then
      FOnError(Self, CodeCopy, MsgCopy, Handled);
  end
  else
    TThread.Queue(nil,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(FOnError) then
          FOnError(Self, CodeCopy, MsgCopy, Handled);
      end);
end;

procedure TOBDConnection.WaitForAsyncOpen;
var
  Worker: TThread;
begin
  // Atomically claim the worker pointer. Either we got the live worker
  // (must wait + free) or the cleanup queued from Execute already
  // ran (Worker = nil, no-op).
  FAsyncOpenLock.Enter;
  try
    Worker := FAsyncOpenThread;
    FAsyncOpenThread := nil;
    if Worker <> nil then
      FAsyncOpenCancelled := True;
  finally
    FAsyncOpenLock.Leave;
  end;
  if Worker = nil then
    Exit;
  // Worker may close the transport on its way out; that's fine.
  Worker.WaitFor;
  Worker.Free;
  FAsyncOpenLock.Enter;
  try
    FAsyncOpenCancelled := False;
  finally
    FAsyncOpenLock.Leave;
  end;
end;

procedure TOBDConnection.DoClose;
var
  Local: IOBDConnectionTransport;
begin
  // Cancel any in-flight async open before tearing down the transport.
  WaitForAsyncOpen;
  Local := FTransportImpl;
  FTransportImpl := nil;
  if Assigned(Local) then
    Local.Close;
  FActive := False;
  FireOnDisconnect;
end;

procedure TOBDConnection.CloseAsync;
var
  Self_: TOBDConnection;
begin
  if not FActive and (FTransportImpl = nil) then
    Exit;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        Self_.DoClose;
      except
        // Close errors are surfaced via OnError on the worker; never
        // propagate out of an anonymous-thread closure.
        on E: Exception do
          Self_.FireOnError(oeIO, E.Message);
      end;
    end).Start;
end;

procedure TOBDConnection.OpenAsync;
var
  Worker: TThread;
begin
  if FActive then
    raise EOBDConfig.Create('Connection is already active');

  FAsyncOpenLock.Enter;
  try
    if FAsyncOpenThread <> nil then
      raise EOBDConfig.Create('OpenAsync is already in progress');
    FAsyncOpenCancelled := False;
  finally
    FAsyncOpenLock.Leave;
  end;

  Worker := TThread.CreateAnonymousThread(
    procedure
    var
      OpenError: string;
      OpenCode: TOBDErrorCode;
      Self_: TOBDConnection;
    begin
      Self_ := Self;
      OpenError := '';
      OpenCode := oeNone;
      try
        try
          DoOpen;
          // DoOpen has fired OnConnect via FireOnConnect (main thread).
        except
          on E: Exception do
          begin
            OpenError := E.Message;
            if E is EOBDConfig then
              OpenCode := oeAdapterFault
            else if E is EOBDNotConnected then
              OpenCode := oeAdapterFault
            else
              OpenCode := oeIO;
          end;
        end;
      finally
        if OpenError <> '' then
          FireOnError(OpenCode, OpenError);
        // Schedule self-reaping on the main thread. WaitForAsyncOpen
        // claims FAsyncOpenThread atomically: either we get to free it
        // here (live worker, no concurrent Wait), or Wait already
        // claimed it (Worker = nil) and we do nothing.
        TThread.Queue(nil,
          procedure
          var
            ToFree: TThread;
          begin
            Self_.FAsyncOpenLock.Enter;
            try
              ToFree := Self_.FAsyncOpenThread;
              Self_.FAsyncOpenThread := nil;
            finally
              Self_.FAsyncOpenLock.Leave;
            end;
            if ToFree <> nil then
            begin
              ToFree.WaitFor;
              ToFree.Free;
            end;
          end);
      end;
    end);
  Worker.FreeOnTerminate := False;

  FAsyncOpenLock.Enter;
  try
    FAsyncOpenThread := Worker;
  finally
    FAsyncOpenLock.Leave;
  end;
  Worker.Start;
end;

procedure TOBDConnection.Open;
begin
  SetActive(True);
end;

procedure TOBDConnection.Close;
begin
  SetActive(False);
end;

function TOBDConnection.WriteBytes(const ABytes: TBytes): Integer;
begin
  if not FActive or (FTransportImpl = nil) then
    raise EOBDNotConnected.Create('TOBDConnection is not active');
  Result := FTransportImpl.WriteBytes(ABytes);
end;

function TOBDConnection.WriteString(const AText: string): Integer;
begin
  Result := WriteBytes(TEncoding.ASCII.GetBytes(AText));
end;

function TOBDConnection.State: TOBDConnectionState;
begin
  if FTransportImpl = nil then
    Result := csClosed
  else
    Result := FTransportImpl.State;
end;

procedure TOBDConnection.HandleTransportBytes(Sender: TObject;
  const ABytes: TBytes);
var
  Snapshot: TBytes;
begin
  // Marshal to main thread; copy because the transport may reuse its
  // internal buffer.
  Snapshot := Copy(ABytes);
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnDataReceived) then
      FOnDataReceived(Self, Snapshot);
  end
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnDataReceived) then
          FOnDataReceived(Self, Snapshot);
      end);
end;

procedure TOBDConnection.HandleTransportState(Sender: TObject;
  NewState: TOBDConnectionState);
begin
  FState := NewState;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnStateChanged) then
      FOnStateChanged(Self, NewState);
  end
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnStateChanged) then
          FOnStateChanged(Self, NewState);
      end);
end;

procedure TOBDConnection.HandleTransportError(Sender: TObject;
  ACode: TOBDErrorCode; const AMessage: string);
var
  Code: TOBDErrorCode;
  Msg: string;
begin
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    if Assigned(FOnError) then
      FOnError(Self, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(FOnError) then
          FOnError(Self, Code, Msg, Handled);
      end);
end;

end.
