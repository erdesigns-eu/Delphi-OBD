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
    ///   Synchronous close. Equivalent to <c>Active := False</c>.
    /// </summary>
    /// <remarks>
    ///   No-op when not active. Joins the transport's worker thread
    ///   before returning.
    /// </remarks>
    procedure Close;

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
      if Assigned(FOnConnect) then
        FOnConnect(Self);
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
    if (not FRetryPolicy.Enabled) or (Attempt >= FRetryPolicy.MaxAttempts) then
      Break;
    Delay := FRetryPolicy.DelayForAttempt(Attempt);
    if Delay > 0 then
      TThread.Sleep(Delay);
  until False;

  if LastError <> nil then
    raise LastError;
  raise EOBDError.Create('Connection open failed (no specific error captured)');
end;

procedure TOBDConnection.DoClose;
var
  Local: IOBDConnectionTransport;
begin
  Local := FTransportImpl;
  FTransportImpl := nil;
  if Assigned(Local) then
    Local.Close;
  FActive := False;
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
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
