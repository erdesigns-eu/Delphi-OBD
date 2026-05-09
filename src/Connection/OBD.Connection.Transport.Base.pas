//------------------------------------------------------------------------------
//  OBD.Connection.Transport.Base
//
//  Abstract base for concrete transports. Owns the boilerplate every
//  transport repeats: lock, lifecycle state, event-handler fields,
//  thread-safe firing helpers, IOBDConnectionTransport accessor /
//  mutator implementations.
//
//  Concrete transports (Serial, Wi-Fi, UDP, Bluetooth, BLE, FTDI,
//  Mock) inherit from this class and implement only their transport-
//  specific Open(ASettings: …), Close, and WriteBytes.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 follow-up: extract common transport
//                     boilerplate so Phase 2 net runtime drops by
//                     ~480 lines and progress wiring is centralised.
//------------------------------------------------------------------------------

unit OBD.Connection.Transport.Base;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Connection.Types;

type
  /// <summary>
  ///   Abstract base implementing <see cref="IOBDConnectionTransport"/>.
  /// </summary>
  /// <remarks>
  ///   Reference-counted (descends from <c>TInterfacedObject</c>);
  ///   hold concrete transports via the interface so the lifetime
  ///   matches the connection.
  ///
  ///   Subclasses override <see cref="Close"/> and
  ///   <see cref="WriteBytes"/>, and add a transport-specific
  ///   <c>Open(ASettings: TOBDxxxSettings)</c>.
  /// </remarks>
  TOBDBaseTransport = class abstract(TInterfacedObject, IOBDConnectionTransport)
  strict protected
    /// <summary>Lock guarding lifecycle state and event handler
    /// fields.</summary>
    FLock: TCriticalSection;
    /// <summary>Current lifecycle state.</summary>
    FState: TOBDConnectionState;
    /// <summary>Inbound-bytes handler.</summary>
    FOnDataReceived: TOBDBytesEvent;
    /// <summary>State-changed handler.</summary>
    FOnStateChanged: TOBDStateEvent;
    /// <summary>Transport-error handler.</summary>
    FOnTransportError: TOBDTransportErrorEvent;
    /// <summary>Progress handler.</summary>
    FOnProgress: TOBDProgressEvent;

    /// <summary>
    ///   Updates <c>FState</c> and fires <c>OnStateChanged</c> if the
    ///   value actually changed. Safe to call from any thread.
    /// </summary>
    /// <param name="ANewState">Target state.</param>
    procedure SetState(ANewState: TOBDConnectionState);

    /// <summary>
    ///   Fires <c>OnDataReceived</c>. Should be called from the
    ///   transport's worker thread.
    /// </summary>
    /// <param name="ABytes">Newly received bytes.</param>
    procedure FireBytes(const ABytes: TBytes);

    /// <summary>
    ///   Fires <c>OnTransportError</c> and transitions to
    ///   <c>csError</c>.
    /// </summary>
    /// <param name="ACode">Coded error.</param>
    /// <param name="AMessage">Human-readable message.</param>
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);

    /// <summary>
    ///   Fires <c>OnProgress</c> with a step-style snapshot.
    /// </summary>
    /// <param name="AIndex">1-based step index.</param>
    /// <param name="ACount">Total expected steps; 0 = unknown.</param>
    /// <param name="AName">Phase name (required).</param>
    /// <param name="ADetail">Optional sub-detail. Pass empty if
    /// none.</param>
    procedure FireProgress(AIndex, ACount: Cardinal;
      const AName: string; const ADetail: string = '');

    /// <summary>
    ///   Fires <c>OnProgress</c> with a transfer-style snapshot.
    /// </summary>
    /// <param name="ABytesDone">Bytes transferred so far.</param>
    /// <param name="ABytesTotal">Total bytes to transfer; 0 =
    /// unknown.</param>
    /// <param name="AName">Phase name (required).</param>
    procedure FireProgressBytes(ABytesDone, ABytesTotal: Int64;
      const AName: string);
  public
    /// <summary>Constructs an idle transport in <c>csClosed</c>.</summary>
    constructor Create;
    /// <summary>Releases the lock (subclass <c>Destroy</c> closes
    /// first).</summary>
    destructor Destroy; override;

    // IOBDConnectionTransport — abstract operations
    /// <summary>Closes the transport. Implemented by subclasses.</summary>
    procedure Close; virtual; abstract;
    /// <summary>Sends bytes. Implemented by subclasses.</summary>
    /// <param name="ABytes">Bytes to send.</param>
    /// <returns>Bytes accepted.</returns>
    function WriteBytes(const ABytes: TBytes): Integer; virtual; abstract;

    // IOBDConnectionTransport — concrete here
    /// <summary>True when state is <c>csOpen</c>.</summary>
    function IsOpen: Boolean;
    /// <summary>Current lifecycle state.</summary>
    function State: TOBDConnectionState;

    function GetOnDataReceived: TOBDBytesEvent;
    procedure SetOnDataReceived(const AValue: TOBDBytesEvent);
    function GetOnStateChanged: TOBDStateEvent;
    procedure SetOnStateChanged(const AValue: TOBDStateEvent);
    function GetOnTransportError: TOBDTransportErrorEvent;
    procedure SetOnTransportError(const AValue: TOBDTransportErrorEvent);
    function GetOnProgress: TOBDProgressEvent;
    procedure SetOnProgress(const AValue: TOBDProgressEvent);
  end;

implementation

{ ---- TOBDBaseTransport ------------------------------------------------------- }

constructor TOBDBaseTransport.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FState := csClosed;
end;

destructor TOBDBaseTransport.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TOBDBaseTransport.SetState(ANewState: TOBDConnectionState);
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

procedure TOBDBaseTransport.FireBytes(const ABytes: TBytes);
var
  Handler: TOBDBytesEvent;
begin
  Handler := FOnDataReceived;
  if Assigned(Handler) then
    Handler(Self, ABytes);
end;

procedure TOBDBaseTransport.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Handler: TOBDTransportErrorEvent;
begin
  Handler := FOnTransportError;
  if Assigned(Handler) then
    Handler(Self, ACode, AMessage);
  SetState(csError);
end;

procedure TOBDBaseTransport.FireProgress(AIndex, ACount: Cardinal;
  const AName: string; const ADetail: string);
var
  Handler: TOBDProgressEvent;
  Step: TOBDProgressStep;
begin
  Handler := FOnProgress;
  if not Assigned(Handler) then Exit;
  Step := TOBDProgressStep.MakeStep(AIndex, ACount, AName, ADetail);
  Handler(Self, Step);
end;

procedure TOBDBaseTransport.FireProgressBytes(ABytesDone, ABytesTotal: Int64;
  const AName: string);
var
  Handler: TOBDProgressEvent;
  Step: TOBDProgressStep;
begin
  Handler := FOnProgress;
  if not Assigned(Handler) then Exit;
  Step := TOBDProgressStep.MakeBytes(ABytesDone, ABytesTotal, AName);
  Handler(Self, Step);
end;

function TOBDBaseTransport.IsOpen: Boolean;
begin
  Result := State = csOpen;
end;

function TOBDBaseTransport.State: TOBDConnectionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

function TOBDBaseTransport.GetOnDataReceived: TOBDBytesEvent;
begin
  Result := FOnDataReceived;
end;

procedure TOBDBaseTransport.SetOnDataReceived(const AValue: TOBDBytesEvent);
begin
  FOnDataReceived := AValue;
end;

function TOBDBaseTransport.GetOnStateChanged: TOBDStateEvent;
begin
  Result := FOnStateChanged;
end;

procedure TOBDBaseTransport.SetOnStateChanged(const AValue: TOBDStateEvent);
begin
  FOnStateChanged := AValue;
end;

function TOBDBaseTransport.GetOnTransportError: TOBDTransportErrorEvent;
begin
  Result := FOnTransportError;
end;

procedure TOBDBaseTransport.SetOnTransportError(
  const AValue: TOBDTransportErrorEvent);
begin
  FOnTransportError := AValue;
end;

function TOBDBaseTransport.GetOnProgress: TOBDProgressEvent;
begin
  Result := FOnProgress;
end;

procedure TOBDBaseTransport.SetOnProgress(const AValue: TOBDProgressEvent);
begin
  FOnProgress := AValue;
end;

end.
