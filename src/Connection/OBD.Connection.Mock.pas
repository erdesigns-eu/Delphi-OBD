//------------------------------------------------------------------------------
//  OBD.Connection.Mock
//
//  In-memory mock transport used by the test suite. Lets a test feed
//  scripted bytes "from the wire" and inspect bytes that were written
//  by the system under test, without any real I/O. Implements the same
//  IOBDConnectionTransport contract as the real transports so any code
//  that talks to a transport can be exercised against this.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//------------------------------------------------------------------------------

unit OBD.Connection.Mock;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Connection.Types;

type
  /// <summary>
  ///   Reference-counted mock transport. Construct with
  ///   <c>TOBDMockTransport.Create</c>, hold via the
  ///   <c>IOBDConnectionTransport</c> interface so the consumer can
  ///   close it; cast back to the concrete class in tests to feed
  ///   bytes and inspect writes.
  /// </summary>
  /// <remarks>
  ///   Thread-safe under <c>FLock</c>. Receive callbacks fire on the
  ///   thread that called <c>FeedBytes</c> (no internal worker).
  /// </remarks>
  TOBDMockTransport = class(TInterfacedObject, IOBDConnectionTransport)
  strict private
    FLock: TCriticalSection;
    FState: TOBDConnectionState;
    FWritten: TBytes;
    FOnDataReceived: TOBDBytesEvent;
    FOnStateChanged: TOBDStateEvent;
    FOnTransportError: TOBDTransportErrorEvent;
    procedure SetState(ANewState: TOBDConnectionState);
  public
    /// <summary>Creates an empty mock transport in
    /// <see cref="TOBDConnectionState.csClosed"/>.</summary>
    constructor Create;
    /// <summary>Releases internal lock and frees the captured buffer.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Closes the mock; transitions through <c>csClosing</c> to
    ///   <c>csClosed</c> and fires <c>OnStateChanged</c> twice.
    /// </summary>
    /// <remarks>Implements <see cref="IOBDConnectionTransport.Close"/>.</remarks>
    procedure Close;

    /// <summary>True if the mock is in <c>csOpen</c>.</summary>
    /// <returns><c>True</c> when ready to accept writes and feed reads.</returns>
    function IsOpen: Boolean;

    /// <summary>Current lifecycle state.</summary>
    /// <returns>State enum.</returns>
    function State: TOBDConnectionState;

    /// <summary>
    ///   Captures the bytes for later inspection via
    ///   <see cref="Written"/> / <see cref="WrittenString"/>.
    /// </summary>
    /// <param name="ABytes">Bytes to "send".</param>
    /// <returns>Always <c>Length(ABytes)</c> when open.</returns>
    /// <exception cref="EOBDNotConnected">Mock is not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer;

    /// <summary>Internal accessor for <c>OnDataReceived</c>.</summary>
    function GetOnDataReceived: TOBDBytesEvent;
    /// <summary>Internal mutator for <c>OnDataReceived</c>.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnDataReceived(const AValue: TOBDBytesEvent);
    /// <summary>Internal accessor for <c>OnStateChanged</c>.</summary>
    function GetOnStateChanged: TOBDStateEvent;
    /// <summary>Internal mutator for <c>OnStateChanged</c>.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnStateChanged(const AValue: TOBDStateEvent);
    /// <summary>Internal accessor for <c>OnTransportError</c>.</summary>
    function GetOnTransportError: TOBDTransportErrorEvent;
    /// <summary>Internal mutator for <c>OnTransportError</c>.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears.</param>
    procedure SetOnTransportError(const AValue: TOBDTransportErrorEvent);

    /// <summary>
    ///   Marks the mock as open: transitions through <c>csOpening</c>
    ///   to <c>csOpen</c> and fires <c>OnStateChanged</c> twice.
    /// </summary>
    procedure SimulateOpen;

    /// <summary>
    ///   Fires <c>OnDataReceived</c> as if <c>ABytes</c> arrived from
    ///   the wire.
    /// </summary>
    /// <param name="ABytes">Bytes to deliver to the consumer.</param>
    /// <remarks>
    ///   Fires synchronously on the calling thread. Tests that exercise
    ///   the main-thread marshalling in <see cref="TOBDConnection"/>
    ///   should call this from a worker thread.
    /// </remarks>
    procedure FeedBytes(const ABytes: TBytes);

    /// <summary>Convenience for <see cref="FeedBytes"/> with an ASCII
    /// string.</summary>
    /// <param name="AText">Text to deliver, encoded as ASCII bytes.</param>
    procedure FeedString(const AText: string);

    /// <summary>
    ///   Fires <c>OnTransportError</c> with the supplied code and
    ///   message.
    /// </summary>
    /// <param name="ACode">Coded error.</param>
    /// <param name="AMessage">Human-readable message.</param>
    procedure SimulateError(ACode: TOBDErrorCode; const AMessage: string);

    /// <summary>
    ///   Snapshot of bytes captured by <see cref="WriteBytes"/> since
    ///   the last <see cref="ClearWritten"/>.
    /// </summary>
    /// <returns>Defensive copy; modifying it does not affect future
    /// captures.</returns>
    function Written: TBytes;

    /// <summary>
    ///   Snapshot of captured bytes decoded as ASCII.
    /// </summary>
    /// <returns>ASCII string. Empty when nothing has been written.</returns>
    function WrittenString: string;

    /// <summary>Resets the capture buffer.</summary>
    procedure ClearWritten;
  end;

implementation

{ ---- TOBDMockTransport ------------------------------------------------------- }

constructor TOBDMockTransport.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FState := csClosed;
end;

destructor TOBDMockTransport.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TOBDMockTransport.SetState(ANewState: TOBDConnectionState);
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

procedure TOBDMockTransport.Close;
begin
  SetState(csClosing);
  SetState(csClosed);
end;

function TOBDMockTransport.IsOpen: Boolean;
begin
  FLock.Enter;
  try
    Result := FState = csOpen;
  finally
    FLock.Leave;
  end;
end;

function TOBDMockTransport.State: TOBDConnectionState;
begin
  FLock.Enter;
  try
    Result := FState;
  finally
    FLock.Leave;
  end;
end;

function TOBDMockTransport.WriteBytes(const ABytes: TBytes): Integer;
var
  OldLen: Integer;
begin
  FLock.Enter;
  try
    if FState <> csOpen then
      raise EOBDNotConnected.Create('Mock transport is not open');
    OldLen := Length(FWritten);
    SetLength(FWritten, OldLen + Length(ABytes));
    if Length(ABytes) > 0 then
      Move(ABytes[0], FWritten[OldLen], Length(ABytes));
    Result := Length(ABytes);
  finally
    FLock.Leave;
  end;
end;

function TOBDMockTransport.GetOnDataReceived: TOBDBytesEvent;
begin
  Result := FOnDataReceived;
end;

procedure TOBDMockTransport.SetOnDataReceived(const AValue: TOBDBytesEvent);
begin
  FOnDataReceived := AValue;
end;

function TOBDMockTransport.GetOnStateChanged: TOBDStateEvent;
begin
  Result := FOnStateChanged;
end;

procedure TOBDMockTransport.SetOnStateChanged(const AValue: TOBDStateEvent);
begin
  FOnStateChanged := AValue;
end;

function TOBDMockTransport.GetOnTransportError: TOBDTransportErrorEvent;
begin
  Result := FOnTransportError;
end;

procedure TOBDMockTransport.SetOnTransportError(
  const AValue: TOBDTransportErrorEvent);
begin
  FOnTransportError := AValue;
end;

procedure TOBDMockTransport.SimulateOpen;
begin
  SetState(csOpening);
  SetState(csOpen);
end;

procedure TOBDMockTransport.FeedBytes(const ABytes: TBytes);
var
  Handler: TOBDBytesEvent;
begin
  Handler := FOnDataReceived;
  if Assigned(Handler) then
    Handler(Self, ABytes);
end;

procedure TOBDMockTransport.FeedString(const AText: string);
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.ASCII.GetBytes(AText);
  FeedBytes(Bytes);
end;

procedure TOBDMockTransport.SimulateError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Handler: TOBDTransportErrorEvent;
begin
  Handler := FOnTransportError;
  if Assigned(Handler) then
    Handler(Self, ACode, AMessage);
end;

function TOBDMockTransport.Written: TBytes;
begin
  FLock.Enter;
  try
    Result := Copy(FWritten);
  finally
    FLock.Leave;
  end;
end;

function TOBDMockTransport.WrittenString: string;
var
  Bytes: TBytes;
begin
  Bytes := Written;
  if Length(Bytes) = 0 then
    Exit('');
  Result := TEncoding.ASCII.GetString(Bytes);
end;

procedure TOBDMockTransport.ClearWritten;
begin
  FLock.Enter;
  try
    SetLength(FWritten, 0);
  finally
    FLock.Leave;
  end;
end;

end.
