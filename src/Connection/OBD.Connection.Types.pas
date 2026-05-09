//------------------------------------------------------------------------------
//  OBD.Connection.Types
//
//  Shared types and the IOBDConnectionTransport contract.
//
//  Every concrete transport (Serial, Bluetooth, BLE, Wi-Fi, UDP, FTDI)
//  implements <see cref="IOBDConnectionTransport"/>; the TOBDConnection
//  component routes its public surface to whichever transport the
//  Transport enum selects.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - OBD.Types (TOBDTransport, TOBDErrorCode)
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial: contract for transports, byte
//                     events, state enum, baud rate / handshake / parity
//                     enums.
//
//  Future work :
//    - SecOC freshness hook (Phase 4) needs an authenticated-frame
//      callback shape; revisit when SecOC lands.
//------------------------------------------------------------------------------

unit OBD.Connection.Types;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types;

type
  /// <summary>
  ///   Lifecycle state of a connection / transport.
  /// </summary>
  /// <remarks>
  ///   Stable enum order; values are streamed.
  /// </remarks>
  TOBDConnectionState = (
    /// <summary>Closed and idle.</summary>
    csClosed,
    /// <summary>Open is in progress (e.g. dialling Bluetooth, opening
    /// COM port). Transient state.</summary>
    csOpening,
    /// <summary>Open and ready for I/O.</summary>
    csOpen,
    /// <summary>Close is in progress. Transient state.</summary>
    csClosing,
    /// <summary>Errored — reason surfaced on the most recent OnError
    /// event. The transport is logically closed.</summary>
    csError
  );

  /// <summary>
  ///   Standard serial baud rates for COM and FTDI transports.
  /// </summary>
  /// <remarks>
  ///   Numeric values match the canonical baud number, not a Windows
  ///   constant. Convert with <see cref="OBDBaudRateValue"/> for the
  ///   underlying API.
  /// </remarks>
  TOBDBaudRate = (
    br9600     = 9600,
    br19200    = 19200,
    br38400    = 38400,
    br57600    = 57600,
    br115200   = 115200,
    br230400   = 230400,
    br460800   = 460800,
    br500000   = 500000,
    br921600   = 921600,
    br1000000  = 1000000,
    br2000000  = 2000000
  );

  /// <summary>Serial parity options.</summary>
  TOBDParity = (
    /// <summary>No parity bit.</summary>
    paNone,
    /// <summary>Odd parity.</summary>
    paOdd,
    /// <summary>Even parity.</summary>
    paEven,
    /// <summary>Mark parity (forced 1).</summary>
    paMark,
    /// <summary>Space parity (forced 0).</summary>
    paSpace
  );

  /// <summary>Serial stop bit options.</summary>
  TOBDStopBits = (
    /// <summary>1 stop bit (default).</summary>
    sb1,
    /// <summary>1.5 stop bits.</summary>
    sb1_5,
    /// <summary>2 stop bits.</summary>
    sb2
  );

  /// <summary>Serial flow control.</summary>
  TOBDFlowControl = (
    /// <summary>No flow control.</summary>
    fcNone,
    /// <summary>RTS/CTS hardware flow control.</summary>
    fcHardware,
    /// <summary>XON/XOFF software flow control.</summary>
    fcSoftware
  );

  /// <summary>
  ///   Event raised when a transport receives bytes from the wire.
  /// </summary>
  /// <param name="Sender">Transport instance.</param>
  /// <param name="ABytes">Newly received bytes. Owned by the transport
  /// (do not free); copy if you need to retain.</param>
  /// <remarks>
  ///   Fired on the worker thread of the transport. The TOBDConnection
  ///   component re-fires its own <c>OnDataReceived</c> on the main
  ///   thread; consumers should subscribe to that, not to the transport
  ///   directly.
  /// </remarks>
  TOBDBytesEvent = procedure(Sender: TObject; const ABytes: TBytes) of object;

  /// <summary>
  ///   Event raised when the transport state changes.
  /// </summary>
  /// <param name="Sender">Transport instance.</param>
  /// <param name="NewState">New lifecycle state.</param>
  TOBDStateEvent = procedure(Sender: TObject;
    NewState: TOBDConnectionState) of object;

  /// <summary>
  ///   Event raised when a transient error occurs.
  /// </summary>
  /// <param name="Sender">Transport instance.</param>
  /// <param name="ACode">Coded error from <see cref="TOBDErrorCode"/>.</param>
  /// <param name="AMessage">Human-readable message; never empty.</param>
  /// <remarks>
  ///   Errors fire here, not exceptions. Configuration / programmer
  ///   errors raise <see cref="EOBDError"/> descendants synchronously
  ///   from the call site instead.
  /// </remarks>
  TOBDTransportErrorEvent = procedure(Sender: TObject;
    ACode: TOBDErrorCode; const AMessage: string) of object;

  /// <summary>
  ///   Snapshot of progress through a long-running operation.
  /// </summary>
  /// <remarks>
  ///   Carries both step-style progress (<c>Index</c> /
  ///   <c>Count</c> / <c>Name</c> / <c>Detail</c>) for sequential
  ///   phases (open Bluetooth, locate device, connect socket, …) and
  ///   transfer-style progress (<c>BytesDone</c> /
  ///   <c>BytesTotal</c>) for byte-counted operations (flash, upload,
  ///   download).
  ///
  ///   A single firing typically populates one shape or the other;
  ///   call <see cref="Percent"/> for a unified 0..1 ratio that
  ///   prefers byte counts when present and falls back to step counts.
  ///
  ///   <c>Count</c> or <c>BytesTotal</c> may be <c>0</c> to indicate
  ///   the total is unknown; consumers should render an indeterminate
  ///   bar / spinner in that case.
  /// </remarks>
  TOBDProgressStep = record
    /// <summary>1-based step index. <c>0</c> when only byte progress
    /// is being reported.</summary>
    Index: Cardinal;
    /// <summary>Total expected step count. <c>0</c> when unknown.</summary>
    Count: Cardinal;
    /// <summary>Human-readable phase name (e.g. <c>'Resolving DNS'</c>,
    /// <c>'Subscribing to notifications'</c>).</summary>
    Name: string;
    /// <summary>Optional sub-detail (e.g. the host being resolved, the
    /// device address being dialled). May be empty.</summary>
    Detail: string;
    /// <summary>Bytes transferred so far. <c>0</c> for non-transfer
    /// progress.</summary>
    BytesDone: Int64;
    /// <summary>Total bytes to transfer. <c>0</c> when unknown or not
    /// applicable.</summary>
    BytesTotal: Int64;

    /// <summary>Computes a 0..1 ratio. Prefers
    /// <c>BytesDone / BytesTotal</c> when both are non-zero; falls
    /// back to <c>Index / Count</c> when no byte counts are
    /// available; returns 0 when both are unknown.</summary>
    function Percent: Double;
    /// <summary>Convenience constructor for a step-style progress
    /// snapshot.</summary>
    /// <param name="AIndex">1-based step index.</param>
    /// <param name="ACount">Total expected steps; pass <c>0</c> if
    /// unknown.</param>
    /// <param name="AName">Human-readable phase name.</param>
    /// <param name="ADetail">Optional sub-detail.</param>
    /// <returns>Initialised record.</returns>
    class function MakeStep(AIndex, ACount: Cardinal;
      const AName, ADetail: string): TOBDProgressStep; static;
    /// <summary>Convenience constructor for a transfer-style progress
    /// snapshot.</summary>
    /// <param name="ABytesDone">Bytes transferred so far.</param>
    /// <param name="ABytesTotal">Total bytes to transfer; <c>0</c> if
    /// unknown.</param>
    /// <param name="AName">Human-readable phase name.</param>
    /// <returns>Initialised record.</returns>
    class function MakeBytes(ABytesDone, ABytesTotal: Int64;
      const AName: string): TOBDProgressStep; static;
  end;

  /// <summary>
  ///   Event raised as a long-running operation progresses.
  /// </summary>
  /// <param name="Sender">Source component / transport.</param>
  /// <param name="AStep">Snapshot of current progress.</param>
  /// <remarks>
  ///   On <c>TOBDConnection</c> and downstream components, fires on
  ///   the main thread. On <c>IOBDConnectionTransport</c> directly,
  ///   fires on the transport's worker thread; consumers normally
  ///   subscribe at the component level instead.
  /// </remarks>
  TOBDProgressEvent = procedure(Sender: TObject;
    const AStep: TOBDProgressStep) of object;

  /// <summary>
  ///   Settings root for a transport. Concrete subclasses
  ///   (<c>TOBDSerialSettings</c>, <c>TOBDWiFiSettings</c>, …) live in
  ///   <c>OBD.Connection.Settings</c>; this base just provides a uniform
  ///   anchor for cross-transport plumbing.
  /// </summary>
  /// <remarks>
  ///   Subclasses must publish their tuneables so they round-trip
  ///   through DFM streaming, and must override <c>Assign</c> to copy
  ///   them in <c>SetXxxSettings</c> on the parent component.
  /// </remarks>
  TOBDTransportSettings = class(TPersistent);

  /// <summary>
  ///   Contract every concrete transport implements.
  /// </summary>
  /// <remarks>
  ///   Implementations are expected to spawn a worker thread on Open
  ///   and tear it down on Close. Read callbacks fire on the worker
  ///   thread; the consumer (TOBDConnection) is responsible for
  ///   marshalling to the main thread.
  ///
  ///   <c>WriteBytes</c> is synchronous from the caller's perspective —
  ///   either the bytes are queued for transmission and the call
  ///   returns <c>Length(ABytes)</c>, or the transport is in error and
  ///   the call raises (configuration-shaped) or fires
  ///   <c>OnTransportError</c> and returns 0 (transient).
  /// </remarks>
  IOBDConnectionTransport = interface
    ['{D4E5C7A2-3F1B-4C9E-9D8A-7E6B5C4D3A2F}']

    /// <summary>
    ///   Closes the transport, terminates its worker thread, and
    ///   releases any OS handle held.
    /// </summary>
    /// <remarks>
    ///   No-op when already closed. Safe to call from any thread.
    ///   Blocks until the worker thread has stopped.
    /// </remarks>
    procedure Close;

    /// <summary>
    ///   Indicates whether the transport is currently in
    ///   <see cref="TOBDConnectionState.csOpen"/>.
    /// </summary>
    /// <returns><c>True</c> when ready for I/O.</returns>
    function IsOpen: Boolean;

    /// <summary>
    ///   Current lifecycle state.
    /// </summary>
    /// <returns>One of <see cref="TOBDConnectionState"/>.</returns>
    function State: TOBDConnectionState;

    /// <summary>
    ///   Sends bytes to the wire.
    /// </summary>
    /// <param name="ABytes">Bytes to transmit. Empty is allowed and
    /// returns 0.</param>
    /// <returns>Number of bytes accepted for transmission. May be less
    /// than <c>Length(ABytes)</c> if the underlying buffer is full;
    /// caller should retry the remainder.</returns>
    /// <remarks>
    ///   Synchronous. Transient transport errors fire
    ///   <c>OnTransportError</c> and return 0 rather than raising.
    /// </remarks>
    /// <exception cref="EOBDNotConnected">Transport is not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer;

    /// <summary>Internal accessor for <c>OnDataReceived</c>.</summary>
    function GetOnDataReceived: TOBDBytesEvent;
    /// <summary>Internal mutator for <c>OnDataReceived</c>.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears the event.</param>
    procedure SetOnDataReceived(const AValue: TOBDBytesEvent);
    /// <summary>Internal accessor for <c>OnStateChanged</c>.</summary>
    function GetOnStateChanged: TOBDStateEvent;
    /// <summary>Internal mutator for <c>OnStateChanged</c>.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears the event.</param>
    procedure SetOnStateChanged(const AValue: TOBDStateEvent);
    /// <summary>Internal accessor for <c>OnTransportError</c>.</summary>
    function GetOnTransportError: TOBDTransportErrorEvent;
    /// <summary>Internal mutator for <c>OnTransportError</c>.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears the event.</param>
    procedure SetOnTransportError(const AValue: TOBDTransportErrorEvent);
    /// <summary>Internal accessor for <c>OnProgress</c>.</summary>
    function GetOnProgress: TOBDProgressEvent;
    /// <summary>Internal mutator for <c>OnProgress</c>.</summary>
    /// <param name="AValue">New handler. <c>nil</c> clears the event.</param>
    procedure SetOnProgress(const AValue: TOBDProgressEvent);

    /// <summary>
    ///   Fires when bytes arrive from the wire.
    /// </summary>
    /// <remarks>
    ///   Always fires on the transport's worker thread.
    ///   <c>TOBDConnection</c> re-fires its own <c>OnDataReceived</c>
    ///   on the main thread; consumers normally subscribe there.
    /// </remarks>
    property OnDataReceived: TOBDBytesEvent
      read GetOnDataReceived write SetOnDataReceived;

    /// <summary>
    ///   Fires when <c>State</c> changes.
    /// </summary>
    /// <remarks>
    ///   May fire on the worker thread (rx side detected disconnect)
    ///   or the caller thread (Open / Close called by the user).
    /// </remarks>
    property OnStateChanged: TOBDStateEvent
      read GetOnStateChanged write SetOnStateChanged;

    /// <summary>
    ///   Fires when a transient I/O error occurs.
    /// </summary>
    /// <remarks>
    ///   Configuration / programmer errors raise
    ///   <see cref="EOBDError"/> instead of firing this event.
    /// </remarks>
    property OnTransportError: TOBDTransportErrorEvent
      read GetOnTransportError write SetOnTransportError;

    /// <summary>
    ///   Fires as a long-running operation progresses.
    /// </summary>
    /// <remarks>
    ///   Fires on the transport's worker thread. The TOBDConnection
    ///   component re-fires its own <c>OnProgress</c> on the main
    ///   thread; consumers normally subscribe there.
    /// </remarks>
    property OnProgress: TOBDProgressEvent
      read GetOnProgress write SetOnProgress;
  end;

/// <summary>
///   Returns the integer baud rate for a <see cref="TOBDBaudRate"/>
///   value (e.g. 115200 for <c>br115200</c>).
/// </summary>
/// <param name="ABaud">Symbolic baud-rate enum value.</param>
/// <returns>Baud rate in bits/second.</returns>
function OBDBaudRateValue(ABaud: TOBDBaudRate): Cardinal;

implementation

function OBDBaudRateValue(ABaud: TOBDBaudRate): Cardinal;
begin
  Result := Cardinal(ABaud);
end;

{ ---- TOBDProgressStep -------------------------------------------------------- }

function TOBDProgressStep.Percent: Double;
begin
  if (BytesTotal > 0) and (BytesDone >= 0) then
  begin
    if BytesDone >= BytesTotal then
      Exit(1.0);
    Exit(BytesDone / BytesTotal);
  end;
  if (Count > 0) and (Index > 0) then
  begin
    if Index >= Count then
      Exit(1.0);
    Exit(Index / Count);
  end;
  Result := 0;
end;

class function TOBDProgressStep.MakeStep(AIndex, ACount: Cardinal;
  const AName, ADetail: string): TOBDProgressStep;
begin
  Result.Index := AIndex;
  Result.Count := ACount;
  Result.Name := AName;
  Result.Detail := ADetail;
  Result.BytesDone := 0;
  Result.BytesTotal := 0;
end;

class function TOBDProgressStep.MakeBytes(ABytesDone, ABytesTotal: Int64;
  const AName: string): TOBDProgressStep;
begin
  Result.Index := 0;
  Result.Count := 0;
  Result.Name := AName;
  Result.Detail := '';
  Result.BytesDone := ABytesDone;
  Result.BytesTotal := ABytesTotal;
end;

end.
