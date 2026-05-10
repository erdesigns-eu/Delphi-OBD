//------------------------------------------------------------------------------
//  OBD.Protocol
//
//  TOBDProtocol — non-visual component that sits between TOBDAdapter
//  and the application code (service-mode components, UDS / KWP /
//  J1939 helpers, etc.). Encodes a TOBDRequest via the right codec,
//  pushes it to the adapter via WriteOBDCommand, parses the textual
//  hex response back into a TOBDResponse, and fires events on the
//  main thread.
//
//  Honours the dual-method + main-thread + progress rule:
//  Send / SendAsync, Request / RequestAsync. Only one async op of the
//  same kind may be in flight per instance; cancellation flows through
//  Close on the bound adapter.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Catalog,
  OBD.Adapter.Types,
  OBD.Adapter,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol.KWP2000,
  OBD.Protocol.ISO9141,
  OBD.Protocol.J1850,
  OBD.Protocol.J1939;

type
  /// <summary>
  ///   Selection mode for the wire protocol.
  /// </summary>
  TOBDProtocolMode = (
    /// <summary>Adapter chooses (<c>ATSP0</c>) — the most common
    /// configuration.</summary>
    pmAuto,
    /// <summary>Force the protocol to <c>Manual</c>.</summary>
    pmManual
  );

  /// <summary>
  ///   Non-visual protocol component bound to a <c>TOBDAdapter</c>.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, point <c>Adapter</c> at a configured
  ///   <c>TOBDAdapter</c>, optionally set <c>Mode := pmManual</c> and
  ///   <c>Manual := pidISO15765_4_CAN_11_500</c>, then call
  ///   <c>Send</c> / <c>Request</c> from code (or use a
  ///   service-mode component like <c>TOBDLiveData</c> that drives
  ///   the protocol behind the scenes).
  ///
  ///   <c>Send</c> takes a fully populated <c>TOBDRequest</c>;
  ///   <c>Request</c> is a one-call shortcut that builds the request
  ///   from a service ID and payload bytes.
  ///
  ///   Both ship in synchronous and asynchronous forms; async fires
  ///   <c>OnResponse</c> on success and <c>OnNRC</c> / <c>OnError</c>
  ///   on failure, all on the main thread.
  /// </remarks>
  TOBDProtocol = class(TComponent)
  strict private
    FAdapter: TOBDAdapter;
    FMode: TOBDProtocolMode;
    FManual: TOBDProtocolID;
    FApplication: TOBDApplicationProtocol;
    FDefaultTimeoutMs: Cardinal;

    // async
    FAsyncLock: TCriticalSection;
    FAsyncThread: TThread;

    // events
    FOnFrame: TOBDProtocolFrameEvent;
    FOnResponse: TOBDProtocolResponseEvent;
    FOnNRC: TOBDProtocolNRCEvent;
    FOnError: TOBDConnectionErrorEvent;
    FOnProgress: TOBDProgressEvent;

    // Multi-cast listener registry. Hosts wire their own OnXxx
    // handlers AND service-component glue (Recorder, Replayer,
    // bus loggers) registers as listeners — neither path clobbers
    // the other.
    FListeners:       TDictionary<Integer, TOBDProtocolListener>;
    FListenerLock:    TCriticalSection;
    FNextListenerId:  Integer;

    procedure SetAdapter(AValue: TOBDAdapter);
    procedure WaitForAsync;

    procedure DispatchFrameListeners(const AFrame: TOBDFrame);
    procedure DispatchResponseListeners(const AResponse: TOBDResponse);
    procedure DispatchNRCListeners(const ARequest: TOBDRequest;
      ANRC: Byte; const AText: string);
    procedure DispatchErrorListeners(ACode: TOBDErrorCode;
      const AMessage: string);
    function  SnapshotListeners: TArray<TOBDProtocolListener>;

    function EncodeRequest(const ARequest: TOBDRequest): string;
    function DecodeResponse(const ARequest: TOBDRequest;
      const ARawHex: string; AElapsedMs: Cardinal): TOBDResponse;

    procedure FireOnResponse(const AResponse: TOBDResponse);
    procedure FireOnNRC(const ARequest: TOBDRequest; ANRC: Byte;
      const AText: string);
    procedure FireOnError(ACode: TOBDErrorCode; const AMessage: string);
    procedure FireOnProgress(AIndex, ACount: Cardinal;
      const AName, ADetail: string);
    procedure FireOnFrame(const AFrame: TOBDFrame);
    procedure DispatchFrames(const ARawHex: string);

    function DoSend(const ARequest: TOBDRequest): TOBDResponse;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Creates the component.</summary>
    /// <param name="AOwner">Standard owner; may be <c>nil</c>.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Joins any in-flight async work.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Encodes a request and sends it through the bound adapter,
    ///   blocking until the response arrives.
    /// </summary>
    /// <param name="ARequest">Fully populated request.</param>
    /// <returns>Decoded response.</returns>
    /// <exception cref="EOBDNotConnected">Adapter not bound or
    /// connection inactive.</exception>
    /// <exception cref="EOBDProtocolErr">Encode or decode failed.</exception>
    /// <exception cref="EOBDAdapter">Underlying adapter
    /// exchange failed.</exception>
    function Send(const ARequest: TOBDRequest): TOBDResponse;

    /// <summary>Non-blocking <see cref="Send"/>. Result fires
    /// <c>OnResponse</c> on success, <c>OnNRC</c> on negative,
    /// <c>OnError</c> on transient failure (main thread).</summary>
    /// <param name="ARequest">Fully populated request.</param>
    /// <exception cref="EOBDConfig">Already in flight.</exception>
    procedure SendAsync(const ARequest: TOBDRequest);

    /// <summary>
    ///   Convenience: build and send a request from a service ID and
    ///   payload bytes. Equivalent to <c>Send(MakeRequest(...))</c>.
    /// </summary>
    /// <param name="AServiceID">Application protocol SID
    /// (e.g. 0x09 for OBD-II Mode 09, 0x22 for UDS
    /// ReadDataByIdentifier).</param>
    /// <param name="AData">Payload bytes after the SID.</param>
    /// <param name="ATimeoutMs">Timeout in milliseconds.
    /// <c>0</c> uses <c>DefaultTimeoutMs</c>.</param>
    /// <returns>Decoded response.</returns>
    /// <exception cref="EOBDNotConnected">Adapter not bound or
    /// connection inactive.</exception>
    function Request(AServiceID: Byte; const AData: TBytes;
      ATimeoutMs: Cardinal = 0): TOBDResponse;

    /// <summary>Non-blocking <see cref="Request"/>.</summary>
    /// <param name="AServiceID">Application protocol SID.</param>
    /// <param name="AData">Payload after the SID.</param>
    /// <param name="ATimeoutMs">Timeout. <c>0</c> uses default.</param>
    /// <exception cref="EOBDConfig">Already in flight.</exception>
    procedure RequestAsync(AServiceID: Byte; const AData: TBytes;
      ATimeoutMs: Cardinal = 0);

    /// <summary>Cancels any in-flight async op, joining its worker.</summary>
    procedure Close;

    /// <summary>Registers a multi-cast listener for the
    /// frame / response / NRC / error stream. Unlike the
    /// single-cast <c>OnFrame</c>..<c>OnError</c> properties, this
    /// path supports any number of registered observers — the
    /// host can keep its own handlers and still drop in a
    /// <c>TOBDRecorder</c> or custom bus logger. Returns an
    /// integer tag the caller passes to
    /// <see cref="RemoveListener"/> later. Thread-safe.</summary>
    function AddListener(
      const AListener: TOBDProtocolListener): Integer;
    /// <summary>Removes the listener registered under
    /// <paramref name="AListenerId"/>. Silently ignored when the
    /// tag isn't found (already removed / never registered).
    /// Thread-safe.</summary>
    procedure RemoveListener(AListenerId: Integer);
  published
    /// <summary>Bound adapter. Required.</summary>
    property Adapter: TOBDAdapter read FAdapter write SetAdapter;
    /// <summary>Auto / manual selector.</summary>
    property Mode: TOBDProtocolMode read FMode write FMode default pmAuto;
    /// <summary>Manual protocol when <c>Mode = pmManual</c>.</summary>
    property Manual: TOBDProtocolID read FManual write FManual
      default pidAuto;
    /// <summary>Application-protocol shape used by the codec
    /// (UDS / KWP / OBD-II / J1939 / WWH-OBD / DoIP).</summary>
    property Application: TOBDApplicationProtocol read FApplication
      write FApplication default apOBD2;
    /// <summary>Default per-request timeout when caller passes
    /// <c>0</c>.</summary>
    property DefaultTimeoutMs: Cardinal read FDefaultTimeoutMs
      write FDefaultTimeoutMs default 5000;

    /// <summary>Fires for every successful (positive) response on
    /// the main thread.</summary>
    property OnResponse: TOBDProtocolResponseEvent read FOnResponse
      write FOnResponse;
    /// <summary>Fires for negative responses (UDS / KWP NRC) on the
    /// main thread.</summary>
    property OnNRC: TOBDProtocolNRCEvent read FOnNRC write FOnNRC;
    /// <summary>Fires for inbound frames before decoding (main
    /// thread).</summary>
    property OnFrame: TOBDProtocolFrameEvent read FOnFrame write FOnFrame;
    /// <summary>Fires for transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
    /// <summary>Fires per phase during async operations (main
    /// thread).</summary>
    property OnProgress: TOBDProgressEvent read FOnProgress write FOnProgress;
  end;

/// <summary>
///   Helper that builds a fully populated <c>TOBDRequest</c> from a
///   service ID + payload bytes.
/// </summary>
/// <param name="AProtocol">Application protocol shape.</param>
/// <param name="AServiceID">Service / SID byte.</param>
/// <param name="AData">Payload bytes after the SID.</param>
/// <param name="ATimeoutMs">Optional per-request timeout (0 = use
/// component default).</param>
/// <returns>Fully populated request record.</returns>
function MakeRequest(AProtocol: TOBDApplicationProtocol;
  AServiceID: Byte; const AData: TBytes;
  ATimeoutMs: Cardinal = 0): TOBDRequest;

implementation

function MakeRequest(AProtocol: TOBDApplicationProtocol;
  AServiceID: Byte; const AData: TBytes;
  ATimeoutMs: Cardinal): TOBDRequest;
begin
  Result := MakeOBDRequest;
  Result.Protocol := AProtocol;
  Result.ServiceID := AServiceID;
  Result.Data := Copy(AData);
  Result.TimeoutMs := ATimeoutMs;
end;

{ ---- TOBDProtocol ------------------------------------------------------------ }

constructor TOBDProtocol.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMode := pmAuto;
  FManual := pidAuto;
  FApplication := apOBD2;
  FDefaultTimeoutMs := 5000;
  FAsyncLock := TCriticalSection.Create;
  FListenerLock := TCriticalSection.Create;
  FListeners := TDictionary<Integer, TOBDProtocolListener>.Create;
  FNextListenerId := 0;
end;

destructor TOBDProtocol.Destroy;
begin
  WaitForAsync;
  // inherited Destroy fires opRemove notifications on every
  // component that registered FreeNotification — including
  // TOBDRecorder, which will call back into RemoveListener.
  // Run that BEFORE freeing the listener registry so the
  // callback finds a valid lock + dictionary.
  inherited;
  FListeners.Free;
  FListenerLock.Free;
  FAsyncLock.Free;
end;

procedure TOBDProtocol.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FAdapter) then
    FAdapter := nil;
end;

procedure TOBDProtocol.SetAdapter(AValue: TOBDAdapter);
begin
  if FAdapter = AValue then Exit;
  // Drain any in-flight async worker before swapping the adapter
  // — the worker reads FAdapter without locking, so swapping it
  // mid-flight would race. WaitForAsync is a no-op when no
  // worker is active, so the common (design-time / setup) path
  // pays nothing.
  WaitForAsync;
  if FAdapter <> nil then
    FAdapter.RemoveFreeNotification(Self);
  FAdapter := AValue;
  if FAdapter <> nil then
    FAdapter.FreeNotification(Self);
end;

function TOBDProtocol.AddListener(
  const AListener: TOBDProtocolListener): Integer;
begin
  FListenerLock.Enter;
  try
    Inc(FNextListenerId);
    Result := FNextListenerId;
    FListeners.Add(Result, AListener);
  finally
    FListenerLock.Leave;
  end;
end;

procedure TOBDProtocol.RemoveListener(AListenerId: Integer);
begin
  FListenerLock.Enter;
  try
    FListeners.Remove(AListenerId);
  finally
    FListenerLock.Leave;
  end;
end;

function TOBDProtocol.SnapshotListeners: TArray<TOBDProtocolListener>;
var
  L: TOBDProtocolListener;
  I: Integer;
begin
  // Snapshot under the lock so dispatch can iterate without
  // holding it (handlers may call back into Add/RemoveListener,
  // which would deadlock if dispatch held the lock).
  FListenerLock.Enter;
  try
    SetLength(Result, FListeners.Count);
    I := 0;
    for L in FListeners.Values do
    begin
      Result[I] := L;
      Inc(I);
    end;
  finally
    FListenerLock.Leave;
  end;
end;

procedure TOBDProtocol.DispatchFrameListeners(const AFrame: TOBDFrame);
var
  Snap: TArray<TOBDProtocolListener>;
  L: TOBDProtocolListener;
begin
  Snap := SnapshotListeners;
  for L in Snap do
    if Assigned(L.OnFrame) then
      try L.OnFrame(Self, AFrame); except end;
end;

procedure TOBDProtocol.DispatchResponseListeners(
  const AResponse: TOBDResponse);
var
  Snap: TArray<TOBDProtocolListener>;
  L: TOBDProtocolListener;
begin
  Snap := SnapshotListeners;
  for L in Snap do
    if Assigned(L.OnResponse) then
      try L.OnResponse(Self, AResponse); except end;
end;

procedure TOBDProtocol.DispatchNRCListeners(const ARequest: TOBDRequest;
  ANRC: Byte; const AText: string);
var
  Snap: TArray<TOBDProtocolListener>;
  L: TOBDProtocolListener;
begin
  Snap := SnapshotListeners;
  for L in Snap do
    if Assigned(L.OnNRC) then
      try L.OnNRC(Self, ARequest, ANRC, AText); except end;
end;

procedure TOBDProtocol.DispatchErrorListeners(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Snap: TArray<TOBDProtocolListener>;
  L: TOBDProtocolListener;
begin
  Snap := SnapshotListeners;
  for L in Snap do
    if Assigned(L.OnError) then
      try L.OnError(Self, ACode, AMessage); except end;
end;

procedure TOBDProtocol.WaitForAsync;
var
  Worker: TThread;
begin
  FAsyncLock.Enter;
  try
    Worker := FAsyncThread;
    FAsyncThread := nil;
  finally
    FAsyncLock.Leave;
  end;
  if Worker = nil then Exit;
  Worker.WaitFor;
  Worker.Free;
end;

procedure TOBDProtocol.Close;
begin
  WaitForAsync;
end;

function TOBDProtocol.EncodeRequest(const ARequest: TOBDRequest): string;
begin
  case ARequest.Protocol of
    apOBD2, apJ1939, apWWHOBD, apDoIP, apUDS:
      Result := TOBDUDSCodec.Encode(ARequest);
    apKWP2000:
      Result := TOBDKWPCodec.Encode(ARequest);
  else
    Result := TOBDUDSCodec.Encode(ARequest);
  end;
end;

function TOBDProtocol.DecodeResponse(const ARequest: TOBDRequest;
  const ARawHex: string; AElapsedMs: Cardinal): TOBDResponse;
begin
  case ARequest.Protocol of
    apKWP2000:
      TOBDKWPCodec.Decode(ARequest, ARawHex, Result);
  else
    TOBDUDSCodec.Decode(ARequest, ARawHex, Result);
  end;
  Result.Elapsed := AElapsedMs;
end;

procedure TOBDProtocol.FireOnResponse(const AResponse: TOBDResponse);
var
  Snapshot: TOBDResponse;
begin
  Snapshot := AResponse;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnResponse) then FOnResponse(Self, Snapshot);
    DispatchResponseListeners(Snapshot);
  end
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnResponse) then
          FOnResponse(Self, Snapshot);
        DispatchResponseListeners(Snapshot);
      end);
end;

procedure TOBDProtocol.FireOnNRC(const ARequest: TOBDRequest;
  ANRC: Byte; const AText: string);
var
  ReqCopy: TOBDRequest;
  TextCopy: string;
  NRCCopy: Byte;
begin
  ReqCopy := ARequest;
  TextCopy := AText;
  NRCCopy := ANRC;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnNRC) then FOnNRC(Self, ReqCopy, NRCCopy, TextCopy);
    DispatchNRCListeners(ReqCopy, NRCCopy, TextCopy);
  end
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnNRC) then
          FOnNRC(Self, ReqCopy, NRCCopy, TextCopy);
        DispatchNRCListeners(ReqCopy, NRCCopy, TextCopy);
      end);
end;

procedure TOBDProtocol.FireOnError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Code: TOBDErrorCode;
  Msg: string;
begin
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnError) then
    begin
      var Handled: Boolean;
      Handled := False;
      FOnError(Self, Code, Msg, Handled);
    end;
    DispatchErrorListeners(Code, Msg);
  end
  else
    TThread.Queue(nil,
      procedure
      var Handled: Boolean;
      begin
        if Assigned(FOnError) then
        begin
          Handled := False;
          FOnError(Self, Code, Msg, Handled);
        end;
        DispatchErrorListeners(Code, Msg);
      end);
end;

procedure TOBDProtocol.FireOnProgress(AIndex, ACount: Cardinal;
  const AName, ADetail: string);
var
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then Exit;
  Step := TOBDProgressStep.MakeStep(AIndex, ACount, AName, ADetail);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self, Step)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnProgress) then
          FOnProgress(Self, Step);
      end);
end;

procedure TOBDProtocol.FireOnFrame(const AFrame: TOBDFrame);
var
  Snapshot: TOBDFrame;
begin
  Snapshot := AFrame;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(FOnFrame) then FOnFrame(Self, Snapshot);
    DispatchFrameListeners(Snapshot);
  end
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnFrame) then
          FOnFrame(Self, Snapshot);
        DispatchFrameListeners(Snapshot);
      end);
end;

procedure TOBDProtocol.DispatchFrames(const ARawHex: string);
var
  Lines: TArray<string>;
  Trimmed, IdToken, Rest: string;
  I, SpaceAt: Integer;
  Frame: TOBDFrame;
  IdValue: Int64;
begin
  // No early-exit on FOnFrame here — multi-cast listeners
  // (TOBDRecorder, custom bus loggers) need frames even when
  // the host doesn't wire a single-cast OnFrame.
  Lines := SplitString(ARawHex, #13#10);
  if Length(Lines) = 0 then
    Lines := SplitString(ARawHex, #13);
  if Length(Lines) = 0 then
    Lines := SplitString(ARawHex, #10);
  if Length(Lines) = 0 then
  begin
    SetLength(Lines, 1);
    Lines[0] := ARawHex;
  end;

  for I := 0 to High(Lines) do
  begin
    Trimmed := Trim(Lines[I]);
    if Trimmed = '' then Continue;

    Frame := Default(TOBDFrame);
    Frame.Timestamp := Now;
    Frame.Kind := fkRaw;
    Frame.Id := 0;
    Frame.IsExtendedId := False;

    // ELM327 with headers on prefixes each frame with the CAN ID
    // (3 hex digits for 11-bit, 8 for 29-bit) followed by a space
    // and the data bytes. Detect by an isolated leading hex token.
    SpaceAt := Pos(' ', Trimmed);
    if (SpaceAt > 0) and (SpaceAt in [4, 9]) then
    begin
      IdToken := Copy(Trimmed, 1, SpaceAt - 1);
      if TryStrToInt64('$' + IdToken, IdValue) then
      begin
        Frame.Id := Cardinal(IdValue);
        Frame.IsExtendedId := SpaceAt = 9;
        Rest := Copy(Trimmed, SpaceAt + 1, MaxInt);
        Frame.Payload := HexToBytes(Rest);
      end
      else
        Frame.Payload := HexToBytes(Trimmed);
    end
    else
      Frame.Payload := HexToBytes(Trimmed);

    FireOnFrame(Frame);
  end;
end;

function TOBDProtocol.DoSend(const ARequest: TOBDRequest): TOBDResponse;
var
  Hex: string;
  AdapterResp: TOBDAdapterResponse;
  Effective: Cardinal;
  ExpectedSID: Byte;
begin
  if (FAdapter = nil) or (FAdapter.Connection = nil) or
     not FAdapter.Connection.Active then
    raise EOBDNotConnected.Create('TOBDProtocol: Adapter is not active');

  Effective := ARequest.TimeoutMs;
  if Effective = 0 then Effective := FDefaultTimeoutMs;

  FireOnProgress(1, 3, 'Encoding', '');
  Hex := EncodeRequest(ARequest);

  FireOnProgress(2, 3, 'Adapter exchange', Hex);
  AdapterResp := FAdapter.WriteOBDCommand(Hex, Effective);

  // Frame-level fan-out (one TOBDFrame per response line). Done
  // before decoding so subscribers see frames in arrival order
  // even if decoding raises.
  DispatchFrames(AdapterResp.Raw);

  FireOnProgress(3, 3, 'Decoding', '');
  if AdapterResp.IsError then
  begin
    Result := MakeOBDResponse;
    Result.Request := ARequest;
    Result.Elapsed := AdapterResp.Elapsed;
    FireOnError(oeAdapterFault,
      Format('Adapter rejected request: %s', [AdapterResp.ErrorKeyword]));
    Exit;
  end;
  Result := DecodeResponse(ARequest, AdapterResp.Raw, AdapterResp.Elapsed);

  if Result.IsNegative then
    FireOnNRC(ARequest, Result.NRC, Result.NRCText)
  else
  begin
    // SID-mismatch surfaces as a transient OnError(oeUnexpectedFrame).
    // Some adapters silently strip the SID echo; only flag when we
    // got a non-zero SID that doesn't match the expected positive
    // response.
    case ARequest.Protocol of
      apKWP2000:
        ExpectedSID := TOBDKWPCodec.ExpectedPositiveResponse(ARequest.ServiceID);
    else
      ExpectedSID := TOBDUDSCodec.ExpectedPositiveResponse(ARequest.ServiceID);
    end;
    if (Result.ServiceID <> 0) and (Result.ServiceID <> ExpectedSID) then
      FireOnError(oeUnexpectedFrame,
        Format('Unexpected response SID 0x%2.2X (expected 0x%2.2X for request 0x%2.2X)',
          [Result.ServiceID, ExpectedSID, ARequest.ServiceID]));
    FireOnResponse(Result);
  end;
end;

function TOBDProtocol.Send(const ARequest: TOBDRequest): TOBDResponse;
begin
  WaitForAsync;
  Result := DoSend(ARequest);
end;

procedure TOBDProtocol.SendAsync(const ARequest: TOBDRequest);
var
  Self_: TOBDProtocol;
  RequestCopy: TOBDRequest;
begin
  if (FAdapter = nil) or (FAdapter.Connection = nil) or
     not FAdapter.Connection.Active then
    raise EOBDNotConnected.Create('TOBDProtocol.SendAsync: Adapter inactive');
  FAsyncLock.Enter;
  try
    if FAsyncThread <> nil then
      raise EOBDConfig.Create('Protocol operation already in flight');
  finally
    FAsyncLock.Leave;
  end;

  Self_ := Self;
  RequestCopy := ARequest;

  FAsyncThread := TThread.CreateAnonymousThread(
    procedure
    var
      Worker: TThread;
    begin
      try
        try
          Self_.DoSend(RequestCopy);
        except
          on E: Exception do
            Self_.FireOnError(oeIO, E.Message);
        end;
      finally
        TThread.Queue(nil,
          procedure
          begin
            Self_.FAsyncLock.Enter;
            try
              Worker := Self_.FAsyncThread;
              Self_.FAsyncThread := nil;
            finally
              Self_.FAsyncLock.Leave;
            end;
            if Worker <> nil then
            begin
              Worker.WaitFor;
              Worker.Free;
            end;
          end);
      end;
    end);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.Start;
end;

function TOBDProtocol.Request(AServiceID: Byte; const AData: TBytes;
  ATimeoutMs: Cardinal): TOBDResponse;
begin
  Result := Send(MakeRequest(FApplication, AServiceID, AData, ATimeoutMs));
end;

procedure TOBDProtocol.RequestAsync(AServiceID: Byte; const AData: TBytes;
  ATimeoutMs: Cardinal);
begin
  SendAsync(MakeRequest(FApplication, AServiceID, AData, ATimeoutMs));
end;

end.
