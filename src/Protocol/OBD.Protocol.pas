//------------------------------------------------------------------------------
//  OBD.Protocol
//
//  TOBDProtocol — non-visual component that sits between TOBDAdapter
//  and the application code (Phase 5 service-mode components, Phase 6
//  UDS/KWP/J1939, etc.). Encodes a TOBDRequest via the right codec
//  (4a foundation), pushes it to the adapter via WriteOBDCommand,
//  parses the textual hex response back into a TOBDResponse, and
//  fires events on the main thread.
//
//  Honours the dual-method + main-thread + progress rule (PLAN §3.7):
//  Send / SendAsync, Request / RequestAsync. Only one async op of the
//  same kind may be in flight per instance; cancellation flows through
//  Close on the bound adapter.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4b initial.
//------------------------------------------------------------------------------

unit OBD.Protocol;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
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
  ///   <c>Send</c> / <c>Request</c> from code (or use a Phase 5
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

    procedure SetAdapter(AValue: TOBDAdapter);
    procedure WaitForAsync;

    function EncodeRequest(const ARequest: TOBDRequest): string;
    function DecodeResponse(const ARequest: TOBDRequest;
      const ARawHex: string; AElapsedMs: Cardinal): TOBDResponse;

    procedure FireOnResponse(const AResponse: TOBDResponse);
    procedure FireOnNRC(const ARequest: TOBDRequest; ANRC: Byte;
      const AText: string);
    procedure FireOnError(ACode: TOBDErrorCode; const AMessage: string);
    procedure FireOnProgress(AIndex, ACount: Cardinal;
      const AName, ADetail: string);

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
end;

destructor TOBDProtocol.Destroy;
begin
  WaitForAsync;
  FAsyncLock.Free;
  inherited;
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
  if FAdapter <> nil then
    FAdapter.RemoveFreeNotification(Self);
  FAdapter := AValue;
  if FAdapter <> nil then
    FAdapter.FreeNotification(Self);
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
  if not Assigned(FOnResponse) then Exit;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnResponse(Self, Snapshot)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnResponse) then
          FOnResponse(Self, Snapshot);
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
  if not Assigned(FOnNRC) then Exit;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnNRC(Self, ReqCopy, NRCCopy, TextCopy)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnNRC) then
          FOnNRC(Self, ReqCopy, NRCCopy, TextCopy);
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
  if not Assigned(FOnError) then Exit;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil,
      procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(FOnError) then
          FOnError(Self, Code, Msg, Handled);
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

function TOBDProtocol.DoSend(const ARequest: TOBDRequest): TOBDResponse;
var
  Hex: string;
  AdapterResp: TOBDAdapterResponse;
  Effective: Cardinal;
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
    FireOnResponse(Result);
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
