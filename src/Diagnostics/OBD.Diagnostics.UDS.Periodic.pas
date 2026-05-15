//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS.Periodic
//
//  TOBDUDSReadByPeriodic — non-visual component for the UDS
//  ReadDataByPeriodicIdentifier service (SID 0x2A). Asks the ECU
//  to publish one or more periodic DIDs at a chosen rate (slow,
//  medium, fast) until the host stops them. The ECU pushes
//  responses asynchronously; the component delivers each through
//  <c>OnSample</c>.
//
//  Wire format per ISO 14229-1 §10.5:
//
//    Request : 2A <transmissionMode> [<PID1> <PID2> ...]
//    Response: 6A <PID> <data...>   (one per published frame)
//
//  Transmission modes:
//    0x01 sendAtSlowRate
//    0x02 sendAtMediumRate
//    0x03 sendAtFastRate
//    0x04 stopSending
//
//  Periodic responses are unsolicited from the UDS layer's POV;
//  this component is currently a request-side helper. Receiving
//  the periodic frames is the job of the underlying TOBDProtocol's
//  OnFrame surface — the host wires that hook and routes matching
//  PIDs back through <see cref="DispatchSample"/>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §10.5 (ReadDataByPeriodicIdentifier)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS.Periodic;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol;

const
  /// <summary>Send at the ECU's "slow" periodic rate.</summary>
  UDS_PER_RATE_SLOW   = $01;
  /// <summary>Send at the ECU's "medium" periodic rate.</summary>
  UDS_PER_RATE_MEDIUM = $02;
  /// <summary>Send at the ECU's "fast" periodic rate.</summary>
  UDS_PER_RATE_FAST   = $03;
  /// <summary>Stop sending the periodic IDs listed.</summary>
  UDS_PER_RATE_STOP   = $04;

type
  /// <summary>
  ///   Fires for each periodic data identifier the ECU pushes.
  ///   Main thread.
  /// </summary>
  /// <remarks>
  ///   The host must route incoming response frames through
  ///   <see cref="TOBDUDSReadByPeriodic.DispatchSample"/> for this
  ///   event to fire — the periodic UDS surface is single-shot at
  ///   the wire layer and the host owns frame demultiplexing.
  /// </remarks>
  TOBDUDSPeriodicSampleEvent = procedure(Sender: TObject; APID: Byte;
    const AData: TBytes) of object;

  /// <summary>
  ///   UDS ReadByPeriodicIdentifier component.
  /// </summary>
  /// <remarks>
  ///   Call <see cref="Start"/> with a transmission mode + the
  ///   periodic data identifier byte(s) to subscribe; the ECU
  ///   begins pushing 6A responses. <see cref="Stop"/> halts a
  ///   running subscription. <see cref="DispatchSample"/> is the
  ///   host's path to deliver received samples back into the
  ///   component (typically called from a TOBDProtocol.OnFrame
  ///   handler).
  /// </remarks>
  TOBDUDSReadByPeriodic = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnSample: TOBDUDSPeriodicSampleEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoSend(ATransmissionMode: Byte; const APIDs: array of Byte);
    procedure FireSample(APID: Byte; const AData: TBytes);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Subscribes the listed periodic IDs at <c>ARate</c>.
    /// </summary>
    /// <param name="ARate">One of <c>UDS_PER_RATE_SLOW</c>,
    /// <c>UDS_PER_RATE_MEDIUM</c>, <c>UDS_PER_RATE_FAST</c>.</param>
    /// <param name="APIDs">Periodic-data-identifier bytes to
    /// subscribe.</param>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned, the PID list is empty, or
    ///   the rate is the stop sentinel.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure Start(ARate: Byte; const APIDs: array of Byte);

    /// <summary>
    ///   Stops every listed periodic ID (sub-function 0x04).
    /// </summary>
    /// <param name="APIDs">Periodic-data-identifier bytes to stop.
    /// Pass an empty array to stop ALL periodic subscriptions.</param>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure Stop(const APIDs: array of Byte);

    /// <summary>Non-blocking <see cref="Start"/>.</summary>
    /// <param name="ARate">Transmission mode.</param>
    /// <param name="APIDs">Periodic-ID bytes (deep-copied for the
    /// worker thread).</param>
    /// <exception cref="EOBDConfig">
    ///   Another async send is already in flight.
    /// </exception>
    procedure StartAsync(ARate: Byte; const APIDs: array of Byte);

    /// <summary>
    ///   Routes a received periodic-response frame into the
    ///   component so <c>OnSample</c> fires.
    /// </summary>
    /// <param name="APID">Periodic data identifier byte from the
    /// wire frame (the byte that follows the 0x6A SID echo).</param>
    /// <param name="AData">Sample payload bytes.</param>
    /// <remarks>
    ///   Idempotent and safe to call from any thread; firing is
    ///   marshalled to the main thread.
    /// </remarks>
    procedure DispatchSample(APID: Byte; const AData: TBytes);
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires for each routed sample. Main thread.</summary>
    property OnSample: TOBDUDSPeriodicSampleEvent read FOnSample
      write FOnSample;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSReadByPeriodic.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDUDSReadByPeriodic.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSReadByPeriodic.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSReadByPeriodic.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSReadByPeriodic.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create(
        'TOBDUDSReadByPeriodic: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSReadByPeriodic.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSReadByPeriodic.DoSend(ATransmissionMode: Byte;
  const APIDs: array of Byte);
var
  Req: TBytes;
  Resp: TOBDResponse;
  I: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSReadByPeriodic: Protocol not assigned');
  SetLength(Req, 1 + Length(APIDs));
  Req[0] := ATransmissionMode;
  for I := 0 to High(APIDs) do
    Req[1 + I] := APIDs[I];

  Resp := FProtocol.Request(UDS_SID_ReadDataByPeriodicIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDataByPeriodicIdentifier (rate 0x%.2x) negative: %s',
      [ATransmissionMode, Resp.NRCText]);
end;

procedure TOBDUDSReadByPeriodic.Start(ARate: Byte;
  const APIDs: array of Byte);
begin
  if ARate = UDS_PER_RATE_STOP then
    raise EOBDConfig.Create(
      'TOBDUDSReadByPeriodic.Start: rate cannot be UDS_PER_RATE_STOP — use Stop');
  if Length(APIDs) = 0 then
    raise EOBDConfig.Create(
      'TOBDUDSReadByPeriodic.Start: at least one periodic ID required');
  DoSend(ARate, APIDs);
end;

procedure TOBDUDSReadByPeriodic.Stop(const APIDs: array of Byte);
begin
  DoSend(UDS_PER_RATE_STOP, APIDs);
end;

procedure TOBDUDSReadByPeriodic.StartAsync(ARate: Byte;
  const APIDs: array of Byte);
var
  Self_: TOBDUDSReadByPeriodic;
  Rate: Byte;
  IDsCopy: TBytes;
  I: Integer;
begin
  GuardSingleAsync;
  Self_ := Self;
  Rate := ARate;
  SetLength(IDsCopy, Length(APIDs));
  for I := 0 to High(APIDs) do
    IDsCopy[I] := APIDs[I];
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.Start(Rate, IDsCopy);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSReadByPeriodic.DispatchSample(APID: Byte;
  const AData: TBytes);
begin
  FireSample(APID, AData);
end;

procedure TOBDUDSReadByPeriodic.FireSample(APID: Byte;
  const AData: TBytes);
var
  Self_: TOBDUDSReadByPeriodic;
  P: Byte;
  Snap: TBytes;
begin
  if not Assigned(FOnSample) then
    Exit;
  Self_ := Self;
  P := APID;
  Snap := Copy(AData, 0, Length(AData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnSample(Self_, P, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnSample) then
          Self_.FOnSample(Self_, P, Snap);
      end);
end;

procedure TOBDUDSReadByPeriodic.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSReadByPeriodic;
  Code: TOBDErrorCode;
  Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
