// ------------------------------------------------------------------------------
// ERD.Diagnostics.KWP.IOControl
//
// TOBDKWPIOControl — non-visual component for the KWP2000 I/O
// control services:
//
// Service 0x2F — InputOutputControlByLocalIdentifier
// Service 0x30 — InputOutputControlByCommonIdentifier
//
// Forces an I/O identifier into a host-driven state (returnControl,
// reportControlState, shortTermAdjustment, longTermAdjustment, …).
// Destructive — the component ships with AutoExecute = False and a
// cancellable OnBeforeSend hook, matching the rest of the
// diagnostics safety contract.
//
// Wire format per ISO 14230-3:1999 §6.10:
//
// ByLocal  request : 2F <LocalId> <IO-Param> [<state>] [<mask>]
// ByLocal  response: 6F <LocalId> <IO-Param> [<state>]
//
// ByCommon request : 30 <CommonId-hi> <CommonId-lo>
// <IO-Param> [<state>] [<mask>]
// ByCommon response: 70 <CommonId-hi> <CommonId-lo>
// <IO-Param> [<state>]
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// References  :
// - ISO 14230-3:1999 §6.10 (InputOutputControl)
//
// History     :
// 2026-05-11  ERD  Initial implementation.
// 2026-10-08  ERD  Add owned async operations and cancellation cleanup.
// ------------------------------------------------------------------------------

unit ERD.Diagnostics.KWP.IOControl;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Connection,
{$IFDEF FPC}ERD.Compat.Functions, {$ENDIF}
  ERD.Connection.Types,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Protocol.KWP2000,
  ERD.Protocol;

const
  /// <summary>Return the I/O identifier to ECU control.</summary>
  KWP_IOCTL_RETURN_CONTROL_TO_ECU = $00;
  /// <summary>Report the current control state.</summary>
  KWP_IOCTL_REPORT_CONTROL_STATE = $01;
  /// <summary>Apply a host-supplied short-term adjustment.</summary>
  KWP_IOCTL_SHORT_TERM_ADJUSTMENT = $07;
  /// <summary>Apply a host-supplied long-term adjustment.</summary>
  KWP_IOCTL_LONG_TERM_ADJUSTMENT = $08;

type
  /// <summary>
  /// Which identifier dialect this control call targets.
  /// </summary>
  TOBDKWPIOIdKind = (
    /// <summary>Service 0x2F — LocalIdentifier (1 byte).</summary>
    ikLocal,
    /// <summary>Service 0x30 — CommonIdentifier (2 bytes).</summary>
    ikCommon);

  /// <summary>
  /// Pre-send confirmation hook. Main thread.
  /// </summary>
  TOBDKWPIOControlBeforeEvent = procedure(Sender: TObject;
    AKind: TOBDKWPIOIdKind; AID: Word; AControlParam: Byte;
    const AState: TBytes; var ACancel: Boolean) of object;

  /// <summary>Fires after a successful response. Main thread.</summary>
  TOBDKWPIOControlResultEvent = procedure(Sender: TObject;
    AKind: TOBDKWPIOIdKind; AID: Word; AControlParam: Byte;
    const AResponseState: TBytes) of object;

  /// <summary>
  /// KWP2000 InputOutputControl component.
  /// </summary>
  /// <remarks>
  /// Drop on a form, assign <c>Protocol</c>, set
  /// <c>AutoExecute := True</c> after operator consent. Use
  /// <see cref="SendLocal"/> for 1-byte LocalIdentifiers and
  /// <see cref="SendCommon"/> for 2-byte CommonIdentifiers.
  /// </remarks>
  TOBDKWPIOControl = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FWorker: TThread;
    FCancelled: Integer;
    FOnProgress: TOBDProgressEvent;
    FOnBeforeSend: TOBDKWPIOControlBeforeEvent;
    FOnResult: TOBDKWPIOControlResultEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure FireProgress(AIndex: Cardinal; const AName: string);
    function IsAsyncCancelled: Boolean;
    procedure BeginAsync(const AAction: TProc);
    procedure FinishAsync;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoSend(AKind: TOBDKWPIOIdKind; AID: Word; AControlParam: Byte;
      const AState: TBytes; const AControlMask: TBytes): TBytes;
    function FireBeforeSend(AKind: TOBDKWPIOIdKind; AID: Word;
      AControlParam: Byte; const AState: TBytes): Boolean;
    procedure FireResult(AKind: TOBDKWPIOIdKind; AID: Word; AControlParam: Byte;
      const AResponseState: TBytes);
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
    /// Service 0x2F — InputOutputControlByLocalIdentifier.
    /// </summary>
    /// <param name="ALocalID">1-byte local identifier.</param>
    /// <param name="AControlParam">I/O control parameter (one of
    /// <c>KWP_IOCTL_*</c>).</param>
    /// <param name="AState">Optional state vector.</param>
    /// <param name="AControlMask">Optional control mask.</param>
    /// <returns>Response state bytes (excluding the LocalID + param
    /// echo).</returns>
    /// <exception cref="EOBDConfig">
    /// <c>Protocol</c> is not assigned, <c>AutoExecute</c> is
    /// <c>False</c>, or <c>OnBeforeSend</c> cancelled.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    /// ECU returned a negative or short response, or the ID /
    /// param echo did not match.
    /// </exception>
    function SendLocal(ALocalID: Byte; AControlParam: Byte;
      const AState: TBytes = nil; const AControlMask: TBytes = nil): TBytes;

    /// <summary>
    /// Service 0x30 — InputOutputControlByCommonIdentifier.
    /// </summary>
    /// <param name="ACommonID">2-byte common identifier.</param>
    /// <param name="AControlParam">I/O control parameter.</param>
    /// <param name="AState">Optional state vector.</param>
    /// <param name="AControlMask">Optional control mask.</param>
    /// <returns>Response state bytes (excluding the ID + param
    /// echo).</returns>
    /// <exception cref="EOBDConfig">
    /// <c>Protocol</c> is not assigned, <c>AutoExecute</c> is
    /// <c>False</c>, or <c>OnBeforeSend</c> cancelled.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    /// ECU returned a negative or short response, or the ID /
    /// param echo did not match.
    /// </exception>
    function SendCommon(ACommonID: Word; AControlParam: Byte;
      const AState: TBytes = nil; const AControlMask: TBytes = nil): TBytes;
    /// <summary>Send a local I/O request asynchronously; OnResult/OnError run
    /// on the main thread. Inputs are copied before returning.</summary>
    /// <param name="ALocalID">Local identifier.</param>
    /// <param name="AControlParam">Control parameter.</param>
    /// <param name="AState">Optional state vector.</param>
    /// <param name="AControlMask">Optional control mask.</param>
    /// <exception cref="EOBDConfig">Another async call is active or caller is
    /// not on the main thread. Request errors are delivered by OnError.</exception>
    procedure SendLocalAsync(ALocalID: Byte; AControlParam: Byte;
      const AState: TBytes = nil; const AControlMask: TBytes = nil);
    /// <summary>Asynchronous common-identifier counterpart of SendCommon.</summary>
    /// <param name="ACommonID">Common identifier.</param>
    /// <param name="AControlParam">Control parameter.</param>
    /// <param name="AState">Optional state vector.</param>
    /// <param name="AControlMask">Optional control mask.</param>
    /// <exception cref="EOBDConfig">Another async call is active or caller is
    /// not on the main thread. Request errors are delivered by OnError.</exception>
    procedure SendCommonAsync(ACommonID: Word; AControlParam: Byte;
      const AState: TBytes = nil; const AControlMask: TBytes = nil);
    /// <summary>Cancel delivery and wait for the owned worker. Main thread only.
    /// An already transmitted ECU request cannot be undone.</summary>
    /// <exception cref="EOBDConfig">Called outside the main thread.</exception>
    procedure CancelAsync;
  published
    /// <summary>Request and response phases on the main thread.</summary>
    property OnProgress: TOBDProgressEvent read FOnProgress write FOnProgress;
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;

    /// <summary>Pre-send confirmation hook. Main thread.</summary>
    property OnBeforeSend: TOBDKWPIOControlBeforeEvent read FOnBeforeSend
      write FOnBeforeSend;
    /// <summary>Fires on success. Main thread.</summary>
    property OnResult: TOBDKWPIOControlResultEvent read FOnResult
      write FOnResult;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDKWPIOControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDKWPIOControl.Destroy;
begin
  CancelAsync;
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDKWPIOControl.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FAsyncInFlight then
    raise EOBDConfig.Create('Cannot replace Protocol during an async request');
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDKWPIOControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
  begin
    CancelAsync;
    FProtocol := nil;
  end;
end;

procedure TOBDKWPIOControl.FireProgress(AIndex: Cardinal; const AName: string);
var
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then
    Exit;
  Step := TOBDProgressStep.MakeStep(AIndex, 2, AName, '');
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self, Step)
  else
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        if not IsAsyncCancelled and Assigned(FOnProgress) then
          FOnProgress(Self, Step);
      end);
end;

function TOBDKWPIOControl.IsAsyncCancelled: Boolean;
begin
  Result := TInterlocked.CompareExchange(FCancelled, 0, 0) <> 0;
end;

procedure TOBDKWPIOControl.BeginAsync(const AAction: TProc);
var
  Action: TProc;
begin
  Action := AAction;
  GuardSingleAsync;
  try
    FWorker := TThread.CreateAnonymousThread(
      procedure
      begin
        try
          if not IsAsyncCancelled then
            Action();
        except
          on E: Exception do
            if not IsAsyncCancelled then
              FireError(oeIO, E.Message);
        end;
        TThread.ForceQueue(TThread.CurrentThread, FinishAsync);
      end);
    FWorker.FreeOnTerminate := False;
    FWorker.Start;
  except
    FreeAndNil(FWorker);
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDKWPIOControl.FinishAsync;
begin
  if IsAsyncCancelled then
    Exit;
  FWorker.WaitFor;
  TThread.RemoveQueuedEvents(FWorker);
  FreeAndNil(FWorker);
  ReleaseAsync;
end;

procedure TOBDKWPIOControl.CancelAsync;
begin
  if FWorker = nil then
    Exit;
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create
      ('TOBDKWPIOControl: async lifecycle requires main thread');
  TInterlocked.Exchange(FCancelled, 1);
  FWorker.Terminate;
  // WaitFor pumps Synchronize on the main thread. Consent checks observe
  // termination before invoking user code, so teardown cannot send a request.
  FWorker.WaitFor;
  TThread.RemoveQueuedEvents(FWorker);
  FreeAndNil(FWorker);
  ReleaseAsync;
end;

procedure TOBDKWPIOControl.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create
      ('TOBDKWPIOControl: async start requires main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDKWPIOControl: async already in flight');
    TInterlocked.Exchange(FCancelled, 0);
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDKWPIOControl.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDKWPIOControl.DoSend(AKind: TOBDKWPIOIdKind; AID: Word;
AControlParam: Byte; const AState: TBytes; const AControlMask: TBytes): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
  Off: Integer;
  IDBytes: Integer;
  SID: Byte;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWPIOControl: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create
      ('TOBDKWPIOControl: AutoExecute is False — set it before sending');
  if not FireBeforeSend(AKind, AID, AControlParam, AState) then
    raise EOBDConfig.Create('TOBDKWPIOControl: cancelled by OnBeforeSend');

  case AKind of
    ikLocal:
      begin
        SID := KWP_SID_InputOutputControlByLocalID;
        IDBytes := 1;
      end;
    ikCommon:
      begin
        SID := KWP_SID_InputOutputControlByCommonID;
        IDBytes := 2;
      end;
  else
    raise EOBDConfig.Create('TOBDKWPIOControl: unknown identifier kind');
  end;

  SetLength(Req, IDBytes + 1 + Length(AState) + Length(AControlMask));
  if AKind = ikLocal then
    Req[0] := Byte(AID and $FF)
  else
  begin
    Req[0] := Byte((AID shr 8) and $FF);
    Req[1] := Byte(AID and $FF);
  end;
  Req[IDBytes] := AControlParam;
  Off := IDBytes + 1;
  if Length(AState) > 0 then
  begin
    Move(AState[0], Req[Off], Length(AState));
    Inc(Off, Length(AState));
  end;
  if Length(AControlMask) > 0 then
    Move(AControlMask[0], Req[Off], Length(AControlMask));

  if (FWorker <> nil) and IsAsyncCancelled then
    raise EOBDConfig.Create('TOBDKWPIOControl: async cancelled');
  FireProgress(1, 'Request');
  Resp := FProtocol.Request(SID, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt
      ('KWP IOControl (SID 0x%.2x, ID 0x%.4x) negative: %s',
      [SID, AID, Resp.NRCText]);
  if Length(Resp.Data) < IDBytes + 1 then
    raise EOBDProtocolErr.CreateFmt
      ('KWP IOControl (SID 0x%.2x): response too short', [SID]);

  // Verify echo bytes.
  if AKind = ikLocal then
  begin
    if Resp.Data[0] <> Byte(AID and $FF) then
      raise EOBDProtocolErr.CreateFmt
        ('KWP IOControl echo mismatch on LocalID 0x%.2x', [AID]);
  end
  else
  begin
    if ((Word(Resp.Data[0]) shl 8) or Word(Resp.Data[1])) <> AID then
      raise EOBDProtocolErr.CreateFmt
        ('KWP IOControl echo mismatch on CommonID 0x%.4x', [AID]);
  end;
  if Resp.Data[IDBytes] <> AControlParam then
    raise EOBDProtocolErr.CreateFmt
      ('KWP IOControl param echo mismatch: requested 0x%.2x, got 0x%.2x',
      [AControlParam, Resp.Data[IDBytes]]);

  if Length(Resp.Data) > IDBytes + 1 then
    Result := Copy(Resp.Data, IDBytes + 1, Length(Resp.Data) - IDBytes - 1)
  else
    SetLength(Result, 0);
  FireProgress(2, 'Response');
end;

function TOBDKWPIOControl.SendLocal(ALocalID: Byte; AControlParam: Byte;
const AState: TBytes; const AControlMask: TBytes): TBytes;
begin
  Result := DoSend(ikLocal, ALocalID, AControlParam, AState, AControlMask);
  FireResult(ikLocal, ALocalID, AControlParam, Result);
end;

function TOBDKWPIOControl.SendCommon(ACommonID: Word; AControlParam: Byte;
const AState: TBytes; const AControlMask: TBytes): TBytes;
begin
  Result := DoSend(ikCommon, ACommonID, AControlParam, AState, AControlMask);
  FireResult(ikCommon, ACommonID, AControlParam, Result);
end;

procedure TOBDKWPIOControl.SendLocalAsync(ALocalID: Byte; AControlParam: Byte;
const AState: TBytes; const AControlMask: TBytes);
var
  State, Mask: TBytes;
begin
  State := Copy(AState, 0, Length(AState));
  Mask := Copy(AControlMask, 0, Length(AControlMask));
  BeginAsync(
    procedure
    begin
      SendLocal(ALocalID, AControlParam, State, Mask);
    end);
end;

procedure TOBDKWPIOControl.SendCommonAsync(ACommonID: Word; AControlParam: Byte;
const AState: TBytes; const AControlMask: TBytes);
var
  State, Mask: TBytes;
begin
  State := Copy(AState, 0, Length(AState));
  Mask := Copy(AControlMask, 0, Length(AControlMask));
  BeginAsync(
    procedure
    begin
      SendCommon(ACommonID, AControlParam, State, Mask);
    end);
end;

function TOBDKWPIOControl.FireBeforeSend(AKind: TOBDKWPIOIdKind; AID: Word;
AControlParam: Byte; const AState: TBytes): Boolean;
var
  Self_: TOBDKWPIOControl;
  Kind: TOBDKWPIOIdKind;
  IDValue: Word;
  Param: Byte;
  Snap: TBytes;
  Cancel: Boolean;
begin
  Result := True;
  if not Assigned(FOnBeforeSend) then
    Exit;
  Self_ := Self;
  Kind := AKind;
  IDValue := AID;
  Param := AControlParam;
  Snap := Copy(AState, 0, Length(AState));
  Cancel := False;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnBeforeSend(Self_, Kind, IDValue, Param, Snap, Cancel)
  else
    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        if (Self_.FWorker <> nil) and Self_.IsAsyncCancelled then
          Cancel := True
        else if Assigned(Self_.FOnBeforeSend) then
          Self_.FOnBeforeSend(Self_, Kind, IDValue, Param, Snap, Cancel);
      end);
  Result := not Cancel;
end;

procedure TOBDKWPIOControl.FireResult(AKind: TOBDKWPIOIdKind; AID: Word;
AControlParam: Byte; const AResponseState: TBytes);
var
  Self_: TOBDKWPIOControl;
  Kind: TOBDKWPIOIdKind;
  IDValue: Word;
  Param: Byte;
  Snap: TBytes;
begin
  if not Assigned(FOnResult) then
    Exit;
  Self_ := Self;
  Kind := AKind;
  IDValue := AID;
  Param := AControlParam;
  Snap := Copy(AResponseState, 0, Length(AResponseState));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnResult(Self_, Kind, IDValue, Param, Snap)
  else
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        if ((Self_.FWorker = nil) or not Self_.IsAsyncCancelled) and
          Assigned(Self_.FOnResult) then
          Self_.FOnResult(Self_, Kind, IDValue, Param, Snap);
      end);
end;

procedure TOBDKWPIOControl.FireError(ACode: TOBDErrorCode;
const AMessage: string);
var
  Self_: TOBDKWPIOControl;
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
    TThread.Queue(TThread.CurrentThread,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if ((Self_.FWorker = nil) or not Self_.IsAsyncCancelled) and
          Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
