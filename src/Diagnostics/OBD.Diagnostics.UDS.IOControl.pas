//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS.IOControl
//
//  TOBDUDSIOControl — non-visual component for the UDS
//  InputOutputControlByIdentifier service (SID 0x2F). Forces an
//  I/O DID into a host-driven state — used during diagnostics to
//  drive actuators directly, freeze sensors at fixed values, or
//  release a forced state back to ECU control.
//
//  Wire format per ISO 14229-1 §10.6:
//
//    Request : 2F <DID-hi> <DID-lo> <IOControlParam> [<state>] [<mask>]
//    Response: 6F <DID-hi> <DID-lo> <IOControlParam> [<state>]
//
//  IOControl is destructive: it actively drives ECU outputs. The
//  component ships with AutoExecute = False default and a
//  TOBDActuator-style OnBeforeSend hook so the host UI can present
//  a final confirmation.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §10.6 (InputOutputControlByIdentifier)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS.IOControl;

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
  /// <summary>Return the I/O DID to ECU control.</summary>
  UDS_IOCTL_RETURN_CONTROL_TO_ECU = $00;
  /// <summary>Reset the I/O DID to its default value.</summary>
  UDS_IOCTL_RESET_TO_DEFAULT      = $01;
  /// <summary>Freeze the current ECU-reported value.</summary>
  UDS_IOCTL_FREEZE_CURRENT_STATE  = $02;
  /// <summary>Apply a host-supplied state vector.</summary>
  UDS_IOCTL_SHORT_TERM_ADJUSTMENT = $03;

type
  /// <summary>
  ///   Pre-send confirmation hook.
  /// </summary>
  /// <remarks>
  ///   Fired on the main thread just before the wire request is
  ///   sent. Set <c>ACancel := True</c> to abort the operation;
  ///   it then raises <c>EOBDConfig</c> in the caller.
  /// </remarks>
  TOBDUDSIOControlBeforeEvent = procedure(Sender: TObject; ADID: Word;
    AControlParam: Byte; const AState: TBytes;
    var ACancel: Boolean) of object;

  /// <summary>
  ///   Fires after a successful IOControl response. Main thread.
  /// </summary>
  TOBDUDSIOControlResultEvent = procedure(Sender: TObject; ADID: Word;
    AControlParam: Byte; const AResponseState: TBytes) of object;

  /// <summary>
  ///   UDS InputOutputControlByIdentifier component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>. Set
  ///   <c>AutoExecute := True</c> after operator consent, then
  ///   call <see cref="Send"/> with the target DID,
  ///   control-parameter byte (one of the <c>UDS_IOCTL_*</c>
  ///   constants), and optional state / mask bytes.
  /// </remarks>
  TOBDUDSIOControl = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnBeforeSend: TOBDUDSIOControlBeforeEvent;
    FOnResult: TOBDUDSIOControlResultEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoSend(ADID: Word; AControlParam: Byte;
      const AState: TBytes; const AControlMask: TBytes): TBytes;
    function FireBeforeSend(ADID: Word; AControlParam: Byte;
      const AState: TBytes): Boolean;
    procedure FireResult(ADID: Word; AControlParam: Byte;
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
    ///   Sends an IOControl request synchronously.
    /// </summary>
    /// <param name="ADID">Target I/O DID.</param>
    /// <param name="AControlParam">Control-parameter byte
    /// (<c>UDS_IOCTL_*</c>).</param>
    /// <param name="AState">Optional state vector for
    /// <c>shortTermAdjustment</c> (may be empty).</param>
    /// <param name="AControlMask">Optional control-mask bytes
    /// (may be empty).</param>
    /// <returns>Response state bytes (excluding the DID + param
    /// echo).</returns>
    /// <remarks>Blocks. Fires <c>OnResult</c> on success.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned, <c>AutoExecute</c> is
    ///   <c>False</c>, or <c>OnBeforeSend</c> cancelled.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response, or the DID
    ///   echo did not match.
    /// </exception>
    function Send(ADID: Word; AControlParam: Byte;
      const AState: TBytes = nil;
      const AControlMask: TBytes = nil): TBytes;

    /// <summary>Non-blocking <see cref="Send"/>.</summary>
    /// <param name="ADID">Target I/O DID.</param>
    /// <param name="AControlParam">Control-parameter byte.</param>
    /// <param name="AState">Optional state vector.</param>
    /// <param name="AControlMask">Optional control-mask bytes.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async send is already in flight.
    /// </exception>
    procedure SendAsync(ADID: Word; AControlParam: Byte;
      const AState: TBytes = nil;
      const AControlMask: TBytes = nil);
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>
    ///   Safety gate. Default <c>False</c>.
    /// </summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;

    /// <summary>
    ///   Pre-send confirmation hook. Main thread.
    /// </summary>
    property OnBeforeSend: TOBDUDSIOControlBeforeEvent
      read FOnBeforeSend write FOnBeforeSend;

    /// <summary>Fires on success. Main thread.</summary>
    property OnResult: TOBDUDSIOControlResultEvent
      read FOnResult write FOnResult;

    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent
      read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSIOControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDUDSIOControl.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSIOControl.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSIOControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSIOControl.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSIOControl: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSIOControl.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDUDSIOControl.DoSend(ADID: Word; AControlParam: Byte;
  const AState: TBytes; const AControlMask: TBytes): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
  Off: Integer;
  EchoDID: Word;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSIOControl: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDUDSIOControl: AutoExecute is False — set it before sending');
  if not FireBeforeSend(ADID, AControlParam, AState) then
    raise EOBDConfig.Create('TOBDUDSIOControl: cancelled by OnBeforeSend');

  SetLength(Req, 3 + Length(AState) + Length(AControlMask));
  Req[0] := Byte((ADID shr 8) and $FF);
  Req[1] := Byte(ADID and $FF);
  Req[2] := AControlParam;
  Off := 3;
  if Length(AState) > 0 then
  begin
    Move(AState[0], Req[Off], Length(AState));
    Inc(Off, Length(AState));
  end;
  if Length(AControlMask) > 0 then
    Move(AControlMask[0], Req[Off], Length(AControlMask));

  Resp := FProtocol.Request(UDS_SID_InputOutputControlByIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'IOControl DID 0x%.4x param 0x%.2x negative: %s',
      [ADID, AControlParam, Resp.NRCText]);
  if Length(Resp.Data) < 3 then
    raise EOBDProtocolErr.CreateFmt(
      'IOControl DID 0x%.4x: response too short', [ADID]);

  EchoDID := (Word(Resp.Data[0]) shl 8) or Word(Resp.Data[1]);
  if EchoDID <> ADID then
    raise EOBDProtocolErr.CreateFmt(
      'IOControl echo mismatch: requested 0x%.4x, got 0x%.4x',
      [ADID, EchoDID]);
  if Resp.Data[2] <> AControlParam then
    raise EOBDProtocolErr.CreateFmt(
      'IOControl param echo mismatch: requested 0x%.2x, got 0x%.2x',
      [AControlParam, Resp.Data[2]]);

  if Length(Resp.Data) > 3 then
    Result := Copy(Resp.Data, 3, Length(Resp.Data) - 3)
  else
    SetLength(Result, 0);
end;

function TOBDUDSIOControl.Send(ADID: Word; AControlParam: Byte;
  const AState: TBytes; const AControlMask: TBytes): TBytes;
begin
  Result := DoSend(ADID, AControlParam, AState, AControlMask);
  FireResult(ADID, AControlParam, Result);
end;

procedure TOBDUDSIOControl.SendAsync(ADID: Word; AControlParam: Byte;
  const AState: TBytes; const AControlMask: TBytes);
var
  Self_: TOBDUDSIOControl;
  DIDValue: Word;
  Param: Byte;
  StateCopy: TBytes;
  MaskCopy: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self;
  DIDValue := ADID;
  Param := AControlParam;
  StateCopy := Copy(AState, 0, Length(AState));
  MaskCopy := Copy(AControlMask, 0, Length(AControlMask));
  TThread.CreateAnonymousThread(
    procedure
    var
      Resp: TBytes;
    begin
      try
        try
          Resp := Self_.DoSend(DIDValue, Param, StateCopy, MaskCopy);
          Self_.FireResult(DIDValue, Param, Resp);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

function TOBDUDSIOControl.FireBeforeSend(ADID: Word; AControlParam: Byte;
  const AState: TBytes): Boolean;
var
  Self_: TOBDUDSIOControl;
  DIDValue: Word;
  Param: Byte;
  Snap: TBytes;
  Cancel: Boolean;
begin
  Result := True;
  if not Assigned(FOnBeforeSend) then
    Exit;
  Self_ := Self;
  DIDValue := ADID;
  Param := AControlParam;
  Snap := Copy(AState, 0, Length(AState));
  Cancel := False;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    FOnBeforeSend(Self_, DIDValue, Param, Snap, Cancel);
    Result := not Cancel;
  end
  else
    TThread.Synchronize(nil,
      procedure
      begin
        if Assigned(Self_.FOnBeforeSend) then
          Self_.FOnBeforeSend(Self_, DIDValue, Param, Snap, Cancel);
      end);
  Result := not Cancel;
end;

procedure TOBDUDSIOControl.FireResult(ADID: Word; AControlParam: Byte;
  const AResponseState: TBytes);
var
  Self_: TOBDUDSIOControl;
  DIDValue: Word;
  Param: Byte;
  Snap: TBytes;
begin
  if not Assigned(FOnResult) then
    Exit;
  Self_ := Self;
  DIDValue := ADID;
  Param := AControlParam;
  Snap := Copy(AResponseState, 0, Length(AResponseState));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnResult(Self_, DIDValue, Param, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnResult) then
          Self_.FOnResult(Self_, DIDValue, Param, Snap);
      end);
end;

procedure TOBDUDSIOControl.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSIOControl;
  Code: TOBDErrorCode;
  Msg: string;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
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
