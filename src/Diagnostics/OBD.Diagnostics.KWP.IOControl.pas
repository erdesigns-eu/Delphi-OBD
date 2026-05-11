//------------------------------------------------------------------------------
//  OBD.Diagnostics.KWP.IOControl
//
//  TOBDKWPIOControl — non-visual component for the KWP2000 I/O
//  control services:
//
//    Service 0x2F — InputOutputControlByLocalIdentifier
//    Service 0x30 — InputOutputControlByCommonIdentifier
//
//  Forces an I/O identifier into a host-driven state (returnControl,
//  reportControlState, shortTermAdjustment, longTermAdjustment, …).
//  Destructive — the component ships with AutoExecute = False and a
//  cancellable OnBeforeSend hook, matching the rest of the
//  diagnostics safety contract.
//
//  Wire format per ISO 14230-3:1999 §6.10:
//
//    ByLocal  request : 2F <LocalId> <IO-Param> [<state>] [<mask>]
//    ByLocal  response: 6F <LocalId> <IO-Param> [<state>]
//
//    ByCommon request : 30 <CommonId-hi> <CommonId-lo>
//                       <IO-Param> [<state>] [<mask>]
//    ByCommon response: 70 <CommonId-hi> <CommonId-lo>
//                       <IO-Param> [<state>]
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14230-3:1999 §6.10 (InputOutputControl)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.KWP.IOControl;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.KWP2000,
  OBD.Protocol;

const
  /// <summary>Return the I/O identifier to ECU control.</summary>
  KWP_IOCTL_RETURN_CONTROL_TO_ECU = $00;
  /// <summary>Report the current control state.</summary>
  KWP_IOCTL_REPORT_CONTROL_STATE  = $01;
  /// <summary>Apply a host-supplied short-term adjustment.</summary>
  KWP_IOCTL_SHORT_TERM_ADJUSTMENT = $07;
  /// <summary>Apply a host-supplied long-term adjustment.</summary>
  KWP_IOCTL_LONG_TERM_ADJUSTMENT  = $08;

type
  /// <summary>
  ///   Which identifier dialect this control call targets.
  /// </summary>
  TOBDKWPIOIdKind = (
    /// <summary>Service 0x2F — LocalIdentifier (1 byte).</summary>
    ikLocal,
    /// <summary>Service 0x30 — CommonIdentifier (2 bytes).</summary>
    ikCommon
  );

  /// <summary>
  ///   Pre-send confirmation hook. Main thread.
  /// </summary>
  TOBDKWPIOControlBeforeEvent = procedure(Sender: TObject;
    AKind: TOBDKWPIOIdKind; AID: Word; AControlParam: Byte;
    const AState: TBytes; var ACancel: Boolean) of object;

  /// <summary>Fires after a successful response. Main thread.</summary>
  TOBDKWPIOControlResultEvent = procedure(Sender: TObject;
    AKind: TOBDKWPIOIdKind; AID: Word; AControlParam: Byte;
    const AResponseState: TBytes) of object;

  /// <summary>
  ///   KWP2000 InputOutputControl component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, assign <c>Protocol</c>, set
  ///   <c>AutoExecute := True</c> after operator consent. Use
  ///   <see cref="SendLocal"/> for 1-byte LocalIdentifiers and
  ///   <see cref="SendCommon"/> for 2-byte CommonIdentifiers.
  /// </remarks>
  TOBDKWPIOControl = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnBeforeSend: TOBDKWPIOControlBeforeEvent;
    FOnResult: TOBDKWPIOControlResultEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoSend(AKind: TOBDKWPIOIdKind; AID: Word;
      AControlParam: Byte; const AState: TBytes;
      const AControlMask: TBytes): TBytes;
    function FireBeforeSend(AKind: TOBDKWPIOIdKind; AID: Word;
      AControlParam: Byte; const AState: TBytes): Boolean;
    procedure FireResult(AKind: TOBDKWPIOIdKind; AID: Word;
      AControlParam: Byte; const AResponseState: TBytes);
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
    ///   Service 0x2F — InputOutputControlByLocalIdentifier.
    /// </summary>
    /// <param name="ALocalID">1-byte local identifier.</param>
    /// <param name="AControlParam">I/O control parameter (one of
    /// <c>KWP_IOCTL_*</c>).</param>
    /// <param name="AState">Optional state vector.</param>
    /// <param name="AControlMask">Optional control mask.</param>
    /// <returns>Response state bytes (excluding the LocalID + param
    /// echo).</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned, <c>AutoExecute</c> is
    ///   <c>False</c>, or <c>OnBeforeSend</c> cancelled.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response, or the ID /
    ///   param echo did not match.
    /// </exception>
    function SendLocal(ALocalID: Byte; AControlParam: Byte;
      const AState: TBytes = nil;
      const AControlMask: TBytes = nil): TBytes;

    /// <summary>
    ///   Service 0x30 — InputOutputControlByCommonIdentifier.
    /// </summary>
    /// <param name="ACommonID">2-byte common identifier.</param>
    /// <param name="AControlParam">I/O control parameter.</param>
    /// <param name="AState">Optional state vector.</param>
    /// <param name="AControlMask">Optional control mask.</param>
    /// <returns>Response state bytes (excluding the ID + param
    /// echo).</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned, <c>AutoExecute</c> is
    ///   <c>False</c>, or <c>OnBeforeSend</c> cancelled.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response, or the ID /
    ///   param echo did not match.
    /// </exception>
    function SendCommon(ACommonID: Word; AControlParam: Byte;
      const AState: TBytes = nil;
      const AControlMask: TBytes = nil): TBytes;
  published
    /// <summary>Protocol stack. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;

    /// <summary>Pre-send confirmation hook. Main thread.</summary>
    property OnBeforeSend: TOBDKWPIOControlBeforeEvent
      read FOnBeforeSend write FOnBeforeSend;
    /// <summary>Fires on success. Main thread.</summary>
    property OnResult: TOBDKWPIOControlResultEvent
      read FOnResult write FOnResult;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent
      read FOnError write FOnError;
  end;

implementation

constructor TOBDKWPIOControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDKWPIOControl.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDKWPIOControl.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
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
    FProtocol := nil;
end;

procedure TOBDKWPIOControl.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDKWPIOControl: async already in flight');
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
  AControlParam: Byte; const AState: TBytes;
  const AControlMask: TBytes): TBytes;
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
    raise EOBDConfig.Create(
      'TOBDKWPIOControl: AutoExecute is False — set it before sending');
  if not FireBeforeSend(AKind, AID, AControlParam, AState) then
    raise EOBDConfig.Create(
      'TOBDKWPIOControl: cancelled by OnBeforeSend');

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

  Resp := FProtocol.Request(SID, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'KWP IOControl (SID 0x%.2x, ID 0x%.4x) negative: %s',
      [SID, AID, Resp.NRCText]);
  if Length(Resp.Data) < IDBytes + 1 then
    raise EOBDProtocolErr.CreateFmt(
      'KWP IOControl (SID 0x%.2x): response too short', [SID]);

  // Verify echo bytes.
  if AKind = ikLocal then
  begin
    if Resp.Data[0] <> Byte(AID and $FF) then
      raise EOBDProtocolErr.CreateFmt(
        'KWP IOControl echo mismatch on LocalID 0x%.2x', [AID]);
  end
  else
  begin
    if ((Word(Resp.Data[0]) shl 8) or Word(Resp.Data[1])) <> AID then
      raise EOBDProtocolErr.CreateFmt(
        'KWP IOControl echo mismatch on CommonID 0x%.4x', [AID]);
  end;
  if Resp.Data[IDBytes] <> AControlParam then
    raise EOBDProtocolErr.CreateFmt(
      'KWP IOControl param echo mismatch: requested 0x%.2x, got 0x%.2x',
      [AControlParam, Resp.Data[IDBytes]]);

  if Length(Resp.Data) > IDBytes + 1 then
    Result := Copy(Resp.Data, IDBytes + 1,
                   Length(Resp.Data) - IDBytes - 1)
  else
    SetLength(Result, 0);
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
  Result := DoSend(ikCommon, ACommonID, AControlParam, AState,
                   AControlMask);
  FireResult(ikCommon, ACommonID, AControlParam, Result);
end;

function TOBDKWPIOControl.FireBeforeSend(AKind: TOBDKWPIOIdKind;
  AID: Word; AControlParam: Byte; const AState: TBytes): Boolean;
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
    TThread.Synchronize(nil,
      procedure
      begin
        if Assigned(Self_.FOnBeforeSend) then
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
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnResult) then
          Self_.FOnResult(Self_, Kind, IDValue, Param, Snap);
      end);
end;

procedure TOBDKWPIOControl.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDKWPIOControl;
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
