//------------------------------------------------------------------------------
//  OBD.Service.Actuator
//
//  TOBDActuator — non-visual component that drives OBD-II Mode 08
//  "Request Control of On-Board Systems / Tests / Components".
//  Mode 08 commands cause the ECU to actuate hardware: fuel pump,
//  cooling fans, EVAP solenoids, EGR valves, etc. It is the most
//  potentially-dangerous standard service in the OBD-II catalogue,
//  so this component ships with an explicit safety gate.
//
//  Safety contract:
//
//    - <c>AutoExecute</c> defaults to <c>False</c>. With the gate
//      closed, every <c>Send</c> call raises <c>EOBDConfig</c>
//      before touching the wire. Hosts that intentionally drive
//      actuators flip the property to True at the same time as a
//      "I know what I'm doing" UI confirmation.
//
//    - The synchronous and asynchronous sends both honour the
//      gate; there is no back-door entry point.
//
//    - When the gate is open, <c>OnBeforeSend</c> fires on the
//      main thread with a <c>Cancel: Boolean</c> out-parameter.
//      This is the host's last chance to abort (e.g. confirm
//      with the operator).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1979 Mode 08 — Request Control of On-Board Systems
//
//  History     :
//    2026-05-09  ERD  Follow-up — close-out deferrals.
//------------------------------------------------------------------------------

unit OBD.Service.Actuator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol;

const
  /// <summary>OBD-II Mode 08 — request control of on-board
  /// systems / tests / components.</summary>
  OBD_MODE_REQUEST_CONTROL = $08;

type
  /// <summary>Argument record for <c>OnBeforeSend</c>.</summary>
  TOBDActuatorBeforeEvent = procedure(Sender: TObject; ATID: Byte;
    const AData: TBytes; var ACancel: Boolean) of object;

  /// <summary>Argument record for <c>OnResult</c>.</summary>
  TOBDActuatorResultEvent = procedure(Sender: TObject; ATID: Byte;
    const AResponseData: TBytes) of object;

  /// <summary>
  ///   Mode 08 actuator-control component.
  /// </summary>
  TOBDActuator = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnBeforeSend: TOBDActuatorBeforeEvent;
    FOnResult: TOBDActuatorResultEvent;
    FOnError: TOBDConnectionErrorEvent;
    function DoSend(ATID: Byte; const AData: TBytes): TBytes;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function FireBeforeSend(ATID: Byte; const AData: TBytes): Boolean;
    procedure FireResult(ATID: Byte; const AResponseData: TBytes);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Sends a Mode 08 actuator command synchronously.
    /// </summary>
    /// <param name="ATID">Test / control ID byte.</param>
    /// <param name="AData">Optional payload bytes following the
    /// TID. May be empty.</param>
    /// <returns>Response data bytes after the SID echo + TID echo.</returns>
    /// <exception cref="EOBDConfig"><c>AutoExecute = False</c> or
    /// <c>OnBeforeSend</c> set <c>Cancel := True</c>.</exception>
    /// <exception cref="EOBDProtocolErr">Negative response.</exception>
    function Send(ATID: Byte; const AData: TBytes = nil): TBytes;
    /// <summary>Non-blocking <see cref="Send"/>. Fires
    /// <c>OnResult</c> on success, <c>OnError</c> on failure.</summary>
    procedure SendAsync(ATID: Byte; const AData: TBytes = nil);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate. Default <c>False</c> — every
    /// <c>Send</c> raises <c>EOBDConfig</c> until the host flips
    /// this to <c>True</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    /// <summary>Fires before sending; the host can set
    /// <c>Cancel := True</c> to abort. Main thread.</summary>
    property OnBeforeSend: TOBDActuatorBeforeEvent
      read FOnBeforeSend write FOnBeforeSend;
    /// <summary>Fires on a positive response (main thread).</summary>
    property OnResult: TOBDActuatorResultEvent
      read FOnResult write FOnResult;
    /// <summary>Fires on transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent
      read FOnError write FOnError;
  end;

implementation

constructor TOBDActuator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAutoExecute := False;
end;

destructor TOBDActuator.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDActuator.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDActuator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDActuator.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDActuator: async send already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDActuator.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDActuator.DoSend(ATID: Byte; const AData: TBytes): TBytes;
var
  Resp: TOBDResponse;
  Body: TBytes;
  Off: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDActuator: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDActuator: AutoExecute is False — set it explicitly before sending');
  if not FireBeforeSend(ATID, AData) then
    raise EOBDConfig.Create('TOBDActuator: cancelled by OnBeforeSend handler');

  SetLength(Body, 1 + Length(AData));
  Body[0] := ATID;
  if Length(AData) > 0 then
    Move(AData[0], Body[1], Length(AData));

  Resp := FProtocol.Request(OBD_MODE_REQUEST_CONTROL, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'Mode 08 TID 0x%2.2X negative: %s', [ATID, Resp.NRCText]);
  // Strip TID echo if present so callers get the application
  // payload directly.
  if (Length(Resp.Data) >= 1) and (Resp.Data[0] = ATID) then Off := 1
  else Off := 0;
  if Length(Resp.Data) > Off then
  begin
    SetLength(Result, Length(Resp.Data) - Off);
    Move(Resp.Data[Off], Result[0], Length(Resp.Data) - Off);
  end
  else
    SetLength(Result, 0);
end;

function TOBDActuator.Send(ATID: Byte; const AData: TBytes): TBytes;
begin
  Result := DoSend(ATID, AData);
  FireResult(ATID, Result);
end;

procedure TOBDActuator.SendAsync(ATID: Byte; const AData: TBytes);
var
  Self_: TOBDActuator;
  TID: Byte;
  Data: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; TID := ATID; Data := Copy(AData, 0, Length(AData));
  TThread.CreateAnonymousThread(
    procedure
    var
      ResData: TBytes;
    begin
      try
        try
          ResData := Self_.DoSend(TID, Data);
          Self_.FireResult(TID, ResData);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

function TOBDActuator.FireBeforeSend(ATID: Byte;
  const AData: TBytes): Boolean;
var
  Cancel: Boolean;
  Self_: TOBDActuator;
  TID: Byte;
  Data: TBytes;
  Local: Boolean;
begin
  if not Assigned(FOnBeforeSend) then Exit(True);
  Self_ := Self; TID := ATID; Data := AData; Cancel := False;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnBeforeSend(Self_, TID, Data, Cancel)
  else
  begin
    Local := False;
    TThread.Synchronize(nil, procedure
      var C: Boolean;
      begin
        C := False;
        if Assigned(Self_.FOnBeforeSend) then
          Self_.FOnBeforeSend(Self_, TID, Data, C);
        Local := C;
      end);
    Cancel := Local;
  end;
  Result := not Cancel;
end;

procedure TOBDActuator.FireResult(ATID: Byte;
  const AResponseData: TBytes);
var
  Self_: TOBDActuator;
  TID: Byte;
  Snap: TBytes;
begin
  if not Assigned(FOnResult) then Exit;
  Self_ := Self; TID := ATID;
  Snap := Copy(AResponseData, 0, Length(AResponseData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnResult(Self_, TID, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnResult) then Self_.FOnResult(Self_, TID, Snap);
    end);
end;

procedure TOBDActuator.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDActuator; Code: TOBDErrorCode; Msg: string;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
