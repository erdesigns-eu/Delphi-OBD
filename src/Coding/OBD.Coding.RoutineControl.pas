//------------------------------------------------------------------------------
//  OBD.Coding.RoutineControl
//
//  TOBDRoutineControl — non-visual component that drives the
//  ISO 14229-1 § 12.7 RoutineControl service (SID 0x31).
//
//  Routines are vendor-specific high-level diagnostic actions
//  identified by a 16-bit Routine Identifier (RID). This component
//  exposes the three sub-functions:
//
//    - 0x01 startRoutine
//    - 0x02 stopRoutine
//    - 0x03 requestRoutineResults
//
//  Start / stop are gated by <c>AutoExecute</c> (default
//  <c>False</c>) because they often kick off destructive actions
//  (clear adaptations, learn key, format flash). Results queries
//  are read-only and not gated.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 12.7 (RoutineControl)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.RoutineControl;

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
  /// <summary>RoutineControl sub-function: start.</summary>
  UDS_RC_START   = $01;
  /// <summary>RoutineControl sub-function: stop.</summary>
  UDS_RC_STOP    = $02;
  /// <summary>RoutineControl sub-function: requestResults.</summary>
  UDS_RC_RESULTS = $03;

type
  /// <summary>Decoded routine response.</summary>
  TOBDRoutineResult = record
    SubFunction: Byte;
    RoutineID: Word;
    Data: TBytes;
  end;

  /// <summary>Fires when a routine call completes.</summary>
  TOBDRoutineEvent = procedure(Sender: TObject;
    const AResult: TOBDRoutineResult) of object;

  /// <summary>RoutineControl service component.</summary>
  TOBDRoutineControl = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnRoutine: TOBDRoutineEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoCall(ASubFunction: Byte; ARoutineID: Word;
      const AParams: TBytes): TOBDRoutineResult;
    procedure FireRoutine(const AResult: TOBDRoutineResult);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Starts a routine. Requires
    /// <c>AutoExecute = True</c>.</summary>
    function Start(ARoutineID: Word;
      const AParams: TBytes = nil): TOBDRoutineResult;
    /// <summary>Non-blocking <see cref="Start"/>.</summary>
    procedure StartAsync(ARoutineID: Word;
      const AParams: TBytes = nil);

    /// <summary>Stops a routine. Requires
    /// <c>AutoExecute = True</c>.</summary>
    function Stop(ARoutineID: Word;
      const AParams: TBytes = nil): TOBDRoutineResult;
    /// <summary>Non-blocking <see cref="Stop"/>.</summary>
    procedure StopAsync(ARoutineID: Word;
      const AParams: TBytes = nil);

    /// <summary>Requests routine results. Read-only — not
    /// gated.</summary>
    function RequestResults(ARoutineID: Word): TOBDRoutineResult;
    /// <summary>Non-blocking <see cref="RequestResults"/>.</summary>
    procedure RequestResultsAsync(ARoutineID: Word);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate for <c>Start</c> / <c>Stop</c>.
    /// Default <c>False</c>. <c>RequestResults</c> is not gated.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    property OnRoutine: TOBDRoutineEvent read FOnRoutine write FOnRoutine;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDRoutineControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FAutoExecute := False;
end;

destructor TOBDRoutineControl.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDRoutineControl.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDRoutineControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDRoutineControl.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDRoutineControl: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDRoutineControl.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDRoutineControl.DoCall(ASubFunction: Byte; ARoutineID: Word;
  const AParams: TBytes): TOBDRoutineResult;
var
  Body: TBytes;
  Resp: TOBDResponse;
  Off: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDRoutineControl: Protocol not assigned');
  if (ASubFunction in [UDS_RC_START, UDS_RC_STOP]) and not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDRoutineControl: AutoExecute is False — set it before start/stop');

  SetLength(Body, 3 + Length(AParams));
  Body[0] := ASubFunction;
  Body[1] := Hi(ARoutineID);
  Body[2] := Lo(ARoutineID);
  if Length(AParams) > 0 then
    Move(AParams[0], Body[3], Length(AParams));

  Resp := FProtocol.Request(UDS_SID_RoutineControl, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'RoutineControl sub 0x%2.2X RID 0x%4.4X negative: %s',
      [ASubFunction, ARoutineID, Resp.NRCText]);

  // Response: <subFunc echo> <RID HI LO> [data...]
  Result := Default(TOBDRoutineResult);
  Result.SubFunction := ASubFunction;
  Result.RoutineID := ARoutineID;
  if Length(Resp.Data) >= 3 then
  begin
    Off := 3;
    SetLength(Result.Data, Length(Resp.Data) - Off);
    if Length(Result.Data) > 0 then
      Move(Resp.Data[Off], Result.Data[0], Length(Result.Data));
  end;
end;

function TOBDRoutineControl.Start(ARoutineID: Word;
  const AParams: TBytes): TOBDRoutineResult;
begin
  Result := DoCall(UDS_RC_START, ARoutineID, AParams);
  FireRoutine(Result);
end;

function TOBDRoutineControl.Stop(ARoutineID: Word;
  const AParams: TBytes): TOBDRoutineResult;
begin
  Result := DoCall(UDS_RC_STOP, ARoutineID, AParams);
  FireRoutine(Result);
end;

function TOBDRoutineControl.RequestResults(
  ARoutineID: Word): TOBDRoutineResult;
begin
  Result := DoCall(UDS_RC_RESULTS, ARoutineID, nil);
  FireRoutine(Result);
end;

procedure TOBDRoutineControl.StartAsync(ARoutineID: Word;
  const AParams: TBytes);
var
  Self_: TOBDRoutineControl; RID: Word; Params: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; RID := ARoutineID;
  Params := Copy(AParams, 0, Length(AParams));
  TThread.CreateAnonymousThread(
    procedure
    var R: TOBDRoutineResult;
    begin
      try
        try
          R := Self_.DoCall(UDS_RC_START, RID, Params);
          Self_.FireRoutine(R);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDRoutineControl.StopAsync(ARoutineID: Word;
  const AParams: TBytes);
var
  Self_: TOBDRoutineControl; RID: Word; Params: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; RID := ARoutineID;
  Params := Copy(AParams, 0, Length(AParams));
  TThread.CreateAnonymousThread(
    procedure
    var R: TOBDRoutineResult;
    begin
      try
        try
          R := Self_.DoCall(UDS_RC_STOP, RID, Params);
          Self_.FireRoutine(R);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDRoutineControl.RequestResultsAsync(ARoutineID: Word);
var
  Self_: TOBDRoutineControl; RID: Word;
begin
  GuardSingleAsync;
  Self_ := Self; RID := ARoutineID;
  TThread.CreateAnonymousThread(
    procedure
    var R: TOBDRoutineResult;
    begin
      try
        try
          R := Self_.DoCall(UDS_RC_RESULTS, RID, nil);
          Self_.FireRoutine(R);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDRoutineControl.FireRoutine(const AResult: TOBDRoutineResult);
var
  Self_: TOBDRoutineControl;
  Snap: TOBDRoutineResult;
begin
  if not Assigned(FOnRoutine) then Exit;
  Self_ := Self; Snap := AResult;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnRoutine(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnRoutine) then Self_.FOnRoutine(Self_, Snap);
    end);
end;

procedure TOBDRoutineControl.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDRoutineControl; Code: TOBDErrorCode; Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
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
