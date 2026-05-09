//------------------------------------------------------------------------------
//  OBD.KWP.WriteID
//
//  TOBDKWPWriteID — non-visual component for ISO 14230-3
//  WriteDataByLocalIdentifier (SID 0x3B). The KWP write counterpart
//  of UDS WriteDataByIdentifier; older European cars (VAG, BMW
//  E-series, PSA EOBD) accept this on the K-line.
//
//  Wire format per ISO 14230-3 §6.7:
//
//    Request : 3B <recordLocalIdentifier> <recordValue...>
//    Response: 7B <recordLocalIdentifier>
//
//  Local identifiers are 8-bit unlike UDS DIDs.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14230-3:1999 § 6.7 (writeDataByLocalIdentifier)
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit OBD.KWP.WriteID;

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
  /// <summary>KWP2000 WriteDataByLocalIdentifier SID.</summary>
  KWP_SID_WriteDataByLocalIdentifier = $3B;

type
  /// <summary>KWP2000 write-local-identifier component.</summary>
  TOBDKWPWriteID = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnWrite: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoWrite(ALocalID: Byte; const AData: TBytes);
    procedure FireWrite;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Writes <c>AData</c> to local identifier
    /// <c>ALocalID</c>. Requires <c>AutoExecute = True</c>.</summary>
    procedure Write(ALocalID: Byte; const AData: TBytes);
    /// <summary>Non-blocking <see cref="Write"/>.</summary>
    procedure WriteAsync(ALocalID: Byte; const AData: TBytes);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    property OnWrite: TNotifyEvent read FOnWrite write FOnWrite;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDKWPWriteID.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDKWPWriteID.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDKWPWriteID.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDKWPWriteID.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDKWPWriteID.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDKWPWriteID: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDKWPWriteID.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDKWPWriteID.DoWrite(ALocalID: Byte; const AData: TBytes);
var
  Body: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDKWPWriteID: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDKWPWriteID: AutoExecute is False — set it before writing');
  SetLength(Body, 1 + Length(AData));
  Body[0] := ALocalID;
  if Length(AData) > 0 then Move(AData[0], Body[1], Length(AData));
  Resp := FProtocol.Request(KWP_SID_WriteDataByLocalIdentifier, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'KWP WriteDataByLocalIdentifier 0x%2.2X negative: %s',
      [ALocalID, Resp.NRCText]);
end;

procedure TOBDKWPWriteID.Write(ALocalID: Byte; const AData: TBytes);
begin
  DoWrite(ALocalID, AData);
  FireWrite;
end;

procedure TOBDKWPWriteID.WriteAsync(ALocalID: Byte; const AData: TBytes);
var
  Self_: TOBDKWPWriteID; LID: Byte; Data: TBytes;
begin
  GuardSingleAsync;
  Self_ := Self; LID := ALocalID;
  Data := Copy(AData, 0, Length(AData));
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoWrite(LID, Data);
          Self_.FireWrite;
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDKWPWriteID.FireWrite;
var
  Self_: TOBDKWPWriteID;
begin
  if not Assigned(FOnWrite) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnWrite(Self_)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnWrite) then Self_.FOnWrite(Self_);
    end);
end;

procedure TOBDKWPWriteID.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDKWPWriteID; Code: TOBDErrorCode; Msg: string;
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
