//------------------------------------------------------------------------------
//  OBD.ClearDTC
//
//  TOBDClearDTC — non-visual component for OBD-II Mode 0x04
//  ClearDiagnosticInformation and the UDS Service 0x14 equivalent.
//  Independent of TOBDDTCs (which still ships a convenience Clear
//  method) so a host can drop a single-purpose clear button on a
//  form without instantiating the full DTC-reader surface.
//
//  Both paths covered:
//    Mode 0x04 (OBD-II)           : request 0x04 with no data
//    Service 0x14 (UDS / KWP2000) : request 0x14 with a 3-byte
//                                   DTC group (0xFFFFFF = all
//                                   groups, ISO 14229-1 §11.5).
//
//  AutoExecute = False default — Clear is destructive (drops the
//  vehicle's stored DTC history) and is gated behind the standard
//  Delphi-OBD confirmation contract.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 15031-5 § 7.6 (Service $04 ClearDiagnosticInformation)
//    - ISO 14229-1:2020 § 11.5 (ClearDiagnosticInformation 0x14)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.ClearDTC;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol;

const
  /// <summary>OBD-II Mode 0x04 — ClearDiagnosticInformation.</summary>
  OBD_MODE_CLEAR_DTC      = $04;
  /// <summary>UDS Service 0x14 — ClearDiagnosticInformation.</summary>
  UDS_SID_CLEAR_DTC       = $14;
  /// <summary>UDS 0x14 group code "all groups".</summary>
  UDS_DTC_GROUP_ALL       = $FFFFFF;

type
  /// <summary>Which clear path to use.</summary>
  TOBDClearDTCDialect = (
    /// <summary>OBD-II Mode 0x04 (no body bytes).</summary>
    cdOBDII,
    /// <summary>UDS Service 0x14 with a 3-byte group selector.</summary>
    cdUDS
  );

  /// <summary>Fires after a successful clear.</summary>
  TOBDClearDTCEvent = procedure(Sender: TObject) of object;

  /// <summary>ClearDTC component.</summary>
  TOBDClearDTC = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FDialect: TOBDClearDTCDialect;
    FUDSGroup: Cardinal;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnCleared: TOBDClearDTCEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure DoClear;
    procedure FireCleared;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Clears DTCs synchronously. Raises <c>EOBDConfig</c>
    /// when <c>AutoExecute</c> is <c>False</c> or
    /// <c>EOBDProtocolErr</c> on a negative response.</summary>
    procedure Clear;
    /// <summary>Non-blocking <see cref="Clear"/>. Fires
    /// <c>OnCleared</c> on success or <c>OnError</c> on failure
    /// on the main thread.</summary>
    procedure ClearAsync;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
    /// <summary>Which clear path. Default <c>cdOBDII</c>.</summary>
    property Dialect: TOBDClearDTCDialect read FDialect write FDialect
      default cdOBDII;
    /// <summary>UDS group selector (24-bit) for <c>cdUDS</c>.
    /// Default <c>0xFFFFFF</c> (all groups).</summary>
    property UDSGroup: Cardinal read FUDSGroup write FUDSGroup
      default UDS_DTC_GROUP_ALL;
    property OnCleared: TOBDClearDTCEvent read FOnCleared write FOnCleared;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDClearDTC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FUDSGroup := UDS_DTC_GROUP_ALL;
  FDialect := cdOBDII;
end;

destructor TOBDClearDTC.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDClearDTC.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDClearDTC.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDClearDTC.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDClearDTC: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDClearDTC.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDClearDTC.DoClear;
var
  Resp: TOBDResponse;
  Body: TBytes;
  SID: Byte;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDClearDTC: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDClearDTC: AutoExecute is False — set it before clearing');

  case FDialect of
    cdOBDII:
    begin
      SID := OBD_MODE_CLEAR_DTC;
      Body := nil;
    end;
    cdUDS:
    begin
      SID := UDS_SID_CLEAR_DTC;
      // 3-byte big-endian DTC group.
      SetLength(Body, 3);
      Body[0] := Byte((FUDSGroup shr 16) and $FF);
      Body[1] := Byte((FUDSGroup shr  8) and $FF);
      Body[2] := Byte( FUDSGroup         and $FF);
    end;
  else
    raise EOBDConfig.Create('TOBDClearDTC: unknown dialect');
  end;

  Resp := FProtocol.Request(SID, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ClearDTC (SID 0x%.2x) negative: %s', [SID, Resp.NRCText]);
end;

procedure TOBDClearDTC.Clear;
begin
  DoClear;
  FireCleared;
end;

procedure TOBDClearDTC.ClearAsync;
var
  Self_: TOBDClearDTC;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoClear;
          Self_.FireCleared;
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDClearDTC.FireCleared;
var
  Self_: TOBDClearDTC;
begin
  if not Assigned(FOnCleared) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnCleared(Self_)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnCleared) then Self_.FOnCleared(Self_);
    end);
end;

procedure TOBDClearDTC.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDClearDTC; Code: TOBDErrorCode; Msg: string;
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
