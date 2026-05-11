//------------------------------------------------------------------------------
//  OBD.ClearDTC
//
//  TOBDClearDTC — non-visual component for OBD-II Mode 0x04
//  ClearDiagnosticInformation and the UDS Service 0x14 equivalent.
//  Independent of TOBDDTCs (which still ships a convenience Clear
//  method) so a host can drop a single-purpose clear button on a
//  form without instantiating the full DTC-reader surface.
//
//  Three paths are covered:
//
//    Mode 0x04 (OBD-II)        : request 0x04 with no data.
//    Service 0x14 (UDS)        : request 0x14 with a 3-byte DTC
//                                group (0xFFFFFF = all groups,
//                                ISO 14229-1 §11.5).
//    Service 0x14 (KWP2000)    : request 0x14 with a 2-byte DTC
//                                group word (ISO 14230-3 §6.6.3).
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
  OBD_MODE_CLEAR_DTC = $04;
  /// <summary>UDS Service 0x14 — ClearDiagnosticInformation.</summary>
  UDS_SID_CLEAR_DTC = $14;
  /// <summary>UDS 0x14 "all groups" group code (3 bytes).</summary>
  UDS_DTC_GROUP_ALL = $FFFFFF;
  /// <summary>KWP2000 Service 0x14 —
  /// ClearDiagnosticInformation.</summary>
  KWP_SID_CLEAR_DTC = $14;
  /// <summary>KWP2000 0x14 "all groups" group code (2 bytes).</summary>
  KWP_DTC_GROUP_ALL = $FFFF;

type
  /// <summary>Which clear path to use.</summary>
  TOBDClearDTCDialect = (
    /// <summary>OBD-II Mode 0x04 (no body bytes).</summary>
    cdOBDII,
    /// <summary>UDS Service 0x14 with a 3-byte group selector.</summary>
    cdUDS,
    /// <summary>KWP2000 Service 0x14 with a 2-byte group word.</summary>
    cdKWP
  );

  /// <summary>
  ///   Fires after a successful clear. Main thread.
  /// </summary>
  TOBDClearDTCEvent = procedure(Sender: TObject) of object;

  /// <summary>
  ///   Mode 0x04 / UDS 0x14 ClearDiagnosticInformation component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form, assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>, set
  ///   <c>AutoExecute := True</c> once the host has explicitly
  ///   consented, then call <see cref="Clear"/> (sync) or
  ///   <see cref="ClearAsync"/> (non-blocking).
  ///
  ///   <c>Dialect</c> picks the wire path. <c>cdOBDII</c> is the
  ///   universal fallback for any vehicle. <c>cdUDS</c> targets
  ///   the broader UDS service and accepts a 24-bit
  ///   <c>UDSGroup</c> selector.
  /// </remarks>
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
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner (standard VCL pattern).</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state and cancels in-flight async work.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Clears DTCs synchronously.
    /// </summary>
    /// <remarks>
    ///   Blocks the caller until the ECU response arrives or the
    ///   protocol times out. From GUI code prefer
    ///   <see cref="ClearAsync"/>.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned, <c>AutoExecute</c> is
    ///   <c>False</c>, or <c>Dialect</c> is out of range.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    procedure Clear;

    /// <summary>
    ///   Clears DTCs without blocking.
    /// </summary>
    /// <remarks>
    ///   Spawns a worker thread; reports completion via
    ///   <c>OnCleared</c> or failure via <c>OnError</c> on the
    ///   main thread. Only one <c>ClearAsync</c> may be in flight
    ///   at a time.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    ///   Another async clear is already in flight.
    /// </exception>
    procedure ClearAsync;
  published
    /// <summary>Protocol stack to clear through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>
    ///   Safety gate. Default <c>False</c>.
    /// </summary>
    /// <remarks>
    ///   Every <c>Clear</c> raises <c>EOBDConfig</c> while this is
    ///   <c>False</c>. The host flips it to <c>True</c> once the
    ///   operator has explicitly consented to the clear.
    /// </remarks>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;

    /// <summary>
    ///   Which clear-path dialect. Default <c>cdOBDII</c>.
    /// </summary>
    property Dialect: TOBDClearDTCDialect read FDialect write FDialect
      default cdOBDII;

    /// <summary>
    ///   24-bit group selector for <c>cdUDS</c>. Default
    ///   <c>0xFFFFFF</c> (all groups). Ignored when
    ///   <c>Dialect = cdOBDII</c>.
    /// </summary>
    property UDSGroup: Cardinal read FUDSGroup write FUDSGroup
      default UDS_DTC_GROUP_ALL;

    /// <summary>Fires on success. Main thread.</summary>
    property OnCleared: TOBDClearDTCEvent read FOnCleared write FOnCleared;

    /// <summary>Fires on transient I/O errors. Main thread.</summary>
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
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
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
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDClearDTC.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
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
        // 3-byte big-endian DTC group selector.
        SetLength(Body, 3);
        Body[0] := Byte((FUDSGroup shr 16) and $FF);
        Body[1] := Byte((FUDSGroup shr  8) and $FF);
        Body[2] := Byte( FUDSGroup         and $FF);
      end;
    cdKWP:
      begin
        SID := KWP_SID_CLEAR_DTC;
        // KWP2000: 2-byte big-endian DTC group word.
        SetLength(Body, 2);
        Body[0] := Byte((FUDSGroup shr 8) and $FF);
        Body[1] := Byte( FUDSGroup        and $FF);
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
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
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
  if not Assigned(FOnCleared) then
    Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnCleared(Self_)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnCleared) then
          Self_.FOnCleared(Self_);
      end);
end;

procedure TOBDClearDTC.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDClearDTC;
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
