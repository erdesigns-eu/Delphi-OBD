//------------------------------------------------------------------------------
//  OBD.Diagnostics.UDS.Reset
//
//  TOBDUDSReset — non-visual component for the UDS ECU Reset
//  service (SID 0x11). Sends a reset request, waits for the
//  positive response, then surfaces the new reset state via
//  <c>OnReset</c>. The component is single-purpose so a host can
//  drop a reset button on a form without instantiating the full
//  session-hub surface.
//
//  Wire format per ISO 14229-1 §9.3:
//
//    Request : 11 <resetType>
//    Response: 51 <resetType> [ <powerDownTime> ]   (sub 0x04 only)
//
//  AutoExecute = False default — ECU reset interrupts every active
//  diagnostic and may bounce the CAN bus; gated behind the standard
//  Delphi-OBD confirmation contract.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 §9.3 (ECUReset 0x11)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Diagnostics.UDS.Reset;

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
  /// <summary>Hard reset (full ECU power cycle).</summary>
  UDS_RESET_HARD                  = $01;
  /// <summary>Key-off / on reset — equivalent to a manual ignition
  /// cycle.</summary>
  UDS_RESET_KEY_OFF_ON            = $02;
  /// <summary>Soft reset (re-initialise application, do not lose
  /// learned values).</summary>
  UDS_RESET_SOFT                  = $03;
  /// <summary>Enable rapid-power-shutdown (post-reset behaviour
  /// hint).</summary>
  UDS_RESET_ENABLE_RAPID_SHUTDOWN  = $04;
  /// <summary>Disable rapid-power-shutdown.</summary>
  UDS_RESET_DISABLE_RAPID_SHUTDOWN = $05;

type
  /// <summary>
  ///   Fires after a successful reset.
  /// </summary>
  /// <remarks>
  ///   <c>AResetType</c> is the sub-function byte echoed by the
  ///   ECU. <c>APowerDownTimeSec</c> carries the
  ///   power-down-time payload from <c>UDS_RESET_ENABLE_RAPID_SHUTDOWN</c>
  ///   responses; it is 0 for every other reset type. Main thread.
  /// </remarks>
  TOBDUDSResetEvent = procedure(Sender: TObject; AResetType: Byte;
    APowerDownTimeSec: Byte) of object;

  /// <summary>
  ///   UDS ECU-Reset component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form, assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>, set
  ///   <c>AutoExecute := True</c> once the operator has consented,
  ///   then call <see cref="Reset"/> (sync) or
  ///   <see cref="ResetAsync"/>.
  /// </remarks>
  TOBDUDSReset = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAutoExecute: Boolean;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnReset: TOBDUDSResetEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoReset(AResetType: Byte): Byte;
    procedure FireReset(AResetType: Byte; APowerDownTime: Byte);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state and cancels in-flight async work.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Requests an ECU reset synchronously.
    /// </summary>
    /// <param name="AResetType">One of the <c>UDS_RESET_*</c>
    /// constants.</param>
    /// <remarks>Blocks. Fires <c>OnReset</c> on success.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned or <c>AutoExecute</c> is
    ///   <c>False</c>.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response, or the response echo
    ///   did not match the requested reset type.
    /// </exception>
    procedure Reset(AResetType: Byte = UDS_RESET_SOFT);

    /// <summary>Non-blocking <see cref="Reset"/>.</summary>
    /// <param name="AResetType">Reset sub-function.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async reset is already in flight.
    /// </exception>
    procedure ResetAsync(AResetType: Byte = UDS_RESET_SOFT);
  published
    /// <summary>Protocol stack to reset through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>
    ///   Safety gate. Default <c>False</c>.
    /// </summary>
    /// <remarks>
    ///   Every <c>Reset</c> raises <c>EOBDConfig</c> while this is
    ///   <c>False</c>. The host flips it to <c>True</c> once the
    ///   operator has explicitly consented.
    /// </remarks>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;

    /// <summary>Fires after a successful reset. Main thread.</summary>
    property OnReset: TOBDUDSResetEvent read FOnReset write FOnReset;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDUDSReset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDUDSReset.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDUDSReset.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDUDSReset.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDUDSReset.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDUDSReset: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDUDSReset.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDUDSReset.DoReset(AResetType: Byte): Byte;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDUDSReset: Protocol not assigned');
  if not FAutoExecute then
    raise EOBDConfig.Create(
      'TOBDUDSReset: AutoExecute is False — set it before resetting');

  SetLength(Req, 1);
  Req[0] := AResetType;
  Resp := FProtocol.Request(UDS_SID_ECUReset, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ECUReset (0x%.2x) negative: %s', [AResetType, Resp.NRCText]);
  if Length(Resp.Data) < 1 then
    raise EOBDProtocolErr.CreateFmt(
      'ECUReset (0x%.2x): short response', [AResetType]);
  if Resp.Data[0] <> AResetType then
    raise EOBDProtocolErr.CreateFmt(
      'ECUReset echo mismatch: requested 0x%.2x, got 0x%.2x',
      [AResetType, Resp.Data[0]]);

  // PowerDownTime is only present for the enableRapidShutdown
  // sub-function (0x04).
  if (AResetType = UDS_RESET_ENABLE_RAPID_SHUTDOWN) and
     (Length(Resp.Data) >= 2) then
    Result := Resp.Data[1]
  else
    Result := 0;
end;

procedure TOBDUDSReset.Reset(AResetType: Byte);
var
  PowerDownTime: Byte;
begin
  PowerDownTime := DoReset(AResetType);
  FireReset(AResetType, PowerDownTime);
end;

procedure TOBDUDSReset.ResetAsync(AResetType: Byte);
var
  Self_: TOBDUDSReset;
  Kind: Byte;
begin
  GuardSingleAsync;
  Self_ := Self;
  Kind := AResetType;
  TThread.CreateAnonymousThread(
    procedure
    var
      PDT: Byte;
    begin
      try
        try
          PDT := Self_.DoReset(Kind);
          Self_.FireReset(Kind, PDT);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDUDSReset.FireReset(AResetType: Byte; APowerDownTime: Byte);
var
  Self_: TOBDUDSReset;
  Kind: Byte;
  PDT: Byte;
begin
  if not Assigned(FOnReset) then
    Exit;
  Self_ := Self;
  Kind := AResetType;
  PDT := APowerDownTime;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnReset(Self_, Kind, PDT)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnReset) then
          Self_.FOnReset(Self_, Kind, PDT);
      end);
end;

procedure TOBDUDSReset.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDUDSReset;
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
