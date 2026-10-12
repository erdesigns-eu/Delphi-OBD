// ------------------------------------------------------------------------------
// ERD.WWHOBD.Readiness
//
// TOBDWWHReadiness - SAE J1979-2 / WWH-OBD monitor-status reporting.
// J1979-2 maps legacy Mode 01 PID 01 to DID F401 and PID 41 to DID
// F441; both carry the same four monitor-status bytes A B C D.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - SAE J1979-2 - J1979 data carried by UDS ReadDataByIdentifier
// - SAE J1979 / ISO 15031-5 - monitor status PID 01 / PID 41
// - ISO 14229-1:2020 - ReadDTCInformation sub-function 0x56
//
// History     :
// 2026-05-11  ERD  Initial implementation.
// 2026-10-12  ERD  Decode J1979-2 F401/F441 monitor status via shared logic.
// ------------------------------------------------------------------------------

unit ERD.WWHOBD.Readiness;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Async.Task,
  ERD.Connection,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Protocol.UDS,
  ERD.Protocol,
  ERD.Service.DriveCycle.Types,
  ERD.WWHOBD;

type
  /// <summary>
  /// Compatibility per-monitor readiness entry.
  /// </summary>
  /// <remarks>
  /// New code should prefer <see cref="TOBDWWHReadinessSnapshot.Monitors"/>
  /// and <see cref="TOBDMonitorReadiness"/>. This record is retained so
  /// older consumers can still inspect a simple supported/complete list.
  /// </remarks>
  TOBDWWHGroupReadiness = record
    /// <summary>Monitor ordinal from <see cref="TOBDMonitor"/>.</summary>
    GroupId: Byte;
    /// <summary>True when the ECU reports the monitor as supported.</summary>
    Supported: Boolean;
    /// <summary>True when the monitor is complete.</summary>
    Complete: Boolean;
    /// <summary>Unused for J1979-2 monitor status; always 0xFFFF.</summary>
    Numerator: UInt16;
    /// <summary>Unused for J1979-2 monitor status; always 0xFFFF.</summary>
    Denominator: UInt16;
  end;

  /// <summary>
  /// Readiness snapshot returned by <see cref="TOBDWWHReadiness.Read"/>.
  /// </summary>
  TOBDWWHReadinessSnapshot = record
    /// <summary>Compatibility bitmap: supported monitors by TOBDMonitor ordinal.</summary>
    MajorGroupSupported: UInt16;
    /// <summary>Compatibility bitmap: complete monitors by TOBDMonitor ordinal.</summary>
    MajorGroupComplete: UInt16;
    /// <summary>Compatibility monitor list derived from DID F401.</summary>
    Groups: TArray<TOBDWWHGroupReadiness>;
    /// <summary>Decoded J1979 monitor status since DTCs were cleared (DID F401).</summary>
    StatusSinceClear: TOBDMonitorStatus;
    /// <summary>Decoded J1979 monitor status for this drive cycle (DID F441).</summary>
    StatusThisCycle: TOBDMonitorStatus;
    /// <summary>Monitor readiness since DTCs were cleared.</summary>
    Monitors: TArray<TOBDMonitorReadiness>;
    /// <summary>Monitor readiness for this drive cycle.</summary>
    ThisCycleMonitors: TArray<TOBDMonitorReadiness>;
    /// <summary>True when DID F401 was read and decoded successfully.</summary>
    Valid: Boolean;
    /// <summary>True when DID F441 was read and decoded successfully.</summary>
    ThisCycleValid: Boolean;
  end;

  /// <summary>Fires after a readiness snapshot. Main thread.</summary>
  TOBDWWHReadinessEvent = procedure(Sender: TObject;
    const ASnapshot: TOBDWWHReadinessSnapshot) of object;

  /// <summary>
  /// WWH-OBD / J1979-2 readiness component.
  /// </summary>
  /// <remarks>
  /// Assign <c>Protocol</c> to a connected UDS-capable protocol stack and
  /// call <see cref="Read"/>. DID F401 is mandatory for a valid snapshot;
  /// DID F441 is optional and fills the this-drive-cycle fields when present.
  /// DTC readiness-group DTC lists are exposed by <c>TOBDWWHOBD.ReadByGroup</c>
  /// using ISO 14229-1 sub-function 0x56.
  /// </remarks>
  TOBDWWHReadiness = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FOwnedTask: TOBDOwnedTask;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnSnapshot: TOBDWWHReadinessEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function ReadDID(ADID: Word): TBytes;
    function DoRead: TOBDWWHReadinessSnapshot;
    procedure FireSnapshot(const ASnapshot: TOBDWWHReadinessSnapshot);
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
    /// Reads the J1979-2 readiness snapshot.
    /// </summary>
    /// <returns>A snapshot with <c>Valid = True</c> when DID F401 was decoded.</returns>
    function Read: TOBDWWHReadinessSnapshot;

    /// <summary>Non-blocking <see cref="Read"/>.</summary>
    procedure ReadAsync;
  published
    /// <summary>Protocol stack to read through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires on snapshot completion. Main thread.</summary>
    property OnSnapshot: TOBDWWHReadinessEvent read FOnSnapshot
      write FOnSnapshot;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDWWHReadiness.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FOwnedTask := TOBDOwnedTask.Create;
end;

destructor TOBDWWHReadiness.Destroy;
begin
  if FOwnedTask <> nil then
    FOwnedTask.Cancel;
  FreeAndNil(FOwnedTask);
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDWWHReadiness.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FOwnedTask <> nil then
    FOwnedTask.Quiesce;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDWWHReadiness.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
  begin
    if FOwnedTask <> nil then
      FOwnedTask.Quiesce;
    FProtocol := nil;
  end;
end;

procedure TOBDWWHReadiness.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('Async start requires the main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDWWHReadiness: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDWWHReadiness.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDWWHReadiness.ReadDID(ADID: Word): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHReadiness: Protocol not assigned');
  Req := TBytes.Create(Hi(ADID), Lo(ADID));
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('ReadDID 0x%.4x negative: %s',
      [ADID, Resp.NRCText]);
  if (Length(Resp.Data) < 2) or (Resp.Data[0] <> Hi(ADID)) or
    (Resp.Data[1] <> Lo(ADID)) then
    raise EOBDProtocolErr.CreateFmt('ReadDID 0x%.4x: mismatched response',
      [ADID]);
  Result := Copy(Resp.Data, 2, Length(Resp.Data) - 2);
end;

function DecodeReadinessBody(const ABody: TBytes; APID: Byte;
  out AStatus: TOBDMonitorStatus): Boolean;
var
  ErrorText: string;
begin
  Result := DecodeMonitorStatus(OBDPIDPayload(ABody, APID, 4), AStatus,
    ErrorText);
end;

procedure PopulateCompatibility(var ASnapshot: TOBDWWHReadinessSnapshot);
var
  I, BitIndex: Integer;
  BitValue: UInt16;
begin
  ASnapshot.MajorGroupSupported := 0;
  ASnapshot.MajorGroupComplete := 0;
  SetLength(ASnapshot.Groups, Length(ASnapshot.Monitors));
  for I := 0 to High(ASnapshot.Monitors) do
  begin
    ASnapshot.Groups[I].GroupId := Ord(ASnapshot.Monitors[I].Monitor);
    ASnapshot.Groups[I].Supported := ASnapshot.Monitors[I].Supported;
    ASnapshot.Groups[I].Complete := ASnapshot.Monitors[I].Complete;
    ASnapshot.Groups[I].Numerator := $FFFF;
    ASnapshot.Groups[I].Denominator := $FFFF;
    BitIndex := Ord(ASnapshot.Monitors[I].Monitor);
    if BitIndex < 16 then
    begin
      BitValue := UInt16(UInt16(1) shl BitIndex);
      if ASnapshot.Monitors[I].Supported then
        ASnapshot.MajorGroupSupported := ASnapshot.MajorGroupSupported or
          BitValue;
      if ASnapshot.Monitors[I].Supported and ASnapshot.Monitors[I].Complete then
        ASnapshot.MajorGroupComplete := ASnapshot.MajorGroupComplete or
          BitValue;
    end;
  end;
end;

function TOBDWWHReadiness.DoRead: TOBDWWHReadinessSnapshot;
var
  Body: TBytes;
  Status: TOBDMonitorStatus;
begin
  Result := Default (TOBDWWHReadinessSnapshot);

  try
    Body := ReadDID(WWHOBD_DID_MonitorStatusSinceClear);
    if not DecodeReadinessBody(Body, $01, Status) then
      Exit;
    Result.StatusSinceClear := Status;
    Result.Monitors := Status.Monitors;
    Result.Valid := True;
    PopulateCompatibility(Result);
  except
    on E: Exception do
    begin
      Result.Valid := False;
      Exit;
    end;
  end;

  try
    Body := ReadDID(WWHOBD_DID_MonitorStatusThisCycle);
    if DecodeReadinessBody(Body, $41, Status) then
    begin
      Result.StatusThisCycle := Status;
      Result.ThisCycleMonitors := Status.Monitors;
      Result.ThisCycleValid := True;
    end;
  except
    // DID F441 is optional on older implementations.
  end;
end;

function TOBDWWHReadiness.Read: TOBDWWHReadinessSnapshot;
begin
  Result := DoRead;
  FireSnapshot(Result);
end;

procedure TOBDWWHReadiness.ReadAsync;
var
  Self_: TOBDWWHReadiness;
begin
  GuardSingleAsync;
  try
    Self_ := Self;
    FOwnedTask.Start(
      procedure
      begin
        try
          try
            Self_.Read;
          except
            on E: Exception do
              Self_.FireError(oeIO, E.Message);
          end;
        finally
          Self_.ReleaseAsync;
        end;
      end);
  except
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDWWHReadiness.FireSnapshot(const ASnapshot
  : TOBDWWHReadinessSnapshot);
var
  Self_: TOBDWWHReadiness;
  Snap: TOBDWWHReadinessSnapshot;
begin
  if not Assigned(FOnSnapshot) then
    Exit;
  Self_ := Self;
  Snap := ASnapshot;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnSnapshot(Self_, Snap)
  else
    FOwnedTask.Post(
      procedure
      begin
        if Assigned(Self_.FOnSnapshot) then
          Self_.FOnSnapshot(Self_, Snap);
      end);
end;

procedure TOBDWWHReadiness.FireError(ACode: TOBDErrorCode;
const AMessage: string);
var
  Self_: TOBDWWHReadiness;
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
    FOwnedTask.Post(
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
