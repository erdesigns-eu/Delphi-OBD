//------------------------------------------------------------------------------
//  OBD.Service.VehicleHealth
//
//  TOBDVehicleHealth — composite component that rolls up the
//  service-mode reads a typical diagnostic UI wants on a single
//  refresh: VIN + MIL status + confirmed / pending / permanent
//  DTCs + on-board monitor results + a handful of key live
//  values, into one <see cref="TOBDVehicleHealthReport"/> record.
//
//  Each section is best-effort: a failed read populates the
//  matching `*Error` field but does not abort the rest of the
//  snapshot. Hosts surface partial results as "we got X but Y
//  failed".
//
//  This component does NOT own the protocol — it consumes
//  whatever child components the host wires (any subset of VIN /
//  DTCs / LiveData / OnBoardMonitor / FreezeFrame). Sections
//  whose component is unassigned are skipped silently.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Algorithm
//                     re-derived from SAE J1979 / ISO 15031-5;
//                     no v1 code copied.
//------------------------------------------------------------------------------

unit OBD.Service.VehicleHealth;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol,
  OBD.Service.LiveData,
  OBD.Service.DTCs,
  OBD.Service.VIN,
  OBD.Service.FreezeFrame,
  OBD.Service.OnBoardMonitor;

type
  /// <summary>One <c>(PID, value)</c> tuple captured by the
  /// LiveValues read. <c>Value</c> mirrors the decoded shape
  /// that <see cref="TOBDLiveData.Read"/> returns.</summary>
  TOBDLiveValueSnapshot = record
    PID:    Byte;
    Value:  TOBDPIDValue;
    Failed: Boolean;
  end;

  /// <summary>
  ///   SAE J1979 / ISO 15031-5 Mode 01 PID 01 monitor-status
  ///   payload: MIL state + DTC count + which readiness monitors
  ///   the ECU supports + which ones have completed since the
  ///   last DTC clear.
  /// </summary>
  TOBDMILStatus = record
    /// <summary>True when the Malfunction Indicator Lamp is on.</summary>
    MILOn:                 Boolean;
    /// <summary>Confirmed DTC count the ECU reports alongside the
    /// MIL flag.</summary>
    DTCCount:              Byte;
    /// <summary>True when the ECU is a compression-ignition
    /// engine (diesel). The non-continuous monitor list differs
    /// between spark and diesel engines.</summary>
    IsDiesel:              Boolean;
    /// <summary>True when each continuous monitor (misfire / fuel
    /// system / components) is supported.</summary>
    MisfireSupported:      Boolean;
    FuelSystemSupported:   Boolean;
    ComponentsSupported:   Boolean;
    /// <summary>True when the matching continuous monitor has
    /// completed since the last DTC clear.</summary>
    MisfireComplete:       Boolean;
    FuelSystemComplete:    Boolean;
    ComponentsComplete:    Boolean;
    /// <summary>Bitmap of supported non-continuous monitors. The
    /// meaning of each bit depends on <c>IsDiesel</c>; consult
    /// SAE J1979 Table A2 for the mapping.</summary>
    NonContinuousSupport:  Byte;
    /// <summary>Bitmap of completed non-continuous monitors. Same
    /// bit layout as <c>NonContinuousSupport</c>.</summary>
    NonContinuousComplete: Byte;
  end;

  /// <summary>
  ///   Snapshot of one cycle of pulled diagnostics. Every section
  ///   is independent — `*Error` text is set when the
  ///   matching read raised, otherwise the section's data is
  ///   trustworthy.
  /// </summary>
  TOBDVehicleHealthReport = record
    /// <summary>Local wall-clock time when the snapshot
    /// started.</summary>
    Timestamp:        TDateTime;

    VIN:              string;
    VINError:         string;

    MIL:              TOBDMILStatus;
    MILValid:         Boolean;
    MILError:         string;

    ConfirmedDTCs:    TArray<TOBDDtcEntry>;
    ConfirmedError:   string;

    PendingDTCs:      TArray<TOBDDtcEntry>;
    PendingError:     string;

    PermanentDTCs:    TArray<TOBDDtcEntry>;
    PermanentError:   string;

    MonitorResults:   TArray<TOBDMonitorResult>;
    MonitorMID:       Byte;
    MonitorError:     string;

    /// <summary>Live values: array of <c>(PID, value)</c>
    /// tuples. Populated when the host sets
    /// <see cref="TOBDVehicleHealth.LivePIDs"/>.</summary>
    LiveValues:       TArray<TOBDLiveValueSnapshot>;
    LiveError:        string;

    /// <summary>True when the snapshot reads as
    /// "healthy": MIL off (or unknown), zero confirmed DTCs.
    /// Pending DTCs and incomplete readiness monitors do not
    /// flip this — they're informational.</summary>
    function IsHealthy: Boolean;
  end;

  /// <summary>Fires when an async snapshot completes.</summary>
  TOBDVehicleHealthEvent = procedure(Sender: TObject;
    const AReport: TOBDVehicleHealthReport) of object;

  /// <summary>
  ///   Composite roll-up component. Drop on a form, point each
  ///   sub-component reference at an existing TOBDVIN / TOBDDTCs
  ///   / TOBDLiveData / TOBDOnBoardMonitor / TOBDFreezeFrame on
  ///   the same form, optionally set <c>LivePIDs</c>, then call
  ///   <c>Snapshot</c> (sync) or <c>SnapshotAsync</c>.
  /// </summary>
  TOBDVehicleHealth = class(TComponent)
  strict private
    FProtocol:       TOBDProtocol;
    FVIN:            TOBDVIN;
    FDTCs:           TOBDDTCs;
    FLiveData:       TOBDLiveData;
    FOnBoardMonitor: TOBDOnBoardMonitor;
    FFreezeFrame:    TOBDFreezeFrame;
    FLivePIDs:       TBytes;
    FAsyncLock:      TCriticalSection;
    FAsyncInFlight:  Boolean;
    FOnReady:        TOBDVehicleHealthEvent;
    FOnError:        TOBDConnectionErrorEvent;

    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure FireReady(const AReport: TOBDVehicleHealthReport);
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure SetVIN(AValue: TOBDVIN);
    procedure SetDTCs(AValue: TOBDDTCs);
    procedure SetLiveData(AValue: TOBDLiveData);
    procedure SetOnBoardMonitor(AValue: TOBDOnBoardMonitor);
    procedure SetFreezeFrame(AValue: TOBDFreezeFrame);
    function  ReadMILStatus(out AStatus: TOBDMILStatus;
      out AError: string): Boolean;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Captures a snapshot synchronously.</summary>
    function Snapshot: TOBDVehicleHealthReport;
    /// <summary>Non-blocking <see cref="Snapshot"/>. Fires
    /// <c>OnReady</c> on completion (main thread).</summary>
    procedure SnapshotAsync;
  published
    /// <summary>Required for the MIL-status read (Mode 01 PID 01).
    /// All other reads use the bound sub-components, but the
    /// MIL bytes need a direct protocol round-trip.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property VIN: TOBDVIN read FVIN write SetVIN;
    property DTCs: TOBDDTCs read FDTCs write SetDTCs;
    property LiveData: TOBDLiveData read FLiveData write SetLiveData;
    property OnBoardMonitor: TOBDOnBoardMonitor
      read FOnBoardMonitor write SetOnBoardMonitor;
    property FreezeFrame: TOBDFreezeFrame
      read FFreezeFrame write SetFreezeFrame;
    /// <summary>List of PIDs to sample under <c>LiveValues</c>
    /// during a snapshot. Default: <c>0x0C</c> (RPM),
    /// <c>0x0D</c> (speed), <c>0x05</c> (coolant), <c>0x42</c>
    /// (control-module voltage).</summary>
    property LivePIDs: TBytes read FLivePIDs write FLivePIDs;
    property OnReady: TOBDVehicleHealthEvent read FOnReady write FOnReady;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

uses
  System.DateUtils;

{ ---- TOBDVehicleHealthReport ----------------------------------------------- }

function TOBDVehicleHealthReport.IsHealthy: Boolean;
begin
  Result := True;
  if MILValid and MIL.MILOn then Exit(False);
  if Length(ConfirmedDTCs) > 0 then Exit(False);
end;

{ ---- TOBDVehicleHealth ----------------------------------------------------- }

constructor TOBDVehicleHealth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FLivePIDs  := TBytes.Create($0C, $0D, $05, $42);
end;

destructor TOBDVehicleHealth.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDVehicleHealth.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDVehicleHealth.SetVIN(AValue: TOBDVIN);
begin
  if FVIN = AValue then Exit;
  if FVIN <> nil then FVIN.RemoveFreeNotification(Self);
  FVIN := AValue;
  if FVIN <> nil then FVIN.FreeNotification(Self);
end;

procedure TOBDVehicleHealth.SetDTCs(AValue: TOBDDTCs);
begin
  if FDTCs = AValue then Exit;
  if FDTCs <> nil then FDTCs.RemoveFreeNotification(Self);
  FDTCs := AValue;
  if FDTCs <> nil then FDTCs.FreeNotification(Self);
end;

procedure TOBDVehicleHealth.SetLiveData(AValue: TOBDLiveData);
begin
  if FLiveData = AValue then Exit;
  if FLiveData <> nil then FLiveData.RemoveFreeNotification(Self);
  FLiveData := AValue;
  if FLiveData <> nil then FLiveData.FreeNotification(Self);
end;

procedure TOBDVehicleHealth.SetOnBoardMonitor(AValue: TOBDOnBoardMonitor);
begin
  if FOnBoardMonitor = AValue then Exit;
  if FOnBoardMonitor <> nil then FOnBoardMonitor.RemoveFreeNotification(Self);
  FOnBoardMonitor := AValue;
  if FOnBoardMonitor <> nil then FOnBoardMonitor.FreeNotification(Self);
end;

procedure TOBDVehicleHealth.SetFreezeFrame(AValue: TOBDFreezeFrame);
begin
  if FFreezeFrame = AValue then Exit;
  if FFreezeFrame <> nil then FFreezeFrame.RemoveFreeNotification(Self);
  FFreezeFrame := AValue;
  if FFreezeFrame <> nil then FFreezeFrame.FreeNotification(Self);
end;

procedure TOBDVehicleHealth.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation <> opRemove then Exit;
  if AComponent = FProtocol       then FProtocol       := nil;
  if AComponent = FVIN            then FVIN            := nil;
  if AComponent = FDTCs           then FDTCs           := nil;
  if AComponent = FLiveData       then FLiveData       := nil;
  if AComponent = FOnBoardMonitor then FOnBoardMonitor := nil;
  if AComponent = FFreezeFrame    then FFreezeFrame    := nil;
end;

procedure TOBDVehicleHealth.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create(
        'TOBDVehicleHealth: snapshot already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDVehicleHealth.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDVehicleHealth.FireReady(
  const AReport: TOBDVehicleHealthReport);
var
  Self_: TOBDVehicleHealth;
  Snap:  TOBDVehicleHealthReport;
begin
  if not Assigned(FOnReady) then Exit;
  Self_ := Self;
  Snap  := AReport;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnReady(Self_, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnReady) then Self_.FOnReady(Self_, Snap);
      end);
end;

function TOBDVehicleHealth.ReadMILStatus(out AStatus: TOBDMILStatus;
  out AError: string): Boolean;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
  A, B, C, D: Byte;
begin
  Result := False;
  AStatus := Default(TOBDMILStatus);
  AError := '';
  if FProtocol = nil then
  begin
    AError := 'Protocol is not assigned';
    Exit;
  end;
  try
    // SAE J1979 Mode 01 PID 01 — MonitorStatusSinceCodesCleared.
    // Request: 01 01. Response data: 4 bytes A B C D.
    Req := MakeOBDRequest;
    Req.ServiceID := $01;
    Req.Data := TBytes.Create($01);
    Resp := FProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      AError := Format('NRC 0x%.2X — %s', [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    if Length(Resp.Data) < 4 then
    begin
      AError := Format('PID 01 response too short (%d bytes)',
        [Length(Resp.Data)]);
      Exit;
    end;
    A := Resp.Data[0];
    B := Resp.Data[1];
    C := Resp.Data[2];
    D := Resp.Data[3];

    AStatus.MILOn               := (A and $80) <> 0;
    AStatus.DTCCount            := A and $7F;
    AStatus.IsDiesel            := (B and $08) <> 0;
    AStatus.MisfireSupported    := (B and $01) <> 0;
    AStatus.FuelSystemSupported := (B and $02) <> 0;
    AStatus.ComponentsSupported := (B and $04) <> 0;
    AStatus.MisfireComplete     := (B and $10) = 0;  // bit set = NOT complete
    AStatus.FuelSystemComplete  := (B and $20) = 0;
    AStatus.ComponentsComplete  := (B and $40) = 0;
    AStatus.NonContinuousSupport  := C;
    AStatus.NonContinuousComplete := not D;          // bit set = NOT complete

    Result := True;
  except
    on E: Exception do
      AError := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDVehicleHealth.Snapshot: TOBDVehicleHealthReport;
var
  Live:    TArray<TOBDLiveValueSnapshot>;
  PID:     Byte;
  Item:    TOBDLiveValueSnapshot;
  I:       Integer;
  VINResult: TOBDVINResult;

  procedure SafeBlock(AAction: TProc; var AError: string);
  begin
    try
      AAction;
    except
      on E: Exception do
        AError := E.ClassName + ': ' + E.Message;
    end;
  end;

begin
  Result := Default(TOBDVehicleHealthReport);
  Result.Timestamp := Now;

  if FVIN <> nil then
    SafeBlock(
      procedure
      begin
        VINResult := FVIN.Read;
        Result.VIN := VINResult.RawVIN;
      end, Result.VINError)
  else
    Result.VINError := 'VIN component not assigned';

  if FProtocol <> nil then
  begin
    if not ReadMILStatus(Result.MIL, Result.MILError) then
      Result.MILValid := False
    else
      Result.MILValid := True;
  end
  else
    Result.MILError := 'Protocol not assigned';

  if FDTCs <> nil then
  begin
    SafeBlock(
      procedure
      begin
        Result.ConfirmedDTCs := FDTCs.ReadConfirmed;
      end, Result.ConfirmedError);
    SafeBlock(
      procedure
      begin
        Result.PendingDTCs := FDTCs.ReadPending;
      end, Result.PendingError);
    SafeBlock(
      procedure
      begin
        Result.PermanentDTCs := FDTCs.ReadPermanent;
      end, Result.PermanentError);
  end
  else
  begin
    Result.ConfirmedError := 'DTCs component not assigned';
    Result.PendingError   := 'DTCs component not assigned';
    Result.PermanentError := 'DTCs component not assigned';
  end;

  if (FOnBoardMonitor <> nil) and (Result.MonitorMID <> 0) then
    SafeBlock(
      procedure
      begin
        Result.MonitorResults := FOnBoardMonitor.Read(Result.MonitorMID);
      end, Result.MonitorError);

  if (FLiveData <> nil) and (Length(FLivePIDs) > 0) then
  begin
    SetLength(Live, Length(FLivePIDs));
    for I := 0 to High(FLivePIDs) do
    begin
      PID := FLivePIDs[I];
      Item := Default(TOBDLiveValueSnapshot);
      Item.PID := PID;
      try
        Item.Value := FLiveData.Read(PID);
      except
        on E: Exception do
        begin
          Item.Failed := True;
          if Result.LiveError = '' then
            Result.LiveError := Format('PID 0x%.2X: %s', [PID, E.Message]);
        end;
      end;
      Live[I] := Item;
    end;
    Result.LiveValues := Live;
  end;
end;

procedure TOBDVehicleHealth.SnapshotAsync;
var
  Self_: TOBDVehicleHealth;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    var
      Report: TOBDVehicleHealthReport;
    begin
      try
        try
          Report := Self_.Snapshot;
          Self_.FireReady(Report);
        except
          on E: Exception do
            if Assigned(Self_.FOnError) then
              TThread.Queue(nil,
                procedure
                var H: Boolean;
                begin
                  H := False;
                  Self_.FOnError(Self_, oeIO, E.Message, H);
                end);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

end.
