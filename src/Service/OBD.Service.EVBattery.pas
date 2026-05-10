//------------------------------------------------------------------------------
//  OBD.Service.EVBattery
//
//  TOBDEVBattery - non-visual component that polls the
//  high-voltage battery management system on supported BEV /
//  PHEV platforms and decodes the per-vendor DID / PID set
//  into a TOBDEVBatterySnapshot.
//
//  The vendor-specific decode rules live in
//  catalogs/ev-battery/<vendor>.json (see
//  OBD.Service.EVBattery.Catalog). Set Vendor to the matching
//  key, wire Protocol, call ReadSnapshot for a one-shot read
//  or Start to drive the polling thread.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Service.EVBattery;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Errors,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Protocol.Types,
  OBD.Protocol,
  OBD.Service.EVBattery.Types,
  OBD.Service.EVBattery.Catalog;

type
  TOBDEVBatterySnapshotEvent = procedure(Sender: TObject;
    const ASnapshot: TOBDEVBatterySnapshot) of object;

  TOBDEVBatteryPollThread = class;

  TOBDEVBattery = class(TComponent)
  strict private
    FProtocol:        TOBDProtocol;
    FVendor:          string;
    FPollIntervalMs:  Cardinal;
    FThread:          TOBDEVBatteryPollThread;
    FOnSnapshot:      TOBDEVBatterySnapshotEvent;
    FOnError:         TOBDConnectionErrorEvent;
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    /// <summary>Issues one diagnostic read for <c>ARule</c> and
    /// returns the response payload bytes (after stripping the
    /// service / DID echo). Returns nil + AError set on failure.</summary>
    function ReadOne(const ARule: TOBDEVBatteryRule;
      out AError: string): TBytes;

    /// <summary>Decodes <c>AData</c> per <c>ARule</c> and stores
    /// the result on <c>ASnapshot</c>.</summary>
    procedure ApplyDecoded(const ARule: TOBDEVBatteryRule;
      const AData: TBytes;
      var ASnapshot: TOBDEVBatterySnapshot);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    /// <summary>One-shot read: walks every rule in the loaded
    /// vendor catalogue, populates a snapshot, returns it.
    /// Per-rule failures land in <c>Snapshot.Errors</c> rather
    /// than aborting the rest.</summary>
    function ReadSnapshot: TOBDEVBatterySnapshot;

    /// <summary>Start polling. Fires <see cref="OnSnapshot"/>
    /// every <see cref="PollIntervalMs"/>. Idempotent.</summary>
    procedure Start;

    /// <summary>Stop the polling thread. Joins.</summary>
    procedure Stop;

    function Running: Boolean;
  published
    /// <summary>Required - source of the bus reads.</summary>
    property Protocol: TOBDProtocol
      read FProtocol write SetProtocol;

    /// <summary>Vendor catalogue key (e.g. <c>"hmg"</c>,
    /// <c>"nissan-leaf"</c>, <c>"bmw-i"</c>). Pre-shipped keys
    /// live under <c>catalogs/ev-battery/</c>.</summary>
    property Vendor: string read FVendor write FVendor;

    /// <summary>Live-mode poll interval. Default 2000 ms.</summary>
    property PollIntervalMs: Cardinal
      read FPollIntervalMs write FPollIntervalMs default 2000;

    property OnSnapshot: TOBDEVBatterySnapshotEvent
      read FOnSnapshot write FOnSnapshot;
    property OnError: TOBDConnectionErrorEvent
      read FOnError write FOnError;
  end;

  TOBDEVBatteryPollThread = class(TThread)
  strict private
    FOwner:     TOBDEVBattery;
    FStopEvent: TEvent;
    procedure FireSnapshotSync(const A: TOBDEVBatterySnapshot);
    procedure FireErrorSync(C: TOBDErrorCode; const M: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TOBDEVBattery);
    destructor  Destroy; override;
    procedure   Stop;
  end;

implementation

{ ---- helpers ----------------------------------------------------------------}

function SliceUInt(const AData: TBytes; AOffset, ALen: Integer;
  out AOk: Boolean): Int64;
var I: Integer;
begin
  AOk := False;
  Result := 0;
  if (AOffset < 0) or (ALen <= 0) then Exit;
  if AOffset + ALen > Length(AData) then Exit;
  for I := 0 to ALen - 1 do
    Result := (Result shl 8) or AData[AOffset + I];
  AOk := True;
end;

function SliceSignInt(const AData: TBytes; AOffset, ALen: Integer;
  out AOk: Boolean): Int64;
var
  Raw: Int64;
  SignBit: Int64;
begin
  Raw := SliceUInt(AData, AOffset, ALen, AOk);
  if not AOk then Exit(0);
  SignBit := Int64(1) shl (ALen * 8 - 1);
  if (Raw and SignBit) <> 0 then
    Raw := Raw - (Int64(1) shl (ALen * 8));
  Result := Raw;
end;

{ ---- TOBDEVBattery ---------------------------------------------------------}

constructor TOBDEVBattery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPollIntervalMs := 2000;
end;

destructor TOBDEVBattery.Destroy;
begin
  Stop;
  inherited;
end;

procedure TOBDEVBattery.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDEVBattery.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

function TOBDEVBattery.ReadOne(const ARule: TOBDEVBatteryRule;
  out AError: string): TBytes;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
begin
  Result := nil;
  AError := '';
  if FProtocol = nil then
  begin
    AError := 'Protocol not assigned';
    Exit;
  end;
  try
    Req := MakeOBDRequest;
    Req.ServiceID := ARule.Service;
    case ARule.Service of
      $22:
        Req.Data := TBytes.Create(Hi(ARule.DIDOrPID), Lo(ARule.DIDOrPID));
      $21, $01:
        Req.Data := TBytes.Create(Lo(ARule.DIDOrPID));
    else
      Req.Data := TBytes.Create(Hi(ARule.DIDOrPID), Lo(ARule.DIDOrPID));
    end;
    Resp := FProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      AError := Format('NRC 0x%.2X - %s', [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result := Resp.Data;
  except
    on E: Exception do
      AError := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TOBDEVBattery.ApplyDecoded(const ARule: TOBDEVBatteryRule;
  const AData: TBytes; var ASnapshot: TOBDEVBatterySnapshot);
var
  Ok:    Boolean;
  Raw:   Int64;
  Phys:  Double;
  Arr:   TArray<Single>;
  I, N:  Integer;
  Cnt:   Integer;
begin
  if ARule.IsArray then
  begin
    if (ARule.ElementSize <= 0) or (ARule.Offset >= Length(AData)) then Exit;
    Cnt := (Length(AData) - ARule.Offset) div ARule.ElementSize;
    if Cnt <= 0 then Exit;
    SetLength(Arr, Cnt);
    for I := 0 to Cnt - 1 do
    begin
      if ARule.Signed then
        Raw := SliceSignInt(AData, ARule.Offset + I * ARule.ElementSize,
                            ARule.ElementSize, Ok)
      else
        Raw := SliceUInt(AData, ARule.Offset + I * ARule.ElementSize,
                         ARule.ElementSize, Ok);
      if Ok then
        Arr[I] := Raw * ARule.Scale + ARule.OffsetVal;
    end;
    case ARule.Field of
      efkCellVoltagesArray:
        begin
          ASnapshot.HasCellVoltages := True;
          ASnapshot.CellVoltages    := Arr;
          // Auto-derive min/max/avg if the catalogue didn't
          // provide them explicitly.
          if not ASnapshot.HasCellVoltageMin then
          begin
            N := Length(Arr);
            if N > 0 then
            begin
              ASnapshot.HasCellVoltageMin := True;
              ASnapshot.HasCellVoltageMax := True;
              ASnapshot.HasCellVoltageAvg := True;
              ASnapshot.CellVoltageMin := Arr[0];
              ASnapshot.CellVoltageMax := Arr[0];
              Phys := 0;
              for I := 0 to N - 1 do
              begin
                if Arr[I] < ASnapshot.CellVoltageMin then ASnapshot.CellVoltageMin := Arr[I];
                if Arr[I] > ASnapshot.CellVoltageMax then ASnapshot.CellVoltageMax := Arr[I];
                Phys := Phys + Arr[I];
              end;
              ASnapshot.CellVoltageAvg := Phys / N;
            end;
          end;
        end;
      efkModuleTempArray:
        begin
          ASnapshot.HasModuleTemps := True;
          ASnapshot.ModuleTempsC   := Arr;
        end;
    end;
    Exit;
  end;

  if ARule.Signed then
    Raw := SliceSignInt(AData, ARule.Offset, ARule.Length, Ok)
  else
    Raw := SliceUInt(AData, ARule.Offset, ARule.Length, Ok);
  if not Ok then Exit;
  Phys := Raw * ARule.Scale + ARule.OffsetVal;

  case ARule.Field of
    efkSOC:                  begin ASnapshot.HasSOC := True;               ASnapshot.SOC := Phys; end;
    efkSOH:                  begin ASnapshot.HasSOH := True;               ASnapshot.SOH := Phys; end;
    efkPackVoltage:          begin ASnapshot.HasPackVoltage := True;       ASnapshot.PackVoltage := Phys; end;
    efkPackCurrent:          begin ASnapshot.HasPackCurrent := True;       ASnapshot.PackCurrent := Phys; end;
    efkPackPower:            begin ASnapshot.HasPackPower := True;         ASnapshot.PackPower := Phys; end;
    efkCapacityRemainingKwh: begin ASnapshot.HasCapacityRemaining := True; ASnapshot.CapacityRemainingKwh := Phys; end;
    efkCapacityNominalKwh:   begin ASnapshot.HasCapacityNominal := True;   ASnapshot.CapacityNominalKwh := Phys; end;
    efkCellVoltageMin:       begin ASnapshot.HasCellVoltageMin := True;    ASnapshot.CellVoltageMin := Phys; end;
    efkCellVoltageMax:       begin ASnapshot.HasCellVoltageMax := True;    ASnapshot.CellVoltageMax := Phys; end;
    efkCellVoltageAvg:       begin ASnapshot.HasCellVoltageAvg := True;    ASnapshot.CellVoltageAvg := Phys; end;
    efkPackTempMin:          begin ASnapshot.HasPackTempMin := True;       ASnapshot.PackTempMinC := Phys; end;
    efkPackTempMax:          begin ASnapshot.HasPackTempMax := True;       ASnapshot.PackTempMaxC := Phys; end;
    efkInletCoolantTemp:     begin ASnapshot.HasInletCoolant := True;      ASnapshot.InletCoolantTempC := Phys; end;
    efkOutletCoolantTemp:    begin ASnapshot.HasOutletCoolant := True;     ASnapshot.OutletCoolantTempC := Phys; end;
    efkRangeKm:              begin ASnapshot.HasRangeKm := True;           ASnapshot.RangeKm := Phys; end;
    efkOdometerKm:           begin ASnapshot.HasOdometerKm := True;        ASnapshot.OdometerKm := Cardinal(Round(Phys)); end;
    efkChargeState:
      begin
        ASnapshot.HasChargeState := True;
        // Numeric mode: 0=idle, 1=AC, 2=DC, 3=drive, 4=regen.
        case Round(Phys) of
          0: ASnapshot.ChargeState := csIdle;
          1: ASnapshot.ChargeState := csACCharging;
          2: ASnapshot.ChargeState := csDCFastCharging;
          3: ASnapshot.ChargeState := csDriving;
          4: ASnapshot.ChargeState := csRegenBraking;
        else ASnapshot.ChargeState := csUnknown;
        end;
      end;
    efkChargePortTemp:       begin ASnapshot.HasChargePortTemp := True;    ASnapshot.ChargePortTempC := Phys; end;
    efkChargingPowerKw:      begin ASnapshot.HasChargingPower := True;     ASnapshot.ChargingPowerKw := Phys; end;
  end;
end;

function TOBDEVBattery.ReadSnapshot: TOBDEVBatterySnapshot;
var
  Cat:    TOBDEVBatteryVendorCatalog;
  Rule:   TOBDEVBatteryRule;
  Data:   TBytes;
  Err:    string;
  Errs:   TList<string>;
begin
  Result := Default(TOBDEVBatterySnapshot);
  Result.Timestamp := Now;
  Result.Vendor    := FVendor;
  if not TOBDEVBatteryCatalog.TryGet(FVendor, Cat) then
    raise EOBDConfig.CreateFmt(
      'TOBDEVBattery: vendor catalogue "%s" not loaded - check ' +
      'catalogs/ev-battery/<vendor>.json', [FVendor]);
  Errs := TList<string>.Create;
  try
    for Rule in Cat.Rules do
    begin
      Data := ReadOne(Rule, Err);
      if Err <> '' then
      begin
        Errs.Add(Format('%s: %s', [Rule.FieldName, Err]));
        if Assigned(FOnError) then
          FOnError(Self, oeIO,
            Format('field %s: %s', [Rule.FieldName, Err]));
        Continue;
      end;
      ApplyDecoded(Rule, Data, Result);
    end;
    Result.Errors := Errs.ToArray;
  finally
    Errs.Free;
  end;
end;

procedure TOBDEVBattery.Start;
begin
  if FThread <> nil then Exit;
  if FProtocol = nil then
    raise EOBDConfig.Create(
      'TOBDEVBattery.Start: Protocol not assigned');
  if FVendor = '' then
    raise EOBDConfig.Create(
      'TOBDEVBattery.Start: Vendor not set');
  FThread := TOBDEVBatteryPollThread.Create(Self);
end;

procedure TOBDEVBattery.Stop;
begin
  if FThread = nil then Exit;
  FThread.Stop;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

function TOBDEVBattery.Running: Boolean;
begin
  Result := FThread <> nil;
end;

{ TOBDEVBatteryPollThread ----------------------------------------------------}

constructor TOBDEVBatteryPollThread.Create(AOwner: TOBDEVBattery);
begin
  FOwner     := AOwner;
  FStopEvent := TEvent.Create(nil, True, False, '');
  inherited Create(False);
end;

destructor TOBDEVBatteryPollThread.Destroy;
begin
  FStopEvent.Free;
  inherited;
end;

procedure TOBDEVBatteryPollThread.Stop;
begin
  Terminate;
  FStopEvent.SetEvent;
end;

procedure TOBDEVBatteryPollThread.FireSnapshotSync(
  const A: TOBDEVBatterySnapshot);
begin
  if Assigned(FOwner.FOnSnapshot) then
    Synchronize(procedure
    begin
      FOwner.FOnSnapshot(FOwner, A);
    end);
end;

procedure TOBDEVBatteryPollThread.FireErrorSync(C: TOBDErrorCode;
  const M: string);
begin
  if Assigned(FOwner.FOnError) then
    Synchronize(procedure
    begin
      FOwner.FOnError(FOwner, C, M);
    end);
end;

procedure TOBDEVBatteryPollThread.Execute;
var
  Snap: TOBDEVBatterySnapshot;
begin
  while not Terminated do
  begin
    try
      Snap := FOwner.ReadSnapshot;
      FireSnapshotSync(Snap);
    except
      on E: Exception do
        FireErrorSync(oeIO, E.ClassName + ': ' + E.Message);
    end;
    if FStopEvent.WaitFor(FOwner.FPollIntervalMs) = wrSignaled then
      Break;
  end;
end;

end.
