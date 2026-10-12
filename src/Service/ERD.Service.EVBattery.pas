// ------------------------------------------------------------------------------
// ERD.Service.EVBattery
//
// TOBDEVBattery - non-visual component that polls the
// high-voltage battery management system on supported BEV /
// PHEV platforms and decodes the per-vendor DID / PID set
// into a TOBDEVBatterySnapshot.
//
// The vendor-specific decode rules live in
// catalogs/ev-battery/<vendor>.json (see
// ERD.Service.EVBattery.Catalog). Set Vendor to the matching
// key, wire Protocol, call ReadSnapshot for a one-shot read
// or Start to drive the polling thread.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.Service.EVBattery;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Connection,
  ERD.Service.EVBattery.Request,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
{$IFNDEF FPC}Data.Bind.Components, System.Bindings.Helper, {$ENDIF}
  ERD.Errors,
  ERD.Types,
  ERD.Binary.Value,
  ERD.Connection.Types,
  ERD.Protocol.Types,
  ERD.Protocol,
  ERD.Service.EVBattery.Types,
  ERD.Service.EVBattery.Catalog;

type
  TOBDEVBatterySnapshotEvent = procedure(Sender: TObject;
    const ASnapshot: TOBDEVBatterySnapshot) of object;

  TOBDEVBatteryPollThread = class;

  TOBDEVBattery = class(TComponent)
  private
    FProtocol: TOBDProtocol;
    FVendor: string;
    FModelYear: Integer;
    FPollIntervalMs: Cardinal;
    FThread: TOBDEVBatteryPollThread;
    FOnSnapshot: TOBDEVBatterySnapshotEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure ReportError(ACode: TOBDErrorCode; const AMessage: string);
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
    procedure ApplyDecoded(const ARule: TOBDEVBatteryRule; const AData: TBytes;
      var ASnapshot: TOBDEVBatterySnapshot);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>Vendor catalogue key (e.g. <c>"hmg"</c>,
    /// <c>"nissan-leaf"</c>, <c>"bmw-i"</c>). Pre-shipped keys
    /// live under <c>catalogs/ev-battery/</c>.</summary>
    /// <summary>Required for catalog rules with a model-year-dependent layout.</summary>
    property ModelYear: Integer read FModelYear write FModelYear default 0;
    property Vendor: string read FVendor write FVendor;

    /// <summary>Live-mode poll interval. Default 2000 ms.</summary>
    property PollIntervalMs: Cardinal read FPollIntervalMs write FPollIntervalMs
      default 2000;

    property OnSnapshot: TOBDEVBatterySnapshotEvent read FOnSnapshot
      write FOnSnapshot;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

  TOBDEVBatteryPollThread = class(TThread)
  strict private
    FOwner: TOBDEVBattery;
    FStopEvent: TEvent;
    procedure FireSnapshotSync(const A: TOBDEVBatterySnapshot);
    procedure FireErrorSync(C: TOBDErrorCode; M: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TOBDEVBattery);
    destructor Destroy; override;
    procedure Stop;
  end;

implementation

{ ---- helpers ---------------------------------------------------------------- }

function SliceUInt(const AData: TBytes; AOffset, ALen: Integer;
  out AOk: Boolean): Int64;
begin
  AOk := False;
  Result := 0;
  try
    Result := DecodeIntegerBE(AData, AOffset, ALen, False);
    AOk := True;
  except
    on E: EOBDConfig do
      AOk := False;
  end;
end;

function SliceSignInt(const AData: TBytes; AOffset, ALen: Integer;
  out AOk: Boolean): Int64;
begin
  AOk := False;
  Result := 0;
  try
    Result := DecodeIntegerBE(AData, AOffset, ALen, True);
    AOk := True;
  except
    on E: EOBDConfig do
      AOk := False;
  end;
end;

{ ---- TOBDEVBattery --------------------------------------------------------- }

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
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDEVBattery.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDEVBattery.ReportError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  MessageCopy: string;
begin
  MessageCopy := AMessage;
  TThread.Synchronize(nil,
    procedure
    var
      Handled: Boolean;
    begin
      Handled := False;
      if Assigned(FOnError) then
        FOnError(Self, ACode, MessageCopy, Handled);
    end);
end;

function TOBDEVBattery.ReadOne(const ARule: TOBDEVBatteryRule;
out AError: string): TBytes;
var
  Req: TOBDRequest;
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
    Req := MakeEVBatteryRequest(ARule);
    Resp := FProtocol.Send(Req);
    Result := EVBatteryResponseData(ARule, Resp);
  except
    on E: Exception do
      AError := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TOBDEVBattery.ApplyDecoded(const ARule: TOBDEVBatteryRule;
const AData: TBytes; var ASnapshot: TOBDEVBatterySnapshot);
var
  Ok: Boolean;
  Raw: Int64;
  Phys: Double;
  Arr: TArray<Single>;
  I, N: Integer;
  Cnt: Integer;
  Decoded: TOBDEVDecodedField;

  procedure SaveDecoded;
  var
    Index: Integer;
  begin
    Index := Length(ASnapshot.DecodedFields);
    SetLength(ASnapshot.DecodedFields, Index + 1);
    ASnapshot.DecodedFields[Index] := Decoded;
  end;

begin
  Decoded := Default (TOBDEVDecodedField);
  Decoded.Name := ARule.FieldName;
  Decoded.Unit_ := ARule.Unit_;
  if ARule.IsArray then
  begin
    if (ARule.ElementSize <= 0) or (ARule.Offset < 0) or (ARule.Length <= 0) or
      (ARule.Offset > Length(AData)) or
      (ARule.Length > Length(AData) - ARule.Offset) or
      (ARule.Length mod ARule.ElementSize <> 0) then
      raise EOBDProtocolErr.Create
        ('EV array payload is truncated or misaligned');
    Cnt := ARule.Length div ARule.ElementSize;
    if Cnt <= 0 then
      raise EOBDProtocolErr.Create('EV array payload is truncated');
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
    Decoded.Values := Arr;
    SaveDecoded;
    case ARule.Field of
      efkCellVoltagesArray:
        begin
          // APPEND mode: HMG splits the per-cell array across
          // four DIDs (1..32 / 33..64 / 65..96 / 97..98). The
          // catalogue lists each as a separate rule with
          // IsArray=True; we extend the existing snapshot
          // array rather than overwriting it.
          ASnapshot.HasCellVoltages := True;
          N := Length(ASnapshot.CellVoltages);
          SetLength(ASnapshot.CellVoltages, N + Length(Arr));
          for I := 0 to High(Arr) do
            ASnapshot.CellVoltages[N + I] := Arr[I];
          // Re-derive min/max/avg if the catalogue didn't
          // provide them explicitly.
          if not ASnapshot.HasCellVoltageMin then
          begin
            N := Length(ASnapshot.CellVoltages);
            if N > 0 then
            begin
              ASnapshot.HasCellVoltageMin := True;
              ASnapshot.HasCellVoltageMax := True;
              ASnapshot.HasCellVoltageAvg := True;
              ASnapshot.CellVoltageMin := ASnapshot.CellVoltages[0];
              ASnapshot.CellVoltageMax := ASnapshot.CellVoltages[0];
              Phys := 0;
              for I := 0 to N - 1 do
              begin
                if ASnapshot.CellVoltages[I] < ASnapshot.CellVoltageMin then
                  ASnapshot.CellVoltageMin := ASnapshot.CellVoltages[I];
                if ASnapshot.CellVoltages[I] > ASnapshot.CellVoltageMax then
                  ASnapshot.CellVoltageMax := ASnapshot.CellVoltages[I];
                Phys := Phys + ASnapshot.CellVoltages[I];
              end;
              ASnapshot.CellVoltageAvg := Phys / N;
            end;
          end;
        end;
      efkModuleTempArray:
        begin
          // APPEND mode same as cell voltages.
          ASnapshot.HasModuleTemps := True;
          N := Length(ASnapshot.ModuleTempsC);
          SetLength(ASnapshot.ModuleTempsC, N + Length(Arr));
          for I := 0 to High(Arr) do
            ASnapshot.ModuleTempsC[N + I] := Arr[I];
        end;
    end;
    Exit;
  end;

  if ARule.Signed then
    Raw := SliceSignInt(AData, ARule.Offset, ARule.Length, Ok)
  else
    Raw := SliceUInt(AData, ARule.Offset, ARule.Length, Ok);
  if not Ok then
    raise EOBDProtocolErr.Create
      ('EV scalar payload is truncated or outside integer range');
  Phys := Raw * ARule.Scale + ARule.OffsetVal;
  Decoded.Value := Phys;
  SaveDecoded;

  case ARule.Field of
    efkCapacityRemainingAh:
      begin
        ASnapshot.HasCapacityRemainingAh := True;
        ASnapshot.CapacityRemainingAh := Phys;
      end;
    efkSOC:
      begin
        ASnapshot.HasSOC := True;
        ASnapshot.SOC := Phys;
      end;
    efkSOH:
      begin
        ASnapshot.HasSOH := True;
        ASnapshot.SOH := Phys;
      end;
    efkPackVoltage:
      begin
        ASnapshot.HasPackVoltage := True;
        ASnapshot.PackVoltage := Phys;
      end;
    efkPackCurrent:
      begin
        ASnapshot.HasPackCurrent := True;
        ASnapshot.PackCurrent := Phys;
      end;
    efkPackPower:
      begin
        ASnapshot.HasPackPower := True;
        ASnapshot.PackPower := Phys;
      end;
    efkCapacityRemainingKwh:
      begin
        ASnapshot.HasCapacityRemaining := True;
        ASnapshot.CapacityRemainingKwh := Phys;
      end;
    efkCapacityNominalKwh:
      begin
        ASnapshot.HasCapacityNominal := True;
        ASnapshot.CapacityNominalKwh := Phys;
      end;
    efkCellVoltageMin:
      begin
        ASnapshot.HasCellVoltageMin := True;
        ASnapshot.CellVoltageMin := Phys;
      end;
    efkCellVoltageMax:
      begin
        ASnapshot.HasCellVoltageMax := True;
        ASnapshot.CellVoltageMax := Phys;
      end;
    efkCellVoltageAvg:
      begin
        ASnapshot.HasCellVoltageAvg := True;
        ASnapshot.CellVoltageAvg := Phys;
      end;
    efkPackTempMin:
      begin
        ASnapshot.HasPackTempMin := True;
        ASnapshot.PackTempMinC := Phys;
      end;
    efkPackTempMax:
      begin
        ASnapshot.HasPackTempMax := True;
        ASnapshot.PackTempMaxC := Phys;
      end;
    efkInletCoolantTemp:
      begin
        ASnapshot.HasInletCoolant := True;
        ASnapshot.InletCoolantTempC := Phys;
      end;
    efkOutletCoolantTemp:
      begin
        ASnapshot.HasOutletCoolant := True;
        ASnapshot.OutletCoolantTempC := Phys;
      end;
    efkRangeKm:
      begin
        ASnapshot.HasRangeKm := True;
        ASnapshot.RangeKm := Phys;
      end;
    efkOdometerKm:
      begin
        ASnapshot.HasOdometerKm := True;
        ASnapshot.OdometerKm := Cardinal(Round(Phys));
      end;
    efkChargeState:
      begin
        ASnapshot.HasChargeState := True;
        // Numeric mode: 0=idle, 1=AC, 2=DC, 3=drive, 4=regen.
        case Round(Phys) of
          0:
            ASnapshot.ChargeState := csIdle;
          1:
            ASnapshot.ChargeState := csACCharging;
          2:
            ASnapshot.ChargeState := csDCFastCharging;
          3:
            ASnapshot.ChargeState := csDriving;
          4:
            ASnapshot.ChargeState := csRegenBraking;
        else
          ASnapshot.ChargeState := csUnknown;
        end;
      end;
    efkChargePortTemp:
      begin
        ASnapshot.HasChargePortTemp := True;
        ASnapshot.ChargePortTempC := Phys;
      end;
    efkChargingPowerKw:
      begin
        ASnapshot.HasChargingPower := True;
        ASnapshot.ChargingPowerKw := Phys;
      end;

    // Per-module / per-pack scalar temps land here when the
    // catalogue ships them as one rule per module rather than
    // as a single array. Append into ModuleTempsC.
    efkModuleTempArray:
      begin
        ASnapshot.HasModuleTemps := True;
        N := Length(ASnapshot.ModuleTempsC);
        SetLength(ASnapshot.ModuleTempsC, N + 1);
        ASnapshot.ModuleTempsC[N] := Phys;
      end;

    // Auxiliary / extended fields.
    efkAuxBatteryVoltage:
      begin
        ASnapshot.HasAuxBatteryVoltage := True;
        ASnapshot.AuxBatteryVoltage := Phys;
      end;
    efkAvailableChargePowerKw:
      begin
        ASnapshot.HasAvailableChargePower := True;
        ASnapshot.AvailableChargePowerKw := Phys;
      end;
    efkAvailableDischargePowerKw:
      begin
        ASnapshot.HasAvailableDischargePower := True;
        ASnapshot.AvailableDischargePowerKw := Phys;
      end;
    efkCumulativeEnergyChargedKwh:
      begin
        ASnapshot.HasCumulativeChargedKwh := True;
        ASnapshot.CumulativeChargedKwh := Phys;
      end;
    efkCumulativeEnergyDischargedKwh:
      begin
        ASnapshot.HasCumulativeDischargedKwh := True;
        ASnapshot.CumulativeDischargedKwh := Phys;
      end;
  end;
end;

function TOBDEVBattery.ReadSnapshot: TOBDEVBatterySnapshot;
var
  Cat: TOBDEVBatteryVendorCatalog;
  Rule: TOBDEVBatteryRule;
  Data: TBytes;
  Err: string;
  Errs: TList<string>;
begin
  Result := Default (TOBDEVBatterySnapshot);
  Result.Timestamp := Now;
  Result.Vendor := FVendor;
  if not TOBDEVBatteryCatalog.TryGet(FVendor, Cat) then
    raise EOBDConfig.CreateFmt
      ('TOBDEVBattery: vendor catalogue "%s" not loaded - check ' +
      'catalogs/ev-battery/<vendor>.json', [FVendor]);
  for Rule in Cat.Rules do
    if ((Rule.MinModelYear > 0) or (Rule.MaxModelYear > 0)) and (FModelYear = 0)
    then
      raise EOBDConfig.Create
        ('Set ModelYear before reading model-dependent EV rules');
  Errs := TList<string>.Create;
  try
    for Rule in Cat.Rules do
    begin
      if not EVBatteryRuleApplies(Rule, FModelYear) then
        Continue;
      Data := ReadOne(Rule, Err);
      if Err <> '' then
      begin
        Errs.Add(Format('%s: %s', [Rule.FieldName, Err]));
        ReportError(oeIO, Format('field %s: %s', [Rule.FieldName, Err]));
        Continue;
      end;
      try
        ApplyDecoded(Rule, Data, Result);
      except
        on E: EOBDProtocolErr do
        begin
          Err := Format('%s: %s', [Rule.FieldName, E.Message]);
          Errs.Add(Err);
          ReportError(oeIO, Err);
        end;
      end;
    end;
    Result.Errors := Errs.ToArray;
  finally
    Errs.Free;
  end;
  // LiveBindings refresh — the synchronous Read path doesn't go
  // through FOnSnapshot (that's for the poll thread), so notify
  // here so a TLinkPropertyToField bound to one of the snapshot
  // fields picks up the new state.
  try
{$IFNDEF FPC}TBindings.Notify(Self, ''); {$ENDIF}
  except
  end;
end;

procedure TOBDEVBattery.Start;
begin
  if FThread <> nil then
    Exit;
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDEVBattery.Start: Protocol not assigned');
  if FVendor = '' then
    raise EOBDConfig.Create('TOBDEVBattery.Start: Vendor not set');
  FThread := TOBDEVBatteryPollThread.Create(Self);
end;

procedure TOBDEVBattery.Stop;
begin
  if FThread = nil then
    Exit;
  FThread.Stop;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

function TOBDEVBattery.Running: Boolean;
begin
  Result := FThread <> nil;
end;

{ TOBDEVBatteryPollThread ---------------------------------------------------- }

constructor TOBDEVBatteryPollThread.Create(AOwner: TOBDEVBattery);
begin
  FOwner := AOwner;
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

procedure TOBDEVBatteryPollThread.FireSnapshotSync
  (const A: TOBDEVBatterySnapshot);
begin
  Synchronize(
    procedure
    begin
      try
{$IFNDEF FPC}TBindings.Notify(FOwner, ''); {$ENDIF}
      except
      end;
      if Assigned(FOwner.FOnSnapshot) then
        FOwner.FOnSnapshot(FOwner, A);
    end);
end;

procedure TOBDEVBatteryPollThread.FireErrorSync(C: TOBDErrorCode; M: string);
begin
  if Assigned(FOwner.FOnError) then
    Synchronize(
      procedure
      begin
        FOwner.ReportError(C, M);
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
