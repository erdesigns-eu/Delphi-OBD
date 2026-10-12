//------------------------------------------------------------------------------
//  untPageEV
//
//  EV battery page for OBD-Studio Personal. It selects a high-voltage
//  battery catalogue profile, starts the shared EV battery reader only while
//  the page is active and renders health, pack and cell-voltage data.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untPageEV;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils,
  System.StrUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  ERD.I18n,
  ERD.Types,
  ERD.Connection,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Chips,
  ERD.UI.Card,
  ERD.UI.Buttons,
  ERD.UI.ValueTile,
  ERD.UI.TrendChart,
  ERD.UI.CellMatrix,
  ERD.UI.KeyValueList,
  ERD.UI.Edits,
  ERD.Service.EVBattery,
  ERD.Service.EVBattery.Types,
  ERD.Service.EVBattery.Catalog,
  untPersonalPage;

type
  TfrmPageEV = class(TFrame, IPersonalPage)
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblSubtitle: TLabel;
    lblVendor: TLabel;
    cmbVendor: TOBDComboBox;
    btnReadAgain: TOBDButton;
    bannerState: TOBDBanner;
    pnlTiles: TPanel;
    tileSOH: TOBDValueTile;
    tileSOC: TOBDValueTile;
    tilePackVoltage: TOBDValueTile;
    tilePackCurrent: TOBDValueTile;
    tileTemperature: TOBDValueTile;
    tileCellDelta: TOBDValueTile;
    pnlBody: TPanel;
    cellMatrix: TOBDCellMatrix;
    pnlRight: TPanel;
    cardDetails: TOBDCard;
    kvDetails: TOBDKeyValueList;
    chtHealth: TOBDTrendChart;
    procedure btnReadAgainClick(Sender: TObject);
    procedure cmbVendorChange(Sender: TObject);
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FManualVendor: Boolean;
    FReading: Boolean;
    FLastSnapshot: TOBDEVBatterySnapshot;
    procedure PopulateVendors;
    procedure StartPolling;
    procedure StopPolling;
    procedure BatterySnapshot(Sender: TObject;
      const ASnapshot: TOBDEVBatterySnapshot);
    procedure BatteryError(Sender: TObject; ACode: TOBDErrorCode;
      const AMessage: string; var AHandled: Boolean);
    procedure UpdateSnapshot(const ASnapshot: TOBDEVBatterySnapshot);
    procedure UpdateDetails(const ASnapshot: TOBDEVBatterySnapshot);
    procedure UpdateCellMatrix(const ASnapshot: TOBDEVBatterySnapshot);
    procedure UpdateBannerUnsupported;
    procedure UpdateBannerOffline;
    procedure UpdateBannerReading;
    procedure UpdateBannerLive(const ASnapshot: TOBDEVBatterySnapshot);
    procedure ConfigureChart;
    function SelectVendorFromVIN: Boolean;
    function CurrentVendor: string;
    function CurrentModelYear: Integer;
    function VendorFromVIN(const AVIN: string): string;
    function ModelYearFromVIN(const AVIN: string): Integer;
    function VendorAvailable(const AVendor: string): Boolean;
    function SnapshotHasUsefulData(const ASnapshot: TOBDEVBatterySnapshot): Boolean;
    function TemperatureText(const ASnapshot: TOBDEVBatterySnapshot): string;
    function CellSummary(const ASnapshot: TOBDEVBatterySnapshot; out AMin,
      AMax, AAvg: Double): Boolean;
    function ChargeStateText(AState: TOBDEVChargeState): string;
    function FormatValue(AValue: Double; const AUnit: string;
      ADecimals: Integer): string;
  public
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
  end;

implementation

{$R *.dfm}

const
  EV_POLL_MS = 3000;

{ TfrmPageEV }

function TfrmPageEV.PageKey: string;
begin
  Result := PAGE_EV;
end;

procedure TfrmPageEV.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  PopulateVendors;
  ConfigureChart;
  Translate;
  UpdateBannerOffline;
  UpdateDetails(Default (TOBDEVBatterySnapshot));
end;

procedure TfrmPageEV.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive then
  begin
    if Assigned(FHost) and FHost.Connected then
      StartPolling
    else
      UpdateBannerOffline;
  end
  else
    StopPolling;
end;

procedure TfrmPageEV.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
  begin
    StopPolling;
    UpdateBannerOffline;
  end
  else if FActive then
    StartPolling;
end;

procedure TfrmPageEV.UnitsChanged;
begin
  UpdateSnapshot(FLastSnapshot);
end;

procedure TfrmPageEV.Translate;
begin
  lblTitle.Caption := Tr('personal.ev.title', 'EV battery');
  lblSubtitle.Caption := Tr('personal.ev.subtitle',
    'High-voltage battery health and cell voltages');
  lblVendor.Caption := Tr('personal.ev.vendor', 'Vendor profile');
  btnReadAgain.Caption := Tr('personal.ev.readAgain', 'Read again');
  tileSOH.Caption := Tr('personal.ev.soh', 'State of health');
  tileSOC.Caption := Tr('personal.ev.soc', 'State of charge');
  tilePackVoltage.Caption := Tr('personal.ev.packVoltage', 'Pack voltage');
  tilePackCurrent.Caption := Tr('personal.ev.current', 'Current');
  tileTemperature.Caption := Tr('personal.ev.temperature', 'Temperature');
  tileCellDelta.Caption := Tr('personal.ev.cellDelta', 'Cell delta');
  cardDetails.Title := Tr('personal.ev.details', 'Battery details');
  cellMatrix.Title := Tr('personal.ev.cellVoltages', 'Cell voltages');
  ConfigureChart;
  if Assigned(FHost) and FHost.Connected then
    UpdateBannerUnsupported
  else
    UpdateBannerOffline;
  UpdateDetails(FLastSnapshot);
end;

procedure TfrmPageEV.PopulateVendors;
var
  Keys: TArray<string>;
  Key: string;
begin
  cmbVendor.Items.BeginUpdate;
  try
    cmbVendor.Items.Clear;
    Keys := TOBDEVBatteryCatalog.VendorKeys;
    for Key in Keys do
      if (Key <> '') and not StartsText('_', Key) then
        cmbVendor.Items.Add(Key);
    cmbVendor.ItemIndex := -1;
  finally
    cmbVendor.Items.EndUpdate;
  end;
end;

procedure TfrmPageEV.ConfigureChart;
begin
  chtHealth.Channels.Clear;
  chtHealth.AddChannel(Tr('personal.ev.sohSeries', 'State of health'), 0, 60,
    100, '%');
  chtHealth.AddChannel(Tr('personal.ev.socSeries', 'State of charge'), 0, 0,
    100, '%');
end;

function TfrmPageEV.CurrentVendor: string;
begin
  Result := '';
  if (cmbVendor.ItemIndex >= 0) and
    (cmbVendor.ItemIndex < cmbVendor.Items.Count) then
    Result := cmbVendor.Items[cmbVendor.ItemIndex];
end;

function TfrmPageEV.CurrentModelYear: Integer;
begin
  Result := 0;
  if Assigned(FHost) then
    Result := ModelYearFromVIN(FHost.Session.VehicleVIN);
end;

function TfrmPageEV.VendorAvailable(const AVendor: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to cmbVendor.Items.Count - 1 do
    if SameText(cmbVendor.Items[I], AVendor) then
      Exit(True);
end;

function TfrmPageEV.SelectVendorFromVIN: Boolean;
var
  Vendor: string;
  I: Integer;
begin
  Result := CurrentVendor <> '';
  if Result or FManualVendor or not Assigned(FHost) then
    Exit;
  Vendor := VendorFromVIN(FHost.Session.VehicleVIN);
  if (Vendor = '') or not VendorAvailable(Vendor) then
    Exit(False);
  for I := 0 to cmbVendor.Items.Count - 1 do
    if SameText(cmbVendor.Items[I], Vendor) then
    begin
      cmbVendor.ItemIndex := I;
      Exit(True);
    end;
end;

function TfrmPageEV.VendorFromVIN(const AVIN: string): string;
var
  VIN: string;
  WMI: string;
begin
  Result := '';
  VIN := UpperCase(Trim(AVIN));
  if Length(VIN) < 3 then
    Exit;
  WMI := Copy(VIN, 1, 3);
  if StartsText('KMH', WMI) or StartsText('KNA', WMI) or
    StartsText('KND', WMI) or StartsText('TMA', WMI) or
    StartsText('U5Y', WMI) then
    Result := 'hmg'
  else if StartsText('WBY', WMI) or StartsText('WBA', WMI) then
    Result := 'bmw'
  else if StartsText('WMW', WMI) then
    Result := 'mini'
  else if StartsText('WVW', WMI) or StartsText('WAU', WMI) or
    StartsText('TMB', WMI) or StartsText('VSS', WMI) then
    Result := 'vw'
  else if StartsText('JN1', WMI) or StartsText('SJN', WMI) or
    StartsText('VSK', WMI) then
    Result := 'nissan'
  else if StartsText('VF1', WMI) then
    Result := 'renault'
  else if StartsText('WDD', WMI) or StartsText('W1K', WMI) then
    Result := 'mercedes'
  else if StartsText('WME', WMI) then
    Result := 'smart'
  else if StartsText('5YJ', WMI) or StartsText('7SA', WMI) or
    StartsText('LRW', WMI) then
    Result := 'tesla'
  else if StartsText('WP0', WMI) then
    Result := 'porsche'
  else if StartsText('JTM', WMI) or StartsText('SB1', WMI) then
    Result := 'toyota'
  else if StartsText('SAD', WMI) then
    Result := 'jlr'
  else if StartsText('1G1', WMI) or StartsText('3GN', WMI) then
    Result := 'gm'
  else if StartsText('WF0', WMI) or StartsText('1FA', WMI) then
    Result := 'ford'
  else if StartsText('VF3', WMI) or StartsText('VR3', WMI) or
    StartsText('ZFA', WMI) or StartsText('VSX', WMI) then
    Result := 'stellantis'
  else if StartsText('LSJ', WMI) then
    Result := 'mg'
  else if StartsText('YV1', WMI) or StartsText('LPY', WMI) then
    Result := 'polestar'
  else if StartsText('JHM', WMI) or StartsText('SHH', WMI) then
    Result := 'honda'
  else if StartsText('JM1', WMI) then
    Result := 'mazda'
  else if StartsText('JF1', WMI) or StartsText('JF2', WMI) then
    Result := 'subaru'
  else if StartsText('LGX', WMI) then
    Result := 'byd';
end;

function TfrmPageEV.ModelYearFromVIN(const AVIN: string): Integer;
const
  CODES = 'ABCDEFGHJKLMNPRSTVWXY123456789';
var
  Code: Char;
  Index: Integer;
  Candidate: Integer;
  LimitYear: Word;
  Month: Word;
  Day: Word;
begin
  Result := 0;
  if Length(AVIN) < 10 then
    Exit;
  Code := UpCase(AVIN[10]);
  Index := Pos(Code, CODES);
  if Index = 0 then
    Exit;
  Candidate := 1980 + Index - 1;
  DecodeDate(Date, LimitYear, Month, Day);
  while Candidate + 30 <= Integer(LimitYear) + 1 do
    Inc(Candidate, 30);
  Result := Candidate;
end;

procedure TfrmPageEV.StartPolling;
var
  Vendor: string;
  ModelYear: Integer;
begin
  if not (Assigned(FHost) and FActive and FHost.Connected) then
    Exit;
  StopPolling;
  if not SelectVendorFromVIN then
  begin
    UpdateBannerUnsupported;
    Exit;
  end;

  Vendor := CurrentVendor;
  ModelYear := CurrentModelYear;
  if Vendor = '' then
  begin
    UpdateBannerUnsupported;
    Exit;
  end;

  try
    FHost.Session.EVBattery.Stop;
    FHost.Session.EVBattery.Vendor := Vendor;
    FHost.Session.EVBattery.ModelYear := ModelYear;
    FHost.Session.EVBattery.PollIntervalMs := EV_POLL_MS;
    FHost.Session.EVBattery.OnSnapshot := BatterySnapshot;
    FHost.Session.EVBattery.OnError := BatteryError;
    FHost.Session.EVBattery.Start;
    FReading := True;
    UpdateBannerReading;
  except
    on E: Exception do
    begin
      FReading := False;
      bannerState.Kind := bnDanger;
      bannerState.Title := Tr('personal.ev.startFailed',
        'EV battery reader could not start');
      bannerState.Text := E.Message;
      if Assigned(FHost) then
        FHost.LogWarning('EV battery reader could not start: ' + E.Message);
    end;
  end;
end;

procedure TfrmPageEV.StopPolling;
begin
  if not Assigned(FHost) then
    Exit;
  try
    FHost.Session.EVBattery.OnSnapshot := nil;
    FHost.Session.EVBattery.OnError := nil;
    FHost.Session.EVBattery.Stop;
  except
    on E: Exception do
      FHost.LogWarning('EV battery polling stopped with an error: ' + E.Message);
  end;
  FReading := False;
end;

procedure TfrmPageEV.btnReadAgainClick(Sender: TObject);
begin
  if Assigned(FHost) and FHost.Connected then
    StartPolling
  else
    UpdateBannerOffline;
end;

procedure TfrmPageEV.cmbVendorChange(Sender: TObject);
begin
  FManualVendor := CurrentVendor <> '';
  if FActive and Assigned(FHost) and FHost.Connected then
    StartPolling;
end;

procedure TfrmPageEV.BatterySnapshot(Sender: TObject;
  const ASnapshot: TOBDEVBatterySnapshot);
begin
  FReading := False;
  FLastSnapshot := ASnapshot;
  UpdateSnapshot(ASnapshot);
end;

procedure TfrmPageEV.BatteryError(Sender: TObject; ACode: TOBDErrorCode;
  const AMessage: string; var AHandled: Boolean);
begin
  AHandled := True;
  bannerState.Kind := bnWarning;
  bannerState.Title := Tr('personal.ev.readWarning',
    'Battery data is not available');
  bannerState.Text := AMessage;
end;

function TfrmPageEV.SnapshotHasUsefulData(
  const ASnapshot: TOBDEVBatterySnapshot): Boolean;
begin
  Result := ASnapshot.HasSOC or ASnapshot.HasSOH or ASnapshot.HasPackVoltage or
    ASnapshot.HasPackCurrent or ASnapshot.HasCellVoltages or
    ASnapshot.HasCellVoltageMin or ASnapshot.HasPackTempMin or
    ASnapshot.HasModuleTemps;
end;

procedure TfrmPageEV.UpdateSnapshot(const ASnapshot: TOBDEVBatterySnapshot);
var
  MinV: Double;
  MaxV: Double;
  AvgV: Double;
  Temp: Double;
begin
  if not SnapshotHasUsefulData(ASnapshot) then
  begin
    if FReading then
      UpdateBannerReading
    else if Assigned(FHost) and FHost.Connected then
      UpdateBannerUnsupported
    else
      UpdateBannerOffline;
    UpdateDetails(ASnapshot);
    Exit;
  end;

  UpdateBannerLive(ASnapshot);
  if ASnapshot.HasSOH then
  begin
    tileSOH.Value := ASnapshot.SOH;
    chtHealth.AddSample(0, ASnapshot.SOH);
  end;
  if ASnapshot.HasSOC then
  begin
    tileSOC.Value := ASnapshot.SOC;
    chtHealth.AddSample(1, ASnapshot.SOC);
  end;
  if ASnapshot.HasPackVoltage then
    tilePackVoltage.Value := ASnapshot.PackVoltage;
  if ASnapshot.HasPackCurrent then
    tilePackCurrent.Value := ASnapshot.PackCurrent;

  Temp := 0;
  if ASnapshot.HasPackTempMax then
    Temp := ASnapshot.PackTempMaxC
  else if ASnapshot.HasModuleTemps and (Length(ASnapshot.ModuleTempsC) > 0) then
    Temp := ASnapshot.ModuleTempsC[0];
  if Temp <> 0 then
    tileTemperature.Value := Temp;

  if CellSummary(ASnapshot, MinV, MaxV, AvgV) then
    tileCellDelta.Value := (MaxV - MinV) * 1000.0;

  UpdateCellMatrix(ASnapshot);
  UpdateDetails(ASnapshot);
end;

function TfrmPageEV.CellSummary(const ASnapshot: TOBDEVBatterySnapshot;
  out AMin, AMax, AAvg: Double): Boolean;
var
  V: Single;
  Sum: Double;
  Count: Integer;
begin
  Result := False;
  AMin := 0;
  AMax := 0;
  AAvg := 0;
  if ASnapshot.HasCellVoltages and (Length(ASnapshot.CellVoltages) > 0) then
  begin
    AMin := ASnapshot.CellVoltages[0];
    AMax := ASnapshot.CellVoltages[0];
    Sum := 0;
    Count := 0;
    for V in ASnapshot.CellVoltages do
    begin
      if V < AMin then
        AMin := V;
      if V > AMax then
        AMax := V;
      Sum := Sum + V;
      Inc(Count);
    end;
    if Count > 0 then
      AAvg := Sum / Count;
    Exit(True);
  end;
  if ASnapshot.HasCellVoltageMin and ASnapshot.HasCellVoltageMax then
  begin
    AMin := ASnapshot.CellVoltageMin;
    AMax := ASnapshot.CellVoltageMax;
    if ASnapshot.HasCellVoltageAvg then
      AAvg := ASnapshot.CellVoltageAvg
    else
      AAvg := (AMin + AMax) * 0.5;
    Exit(True);
  end;
end;

procedure TfrmPageEV.UpdateCellMatrix(const ASnapshot: TOBDEVBatterySnapshot);
var
  CellCount: Integer;
  CellsPerModule: Integer;
  ModuleCount: Integer;
  ModuleIndex: Integer;
  CellIndex: Integer;
  GlobalIndex: Integer;
  Values: TArray<Double>;
  Temperature: Double;
  MinV: Double;
  MaxV: Double;
  AvgV: Double;
begin
  if not ASnapshot.HasCellVoltages or (Length(ASnapshot.CellVoltages) = 0) then
    Exit;
  CellCount := Length(ASnapshot.CellVoltages);
  if CellCount <= 84 then
    CellsPerModule := 12
  else
    CellsPerModule := 14;
  ModuleCount := Ceil(CellCount / CellsPerModule);
  cellMatrix.ModuleCount := ModuleCount;
  cellMatrix.CellsPerModule := CellsPerModule;
  CellSummary(ASnapshot, MinV, MaxV, AvgV);
  for ModuleIndex := 0 to ModuleCount - 1 do
  begin
    SetLength(Values, CellsPerModule);
    for CellIndex := 0 to CellsPerModule - 1 do
    begin
      GlobalIndex := ModuleIndex * CellsPerModule + CellIndex;
      if GlobalIndex < CellCount then
        Values[CellIndex] := ASnapshot.CellVoltages[GlobalIndex]
      else
        Values[CellIndex] := AvgV;
    end;
    Temperature := 0;
    if ASnapshot.HasModuleTemps and
      (ModuleIndex < Length(ASnapshot.ModuleTempsC)) then
      Temperature := ASnapshot.ModuleTempsC[ModuleIndex]
    else if ASnapshot.HasPackTempMax then
      Temperature := ASnapshot.PackTempMaxC;
    cellMatrix.SetModule(ModuleIndex, Values, Temperature);
  end;
end;

function TfrmPageEV.FormatValue(AValue: Double; const AUnit: string;
  ADecimals: Integer): string;
begin
  Result := FormatFloat('0.' + StringOfChar('0', ADecimals), AValue,
    TFormatSettings.Invariant) + ' ' + AUnit;
end;

function TfrmPageEV.TemperatureText(
  const ASnapshot: TOBDEVBatterySnapshot): string;
var
  MinT: Double;
  MaxT: Double;
  T: Single;
begin
  Result := Tr('personal.ev.notAvailable', 'Not available');
  if ASnapshot.HasPackTempMin and ASnapshot.HasPackTempMax then
    Result := FormatValue(ASnapshot.PackTempMinC, #176'C', 1) + ' - ' +
      FormatValue(ASnapshot.PackTempMaxC, #176'C', 1)
  else if ASnapshot.HasModuleTemps and (Length(ASnapshot.ModuleTempsC) > 0) then
  begin
    MinT := ASnapshot.ModuleTempsC[0];
    MaxT := ASnapshot.ModuleTempsC[0];
    for T in ASnapshot.ModuleTempsC do
    begin
      if T < MinT then
        MinT := T;
      if T > MaxT then
        MaxT := T;
    end;
    Result := FormatValue(MinT, #176'C', 1) + ' - ' + FormatValue(MaxT,
      #176'C', 1);
  end;
end;

function TfrmPageEV.ChargeStateText(AState: TOBDEVChargeState): string;
begin
  case AState of
    csIdle:
      Result := Tr('personal.ev.chargeIdle', 'Idle');
    csACCharging:
      Result := Tr('personal.ev.chargeAC', 'AC charging');
    csDCFastCharging:
      Result := Tr('personal.ev.chargeDC', 'DC fast charging');
    csDriving:
      Result := Tr('personal.ev.chargeDriving', 'Driving');
    csRegenBraking:
      Result := Tr('personal.ev.chargeRegen', 'Regeneration');
  else
    Result := Tr('personal.ev.chargeUnknown', 'Unknown');
  end;
end;

procedure TfrmPageEV.UpdateDetails(const ASnapshot: TOBDEVBatterySnapshot);
var
  MinV: Double;
  MaxV: Double;
  AvgV: Double;
  CellText: string;
begin
  kvDetails.BeginUpdate;
  try
    kvDetails.Clear;
    kvDetails.Add(Tr('personal.ev.profileCategory', 'Profile'),
      Tr('personal.ev.vendorKey', 'Vendor'), CurrentVendor);
    if CurrentModelYear > 0 then
      kvDetails.Add(Tr('personal.ev.profileCategory', 'Profile'),
        Tr('personal.ev.modelYear', 'Model year'), IntToStr(CurrentModelYear));
    if ASnapshot.Timestamp > 0 then
      kvDetails.Add(Tr('personal.ev.profileCategory', 'Profile'),
        Tr('personal.ev.lastRead', 'Last read'),
        FormatDateTime('dd mmm yyyy hh:nn:ss', ASnapshot.Timestamp));

    if ASnapshot.HasChargeState then
      kvDetails.Add(Tr('personal.ev.packCategory', 'Pack'),
        Tr('personal.ev.chargeState', 'Charge state'),
        ChargeStateText(ASnapshot.ChargeState));
    if ASnapshot.HasPackPower then
      kvDetails.Add(Tr('personal.ev.packCategory', 'Pack'),
        Tr('personal.ev.packPower', 'Pack power'),
        FormatValue(ASnapshot.PackPower, 'kW', 1));
    if ASnapshot.HasCapacityRemaining then
      kvDetails.Add(Tr('personal.ev.packCategory', 'Pack'),
        Tr('personal.ev.capacityRemaining', 'Capacity remaining'),
        FormatValue(ASnapshot.CapacityRemainingKwh, 'kWh', 1));
    if ASnapshot.HasCapacityRemainingAh then
      kvDetails.Add(Tr('personal.ev.packCategory', 'Pack'),
        Tr('personal.ev.capacityAh', 'Capacity remaining'),
        FormatValue(ASnapshot.CapacityRemainingAh, 'Ah', 1));
    if ASnapshot.HasOdometerKm then
      kvDetails.Add(Tr('personal.ev.lifetimeCategory', 'Lifetime'),
        Tr('personal.ev.odometer', 'Odometer'),
        FormatFloat('0 km', ASnapshot.OdometerKm, TFormatSettings.Invariant));
    if ASnapshot.HasCumulativeChargedKwh then
      kvDetails.Add(Tr('personal.ev.lifetimeCategory', 'Lifetime'),
        Tr('personal.ev.energyCharged', 'Energy charged'),
        FormatValue(ASnapshot.CumulativeChargedKwh, 'kWh', 0));
    if ASnapshot.HasCumulativeDischargedKwh then
      kvDetails.Add(Tr('personal.ev.lifetimeCategory', 'Lifetime'),
        Tr('personal.ev.energyUsed', 'Energy used'),
        FormatValue(ASnapshot.CumulativeDischargedKwh, 'kWh', 0));

    if CellSummary(ASnapshot, MinV, MaxV, AvgV) then
    begin
      if ASnapshot.HasCellVoltages then
        CellText := Format(Tr('personal.ev.cellCountFmt', '%d cells'),
          [Length(ASnapshot.CellVoltages)])
      else
        CellText := Tr('personal.ev.minMaxOnly', 'Min/max only');
      kvDetails.Add(Tr('personal.ev.cellsCategory', 'Cells'),
        Tr('personal.ev.cells', 'Cells'), CellText);
      kvDetails.Add(Tr('personal.ev.cellsCategory', 'Cells'),
        Tr('personal.ev.lowestCell', 'Lowest'), FormatValue(MinV, 'V', 3));
      kvDetails.Add(Tr('personal.ev.cellsCategory', 'Cells'),
        Tr('personal.ev.meanCell', 'Mean'), FormatValue(AvgV, 'V', 3));
      kvDetails.Add(Tr('personal.ev.cellsCategory', 'Cells'),
        Tr('personal.ev.highestCell', 'Highest'), FormatValue(MaxV, 'V', 3));
      kvDetails.Add(Tr('personal.ev.cellsCategory', 'Cells'),
        Tr('personal.ev.spread', 'Delta'),
        FormatValue((MaxV - MinV) * 1000.0, 'mV', 0));
    end;
    kvDetails.Add(Tr('personal.ev.thermalCategory', 'Thermal'),
      Tr('personal.ev.temperature', 'Temperature'), TemperatureText(ASnapshot));

    if Length(ASnapshot.Errors) > 0 then
      kvDetails.Add(Tr('personal.ev.errorsCategory', 'Read warnings'),
        Tr('personal.ev.errorCount', 'Fields with errors'),
        IntToStr(Length(ASnapshot.Errors)), kvkTone).Tone := skWarning;
    if kvDetails.Items.Count = 0 then
      kvDetails.Add(Tr('personal.ev.profileCategory', 'Profile'),
        Tr('personal.ev.waiting', 'Waiting'),
        Tr('personal.ev.waitingText', 'No battery snapshot has been read yet.'));
  finally
    kvDetails.EndUpdate;
  end;
end;

procedure TfrmPageEV.UpdateBannerOffline;
begin
  bannerState.Kind := bnWarning;
  bannerState.Title := Tr('personal.ev.offlineTitle', 'Connect to your car');
  bannerState.Text := Tr('personal.ev.offlineText',
    'EV battery data is read from the high-voltage battery ECU while connected.');
end;

procedure TfrmPageEV.UpdateBannerUnsupported;
begin
  bannerState.Kind := bnDanger;
  bannerState.Title := Tr('personal.ev.unsupportedTitle',
    'Not an electric or hybrid car, or not supported');
  bannerState.Text := Tr('personal.ev.unsupportedText',
    'No matching EV battery profile was selected from the VIN. Choose a vendor profile if this is an EV or hybrid.');
end;

procedure TfrmPageEV.UpdateBannerReading;
begin
  bannerState.Kind := bnInfo;
  bannerState.Title := Tr('personal.ev.readingTitle', 'Reading EV battery');
  bannerState.Text := Tr('personal.ev.readingText',
    'OBD-Studio is polling the selected high-voltage battery profile.');
end;

procedure TfrmPageEV.UpdateBannerLive(const ASnapshot: TOBDEVBatterySnapshot);
begin
  bannerState.Kind := bnSuccess;
  bannerState.Title := Tr('personal.ev.liveTitle', 'EV battery data');
  bannerState.Text := Format(Tr('personal.ev.liveText',
    'Profile %s read successfully. Values come from the EV battery catalogue.'),
    [ASnapshot.Vendor]);
end;

end.
