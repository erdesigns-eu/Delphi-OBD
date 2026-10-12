//------------------------------------------------------------------------------
//  untPageFuel
//
//  Fuel economy page for OBD-Studio Personal. It computes live and trip
//  consumption from Mode 01 MAF or fuel-rate data, keeps trip totals and
//  lets the user edit the local fuel price used for cost estimates.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untPageFuel;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  ERD.I18n,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Dialogs,
  ERD.UI.Chips,
  ERD.UI.Card,
  ERD.UI.Buttons,
  ERD.UI.ValueTile,
  ERD.UI.TrendChart,
  ERD.UI.Histogram,
  ERD.UI.KeyValueList,
  ERD.UI.Edits,
  ERD.Service.LiveData,
  ERD.Service.Dyno,
  untPersonalPage;

type
  TFuelMethod = (fmNone, fmMAF, fmFuelRate);

  TfrmPageFuel = class(TFrame, IPersonalPage, IPersonalCommands)
    pnlHeader: TPanel;
    lblTitle: TOBDDialogText;
    lblSubtitle: TOBDDialogText;
    btnResetTrip: TOBDButton;
    bannerState: TOBDBanner;
    pnlTiles: TPanel;
    tileAverage: TOBDValueTile;
    tileInstant: TOBDValueTile;
    tileDistance: TOBDValueTile;
    tileFuelUsed: TOBDValueTile;
    tileCost: TOBDValueTile;
    tileCO2: TOBDValueTile;
    pnlMain: TPanel;
    chtConsumption: TOBDTrendChart;
    cardMeasure: TOBDCard;
    kvMeasure: TOBDKeyValueList;
    lblFuelPrice: TOBDDialogText;
    edtFuelPrice: TOBDEdit;
    lblCurrency: TOBDDialogText;
    edtCurrency: TOBDEdit;
    histConsumption: TOBDHistogram;
    procedure btnResetTripClick(Sender: TObject);
    procedure edtFuelPriceChange(Sender: TObject);
    procedure edtCurrencyChange(Sender: TObject);
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FSubscribed: Boolean;
    FLoadingSettings: Boolean;
    FFuelMeter: TOBDFuelEconomyMeter;
    FEmissions: TOBDEmissionsEstimator;
    FMethod: TFuelMethod;
    FHaveSpeed: Boolean;
    FHaveMAF: Boolean;
    FHaveFuelRate: Boolean;
    FHaveFuelType: Boolean;
    FSpeedKmh: Double;
    FMAFGramsPerSec: Double;
    FFuelRateLh: Double;
    FStoich: Double;
    FFuelDensity: Double;
    FCO2PerGram: Double;
    FFuelTypeCode: Byte;
    FTripStart: TDateTime;
    FDrivingSeconds: Double;
    FLastSampleMs: Cardinal;
    procedure LoadSettingsToEditors;
    procedure BeginPolling;
    procedure StopPolling;
    procedure LiveValue(Sender: TObject; const AValue: TOBDPIDValue);
    procedure ProcessSample;
    procedure ResetTripState;
    procedure ConfigureCharts;
    procedure UpdateTiles;
    procedure UpdateMethodList;
    procedure UpdateUnitCaptions;
    procedure ApplyFuelType(AFuelType: Byte);
    procedure ShowOffline;
    procedure ShowUnsupported;
    procedure ShowPolling;
    procedure AddPollPID(var APIDs: TBytes; APID: Byte);
    function HasPID(const APIDs: TBytes; APID: Byte): Boolean;
    function ParseEditFloat(const AText: string; out AValue: Double): Boolean;
    function ConsumptionForDisplay(ALp100Km: Double): Double;
    function ConsumptionUnit: string;
    function DistanceForDisplay(AKm: Double): Double;
    function DistanceUnit: string;
    function FuelForDisplay(ALitres: Double): Double;
    function FuelUnit: string;
    function CO2ForDisplay(AGPerKm: Double): Double;
    function CO2Unit: string;
    function FuelTypeText: string;
    function MethodText: string;
  public
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
    // IPersonalCommands
    function Commands: TPersonalCommands;
    function CommandEnabled(ACommand: TPersonalCommand): Boolean;
    function CommandChecked(ACommand: TPersonalCommand): Boolean;
    procedure ExecuteCommand(ACommand: TPersonalCommand);
  end;

implementation

{$R *.dfm}

const
  FUEL_PID_SPEED = $0D;
  FUEL_PID_MAF = $10;
  FUEL_PID_FUEL_TYPE = $51;
  FUEL_PID_FUEL_RATE = $5E;
  POLL_MS = 500;
  L_PER_US_GALLON = 3.785411784;
  MPG_FROM_LP100KM = 235.214583;
  GASOLINE_STOICH = 14.7;
  GASOLINE_DENSITY = 0.745;
  GASOLINE_CO2_PER_G = 3.17;
  DIESEL_STOICH = 14.5;
  DIESEL_DENSITY = 0.835;
  DIESEL_CO2_PER_G = 3.16;

function TickDeltaSec(ANow, AThen: Cardinal): Double;
begin
  Result := (ANow - AThen) / 1000.0;
end;

{ TfrmPageFuel }

function TfrmPageFuel.PageKey: string;
begin
  Result := PAGE_FUEL;
end;

procedure TfrmPageFuel.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  FFuelMeter := TOBDFuelEconomyMeter.Create(Self);
  FEmissions := TOBDEmissionsEstimator.Create(Self);
  ApplyFuelType(1);
  FTripStart := Now;
  LoadSettingsToEditors;
  ConfigureCharts;
  Translate;
  if Assigned(FHost) and FHost.Connected then
    ShowPolling
  else
    ShowOffline;
end;

procedure TfrmPageFuel.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive then
  begin
    LoadSettingsToEditors;
    if Assigned(FHost) and FHost.Connected then
      BeginPolling
    else
      ShowOffline;
  end
  else
    StopPolling;
end;

procedure TfrmPageFuel.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
  begin
    StopPolling;
    FHaveSpeed := False;
    FHaveMAF := False;
    FHaveFuelRate := False;
    ShowOffline;
    UpdateTiles;
  end
  else if FActive then
    BeginPolling;
end;

procedure TfrmPageFuel.UnitsChanged;
begin
  UpdateUnitCaptions;
  ConfigureCharts;
  UpdateTiles;
  UpdateMethodList;
end;

procedure TfrmPageFuel.Translate;
begin
  lblTitle.Caption := Tr('personal.fuel.title', 'Fuel economy');
  lblSubtitle.Caption := Tr('personal.fuel.subtitle',
    'Live consumption, trip fuel use and cost');
  btnResetTrip.Caption := Tr('personal.fuel.resetTrip', 'Reset trip');
  btnResetTrip.Hint := Tr('personal.fuel.resetTripHint',
    'Clear the current trip totals');
  tileAverage.Caption := Tr('personal.fuel.average', 'Average');
  tileInstant.Caption := Tr('personal.fuel.instant', 'Right now');
  tileDistance.Caption := Tr('personal.fuel.distance', 'Distance');
  tileFuelUsed.Caption := Tr('personal.fuel.fuelUsed', 'Fuel used');
  tileCost.Caption := Tr('personal.fuel.cost', 'Cost');
  tileCO2.Caption := Tr('personal.fuel.co2', 'CO2');
  cardMeasure.Title := Tr('personal.fuel.measureCard', 'How it is measured');
  lblFuelPrice.Caption := Tr('personal.fuel.fuelPrice', 'Fuel price');
  lblCurrency.Caption := Tr('personal.fuel.currency', 'Currency');
  histConsumption.Title := Tr('personal.fuel.histogram', 'Consumption histogram');
  UpdateUnitCaptions;
  UpdateMethodList;
  if Assigned(FHost) and not FHost.Connected then
    ShowOffline
  else if FMethod = fmNone then
    ShowUnsupported
  else
    ShowPolling;
end;

procedure TfrmPageFuel.LoadSettingsToEditors;
begin
  if not Assigned(FHost) then
    Exit;
  FLoadingSettings := True;
  try
    edtFuelPrice.Text := FormatFloat('0.00', FHost.Settings.FuelPrice,
      TFormatSettings.Invariant);
    edtCurrency.Text := FHost.Settings.Currency;
  finally
    FLoadingSettings := False;
  end;
end;

function TfrmPageFuel.ParseEditFloat(const AText: string;
  out AValue: Double): Boolean;
var
  Text: string;
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Invariant;
  Text := Trim(AText).Replace(',', '.');
  Result := TryStrToFloat(Text, AValue, FS);
end;

procedure TfrmPageFuel.edtFuelPriceChange(Sender: TObject);
var
  Price: Double;
begin
  if FLoadingSettings or not Assigned(FHost) then
    Exit;
  if ParseEditFloat(edtFuelPrice.Text, Price) and (Price >= 0) then
  begin
    FHost.Settings.FuelPrice := Price;
    FHost.SettingsChanged;
    UpdateTiles;
    UpdateMethodList;
  end;
end;

procedure TfrmPageFuel.edtCurrencyChange(Sender: TObject);
var
  Currency: string;
begin
  if FLoadingSettings or not Assigned(FHost) then
    Exit;
  Currency := Trim(edtCurrency.Text);
  if Currency = '' then
    Exit;
  FHost.Settings.Currency := Currency;
  FHost.SettingsChanged;
  UpdateTiles;
  UpdateMethodList;
end;

procedure TfrmPageFuel.btnResetTripClick(Sender: TObject);
begin
  ResetTripState;
  if Assigned(FHost) then
    FHost.Log(Tr('personal.fuel.tripResetLog', 'Fuel economy trip reset.'));
end;

procedure TfrmPageFuel.ResetTripState;
begin
  FFuelMeter.ResetTripA;
  FEmissions.PushSample(0, 0);
  FTripStart := Now;
  FDrivingSeconds := 0;
  FLastSampleMs := 0;
  chtConsumption.ClearSamples;
  histConsumption.Clear;
  UpdateTiles;
end;

procedure TfrmPageFuel.AddPollPID(var APIDs: TBytes; APID: Byte);
var
  N: Integer;
begin
  N := Length(APIDs);
  SetLength(APIDs, N + 1);
  APIDs[N] := APID;
end;

function TfrmPageFuel.HasPID(const APIDs: TBytes; APID: Byte): Boolean;
var
  PID: Byte;
begin
  Result := False;
  for PID in APIDs do
    if PID = APID then
      Exit(True);
end;

procedure TfrmPageFuel.BeginPolling;
var
  Supported: TBytes;
begin
  if not (Assigned(FHost) and FActive and FHost.Connected) then
    Exit;
  StopPolling;
  ShowPolling;
  Supported := nil;
  FHost.Session.RunJob(
    procedure
    begin
      Supported := FHost.Session.LiveData.SupportedPIDs;
    end,
    procedure(const AError: string)
    var
      PollPIDs: TBytes;
    begin
      if not (Assigned(FHost) and FActive and FHost.Connected) then
        Exit;
      if AError <> '' then
      begin
        bannerState.Kind := bnDanger;
        bannerState.Title := Tr('personal.fuel.pidCheckFailed',
          'Live data is not available');
        bannerState.Text := AError;
        FHost.LogWarning('Fuel economy PID check failed: ' + AError);
        Exit;
      end;

      FMethod := fmNone;
      PollPIDs := nil;
      if HasPID(Supported, FUEL_PID_SPEED) then
        AddPollPID(PollPIDs, FUEL_PID_SPEED);
      if HasPID(Supported, FUEL_PID_FUEL_RATE) then
      begin
        FMethod := fmFuelRate;
        AddPollPID(PollPIDs, FUEL_PID_FUEL_RATE);
      end
      else if HasPID(Supported, FUEL_PID_MAF) then
      begin
        FMethod := fmMAF;
        AddPollPID(PollPIDs, FUEL_PID_MAF);
      end;
      if HasPID(Supported, FUEL_PID_FUEL_TYPE) then
        AddPollPID(PollPIDs, FUEL_PID_FUEL_TYPE);

      if (FMethod = fmNone) or not HasPID(Supported, FUEL_PID_SPEED) then
      begin
        ShowUnsupported;
        UpdateMethodList;
        Exit;
      end;

      FHost.Session.LiveData.Subscribe(FUEL_PID_SPEED, LiveValue);
      if FMethod = fmFuelRate then
        FHost.Session.LiveData.Subscribe(FUEL_PID_FUEL_RATE, LiveValue)
      else
        FHost.Session.LiveData.Subscribe(FUEL_PID_MAF, LiveValue);
      if HasPID(Supported, FUEL_PID_FUEL_TYPE) then
        FHost.Session.LiveData.Subscribe(FUEL_PID_FUEL_TYPE, LiveValue);
      FSubscribed := True;
      FHost.Session.LiveData.Poll(PollPIDs, POLL_MS);
      ShowPolling;
      UpdateMethodList;
    end);
end;

procedure TfrmPageFuel.StopPolling;
begin
  if not Assigned(FHost) then
    Exit;
  if FSubscribed then
  begin
    FHost.Session.LiveData.Unsubscribe(FUEL_PID_SPEED, LiveValue);
    FHost.Session.LiveData.Unsubscribe(FUEL_PID_MAF, LiveValue);
    FHost.Session.LiveData.Unsubscribe(FUEL_PID_FUEL_RATE, LiveValue);
    FHost.Session.LiveData.Unsubscribe(FUEL_PID_FUEL_TYPE, LiveValue);
    FSubscribed := False;
  end;
  if FHost.Session.LiveData.IsPolling then
    FHost.Session.LiveData.PollStop;
end;

procedure TfrmPageFuel.LiveValue(Sender: TObject; const AValue: TOBDPIDValue);
var
  Code: Byte;
begin
  case AValue.PID of
    FUEL_PID_SPEED:
      if not IsNan(AValue.Value) then
      begin
        FHaveSpeed := True;
        FSpeedKmh := Max(0, AValue.Value);
        ProcessSample;
      end;
    FUEL_PID_MAF:
      if not IsNan(AValue.Value) then
      begin
        FHaveMAF := True;
        FMAFGramsPerSec := Max(0, AValue.Value);
      end;
    FUEL_PID_FUEL_RATE:
      if not IsNan(AValue.Value) then
      begin
        FHaveFuelRate := True;
        FFuelRateLh := Max(0, AValue.Value);
      end;
    FUEL_PID_FUEL_TYPE:
      begin
        Code := 0;
        if Length(AValue.Raw) > 0 then
          Code := AValue.Raw[0]
        else if not IsNan(AValue.Value) then
          Code := Round(AValue.Value);
        if Code <> 0 then
          ApplyFuelType(Code);
      end;
  end;
end;

procedure TfrmPageFuel.ProcessSample;
var
  NowMs: Cardinal;
  EquivalentMAF: Double;
  Dt: Double;
  InstantDisplay: Double;
begin
  if not FHaveSpeed then
    Exit;

  EquivalentMAF := NaN;
  case FMethod of
    fmFuelRate:
      if FHaveFuelRate then
        EquivalentMAF := FFuelRateLh * FFuelDensity * 1000.0 / 3600.0 * FStoich;
    fmMAF:
      if FHaveMAF then
        EquivalentMAF := FMAFGramsPerSec;
  end;
  if IsNan(EquivalentMAF) then
    Exit;

  NowMs := GetTickCount;
  if (FLastSampleMs <> 0) and (FSpeedKmh > 1.0) then
  begin
    Dt := TickDeltaSec(NowMs, FLastSampleMs);
    if (Dt > 0) and (Dt < 10) then
      FDrivingSeconds := FDrivingSeconds + Dt;
  end;
  FLastSampleMs := NowMs;

  FFuelMeter.PushSample(NowMs, EquivalentMAF, FSpeedKmh);
  FEmissions.PushSample(EquivalentMAF, FSpeedKmh);
  UpdateTiles;

  InstantDisplay := ConsumptionForDisplay(FFuelMeter.InstantLp100Km);
  if not IsNan(InstantDisplay) and (InstantDisplay > 0) then
  begin
    chtConsumption.AddSample(0, InstantDisplay);
    if FFuelMeter.TripADistanceKm > 0 then
      chtConsumption.AddSample(1, ConsumptionForDisplay(
        FFuelMeter.TripAFuelL / FFuelMeter.TripADistanceKm * 100.0));
    histConsumption.AddSample(InstantDisplay);
  end;
end;

procedure TfrmPageFuel.ApplyFuelType(AFuelType: Byte);
begin
  FHaveFuelType := True;
  FFuelTypeCode := AFuelType;
  case AFuelType of
    4, 12:
      begin
        FStoich := DIESEL_STOICH;
        FFuelDensity := DIESEL_DENSITY;
        FCO2PerGram := DIESEL_CO2_PER_G;
      end;
  else
    begin
      FStoich := GASOLINE_STOICH;
      FFuelDensity := GASOLINE_DENSITY;
      FCO2PerGram := GASOLINE_CO2_PER_G;
    end;
  end;
  FFuelMeter.Stoich := FStoich;
  FFuelMeter.FuelDensityKgPerL := FFuelDensity;
  FEmissions.Stoich := FStoich;
  FEmissions.FuelDensityKgPerL := FFuelDensity;
  FEmissions.Co2PerGramOfFuel := FCO2PerGram;
  UpdateMethodList;
end;

function TfrmPageFuel.FuelTypeText: string;
begin
  case FFuelTypeCode of
    4, 12:
      Result := Tr('personal.fuel.diesel', 'Diesel');
    8, 13, 15:
      Result := Tr('personal.fuel.electric', 'Electric / hybrid');
    5:
      Result := Tr('personal.fuel.lpg', 'LPG');
    6:
      Result := Tr('personal.fuel.cng', 'CNG');
  else
    Result := Tr('personal.fuel.petrol', 'Petrol');
  end;
  if not FHaveFuelType then
    Result := Result + ' (' + Tr('personal.fuel.assumed', 'assumed') + ')';
end;

function TfrmPageFuel.MethodText: string;
begin
  case FMethod of
    fmFuelRate:
      Result := Tr('personal.fuel.methodFuelRate', 'Fuel rate PID 5E');
    fmMAF:
      Result := Tr('personal.fuel.methodMAF', 'Air mass (MAF)');
  else
    Result := Tr('personal.fuel.methodNone', 'Not supported');
  end;
end;

function TfrmPageFuel.ConsumptionUnit: string;
begin
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := 'mpg'
  else
    Result := 'l/100 km';
end;

function TfrmPageFuel.ConsumptionForDisplay(ALp100Km: Double): Double;
begin
  if IsNan(ALp100Km) or (ALp100Km <= 0) then
    Exit(0);
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := MPG_FROM_LP100KM / ALp100Km
  else
    Result := ALp100Km;
end;

function TfrmPageFuel.DistanceForDisplay(AKm: Double): Double;
begin
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := AKm * 0.621371192
  else
    Result := AKm;
end;

function TfrmPageFuel.DistanceUnit: string;
begin
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := 'mi'
  else
    Result := 'km';
end;

function TfrmPageFuel.FuelForDisplay(ALitres: Double): Double;
begin
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := ALitres / L_PER_US_GALLON
  else
    Result := ALitres;
end;

function TfrmPageFuel.FuelUnit: string;
begin
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := 'gal'
  else
    Result := 'l';
end;

function TfrmPageFuel.CO2ForDisplay(AGPerKm: Double): Double;
begin
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := AGPerKm * 1.609344
  else
    Result := AGPerKm;
end;

function TfrmPageFuel.CO2Unit: string;
begin
  if Assigned(FHost) and (FHost.Settings.UnitSystem = usImperial) then
    Result := 'g/mi'
  else
    Result := 'g/km';
end;

procedure TfrmPageFuel.UpdateUnitCaptions;
begin
  tileAverage.&Unit := ConsumptionUnit;
  tileInstant.&Unit := ConsumptionUnit;
  tileAverage.Max := IfThen(ConsumptionUnit = 'mpg', 80, 20);
  tileInstant.Max := tileAverage.Max;
  tileDistance.&Unit := DistanceUnit;
  tileFuelUsed.&Unit := FuelUnit;
  tileCost.&Unit := FHost.Settings.Currency;
  tileCO2.&Unit := CO2Unit;
  histConsumption.UnitText := ConsumptionUnit;
  histConsumption.AutoRange := False;
  if ConsumptionUnit = 'mpg' then
  begin
    histConsumption.Min := 0;
    histConsumption.Max := 80;
    histConsumption.NormalLow := 25;
    histConsumption.NormalHigh := 60;
  end
  else
  begin
    histConsumption.Min := 0;
    histConsumption.Max := 20;
    histConsumption.NormalLow := 3;
    histConsumption.NormalHigh := 8;
  end;
end;

procedure TfrmPageFuel.ConfigureCharts;
begin
  chtConsumption.Channels.Clear;
  chtConsumption.AddChannel(Tr('personal.fuel.instantSeries', 'Instant'), 0, 0,
    IfThen(ConsumptionUnit = 'mpg', 80, 20), ConsumptionUnit);
  chtConsumption.AddChannel(Tr('personal.fuel.averageSeries', 'Trip average'),
    0, 0, IfThen(ConsumptionUnit = 'mpg', 80, 20), ConsumptionUnit);
  histConsumption.Clear;
end;

procedure TfrmPageFuel.UpdateTiles;
var
  DistanceKm: Double;
  FuelL: Double;
  AvgLp100: Double;
  CostFuel: Double;
  Cost: Double;
  Co2PerL: Double;
  Co2Gkm: Double;
begin
  if not Assigned(FFuelMeter) then
    Exit;
  DistanceKm := FFuelMeter.TripADistanceKm;
  FuelL := FFuelMeter.TripAFuelL;
  AvgLp100 := 0;
  if DistanceKm > 0.001 then
    AvgLp100 := FuelL / DistanceKm * 100.0;
  CostFuel := FuelForDisplay(FuelL);
  Cost := 0;
  if Assigned(FHost) then
    Cost := CostFuel * FHost.Settings.FuelPrice;
  Co2PerL := FFuelDensity * 1000.0 * FCO2PerGram;
  Co2Gkm := 0;
  if AvgLp100 > 0 then
    Co2Gkm := AvgLp100 * Co2PerL / 100.0;

  tileAverage.Value := ConsumptionForDisplay(AvgLp100);
  tileInstant.Value := ConsumptionForDisplay(FFuelMeter.InstantLp100Km);
  tileDistance.Value := DistanceForDisplay(DistanceKm);
  tileFuelUsed.Value := FuelForDisplay(FuelL);
  tileCost.Value := Cost;
  tileCO2.Value := CO2ForDisplay(Co2Gkm);
  UpdateMethodList;
end;

procedure TfrmPageFuel.UpdateMethodList;
var
  UnitPrice: string;
  TripTime: string;
  Hours: Integer;
  Minutes: Integer;
begin
  if not Assigned(FHost) then
    Exit;
  UnitPrice := Format('%s %.2f / %s', [FHost.Settings.Currency,
    FHost.Settings.FuelPrice, FuelUnit]);
  Hours := Trunc(FDrivingSeconds / 3600);
  Minutes := Trunc((FDrivingSeconds - Hours * 3600) / 60);
  TripTime := Format('%.2d:%.2d', [Hours, Minutes]);

  kvMeasure.BeginUpdate;
  try
    kvMeasure.Clear;
    kvMeasure.Add(Tr('personal.fuel.measureCategory', 'Fuel calculation'),
      Tr('personal.fuel.method', 'Method'), MethodText);
    kvMeasure.Add(Tr('personal.fuel.measureCategory', 'Fuel calculation'),
      Tr('personal.fuel.fuelType', 'Fuel type'), FuelTypeText);
    kvMeasure.Add(Tr('personal.fuel.measureCategory', 'Fuel calculation'),
      Tr('personal.fuel.stoich', 'Stoichiometric ratio'),
      FormatFloat('0.0', FStoich, TFormatSettings.Invariant) + ':1');
    kvMeasure.Add(Tr('personal.fuel.measureCategory', 'Fuel calculation'),
      Tr('personal.fuel.density', 'Fuel density'),
      FormatFloat('0.000', FFuelDensity, TFormatSettings.Invariant) + ' kg/l');
    kvMeasure.Add(Tr('personal.fuel.measureCategory', 'Fuel calculation'),
      Tr('personal.fuel.unitPrice', 'Fuel price'), UnitPrice);
    kvMeasure.Add(Tr('personal.fuel.tripCategory', 'Trip'),
      Tr('personal.fuel.tripStarted', 'Started'),
      FormatDateTime('dd mmm yyyy hh:nn', FTripStart));
    kvMeasure.Add(Tr('personal.fuel.tripCategory', 'Trip'),
      Tr('personal.fuel.drivingTime', 'Driving time'), TripTime);
  finally
    kvMeasure.EndUpdate;
  end;
end;

procedure TfrmPageFuel.ShowOffline;
begin
  bannerState.Kind := bnWarning;
  bannerState.Title := Tr('personal.fuel.offlineTitle', 'Connect to your car');
  bannerState.Text := Tr('personal.fuel.offlineText',
    'Fuel economy needs live data. Open the Adapter page and connect first.');
end;

procedure TfrmPageFuel.ShowUnsupported;
begin
  bannerState.Kind := bnDanger;
  bannerState.Title := Tr('personal.fuel.unsupportedTitle',
    'Fuel economy is not supported by this car');
  bannerState.Text := Tr('personal.fuel.unsupportedText',
    'Vehicle speed plus MAF or fuel-rate PID is required for live consumption.');
end;

procedure TfrmPageFuel.ShowPolling;
begin
  if FMethod = fmNone then
  begin
    bannerState.Kind := bnInfo;
    bannerState.Title := Tr('personal.fuel.checkingTitle',
      'Checking fuel economy support');
    bannerState.Text := Tr('personal.fuel.checkingText',
      'OBD-Studio is reading the supported live-data PIDs.');
  end
  else
  begin
    bannerState.Kind := bnSuccess;
    bannerState.Title := Tr('personal.fuel.liveTitle', 'Live fuel economy');
    bannerState.Text := Format(Tr('personal.fuel.liveText',
      'Calculated from %s and vehicle speed.'), [MethodText]);
  end;
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageFuel.Commands: TPersonalCommands;
begin
  Result := [pcResetTrip];
end;

function TfrmPageFuel.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  Result := (ACommand = pcResetTrip) and btnResetTrip.Enabled;
end;

function TfrmPageFuel.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPageFuel.ExecuteCommand(ACommand: TPersonalCommand);
begin
  if ACommand = pcResetTrip then
    btnResetTripClick(Self);
end;

end.
