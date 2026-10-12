//------------------------------------------------------------------------------
//  untPageService
//
//  OBD Studio Pro page for guided service functions with a searchable
//  catalogue, preconditions and a running procedure.
//
//  Components used: TOBDVehicleInfoCard, TOBDCard, TOBDEdit, TOBDComboBox,
//  TOBDTileGrid, TOBDButton, TOBDProcedureView and TOBDPreconditionList.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageService;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.VehicleCard, ERD.UI.Card, ERD.UI.Edits, ERD.UI.TileGrid,
  ERD.UI.Buttons, ERD.UI.ProcedureView, ERD.UI.PreconditionList,
  untProPage, untSimulator;

type
  TfrmService = class(TFrame, IProPage)
    vcVehicle: TOBDVehicleInfoCard;
    pnlBody: TPanel;
    cdCatalogue: TOBDCard;
    edSearch: TOBDEdit;
    cbCategory: TOBDComboBox;
    tilesCatalogue: TOBDTileGrid;
    btnRun: TOBDButton;
    pnlRight: TPanel;
    procService: TOBDProcedureView;
    preService: TOBDPreconditionList;
    procedure edSearchChange(Sender: TObject);
    procedure cbCategoryChange(Sender: TObject);
    procedure tilesCatalogueChange(Sender: TObject);
    procedure tilesCatalogueItemClick(Sender: TObject; AIndex: Integer);
    procedure tilesCatalogueItemActivate(Sender: TObject; AIndex: Integer);
    procedure btnRunClick(Sender: TObject);
    procedure procServiceNext(Sender: TObject);
    procedure procServiceBack(Sender: TObject);
    procedure procServiceAbort(Sender: TObject);
    procedure procServiceAction(Sender: TObject);
  private
    FHost: IProHost;
    FSelectedService: Integer;
    FRunActive: Boolean;
    FRunTick: Integer;
    FStep: Integer;
    procedure FillVehicle;
    procedure FillCategories;
    procedure RefreshCatalogue;
    procedure LoadService(AIndex: Integer);
    procedure LoadProcedure(const ATitle, AECU: string);
    procedure LoadPreconditions;
    procedure UpdateProcedureState;
    procedure StartProcedure;
    procedure FinishProcedure;
    function ServiceGlyph(AIndex: Integer): TOBDGlyph;
    function ServiceMatchesFilter(AIndex: Integer; const AQuery: string): Boolean;
    function TileServiceIndex(AIndex: Integer): Integer;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

type
  TServiceSample = record
    Name: string;
    Ecu: string;
    Category: string;
    Supported: Boolean;
    Custom: Boolean;
  end;

const
  SERVICE_COUNT = 12;
  SERVICE_SAMPLES: array [0 .. SERVICE_COUNT - 1] of TServiceSample = (
    (Name: 'Oil service reset'; Ecu: '17 Instruments'; Category: 'Maintenance';
      Supported: True; Custom: False),
    (Name: 'Parking brake service'; Ecu: '53 Parking brake'; Category: 'Brakes';
      Supported: True; Custom: False),
    (Name: 'Steering angle calibration'; Ecu: '44 Steering assist';
      Category: 'Chassis'; Supported: True; Custom: False),
    (Name: 'Battery registration'; Ecu: '19 Gateway'; Category: 'Maintenance';
      Supported: True; Custom: False),
    (Name: 'DPF regeneration'; Ecu: '01 Engine electronics'; Category: 'Engine';
      Supported: True; Custom: False),
    (Name: 'TPMS relearn'; Ecu: '03 Brakes (indirect)'; Category: 'Chassis';
      Supported: True; Custom: False),
    (Name: 'Throttle relearn'; Ecu: '01 Engine electronics'; Category: 'Engine';
      Supported: True; Custom: False),
    (Name: 'Brake bleeding'; Ecu: '03 Brakes'; Category: 'Brakes';
      Supported: True; Custom: False),
    (Name: 'Clear adaptations'; Ecu: '02 Transmission'; Category: 'Maintenance';
      Supported: True; Custom: False),
    (Name: 'Glow plug test'; Ecu: '01 Engine '#183' garage'; Category: 'Garage';
      Supported: True; Custom: True),
    (Name: 'AWD coupling calibration'; Ecu: 'Not fitted'; Category: 'Chassis';
      Supported: False; Custom: False),
    (Name: 'Air suspension calibration'; Ecu: 'Not fitted'; Category: 'Chassis';
      Supported: False; Custom: False)
  );

procedure TfrmService.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FSelectedService := 1;
  FillVehicle;
  FillCategories;
  RefreshCatalogue;
  LoadService(FSelectedService);
  ConnectionChanged;
end;

procedure TfrmService.Tick;
var
  Battery: Double;
  Progress: Integer;
begin
  if (FHost <> nil) and FHost.Connected and
    (procService.LiveValues.Count >= 4) then
  begin
    Battery := FHost.Simulator.Value(PID_MODULE_VOLTAGE);
    procService.LiveValues[3].ValueText := FormatFloat('0.0 V', Battery);
  end;
  if not FRunActive then
    Exit;
  Inc(FRunTick);
  Progress := EnsureRange(FRunTick * 4, 0, 100);
  if procService.LiveValues.Count > 1 then
  begin
    procService.LiveValues[1].ValueText := Format('Running '#183' %d%%',
      [Progress]);
    procService.LiveValues[1].Fraction := Progress / 100;
  end;
  if FHost <> nil then
    FHost.SetProgress(SERVICE_SAMPLES[FSelectedService].Name, Progress);
  if FRunTick >= 25 then
  begin
    FRunTick := 0;
    if FStep < 4 then
    begin
      Inc(FStep);
      UpdateProcedureState;
      if FHost <> nil then
        FHost.Log(Format('%s: step %d completed.',
          [SERVICE_SAMPLES[FSelectedService].Name, FStep]));
    end
    else
      FinishProcedure;
  end;
end;

procedure TfrmService.ConnectionChanged;
begin
  FillVehicle;
  btnRun.Enabled := SERVICE_SAMPLES[FSelectedService].Supported and
    ((FHost = nil) or FHost.Connected);
  LoadPreconditions;
end;

procedure TfrmService.edSearchChange(Sender: TObject);
begin
  RefreshCatalogue;
end;

procedure TfrmService.cbCategoryChange(Sender: TObject);
begin
  RefreshCatalogue;
end;

procedure TfrmService.tilesCatalogueChange(Sender: TObject);
begin
  LoadService(TileServiceIndex(tilesCatalogue.ItemIndex));
end;

procedure TfrmService.tilesCatalogueItemActivate(Sender: TObject;
  AIndex: Integer);
begin
  LoadService(TileServiceIndex(AIndex));
end;

procedure TfrmService.tilesCatalogueItemClick(Sender: TObject; AIndex: Integer);
begin
  LoadService(TileServiceIndex(AIndex));
end;

procedure TfrmService.btnRunClick(Sender: TObject);
begin
  StartProcedure;
end;

procedure TfrmService.procServiceNext(Sender: TObject);
begin
  if FStep < 4 then
  begin
    Inc(FStep);
    UpdateProcedureState;
  end;
end;

procedure TfrmService.procServiceBack(Sender: TObject);
begin
  if FStep > 0 then
  begin
    Dec(FStep);
    UpdateProcedureState;
  end;
end;

procedure TfrmService.procServiceAbort(Sender: TObject);
begin
  if not FRunActive then
    Exit;
  FRunActive := False;
  btnRun.Enabled := True;
  if FHost <> nil then
  begin
    FHost.SetProgress('', -1);
    FHost.LogWarning(SERVICE_SAMPLES[FSelectedService].Name + ' aborted.');
  end;
  UpdateProcedureState;
end;

procedure TfrmService.procServiceAction(Sender: TObject);
begin
  StartProcedure;
end;

procedure TfrmService.FillVehicle;
begin
  vcVehicle.Layout := vlCompact;
  vcVehicle.VIN := 'WVWZZZAUZGW123456';
  vcVehicle.Make := 'Volkswagen';
  vcVehicle.Model := 'Golf VII';
  vcVehicle.ModelYear := 2016;
  vcVehicle.Engine := '1.6 TDI (CR)';
  vcVehicle.BodyStyle := 'Hatchback';
  vcVehicle.FuelType := 'Diesel';
  vcVehicle.EngineCode := 'CRKB';
  vcVehicle.EnginePower := '81 kW';
  vcVehicle.ProtocolCompact := 'CAN 11 bit 500 kbaud';
  vcVehicle.EcuCount := 14;
  vcVehicle.Odometer := '184352 km';
  vcVehicle.MilOn := True;
  vcVehicle.ScanTimeText := 'Garage Peeters '#183' service bay';
  vcVehicle.ActionCaption := 'Job 2026-0142';
  if FHost <> nil then
    vcVehicle.Connected := FHost.Connected
  else
    vcVehicle.Connected := False;
end;

procedure TfrmService.FillCategories;
begin
  cbCategory.Items.BeginUpdate;
  try
    cbCategory.Items.Clear;
    cbCategory.Items.Add('All');
    cbCategory.Items.Add('Maintenance');
    cbCategory.Items.Add('Brakes');
    cbCategory.Items.Add('Engine');
    cbCategory.Items.Add('Chassis');
    cbCategory.Items.Add('Garage');
    cbCategory.ItemIndex := 0;
  finally
    cbCategory.Items.EndUpdate;
  end;
end;

procedure TfrmService.RefreshCatalogue;
var
  I: Integer;
  Query: string;
  Category: string;
  FirstAny: Integer;
  FirstSelectable: Integer;
  SelectedIndex: Integer;
  Tile: TOBDTileItem;
begin
  Query := LowerCase(Trim(edSearch.Text));
  if cbCategory.ItemIndex >= 0 then
    Category := cbCategory.Items[cbCategory.ItemIndex]
  else
    Category := 'All';

  FirstAny := -1;
  FirstSelectable := -1;
  SelectedIndex := -1;
  tilesCatalogue.BeginUpdate;
  try
    tilesCatalogue.Filter := '';
    tilesCatalogue.Clear;
    for I := 0 to SERVICE_COUNT - 1 do
    begin
      if (Category <> 'All') and not SameText(Category,
        SERVICE_SAMPLES[I].Category) then
        Continue;

      Tile := tilesCatalogue.AddTile(SERVICE_SAMPLES[I].Name,
        SERVICE_SAMPLES[I].Ecu, ServiceGlyph(I), SERVICE_SAMPLES[I].Category);
      Tile.Tag := I;
      if SERVICE_SAMPLES[I].Supported then
      begin
        if SERVICE_SAMPLES[I].Custom then
        begin
          Tile.Badge := 'Garage';
          Tile.BadgeTone := skAccent;
        end
        else
        begin
          Tile.Badge := 'Ready';
          Tile.BadgeTone := skSuccess;
        end;
      end
      else
      begin
        Tile.Subtitle := 'Not fitted';
        Tile.Badge := 'Not fitted';
        Tile.BadgeTone := skNeutral;
        Tile.Enabled := False;
      end;

      if ServiceMatchesFilter(I, Query) then
      begin
        if FirstAny < 0 then
          FirstAny := Tile.Index;
        if SERVICE_SAMPLES[I].Supported and (FirstSelectable < 0) then
          FirstSelectable := Tile.Index;
        if SERVICE_SAMPLES[I].Supported and (I = FSelectedService) then
          SelectedIndex := Tile.Index;
      end;
    end;
    tilesCatalogue.Filter := Query;
  finally
    tilesCatalogue.EndUpdate;
  end;

  if SelectedIndex < 0 then
    SelectedIndex := FirstSelectable;
  tilesCatalogue.ItemIndex := SelectedIndex;
  if tilesCatalogue.ItemIndex >= 0 then
    LoadService(TileServiceIndex(tilesCatalogue.ItemIndex))
  else if FirstAny >= 0 then
    LoadService(TileServiceIndex(FirstAny));
end;

procedure TfrmService.LoadService(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= SERVICE_COUNT) then
    Exit;
  FSelectedService := AIndex;
  cdCatalogue.FooterText := 'From catalogs/procedures; garage files are listed under Garage.';
  btnRun.Enabled := SERVICE_SAMPLES[AIndex].Supported and
    ((FHost = nil) or FHost.Connected);
  if SERVICE_SAMPLES[AIndex].Supported then
    btnRun.Caption := 'Run function'
  else
    btnRun.Caption := 'Not fitted';
  LoadProcedure(SERVICE_SAMPLES[AIndex].Name, SERVICE_SAMPLES[AIndex].Ecu);
  LoadPreconditions;
end;

procedure TfrmService.LoadProcedure(const ATitle, AECU: string);
var
  Step: TOBDProcedureStep;
begin
  procService.BeginUpdate;
  try
    procService.Clear;
    procService.Title := ATitle;
    procService.Subtitle := AECU + ' '#183' Golf VII MQB catalogue';
    procService.Glyph := glRoutine;
    procService.SourceText := 'catalogs/procedures/vag/mqb-service.json';
    Step := procService.AddStep('Check', 'Check preconditions');
    Step.Instructions.Add('Connect a stable battery charger.');
    Step.Instructions.Add('Keep ignition on and do not disconnect the adapter.');
    Step.Glyph := glCheck;
    Step.ActionCaption := 'Re-check';
    Step := procService.AddStep('Start', 'Start the control-unit routine');
    Step.Instructions.Add('Send the routine-control start request to the ECU.');
    Step.Instructions.Add('Watch the live values until the ECU reports active.');
    Step.PictureCaption := 'Rear caliper service mode';
    Step.Glyph := glBrake;
    Step.WarningTitle := 'Moving components';
    Step.WarningText := 'Keep hands away from the calipers and brake pedal.';
    Step.ActionCaption := 'Start routine';
    Step := procService.AddStep('Run', 'Wait for routine progress');
    Step.Instructions.Add('Follow the on-screen progress and workshop manual.');
    Step.Instructions.Add('Do not cycle ignition while the routine is active.');
    Step.Glyph := glPending;
    Step.ActionCaption := 'Read status';
    Step := procService.AddStep('Verify', 'Verify ECU result');
    Step.Instructions.Add('Read final status and clear routine temporary data.');
    Step.Instructions.Add('Check that no new DTC was logged.');
    Step.Glyph := glRead;
    Step.ActionCaption := 'Verify';
    Step := procService.AddStep('Finish', 'Close the procedure');
    Step.Instructions.Add('Store the result in the Garage Peeters job log.');
    Step.Instructions.Add('Return the vehicle to the customer state.');
    Step.Glyph := glCheck;
    procService.AddLiveValue('ECU', AECU, opvNeutral, -1);
    procService.AddLiveValue('Routine', 'Ready', opvAccent, 0);
    procService.AddLiveValue('Engine', '0 rpm', opvSuccess, -1);
    procService.AddLiveValue('Battery', '12.7 V', opvSuccess, -1);
    FStep := 0;
  finally
    procService.EndUpdate;
  end;
  UpdateProcedureState;
end;

procedure TfrmService.LoadPreconditions;
var
  Voltage: string;
begin
  Voltage := '12.7 V';
  if (FHost <> nil) and FHost.Connected then
    Voltage := FormatFloat('0.0 V', FHost.Simulator.Value(PID_MODULE_VOLTAGE));
  preService.BeginUpdate;
  try
    preService.Clear;
    preService.Title := 'Preconditions '#183' ' + SERVICE_SAMPLES[FSelectedService].Name;
    preService.FooterText := 'Checked again before every step.';
    preService.AddPrecondition(pcnMet, 'Ignition on', 'Terminal 15');
    preService.AddPrecondition(pcnMet, 'Engine off', '0 rpm');
    preService.AddPrecondition(pcnMet, 'Battery voltage above 12.5 V', Voltage);
    preService.AddPrecondition(pcnMet, 'Battery charger connected', 'Detected');
    preService.AddPrecondition(pcnMet, 'Security access', 'Granted');
  finally
    preService.EndUpdate;
  end;
end;

procedure TfrmService.UpdateProcedureState;
begin
  procService.CurrentStep := FStep;
  procService.NextEnabled := not FRunActive and (FStep < 4);
  if procService.LiveValues.Count > 1 then
  begin
    if FRunActive then
    begin
      procService.LiveValues[1].ValueText := 'Running '#183' 0%';
      procService.LiveValues[1].State := opvAccent;
      procService.LiveValues[1].Fraction := 0;
    end
    else if FStep >= 4 then
    begin
      procService.LiveValues[1].ValueText := 'Completed';
      procService.LiveValues[1].State := opvSuccess;
      procService.LiveValues[1].Fraction := 1;
    end
    else
    begin
      procService.LiveValues[1].ValueText := 'Ready';
      procService.LiveValues[1].State := opvAccent;
      procService.LiveValues[1].Fraction := 0;
    end;
  end;
end;

procedure TfrmService.StartProcedure;
begin
  if FRunActive or not SERVICE_SAMPLES[FSelectedService].Supported then
    Exit;
  if (FHost <> nil) and not FHost.Connected then
  begin
    FHost.Notify(tkWarning, 'Adapter offline',
      'Connect to the Golf VII before running a service function.');
    Exit;
  end;
  FRunActive := True;
  FRunTick := 0;
  btnRun.Enabled := False;
  if FHost <> nil then
  begin
    FHost.Log('Started service function: ' +
      SERVICE_SAMPLES[FSelectedService].Name + '.');
    FHost.SetProgress(SERVICE_SAMPLES[FSelectedService].Name, 0);
  end;
  UpdateProcedureState;
end;

procedure TfrmService.FinishProcedure;
begin
  FRunActive := False;
  btnRun.Enabled := SERVICE_SAMPLES[FSelectedService].Supported and
    ((FHost = nil) or FHost.Connected);
  FStep := 4;
  UpdateProcedureState;
  if FHost <> nil then
  begin
    FHost.SetProgress('', -1);
    FHost.Log(SERVICE_SAMPLES[FSelectedService].Name + ' completed.');
    FHost.Notify(tkSuccess, 'Service function complete',
      SERVICE_SAMPLES[FSelectedService].Name + ' finished for the Golf VII.');
  end;
end;

function TfrmService.ServiceGlyph(AIndex: Integer): TOBDGlyph;
begin
  case AIndex of
    0:
      Result := glOil;
    1, 7:
      Result := glBrake;
    2:
      Result := glSteering;
    3:
      Result := glBattery;
    4:
      Result := glDpfRegen;
    5:
      Result := glTpms;
    9:
      Result := glFan;
  else
    Result := glRoutine;
  end;
end;

function TfrmService.ServiceMatchesFilter(AIndex: Integer;
  const AQuery: string): Boolean;
begin
  Result := AQuery = '';
  if Result or (AIndex < 0) or (AIndex >= SERVICE_COUNT) then
    Exit;

  Result := (Pos(AQuery, LowerCase(SERVICE_SAMPLES[AIndex].Name)) > 0) or
    (Pos(AQuery, LowerCase(SERVICE_SAMPLES[AIndex].Ecu)) > 0) or
    (Pos(AQuery, LowerCase(SERVICE_SAMPLES[AIndex].Category)) > 0);
  if not Result and not SERVICE_SAMPLES[AIndex].Supported then
    Result := Pos(AQuery, 'not fitted') > 0;
end;

function TfrmService.TileServiceIndex(AIndex: Integer): Integer;
begin
  Result := -1;
  if (AIndex >= 0) and (AIndex < tilesCatalogue.Items.Count) then
    Result := tilesCatalogue.Items[AIndex].Tag;
end;

end.
