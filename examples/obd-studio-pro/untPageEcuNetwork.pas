//------------------------------------------------------------------------------
//  untPageEcuNetwork
//
//  OBD Studio Pro page showing the vehicle control-unit network with a
//  topology view, ECU list, ECU details and a vehicle strip.
//
//  Components used: TOBDVehicleInfoCard, TOBDEcuTopology, TOBDEcuList,
//  TOBDCard, TOBDKeyValueList and TOBDButton.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageEcuNetwork;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.VehicleCard, ERD.UI.EcuTopology, ERD.UI.Card, ERD.UI.KeyValueList,
  ERD.UI.Buttons,
  untProPage, untSimulator;

type
  TfrmEcuNetwork = class(TFrame, IProPage)
    vcVehicle: TOBDVehicleInfoCard;
    pnlBody: TPanel;
    pnlLeft: TPanel;
    btnScanAll: TOBDButton;
    ecuList: TOBDEcuList;
    pnlRight: TPanel;
    cdEcuDetail: TOBDCard;
    kvEcuDetail: TOBDKeyValueList;
    topoNetwork: TOBDEcuTopology;
    procedure btnScanAllClick(Sender: TObject);
    procedure EcuSelect(Sender: TObject; const AAddress: string;
      AIndex: Integer);
  private
    FHost: IProHost;
    FSelectedAddress: string;
    FScanActive: Boolean;
    FScanTick: Integer;
    FScanIndex: Integer;
    procedure FillVehicle;
    procedure FillNetwork(AInitialScan: Boolean);
    procedure ApplyScanState(AIndex: Integer; AScanned: Boolean);
    procedure SelectEcu(const AAddress: string);
    procedure UpdateDetail;
    procedure StartScan;
    procedure FinishScan;
    function FindEcuIndex(const AAddress: string): Integer;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

type
  TEcuSample = record
    Address: string;
    Name: string;
    PartNumber: string;
    SystemText: string;
    FaultCount: Integer;
    State: TOBDEcuNodeState;
    BusIndex: Integer;
    Hardware: string;
    Software: string;
  end;

const
  ECU_COUNT = 14;
  ECU_SAMPLES: array [0 .. ECU_COUNT - 1] of TEcuSample = (
    (Address: '01'; Name: 'Engine electronics'; PartNumber: '04L906056HT';
      SystemText: 'EDC17C64'; FaultCount: 1; State: ensFaults; BusIndex: 0;
      Hardware: 'H35'; Software: '9978'),
    (Address: '02'; Name: 'Transmission (DSG)'; PartNumber: '0AM300050R';
      SystemText: 'DQ200'; FaultCount: 0; State: ensOk; BusIndex: 0;
      Hardware: 'H42'; Software: '5211'),
    (Address: '03'; Name: 'Brakes (ABS/ESC)'; PartNumber: '5Q0907379AA';
      SystemText: 'MK100'; FaultCount: 1; State: ensFaults; BusIndex: 1;
      Hardware: 'H32'; Software: '0359'),
    (Address: '08'; Name: 'Climatronic'; PartNumber: '5G0907044EM';
      SystemText: ''; FaultCount: 0; State: ensOk; BusIndex: 2;
      Hardware: 'H05'; Software: '0910'),
    (Address: '09'; Name: 'Central electrics'; PartNumber: '5Q0937084CK';
      SystemText: 'BCM MQB'; FaultCount: 1; State: ensFaults; BusIndex: 2;
      Hardware: '5Q0937084AN H45'; Software: '0254'),
    (Address: '13'; Name: 'Distance regulation'; PartNumber: '5Q0907572H';
      SystemText: 'ACC'; FaultCount: 0; State: ensOk; BusIndex: 4;
      Hardware: 'H07'; Software: '0361'),
    (Address: '15'; Name: 'Airbag'; PartNumber: '5Q0959655BJ';
      SystemText: ''; FaultCount: 1; State: ensFaults; BusIndex: 1;
      Hardware: 'H13'; Software: '0122'),
    (Address: '16'; Name: 'Steering column'; PartNumber: '5Q0953507DH';
      SystemText: ''; FaultCount: 0; State: ensOk; BusIndex: 2;
      Hardware: 'H18'; Software: '0110'),
    (Address: '17'; Name: 'Instruments'; PartNumber: '5G1920741A';
      SystemText: ''; FaultCount: 0; State: ensOk; BusIndex: 2;
      Hardware: 'H18'; Software: '0420'),
    (Address: '19'; Name: 'Gateway'; PartNumber: '5Q0907530AH';
      SystemText: 'J533'; FaultCount: 0; State: ensOk; BusIndex: 2;
      Hardware: 'H16'; Software: '1620'),
    (Address: '44'; Name: 'Steering assist'; PartNumber: '5Q0909144R';
      SystemText: ''; FaultCount: 1; State: ensFaults; BusIndex: 1;
      Hardware: 'H06'; Software: '0041'),
    (Address: '53'; Name: 'Parking brake'; PartNumber: '5Q0907801B';
      SystemText: ''; FaultCount: 0; State: ensOk; BusIndex: 1;
      Hardware: 'H10'; Software: '0101'),
    (Address: '5F'; Name: 'Infotainment'; PartNumber: '5G0035867A';
      SystemText: 'MIB2'; FaultCount: 1; State: ensLocked; BusIndex: 3;
      Hardware: 'H41'; Software: '0478'),
    (Address: 'A5'; Name: 'Front camera'; PartNumber: '3Q0980653';
      SystemText: ''; FaultCount: 0; State: ensNoResponse; BusIndex: 4;
      Hardware: 'H05'; Software: '0189')
  );

function FaultText(const ASample: TEcuSample): string;
begin
  case ASample.State of
    ensFaults:
      begin
        if ASample.FaultCount = 1 then
          Result := Format('%d fault', [ASample.FaultCount])
        else
          Result := Format('%d faults', [ASample.FaultCount]);
      end;
    ensLocked:
      Result := 'Protected';
    ensNoResponse:
      Result := 'No reply';
  else
    Result := 'OK';
  end;
end;

{ TfrmEcuNetwork }

procedure TfrmEcuNetwork.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FSelectedAddress := '09';
  FillVehicle;
  FillNetwork(True);
  SelectEcu(FSelectedAddress);
  ConnectionChanged;
end;

procedure TfrmEcuNetwork.Tick;
begin
  if not FScanActive then
    Exit;
  Inc(FScanTick);
  if FScanTick mod 3 = 0 then
  begin
    if FScanIndex < ECU_COUNT then
    begin
      ApplyScanState(FScanIndex, True);
      if FHost <> nil then
        FHost.SetProgress(Format('Scanning ECU %s %s',
          [ECU_SAMPLES[FScanIndex].Address, ECU_SAMPLES[FScanIndex].Name]),
          Round((FScanIndex + 1) * 100 / ECU_COUNT));
      Inc(FScanIndex);
    end
    else
      FinishScan;
  end;
end;

procedure TfrmEcuNetwork.ConnectionChanged;
begin
  FillVehicle;
  if FHost <> nil then
    btnScanAll.Enabled := FHost.Connected
  else
    btnScanAll.Enabled := False;
end;

procedure TfrmEcuNetwork.btnScanAllClick(Sender: TObject);
begin
  StartScan;
end;

procedure TfrmEcuNetwork.EcuSelect(Sender: TObject; const AAddress: string;
  AIndex: Integer);
begin
  SelectEcu(AAddress);
end;

procedure TfrmEcuNetwork.FillVehicle;
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
  vcVehicle.Protocol := 'ISO 15765-4 CAN 11 bit 500 kbaud';
  vcVehicle.ProtocolCompact := 'CAN 11 bit 500 kbaud';
  vcVehicle.EcuCount := ECU_COUNT;
  vcVehicle.Odometer := '184352 km';
  vcVehicle.CalibrationID := '04L906056HT 9978';
  vcVehicle.MilOn := True;
  vcVehicle.ScanTimeText := 'Garage Peeters '#183' 18:40';
  vcVehicle.ActionCaption := 'Job 2026-0142';
  if FHost <> nil then
    vcVehicle.Connected := FHost.Connected
  else
    vcVehicle.Connected := False;
end;

procedure TfrmEcuNetwork.FillNetwork(AInitialScan: Boolean);
var
  I: Integer;
  Item: TOBDEcuItem;
begin
  ecuList.Clear;
  topoNetwork.Clear;
  ecuList.AddBus('Powertrain CAN', '500 kbit/s');
  ecuList.AddBus('Chassis CAN', '500 kbit/s');
  ecuList.AddBus('Comfort CAN', '500 kbit/s');
  ecuList.AddBus('Infotainment CAN', '500 kbit/s');
  ecuList.AddBus('Extended CAN', '500 kbit/s');
  topoNetwork.AddBus('Powertrain CAN', '500 kbit/s');
  topoNetwork.AddBus('Chassis CAN', '500 kbit/s');
  topoNetwork.AddBus('Comfort CAN', '500 kbit/s');
  topoNetwork.AddBus('Infotainment CAN', '500 kbit/s');
  topoNetwork.AddBus('Extended CAN', '500 kbit/s');
  for I := 0 to ECU_COUNT - 1 do
  begin
    if AInitialScan then
    begin
      Item := ecuList.AddEcu(ECU_SAMPLES[I].Address, ECU_SAMPLES[I].Name,
        ECU_SAMPLES[I].PartNumber, ECU_SAMPLES[I].BusIndex,
        ECU_SAMPLES[I].State, ECU_SAMPLES[I].FaultCount);
      Item.SystemText := ECU_SAMPLES[I].SystemText;
      Item := topoNetwork.AddEcu(ECU_SAMPLES[I].Address, ECU_SAMPLES[I].Name,
        ECU_SAMPLES[I].PartNumber, ECU_SAMPLES[I].BusIndex,
        ECU_SAMPLES[I].State, ECU_SAMPLES[I].FaultCount);
      Item.SystemText := ECU_SAMPLES[I].SystemText;
    end
    else
    begin
      Item := ecuList.AddEcu(ECU_SAMPLES[I].Address, ECU_SAMPLES[I].Name,
        ECU_SAMPLES[I].PartNumber, ECU_SAMPLES[I].BusIndex, ensNotScanned, 0);
      Item.SystemText := ECU_SAMPLES[I].SystemText;
      Item := topoNetwork.AddEcu(ECU_SAMPLES[I].Address, ECU_SAMPLES[I].Name,
        ECU_SAMPLES[I].PartNumber, ECU_SAMPLES[I].BusIndex, ensNotScanned, 0);
      Item.SystemText := ECU_SAMPLES[I].SystemText;
    end;
  end;
  ecuList.SelectedAddress := FSelectedAddress;
  topoNetwork.SelectedAddress := FSelectedAddress;
end;

procedure TfrmEcuNetwork.ApplyScanState(AIndex: Integer; AScanned: Boolean);
var
  State: TOBDEcuNodeState;
  Faults: Integer;
begin
  if (AIndex < 0) or (AIndex >= ECU_COUNT) then
    Exit;
  if AScanned then
  begin
    State := ECU_SAMPLES[AIndex].State;
    Faults := ECU_SAMPLES[AIndex].FaultCount;
  end
  else
  begin
    State := ensNotScanned;
    Faults := 0;
  end;
  if AIndex < ecuList.Ecus.Count then
  begin
    ecuList.Ecus[AIndex].State := State;
    ecuList.Ecus[AIndex].FaultCount := Faults;
  end;
  if AIndex < topoNetwork.Ecus.Count then
  begin
    topoNetwork.Ecus[AIndex].State := State;
    topoNetwork.Ecus[AIndex].FaultCount := Faults;
  end;
  UpdateDetail;
end;

procedure TfrmEcuNetwork.SelectEcu(const AAddress: string);
begin
  if AAddress = '' then
    Exit;
  FSelectedAddress := AAddress;
  ecuList.SelectedAddress := AAddress;
  topoNetwork.SelectedAddress := AAddress;
  UpdateDetail;
end;

procedure TfrmEcuNetwork.UpdateDetail;
var
  I: Integer;
  Item: TOBDKeyValueItem;
  Tone: TOBDStatusKind;
begin
  I := FindEcuIndex(FSelectedAddress);
  if I < 0 then
    Exit;
  cdEcuDetail.Title := ECU_SAMPLES[I].Address + ' ' + ECU_SAMPLES[I].Name;
  case ECU_SAMPLES[I].State of
    ensFaults:
      begin
        cdEcuDetail.StatusEdge := ceDanger;
        Tone := skDanger;
      end;
    ensLocked:
      begin
        cdEcuDetail.StatusEdge := ceAccent;
        Tone := skWarning;
      end;
    ensNoResponse:
      begin
        cdEcuDetail.StatusEdge := ceNeutral;
        Tone := skNeutral;
      end;
  else
    cdEcuDetail.StatusEdge := ceSuccess;
    Tone := skSuccess;
  end;
  kvEcuDetail.BeginUpdate;
  try
    kvEcuDetail.Clear;
    kvEcuDetail.Add('Identification', 'Diagnostic address',
      ECU_SAMPLES[I].Address, kvkMono);
    kvEcuDetail.Add('Identification', 'Control unit', ECU_SAMPLES[I].Name);
    kvEcuDetail.Add('Identification', 'Part number',
      ECU_SAMPLES[I].PartNumber, kvkMono);
    kvEcuDetail.Add('Identification', 'System', ECU_SAMPLES[I].SystemText);
    kvEcuDetail.Add('Software', 'Hardware', ECU_SAMPLES[I].Hardware, kvkMono);
    kvEcuDetail.Add('Software', 'Software', ECU_SAMPLES[I].Software, kvkMono);
    kvEcuDetail.Add('Software', 'Workshop code',
      'WSC 01234 '#183' 2026-10-11', kvkMono);
    kvEcuDetail.Add('Software', 'Protocol', 'UDS on CAN');
    Item := kvEcuDetail.Add('State', 'Result', FaultText(ECU_SAMPLES[I]),
      kvkChip);
    Item.Tone := Tone;
    if ECU_SAMPLES[I].Address = '01' then
    begin
      Item := kvEcuDetail.Add('State', 'Primary DTC',
        'P0401 EGR flow insufficient', kvkTone);
      Item.Tone := skDanger;
    end
    else if ECU_SAMPLES[I].Address = '09' then
      kvEcuDetail.Add('Coding', 'Long coding',
        '2D 0B 3C 84 40 1A 00 0C 80 6C 03 12 08 00 51 C0', kvkMono)
    else if ECU_SAMPLES[I].State = ensNoResponse then
    begin
      Item := kvEcuDetail.Add('State', 'Last response',
        'No response to address probe', kvkTone);
      Item.Tone := skWarning;
    end
    else if ECU_SAMPLES[I].State = ensLocked then
    begin
      Item := kvEcuDetail.Add('State', 'Access',
        'Security access required', kvkTone);
      Item.Tone := skWarning;
    end;
  finally
    kvEcuDetail.EndUpdate;
  end;
end;

procedure TfrmEcuNetwork.StartScan;
begin
  if FScanActive then
    Exit;
  if (FHost <> nil) and not FHost.Connected then
  begin
    FHost.Notify(tkWarning, 'Adapter offline',
      'Connect to the vehicle before scanning the gateway list.');
    Exit;
  end;
  FScanActive := True;
  FScanTick := 0;
  FScanIndex := 0;
  btnScanAll.Enabled := False;
  FillNetwork(False);
  if FHost <> nil then
  begin
    FHost.Log('Started network scan for Golf VII at Garage Peeters.');
    FHost.SetProgress('Scanning control units', 0);
  end;
end;

procedure TfrmEcuNetwork.FinishScan;
begin
  FScanActive := False;
  btnScanAll.Enabled := True;
  if FHost <> nil then
  begin
    FHost.SetProgress('', -1);
    FHost.LogWarning('Network scan completed: engine ECU reports P0401.');
    FHost.Notify(tkSuccess, 'Network scan complete',
      '14 ECUs found; 6 with faults including 01 Engine P0401.');
  end;
end;

function TfrmEcuNetwork.FindEcuIndex(const AAddress: string): Integer;
var
  I: Integer;
begin
  for I := 0 to ECU_COUNT - 1 do
    if SameText(ECU_SAMPLES[I].Address, AAddress) then
      Exit(I);
  Result := -1;
end;

end.
