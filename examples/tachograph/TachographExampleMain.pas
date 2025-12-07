unit TachographExampleMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Grids,
  OBD.Adapter.ELM327, OBD.Connection, OBD.Protocol.Tacho;

type
  TTachographForm = class(TForm)
    MainPanel: TPanel;
    TopPanel: TPanel;
    TitleLabel: TLabel;
    ConnectionGroupBox: TGroupBox;
    PortLabel: TLabel;
    PortComboBox: TComboBox;
    BaudRateLabel: TLabel;
    BaudRateComboBox: TComboBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    VehicleGroupBox: TGroupBox;
    VehicleMakeLabel: TLabel;
    VehicleMakeComboBox: TComboBox;
    TachoManufacturerLabel: TLabel;
    TachoManufacturerComboBox: TComboBox;
    StandardLabel: TLabel;
    StandardComboBox: TComboBox;
    AutoDetectButton: TButton;
    OdometerGroupBox: TGroupBox;
    OdometerPanel: TPanel;
    TotalDistanceLabel: TLabel;
    TotalDistanceValue: TLabel;
    TripDistanceLabel: TLabel;
    TripDistanceValue: TLabel;
    CurrentSpeedLabel: TLabel;
    CurrentSpeedValue: TLabel;
    ReadOdometerButton: TButton;
    ResetTripButton: TButton;
    CalibrationGroupBox: TGroupBox;
    TireCircumferenceLabel: TLabel;
    TireCircumferenceEdit: TEdit;
    PulsesPerKmLabel: TLabel;
    PulsesPerKmEdit: TEdit;
    CalibrateButton: TButton;
    LastCalibrationLabel: TLabel;
    LastCalibrationValue: TLabel;
    DriverActivityGroupBox: TGroupBox;
    ActivityGrid: TStringGrid;
    ReadActivitiesButton: TButton;
    DaysLabel: TLabel;
    DaysEdit: TEdit;
    VehicleInfoGroupBox: TGroupBox;
    VINLabel: TLabel;
    VINValue: TLabel;
    VehicleUnitIDLabel: TLabel;
    VehicleUnitIDValue: TLabel;
    ReadVehicleInfoButton: TButton;
    StatusBar: TStatusBar;
    LogMemo: TMemo;
    LogLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure AutoDetectButtonClick(Sender: TObject);
    procedure ReadOdometerButtonClick(Sender: TObject);
    procedure ResetTripButtonClick(Sender: TObject);
    procedure CalibrateButtonClick(Sender: TObject);
    procedure ReadActivitiesButtonClick(Sender: TObject);
    procedure ReadVehicleInfoButtonClick(Sender: TObject);
  private
    FConnection: TOBDConnection;
    FAdapter: TOBDELM327Adapter;
    FTacho: TTachographProtocol;
    procedure Log(const Msg: string);
    procedure UpdateConnectionState(Connected: Boolean);
    function GetSelectedStandard: TTachoStandard;
    function GetSelectedMake: TTachoVehicleMake;
    function GetSelectedManufacturer: TTachoManufacturer;
  end;

var
  TachographForm: TTachographForm;

implementation

{$R *.dfm}

uses
  System.DateUtils;

procedure TTachographForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Initialize COM ports list
  PortComboBox.Clear;
  for I := 1 to 20 do
    PortComboBox.Items.Add('COM' + IntToStr(I));
  PortComboBox.ItemIndex := 0;
  
  // Initialize baud rates
  BaudRateComboBox.Items.AddStrings(['9600', '19200', '38400', '57600', '115200', '230400', '500000']);
  BaudRateComboBox.ItemIndex := 4; // 115200
  
  // Initialize vehicle makes
  VehicleMakeComboBox.Items.Add('Generic');
  VehicleMakeComboBox.Items.Add('Mercedes-Benz');
  VehicleMakeComboBox.Items.Add('Volvo Trucks');
  VehicleMakeComboBox.Items.Add('Scania');
  VehicleMakeComboBox.Items.Add('MAN');
  VehicleMakeComboBox.Items.Add('DAF');
  VehicleMakeComboBox.Items.Add('Iveco');
  VehicleMakeComboBox.Items.Add('Renault Trucks');
  VehicleMakeComboBox.Items.Add('Hino');
  VehicleMakeComboBox.Items.Add('Isuzu Trucks');
  VehicleMakeComboBox.Items.Add('Mitsubishi Fuso');
  VehicleMakeComboBox.Items.Add('UD Trucks/Nissan Diesel');
  VehicleMakeComboBox.Items.Add('Hyundai Trucks');
  VehicleMakeComboBox.Items.Add('Freightliner');
  VehicleMakeComboBox.Items.Add('Peterbilt');
  VehicleMakeComboBox.Items.Add('Kenworth');
  VehicleMakeComboBox.Items.Add('International');
  VehicleMakeComboBox.Items.Add('Mack Trucks');
  VehicleMakeComboBox.Items.Add('Western Star');
  VehicleMakeComboBox.Items.Add('Volvo NA');
  VehicleMakeComboBox.ItemIndex := 0;
  
  // Initialize tachograph manufacturers
  TachoManufacturerComboBox.Items.Add('Generic');
  TachoManufacturerComboBox.Items.Add('VDO/Continental');
  TachoManufacturerComboBox.Items.Add('Stoneridge');
  TachoManufacturerComboBox.Items.Add('Denso');
  TachoManufacturerComboBox.Items.Add('Continental');
  TachoManufacturerComboBox.Items.Add('Yazaki');
  TachoManufacturerComboBox.Items.Add('Actia');
  TachoManufacturerComboBox.Items.Add('Cummins');
  TachoManufacturerComboBox.Items.Add('Detroit Diesel');
  TachoManufacturerComboBox.ItemIndex := 0;
  
  // Initialize standards
  StandardComboBox.Items.Add('EU Gen1 (1360/2002)');
  StandardComboBox.Items.Add('EU Gen2 Smart Tachograph (165/2014)');
  StandardComboBox.Items.Add('Korean E-Tachograph');
  StandardComboBox.Items.Add('Russian ERA-GLONASS');
  StandardComboBox.Items.Add('China GB 17691');
  StandardComboBox.Items.Add('Brazilian OBD-BR1');
  StandardComboBox.Items.Add('Japanese DPF/SCR');
  StandardComboBox.Items.Add('Australian ADR 80/03');
  StandardComboBox.ItemIndex := 1; // EU Gen2
  
  // Initialize activity grid
  ActivityGrid.ColCount := 5;
  ActivityGrid.RowCount := 2;
  ActivityGrid.FixedRows := 1;
  ActivityGrid.Cells[0, 0] := 'Activity';
  ActivityGrid.Cells[1, 0] := 'Start Time';
  ActivityGrid.Cells[2, 0] := 'Duration (min)';
  ActivityGrid.Cells[3, 0] := 'Distance (km)';
  ActivityGrid.Cells[4, 0] := 'Status';
  ActivityGrid.ColWidths[0] := 80;
  ActivityGrid.ColWidths[1] := 120;
  ActivityGrid.ColWidths[2] := 100;
  ActivityGrid.ColWidths[3] := 100;
  ActivityGrid.ColWidths[4] := 100;
  
  // Set default values
  TireCircumferenceEdit.Text := '2000';
  PulsesPerKmEdit.Text := '4000';
  DaysEdit.Text := '7';
  
  UpdateConnectionState(False);
  Log('Tachograph/Odometer Example Ready');
end;

procedure TTachographForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FTacho) then
    FTacho.Free;
  if Assigned(FAdapter) then
    FAdapter.Free;
  if Assigned(FConnection) then
    FConnection.Free;
end;

procedure TTachographForm.Log(const Msg: string);
begin
  LogMemo.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  StatusBar.SimpleText := Msg;
end;

procedure TTachographForm.UpdateConnectionState(Connected: Boolean);
begin
  ConnectButton.Enabled := not Connected;
  DisconnectButton.Enabled := Connected;
  PortComboBox.Enabled := not Connected;
  BaudRateComboBox.Enabled := not Connected;
  
  AutoDetectButton.Enabled := Connected;
  ReadOdometerButton.Enabled := Connected;
  ResetTripButton.Enabled := Connected;
  CalibrateButton.Enabled := Connected;
  ReadActivitiesButton.Enabled := Connected;
  ReadVehicleInfoButton.Enabled := Connected;
end;

function TTachographForm.GetSelectedStandard: TTachoStandard;
begin
  case StandardComboBox.ItemIndex of
    0: Result := tsEU_1360_2002;
    1: Result := tsEU_165_2014;
    2: Result := tsKORETS;
    3: Result := tsRussiaERA;
    4: Result := tsChina_GB17691;
    5: Result := tsBrazilOBD_BR1;
    6: Result := tsJapanDPF;
    7: Result := tsAustraliaADR80;
  else
    Result := tsEU_165_2014;
  end;
end;

function TTachographForm.GetSelectedMake: TTachoVehicleMake;
begin
  case VehicleMakeComboBox.ItemIndex of
    0: Result := tvmGeneric;
    1: Result := tvmMercedes;
    2: Result := tvmVolvo;
    3: Result := tvmScania;
    4: Result := tvmMAN;
    5: Result := tvmDAF;
    6: Result := tvmIveco;
    7: Result := tvmRenaultTrucks;
    8: Result := tvmHino;
    9: Result := tvmIsuzuTruck;
    10: Result := tvmMitsubishiFuso;
    11: Result := tvmUDTrucks;
    12: Result := tvmHyundaiTruck;
    13: Result := tvmFreightliner;
    14: Result := tvmPeterbilt;
    15: Result := tvmKenworth;
    16: Result := tvmInternational;
    17: Result := tvmMack;
    18: Result := tvmWesternStar;
    19: Result := tvmVolvoNA;
  else
    Result := tvmGeneric;
  end;
end;

function TTachographForm.GetSelectedManufacturer: TTachoManufacturer;
begin
  case TachoManufacturerComboBox.ItemIndex of
    0: Result := tmGeneric;
    1: Result := tmVDO;
    2: Result := tmStoneridge;
    3: Result := tmDenso;
    4: Result := tmContinental;
    5: Result := tmYazaki;
    6: Result := tmActia;
    7: Result := tmCummins;
    8: Result := tmDetroitDiesel;
  else
    Result := tmGeneric;
  end;
end;

procedure TTachographForm.ConnectButtonClick(Sender: TObject);
var
  PortName: string;
  BaudRate: Integer;
begin
  try
    PortName := PortComboBox.Text;
    BaudRate := StrToInt(BaudRateComboBox.Text);
    
    Log('Connecting to ' + PortName + ' at ' + IntToStr(BaudRate) + ' baud...');
    
    // Create connection and adapter
    FConnection := TOBDConnection.Create(PortName, BaudRate);
    FAdapter := TOBDELM327Adapter.Create(FConnection);
    
    if FAdapter.Connect then
    begin
      Log('Connected successfully to ' + FAdapter.Version);
      
      // Create tachograph protocol
      FTacho := TTachographProtocol.Create(GetSelectedStandard, GetSelectedMake, GetSelectedManufacturer);
      
      UpdateConnectionState(True);
    end
    else
    begin
      Log('Connection failed');
      FAdapter.Free;
      FAdapter := nil;
      FConnection.Free;
      FConnection := nil;
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Connection error: ' + E.Message);
    end;
  end;
end;

procedure TTachographForm.DisconnectButtonClick(Sender: TObject);
begin
  try
    if Assigned(FTacho) then
    begin
      FTacho.Free;
      FTacho := nil;
    end;
    
    if Assigned(FAdapter) then
    begin
      FAdapter.Disconnect;
      FAdapter.Free;
      FAdapter := nil;
    end;
    
    if Assigned(FConnection) then
    begin
      FConnection.Free;
      FConnection := nil;
    end;
    
    Log('Disconnected');
    UpdateConnectionState(False);
  except
    on E: Exception do
      Log('Error during disconnect: ' + E.Message);
  end;
end;

procedure TTachographForm.AutoDetectButtonClick(Sender: TObject);
var
  ManufacturerName: string;
begin
  if not Assigned(FTacho) then
    Exit;
    
  try
    Log('Auto-detecting tachograph...');
    
    if FTacho.AutoDetect then
    begin
      case FTacho.Manufacturer of
        tmVDO: ManufacturerName := 'VDO/Continental';
        tmStoneridge: ManufacturerName := 'Stoneridge';
        tmDenso: ManufacturerName := 'Denso';
        tmContinental: ManufacturerName := 'Continental';
        tmYazaki: ManufacturerName := 'Yazaki';
        tmActia: ManufacturerName := 'Actia';
        tmCummins: ManufacturerName := 'Cummins';
        tmDetroitDiesel: ManufacturerName := 'Detroit Diesel';
      else
        ManufacturerName := 'Generic';
      end;
      
      Log('Detected: ' + ManufacturerName);
      ShowMessage('Tachograph detected: ' + ManufacturerName);
      
      // Update UI
      TachoManufacturerComboBox.ItemIndex := Ord(FTacho.Manufacturer);
    end
    else
    begin
      Log('Auto-detection failed');
      ShowMessage('Could not auto-detect tachograph. Using manual settings.');
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Auto-detection error: ' + E.Message);
    end;
  end;
end;

procedure TTachographForm.ReadOdometerButtonClick(Sender: TObject);
var
  Data: TOdometerData;
begin
  if not Assigned(FTacho) then
    Exit;
    
  try
    Log('Reading odometer data...');
    
    if FTacho.ReadOdometer(Data) then
    begin
      TotalDistanceValue.Caption := FormatFloat('#,##0', Data.TotalDistance) + ' km';
      TripDistanceValue.Caption := FormatFloat('#,##0', Data.TripDistance) + ' km';
      
      if Data.LastCalibration > 0 then
        LastCalibrationValue.Caption := FormatDateTime('dd/mm/yyyy', Data.LastCalibration)
      else
        LastCalibrationValue.Caption := 'N/A';
        
      // Read current speed
      CurrentSpeedValue.Caption := FormatFloat('0.0', FTacho.ReadCurrentSpeed) + ' km/h';
      
      Log('Odometer data read successfully');
      Log('Total: ' + IntToStr(Data.TotalDistance) + ' km, Trip: ' + IntToStr(Data.TripDistance) + ' km');
    end
    else
    begin
      Log('Failed to read odometer data');
      ShowMessage('Could not read odometer data. Check connection and authentication.');
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Odometer read error: ' + E.Message);
    end;
  end;
end;

procedure TTachographForm.ResetTripButtonClick(Sender: TObject);
begin
  if not Assigned(FTacho) then
    Exit;
    
  try
    if MessageDlg('Reset trip counter? This requires workshop authentication.', 
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
      
    Log('Resetting trip counter...');
    
    if FTacho.ResetTripCounter then
    begin
      Log('Trip counter reset successfully');
      TripDistanceValue.Caption := '0 km';
      ShowMessage('Trip counter has been reset.');
    end
    else
    begin
      Log('Failed to reset trip counter');
      ShowMessage('Could not reset trip counter. Check workshop authentication.');
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Reset error: ' + E.Message);
    end;
  end;
end;

procedure TTachographForm.CalibrateButtonClick(Sender: TObject);
var
  TireCirc, PulsesKm: Word;
begin
  if not Assigned(FTacho) then
    Exit;
    
  try
    TireCirc := StrToInt(TireCircumferenceEdit.Text);
    PulsesKm := StrToInt(PulsesPerKmEdit.Text);
    
    if MessageDlg('Calibrate odometer with these values?' + sLineBreak +
       'Tire Circumference: ' + IntToStr(TireCirc) + ' mm' + sLineBreak +
       'Pulses per km: ' + IntToStr(PulsesKm) + sLineBreak + sLineBreak +
       'This requires workshop card authentication.', 
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
      
    Log('Calibrating odometer...');
    
    if FTacho.CalibrateOdometer(TireCirc, PulsesKm) then
    begin
      Log('Odometer calibrated successfully');
      ShowMessage('Odometer has been calibrated.');
      LastCalibrationValue.Caption := FormatDateTime('dd/mm/yyyy', Now);
    end
    else
    begin
      Log('Failed to calibrate odometer');
      ShowMessage('Could not calibrate odometer. Check workshop authentication.');
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Calibration error: ' + E.Message);
    end;
  end;
end;

procedure TTachographForm.ReadActivitiesButtonClick(Sender: TObject);
var
  Days: Integer;
  Activities: TArray<TDriverActivity>;
  I, Row: Integer;
  ActivityName: string;
begin
  if not Assigned(FTacho) then
    Exit;
    
  try
    Days := StrToInt(DaysEdit.Text);
    if Days > 28 then Days := 28;
    if Days < 1 then Days := 1;
    
    Log('Reading driver activities for last ' + IntToStr(Days) + ' days...');
    
    Activities := FTacho.ReadDriverActivities(Days);
    
    if Length(Activities) > 0 then
    begin
      ActivityGrid.RowCount := Length(Activities) + 1;
      
      for I := 0 to High(Activities) do
      begin
        Row := I + 1;
        
        case Activities[I].Activity of
          taDriving: ActivityName := 'Driving';
          taWork: ActivityName := 'Work';
          taAvailable: ActivityName := 'Available';
          taRest: ActivityName := 'Rest';
        else
          ActivityName := 'Unknown';
        end;
        
        ActivityGrid.Cells[0, Row] := ActivityName;
        ActivityGrid.Cells[1, Row] := FormatDateTime('dd/mm/yyyy hh:nn', Activities[I].StartTime);
        ActivityGrid.Cells[2, Row] := IntToStr(Activities[I].Duration);
        ActivityGrid.Cells[3, Row] := IntToStr(Activities[I].Distance);
        ActivityGrid.Cells[4, Row] := 'Complete';
      end;
      
      Log('Read ' + IntToStr(Length(Activities)) + ' activity records');
    end
    else
    begin
      Log('No activity data available');
      ShowMessage('No driver activity data found.');
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Activity read error: ' + E.Message);
    end;
  end;
end;

procedure TTachographForm.ReadVehicleInfoButtonClick(Sender: TObject);
var
  VIN, UnitID: string;
begin
  if not Assigned(FTacho) then
    Exit;
    
  try
    Log('Reading vehicle information...');
    
    VIN := FTacho.ReadVIN;
    UnitID := FTacho.ReadVehicleUnitID;
    
    VINValue.Caption := VIN;
    VehicleUnitIDValue.Caption := UnitID;
    
    Log('VIN: ' + VIN);
    Log('Unit ID: ' + UnitID);
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Vehicle info read error: ' + E.Message);
    end;
  end;
end;

end.
