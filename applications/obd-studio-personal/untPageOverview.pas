//------------------------------------------------------------------------------
//  untPageOverview
//
//  Overview page for OBD-Studio Personal. It gives a one-screen health summary,
//  runs a background vehicle scan and saves a local text report.
//------------------------------------------------------------------------------

unit untPageOverview;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  ERD.I18n,
  untPersonalPage;

type
  TOverviewScanResult = record
    Timestamp: TDateTime;
    VIN: string;
    VehicleLine: string;
    VehicleDetail: string;
    ProtocolName: string;
    MILKnown: Boolean;
    MILOn: Boolean;
    MILDTCCount: Integer;
    ConfirmedCount: Integer;
    PendingCount: Integer;
    PermanentCount: Integer;
    ReadinessSupported: Integer;
    ReadinessComplete: Integer;
    ReadinessIncomplete: Integer;
    ReadinessError: string;
    CoolantText: string;
    BatteryText: string;
    RPMText: string;
    FuelLevelText: string;
    ErrorText: string;
  end;

  TfrmPageOverview = class(TFrame, IPersonalPage, IPersonalCommands)
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FScanning: Boolean;
    FScannedConnection: Boolean;
    FLastScan: TOverviewScanResult;
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblSubtitle: TLabel;
    btnScan: TButton;
    btnSaveReport: TButton;
    pnlEmpty: TPanel;
    lblEmptyTitle: TLabel;
    lblEmptyText: TLabel;
    btnEmptyConnect: TButton;
    btnDemo: TButton;
    sbContent: TScrollBox;
    grpVehicle: TGroupBox;
    grpHealth: TGroupBox;
    grpReadiness: TGroupBox;
    grpLive: TGroupBox;
    memAttention: TMemo;
    lblVehicleValue: TLabel;
    lblVINValue: TLabel;
    lblDetailValue: TLabel;
    lblMILValue: TLabel;
    lblCodesValue: TLabel;
    lblMILCountValue: TLabel;
    btnShowCodes: TButton;
    lblReadinessValue: TLabel;
    lblReadinessError: TLabel;
    btnShowReadiness: TButton;
    lblCoolantValue: TLabel;
    lblBatteryValue: TLabel;
    lblRPMValue: TLabel;
    lblFuelValue: TLabel;
    procedure BuildUI;
    procedure HeaderResize(Sender: TObject);
    procedure ContentResize(Sender: TObject);
    procedure ScanClicked(Sender: TObject);
    procedure SaveReportClicked(Sender: TObject);
    procedure EmptyConnectClicked(Sender: TObject);
    procedure DemoClicked(Sender: TObject);
    procedure ShowCodesClicked(Sender: TObject);
    procedure ShowReadinessClicked(Sender: TObject);
    procedure StartScan;
    procedure UpdateView;
    function TotalCodeCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
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

uses
  System.IOUtils,
  System.Math,
  ERD.UI.Toast,
  ERD.Service.DTCs,
  ERD.Service.LiveData,
  ERD.Service.DriveCycle.Types,
  ERD.Service.VINDecoder,
  ERD.Service.VINDecoder.Types,
  ERD.Service.VehicleHealth,
  untSettings,
  untSession;

function DashIfEmpty(const AText: string): string;
begin
  if Trim(AText) = '' then
    Result := '—'
  else
    Result := AText;
end;

procedure AppendLine(var AText: string; const ALine: string);
begin
  if ALine = '' then
    Exit;
  if AText <> '' then
    AText := AText + sLineBreak;
  AText := AText + ALine;
end;

function VINVehicleTypeText(AType: TOBDVINVehicleType): string;
begin
  case AType of
    vtPassengerCar:
      Result := 'Passenger car';
    vtTruck:
      Result := 'Truck';
    vtSUV:
      Result := 'SUV';
    vtVan:
      Result := 'Van';
    vtBus:
      Result := 'Bus';
    vtMotorcycle:
      Result := 'Motorcycle';
    vtElectric:
      Result := 'Electric vehicle';
    vtHybrid:
      Result := 'Hybrid vehicle';
  else
    Result := '';
  end;
end;

procedure ApplyVINInfo(const AInfo: TOBDVINInfo; var AScan: TOverviewScanResult);
var
  Parts: TStringList;
  Detail: TStringList;
  Body: string;
begin
  if AInfo.VIN <> '' then
    AScan.VIN := AInfo.VIN;
  Parts := TStringList.Create;
  Detail := TStringList.Create;
  try
    if AInfo.Manufacturer.Name <> '' then
      Parts.Add(AInfo.Manufacturer.Name)
    else if AInfo.WMI <> '' then
      Parts.Add(AInfo.WMI);
    if AInfo.ModelYear > 0 then
      Parts.Add(IntToStr(AInfo.ModelYear));
    Body := AInfo.Features.BodyStyle;
    if Body = '' then
      Body := VINVehicleTypeText(AInfo.Features.VehicleType);
    if Body <> '' then
      Parts.Add(Body);
    if Parts.Count > 0 then
      AScan.VehicleLine := StringReplace(Trim(Parts.Text), sLineBreak, '  ·  ',
        [rfReplaceAll]);

    if AInfo.Country.Name <> '' then
      Detail.Add(AInfo.Country.Name);
    if AInfo.Plant.Name <> '' then
      Detail.Add(AInfo.Plant.Name)
    else if AInfo.Plant.City <> '' then
      Detail.Add(AInfo.Plant.City);
    if AInfo.CheckDigitValid then
      Detail.Add('check digit valid')
    else if AInfo.CheckDigit <> #0 then
      Detail.Add('check digit not confirmed');
    if Detail.Count > 0 then
      AScan.VehicleDetail := StringReplace(Trim(Detail.Text), sLineBreak,
        '  ·  ', [rfReplaceAll]);
  finally
    Detail.Free;
    Parts.Free;
  end;
end;

procedure CountReadiness(const AMonitors: TArray<TOBDMonitorReadiness>;
  var ASupported, AComplete, AIncomplete: Integer);
var
  I: Integer;
begin
  ASupported := 0;
  AComplete := 0;
  AIncomplete := 0;
  for I := 0 to High(AMonitors) do
    if AMonitors[I].Supported then
    begin
      Inc(ASupported);
      if AMonitors[I].Complete then
        Inc(AComplete)
      else
        Inc(AIncomplete);
    end;
end;

function PIDText(const AValue: TOBDPIDValue): string;
begin
  if IsNan(AValue.Value) then
    Result := '—'
  else if AValue.Unit_ <> '' then
    Result := FormatFloat('0.#', AValue.Value) + ' ' + AValue.Unit_
  else
    Result := FormatFloat('0.#', AValue.Value);
end;

function TryReadPID(ALiveData: TOBDLiveData; APID: Byte): string;
var
  Value: TOBDPIDValue;
begin
  Result := '—';
  try
    Value := ALiveData.Read(APID);
    Result := PIDText(Value);
  except
    on E: Exception do
      Result := '—';
  end;
end;

function DtcKindText(AKind: TOBDDtcKind): string;
begin
  case AKind of
    dkConfirmed:
      Result := 'confirmed';
    dkPending:
      Result := 'pending';
    dkPermanent:
      Result := 'permanent';
    dkUDSStatusMask:
      Result := 'UDS';
  else
    Result := 'code';
  end;
end;

procedure AppendDTCs(ALines: TStrings; const ATitle: string;
  const AEntries: TArray<TOBDDtcEntry>);
var
  I: Integer;
  Line: string;
begin
  ALines.Add(ATitle + ': ' + IntToStr(Length(AEntries)));
  for I := 0 to High(AEntries) do
  begin
    Line := '  ' + AEntries[I].Code + '  ' + DtcKindText(AEntries[I].Kind);
    if AEntries[I].Ecu <> '' then
      Line := Line + '  ECU ' + AEntries[I].Ecu;
    if AEntries[I].Description <> '' then
      Line := Line + '  ' + AEntries[I].Description;
    ALines.Add(Line);
  end;
end;

function VehicleHealthReportToText(
  const AReport: TOBDVehicleHealthReport): string;
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('OBD-Studio Personal vehicle health report');
    Lines.Add('Created: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss',
      AReport.Timestamp));
    Lines.Add('');
    Lines.Add('VIN: ' + DashIfEmpty(AReport.VIN));
    if AReport.VINError <> '' then
      Lines.Add('VIN error: ' + AReport.VINError);
    if AReport.MILValid then
    begin
      if AReport.MIL.MILOn then
        Lines.Add('MIL: on')
      else
        Lines.Add('MIL: off');
      Lines.Add('MIL DTC count: ' + IntToStr(AReport.MIL.DTCCount));
    end
    else if AReport.MILError <> '' then
      Lines.Add('MIL error: ' + AReport.MILError);
    Lines.Add('');
    AppendDTCs(Lines, 'Stored codes', AReport.ConfirmedDTCs);
    AppendDTCs(Lines, 'Pending codes', AReport.PendingDTCs);
    AppendDTCs(Lines, 'Permanent codes', AReport.PermanentDTCs);
    Lines.Add('');
    Lines.Add('Live values:');
    for I := 0 to High(AReport.LiveValues) do
      if AReport.LiveValues[I].Failed then
        Lines.Add(Format('  PID 0x%.2X: failed', [AReport.LiveValues[I].PID]))
      else
        Lines.Add(Format('  PID 0x%.2X: %s', [AReport.LiveValues[I].PID,
          PIDText(AReport.LiveValues[I].Value)]));
    if AReport.LiveError <> '' then
      Lines.Add('Live data error: ' + AReport.LiveError);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ TfrmPageOverview }

constructor TfrmPageOverview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

procedure TfrmPageOverview.BuildUI;
var
  TopPos: Integer;

  function AddLabel(AParent: TWinControl; ALeft, ATop: Integer;
    const ACaption: string; ABold: Boolean = False): TLabel;
  begin
    Result := TLabel.Create(Self);
    Result.Parent := AParent;
    Result.Left := ALeft;
    Result.Top := ATop;
    Result.Caption := ACaption;
    Result.AutoSize := True;
    if ABold then
      Result.Font.Style := [fsBold];
  end;

begin
  Align := alClient;
  ParentBackground := False;

  pnlHeader := TPanel.Create(Self);
  pnlHeader.Parent := Self;
  pnlHeader.Align := alTop;
  pnlHeader.Height := 76;
  pnlHeader.BevelOuter := bvNone;
  pnlHeader.ParentBackground := False;
  pnlHeader.OnResize := HeaderResize;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := pnlHeader;
  lblTitle.Left := 16;
  lblTitle.Top := 10;
  lblTitle.Font.Size := 13;
  lblTitle.Font.Style := [fsBold];

  lblSubtitle := TLabel.Create(Self);
  lblSubtitle.Parent := pnlHeader;
  lblSubtitle.Left := 16;
  lblSubtitle.Top := 38;
  lblSubtitle.Width := 520;

  btnSaveReport := TButton.Create(Self);
  btnSaveReport.Parent := pnlHeader;
  btnSaveReport.Top := 22;
  btnSaveReport.Width := 112;
  btnSaveReport.OnClick := SaveReportClicked;

  btnScan := TButton.Create(Self);
  btnScan.Parent := pnlHeader;
  btnScan.Top := 22;
  btnScan.Width := 112;
  btnScan.Default := True;
  btnScan.OnClick := ScanClicked;

  pnlEmpty := TPanel.Create(Self);
  pnlEmpty.Parent := Self;
  pnlEmpty.Align := alClient;
  pnlEmpty.BevelOuter := bvNone;
  pnlEmpty.ParentBackground := False;

  lblEmptyTitle := TLabel.Create(Self);
  lblEmptyTitle.Parent := pnlEmpty;
  lblEmptyTitle.Left := 40;
  lblEmptyTitle.Top := 72;
  lblEmptyTitle.Font.Size := 14;
  lblEmptyTitle.Font.Style := [fsBold];

  lblEmptyText := TLabel.Create(Self);
  lblEmptyText.Parent := pnlEmpty;
  lblEmptyText.Left := 40;
  lblEmptyText.Top := 108;
  lblEmptyText.Width := 560;
  lblEmptyText.WordWrap := True;

  btnEmptyConnect := TButton.Create(Self);
  btnEmptyConnect.Parent := pnlEmpty;
  btnEmptyConnect.Left := 40;
  btnEmptyConnect.Top := 168;
  btnEmptyConnect.Width := 120;
  btnEmptyConnect.OnClick := EmptyConnectClicked;

  btnDemo := TButton.Create(Self);
  btnDemo.Parent := pnlEmpty;
  btnDemo.Left := 172;
  btnDemo.Top := 168;
  btnDemo.Width := 140;
  btnDemo.OnClick := DemoClicked;

  sbContent := TScrollBox.Create(Self);
  sbContent.Parent := Self;
  sbContent.Align := alClient;
  sbContent.BorderStyle := bsNone;
  sbContent.OnResize := ContentResize;

  grpVehicle := TGroupBox.Create(Self);
  grpVehicle.Parent := sbContent;
  lblVehicleValue := AddLabel(grpVehicle, 20, 32, '', True);
  lblVINValue := AddLabel(grpVehicle, 20, 62, '');
  lblDetailValue := AddLabel(grpVehicle, 20, 92, '');

  grpHealth := TGroupBox.Create(Self);
  grpHealth.Parent := sbContent;
  AddLabel(grpHealth, 20, 34, 'MIL');
  lblMILValue := AddLabel(grpHealth, 144, 34, '', True);
  AddLabel(grpHealth, 20, 62, 'Codes');
  lblCodesValue := AddLabel(grpHealth, 144, 62, '');
  AddLabel(grpHealth, 20, 90, 'PID 01 count');
  lblMILCountValue := AddLabel(grpHealth, 144, 90, '');
  btnShowCodes := TButton.Create(Self);
  btnShowCodes.Parent := grpHealth;
  btnShowCodes.Left := 20;
  btnShowCodes.Top := 118;
  btnShowCodes.Width := 112;
  btnShowCodes.OnClick := ShowCodesClicked;

  grpReadiness := TGroupBox.Create(Self);
  grpReadiness.Parent := sbContent;
  lblReadinessValue := AddLabel(grpReadiness, 20, 34, '', True);
  lblReadinessError := AddLabel(grpReadiness, 20, 64, '');
  btnShowReadiness := TButton.Create(Self);
  btnShowReadiness.Parent := grpReadiness;
  btnShowReadiness.Left := 20;
  btnShowReadiness.Top := 100;
  btnShowReadiness.Width := 128;
  btnShowReadiness.OnClick := ShowReadinessClicked;

  grpLive := TGroupBox.Create(Self);
  grpLive.Parent := sbContent;
  TopPos := 34;
  AddLabel(grpLive, 20, TopPos, 'Coolant');
  lblCoolantValue := AddLabel(grpLive, 144, TopPos, '', True);
  AddLabel(grpLive, 320, TopPos, 'Battery');
  lblBatteryValue := AddLabel(grpLive, 444, TopPos, '', True);
  Inc(TopPos, 32);
  AddLabel(grpLive, 20, TopPos, 'RPM');
  lblRPMValue := AddLabel(grpLive, 144, TopPos, '', True);
  AddLabel(grpLive, 320, TopPos, 'Fuel level');
  lblFuelValue := AddLabel(grpLive, 444, TopPos, '', True);

  memAttention := TMemo.Create(Self);
  memAttention.Parent := sbContent;
  memAttention.ReadOnly := True;
  memAttention.ScrollBars := ssVertical;

  Translate;
  ContentResize(sbContent);
end;

procedure TfrmPageOverview.HeaderResize(Sender: TObject);
begin
  btnSaveReport.Left := pnlHeader.Width - btnSaveReport.Width - 16;
  btnScan.Left := btnSaveReport.Left - btnScan.Width - 8;
  lblSubtitle.Width := btnScan.Left - lblSubtitle.Left - 16;
end;

procedure TfrmPageOverview.ContentResize(Sender: TObject);
var
  Available: Integer;
  Half: Integer;
begin
  Available := sbContent.ClientWidth - 32;
  if Available < 640 then
    Available := 640;
  grpVehicle.SetBounds(16, 16, Available, 124);
  Half := (Available - 12) div 2;
  grpHealth.SetBounds(16, 152, Half, 160);
  grpReadiness.SetBounds(28 + Half, 152, Half, 160);
  grpLive.SetBounds(16, 324, Available, 116);
  memAttention.SetBounds(16, 452, Available, 148);
  sbContent.VertScrollBar.Range := memAttention.Top + memAttention.Height + 24;
end;

function TfrmPageOverview.PageKey: string;
begin
  Result := PAGE_OVERVIEW;
end;

procedure TfrmPageOverview.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  UpdateView;
end;

procedure TfrmPageOverview.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive then
  begin
    UpdateView;
    if Assigned(FHost) and FHost.Connected and not FScannedConnection then
      StartScan;
  end;
end;

procedure TfrmPageOverview.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
  begin
    FScannedConnection := False;
    FLastScan := Default(TOverviewScanResult);
    if Assigned(FHost) then
    begin
      FHost.SetBadge(PAGE_CODES, 0);
      FHost.SetBadge(PAGE_READINESS, 0);
    end;
    UpdateView;
  end
  else if FActive and not FScannedConnection then
    StartScan
  else
    UpdateView;
end;

procedure TfrmPageOverview.UnitsChanged;
begin
  UpdateView;
end;

procedure TfrmPageOverview.Translate;
begin
  lblTitle.Caption := Tr('personal.overview.title', 'Overview');
  lblSubtitle.Caption := Tr('personal.overview.subtitle',
    'Vehicle summary, codes, readiness and key live values.');
  btnScan.Caption := Tr('personal.overview.scan', 'Scan vehicle');
  btnSaveReport.Caption := Tr('personal.overview.saveReport', 'Save report');
  lblEmptyTitle.Caption := Tr('personal.overview.offlineTitle',
    'Connect to a vehicle');
  lblEmptyText.Caption := Tr('personal.overview.offlineText',
    'The overview is filled after a quick scan. Connect an adapter, or try the built-in demo car.');
  btnEmptyConnect.Caption := Tr('personal.overview.connect', 'Connect');
  btnDemo.Caption := Tr('personal.overview.demo', 'Try demo car');
  grpVehicle.Caption := Tr('personal.overview.vehicle', 'Vehicle summary');
  grpHealth.Caption := Tr('personal.overview.health', 'MIL and trouble codes');
  grpReadiness.Caption := Tr('personal.overview.readiness', 'Readiness');
  grpLive.Caption := Tr('personal.overview.live', 'Live values snapshot');
  memAttention.Hint := Tr('personal.overview.attentionHint',
    'Scan results and next steps');
  btnShowCodes.Caption := Tr('personal.overview.showCodes', 'Show codes');
  btnShowReadiness.Caption := Tr('personal.overview.showReadiness',
    'Show readiness');
  UpdateView;
end;

function TfrmPageOverview.TotalCodeCount: Integer;
begin
  Result := FLastScan.ConfirmedCount + FLastScan.PendingCount +
    FLastScan.PermanentCount;
end;

procedure TfrmPageOverview.UpdateView;
var
  Online: Boolean;
  Total: Integer;
begin
  Online := Assigned(FHost) and FHost.Connected;
  pnlEmpty.Visible := not Online;
  sbContent.Visible := Online;
  btnScan.Enabled := Online and not FScanning;
  btnSaveReport.Enabled := Online and not FScanning;
  if not Online then
    Exit;

  if FScanning then
    lblSubtitle.Caption := Tr('personal.overview.scanning', 'Scanning vehicle…')
  else if FLastScan.Timestamp > 0 then
    lblSubtitle.Caption := Format(Tr('personal.overview.scannedAt',
      'Scanned at %s'), [FormatDateTime('hh:nn', FLastScan.Timestamp)]);

  lblVehicleValue.Caption := DashIfEmpty(FLastScan.VehicleLine);
  lblVINValue.Caption := Tr('personal.overview.vin', 'VIN') + ': ' +
    DashIfEmpty(FLastScan.VIN);
  if FLastScan.ProtocolName <> '' then
    lblDetailValue.Caption := DashIfEmpty(FLastScan.VehicleDetail) +
      '  ·  ' + FLastScan.ProtocolName
  else
    lblDetailValue.Caption := DashIfEmpty(FLastScan.VehicleDetail);

  if FLastScan.MILKnown then
  begin
    if FLastScan.MILOn then
      lblMILValue.Caption := Tr('personal.overview.milOn', 'On')
    else
      lblMILValue.Caption := Tr('personal.overview.milOff', 'Off');
    lblMILCountValue.Caption := IntToStr(FLastScan.MILDTCCount);
  end
  else
  begin
    lblMILValue.Caption := Tr('personal.common.unknown', 'Unknown');
    lblMILCountValue.Caption := '—';
  end;
  lblCodesValue.Caption := Format('%d stored, %d pending, %d permanent',
    [FLastScan.ConfirmedCount, FLastScan.PendingCount,
    FLastScan.PermanentCount]);

  lblReadinessValue.Caption := Format('%d complete, %d incomplete',
    [FLastScan.ReadinessComplete, FLastScan.ReadinessIncomplete]);
  lblReadinessError.Caption := FLastScan.ReadinessError;
  lblCoolantValue.Caption := DashIfEmpty(FLastScan.CoolantText);
  lblBatteryValue.Caption := DashIfEmpty(FLastScan.BatteryText);
  lblRPMValue.Caption := DashIfEmpty(FLastScan.RPMText);
  lblFuelValue.Caption := DashIfEmpty(FLastScan.FuelLevelText);

  Total := TotalCodeCount;
  btnShowCodes.Enabled := Total > 0;
  btnShowReadiness.Enabled := FLastScan.ReadinessSupported > 0;
  memAttention.Lines.BeginUpdate;
  try
    memAttention.Lines.Clear;
    if FLastScan.Timestamp = 0 then
      memAttention.Lines.Add(Tr('personal.overview.scanHint',
        'Press Scan vehicle to read the current state.'))
    else
    begin
      if FLastScan.MILKnown and FLastScan.MILOn then
        memAttention.Lines.Add(Tr('personal.overview.attentionMil',
          'The check engine light is on. Read the trouble codes first.'));
      if Total > 0 then
        memAttention.Lines.Add(Format('%d code(s): open Trouble codes for details.',
          [Total]));
      if FLastScan.ReadinessIncomplete > 0 then
        memAttention.Lines.Add(Format('%d readiness monitor(s) incomplete.',
          [FLastScan.ReadinessIncomplete]));
      if FLastScan.ErrorText <> '' then
        memAttention.Lines.Add(FLastScan.ErrorText);
      if memAttention.Lines.Count = 0 then
        memAttention.Lines.Add(Tr('personal.overview.noAttention',
          'No immediate attention items found in the quick scan.'));
    end;
  finally
    memAttention.Lines.EndUpdate;
  end;
end;

procedure TfrmPageOverview.StartScan;
var
  SessionObj: TPersonalSession;
  Scan: TOverviewScanResult;
begin
  if (FHost = nil) or not FHost.Connected or FScanning then
    Exit;
  SessionObj := FHost.Session;
  Scan := Default(TOverviewScanResult);
  FScanning := True;
  FHost.SetProgress(Tr('personal.overview.scanProgress', 'Scanning vehicle'), 5);
  UpdateView;
  SessionObj.RunJob(
    procedure
    var
      Confirmed: TArray<TOBDDtcEntry>;
      Pending: TArray<TOBDDtcEntry>;
      Permanent: TArray<TOBDDtcEntry>;
      Status: TOBDMonitorStatus;
      Readiness: TArray<TOBDMonitorReadiness>;
      ErrorText: string;
      VINInfo: TOBDVINInfo;
    begin
      Scan.Timestamp := Now;
      Scan.ProtocolName := SessionObj.ProtocolName;
      Scan.VIN := SessionObj.VehicleVIN;
      try
        if Scan.VIN <> '' then
        begin
          VINInfo := TOBDVINDecoder.Decode(Scan.VIN);
          ApplyVINInfo(VINInfo, Scan);
        end
        else
        begin
          VINInfo := SessionObj.VIN.ReadAndDecode;
          ApplyVINInfo(VINInfo, Scan);
        end;
      except
        on E: Exception do
          AppendLine(Scan.ErrorText, 'VIN decode: ' + E.Message);
      end;

      try
        Confirmed := SessionObj.DTCs.ReadConfirmed;
      except
        on E: Exception do
          AppendLine(Scan.ErrorText, 'Stored codes: ' + E.Message);
      end;
      try
        Pending := SessionObj.DTCs.ReadPending;
      except
        on E: Exception do
          AppendLine(Scan.ErrorText, 'Pending codes: ' + E.Message);
      end;
      try
        Permanent := SessionObj.DTCs.ReadPermanent;
      except
        on E: Exception do
          AppendLine(Scan.ErrorText, 'Permanent codes: ' + E.Message);
      end;
      Scan.ConfirmedCount := Length(Confirmed);
      Scan.PendingCount := Length(Pending);
      Scan.PermanentCount := Length(Permanent);

      ErrorText := '';
      try
        if SessionObj.DriveCycle.ReadMonitorStatus($01, Status, ErrorText) then
        begin
          Scan.MILKnown := True;
          Scan.MILOn := Status.MILOn;
          Scan.MILDTCCount := Status.DTCCount;
          CountReadiness(Status.Monitors, Scan.ReadinessSupported,
            Scan.ReadinessComplete, Scan.ReadinessIncomplete);
        end
        else
        begin
          Scan.ReadinessError := ErrorText;
          Readiness := SessionObj.DriveCycle.ReadReadiness;
          CountReadiness(Readiness, Scan.ReadinessSupported,
            Scan.ReadinessComplete, Scan.ReadinessIncomplete);
        end;
      except
        on E: Exception do
        begin
          Scan.ReadinessError := E.Message;
          AppendLine(Scan.ErrorText, 'Readiness: ' + E.Message);
        end;
      end;

      Scan.CoolantText := TryReadPID(SessionObj.LiveData, $05);
      Scan.BatteryText := TryReadPID(SessionObj.LiveData, $42);
      if (Scan.BatteryText = '—') and (SessionObj.BatteryVolts > 0) then
        Scan.BatteryText := FormatFloat('0.0', SessionObj.BatteryVolts) + ' V';
      Scan.RPMText := TryReadPID(SessionObj.LiveData, $0C);
      Scan.FuelLevelText := TryReadPID(SessionObj.LiveData, $2F);
    end,
    procedure(const AError: string)
    var
      Count: Integer;
    begin
      FScanning := False;
      if Assigned(FHost) then
        FHost.SetProgress('', -1);
      if AError <> '' then
      begin
        if Assigned(FHost) then
          FHost.Notify(tkDanger, Tr('personal.overview.scanFailed',
            'Scan failed'), AError);
      end
      else
      begin
        FLastScan := Scan;
        FScannedConnection := True;
        Count := TotalCodeCount;
        if Assigned(FHost) then
        begin
          FHost.SetCodeSummary(Count, FLastScan.MILOn);
          FHost.SetBadge(PAGE_CODES, Count);
          FHost.SetBadge(PAGE_READINESS, FLastScan.ReadinessIncomplete);
        end;
      end;
      UpdateView;
    end);
end;

procedure TfrmPageOverview.ScanClicked(Sender: TObject);
begin
  FScannedConnection := False;
  StartScan;
end;

procedure TfrmPageOverview.SaveReportClicked(Sender: TObject);
var
  SessionObj: TPersonalSession;
  FileName: string;
  ReportText: string;
begin
  if (FHost = nil) or not FHost.Connected then
    Exit;
  SessionObj := FHost.Session;
  FileName := TPath.Combine(FHost.Settings.DataSubFolder('Reports'),
    'vehicle-health-' + FormatDateTime('yyyymmdd-hhnnss', Now) + '.txt');
  FHost.SetProgress(Tr('personal.overview.reportProgress', 'Saving report'), 20);
  SessionObj.RunJob(
    procedure
    var
      Report: TOBDVehicleHealthReport;
    begin
      SessionObj.VehicleHealth.LivePIDs := TBytes.Create($05, $42, $0C, $2F);
      Report := SessionObj.VehicleHealth.Snapshot;
      ReportText := VehicleHealthReportToText(Report);
      TFile.WriteAllText(FileName, ReportText, TEncoding.UTF8);
    end,
    procedure(const AError: string)
    begin
      if Assigned(FHost) then
        FHost.SetProgress('', -1);
      if AError = '' then
      begin
        if Assigned(FHost) then
        begin
          FHost.Log('Saved vehicle health report: ' + FileName);
          FHost.Notify(tkSuccess, Tr('personal.overview.reportSaved',
            'Report saved'), FileName);
        end;
      end
      else if Assigned(FHost) then
        FHost.Notify(tkDanger, Tr('personal.overview.reportFailed',
          'Report not saved'), AError);
    end);
end;

procedure TfrmPageOverview.EmptyConnectClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_ADAPTER);
end;

procedure TfrmPageOverview.DemoClicked(Sender: TObject);
begin
  if FHost = nil then
    Exit;
  FHost.Settings.ConnectionKind := pckDemo;
  FHost.SettingsChanged;
  FHost.Session.Connect(FHost.Settings);
end;

procedure TfrmPageOverview.ShowCodesClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_CODES);
end;

procedure TfrmPageOverview.ShowReadinessClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_READINESS);
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageOverview.Commands: TPersonalCommands;
begin
  Result := [pcScanVehicle, pcSaveVehicleReport];
end;

function TfrmPageOverview.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  case ACommand of
    pcScanVehicle:
      Result := btnScan.Enabled;
    pcSaveVehicleReport:
      Result := btnSaveReport.Enabled;
  else
    Result := False;
  end;
end;

function TfrmPageOverview.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPageOverview.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcScanVehicle:
      ScanClicked(Self);
    pcSaveVehicleReport:
      SaveReportClicked(Self);
  end;
end;

end.
