//------------------------------------------------------------------------------
//  untPageInfo
//
//  Vehicle information page for OBD-Studio Personal. It reads Service 09 / Mode
//  09 data, decodes the VIN locally and lets the user copy or save the result.
//------------------------------------------------------------------------------

unit untPageInfo;

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
  ERD.Service.VehicleInfo,
  untPersonalPage;

type
  TfrmPageInfo = class(TFrame, IPersonalPage)
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FReading: Boolean;
    FReportValid: Boolean;
    FReport: TOBDVehicleInfoReport;
    FLastText: string;
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblSubtitle: TLabel;
    btnRead: TButton;
    btnCopy: TButton;
    btnSave: TButton;
    pnlOffline: TPanel;
    lblOfflineTitle: TLabel;
    lblOfflineText: TLabel;
    btnConnect: TButton;
    pnlContent: TPanel;
    grpVIN: TGroupBox;
    grpECUs: TGroupBox;
    grpDetails: TGroupBox;
    memVIN: TMemo;
    lstECUs: TListBox;
    memDetails: TMemo;
    procedure BuildUI;
    procedure HeaderResize(Sender: TObject);
    procedure ContentResize(Sender: TObject);
    procedure ReadClicked(Sender: TObject);
    procedure CopyClicked(Sender: TObject);
    procedure SaveClicked(Sender: TObject);
    procedure ConnectClicked(Sender: TObject);
    procedure ReadInfo;
    procedure UpdateView;
  public
    constructor Create(AOwner: TComponent); override;
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
  end;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Vcl.Clipbrd,
  ERD.UI.Toast,
  ERD.Service.VINDecoder,
  ERD.Service.VINDecoder.Types,
  untSession;

function DashIfBlank(const AText: string): string;
begin
  if Trim(AText) = '' then
    Result := '—'
  else
    Result := AText;
end;

function VehicleInfoSourceText(ASource: TOBDVehicleInfoSource): string;
begin
  case ASource of
    visOBD:
      Result := 'Service 09';
    visUDS:
      Result := 'J1979-2 / UDS';
  else
    Result := 'Automatic';
  end;
end;

function EcuLabel(const AEcu: string; AEcuId: Cardinal): string;
begin
  if AEcu <> '' then
    Result := AEcu
  else if AEcuId <> 0 then
    Result := IntToHex(AEcuId, 3)
  else
    Result := 'ECU';
end;

procedure AddUniqueLine(ALines: TStrings; const ALine: string);
begin
  if (ALine <> '') and (ALines.IndexOf(ALine) < 0) then
    ALines.Add(ALine);
end;

function PlantText(const AInfo: TOBDVINInfo): string;
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    if AInfo.Plant.Name <> '' then
      Parts.Add(AInfo.Plant.Name);
    if AInfo.Plant.City <> '' then
      Parts.Add(AInfo.Plant.City);
    if AInfo.Plant.Country <> '' then
      Parts.Add(AInfo.Plant.Country);
    Result := StringReplace(Trim(Parts.Text), sLineBreak, '  ·  ',
      [rfReplaceAll]);
  finally
    Parts.Free;
  end;
end;

procedure FillVINLines(ALines: TStrings; const AVIN: string);
var
  Info: TOBDVINInfo;
begin
  ALines.Clear;
  if AVIN = '' then
  begin
    ALines.Add('VIN not reported by the vehicle.');
    Exit;
  end;
  Info := TOBDVINDecoder.Decode(AVIN);
  ALines.Add('VIN: ' + AVIN);
  ALines.Add('WMI: ' + DashIfBlank(Info.WMI));
  ALines.Add('Manufacturer: ' + DashIfBlank(Info.Manufacturer.Name));
  if Info.ModelYear > 0 then
    ALines.Add('Model year: ' + IntToStr(Info.ModelYear))
  else
    ALines.Add('Model year: —');
  ALines.Add('Plant: ' + DashIfBlank(PlantText(Info)));
  ALines.Add('Country: ' + DashIfBlank(Info.Country.Name));
  if Info.CheckDigitValid then
    ALines.Add('Check digit: valid')
  else
    ALines.Add('Check digit: not confirmed');
  if Info.Features.BodyStyle <> '' then
    ALines.Add('Body: ' + Info.Features.BodyStyle);
  if Info.Features.EngineDisplacement <> '' then
    ALines.Add('Engine: ' + Info.Features.EngineDisplacement);
end;

procedure AddReportEcus(const AReport: TOBDVehicleInfoReport; ALines: TStrings);
var
  I: Integer;
begin
  ALines.BeginUpdate;
  try
    ALines.Clear;
    for I := 0 to High(AReport.VINs) do
      AddUniqueLine(ALines, EcuLabel(AReport.VINs[I].Ecu,
        AReport.VINs[I].EcuId));
    for I := 0 to High(AReport.EcuNames) do
      AddUniqueLine(ALines, EcuLabel(AReport.EcuNames[I].Ecu,
        AReport.EcuNames[I].EcuId) + '  ' +
        DashIfBlank(AReport.EcuNames[I].Acronym + ' ' +
        AReport.EcuNames[I].Name));
    for I := 0 to High(AReport.Calibrations) do
      AddUniqueLine(ALines, EcuLabel(AReport.Calibrations[I].Ecu,
        AReport.Calibrations[I].EcuId));
    for I := 0 to High(AReport.SerialNumbers) do
      AddUniqueLine(ALines, EcuLabel(AReport.SerialNumbers[I].Ecu,
        AReport.SerialNumbers[I].EcuId));
    for I := 0 to High(AReport.TypeApprovals) do
      AddUniqueLine(ALines, EcuLabel(AReport.TypeApprovals[I].Ecu,
        AReport.TypeApprovals[I].EcuId));
    if ALines.Count = 0 then
      ALines.Add('No ECU reported Mode 09 data.');
  finally
    ALines.EndUpdate;
  end;
end;

procedure FillMode09Lines(ALines: TStrings; const AReport: TOBDVehicleInfoReport);
var
  I: Integer;
  Line: string;
begin
  ALines.Clear;
  ALines.Add('Source: ' + VehicleInfoSourceText(AReport.Source));
  ALines.Add('VIN: ' + DashIfBlank(AReport.VIN));
  ALines.Add('');
  ALines.Add('Calibration IDs and CVNs');
  if Length(AReport.Calibrations) = 0 then
    ALines.Add('  —')
  else
    for I := 0 to High(AReport.Calibrations) do
    begin
      Line := '  ' + EcuLabel(AReport.Calibrations[I].Ecu,
        AReport.Calibrations[I].EcuId) + ': ' +
        DashIfBlank(AReport.Calibrations[I].CalibrationId);
      if AReport.Calibrations[I].HasCVN then
        Line := Line + '  CVN ' + AReport.Calibrations[I].CVN
      else
        Line := Line + '  CVN —';
      ALines.Add(Line);
    end;
  ALines.Add('');
  ALines.Add('ECU names');
  if Length(AReport.EcuNames) = 0 then
    ALines.Add('  —')
  else
    for I := 0 to High(AReport.EcuNames) do
      ALines.Add('  ' + EcuLabel(AReport.EcuNames[I].Ecu,
        AReport.EcuNames[I].EcuId) + ': ' +
        DashIfBlank(AReport.EcuNames[I].Acronym + ' ' +
        AReport.EcuNames[I].Name));
  ALines.Add('');
  ALines.Add('Engine serial numbers');
  if Length(AReport.SerialNumbers) = 0 then
    ALines.Add('  —')
  else
    for I := 0 to High(AReport.SerialNumbers) do
      ALines.Add('  ' + EcuLabel(AReport.SerialNumbers[I].Ecu,
        AReport.SerialNumbers[I].EcuId) + ': ' +
        DashIfBlank(AReport.SerialNumbers[I].Text));
  ALines.Add('');
  ALines.Add('Type approval numbers');
  if Length(AReport.TypeApprovals) = 0 then
    ALines.Add('  —')
  else
    for I := 0 to High(AReport.TypeApprovals) do
      ALines.Add('  ' + EcuLabel(AReport.TypeApprovals[I].Ecu,
        AReport.TypeApprovals[I].EcuId) + ': ' +
        DashIfBlank(AReport.TypeApprovals[I].Text));
end;

function VehicleInfoReportText(const AReport: TOBDVehicleInfoReport): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('OBD-Studio Personal vehicle information');
    Lines.Add('Created: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Lines.Add('');
    FillVINLines(Lines, AReport.VIN);
    Lines.Add('');
    FillMode09Lines(Lines, AReport);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ TfrmPageInfo }

constructor TfrmPageInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

procedure TfrmPageInfo.BuildUI;
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

  btnSave := TButton.Create(Self);
  btnSave.Parent := pnlHeader;
  btnSave.Top := 22;
  btnSave.Width := 112;
  btnSave.OnClick := SaveClicked;

  btnCopy := TButton.Create(Self);
  btnCopy.Parent := pnlHeader;
  btnCopy.Top := 22;
  btnCopy.Width := 104;
  btnCopy.OnClick := CopyClicked;

  btnRead := TButton.Create(Self);
  btnRead.Parent := pnlHeader;
  btnRead.Top := 22;
  btnRead.Width := 104;
  btnRead.Default := True;
  btnRead.OnClick := ReadClicked;

  pnlOffline := TPanel.Create(Self);
  pnlOffline.Parent := Self;
  pnlOffline.Align := alClient;
  pnlOffline.BevelOuter := bvNone;
  pnlOffline.ParentBackground := False;

  lblOfflineTitle := TLabel.Create(Self);
  lblOfflineTitle.Parent := pnlOffline;
  lblOfflineTitle.Left := 40;
  lblOfflineTitle.Top := 72;
  lblOfflineTitle.Font.Size := 14;
  lblOfflineTitle.Font.Style := [fsBold];

  lblOfflineText := TLabel.Create(Self);
  lblOfflineText.Parent := pnlOffline;
  lblOfflineText.Left := 40;
  lblOfflineText.Top := 108;
  lblOfflineText.Width := 560;
  lblOfflineText.WordWrap := True;

  btnConnect := TButton.Create(Self);
  btnConnect.Parent := pnlOffline;
  btnConnect.Left := 40;
  btnConnect.Top := 168;
  btnConnect.Width := 120;
  btnConnect.OnClick := ConnectClicked;

  pnlContent := TPanel.Create(Self);
  pnlContent.Parent := Self;
  pnlContent.Align := alClient;
  pnlContent.BevelOuter := bvNone;
  pnlContent.ParentBackground := False;
  pnlContent.OnResize := ContentResize;

  grpVIN := TGroupBox.Create(Self);
  grpVIN.Parent := pnlContent;
  grpVIN.Padding.SetBounds(8, 18, 8, 8);
  memVIN := TMemo.Create(Self);
  memVIN.Parent := grpVIN;
  memVIN.Align := alClient;
  memVIN.ReadOnly := True;
  memVIN.ScrollBars := ssVertical;

  grpECUs := TGroupBox.Create(Self);
  grpECUs.Parent := pnlContent;
  grpECUs.Padding.SetBounds(8, 18, 8, 8);
  lstECUs := TListBox.Create(Self);
  lstECUs.Parent := grpECUs;
  lstECUs.Align := alClient;

  grpDetails := TGroupBox.Create(Self);
  grpDetails.Parent := pnlContent;
  grpDetails.Padding.SetBounds(8, 18, 8, 8);
  memDetails := TMemo.Create(Self);
  memDetails.Parent := grpDetails;
  memDetails.Align := alClient;
  memDetails.ReadOnly := True;
  memDetails.ScrollBars := ssVertical;

  Translate;
  ContentResize(pnlContent);
end;

procedure TfrmPageInfo.HeaderResize(Sender: TObject);
begin
  btnSave.Left := pnlHeader.Width - btnSave.Width - 16;
  btnCopy.Left := btnSave.Left - btnCopy.Width - 8;
  btnRead.Left := btnCopy.Left - btnRead.Width - 8;
  lblSubtitle.Width := btnRead.Left - lblSubtitle.Left - 16;
end;

procedure TfrmPageInfo.ContentResize(Sender: TObject);
var
  Available: Integer;
  LeftWidth: Integer;
begin
  Available := pnlContent.ClientWidth - 48;
  if Available < 640 then
    Available := 640;
  LeftWidth := Available div 2;
  grpECUs.SetBounds(16, 16, LeftWidth, 220);
  grpDetails.SetBounds(16, 248, LeftWidth, pnlContent.ClientHeight - 264);
  grpVIN.SetBounds(32 + LeftWidth, 16, Available - LeftWidth,
    pnlContent.ClientHeight - 32);
end;

function TfrmPageInfo.PageKey: string;
begin
  Result := PAGE_INFO;
end;

procedure TfrmPageInfo.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  UpdateView;
end;

procedure TfrmPageInfo.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive then
  begin
    UpdateView;
    if Assigned(FHost) and FHost.Connected and not FReportValid then
      ReadInfo;
  end;
end;

procedure TfrmPageInfo.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
  begin
    FReportValid := False;
    FReport := Default(TOBDVehicleInfoReport);
    FLastText := '';
    UpdateView;
  end
  else if FActive then
    ReadInfo
  else
    UpdateView;
end;

procedure TfrmPageInfo.UnitsChanged;
begin
  UpdateView;
end;

procedure TfrmPageInfo.Translate;
begin
  lblTitle.Caption := Tr('personal.info.title', 'Vehicle information');
  lblSubtitle.Caption := Tr('personal.info.subtitle',
    'Mode 09 data, control units, calibration IDs, CVNs and local VIN decode.');
  btnRead.Caption := Tr('personal.info.read', 'Read again');
  btnCopy.Caption := Tr('personal.info.copy', 'Copy all');
  btnSave.Caption := Tr('personal.info.save', 'Save text');
  lblOfflineTitle.Caption := Tr('personal.info.offlineTitle',
    'Connect to read vehicle information');
  lblOfflineText.Caption := Tr('personal.info.offlineText',
    'Vehicle information is reported by emissions control units over Mode 09.');
  btnConnect.Caption := Tr('personal.info.connect', 'Connect');
  grpVIN.Caption := Tr('personal.info.vinCard', 'VIN decoder');
  grpECUs.Caption := Tr('personal.info.ecuCard', 'ECUs responding');
  grpDetails.Caption := Tr('personal.info.detailsCard',
    'Calibration IDs, CVNs and ECU names');
  UpdateView;
end;

procedure TfrmPageInfo.UpdateView;
var
  Online: Boolean;
begin
  Online := Assigned(FHost) and FHost.Connected;
  pnlOffline.Visible := not Online;
  pnlContent.Visible := Online;
  btnRead.Enabled := Online and not FReading;
  btnCopy.Enabled := Online and FReportValid and not FReading;
  btnSave.Enabled := Online and FReportValid and not FReading;
  if not Online then
    Exit;

  if FReading then
  begin
    memVIN.Lines.Text := 'Reading Mode 09 vehicle information…';
    lstECUs.Items.Text := '';
    memDetails.Lines.Text := '';
    Exit;
  end;

  if not FReportValid then
  begin
    memVIN.Lines.Text := 'Press Read again to ask the vehicle for Mode 09 data.';
    lstECUs.Items.Text := '';
    memDetails.Lines.Text := '';
    Exit;
  end;

  FillVINLines(memVIN.Lines, FReport.VIN);
  AddReportEcus(FReport, lstECUs.Items);
  FillMode09Lines(memDetails.Lines, FReport);
  FLastText := VehicleInfoReportText(FReport);
end;

procedure TfrmPageInfo.ReadInfo;
var
  SessionObj: TPersonalSession;
  Report: TOBDVehicleInfoReport;
begin
  if (FHost = nil) or not FHost.Connected or FReading then
    Exit;
  SessionObj := FHost.Session;
  Report := Default(TOBDVehicleInfoReport);
  FReading := True;
  FHost.SetProgress(Tr('personal.info.readProgress',
    'Reading vehicle information'), 20);
  UpdateView;
  SessionObj.RunJob(
    procedure
    begin
      Report := SessionObj.VehicleInfo.ReadAll;
    end,
    procedure(const AError: string)
    begin
      FReading := False;
      if Assigned(FHost) then
        FHost.SetProgress('', -1);
      if AError = '' then
      begin
        FReport := Report;
        FReportValid := True;
        if Assigned(FHost) then
          FHost.Notify(tkSuccess, Tr('personal.info.readDone',
            'Vehicle information read'), FReport.VIN);
      end
      else if Assigned(FHost) then
        FHost.Notify(tkDanger, Tr('personal.info.readFailed',
          'Vehicle information failed'), AError);
      UpdateView;
    end);
end;

procedure TfrmPageInfo.ReadClicked(Sender: TObject);
begin
  FReportValid := False;
  ReadInfo;
end;

procedure TfrmPageInfo.CopyClicked(Sender: TObject);
begin
  if FLastText = '' then
    FLastText := VehicleInfoReportText(FReport);
  Clipboard.AsText := FLastText;
  if FHost <> nil then
    FHost.Notify(tkInfo, Tr('personal.info.copied', 'Copied'),
      Tr('personal.info.copiedText', 'Vehicle information copied to the clipboard.'));
end;

procedure TfrmPageInfo.SaveClicked(Sender: TObject);
var
  FileName: string;
begin
  if (FHost = nil) or not FReportValid then
    Exit;
  if FLastText = '' then
    FLastText := VehicleInfoReportText(FReport);
  FileName := TPath.Combine(FHost.Settings.DataSubFolder('Reports'),
    'vehicle-info-' + FormatDateTime('yyyymmdd-hhnnss', Now) + '.txt');
  TFile.WriteAllText(FileName, FLastText, TEncoding.UTF8);
  FHost.Log('Saved vehicle information: ' + FileName);
  FHost.Notify(tkSuccess, Tr('personal.info.saved', 'Vehicle information saved'),
    FileName);
end;

procedure TfrmPageInfo.ConnectClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_ADAPTER);
end;

end.
