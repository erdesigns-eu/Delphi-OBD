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
  Vcl.Controls,
  Vcl.Forms,
  ERD.I18n,
  ERD.Service.VehicleInfo,
  ERD.UI.Buttons,
  ERD.UI.Card,
  ERD.UI.DataGrid,
  ERD.UI.Dialogs,
  ERD.UI.KeyValueList,
  ERD.UI.ScrollBox,
  untPersonalPage;

type
  TfrmPageInfo = class(TFrame, IPersonalPage, IPersonalCommands)
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FReading: Boolean;
    FReportValid: Boolean;
    FReport: TOBDVehicleInfoReport;
    FLastText: string;
    FTitle: TOBDDialogText;
    FSubtitle: TOBDDialogText;
    FButtonRead: TOBDButton;
    FButtonCopy: TOBDButton;
    FButtonSave: TOBDButton;
    FOfflineCard: TOBDCard;
    FOfflineTitle: TOBDDialogText;
    FOfflineText: TOBDDialogText;
    FButtonConnect: TOBDButton;
    FScroll: TOBDScrollBox;
    FECUCard: TOBDCard;
    FECUGrid: TOBDDataGrid;
    FVINCard: TOBDCard;
    FVINList: TOBDKeyValueList;
    FModeCard: TOBDCard;
    FModeList: TOBDKeyValueList;
    procedure BuildUI;
    procedure LayoutControls;
    procedure ReadClicked(Sender: TObject);
    procedure CopyClicked(Sender: TObject);
    procedure SaveClicked(Sender: TObject);
    procedure ConnectClicked(Sender: TObject);
    procedure ReadInfo;
    procedure UpdateView;
  protected
    procedure Resize; override;
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
  Vcl.Clipbrd,
  ERD.UI.Paint,
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

procedure AddVINDecodeRows(AList: TOBDKeyValueList; const AVIN: string);
var
  Info: TOBDVINInfo;
  Item: TOBDKeyValueItem;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    if AVIN = '' then
    begin
      Item := AList.Add(Tr('personal.info.vinCategory', 'VIN'),
        Tr('personal.info.vin', 'VIN'),
        Tr('personal.info.vinMissing', 'Not reported'), kvkChip);
      Item.Tone := skWarning;
      Exit;
    end;

    Info := TOBDVINDecoder.Decode(AVIN);
    AList.Add(Tr('personal.info.vinCategory', 'VIN'), Tr('personal.info.vin',
      'VIN'), AVIN, kvkMono);
    AList.Add(Tr('personal.info.vinCategory', 'VIN'), Tr('personal.info.wmi',
      'WMI'), DashIfBlank(Info.WMI), kvkMono);
    AList.Add(Tr('personal.info.vinCategory', 'VIN'),
      Tr('personal.info.manufacturer', 'Manufacturer'),
      DashIfBlank(Info.Manufacturer.Name));
    if Info.ModelYear > 0 then
      AList.Add(Tr('personal.info.vehicleCategory', 'Vehicle'),
        Tr('personal.info.modelYear', 'Model year'), IntToStr(Info.ModelYear))
    else
      AList.Add(Tr('personal.info.vehicleCategory', 'Vehicle'),
        Tr('personal.info.modelYear', 'Model year'), '—');
    AList.Add(Tr('personal.info.vehicleCategory', 'Vehicle'),
      Tr('personal.info.body', 'Body'), DashIfBlank(Info.Features.BodyStyle));
    AList.Add(Tr('personal.info.vehicleCategory', 'Vehicle'),
      Tr('personal.info.engine', 'Engine'),
      DashIfBlank(Info.Features.EngineDisplacement));
    AList.Add(Tr('personal.info.originCategory', 'Origin'),
      Tr('personal.info.country', 'Country'), DashIfBlank(Info.Country.Name));
    AList.Add(Tr('personal.info.originCategory', 'Origin'),
      Tr('personal.info.plant', 'Plant'), DashIfBlank(PlantText(Info)));
    Item := AList.Add(Tr('personal.info.vinCategory', 'VIN'),
      Tr('personal.info.checkDigit', 'Check digit'), '', kvkChip);
    if Info.CheckDigitValid then
    begin
      Item.Value := Tr('personal.info.checkDigitValid', 'Valid');
      Item.Tone := skSuccess;
    end
    else
    begin
      Item.Value := Tr('personal.info.checkDigitUnknown', 'Not confirmed');
      Item.Tone := skWarning;
    end;
  finally
    AList.EndUpdate;
  end;
end;

function Mode09DetailText(const AText, ACVN: string; AHasCVN: Boolean): string;
begin
  Result := DashIfBlank(AText);
  if AHasCVN then
    Result := Result + '  ·  CVN ' + ACVN
  else
    Result := Result + '  ·  CVN —';
end;

procedure FillEcuGrid(AGrid: TOBDDataGrid; const AReport: TOBDVehicleInfoReport);
var
  I: Integer;
  Row: Integer;

  procedure AddRow(const AEcu, AKind, ADetail: string; ATone: TOBDStatusKind);
  begin
    Row := AGrid.AddRow([AEcu, AKind, ADetail]);
    AGrid.CellTone[1, Row] := ATone;
    AGrid.RowAccent[Row] := ATone in [skAccent, skSuccess];
  end;

begin
  AGrid.BeginUpdate;
  try
    AGrid.Clear;
    AGrid.EmptyTitle := Tr('personal.info.noEcusTitle', 'No ECU Mode 09 data');
    AGrid.EmptyHint := Tr('personal.info.noEcusHint',
      'Read vehicle information to list responding control units.');

    for I := 0 to High(AReport.VINs) do
      AddRow(EcuLabel(AReport.VINs[I].Ecu, AReport.VINs[I].EcuId),
        Tr('personal.info.kindVin', 'VIN'), DashIfBlank(AReport.VINs[I].VIN),
        skSuccess);
    for I := 0 to High(AReport.EcuNames) do
      AddRow(EcuLabel(AReport.EcuNames[I].Ecu, AReport.EcuNames[I].EcuId),
        Tr('personal.info.kindEcuName', 'ECU name'),
        DashIfBlank(Trim(AReport.EcuNames[I].Acronym + ' ' +
        AReport.EcuNames[I].Name)), skAccent);
    for I := 0 to High(AReport.Calibrations) do
      AddRow(EcuLabel(AReport.Calibrations[I].Ecu,
        AReport.Calibrations[I].EcuId),
        Tr('personal.info.kindCalibration', 'Calibration'),
        Mode09DetailText(AReport.Calibrations[I].CalibrationId,
        AReport.Calibrations[I].CVN, AReport.Calibrations[I].HasCVN), skAccent);
    for I := 0 to High(AReport.SerialNumbers) do
      AddRow(EcuLabel(AReport.SerialNumbers[I].Ecu,
        AReport.SerialNumbers[I].EcuId),
        Tr('personal.info.kindSerial', 'Serial'),
        DashIfBlank(AReport.SerialNumbers[I].Text), skNeutral);
    for I := 0 to High(AReport.TypeApprovals) do
      AddRow(EcuLabel(AReport.TypeApprovals[I].Ecu,
        AReport.TypeApprovals[I].EcuId),
        Tr('personal.info.kindTypeApproval', 'Type approval'),
        DashIfBlank(AReport.TypeApprovals[I].Text), skNeutral);
  finally
    AGrid.EndUpdate;
  end;
end;

procedure FillMode09List(AList: TOBDKeyValueList;
  const AReport: TOBDVehicleInfoReport);
var
  I: Integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    AList.Add(Tr('personal.info.summaryCategory', 'Summary'),
      Tr('personal.info.source', 'Source'), VehicleInfoSourceText(AReport.Source));
    AList.Add(Tr('personal.info.summaryCategory', 'Summary'),
      Tr('personal.info.vin', 'VIN'), DashIfBlank(AReport.VIN), kvkMono);
    AList.Add(Tr('personal.info.summaryCategory', 'Summary'),
      Tr('personal.info.vinRecords', 'VIN records'),
      IntToStr(Length(AReport.VINs)));
    AList.Add(Tr('personal.info.summaryCategory', 'Summary'),
      Tr('personal.info.ecuNames', 'ECU names'),
      IntToStr(Length(AReport.EcuNames)));
    AList.Add(Tr('personal.info.summaryCategory', 'Summary'),
      Tr('personal.info.calibrations', 'Calibrations'),
      IntToStr(Length(AReport.Calibrations)));
    AList.Add(Tr('personal.info.summaryCategory', 'Summary'),
      Tr('personal.info.serialNumbers', 'Serial numbers'),
      IntToStr(Length(AReport.SerialNumbers)));
    AList.Add(Tr('personal.info.summaryCategory', 'Summary'),
      Tr('personal.info.typeApprovals', 'Type approvals'),
      IntToStr(Length(AReport.TypeApprovals)));

    for I := 0 to High(AReport.Calibrations) do
      AList.Add(Tr('personal.info.calibrationCategory', 'Calibration IDs and CVNs'),
        EcuLabel(AReport.Calibrations[I].Ecu, AReport.Calibrations[I].EcuId),
        Mode09DetailText(AReport.Calibrations[I].CalibrationId,
        AReport.Calibrations[I].CVN, AReport.Calibrations[I].HasCVN), kvkMono);
    for I := 0 to High(AReport.SerialNumbers) do
      AList.Add(Tr('personal.info.serialCategory', 'Engine serial numbers'),
        EcuLabel(AReport.SerialNumbers[I].Ecu, AReport.SerialNumbers[I].EcuId),
        DashIfBlank(AReport.SerialNumbers[I].Text), kvkMono);
    for I := 0 to High(AReport.TypeApprovals) do
      AList.Add(Tr('personal.info.approvalCategory', 'Type approval numbers'),
        EcuLabel(AReport.TypeApprovals[I].Ecu, AReport.TypeApprovals[I].EcuId),
        DashIfBlank(AReport.TypeApprovals[I].Text), kvkMono);
  finally
    AList.EndUpdate;
  end;
end;

{ TfrmPageInfo }

constructor TfrmPageInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

procedure TfrmPageInfo.BuildUI;

  function NewText(AParent: TWinControl; AStyle: TOBDDialogTextStyle;
    const ACaption: string; AWordWrap: Boolean = False): TOBDDialogText;
  begin
    Result := TOBDDialogText.Create(Self);
    Result.Parent := AParent;
    Result.Style := AStyle;
    Result.Caption := ACaption;
    Result.WordWrap := AWordWrap;
  end;

  function NewButton(AParent: TWinControl; AKind: TOBDButtonKind;
    AGlyph: TOBDGlyph; const ACaption: string; AHandler: TNotifyEvent): TOBDButton;
  begin
    Result := TOBDButton.Create(Self);
    Result.Parent := AParent;
    Result.AutoSize := False;
    Result.Kind := AKind;
    Result.Glyph := AGlyph;
    Result.Caption := ACaption;
    Result.OnClick := AHandler;
  end;

  function NewCard(AParent: TWinControl; const ATitle: string;
    AEdge: TOBDCardEdge = ceNone): TOBDCard;
  begin
    Result := TOBDCard.Create(Self);
    Result.Parent := AParent;
    Result.Title := ATitle;
    Result.StatusEdge := AEdge;
    Result.Padding.SetBounds(16, 8, 16, 14);
  end;

begin
  Align := alClient;
  ParentBackground := False;

  FTitle := NewText(Self, dtsHeading, '');
  FSubtitle := NewText(Self, dtsSubtle, '', True);
  FButtonSave := NewButton(Self, bkSecondary, glSave, '', SaveClicked);
  FButtonCopy := NewButton(Self, bkSecondary, glCopy, '', CopyClicked);
  FButtonRead := NewButton(Self, bkPrimary, glRead, '', ReadClicked);
  FButtonRead.Default := True;

  FOfflineCard := NewCard(Self, '', ceAccent);
  FOfflineCard.ShowHeader := False;
  FOfflineCard.Padding.SetBounds(24, 24, 24, 24);
  FOfflineTitle := NewText(FOfflineCard, dtsHeading, '');
  FOfflineText := NewText(FOfflineCard, dtsBody, '', True);
  FButtonConnect := NewButton(FOfflineCard, bkPrimary, glConnect, '',
    ConnectClicked);

  FScroll := TOBDScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.LineStep := 72;

  FECUCard := NewCard(FScroll, '', ceAccent);
  FECUGrid := TOBDDataGrid.Create(Self);
  FECUGrid.Parent := FECUCard;
  FECUGrid.Align := alClient;
  FECUGrid.BorderVisible := False;
  FECUGrid.EmptyGlyph := glEcu;
  with FECUGrid.DataColumns.Add do
  begin
    Caption := 'ECU';
    Width := 92;
    Sortable := False;
  end;
  with FECUGrid.DataColumns.Add do
  begin
    Caption := 'Data';
    Width := 110;
    Kind := dckChip;
    Sortable := False;
  end;
  with FECUGrid.DataColumns.Add do
  begin
    Caption := 'Details';
    Width := 0;
    Sortable := False;
  end;

  FVINCard := NewCard(FScroll, '', ceSuccess);
  FVINList := TOBDKeyValueList.Create(Self);
  FVINList.Parent := FVINCard;
  FVINList.Align := alClient;
  FVINList.KeyWidth := 132;
  FVINList.CopyOnHover := True;
  FVINList.WordWrap := True;

  FModeCard := NewCard(FScroll, '', ceNeutral);
  FModeList := TOBDKeyValueList.Create(Self);
  FModeList.Parent := FModeCard;
  FModeList.Align := alClient;
  FModeList.KeyWidth := 150;
  FModeList.CopyOnHover := True;
  FModeList.WordWrap := True;

  Translate;
  LayoutControls;
end;

procedure TfrmPageInfo.Resize;
begin
  inherited;
  LayoutControls;
end;

procedure TfrmPageInfo.LayoutControls;
const
  Pad = 24;
  Gutter = 16;
  HeaderH = 96;
  ButtonH = 40;
var
  ContentW: Integer;
  LeftW: Integer;
  RightW: Integer;
  RightX: Integer;
  Y: Integer;
  ScrollY: Integer;
  Wide: Boolean;

  procedure Place(AControl: TControl; AX, AY, AW, AH: Integer);
  begin
    AControl.SetBounds(AX, AY - ScrollY, AW, AH);
  end;

begin
  if ClientWidth <= 0 then
    Exit;

  FTitle.SetBounds(Pad, 18, Max(160, ClientWidth - 2 * Pad - 360), 28);
  FSubtitle.SetBounds(Pad, 50, Max(160, ClientWidth - 2 * Pad - 360), 36);
  FButtonSave.SetBounds(ClientWidth - Pad - 118, 28, 118, ButtonH);
  FButtonCopy.SetBounds(FButtonSave.Left - Gutter - 112, 28, 112, ButtonH);
  FButtonRead.SetBounds(FButtonCopy.Left - Gutter - 126, 28, 126, ButtonH);

  FOfflineCard.SetBounds(Pad, HeaderH + Pad, Min(680, Max(320,
    ClientWidth - 2 * Pad)), 194);
  FOfflineTitle.SetBounds(24, 24, FOfflineCard.Width - 48, 30);
  FOfflineText.SetBounds(24, 66, FOfflineCard.Width - 48, 52);
  FButtonConnect.SetBounds(24, 132, 128, ButtonH);

  FScroll.SetBounds(0, HeaderH, ClientWidth, Max(0, ClientHeight - HeaderH));
  ScrollY := FScroll.ScrollPosition;
  ContentW := Max(320, FScroll.ClientWidth - 2 * Pad);
  Wide := ContentW >= 900;

  if Wide then
  begin
    LeftW := Min(430, (ContentW - Gutter) div 2);
    RightW := ContentW - Gutter - LeftW;
    RightX := Pad + LeftW + Gutter;
    Place(FECUCard, Pad, Pad, LeftW, 350);
    Place(FVINCard, RightX, Pad, RightW, 320);
    Place(FModeCard, RightX, Pad + 320 + Gutter, RightW, 390);
  end
  else
  begin
    Y := Pad;
    Place(FECUCard, Pad, Y, ContentW, 350);
    Inc(Y, 350 + Gutter);
    Place(FVINCard, Pad, Y, ContentW, 320);
    Inc(Y, 320 + Gutter);
    Place(FModeCard, Pad, Y, ContentW, 390);
  end;
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
  FTitle.Caption := Tr('personal.info.title', 'Vehicle information');
  FSubtitle.Caption := Tr('personal.info.subtitle',
    'Mode 09 data, control units, calibration IDs, CVNs and local VIN decode.');
  FButtonRead.Caption := Tr('personal.info.read', 'Read again');
  FButtonCopy.Caption := Tr('personal.info.copy', 'Copy all');
  FButtonSave.Caption := Tr('personal.info.save', 'Save text');
  FOfflineTitle.Caption := Tr('personal.info.offlineTitle',
    'Connect to read vehicle information');
  FOfflineText.Caption := Tr('personal.info.offlineText',
    'Vehicle information is reported by emissions control units over Mode 09.');
  FButtonConnect.Caption := Tr('personal.info.connect', 'Connect');
  FECUCard.Title := Tr('personal.info.ecuCard', 'Control units');
  FVINCard.Title := Tr('personal.info.vinCard', 'VIN decoder');
  FModeCard.Title := Tr('personal.info.detailsCard', 'Mode 09 report');
  FECUGrid.DataColumns[0].Caption := Tr('personal.info.ecuColumn', 'ECU');
  FECUGrid.DataColumns[1].Caption := Tr('personal.info.dataColumn', 'Data');
  FECUGrid.DataColumns[2].Caption := Tr('personal.info.detailsColumn', 'Details');
  UpdateView;
  LayoutControls;
end;

procedure TfrmPageInfo.UpdateView;
var
  Online: Boolean;
begin
  Online := Assigned(FHost) and FHost.Connected;
  FOfflineCard.Visible := not Online;
  FScroll.Visible := Online;
  FButtonRead.Enabled := Online and not FReading;
  FButtonCopy.Enabled := Online and FReportValid and not FReading;
  FButtonSave.Enabled := Online and FReportValid and not FReading;
  if FReading then
    FButtonRead.Caption := Tr('personal.info.readingButton', 'Reading…')
  else
    FButtonRead.Caption := Tr('personal.info.read', 'Read again');

  if not Online then
  begin
    FSubtitle.Caption := Tr('personal.info.subtitle',
      'Mode 09 data, control units, calibration IDs, CVNs and local VIN decode.');
    LayoutControls;
    Exit;
  end;

  if FReading then
  begin
    FSubtitle.Caption := Tr('personal.info.reading',
      'Reading Mode 09 vehicle information…');
    FVINList.Clear;
    FModeList.Clear;
    FECUGrid.BeginUpdate;
    try
      FECUGrid.Clear;
      FECUGrid.EmptyTitle := Tr('personal.info.readingTitle', 'Reading vehicle');
      FECUGrid.EmptyHint := Tr('personal.info.readingHint',
        'Waiting for control units to answer Mode 09.');
    finally
      FECUGrid.EndUpdate;
    end;
    LayoutControls;
    Exit;
  end;

  if not FReportValid then
  begin
    FSubtitle.Caption := Tr('personal.info.subtitle',
      'Mode 09 data, control units, calibration IDs, CVNs and local VIN decode.');
    FVINList.Clear;
    FModeList.Clear;
    FECUGrid.BeginUpdate;
    try
      FECUGrid.Clear;
      FECUGrid.EmptyTitle := Tr('personal.info.emptyTitle', 'Read vehicle information');
      FECUGrid.EmptyHint := Tr('personal.info.emptyHint',
        'Press Read again to ask the vehicle for VIN and calibration data.');
    finally
      FECUGrid.EndUpdate;
    end;
    LayoutControls;
    Exit;
  end;

  FSubtitle.Caption := Format(Tr('personal.info.readSummary',
    'Mode 09 report read from %d data group(s).'),
    [Length(FReport.VINs) + Length(FReport.EcuNames) +
    Length(FReport.Calibrations) + Length(FReport.SerialNumbers) +
    Length(FReport.TypeApprovals)]);
  AddVINDecodeRows(FVINList, FReport.VIN);
  FillEcuGrid(FECUGrid, FReport);
  FillMode09List(FModeList, FReport);
  FLastText := VehicleInfoReportText(FReport);
  LayoutControls;
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

{ Commands ------------------------------------------------------------------ }

function TfrmPageInfo.Commands: TPersonalCommands;
begin
  Result := [pcReadVehicleInfo, pcCopyVehicleInfo, pcSaveVehicleInfo];
end;

function TfrmPageInfo.CommandEnabled(ACommand: TPersonalCommand): Boolean;
var
  Online: Boolean;
begin
  Online := Assigned(FHost) and FHost.Connected;
  case ACommand of
    pcReadVehicleInfo:
      Result := Online and not FReading;
    pcCopyVehicleInfo:
      Result := Online and FReportValid and not FReading;
    pcSaveVehicleInfo:
      Result := Online and FReportValid and not FReading;
  else
    Result := False;
  end;
end;

function TfrmPageInfo.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPageInfo.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcReadVehicleInfo:
      ReadClicked(Self);
    pcCopyVehicleInfo:
      CopyClicked(Self);
    pcSaveVehicleInfo:
      SaveClicked(Self);
  end;
end;

end.
