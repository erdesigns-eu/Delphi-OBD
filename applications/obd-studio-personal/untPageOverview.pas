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
  Vcl.Controls,
  Vcl.Forms,
  ERD.I18n,
  ERD.UI.Buttons,
  ERD.UI.Card,
  ERD.UI.DataGrid,
  ERD.UI.Dialogs,
  ERD.UI.KeyValueList,
  ERD.UI.ScrollBox,
  ERD.UI.ValueTile,
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
    FTitle: TOBDDialogText;
    FSubtitle: TOBDDialogText;
    FButtonScan: TOBDButton;
    FButtonSaveReport: TOBDButton;
    FEmptyCard: TOBDCard;
    FEmptyTitle: TOBDDialogText;
    FEmptyText: TOBDDialogText;
    FButtonEmptyConnect: TOBDButton;
    FButtonDemo: TOBDButton;
    FScroll: TOBDScrollBox;
    FVehicleCard: TOBDCard;
    FVehicleCaption: TOBDDialogText;
    FVehicleTitle: TOBDDialogText;
    FVehicleDetail: TOBDDialogText;
    FVehicleList: TOBDKeyValueList;
    FButtonChangeVehicle: TOBDButton;
    FTileMIL: TOBDValueTile;
    FTileCodes: TOBDValueTile;
    FTileReadiness: TOBDValueTile;
    FTileBattery: TOBDValueTile;
    FAttentionCard: TOBDCard;
    FAttentionGrid: TOBDDataGrid;
    FQuickCard: TOBDCard;
    FButtonQuickCodes: TOBDButton;
    FButtonQuickLive: TOBDButton;
    FButtonQuickDashboard: TOBDButton;
    FButtonQuickReadiness: TOBDButton;
    FButtonQuickFuel: TOBDButton;
    FLiveCard: TOBDCard;
    FLiveList: TOBDKeyValueList;
    procedure BuildUI;
    procedure LayoutControls;
    procedure ScanClicked(Sender: TObject);
    procedure SaveReportClicked(Sender: TObject);
    procedure EmptyConnectClicked(Sender: TObject);
    procedure DemoClicked(Sender: TObject);
    procedure ShowCodesClicked(Sender: TObject);
    procedure ShowReadinessClicked(Sender: TObject);
    procedure ChangeVehicleClicked(Sender: TObject);
    procedure ShowLiveClicked(Sender: TObject);
    procedure ShowDashboardClicked(Sender: TObject);
    procedure ShowFuelClicked(Sender: TObject);
    procedure AttentionActivate(Sender: TObject);
    procedure StartScan;
    procedure UpdateView;
    function TotalCodeCount: Integer;
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
  ERD.UI.Paint,
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

function TryLeadingNumber(const AText: string; out AValue: Double): Boolean;
var
  I: Integer;
  Token: string;
  FS: TFormatSettings;
begin
  Token := '';
  for I := 1 to Length(Trim(AText)) do
  begin
    if CharInSet(Trim(AText)[I], ['0'..'9', '-', '+', '.', ',']) then
      Token := Token + Trim(AText)[I]
    else if Token <> '' then
      Break;
  end;
  FS := TFormatSettings.Create;
  Token := StringReplace(Token, ',', FS.DecimalSeparator, [rfReplaceAll]);
  Token := StringReplace(Token, '.', FS.DecimalSeparator, [rfReplaceAll]);
  Result := (Token <> '') and TryStrToFloat(Token, AValue, FS);
end;

procedure ClearTile(ATile: TOBDValueTile; const AUnit: string);
begin
  ATile.&Unit := AUnit;
  ATile.Channel.Clear;
  ATile.ClearHistory;
end;

procedure SetTileValue(ATile: TOBDValueTile; AValue: Double;
  const AUnit: string);
begin
  ATile.&Unit := AUnit;
  ATile.Value := AValue;
end;

{ TfrmPageOverview }

constructor TfrmPageOverview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

procedure TfrmPageOverview.BuildUI;

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

  procedure ConfigureTile(ATile: TOBDValueTile; const ACaption: string;
    AMin, AMax: Double; ADecimals: Byte);
  begin
    ATile.Caption := ACaption;
    ATile.Min := AMin;
    ATile.Max := AMax;
    ATile.Decimals := ADecimals;
    ATile.AnimateValueChanges := False;
    ATile.ShowMinMax := False;
    ATile.ShowTrend := False;
    ATile.ShowSparkline := False;
    ATile.Channel.StaleAfterMs := 0;
    ClearTile(ATile, '');
  end;

begin
  Align := alClient;
  ParentBackground := False;

  FTitle := NewText(Self, dtsHeading, '');
  FSubtitle := NewText(Self, dtsSubtle, '', True);
  FButtonSaveReport := NewButton(Self, bkSecondary, glSave, '', SaveReportClicked);
  FButtonScan := NewButton(Self, bkPrimary, glRead, '', ScanClicked);
  FButtonScan.Default := True;

  FEmptyCard := NewCard(Self, '', ceAccent);
  FEmptyCard.ShowHeader := False;
  FEmptyCard.Padding.SetBounds(24, 24, 24, 24);
  FEmptyTitle := NewText(FEmptyCard, dtsHeading, '');
  FEmptyText := NewText(FEmptyCard, dtsBody, '', True);
  FButtonEmptyConnect := NewButton(FEmptyCard, bkPrimary, glConnect, '',
    EmptyConnectClicked);
  FButtonDemo := NewButton(FEmptyCard, bkSecondary, glVehicle, '', DemoClicked);

  FScroll := TOBDScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.LineStep := 72;

  FVehicleCard := NewCard(FScroll, '', ceAccent);
  FVehicleCard.ShowHeader := False;
  FVehicleCard.Padding.SetBounds(20, 16, 20, 16);
  FVehicleCaption := NewText(FVehicleCard, dtsCaps, '');
  FVehicleTitle := NewText(FVehicleCard, dtsHeading, '');
  FVehicleDetail := NewText(FVehicleCard, dtsSubtle, '', True);
  FButtonChangeVehicle := NewButton(FVehicleCard, bkGhost, glVehicle, '',
    ChangeVehicleClicked);
  FVehicleList := TOBDKeyValueList.Create(Self);
  FVehicleList.Parent := FVehicleCard;
  FVehicleList.ShowCategories := False;
  FVehicleList.CopyOnHover := False;
  FVehicleList.ColumnCount := 2;
  FVehicleList.KeyWidth := 104;
  FVehicleList.WordWrap := True;

  FTileMIL := TOBDValueTile.Create(Self);
  FTileMIL.Parent := FScroll;
  ConfigureTile(FTileMIL, '', 0, 1, 0);
  FTileMIL.Alerts.SetHigh(0.5, 0.5);
  FTileCodes := TOBDValueTile.Create(Self);
  FTileCodes.Parent := FScroll;
  ConfigureTile(FTileCodes, '', 0, 20, 0);
  FTileCodes.Alerts.SetHigh(1, 1);
  FTileReadiness := TOBDValueTile.Create(Self);
  FTileReadiness.Parent := FScroll;
  ConfigureTile(FTileReadiness, '', 0, 12, 0);
  FTileReadiness.Alerts.SetHigh(1, 1);
  FTileBattery := TOBDValueTile.Create(Self);
  FTileBattery.Parent := FScroll;
  ConfigureTile(FTileBattery, '', 10, 16, 1);
  FTileBattery.Alerts.SetLow(12.0, 11.5);

  FAttentionCard := NewCard(FScroll, '', ceWarning);
  FAttentionGrid := TOBDDataGrid.Create(Self);
  FAttentionGrid.Parent := FAttentionCard;
  FAttentionGrid.Align := alClient;
  FAttentionGrid.BorderVisible := False;
  FAttentionGrid.OnItemActivate := AttentionActivate;
  FAttentionGrid.OnEmptyAction := ScanClicked;
  with FAttentionGrid.DataColumns.Add do
  begin
    Caption := 'Severity';
    Width := 96;
    Kind := dckChip;
    Sortable := False;
  end;
  with FAttentionGrid.DataColumns.Add do
  begin
    Caption := 'Item';
    Width := 0;
    Sortable := False;
  end;
  with FAttentionGrid.DataColumns.Add do
  begin
    Caption := 'Next step';
    Width := 132;
    Sortable := False;
  end;

  FQuickCard := NewCard(FScroll, '', ceAccent);
  FButtonQuickCodes := NewButton(FQuickCard, bkSecondary, glCodes, '',
    ShowCodesClicked);
  FButtonQuickLive := NewButton(FQuickCard, bkSecondary, glLive, '',
    ShowLiveClicked);
  FButtonQuickDashboard := NewButton(FQuickCard, bkSecondary, glDashboard, '',
    ShowDashboardClicked);
  FButtonQuickReadiness := NewButton(FQuickCard, bkSecondary, glReadiness, '',
    ShowReadinessClicked);
  FButtonQuickFuel := NewButton(FQuickCard, bkSecondary, glVehicle, '',
    ShowFuelClicked);

  FLiveCard := NewCard(FScroll, '', ceSuccess);
  FLiveList := TOBDKeyValueList.Create(Self);
  FLiveList.Parent := FLiveCard;
  FLiveList.Align := alClient;
  FLiveList.ShowCategories := False;
  FLiveList.CopyOnHover := True;
  FLiveList.ColumnCount := 2;
  FLiveList.KeyWidth := 112;

  Translate;
  LayoutControls;
end;

procedure TfrmPageOverview.Resize;
begin
  inherited;
  LayoutControls;
end;

procedure TfrmPageOverview.LayoutControls;
const
  Pad = 24;
  Gutter = 16;
  HeaderH = 96;
  ButtonH = 40;
var
  ContentW: Integer;
  LeftW: Integer;
  RightW: Integer;
  TileH: Integer;
  Y: Integer;
  RightX: Integer;
  ScrollY: Integer;
  Wide: Boolean;

  procedure Place(AControl: TControl; AX, AY, AW, AH: Integer);
  begin
    AControl.SetBounds(AX, AY - ScrollY, AW, AH);
  end;

  procedure PlaceStatusTiles(AX, AY, AW: Integer; AColumns: Integer;
    out ABottom: Integer);
  var
    W: Integer;
  begin
    if AColumns < 1 then
      AColumns := 1;
    W := (AW - (AColumns - 1) * Gutter) div AColumns;
    TileH := 104;
    Place(FTileMIL, AX, AY, W, TileH);
    if AColumns = 1 then
    begin
      Place(FTileCodes, AX, AY + TileH + Gutter, W, TileH);
      Place(FTileReadiness, AX, AY + 2 * (TileH + Gutter), W, TileH);
      Place(FTileBattery, AX, AY + 3 * (TileH + Gutter), W, TileH);
      ABottom := AY + 4 * TileH + 3 * Gutter;
    end
    else
    begin
      Place(FTileCodes, AX + W + Gutter, AY, W, TileH);
      Place(FTileReadiness, AX, AY + TileH + Gutter, W, TileH);
      Place(FTileBattery, AX + W + Gutter, AY + TileH + Gutter, W, TileH);
      ABottom := AY + 2 * TileH + Gutter;
    end;
  end;

var
  StatusBottom: Integer;
  BottomLeft: Integer;
  BottomRight: Integer;
  QuickTop: Integer;
  QuickButtonW: Integer;
begin
  if ClientWidth <= 0 then
    Exit;

  FTitle.SetBounds(Pad, 18, Max(160, ClientWidth - 2 * Pad - 300), 28);
  FSubtitle.SetBounds(Pad, 50, Max(160, ClientWidth - 2 * Pad - 300), 36);
  FButtonScan.SetBounds(ClientWidth - Pad - 136, 28, 136, ButtonH);
  FButtonSaveReport.SetBounds(FButtonScan.Left - Gutter - 124, 28, 124, ButtonH);

  FEmptyCard.SetBounds(Pad, HeaderH + Pad, Min(680, Max(320,
    ClientWidth - 2 * Pad)), 214);
  FEmptyTitle.SetBounds(24, 24, FEmptyCard.Width - 48, 30);
  FEmptyText.SetBounds(24, 66, FEmptyCard.Width - 48, 52);
  FButtonEmptyConnect.SetBounds(24, 142, 128, ButtonH);
  FButtonDemo.SetBounds(164, 142, 156, ButtonH);

  FScroll.SetBounds(0, HeaderH, ClientWidth, Max(0, ClientHeight - HeaderH));
  ScrollY := FScroll.ScrollPosition;
  ContentW := Max(320, FScroll.ClientWidth - 2 * Pad);
  Wide := ContentW >= 980;
  Y := Pad;

  if Wide then
  begin
    LeftW := (ContentW - Gutter) * 58 div 100;
    RightW := ContentW - Gutter - LeftW;
    RightX := Pad + LeftW + Gutter;
    Place(FVehicleCard, Pad, Y, LeftW, 224);
    PlaceStatusTiles(RightX, Y, RightW, 2, StatusBottom);
    Y := Max(Y + 224, StatusBottom) + Gutter;

    Place(FAttentionCard, Pad, Y, LeftW, 314);
    Place(FQuickCard, RightX, Y, RightW, 314);
    BottomLeft := Y + 314;
    BottomRight := Y + 314;
    Place(FLiveCard, Pad, BottomLeft + Gutter, LeftW, 190);
    BottomLeft := BottomLeft + Gutter + 190;
    Y := Max(BottomLeft, BottomRight) + Pad;
  end
  else
  begin
    Place(FVehicleCard, Pad, Y, ContentW, 226);
    Inc(Y, 226 + Gutter);
    if ContentW >= 620 then
      PlaceStatusTiles(Pad, Y, ContentW, 2, StatusBottom)
    else
      PlaceStatusTiles(Pad, Y, ContentW, 1, StatusBottom);
    Y := StatusBottom + Gutter;
    Place(FAttentionCard, Pad, Y, ContentW, 314);
    Inc(Y, 314 + Gutter);
    Place(FQuickCard, Pad, Y, ContentW, 290);
    Inc(Y, 290 + Gutter);
    Place(FLiveCard, Pad, Y, ContentW, 190);
    Inc(Y, 190 + Pad);
  end;

  FVehicleCaption.SetBounds(24, 18, FVehicleCard.Width - 48, 18);
  FButtonChangeVehicle.SetBounds(FVehicleCard.Width - 140, 14, 124, 34);
  FVehicleTitle.SetBounds(24, 50, FVehicleCard.Width - 48, 30);
  FVehicleDetail.SetBounds(24, 84, FVehicleCard.Width - 48, 34);
  FVehicleList.SetBounds(24, 126, FVehicleCard.Width - 48,
    FVehicleCard.Height - 144);

  QuickTop := 70;
  QuickButtonW := (FQuickCard.Width - 48 - Gutter) div 2;
  if QuickButtonW < 150 then
  begin
    QuickButtonW := FQuickCard.Width - 48;
    FButtonQuickCodes.SetBounds(24, QuickTop, QuickButtonW, ButtonH);
    FButtonQuickLive.SetBounds(24, QuickTop + ButtonH + 10, QuickButtonW,
      ButtonH);
    FButtonQuickDashboard.SetBounds(24, QuickTop + 2 * (ButtonH + 10),
      QuickButtonW, ButtonH);
    FButtonQuickReadiness.SetBounds(24, QuickTop + 3 * (ButtonH + 10),
      QuickButtonW, ButtonH);
    FButtonQuickFuel.SetBounds(24, QuickTop + 4 * (ButtonH + 10), QuickButtonW,
      ButtonH);
  end
  else
  begin
    FButtonQuickCodes.SetBounds(24, QuickTop, QuickButtonW, ButtonH);
    FButtonQuickLive.SetBounds(24 + QuickButtonW + Gutter, QuickTop,
      QuickButtonW, ButtonH);
    FButtonQuickDashboard.SetBounds(24, QuickTop + ButtonH + 12, QuickButtonW,
      ButtonH);
    FButtonQuickReadiness.SetBounds(24 + QuickButtonW + Gutter,
      QuickTop + ButtonH + 12, QuickButtonW, ButtonH);
    FButtonQuickFuel.SetBounds(24, QuickTop + 2 * (ButtonH + 12),
      QuickButtonW, ButtonH);
  end;
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
  FTitle.Caption := Tr('personal.overview.title', 'Overview');
  FSubtitle.Caption := Tr('personal.overview.subtitle',
    'Vehicle summary, codes, readiness and key live values.');
  FButtonScan.Caption := Tr('personal.overview.scanAgain', 'Scan again');
  FButtonSaveReport.Caption := Tr('personal.overview.saveReport', 'Save report');
  FEmptyTitle.Caption := Tr('personal.overview.offlineTitle',
    'Connect to a vehicle');
  FEmptyText.Caption := Tr('personal.overview.offlineText',
    'The overview is filled after a quick scan. Connect an adapter, or try the built-in demo car.');
  FButtonEmptyConnect.Caption := Tr('personal.overview.connect', 'Connect');
  FButtonDemo.Caption := Tr('personal.overview.demo', 'Try the demo car');
  FVehicleCaption.Caption := Tr('personal.overview.vehicle', 'Vehicle');
  FButtonChangeVehicle.Caption := Tr('personal.overview.changeVehicle',
    'Change car');
  FTileMIL.Caption := Tr('personal.overview.tileMil', 'Check-engine light');
  FTileCodes.Caption := Tr('personal.overview.tileCodes', 'Trouble codes');
  FTileReadiness.Caption := Tr('personal.overview.tileReadiness', 'Readiness');
  FTileBattery.Caption := Tr('personal.overview.tileBattery', 'Battery');
  FAttentionCard.Title := Tr('personal.overview.attention', 'Needs attention');
  FQuickCard.Title := Tr('personal.overview.quickLinks', 'Quick links');
  FLiveCard.Title := Tr('personal.overview.live', 'Live values');
  FButtonQuickCodes.Caption := Tr('personal.overview.showCodes', 'Show codes');
  FButtonQuickLive.Caption := Tr('personal.overview.showLive', 'Live data');
  FButtonQuickDashboard.Caption := Tr('personal.overview.showDashboard',
    'Dashboard');
  FButtonQuickReadiness.Caption := Tr('personal.overview.showReadiness',
    'Show readiness');
  FButtonQuickFuel.Caption := Tr('personal.overview.showFuel', 'Fuel economy');
  FAttentionGrid.DataColumns[0].Caption := Tr('personal.overview.attentionSeverity',
    'Severity');
  FAttentionGrid.DataColumns[1].Caption := Tr('personal.overview.attentionItem',
    'Item');
  FAttentionGrid.DataColumns[2].Caption := Tr('personal.overview.attentionNext',
    'Next step');
  FAttentionGrid.EmptyGlyph := glAlert;
  UpdateView;
  LayoutControls;
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
  Battery: Double;
  LastScanText: string;
  Row: Integer;
  Item: TOBDKeyValueItem;

  procedure AddAttention(const ASeverity, AText, AAction: string;
    ATone: TOBDStatusKind; AData: NativeInt);
  begin
    Row := FAttentionGrid.AddRow([ASeverity, AText, AAction]);
    FAttentionGrid.CellTone[0, Row] := ATone;
    FAttentionGrid.RowData[Row] := AData;
    FAttentionGrid.RowAccent[Row] := ATone in [skWarning, skDanger];
  end;

begin
  Online := Assigned(FHost) and FHost.Connected;
  FEmptyCard.Visible := not Online;
  FScroll.Visible := Online;
  FButtonScan.Enabled := Online and not FScanning;
  FButtonSaveReport.Enabled := Online and not FScanning;
  if FScanning then
    FButtonScan.Caption := Tr('personal.overview.scanningButton', 'Scanning…')
  else
    FButtonScan.Caption := Tr('personal.overview.scanAgain', 'Scan again');

  if not Online then
  begin
    FSubtitle.Caption := Tr('personal.overview.subtitle',
      'Vehicle summary, codes, readiness and key live values.');
    LayoutControls;
    Exit;
  end;

  if FScanning then
    FSubtitle.Caption := Tr('personal.overview.scanning', 'Scanning vehicle…')
  else if FLastScan.Timestamp > 0 then
    FSubtitle.Caption := Format(Tr('personal.overview.scannedAt',
      'Scanned at %s'), [FormatDateTime('hh:nn', FLastScan.Timestamp)])
  else
    FSubtitle.Caption := Tr('personal.overview.subtitle',
      'Vehicle summary, codes, readiness and key live values.');

  FVehicleTitle.Caption := DashIfEmpty(FLastScan.VehicleLine);
  if FLastScan.ProtocolName <> '' then
    FVehicleDetail.Caption := DashIfEmpty(FLastScan.VehicleDetail) + '  ·  ' +
      FLastScan.ProtocolName
  else
    FVehicleDetail.Caption := DashIfEmpty(FLastScan.VehicleDetail);

  FVehicleList.BeginUpdate;
  try
    FVehicleList.Clear;
    FVehicleList.Add('', Tr('personal.overview.vin', 'VIN'),
      DashIfEmpty(FLastScan.VIN), kvkMono);
    FVehicleList.Add('', Tr('personal.overview.protocol', 'Protocol'),
      DashIfEmpty(FLastScan.ProtocolName));
    if FLastScan.Timestamp > 0 then
      LastScanText := FormatDateTime('hh:nn', FLastScan.Timestamp)
    else
      LastScanText := '—';
    FVehicleList.Add('', Tr('personal.overview.lastScan', 'Last scan'),
      LastScanText);
    Item := FVehicleList.Add('', Tr('personal.overview.status', 'Status'), '',
      kvkChip);
    if FLastScan.Timestamp = 0 then
    begin
      Item.Value := Tr('personal.overview.notScanned', 'Not scanned');
      Item.Tone := skNeutral;
    end
    else if TotalCodeCount > 0 then
    begin
      Item.Value := Tr('personal.overview.needsAttention', 'Needs attention');
      Item.Tone := skWarning;
    end
    else
    begin
      Item.Value := Tr('personal.overview.allGood', 'No immediate issues');
      Item.Tone := skSuccess;
    end;
  finally
    FVehicleList.EndUpdate;
  end;

  Total := TotalCodeCount;
  if FLastScan.Timestamp = 0 then
  begin
    ClearTile(FTileMIL, '');
    ClearTile(FTileCodes, Tr('personal.overview.codesUnit', 'codes'));
    ClearTile(FTileReadiness, Tr('personal.overview.openUnit', 'open'));
    ClearTile(FTileBattery, 'V');
  end
  else
  begin
    if FLastScan.MILKnown then
    begin
      if FLastScan.MILOn then
        SetTileValue(FTileMIL, 1, Tr('personal.overview.milOn', 'On'))
      else
        SetTileValue(FTileMIL, 0, Tr('personal.overview.milOff', 'Off'));
    end
    else
      ClearTile(FTileMIL, '');
    SetTileValue(FTileCodes, Total, Tr('personal.overview.codesUnit', 'codes'));
    SetTileValue(FTileReadiness, FLastScan.ReadinessIncomplete,
      Tr('personal.overview.openUnit', 'open'));
    if TryLeadingNumber(FLastScan.BatteryText, Battery) then
      SetTileValue(FTileBattery, Battery, 'V')
    else
      ClearTile(FTileBattery, 'V');
  end;

  FButtonQuickCodes.Enabled := Total > 0;
  FButtonQuickReadiness.Enabled := FLastScan.ReadinessSupported > 0;
  FButtonQuickLive.Enabled := Online;
  FButtonQuickDashboard.Enabled := Online;
  FButtonQuickFuel.Enabled := Online;

  FLiveList.BeginUpdate;
  try
    FLiveList.Clear;
    FLiveList.Add('', Tr('personal.overview.coolant', 'Coolant'),
      DashIfEmpty(FLastScan.CoolantText));
    FLiveList.Add('', Tr('personal.overview.battery', 'Battery'),
      DashIfEmpty(FLastScan.BatteryText));
    FLiveList.Add('', Tr('personal.overview.rpm', 'RPM'),
      DashIfEmpty(FLastScan.RPMText));
    FLiveList.Add('', Tr('personal.overview.fuelLevel', 'Fuel level'),
      DashIfEmpty(FLastScan.FuelLevelText));
  finally
    FLiveList.EndUpdate;
  end;

  FAttentionGrid.BeginUpdate;
  try
    FAttentionGrid.Clear;
    if FLastScan.Timestamp = 0 then
    begin
      FAttentionGrid.EmptyTitle := Tr('personal.overview.scanHintTitle',
        'Scan the vehicle');
      FAttentionGrid.EmptyHint := Tr('personal.overview.scanHint',
        'Press Scan again to read the current state.');
      FAttentionGrid.EmptyAction := Tr('personal.overview.scanAgain',
        'Scan again');
    end
    else
    begin
      FAttentionGrid.EmptyTitle := Tr('personal.overview.noAttention',
        'No immediate attention items found in the quick scan.');
      FAttentionGrid.EmptyHint := '';
      FAttentionGrid.EmptyAction := '';
      if FLastScan.MILKnown and FLastScan.MILOn then
        AddAttention(Tr('personal.overview.severityFault', 'Fault'),
          Tr('personal.overview.attentionMil',
          'The check engine light is on. Read the trouble codes first.'),
          Tr('personal.overview.showCodes', 'Show codes'), skDanger, 1);
      if Total > 0 then
        AddAttention(Tr('personal.overview.severityCodes', 'Codes'),
          Format(Tr('personal.overview.attentionCodes',
          '%d code(s): open Trouble codes for details.'), [Total]),
          Tr('personal.overview.showCodes', 'Show codes'), skDanger, 1);
      if FLastScan.ReadinessIncomplete > 0 then
        AddAttention(Tr('personal.overview.severityInspection', 'Inspection'),
          Format(Tr('personal.overview.attentionReadiness',
          '%d readiness monitor(s) incomplete.'),
          [FLastScan.ReadinessIncomplete]),
          Tr('personal.overview.showReadiness', 'Show readiness'), skWarning, 2);
      if FLastScan.ErrorText <> '' then
        AddAttention(Tr('personal.overview.severityInfo', 'Info'),
          FLastScan.ErrorText, Tr('personal.overview.review', 'Review'),
          skNeutral, 0);
    end;
  finally
    FAttentionGrid.EndUpdate;
  end;

  LayoutControls;
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

procedure TfrmPageOverview.ChangeVehicleClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_VEHICLES);
end;

procedure TfrmPageOverview.ShowLiveClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_LIVE);
end;

procedure TfrmPageOverview.ShowDashboardClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_DASHBOARD);
end;

procedure TfrmPageOverview.ShowFuelClicked(Sender: TObject);
begin
  if FHost <> nil then
    FHost.ShowPage(PAGE_FUEL);
end;

procedure TfrmPageOverview.AttentionActivate(Sender: TObject);
var
  Row: Integer;
begin
  Row := FAttentionGrid.SelectedRow;
  if Row < 0 then
    Exit;
  case FAttentionGrid.RowData[Row] of
    1:
      ShowCodesClicked(Sender);
    2:
      ShowReadinessClicked(Sender);
  end;
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageOverview.Commands: TPersonalCommands;
begin
  Result := [pcScanVehicle, pcSaveVehicleReport];
end;

function TfrmPageOverview.CommandEnabled(ACommand: TPersonalCommand): Boolean;
var
  Online: Boolean;
begin
  Online := Assigned(FHost) and FHost.Connected;
  case ACommand of
    pcScanVehicle:
      Result := Online and not FScanning;
    pcSaveVehicleReport:
      Result := Online and not FScanning;
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
