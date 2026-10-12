//------------------------------------------------------------------------------
//  untPageVehicles
//
//  My vehicles page of OBD-Studio Personal: every car of the user with its
//  details and its own history (scans, trouble codes, readiness, tests,
//  recordings, runs and notes). Connecting picks the car by the VIN it
//  reports; the user can also add cars by hand and choose the current car.
//------------------------------------------------------------------------------

unit untPageVehicles;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  ERD.I18n,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.UI.Card,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.Memo,
  ERD.UI.Chips,
  ERD.UI.DataGrid,
  ERD.UI.ScrollBox,
  untGarage,
  untPersonalPage;

type
  TfrmPageVehicles = class(TFrame, IPersonalPage, IPersonalCommands,
    IPersonalVehicleAware)
  strict private
    FHost: IPersonalHost;
    FScroll: TOBDScrollBox;
    FTitle: TOBDDialogText;
    FSubtitle: TOBDDialogText;
    FAddButton: TOBDButton;
    FUseButton: TOBDButton;
    FOffBanner: TOBDBanner;
    FListCard: TOBDCard;
    FList: TOBDDataGrid;
    FDetailsCard: TOBDCard;
    FNameLabel: TOBDDialogText;
    FNameEdit: TOBDEdit;
    FPlateLabel: TOBDDialogText;
    FPlateEdit: TOBDEdit;
    FMakeLabel: TOBDDialogText;
    FMakeEdit: TOBDEdit;
    FModelLabel: TOBDDialogText;
    FModelEdit: TOBDEdit;
    FYearLabel: TOBDDialogText;
    FYearEdit: TOBDEdit;
    FEngineLabel: TOBDDialogText;
    FEngineEdit: TOBDEdit;
    FFuelLabel: TOBDDialogText;
    FFuelCombo: TOBDComboBox;
    FCountryLabel: TOBDDialogText;
    FCountryCombo: TOBDComboBox;
    FVINLabel: TOBDDialogText;
    FVINEdit: TOBDEdit;
    FNotesLabel: TOBDDialogText;
    FNotesMemo: TOBDMemo;
    FSaveButton: TOBDButton;
    FDeleteButton: TOBDButton;
    FHistoryCard: TOBDCard;
    FHistory: TOBDDataGrid;
    FHistoryDetail: TOBDDialogText;
    FNoteButton: TOBDButton;
    FExportButton: TOBDButton;
    FClearButton: TOBDButton;
    FSelectedId: string;
    FCountryCodes: TArray<string>;
    FDirty: Boolean;
    FLoading: Boolean;
    procedure BuildControls;
    function NewText(AParent: TWinControl;
      AStyle: TOBDDialogTextStyle): TOBDDialogText;
    function NewEdit(AParent: TWinControl): TOBDEdit;
    function NewCombo(AParent: TWinControl): TOBDComboBox;
    function NewButton(AParent: TWinControl; AKind: TOBDButtonKind;
      AGlyph: TOBDGlyph): TOBDButton;
    function NewCard(AEdge: TOBDCardEdge): TOBDCard;
    procedure LayoutControls;
    procedure LayoutDetails;
    procedure LayoutHistory;
    procedure FillFuels;
    procedure FillCountries;
    procedure FillVehicles;
    procedure ShowDetails;
    procedure ShowHistory;
    procedure ShowHistoryDetail;
    procedure UpdateButtons;
    function SelectedVehicle: TPersonalVehicle;
    function VehicleCaption(AVehicle: TPersonalVehicle): string;
    function SaveDetails(AVehicle: TPersonalVehicle): Boolean;
    procedure AddVehicle;
    procedure UseVehicle;
    procedure DeleteVehicle;
    procedure AddNote;
    procedure ExportHistory;
    procedure ClearHistory;
    procedure ListChange(Sender: TObject);
    procedure ListActivate(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure DetailChange(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure UseClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure NoteClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    // IPersonalPage
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
    // IPersonalVehicleAware
    procedure VehicleChanged;
  end;

implementation

{$R *.dfm}

const
  // Fuel combo order; '' = not set.
  FUEL_CODES: array[0..6] of string = ('', PERSONAL_FUEL_PETROL,
    PERSONAL_FUEL_DIESEL, PERSONAL_FUEL_LPG, PERSONAL_FUEL_CNG,
    PERSONAL_FUEL_HYBRID, PERSONAL_FUEL_ELECTRIC);

  COL_HISTORY_KIND = 1;

  DETAILS_HEIGHT = 548;
  HISTORY_HEIGHT = 430;

function KindCaption(AKind: TPersonalHistoryKind): string;
begin
  case AKind of
    phkScan:
      Result := Tr('personal.history.kind.scan', 'Scan');
    phkCodesRead:
      Result := Tr('personal.history.kind.codesRead', 'Codes read');
    phkCodesCleared:
      Result := Tr('personal.history.kind.codesCleared', 'Codes cleared');
    phkReadiness:
      Result := Tr('personal.history.kind.readiness', 'Readiness');
    phkTests:
      Result := Tr('personal.history.kind.tests', 'Monitor tests');
    phkVehicleInfo:
      Result := Tr('personal.history.kind.vehicleInfo', 'Vehicle info');
    phkRecording:
      Result := Tr('personal.history.kind.recording', 'Recording');
    phkPerformance:
      Result := Tr('personal.history.kind.performance', 'Performance');
    phkBattery:
      Result := Tr('personal.history.kind.battery', 'EV battery');
  else
    Result := Tr('personal.history.kind.note', 'Note');
  end;
end;

function HistoryTone(const AEntry: TPersonalHistoryEntry): TOBDStatusKind;
begin
  case AEntry.Level of
    phlGood:
      Result := skSuccess;
    phlWarning:
      Result := skWarning;
    phlProblem:
      Result := skDanger;
  else
    if AEntry.Kind = phkNote then
      Result := skAccent
    else
      Result := skNeutral;
  end;
end;

{ TfrmPageVehicles }

constructor TfrmPageVehicles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentBackground := False;
  BuildControls;
  Translate;
end;

function TfrmPageVehicles.NewText(AParent: TWinControl;
  AStyle: TOBDDialogTextStyle): TOBDDialogText;
begin
  Result := TOBDDialogText.Create(Self);
  Result.Parent := AParent;
  Result.Style := AStyle;
  Result.WordWrap := True;
end;

function TfrmPageVehicles.NewEdit(AParent: TWinControl): TOBDEdit;
begin
  Result := TOBDEdit.Create(Self);
  Result.Parent := AParent;
  Result.OnChange := DetailChange;
end;

function TfrmPageVehicles.NewCombo(AParent: TWinControl): TOBDComboBox;
begin
  Result := TOBDComboBox.Create(Self);
  Result.Parent := AParent;
  Result.DropDownCount := 12;
  Result.OnChange := DetailChange;
end;

function TfrmPageVehicles.NewButton(AParent: TWinControl;
  AKind: TOBDButtonKind; AGlyph: TOBDGlyph): TOBDButton;
begin
  Result := TOBDButton.Create(Self);
  Result.Parent := AParent;
  Result.Kind := AKind;
  Result.Glyph := AGlyph;
end;

function TfrmPageVehicles.NewCard(AEdge: TOBDCardEdge): TOBDCard;
begin
  Result := TOBDCard.Create(Self);
  Result.Parent := FScroll;
  Result.StatusEdge := AEdge;
end;

procedure TfrmPageVehicles.BuildControls;
var
  Column: TOBDDataGridColumn;
begin
  FScroll := TOBDScrollBox.Create(Self);
  FScroll.Parent := Self;

  FTitle := NewText(FScroll, dtsHeading);
  FSubtitle := NewText(FScroll, dtsSubtle);
  FAddButton := NewButton(FScroll, bkPrimary, glPlus);
  FAddButton.OnClick := AddClick;
  FUseButton := NewButton(FScroll, bkSecondary, glCheck);
  FUseButton.OnClick := UseClick;

  FOffBanner := TOBDBanner.Create(Self);
  FOffBanner.Parent := FScroll;
  FOffBanner.Kind := bnWarning;
  FOffBanner.Visible := False;

  // The cars.
  FListCard := NewCard(ceNone);
  FList := TOBDDataGrid.Create(Self);
  FList.Parent := FListCard;
  Column := FList.DataColumns.Add;
  Column.Width := 32;
  Column.Kind := dckIcon;
  Column.Sortable := False;
  Column := FList.DataColumns.Add;
  Column.Width := 0;
  Column.Sortable := False;
  Column := FList.DataColumns.Add;
  Column.Width := 96;
  Column.Kind := dckMono;
  Column.Sortable := False;
  Column := FList.DataColumns.Add;
  Column.Width := 100;
  Column.Kind := dckMono;
  Column.Sortable := False;
  FList.EmptyGlyph := glVehicle;
  FList.OnChange := ListChange;
  FList.OnItemActivate := ListActivate;
  FList.OnEmptyAction := AddClick;

  // Details of the selected car.
  FDetailsCard := NewCard(ceNone);
  FNameLabel := NewText(FDetailsCard, dtsCaps);
  FNameEdit := NewEdit(FDetailsCard);
  FNameEdit.MaxLength := 60;
  FPlateLabel := NewText(FDetailsCard, dtsCaps);
  FPlateEdit := NewEdit(FDetailsCard);
  FPlateEdit.MaxLength := 16;
  FPlateEdit.CharCase := ecUpperCase;
  FMakeLabel := NewText(FDetailsCard, dtsCaps);
  FMakeEdit := NewEdit(FDetailsCard);
  FMakeEdit.MaxLength := 40;
  FModelLabel := NewText(FDetailsCard, dtsCaps);
  FModelEdit := NewEdit(FDetailsCard);
  FModelEdit.MaxLength := 40;
  FYearLabel := NewText(FDetailsCard, dtsCaps);
  FYearEdit := NewEdit(FDetailsCard);
  FYearEdit.MaxLength := 4;
  FYearEdit.NumbersOnly := True;
  FEngineLabel := NewText(FDetailsCard, dtsCaps);
  FEngineEdit := NewEdit(FDetailsCard);
  FEngineEdit.MaxLength := 5;
  FEngineEdit.NumbersOnly := True;
  FFuelLabel := NewText(FDetailsCard, dtsCaps);
  FFuelCombo := NewCombo(FDetailsCard);
  FCountryLabel := NewText(FDetailsCard, dtsCaps);
  FCountryCombo := NewCombo(FDetailsCard);
  FVINLabel := NewText(FDetailsCard, dtsCaps);
  FVINEdit := NewEdit(FDetailsCard);
  FVINEdit.MaxLength := 17;
  FVINEdit.CharCase := ecUpperCase;
  FVINEdit.Mono := True;
  FNotesLabel := NewText(FDetailsCard, dtsCaps);
  FNotesMemo := TOBDMemo.Create(Self);
  FNotesMemo.Parent := FDetailsCard;
  FNotesMemo.WordWrap := True;
  FNotesMemo.MaxLength := 2000;
  FNotesMemo.OnChange := DetailChange;
  FSaveButton := NewButton(FDetailsCard, bkPrimary, glSave);
  FSaveButton.OnClick := SaveClick;
  FDeleteButton := NewButton(FDetailsCard, bkDangerOutline, glClear);
  FDeleteButton.OnClick := DeleteClick;

  // History of the selected car, newest first.
  FHistoryCard := NewCard(ceNone);
  FHistory := TOBDDataGrid.Create(Self);
  FHistory.Parent := FHistoryCard;
  Column := FHistory.DataColumns.Add;
  Column.Width := 132;
  Column.Kind := dckMono;
  Column.Sortable := False;
  Column := FHistory.DataColumns.Add;
  Column.Width := 126;
  Column.Kind := dckChip;
  Column.Sortable := False;
  Column := FHistory.DataColumns.Add;
  Column.Width := 0;
  Column.Sortable := False;
  FHistory.EmptyGlyph := glClock;
  FHistory.OnChange := HistoryChange;
  FHistoryDetail := NewText(FHistoryCard, dtsSmall);
  FNoteButton := NewButton(FHistoryCard, bkSecondary, glEdit);
  FNoteButton.OnClick := NoteClick;
  FExportButton := NewButton(FHistoryCard, bkSecondary, glExport);
  FExportButton.OnClick := ExportClick;
  FClearButton := NewButton(FHistoryCard, bkDangerOutline, glClear);
  FClearButton.OnClick := ClearClick;
end;

procedure TfrmPageVehicles.Resize;
begin
  inherited Resize;
  LayoutControls;
end;

procedure TfrmPageVehicles.LayoutControls;
var
  ContentW, LeftX, RightX, ListW, RightW, Gap, Y, ButtonsX: Integer;
  TwoColumns: Boolean;
begin
  // Resize also runs while the frame is created, before BuildControls.
  if FClearButton = nil then
    Exit;
  FScroll.SetBounds(0, 0, ClientWidth, ClientHeight);
  ContentW := Max(320, ClientWidth - 48);
  Gap := 16;
  LeftX := 24;

  // Header: title and subtitle on the left, the main buttons on the right.
  FUseButton.Left := LeftX + ContentW - FUseButton.Width;
  FUseButton.Top := 22;
  FAddButton.Left := FUseButton.Left - 8 - FAddButton.Width;
  FAddButton.Top := 22;
  ButtonsX := Max(LeftX + 160, FAddButton.Left - Gap);
  FTitle.SetBounds(LeftX, 18, ButtonsX - LeftX, 28);
  FSubtitle.SetBounds(LeftX, 46, ButtonsX - LeftX, 36);

  Y := 92;
  if FOffBanner.Visible then
  begin
    FOffBanner.SetBounds(LeftX, Y, ContentW, FOffBanner.Height);
    Inc(Y, FOffBanner.Height + Gap);
  end;

  TwoColumns := ContentW >= 900;
  if TwoColumns then
  begin
    ListW := Max(300, ContentW * 2 div 5);
    RightW := ContentW - ListW - Gap;
    RightX := LeftX + ListW + Gap;
    FListCard.SetBounds(LeftX, Y, ListW,
      DETAILS_HEIGHT + Gap + HISTORY_HEIGHT);
    FDetailsCard.SetBounds(RightX, Y, RightW, DETAILS_HEIGHT);
    FHistoryCard.SetBounds(RightX, Y + DETAILS_HEIGHT + Gap, RightW,
      HISTORY_HEIGHT);
  end
  else
  begin
    FListCard.SetBounds(LeftX, Y, ContentW, 300);
    Inc(Y, FListCard.Height + Gap);
    FDetailsCard.SetBounds(LeftX, Y, ContentW, DETAILS_HEIGHT);
    Inc(Y, FDetailsCard.Height + Gap);
    FHistoryCard.SetBounds(LeftX, Y, ContentW, HISTORY_HEIGHT);
  end;

  FList.SetBounds(16, 58, FListCard.Width - 32, FListCard.Height - 58 - 48);
  LayoutDetails;
  LayoutHistory;
end;

procedure TfrmPageVehicles.LayoutDetails;
var
  ColW, X1, X2, Y, RowW: Integer;

  procedure Row(ALabel1: TOBDDialogText; AControl1: TControl;
    ALabel2: TOBDDialogText; AControl2: TControl);
  begin
    ALabel1.SetBounds(X1, Y, ColW, 18);
    AControl1.SetBounds(X1, Y + 20, ColW, 30);
    if ALabel2 <> nil then
    begin
      ALabel2.SetBounds(X2, Y, ColW, 18);
      AControl2.SetBounds(X2, Y + 20, ColW, 30);
    end;
    Inc(Y, 62);
  end;

begin
  RowW := FDetailsCard.Width - 32;
  ColW := (RowW - 16) div 2;
  X1 := 16;
  X2 := X1 + ColW + 16;
  Y := 60;
  Row(FNameLabel, FNameEdit, FPlateLabel, FPlateEdit);
  Row(FMakeLabel, FMakeEdit, FModelLabel, FModelEdit);
  Row(FYearLabel, FYearEdit, FEngineLabel, FEngineEdit);
  Row(FFuelLabel, FFuelCombo, FCountryLabel, FCountryCombo);
  Row(FVINLabel, FVINEdit, nil, nil);
  FVINEdit.Width := RowW;
  FVINLabel.Width := RowW;
  FNotesLabel.SetBounds(X1, Y, RowW, 18);
  FNotesMemo.SetBounds(X1, Y + 20, RowW, 64);
  Inc(Y, 96);
  FSaveButton.Left := X1;
  FSaveButton.Top := Y;
  FDeleteButton.Left := FSaveButton.Left + FSaveButton.Width + 8;
  FDeleteButton.Top := Y;
end;

procedure TfrmPageVehicles.LayoutHistory;
var
  RowW, ButtonsY, X: Integer;
begin
  RowW := FHistoryCard.Width - 32;
  ButtonsY := FHistoryCard.Height - 48 - 16 - FNoteButton.Height;
  FHistoryDetail.SetBounds(16, ButtonsY - 50, RowW, 42);
  FHistory.SetBounds(16, 58, RowW, Max(80, ButtonsY - 58 - 58));
  X := 16;
  FNoteButton.Left := X;
  FNoteButton.Top := ButtonsY;
  Inc(X, FNoteButton.Width + 8);
  FExportButton.Left := X;
  FExportButton.Top := ButtonsY;
  Inc(X, FExportButton.Width + 8);
  FClearButton.Left := X;
  FClearButton.Top := ButtonsY;
end;

{ IPersonalPage ------------------------------------------------------------- }

function TfrmPageVehicles.PageKey: string;
begin
  Result := PAGE_VEHICLES;
end;

procedure TfrmPageVehicles.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  FillCountries;
  VehicleChanged;
end;

procedure TfrmPageVehicles.Activated(AActive: Boolean);
begin
  if AActive then
  begin
    FillCountries;
    VehicleChanged;
  end
  else if FDirty then
    // Leaving the page keeps what was typed.
    SaveDetails(SelectedVehicle);
end;

procedure TfrmPageVehicles.ConnectionChanged(AConnected: Boolean);
begin
  UpdateButtons;
end;

procedure TfrmPageVehicles.UnitsChanged;
begin
end;

procedure TfrmPageVehicles.Translate;
begin
  FTitle.Caption := Tr('personal.vehicles.title', 'My vehicles');
  FSubtitle.Caption := Tr('personal.vehicles.subtitle',
    'Every car keeps its own history: scans, trouble codes, readiness, ' +
    'tests, recordings and your notes. Connecting picks the car by its VIN.');
  FAddButton.Caption := Tr('personal.cmd.addVehicle', 'Add vehicle');
  FUseButton.Caption := Tr('personal.cmd.useVehicle', 'Use this car');
  FOffBanner.Title := Tr('personal.vehicles.offTitle',
    'Remembering vehicles is switched off');
  FOffBanner.Text := Tr('personal.vehicles.offText',
    'Connecting does not add cars and nothing is written to the history. ' +
    'Switch it on again in Settings, Privacy.');

  FListCard.Title := Tr('personal.vehicles.listTitle', 'Vehicles');
  FList.DataColumns[1].Caption := Tr('personal.vehicles.colName', 'Name');
  FList.DataColumns[2].Caption := Tr('personal.vehicles.colPlate', 'Plate');
  FList.DataColumns[3].Caption := Tr('personal.vehicles.colLastSeen',
    'Last connected');
  FList.EmptyTitle := Tr('personal.vehicles.emptyTitle', 'No vehicles yet');
  FList.EmptyHint := Tr('personal.vehicles.emptyHint',
    'Connect to your car and it is added by its VIN, or add one by hand.');
  FList.EmptyAction := Tr('personal.cmd.addVehicle', 'Add vehicle');

  FNameLabel.Caption := Tr('personal.vehicles.name', 'Name');
  FNameEdit.TextHint := Tr('personal.vehicles.nameHint',
    'For example: Golf, the van');
  FPlateLabel.Caption := Tr('personal.vehicles.plate', 'Licence plate');
  FMakeLabel.Caption := Tr('personal.vehicles.make', 'Make');
  FModelLabel.Caption := Tr('personal.vehicles.model', 'Model');
  FYearLabel.Caption := Tr('personal.vehicles.year', 'Model year');
  FEngineLabel.Caption := Tr('personal.vehicles.engine', 'Engine (cc)');
  FFuelLabel.Caption := Tr('personal.vehicles.fuel', 'Fuel');
  FCountryLabel.Caption := Tr('personal.vehicles.country',
    'Inspection country');
  FVINLabel.Caption := Tr('personal.vehicles.vin', 'VIN');
  FVINEdit.TextHint := Tr('personal.vehicles.vinHint',
    'Filled in when you connect');
  FNotesLabel.Caption := Tr('personal.vehicles.notes', 'Notes');
  FSaveButton.Caption := Tr('personal.vehicles.save', 'Save changes');
  FDeleteButton.Caption := Tr('personal.cmd.deleteVehicle', 'Delete vehicle');

  FHistoryCard.Title := Tr('personal.vehicles.historyTitle', 'History');
  FHistory.DataColumns[0].Caption := Tr('personal.vehicles.colDate', 'Date');
  FHistory.DataColumns[1].Caption := Tr('personal.vehicles.colEvent',
    'Event');
  FHistory.DataColumns[2].Caption := Tr('personal.vehicles.colSummary',
    'Summary');
  FHistory.EmptyTitle := Tr('personal.vehicles.historyEmptyTitle',
    'No history yet');
  FHistory.EmptyHint := Tr('personal.vehicles.historyEmptyHint',
    'Scans, trouble codes, readiness checks, tests and your notes for this ' +
    'car appear here.');
  FNoteButton.Caption := Tr('personal.cmd.addNote', 'Add note');
  FExportButton.Caption := Tr('personal.cmd.exportHistory', 'Export history');
  FClearButton.Caption := Tr('personal.vehicles.clearHistory',
    'Clear history');

  FillFuels;
  if FHost <> nil then
  begin
    FillCountries;
    VehicleChanged;
  end;
  LayoutControls;
end;

{ IPersonalVehicleAware ----------------------------------------------------- }

procedure TfrmPageVehicles.VehicleChanged;
var
  ShowBanner: Boolean;
begin
  if FHost = nil then
    Exit;
  ShowBanner := not FHost.Settings.RememberVehicles;
  if FOffBanner.Visible <> ShowBanner then
  begin
    FOffBanner.Visible := ShowBanner;
    LayoutControls;
  end;
  FillVehicles;
  // What the user is typing stays; a car that was deleted or another
  // selection shows its own details.
  if not FDirty then
    ShowDetails;
  ShowHistory;
  UpdateButtons;
end;

{ Filling ------------------------------------------------------------------- }

procedure TfrmPageVehicles.FillFuels;
var
  Index: Integer;
begin
  Index := FFuelCombo.ItemIndex;
  FLoading := True;
  try
    FFuelCombo.Items.Text := Tr('personal.vehicles.fuelNone', 'Not set') +
      sLineBreak + Tr('personal.vehicles.fuelPetrol', 'Petrol') + sLineBreak +
      Tr('personal.vehicles.fuelDiesel', 'Diesel') + sLineBreak +
      Tr('personal.vehicles.fuelLPG', 'LPG') + sLineBreak +
      Tr('personal.vehicles.fuelCNG', 'CNG') + sLineBreak +
      Tr('personal.vehicles.fuelHybrid', 'Hybrid') + sLineBreak +
      Tr('personal.vehicles.fuelElectric', 'Electric');
    FFuelCombo.ItemIndex := Max(0, Index);
  finally
    FLoading := False;
  end;
end;

procedure TfrmPageVehicles.FillCountries;
var
  Codes: TArray<string>;
  Index, I: Integer;
  SettingsCountry: string;
begin
  if FHost = nil then
    Exit;
  Codes := FHost.Session.Inspection.CountryCodes;
  Index := FCountryCombo.ItemIndex;
  SettingsCountry := FHost.Settings.InspectionCountry;
  FLoading := True;
  try
    FCountryCombo.Items.BeginUpdate;
    try
      FCountryCombo.Items.Clear;
      if SettingsCountry <> '' then
        FCountryCombo.Items.Add(TrFmt('personal.vehicles.countryDefault',
          'Same as in Settings (%s)', [UpperCase(SettingsCountry)]))
      else
        FCountryCombo.Items.Add(Tr('personal.vehicles.countryNone',
          'Same as in Settings'));
      SetLength(FCountryCodes, Length(Codes));
      for I := 0 to High(Codes) do
      begin
        FCountryCodes[I] := UpperCase(Codes[I]);
        FCountryCombo.Items.Add(FCountryCodes[I]);
      end;
    finally
      FCountryCombo.Items.EndUpdate;
    end;
    if Index >= FCountryCombo.Items.Count then
      Index := 0;
    FCountryCombo.ItemIndex := Max(0, Index);
  finally
    FLoading := False;
  end;
end;

function TfrmPageVehicles.VehicleCaption(AVehicle: TPersonalVehicle): string;
begin
  Result := AVehicle.DisplayName;
  if Result = '' then
    Result := Tr('personal.vehicles.unnamed', 'My car');
end;

function TfrmPageVehicles.SelectedVehicle: TPersonalVehicle;
begin
  if FHost = nil then
    Exit(nil);
  Result := FHost.Garage.FindById(FSelectedId);
end;

procedure TfrmPageVehicles.FillVehicles;
var
  Garage: TPersonalGarage;
  Current, Vehicle: TPersonalVehicle;
  I, Row, Selected: Integer;
  LastSeen: string;
begin
  Garage := FHost.Garage;
  Current := FHost.CurrentVehicle;
  // The selected car was deleted, or nothing was selected yet: show the
  // current car.
  if Garage.FindById(FSelectedId) = nil then
  begin
    FDirty := False;
    if Current <> nil then
      FSelectedId := Current.Id
    else if Garage.Count > 0 then
      FSelectedId := Garage[0].Id
    else
      FSelectedId := '';
  end;

  FLoading := True;
  try
    Selected := -1;
    FList.BeginUpdate;
    try
      FList.Clear;
      for I := 0 to Garage.Count - 1 do
      begin
        Vehicle := Garage[I];
        if Vehicle.LastSeen > 0 then
          LastSeen := FormatDateTime('yyyy-mm-dd', Vehicle.LastSeen)
        else
          LastSeen := Tr('personal.vehicles.never', 'Never');
        Row := FList.AddRow(['', VehicleCaption(Vehicle), Vehicle.Plate,
          LastSeen]);
        FList.RowData[Row] := I;
        if Vehicle = Current then
        begin
          FList.RowGlyph[Row] := glCheck;
          FList.RowAccent[Row] := True;
        end;
        if SameText(Vehicle.Id, FSelectedId) then
          Selected := Row;
      end;
    finally
      FList.EndUpdate;
    end;
    FList.ItemIndex := Selected;
  finally
    FLoading := False;
  end;
  FListCard.FooterText := TrFmt('personal.vehicles.count', 'Vehicles: %d',
    [Garage.Count]);
end;

procedure TfrmPageVehicles.ShowDetails;
var
  Vehicle: TPersonalVehicle;
  Index, I: Integer;
  Footer: string;
begin
  Vehicle := SelectedVehicle;
  FLoading := True;
  try
    if Vehicle = nil then
    begin
      FNameEdit.Text := '';
      FPlateEdit.Text := '';
      FMakeEdit.Text := '';
      FModelEdit.Text := '';
      FYearEdit.Text := '';
      FEngineEdit.Text := '';
      FVINEdit.Text := '';
      FNotesMemo.Text := '';
      FFuelCombo.ItemIndex := 0;
      FCountryCombo.ItemIndex := 0;
    end
    else
    begin
      FNameEdit.Text := Vehicle.Name;
      FPlateEdit.Text := Vehicle.Plate;
      FMakeEdit.Text := Vehicle.Make;
      FModelEdit.Text := Vehicle.Model;
      if Vehicle.ModelYear > 0 then
        FYearEdit.Text := IntToStr(Vehicle.ModelYear)
      else
        FYearEdit.Text := '';
      if Vehicle.EngineCC > 0 then
        FEngineEdit.Text := IntToStr(Vehicle.EngineCC)
      else
        FEngineEdit.Text := '';
      FVINEdit.Text := Vehicle.VIN;
      FNotesMemo.Text := Vehicle.Notes;
      Index := 0;
      for I := 0 to High(FUEL_CODES) do
        if SameText(FUEL_CODES[I], Vehicle.Fuel) then
          Index := I;
      FFuelCombo.ItemIndex := Index;
      Index := 0;
      for I := 0 to High(FCountryCodes) do
        if SameText(FCountryCodes[I], Vehicle.InspectionCountry) then
          Index := I + 1;
      FCountryCombo.ItemIndex := Index;
    end;
  finally
    FLoading := False;
  end;
  FDirty := False;

  FNameEdit.Enabled := Vehicle <> nil;
  FPlateEdit.Enabled := Vehicle <> nil;
  FMakeEdit.Enabled := Vehicle <> nil;
  FModelEdit.Enabled := Vehicle <> nil;
  FYearEdit.Enabled := Vehicle <> nil;
  FEngineEdit.Enabled := Vehicle <> nil;
  FFuelCombo.Enabled := Vehicle <> nil;
  FCountryCombo.Enabled := Vehicle <> nil;
  FVINEdit.Enabled := Vehicle <> nil;
  FNotesMemo.Enabled := Vehicle <> nil;

  if Vehicle = nil then
  begin
    FDetailsCard.Title := Tr('personal.vehicles.detailsTitle', 'Details');
    FDetailsCard.StatusEdge := ceNone;
    FDetailsCard.FooterText := '';
  end
  else
  begin
    FDetailsCard.Title := VehicleCaption(Vehicle);
    Footer := TrFmt('personal.vehicles.added', 'Added %s',
      [FormatDateTime('yyyy-mm-dd', Vehicle.Added)]);
    if Vehicle = FHost.CurrentVehicle then
    begin
      FDetailsCard.StatusEdge := ceAccent;
      Footer := Footer + ' · ' + Tr('personal.vehicles.isCurrent',
        'Current car: new results are saved in its history');
    end
    else
      FDetailsCard.StatusEdge := ceNone;
    FDetailsCard.FooterText := Footer;
  end;
end;

procedure TfrmPageVehicles.ShowHistory;
var
  Vehicle: TPersonalVehicle;
  Entry: TPersonalHistoryEntry;
  I, Row: Integer;
begin
  Vehicle := SelectedVehicle;
  FLoading := True;
  try
    FHistory.BeginUpdate;
    try
      FHistory.Clear;
      if Vehicle <> nil then
        for I := Vehicle.History.Count - 1 downto 0 do
        begin
          Entry := Vehicle.History[I];
          Row := FHistory.AddRow([FormatDateTime('yyyy-mm-dd hh:nn',
            Entry.Time), KindCaption(Entry.Kind), Entry.Title]);
          FHistory.RowData[Row] := I;
          FHistory.CellTone[COL_HISTORY_KIND, Row] := HistoryTone(Entry);
        end;
    finally
      FHistory.EndUpdate;
    end;
    if FHistory.RowCount > 0 then
      FHistory.ItemIndex := 0;
  finally
    FLoading := False;
  end;
  if Vehicle <> nil then
    FHistoryCard.FooterText := TrFmt('personal.vehicles.historyCount',
      'Entries: %d', [Vehicle.History.Count])
  else
    FHistoryCard.FooterText := '';
  ShowHistoryDetail;
end;

procedure TfrmPageVehicles.ShowHistoryDetail;
var
  Vehicle: TPersonalVehicle;
  Row, Index: Integer;
  Entry: TPersonalHistoryEntry;
begin
  Vehicle := SelectedVehicle;
  Row := FHistory.SelectedRow;
  if (Vehicle = nil) or (Row < 0) then
  begin
    FHistoryDetail.Caption := '';
    Exit;
  end;
  Index := FHistory.RowData[Row];
  if (Index < 0) or (Index >= Vehicle.History.Count) then
  begin
    FHistoryDetail.Caption := '';
    Exit;
  end;
  Entry := Vehicle.History[Index];
  if Entry.Detail <> '' then
    FHistoryDetail.Caption := Entry.Title + sLineBreak + Entry.Detail
  else
    FHistoryDetail.Caption := Entry.Title;
end;

procedure TfrmPageVehicles.UpdateButtons;
var
  Vehicle: TPersonalVehicle;
begin
  Vehicle := SelectedVehicle;
  FAddButton.Enabled := FHost <> nil;
  FUseButton.Enabled := CommandEnabled(pcUseVehicle);
  FSaveButton.Enabled := (Vehicle <> nil) and FDirty;
  FDeleteButton.Enabled := Vehicle <> nil;
  FNoteButton.Enabled := Vehicle <> nil;
  FExportButton.Enabled := CommandEnabled(pcExportHistory);
  FClearButton.Enabled := (Vehicle <> nil) and (Vehicle.History.Count > 0);
end;

{ Editing ------------------------------------------------------------------- }

function TfrmPageVehicles.SaveDetails(AVehicle: TPersonalVehicle): Boolean;
var
  VIN: string;
  Other: TPersonalVehicle;
  Value: Integer;
begin
  Result := False;
  FDirty := False;
  if (FHost = nil) or (AVehicle = nil) then
    Exit;
  VIN := UpperCase(Trim(FVINEdit.Text));
  Other := nil;
  if VIN <> '' then
    Other := FHost.Garage.FindByVIN(VIN);
  if (Other <> nil) and (Other <> AVehicle) then
  begin
    FHost.Notify(tkWarning, Tr('personal.vehicles.vinTakenTitle',
      'VIN already used'), TrFmt('personal.vehicles.vinTakenText',
      '%s already has this VIN; the VIN was not changed.',
      [VehicleCaption(Other)]));
    VIN := AVehicle.VIN;
  end;
  AVehicle.Name := Trim(FNameEdit.Text);
  AVehicle.Plate := Trim(FPlateEdit.Text);
  AVehicle.Make := Trim(FMakeEdit.Text);
  AVehicle.Model := Trim(FModelEdit.Text);
  Value := StrToIntDef(Trim(FYearEdit.Text), 0);
  if (Value < 1950) or (Value > 2100) then
    Value := 0;
  AVehicle.ModelYear := Value;
  Value := StrToIntDef(Trim(FEngineEdit.Text), 0);
  if (Value < 0) or (Value > 20000) then
    Value := 0;
  AVehicle.EngineCC := Value;
  if (FFuelCombo.ItemIndex >= 0) and
    (FFuelCombo.ItemIndex <= High(FUEL_CODES)) then
    AVehicle.Fuel := FUEL_CODES[FFuelCombo.ItemIndex];
  if (FCountryCombo.ItemIndex > 0) and
    (FCountryCombo.ItemIndex <= Length(FCountryCodes)) then
    AVehicle.InspectionCountry := FCountryCodes[FCountryCombo.ItemIndex - 1]
  else
    AVehicle.InspectionCountry := '';
  AVehicle.VIN := VIN;
  AVehicle.Notes := TrimRight(FNotesMemo.Text);
  try
    FHost.Garage.Save(AVehicle);
    Result := True;
  except
    on E: Exception do
      FHost.Notify(tkDanger, Tr('personal.vehicles.notSaved',
        'Vehicle not saved'), E.Message);
  end;
  FHost.VehiclesChanged;
end;

procedure TfrmPageVehicles.AddVehicle;
var
  VehicleName: string;
  Vehicle: TPersonalVehicle;
begin
  if FHost = nil then
    Exit;
  VehicleName := '';
  if not OBDInputQuery(Tr('personal.cmd.addVehicle', 'Add vehicle'),
    Tr('personal.vehicles.addPrompt',
    'Name of the car, for example Golf or the van'), VehicleName) then
    Exit;
  VehicleName := Trim(VehicleName);
  if VehicleName = '' then
    Exit;
  if FDirty then
    SaveDetails(SelectedVehicle);
  Vehicle := nil;
  try
    Vehicle := FHost.Garage.Add(VehicleName, '');
  except
    on E: Exception do
      FHost.Notify(tkDanger, Tr('personal.vehicles.notSaved',
        'Vehicle not saved'), E.Message);
  end;
  if Vehicle <> nil then
    FSelectedId := Vehicle.Id;
  FDirty := False;
  // The first car becomes the current one.
  if (Vehicle <> nil) and (FHost.CurrentVehicle = nil) then
    FHost.SelectVehicle(Vehicle)
  else
    FHost.VehiclesChanged;
end;

procedure TfrmPageVehicles.UseVehicle;
var
  Vehicle: TPersonalVehicle;
  VIN: string;
begin
  Vehicle := SelectedVehicle;
  if Vehicle = nil then
    Exit;
  if FDirty then
    SaveDetails(Vehicle);
  FHost.SelectVehicle(Vehicle);
  VIN := FHost.Session.VehicleVIN;
  if FHost.Connected and (VIN <> '') and (Vehicle.VIN <> '') and
    not SameText(VIN, Vehicle.VIN) then
    FHost.Notify(tkWarning, Tr('personal.vehicles.otherCarTitle',
      'Another car is connected'), TrFmt('personal.vehicles.otherCarText',
      'The connected car reports VIN %s, not the VIN of %s.',
      [VIN, VehicleCaption(Vehicle)]))
  else
    FHost.Notify(tkInfo, Tr('personal.vehicles.usingTitle',
      'Current vehicle'), TrFmt('personal.vehicles.usingText',
      'New results are saved in the history of %s.',
      [VehicleCaption(Vehicle)]));
end;

procedure TfrmPageVehicles.DeleteVehicle;
var
  Vehicle: TPersonalVehicle;
begin
  Vehicle := SelectedVehicle;
  if Vehicle = nil then
    Exit;
  if not FHost.Confirm(dkDanger, Tr('personal.vehicles.deleteTitle',
    'Delete vehicle?'), TrFmt('personal.vehicles.deleteText',
    '%s and its history (%d entries) are deleted from this PC. This ' +
    'cannot be undone.', [VehicleCaption(Vehicle), Vehicle.History.Count]),
    Tr('personal.vehicles.deleteConfirm', 'Delete')) then
    Exit;
  FDirty := False;
  if Vehicle = FHost.CurrentVehicle then
    FHost.SelectVehicle(nil);
  try
    FHost.Garage.Delete(Vehicle);
  except
    on E: Exception do
      FHost.Notify(tkDanger, Tr('personal.vehicles.notDeleted',
        'Vehicle not deleted'), E.Message);
  end;
  FSelectedId := '';
  FHost.VehiclesChanged;
end;

procedure TfrmPageVehicles.AddNote;
var
  Vehicle: TPersonalVehicle;
  Note: string;
begin
  Vehicle := SelectedVehicle;
  if Vehicle = nil then
    Exit;
  Note := '';
  if not OBDInputQuery(Tr('personal.cmd.addNote', 'Add note'),
    Tr('personal.vehicles.notePrompt',
    'Note, for example: oil changed at 182,000 km'), Note) then
    Exit;
  Note := Trim(Note);
  if Note = '' then
    Exit;
  try
    FHost.Garage.AddHistory(Vehicle, phkNote, phlInfo, Note, '');
  except
    on E: Exception do
      FHost.Notify(tkDanger, Tr('personal.vehicles.noteNotSaved',
        'Note not saved'), E.Message);
  end;
  FHost.VehiclesChanged;
end;

procedure TfrmPageVehicles.ExportHistory;
var
  Vehicle: TPersonalVehicle;
  FileName: string;
begin
  Vehicle := SelectedVehicle;
  if (Vehicle = nil) or (Vehicle.History.Count = 0) then
    Exit;
  try
    FileName := TPath.Combine(FHost.Settings.DataSubFolder('Reports'),
      FormatDateTime('yyyymmdd-hhnnss', Now) + '-history-' + Vehicle.Id +
      '.csv');
    FHost.Garage.ExportHistoryCSV(Vehicle, FileName);
    FHost.Notify(tkSuccess, Tr('personal.vehicles.exported',
      'History exported'), FileName);
  except
    on E: Exception do
      FHost.Notify(tkDanger, Tr('personal.vehicles.exportFailed',
        'History not exported'), E.Message);
  end;
end;

procedure TfrmPageVehicles.ClearHistory;
var
  Vehicle: TPersonalVehicle;
begin
  Vehicle := SelectedVehicle;
  if (Vehicle = nil) or (Vehicle.History.Count = 0) then
    Exit;
  if not FHost.Confirm(dkDanger, Tr('personal.vehicles.clearTitle',
    'Clear history?'), TrFmt('personal.vehicles.clearText',
    'All %d entries in the history of %s are deleted. The car itself ' +
    'stays.', [Vehicle.History.Count, VehicleCaption(Vehicle)]),
    Tr('personal.vehicles.clearHistory', 'Clear history')) then
    Exit;
  try
    FHost.Garage.ClearHistory(Vehicle);
  except
    on E: Exception do
      FHost.Notify(tkDanger, Tr('personal.vehicles.notSaved',
        'Vehicle not saved'), E.Message);
  end;
  FHost.VehiclesChanged;
end;

{ Events -------------------------------------------------------------------- }

procedure TfrmPageVehicles.ListChange(Sender: TObject);
var
  Row, Index: Integer;
  Previous: TPersonalVehicle;
  NewId: string;
begin
  if FLoading or (FHost = nil) then
    Exit;
  Row := FList.SelectedRow;
  if Row < 0 then
    Exit;
  Index := FList.RowData[Row];
  if (Index < 0) or (Index >= FHost.Garage.Count) then
    Exit;
  NewId := FHost.Garage[Index].Id;
  if SameText(NewId, FSelectedId) then
    Exit;
  Previous := SelectedVehicle;
  FSelectedId := NewId;
  // Choosing another car keeps what was typed for the previous one.
  if FDirty and (Previous <> nil) then
    SaveDetails(Previous);
  ShowDetails;
  ShowHistory;
  UpdateButtons;
end;

procedure TfrmPageVehicles.ListActivate(Sender: TObject);
begin
  UseVehicle;
end;

procedure TfrmPageVehicles.HistoryChange(Sender: TObject);
begin
  if not FLoading then
    ShowHistoryDetail;
end;

procedure TfrmPageVehicles.DetailChange(Sender: TObject);
begin
  if FLoading then
    Exit;
  FDirty := True;
  FSaveButton.Enabled := SelectedVehicle <> nil;
end;

procedure TfrmPageVehicles.AddClick(Sender: TObject);
begin
  AddVehicle;
end;

procedure TfrmPageVehicles.UseClick(Sender: TObject);
begin
  UseVehicle;
end;

procedure TfrmPageVehicles.SaveClick(Sender: TObject);
begin
  if SaveDetails(SelectedVehicle) then
    FHost.Notify(tkSuccess, Tr('personal.vehicles.savedTitle',
      'Vehicle saved'), VehicleCaption(SelectedVehicle));
end;

procedure TfrmPageVehicles.DeleteClick(Sender: TObject);
begin
  DeleteVehicle;
end;

procedure TfrmPageVehicles.NoteClick(Sender: TObject);
begin
  AddNote;
end;

procedure TfrmPageVehicles.ExportClick(Sender: TObject);
begin
  ExportHistory;
end;

procedure TfrmPageVehicles.ClearClick(Sender: TObject);
begin
  ClearHistory;
end;

{ IPersonalCommands --------------------------------------------------------- }

function TfrmPageVehicles.Commands: TPersonalCommands;
begin
  Result := [pcAddVehicle, pcUseVehicle, pcDeleteVehicle, pcAddNote,
    pcExportHistory];
end;

function TfrmPageVehicles.CommandEnabled(ACommand: TPersonalCommand): Boolean;
var
  Vehicle: TPersonalVehicle;
begin
  if FHost = nil then
    Exit(False);
  Vehicle := SelectedVehicle;
  case ACommand of
    pcAddVehicle:
      Result := True;
    pcUseVehicle:
      Result := (Vehicle <> nil) and (Vehicle <> FHost.CurrentVehicle);
    pcDeleteVehicle, pcAddNote:
      Result := Vehicle <> nil;
    pcExportHistory:
      Result := (Vehicle <> nil) and (Vehicle.History.Count > 0);
  else
    Result := False;
  end;
end;

function TfrmPageVehicles.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPageVehicles.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcAddVehicle:
      AddVehicle;
    pcUseVehicle:
      UseVehicle;
    pcDeleteVehicle:
      DeleteVehicle;
    pcAddNote:
      AddNote;
    pcExportHistory:
      ExportHistory;
  end;
end;

end.
