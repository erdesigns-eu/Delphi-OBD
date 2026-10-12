//------------------------------------------------------------------------------
//  untPageSettings
//
//  Settings page of OBD-Studio Personal.
//------------------------------------------------------------------------------

unit untPageSettings;

interface

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
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
  ERD.UI.ScrollBox,
  ERD.UI.FileDialogs,
  untSettings,
  untPersonalPage;

type
  TfrmPageSettings = class(TFrame, IPersonalPage)
  strict private
    FHost: IPersonalHost;
    FScroll: TOBDScrollBox;
    FTitle: TOBDDialogText;
    FSubtitle: TOBDDialogText;
    FLanguageCard: TOBDCard;
    FAppearanceCard: TOBDCard;
    FUnitsCard: TOBDCard;
    FInspectionCard: TOBDCard;
    FDataCard: TOBDCard;
    FPrivacyCard: TOBDCard;
    FAboutCard: TOBDCard;
    FLanguageLabel: TOBDDialogText;
    FLanguageNote: TOBDDialogText;
    FLanguageCombo: TOBDComboBox;
    FThemeLabel: TOBDDialogText;
    FThemeCombo: TOBDComboBox;
    FDensityLabel: TOBDDialogText;
    FDensityCombo: TOBDComboBox;
    FUnitsLabel: TOBDDialogText;
    FUnitsCombo: TOBDComboBox;
    FFuelPriceLabel: TOBDDialogText;
    FFuelPriceEdit: TOBDEdit;
    FCurrencyLabel: TOBDDialogText;
    FCurrencyEdit: TOBDEdit;
    FCountryLabel: TOBDDialogText;
    FCountryCombo: TOBDComboBox;
    FDataFolderLabel: TOBDDialogText;
    FDataFolderEdit: TOBDEdit;
    FChangeFolderButton: TOBDButton;
    FOpenFolderButton: TOBDButton;
    FAutoConnectCheck: TOBDCheckBox;
    FPrivacyText: TOBDDialogText;
    FSettingsFileLabel: TOBDDialogText;
    FShowLicenceButton: TOBDButton;
    FLanguageCodes: TArray<string>;
    FCountryCodes: TArray<string>;
    FLoading: Boolean;
    procedure BuildControls;
    function NewText(AParent: TWinControl; const ACaption: string;
      AStyle: TOBDDialogTextStyle): TOBDDialogText;
    function NewCombo(AParent: TWinControl): TOBDComboBox;
    function NewEdit(AParent: TWinControl): TOBDEdit;
    function NewButton(AParent: TWinControl; AKind: TOBDButtonKind):
      TOBDButton;
    procedure LayoutControls;
    procedure FillLanguages;
    procedure FillCountries;
    procedure ApplySettingsToControls;
    procedure SettingsChanged;
    procedure LanguageChanged(Sender: TObject);
    procedure ThemeChanged(Sender: TObject);
    procedure DensityChanged(Sender: TObject);
    procedure UnitsComboChanged(Sender: TObject);
    procedure FuelPriceChanged(Sender: TObject);
    procedure CurrencyChanged(Sender: TObject);
    procedure CountryChanged(Sender: TObject);
    procedure AutoConnectChanged(Sender: TObject);
    procedure ChangeFolderClick(Sender: TObject);
    procedure OpenFolderClick(Sender: TObject);
    procedure ShowLicenceClick(Sender: TObject);
    function SelectedLanguageCode: string;
    function SelectedCountryCode: string;
    function EulaFileName: string;
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
  end;

implementation

{$R *.dfm}

{ TfrmPageSettings }

constructor TfrmPageSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentBackground := False;
  BuildControls;
  Translate;
end;

function TfrmPageSettings.NewText(AParent: TWinControl; const ACaption: string;
  AStyle: TOBDDialogTextStyle): TOBDDialogText;
begin
  Result := TOBDDialogText.Create(Self);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.Style := AStyle;
  Result.WordWrap := True;
end;

function TfrmPageSettings.NewCombo(AParent: TWinControl): TOBDComboBox;
begin
  Result := TOBDComboBox.Create(Self);
  Result.Parent := AParent;
  Result.DropDownCount := 12;
end;

function TfrmPageSettings.NewEdit(AParent: TWinControl): TOBDEdit;
begin
  Result := TOBDEdit.Create(Self);
  Result.Parent := AParent;
end;

function TfrmPageSettings.NewButton(AParent: TWinControl;
  AKind: TOBDButtonKind): TOBDButton;
begin
  Result := TOBDButton.Create(Self);
  Result.Parent := AParent;
  Result.Kind := AKind;
end;

procedure TfrmPageSettings.BuildControls;
begin
  FScroll := TOBDScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.Padding.SetBounds(24, 18, 24, 24);

  FTitle := NewText(FScroll, '', dtsHeading);
  FSubtitle := NewText(FScroll, '', dtsSubtle);

  FLanguageCard := TOBDCard.Create(Self);
  FLanguageCard.Parent := FScroll;
  FLanguageCard.StatusEdge := ceAccent;
  FLanguageLabel := NewText(FLanguageCard, '', dtsCaps);
  FLanguageCombo := NewCombo(FLanguageCard);
  FLanguageCombo.OnChange := LanguageChanged;
  FLanguageNote := NewText(FLanguageCard, '', dtsSmall);

  FAppearanceCard := TOBDCard.Create(Self);
  FAppearanceCard.Parent := FScroll;
  FThemeLabel := NewText(FAppearanceCard, '', dtsCaps);
  FThemeCombo := NewCombo(FAppearanceCard);
  FThemeCombo.OnChange := ThemeChanged;
  FDensityLabel := NewText(FAppearanceCard, '', dtsCaps);
  FDensityCombo := NewCombo(FAppearanceCard);
  FDensityCombo.OnChange := DensityChanged;

  FUnitsCard := TOBDCard.Create(Self);
  FUnitsCard.Parent := FScroll;
  FUnitsLabel := NewText(FUnitsCard, '', dtsCaps);
  FUnitsCombo := NewCombo(FUnitsCard);
  FUnitsCombo.OnChange := UnitsComboChanged;
  FFuelPriceLabel := NewText(FUnitsCard, '', dtsCaps);
  FFuelPriceEdit := NewEdit(FUnitsCard);
  FFuelPriceEdit.OnChange := FuelPriceChanged;
  FCurrencyLabel := NewText(FUnitsCard, '', dtsCaps);
  FCurrencyEdit := NewEdit(FUnitsCard);
  FCurrencyEdit.MaxLength := 8;
  FCurrencyEdit.CharCase := ecUpperCase;
  FCurrencyEdit.OnChange := CurrencyChanged;

  FInspectionCard := TOBDCard.Create(Self);
  FInspectionCard.Parent := FScroll;
  FCountryLabel := NewText(FInspectionCard, '', dtsCaps);
  FCountryCombo := NewCombo(FInspectionCard);
  FCountryCombo.OnChange := CountryChanged;

  FDataCard := TOBDCard.Create(Self);
  FDataCard.Parent := FScroll;
  FDataFolderLabel := NewText(FDataCard, '', dtsCaps);
  FDataFolderEdit := NewEdit(FDataCard);
  FDataFolderEdit.ReadOnly := True;
  FChangeFolderButton := NewButton(FDataCard, bkSecondary);
  FChangeFolderButton.OnClick := ChangeFolderClick;
  FOpenFolderButton := NewButton(FDataCard, bkSecondary);
  FOpenFolderButton.OnClick := OpenFolderClick;
  FAutoConnectCheck := TOBDCheckBox.Create(Self);
  FAutoConnectCheck.Parent := FDataCard;
  FAutoConnectCheck.Style := csSwitch;
  FAutoConnectCheck.OnChange := AutoConnectChanged;

  FPrivacyCard := TOBDCard.Create(Self);
  FPrivacyCard.Parent := FScroll;
  FPrivacyCard.StatusEdge := ceSuccess;
  FPrivacyText := NewText(FPrivacyCard, '', dtsBody);
  FSettingsFileLabel := NewText(FPrivacyCard, '', dtsSmall);

  FAboutCard := TOBDCard.Create(Self);
  FAboutCard.Parent := FScroll;
  FShowLicenceButton := NewButton(FAboutCard, bkSecondary);
  FShowLicenceButton.OnClick := ShowLicenceClick;
end;

procedure TfrmPageSettings.Resize;
begin
  inherited Resize;
  LayoutControls;
end;

procedure TfrmPageSettings.LayoutControls;
var
  ContentW, ColW, Gap, LeftX, RightX, Y, RightY, RowW: Integer;
  TwoColumns: Boolean;
begin
  FScroll.SetBounds(0, 0, ClientWidth, ClientHeight);
  ContentW := ClientWidth - 48;
  if ContentW < 320 then
    ContentW := 320;
  Gap := 16;
  TwoColumns := ContentW >= 860;
  if TwoColumns then
    ColW := (ContentW - Gap) div 2
  else
    ColW := ContentW;

  LeftX := 24;
  RightX := LeftX + ColW + Gap;
  FTitle.SetBounds(LeftX, 18, ContentW, 28);
  FSubtitle.SetBounds(LeftX, 46, ContentW, 34);

  Y := 96;
  FLanguageCard.SetBounds(LeftX, Y, ColW, 170);
  Inc(Y, FLanguageCard.Height + Gap);
  FAppearanceCard.SetBounds(LeftX, Y, ColW, 190);
  Inc(Y, FAppearanceCard.Height + Gap);
  FUnitsCard.SetBounds(LeftX, Y, ColW, 250);

  if TwoColumns then
    RightY := 96
  else
  begin
    RightX := LeftX;
    RightY := FUnitsCard.Top + FUnitsCard.Height + Gap;
  end;
  FInspectionCard.SetBounds(RightX, RightY, ColW, 138);
  Inc(RightY, FInspectionCard.Height + Gap);
  FDataCard.SetBounds(RightX, RightY, ColW, 226);
  Inc(RightY, FDataCard.Height + Gap);
  FPrivacyCard.SetBounds(RightX, RightY, ColW, 210);
  Inc(RightY, FPrivacyCard.Height + Gap);
  FAboutCard.SetBounds(RightX, RightY, ColW, 118);

  RowW := ColW - 32;
  FLanguageLabel.SetBounds(16, 64, RowW, 18);
  FLanguageCombo.SetBounds(16, 84, RowW, 30);
  FLanguageNote.SetBounds(16, 122, RowW, 38);

  FThemeLabel.SetBounds(16, 64, RowW, 18);
  FThemeCombo.SetBounds(16, 84, RowW, 30);
  FDensityLabel.SetBounds(16, 122, RowW, 18);
  FDensityCombo.SetBounds(16, 142, RowW, 30);

  FUnitsLabel.SetBounds(16, 64, RowW, 18);
  FUnitsCombo.SetBounds(16, 84, RowW, 30);
  FFuelPriceLabel.SetBounds(16, 126, (RowW - Gap) div 2, 18);
  FFuelPriceEdit.SetBounds(16, 146, (RowW - Gap) div 2, 30);
  FCurrencyLabel.SetBounds(24 + (RowW - Gap) div 2, 126,
    (RowW - Gap) div 2, 18);
  FCurrencyEdit.SetBounds(24 + (RowW - Gap) div 2, 146,
    (RowW - Gap) div 2, 30);

  FCountryLabel.SetBounds(16, 64, RowW, 18);
  FCountryCombo.SetBounds(16, 84, RowW, 30);

  FDataFolderLabel.SetBounds(16, 64, RowW, 18);
  FDataFolderEdit.SetBounds(16, 84, RowW, 30);
  FChangeFolderButton.SetBounds(16, 126, 118, 30);
  FOpenFolderButton.SetBounds(144, 126, 118, 30);
  FAutoConnectCheck.SetBounds(16, 172, RowW, 30);

  FPrivacyText.SetBounds(16, 64, RowW, 78);
  FSettingsFileLabel.SetBounds(16, 146, RowW, 50);
  FShowLicenceButton.SetBounds(16, 66, 132, 30);
end;

function TfrmPageSettings.PageKey: string;
begin
  Result := PAGE_SETTINGS;
end;

procedure TfrmPageSettings.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  FillCountries;
  ApplySettingsToControls;
end;

procedure TfrmPageSettings.Activated(AActive: Boolean);
begin
  if AActive then
  begin
    FillCountries;
    ApplySettingsToControls;
  end;
end;

procedure TfrmPageSettings.ConnectionChanged(AConnected: Boolean);
begin
end;

procedure TfrmPageSettings.UnitsChanged;
begin
  ApplySettingsToControls;
end;

procedure TfrmPageSettings.Translate;
begin
  FTitle.Caption := Tr('personal.settings.title', 'Settings');
  FSubtitle.Caption := Tr('personal.settings.subtitle',
    'Saved for this Windows user.');
  FLanguageCard.Title := Tr('personal.settings.language.title', 'Language');
  FLanguageLabel.Caption := Tr('personal.settings.language.label',
    'Display language');
  FLanguageNote.Caption := Tr('personal.settings.language.note',
    'Draft catalogues are labelled as machine translation until reviewed.');
  FAppearanceCard.Title := Tr('personal.settings.appearance.title',
    'Appearance');
  FThemeLabel.Caption := Tr('personal.settings.theme.label', 'Theme');
  FDensityLabel.Caption := Tr('personal.settings.density.label', 'Density');
  FUnitsCard.Title := Tr('personal.settings.units.title', 'Units and fuel');
  FUnitsLabel.Caption := Tr('personal.settings.units.label', 'Units');
  FFuelPriceLabel.Caption := Tr('personal.settings.fuelPrice.label',
    'Fuel price');
  FCurrencyLabel.Caption := Tr('personal.settings.currency.label',
    'Currency');
  FInspectionCard.Title := Tr('personal.settings.inspection.title',
    'Inspection');
  FCountryLabel.Caption := Tr('personal.settings.country.label', 'Country');
  FDataCard.Title := Tr('personal.settings.data.title', 'Data folder');
  FDataFolderLabel.Caption := Tr('personal.settings.data.folder',
    'Folder');
  FChangeFolderButton.Caption := Tr('personal.settings.data.change',
    'Change...');
  FOpenFolderButton.Caption := Tr('personal.settings.data.open',
    'Open folder');
  FAutoConnectCheck.Caption := Tr('personal.settings.autoconnect',
    'Connect automatically on start');
  FPrivacyCard.Title := Tr('personal.settings.privacy.title', 'Privacy');
  FPrivacyText.Caption := Tr('personal.settings.privacy.text',
    'Local first: no account, no telemetry and no automatic uploads. ' +
    'Vehicles, logs, recordings and settings stay on this PC.');
  FAboutCard.Title := Tr('personal.settings.about.title', 'Licence');
  FShowLicenceButton.Caption := Tr('personal.settings.showLicence',
    'Show licence');

  FillLanguages;
  FThemeCombo.Items.Text := Tr('personal.settings.theme.light', 'Light') +
    sLineBreak + Tr('personal.settings.theme.dark', 'Dark') + sLineBreak +
    Tr('personal.settings.theme.windows', 'Follow Windows');
  FDensityCombo.Items.Text := Tr('personal.settings.density.desktop',
    'Desktop') + sLineBreak + Tr('personal.settings.density.tablet',
    'Tablet / touch');
  FUnitsCombo.Items.Text := Tr('personal.settings.units.metric',
    'Metric (km, °C, kPa, litres)') + sLineBreak +
    Tr('personal.settings.units.imperial',
    'Imperial (miles, °F, psi, gallons)');
  ApplySettingsToControls;
  LayoutControls;
end;

procedure TfrmPageSettings.FillLanguages;
var
  Infos: TArray<TOBDLanguageInfo>;
  Info: TOBDLanguageInfo;
  Caption, Current: string;
  Index, Selected: Integer;
begin
  Current := '';
  if FHost <> nil then
    Current := FHost.Settings.Language;
  FLoading := True;
  try
    FLanguageCombo.Items.BeginUpdate;
    try
      FLanguageCombo.Items.Clear;
      SetLength(FLanguageCodes, 1);
      FLanguageCodes[0] := '';
      FLanguageCombo.Items.Add(Tr('personal.settings.language.windows',
        'Windows default'));
      Selected := 0;
      if FHost <> nil then
        Infos := OBDI18n.AvailableLanguages(FHost.LanguageFolder)
      else
        Infos := nil;
      for Info in Infos do
      begin
        Caption := Info.Name;
        if (Info.EnglishName <> '') and not SameText(Info.EnglishName,
          Info.Name) then
          Caption := Caption + ' · ' + Info.EnglishName;
        if SameText(Info.Status, 'draft') then
          Caption := Caption + ' (' +
            Tr('personal.settings.language.machine',
            'machine translation') + ')';
        Index := Length(FLanguageCodes);
        SetLength(FLanguageCodes, Index + 1);
        FLanguageCodes[Index] := Info.Code;
        FLanguageCombo.Items.Add(Caption);
        if SameText(Current, Info.Code) then
          Selected := Index;
      end;
      FLanguageCombo.ItemIndex := Selected;
    finally
      FLanguageCombo.Items.EndUpdate;
    end;
  finally
    FLoading := False;
  end;
end;

procedure TfrmPageSettings.FillCountries;
var
  Codes: TArray<string>;
  Code, Current: string;
  I, Selected: Integer;
  Found: Boolean;
begin
  Current := '';
  if FHost <> nil then
    Current := FHost.Settings.InspectionCountry;
  if FHost <> nil then
    Codes := FHost.Session.Inspection.CountryCodes
  else
    Codes := nil;
  Found := Current = '';
  for Code in Codes do
    if SameText(Code, Current) then
      Found := True;
  if not Found then
  begin
    I := Length(Codes);
    SetLength(Codes, I + 1);
    Codes[I] := Current;
  end;

  FLoading := True;
  try
    FCountryCombo.Items.BeginUpdate;
    try
      FCountryCombo.Items.Clear;
      SetLength(FCountryCodes, Length(Codes));
      Selected := -1;
      for I := 0 to High(Codes) do
      begin
        FCountryCodes[I] := UpperCase(Codes[I]);
        FCountryCombo.Items.Add(FCountryCodes[I]);
        if SameText(FCountryCodes[I], Current) then
          Selected := I;
      end;
      FCountryCombo.ItemIndex := Selected;
    finally
      FCountryCombo.Items.EndUpdate;
    end;
  finally
    FLoading := False;
  end;
end;

procedure TfrmPageSettings.ApplySettingsToControls;
var
  Settings: TPersonalSettings;
begin
  if FHost = nil then
    Exit;
  Settings := FHost.Settings;
  FLoading := True;
  try
    case Settings.ThemeMode of
      tmLight:
        FThemeCombo.ItemIndex := 0;
      tmDark:
        FThemeCombo.ItemIndex := 1;
    else
      FThemeCombo.ItemIndex := 2;
    end;
    if Settings.Density = dnTablet then
      FDensityCombo.ItemIndex := 1
    else
      FDensityCombo.ItemIndex := 0;
    if Settings.UnitSystem = usImperial then
      FUnitsCombo.ItemIndex := 1
    else
      FUnitsCombo.ItemIndex := 0;
    FFuelPriceEdit.Text := FormatFloat('0.00', Settings.FuelPrice);
    FCurrencyEdit.Text := Settings.Currency;
    FDataFolderEdit.Text := Settings.DataFolder;
    FAutoConnectCheck.Checked := Settings.AutoConnect;
    FSettingsFileLabel.Caption := TrFmt('personal.settings.privacy.file',
      'Settings file: %s', [Settings.FileName]);
  finally
    FLoading := False;
  end;
end;

function TfrmPageSettings.SelectedLanguageCode: string;
begin
  if (FLanguageCombo.ItemIndex >= 0) and
    (FLanguageCombo.ItemIndex <= High(FLanguageCodes)) then
    Result := FLanguageCodes[FLanguageCombo.ItemIndex]
  else
    Result := '';
end;

function TfrmPageSettings.SelectedCountryCode: string;
begin
  if (FCountryCombo.ItemIndex >= 0) and
    (FCountryCombo.ItemIndex <= High(FCountryCodes)) then
    Result := FCountryCodes[FCountryCombo.ItemIndex]
  else
    Result := '';
end;

procedure TfrmPageSettings.SettingsChanged;
begin
  if (FHost <> nil) and not FLoading then
    FHost.SettingsChanged;
end;

procedure TfrmPageSettings.LanguageChanged(Sender: TObject);
begin
  if (FHost = nil) or FLoading then
    Exit;
  FHost.Settings.Language := SelectedLanguageCode;
  SettingsChanged;
end;

procedure TfrmPageSettings.ThemeChanged(Sender: TObject);
begin
  if (FHost = nil) or FLoading then
    Exit;
  case FThemeCombo.ItemIndex of
    0:
      FHost.Settings.ThemeMode := tmLight;
    1:
      FHost.Settings.ThemeMode := tmDark;
  else
    FHost.Settings.ThemeMode := tmWindows;
  end;
  SettingsChanged;
end;

procedure TfrmPageSettings.DensityChanged(Sender: TObject);
begin
  if (FHost = nil) or FLoading then
    Exit;
  if FDensityCombo.ItemIndex = 1 then
    FHost.Settings.Density := dnTablet
  else
    FHost.Settings.Density := dnDesktop;
  SettingsChanged;
end;

procedure TfrmPageSettings.UnitsComboChanged(Sender: TObject);
begin
  if (FHost = nil) or FLoading then
    Exit;
  if FUnitsCombo.ItemIndex = 1 then
    FHost.Settings.UnitSystem := usImperial
  else
    FHost.Settings.UnitSystem := usMetric;
  SettingsChanged;
end;

procedure TfrmPageSettings.FuelPriceChanged(Sender: TObject);
var
  Value: Double;
  FormatSettings: TFormatSettings;
begin
  if (FHost = nil) or FLoading then
    Exit;
  FormatSettings := TFormatSettings.Current;
  if not TryStrToFloat(Trim(FFuelPriceEdit.Text), Value, FormatSettings) then
    if not TryStrToFloat(Trim(FFuelPriceEdit.Text), Value,
      TFormatSettings.Invariant) then
      Value := FHost.Settings.FuelPrice;
  FHost.Settings.FuelPrice := Value;
  SettingsChanged;
end;

procedure TfrmPageSettings.CurrencyChanged(Sender: TObject);
begin
  if (FHost = nil) or FLoading then
    Exit;
  FHost.Settings.Currency := Trim(FCurrencyEdit.Text);
  SettingsChanged;
end;

procedure TfrmPageSettings.CountryChanged(Sender: TObject);
begin
  if (FHost = nil) or FLoading then
    Exit;
  FHost.Settings.InspectionCountry := SelectedCountryCode;
  SettingsChanged;
end;

procedure TfrmPageSettings.AutoConnectChanged(Sender: TObject);
begin
  if (FHost = nil) or FLoading then
    Exit;
  FHost.Settings.AutoConnect := FAutoConnectCheck.Checked;
  SettingsChanged;
end;

procedure TfrmPageSettings.ChangeFolderClick(Sender: TObject);
var
  Dialog: TOBDFolderDialog;
begin
  if FHost = nil then
    Exit;
  Dialog := TOBDFolderDialog.Create(Self);
  try
    Dialog.Title := Tr('personal.settings.data.choose',
      'Choose data folder');
    Dialog.InitialDir := FHost.Settings.DataFolder;
    Dialog.Directory := FHost.Settings.DataFolder;
    if Dialog.Execute then
    begin
      FHost.Settings.DataFolder := Dialog.Directory;
      FDataFolderEdit.Text := FHost.Settings.DataFolder;
      SettingsChanged;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrmPageSettings.OpenFolderClick(Sender: TObject);
var
  Folder: string;
begin
  if FHost = nil then
    Exit;
  Folder := FHost.Settings.DataFolder;
  ForceDirectories(Folder);
  ShellExecute(Handle, 'open', PChar(Folder), nil, nil, SW_SHOWNORMAL);
end;

function TfrmPageSettings.EulaFileName: string;
var
  Candidate: string;
begin
  Candidate := TPath.Combine(ExtractFilePath(ParamStr(0)), 'EULA.md');
  if TFile.Exists(Candidate) then
    Exit(Candidate);
  Candidate := TPath.Combine(TDirectory.GetCurrentDirectory,
    TPath.Combine('applications', TPath.Combine('obd-studio-personal',
    'EULA.md')));
  Result := Candidate;
end;

procedure TfrmPageSettings.ShowLicenceClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := EulaFileName;
  if TFile.Exists(FileName) then
    ShellExecute(Handle, 'open', PChar(FileName), nil, nil, SW_SHOWNORMAL)
  else if FHost <> nil then
    FHost.Notify(tkWarning, Tr('personal.settings.licenceMissing',
      'Licence not found'), FileName);
end;

end.
