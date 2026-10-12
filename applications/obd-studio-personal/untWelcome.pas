//------------------------------------------------------------------------------
//  untWelcome
//
//  First-start wizard for OBD-Studio Personal.
//------------------------------------------------------------------------------

unit untWelcome;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  ERD.I18n,
  ERD.Service.Inspection,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Theme,
  ERD.UI.Dialogs,
  ERD.UI.Card,
  ERD.UI.Chips,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.Memo,
  untSettings;

type
  TfrmWelcome = class(TForm)
  strict private
    FSettings: TPersonalSettings;
    FLanguageFolder: string;
    FStep: Integer;
    FLoading: Boolean;
    FLanguageCodes: TArray<string>;
    FCountryCodes: TArray<string>;
    FMainCard: TOBDCard;
    FRailCard: TOBDCard;
    FBrandTitle: TOBDDialogText;
    FBrandSub: TOBDDialogText;
    FStepLabels: array[0..3] of TOBDDialogText;
    FStepSubs: array[0..3] of TOBDDialogText;
    FStepChips: array[0..3] of TOBDChip;
    FStepTitle: TOBDDialogText;
    FStepIntro: TOBDDialogText;
    FLanguageLabel: TOBDDialogText;
    FLanguageCombo: TOBDComboBox;
    FLanguageNote: TOBDDialogText;
    FLicenceMemo: TOBDMemo;
    FAcceptCheck: TOBDCheckBox;
    FLicenceNote: TOBDDialogText;
    FDemoRadio: TOBDRadioButton;
    FSerialRadio: TOBDRadioButton;
    FWiFiRadio: TOBDRadioButton;
    FBleRadio: TOBDRadioButton;
    FPortLabel: TOBDDialogText;
    FPortEdit: TOBDEdit;
    FWiFiHostLabel: TOBDDialogText;
    FWiFiHostEdit: TOBDEdit;
    FWiFiPortLabel: TOBDDialogText;
    FWiFiPortEdit: TOBDEdit;
    FBleLabel: TOBDDialogText;
    FBleEdit: TOBDEdit;
    FUnitsLabel: TOBDDialogText;
    FUnitsCombo: TOBDComboBox;
    FCountryLabel: TOBDDialogText;
    FCountryCombo: TOBDComboBox;
    FDoneText: TOBDDialogText;
    FDeclineButton: TOBDButton;
    FBackButton: TOBDButton;
    FNextButton: TOBDButton;
    procedure BuildControls;
    function NewText(AParent: TWinControl; const ACaption: string;
      AStyle: TOBDDialogTextStyle): TOBDDialogText;
    function NewCombo(AParent: TWinControl): TOBDComboBox;
    function NewEdit(AParent: TWinControl): TOBDEdit;
    procedure LayoutControls;
    procedure ApplyTheme;
    procedure LoadWizard;
    procedure FillLanguages;
    procedure FillCountries;
    procedure LoadLicence;
    procedure UpdateStep;
    procedure UpdateButtons;
    procedure UpdateConnectionFields;
    procedure SetStepVisible(AIndex: Integer; AVisible: Boolean);
    procedure LanguageChanged(Sender: TObject);
    procedure AcceptChanged(Sender: TObject);
    procedure ConnectionKindChanged(Sender: TObject);
    procedure ConnectionEditChanged(Sender: TObject);
    procedure UnitsChanged(Sender: TObject);
    procedure CountryChanged(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure DeclineClick(Sender: TObject);
    function SelectedLanguageCode: string;
    function SelectedCountryCode: string;
    function EulaFileName: string;
    procedure ApplyLanguage;
    procedure Translate;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function Execute(AOwner: TComponent; ASettings: TPersonalSettings;
      const ALanguageFolder: string): Boolean; static;
  end;

implementation

{$R *.dfm}

function MergedCatalog(const ALibraryFile, AAppFile, ACode: string): string;
var
  Value, StringsValue, AppStringsValue: TJSONValue;
  Root, Strings, AppStrings: TJSONObject;
  Pair: TJSONPair;
begin
  Root := nil;
  if TFile.Exists(ALibraryFile) then
  begin
    Value := TJSONObject.ParseJSONValue(TFile.ReadAllText(ALibraryFile,
      TEncoding.UTF8));
    if Value is TJSONObject then
      Root := TJSONObject(Value)
    else
      Value.Free;
  end;
  if Root = nil then
  begin
    Root := TJSONObject.Create;
    Root.AddPair('language', ACode);
    Root.AddPair('strings', TJSONObject.Create);
  end;
  try
    if TFile.Exists(AAppFile) then
    begin
      Value := TJSONObject.ParseJSONValue(TFile.ReadAllText(AAppFile,
        TEncoding.UTF8));
      try
        StringsValue := Root.GetValue('strings');
        AppStringsValue := nil;
        if Value is TJSONObject then
          AppStringsValue := TJSONObject(Value).GetValue('strings');
        if (StringsValue is TJSONObject) and
          (AppStringsValue is TJSONObject) then
        begin
          Strings := TJSONObject(StringsValue);
          AppStrings := TJSONObject(AppStringsValue);
          for Pair in AppStrings do
          begin
            Strings.RemovePair(Pair.JsonString.Value).Free;
            Strings.AddPair(Pair.JsonString.Value,
              TJSONValue(Pair.JsonValue.Clone));
          end;
        end;
      finally
        Value.Free;
      end;
    end;
    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

{ TfrmWelcome }

constructor TfrmWelcome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsDialog;
  Position := poOwnerFormCenter;
  Caption := 'OBD-Studio Personal';
  Width := 1040;
  Height := 680;
  FStep := 0;
  BuildControls;
end;

class function TfrmWelcome.Execute(AOwner: TComponent;
  ASettings: TPersonalSettings; const ALanguageFolder: string): Boolean;
var
  Form: TfrmWelcome;
begin
  Form := TfrmWelcome.Create(AOwner);
  try
    Form.FSettings := ASettings;
    Form.FLanguageFolder := ALanguageFolder;
    Form.LoadWizard;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

function TfrmWelcome.NewText(AParent: TWinControl; const ACaption: string;
  AStyle: TOBDDialogTextStyle): TOBDDialogText;
begin
  Result := TOBDDialogText.Create(Self);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.Style := AStyle;
  Result.WordWrap := True;
end;

function TfrmWelcome.NewCombo(AParent: TWinControl): TOBDComboBox;
begin
  Result := TOBDComboBox.Create(Self);
  Result.Parent := AParent;
  Result.DropDownCount := 12;
end;

function TfrmWelcome.NewEdit(AParent: TWinControl): TOBDEdit;
begin
  Result := TOBDEdit.Create(Self);
  Result.Parent := AParent;
end;

procedure TfrmWelcome.BuildControls;
var
  I: Integer;
begin
  FMainCard := TOBDCard.Create(Self);
  FMainCard.Parent := Self;
  FMainCard.ShowHeader := False;
  FMainCard.Padding.SetBounds(0, 0, 0, 0);

  FRailCard := TOBDCard.Create(Self);
  FRailCard.Parent := FMainCard;
  FRailCard.ShowHeader := False;
  FRailCard.StatusEdge := ceAccent;

  FBrandTitle := NewText(FRailCard, 'OBD-Studio', dtsHeading);
  FBrandSub := NewText(FRailCard, 'Personal', dtsSubtle);
  for I := 0 to 3 do
  begin
    FStepChips[I] := TOBDChip.Create(Self);
    FStepChips[I].Parent := FRailCard;
    FStepChips[I].Filled := True;
    FStepChips[I].Caption := IntToStr(I + 1);
    FStepLabels[I] := NewText(FRailCard, '', dtsBody);
    FStepSubs[I] := NewText(FRailCard, '', dtsSmall);
  end;

  FStepTitle := NewText(FMainCard, '', dtsHeading);
  FStepIntro := NewText(FMainCard, '', dtsSubtle);

  FLanguageLabel := NewText(FMainCard, '', dtsCaps);
  FLanguageCombo := NewCombo(FMainCard);
  FLanguageCombo.OnChange := LanguageChanged;
  FLanguageNote := NewText(FMainCard, '', dtsBody);

  FLicenceMemo := TOBDMemo.Create(Self);
  FLicenceMemo.Parent := FMainCard;
  FLicenceMemo.ReadOnly := True;
  FLicenceMemo.WordWrap := True;
  FAcceptCheck := TOBDCheckBox.Create(Self);
  FAcceptCheck.Parent := FMainCard;
  FAcceptCheck.OnChange := AcceptChanged;
  FLicenceNote := NewText(FMainCard, '', dtsSmall);

  FDemoRadio := TOBDRadioButton.Create(Self);
  FDemoRadio.Parent := FMainCard;
  FDemoRadio.GroupIndex := 1;
  FDemoRadio.OnChange := ConnectionKindChanged;
  FSerialRadio := TOBDRadioButton.Create(Self);
  FSerialRadio.Parent := FMainCard;
  FSerialRadio.GroupIndex := 1;
  FSerialRadio.OnChange := ConnectionKindChanged;
  FWiFiRadio := TOBDRadioButton.Create(Self);
  FWiFiRadio.Parent := FMainCard;
  FWiFiRadio.GroupIndex := 1;
  FWiFiRadio.OnChange := ConnectionKindChanged;
  FBleRadio := TOBDRadioButton.Create(Self);
  FBleRadio.Parent := FMainCard;
  FBleRadio.GroupIndex := 1;
  FBleRadio.OnChange := ConnectionKindChanged;

  FPortLabel := NewText(FMainCard, '', dtsCaps);
  FPortEdit := NewEdit(FMainCard);
  FPortEdit.OnChange := ConnectionEditChanged;
  FWiFiHostLabel := NewText(FMainCard, '', dtsCaps);
  FWiFiHostEdit := NewEdit(FMainCard);
  FWiFiHostEdit.OnChange := ConnectionEditChanged;
  FWiFiPortLabel := NewText(FMainCard, '', dtsCaps);
  FWiFiPortEdit := NewEdit(FMainCard);
  FWiFiPortEdit.NumbersOnly := True;
  FWiFiPortEdit.OnChange := ConnectionEditChanged;
  FBleLabel := NewText(FMainCard, '', dtsCaps);
  FBleEdit := NewEdit(FMainCard);
  FBleEdit.OnChange := ConnectionEditChanged;
  FUnitsLabel := NewText(FMainCard, '', dtsCaps);
  FUnitsCombo := NewCombo(FMainCard);
  FUnitsCombo.OnChange := UnitsChanged;
  FCountryLabel := NewText(FMainCard, '', dtsCaps);
  FCountryCombo := NewCombo(FMainCard);
  FCountryCombo.OnChange := CountryChanged;

  FDoneText := NewText(FMainCard, '', dtsBody);

  FDeclineButton := TOBDButton.Create(Self);
  FDeclineButton.Parent := FMainCard;
  FDeclineButton.Kind := bkGhost;
  FDeclineButton.OnClick := DeclineClick;
  FBackButton := TOBDButton.Create(Self);
  FBackButton.Parent := FMainCard;
  FBackButton.Kind := bkSecondary;
  FBackButton.OnClick := BackClick;
  FNextButton := TOBDButton.Create(Self);
  FNextButton.Parent := FMainCard;
  FNextButton.Kind := bkPrimary;
  FNextButton.Default := True;
  FNextButton.OnClick := NextClick;
end;

procedure TfrmWelcome.Resize;
begin
  inherited Resize;
  LayoutControls;
end;

procedure TfrmWelcome.LayoutControls;
var
  CardW, CardH, RailW, BodyX, BodyW, FooterY, Y, RowW, HalfW, Gap: Integer;
  I: Integer;
begin
  CardW := ClientWidth - 64;
  CardH := ClientHeight - 64;
  if CardW < 760 then
    CardW := 760;
  if CardH < 540 then
    CardH := 540;
  FMainCard.SetBounds((ClientWidth - CardW) div 2, (ClientHeight - CardH) div 2,
    CardW, CardH);
  RailW := 260;
  FRailCard.SetBounds(1, 1, RailW, CardH - 2);
  BodyX := RailW + 32;
  BodyW := CardW - BodyX - 32;
  FooterY := CardH - 70;
  Gap := 16;

  FBrandTitle.SetBounds(24, 28, RailW - 48, 28);
  FBrandSub.SetBounds(24, 58, RailW - 48, 24);
  Y := 118;
  for I := 0 to 3 do
  begin
    FStepChips[I].SetBounds(24, Y + 5, 28, 24);
    FStepLabels[I].SetBounds(64, Y, RailW - 88, 24);
    FStepSubs[I].SetBounds(64, Y + 24, RailW - 88, 36);
    Inc(Y, 76);
  end;

  FStepTitle.SetBounds(BodyX, 36, BodyW, 34);
  FStepIntro.SetBounds(BodyX, 72, BodyW, 54);
  RowW := BodyW;
  Y := 148;

  FLanguageLabel.SetBounds(BodyX, Y, RowW, 18);
  FLanguageCombo.SetBounds(BodyX, Y + 22, RowW, 30);
  FLanguageNote.SetBounds(BodyX, Y + 68, RowW, 90);

  FLicenceMemo.SetBounds(BodyX, Y, RowW, FooterY - Y - 82);
  FAcceptCheck.SetBounds(BodyX, FooterY - 72, RowW, 30);
  FLicenceNote.SetBounds(BodyX, FooterY - 34, RowW, 30);

  FDemoRadio.SetBounds(BodyX, Y, RowW, 28);
  FSerialRadio.SetBounds(BodyX, Y + 34, RowW, 28);
  FWiFiRadio.SetBounds(BodyX, Y + 68, RowW, 28);
  FBleRadio.SetBounds(BodyX, Y + 102, RowW, 28);
  Y := Y + 148;
  HalfW := (RowW - Gap) div 2;
  FPortLabel.SetBounds(BodyX, Y, HalfW, 18);
  FPortEdit.SetBounds(BodyX, Y + 22, HalfW, 30);
  FBleLabel.SetBounds(BodyX + HalfW + Gap, Y, HalfW, 18);
  FBleEdit.SetBounds(BodyX + HalfW + Gap, Y + 22, HalfW, 30);
  FWiFiHostLabel.SetBounds(BodyX, Y + 70, HalfW, 18);
  FWiFiHostEdit.SetBounds(BodyX, Y + 92, HalfW, 30);
  FWiFiPortLabel.SetBounds(BodyX + HalfW + Gap, Y + 70, HalfW, 18);
  FWiFiPortEdit.SetBounds(BodyX + HalfW + Gap, Y + 92, HalfW, 30);
  FUnitsLabel.SetBounds(BodyX, Y + 142, HalfW, 18);
  FUnitsCombo.SetBounds(BodyX, Y + 164, HalfW, 30);
  FCountryLabel.SetBounds(BodyX + HalfW + Gap, Y + 142, HalfW, 18);
  FCountryCombo.SetBounds(BodyX + HalfW + Gap, Y + 164, HalfW, 30);

  FDoneText.SetBounds(BodyX, 150, RowW, 220);

  FDeclineButton.SetBounds(BodyX, FooterY + 20, 148, 30);
  FNextButton.SetBounds(CardW - 32 - 154, FooterY + 20, 154, 30);
  FBackButton.SetBounds(FNextButton.Left - 96, FooterY + 20, 88, 30);
end;

procedure TfrmWelcome.ApplyTheme;
var
  Theme: TOBDTheme;
begin
  Theme := TOBDTheme.GetDefault;
  if Theme <> nil then
    Color := Theme.Palette.Background;
end;

procedure TfrmWelcome.LoadWizard;
begin
  ApplyTheme;
  FillLanguages;
  FillCountries;
  LoadLicence;
  ApplyLanguage;
  FLoading := True;
  try
    if FSettings.ConnectionKind = pckDemo then
      FDemoRadio.Checked := True
    else if FSettings.ConnectionKind = pckWiFi then
      FWiFiRadio.Checked := True
    else if FSettings.ConnectionKind = pckBLE then
      FBleRadio.Checked := True
    else
      FSerialRadio.Checked := True;
    FPortEdit.Text := FSettings.SerialPort;
    FWiFiHostEdit.Text := FSettings.WiFiHost;
    FWiFiPortEdit.Text := IntToStr(FSettings.WiFiPort);
    FBleEdit.Text := FSettings.BLEAddress;
    if FSettings.UnitSystem = usImperial then
      FUnitsCombo.ItemIndex := 1
    else
      FUnitsCombo.ItemIndex := 0;
  finally
    FLoading := False;
  end;
  Translate;
  UpdateConnectionFields;
  UpdateStep;
end;

procedure TfrmWelcome.FillLanguages;
var
  Infos: TArray<TOBDLanguageInfo>;
  Info: TOBDLanguageInfo;
  Caption: string;
  Index, Selected: Integer;
begin
  FLoading := True;
  try
    FLanguageCombo.Items.BeginUpdate;
    try
      FLanguageCombo.Items.Clear;
      SetLength(FLanguageCodes, 1);
      FLanguageCodes[0] := '';
      FLanguageCombo.Items.Add(Tr('personal.welcome.language.windows',
        'Windows default'));
      Selected := 0;
      Infos := OBDI18n.AvailableLanguages(FLanguageFolder);
      for Info in Infos do
      begin
        Caption := Info.Name;
        if (Info.EnglishName <> '') and not SameText(Info.EnglishName,
          Info.Name) then
          Caption := Caption + ' · ' + Info.EnglishName;
        if SameText(Info.Status, 'draft') then
          Caption := Caption + ' (' + Tr('personal.welcome.language.machine',
            'machine translation') + ')';
        Index := Length(FLanguageCodes);
        SetLength(FLanguageCodes, Index + 1);
        FLanguageCodes[Index] := Info.Code;
        FLanguageCombo.Items.Add(Caption);
        if SameText(FSettings.Language, Info.Code) then
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

procedure TfrmWelcome.FillCountries;
var
  Codes: TArray<string>;
  Code: string;
  I, Selected: Integer;
begin
  try
    Codes := TOBDInspectionCatalog.CountryCodes;
  except
    Codes := nil;
  end;
  if Length(Codes) = 0 then
  begin
    SetLength(Codes, 6);
    Codes[0] := 'NL';
    Codes[1] := 'BE';
    Codes[2] := 'DE';
    Codes[3] := 'FR';
    Codes[4] := 'GB';
    Codes[5] := 'IE';
  end;
  FLoading := True;
  try
    FCountryCombo.Items.BeginUpdate;
    try
      FCountryCombo.Items.Clear;
      SetLength(FCountryCodes, Length(Codes));
      Selected := 0;
      for I := 0 to High(Codes) do
      begin
        Code := UpperCase(Codes[I]);
        FCountryCodes[I] := Code;
        FCountryCombo.Items.Add(Code);
        if SameText(Code, FSettings.InspectionCountry) then
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

function TfrmWelcome.EulaFileName: string;
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

procedure TfrmWelcome.LoadLicence;
var
  FileName: string;
begin
  FileName := EulaFileName;
  if TFile.Exists(FileName) then
    FLicenceMemo.Text := TFile.ReadAllText(FileName, TEncoding.UTF8)
  else
    FLicenceMemo.Text :=
      'OBD-Studio Personal is free for personal, non-commercial use. ' +
      'Do not use it while driving. The software is provided as is, ' +
      'without warranty. Your data stays on this PC.';
end;

procedure TfrmWelcome.ApplyLanguage;
var
  Code: string;
begin
  if FSettings = nil then
    Exit;
  Code := FSettings.Language;
  if Code = '' then
    Code := OBDDefaultLanguageCode;
  if not OBDIsSupportedLanguage(Code) then
    Code := 'en';
  if Code <> 'en' then
  try
    OBDI18n.LoadLanguageFromString(MergedCatalog(
      TPath.Combine(FLanguageFolder, Code + '.json'),
      TPath.Combine(TPath.Combine(FLanguageFolder, 'personal'),
      Code + '.json'), Code));
  except
    Code := 'en';
  end;
  OBDI18n.CurrentLanguage := Code;
end;

procedure TfrmWelcome.Translate;
const
  STEP_TITLES: array[0..3] of string = ('Language', 'Licence',
    'Connection', 'Done');
  STEP_SUBS: array[0..3] of string = ('Choose your language',
    'Read and accept', 'Adapter and units', 'Ready to start');
begin
  Caption := Tr('personal.welcome.caption', 'Welcome to OBD-Studio Personal');
  FBrandTitle.Caption := Tr('personal.welcome.brand', 'OBD-Studio');
  FBrandSub.Caption := Tr('personal.welcome.personal', 'Personal');
  FStepLabels[0].Caption := Tr('personal.welcome.step.language',
    STEP_TITLES[0]);
  FStepLabels[1].Caption := Tr('personal.welcome.step.licence',
    STEP_TITLES[1]);
  FStepLabels[2].Caption := Tr('personal.welcome.step.connect',
    STEP_TITLES[2]);
  FStepLabels[3].Caption := Tr('personal.welcome.step.done',
    STEP_TITLES[3]);
  FStepSubs[0].Caption := Tr('personal.welcome.step.language.sub',
    STEP_SUBS[0]);
  FStepSubs[1].Caption := Tr('personal.welcome.step.licence.sub',
    STEP_SUBS[1]);
  FStepSubs[2].Caption := Tr('personal.welcome.step.connect.sub',
    STEP_SUBS[2]);
  FStepSubs[3].Caption := Tr('personal.welcome.step.done.sub',
    STEP_SUBS[3]);

  FLanguageLabel.Caption := Tr('personal.welcome.language.label',
    'Display language');
  FLanguageNote.Caption := Tr('personal.welcome.language.note',
    'You can change the language later in Settings. Draft catalogues are ' +
    'marked as machine translation until reviewed by a native speaker.');
  FAcceptCheck.Caption := Tr('personal.welcome.licence.accept',
    'I have read and accept the licence agreement');
  FLicenceNote.Caption := TrFmt('personal.welcome.licence.version',
    'Licence version %s. If a later update changes it, OBD-Studio asks again.',
    [PERSONAL_EULA_VERSION]);
  FDemoRadio.Caption := Tr('personal.welcome.connect.demo', 'Demo car');
  FSerialRadio.Caption := Tr('personal.welcome.connect.serial',
    'USB / Bluetooth COM');
  FWiFiRadio.Caption := Tr('personal.welcome.connect.wifi', 'Wi-Fi');
  FBleRadio.Caption := Tr('personal.welcome.connect.ble', 'Bluetooth LE');
  FPortLabel.Caption := Tr('personal.welcome.connect.port', 'COM port');
  FWiFiHostLabel.Caption := Tr('personal.welcome.connect.host', 'Wi-Fi host');
  FWiFiPortLabel.Caption := Tr('personal.welcome.connect.tcpPort',
    'TCP port');
  FBleLabel.Caption := Tr('personal.welcome.connect.bleAddress',
    'BLE address');
  FUnitsLabel.Caption := Tr('personal.welcome.units.label', 'Units');
  FCountryLabel.Caption := Tr('personal.welcome.country.label',
    'Inspection country');
  FUnitsCombo.Items.Text := Tr('personal.welcome.units.metric',
    'Metric (km, °C, kPa, litres)') + sLineBreak +
    Tr('personal.welcome.units.imperial',
    'Imperial (miles, °F, psi, gallons)');
  FDoneText.Caption := Tr('personal.welcome.done.text',
    'Setup is complete. OBD-Studio Personal works offline and keeps your ' +
    'vehicles, recordings and settings on this computer. Click Finish to ' +
    'start.');
  FDeclineButton.Caption := Tr('personal.welcome.decline',
    'Decline and quit');
  FBackButton.Caption := Tr('common.back', 'Back');
  FillLanguages;
  UpdateStep;
end;

procedure TfrmWelcome.SetStepVisible(AIndex: Integer; AVisible: Boolean);
begin
  case AIndex of
    0:
      begin
        FLanguageLabel.Visible := AVisible;
        FLanguageCombo.Visible := AVisible;
        FLanguageNote.Visible := AVisible;
      end;
    1:
      begin
        FLicenceMemo.Visible := AVisible;
        FAcceptCheck.Visible := AVisible;
        FLicenceNote.Visible := AVisible;
      end;
    2:
      begin
        FDemoRadio.Visible := AVisible;
        FSerialRadio.Visible := AVisible;
        FWiFiRadio.Visible := AVisible;
        FBleRadio.Visible := AVisible;
        FPortLabel.Visible := AVisible;
        FPortEdit.Visible := AVisible;
        FWiFiHostLabel.Visible := AVisible;
        FWiFiHostEdit.Visible := AVisible;
        FWiFiPortLabel.Visible := AVisible;
        FWiFiPortEdit.Visible := AVisible;
        FBleLabel.Visible := AVisible;
        FBleEdit.Visible := AVisible;
        FUnitsLabel.Visible := AVisible;
        FUnitsCombo.Visible := AVisible;
        FCountryLabel.Visible := AVisible;
        FCountryCombo.Visible := AVisible;
      end;
    3:
      FDoneText.Visible := AVisible;
  end;
end;

procedure TfrmWelcome.UpdateStep;
var
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    SetStepVisible(I, I = FStep);
    if I < FStep then
    begin
      FStepChips[I].Caption := '✓';
      FStepChips[I].Kind := skSuccess;
    end
    else if I = FStep then
    begin
      FStepChips[I].Caption := IntToStr(I + 1);
      FStepChips[I].Kind := skAccent;
    end
    else
    begin
      FStepChips[I].Caption := IntToStr(I + 1);
      FStepChips[I].Kind := skNeutral;
    end;
  end;
  case FStep of
    0:
      begin
        FStepTitle.Caption := Tr('personal.welcome.language.title',
          'Choose your language');
        FStepIntro.Caption := Tr('personal.welcome.language.intro',
          'The change applies immediately to this wizard.');
      end;
    1:
      begin
        FStepTitle.Caption := Tr('personal.welcome.licence.title',
          'Licence agreement');
        FStepIntro.Caption := Tr('personal.welcome.licence.intro',
          'OBD-Studio Personal is free for personal use. Read and accept ' +
          'the licence to continue.');
      end;
    2:
      begin
        FStepTitle.Caption := Tr('personal.welcome.connect.title',
          'Connect to your car');
        FStepIntro.Caption := Tr('personal.welcome.connect.intro',
          'Choose Demo car to explore without an adapter, or enter the ' +
          'details of your OBD adapter.');
      end;
  else
    FStepTitle.Caption := Tr('personal.welcome.done.title',
      'You are ready');
    FStepIntro.Caption := Tr('personal.welcome.done.intro',
      'These choices can all be changed later in Settings.');
  end;
  UpdateButtons;
end;

procedure TfrmWelcome.UpdateButtons;
begin
  FBackButton.Enabled := FStep > 0;
  if FStep = 3 then
    FNextButton.Caption := Tr('common.finish', 'Finish')
  else if FStep = 1 then
    FNextButton.Caption := Tr('personal.welcome.acceptContinue',
      'Accept and continue')
  else
    FNextButton.Caption := Tr('common.next', 'Next');
  FNextButton.Enabled := (FStep <> 1) or FAcceptCheck.Checked;
end;

procedure TfrmWelcome.UpdateConnectionFields;
var
  Serial, WiFi, Ble: Boolean;
begin
  Serial := FSerialRadio.Checked;
  WiFi := FWiFiRadio.Checked;
  Ble := FBleRadio.Checked;
  FPortEdit.Enabled := Serial;
  FWiFiHostEdit.Enabled := WiFi;
  FWiFiPortEdit.Enabled := WiFi;
  FBleEdit.Enabled := Ble;
end;

function TfrmWelcome.SelectedLanguageCode: string;
begin
  if (FLanguageCombo.ItemIndex >= 0) and
    (FLanguageCombo.ItemIndex <= High(FLanguageCodes)) then
    Result := FLanguageCodes[FLanguageCombo.ItemIndex]
  else
    Result := '';
end;

function TfrmWelcome.SelectedCountryCode: string;
begin
  if (FCountryCombo.ItemIndex >= 0) and
    (FCountryCombo.ItemIndex <= High(FCountryCodes)) then
    Result := FCountryCodes[FCountryCombo.ItemIndex]
  else
    Result := '';
end;

procedure TfrmWelcome.LanguageChanged(Sender: TObject);
begin
  if FLoading or (FSettings = nil) then
    Exit;
  FSettings.Language := SelectedLanguageCode;
  ApplyLanguage;
  Translate;
end;

procedure TfrmWelcome.AcceptChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmWelcome.ConnectionKindChanged(Sender: TObject);
begin
  if FLoading or (FSettings = nil) then
    Exit;
  if FDemoRadio.Checked then
    FSettings.ConnectionKind := pckDemo
  else if FWiFiRadio.Checked then
    FSettings.ConnectionKind := pckWiFi
  else if FBleRadio.Checked then
    FSettings.ConnectionKind := pckBLE
  else if FSerialRadio.Checked then
    FSettings.ConnectionKind := pckSerial;
  UpdateConnectionFields;
end;

procedure TfrmWelcome.ConnectionEditChanged(Sender: TObject);
begin
  if FLoading or (FSettings = nil) then
    Exit;
  FSettings.SerialPort := Trim(FPortEdit.Text);
  FSettings.WiFiHost := Trim(FWiFiHostEdit.Text);
  FSettings.WiFiPort := StrToIntDef(FWiFiPortEdit.Text, FSettings.WiFiPort);
  FSettings.BLEAddress := Trim(FBleEdit.Text);
end;

procedure TfrmWelcome.UnitsChanged(Sender: TObject);
begin
  if FLoading or (FSettings = nil) then
    Exit;
  if FUnitsCombo.ItemIndex = 1 then
    FSettings.UnitSystem := usImperial
  else
    FSettings.UnitSystem := usMetric;
end;

procedure TfrmWelcome.CountryChanged(Sender: TObject);
begin
  if FLoading or (FSettings = nil) then
    Exit;
  FSettings.InspectionCountry := SelectedCountryCode;
end;

procedure TfrmWelcome.BackClick(Sender: TObject);
begin
  if FStep > 0 then
  begin
    Dec(FStep);
    UpdateStep;
  end;
end;

procedure TfrmWelcome.NextClick(Sender: TObject);
begin
  if (FStep = 1) and not FAcceptCheck.Checked then
    Exit;
  if FStep < 3 then
  begin
    Inc(FStep);
    UpdateStep;
    Exit;
  end;
  FSettings.AcceptedEula := PERSONAL_EULA_VERSION;
  FSettings.WelcomeDone := True;
  ModalResult := mrOk;
end;

procedure TfrmWelcome.DeclineClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
