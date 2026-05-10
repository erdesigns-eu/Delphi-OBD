//------------------------------------------------------------------------------
//  OBD.Design.Wizards.PerCategory
//
//  P-A7 commit 1: per-category Tools-API wizards. One
//  IOTARepositoryWizard per palette tab so users browsing
//  File > New > Other > Delphi-OBD see the surface area
//  organised the same way the IDE component palette is.
//
//  The existing "Delphi-OBD project starter" (P-13) stays as
//  the all-categories entry; these new wizards are filtered
//  views on the same starter registry.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Design.Wizards.PerCategory;

interface

uses
  ToolsAPI;

type
  TOBDCategoryStartersWizard = class(TInterfacedObject,
    IOTAWizard, IOTARepositoryWizard, IOTAProjectWizard)
  strict private
    FIDSuffix:   string;
    FName:       string;
    FComment:    string;
    FCategories: TArray<string>;
  public
    constructor Create(const AIDSuffix, AName, AComment: string;
      const ACategories: TArray<string>);
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
  end;

procedure RegisterPerCategoryWizards;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Design.Starters,
  OBD.Design.Forms.StarterPicker;

constructor TOBDCategoryStartersWizard.Create(
  const AIDSuffix, AName, AComment: string;
  const ACategories: TArray<string>);
begin
  inherited Create;
  FIDSuffix   := AIDSuffix;
  FName       := AName;
  FComment    := AComment;
  FCategories := ACategories;
end;

function TOBDCategoryStartersWizard.GetIDString: string;
begin
  Result := 'ERDesigns.DelphiOBD.Starters.' + FIDSuffix;
end;

function TOBDCategoryStartersWizard.GetName: string;
begin
  Result := FName;
end;

function TOBDCategoryStartersWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TOBDCategoryStartersWizard.GetAuthor: string;
begin
  Result := 'ERDesigns';
end;

function TOBDCategoryStartersWizard.GetComment: string;
begin
  Result := FComment;
end;

function TOBDCategoryStartersWizard.GetPage: string;
begin
  Result := 'Delphi-OBD';
end;

function TOBDCategoryStartersWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

procedure TOBDCategoryStartersWizard.Execute;
var
  Context:   TOBDStarterContext;
  Starter:   TOBDStarter;
  Artifacts: TArray<TOBDStarterArtifact>;
  A:         TOBDStarterArtifact;
  ProjectPath: string;
  Action:    IOTAActionServices;
begin
  if not TOBDStarterPickerDlg.Pick(Context, Starter, FCategories) then Exit;
  try
    if not TDirectory.Exists(Context.TargetDir) then
      TDirectory.CreateDirectory(Context.TargetDir);
    Artifacts := Starter.Generate(Context);
    ProjectPath := '';
    for A in Artifacts do
    begin
      TFile.WriteAllText(
        TPath.Combine(Context.TargetDir, A.RelativePath),
        A.Content,
        TEncoding.UTF8);
      if A.IsProjectFile then
        ProjectPath := TPath.Combine(Context.TargetDir, A.RelativePath);
    end;
    if (ProjectPath <> '') and
       Supports(BorlandIDEServices, IOTAActionServices, Action) then
      Action.OpenProject(ProjectPath, True);
  finally
    Context.Choices.Free;
  end;
end;

procedure RegisterPerCategoryWizards;
var Svc: IOTAWizardServices;

  procedure Add(const ASuffix, AName, AComment: string;
    const ACats: TArray<string>);
  begin
    Svc.AddWizard(
      TOBDCategoryStartersWizard.Create(ASuffix, AName, AComment, ACats));
  end;

begin
  if not Supports(BorlandIDEServices, IOTAWizardServices, Svc) then Exit;

  Add('Connection',
      'Delphi-OBD: Connection & Diagnostics',
      'Bare connection / adapter / protocol chain plus the recorder ' +
      'tooling. Foundation for any project that talks to a vehicle.',
      ['Foundation', 'Network', 'Tooling']);

  Add('Services',
      'Delphi-OBD: Service-mode (live data, DTCs, VIN, ...)',
      'OBD-II service-mode reads: live data, DTCs, freeze frame, VIN, ' +
      'on-board monitor. Plus the VehicleHealth roll-up.',
      ['Service-mode']);

  Add('Coding',
      'Delphi-OBD: Coding & UDS write-side',
      'Security access, DataIdentifierIO, RoutineControl, recoding, ' +
      'audit log. Component-protection + key-adaptation per OEM.',
      ['Coding']);

  Add('Calibration',
      'Delphi-OBD: Calibration (XCP / CCP / IsoBus)',
      'Calibration-bus components: XCP master with CAN/Ethernet/USB ' +
      'transport, CCP, IsoBus implement.',
      ['Calibration']);

  Add('Flashing',
      'Delphi-OBD: Flashing & UDS transfer',
      'UDS transfer, voltage-gate, flash pipeline. AutoExecute is ' +
      'False by default - hosts must wire OnConfirmExecute.',
      ['Flashing']);

  Add('Radio',
      'Delphi-OBD: Radio code calculator',
      'Single-vendor radio-code calculator form. Ships with one of ' +
      'the 47 bundled vendor components dropped in - swap to your ' +
      'target vendor after generation.',
      ['Radio']);

  Add('EEPROM',
      'Delphi-OBD: EEPROM extractor',
      'Form that loads a .bin / .eep dump and extracts the unlock ' +
      'code at the documented offset. Ships with one of the three ' +
      'bundled extractors dropped in.',
      ['EEPROM']);

  Add('Catalogs',
      'Delphi-OBD: Catalogue manager DataModule',
      'TDataModule wiring TOBDVINCatalog + TOBDDriveCycleCatalogComp ' +
      '+ TOBDEVBatteryCatalogComp with AutoLoad = True. Centralises ' +
      'CatalogDir setup.',
      ['Catalogs']);

  Add('Sessions',
      'Delphi-OBD: KWP1281 / TP2.0 / J2534 session',
      'Form with a TOBDKWP1281Session pre-configured for VW radios. ' +
      'TODO comments show where to wire the K-line / CAN / J2534 ' +
      'transport.',
      ['Sessions']);

  Add('Suite',
      'Delphi-OBD: Full kitchen-sink suite',
      'Multi-tab project with live data + DTCs + VIN + freeze-frame ' +
      '+ coding + flashing pre-wired against a shared connection.',
      ['Suite']);
end;

end.
