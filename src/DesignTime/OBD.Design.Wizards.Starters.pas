//------------------------------------------------------------------------------
//  OBD.Design.Wizards.Starters
//
//  Tools-API wizard registered under File > New > Other...
//  -> Delphi-OBD page. Pops the
//  <see cref="OBD.Design.Forms.StarterPicker.TOBDStarterPickerDlg"/>
//  picker, runs the chosen starter's generator, writes the
//  artifacts to disk, and opens the resulting <c>.dpr</c> in the
//  IDE so RAD regenerates the <c>.dproj</c> on first save.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Wizards.Starters;

interface

uses
  ToolsAPI;

type
  /// <summary>Tools-API repository wizard hosting all
  /// Delphi-OBD starters under one entry.</summary>
  TOBDStartersWizard = class(TInterfacedObject,
    IOTAWizard, IOTARepositoryWizard, IOTAProjectWizard)
  public
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

/// <summary>Registers the wizard with the IDE. Call from the
/// design-time package's <c>Register</c>.</summary>
procedure RegisterDelphiOBDStarterWizard;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Vcl.Dialogs,
  OBD.Design.Starters,
  OBD.Design.Forms.StarterPicker;

{ ---- TOBDStartersWizard ----------------------------------------------------- }

function TOBDStartersWizard.GetIDString: string;
begin
  Result := 'ERDesigns.DelphiOBD.Starters';
end;

function TOBDStartersWizard.GetName: string;
begin
  Result := 'Delphi-OBD project starter';
end;

function TOBDStartersWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TOBDStartersWizard.GetAuthor: string;
begin
  Result := 'ERDesigns';
end;

function TOBDStartersWizard.GetComment: string;
begin
  Result :=
    'Scaffold a Delphi-OBD project with the components a host ' +
    'typically wants for a specific task — DTC reader, VIN reader, ' +
    'live-data dashboard, coding session, flash session, recorder ' +
    'workbench, or a bare connection chain.';
end;

function TOBDStartersWizard.GetPage: string;
begin
  Result := 'Delphi-OBD';
end;

function TOBDStartersWizard.GetGlyph: Cardinal;
begin
  // 0 means "use the default new-item glyph". A future revision
  // can load the brand mark from DelphiOBD_DT.res and return it
  // as an HBITMAP cast to Cardinal.
  Result := 0;
end;

procedure TOBDStartersWizard.Execute;
var
  Context: TOBDStarterContext;
  Starter: TOBDStarter;
  Artifacts: TArray<TOBDStarterArtifact>;
  A: TOBDStarterArtifact;
  ProjectPath: string;
  Action: IOTAActionServices;
begin
  if not TOBDStarterPickerDlg.Pick(Context, Starter) then Exit;
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

    // Open the new project so RAD regenerates the .dproj.
    if (ProjectPath <> '') and
       Supports(BorlandIDEServices, IOTAActionServices, Action) then
      Action.OpenProject(ProjectPath, True);
  finally
    // Pick() handed ownership of the Choices map to us.
    Context.Choices.Free;
  end;
end;

procedure RegisterDelphiOBDStarterWizard;
var
  Svc: IOTAWizardServices;
begin
  if Supports(BorlandIDEServices, IOTAWizardServices, Svc) then
    Svc.AddWizard(TOBDStartersWizard.Create);
end;

end.
