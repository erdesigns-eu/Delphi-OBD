//------------------------------------------------------------------------------
//  OBD.Design.Starters
//
//  Starter framework for the Delphi-OBD IDE wizard.
//
//  A "starter" is a small project template that scaffolds a
//  Delphi VCL project with the Delphi-OBD components a host
//  typically needs for a specific task. Each starter ships a
//  single registry entry (title + description + generator
//  callback) plus an optional list of <see cref="TOBDStarterOptionGroup"/>
//  rendered by the wizard as a second page so the host can fold
//  one starter into many concrete projects without us shipping
//  separate templates for every permutation.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//    2026-05-10  ERD  Add option-group surface + public template
//                     helpers so split starter units can reuse
//                     the .dpr boilerplate.
//------------------------------------------------------------------------------

unit OBD.Design.Starters;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  /// <summary>How a starter option group is rendered on the
  /// wizard's options page.</summary>
  TOBDStarterOptionKind = (
    /// <summary>Multi-select checkboxes; any subset OK.</summary>
    sokMultiSelect,
    /// <summary>Single-select radio group; exactly one wins.</summary>
    sokSingleSelect
  );

  /// <summary>One option in a group.</summary>
  TOBDStarterOption = record
    /// <summary>Stable id passed back to the generator (e.g.
    /// <c>'oem-vag'</c>, <c>'pipeline'</c>, <c>'audit-log'</c>).</summary>
    Id: string;
    /// <summary>Visible label on the checkbox / radio.</summary>
    Caption: string;
    /// <summary>Pre-selected by default (multi-select) or the
    /// group's default radio (single-select).</summary>
    Selected: Boolean;
  end;

  /// <summary>One group of options the wizard renders as a
  /// labelled box on its options page.</summary>
  TOBDStarterOptionGroup = record
    /// <summary>Group id; used as the key in
    /// <see cref="TOBDStarterContext.Choices"/>.</summary>
    Id: string;
    /// <summary>Visible group title.</summary>
    Title: string;
    /// <summary>Helper text shown under the title.</summary>
    Description: string;
    /// <summary>Render mode.</summary>
    Kind: TOBDStarterOptionKind;
    /// <summary>The options.</summary>
    Options: TArray<TOBDStarterOption>;
  end;

  /// <summary>One file the wizard will write to disk during a
  /// starter run.</summary>
  TOBDStarterArtifact = record
    /// <summary>Relative path within the target folder
    /// (e.g. <c>Form1.pas</c>).</summary>
    RelativePath: string;
    /// <summary>Final text content with all template tokens
    /// expanded.</summary>
    Content: string;
    /// <summary>True for the project file. The wizard opens it
    /// in the IDE after writing.</summary>
    IsProjectFile: Boolean;
  end;

  /// <summary>Per-run inputs collected by the wizard. Generators
  /// expand templates against these values.</summary>
  TOBDStarterContext = record
    /// <summary>Absolute target directory.</summary>
    TargetDir: string;
    /// <summary>Project name without extension.</summary>
    ProjectName: string;
    /// <summary>Unit name without extension.</summary>
    UnitName: string;
    /// <summary>Form class name (with <c>T</c> prefix).</summary>
    FormClassName: string;
    /// <summary>Form instance name (no <c>T</c> prefix).</summary>
    FormInstanceName: string;
    /// <summary>Per-group selections — group id → list of
    /// selected option ids. Multi-select groups carry an array
    /// of any length; single-select groups carry exactly one
    /// element. Empty / missing groups mean "none selected".</summary>
    Choices: TDictionary<string, TArray<string>>;

    /// <summary>True when <c>Choices</c> contains
    /// <c>AOptionId</c> in <c>AGroupId</c>. Convenience for
    /// generators.</summary>
    function HasChoice(const AGroupId, AOptionId: string): Boolean;
    /// <summary>Returns the first selected option id in
    /// <c>AGroupId</c>, or <c>ADefault</c> when none.</summary>
    function FirstChoice(const AGroupId, ADefault: string): string;
  end;

  /// <summary>Generator signature.</summary>
  TOBDStarterGenerator = reference to function(
    const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;

  /// <summary>One starter entry.</summary>
  TOBDStarter = record
    /// <summary>Stable id used in URLs / logs.</summary>
    Id: string;
    /// <summary>Display title.</summary>
    Title: string;
    /// <summary>1–3 line description shown on page 1.</summary>
    Description: string;
    /// <summary>Display category.</summary>
    Category: string;
    /// <summary>Optional option groups. Empty array → wizard
    /// skips the options page for this starter.</summary>
    OptionGroups: TArray<TOBDStarterOptionGroup>;
    /// <summary>Generator callback.</summary>
    Generate: TOBDStarterGenerator;
  end;

  /// <summary>Process-wide starter registry.</summary>
  TOBDStarterRegistry = class
  strict private
    class var FInstance: TOBDStarterRegistry;
    FStarters: TList<TOBDStarter>;
  public
    constructor Create;
    destructor Destroy; override;
    class function Default: TOBDStarterRegistry;
    class destructor ClassDestroy;

    /// <summary>Adds a starter; duplicate ids overwrite.</summary>
    procedure Register(const AStarter: TOBDStarter);
    /// <summary>Sorted by category then title.</summary>
    function All: TArray<TOBDStarter>;
  end;

{ ---- Template helpers (shared across starter units) ------------------------ }

/// <summary>Replaces <c>{PROJ}</c>, <c>{UNIT}</c>, <c>{FORM}</c>,
/// <c>{FORMVAR}</c> in <c>ATemplate</c> with the matching fields
/// from <c>AContext</c>. Public so split starter files can reuse.</summary>
function ExpandTemplate(const ATemplate: string;
  const AContext: TOBDStarterContext): string;

/// <summary>Builds the canonical <c>.dpr</c> artifact for a starter
/// (single-form VCL project that creates the host's main form).</summary>
function MakeDprArtifact(
  const AContext: TOBDStarterContext): TOBDStarterArtifact;

implementation

uses
  System.Generics.Defaults;

{ ---- TOBDStarterContext ----------------------------------------------------- }

function TOBDStarterContext.HasChoice(
  const AGroupId, AOptionId: string): Boolean;
var
  Selected: TArray<string>;
  S: string;
begin
  Result := False;
  if Choices = nil then Exit;
  if not Choices.TryGetValue(AGroupId, Selected) then Exit;
  for S in Selected do
    if SameText(S, AOptionId) then Exit(True);
end;

function TOBDStarterContext.FirstChoice(
  const AGroupId, ADefault: string): string;
var
  Selected: TArray<string>;
begin
  if (Choices <> nil) and Choices.TryGetValue(AGroupId, Selected) and
     (Length(Selected) > 0) then
    Result := Selected[0]
  else
    Result := ADefault;
end;

{ ---- TOBDStarterRegistry ----------------------------------------------------- }

constructor TOBDStarterRegistry.Create;
begin
  inherited Create;
  FStarters := TList<TOBDStarter>.Create;
end;

destructor TOBDStarterRegistry.Destroy;
begin
  FStarters.Free;
  inherited;
end;

class function TOBDStarterRegistry.Default: TOBDStarterRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDStarterRegistry.Create;
  Result := FInstance;
end;

class destructor TOBDStarterRegistry.ClassDestroy;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDStarterRegistry.Register(const AStarter: TOBDStarter);
var
  I: Integer;
begin
  for I := 0 to FStarters.Count - 1 do
    if FStarters[I].Id = AStarter.Id then
    begin
      FStarters[I] := AStarter;
      Exit;
    end;
  FStarters.Add(AStarter);
end;

function TOBDStarterRegistry.All: TArray<TOBDStarter>;
begin
  Result := FStarters.ToArray;
  TArray.Sort<TOBDStarter>(Result,
    TComparer<TOBDStarter>.Construct(
      function(const L, R: TOBDStarter): Integer
      begin
        Result := CompareText(L.Category, R.Category);
        if Result = 0 then
          Result := CompareText(L.Title, R.Title);
      end));
end;

{ ---- Template helpers ------------------------------------------------------- }

function ExpandTemplate(const ATemplate: string;
  const AContext: TOBDStarterContext): string;
begin
  Result := ATemplate;
  Result := StringReplace(Result, '{PROJ}',    AContext.ProjectName,      [rfReplaceAll]);
  Result := StringReplace(Result, '{UNIT}',    AContext.UnitName,         [rfReplaceAll]);
  Result := StringReplace(Result, '{FORM}',    AContext.FormClassName,    [rfReplaceAll]);
  Result := StringReplace(Result, '{FORMVAR}', AContext.FormInstanceName, [rfReplaceAll]);
end;

function MakeDprArtifact(
  const AContext: TOBDStarterContext): TOBDStarterArtifact;
const
  TPL =
    'program {PROJ};'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  Vcl.Forms,'#13#10 +
    '  {UNIT} in ''{UNIT}.pas'' {{FORM};'#13#10 +
    ''#13#10 +
    '{$R *.res}'#13#10 +
    ''#13#10 +
    'begin'#13#10 +
    '  Application.Initialize;'#13#10 +
    '  Application.MainFormOnTaskbar := True;'#13#10 +
    '  Application.CreateForm({FORM}, {FORMVAR});'#13#10 +
    '  Application.Run;'#13#10 +
    'end.'#13#10;
begin
  Result.RelativePath  := AContext.ProjectName + '.dpr';
  Result.Content       := ExpandTemplate(TPL, AContext);
  Result.IsProjectFile := True;
end;

end.
