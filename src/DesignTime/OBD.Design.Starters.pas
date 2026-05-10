//------------------------------------------------------------------------------
//  OBD.Design.Starters
//
//  Starter framework for the Delphi-OBD IDE wizard.
//
//  A "starter" is a small project template that scaffolds a
//  Delphi VCL project with the Delphi-OBD components a host
//  typically needs for a specific task (read DTCs, code DIDs,
//  flash an ECU, record a session, …). Each starter is a single
//  registry entry — title, description, generator callback —
//  and produces the file artifacts (<c>.dpr</c>, <c>.pas</c>,
//  <c>.dfm</c>) the wizard writes to disk before opening the
//  project in the IDE.
//
//  Adding a starter is a one-record exercise: hand
//  <see cref="TOBDStarterRegistry.Register"/> a generator and
//  the wizard picks it up on next install.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Starters;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  /// <summary>One file the wizard will write to disk during a
  /// starter run.</summary>
  TOBDStarterArtifact = record
    /// <summary>Repo-relative path (e.g. <c>Form1.pas</c>) — the
    /// wizard joins this with the host's chosen target folder.</summary>
    RelativePath: string;
    /// <summary>Final text content. Templates have been expanded
    /// against the host's project / form / unit names.</summary>
    Content: string;
    /// <summary>True for the project file. The wizard opens it
    /// in the IDE after writing.</summary>
    IsProjectFile: Boolean;
  end;

  /// <summary>Per-run inputs collected by the picker dialog.
  /// Generators expand templates against these values.</summary>
  TOBDStarterContext = record
    /// <summary>Absolute target directory. Wizard creates it if
    /// missing.</summary>
    TargetDir: string;
    /// <summary>Project name without extension (e.g.
    /// <c>MyDiagnostics</c>). Becomes the <c>.dpr</c> name.</summary>
    ProjectName: string;
    /// <summary>Unit name without extension (e.g.
    /// <c>MainForm</c>). Becomes the <c>.pas</c> / <c>.dfm</c>
    /// name and the unit identifier.</summary>
    UnitName: string;
    /// <summary>Form class name (e.g.
    /// <c>TMainForm</c>). Becomes the form's class declaration.</summary>
    FormClassName: string;
    /// <summary>Form instance name (e.g.
    /// <c>MainForm</c>). Becomes the global var the project
    /// creates.</summary>
    FormInstanceName: string;
  end;

  /// <summary>Generator signature.</summary>
  TOBDStarterGenerator = reference to function(
    const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;

  /// <summary>One starter entry.</summary>
  TOBDStarter = record
    /// <summary>Stable id used in URLs / logs (e.g.
    /// <c>'dtc-reader'</c>).</summary>
    Id: string;
    /// <summary>Display title (e.g.
    /// <c>'DTC reader / reset (ELM327)'</c>).</summary>
    Title: string;
    /// <summary>1–3 line description shown in the picker.</summary>
    Description: string;
    /// <summary>Display category (e.g. <c>'Service-mode'</c>).
    /// The picker groups starters under their category.</summary>
    Category: string;
    /// <summary>Generator callback.</summary>
    Generate: TOBDStarterGenerator;
  end;

  /// <summary>Process-wide starter registry. Threaded access
  /// is OK during package install but the registry is intended
  /// to be populated once per process from initialization
  /// blocks of the starter implementation units.</summary>
  TOBDStarterRegistry = class
  strict private
    class var FInstance: TOBDStarterRegistry;
    FStarters: TList<TOBDStarter>;
  public
    constructor Create;
    destructor Destroy; override;
    class function Default: TOBDStarterRegistry;
    class destructor ClassDestroy;

    /// <summary>Adds a starter to the registry. Duplicate
    /// <c>Id</c> values silently overwrite the existing
    /// entry — useful when a contributor reloads a custom
    /// starter unit during development.</summary>
    procedure Register(const AStarter: TOBDStarter);
    /// <summary>Returns every registered starter, sorted by
    /// category then title.</summary>
    function All: TArray<TOBDStarter>;
  end;

implementation

uses
  System.Generics.Defaults;

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

end.
