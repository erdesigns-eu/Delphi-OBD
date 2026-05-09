//------------------------------------------------------------------------------
// UNIT           : OBD.Catalog.Path.pas
// CONTENTS       : Dependency-free catalog file path resolver
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Catalog.Path;

interface

uses
  System.SysUtils, System.IOUtils;

/// <summary>
///   Override the catalog search path. Pass empty to revert.
/// </summary>
procedure SetGlobalCatalogPath(const Path: string);

/// <summary>
///   Resolve a catalog file by name. Probes (in order):
///   user override / exe-dir/catalogs/ / exe-dir/../catalogs/ / cwd/catalogs/
///   and the four v3.77 vehicle-class subdirectories under each root.
///   Returns '' if nothing matches.
/// </summary>
function ResolveCatalogPath(const FileName: string): string;

implementation

var
  GGlobalCatalogPath: string = '';

//------------------------------------------------------------------------------
// SET GLOBAL CATALOG PATH
//------------------------------------------------------------------------------
procedure SetGlobalCatalogPath(const Path: string);
begin
  GGlobalCatalogPath := Path;
end;

//------------------------------------------------------------------------------
// RESOLVE CATALOG PATH
//------------------------------------------------------------------------------
function ResolveCatalogPath(const FileName: string): string;
const
  Subdirs: array[0..3] of string =
    ('motorcycle', 'agricultural', 'marine', 'powersports');
var
  Roots: TArray<string>;
  Root, Sub, Candidate: string;
begin
  Roots := [];
  if GGlobalCatalogPath <> '' then
    Roots := Roots + [GGlobalCatalogPath];
  Roots := Roots + [
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'catalogs'),
    TPath.Combine(TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..'), 'catalogs'),
    TPath.Combine(GetCurrentDir, 'catalogs')
  ];
  // Loop over Roots
  for Root in Roots do
  begin
    Candidate := TPath.Combine(Root, FileName);
    if TFile.Exists(Candidate) then Exit(TPath.GetFullPath(Candidate));
    // Loop over Subdirs
    for Sub in Subdirs do
    begin
      Candidate := TPath.Combine(TPath.Combine(Root, Sub), FileName);
      if TFile.Exists(Candidate) then Exit(TPath.GetFullPath(Candidate));
    end;
  end;
  // Initialize result
  Result := '';
end;

end.
