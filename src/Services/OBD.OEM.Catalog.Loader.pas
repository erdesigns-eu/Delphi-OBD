//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Catalog.Loader.pas
// CONTENTS       : Bridge that lets OEM extensions load their JSON
//                  catalog from disk and merge it with their hard-coded
//                  starter entries.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Each extension's `BuildCatalog` calls
//                  `MergeCatalogJSON('vw.json', DIDs, Routines)` after
//                  populating its hard-coded fallback. JSON entries
//                  win on conflict (same DID), so user-supplied
//                  catalogs override the framework defaults.
//
//                  Search path for the catalog file:
//                    1. exe-dir / catalogs / <name>
//                    2. exe-dir / .. / catalogs / <name>
//                    3. CWD     / catalogs / <name>
//                    4. user override via SetCatalogSearchPath
//------------------------------------------------------------------------------
unit OBD.OEM.Catalog.Loader;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  OBD.OEM, OBD.OEM.Catalog.JSON;

/// <summary>
///   Override the catalog search path. Pass an empty string to revert to
///   the default exe-relative search.
/// </summary>
procedure SetCatalogSearchPath(const Path: string);

/// <summary>
///   Resolve a catalog file by name. Returns empty string if not found.
/// </summary>
function ResolveCatalogPath(const FileName: string): string;

/// <summary>
///   Load <c>FileName</c> and merge its entries into <c>DIDs</c> +
///   <c>Routines</c>. JSON entries take precedence over already-present
///   hard-coded entries with the same DID / Routine identifier.
///
///   Silently no-ops if the file isn't found — the caller's hard-coded
///   fallback continues to work.
/// </summary>
procedure MergeCatalogJSON(const FileName: string;
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>); overload;

/// <summary>
///   Same as the two-argument overload but additionally merges any
///   <c>ecus</c> array from the JSON into the OEM extension's ECU map.
///   Existing ECUs (matched by Address) are replaced; new ones are
///   appended.
/// </summary>
procedure MergeCatalogJSON(const FileName: string;
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>); overload;

implementation

var
  GCatalogSearchPath: string = '';

procedure SetCatalogSearchPath(const Path: string);
begin
  GCatalogSearchPath := Path;
end;

function ExecutableDir: string;
begin
  Result := TPath.GetDirectoryName(ParamStr(0));
end;

function ResolveCatalogPath(const FileName: string): string;
var
  Candidates: TArray<string>;
  Candidate: string;
begin
  Candidates := [];

  if GCatalogSearchPath <> '' then
    Candidates := Candidates + [TPath.Combine(GCatalogSearchPath, FileName)];

  Candidates := Candidates + [
    TPath.Combine(TPath.Combine(ExecutableDir, 'catalogs'), FileName),
    TPath.Combine(TPath.Combine(TPath.Combine(ExecutableDir, '..'), 'catalogs'), FileName),
    TPath.Combine(TPath.Combine(GetCurrentDir, 'catalogs'), FileName)
  ];

  for Candidate in Candidates do
    if TFile.Exists(Candidate) then
      Exit(TPath.GetFullPath(Candidate));

  Result := '';
end;

procedure MergeDIDs(var Existing: TArray<TOBDOEMDataIdentifier>;
  const Loaded: TArray<TOBDOEMDataIdentifier>);
var
  I, J: Integer;
  Found: Boolean;
begin
  // For every loaded entry, replace by-DID if existing, else append.
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if Existing[J].DID = Loaded[I].DID then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeRoutines(var Existing: TArray<TOBDOEMRoutine>;
  const Loaded: TArray<TOBDOEMRoutine>);
var
  I, J: Integer;
  Found: Boolean;
begin
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if Existing[J].Identifier = Loaded[I].Identifier then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeECUs(var Existing: TArray<TOBDOEMECU>;
  const Loaded: TArray<TOBDOEMECU>);
var
  I, J: Integer;
  Found: Boolean;
begin
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if Existing[J].Address = Loaded[I].Address then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeCatalogJSON(const FileName: string;
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>);
var
  Discard: TArray<TOBDOEMECU>;
begin
  Discard := nil;
  MergeCatalogJSON(FileName, DIDs, Routines, Discard);
end;

procedure MergeCatalogJSON(const FileName: string;
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
var
  Path: string;
  Catalog: TOBDOEMJSONCatalog;
begin
  Path := ResolveCatalogPath(FileName);
  if Path = '' then Exit;
  try
    Catalog := TOBDOEMJSONCatalog.Create(Path);
    try
      MergeDIDs(DIDs, Catalog.AsBaseDIDs);
      MergeRoutines(Routines, Catalog.AsBaseRoutines);
      MergeECUs(ECUs, Catalog.AsBaseECUs);
    finally
      Catalog.Free;
    end;
  except
    // Log via the standard logger sink in production code; here we
    // silently fall through so a malformed catalog doesn't break
    // application startup.
  end;
end;

end.
