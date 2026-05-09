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
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>); overload;

/// <summary>
///   Same as the two-argument overload but additionally merges any
///   <c>ecus</c> array from the JSON into the OEM extension's ECU map.
///   Existing ECUs (matched by Address) are replaced; new ones are
///   appended.
/// </summary>
procedure MergeCatalogJSON(const FileName: string;
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>); overload;

/// <summary>
///   v3.29 Phase A — load the extended-catalog sections (coding
///   blocks, adaptations, actuator tests, live PIDs, DTC extended-data
///   records) from <c>FileName</c> and merge them into the supplied
///   arrays. Same merge semantics as <c>MergeCatalogJSON</c>: entries
///   matching by primary key (DID, channel, identifier, mode+pid,
///   code+record) are replaced; new ones are appended. Silently
///   no-ops when the file isn't found.
/// </summary>
procedure MergeExtendedCatalogJSON(const FileName: string;
  var
    CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var
    Adaptations: TArray<TOBDOEMAdaptation>;
  var
    ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var
    LivePIDs: TArray<TOBDOEMLivePID>;
  var
    DtcExtended: TArray<TOBDDtcExtendedDataRecord>);

/// <summary>
///   v3.31 — JSON-driven VIN routing. Returns True if the catalog's
///   <c>applicable_wmis</c> array contains the first 3 chars of
///   <c>VIN</c>. Returns False on missing file, missing array, or
///   malformed input. Matches case-insensitively.
/// </summary>
function VINMatchesCatalog(const FileName, VIN: string): Boolean;

implementation

var
  GCatalogSearchPath: string = '';

//------------------------------------------------------------------------------
// SET CATALOG SEARCH PATH
//------------------------------------------------------------------------------
procedure SetCatalogSearchPath(const Path: string);
begin
  GCatalogSearchPath := Path;
end;

//------------------------------------------------------------------------------
// EXECUTABLE DIR
//------------------------------------------------------------------------------
function ExecutableDir: string;
begin
  Result := TPath.GetDirectoryName(ParamStr(0));
end;

//------------------------------------------------------------------------------
// RESOLVE CATALOG PATH
//------------------------------------------------------------------------------
function ResolveCatalogPath(const FileName: string): string;
const
  // Vehicle-class subdirectories introduced in v3.77 (Phase B):
  // motorcycle / agricultural / marine / powersports. Probed after
  // the top-level catalogs/ folder so the legacy car/truck OEMs
  // (which sit at the top level) still resolve in O(1) — the
  // subdirs are only consulted on a miss.
  VehicleClassSubdirs: array[0..3] of string =
    ('motorcycle', 'agricultural', 'marine', 'powersports');
var
  Candidates: TArray<string>;
  BaseRoots: TArray<string>;
  Candidate, Sub, Root: string;
begin
  // Build the roots in priority order: the user override (if set)
  // wins, then exe-dir/catalogs, then exe-dir/../catalogs, then
  // CWD/catalogs.
  BaseRoots := [];
  if GCatalogSearchPath <> '' then
    BaseRoots := BaseRoots + [GCatalogSearchPath];
  BaseRoots := BaseRoots + [
    TPath.Combine(ExecutableDir, 'catalogs'),
    TPath.Combine(TPath.Combine(ExecutableDir, '..'), 'catalogs'),
    TPath.Combine(GetCurrentDir, 'catalogs')
  ];

  Candidates := [];
  for Root in BaseRoots do
  begin
    // Top-level first — fast path for the 46 cars/trucks.
    Candidates := Candidates + [TPath.Combine(Root, FileName)];
    // Then each vehicle-class subdirectory, so a Phase B catalog
    // (e.g. ducati.json) is found at catalogs/motorcycle/ducati.json.
    for Sub in VehicleClassSubdirs do
      Candidates := Candidates + [
        TPath.Combine(TPath.Combine(Root, Sub), FileName)];
  end;

  for Candidate in Candidates do
    if TFile.Exists(Candidate) then
      Exit(TPath.GetFullPath(Candidate));

  Result := '';
end;

//------------------------------------------------------------------------------
// MERGE DIDS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// MERGE ROUTINES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// MERGE ECUS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// MERGE CATALOG JSON
//------------------------------------------------------------------------------
procedure MergeCatalogJSON(const FileName: string;
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>);
var
  Discard: TArray<TOBDOEMECU>;
begin
  Discard := nil;
  MergeCatalogJSON(FileName, DIDs, Routines, Discard);
end;

//------------------------------------------------------------------------------
// MERGE CATALOG JSON
//------------------------------------------------------------------------------
procedure MergeCatalogJSON(const FileName: string;
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
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

//==============================================================================
// v3.29 Phase A — extended-catalog merge
//==============================================================================

//------------------------------------------------------------------------------
// CONVERT CODING BLOCK
//------------------------------------------------------------------------------
function ConvertCodingBlock(const E: TOBDCodingBlockEntry): TOBDOEMCodingBlock;
var
  I: Integer;
  Field: TOBDCodingField;
  FE: TOBDCodingFieldEntry;
begin
  Result := Default(TOBDOEMCodingBlock);
  Result.DataIdentifier := E.DataIdentifier;
  Result.Name := E.Name;
  Result.Description := E.Description;
  Result.EcuAddress := E.EcuAddress;
  Result.PayloadSize := E.PayloadSize;
  SetLength(Result.Fields, Length(E.Fields));
  for I := 0 to High(E.Fields) do
  begin
    FE := E.Fields[I];
    Field := Default(TOBDCodingField);
    Field.Name := FE.Name;
    Field.Label_ := FE.Label_;
    Field.Description := FE.Description;
    Field.Kind := ParseCodingFieldKind(FE.KindStr);
    Field.ByteOffset := FE.ByteOffset;
    Field.BitOffset := FE.BitOffset;
    Field.BitWidth := FE.BitWidth;
    Field.DefaultValue := FE.DefaultValue;
    Field.DefaultAscii := FE.DefaultAscii;
    Field.MinValue := FE.MinValue;
    Field.MaxValue := FE.MaxValue;
    Field.EnumValues := FE.EnumValues;
    Result.Fields[I] := Field;
  end;
end;

//------------------------------------------------------------------------------
// CONVERT ADAPTATION
//------------------------------------------------------------------------------
function ConvertAdaptation(const E: TOBDAdaptationEntry): TOBDOEMAdaptation;
begin
  Result := Default(TOBDOEMAdaptation);
  Result.Channel := E.Channel;
  Result.Name := E.Name;
  Result.Description := E.Description;
  Result.EcuAddress := E.EcuAddress;
  Result.Kind := ParseAdaptationKind(E.KindStr);
  Result.MinValue := E.MinValue;
  Result.MaxValue := E.MaxValue;
  Result.DefaultValue := E.DefaultValue;
  Result.Unit_ := E.Unit_;
  Result.EnumValues := E.EnumValues;
end;

//------------------------------------------------------------------------------
// CONVERT ACTUATOR TEST
//------------------------------------------------------------------------------
function ConvertActuatorTest(const E: TOBDActuatorTestEntry): TOBDOEMActuatorTest;
begin
  Result := Default(TOBDOEMActuatorTest);
  Result.Identifier := E.Identifier;
  Result.Name := E.Name;
  Result.Description := E.Description;
  Result.EcuAddress := E.EcuAddress;
  Result.DurationMs := E.DurationMs;
  Result.SafetyWarning := E.SafetyWarning;
  Result.ExpectedResponseKind := ParseActuatorResponseKind(E.ExpectedResponseKind);
  Result.ExpectedResponseLabel := E.ExpectedResponseLabel;
end;

//------------------------------------------------------------------------------
// CONVERT LIVE PID
//------------------------------------------------------------------------------
function ConvertLivePID(const E: TOBDLivePIDEntry): TOBDOEMLivePID;
begin
  Result := Default(TOBDOEMLivePID);
  Result.Mode := ParseLivePIDMode(E.Mode);
  Result.PID := E.PID;
  Result.Name := E.Name;
  Result.Description := E.Description;
  Result.EcuAddress := E.EcuAddress;
  Result.FrameOffset := E.FrameOffset;
  Result.DecoderKind := ParseOEMDecoderKind(E.DecoderKindStr);
  Result.Scale := E.Scale;
  Result.Offset := E.Offset;
  Result.Unit_ := E.Unit_;
end;

//------------------------------------------------------------------------------
// CONVERT DTC EXTENDED
//------------------------------------------------------------------------------
function ConvertDtcExtended(const E: TOBDDtcExtendedDataEntry): TOBDDtcExtendedDataRecord;
begin
  Result := Default(TOBDDtcExtendedDataRecord);
  Result.DtcCode := E.DtcCode;
  Result.RecordNumber := E.RecordNumber;
  Result.Kind := ParseDtcExtendedKind(E.KindStr);
  Result.Description := E.Description;
  Result.DecoderKind := ParseOEMDecoderKind(E.DecoderKindStr);
  Result.Scale := E.Scale;
  Result.Offset := E.Offset;
  Result.Unit_ := E.Unit_;
end;

//------------------------------------------------------------------------------
// MERGE CODING BLOCKS
//------------------------------------------------------------------------------
procedure MergeCodingBlocks(var Existing: TArray<TOBDOEMCodingBlock>;
  const Loaded: TArray<TOBDOEMCodingBlock>);
var I, J: Integer; Found: Boolean;
begin
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if Existing[J].DataIdentifier = Loaded[I].DataIdentifier then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then Existing := Existing + [Loaded[I]];
  end;
end;

//------------------------------------------------------------------------------
// MERGE ADAPTATIONS
//------------------------------------------------------------------------------
procedure MergeAdaptations(var Existing: TArray<TOBDOEMAdaptation>;
  const Loaded: TArray<TOBDOEMAdaptation>);
var I, J: Integer; Found: Boolean;
begin
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if (Existing[J].Channel = Loaded[I].Channel) and
         (Existing[J].EcuAddress = Loaded[I].EcuAddress) then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then Existing := Existing + [Loaded[I]];
  end;
end;

//------------------------------------------------------------------------------
// MERGE ACTUATOR TESTS
//------------------------------------------------------------------------------
procedure MergeActuatorTests(var Existing: TArray<TOBDOEMActuatorTest>;
  const Loaded: TArray<TOBDOEMActuatorTest>);
var I, J: Integer; Found: Boolean;
begin
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if (Existing[J].Identifier = Loaded[I].Identifier) and
         (Existing[J].EcuAddress = Loaded[I].EcuAddress) then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then Existing := Existing + [Loaded[I]];
  end;
end;

//------------------------------------------------------------------------------
// MERGE LIVE PIDS
//------------------------------------------------------------------------------
procedure MergeLivePIDs(var Existing: TArray<TOBDOEMLivePID>;
  const Loaded: TArray<TOBDOEMLivePID>);
var I, J: Integer; Found: Boolean;
begin
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if (Existing[J].Mode = Loaded[I].Mode) and
         (Existing[J].PID = Loaded[I].PID) and
         (Existing[J].EcuAddress = Loaded[I].EcuAddress) then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then Existing := Existing + [Loaded[I]];
  end;
end;

//------------------------------------------------------------------------------
// MERGE DTC EXTENDED
//------------------------------------------------------------------------------
procedure MergeDtcExtended(var Existing: TArray<TOBDDtcExtendedDataRecord>;
  const Loaded: TArray<TOBDDtcExtendedDataRecord>);
var I, J: Integer; Found: Boolean;
begin
  for I := 0 to High(Loaded) do
  begin
    Found := False;
    for J := 0 to High(Existing) do
      if (Existing[J].DtcCode = Loaded[I].DtcCode) and
         (Existing[J].RecordNumber = Loaded[I].RecordNumber) then
      begin
        Existing[J] := Loaded[I];
        Found := True;
        Break;
      end;
    if not Found then Existing := Existing + [Loaded[I]];
  end;
end;

//------------------------------------------------------------------------------
// MERGE EXTENDED CATALOG JSON
//------------------------------------------------------------------------------
procedure MergeExtendedCatalogJSON(const FileName: string;
  var
    CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var
    Adaptations: TArray<TOBDOEMAdaptation>;
  var
    ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var
    LivePIDs: TArray<TOBDOEMLivePID>;
  var
    DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
var
  Path: string;
  Catalog: TOBDOEMJSONCatalog;
  I: Integer;
  CB: TArray<TOBDOEMCodingBlock>;
  Ad: TArray<TOBDOEMAdaptation>;
  AT: TArray<TOBDOEMActuatorTest>;
  LP: TArray<TOBDOEMLivePID>;
  DX: TArray<TOBDDtcExtendedDataRecord>;
begin
  Path := ResolveCatalogPath(FileName);
  if Path = '' then Exit;
  try
    Catalog := TOBDOEMJSONCatalog.Create(Path);
    try
      SetLength(CB, Catalog.CodingBlockCount);
      for I := 0 to Catalog.CodingBlockCount - 1 do
        CB[I] := ConvertCodingBlock(Catalog.CodingBlock(I));
      MergeCodingBlocks(CodingBlocks, CB);

      SetLength(Ad, Catalog.AdaptationCount);
      for I := 0 to Catalog.AdaptationCount - 1 do
        Ad[I] := ConvertAdaptation(Catalog.Adaptation(I));
      MergeAdaptations(Adaptations, Ad);

      SetLength(AT, Catalog.ActuatorTestCount);
      for I := 0 to Catalog.ActuatorTestCount - 1 do
        AT[I] := ConvertActuatorTest(Catalog.ActuatorTest(I));
      MergeActuatorTests(ActuatorTests, AT);

      SetLength(LP, Catalog.LivePIDCount);
      for I := 0 to Catalog.LivePIDCount - 1 do
        LP[I] := ConvertLivePID(Catalog.LivePID(I));
      MergeLivePIDs(LivePIDs, LP);

      SetLength(DX, Catalog.DtcExtendedCount);
      for I := 0 to Catalog.DtcExtendedCount - 1 do
        DX[I] := ConvertDtcExtended(Catalog.DtcExtended(I));
      MergeDtcExtended(DtcExtended, DX);
    finally
      Catalog.Free;
    end;
  except
    // Same silent-fall-through policy as MergeCatalogJSON.
  end;
end;

//------------------------------------------------------------------------------
// VINMATCHES CATALOG
//------------------------------------------------------------------------------
function VINMatchesCatalog(const FileName, VIN: string): Boolean;
var
  Path, WMI, CatalogWMI: string;
  Catalog: TOBDOEMJSONCatalog;
  WMIs: TArray<string>;
begin
  Result := False;
  if Length(VIN) < 3 then Exit;
  WMI := UpperCase(Copy(VIN, 1, 3));
  Path := ResolveCatalogPath(FileName);
  if Path = '' then Exit;
  try
    Catalog := TOBDOEMJSONCatalog.Create(Path);
    try
      WMIs := Catalog.ApplicableWMIs;
      for CatalogWMI in WMIs do
        if UpperCase(CatalogWMI) = WMI then Exit(True);
    finally
      Catalog.Free;
    end;
  except
    // Silent fall-through.
  end;
end;

end.
