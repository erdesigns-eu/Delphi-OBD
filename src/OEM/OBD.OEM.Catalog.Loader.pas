//------------------------------------------------------------------------------
//  OBD.OEM.Catalog.Loader
//
//  Bridge that lets OEM extensions load their JSON catalogue from
//  disk and merge it with their hard-coded starter entries.
//  Each extension's <c>BuildCatalog</c> calls
//  <c>MergeCatalogJSON('vw.json', DIDs, Routines)</c> after
//  populating its hard-coded fallback. JSON entries win on
//  conflict (same DID), so user-supplied catalogues override the
//  framework defaults.
//
//  Catalogue search path:
//
//    1. user override via <see cref="SetCatalogSearchPath"/>
//    2. <c>exe-dir / catalogs / &lt;name&gt;</c>
//    3. <c>exe-dir / .. / catalogs / &lt;name&gt;</c>
//    4. <c>CWD     / catalogs / &lt;name&gt;</c>
//
//  Each root is probed at the top level first (fast path for
//  cars / trucks) and then under the vehicle-class subdirectories
//  <c>motorcycle</c> / <c>agricultural</c> / <c>marine</c> /
//  <c>powersports</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Catalog.Loader;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  OBD.OEM.Types,
  OBD.OEM.Extensions,
  OBD.OEM.Catalog.JSON;

/// <summary>
///   Overrides the catalogue search path. Pass an empty string to
///   revert to the default exe-relative search.
/// </summary>
/// <param name="Path">Custom search root.</param>
procedure SetCatalogSearchPath(const Path: string);

/// <summary>
///   Resolves a catalogue file by name through the search path.
///   Returns an empty string when no candidate exists.
/// </summary>
/// <param name="FileName">Catalogue file name.</param>
function ResolveCatalogPath(const FileName: string): string;

/// <summary>
///   Loads <c>FileName</c> and merges its entries into <c>DIDs</c>
///   + <c>Routines</c>. JSON entries take precedence over
///   already-present hard-coded entries with the same DID /
///   identifier. Silently no-ops when the file isn't found.
/// </summary>
procedure MergeCatalogJSON(const FileName: string;
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>); overload;

/// <summary>
///   Same as the two-argument overload but additionally merges any
///   <c>ecus</c> array from the JSON into the supplied ECU map.
///   Existing ECUs (matched by Address) are replaced; new ones are
///   appended.
/// </summary>
procedure MergeCatalogJSON(const FileName: string;
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>); overload;

/// <summary>
///   Loads the extended-catalogue sections (coding blocks,
///   adaptations, actuator tests, live PIDs, DTC extended-data
///   records) from <c>FileName</c> and merges them. Replace-by-key
///   semantics: coding-blocks match by DID, adaptations by
///   (channel, ECU), actuator tests by (id, ECU), live PIDs by
///   (mode, PID, ECU), DTC extended records by (code, record).
///   Silently no-ops when the file isn't found.
/// </summary>
procedure MergeExtendedCatalogJSON(const FileName: string;
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);

/// <summary>
///   JSON-driven VIN routing. Returns <c>True</c> when the
///   catalogue's <c>applicable_wmis</c> array contains
///   <c>Copy(VIN, 1, 3)</c> (case-insensitive).
/// </summary>
/// <param name="FileName">Catalogue file name.</param>
/// <param name="VIN">17-character VIN.</param>
function VINMatchesCatalog(const FileName, VIN: string): Boolean;

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
const
  VehicleClassSubdirs: array[0..3] of string =
    ('motorcycle', 'agricultural', 'marine', 'powersports');
var
  Candidates: TArray<string>;
  BaseRoots: TArray<string>;
  Candidate, Sub, Root: string;
begin
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
    Candidates := Candidates + [TPath.Combine(Root, FileName)];
    for Sub in VehicleClassSubdirs do
      Candidates := Candidates + [
        TPath.Combine(TPath.Combine(Root, Sub), FileName)];
  end;

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
  if Path = '' then
    Exit;
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
    // A malformed catalogue must not break application startup;
    // production deployments validate catalogues at build time.
  end;
end;

function ConvertCodingBlock(
  const E: TOBDCodingBlockEntry): TOBDOEMCodingBlock;
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

function ConvertAdaptation(
  const E: TOBDAdaptationEntry): TOBDOEMAdaptation;
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

function ConvertActuatorTest(
  const E: TOBDActuatorTestEntry): TOBDOEMActuatorTest;
begin
  Result := Default(TOBDOEMActuatorTest);
  Result.Identifier := E.Identifier;
  Result.Name := E.Name;
  Result.Description := E.Description;
  Result.EcuAddress := E.EcuAddress;
  Result.DurationMs := E.DurationMs;
  Result.SafetyWarning := E.SafetyWarning;
  Result.ExpectedResponseKind :=
    ParseActuatorResponseKind(E.ExpectedResponseKind);
  Result.ExpectedResponseLabel := E.ExpectedResponseLabel;
end;

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

function ConvertDtcExtended(
  const E: TOBDDtcExtendedDataEntry): TOBDDtcExtendedDataRecord;
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

procedure MergeCodingBlocks(var Existing: TArray<TOBDOEMCodingBlock>;
  const Loaded: TArray<TOBDOEMCodingBlock>);
var
  I, J: Integer;
  Found: Boolean;
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
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeAdaptations(var Existing: TArray<TOBDOEMAdaptation>;
  const Loaded: TArray<TOBDOEMAdaptation>);
var
  I, J: Integer;
  Found: Boolean;
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
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeActuatorTests(
  var Existing: TArray<TOBDOEMActuatorTest>;
  const Loaded: TArray<TOBDOEMActuatorTest>);
var
  I, J: Integer;
  Found: Boolean;
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
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeLivePIDs(var Existing: TArray<TOBDOEMLivePID>;
  const Loaded: TArray<TOBDOEMLivePID>);
var
  I, J: Integer;
  Found: Boolean;
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
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeDtcExtended(
  var Existing: TArray<TOBDDtcExtendedDataRecord>;
  const Loaded: TArray<TOBDDtcExtendedDataRecord>);
var
  I, J: Integer;
  Found: Boolean;
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
    if not Found then
      Existing := Existing + [Loaded[I]];
  end;
end;

procedure MergeExtendedCatalogJSON(const FileName: string;
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
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
  if Path = '' then
    Exit;
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
    // Same silent fall-through as MergeCatalogJSON.
  end;
end;

function VINMatchesCatalog(const FileName, VIN: string): Boolean;
var
  Path, WMI, CatalogWMI: string;
  Catalog: TOBDOEMJSONCatalog;
  WMIs: TArray<string>;
begin
  Result := False;
  if Length(VIN) < 3 then
    Exit;
  WMI := UpperCase(Copy(VIN, 1, 3));
  Path := ResolveCatalogPath(FileName);
  if Path = '' then
    Exit;
  try
    Catalog := TOBDOEMJSONCatalog.Create(Path);
    try
      WMIs := Catalog.ApplicableWMIs;
      for CatalogWMI in WMIs do
        if UpperCase(CatalogWMI) = WMI then
          Exit(True);
    finally
      Catalog.Free;
    end;
  except
    // Silent fall-through.
  end;
end;

end.
