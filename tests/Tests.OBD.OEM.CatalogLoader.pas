//------------------------------------------------------------------------------
//  Tests.OBD.OEM.CatalogLoader
//
//  Coverage for the OEM catalogue / DTC-catalogue loaders:
//    - TOBDOEMJSONCatalog          (OBD.OEM.Catalog.JSON)
//    - SetCatalogSearchPath /
//      ResolveCatalogPath /
//      MergeCatalogJSON /
//      MergeExtendedCatalogJSON /
//      VINMatchesCatalog           (OBD.OEM.Catalog.Loader)
//    - TOBDDtcCatalog.LoadFromFile (OBD.OEM.DTC)
//    - MergeDtcCatalog             (OBD.OEM.DTC.Loader)
//
//  Each test writes its fixture to a temp folder and points the
//  loader at it via SetCatalogSearchPath, so the fixture is
//  self-contained and doesn't require a shipped catalogues
//  folder.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.CatalogLoader;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DUnitX.TestFramework,
  OBD.OEM.Types,
  OBD.OEM.DTC,
  OBD.OEM.DTC.Loader,
  OBD.OEM.Catalog.JSON,
  OBD.OEM.Catalog.Loader;

type
  /// <summary>DUnitX fixture for the JSON catalogue loaders.</summary>
  [TestFixture]
  TOEMCatalogLoaderTests = class
  strict private
    FTempRoot: string;
    procedure WriteFile(const ARelativePath, AContent: string);
    function SampleCatalogJSON: string;
    function SampleDtcJSON: string;
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;

    // ---- TOBDOEMJSONCatalog ----
    [Test] procedure JSON_ParsesHeaderFields;
    [Test] procedure JSON_LoadsDIDsAndRoutines;
    [Test] procedure JSON_LoadsECUs;
    [Test] procedure JSON_LoadsCodingBlocks;
    [Test] procedure JSON_LoadsAdaptations;
    [Test] procedure JSON_LoadsActuatorTests;
    [Test] procedure JSON_LoadsLivePIDs;
    [Test] procedure JSON_LoadsDtcExtended;
    [Test] procedure JSON_DecodePayloadUInt16;
    [Test] procedure JSON_DecodePayloadEnum;
    [Test] procedure JSON_AsBaseDIDsAndRoutinesAndECUs;
    [Test] procedure JSON_FileNotFoundRaises;
    [Test] procedure JSON_NonObjectRootRaises;

    // ---- ResolveCatalogPath ----
    [Test] procedure Resolve_FindsViaSearchPathOverride;
    [Test] procedure Resolve_FindsInVehicleClassSubdir;
    [Test] procedure Resolve_MissingReturnsEmpty;

    // ---- MergeCatalogJSON ----
    [Test] procedure Merge_AppendsAndReplacesByPrimaryKey;
    [Test] procedure Merge_PreservesPrelodadedECUs;
    [Test] procedure Merge_NoFileIsNoOp;

    // ---- MergeExtendedCatalogJSON ----
    [Test] procedure MergeExt_PopulatesCodingBlocksAdaptations;

    // ---- VINMatchesCatalog ----
    [Test] procedure VINMatches_FirstThreeCharsAreCaseInsensitive;
    [Test] procedure VINMatches_NonMatchingReturnsFalse;
    [Test] procedure VINMatches_TooShortVinReturnsFalse;

    // ---- DTC loader ----
    [Test] procedure DtcLoader_LoadsFromJSON;
    [Test] procedure DtcLoader_MissingFileNoOps;
    [Test] procedure DtcLoader_MalformedFileRaises;

    // ---- Parse helpers ----
    [Test] procedure Parse_DecoderKindMapping;
    [Test] procedure Parse_CodingFieldKindMapping;
    [Test] procedure Parse_LivePIDModeMapping;
  end;

implementation

{ TOEMCatalogLoaderTests }

procedure TOEMCatalogLoaderTests.SetUp;
begin
  FTempRoot := TPath.Combine(TPath.GetTempPath,
    'oem-catalog-tests-' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempRoot);
  SetCatalogSearchPath(FTempRoot);
end;

procedure TOEMCatalogLoaderTests.TearDown;
begin
  SetCatalogSearchPath('');
  if (FTempRoot <> '') and TDirectory.Exists(FTempRoot) then
    TDirectory.Delete(FTempRoot, True);
end;

procedure TOEMCatalogLoaderTests.WriteFile(
  const ARelativePath, AContent: string);
var
  FullPath, DirName: string;
begin
  FullPath := TPath.Combine(FTempRoot, ARelativePath);
  DirName := TPath.GetDirectoryName(FullPath);
  if not TDirectory.Exists(DirName) then
    TDirectory.CreateDirectory(DirName);
  TFile.WriteAllText(FullPath, AContent, TEncoding.UTF8);
end;

function TOEMCatalogLoaderTests.SampleCatalogJSON: string;
begin
  Result :=
    '{' +
    '  "version": 1,' +
    '  "manufacturer_key": "VAG",' +
    '  "display_name": "Volkswagen Audi Group",' +
    '  "applicable_wmis": ["WVW", "WAU", "WVG"],' +
    '  "default_source": "ODIS",' +
    '  "default_ecu_address": "0x7E0",' +
    '  "ecus": [' +
    '    { "address": "0x7E0", "name": "engine", "common_name": "Engine ECM" },' +
    '    { "address": "0x7E2", "name": "trans",  "common_name": "Trans" }' +
    '  ],' +
    '  "dids": [' +
    '    { "did": "0xF190", "name": "vin", "description": "VIN",' +
    '      "ecu_address": "0", "source": "ISO 14229", "verified": true },' +
    '    { "did": "0x1234", "name": "battery_voltage",' +
    '      "description": "Battery", "source": "ODIS", "verified": true,' +
    '      "decoder": { "kind": "uint16_be", "scale": 0.001, "unit": "V" } },' +
    '    { "did": "0x5678", "name": "fuel_state",' +
    '      "description": "Fuel state", "verified": false,' +
    '      "decoder": { "kind": "enum", "size": 1, "values": {' +
    '        "0": "empty", "1": "low", "2": "ok" } } }' +
    '  ],' +
    '  "routines": [' +
    '    { "id": "0x0203", "name": "erase_memory",' +
    '      "description": "Erase memory", "ecu_address": "0x7E0" }' +
    '  ],' +
    '  "coding_blocks": [' +
    '    { "did": "0xF1F0", "name": "comfort",' +
    '      "description": "Comfort coding", "payload_size": 4,' +
    '      "fields": [' +
    '        { "name": "auto_lock", "label": "Auto Lock", "kind": "bit",' +
    '          "byte_offset": 0, "bit_offset": 0, "bit_width": 1,' +
    '          "default": 1, "min": 0, "max": 1 }' +
    '      ] }' +
    '  ],' +
    '  "adaptations": [' +
    '    { "channel": "0x05", "name": "idle_offset",' +
    '      "description": "Idle offset", "kind": "int16_be",' +
    '      "min": -1000, "max": 1000, "default": 0, "unit": "rpm" }' +
    '  ],' +
    '  "actuator_tests": [' +
    '    { "id": "0x4001", "name": "fuel_pump_relay",' +
    '      "description": "Fuel pump relay test",' +
    '      "duration_ms": 5000, "safety_warning": "Ignition on, engine off",' +
    '      "response_kind": "boolean", "response_label": "Energised" }' +
    '  ],' +
    '  "live_pids": [' +
    '    { "mode": "service22", "pid": "0x1234",' +
    '      "name": "boost_pressure", "ecu_address": "0x7E0",' +
    '      "frame_offset": 3,' +
    '      "decoder": { "kind": "uint16_be", "scale": 0.01, "unit": "bar" } }' +
    '  ],' +
    '  "dtc_extended_data": [' +
    '    { "code": "P0420", "record": "0x08",' +
    '      "kind": "occurrence_counter",' +
    '      "description": "Occurrence counter",' +
    '      "decoder": { "kind": "uint8" } }' +
    '  ]' +
    '}';
end;

function TOEMCatalogLoaderTests.SampleDtcJSON: string;
begin
  Result :=
    '{' +
    '  "dtcs": [' +
    '    { "code": "P0420", "description": "Catalyst",' +
    '      "notes": "Check upstream sensor" },' +
    '    { "code": "P0171", "description": "System Lean (Bank 1)" }' +
    '  ]' +
    '}';
end;

{ ---- TOBDOEMJSONCatalog ------------------------------------------------- }

procedure TOEMCatalogLoaderTests.JSON_ParsesHeaderFields;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(1, Cat.Version);
    Assert.AreEqual('VAG', Cat.ManufacturerKey);
    Assert.AreEqual('Volkswagen Audi Group', Cat.DisplayName);
    Assert.AreEqual(3, Length(Cat.ApplicableWMIs));
    Assert.AreEqual('WVW', Cat.ApplicableWMIs[0]);
    Assert.AreEqual('ODIS', Cat.DefaultSource);
    Assert.AreEqual($7E0, Integer(Cat.DefaultEcuAddress));
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_LoadsDIDsAndRoutines;
var
  Cat: TOBDOEMJSONCatalog;
  Entry: TOBDOEMDIDEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(3, Cat.DIDCount);
    Assert.IsTrue(Cat.FindDID($1234, Entry));
    Assert.AreEqual('battery_voltage', Entry.Name);
    Assert.AreEqual('ODIS', Entry.Source);
    Assert.IsTrue(Entry.Verified);
    Assert.AreEqual($7E0, Integer(Entry.EcuAddress)); // default_ecu_address
    Assert.IsTrue(Cat.FindDID($F190, Entry));
    Assert.AreEqual(0, Integer(Entry.EcuAddress));    // explicit 0 wins
    Assert.AreEqual(1, Cat.RoutineCount);
    Assert.AreEqual($0203, Integer(Cat.Routine(0).Identifier));
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_LoadsECUs;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(2, Cat.ECUCount);
    Assert.AreEqual('engine', Cat.ECU(0).Name);
    Assert.AreEqual('Engine ECM', Cat.ECU(0).CommonName);
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_LoadsCodingBlocks;
var
  Cat: TOBDOEMJSONCatalog;
  Block: TOBDCodingBlockEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(1, Cat.CodingBlockCount);
    Block := Cat.CodingBlock(0);
    Assert.AreEqual($F1F0, Integer(Block.DataIdentifier));
    Assert.AreEqual(4, Block.PayloadSize);
    Assert.AreEqual(1, Length(Block.Fields));
    Assert.AreEqual('auto_lock', Block.Fields[0].Name);
    Assert.AreEqual('Auto Lock', Block.Fields[0].Label_);
    Assert.AreEqual('bit', Block.Fields[0].KindStr);
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_LoadsAdaptations;
var
  Cat: TOBDOEMJSONCatalog;
  A: TOBDAdaptationEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(1, Cat.AdaptationCount);
    A := Cat.Adaptation(0);
    Assert.AreEqual($05, Integer(A.Channel));
    Assert.AreEqual('idle_offset', A.Name);
    Assert.AreEqual<Int64>(-1000, A.MinValue);
    Assert.AreEqual<Int64>(1000, A.MaxValue);
    Assert.AreEqual('rpm', A.Unit_);
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_LoadsActuatorTests;
var
  Cat: TOBDOEMJSONCatalog;
  T: TOBDActuatorTestEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(1, Cat.ActuatorTestCount);
    T := Cat.ActuatorTest(0);
    Assert.AreEqual($4001, Integer(T.Identifier));
    Assert.AreEqual<Cardinal>(5000, T.DurationMs);
    Assert.AreEqual('Ignition on, engine off', T.SafetyWarning);
    Assert.AreEqual('boolean', T.ExpectedResponseKind);
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_LoadsLivePIDs;
var
  Cat: TOBDOEMJSONCatalog;
  L: TOBDLivePIDEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(1, Cat.LivePIDCount);
    L := Cat.LivePID(0);
    Assert.AreEqual('service22', L.Mode);
    Assert.AreEqual($1234, Integer(L.PID));
    Assert.AreEqual(3, L.FrameOffset);
    Assert.AreEqual(0.01, L.Scale, 1E-9);
    Assert.AreEqual('bar', L.Unit_);
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_LoadsDtcExtended;
var
  Cat: TOBDOEMJSONCatalog;
  D: TOBDDtcExtendedDataEntry;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual(1, Cat.DtcExtendedCount);
    D := Cat.DtcExtended(0);
    Assert.AreEqual('P0420', D.DtcCode);
    Assert.AreEqual($08, Integer(D.RecordNumber));
    Assert.AreEqual('occurrence_counter', D.KindStr);
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_DecodePayloadUInt16;
var
  Cat: TOBDOEMJSONCatalog;
  S: string;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    // 0x3000 * 0.001 = 12.288 V
    S := Cat.DecodePayload($1234, TBytes.Create($30, $00));
    Assert.Contains(S, '12.288');
    Assert.Contains(S, 'V');
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_DecodePayloadEnum;
var
  Cat: TOBDOEMJSONCatalog;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    Assert.AreEqual('low', Cat.DecodePayload($5678, TBytes.Create($01)));
    Assert.AreEqual('ok',  Cat.DecodePayload($5678, TBytes.Create($02)));
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_AsBaseDIDsAndRoutinesAndECUs;
var
  Cat: TOBDOEMJSONCatalog;
  DIDs: TArray<TOBDOEMDataIdentifier>;
  Routines: TArray<TOBDOEMRoutine>;
  ECUs: TArray<TOBDOEMECU>;
begin
  Cat := TOBDOEMJSONCatalog.CreateFromText(SampleCatalogJSON);
  try
    DIDs := Cat.AsBaseDIDs;
    Routines := Cat.AsBaseRoutines;
    ECUs := Cat.AsBaseECUs;
    Assert.AreEqual(3, Length(DIDs));
    Assert.AreEqual(1, Length(Routines));
    Assert.AreEqual(2, Length(ECUs));
    Assert.AreEqual($F190, Integer(DIDs[0].DID));
    Assert.AreEqual($7E0, Integer(ECUs[0].Address));
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.JSON_FileNotFoundRaises;
begin
  Assert.WillRaise(
    procedure
    var
      Cat: TOBDOEMJSONCatalog;
    begin
      Cat := TOBDOEMJSONCatalog.Create(
        TPath.Combine(FTempRoot, 'nope.json'));
      Cat.Free;
    end,
    EOBDCatalogError);
end;

procedure TOEMCatalogLoaderTests.JSON_NonObjectRootRaises;
begin
  Assert.WillRaise(
    procedure
    var
      Cat: TOBDOEMJSONCatalog;
    begin
      Cat := TOBDOEMJSONCatalog.CreateFromText('[]');
      Cat.Free;
    end,
    EOBDCatalogError);
end;

{ ---- ResolveCatalogPath ------------------------------------------------- }

procedure TOEMCatalogLoaderTests.Resolve_FindsViaSearchPathOverride;
var
  Path: string;
begin
  WriteFile('test.json', SampleCatalogJSON);
  Path := ResolveCatalogPath('test.json');
  Assert.IsTrue(TFile.Exists(Path));
end;

procedure TOEMCatalogLoaderTests.Resolve_FindsInVehicleClassSubdir;
var
  Path: string;
begin
  WriteFile(TPath.Combine('motorcycle', 'ducati.json'), SampleCatalogJSON);
  Path := ResolveCatalogPath('ducati.json');
  Assert.IsTrue(TFile.Exists(Path));
  Assert.Contains(Path, 'motorcycle');
end;

procedure TOEMCatalogLoaderTests.Resolve_MissingReturnsEmpty;
begin
  Assert.AreEqual('', ResolveCatalogPath('does-not-exist.json'));
end;

{ ---- MergeCatalogJSON --------------------------------------------------- }

procedure TOEMCatalogLoaderTests.Merge_AppendsAndReplacesByPrimaryKey;
var
  DIDs: TArray<TOBDOEMDataIdentifier>;
  Routines: TArray<TOBDOEMRoutine>;
  PreLoaded: TOBDOEMDataIdentifier;
begin
  WriteFile('vw.json', SampleCatalogJSON);
  PreLoaded := Default(TOBDOEMDataIdentifier);
  PreLoaded.DID := $1234;
  PreLoaded.Name := 'will_be_replaced';
  DIDs := [PreLoaded];
  Routines := nil;
  MergeCatalogJSON('vw.json', DIDs, Routines);
  Assert.AreEqual(3, Length(DIDs));
  Assert.AreEqual('battery_voltage', DIDs[0].Name); // replaced
  Assert.AreEqual(1, Length(Routines));
end;

procedure TOEMCatalogLoaderTests.Merge_PreservesPrelodadedECUs;
var
  DIDs: TArray<TOBDOEMDataIdentifier>;
  Routines: TArray<TOBDOEMRoutine>;
  ECUs: TArray<TOBDOEMECU>;
  Pre: TOBDOEMECU;
begin
  WriteFile('vw.json', SampleCatalogJSON);
  Pre.Address := $7E1;
  Pre.Name := 'abs';
  Pre.CommonName := 'ABS';
  ECUs := [Pre];
  DIDs := nil;
  Routines := nil;
  MergeCatalogJSON('vw.json', DIDs, Routines, ECUs);
  Assert.AreEqual(3, Length(ECUs));                 // 1 pre + 2 from JSON
end;

procedure TOEMCatalogLoaderTests.Merge_NoFileIsNoOp;
var
  DIDs: TArray<TOBDOEMDataIdentifier>;
  Routines: TArray<TOBDOEMRoutine>;
begin
  DIDs := nil;
  Routines := nil;
  MergeCatalogJSON('missing.json', DIDs, Routines);
  Assert.AreEqual(0, Length(DIDs));
  Assert.AreEqual(0, Length(Routines));
end;

{ ---- MergeExtendedCatalogJSON ------------------------------------------- }

procedure TOEMCatalogLoaderTests.MergeExt_PopulatesCodingBlocksAdaptations;
var
  CB: TArray<TOBDOEMCodingBlock>;
  Ad: TArray<TOBDOEMAdaptation>;
  AT: TArray<TOBDOEMActuatorTest>;
  LP: TArray<TOBDOEMLivePID>;
  DX: TArray<TOBDDtcExtendedDataRecord>;
begin
  WriteFile('vw.json', SampleCatalogJSON);
  CB := nil; Ad := nil; AT := nil; LP := nil; DX := nil;
  MergeExtendedCatalogJSON('vw.json', CB, Ad, AT, LP, DX);
  Assert.AreEqual(1, Length(CB));
  Assert.AreEqual($F1F0, Integer(CB[0].DataIdentifier));
  Assert.AreEqual(1, Length(Ad));
  Assert.AreEqual($05, Integer(Ad[0].Channel));
  Assert.AreEqual(Ord(adkInt16BE), Ord(Ad[0].Kind));
  Assert.AreEqual(1, Length(AT));
  Assert.AreEqual(Ord(arkBoolean), Ord(AT[0].ExpectedResponseKind));
  Assert.AreEqual(1, Length(LP));
  Assert.AreEqual(Ord(lpmService22), Ord(LP[0].Mode));
  Assert.AreEqual(1, Length(DX));
  Assert.AreEqual(Ord(xdkOccurrenceCounter), Ord(DX[0].Kind));
end;

{ ---- VINMatchesCatalog -------------------------------------------------- }

procedure TOEMCatalogLoaderTests.VINMatches_FirstThreeCharsAreCaseInsensitive;
begin
  WriteFile('vw.json', SampleCatalogJSON);
  Assert.IsTrue(VINMatchesCatalog('vw.json', 'WVWZZZ1KZAW123456'));
  Assert.IsTrue(VINMatchesCatalog('vw.json', 'wauzzz4G0DN098765'));
end;

procedure TOEMCatalogLoaderTests.VINMatches_NonMatchingReturnsFalse;
begin
  WriteFile('vw.json', SampleCatalogJSON);
  Assert.IsFalse(VINMatchesCatalog('vw.json', 'WBAFR9C57DC123456'));
end;

procedure TOEMCatalogLoaderTests.VINMatches_TooShortVinReturnsFalse;
begin
  WriteFile('vw.json', SampleCatalogJSON);
  Assert.IsFalse(VINMatchesCatalog('vw.json', 'WV'));
end;

{ ---- DTC loader --------------------------------------------------------- }

procedure TOEMCatalogLoaderTests.DtcLoader_LoadsFromJSON;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  WriteFile('dtc-vw.json', SampleDtcJSON);
  Cat := TOBDDtcCatalog.Create;
  try
    MergeDtcCatalog('dtc-vw.json', Cat);
    Assert.AreEqual(2, Cat.Count);
    Assert.IsTrue(Cat.FindByCode('P0420', Entry));
    Assert.AreEqual('Catalyst', Entry.Description);
    Assert.AreEqual('Check upstream sensor', Entry.Notes);
    Assert.IsTrue(Cat.FindByCode('P0171', Entry));
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.DtcLoader_MissingFileNoOps;
var
  Cat: TOBDDtcCatalog;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    MergeDtcCatalog('missing-dtc.json', Cat);
    Assert.AreEqual(0, Cat.Count);
  finally
    Cat.Free;
  end;
end;

procedure TOEMCatalogLoaderTests.DtcLoader_MalformedFileRaises;
var
  Cat: TOBDDtcCatalog;
begin
  WriteFile('bad.json', 'not json');
  Cat := TOBDDtcCatalog.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        Cat.LoadFromFile(TPath.Combine(FTempRoot, 'bad.json'));
      end,
      EOBDDtcCatalog);
  finally
    Cat.Free;
  end;
end;

{ ---- Parse helpers ------------------------------------------------------ }

procedure TOEMCatalogLoaderTests.Parse_DecoderKindMapping;
begin
  Assert.AreEqual(Ord(TOBDOEMDecoderKind.dkAscii),
    Ord(ParseOEMDecoderKind('ascii')));
  Assert.AreEqual(Ord(TOBDOEMDecoderKind.dkUInt16BE),
    Ord(ParseOEMDecoderKind('UINT16_BE')));
  Assert.AreEqual(Ord(TOBDOEMDecoderKind.dkUnknown),
    Ord(ParseOEMDecoderKind('nope')));
end;

procedure TOEMCatalogLoaderTests.Parse_CodingFieldKindMapping;
begin
  Assert.AreEqual(Ord(cfkBit), Ord(ParseCodingFieldKind('bit')));
  Assert.AreEqual(Ord(cfkEnum), Ord(ParseCodingFieldKind('ENUM')));
  Assert.AreEqual(Ord(cfkUnknown), Ord(ParseCodingFieldKind('nope')));
end;

procedure TOEMCatalogLoaderTests.Parse_LivePIDModeMapping;
begin
  Assert.AreEqual(Ord(lpmService01), Ord(ParseLivePIDMode('service01')));
  Assert.AreEqual(Ord(lpmService22), Ord(ParseLivePIDMode('SERVICE22')));
  Assert.AreEqual(Ord(lpmUnknown),   Ord(ParseLivePIDMode('nope')));
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMCatalogLoaderTests);

end.
