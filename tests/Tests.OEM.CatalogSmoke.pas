//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.CatalogSmoke
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Catalog-load smoke tests. Cycles through every
//                  shipped JSON catalog file in `catalogs/` and
//                  asserts:
//                    1. The file parses without raising.
//                    2. It declares a non-empty manufacturer_key.
//                    3. It has at least one DID OR one routine.
//                  This is the regression guard that catches a
//                  trailing-comma typo or a bad decoder kind in
//                  a catalog before tagging.
//------------------------------------------------------------------------------
unit Tests.OEM.CatalogSmoke;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCatalogLoadSmokeTests = class
  public
    /// <summary>Universal u d s catalog loads.</summary>
    [Test] procedure UniversalUDSCatalogLoads;
    /// <summary>Universal o b d pids catalog loads.</summary>
    [Test] procedure UniversalOBDPidsCatalogLoads;
    /// <summary>V w catalog loads.</summary>
    [Test] procedure VWCatalogLoads;
    /// <summary>B m w catalog loads.</summary>
    [Test] procedure BMWCatalogLoads;
    /// <summary>Mercedes catalog loads.</summary>
    [Test] procedure MercedesCatalogLoads;
    /// <summary>Ford catalog loads.</summary>
    [Test] procedure FordCatalogLoads;
    /// <summary>G m catalog loads.</summary>
    [Test] procedure GMCatalogLoads;
    /// <summary>Stellantis catalog loads.</summary>
    [Test] procedure StellantisCatalogLoads;
    /// <summary>Toyota catalog loads.</summary>
    [Test] procedure ToyotaCatalogLoads;
    /// <summary>Honda catalog loads.</summary>
    [Test] procedure HondaCatalogLoads;
    /// <summary>H m g catalog loads.</summary>
    [Test] procedure HMGCatalogLoads;
    /// <summary>Nissan catalog loads.</summary>
    [Test] procedure NissanCatalogLoads;
    /// <summary>Subaru catalog loads.</summary>
    [Test] procedure SubaruCatalogLoads;
    /// <summary>Mazda catalog loads.</summary>
    [Test] procedure MazdaCatalogLoads;
    /// <summary>Renault catalog loads.</summary>
    [Test] procedure RenaultCatalogLoads;
    /// <summary>Volvo catalog loads.</summary>
    [Test] procedure VolvoCatalogLoads;
    /// <summary>Tesla catalog loads.</summary>
    [Test] procedure TeslaCatalogLoads;
    /// <summary>Suzuki catalog loads.</summary>
    [Test] procedure SuzukiCatalogLoads;
    /// <summary>Mitsubishi catalog loads.</summary>
    [Test] procedure MitsubishiCatalogLoads;
    /// <summary>Cummins catalog loads.</summary>
    [Test] procedure CumminsCatalogLoads;
    /// <summary>Detroit catalog loads.</summary>
    [Test] procedure DetroitCatalogLoads;
    /// <summary>P a c c a r catalog loads.</summary>
    [Test] procedure PACCARCatalogLoads;
    /// <summary>Volvo trucks catalog loads.</summary>
    [Test] procedure VolvoTrucksCatalogLoads;
    /// <summary>Scania catalog loads.</summary>
    [Test] procedure ScaniaCatalogLoads;
    /// <summary>M a n catalog loads.</summary>
    [Test] procedure MANCatalogLoads;
    /// <summary>B y d catalog loads.</summary>
    [Test] procedure BYDCatalogLoads;
    /// <summary>Geely catalog loads.</summary>
    [Test] procedure GeelyCatalogLoads;
    /// <summary>N i o catalog loads.</summary>
    [Test] procedure NIOCatalogLoads;
    /// <summary>Xpeng catalog loads.</summary>
    [Test] procedure XpengCatalogLoads;
    /// <summary>G w m catalog loads.</summary>
    [Test] procedure GWMCatalogLoads;
    /// <summary>J l r catalog loads.</summary>
    [Test] procedure JLRCatalogLoads;
    /// <summary>Porsche catalog loads.</summary>
    [Test] procedure PorscheCatalogLoads;
    /// <summary>Polestar catalog loads.</summary>
    [Test] procedure PolestarCatalogLoads;
    /// <summary>M i n i catalog loads.</summary>
    [Test] procedure MINICatalogLoads;
    /// <summary>Smart catalog loads.</summary>
    [Test] procedure SmartCatalogLoads;
    /// <summary>Dacia catalog loads.</summary>
    [Test] procedure DaciaCatalogLoads;
    /// <summary>Lada catalog loads.</summary>
    [Test] procedure LadaCatalogLoads;
    /// <summary>Mahindra catalog loads.</summary>
    [Test] procedure MahindraCatalogLoads;
    /// <summary>Tata catalog loads.</summary>
    [Test] procedure TataCatalogLoads;
    /// <summary>Aston martin catalog loads.</summary>
    [Test] procedure AstonMartinCatalogLoads;
    /// <summary>Bentley catalog loads.</summary>
    [Test] procedure BentleyCatalogLoads;
    /// <summary>Rolls royce catalog loads.</summary>
    [Test] procedure RollsRoyceCatalogLoads;
    /// <summary>Ferrari catalog loads.</summary>
    [Test] procedure FerrariCatalogLoads;
    /// <summary>Mc laren catalog loads.</summary>
    [Test] procedure McLarenCatalogLoads;
    /// <summary>Rivian catalog loads.</summary>
    [Test] procedure RivianCatalogLoads;
    /// <summary>Lucid catalog loads.</summary>
    [Test] procedure LucidCatalogLoads;
    /// <summary>Isuzu catalog loads.</summary>
    [Test] procedure IsuzuCatalogLoads;
    /// <summary>Iveco catalog loads.</summary>
    [Test] procedure IvecoCatalogLoads;
    /// <summary>D t c i s o15031 catalog loads.</summary>
    [Test] procedure DTCISO15031CatalogLoads;
    /// <summary>Data-driven sweep: every <c>catalogs/*.json</c> that
    /// isn't a DTC, ISO standard, or test fixture must load without
    /// raising. Auto-picks up new catalogs (motorcycles, agricultural,
    /// marine, etc.) without test-file edits.</summary>
    [Test] procedure AllOEMCatalogsLoadFromDirectory;
    /// <summary>Every shipped <c>catalogs/dtc-*.json</c> must load and
    /// declare at least one DTC entry.</summary>
    [Test] procedure AllDtcCatalogsLoadFromDirectory;
    // -------- Phase B vehicle-class subdirectories --------
    /// <summary>All motorcycle catalogs load.</summary>
    [Test] procedure AllMotorcycleCatalogsLoad;
    /// <summary>All agricultural catalogs load.</summary>
    [Test] procedure AllAgriculturalCatalogsLoad;
    /// <summary>All marine catalogs load.</summary>
    [Test] procedure AllMarineCatalogsLoad;
    /// <summary>All powersports catalogs load.</summary>
    [Test] procedure AllPowersportsCatalogsLoad;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  OBD.OEM.Catalog.JSON, OBD.OEM.Catalog.Loader, OBD.OEM.DTC;

// G9 (closed): the local helper used to duplicate
// OBD.OEM.Catalog.Loader.ResolveCatalogPath. We now delegate to the
// loader's exported function so the test sees the same search path
// (including the v3.77 vehicle-class subdirectories) as production
// code does. Retained as a thin wrapper for the loader miss case
// where Tests want to surface a clearer skip message.
function LocateCatalogOrSkip(const FileName: string): string;
begin
  Result := ResolveCatalogPath(FileName);
  if Result = '' then
    Assert.Pass(Format('catalog %s not on path; skipping', [FileName]));
end;

procedure SmokeLoad(const FileName: string;
  const RequireManufacturerKey: Boolean = True);
var
  Cat: TOBDOEMJSONCatalog;
  Path: string;
begin
  Path := LocateCatalogOrSkip(FileName);
  Cat := TOBDOEMJSONCatalog.Create(Path);
  try
    if RequireManufacturerKey then
      Assert.IsTrue(Cat.ManufacturerKey <> '',
        'manufacturer_key required: ' + FileName);
    Assert.IsTrue((Cat.DIDCount > 0) or (Cat.RoutineCount > 0),
      'catalog must contribute at least one DID or routine: ' + FileName);
  finally
    Cat.Free;
  end;
end;

procedure SmokeLoadDtc(const FileName: string);
var
  Cat: TOBDDtcCatalog;
  Path: string;
begin
  Path := LocateCatalogOrSkip(FileName);
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromFile(Path);
    Assert.IsTrue(Cat.Count > 0,
      'DTC catalog must declare at least one entry: ' + FileName);
  finally
    Cat.Free;
  end;
end;

procedure TCatalogLoadSmokeTests.UniversalUDSCatalogLoads;
begin SmokeLoad('uds-standard.json', False); end;

procedure TCatalogLoadSmokeTests.UniversalOBDPidsCatalogLoads;
begin SmokeLoad('obd2-pids.json', False); end;

procedure TCatalogLoadSmokeTests.VWCatalogLoads;          begin SmokeLoad('vw.json'); end;
procedure TCatalogLoadSmokeTests.BMWCatalogLoads;         begin SmokeLoad('bmw.json'); end;
procedure TCatalogLoadSmokeTests.MercedesCatalogLoads;    begin SmokeLoad('mercedes.json'); end;
procedure TCatalogLoadSmokeTests.FordCatalogLoads;        begin SmokeLoad('ford.json'); end;
procedure TCatalogLoadSmokeTests.GMCatalogLoads;          begin SmokeLoad('gm.json'); end;
procedure TCatalogLoadSmokeTests.StellantisCatalogLoads;  begin SmokeLoad('stellantis.json'); end;
procedure TCatalogLoadSmokeTests.ToyotaCatalogLoads;      begin SmokeLoad('toyota.json'); end;
procedure TCatalogLoadSmokeTests.HondaCatalogLoads;       begin SmokeLoad('honda.json'); end;
procedure TCatalogLoadSmokeTests.HMGCatalogLoads;         begin SmokeLoad('hmg.json'); end;
procedure TCatalogLoadSmokeTests.NissanCatalogLoads;      begin SmokeLoad('nissan.json'); end;
procedure TCatalogLoadSmokeTests.SubaruCatalogLoads;      begin SmokeLoad('subaru.json'); end;
procedure TCatalogLoadSmokeTests.MazdaCatalogLoads;       begin SmokeLoad('mazda.json'); end;
procedure TCatalogLoadSmokeTests.RenaultCatalogLoads;     begin SmokeLoad('renault.json'); end;
procedure TCatalogLoadSmokeTests.VolvoCatalogLoads;       begin SmokeLoad('volvo.json'); end;
procedure TCatalogLoadSmokeTests.TeslaCatalogLoads;       begin SmokeLoad('tesla.json'); end;
procedure TCatalogLoadSmokeTests.SuzukiCatalogLoads;      begin SmokeLoad('suzuki.json'); end;
procedure TCatalogLoadSmokeTests.MitsubishiCatalogLoads;  begin SmokeLoad('mitsubishi.json'); end;
procedure TCatalogLoadSmokeTests.CumminsCatalogLoads;     begin SmokeLoad('cummins.json'); end;
procedure TCatalogLoadSmokeTests.DetroitCatalogLoads;     begin SmokeLoad('detroit.json'); end;
procedure TCatalogLoadSmokeTests.PACCARCatalogLoads;      begin SmokeLoad('paccar.json'); end;
procedure TCatalogLoadSmokeTests.VolvoTrucksCatalogLoads; begin SmokeLoad('volvotrucks.json'); end;
procedure TCatalogLoadSmokeTests.ScaniaCatalogLoads;      begin SmokeLoad('scania.json'); end;
procedure TCatalogLoadSmokeTests.MANCatalogLoads;         begin SmokeLoad('man.json'); end;
procedure TCatalogLoadSmokeTests.BYDCatalogLoads;         begin SmokeLoad('byd.json'); end;
procedure TCatalogLoadSmokeTests.GeelyCatalogLoads;       begin SmokeLoad('geely.json'); end;
procedure TCatalogLoadSmokeTests.NIOCatalogLoads;         begin SmokeLoad('nio.json'); end;
procedure TCatalogLoadSmokeTests.XpengCatalogLoads;       begin SmokeLoad('xpeng.json'); end;
procedure TCatalogLoadSmokeTests.GWMCatalogLoads;         begin SmokeLoad('gwm.json'); end;
procedure TCatalogLoadSmokeTests.JLRCatalogLoads;         begin SmokeLoad('jlr.json'); end;
procedure TCatalogLoadSmokeTests.PorscheCatalogLoads;     begin SmokeLoad('porsche.json'); end;
procedure TCatalogLoadSmokeTests.PolestarCatalogLoads;    begin SmokeLoad('polestar.json'); end;
procedure TCatalogLoadSmokeTests.MINICatalogLoads;        begin SmokeLoad('mini.json'); end;
procedure TCatalogLoadSmokeTests.SmartCatalogLoads;       begin SmokeLoad('smart.json'); end;
procedure TCatalogLoadSmokeTests.DaciaCatalogLoads;       begin SmokeLoad('dacia.json'); end;
procedure TCatalogLoadSmokeTests.LadaCatalogLoads;        begin SmokeLoad('lada.json'); end;
procedure TCatalogLoadSmokeTests.MahindraCatalogLoads;    begin SmokeLoad('mahindra.json'); end;
procedure TCatalogLoadSmokeTests.TataCatalogLoads;        begin SmokeLoad('tata.json'); end;
procedure TCatalogLoadSmokeTests.AstonMartinCatalogLoads; begin SmokeLoad('aston-martin.json'); end;
procedure TCatalogLoadSmokeTests.BentleyCatalogLoads;     begin SmokeLoad('bentley.json'); end;
procedure TCatalogLoadSmokeTests.RollsRoyceCatalogLoads;  begin SmokeLoad('rolls-royce.json'); end;
procedure TCatalogLoadSmokeTests.FerrariCatalogLoads;     begin SmokeLoad('ferrari.json'); end;
procedure TCatalogLoadSmokeTests.McLarenCatalogLoads;     begin SmokeLoad('mclaren.json'); end;
procedure TCatalogLoadSmokeTests.RivianCatalogLoads;      begin SmokeLoad('rivian.json'); end;
procedure TCatalogLoadSmokeTests.LucidCatalogLoads;       begin SmokeLoad('lucid.json'); end;
procedure TCatalogLoadSmokeTests.IsuzuCatalogLoads;       begin SmokeLoad('isuzu.json'); end;
procedure TCatalogLoadSmokeTests.IvecoCatalogLoads;       begin SmokeLoad('iveco.json'); end;
procedure TCatalogLoadSmokeTests.DTCISO15031CatalogLoads; begin SmokeLoadDtc('dtc-iso-15031.json'); end;

function CatalogsDirectory: string;
var
  Candidate: string;
begin
  Candidate := TPath.Combine(GetCurrentDir, 'catalogs');
  if TDirectory.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(TPath.Combine(GetCurrentDir, '..'), 'catalogs');
  if TDirectory.Exists(Candidate) then
    Exit(TPath.GetFullPath(Candidate));
  Result := '';
end;

procedure TCatalogLoadSmokeTests.AllOEMCatalogsLoadFromDirectory;
var
  Dir, FilePath, Name, RelDir: string;
  Files: TArray<string>;
  Loaded, Skipped: Integer;
  Cat: TOBDOEMJSONCatalog;
begin
  Dir := CatalogsDirectory;
  if Dir = '' then Assert.Pass('catalogs/ directory not on path; skipping');
  // Recurse so vehicle-class subdirs (motorcycle/, agricultural/,
  // marine/, powersports/) are exercised by the same sweep.
  Files := TDirectory.GetFiles(Dir, '*.json',
                                TSearchOption.soAllDirectories);
  Loaded := 0;
  Skipped := 0;
  for FilePath in Files do
  begin
    Name := TPath.GetFileName(FilePath);
    RelDir := TPath.GetDirectoryName(FilePath);
    // Skip DTC catalogs (loaded via the DTC sweep), ISO/standard
    // catalogs (different schema), test fixtures, and the JSON
    // schema document itself.
    if Name.StartsWith('dtc-', True) or Name.StartsWith('iso-', True) or
       Name.StartsWith('uds-', True) or Name.StartsWith('obd2-', True) or
       Name.StartsWith('test-', True) or
       RelDir.EndsWith(PathDelim + '_schema') then
    begin
      Inc(Skipped);
      Continue;
    end;
    // Load by absolute path because the file may be under a
    // sub-directory that the regular SmokeLoad's name-only
    // ResolveCatalogPath wouldn't reach without the v3.77 vehicle-
    // class probe.
    Cat := TOBDOEMJSONCatalog.Create(FilePath);
    try
      Assert.IsTrue(Cat.ManufacturerKey <> '',
        Format('manufacturer_key required: %s', [FilePath]));
      Assert.IsTrue((Cat.DIDCount > 0) or (Cat.RoutineCount > 0),
        Format('catalog must contribute at least one DID or routine: %s',
          [FilePath]));
    finally
      Cat.Free;
    end;
    Inc(Loaded);
  end;
  Assert.IsTrue(Loaded >= 70,
    Format('expected >=70 OEM catalogs loaded, got %d (skipped %d)',
      [Loaded, Skipped]));
end;

procedure TCatalogLoadSmokeTests.AllDtcCatalogsLoadFromDirectory;
var
  Dir, FilePath, Name: string;
  Files: TArray<string>;
  Loaded: Integer;
begin
  Dir := CatalogsDirectory;
  if Dir = '' then Assert.Pass('catalogs/ directory not on path; skipping');
  Files := TDirectory.GetFiles(Dir, 'dtc-*.json');
  Loaded := 0;
  for FilePath in Files do
  begin
    Name := TPath.GetFileName(FilePath);
    SmokeLoadDtc(Name);
    Inc(Loaded);
  end;
  Assert.IsTrue(Loaded >= 30,
    Format('expected >=30 DTC catalogs loaded, got %d', [Loaded]));
end;

procedure SweepSubdir(const Subdir: string; MinCount: Integer);
var
  Dir, FilePath: string;
  Files: TArray<string>;
  Loaded: Integer;
  Cat: TOBDOEMJSONCatalog;
begin
  Dir := CatalogsDirectory;
  if Dir = '' then Assert.Pass('catalogs/ directory not on path; skipping');
  Dir := TPath.Combine(Dir, Subdir);
  if not TDirectory.Exists(Dir) then
    Assert.Pass(Format('catalogs/%s not on path; skipping', [Subdir]));
  Files := TDirectory.GetFiles(Dir, '*.json');
  Loaded := 0;
  for FilePath in Files do
  begin
    Cat := TOBDOEMJSONCatalog.Create(FilePath);
    try
      Assert.IsTrue(Cat.ManufacturerKey <> '',
        Format('manufacturer_key required: %s', [FilePath]));
      Assert.IsTrue((Cat.DIDCount > 0) or (Cat.RoutineCount > 0),
        Format('catalog must contribute at least one DID or routine: %s',
          [FilePath]));
    finally
      Cat.Free;
    end;
    Inc(Loaded);
  end;
  Assert.IsTrue(Loaded >= MinCount,
    Format('%s/: expected >=%d catalogs, got %d',
      [Subdir, MinCount, Loaded]));
end;

procedure TCatalogLoadSmokeTests.AllMotorcycleCatalogsLoad;
begin SweepSubdir('motorcycle', 14); end;

procedure TCatalogLoadSmokeTests.AllAgriculturalCatalogsLoad;
begin SweepSubdir('agricultural', 8); end;

procedure TCatalogLoadSmokeTests.AllMarineCatalogsLoad;
begin SweepSubdir('marine', 6); end;

procedure TCatalogLoadSmokeTests.AllPowersportsCatalogsLoad;
begin SweepSubdir('powersports', 5); end;

initialization
  TDUnitX.RegisterTestFixture(TCatalogLoadSmokeTests);

end.
