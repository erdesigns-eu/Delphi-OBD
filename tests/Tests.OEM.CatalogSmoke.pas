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
    [Test] procedure UniversalUDSCatalogLoads;
    [Test] procedure UniversalOBDPidsCatalogLoads;
    [Test] procedure VWCatalogLoads;
    [Test] procedure BMWCatalogLoads;
    [Test] procedure MercedesCatalogLoads;
    [Test] procedure FordCatalogLoads;
    [Test] procedure GMCatalogLoads;
    [Test] procedure StellantisCatalogLoads;
    [Test] procedure ToyotaCatalogLoads;
    [Test] procedure HondaCatalogLoads;
    [Test] procedure HMGCatalogLoads;
    [Test] procedure NissanCatalogLoads;
    [Test] procedure SubaruCatalogLoads;
    [Test] procedure MazdaCatalogLoads;
    [Test] procedure RenaultCatalogLoads;
    [Test] procedure VolvoCatalogLoads;
    [Test] procedure TeslaCatalogLoads;
    [Test] procedure SuzukiCatalogLoads;
    [Test] procedure MitsubishiCatalogLoads;
    [Test] procedure CumminsCatalogLoads;
    [Test] procedure DetroitCatalogLoads;
    [Test] procedure PACCARCatalogLoads;
    [Test] procedure VolvoTrucksCatalogLoads;
    [Test] procedure ScaniaCatalogLoads;
    [Test] procedure MANCatalogLoads;
    [Test] procedure BYDCatalogLoads;
    [Test] procedure GeelyCatalogLoads;
    [Test] procedure NIOCatalogLoads;
    [Test] procedure XpengCatalogLoads;
    [Test] procedure GWMCatalogLoads;
    [Test] procedure DTCISO15031CatalogLoads;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  OBD.OEM.Catalog.JSON, OBD.OEM.DTC;

function ResolveCatalogPath(const FileName: string): string;
var
  Candidate: string;
begin
  Candidate := TPath.Combine(TPath.Combine(GetCurrentDir, 'catalogs'), FileName);
  if TFile.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(TPath.Combine(GetCurrentDir, '..'),
    TPath.Combine('catalogs', FileName));
  Result := TPath.GetFullPath(Candidate);
end;

procedure SmokeLoad(const FileName: string;
  const RequireManufacturerKey: Boolean = True);
var
  Cat: TOBDOEMJSONCatalog;
  Path: string;
begin
  Path := ResolveCatalogPath(FileName);
  if not TFile.Exists(Path) then
    Assert.Pass('catalog ' + FileName + ' not on path; skipping');
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
  Path := ResolveCatalogPath(FileName);
  if not TFile.Exists(Path) then
    Assert.Pass('catalog ' + FileName + ' not on path; skipping');
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
procedure TCatalogLoadSmokeTests.DTCISO15031CatalogLoads; begin SmokeLoadDtc('dtc-iso-15031.json'); end;

initialization
  TDUnitX.RegisterTestFixture(TCatalogLoadSmokeTests);

end.
