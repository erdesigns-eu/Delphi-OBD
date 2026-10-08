//------------------------------------------------------------------------------
//  CatalogBundleDM — sample 17
//
//  Demonstrates the catalogue manager components — three
//  non-visual components that wrap the static catalogue
//  classes for design-time configuration:
//
//    TOBDVINCatalog            wraps TOBDVINDecoder
//    TOBDDriveCycleCatalogComp wraps TOBDDriveCycleCatalog
//    TOBDEVBatteryCatalogComp  wraps TOBDEVBatteryCatalog
//
//  Each one has CatalogDir + AutoLoad published, a Reload
//  method, and an OnReloaded event. Drop them on a
//  TDataModule, set CatalogDir once, and every component on
//  every form gets a configured catalogue.
//------------------------------------------------------------------------------

program CatalogBundleDM;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  ERD.Service.VINDecoder.Types
    in '..\..\src\Service\ERD.Service.VINDecoder.Types.pas',
  ERD.Service.VINDecoder
    in '..\..\src\Service\ERD.Service.VINDecoder.pas',
  ERD.Service.VINDecoder.Catalog.Component
    in '..\..\src\Service\ERD.Service.VINDecoder.Catalog.Component.pas',
  ERD.Service.DriveCycle.Types
    in '..\..\src\Service\ERD.Service.DriveCycle.Types.pas',
  ERD.Service.DriveCycle.Catalog
    in '..\..\src\Service\ERD.Service.DriveCycle.Catalog.pas',
  ERD.Service.DriveCycle.Catalog.Component
    in '..\..\src\Service\ERD.Service.DriveCycle.Catalog.Component.pas',
  ERD.Service.EVBattery.Types
    in '..\..\src\Service\ERD.Service.EVBattery.Types.pas',
  ERD.Service.EVBattery.Catalog
    in '..\..\src\Service\ERD.Service.EVBattery.Catalog.pas',
  ERD.Service.EVBattery.Catalog.Component
    in '..\..\src\Service\ERD.Service.EVBattery.Catalog.Component.pas';

procedure HandleVINReloaded(Sender: TObject);
begin
  Writeln('  [TOBDVINCatalog reloaded]');
end;

procedure HandleDriveCycleReloaded(Sender: TObject);
begin
  Writeln('  [TOBDDriveCycleCatalogComp reloaded]');
end;

procedure HandleEVReloaded(Sender: TObject);
begin
  Writeln('  [TOBDEVBatteryCatalogComp reloaded]');
end;

var
  CatalogDir: string;
  VIN_:       TOBDVINCatalog;
  DC:         TOBDDriveCycleCatalogComp;
  EV:         TOBDEVBatteryCatalogComp;
begin
  CatalogDir :=
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\catalogs');

  Writeln('Catalogue manager components demo');
  Writeln(StringOfChar('-', 60));
  Writeln('CatalogDir: ', CatalogDir);
  Writeln;

  // The three components share an AutoLoad pattern. AutoLoad
  // = True (default) calls Reload during the .dfm streaming
  // step, which is why "drop on a DataModule, set CatalogDir,
  // forget" works in real apps. We replicate that here by
  // calling Reload manually after CatalogDir is set.
  VIN_ := TOBDVINCatalog.Create(nil);
  DC   := TOBDDriveCycleCatalogComp.Create(nil);
  EV   := TOBDEVBatteryCatalogComp.Create(nil);
  try
    VIN_.OnReloaded := HandleVINReloaded;
    DC  .OnReloaded := HandleDriveCycleReloaded;
    EV  .OnReloaded := HandleEVReloaded;

    VIN_.CatalogDir := CatalogDir;
    DC  .CatalogDir := CatalogDir;
    EV  .CatalogDir := CatalogDir;

    VIN_.Reload;
    DC  .Reload;
    EV  .Reload;

    Writeln;
    Writeln('Drive-cycle monitors registered:');
    for var Mon in DC.RegisteredMonitors do
      Writeln('  - ', MonitorName(Mon));

    Writeln;
    Writeln('EV battery vendors registered:');
    for var V in EV.VendorKeys do
      if not V.StartsWith('_') then
        Writeln('  - ', V);
  finally
    EV.Free;
    DC.Free;
    VIN_.Free;
  end;
  Writeln(StringOfChar('-', 60));
  Writeln('Done.');
end.
