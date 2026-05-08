//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Powersports.pas
// CONTENTS       : OEM extensions for the 5 powersports catalogs
//                  (catalogs/powersports/<oem>.json) — UTV, ATV,
//                  snowmobile, jet ski.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Powersports;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMPowersportsBase = class abstract(TOBDOEMExtensionBase)
  protected
    function JsonFilename: string; virtual; abstract;
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure BuildExtendedCatalog(
      var CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var Adaptations: TArray<TOBDOEMAdaptation>;
      var ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var LivePIDs: TArray<TOBDOEMLivePID>;
      var DtcExtended: TArray<TOBDDtcExtendedDataRecord>); override;
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); override;
    function DtcCatalogFileName: string; override;
  public
    function ApplicableToVIN(const VIN: string): Boolean; override;
  end;

  TOBDOEMPolaris = class(TOBDOEMPowersportsBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMCanAmBrp = class(TOBDOEMPowersportsBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMArcticCat = class(TOBDOEMPowersportsBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMYamahaWaverunner = class(TOBDOEMPowersportsBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMKawasakiJet = class(TOBDOEMPowersportsBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;

implementation

uses
  OBD.OEM.Catalog.Loader, OBD.OEM.DTC.Loader;

function TOBDOEMPowersportsBase.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog(JsonFilename, VIN);
end;

procedure TOBDOEMPowersportsBase.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON(JsonFilename, DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMPowersportsBase.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON(JsonFilename,
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMPowersportsBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

procedure TOBDOEMPowersportsBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMPolaris.JsonFilename: string;       begin Result := 'polaris.json'; end;
function TOBDOEMPolaris.ManufacturerKey: string;    begin Result := 'POLARIS'; end;
function TOBDOEMPolaris.DisplayName: string;        begin Result := 'Polaris Industries'; end;

function TOBDOEMCanAmBrp.JsonFilename: string;       begin Result := 'can-am-brp.json'; end;
function TOBDOEMCanAmBrp.ManufacturerKey: string;    begin Result := 'CAN-AM-BRP'; end;
function TOBDOEMCanAmBrp.DisplayName: string;        begin Result := 'Can-Am / BRP (Bombardier)'; end;

function TOBDOEMArcticCat.JsonFilename: string;       begin Result := 'arctic-cat.json'; end;
function TOBDOEMArcticCat.ManufacturerKey: string;    begin Result := 'ARCTIC-CAT'; end;
function TOBDOEMArcticCat.DisplayName: string;        begin Result := 'Arctic Cat (Textron)'; end;

function TOBDOEMYamahaWaverunner.JsonFilename: string;
  begin Result := 'yamaha-waverunner.json'; end;
function TOBDOEMYamahaWaverunner.ManufacturerKey: string;
  begin Result := 'YAMAHA-WAVERUNNER'; end;
function TOBDOEMYamahaWaverunner.DisplayName: string;
  begin Result := 'Yamaha WaveRunner (Yamaha Marine subdivision)'; end;

function TOBDOEMKawasakiJet.JsonFilename: string;     begin Result := 'kawasaki-jet.json'; end;
function TOBDOEMKawasakiJet.ManufacturerKey: string;  begin Result := 'KAWASAKI-JET'; end;
function TOBDOEMKawasakiJet.DisplayName: string;      begin Result := 'Kawasaki Jet Ski'; end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMPolaris.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMCanAmBrp.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMArcticCat.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYamahaWaverunner.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMKawasakiJet.Create);

end.
