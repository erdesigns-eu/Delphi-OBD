//------------------------------------------------------------------------------
//  OBD.OEM.Powersports
//
//  OEM extensions for the powersports brands (ATV / UTV / sled
//  / PWC). Catalogues live in
//  <c>catalogs/powersports/&lt;oem&gt;.json</c> and the matching
//  <c>dtc-&lt;oem&gt;.json</c> files; the Pascal here is a thin
//  registration shell on top of the shared base.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Powersports;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Abstract base for the powersports
  /// extensions.</summary>
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
    procedure SeedDefaultSeedKeyAlgorithms(
      Reg: TOBDSeedKeyRegistry); override;
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
  OBD.OEM.Helpers,
  OBD.OEM.Catalog.Loader,
  OBD.OEM.DTC.Loader;

{ TOBDOEMPowersportsBase }

function TOBDOEMPowersportsBase.ApplicableToVIN(
  const VIN: string): Boolean;
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

procedure TOBDOEMPowersportsBase.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

function TOBDOEMPowersportsBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

procedure TOBDOEMPowersportsBase.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMPolaris.JsonFilename: string;          begin Result := 'polaris.json'; end;
function TOBDOEMPolaris.ManufacturerKey: string;       begin Result := 'POLARIS';      end;
function TOBDOEMPolaris.DisplayName: string;           begin Result := 'Polaris Industries'; end;

function TOBDOEMCanAmBrp.JsonFilename: string;         begin Result := 'can-am-brp.json'; end;
function TOBDOEMCanAmBrp.ManufacturerKey: string;      begin Result := 'CAN-AM-BRP';      end;
function TOBDOEMCanAmBrp.DisplayName: string;          begin Result := 'Can-Am / BRP (Bombardier)'; end;

function TOBDOEMArcticCat.JsonFilename: string;        begin Result := 'arctic-cat.json'; end;
function TOBDOEMArcticCat.ManufacturerKey: string;     begin Result := 'ARCTIC-CAT';      end;
function TOBDOEMArcticCat.DisplayName: string;         begin Result := 'Arctic Cat (Textron)'; end;

function TOBDOEMYamahaWaverunner.JsonFilename: string;    begin Result := 'yamaha-waverunner.json'; end;
function TOBDOEMYamahaWaverunner.ManufacturerKey: string; begin Result := 'YAMAHA-WAVERUNNER';      end;
function TOBDOEMYamahaWaverunner.DisplayName: string;     begin Result := 'Yamaha WaveRunner';      end;

function TOBDOEMKawasakiJet.JsonFilename: string;      begin Result := 'kawasaki-jet.json'; end;
function TOBDOEMKawasakiJet.ManufacturerKey: string;   begin Result := 'KAWASAKI-JET';      end;
function TOBDOEMKawasakiJet.DisplayName: string;       begin Result := 'Kawasaki Jet Ski';  end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMPolaris.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMCanAmBrp.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMArcticCat.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYamahaWaverunner.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMKawasakiJet.Create);

end.
