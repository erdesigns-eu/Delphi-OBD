//------------------------------------------------------------------------------
//  OBD.OEM.Motorcycles
//
//  OEM extensions for the motorcycle brands. Catalogues live in
//  <c>catalogs/motorcycle/&lt;oem&gt;.json</c> and the matching
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

unit OBD.OEM.Motorcycles;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Abstract base for the motorcycle
  /// extensions.</summary>
  TOBDOEMMotorcycleBase = class abstract(TOBDOEMExtensionBase)
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

  TOBDOEMDucati = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMHarleyDavidson = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMTriumph = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMBmwMotorrad = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMKtm = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMYamahaMoto = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMHondaMoto = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMKawasaki = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMSuzukiMoto = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMIndianMotorcycle = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMRoyalEnfield = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMMVAgusta = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMAprilia = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMHusqvarnaMoto = class(TOBDOEMMotorcycleBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;

implementation

uses
  OBD.OEM.Helpers,
  OBD.OEM.Catalog.Loader,
  OBD.OEM.DTC.Loader;

{ TOBDOEMMotorcycleBase }

function TOBDOEMMotorcycleBase.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog(JsonFilename, VIN);
end;

procedure TOBDOEMMotorcycleBase.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON(JsonFilename, DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMMotorcycleBase.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON(JsonFilename,
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMMotorcycleBase.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

function TOBDOEMMotorcycleBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

procedure TOBDOEMMotorcycleBase.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMDucati.JsonFilename: string;            begin Result := 'ducati.json'; end;
function TOBDOEMDucati.ManufacturerKey: string;         begin Result := 'DUCATI';      end;
function TOBDOEMDucati.DisplayName: string;             begin Result := 'Ducati Motor Holding'; end;

function TOBDOEMHarleyDavidson.JsonFilename: string;    begin Result := 'harley-davidson.json'; end;
function TOBDOEMHarleyDavidson.ManufacturerKey: string; begin Result := 'HARLEY-DAVIDSON';      end;
function TOBDOEMHarleyDavidson.DisplayName: string;     begin Result := 'Harley-Davidson';      end;

function TOBDOEMTriumph.JsonFilename: string;           begin Result := 'triumph.json'; end;
function TOBDOEMTriumph.ManufacturerKey: string;        begin Result := 'TRIUMPH';      end;
function TOBDOEMTriumph.DisplayName: string;            begin Result := 'Triumph Motorcycles'; end;

function TOBDOEMBmwMotorrad.JsonFilename: string;       begin Result := 'bmw-motorrad.json'; end;
function TOBDOEMBmwMotorrad.ManufacturerKey: string;    begin Result := 'BMW-MOTORRAD';      end;
function TOBDOEMBmwMotorrad.DisplayName: string;        begin Result := 'BMW Motorrad';      end;

function TOBDOEMKtm.JsonFilename: string;               begin Result := 'ktm.json'; end;
function TOBDOEMKtm.ManufacturerKey: string;            begin Result := 'KTM';      end;
function TOBDOEMKtm.DisplayName: string;                begin Result := 'KTM AG';   end;

function TOBDOEMYamahaMoto.JsonFilename: string;        begin Result := 'yamaha-moto.json'; end;
function TOBDOEMYamahaMoto.ManufacturerKey: string;     begin Result := 'YAMAHA-MOTO';      end;
function TOBDOEMYamahaMoto.DisplayName: string;         begin Result := 'Yamaha Motor (motorcycles)'; end;

function TOBDOEMHondaMoto.JsonFilename: string;         begin Result := 'honda-moto.json'; end;
function TOBDOEMHondaMoto.ManufacturerKey: string;      begin Result := 'HONDA-MOTO';      end;
function TOBDOEMHondaMoto.DisplayName: string;          begin Result := 'Honda Motor (motorcycles)'; end;

function TOBDOEMKawasaki.JsonFilename: string;          begin Result := 'kawasaki.json'; end;
function TOBDOEMKawasaki.ManufacturerKey: string;       begin Result := 'KAWASAKI';      end;
function TOBDOEMKawasaki.DisplayName: string;           begin Result := 'Kawasaki Heavy Industries'; end;

function TOBDOEMSuzukiMoto.JsonFilename: string;        begin Result := 'suzuki-moto.json'; end;
function TOBDOEMSuzukiMoto.ManufacturerKey: string;     begin Result := 'SUZUKI-MOTO';      end;
function TOBDOEMSuzukiMoto.DisplayName: string;         begin Result := 'Suzuki Motor (motorcycles)'; end;

function TOBDOEMIndianMotorcycle.JsonFilename: string;    begin Result := 'indian-motorcycle.json'; end;
function TOBDOEMIndianMotorcycle.ManufacturerKey: string; begin Result := 'INDIAN-MOTORCYCLE';      end;
function TOBDOEMIndianMotorcycle.DisplayName: string;     begin Result := 'Indian Motorcycle (Polaris)'; end;

function TOBDOEMRoyalEnfield.JsonFilename: string;      begin Result := 'royal-enfield.json'; end;
function TOBDOEMRoyalEnfield.ManufacturerKey: string;   begin Result := 'ROYAL-ENFIELD';      end;
function TOBDOEMRoyalEnfield.DisplayName: string;       begin Result := 'Royal Enfield (Eicher)'; end;

function TOBDOEMMVAgusta.JsonFilename: string;          begin Result := 'mv-agusta.json'; end;
function TOBDOEMMVAgusta.ManufacturerKey: string;       begin Result := 'MV-AGUSTA';      end;
function TOBDOEMMVAgusta.DisplayName: string;           begin Result := 'MV Agusta';      end;

function TOBDOEMAprilia.JsonFilename: string;           begin Result := 'aprilia.json'; end;
function TOBDOEMAprilia.ManufacturerKey: string;        begin Result := 'APRILIA';      end;
function TOBDOEMAprilia.DisplayName: string;            begin Result := 'Aprilia (Piaggio)'; end;

function TOBDOEMHusqvarnaMoto.JsonFilename: string;     begin Result := 'husqvarna-moto.json'; end;
function TOBDOEMHusqvarnaMoto.ManufacturerKey: string;  begin Result := 'HUSQVARNA-MOTO';      end;
function TOBDOEMHusqvarnaMoto.DisplayName: string;      begin Result := 'Husqvarna Motorcycles (KTM AG)'; end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMDucati.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMHarleyDavidson.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMTriumph.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMBmwMotorrad.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMKtm.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYamahaMoto.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMHondaMoto.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMKawasaki.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMSuzukiMoto.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMIndianMotorcycle.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMRoyalEnfield.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMMVAgusta.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMAprilia.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMHusqvarnaMoto.Create);

end.
