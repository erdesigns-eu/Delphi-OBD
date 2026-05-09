//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Marine.pas
// CONTENTS       : OEM extensions for the 6 marine catalogs
//                  (catalogs/marine/<oem>.json).
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Marine OEMs typically expose their diagnostic
//                  surface over NMEA 2000 / SmartCraft (Mercury) or
//                  J1939 (commercial diesel) — but the request /
//                  response semantics align with UDS once a tool is
//                  bridged to NMEA. Catalog content lives in JSON.
//------------------------------------------------------------------------------
unit OBD.OEM.Marine;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMMarineBase = class abstract(TOBDOEMExtensionBase)
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

  TOBDOEMMercuryMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMVolvoPenta = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMYanmarMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMMtu = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMCumminsMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMYamahaMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;

implementation

uses
  OBD.OEM.Catalog.Loader, OBD.OEM.DTC.Loader;

function TOBDOEMMarineBase.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog(JsonFilename, VIN);
end;

procedure TOBDOEMMarineBase.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON(JsonFilename, DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMMarineBase.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON(JsonFilename,
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMMarineBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

procedure TOBDOEMMarineBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMMercuryMarine.JsonFilename: string;    begin Result := 'mercury-marine.json'; end;
function TOBDOEMMercuryMarine.ManufacturerKey: string; begin Result := 'MERCURY-MARINE'; end;
function TOBDOEMMercuryMarine.DisplayName: string;     begin Result := 'Mercury Marine (Brunswick)'; end;

function TOBDOEMVolvoPenta.JsonFilename: string;    begin Result := 'volvo-penta.json'; end;
function TOBDOEMVolvoPenta.ManufacturerKey: string; begin Result := 'VOLVO-PENTA'; end;
function TOBDOEMVolvoPenta.DisplayName: string;     begin Result := 'Volvo Penta'; end;

function TOBDOEMYanmarMarine.JsonFilename: string;    begin Result := 'yanmar-marine.json'; end;
function TOBDOEMYanmarMarine.ManufacturerKey: string; begin Result := 'YANMAR-MARINE'; end;
function TOBDOEMYanmarMarine.DisplayName: string;     begin Result := 'Yanmar Marine'; end;

function TOBDOEMMtu.JsonFilename: string;    begin Result := 'mtu.json'; end;
function TOBDOEMMtu.ManufacturerKey: string; begin Result := 'MTU'; end;
function TOBDOEMMtu.DisplayName: string;     begin Result := 'MTU (Rolls-Royce Power Systems)'; end;

function TOBDOEMCumminsMarine.JsonFilename: string;    begin Result := 'cummins-marine.json'; end;
function TOBDOEMCumminsMarine.ManufacturerKey: string; begin Result := 'CUMMINS-MARINE'; end;
function TOBDOEMCumminsMarine.DisplayName: string;     begin Result := 'Cummins Marine'; end;

function TOBDOEMYamahaMarine.JsonFilename: string;    begin Result := 'yamaha-marine.json'; end;
function TOBDOEMYamahaMarine.ManufacturerKey: string; begin Result := 'YAMAHA-MARINE'; end;
function TOBDOEMYamahaMarine.DisplayName: string;     begin Result := 'Yamaha Marine'; end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMMercuryMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMVolvoPenta.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYanmarMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMMtu.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMCumminsMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYamahaMarine.Create);

end.
