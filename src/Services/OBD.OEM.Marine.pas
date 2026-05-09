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
      var
        Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure BuildExtendedCatalog(
      var
        CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var
        Adaptations: TArray<TOBDOEMAdaptation>;
      var
        ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var
        LivePIDs: TArray<TOBDOEMLivePID>;
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

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TOBDOEMMarineBase.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog(JsonFilename, VIN);
end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMMarineBase.BuildCatalog(
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON(JsonFilename, DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

//------------------------------------------------------------------------------
// BUILD EXTENDED CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMMarineBase.BuildExtendedCatalog(
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
begin
  MergeExtendedCatalogJSON(JsonFilename,
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

//------------------------------------------------------------------------------
// DTC CATALOG FILE NAME
//------------------------------------------------------------------------------
function TOBDOEMMarineBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMMarineBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMMercuryMarine.JsonFilename: string;    begin Result := 'mercury-marine.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMMercuryMarine.ManufacturerKey: string; begin Result := 'MERCURY-MARINE'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMMercuryMarine.DisplayName: string;     begin Result := 'Mercury Marine (Brunswick)'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMVolvoPenta.JsonFilename: string;    begin Result := 'volvo-penta.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMVolvoPenta.ManufacturerKey: string; begin Result := 'VOLVO-PENTA'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMVolvoPenta.DisplayName: string;     begin Result := 'Volvo Penta'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMYanmarMarine.JsonFilename: string;    begin Result := 'yanmar-marine.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMYanmarMarine.ManufacturerKey: string; begin Result := 'YANMAR-MARINE'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMYanmarMarine.DisplayName: string;     begin Result := 'Yanmar Marine'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMMtu.JsonFilename: string;    begin Result := 'mtu.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMMtu.ManufacturerKey: string; begin Result := 'MTU'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMMtu.DisplayName: string;     begin Result := 'MTU (Rolls-Royce Power Systems)'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMCumminsMarine.JsonFilename: string;    begin Result := 'cummins-marine.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMCumminsMarine.ManufacturerKey: string; begin Result := 'CUMMINS-MARINE'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMCumminsMarine.DisplayName: string;     begin Result := 'Cummins Marine'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMYamahaMarine.JsonFilename: string;    begin Result := 'yamaha-marine.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMYamahaMarine.ManufacturerKey: string; begin Result := 'YAMAHA-MARINE'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMYamahaMarine.DisplayName: string;     begin Result := 'Yamaha Marine'; end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMMercuryMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMVolvoPenta.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYanmarMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMMtu.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMCumminsMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYamahaMarine.Create);

end.
