//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Agricultural.pas
// CONTENTS       : OEM extensions for the 8 agricultural / construction
//                  catalogs (catalogs/agricultural/<oem>.json).
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Most agricultural / construction OEMs publish DTCs
//                  in J1939 SPN-FMI format and DIDs over UDS / J1939-21.
//                  Catalog content lives in JSON; this unit is a thin
//                  registration shell.
//------------------------------------------------------------------------------
unit OBD.OEM.Agricultural;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMAgriculturalBase = class abstract(TOBDOEMExtensionBase)
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

  TOBDOEMJohnDeere = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMCnh = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMCaterpillarAgri = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMKomatsu = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMKubota = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMAgco = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMClaas = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  TOBDOEMVolvoCe = class(TOBDOEMAgriculturalBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;

implementation

uses
  OBD.OEM.Catalog.Loader, OBD.OEM.DTC.Loader;

function TOBDOEMAgriculturalBase.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog(JsonFilename, VIN);
end;

procedure TOBDOEMAgriculturalBase.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON(JsonFilename, DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMAgriculturalBase.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON(JsonFilename,
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMAgriculturalBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

procedure TOBDOEMAgriculturalBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMJohnDeere.JsonFilename: string;       begin Result := 'john-deere.json'; end;
function TOBDOEMJohnDeere.ManufacturerKey: string;    begin Result := 'JOHN-DEERE'; end;
function TOBDOEMJohnDeere.DisplayName: string;        begin Result := 'John Deere (Deere & Company)'; end;

function TOBDOEMCnh.JsonFilename: string;       begin Result := 'cnh.json'; end;
function TOBDOEMCnh.ManufacturerKey: string;    begin Result := 'CNH'; end;
function TOBDOEMCnh.DisplayName: string;        begin Result := 'CNH Industrial (Case IH / New Holland / Steyr)'; end;

function TOBDOEMCaterpillarAgri.JsonFilename: string;    begin Result := 'caterpillar.json'; end;
function TOBDOEMCaterpillarAgri.ManufacturerKey: string; begin Result := 'CATERPILLAR'; end;
function TOBDOEMCaterpillarAgri.DisplayName: string;     begin Result := 'Caterpillar (Cat)'; end;

function TOBDOEMKomatsu.JsonFilename: string;    begin Result := 'komatsu.json'; end;
function TOBDOEMKomatsu.ManufacturerKey: string; begin Result := 'KOMATSU'; end;
function TOBDOEMKomatsu.DisplayName: string;     begin Result := 'Komatsu'; end;

function TOBDOEMKubota.JsonFilename: string;    begin Result := 'kubota.json'; end;
function TOBDOEMKubota.ManufacturerKey: string; begin Result := 'KUBOTA'; end;
function TOBDOEMKubota.DisplayName: string;     begin Result := 'Kubota Corporation'; end;

function TOBDOEMAgco.JsonFilename: string;    begin Result := 'agco.json'; end;
function TOBDOEMAgco.ManufacturerKey: string; begin Result := 'AGCO'; end;
function TOBDOEMAgco.DisplayName: string;     begin Result := 'AGCO (Massey Ferguson / Fendt / Valtra / Challenger)'; end;

function TOBDOEMClaas.JsonFilename: string;    begin Result := 'claas.json'; end;
function TOBDOEMClaas.ManufacturerKey: string; begin Result := 'CLAAS'; end;
function TOBDOEMClaas.DisplayName: string;     begin Result := 'Claas KGaA'; end;

function TOBDOEMVolvoCe.JsonFilename: string;    begin Result := 'volvo-ce.json'; end;
function TOBDOEMVolvoCe.ManufacturerKey: string; begin Result := 'VOLVO-CE'; end;
function TOBDOEMVolvoCe.DisplayName: string;     begin Result := 'Volvo Construction Equipment'; end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMJohnDeere.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMCnh.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMCaterpillarAgri.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMKomatsu.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMKubota.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMAgco.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMClaas.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMVolvoCe.Create);

end.
