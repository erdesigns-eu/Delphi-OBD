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

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TOBDOEMAgriculturalBase.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog(JsonFilename, VIN);
end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMAgriculturalBase.BuildCatalog(
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
procedure TOBDOEMAgriculturalBase.BuildExtendedCatalog(
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
function TOBDOEMAgriculturalBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMAgriculturalBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMJohnDeere.JsonFilename: string;       begin Result := 'john-deere.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMJohnDeere.ManufacturerKey: string;    begin Result := 'JOHN-DEERE'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMJohnDeere.DisplayName: string;        begin Result := 'John Deere (Deere & Company)'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMCnh.JsonFilename: string;       begin Result := 'cnh.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMCnh.ManufacturerKey: string;    begin Result := 'CNH'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMCnh.DisplayName: string;        begin Result := 'CNH Industrial (Case IH / New Holland / Steyr)'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMCaterpillarAgri.JsonFilename: string;    begin Result := 'caterpillar.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMCaterpillarAgri.ManufacturerKey: string; begin Result := 'CATERPILLAR'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMCaterpillarAgri.DisplayName: string;     begin Result := 'Caterpillar (Cat)'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMKomatsu.JsonFilename: string;    begin Result := 'komatsu.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMKomatsu.ManufacturerKey: string; begin Result := 'KOMATSU'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMKomatsu.DisplayName: string;     begin Result := 'Komatsu'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMKubota.JsonFilename: string;    begin Result := 'kubota.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMKubota.ManufacturerKey: string; begin Result := 'KUBOTA'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMKubota.DisplayName: string;     begin Result := 'Kubota Corporation'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMAgco.JsonFilename: string;    begin Result := 'agco.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMAgco.ManufacturerKey: string; begin Result := 'AGCO'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMAgco.DisplayName: string;     begin Result := 'AGCO (Massey Ferguson / Fendt / Valtra / Challenger)'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMClaas.JsonFilename: string;    begin Result := 'claas.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMClaas.ManufacturerKey: string; begin Result := 'CLAAS'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMClaas.DisplayName: string;     begin Result := 'Claas KGaA'; end;

//------------------------------------------------------------------------------
// JSON FILENAME
//------------------------------------------------------------------------------
function TOBDOEMVolvoCe.JsonFilename: string;    begin Result := 'volvo-ce.json'; end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMVolvoCe.ManufacturerKey: string; begin Result := 'VOLVO-CE'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
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
