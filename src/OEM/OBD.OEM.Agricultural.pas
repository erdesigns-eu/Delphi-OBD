//------------------------------------------------------------------------------
//  OBD.OEM.Agricultural
//
//  OEM extensions for the agricultural / construction equipment
//  brands. Most of these OEMs publish DTCs in J1939 SPN-FMI
//  format and DIDs over UDS / J1939-21; catalogue content lives
//  in <c>catalogs/agricultural/&lt;oem&gt;.json</c> and the
//  matching <c>dtc-&lt;oem&gt;.json</c> files. The Pascal here
//  is a thin registration shell on top of the shared base.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Agricultural;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Abstract base for every agricultural /
  /// construction extension. Concrete subclasses only declare
  /// the catalogue filename + identity strings.</summary>
  TOBDOEMAgriculturalBase = class abstract(TOBDOEMExtensionBase)
  protected
    /// <summary>Catalogue filename (resolved via the standard
    /// catalogs / subdirectory search).</summary>
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

  /// <summary>John Deere (Deere & Company) extension.</summary>
  TOBDOEMJohnDeere = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>CNH Industrial extension.</summary>
  TOBDOEMCnh = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>Caterpillar (agricultural / construction)
  /// extension.</summary>
  TOBDOEMCaterpillarAgri = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>Komatsu extension.</summary>
  TOBDOEMKomatsu = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>Kubota Corporation extension.</summary>
  TOBDOEMKubota = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>AGCO group extension (Massey Ferguson / Fendt /
  /// Valtra / Challenger).</summary>
  TOBDOEMAgco = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>Claas KGaA extension.</summary>
  TOBDOEMClaas = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>Volvo Construction Equipment
  /// extension.</summary>
  TOBDOEMVolvoCe = class(TOBDOEMAgriculturalBase)
  protected
    function JsonFilename: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

implementation

uses
  OBD.OEM.Helpers,
  OBD.OEM.Catalog.Loader,
  OBD.OEM.DTC.Loader;

{ TOBDOEMAgriculturalBase }

function TOBDOEMAgriculturalBase.ApplicableToVIN(
  const VIN: string): Boolean;
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

procedure TOBDOEMAgriculturalBase.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

function TOBDOEMAgriculturalBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

procedure TOBDOEMAgriculturalBase.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

{ Concrete brand wrappers — every entry is a one-line
  catalogue-filename / display-label triple. }

function TOBDOEMJohnDeere.JsonFilename: string;
begin
  Result := 'john-deere.json';
end;

function TOBDOEMJohnDeere.ManufacturerKey: string;
begin
  Result := 'JOHN-DEERE';
end;

function TOBDOEMJohnDeere.DisplayName: string;
begin
  Result := 'John Deere (Deere & Company)';
end;

function TOBDOEMCnh.JsonFilename: string;
begin
  Result := 'cnh.json';
end;

function TOBDOEMCnh.ManufacturerKey: string;
begin
  Result := 'CNH';
end;

function TOBDOEMCnh.DisplayName: string;
begin
  Result := 'CNH Industrial (Case IH / New Holland / Steyr)';
end;

function TOBDOEMCaterpillarAgri.JsonFilename: string;
begin
  Result := 'caterpillar.json';
end;

function TOBDOEMCaterpillarAgri.ManufacturerKey: string;
begin
  Result := 'CATERPILLAR';
end;

function TOBDOEMCaterpillarAgri.DisplayName: string;
begin
  Result := 'Caterpillar (Cat)';
end;

function TOBDOEMKomatsu.JsonFilename: string;
begin
  Result := 'komatsu.json';
end;

function TOBDOEMKomatsu.ManufacturerKey: string;
begin
  Result := 'KOMATSU';
end;

function TOBDOEMKomatsu.DisplayName: string;
begin
  Result := 'Komatsu';
end;

function TOBDOEMKubota.JsonFilename: string;
begin
  Result := 'kubota.json';
end;

function TOBDOEMKubota.ManufacturerKey: string;
begin
  Result := 'KUBOTA';
end;

function TOBDOEMKubota.DisplayName: string;
begin
  Result := 'Kubota Corporation';
end;

function TOBDOEMAgco.JsonFilename: string;
begin
  Result := 'agco.json';
end;

function TOBDOEMAgco.ManufacturerKey: string;
begin
  Result := 'AGCO';
end;

function TOBDOEMAgco.DisplayName: string;
begin
  Result := 'AGCO (Massey Ferguson / Fendt / Valtra / Challenger)';
end;

function TOBDOEMClaas.JsonFilename: string;
begin
  Result := 'claas.json';
end;

function TOBDOEMClaas.ManufacturerKey: string;
begin
  Result := 'CLAAS';
end;

function TOBDOEMClaas.DisplayName: string;
begin
  Result := 'Claas KGaA';
end;

function TOBDOEMVolvoCe.JsonFilename: string;
begin
  Result := 'volvo-ce.json';
end;

function TOBDOEMVolvoCe.ManufacturerKey: string;
begin
  Result := 'VOLVO-CE';
end;

function TOBDOEMVolvoCe.DisplayName: string;
begin
  Result := 'Volvo Construction Equipment';
end;

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
