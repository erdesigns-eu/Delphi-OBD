// ------------------------------------------------------------------------------
// ERD.OEM.CatalogBrands
//
// OEM extensions whose data lives entirely in a catalogue
// (<c>catalogs/&lt;oem&gt;.json</c>). The Pascal here is a thin
// registration shell: VIN matching uses the catalogue's
// <c>applicable_wmis</c>, and the catalogue's <c>extends</c> chain supplies
// the shared template or the parent brand's data.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation (SAIC MG, Lamborghini).
// ------------------------------------------------------------------------------

unit ERD.OEM.CatalogBrands;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.OEM,
  ERD.OEM.Session,
  ERD.OEM.SeedKey,
  ERD.OEM.DTC;

type
  /// <summary>Abstract base for brands that are described only by a
  /// catalogue file.</summary>
  TOBDOEMCatalogBrandBase = class abstract(TOBDOEMExtensionBase)
  protected
    /// <summary>The brand's catalogue file name.</summary>
    function JsonFilename: string; virtual; abstract;
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure BuildExtendedCatalog(var CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var Adaptations: TArray<TOBDOEMAdaptation>;
      var ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var LivePIDs: TArray<TOBDOEMLivePID>;
      var DtcExtended: TArray<TOBDDtcExtendedDataRecord>); override;
    procedure SeedDefaultSeedKeyAlgorithms(Reg: TOBDSeedKeyRegistry); override;
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); override;
  public
    function ApplicableToVIN(const VIN: string): Boolean; override;
  end;

  /// <summary>SAIC Motor MG (catalogs/mg.json).</summary>
  TOBDOEMSaicMG = class(TOBDOEMCatalogBrandBase)
  protected
    function JsonFilename: string; override;
    function DtcCatalogFileName: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

  /// <summary>Automobili Lamborghini (catalogs/lamborghini.json, built on
  /// the VW group catalogue).</summary>
  TOBDOEMLamborghini = class(TOBDOEMCatalogBrandBase)
  protected
    function JsonFilename: string; override;
    function DtcCatalogFileName: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
  end;

implementation

uses
  ERD.OEM.Helpers,
  ERD.OEM.Catalog.Loader,
  ERD.OEM.DTC.Loader;

{ TOBDOEMCatalogBrandBase }

function TOBDOEMCatalogBrandBase.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog(JsonFilename, VIN);
end;

procedure TOBDOEMCatalogBrandBase.BuildCatalog
  (var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>; var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON(JsonFilename, DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMCatalogBrandBase.BuildExtendedCatalog(var CodingBlocks
  : TArray<TOBDOEMCodingBlock>; var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON(JsonFilename, CodingBlocks, Adaptations,
    ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMCatalogBrandBase.SeedDefaultSeedKeyAlgorithms
  (Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    IOBDSeedKeyAlgorithm(TOBDSeedKeyKWP2000TwosComplement.Create()));
end;

procedure TOBDOEMCatalogBrandBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  if DtcCatalogFileName <> '' then
    MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

{ TOBDOEMSaicMG }

function TOBDOEMSaicMG.JsonFilename: string;
begin
  Result := 'mg.json';
end;

function TOBDOEMSaicMG.DtcCatalogFileName: string;
begin
  // No public MG-specific DTC list yet: the generic catalogue only.
  Result := '';
end;

function TOBDOEMSaicMG.ManufacturerKey: string;
begin
  Result := 'SAIC-MG';
end;

function TOBDOEMSaicMG.DisplayName: string;
begin
  Result := 'SAIC Motor - MG';
end;

{ TOBDOEMLamborghini }

function TOBDOEMLamborghini.JsonFilename: string;
begin
  Result := 'lamborghini.json';
end;

function TOBDOEMLamborghini.DtcCatalogFileName: string;
begin
  // VW group fault codes apply to the shared VAG electronics.
  Result := 'dtc-vw.json';
end;

function TOBDOEMLamborghini.ManufacturerKey: string;
begin
  Result := 'LAMBORGHINI';
end;

function TOBDOEMLamborghini.DisplayName: string;
begin
  Result := 'Automobili Lamborghini';
end;

initialization

TOBDOEMRegistry.RegisterExtension(TOBDOEMSaicMG.Create);
TOBDOEMRegistry.RegisterExtension(TOBDOEMLamborghini.Create);

end.
