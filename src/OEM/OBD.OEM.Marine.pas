//------------------------------------------------------------------------------
//  OBD.OEM.Marine
//
//  OEM extensions for the recreational + commercial marine
//  brands. Catalogues live in <c>catalogs/marine/&lt;oem&gt;.json</c>
//  and the matching <c>dtc-&lt;oem&gt;.json</c> files; the
//  Pascal here is a thin registration shell on top of the
//  shared base.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Marine;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Abstract base for the marine extensions.</summary>
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
    procedure SeedDefaultSeedKeyAlgorithms(
      Reg: TOBDSeedKeyRegistry); override;
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); override;
    function DtcCatalogFileName: string; override;
  public
    function ApplicableToVIN(const VIN: string): Boolean; override;
  end;

  /// <summary>Mercury Marine (Brunswick).</summary>
  TOBDOEMMercuryMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  /// <summary>Volvo Penta.</summary>
  TOBDOEMVolvoPenta = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  /// <summary>Yanmar Marine.</summary>
  TOBDOEMYanmarMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  /// <summary>MTU (Rolls-Royce Power Systems).</summary>
  TOBDOEMMtu = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  /// <summary>Cummins Marine.</summary>
  TOBDOEMCumminsMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;
  /// <summary>Yamaha Marine.</summary>
  TOBDOEMYamahaMarine = class(TOBDOEMMarineBase)
  protected function JsonFilename: string; override;
  public function ManufacturerKey: string; override;
         function DisplayName: string; override;
  end;

implementation

uses
  OBD.OEM.Helpers,
  OBD.OEM.Catalog.Loader,
  OBD.OEM.DTC.Loader;

{ TOBDOEMMarineBase }

function TOBDOEMMarineBase.ApplicableToVIN(
  const VIN: string): Boolean;
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

procedure TOBDOEMMarineBase.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

function TOBDOEMMarineBase.DtcCatalogFileName: string;
begin
  Result := 'dtc-' + JsonFilename;
end;

procedure TOBDOEMMarineBase.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMMercuryMarine.JsonFilename: string;     begin Result := 'mercury-marine.json'; end;
function TOBDOEMMercuryMarine.ManufacturerKey: string;  begin Result := 'MERCURY-MARINE';     end;
function TOBDOEMMercuryMarine.DisplayName: string;      begin Result := 'Mercury Marine (Brunswick)'; end;

function TOBDOEMVolvoPenta.JsonFilename: string;        begin Result := 'volvo-penta.json'; end;
function TOBDOEMVolvoPenta.ManufacturerKey: string;     begin Result := 'VOLVO-PENTA';      end;
function TOBDOEMVolvoPenta.DisplayName: string;         begin Result := 'Volvo Penta';      end;

function TOBDOEMYanmarMarine.JsonFilename: string;      begin Result := 'yanmar-marine.json'; end;
function TOBDOEMYanmarMarine.ManufacturerKey: string;   begin Result := 'YANMAR-MARINE';      end;
function TOBDOEMYanmarMarine.DisplayName: string;       begin Result := 'Yanmar Marine';      end;

function TOBDOEMMtu.JsonFilename: string;               begin Result := 'mtu.json'; end;
function TOBDOEMMtu.ManufacturerKey: string;            begin Result := 'MTU';      end;
function TOBDOEMMtu.DisplayName: string;                begin Result := 'MTU (Rolls-Royce Power Systems)'; end;

function TOBDOEMCumminsMarine.JsonFilename: string;     begin Result := 'cummins-marine.json'; end;
function TOBDOEMCumminsMarine.ManufacturerKey: string;  begin Result := 'CUMMINS-MARINE';      end;
function TOBDOEMCumminsMarine.DisplayName: string;      begin Result := 'Cummins Marine';      end;

function TOBDOEMYamahaMarine.JsonFilename: string;      begin Result := 'yamaha-marine.json'; end;
function TOBDOEMYamahaMarine.ManufacturerKey: string;   begin Result := 'YAMAHA-MARINE';      end;
function TOBDOEMYamahaMarine.DisplayName: string;       begin Result := 'Yamaha Marine';      end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMMercuryMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMVolvoPenta.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYanmarMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMMtu.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMCumminsMarine.Create);
  TOBDOEMRegistry.RegisterExtension(TOBDOEMYamahaMarine.Create);

end.
