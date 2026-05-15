//------------------------------------------------------------------------------
//  OBD.OEM.AstonMartin
//
//  Aston Martin Lagonda Ltd. OEM extension. UK-built sports
//  cars; Ford-derived powertrains pre-2018 (V8 Vantage, DB9 with
//  Ford / Volvo V8) and AMG-derived post-2017 (DB11, DBX, V8
//  Vantage 2018+). Catalogue + DTC overlay in
//  <c>catalogs/aston-martin.json</c> +
//  <c>catalogs/dtc-aston-martin.json</c>.
//
//  Seed-key starter is the Ford-lineage byte-rotate placeholder
//  matching pre-2018 Visteon PCM behaviour; AMG-era ECUs use
//  Mercedes XENTRY seed-key which production callers register
//  via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.AstonMartin;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Aston Martin OEM extension.</summary>
  TOBDOEMExtensionAstonMartin = class(TOBDOEMExtensionBase)
  protected
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
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word;
      const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers,
  OBD.OEM.Catalog.Loader,
  OBD.OEM.DTC.Loader;

function TOBDOEMExtensionAstonMartin.ManufacturerKey: string;
begin
  Result := 'ASTON_MARTIN';
end;

function TOBDOEMExtensionAstonMartin.DisplayName: string;
begin
  Result := 'Aston Martin Lagonda Ltd.';
end;

function TOBDOEMExtensionAstonMartin.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('aston-martin.json', VIN);
end;

procedure TOBDOEMExtensionAstonMartin.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('aston-martin.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionAstonMartin.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('aston-martin.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionAstonMartin.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  MASK: array[0..1] of Byte = ($EF, $CD);
var
  Mask: TBytes;
begin
  // Ford-lineage byte-rotate (pre-2018 Visteon PCM family).
  // Modern AMG-derived ECUs use Mercedes XENTRY crypto;
  // production callers replace via RegisterAlgorithm.
  SetLength(Mask, Length(MASK));
  Move(MASK[0], Mask[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyByteRotate.Create(
    1, 3, Mask,
    'Aston Martin (Ford-lineage) byte-rotate placeholder',
    'forscan-community', False));
end;

procedure TOBDOEMExtensionAstonMartin.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionAstonMartin.DtcCatalogFileName: string;
begin
  Result := 'dtc-aston-martin.json';
end;

function TOBDOEMExtensionAstonMartin.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4, $F1A6, $F1A8:
      if Length(Payload) > 0 then
      begin
        Result := Format('aml_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionAstonMartin.Create);

end.
