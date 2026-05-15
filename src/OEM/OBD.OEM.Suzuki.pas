//------------------------------------------------------------------------------
//  OBD.OEM.Suzuki
//
//  Suzuki Motor Corp. (incl. Maruti Suzuki India) OEM extension.
//  Catalogue + DTC overlay in <c>catalogs/suzuki.json</c> +
//  <c>catalogs/dtc-suzuki.json</c>.
//
//  Seed-key starter is the KWP2000 two's-complement accepted by
//  legacy SDT diagnostic tools; production callers register the
//  modern Suzuki Diagnostic Tool algorithm via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Suzuki;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Suzuki OEM extension.</summary>
  TOBDOEMExtensionSuzuki = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionSuzuki.ManufacturerKey: string;
begin
  Result := 'SUZUKI';
end;

function TOBDOEMExtensionSuzuki.DisplayName: string;
begin
  Result := 'Suzuki Motor Corp. (incl. Maruti Suzuki India)';
end;

function TOBDOEMExtensionSuzuki.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('suzuki.json', VIN);
end;

procedure TOBDOEMExtensionSuzuki.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('suzuki.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionSuzuki.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('suzuki.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionSuzuki.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionSuzuki.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionSuzuki.DtcCatalogFileName: string;
begin
  Result := 'dtc-suzuki.json';
end;

function TOBDOEMExtensionSuzuki.DecodeDID(const DID: Word;
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
    $F1A0:
      if Length(Payload) > 0 then
      begin
        Result := Format('suzuki_chassis_code = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionSuzuki.Create);

end.
