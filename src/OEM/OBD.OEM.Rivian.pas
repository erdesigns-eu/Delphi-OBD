//------------------------------------------------------------------------------
//  OBD.OEM.Rivian
//
//  Rivian Automotive, Inc. OEM extension. EV-only line-up (R1T
//  pickup, R1S SUV, EDV commercial vans). Catalogue + DTC
//  overlay in <c>catalogs/rivian.json</c> +
//  <c>catalogs/dtc-rivian.json</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Rivian;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Rivian OEM extension.</summary>
  TOBDOEMExtensionRivian = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionRivian.ManufacturerKey: string;
begin
  Result := 'RIVIAN';
end;

function TOBDOEMExtensionRivian.DisplayName: string;
begin
  Result := 'Rivian Automotive, Inc.';
end;

function TOBDOEMExtensionRivian.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('rivian.json', VIN);
end;

procedure TOBDOEMExtensionRivian.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('rivian.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionRivian.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('rivian.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionRivian.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Rivian uses an in-house service-mode unlock. The textbook
  // two's-complement is the starter placeholder for lab fixtures;
  // production callers register the real implementation via
  // RegisterAlgorithm.
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionRivian.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionRivian.DtcCatalogFileName: string;
begin
  Result := 'dtc-rivian.json';
end;

function TOBDOEMExtensionRivian.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  FieldName: string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2:
      if Length(Payload) > 0 then
      begin
        case DID of
          $F1A0: FieldName := 'rivian_model_code';
          $F1A2: FieldName := 'rivian_drivetrain';
        else
          FieldName := 'unknown';
        end;
        Result := Format('%s = "%s"',
          [FieldName, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionRivian.Create);

end.
