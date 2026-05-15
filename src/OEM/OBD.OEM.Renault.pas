//------------------------------------------------------------------------------
//  OBD.OEM.Renault
//
//  Renault Group (Renault / Dacia / Alpine) OEM extension.
//  Catalogue + DTC overlay in <c>catalogs/renault.json</c> +
//  <c>catalogs/dtc-renault.json</c>.
//
//  Seed-key starter is the 'RNLT' XOR-mask documented in the
//  CLIP community for pre-2014 EMS modules; modern CLIP uses
//  NDA crypto which production callers register via
//  RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Renault;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Renault OEM extension.</summary>
  TOBDOEMExtensionRenault = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionRenault.ManufacturerKey: string;
begin
  Result := 'RNLT';
end;

function TOBDOEMExtensionRenault.DisplayName: string;
begin
  Result := 'Renault Group (Renault / Dacia / Alpine)';
end;

function TOBDOEMExtensionRenault.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('renault.json', VIN);
end;

procedure TOBDOEMExtensionRenault.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('renault.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionRenault.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('renault.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionRenault.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  MASK: array[0..3] of Byte = ($52, $4E, $4C, $54);  // 'RNLT'
var
  M: TBytes;
begin
  SetLength(M, Length(MASK));
  Move(MASK[0], M[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(M,
    'Renault CLIP community ''RNLT'' XOR-mask placeholder',
    'community-pr', False));
end;

procedure TOBDOEMExtensionRenault.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionRenault.DtcCatalogFileName: string;
begin
  Result := 'dtc-renault.json';
end;

function TOBDOEMExtensionRenault.DecodeDID(const DID: Word;
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
          $F1A0: FieldName := 'renault_calibration_id';
          $F1A2: FieldName := 'renault_market_code';
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
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionRenault.Create);

end.
