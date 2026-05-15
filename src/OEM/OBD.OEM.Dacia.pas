//------------------------------------------------------------------------------
//  OBD.OEM.Dacia
//
//  Automobile Dacia SA (Renault Group, Romania) OEM extension.
//  Catalogue + DTC overlay in <c>catalogs/dacia.json</c> +
//  <c>catalogs/dtc-dacia.json</c>.
//
//  Seed-key starter inherits the Renault CLIP-community 'RNLT'
//  XOR-mask matching the shared platform; production callers
//  register the modern CLIP algorithm via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Dacia;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Dacia OEM extension.</summary>
  TOBDOEMExtensionDacia = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionDacia.ManufacturerKey: string;
begin
  Result := 'DACIA';
end;

function TOBDOEMExtensionDacia.DisplayName: string;
begin
  Result := 'Automobile Dacia SA (Renault Group)';
end;

function TOBDOEMExtensionDacia.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('dacia.json', VIN);
end;

procedure TOBDOEMExtensionDacia.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('dacia.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionDacia.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('dacia.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionDacia.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  MASK: array[0..3] of Byte = ($52, $4E, $4C, $54);  // 'RNLT'
var
  M: TBytes;
begin
  // Renault-Group lineage — same 'RNLT' XOR-mask the Renault
  // extension registers. Production callers replace via
  // RegisterAlgorithm.
  SetLength(M, Length(MASK));
  Move(MASK[0], M[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(M,
    'Dacia (Renault-lineage) XOR-mask placeholder',
    'community-pr', False));
end;

procedure TOBDOEMExtensionDacia.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionDacia.DtcCatalogFileName: string;
begin
  Result := 'dtc-dacia.json';
end;

function TOBDOEMExtensionDacia.DecodeDID(const DID: Word;
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
    $F1A0, $F1A2, $F1A4, $F1A6:
      if Length(Payload) > 0 then
      begin
        Result := Format('dacia_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionDacia.Create);

end.
