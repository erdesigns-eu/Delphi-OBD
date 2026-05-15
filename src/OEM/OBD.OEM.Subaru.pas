//------------------------------------------------------------------------------
//  OBD.OEM.Subaru
//
//  Subaru Corporation OEM extension. Catalogue + DTC overlay in
//  <c>catalogs/subaru.json</c> + <c>catalogs/dtc-subaru.json</c>.
//
//  Seed-key starter is the SSM-community byte-rotate placeholder
//  (2-byte 'SB' mask, rotate 4); production callers register the
//  modern Subaru Select Monitor algorithm via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Subaru;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Subaru OEM extension.</summary>
  TOBDOEMExtensionSubaru = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionSubaru.ManufacturerKey: string;
begin
  Result := 'SUBARU';
end;

function TOBDOEMExtensionSubaru.DisplayName: string;
begin
  Result := 'Subaru Corporation';
end;

function TOBDOEMExtensionSubaru.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('subaru.json', VIN);
end;

procedure TOBDOEMExtensionSubaru.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('subaru.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionSubaru.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('subaru.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionSubaru.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  MASK: array[0..1] of Byte = ($53, $42);  // 'SB'
var
  Mask: TBytes;
begin
  // Pre-2012 SSM-documented byte-rotate (rotate 4, 'SB' mask).
  // Production callers replace via RegisterAlgorithm.
  SetLength(Mask, Length(MASK));
  Move(MASK[0], Mask[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyByteRotate.Create(
    1, 4, Mask,
    'Subaru SSM community byte-rotate placeholder',
    'community-pr', False));
end;

procedure TOBDOEMExtensionSubaru.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionSubaru.DtcCatalogFileName: string;
begin
  Result := 'dtc-subaru.json';
end;

function TOBDOEMExtensionSubaru.DecodeDID(const DID: Word;
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
        Result := Format('subaru_chassis_code = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionSubaru.Create);

end.
