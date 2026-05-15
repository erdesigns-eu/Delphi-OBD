//------------------------------------------------------------------------------
//  OBD.OEM.DetroitDiesel
//
//  Detroit Diesel Corp. (Daimler Truck) OEM extension — engine
//  supplier for Freightliner / Western Star Class 8 trucks plus
//  industrial / marine gensets. Catalogue + DTC overlay in
//  <c>catalogs/detroit.json</c> + <c>catalogs/dtc-detroit.json</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.DetroitDiesel;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC,
  OBD.OEM.HD;

type
  /// <summary>Detroit Diesel engine-OEM extension.</summary>
  TOBDOEMExtensionDetroitDiesel = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionDetroitDiesel.ManufacturerKey: string;
begin
  Result := 'DDC';
end;

function TOBDOEMExtensionDetroitDiesel.DisplayName: string;
begin
  Result := 'Detroit Diesel Corp. (engine OEM)';
end;

function TOBDOEMExtensionDetroitDiesel.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('detroit.json', VIN);
end;

procedure TOBDOEMExtensionDetroitDiesel.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('detroit.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionDetroitDiesel.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('detroit.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionDetroitDiesel.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionDetroitDiesel.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionDetroitDiesel.DtcCatalogFileName: string;
begin
  Result := 'dtc-detroit.json';
end;

function TOBDOEMExtensionDetroitDiesel.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  FieldName: string;
begin
  case DID of
    $F1A0, $F1A1, $F1A2:
      if Length(Payload) > 0 then
      begin
        case DID of
          $F1A0: FieldName := 'detroit_engine_serial';
          $F1A1: FieldName := 'detroit_calibration_id';
          $F1A2: FieldName := 'detroit_emissions_family';
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
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionDetroitDiesel.Create);

end.
