//------------------------------------------------------------------------------
//  OBD.OEM.Smart
//
//  smart Automobile Co. (Mercedes-Geely joint venture) OEM
//  extension. smart was originally a Mercedes-Benz Group brand
//  (1998, Hambach France, original two-seater). Since 2020 it
//  has been a 50/50 joint venture between Mercedes-Benz Group
//  and Geely Holding, with vehicles built in China (Xi'an) on
//  Geely's SEA platform. The current line-up (#1 / #3 / #5
//  SUV) is EV-only. Legacy Hambach two-seater models use the
//  older Mercedes XENTRY toolchain; Geely-era models use the
//  smart Workshop System.
//
//  Catalogue + DTC overlay live in <c>catalogs/smart.json</c>
//  and <c>catalogs/dtc-smart.json</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Smart;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>smart OEM extension.</summary>
  TOBDOEMExtensionSmart = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionSmart.ManufacturerKey: string;
begin
  Result := 'SMART';
end;

function TOBDOEMExtensionSmart.DisplayName: string;
begin
  Result := 'smart Automobile Co. (Mercedes-Geely JV)';
end;

function TOBDOEMExtensionSmart.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('smart.json', VIN);
end;

procedure TOBDOEMExtensionSmart.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('smart.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionSmart.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('smart.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionSmart.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Modern Geely-era smart shares SEA-platform seed-key
  // algorithms with Polestar / Zeekr; legacy Hambach-era smart
  // used the Mercedes XENTRY two's-complement at Level 1.
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionSmart.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionSmart.DtcCatalogFileName: string;
begin
  Result := 'dtc-smart.json';
end;

function TOBDOEMExtensionSmart.DecodeDID(const DID: Word;
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
    $F1A0, $F1A2, $F1A4:
      if Length(Payload) > 0 then
      begin
        case DID of
          $F1A0: FieldName := 'smart_model_code';
          $F1A2: FieldName := 'smart_drivetrain';
          $F1A4: FieldName := 'smart_battery_pack';
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
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionSmart.Create);

end.
