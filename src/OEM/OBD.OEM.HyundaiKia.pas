//------------------------------------------------------------------------------
//  OBD.OEM.HyundaiKia
//
//  Hyundai Motor Group OEM extension. Covers Hyundai + Kia +
//  Genesis WMIs. Catalogue + DTC overlay in
//  <c>catalogs/hmg.json</c> + <c>catalogs/dtc-hmg.json</c>.
//
//  TOBDHyundaiKiaSessionNegotiator runs the inherited plan but
//  picks a 1500 ms tester-present cadence — GDS / KDS keep the
//  extended session alive at that rate on K-line and CAN ECUs.
//
//  Seed-key starter is the community XOR-mask placeholder using
//  the HMC marker bytes; production callers register the GDS
//  algorithm via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.HyundaiKia;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Hyundai/Kia GDS / KDS session negotiator.</summary>
  TOBDHyundaiKiaSessionNegotiator =
    class(TOBDStandardSessionNegotiator)
  public
    /// <summary>1500 ms heartbeat.</summary>
    function DefaultTesterPresentMs: Cardinal; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>HMG OEM extension.</summary>
  TOBDOEMExtensionHyundaiKia = class(TOBDOEMExtensionBase)
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
    function CreateSessionNegotiator: IOBDSessionNegotiator; override;
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

{ TOBDHyundaiKiaSessionNegotiator }

function TOBDHyundaiKiaSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 1500;
end;

function TOBDHyundaiKiaSessionNegotiator.DisplayName: string;
begin
  Result := 'Hyundai/Kia GDS / KDS';
end;

{ TOBDOEMExtensionHyundaiKia }

function TOBDOEMExtensionHyundaiKia.ManufacturerKey: string;
begin
  Result := 'HMG';
end;

function TOBDOEMExtensionHyundaiKia.DisplayName: string;
begin
  Result := 'Hyundai Motor Group (Hyundai / Kia / Genesis)';
end;

function TOBDOEMExtensionHyundaiKia.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('hmg.json', VIN);
end;

function TOBDOEMExtensionHyundaiKia.CreateSessionNegotiator:
  IOBDSessionNegotiator;
begin
  Result := TOBDHyundaiKiaSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionHyundaiKia.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('hmg.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionHyundaiKia.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('hmg.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionHyundaiKia.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  PUBLIC_MASK: array[0..3] of Byte = ($48, $4D, $43, $00);  // 'HMC'
var
  Mask: TBytes;
begin
  SetLength(Mask, Length(PUBLIC_MASK));
  Move(PUBLIC_MASK[0], Mask[0], Length(PUBLIC_MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(Mask,
    'HMG community XOR-mask placeholder',
    'community-pr', False));
end;

procedure TOBDOEMExtensionHyundaiKia.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionHyundaiKia.DtcCatalogFileName: string;
begin
  Result := 'dtc-hmg.json';
end;

function TOBDOEMExtensionHyundaiKia.DecodeDID(const DID: Word;
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
    $F193, $F1A0, $F1B0:
      if Length(Payload) > 0 then
      begin
        case DID of
          $F193: FieldName := 'hmg_rom_id';
          $F1A0: FieldName := 'hmg_calibration_id';
          $F1B0: FieldName := 'hmg_vehicle_option_code';
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
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionHyundaiKia.Create);

end.
