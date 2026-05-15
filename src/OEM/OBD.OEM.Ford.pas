//------------------------------------------------------------------------------
//  OBD.OEM.Ford
//
//  Ford Motor Company OEM extension. Covers Ford US / Europe /
//  Lincoln WMIs. Catalogue + DTC overlay in
//  <c>catalogs/ford.json</c> and <c>catalogs/dtc-ford.json</c>.
//
//  TOBDFordSessionNegotiator prepends <c>AT ST 32</c> when
//  entering the programming session so the ELM327 OBD timeout
//  stretches to ~3.2 s — FDRS waits this long after <c>10 02</c>
//  before the ECU stabilises into programming mode.
//
//  Seed-key starter is the ForScan-community byte-rotate
//  placeholder used against pre-2010 Visteon PCMs; modern FDRS
//  uses proprietary algorithms which production callers register
//  via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Ford;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Ford IDS / FDRS session negotiator.</summary>
  TOBDFordSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    /// <summary>Builds the entry plan. For the programming
    /// session, prepends an AT ST 32 to extend the ELM327 OBD
    /// timeout.</summary>
    function BeginSessionPlan(ASessionType: TOBDSessionType;
      const AEcuAddress: Word): TOBDSessionPlan; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>Ford OEM extension.</summary>
  TOBDOEMExtensionFord = class(TOBDOEMExtensionBase)
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

{ TOBDFordSessionNegotiator }

function TOBDFordSessionNegotiator.BeginSessionPlan(
  ASessionType: TOBDSessionType;
  const AEcuAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(ASessionType, AEcuAddress);
  if ASessionType = sstProgramming then
    Result.Steps := [
      ATStep('ST 32',
        'Extend ELM327 OBD timeout to ~3.2 s for programming')
    ] + Result.Steps;
end;

function TOBDFordSessionNegotiator.DisplayName: string;
begin
  Result := 'Ford IDS / FDRS';
end;

{ TOBDOEMExtensionFord }

function TOBDOEMExtensionFord.CreateSessionNegotiator:
  IOBDSessionNegotiator;
begin
  Result := TOBDFordSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionFord.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  // Public ForScan-documented mask used against pre-2010 Visteon
  // PCMs. Modern FDRS modules use a proprietary algorithm —
  // production callers replace this via RegisterAlgorithm.
  MASK: array[0..1] of Byte = ($EF, $CD);
var
  Mask: TBytes;
begin
  SetLength(Mask, Length(MASK));
  Move(MASK[0], Mask[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyByteRotate.Create(
    1, 3, Mask, 'Ford community byte-rotate placeholder',
    'forscan-community', False));
end;

procedure TOBDOEMExtensionFord.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionFord.DtcCatalogFileName: string;
begin
  Result := 'dtc-ford.json';
end;

function TOBDOEMExtensionFord.ManufacturerKey: string;
begin
  Result := 'FORD';
end;

function TOBDOEMExtensionFord.DisplayName: string;
begin
  Result := 'Ford Motor Company';
end;

function TOBDOEMExtensionFord.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('ford.json', VIN);
end;

procedure TOBDOEMExtensionFord.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('ford.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionFord.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('ford.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMExtensionFord.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Mileage, Seconds, Voltage: Cardinal;
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
    $DD00:
      if Length(Payload) >= 4 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8) or
                    Cardinal(Payload[3]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $DE00:
      if Length(Payload) >= 1 then
      begin
        Result := Format('fuel_level = %d %%', [Payload[0]]);
        Exit;
      end;
    $DE01:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8) or
                    Cardinal(Payload[3]);
        Result := Format('engine_run_time = %d s', [Seconds]);
        Exit;
      end;
    $DE02:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Cardinal(Payload[0]) shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V',
          [Voltage / 1000.0]);
        Exit;
      end;
    $DF00, $DF01, $DF02:
      if Length(Payload) > 0 then
      begin
        case DID of
          $DF00: FieldName := 'pcm_strategy_id';
          $DF01: FieldName := 'calibration_id';
          $DF02: FieldName := 'calibration_verification';
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
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionFord.Create);

end.
