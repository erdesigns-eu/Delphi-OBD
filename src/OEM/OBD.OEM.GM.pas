//------------------------------------------------------------------------------
//  OBD.OEM.GM
//
//  General Motors OEM extension. Covers GMLAN ECUs across the
//  Chevrolet / Buick / Cadillac / GMC / Holden / Opel-pre-2017
//  WMIs. Catalogue + DTC overlay in <c>catalogs/gm.json</c> and
//  <c>catalogs/dtc-gm.json</c>.
//
//  TOBDGMSessionNegotiator prepends an <c>AT SP 6</c> protocol
//  lock so the adapter is on ISO 15765-4 11-bit / 500 kbps
//  (GMLAN) regardless of any previous auto-protocol selection.
//
//  Seed-key starter is the documented GMLAN Class B "trial mode"
//  constant key (four zero bytes), accepted by some pre-2010 body
//  modules; modern GDS-2 / Tech 2 uses proprietary algorithms
//  which production callers register via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.GM;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>GM Tech 2 / GDS-2 session negotiator.</summary>
  TOBDGMSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    /// <summary>Builds the entry plan; for non-default sessions
    /// prepends an AT SP 6 protocol-lock step.</summary>
    function BeginSessionPlan(ASessionType: TOBDSessionType;
      const AEcuAddress: Word): TOBDSessionPlan; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>GM OEM extension.</summary>
  TOBDOEMExtensionGM = class(TOBDOEMExtensionBase)
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

{ TOBDGMSessionNegotiator }

function TOBDGMSessionNegotiator.BeginSessionPlan(
  ASessionType: TOBDSessionType;
  const AEcuAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(ASessionType, AEcuAddress);
  if ASessionType = sstDefault then
    Exit;
  Result.Steps := [
    ATStep('SP 6',
      'Lock ELM327 to ISO 15765-4 11-bit/500 kbps (GMLAN)')
  ] + Result.Steps;
end;

function TOBDGMSessionNegotiator.DisplayName: string;
begin
  Result := 'GM Tech 2 / GDS-2';
end;

{ TOBDOEMExtensionGM }

function TOBDOEMExtensionGM.CreateSessionNegotiator:
  IOBDSessionNegotiator;
begin
  Result := TOBDGMSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionGM.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  // GMLAN Class B reference "trial mode" — some pre-2010 body
  // modules accept a fixed 4-byte zero key at Level 1. Production
  // callers replace via RegisterAlgorithm.
  CONST_KEY: array[0..3] of Byte = ($00, $00, $00, $00);
var
  K: TBytes;
begin
  SetLength(K, Length(CONST_KEY));
  Move(CONST_KEY[0], K[0], Length(CONST_KEY));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyConstant.Create(K,
    'GMLAN Class B trial-mode constant key',
    'gmlan-public', False));
end;

procedure TOBDOEMExtensionGM.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionGM.DtcCatalogFileName: string;
begin
  Result := 'dtc-gm.json';
end;

function TOBDOEMExtensionGM.ManufacturerKey: string;
begin
  Result := 'GM';
end;

function TOBDOEMExtensionGM.DisplayName: string;
begin
  Result := 'General Motors';
end;

function TOBDOEMExtensionGM.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('gm.json', VIN);
end;

procedure TOBDOEMExtensionGM.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('gm.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionGM.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('gm.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMExtensionGM.DecodeDID(const DID: Word;
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
    $1981:
      if Length(Payload) >= 4 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8) or
                    Cardinal(Payload[3]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $1982:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8) or
                    Cardinal(Payload[3]);
        Result := Format('engine_run_time = %d s', [Seconds]);
        Exit;
      end;
    $1983:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Cardinal(Payload[0]) shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V',
          [Voltage / 1000.0]);
        Exit;
      end;
    $F1A0, $F1A4:
      if Length(Payload) > 0 then
      begin
        case DID of
          $F1A0: FieldName := 'broadcast_code';
          $F1A4: FieldName := 'engineering_part_number';
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
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionGM.Create);

end.
