//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Ford.pas
// CONTENTS       : Reference Ford OEM extension (FoCCCAN / IDS)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Ford;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>
  ///   Ford IDS / FDRS choreography. Programming sessions still use
  ///   the standard 10 02 sub-function, but the FDRS workflow
  ///   prepends an <c>AT ST 32</c> (3.2 second timeout) to absorb the
  ///   long ECU-pause that Ford modules take when entering
  ///   programming. Tester-present interval is 2000 ms.
  /// </summary>
  TOBDFordSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan; override;
    function DisplayName: string; override;
  end;

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
    procedure SeedDefaultSeedKeyAlgorithms(Reg: TOBDSeedKeyRegistry); override;
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); override;
    function DtcCatalogFileName: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers, OBD.OEM.Catalog.Loader, OBD.OEM.DTC.Loader;

function TOBDFordSessionNegotiator.BeginSessionPlan(
  SessionType: TOBDSessionType;
  const ECUAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(SessionType, ECUAddress);
  if SessionType = sstProgramming then
  begin
    // Prepend AT ST 32 — sets the ELM327 OBD response timeout to
    // 3.2 s (50 * 4 ms units, rounded). FDRS waits this long after
    // 10 02 before the ECU stabilises into programming mode.
    Result.Steps := [
      ATStep('ST 32', 'Extend ELM327 OBD timeout to ~3.2 s for programming')
    ] + Result.Steps;
  end;
end;

function TOBDFordSessionNegotiator.DisplayName: string;
begin
  Result := 'Ford IDS / FDRS';
end;

function TOBDOEMExtensionFord.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDFordSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionFord.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  // Public Ford ELM-bypass mask used in the ForScan documentation
  // for pre-2010 PCMs (Visteon supplier). FDRS modules use a
  // proprietary algorithm — replace at startup for production.
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

procedure TOBDOEMExtensionFord.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionFord.DtcCatalogFileName: string;
begin Result := 'dtc-ford.json'; end;

function TOBDOEMExtensionFord.ManufacturerKey: string;
begin Result := 'FORD'; end;
function TOBDOEMExtensionFord.DisplayName: string;
begin Result := 'Ford Motor Company'; end;

function TOBDOEMExtensionFord.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in ford.json.
  Result := VINMatchesCatalog('ford.json', VIN);
end;
procedure TOBDOEMExtensionFord.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are ford.json
  // + uds-standard.json. Hardcoded entries removed.


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
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $DD00:
      if Length(Payload) >= 4 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
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
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('engine_run_time = %d s', [Seconds]);
        Exit;
      end;
    $DE02:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Cardinal(Payload[0]) shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V', [Voltage / 1000.0]);
        Exit;
      end;
    $DF01, $DF02, $DF00:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = %s',
          [inherited DecodeDID(DID, []), TEncoding.ASCII.GetString(Payload)]);
        // Strip the trailing 'inherited' empty hex by re-formatting:
        Result := Format('%s = "%s"',
          [string(case DID of
                    $DF00: 'pcm_strategy_id';
                    $DF01: 'calibration_id';
                    $DF02: 'calibration_verification';
                  else  'unknown';
                  end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionFord.Create);

end.
