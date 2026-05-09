//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Stellantis.pas
// CONTENTS       : Reference Stellantis OEM extension (FCA + PSA brands)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Stellantis covers the merged FCA (Fiat / Chrysler /
//                  Jeep / Dodge / Ram / Alfa Romeo / Maserati) and PSA
//                  (Peugeot / Citroen / DS / Opel / Vauxhall) brand
//                  portfolios. The two camps still use distinct
//                  diagnostic stacks (FCA wiTech vs PSA Lexia/DiagBox)
//                  but converge on UDS for new platforms; this catalog
//                  targets the UDS overlap.
//------------------------------------------------------------------------------
unit OBD.OEM.Stellantis;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>
  ///   Stellantis blends DiagBox (PSA) and wiTech (FCA). The shared
  ///   choreography is plain 10 03 with a follow-up 22 F198 for PSA-
  ///   side ECUs (BSI / NAC) and nothing for FCA-side. We always
  ///   send the F198 read — FCA modules tolerate it (they reply
  ///   "0x7F 22 31 — requestOutOfRange" which the runner treats as
  ///   non-fatal because the step's <c>ExpectedResponse</c> is empty).
  /// </summary>
  TOBDStellantisSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionStellantis = class(TOBDOEMExtensionBase)
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

function TOBDStellantisSessionNegotiator.BeginSessionPlan(
  SessionType: TOBDSessionType;
  const ECUAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(SessionType, ECUAddress);
  if SessionType = sstDefault then Exit;
  // PSA DiagBox always probes F198 after 10 03. ExpectedResponse left
  // empty so FCA's negative-response 7F 22 31 doesn't fail the plan.
  Result.Steps := Result.Steps + [
    UDSStep(TBytes.Create($22, $F1, $98),
      'Stellantis workshop-code probe (PSA only — FCA may NACK)')
  ];
end;

function TOBDStellantisSessionNegotiator.DisplayName: string;
begin
  Result := 'Stellantis DiagBox / wiTech';
end;

function TOBDOEMExtensionStellantis.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDStellantisSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionStellantis.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // PSA legacy BSI / FCA legacy Body Computer modules accepted the
  // KWP2000 textbook two's-complement at Level 1. Modern DiagBox /
  // wiTech use proprietary algorithms — replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionStellantis.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionStellantis.DtcCatalogFileName: string;
begin Result := 'dtc-stellantis.json'; end;

function TOBDOEMExtensionStellantis.ManufacturerKey: string;
begin Result := 'STLA'; end;
function TOBDOEMExtensionStellantis.DisplayName: string;
begin Result := 'Stellantis (FCA + PSA)'; end;

function TOBDOEMExtensionStellantis.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in stellantis.json.
  Result := VINMatchesCatalog('stellantis.json', VIN);
end;
procedure TOBDOEMExtensionStellantis.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are stellantis.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('stellantis.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionStellantis.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('stellantis.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
function TOBDOEMExtensionStellantis.DecodeDID(const DID: Word;
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
    $F199:
      if Length(Payload) >= 3 then
      begin
        // BCD-encoded YYMMDD.
        Result := Format('programming_date = 20%.2x-%.2x-%.2x',
          [Payload[0], Payload[1], Payload[2]]);
        Exit;
      end;
    $1A02:
      if Length(Payload) >= 3 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 16) or
                   (Cardinal(Payload[1]) shl 8)  or
                    Cardinal(Payload[2]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $1B01:
      if Length(Payload) >= 1 then
      begin
        Result := Format('fuel_level = %d %%', [Payload[0]]);
        Exit;
      end;
    $1B02:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('engine_run_time = %d s', [Seconds]);
        Exit;
      end;
    $1B03:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Cardinal(Payload[0]) shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V', [Voltage / 1000.0]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionStellantis.Create);

end.
