//------------------------------------------------------------------------------
//  OBD.OEM.Stellantis
//
//  Stellantis (FCA + PSA) OEM extension. Covers the Fiat,
//  Chrysler, Jeep, Dodge, RAM, Alfa Romeo, Lancia, Maserati,
//  Peugeot, Citroën, DS, Opel and Vauxhall WMIs.
//
//  Session negotiator extends the inherited plan with an
//  optional DID 0xF198 workshop-code probe — PSA DiagBox always
//  reads it; FCA modules may NACK. The expected-response prefix
//  is left empty so a negative <c>7F 22 31</c> from an FCA ECU
//  does not fail the plan.
//
//  Catalogue + DTC overlay live in
//  <c>catalogs/stellantis.json</c> and
//  <c>catalogs/dtc-stellantis.json</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Stellantis;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Stellantis DiagBox / wiTech session
  /// negotiator.</summary>
  TOBDStellantisSessionNegotiator =
    class(TOBDStandardSessionNegotiator)
  public
    /// <summary>Builds the entry plan. For non-default sessions,
    /// appends the optional F198 workshop-code probe.</summary>
    function BeginSessionPlan(ASessionType: TOBDSessionType;
      const AEcuAddress: Word): TOBDSessionPlan; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>Stellantis OEM extension.</summary>
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

{ TOBDStellantisSessionNegotiator }

function TOBDStellantisSessionNegotiator.BeginSessionPlan(
  ASessionType: TOBDSessionType;
  const AEcuAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(ASessionType, AEcuAddress);
  if ASessionType = sstDefault then
    Exit;
  // PSA DiagBox always probes F198 after 10 03. Expected-response
  // prefix is left empty so FCA's 7F 22 31 negative-response does
  // not fail the plan.
  Result.Steps := Result.Steps + [
    UDSStep(TBytes.Create($22, $F1, $98),
      'Stellantis workshop-code probe (PSA — FCA may NACK)')
  ];
end;

function TOBDStellantisSessionNegotiator.DisplayName: string;
begin
  Result := 'Stellantis DiagBox / wiTech';
end;

{ TOBDOEMExtensionStellantis }

function TOBDOEMExtensionStellantis.CreateSessionNegotiator:
  IOBDSessionNegotiator;
begin
  Result := TOBDStellantisSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionStellantis.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // PSA legacy BSI + FCA legacy Body Computer modules accepted
  // the KWP2000 two's-complement at Level 1. Modern DiagBox /
  // wiTech use proprietary algorithms; production callers
  // replace this via RegisterAlgorithm.
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionStellantis.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionStellantis.DtcCatalogFileName: string;
begin
  Result := 'dtc-stellantis.json';
end;

function TOBDOEMExtensionStellantis.ManufacturerKey: string;
begin
  Result := 'STLA';
end;

function TOBDOEMExtensionStellantis.DisplayName: string;
begin
  Result := 'Stellantis (FCA + PSA)';
end;

function TOBDOEMExtensionStellantis.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('stellantis.json', VIN);
end;

procedure TOBDOEMExtensionStellantis.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
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
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F199:
      if Length(Payload) >= 3 then
      begin
        // BCD-encoded YY MM DD.
        Result := Format(
          'programming_date = 20%.2x-%.2x-%.2x',
          [Payload[0], Payload[1], Payload[2]]);
        Exit;
      end;
    $1A02:
      if Length(Payload) >= 3 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 16) or
                   (Cardinal(Payload[1]) shl 8) or
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
                   (Cardinal(Payload[2]) shl 8) or
                    Cardinal(Payload[3]);
        Result := Format('engine_run_time = %d s', [Seconds]);
        Exit;
      end;
    $1B03:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Cardinal(Payload[0]) shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V',
          [Voltage / 1000.0]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionStellantis.Create);

end.
