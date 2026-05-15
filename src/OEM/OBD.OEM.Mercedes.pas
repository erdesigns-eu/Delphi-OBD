//------------------------------------------------------------------------------
//  OBD.OEM.Mercedes
//
//  Mercedes-Benz Group OEM extension. Covers the Daimler
//  passenger-car WMIs (WDB / WDC / WDD / WDF) and Sprinter
//  commercial WMIs (WD3 / WD4 / 4JG). Catalogue +
//  DTC overlay live in <c>catalogs/mercedes.json</c> and
//  <c>catalogs/dtc-mercedes.json</c>.
//
//  TOBDMercedesSessionNegotiator appends a read of DID 0xF198
//  (workshop code) after the session-control reply because
//  XENTRY ECUs gate subsequent routines on a known last-writer
//  identity; the appended step uses an empty expected-response
//  prefix so the runner tolerates an NRC if the ECU refuses the
//  probe. Tester-present cadence matches XENTRY's 1500 ms.
//
//  Seed-key starter is the textbook KWP2000 two's-complement at
//  Level 1, accepted by pre-XENTRY HHTwin / Star Diagnosis
//  modules; production callers register the NDA algorithm via
//  RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Mercedes;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>XENTRY-style Mercedes session negotiator.</summary>
  TOBDMercedesSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    /// <summary>Builds the entry plan. For non-default sessions,
    /// appends a <c>22 F198</c> workshop-code probe.</summary>
    /// <param name="ASessionType">UDS session sub-function.</param>
    /// <param name="AEcuAddress">UDS request address; 0 leaves
    /// the adapter header untouched.</param>
    function BeginSessionPlan(ASessionType: TOBDSessionType;
      const AEcuAddress: Word): TOBDSessionPlan; override;
    /// <summary>1500 ms heartbeat.</summary>
    function DefaultTesterPresentMs: Cardinal; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>Mercedes-Benz OEM extension.</summary>
  TOBDOEMExtensionMercedes = class(TOBDOEMExtensionBase)
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

{ TOBDMercedesSessionNegotiator }

function TOBDMercedesSessionNegotiator.BeginSessionPlan(
  ASessionType: TOBDSessionType;
  const AEcuAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(ASessionType, AEcuAddress);
  if ASessionType = sstDefault then
    Exit;
  // XENTRY relies on the response to log the last writer; many
  // ECUs refuse subsequent routines if this probe is skipped
  // after entering extended / programming.
  Result.Steps := Result.Steps + [
    UDSStep(TBytes.Create($22, $F1, $98),
            TBytes.Create($62, $F1, $98),
      'XENTRY workshop-code probe (DID F198)')
  ];
end;

function TOBDMercedesSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 1500;
end;

function TOBDMercedesSessionNegotiator.DisplayName: string;
begin
  Result := 'Mercedes XENTRY';
end;

{ TOBDOEMExtensionMercedes }

function TOBDOEMExtensionMercedes.CreateSessionNegotiator:
  IOBDSessionNegotiator;
begin
  Result := TOBDMercedesSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionMercedes.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-XENTRY HHTwin / Star Diagnosis modules accept the
  // textbook two's-complement on legacy KWP2000 ECUs (EZS / EIS
  // readers). The modern XENTRY algorithm is NDA-protected;
  // production callers replace this via RegisterAlgorithm.
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionMercedes.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMercedes.DtcCatalogFileName: string;
begin
  Result := 'dtc-mercedes.json';
end;

function TOBDOEMExtensionMercedes.ManufacturerKey: string;
begin
  Result := 'MB';
end;

function TOBDOEMExtensionMercedes.DisplayName: string;
begin
  Result := 'Mercedes-Benz Group';
end;

function TOBDOEMExtensionMercedes.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('mercedes.json', VIN);
end;

procedure TOBDOEMExtensionMercedes.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('mercedes.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionMercedes.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('mercedes.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMExtensionMercedes.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Mileage: Cardinal;
  Seconds: Cardinal;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F18B:
      if Length(Payload) >= 3 then
      begin
        // BCD-encoded YY MM DD.
        Result := Format(
          'manufacturing_date = 20%.2x-%.2x-%.2x',
          [Payload[0], Payload[1], Payload[2]]);
        Exit;
      end;
    $0202:
      if Length(Payload) >= 3 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 16) or
                   (Cardinal(Payload[1]) shl 8) or
                    Cardinal(Payload[2]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $0203:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8) or
                    Cardinal(Payload[3]);
        Result := Format('engine_running = %d s (%.1f h)',
          [Seconds, Seconds / 3600.0]);
        Exit;
      end;
    $F19E:
      if Length(Payload) >= 1 then
      begin
        case Payload[0] of
          $00: Result := 'programming_status = OK';
          $01: Result := 'programming_status = INTERRUPTED';
          $02: Result := 'programming_status = CHECKSUM_INVALID';
          $03: Result := 'programming_status = HW_INCOMPATIBLE';
        else
          Result := Format('programming_status = 0x%.2X',
            [Payload[0]]);
        end;
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMercedes.Create);

end.
