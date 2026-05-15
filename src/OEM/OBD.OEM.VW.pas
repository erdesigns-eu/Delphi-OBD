//------------------------------------------------------------------------------
//  OBD.OEM.VW
//
//  VW Group (Volkswagen / Audi / SEAT / Škoda) OEM extension.
//  Covers the four common WMIs in the VAG family; vehicle
//  catalogue (DIDs, routines, ECUs, coding blocks, adaptations,
//  actuator tests, live PIDs, DTC extended-data records) lives
//  in <c>catalogs/vw.json</c>. The shipped UDS baseline overlay
//  (<c>catalogs/uds-standard.json</c>) is layered on top so
//  callers always have the J2012 / ISO 14229 generic DIDs even
//  before the VAG-specific catalogue resolves.
//
//  Session negotiator overrides
//  <see cref="TOBDStandardSessionNegotiator.BeginSessionPlan"/>
//  to prepend the <c>AT SH</c> + <c>AT CRA</c> handshake VCDS
//  and ODIS issue before the UDS request — MQB-platform ECUs
//  occasionally reject the session-control request when CRA is
//  wrong. Tester-present cadence is 2000 ms (ODIS service mode 2).
//
//  Seed-key starter is the textbook KWP2000 two's-complement at
//  Level 1; production users register their NDA algorithm via
//  <see cref="TOBDSeedKeyRegistry.RegisterAlgorithm"/>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1 §9 DiagnosticSessionControl
//    - ISO 15765-4 (CAN-ID + 8 response convention)
//    - VCDS / ODIS public service literature
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.VW;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>
  ///   VAG-specific session negotiator. Adds the
  ///   <c>AT SH &lt;ECU&gt;</c> + <c>AT CRA &lt;ECU+8&gt;</c>
  ///   handshake before the UDS DiagnosticSessionControl
  ///   request.
  /// </summary>
  TOBDVWSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    /// <summary>Builds the entry plan for <c>SessionType</c>
    /// against <c>ECUAddress</c>.</summary>
    /// <param name="ASessionType">UDS session sub-function
    /// (default / programming / extended).</param>
    /// <param name="AEcuAddress">UDS request address; 0 leaves
    /// the adapter header untouched.</param>
    /// <returns>Plan with the AT prelude prepended.</returns>
    function BeginSessionPlan(ASessionType: TOBDSessionType;
      const AEcuAddress: Word): TOBDSessionPlan; override;
    /// <summary>Display label for logs / audit.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>VAG OEM extension.</summary>
  TOBDOEMExtensionVW = class(TOBDOEMExtensionBase)
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

{ TOBDVWSessionNegotiator }

function TOBDVWSessionNegotiator.BeginSessionPlan(
  ASessionType: TOBDSessionType;
  const AEcuAddress: Word): TOBDSessionPlan;
var
  Sub: Byte;
  Steps: TArray<TOBDSessionStep>;
  ResponseAddr: Word;
begin
  Result := Default(TOBDSessionPlan);
  Sub := SessionTypeByte(ASessionType);
  Steps := nil;

  if AEcuAddress <> 0 then
  begin
    // ISO 15765-4: positive response CAN-ID = request + 8.
    ResponseAddr := AEcuAddress + 8;
    Steps := Steps + [
      ATStep('SH ' + FormatHeader(AEcuAddress),
        Format('Set request header to 0x%s',
          [FormatHeader(AEcuAddress)])),
      ATStep('CRA ' + FormatHeader(ResponseAddr),
        Format('Filter response to 0x%s',
          [FormatHeader(ResponseAddr)]))
    ];
  end;

  Steps := Steps + [
    UDSStep(TBytes.Create($10, Sub), TBytes.Create($50, Sub),
      Format('VAG DiagnosticSessionControl 0x%.2X', [Sub]))
  ];

  Result.Steps := Steps;
  if ASessionType = sstDefault then
    Result.TesterPresentMs := 0
  else
    Result.TesterPresentMs := DefaultTesterPresentMs;
  Result.TesterPresentRequest := TBytes.Create($3E, $80);
end;

function TOBDVWSessionNegotiator.DisplayName: string;
begin
  Result := 'VAG (ODIS / VCDS)';
end;

{ TOBDOEMExtensionVW }

function TOBDOEMExtensionVW.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDVWSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionVW.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Legacy VAG KWP2000 components (pre-2008 instrument clusters,
  // some MED9 ECMs) accept the textbook two's-complement at
  // Level 1. Verified=False — production callers replace this
  // with their proprietary algorithm via RegisterAlgorithm.
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionVW.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionVW.DtcCatalogFileName: string;
begin
  Result := 'dtc-vw.json';
end;

function TOBDOEMExtensionVW.ManufacturerKey: string;
begin
  Result := 'VAG';
end;

function TOBDOEMExtensionVW.DisplayName: string;
begin
  Result := 'Volkswagen Audi Group';
end;

function TOBDOEMExtensionVW.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('vw.json', VIN);
end;

procedure TOBDOEMExtensionVW.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('vw.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionVW.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('vw.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMExtensionVW.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Voltage: Word;
  Speed: Word;
begin
  // Hard-coded specialisations for the few DIDs whose unit
  // conversion is universally documented; everything else falls
  // through to the base hex-dump renderer.
  case DID of
    $F405:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Payload[0] shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V',
          [Voltage / 1000.0]);
        Exit;
      end;
    $F40D:
      if Length(Payload) >= 2 then
      begin
        Speed := (Payload[0] shl 8) or Payload[1];
        Result := Format('vehicle_speed = %d km/h', [Speed]);
        Exit;
      end;
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionVW.Create);

end.
