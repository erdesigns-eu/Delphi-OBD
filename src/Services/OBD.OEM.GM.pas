//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.GM.pas
// CONTENTS       : Reference General Motors OEM extension (Global B / GMLAN)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : GM uses two diagnostic stacks across its model years:
//                  GMLAN (legacy CAN-based, pre-2015) and Global B
//                  (UDS over ISO 13400 DoIP and ISO 15765 CAN, 2015+).
//                  This catalogue targets the modern Global B set;
//                  GMLAN-only ECUs are reachable via the standard SAE
//                  J1979 services already shipped in OBD.Service01..0A.
//------------------------------------------------------------------------------
unit OBD.OEM.GM;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>
  ///   GM Global B negotiator. GMLAN UDS uses standard 10 03 / 10 02
  ///   but Tech 2 always asserts <c>AT SP 6</c> (ISO 15765-4 11/500)
  ///   first to lock the protocol — relevant when the adapter was
  ///   left on auto-protocol from a different vehicle.
  /// </summary>
  TOBDGMSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan; override;
    function DisplayName: string; override;
  end;

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

function TOBDGMSessionNegotiator.BeginSessionPlan(
  SessionType: TOBDSessionType;
  const ECUAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(SessionType, ECUAddress);
  if SessionType = sstDefault then Exit;
  Result.Steps := [
    ATStep('SP 6', 'Lock ELM327 to ISO 15765-4 11-bit/500 kbps (GMLAN)')
  ] + Result.Steps;
end;

function TOBDGMSessionNegotiator.DisplayName: string;
begin
  Result := 'GM Tech 2 / GDS-2';
end;

function TOBDOEMExtensionGM.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDGMSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionGM.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  // Some pre-2010 GMLAN body modules accepted a fixed 4-byte key at
  // Level 1 (GMLAN AllNodes "trial" mode). The constant below is
  // documented in the public GMLAN Class B reference.
  CONST_KEY: array[0..3] of Byte = ($00, $00, $00, $00);
var
  K: TBytes;
begin
  SetLength(K, Length(CONST_KEY));
  Move(CONST_KEY[0], K[0], Length(CONST_KEY));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyConstant.Create(K,
    'GMLAN Class B trial-mode constant key', 'gmlan-public', False));
end;

procedure TOBDOEMExtensionGM.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionGM.DtcCatalogFileName: string;
begin Result := 'dtc-gm.json'; end;

function TOBDOEMExtensionGM.ManufacturerKey: string;
begin Result := 'GM'; end;
function TOBDOEMExtensionGM.DisplayName: string;
begin Result := 'General Motors'; end;

function TOBDOEMExtensionGM.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in gm.json.
  Result := VINMatchesCatalog('gm.json', VIN);
end;
procedure TOBDOEMExtensionGM.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are gm.json
  // + uds-standard.json. Hardcoded entries removed.


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
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $1981:
      if Length(Payload) >= 4 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $1982:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('engine_run_time = %d s', [Seconds]);
        Exit;
      end;
    $1983:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Cardinal(Payload[0]) shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V', [Voltage / 1000.0]);
        Exit;
      end;
    $F1A0, $F1A4:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'broadcast_code';
                    $F1A4: 'engineering_part_number';
                  else 'unknown';
                  end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionGM.Create);

end.
