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
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Chrysler: 1C3 / 1C4 / 1C6 / 2C3 / 2C4 / 3C3 / 3C4
  // Dodge: 1D4 / 1D7 / 2D4 / 2D8 / 3D4
  // Jeep: 1J4 / 1J8
  // Ram (formerly Dodge): 1RR / 3C6
  // Fiat: ZFA / ZFB / ZFC / 9BD
  // Alfa Romeo: ZAR
  // Maserati: ZAM
  // Peugeot: VF3
  // Citroen: VF7
  // DS Automobiles: VR1
  // Opel/Vauxhall (post-2017): W0L (legacy GM tag retained), VXR
  Result :=
    // FCA-side
    (WMI = '1C3') or (WMI = '1C4') or (WMI = '1C6') or
    (WMI = '2C3') or (WMI = '2C4') or
    (WMI = '3C3') or (WMI = '3C4') or (WMI = '3C6') or
    (WMI = '1D4') or (WMI = '1D7') or
    (WMI = '2D4') or (WMI = '2D8') or
    (WMI = '3D4') or
    (WMI = '1J4') or (WMI = '1J8') or
    (WMI = '1RR') or
    (WMI = 'ZFA') or (WMI = 'ZFB') or (WMI = 'ZFC') or (WMI = '9BD') or
    (WMI = 'ZAR') or (WMI = 'ZAM') or
    // PSA-side
    (WMI = 'VF3') or (WMI = 'VF7') or (WMI = 'VR1') or
    (WMI = 'W0L') or (WMI = 'VXR');
end;

procedure TOBDOEMExtensionStellantis.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // Stellantis covers PSA (DiagBox/Lexia), FCA (wiTech), and Opel/
  // Vauxhall (TIS2Web). Powertrain ECUs use the standard 0x7E0 range;
  // PSA BSI sits at 0x652, FCA Body Computer at 0x7A2.
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU'),
    ECU($7E1, 'transmission',   'Transmission'),
    ECU($652, 'bsi',            'PSA BSI — Body System Interface'),
    ECU($658, 'cluster_psa',    'PSA Instrument Cluster (NAC/RNEG)'),
    ECU($7A2, 'bcm_fca',        'FCA Body Computer Module'),
    ECU($7A0, 'cluster_fca',    'FCA Instrument Cluster'),
    ECU($760, 'abs',            'ABS / ESP'),
    ECU($731, 'srs',            'SRS / Airbag'),
    ECU($793, 'gateway',        'CAN Gateway')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Stellantis service part number'),
    DID($F188, 'ecu_part_number',           'ECU part number'),
    DID($F189, 'sw_version',                'Software version'),
    DID($F18A, 'system_supplier_id',        'ECU supplier id'),
    DID($F18C, 'ecu_serial_number',         'ECU serial number'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F198, 'last_workshop_code',        'Last workshop code (PSA: BSI; FCA: wiTech tag)'),
    DID($F199, 'programming_date',          'Last programming date (YYMMDD BCD)'),
    DID($F1A8, 'fca_calibration_id',        'FCA calibration id'),
    DID($F1B0, 'psa_brand_code',            'PSA brand id (model fingerprint)'),
    DID($1A02, 'mileage',                   'Vehicle mileage in km'),
    DID($1B01, 'fuel_level',                'Fuel level (%)'),
    DID($1B02, 'engine_run_time',           'Engine run time (seconds)'),
    DID($1B03, 'battery_voltage',           'Battery voltage (mV)')
  ];

  Routines := [
    Routine($0202, 'reset_service_indicator', 'Reset service-due indicator'),
    Routine($0203, 'reset_adaptations',       'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',         'Steering-angle sensor calibration'),
    Routine($0F01, 'bsi_telecoding',          'PSA BSI telecoding'),
    Routine($0F02, 'wiTech_proxi_align',      'FCA wiTech proxi alignment'),
    Routine($FF00, 'erase_memory',            'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',         'Post-flash checksum verification')
  ];

  MergeCatalogJSON('stellantis.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
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
