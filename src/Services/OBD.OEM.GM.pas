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
  System.SysUtils, OBD.OEM, OBD.OEM.Session;

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
    function CreateSessionNegotiator: IOBDSessionNegotiator; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers, OBD.OEM.Catalog.Loader;

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

function TOBDOEMExtensionGM.ManufacturerKey: string;
begin Result := 'GM'; end;
function TOBDOEMExtensionGM.DisplayName: string;
begin Result := 'General Motors'; end;

function TOBDOEMExtensionGM.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Chevrolet: 1G1 (US passenger), 1GC (US truck), 2G1 (CA), 3G1 (MX)
  // Buick: 1G4
  // Cadillac: 1G6
  // GMC: 1GT (truck), 2GT, 3GT
  // Pontiac (defunct): 1G2
  // Saturn (defunct): 1G8
  // Hummer: 5GR
  // Holden (defunct, AU): 6G1
  // Opel/Vauxhall (sold to Stellantis 2017 — pre-2017 W0L)
  Result :=
    (WMI = '1G1') or (WMI = '1G2') or (WMI = '1G4') or (WMI = '1G6') or
    (WMI = '1G8') or (WMI = '1GC') or (WMI = '1GT') or
    (WMI = '2G1') or (WMI = '2GT') or
    (WMI = '3G1') or (WMI = '3GT') or
    (WMI = '5GR') or (WMI = '6G1');
end;

procedure TOBDOEMExtensionGM.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // GM Global B / Global A bus map. The Tech 2 / GDS-2 module list maps
  // ECUs to GMLAN ARB-IDs; the canonical UDS request set is below.
  ECUs := [
    ECU($7E0, 'ecm',           'ECM — Engine Control'),
    ECU($7E1, 'tcm',           'TCM — Transmission Control'),
    ECU($241, 'ebcm',          'EBCM — ABS / Stability'),
    ECU($242, 'sdm',           'SDM — Sensing Diagnostic Module (Airbag)'),
    ECU($243, 'bcm',           'BCM — Body Control Module'),
    ECU($244, 'ipc',           'IPC — Instrument Panel Cluster'),
    ECU($245, 'eps',           'EPS — Electric Power Steering'),
    ECU($246, 'hvac',          'HVAC — Climate'),
    ECU($247, 'radio',         'Radio / Infotainment')
  ];

  // GM Global B: standard UDS DIDs for ECU identification + GM-specific
  // calibration block (CVN list, broadcast code).
  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'GM service part number'),
    DID($F188, 'ecu_part_number',           'ECU part number'),
    DID($F189, 'sw_version',                'Software version'),
    DID($F18A, 'system_supplier_id',        'ECU supplier id'),
    DID($F18C, 'ecu_serial_number',         'ECU serial number'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'broadcast_code',            'GM broadcast code (model-year tag)'),
    DID($F1A2, 'cvn_list',                  'Calibration Verification Number list'),
    DID($F1A4, 'engineering_part_number',   'GM engineering part number'),
    DID($1981, 'mileage',                   'Vehicle mileage in km'),
    DID($1982, 'engine_run_time',           'Engine run time (seconds)'),
    DID($1983, 'battery_voltage',           'Battery voltage (mV)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'crank_relearn',         'Crank position relearn'),
    Routine($0205, 'tps_relearn',           'Throttle-position-sensor relearn'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor calibration'),
    Routine($0F01, 'bcm_relearn',           'Body-control-module immobilizer relearn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF01, 'check_dependencies',    'Cross-ECU dependency check'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('gm.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
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
