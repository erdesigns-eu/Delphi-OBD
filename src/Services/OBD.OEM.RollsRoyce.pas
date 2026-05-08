//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.RollsRoyce.pas
// CONTENTS       : Rolls-Royce Motor Cars (BMW Group) OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Rolls-Royce Motor Cars Ltd. — Goodwood, England.
//                  BMW Group sub-brand; uses BMW E-Sys / ISTA stack
//                  with Rolls-Royce-specific extensions. Catalog
//                  covers Phantom (RR1 / 8th-gen) + Ghost (RR21) +
//                  Cullinan (RR31) + Spectre (RR23 EV) + Black Badge
//                  trims.
//------------------------------------------------------------------------------
unit OBD.OEM.RollsRoyce;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  /// <summary>Rolls-Royce inherits the BMW Group session-negotiation
  /// lineage: security access required for extended + programming;
  /// 1500 ms heartbeat for older DMEs.</summary>
  TOBDRRSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean; override;
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionRollsRoyce = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    function CreateSessionNegotiator: IOBDSessionNegotiator; override;
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

function TOBDRRSessionNegotiator.RequiresSecurityAccess(
  SessionType: TOBDSessionType): Boolean;
begin
  Result := SessionType in [sstExtendedDiagnostic, sstProgramming,
                            sstOEMSpecific1, sstOEMSpecific2];
end;

function TOBDRRSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin Result := 1500; end;

function TOBDRRSessionNegotiator.DisplayName: string;
begin Result := 'Rolls-Royce (BMW E-Sys / ISTA)'; end;

function TOBDOEMExtensionRollsRoyce.ManufacturerKey: string;
begin Result := 'ROLLS_ROYCE'; end;

function TOBDOEMExtensionRollsRoyce.DisplayName: string;
begin Result := 'Rolls-Royce Motor Cars Ltd.'; end;

function TOBDOEMExtensionRollsRoyce.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // SCA — Rolls-Royce Goodwood (sole assembly plant).
  Result := (WMI = 'SCA');
end;

function TOBDOEMExtensionRollsRoyce.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDRRSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionRollsRoyce.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // BMW UDS request CAN-IDs — Rolls-Royce uses the same address
  // map as F-series / G-series BMWs.
  ECUs := [
    ECU($12, 'dme',           'DME — N74 6.75L V12 (Phantom/Cullinan/Ghost) / N63 4.4L V8'),
    ECU($07, 'egs',           'EGS — ZF 8HP'),
    ECU($29, 'dsc',           'DSC — Stability Control + Magic Carpet ride'),
    ECU($40, 'kombi',         'KOMBI — Rolls-Royce instrument cluster'),
    ECU($60, 'frm',           'FRM — Footwell Module'),
    ECU($72, 'cas',           'CAS — Car Access System'),
    ECU($10, 'gateway',       'ZGW — Central Gateway'),
    ECU($63, 'idrive',        'iDrive / Spirit (Phantom 8 / Ghost / Spectre OS)'),
    ECU($65, 'climate',       'IHKA — 4-zone climate'),
    ECU($30, 'srs',           'ACSM — Crash Safety + 8-airbag'),
    ECU($66, 'rear_seat_ent', 'Bespoke Audio + rear-seat entertainment'),
    ECU($53, 'air_susp',      'Magic Carpet Ride — air suspension + camera-aided'),
    ECU($55, 'rear_steer',    'Rear-wheel steering (Spectre / Ghost EWB)'),
    ECU($7E5, 'evcc',         'Spectre — AC + DC charge controller (195 kW)'),
    ECU($7E6, 'ev_motor',     'Spectre — dual e-motor (430 kW)'),
    ECU($7E7, 'ev_battery',   'Spectre — 102 kWh HV battery (Gen5 BMW eDrive)'),
    ECU($35, 'starlight',     'Starlight Headliner / Doors fibre-optic controller')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Rolls-Royce service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F100, 'i_stufe_werks',             'Factory I-Stufe (build version)'),
    DID($F101, 'i_stufe_aktuell',           'Current I-Stufe (last programmed)'),
    DID($F1A0, 'rr_model_code',             'Rolls-Royce model (RR1/RR21/RR31/RR23)'),
    DID($F1A2, 'fa_assembly',               'Vehicle order (FA / SALAPA)'),
    DID($F1A4, 'rr_bespoke_code',           'Bespoke programme commission number'),
    DID($F1A6, 'rr_starlight_pattern',      'Starlight Headliner constellation pattern'),

    DID($D050, 'mileage_km',                'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       '12 V battery voltage (mV)'),
    DID($D052, 'engine_oil_temp',           'Engine oil temperature (C, V12 / V8)'),
    DID($D053, 'engine_oil_level_mm',       'Engine oil level (mm above min)'),
    DID($D054, 'coolant_temperature',       'Coolant temperature (C)'),
    DID($D055, 'engine_runtime_hours',      'Engine total runtime (h)'),
    DID($D056, 'turbo_boost_pressure',      'Turbo boost pressure (mbar)'),
    DID($D057, 'fuel_level_pct',            'Fuel level (%)'),

    DID($D060, 'magic_carpet_active',       'Magic Carpet Ride active (bool, camera + GPS)'),
    DID($D061, 'air_height_fl',             'Air height — front-left (mm)'),
    DID($D062, 'air_height_fr',             'Air height — front-right (mm)'),
    DID($D063, 'air_height_rl',             'Air height — rear-left (mm)'),
    DID($D064, 'air_height_rr',             'Air height — rear-right (mm)'),

    DID($D070, 'rear_steer_angle_deg',      'Rear-wheel steering angle (deg)'),
    DID($D071, 'spirit_software_version',   'Spirit OS / iDrive 8.5 SW version'),

    DID($D080, 'spectre_pack_voltage',      'Spectre HV pack voltage (V)'),
    DID($D081, 'spectre_pack_soc',          'Spectre HV pack SOC (%)'),
    DID($D082, 'spectre_pack_soh',          'Spectre HV pack SOH (%)'),
    DID($D083, 'spectre_motor_front_temp',  'Spectre front motor temp (C)'),
    DID($D084, 'spectre_motor_rear_temp',   'Spectre rear motor temp (C)'),
    DID($D085, 'spectre_charge_status',     'Spectre charge status enum'),
    DID($D086, 'spectre_range_km',          'Spectre remaining range (km, WLTP)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'rr_sas_calibration',    'Steering-angle sensor reset'),
    Routine($0F01, 'rr_cas_relearn',        'CAS proximity / key relearn (Rolls-Royce)'),
    Routine($0F02, 'rr_oil_service_reset',  'Oil-service reset'),
    Routine($0F03, 'rr_air_susp_calib',     'Magic Carpet air-suspension calibration'),
    Routine($0F04, 'rr_rear_steer_calib',   'Rear-wheel steering calibration'),
    Routine($0F05, 'rr_battery_register',   '12 V battery registration (BMS)'),
    Routine($0F06, 'rr_starlight_program',  'Bespoke Starlight constellation programming'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('rolls-royce.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionRollsRoyce.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionRollsRoyce.DtcCatalogFileName: string;
begin Result := 'dtc-rolls-royce.json'; end;

function TOBDOEMExtensionRollsRoyce.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4, $F1A6:
      if Length(Payload) > 0 then
      begin
        Result := Format('rr_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionRollsRoyce.Create);

end.
