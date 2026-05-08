//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Bentley.pas
// CONTENTS       : Bentley Motors OEM extension (VAG luxury)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Bentley Motors Ltd. — Crewe, England. VAG group
//                  member; uses VAG diagnostic stack (ODIS / VAS)
//                  with Bentley-specific extensions. Catalog covers
//                  Continental GT / GTC + Flying Spur (MSB platform,
//                  shared with Porsche Panamera) + Bentayga (MLB
//                  Evo, shared with Audi Q7 / Porsche Cayenne /
//                  Lamborghini Urus).
//------------------------------------------------------------------------------
unit OBD.OEM.Bentley;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionBentley = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
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

function TOBDOEMExtensionBentley.ManufacturerKey: string;
begin Result := 'BENTLEY'; end;

function TOBDOEMExtensionBentley.DisplayName: string;
begin Result := 'Bentley Motors Ltd.'; end;

function TOBDOEMExtensionBentley.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // SCB — Bentley Crewe (sole assembly plant).
  Result := (WMI = 'SCB');
end;

procedure TOBDOEMExtensionBentley.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // VAG architecture — addresses follow KWP2000 / UDS-on-CAN
  // conventions used by Audi / Porsche.
  ECUs := [
    ECU($7E0, 'engine',          'ECM — 4.0L V8 TFSI / 6.0L W12 TSI / 4.0L V8 PHEV (Bentayga / Flying Spur Hybrid)'),
    ECU($7E1, 'transmission',    'TCM — ZF 8HP90 / DL501 7-DCT (Hybrid)'),
    ECU($7E2, 'awd_torsen',      'Torsen / electronic 4WD coupling'),
    ECU($7B0, 'abs_esp',         'ABS / ESP — Bosch ESP 9.3 / iBooster'),
    ECU($720, 'cluster',         'KOMBI — Bentley TFT cluster'),
    ECU($760, 'mmi',             'Bentley Rotating Display + MMI (MIB3 / MIB4)'),
    ECU($740, 'comfort',         'Comfort — central body controller'),
    ECU($770, 'kessy',           'KESSY — Bentley keyless entry'),
    ECU($780, 'tpms',            'TPMS — direct sensors'),
    ECU($784, 'air_susp_44way',  'Adaptive air suspension (44-way Mulliner massage seats data)'),
    ECU($785, 'dynamic_ride',    'Bentley Dynamic Ride — 48 V active anti-roll'),
    ECU($786, 'rear_steer',      'Rear-wheel steering (Flying Spur Mulliner)'),
    ECU($7E5, 'evcc',            'Hybrid — AC charge controller (7.2 kW)'),
    ECU($7E6, 'phev_inverter',   'Hybrid — e-motor inverter'),
    ECU($7E7, 'phev_battery',    'Hybrid — 14.1 kWh / 25.9 kWh PHEV battery'),
    ECU($790, 'rear_seat_ent',   'Rear-Seat Entertainment (Bentley Mulliner)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Bentley part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'bentley_model_code',        'Bentley model (Continental GT/GTC/FS/Bentayga)'),
    DID($F1A2, 'bentley_engine_code',       'Engine code (V8 TFSI / W12 / V8 Hybrid)'),
    DID($F1A4, 'bentley_paint_code',        'Bentley paint code (Mulliner palette)'),
    DID($F1A6, 'bentley_trim_code',         'Bentley interior trim code'),
    DID($F1A8, 'bentley_commission_no',     'Commission number (build slot)'),

    DID($D050, 'mileage_km',                'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       '12 V battery voltage (mV)'),
    DID($D052, 'engine_oil_temp',           'Engine oil temperature (C)'),
    DID($D053, 'engine_oil_pressure',       'Engine oil pressure (kPa)'),
    DID($D054, 'engine_oil_level_mm',       'Engine oil level (mm above min)'),
    DID($D055, 'coolant_temperature',       'Coolant temperature (C)'),
    DID($D056, 'engine_runtime_hours',      'Engine total runtime (h)'),
    DID($D057, 'turbo_boost_pressure',      'Turbo boost pressure (mbar, V8 / W12)'),
    DID($D058, 'fuel_level_pct',            'Fuel level (%)'),
    DID($D059, 'remaining_oil_distance',    'Distance to next oil service (km)'),

    DID($D070, 'drive_mode_enum',           'Drive mode (Bentley / Comfort / Sport / Custom)'),
    DID($D071, 'air_susp_mode',             'Air-suspension mode (Comfort / Bentley / Sport / Off-Road)'),
    DID($D072, 'dynamic_ride_active',       'Dynamic Ride 48 V anti-roll active (bool)'),
    DID($D073, 'rear_steer_angle_deg',      'Rear-wheel steering angle (deg, Flying Spur)'),

    DID($D080, 'air_height_fl',             'Air height — front-left (mm)'),
    DID($D081, 'air_height_fr',             'Air height — front-right (mm)'),
    DID($D082, 'air_height_rl',             'Air height — rear-left (mm)'),
    DID($D083, 'air_height_rr',             'Air height — rear-right (mm)'),

    DID($D090, 'phev_pack_voltage',         'PHEV pack voltage (V)'),
    DID($D091, 'phev_pack_soc',             'PHEV pack SOC (%)'),
    DID($D092, 'phev_motor_temp',           'PHEV e-motor temperature (C)'),
    DID($D093, 'phev_charge_status',        'PHEV charge status enum')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',          'Reset adaptive learning'),
    Routine($0F00, 'bentley_sas_calibration',    'Steering-angle sensor reset'),
    Routine($0F01, 'bentley_oil_service_reset',  'Oil-service reset'),
    Routine($0F02, 'bentley_air_susp_calib',     'Air-suspension calibration'),
    Routine($0F03, 'bentley_dynamic_ride_calib', 'Bentley Dynamic Ride 48 V calibration'),
    Routine($0F04, 'bentley_rear_steer_calib',   'Rear-wheel steering calibration'),
    Routine($0F05, 'bentley_battery_register',   '12 V battery registration'),
    Routine($0F06, 'bentley_dpf_force_regen',    'Forced DPF regeneration (W12 / V8 TFSI)'),
    Routine($FF00, 'erase_memory',               'Pre-flash erase')
  ];

  MergeCatalogJSON('bentley.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionBentley.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionBentley.DtcCatalogFileName: string;
begin Result := 'dtc-bentley.json'; end;

function TOBDOEMExtensionBentley.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4, $F1A6, $F1A8:
      if Length(Payload) > 0 then
      begin
        Result := Format('bentley_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionBentley.Create);

end.
