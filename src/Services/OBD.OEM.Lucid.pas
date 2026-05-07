//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Lucid.pas
// CONTENTS       : Lucid Group OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Lucid Group (Casa Grande, Arizona — AMP-1 plant)
//                  builds the Air sedan and Gravity SUV. The
//                  catalog targets the dual- / tri-motor Air and
//                  the upcoming Gravity. Lucid runs proprietary
//                  cloud-connected diagnostics; the catalog
//                  surfaces what's reachable over standard
//                  ISO 14229 UDS-on-CAN.
//------------------------------------------------------------------------------
unit OBD.OEM.Lucid;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionLucid = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionLucid.ManufacturerKey: string;
begin Result := 'LUCID'; end;

function TOBDOEMExtensionLucid.DisplayName: string;
begin Result := 'Lucid Group, Inc.'; end;

function TOBDOEMExtensionLucid.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Lucid Casa Grande AMP-1: 50A.
  Result := (WMI = '50A');
end;

procedure TOBDOEMExtensionLucid.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // Lucid Air's compact 900V 'Wunderbox' integrates inverter +
  // charger + DC-DC. Tri-motor variants split front + rear-left
  // + rear-right drive units; dual-motor uses front + rear.
  ECUs := [
    ECU($7E0, 'vcu',           'VCU — Vehicle Control Unit'),
    ECU($710, 'motor_front',   'Front Motor Inverter'),
    ECU($712, 'motor_rear_l',  'Rear-Left Motor Inverter (Sapphire / tri-motor)'),
    ECU($713, 'motor_rear_r',  'Rear-Right Motor Inverter (Sapphire / tri-motor)'),
    ECU($782, 'bms',           'BMS — 900 V pack (Touring / Grand Touring / Sapphire)'),
    ECU($792, 'wunderbox',     'Wunderbox — integrated charger + DC-DC + inverter'),
    ECU($720, 'cluster',       'Lucid driver display (Pixel cluster)'),
    ECU($724, 'dreamdrive',    'DreamDrive ADAS computer (32-sensor stack)'),
    ECU($726, 'lidar',         'Lidar (DreamDrive Pro)'),
    ECU($740, 'bcm',           'BCM — Body Control'),
    ECU($742, 'glass_canopy',  'Glass Canopy + sun-shade controller'),
    ECU($762, 'climate',       'Climate / heat-pump (CO₂)'),
    ECU($770, 'ivi',           'Lucid UX touchscreen + Pilot Panel'),
    ECU($7A0, 'thermal_mgmt',  'Thermal management — pack + cabin'),
    ECU($7B0, 'air_susp',      'Active air-suspension controller')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'lucid_model_code',          'Lucid model (Air / Gravity / Sapphire)'),
    DID($F1A2, 'lucid_drivetrain',          'Drivetrain (Pure / Touring / Grand Touring / Sapphire)'),
    DID($F1A4, 'lucid_battery_pack',        'Battery pack (88/92/112/118 kWh) ID'),
    DID($F1A6, 'lucid_software_release',    'Lucid OTA software release identifier'),

    DID($D050, 'vehicle_mileage',           'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       'Battery voltage (mV)'),

    DID($D060, 'battery_pack_voltage',      'HV pack voltage (V, 900V architecture)'),
    DID($D061, 'battery_pack_soc',          'HV pack SOC (%)'),
    DID($D062, 'battery_pack_soh',          'HV pack SOH (%)'),
    DID($D063, 'battery_pack_temp_min',     'Pack min cell temperature (°C)'),
    DID($D064, 'battery_pack_temp_max',     'Pack max cell temperature (°C)'),
    DID($D065, 'remaining_range_km',        'Remaining range (km, EPA-rated)'),
    DID($D066, 'consumption_recent_kwh',    'Recent average consumption (kWh / 100 km)'),
    DID($D068, 'charge_status_enum',        'Charge port status enum'),
    DID($D069, 'charge_session_kwh',        'Energy delivered this charge session (kWh)'),
    DID($D06A, 'charge_power_kw',           'Current charge power (kW, up to 350 kW DC)'),

    DID($D080, 'motor_front_temperature',   'Front motor temperature (°C)'),
    DID($D081, 'motor_rear_l_temperature',  'Rear-Left motor temperature (Sapphire)'),
    DID($D082, 'motor_rear_r_temperature',  'Rear-Right motor temperature (Sapphire)'),
    DID($D083, 'motor_front_torque_pct',    'Front motor torque request (%)'),
    DID($D084, 'motor_rear_torque_pct',     'Rear motor torque request (%)'),

    DID($D0A0, 'lucid_air_height_fl',       'Air-suspension height — front-left (mm)'),
    DID($D0A1, 'lucid_air_height_fr',       'Air-suspension height — front-right (mm)'),
    DID($D0A2, 'lucid_air_height_rl',       'Air-suspension height — rear-left (mm)'),
    DID($D0A3, 'lucid_air_height_rr',       'Air-suspension height — rear-right (mm)'),
    DID($D0B0, 'lucid_drive_mode',          'Selected drive mode enum')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'lucid_sas_calibration', 'Steering-angle sensor reset'),
    Routine($0F02, 'lucid_air_susp_calib',  'Air-suspension ride-height calibration'),
    Routine($0F03, 'lucid_brake_bleed',     'Brake bleed cycle'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('lucid.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionLucid.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionLucid.DtcCatalogFileName: string;
begin Result := 'dtc-lucid.json'; end;

function TOBDOEMExtensionLucid.DecodeDID(const DID: Word;
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
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'lucid_model_code';
                    $F1A2: 'lucid_drivetrain';
                    $F1A4: 'lucid_battery_pack';
                    $F1A6: 'lucid_software_release';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionLucid.Create);

end.
