//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Smart.pas
// CONTENTS       : smart Automobile Co. (Mercedes-Geely JV) OEM
//                  extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : smart was originally a Mercedes-Benz Group brand
//                  (1998 — Hambach France, original two-seater).
//                  Since 2020 it's a 50/50 joint venture between
//                  Mercedes-Benz Group + Geely Holding, with
//                  vehicles built in China (Xi'an) on Geely's SEA
//                  platform. The current line-up (#1 / #3 / #5 SUV)
//                  is EV-only. Legacy Hambach two-seater models
//                  use the older Mercedes XENTRY toolchain;
//                  Geely-era models use the smart Workshop System.
//------------------------------------------------------------------------------
unit OBD.OEM.Smart;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionSmart = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
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

function TOBDOEMExtensionSmart.ManufacturerKey: string;
begin Result := 'SMART'; end;

function TOBDOEMExtensionSmart.DisplayName: string;
begin Result := 'smart Automobile Co. (Mercedes-Geely JV)'; end;

function TOBDOEMExtensionSmart.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Legacy smart Hambach (Mercedes-era 2-seater): WME.
  // Current smart China (Geely-era #1 / #3 / #5 SUV, Xi'an): L7M.
  Result := (WMI = 'WME') or (WMI = 'L7M');
end;

procedure TOBDOEMExtensionSmart.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // Geely SEA platform is shared with Polestar 4 / Lotus EV /
  // Zeekr 001; ECU map mirrors that lineage with smart-specific
  // additions.
  ECUs := [
    ECU($7E0, 'vcu',          'VCU — Vehicle Control Unit (Geely SEA)'),
    ECU($710, 'motor_front',  'Front Motor Inverter (#1 / #3 / #5 AWD only)'),
    ECU($712, 'motor_rear',   'Rear Motor Inverter (single-motor RWD + AWD)'),
    ECU($782, 'bms',          'BMS — HV Battery (66 kWh / 100 kWh)'),
    ECU($785, 'evcc',         'On-board Charger + Charge Port'),
    ECU($720, 'cluster',      'Driver display cluster'),
    ECU($740, 'bcm',          'BCM — Body Control'),
    ECU($742, 'hud',          'HUD — Head-Up Display (#5 Premium)'),
    ECU($760, 'srs',          'SRS / Airbag'),
    ECU($762, 'abs',          'ABS / ESP'),
    ECU($768, 'climate',      'Climate Control + Heat Pump'),
    ECU($770, 'ivi',          'smart UI — central touchscreen'),
    ECU($724, 'pilot_assist', 'Pilot Assist (Mobileye-based ADAS)'),
    ECU($7B0, 'air_susp',     'Active air-suspension (#5 only)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'smart_model_code',          'smart model (#1 / #3 / #5 / legacy 451 / 453)'),
    DID($F1A2, 'smart_drivetrain',          'Drivetrain (RWD / AWD / Brabus)'),
    DID($F1A4, 'smart_battery_pack',        'Battery pack (66 kWh BYD-LFP / 100 kWh CATL-NMC)'),
    DID($F1A6, 'smart_software_release',    'smart OTA software release identifier'),

    DID($D050, 'vehicle_mileage',           'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       'Battery voltage (mV)'),
    DID($D053, 'ambient_temperature',       'Ambient temperature (°C)'),

    DID($D060, 'battery_pack_voltage',      'HV pack voltage (V)'),
    DID($D061, 'battery_pack_soc',          'HV pack SOC (%)'),
    DID($D062, 'battery_pack_soh',          'HV pack SOH (%)'),
    DID($D063, 'battery_pack_temp_min',     'Pack min cell temperature (°C)'),
    DID($D064, 'battery_pack_temp_max',     'Pack max cell temperature (°C)'),
    DID($D065, 'remaining_range_km',        'Remaining range (km, WLTP)'),
    DID($D066, 'energy_consumption_avg',    'Average energy consumption (kWh / 100 km)'),
    DID($D068, 'charge_status_enum',        'Charge port status enum'),
    DID($D069, 'charge_session_kwh',        'Energy delivered this charge session (kWh)'),

    DID($D070, 'motor_front_temperature',   'Front motor temperature (°C, AWD)'),
    DID($D071, 'motor_rear_temperature',    'Rear motor temperature (°C)'),
    DID($D072, 'motor_rear_torque_pct',     'Rear motor torque request (%)'),

    DID($D080, 'smart_brake_pad_front',     'Front brake-pad remaining (%)'),
    DID($D081, 'smart_brake_pad_rear',      'Rear brake-pad remaining (%)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F02, 'rdc_relearn',           'TPMS / RDC sensor relearn'),
    Routine($0F03, 'smart_battery_register','12 V battery registration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('smart.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionSmart.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Modern Geely-era smart shares SEA platform seed-key algorithms
  // with Polestar / Zeekr; legacy Hambach-era smart used the older
  // Mercedes XENTRY two's-complement at Level 1.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionSmart.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionSmart.DtcCatalogFileName: string;
begin Result := 'dtc-smart.json'; end;

function TOBDOEMExtensionSmart.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'smart_model_code';
                    $F1A2: 'smart_drivetrain';
                    $F1A4: 'smart_battery_pack';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionSmart.Create);

end.
