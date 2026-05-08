//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.McLaren.pas
// CONTENTS       : McLaren Automotive OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : McLaren Automotive Ltd. — McLaren Production Centre
//                  (MPC), Woking, England. Mid-engine carbon-tub
//                  construction (Monocell / MonoCell II / II-J /
//                  III) shared across the line. Engines are the
//                  Ricardo-built M838T / M840T / M840TE 4.0 V8
//                  twin-turbo (720S / 750S / 765LT / GT) and the new
//                  M630 3.0 V6 hybrid (Artura / 750S derivatives).
//                  Transmissions are the Graziano 7-DCT.
//------------------------------------------------------------------------------
unit OBD.OEM.McLaren;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionMcLaren = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionMcLaren.ManufacturerKey: string;
begin Result := 'MCLAREN'; end;

function TOBDOEMExtensionMcLaren.DisplayName: string;
begin Result := 'McLaren Automotive Ltd.'; end;

function TOBDOEMExtensionMcLaren.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // SBM — McLaren Production Centre, Woking.
  Result := (WMI = 'SBM');
end;

procedure TOBDOEMExtensionMcLaren.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'PCM — M840T 4.0 V8 / M630 3.0 V6 hybrid'),
    ECU($7E1, 'tcm',            'TCM — Graziano 7-DCT (SSG)'),
    ECU($7B0, 'abs_esp',        'ABS / ESP — Bosch ESP 9'),
    ECU($7B1, 'pccm',           'PCCM — Proactive Chassis Control (linked-hydraulic)'),
    ECU($720, 'cluster',        'IPK — McLaren TFT cluster (Folding Driver Display on 750S/Artura)'),
    ECU($760, 'iris',           'IRIS / IRIS II infotainment (vertical 8" touchscreen)'),
    ECU($7C0, 'mcl_cdc',        'CDC — Cylinder-Deactivation Controller'),
    ECU($740, 'bcm',            'BCM — body controller'),
    ECU($770, 'srs',            'SRS — airbag controller'),
    ECU($780, 'tpms',            'TPMS — direct sensors (Schrader)'),
    ECU($784, 'lift_axle',      'McLaren Vehicle-Lift system (front-axle)'),
    ECU($785, 'aero_active',    'Active rear wing / airbrake controller (P1 / 720S / 750S)'),
    ECU($786, 'helical_diff',   'Electronic helical LSD'),
    ECU($7E5, 'evcc',           'Artura — AC charge controller (Type 2)'),
    ECU($7E6, 'phev_inverter',  'Artura — front-axle e-motor inverter (70 kW)'),
    ECU($7E7, 'phev_battery',   'Artura — 7.4 kWh PHEV battery'),
    ECU($790, 'pdk',            'Practical Data Capture (track telemetry / V-Box)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'McLaren part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'mcl_model_code',            'McLaren model (570S/600LT/720S/750S/765LT/Artura/GT)'),
    DID($F1A2, 'mcl_engine_code',           'Engine code (M838T / M840T / M840TE / M630 hybrid)'),
    DID($F1A4, 'mcl_paint_code',            'McLaren Special Operations paint code'),
    DID($F1A6, 'mcl_chassis_no',            'Chassis carbon-tub serial (MonoCell II / II-J / III)'),

    DID($D050, 'mileage_km',                'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       '12 V battery voltage (mV)'),
    DID($D052, 'engine_oil_temp',           'Engine oil temperature (C)'),
    DID($D053, 'engine_oil_pressure',       'Engine oil pressure (kPa)'),
    DID($D054, 'engine_oil_level_mm',       'Engine oil level (mm above min)'),
    DID($D055, 'coolant_temperature',       'Coolant temperature (C, dual-circuit)'),
    DID($D056, 'engine_runtime_hours',      'Engine total runtime (h)'),
    DID($D057, 'turbo_boost_pressure',      'Turbo boost pressure (mbar)'),
    DID($D058, 'turbo_temp_left',           'Left-bank turbo temperature (C)'),
    DID($D059, 'turbo_temp_right',          'Right-bank turbo temperature (C)'),
    DID($D05A, 'fuel_level_pct',            'Fuel level (%)'),

    DID($D060, 'pccm_handling_mode',        'Handling mode (Comfort / Sport / Track)'),
    DID($D061, 'pccm_powertrain_mode',      'Powertrain mode (Comfort / Sport / Track)'),
    DID($D062, 'pccm_aero_position',        'Active rear wing position enum'),
    DID($D063, 'pccm_lift_status',          'Vehicle-Lift status (Off / Up / Lowering)'),

    DID($D070, 'mcl_dct_clutch_temp_a',     'DCT clutch A temperature (C)'),
    DID($D071, 'mcl_dct_clutch_temp_b',     'DCT clutch B temperature (C)'),
    DID($D072, 'mcl_brake_pad_remaining',   'Brake pad remaining (% — CCM-R / CCM-J)'),

    DID($D080, 'phev_pack_voltage',         'Artura PHEV pack voltage (V)'),
    DID($D081, 'phev_pack_soc',             'Artura PHEV pack SOC (%)'),
    DID($D082, 'phev_pack_soh',             'Artura PHEV pack SOH (%)'),
    DID($D083, 'phev_motor_temp',           'Artura front e-motor temperature (C)'),
    DID($D084, 'phev_charge_status',        'Artura charge status enum')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',         'Reset adaptive learning'),
    Routine($0F00, 'mcl_sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F01, 'mcl_oil_life_reset',        'Oil-life reset (McLaren service indicator)'),
    Routine($0F02, 'mcl_dct_clutch_calibration','7-DCT (SSG) clutch calibration'),
    Routine($0F03, 'mcl_lift_axle_test',        'Front-axle lift system test'),
    Routine($0F04, 'mcl_aero_calibration',      'Active rear-wing / airbrake calibration'),
    Routine($0F05, 'mcl_brake_bleed',           'Electronic brake-bleed cycle'),
    Routine($0F06, 'mcl_battery_register',      '12 V battery registration'),
    Routine($FF00, 'erase_memory',              'Pre-flash erase')
  ];

  MergeCatalogJSON('mclaren.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionMcLaren.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMcLaren.DtcCatalogFileName: string;
begin Result := 'dtc-mclaren.json'; end;

function TOBDOEMExtensionMcLaren.DecodeDID(const DID: Word;
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
        Result := Format('mcl_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMcLaren.Create);

end.
