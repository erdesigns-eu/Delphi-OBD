//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.AstonMartin.pas
// CONTENTS       : Aston Martin Lagonda OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Aston Martin Lagonda Ltd. — Gaydon (sports cars,
//                  Vantage / DB12 / Vanquish) + St Athan (DBX SUV) +
//                  Gaydon FAB-1 (Valhalla mid-engine PHEV). The AMR
//                  diagnostic stack uses Bosch MED + Continental
//                  Mercedes-AMG-derived ECUs (M177 V8 twin-turbo
//                  shared with AMG; V12 N/A is the in-house unit).
//                  Catalog covers DB11 through Vanquish + DBX +
//                  Valhalla.
//------------------------------------------------------------------------------
unit OBD.OEM.AstonMartin;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionAstonMartin = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionAstonMartin.ManufacturerKey: string;
begin Result := 'ASTON_MARTIN'; end;

function TOBDOEMExtensionAstonMartin.DisplayName: string;
begin Result := 'Aston Martin Lagonda Ltd.'; end;

function TOBDOEMExtensionAstonMartin.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // SCF — Aston Martin Gaydon (sports cars + DBX St Athan).
  Result := (WMI = 'SCF');
end;

procedure TOBDOEMExtensionAstonMartin.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',          'PCM — M177 4.0L V8 / N/A 5.2L V12 / Valhalla V8 PHEV'),
    ECU($7E1, 'tcm',             'TCM — ZF 8HP / 7-speed Graziano / 8-DCT (Valhalla)'),
    ECU($7E2, 'awd',             'AWD coupling (DBX 707)'),
    ECU($7B0, 'abs_esp',         'ABS / ESP — Bosch ESP 9.3 / iBooster (DBX)'),
    ECU($7E3, 'bcm',             'BCM — body controller'),
    ECU($720, 'cluster',         'IPK — Aston Martin TFT cluster'),
    ECU($760, 'ivi',             'AMi infotainment (Mercedes NTG-derived)'),
    ECU($740, 'kessy',           'PASE / keyless entry'),
    ECU($770, 'srs',             'SRS — airbag controller (six-airbag)'),
    ECU($780, 'tpms',            'TPMS — direct sensors (Schrader / Continental)'),
    ECU($784, 'adaptive_dampers','Bilstein DTX / DCT adaptive damping'),
    ECU($785, 'air_susp',        'DBX / DBX 707 — triple-chamber air suspension'),
    ECU($786, 'electronic_diff', 'eDiff — DBX 707 / Vantage'),
    ECU($7E5, 'evcc',            'Valhalla — PHEV charge controller (3.6 kW AC)'),
    ECU($7E6, 'pheV_inverter',   'Valhalla — front-axle e-motor inverter'),
    ECU($7E7, 'pheV_battery',    'Valhalla — 6.6 kWh PHEV battery')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Aston Martin part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'aml_model_code',            'AML model (DB11/DB12/Vantage/DBX/Vanquish/Valhalla)'),
    DID($F1A2, 'aml_engine_code',           'Engine code (M177 V8 / 5.2 V12 / Valhalla V8 PHEV)'),
    DID($F1A4, 'aml_paint_code',            'Q by Aston Martin paint code'),
    DID($F1A6, 'aml_trim_code',             'Q by Aston Martin trim code'),
    DID($F1A8, 'aml_assembly_plant',        'Assembly plant (Gaydon / St Athan)'),

    DID($D050, 'mileage_km',                'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       '12 V battery voltage (mV)'),
    DID($D052, 'engine_oil_temp',           'Engine oil temperature (C)'),
    DID($D053, 'engine_oil_pressure',       'Engine oil pressure (kPa)'),
    DID($D054, 'engine_oil_level_mm',       'Engine oil level (mm above min)'),
    DID($D055, 'coolant_temperature',       'Coolant temperature (C)'),
    DID($D056, 'engine_runtime_hours',      'Engine total runtime (h)'),
    DID($D057, 'turbo_boost_pressure',      'Turbo boost pressure (mbar, V8 only)'),
    DID($D058, 'fuel_level_pct',            'Fuel level (%)'),

    DID($D070, 'damper_mode_enum',          'Adaptive damper mode (Sport / Sport+ / Track)'),
    DID($D071, 'drive_mode_enum',           'Drive mode (GT / Sport / Sport+ / Track / Individual)'),
    DID($D072, 'ediff_lock_pct',            'eDiff lock percentage'),

    DID($D080, 'air_height_fl',             'DBX air-suspension height — front-left (mm)'),
    DID($D081, 'air_height_fr',             'DBX air-suspension height — front-right (mm)'),
    DID($D082, 'air_height_rl',             'DBX air-suspension height — rear-left (mm)'),
    DID($D083, 'air_height_rr',             'DBX air-suspension height — rear-right (mm)'),
    DID($D084, 'air_susp_mode',             'DBX air-suspension mode (Access / Standard / Off-Road / Sport)'),

    DID($D090, 'phev_pack_voltage',         'Valhalla PHEV pack voltage (V)'),
    DID($D091, 'phev_pack_soc',             'Valhalla PHEV pack SOC (%)'),
    DID($D092, 'phev_motor_temp',           'Valhalla front e-motor temperature (C)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',       'Reset adaptive learning'),
    Routine($0F00, 'aml_sas_calibration',     'Steering-angle sensor reset'),
    Routine($0F01, 'aml_oil_life_reset',      'Oil-life reset (Aston Martin service indicator)'),
    Routine($0F02, 'aml_air_susp_calibration','DBX air-suspension calibration'),
    Routine($0F03, 'aml_brake_bleed',         'Electronic brake-bleed cycle'),
    Routine($0F04, 'aml_battery_register',    '12 V battery registration'),
    Routine($FF00, 'erase_memory',            'Pre-flash erase')
  ];

  MergeCatalogJSON('aston-martin.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionAstonMartin.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionAstonMartin.DtcCatalogFileName: string;
begin Result := 'dtc-aston-martin.json'; end;

function TOBDOEMExtensionAstonMartin.DecodeDID(const DID: Word;
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
        Result := Format('aml_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionAstonMartin.Create);

end.
