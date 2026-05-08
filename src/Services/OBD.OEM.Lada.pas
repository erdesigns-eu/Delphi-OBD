//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Lada.pas
// CONTENTS       : AvtoVAZ / Lada OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : AO AvtoVAZ — Tolyatti (XTA) + Izhevsk (XTC) +
//                  Bronto special-vehicles. Lada is the volume
//                  passenger brand. Engines are the VAZ-2126x
//                  (Granta / Vesta) and VAZ-2123x (Niva / Niva
//                  Travel) families with Bosch ME17.9.7 / EDC17 ECMs.
//                  Catalog covers Granta + Vesta + Niva (Legend +
//                  Travel) + Largus (Logan-derived).
//------------------------------------------------------------------------------
unit OBD.OEM.Lada;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionLada = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionLada.ManufacturerKey: string;
begin Result := 'LADA'; end;

function TOBDOEMExtensionLada.DisplayName: string;
begin Result := 'AvtoVAZ (Lada)'; end;

function TOBDOEMExtensionLada.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // XTA — Tolyatti (volume passenger).
  // XTC — Izhevsk (Vesta SW Cross / Largus).
  // Bronto: XTV (Niva Bronto special vehicles).
  Result := (WMI = 'XTA') or (WMI = 'XTC') or (WMI = 'XTV');
end;

procedure TOBDOEMExtensionLada.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'ECM — VAZ-21127 (1.6L 16V) / VAZ-21179 (1.8L) / VAZ-21214 (Niva 1.7L)'),
    ECU($7E1, 'tcm',            'TCM — JATCO JF015E CVT / Aisin AT (Vesta)'),
    ECU($7B0, 'abs_esp',        'ABS / ESP — Bosch ABS 9 / Mando ABS'),
    ECU($720, 'cluster',        'KOMBI — Lada electronic instrument cluster'),
    ECU($740, 'bcm',            'BCM — central body controller'),
    ECU($760, 'ivi',            'EnjoY infotainment'),
    ECU($770, 'srs',            'SRS — airbag (driver + passenger)'),
    ECU($780, 'tpms',           'TPMS — Vesta / Niva Travel'),
    ECU($784, 'pcvk',           'PVCK — power-window / central locking module'),
    ECU($785, 'climate',        'Climate / heater controller'),
    ECU($790, 'eps',            'EPS — electric power steering'),
    ECU($7C0, 'immo',           'APS-6 / APS-9 immobilizer'),
    ECU($7C1, 'transfer_case',  'Transfer case (Niva 4x4)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'AvtoVAZ part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'lada_model_code',           'Model (Granta/Vesta/Niva Legend/Niva Travel/Largus)'),
    DID($F1A2, 'lada_engine_code',          'Engine code (21127 / 21179 / 21214)'),
    DID($F1A4, 'lada_trim_code',            'Trim level (Standard/Classic/Comfort/Luxe/Cross)'),
    DID($F1A6, 'lada_transmission_code',    'Transmission (5MT / 5AMT / JF015E CVT / 4AT)'),
    DID($F1A8, 'lada_immo_state',           'APS immobilizer state enum'),

    DID($D050, 'mileage_km',                'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       '12 V battery voltage (mV)'),
    DID($D052, 'engine_oil_temp',           'Engine oil temperature (C)'),
    DID($D053, 'coolant_temperature',       'Coolant temperature (C)'),
    DID($D054, 'engine_runtime_hours',      'Engine total runtime (h)'),
    DID($D055, 'fuel_level_pct',            'Fuel level (%)'),
    DID($D056, 'fuel_consumption_recent',   'Recent fuel consumption (L/100 km)'),
    DID($D057, 'intake_air_temp',           'Intake-air temperature (C)'),
    DID($D058, 'intake_mass_flow',          'MAF (g/s)'),
    DID($D059, 'throttle_position_pct',     'Throttle position (%)'),
    DID($D05A, 'engine_load_pct',           'Engine load (%)'),
    DID($D05B, 'manifold_pressure_kpa',     'Manifold absolute pressure (kPa)'),

    DID($D060, 'eps_torque',                'EPS torque request (Nm)'),
    DID($D061, 'eps_motor_current',         'EPS motor current (A)'),
    DID($D062, 'cabin_temperature',         'Cabin temperature (C)'),
    DID($D063, 'ambient_temperature',       'Ambient temperature (C)'),

    DID($D070, 'transfer_case_mode',        'Niva transfer-case mode (2H / 4H / 4L / N)'),
    DID($D071, 'tps_pedal_pct',             'Accelerator pedal position (%)'),
    DID($D072, 'cvt_oil_temperature',       'CVT oil temperature (C, Vesta CVT)'),
    DID($D073, 'cvt_ratio',                 'CVT ratio'),
    DID($D074, 'amt_clutch_position',       'AMT clutch position (%, 5AMT)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'lada_throttle_relearn', 'Throttle position relearn'),
    Routine($0F01, 'lada_idle_relearn',     'Idle air relearn'),
    Routine($0F02, 'lada_immo_relearn',     'APS immobilizer key learning'),
    Routine($0F03, 'lada_oil_service_reset','Oil-service reset (Vesta / Niva Travel cluster)'),
    Routine($0F04, 'lada_amt_calibration',  '5AMT clutch calibration (Granta / Vesta)'),
    Routine($0F05, 'lada_cvt_relearn',      'JF015E CVT relearn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('lada.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionLada.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionLada.DtcCatalogFileName: string;
begin Result := 'dtc-lada.json'; end;

function TOBDOEMExtensionLada.DecodeDID(const DID: Word;
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
        Result := Format('lada_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionLada.Create);

end.
