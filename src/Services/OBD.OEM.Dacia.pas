//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Dacia.pas
// CONTENTS       : Dacia (Renault Group budget brand) OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Automobile Dacia SA — Mioveni, Romania (UU1).
//                  Renault Group brand — uses Renault CMF-B / CMF-A
//                  platforms + Renault diagnostic stack (CLIP).
//                  Spring EV is built in Dongfeng-Renault JV
//                  (Wuhan, China — LBR / LRY). Catalog covers
//                  Sandero / Logan / Duster / Jogger / Bigster +
//                  Spring EV.
//                  Renault, Renault Trucks, Alpine and Mobilize use
//                  separate extensions; Dacia is intentionally
//                  scoped to Mioveni + Spring China.
//------------------------------------------------------------------------------
unit OBD.OEM.Dacia;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionDacia = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionDacia.ManufacturerKey: string;
begin Result := 'DACIA'; end;

function TOBDOEMExtensionDacia.DisplayName: string;
begin Result := 'Automobile Dacia SA (Renault Group)'; end;

function TOBDOEMExtensionDacia.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // UU1 — Mioveni, Romania (passenger volume).
  // UU3 — Mioveni commercial / Lodgy / Dokker.
  // LBR / LRY — Dongfeng-Renault Wuhan (Spring EV / K-ZE export).
  Result := (WMI = 'UU1') or (WMI = 'UU3') or (WMI = 'LBR') or (WMI = 'LRY');
end;

procedure TOBDOEMExtensionDacia.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'ECM — H4D TCe / H4Bt LPG / H5Ht hybrid / 5AQ Spring EV'),
    ECU($7E1, 'tcm',            'TCM — Jatco JF016E CVT / Renault EDC 6-DCT / 1-speed reducer (Spring)'),
    ECU($7B0, 'abs_esp',        'ABS / ESP — Bosch / Continental'),
    ECU($720, 'cluster',        'KOMBI — Dacia instrument cluster (TFT on Bigster)'),
    ECU($740, 'bcm',            'UCH — Unite Centrale Habitacle (body controller)'),
    ECU($760, 'ivi',            'Media Display / Media Nav / Media Control (8" / 10")'),
    ECU($770, 'srs',            'SRS — Renault airbag controller'),
    ECU($780, 'tpms',           'TPMS — direct sensors (Bigster) / indirect (Sandero)'),
    ECU($784, 'eps',            'EPS — electric power steering'),
    ECU($785, 'climate',        'A/C / climate'),
    ECU($790, 'gateway',        'Renault diagnostic gateway'),
    ECU($7C0, 'lpg_module',     'LPG bi-fuel module (ECO-G / Bi-Fuel)'),
    ECU($7C1, 'awd_coupling',   '4WD coupling (Duster 4x4)'),
    ECU($7E5, 'evcc',           'Spring EV — AC charge controller (7 kW)'),
    ECU($7E6, 'ev_motor',       'Spring EV / Bigster Hybrid — e-motor inverter'),
    ECU($7E7, 'ev_battery',     'Spring EV — 26.8 kWh / Bigster Hybrid HV pack')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Renault Group part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'dacia_model_code',          'Model (Sandero/Logan/Duster/Jogger/Bigster/Spring)'),
    DID($F1A2, 'dacia_engine_code',         'Engine code (TCe 90 / TCe 130 / ECO-G LPG / Hybrid 140 / 5AQ)'),
    DID($F1A4, 'dacia_trim_code',           'Trim (Essential/Expression/Extreme/Journey)'),
    DID($F1A6, 'dacia_assembly_plant',      'Assembly plant (Mioveni / Wuhan / Tangier)'),

    DID($D050, 'mileage_km',                'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       '12 V battery voltage (mV)'),
    DID($D052, 'engine_oil_temp',           'Engine oil temperature (C)'),
    DID($D053, 'coolant_temperature',       'Coolant temperature (C)'),
    DID($D054, 'engine_runtime_hours',      'Engine total runtime (h)'),
    DID($D055, 'fuel_level_pct',            'Fuel level (%)'),
    DID($D056, 'lpg_level_pct',             'LPG tank level (%, ECO-G only)'),
    DID($D057, 'lpg_active',                'LPG active (bool)'),
    DID($D058, 'turbo_boost_pressure',      'Turbo boost pressure (mbar)'),
    DID($D059, 'fuel_consumption_recent',   'Recent fuel consumption (L/100 km)'),
    DID($D05A, 'remaining_oil_distance',    'Distance to next oil service (km)'),

    DID($D060, 'eps_torque',                'EPS torque request (Nm)'),
    DID($D061, 'eps_motor_current',         'EPS motor current (A)'),
    DID($D062, 'cabin_temperature',         'Cabin temperature (C)'),
    DID($D063, 'ambient_temperature',       'Ambient temperature (C)'),

    DID($D070, 'awd_mode_enum',             'Duster 4x4 mode (Auto / 4x4 Lock)'),
    DID($D071, 'tcm_clutch_temp',           'EDC dual-clutch oil temperature (C, Hybrid)'),

    DID($D080, 'spring_pack_voltage',       'Spring EV pack voltage (V)'),
    DID($D081, 'spring_pack_soc',           'Spring EV pack SOC (%)'),
    DID($D082, 'spring_pack_soh',           'Spring EV pack SOH (%)'),
    DID($D083, 'spring_motor_temp',         'Spring EV motor temperature (C)'),
    DID($D084, 'spring_charge_status',      'Spring EV charge status enum'),
    DID($D085, 'spring_range_km',           'Spring EV remaining range (km, WLTP)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'dacia_throttle_relearn','Throttle position relearn'),
    Routine($0F01, 'dacia_idle_relearn',    'Idle air relearn'),
    Routine($0F02, 'dacia_oil_service_reset','Oil-service reset'),
    Routine($0F03, 'dacia_battery_register','12 V battery registration'),
    Routine($0F04, 'dacia_dpf_force_regen', 'Forced DPF regeneration (TCe diesel-equivalent / not on petrol)'),
    Routine($0F05, 'dacia_eps_calibration', 'EPS calibration after replacement'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('dacia.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionDacia.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionDacia.DtcCatalogFileName: string;
begin Result := 'dtc-dacia.json'; end;

function TOBDOEMExtensionDacia.DecodeDID(const DID: Word;
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
        Result := Format('dacia_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionDacia.Create);

end.
