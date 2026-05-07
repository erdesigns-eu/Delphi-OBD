//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Tata.pas
// CONTENTS       : Tata Motors Ltd. OEM extension
//                  (Tata passenger + Tata commercial; JLR — also
//                   Tata-owned — has its own OBD.OEM.JLR extension)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Tata Motors covers passenger (Nexon / Punch /
//                  Curvv / Harrier / Safari + EV variants) and
//                  commercial (Ace / 407 / 1109 / Prima trucks +
//                  buses). Dealer toolchain is Tata Diagstar /
//                  X-Pert. JLR — also Tata-owned — uses its own
//                  Topix toolchain and has its own extension.
//------------------------------------------------------------------------------
unit OBD.OEM.Tata;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionTata = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionTata.ManufacturerKey: string;
begin Result := 'TATA'; end;

function TOBDOEMExtensionTata.DisplayName: string;
begin Result := 'Tata Motors Ltd.'; end;

function TOBDOEMExtensionTata.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Tata Motors passenger (Pune Pimpri-Chinchwad + Sanand): MAT.
  // Tata Motors commercial truck (Jamshedpur + Lucknow): MAR.
  // Tata Daewoo Commercial Vehicle (South Korea, since 2004): KMU.
  Result := (WMI = 'MAT') or (WMI = 'MAR') or (WMI = 'KMU');
end;

procedure TOBDOEMExtensionTata.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU (Revotron / Revotorq / Kryotec / TGDI)'),
    ECU($7E1, 'transmission',   'Transmission Control (5MT / 6MT / DCA)'),
    ECU($7E2, 'cng_module',     'CNG / Bi-fuel module (Tiago iCNG, Tigor iCNG)'),
    ECU($7E5, 'evcc',           'EV Charge Controller (Nexon EV / Tigor EV / Punch EV / Curvv EV)'),
    ECU($7E6, 'ev_motor',       'EV Drive Motor Inverter (Ziptron / Acti.ev architecture)'),
    ECU($720, 'cluster',        'Instrument Cluster (Tata digital MID)'),
    ECU($740, 'bcm',            'BCM — Body Control'),
    ECU($760, 'srs',            'SRS / Airbag (six-airbag standard from MY2024)'),
    ECU($762, 'abs',            'ABS / ESP / EBA'),
    ECU($768, 'climate',        'Climate Control'),
    ECU($770, 'ivi',            'iRA Connected Car / Harman IVI'),
    ECU($724, 'adas',           'ADAS Level 2 (Harrier facelift / Safari / Curvv)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Tata service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'tata_model_code',           'Tata model (Nexon, Punch, Curvv, Harrier, Safari)'),
    DID($F1A2, 'tata_variant_code',         'Tata variant (XE, XM, XT, XZ, XZ+)'),
    DID($F1A4, 'tata_engine_code',          'Tata engine code (Revotron 1.2T, Kryotec 2.0L)'),
    DID($F1B0, 'tata_options_block',        'Tata factory options block'),

    DID($D050, 'vehicle_mileage',           'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       'Battery voltage (mV)'),
    DID($D052, 'engine_oil_temperature',    'Engine oil temperature (°C)'),
    DID($D053, 'engine_coolant_temperature','Engine coolant temperature (°C)'),
    DID($D054, 'boost_pressure',            'Charge-air boost pressure (kPa, Revotron T-GDi)'),
    DID($D055, 'fuel_rail_pressure',        'Common-rail pressure (bar, Kryotec diesel)'),
    DID($D056, 'fuel_level_pct',            'Fuel level (%)'),
    DID($D057, 'cng_tank_pressure',         'CNG tank pressure (bar, iCNG)'),
    DID($D058, 'engine_runtime_h',          'Engine runtime lifetime (h)'),
    DID($D059, 'dpf_soot_load',             'DPF soot load (%, BS-VI Kryotec diesel)'),

    DID($D060, 'ziptron_pack_voltage',      'Ziptron HV pack voltage (V)'),
    DID($D061, 'ziptron_pack_soc',          'Ziptron HV pack SOC (%)'),
    DID($D062, 'ziptron_pack_soh',          'Ziptron HV pack SOH (%)'),
    DID($D063, 'ziptron_motor_temperature', 'Ziptron motor temperature (°C)'),
    DID($D068, 'ziptron_charge_status',     'Ziptron charge port status enum'),
    DID($D069, 'ziptron_remaining_range',   'Ziptron remaining range (km)'),

    DID($D070, 'tata_brake_pad_front_pct',  'Front brake-pad remaining (%)'),
    DID($D071, 'tata_brake_pad_rear_pct',   'Rear brake-pad remaining (%)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'idle_relearn',          'Idle re-learn (Tata Diagstar)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F01, 'tata_oil_life_reset',   'Reset engine oil-life monitor'),
    Routine($0F02, 'tata_dpf_force_regen',  'Forced DPF regeneration (Kryotec BS-VI)'),
    Routine($0F03, 'tata_battery_register', '12 V battery registration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('tata.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionTata.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionTata.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionTata.DtcCatalogFileName: string;
begin Result := 'dtc-tata.json'; end;

function TOBDOEMExtensionTata.DecodeDID(const DID: Word;
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
                    $F1A0: 'tata_model_code';
                    $F1A2: 'tata_variant_code';
                    $F1A4: 'tata_engine_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionTata.Create);

end.
