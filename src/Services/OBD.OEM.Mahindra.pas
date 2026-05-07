//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Mahindra.pas
// CONTENTS       : Mahindra & Mahindra Ltd. OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Mahindra (Mumbai HQ) covers passenger SUV
//                  (XUV / Scorpio / Thar / Bolero), pickup
//                  (Pik-Up / Goa), and commercial truck (Blazo /
//                  Furio). The catalog targets the modern XUV700
//                  / XUV3OO / Thar / Scorpio-N range plus the BE
//                  EV sub-brand. Dealer toolchain is Mahindra
//                  e-Tool / DiagBox-Mahindra.
//------------------------------------------------------------------------------
unit OBD.OEM.Mahindra;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionMahindra = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionMahindra.ManufacturerKey: string;
begin Result := 'MAHINDRA'; end;

function TOBDOEMExtensionMahindra.DisplayName: string;
begin Result := 'Mahindra & Mahindra Ltd.'; end;

function TOBDOEMExtensionMahindra.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Mahindra Chakan + Nashik plants: MAJ. Mahindra Bengaluru
  // (commercial truck Blazo/Furio): MA6. Mahindra electric
  // (BE EV sub-brand) Pune: M3M.
  Result := (WMI = 'MAJ') or (WMI = 'MA6') or (WMI = 'M3M');
end;

procedure TOBDOEMExtensionMahindra.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',        'Engine ECU (mHawk diesel / mStallion petrol)'),
    ECU($7E1, 'transmission',  'Transmission Control (Aisin / Punch CVT / 6-speed AT)'),
    ECU($7E5, 'evcc',          'EV Charge Controller (XUV400 EV / BE EV)'),
    ECU($7E6, 'ev_motor',      'EV Drive Motor Inverter (BE EV)'),
    ECU($720, 'cluster',       'Instrument Cluster (digital MID)'),
    ECU($740, 'bcm',           'BCM — Body Control'),
    ECU($760, 'srs',           'SRS / Airbag (six-airbag standard from MY2024)'),
    ECU($762, 'abs',           'ABS / ESP'),
    ECU($768, 'climate',       'Climate Control'),
    ECU($770, 'ivi',           'AdrenoX / Sony Synaptic IVI'),
    ECU($724, 'adas',          'ADAS Level 2 (XUV700 / Scorpio-N AX7L)'),
    ECU($7B0, 'air_susp',      'Air-suspension (XUV700 AX7L Adventure)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Mahindra service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'mahindra_model_code',       'Mahindra model (e.g. XUV700, ScorpioN, Thar)'),
    DID($F1A2, 'mahindra_variant_code',     'Mahindra variant (AX5 / AX7 / AX7L / Z8 / Z8L)'),
    DID($F1A4, 'mahindra_engine_code',      'Engine code (mHawk D2.2, mStallion T-GDi 2.0)'),
    DID($F1B0, 'mahindra_options_block',    'Mahindra factory options block'),

    DID($D050, 'vehicle_mileage',           'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       'Battery voltage (mV)'),
    DID($D052, 'engine_oil_temperature',    'Engine oil temperature (°C)'),
    DID($D053, 'engine_coolant_temperature','Engine coolant temperature (°C)'),
    DID($D054, 'boost_pressure',            'Charge-air boost pressure (kPa, T-GDi)'),
    DID($D055, 'fuel_rail_pressure',        'Common-rail pressure (bar, mHawk diesel)'),
    DID($D056, 'fuel_level_pct',            'Fuel level (%)'),
    DID($D057, 'engine_runtime_h',          'Engine runtime lifetime (h)'),
    DID($D058, 'dpf_soot_load',             'DPF soot load (%, BS-VI mHawk diesel)'),

    DID($D060, 'be_ev_pack_voltage',        'BE EV / XUV400 EV pack voltage (V)'),
    DID($D061, 'be_ev_pack_soc',            'BE EV / XUV400 EV pack SOC (%)'),
    DID($D062, 'be_ev_pack_soh',            'BE EV / XUV400 EV pack SOH (%)'),
    DID($D063, 'be_ev_motor_temperature',   'BE EV motor temperature (°C)'),
    DID($D068, 'be_ev_charge_status',       'BE EV charge port status enum'),

    DID($D070, 'mahindra_atf_temp',         'AT / CVT fluid temperature (°C)'),
    DID($D080, 'mahindra_air_height_front', 'Air-suspension height — front (mm, AX7L)'),
    DID($D081, 'mahindra_air_height_rear',  'Air-suspension height — rear (mm, AX7L)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'idle_relearn',          'Idle re-learn (Mahindra e-Tool)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F01, 'mahindra_oil_life_reset','Reset engine oil-life monitor'),
    Routine($0F02, 'mahindra_dpf_force_regen','Forced DPF regeneration (mHawk BS-VI)'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('mahindra.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionMahindra.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionMahindra.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMahindra.DtcCatalogFileName: string;
begin Result := 'dtc-mahindra.json'; end;

function TOBDOEMExtensionMahindra.DecodeDID(const DID: Word;
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
                    $F1A0: 'mahindra_model_code';
                    $F1A2: 'mahindra_variant_code';
                    $F1A4: 'mahindra_engine_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMahindra.Create);

end.
