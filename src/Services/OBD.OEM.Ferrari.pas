//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Ferrari.pas
// CONTENTS       : Ferrari N.V. OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Ferrari is a separate company since the 2016
//                  spin-off from FCA / Stellantis. Dealer toolchain
//                  is Ferrari SD3 / Leonardo. The catalog targets
//                  the F8 / 296 / Roma / Purosangue / SF90 / 12Cilindri
//                  range. Ferrari uses the Magneti Marelli / Marelli
//                  ECUs that share heritage with FCA but the DIDs
//                  diverge.
//------------------------------------------------------------------------------
unit OBD.OEM.Ferrari;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionFerrari = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionFerrari.ManufacturerKey: string;
begin Result := 'FERRARI'; end;

function TOBDOEMExtensionFerrari.DisplayName: string;
begin Result := 'Ferrari N.V.'; end;

function TOBDOEMExtensionFerrari.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Ferrari Maranello: ZFF.
  Result := (WMI = 'ZFF');
end;

procedure TOBDOEMExtensionFerrari.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'me_ecu',         'ME — Engine Control (Marelli)'),
    ECU($7E1, 'tcu_dct',        'TCU — 7- / 8-speed DCT (Getrag F1)'),
    ECU($7E2, 'me_secondary',   'Secondary engine controller (V8 / V12 dual-bank)'),
    ECU($7E5, 'sf90_inverter',  'SF90 / 296 / 12Cilindri hybrid inverter'),
    ECU($7E6, 'sf90_emotor',    'SF90 / 296 hybrid e-motor'),
    ECU($7E7, 'sf90_battery',   'SF90 / 296 HV battery'),
    ECU($720, 'cluster',        'Driver Information Display'),
    ECU($740, 'srs',            'SRS / Airbag'),
    ECU($742, 'esp',            'CST — F1-Trac / SSC stability + traction'),
    ECU($744, 'manettino',      'Manettino + Race-Mode controller'),
    ECU($746, 'magneride',      'Magnetorheological dampers controller'),
    ECU($748, 'lift_axle',      'Front lift / nose-lift system'),
    ECU($760, 'climate',        'Climate Control'),
    ECU($770, 'mim_infotainment','Multifunction Infotainment Module'),
    ECU($780, 'tpms',           'Tire-pressure monitoring + temperature'),
    ECU($724, 'adas',           'ADAS / forward radar (Roma / Purosangue)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Ferrari service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'ferrari_model_code',        'Ferrari internal model (F142, F154, F160)'),
    DID($F1A2, 'ferrari_paint_code',        'Ferrari paint code (Rosso Corsa = 322, …)'),
    DID($F1A4, 'ferrari_options_block',     'Ferrari individual-options block'),
    DID($F1A6, 'ferrari_assembly_data',     'Maranello assembly date / line / inspector'),
    DID($F1B0, 'ferrari_warranty_block',    'Warranty start + delivery dealer'),

    DID($D050, 'vehicle_mileage',           'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       'Battery voltage (mV)'),
    DID($D052, 'engine_oil_temperature',    'Engine oil temperature (°C)'),
    DID($D053, 'engine_oil_pressure',       'Engine oil pressure (kPa)'),
    DID($D054, 'engine_oil_level',          'Engine oil level (mm above min)'),
    DID($D055, 'boost_pressure',            'Charge-air pressure (kPa, V8 turbo)'),
    DID($D056, 'engine_coolant_temp',       'Engine coolant temperature (°C)'),
    DID($D057, 'engine_runtime_h',          'Engine runtime lifetime (h)'),
    DID($D058, 'rear_axle_temperature',     'DCT / rear-axle temperature (°C)'),

    DID($D060, 'sf90_pack_voltage',         'SF90 / 296 HV pack voltage (V)'),
    DID($D061, 'sf90_pack_soc',             'SF90 / 296 HV pack SOC (%)'),
    DID($D062, 'sf90_pack_soh',             'SF90 / 296 HV pack SOH (%)'),
    DID($D063, 'sf90_motor_temperature',    'SF90 / 296 e-motor temperature (°C)'),
    DID($D068, 'sf90_charge_status',        'SF90 / 296 charge status enum'),

    DID($D080, 'manettino_position',        'Manettino selected position enum'),
    DID($D082, 'magneride_mode',            'Magnetorheological damper mode'),
    DID($D084, 'lift_axle_status',          'Front lift status (raised / lowered / fault)'),
    DID($D090, 'tire_temp_fl',              'Tire surface temperature — front-left (°C)'),
    DID($D091, 'tire_temp_fr',              'Tire surface temperature — front-right (°C)'),
    DID($D092, 'tire_temp_rl',              'Tire surface temperature — rear-left (°C)'),
    DID($D093, 'tire_temp_rr',              'Tire surface temperature — rear-right (°C)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'ferrari_dct_calibration','DCT touchpoint calibration (SD3)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F02, 'magneride_calibration', 'Magneride damper calibration'),
    Routine($0F03, 'lift_axle_test',        'Front lift system functional test'),
    Routine($0F04, 'ferrari_oil_life_reset','Reset engine oil-life monitor'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('ferrari.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionFerrari.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Ferrari shares Magneti Marelli ECU lineage with legacy FCA;
  // the textbook two's-complement is documented for the older
  // Marelli ECMs. Modern SD3 uses an NDA algorithm.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionFerrari.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionFerrari.DtcCatalogFileName: string;
begin Result := 'dtc-ferrari.json'; end;

function TOBDOEMExtensionFerrari.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4, $F1A6, $F1B0:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'ferrari_model_code';
                    $F1A2: 'ferrari_paint_code';
                    $F1A4: 'ferrari_options_block';
                    $F1A6: 'ferrari_assembly_data';
                    $F1B0: 'ferrari_warranty_block';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionFerrari.Create);

end.
