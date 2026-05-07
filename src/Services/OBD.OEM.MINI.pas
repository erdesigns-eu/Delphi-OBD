//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.MINI.pas
// CONTENTS       : MINI (BMW Group sub-brand) OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : MINI is a BMW Group sub-brand but uses its own
//                  WMIs (WMW Oxford UK + SAW China BMW Brilliance).
//                  Architecture overlaps with BMW E-Sys / ISTA but
//                  the model codes (R56, F56, F60, J01, J05) differ.
//                  Inherits the BMW session-negotiation lineage
//                  (1500 ms heartbeat, security access required for
//                  coding writes).
//------------------------------------------------------------------------------
unit OBD.OEM.MINI;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  /// <summary>MINI uses BMW E-Sys / ISTA, so the same negotiator
  /// applies (security access required for both extended +
  /// programming sessions; 1500 ms heartbeat for older R-series
  /// DMEs).</summary>
  TOBDMINISessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean; override;
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionMINI = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    function CreateSessionNegotiator: IOBDSessionNegotiator; override;
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

function TOBDMINISessionNegotiator.RequiresSecurityAccess(
  SessionType: TOBDSessionType): Boolean;
begin
  Result := SessionType in [sstExtendedDiagnostic, sstProgramming,
                            sstOEMSpecific1, sstOEMSpecific2];
end;

function TOBDMINISessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin Result := 1500; end;

function TOBDMINISessionNegotiator.DisplayName: string;
begin Result := 'MINI (BMW E-Sys / ISTA)'; end;

function TOBDOEMExtensionMINI.ManufacturerKey: string;
begin Result := 'MINI'; end;

function TOBDOEMExtensionMINI.DisplayName: string;
begin Result := 'MINI (BMW Group sub-brand)'; end;

function TOBDOEMExtensionMINI.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // MINI Oxford (UK Plant Oxford): WMW.
  // MINI Spotlight Automotive (BMW Brilliance JV, Zhangjiagang
  // China — built MINI Cooper E + Aceman): SAW.
  Result := (WMI = 'WMW') or (WMI = 'SAW');
end;

procedure TOBDOEMExtensionMINI.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($12, 'dme',          'DME — Engine ECU (B38 / B48 / B58)'),
    ECU($07, 'egs',          'EGS — Aisin / GS6F-21WA / 7DCT'),
    ECU($29, 'dsc',          'DSC — Stability Control'),
    ECU($40, 'kombi',        'KOMBI — Instrument Cluster'),
    ECU($60, 'frm',          'FRM — Footwell Module'),
    ECU($72, 'cas',          'CAS — Car Access System'),
    ECU($10, 'gateway',      'ZGW — Central Gateway'),
    ECU($63, 'idrive',       'iDrive / NBT / EntryNav'),
    ECU($65, 'climate',      'IHKA — Climate Control'),
    ECU($30, 'srs',          'ACSM — Crash Safety Module'),
    ECU($7E5, 'evcc',        'MINI Cooper E / SE / Aceman EV charge controller'),
    ECU($7E6, 'ev_motor',    'MINI Cooper E / SE / Aceman EV motor inverter'),
    ECU($7E7, 'ev_battery',  'MINI Cooper E / SE / Aceman HV battery')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'MINI service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F100, 'i_stufe_werks',             'Factory I-Stufe (build version)'),
    DID($F101, 'i_stufe_aktuell',           'Current I-Stufe (last programmed)'),
    DID($F1A2, 'fa_assembly',               'Vehicle order (FA / SALAPA option codes)'),
    DID($F1A4, 'mini_chassis_code',         'MINI chassis code (R56, F56, F60, J01, J05)'),

    DID($D050, 'mileage_km',                'Vehicle mileage in km'),
    DID($D051, 'battery_voltage_12v',       'Battery voltage (mV)'),
    DID($D052, 'engine_oil_temperature',    'Engine oil temperature (°C)'),
    DID($D053, 'engine_oil_level_mm',       'Engine oil level (mm above min)'),
    DID($D054, 'engine_runtime_h',          'Engine runtime lifetime (h)'),
    DID($D055, 'boost_pressure',            'Charge-air boost pressure (kPa, B38/B48 turbo)'),
    DID($D056, 'engine_coolant_temperature','Engine coolant temperature (°C)'),
    DID($D057, 'fuel_level_pct',            'Fuel level (%)'),
    DID($D058, 'fuel_consumption_recent',   'Recent fuel consumption (L/100km)'),

    DID($D060, 'mini_e_pack_voltage',       'MINI Cooper E / SE pack voltage (V)'),
    DID($D061, 'mini_e_pack_soc',           'MINI Cooper E / SE pack SOC (%)'),
    DID($D062, 'mini_e_pack_soh',           'MINI Cooper E / SE pack SOH (%)'),
    DID($D063, 'mini_e_remaining_range',    'MINI Cooper E / SE remaining range (km)'),
    DID($D064, 'mini_e_motor_temperature',  'MINI Cooper E / SE motor temperature (°C)'),
    DID($D068, 'mini_e_charge_status',      'MINI Cooper E / SE charge port status enum'),

    DID($D070, 'mini_brake_pad_front',      'Front brake-pad remaining (%)'),
    DID($D071, 'mini_brake_pad_rear',       'Rear brake-pad remaining (%)'),
    DID($D072, 'mini_oil_quality',          'Engine oil quality (%, MINI CBS)'),
    DID($D073, 'mini_remaining_oil_distance','Remaining distance to next oil service (km)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F03, 'fa_write',              'Write FA / vehicle order (E-Sys)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F01, 'mini_cas_relearn',      'CAS proximity / key relearn'),
    Routine($0F02, 'mini_epb_service',      'EMF / EPB electronic parking-brake service mode'),
    Routine($0F04, 'mini_rdc_relearn',      'RDC tire-pressure sensor relearn'),
    Routine($0F05, 'mini_battery_register', 'Battery registration after replacement (BMS)'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('mini.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionMINI.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDMINISessionNegotiator.Create; end;

procedure TOBDOEMExtensionMINI.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  PUBLIC_MASK: array[0..3] of Byte = ($A5, $5A, $C3, $3C);
var
  Mask: TBytes;
begin
  // MINI inherits BMW E-Sys lineage; same XOR-mask placeholder
  // applies until the production NDA algorithm is registered.
  SetLength(Mask, Length(PUBLIC_MASK));
  Move(PUBLIC_MASK[0], Mask[0], Length(PUBLIC_MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(Mask,
    'MINI (BMW E-Sys lineage) XOR-mask placeholder', 'community-pr', False));
end;

procedure TOBDOEMExtensionMINI.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMINI.DtcCatalogFileName: string;
begin Result := 'dtc-mini.json'; end;

function TOBDOEMExtensionMINI.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A4:
      if Length(Payload) > 0 then
      begin
        Result := Format('mini_chassis_code = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMINI.Create);

end.
