//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Mercedes.pas
// CONTENTS       : Reference Mercedes-Benz OEM extension (XENTRY-style)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Covers the WMIs Mercedes-Benz issues for passenger
//                  cars (WDB / WDC / WDD / WDF) plus Sprinter
//                  commercials (WD3 / WD4 / 4JG). Catalogues a starter
//                  set of XENTRY-visible DIDs and routines. Production
//                  XENTRY coverage requires per-FIN data files; this
//                  is a structural starting point.
//------------------------------------------------------------------------------
unit OBD.OEM.Mercedes;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>
  ///   XENTRY-style Mercedes negotiator. Reads F198 (workshop code)
  ///   immediately after the session-control reply because Daimler
  ///   ECUs gate routines on a known last-writer ID. Tester-present
  ///   interval is 1500 ms (matches XENTRY default).
  /// </summary>
  TOBDMercedesSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan; override;
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionMercedes = class(TOBDOEMExtensionBase)
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

function TOBDMercedesSessionNegotiator.BeginSessionPlan(
  SessionType: TOBDSessionType;
  const ECUAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(SessionType, ECUAddress);
  if SessionType = sstDefault then Exit;

  // Append a 22 F198 read — XENTRY relies on the response to log the
  // last writer; many ECUs refuse subsequent routines if it's
  // skipped after entering extended/programming.
  Result.Steps := Result.Steps + [
    UDSStep(TBytes.Create($22, $F1, $98), TBytes.Create($62, $F1, $98),
      'XENTRY workshop-code probe (DID F198)')
  ];
end;

function TOBDMercedesSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 1500;
end;

function TOBDMercedesSessionNegotiator.DisplayName: string;
begin
  Result := 'Mercedes XENTRY';
end;

function TOBDOEMExtensionMercedes.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDMercedesSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionMercedes.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-XENTRY HHTwin / Star Diagnosis modules accepted the textbook
  // two's-complement on legacy KWP2000 ECUs (e.g. EZS / EIS readers).
  // XENTRY's modern algorithm is NDA-protected — replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionMercedes.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMercedes.DtcCatalogFileName: string;
begin Result := 'dtc-mercedes.json'; end;

function TOBDOEMExtensionMercedes.ManufacturerKey: string;
begin Result := 'MB'; end;
function TOBDOEMExtensionMercedes.DisplayName: string;
begin Result := 'Mercedes-Benz Group'; end;

function TOBDOEMExtensionMercedes.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Passenger cars: WDB (legacy), WDC (US-built ML/GL/R), WDD (modern global),
  // WDF (Sprinter/Vito-class commercials). Sprinter/V-Class: WD3, WD4, 4JG.
  // smart (legacy MB shareholding): WME — handled by OBD.OEM.Smart if added.
  Result :=
    (WMI = 'WDB') or (WMI = 'WDC') or (WMI = 'WDD') or (WMI = 'WDF') or
    (WMI = 'WD3') or (WMI = 'WD4') or (WMI = '4JG');
end;

procedure TOBDOEMExtensionMercedes.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // XENTRY ECU bus map. Mercedes uses the ISO 15765-4 11-bit standard
  // OBD addresses for emissions ECUs (0x7E0-0x7E7) and a private 0x6xx
  // / 0x5xx range for body / chassis on each model platform.
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU (ME / CDI)'),
    ECU($7E1, 'transmission',   'Transmission (722.x / 7G-Tronic)'),
    ECU($7E2, 'me_secondary',   'Secondary engine controller (V-engines)'),
    ECU($620, 'esp',            'ESP / Brakes'),
    ECU($630, 'srs',            'SRS / Airbag'),
    ECU($640, 'cluster',        'Instrument Cluster (IC)'),
    ECU($660, 'eis',            'EIS — Electronic Ignition Switch'),
    ECU($6F1, 'tester',         'Tester (functional address)')
  ];

  // Mercedes XENTRY exposes most ECU metadata via standard UDS DIDs;
  // the F1xx range covers the SAE J2010 ISO mappings and is portable
  // across many vendors. Daimler-specific variant-coding lives in
  // F18x..F19x and per-component blocks in 02xx/03xx.
  DIDs := [
    DID($F186, 'active_diagnostic_session',     'Currently active UDS session'),
    DID($F187, 'spare_part_number',             'Mercedes hardware part number'),
    DID($F189, 'sw_version_number',             'Software version'),
    DID($F18A, 'system_supplier_identifier',    'ECU supplier id'),
    DID($F18B, 'manufacturing_date',            'ECU manufacturing date (YYMMDD BCD)'),
    DID($F18C, 'ecu_serial_number',             'ECU serial number'),
    DID($F190, 'vin',                           'Vehicle identification number'),
    DID($F191, 'vehicle_manufacturer_ecu_hw',   'MB hardware revision'),
    DID($F192, 'system_supplier_ecu_hw',        'Supplier hardware id'),
    DID($F195, 'system_supplier_sw',            'Supplier software version'),
    DID($F197, 'system_name',                   'ECU long name'),
    DID($F198, 'repair_shop_code',              'XENTRY repair shop code (last write)'),
    DID($F199, 'programming_date',              'Last programming date'),
    DID($F19D, 'tester_serial_number',          'Tester serial number'),
    DID($F19E, 'programming_status',            'Programming status code'),
    DID($0202, 'mileage_km',                    'Mileage in km (when supported)'),
    DID($0203, 'engine_running_seconds',        'Engine run-time (seconds)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',         'Reset adaptive learning'),
    Routine($0301, 'basic_setting',             'XENTRY basic setting routine'),
    Routine($0302, 'output_test',               'XENTRY actuator test'),
    Routine($0F00, 'sas_calibration',           'Steering-angle sensor calibration'),
    Routine($FF00, 'erase_memory',              'Pre-flash erase'),
    Routine($FF01, 'check_programming_dependencies',
                   'Cross-ECU dependency check'),
    Routine($FF02, 'verify_programming_checksum',
                   'Post-flash checksum verification')
  ];

  MergeCatalogJSON('mercedes.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionMercedes.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Mileage: Cardinal;
  Seconds: Cardinal;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F18B:
      if Length(Payload) >= 3 then
      begin
        // BCD-encoded YYMMDD.
        Result := Format('manufacturing_date = 20%.2x-%.2x-%.2x',
          [Payload[0], Payload[1], Payload[2]]);
        Exit;
      end;
    $0202:
      if Length(Payload) >= 3 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 16) or
                   (Cardinal(Payload[1]) shl 8)  or
                    Cardinal(Payload[2]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $0203:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('engine_running = %d s (%.1f h)',
          [Seconds, Seconds / 3600.0]);
        Exit;
      end;
    $F19E:
      if Length(Payload) >= 1 then
      begin
        case Payload[0] of
          $00: Result := 'programming_status = OK';
          $01: Result := 'programming_status = INTERRUPTED';
          $02: Result := 'programming_status = CHECKSUM_INVALID';
          $03: Result := 'programming_status = HW_INCOMPATIBLE';
        else
          Result := Format('programming_status = 0x%.2X', [Payload[0]]);
        end;
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMercedes.Create);

end.
