//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Nissan.pas
// CONTENTS       : Nissan / Infiniti / Datsun OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Consult III+ is the Nissan dealer tool. Pre-2010
//                  ECUs use Consult-II / KWP2000 — wrap those via the
//                  legacy 0x10 81 sub-function. Modern ECUs follow
//                  ISO 14229 UDS on the standard powertrain range.
//------------------------------------------------------------------------------
unit OBD.OEM.Nissan;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionNissan = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionNissan.ManufacturerKey: string;
begin Result := 'NISSAN'; end;

function TOBDOEMExtensionNissan.DisplayName: string;
begin Result := 'Nissan Motor (incl. Infiniti, Datsun)'; end;

function TOBDOEMExtensionNissan.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Nissan Japan: JN1 (passenger), JN6 (trucks), JN8 (Pathfinder JP).
  // Nissan US-built: 1N4 (Altima Tennessee), 1N6 (Frontier/Titan),
  //   3N1 (Mexico Sentra/Versa), 5N1 (Murano), 5BZ.
  // Infiniti: JNK (Japan G/Q sedan), JNR (Q60), JNX (QX SUV).
  // Datsun (revived budget brand 2014-2022): MNT (India).
  Result :=
    (WMI = 'JN1') or (WMI = 'JN6') or (WMI = 'JN8') or
    (WMI = '1N4') or (WMI = '1N6') or (WMI = '3N1') or
    (WMI = '5N1') or (WMI = '5BZ') or
    (WMI = 'JNK') or (WMI = 'JNR') or (WMI = 'JNX') or
    (WMI = 'MNT');
end;

procedure TOBDOEMExtensionNissan.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // Consult III+ ECU map. Powertrain modules sit on 0x7E0/0x7E1
  // (ISO 15765-4); body modules cluster around 0x740 / 0x745
  // (BCM / IPDM); chassis modules in 0x76x.
  ECUs := [
    ECU($7E0, 'ecm',            'ECM — Engine Control'),
    ECU($7E1, 'tcm',            'TCM — Transmission Control'),
    ECU($740, 'bcm',            'BCM — Body Control'),
    ECU($745, 'ipdm',           'IPDM E/R — Power Distribution'),
    ECU($763, 'abs',            'ABS / VDC'),
    ECU($75A, 'cluster',        'Combination Meter'),
    ECU($75D, 'srs',            'SRS / Airbag'),
    ECU($768, 'avm',            'AVM — Around-View Monitor'),
    ECU($793, 'evcc',            'EV Charge Controller (Leaf / Ariya)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Nissan service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A1, 'nissan_chassis_code',       'Nissan chassis code (e.g. T32, R35)'),
    DID($F1B0, 'nissan_market_code',        'Nissan market / region code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'idle_relearn',          'Idle volume relearn (Consult III+)'),
    Routine($0205, 'tps_relearn',           'Throttle valve closed-position relearn'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('nissan.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionNissan.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-2012 NATS (Nissan Anti-Theft System) modules accept the
  // KWP2000 textbook two's-complement at Level 1. Modern Consult
  // III+ uses an NDA-protected algorithm — replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionNissan.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionNissan.DtcCatalogFileName: string;
begin Result := 'dtc-nissan.json'; end;

function TOBDOEMExtensionNissan.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A1, $F1B0:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A1: 'nissan_chassis_code';
                    $F1B0: 'nissan_market_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionNissan.Create);

end.
