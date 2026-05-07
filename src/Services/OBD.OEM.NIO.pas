//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.NIO.pas
// CONTENTS       : NIO Inc. OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : NIO is a Chinese EV-only OEM (Shanghai HQ,
//                  Hefei plant) known for its battery-swap
//                  network. The catalog targets the ES8 / ES6 /
//                  EC6 / ET7 / ET5 range plus the Banyan + Aspen
//                  software platforms. NIO doesn't ship a
//                  classical dealer tool — service is done via
//                  cloud-connected NIO House sessions — but the
//                  on-vehicle ECUs respond to standard ISO 14229.
//------------------------------------------------------------------------------
unit OBD.OEM.NIO;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionNIO = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionNIO.ManufacturerKey: string;
begin Result := 'NIO'; end;

function TOBDOEMExtensionNIO.DisplayName: string;
begin Result := 'NIO Inc.'; end;

function TOBDOEMExtensionNIO.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // NIO: LJN (Hefei plant, JAC-NIO joint manufacturing), LBL.
  Result :=
    (WMI = 'LJN') or (WMI = 'LBL');
end;

procedure TOBDOEMExtensionNIO.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // NIO ECU map. The swappable battery pack lives behind 0x782
  // BMS plus 0x792 charger; the dual-motor system has front +
  // rear motor inverters at 0x710 / 0x712.
  ECUs := [
    ECU($7E0, 'vcu',          'VCU — Vehicle Control Unit'),
    ECU($710, 'motor_front',  'Front Motor Inverter'),
    ECU($712, 'motor_rear',   'Rear Motor Inverter'),
    ECU($782, 'bms',          'BMS — Swappable Battery'),
    ECU($792, 'charge',       'Charge Port + AC/DC charger'),
    ECU($720, 'cluster',      'Instrument Cluster + AR-HUD'),
    ECU($724, 'aquila',       'Aquila — Autonomous Driving Sensor Suite'),
    ECU($762, 'bcm',          'BCM — Body Control'),
    ECU($768, 'climate',      'Climate Control / Heat Pump'),
    ECU($770, 'banyan',       'Banyan / Aspen IVI computer (NOMI)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'NIO service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'nio_model_code',            'NIO model code (e.g. ES8, ET7)'),
    DID($F1A2, 'nio_battery_swap_id',       'Swappable battery pack ID')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'battery_swap_handshake', 'Pre-swap handshake (NIO Power Swap)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('nio.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionNIO.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionNIO.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionNIO.DtcCatalogFileName: string;
begin Result := 'dtc-nio.json'; end;

function TOBDOEMExtensionNIO.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'nio_model_code';
                    $F1A2: 'nio_battery_swap_id';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionNIO.Create);

end.
