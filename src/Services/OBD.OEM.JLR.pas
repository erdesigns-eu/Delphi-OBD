//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.JLR.pas
// CONTENTS       : Jaguar Land Rover OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : JLR (Tata Motors-owned since 2008) ships its
//                  own diagnostic stack (SDD / Pathfinder, more
//                  recently the Topix Cloud workshop platform).
//                  Catalog targets the Range Rover / Range Rover
//                  Sport / Velar / Evoque / Discovery / Defender /
//                  Jaguar XF / F-Pace / I-Pace / F-Type range.
//------------------------------------------------------------------------------
unit OBD.OEM.JLR;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionJLR = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionJLR.ManufacturerKey: string;
begin Result := 'JLR'; end;

function TOBDOEMExtensionJLR.DisplayName: string;
begin Result := 'Jaguar Land Rover Limited (Tata)'; end;

function TOBDOEMExtensionJLR.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Jaguar Castle Bromwich + Solihull: SAJ.
  // Land Rover Solihull: SAL.
  // Range Rover Halewood (Evoque / Discovery Sport): SAD.
  // JLR China (Chery-JLR JV, Changshu): LRW share — but LRW
  // collides with Tesla Shanghai. Skip China-built JLR for now.
  // JLR India (Pune assembly): MA1.
  Result :=
    (WMI = 'SAJ') or (WMI = 'SAL') or (WMI = 'SAD') or (WMI = 'MA1');
end;

procedure TOBDOEMExtensionJLR.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'pcm',          'PCM — Powertrain Control'),
    ECU($7E1, 'tcm',          'TCM — Transmission'),
    ECU($7E5, 'evcc',         'EV Charge Controller (I-Pace / Range Rover EV)'),
    ECU($720, 'cluster',      'IPC — Instrument Cluster'),
    ECU($731, 'cjb',          'CJB — Central Junction Box (BCM)'),
    ECU($746, 'abs',          'ABS / DSC'),
    ECU($760, 'srs',          'SRS / Airbag'),
    ECU($762, 'tcb',          'TCB — Telematics / SOS'),
    ECU($768, 'climate',      'ATC — Climate Control'),
    ECU($770, 'ivi',          'IVI / Touch Pro Duo')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'JLR service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'jlr_model_code',            'JLR model code (e.g. L405, X590)'),
    DID($F1A2, 'jlr_assembly_plant',        'JLR assembly plant code'),
    DID($F1B0, 'jlr_options_block',         'JLR factory options block')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F02, 'air_suspension_calibration', 'Air-suspension ride-height calibration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('jlr.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionJLR.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionJLR.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionJLR.DtcCatalogFileName: string;
begin Result := 'dtc-jlr.json'; end;

function TOBDOEMExtensionJLR.DecodeDID(const DID: Word;
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
                    $F1A0: 'jlr_model_code';
                    $F1A2: 'jlr_assembly_plant';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionJLR.Create);

end.
