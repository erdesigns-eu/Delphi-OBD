//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Honda.pas
// CONTENTS       : Honda / Acura OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Honda Diagnostic System (HDS) targets the
//                  ISO 15765-4 standard powertrain range plus a
//                  small set of Honda-private addresses for the
//                  body / immobilizer / SRS modules.
//------------------------------------------------------------------------------
unit OBD.OEM.Honda;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionHonda = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionHonda.ManufacturerKey: string;
begin Result := 'HONDA'; end;

function TOBDOEMExtensionHonda.DisplayName: string;
begin Result := 'Honda Motor Co. (incl. Acura)'; end;

function TOBDOEMExtensionHonda.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Honda Japan: JHM (passenger), JHL (trucks), JHF (Acura).
  // Honda US-built: 1HG (Civic / Accord), 19U (Acura), 19V (CR-V),
  //   2HG (Canada Civic), 2HK, 2HN (Acura MDX), 5J6 (Pilot/Acura RDX),
  //   5FN (Pilot/Odyssey).
  // Honda Mexico: 3HG.
  Result :=
    (WMI = 'JHM') or (WMI = 'JHL') or (WMI = 'JHF') or
    (WMI = 'JH4') or (WMI = '1HG') or (WMI = '19U') or
    (WMI = '19V') or (WMI = '2HG') or (WMI = '2HK') or
    (WMI = '2HN') or (WMI = '3HG') or (WMI = '5J6') or
    (WMI = '5FN') or (WMI = '5FP');
end;

procedure TOBDOEMExtensionHonda.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'pcm',            'PCM — Powertrain Control'),
    ECU($7E1, 'tcm',            'TCM — Transmission Control'),
    ECU($7E2, 'abs',            'VSA / ABS'),
    ECU($7E3, 'srs',            'SRS / Airbag'),
    ECU($7E4, 'bcm',            'BCM — Body Control'),
    ECU($7E5, 'cluster',        'Gauge Control / Cluster'),
    ECU($7E6, 'hvac',           'Climate Control')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Honda service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'honda_chassis_code',        'Honda chassis code (e.g. FK7, RZ4)'),
    DID($F1A2, 'honda_factory_code',        'Honda factory / build plant code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'idle_relearn',          'Idle-air-control relearn (HDS)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('honda.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionHonda.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  PUBLIC_MASK: array[0..3] of Byte = ($53, $48, $4D, $4F); // 'SHMO'
var
  Mask: TBytes;
begin
  // HDS community placeholder XOR mask (the 'SHMO' marker some
  // pre-2010 PCMs used for Level 1). Replace at startup with the
  // real algorithm.
  SetLength(Mask, Length(PUBLIC_MASK));
  Move(PUBLIC_MASK[0], Mask[0], Length(PUBLIC_MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(Mask,
    'Honda HDS community XOR-mask placeholder', 'community-pr', False));
end;

procedure TOBDOEMExtensionHonda.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionHonda.DtcCatalogFileName: string;
begin Result := 'dtc-honda.json'; end;

function TOBDOEMExtensionHonda.DecodeDID(const DID: Word;
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
                    $F1A0: 'honda_chassis_code';
                    $F1A2: 'honda_factory_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionHonda.Create);

end.
