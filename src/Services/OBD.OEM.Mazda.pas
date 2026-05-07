//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Mazda.pas
// CONTENTS       : Mazda OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Mazda's M-MDS / IDS-Mazda toolchain shares the
//                  Ford IDS heritage (the brands shared a platform
//                  era); the wire format is the same, but the DID
//                  set diverges from CY-ML 2014 onward.
//------------------------------------------------------------------------------
unit OBD.OEM.Mazda;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionMazda = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionMazda.ManufacturerKey: string;
begin Result := 'MAZDA'; end;

function TOBDOEMExtensionMazda.DisplayName: string;
begin Result := 'Mazda Motor Corporation'; end;

function TOBDOEMExtensionMazda.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Mazda Japan: JM1 (passenger), JM3 (Mazda 3 export Eu/AU), JMZ
  //   (export Europe), JM7 (Mazda5).
  // Mazda US-built (was AAI joint venture w/ Ford in MI): 4F2 (CX-9
  //   Flat Rock), 4F4 (B-series).
  Result :=
    (WMI = 'JM1') or (WMI = 'JM3') or (WMI = 'JM7') or (WMI = 'JMZ') or
    (WMI = '4F2') or (WMI = '4F4');
end;

procedure TOBDOEMExtensionMazda.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'pcm',            'PCM — Powertrain Control'),
    ECU($7E1, 'tcm',            'TCM — Transmission Control'),
    ECU($726, 'rbcm',           'RBCM — Rear Body Control'),
    ECU($731, 'bcm',            'BCM — Body Control'),
    ECU($741, 'abs',            'ABS / DSC'),
    ECU($732, 'srs',            'SRS / Airbag'),
    ECU($720, 'cluster',        'Instrument Cluster'),
    ECU($7E5, 'hvac',           'Climate Control')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Mazda service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'mazda_as_built_code',       'Mazda As-Built configuration code'),
    DID($F1B0, 'mazda_market_code',         'Mazda market / region code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'idle_relearn',          'Idle re-learn (M-MDS)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('mazda.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionMazda.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-2014 Mazda PCMs (shared Ford lineage) accept the textbook
  // KWP2000 two's-complement at Level 1. M-MDS uses a proprietary
  // algorithm now — replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionMazda.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMazda.DtcCatalogFileName: string;
begin Result := 'dtc-mazda.json'; end;

function TOBDOEMExtensionMazda.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1B0:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'mazda_as_built_code';
                    $F1B0: 'mazda_market_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMazda.Create);

end.
