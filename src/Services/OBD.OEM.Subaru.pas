//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Subaru.pas
// CONTENTS       : Subaru OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : SSM4 (Subaru Select Monitor 4) is the dealer
//                  tool. Powertrain ECUs use the standard ISO 15765-4
//                  range; AWD-specific modules (AT/AWD controller,
//                  Active Torque Vectoring) live in 0x7Ex.
//------------------------------------------------------------------------------
unit OBD.OEM.Subaru;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionSubaru = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionSubaru.ManufacturerKey: string;
begin Result := 'SUBARU'; end;

function TOBDOEMExtensionSubaru.DisplayName: string;
begin Result := 'Subaru Corporation'; end;

function TOBDOEMExtensionSubaru.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Subaru Japan: JF1 (passenger Impreza/WRX), JF2 (SUV/CUV
  //   Outback/Forester), JF3 (XV/Crosstrek).
  // Subaru US-built (Lafayette IN, SIA): 4S3 (Legacy/Outback),
  //   4S4 (Forester / Ascent / Crosstrek).
  Result :=
    (WMI = 'JF1') or (WMI = 'JF2') or (WMI = 'JF3') or
    (WMI = '4S3') or (WMI = '4S4');
end;

procedure TOBDOEMExtensionSubaru.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU (ECM)'),
    ECU($7E1, 'transmission',   'TCU — incl. CVT control'),
    ECU($7E2, 'awd',            'Active Torque Vectoring / AWD controller'),
    ECU($7B0, 'abs',            'ABS / VDC'),
    ECU($7C0, 'srs',            'SRS / Airbag'),
    ECU($7C8, 'cluster',        'Combination Meter (cluster)'),
    ECU($7C4, 'body',           'Body Integrated Unit (BIU)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Subaru service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'subaru_chassis_code',       'Subaru chassis code (e.g. GP, SK)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'cvt_relearn',           'CVT clutch-pressure relearn (SSM4)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('subaru.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionSubaru.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  MASK: array[0..1] of Byte = ($53, $42); // 'SB'
var
  Mask: TBytes;
begin
  // Pre-2012 ECUs documented in the OpenECU community ship a 2-byte
  // byte-rotate algorithm at Level 1. Replace at startup.
  SetLength(Mask, Length(MASK));
  Move(MASK[0], Mask[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyByteRotate.Create(
    1, 4, Mask, 'Subaru SSM community byte-rotate placeholder',
    'community-pr', False));
end;

procedure TOBDOEMExtensionSubaru.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionSubaru.DtcCatalogFileName: string;
begin Result := 'dtc-subaru.json'; end;

function TOBDOEMExtensionSubaru.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0:
      if Length(Payload) > 0 then
      begin
        Result := Format('subaru_chassis_code = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionSubaru.Create);

end.
