//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Suzuki.pas
// CONTENTS       : Suzuki Motor Corp / Maruti Suzuki OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Suzuki Diagnostic Tool II (SDT-II) is the
//                  dealer system. Maruti Suzuki (India) shares the
//                  same toolchain on most newer-platform models;
//                  legacy Maruti 800 / Alto 800 use a custom
//                  KWP1281-style protocol that is NOT covered here.
//------------------------------------------------------------------------------
unit OBD.OEM.Suzuki;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionSuzuki = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionSuzuki.ManufacturerKey: string;
begin Result := 'SUZUKI'; end;

function TOBDOEMExtensionSuzuki.DisplayName: string;
begin Result := 'Suzuki Motor Corp. (incl. Maruti Suzuki India)'; end;

function TOBDOEMExtensionSuzuki.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Suzuki Japan: JS1 (passenger), JS2 (Hamamatsu vans / commercial),
  //   JSA (Allion era), JSB. Suzuki Hungary (Esztergom): TSM.
  // Suzuki Changhe China (joint venture): LSJ.
  // Maruti Suzuki India: MA3 (Gurgaon), MBH (Manesar export), ML8.
  Result :=
    (WMI = 'JS1') or (WMI = 'JS2') or (WMI = 'JSA') or (WMI = 'JSB') or
    (WMI = 'TSM') or (WMI = 'LSJ') or
    (WMI = 'MA3') or (WMI = 'MBH') or (WMI = 'ML8');
end;

procedure TOBDOEMExtensionSuzuki.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU (ECM)'),
    ECU($7E1, 'transmission',   'TCM (incl. AGS / CVT controllers)'),
    ECU($7B0, 'abs',            'ABS / ESP'),
    ECU($7C0, 'srs',            'SRS / Airbag'),
    ECU($7C8, 'cluster',        'Combination Meter'),
    ECU($7E5, 'hvac',           'Climate Control'),
    ECU($7C4, 'body',           'BCM — Body Control')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Suzuki service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'suzuki_chassis_code',       'Suzuki / Maruti chassis code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'idle_relearn',          'Idle-air-control relearn (SDT-II)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('suzuki.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionSuzuki.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionSuzuki.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionSuzuki.DtcCatalogFileName: string;
begin Result := 'dtc-suzuki.json'; end;

function TOBDOEMExtensionSuzuki.DecodeDID(const DID: Word;
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
        Result := Format('suzuki_chassis_code = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionSuzuki.Create);

end.
