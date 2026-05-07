//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Toyota.pas
// CONTENTS       : Toyota / Lexus / Daihatsu OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : TechStream / Techstream-Lite is the Toyota dealer
//                  tool. Powertrain ECUs use the standard OBD-II
//                  0x7E0-0x7E7 range; chassis/body modules sit in
//                  0x7B0/0x7C0/0x758 (immobilizer). The starter
//                  catalogs ship verified=false; production users
//                  contribute via the v3.3 JSON pipeline.
//------------------------------------------------------------------------------
unit OBD.OEM.Toyota;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  TOBDOEMExtensionToyota = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionToyota.ManufacturerKey: string;
begin Result := 'TOYOTA'; end;

function TOBDOEMExtensionToyota.DisplayName: string;
begin Result := 'Toyota Motor Corporation'; end;

function TOBDOEMExtensionToyota.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Toyota Japan: JTD (passenger), JTE (Lexus SUV), JTH (Lexus
  // passenger), JTJ (Lexus crossover), JTK (Scion), JTM, JTN (RAV4)
  // Toyota US-built: 4T1 (Camry/Avalon), 5TD (Sienna), 5TE (Tacoma),
  // 5TF (Tundra), 5TY. Toyota Canada: 2T1 (Corolla).
  // Daihatsu: JDA. Lexus US-built (Georgetown): 2T2.
  Result :=
    (WMI = 'JTD') or (WMI = 'JTE') or (WMI = 'JTH') or
    (WMI = 'JTJ') or (WMI = 'JTK') or (WMI = 'JTM') or (WMI = 'JTN') or
    (WMI = '2T1') or (WMI = '2T2') or
    (WMI = '4T1') or (WMI = '4T3') or
    (WMI = '5TD') or (WMI = '5TE') or (WMI = '5TF') or (WMI = '5TY') or
    (WMI = 'JDA');
end;

procedure TOBDOEMExtensionToyota.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // TechStream physical request IDs. The 0x7Ex range is ISO 15765-4
  // emissions; the 0x7Bx / 0x7Cx range is Toyota-private.
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU (ECM)'),
    ECU($7E1, 'transmission',   'Transmission ECU'),
    ECU($7E2, 'hybrid',         'Hybrid Vehicle ECU (HV)'),
    ECU($7B0, 'abs',            'ABS / VSC'),
    ECU($7C0, 'srs',            'SRS / Airbag'),
    ECU($758, 'immo',           'Immobilizer / Smart Key'),
    ECU($7C4, 'body',           'Body Control'),
    ECU($7C8, 'cluster',        'Combination Meter (cluster)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session',     'Currently active UDS session'),
    DID($F187, 'spare_part_number',             'Toyota service part number'),
    DID($F189, 'sw_version_number',             'ECU software version'),
    DID($F190, 'vin',                           'Vehicle identification number'),
    DID($F1A0, 'toyota_calibration_id_list',    'CALID list for emissions cert'),
    DID($F1A1, 'toyota_ecu_serial',             'Toyota ECU serial (long form)'),
    DID($F197, 'system_name',                   'ECU long name')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',         'Reset adaptive learning'),
    Routine($0204, 'crank_relearn',             'Crankshaft position relearn'),
    Routine($0F00, 'sas_calibration',           'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',              'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',           'Post-flash checksum verification')
  ];

  MergeCatalogJSON('toyota.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionToyota.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-2010 ECUs (some 2AZ-FE / 2GR-FE controllers) accept the
  // ISO 14229 textbook two's-complement at Level 1. TechStream's
  // modern algorithm is NDA-protected — replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionToyota.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionToyota.DtcCatalogFileName: string;
begin Result := 'dtc-toyota.json'; end;

function TOBDOEMExtensionToyota.DecodeDID(const DID: Word;
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
        Result := Format('toyota_calibration_id_list = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionToyota.Create);

end.
