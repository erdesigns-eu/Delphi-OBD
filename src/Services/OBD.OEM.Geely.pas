//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Geely.pas
// CONTENTS       : Geely Auto + Lynk & Co OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Geely Holding Group owns Geely Auto + Lynk & Co
//                  + Volvo Cars + Polestar + LEVC + Proton (49%) +
//                  Lotus + smart (50%, joint with Mercedes). This
//                  extension covers the Geely Auto + Lynk & Co
//                  brands; Volvo Cars / Polestar use OBD.OEM.Volvo.
//                  Geely's CMA / SEA / SPA platforms share the
//                  electronic architecture used here.
//------------------------------------------------------------------------------
unit OBD.OEM.Geely;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionGeely = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionGeely.ManufacturerKey: string;
begin Result := 'GEELY'; end;

function TOBDOEMExtensionGeely.DisplayName: string;
begin Result := 'Geely Auto / Lynk & Co'; end;

function TOBDOEMExtensionGeely.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Geely Auto: LB3 (Geely Holding passenger), LFM (commercial).
  // Lynk & Co (Geely sub-brand, sold globally): LJV (Luqiao plant),
  //   LBE (Belgium-built export).
  // Geely's high-end Zeekr brand: LGZ.
  Result :=
    (WMI = 'LB3') or (WMI = 'LFM') or
    (WMI = 'LJV') or (WMI = 'LBE') or
    (WMI = 'LGZ');
end;

procedure TOBDOEMExtensionGeely.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'EMS — Engine Management'),
    ECU($7E1, 'transmission',   'TCU — Transmission Control'),
    ECU($7E2, 'hybrid',         'Hybrid Vehicle Control (PHEV)'),
    ECU($7E5, 'evcc',           'EV Charge Controller (Geometry / Zeekr)'),
    ECU($720, 'cluster',        'Instrument Cluster'),
    ECU($740, 'bcm',            'BCM — Body Control'),
    ECU($760, 'srs',            'SRS / Airbag'),
    ECU($762, 'abs',            'ABS / ESP'),
    ECU($768, 'climate',        'Climate Control'),
    ECU($724, 'adas',           'ADAS / driver-assistance')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Geely service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'geely_platform_code',       'Geely platform code (CMA / SEA / SPA / BMA)'),
    DID($F1A2, 'geely_market_code',         'Geely market / region code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('geely.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionGeely.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionGeely.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionGeely.DtcCatalogFileName: string;
begin Result := 'dtc-geely.json'; end;

function TOBDOEMExtensionGeely.DecodeDID(const DID: Word;
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
                    $F1A0: 'geely_platform_code';
                    $F1A2: 'geely_market_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionGeely.Create);

end.
