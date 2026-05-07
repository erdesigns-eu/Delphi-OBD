//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.GreatWall.pas
// CONTENTS       : Great Wall Motor (GWM) OEM extension
//                  (Haval / WEY / ORA / Tank / Poer)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Great Wall Motor (Baoding HQ) groups several
//                  brands under one electronic platform: Haval
//                  (mid-range SUV), WEY (premium SUV), ORA (EV
//                  hatch), Tank (off-road SUV), Poer (pickup).
//                  The catalog covers the shared E-Drive Toolbox
//                  surface across all five brands.
//------------------------------------------------------------------------------
unit OBD.OEM.GreatWall;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionGreatWall = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionGreatWall.ManufacturerKey: string;
begin Result := 'GWM'; end;

function TOBDOEMExtensionGreatWall.DisplayName: string;
begin Result := 'Great Wall Motor (Haval / WEY / ORA / Tank / Poer)'; end;

function TOBDOEMExtensionGreatWall.ApplicableToVIN(
  const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // GWM Baoding HQ: LGW (passenger / Haval / WEY).
  // ORA (GWM EV brand): LGE.
  // Tank (GWM off-road sub-brand): LGT.
  // GWM Russia (Tula): X9X (export).
  Result :=
    (WMI = 'LGW') or (WMI = 'LGE') or (WMI = 'LGT') or
    (WMI = 'X9X');
end;

procedure TOBDOEMExtensionGreatWall.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'EMS — Engine Management'),
    ECU($7E1, 'transmission',   'TCU — Transmission Control (incl. 9HAT)'),
    ECU($7E2, 'hybrid',         'GWM Hi4 hybrid controller (PHEV)'),
    ECU($7E5, 'evcc',           'EV Charge Controller (ORA / Coffee EV)'),
    ECU($720, 'cluster',        'Instrument Cluster'),
    ECU($740, 'bcm',            'BCM — Body Control'),
    ECU($760, 'srs',            'SRS / Airbag'),
    ECU($762, 'abs',            'ABS / ESP'),
    ECU($768, 'climate',        'Climate Control'),
    ECU($724, 'adas',           'ADAS / Coffee Pilot')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'GWM service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'gwm_brand_code',            'GWM brand code (HAVAL / WEY / ORA / TANK / POER)'),
    DID($F1A2, 'gwm_platform_code',         'GWM platform code (Lemon / Tank / Coffee)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('gwm.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionGreatWall.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionGreatWall.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionGreatWall.DtcCatalogFileName: string;
begin Result := 'dtc-gwm.json'; end;

function TOBDOEMExtensionGreatWall.DecodeDID(const DID: Word;
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
                    $F1A0: 'gwm_brand_code';
                    $F1A2: 'gwm_platform_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionGreatWall.Create);

end.
