//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Mitsubishi.pas
// CONTENTS       : Mitsubishi Motors Corp. OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Mitsubishi MUT-III is the dealer tool. Mitsubishi
//                  is part of the Renault-Nissan-Mitsubishi alliance
//                  but ships its own electronic platform — the
//                  catalog is intentionally kept separate.
//------------------------------------------------------------------------------
unit OBD.OEM.Mitsubishi;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionMitsubishi = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionMitsubishi.ManufacturerKey: string;
begin Result := 'MITSU'; end;

function TOBDOEMExtensionMitsubishi.DisplayName: string;
begin Result := 'Mitsubishi Motors Corp.'; end;

function TOBDOEMExtensionMitsubishi.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Mitsubishi Japan: JA3 (passenger Mirage / Lancer), JA4 (SUV
  //   Outlander / ASX / Eclipse Cross), JMB (Hong Kong assembled),
  //   JMY (Mizushima passenger).
  // Mitsubishi US-built (1988-2015 Diamond Star Motors / DSM,
  //   Normal IL): 4A3, 4A4 (Eclipse / Eagle Talon).
  // Mitsubishi Thailand (Laem Chabang): MMB.
  // Mitsubishi Australia (Adelaide, defunct 2008): 6MM.
  Result :=
    (WMI = 'JA3') or (WMI = 'JA4') or (WMI = 'JMB') or (WMI = 'JMY') or
    (WMI = '4A3') or (WMI = '4A4') or
    (WMI = 'MMB') or (WMI = '6MM');
end;

procedure TOBDOEMExtensionMitsubishi.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU'),
    ECU($7E1, 'transmission',   'TCM (incl. SST DCT controllers)'),
    ECU($7B0, 'abs',            'ASC / ABS'),
    ECU($7C0, 'srs',            'SRS / Airbag'),
    ECU($7C4, 'etacs',          'ETACS — Body Control'),
    ECU($7C8, 'cluster',        'Combination Meter'),
    ECU($762, 'awc',            'AWC — Active Wheel Control (Outlander PHEV)'),
    ECU($7E5, 'evcc',           'EV Charge Controller (Outlander PHEV / iMiEV)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Mitsubishi service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'mitsu_chassis_code',        'Mitsubishi chassis code (e.g. CW6W)'),
    DID($F1B0, 'mitsu_market_code',         'Mitsubishi market / region code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'sst_dct_calibration',   'SST DCT calibration (MUT-III)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('mitsubishi.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionMitsubishi.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionMitsubishi.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMitsubishi.DtcCatalogFileName: string;
begin Result := 'dtc-mitsubishi.json'; end;

function TOBDOEMExtensionMitsubishi.DecodeDID(const DID: Word;
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
                    $F1A0: 'mitsu_chassis_code';
                    $F1B0: 'mitsu_market_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMitsubishi.Create);

end.
