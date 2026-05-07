//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Porsche.pas
// CONTENTS       : Porsche AG OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Porsche is part of the VW Group but ships its
//                  own diagnostic stack (PIWIS — Porsche Integrated
//                  Workshop Information System), so it gets its
//                  own extension separate from OBD.OEM.VW. The
//                  catalog targets the 911 / 718 / Cayenne / Macan
//                  / Panamera / Taycan range.
//------------------------------------------------------------------------------
unit OBD.OEM.Porsche;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionPorsche = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionPorsche.ManufacturerKey: string;
begin Result := 'PORSCHE'; end;

function TOBDOEMExtensionPorsche.DisplayName: string;
begin Result := 'Dr. Ing. h.c. F. Porsche AG'; end;

function TOBDOEMExtensionPorsche.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Porsche Stuttgart Zuffenhausen (911 / 718): WP0.
  // Porsche Leipzig (Cayenne / Macan / Panamera): WP1.
  // Taycan also Zuffenhausen → WP0.
  Result := (WMI = 'WP0') or (WMI = 'WP1');
end;

procedure TOBDOEMExtensionPorsche.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'dme',          'DME — Engine Control'),
    ECU($7E1, 'pdk',          'PDK — Doppelkupplungsgetriebe (DCT)'),
    ECU($7E2, 'pasm',         'PASM — Active Suspension Management'),
    ECU($7E5, 'pe_inverter',  'EV Inverter / Front Motor (Taycan)'),
    ECU($7E7, 'pe_battery',   'High-Voltage Battery (Taycan)'),
    ECU($720, 'cluster',      'Instrument Cluster'),
    ECU($740, 'pcm',          'PCM — Infotainment'),
    ECU($760, 'srs',          'SRS / Airbag'),
    ECU($762, 'pccm',         'Climate Control'),
    ECU($724, 'lwl_radar',    'Driver assist / radar (LWL fiber-bus gateway)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Porsche service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'porsche_model_code',        'Porsche internal model code (e.g. 992, 9YA)'),
    DID($F1A2, 'porsche_paint_code',        'Porsche paint code (5-character)'),
    DID($F1A4, 'porsche_options_block',     'Porsche M-Nummern option block')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F02, 'pasm_calibration',      'PASM ride-height calibration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('porsche.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionPorsche.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionPorsche.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionPorsche.DtcCatalogFileName: string;
begin Result := 'dtc-porsche.json'; end;

function TOBDOEMExtensionPorsche.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'porsche_model_code';
                    $F1A2: 'porsche_paint_code';
                    $F1A4: 'porsche_options_block';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionPorsche.Create);

end.
