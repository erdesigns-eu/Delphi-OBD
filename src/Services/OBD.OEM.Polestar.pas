//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Polestar.pas
// CONTENTS       : Polestar Performance AB OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Polestar is the Geely-owned premium-EV brand
//                  spun out of Volvo Cars. Polestar 2 / 3 / 4 /
//                  5 share platforms with Volvo (CMA / SPA2 /
//                  SEA / PPA) so VIDA tooling overlaps; this
//                  extension covers Polestar-specific WMIs that
//                  don't collide with Volvo Cars' own range.
//------------------------------------------------------------------------------
unit OBD.OEM.Polestar;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionPolestar = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionPolestar.ManufacturerKey: string;
begin Result := 'POLESTAR'; end;

function TOBDOEMExtensionPolestar.DisplayName: string;
begin Result := 'Polestar Performance AB (Geely)'; end;

function TOBDOEMExtensionPolestar.ApplicableToVIN(
  const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Polestar 2 (Luqiao + Chengdu China): LPS.
  // Polestar 4 (Hangzhou China, SEA platform): LFP.
  // Polestar 3 shares VIN with Volvo Cars Chengdu (LYV) — claimed
  // by OBD.OEM.Volvo to avoid a collision; resolve Polestar 3
  // explicitly via TOBDOEMRegistry.FindByKey('POLESTAR') after
  // VIN routing.
  Result := (WMI = 'LPS') or (WMI = 'LFP');
end;

procedure TOBDOEMExtensionPolestar.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // Polestar 2 / 3 / 4 share much of Volvo's CMA / SPA2 / SEA
  // electronic architecture; the ECU map mirrors OBD.OEM.Volvo
  // with EV-specific additions for the Polestar 2's larger
  // pack and the dual-motor + Performance Pack variants.
  ECUs := [
    ECU($740, 'cem',          'CEM — Central Electronic Module'),
    ECU($742, 'cluster',      'DIM — Driver Information Module'),
    ECU($760, 'srs',          'SRS / Airbag'),
    ECU($762, 'abs',          'ABS / DSTC'),
    ECU($770, 'sensus',       'Sensus / IHU — Infotainment'),
    ECU($782, 'bms',          'BMS — High-voltage battery (75/79/82/106 kWh)'),
    ECU($785, 'evcc',         'On-board Charger + Charge Port'),
    ECU($710, 'motor_front',  'Front Motor Inverter (single / dual-motor)'),
    ECU($712, 'motor_rear',   'Rear Motor Inverter (Performance Pack)'),
    ECU($724, 'pilot_assist', 'Pilot Assist driver-assistance computer')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'polestar_model_code',       'Polestar model code (P2 / P3 / P4 / P5)'),
    DID($F1A2, 'polestar_drivetrain',       'Polestar drivetrain (Single / Long-range / Performance)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F02, 'rdc_relearn',           'TPMS / RDC sensor relearn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('polestar.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionPolestar.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionPolestar.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionPolestar.DtcCatalogFileName: string;
begin Result := 'dtc-polestar.json'; end;

function TOBDOEMExtensionPolestar.DecodeDID(const DID: Word;
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
                    $F1A0: 'polestar_model_code';
                    $F1A2: 'polestar_drivetrain';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionPolestar.Create);

end.
