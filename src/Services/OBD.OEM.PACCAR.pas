//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.PACCAR.pas
// CONTENTS       : PACCAR Inc. heavy-duty OEM extension
//                  (Peterbilt + Kenworth + DAF + Leyland)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : PACCAR ships its own MX engine line (MX-11 /
//                  MX-13 from DAF Eindhoven) plus integrates
//                  Cummins X15 / ISX15 in NA market trucks. The
//                  catalog targets the MX-side; for Cummins-engined
//                  trucks layer OBD.OEM.Cummins on top via
//                  TOBDOEMRegistry.FindByKey('CUMMINS').
//------------------------------------------------------------------------------
unit OBD.OEM.PACCAR;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionPACCAR = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    function CreateSessionNegotiator: IOBDSessionNegotiator; override;
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

function TOBDOEMExtensionPACCAR.ManufacturerKey: string;
begin Result := 'PACCAR'; end;

function TOBDOEMExtensionPACCAR.DisplayName: string;
begin Result := 'PACCAR Inc. (Peterbilt / Kenworth / DAF / Leyland)'; end;

function TOBDOEMExtensionPACCAR.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Peterbilt: 1XP (Denton TX), 1NP (Madison TN), 5KJ (Mexico).
  // Kenworth: 1NK (Chillicothe OH), 1XK (Renton WA), 2NK (Canada).
  // DAF (Eindhoven): XLR.
  // Leyland Trucks (UK, PACCAR-owned): SCB.
  Result :=
    (WMI = '1XP') or (WMI = '1NP') or (WMI = '5KJ') or
    (WMI = '1NK') or (WMI = '1XK') or (WMI = '2NK') or
    (WMI = 'XLR') or (WMI = 'SCB');
end;

procedure TOBDOEMExtensionPACCAR.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'engine',         'PACCAR MX engine ECM (or Cummins ECM)'),
    ECU(J1939_ADDR_TRANSMISSION_1,   'transmission',   'Eaton / Allison / TraXon transmission'),
    ECU(J1939_ADDR_BRAKES_SYSTEM,    'abs',            'Bendix EC-80 / Wabco brakes'),
    ECU(J1939_ADDR_INSTRUMENT_CLUSTER,'cluster',       'Driver Information Cluster'),
    ECU(J1939_ADDR_CAB_PRIMARY,      'cab',            'Cab Controller — Primary'),
    ECU(J1939_ADDR_BODY_PRIMARY,     'body',           'Body Controller — Primary'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'atd1',           'Aftertreatment Control 1 (DPF)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'PACCAR / DAF service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'paccar_chassis_code',       'PACCAR chassis code (e.g. 579, 567, T880)'),
    DID($F1A2, 'paccar_factory_code',       'PACCAR factory code (Denton / Madison / Eindhoven)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('paccar.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionPACCAR.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionPACCAR.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionPACCAR.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionPACCAR.DtcCatalogFileName: string;
begin Result := 'dtc-paccar.json'; end;

function TOBDOEMExtensionPACCAR.DecodeDID(const DID: Word;
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
                    $F1A0: 'paccar_chassis_code';
                    $F1A2: 'paccar_factory_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionPACCAR.Create);

end.
