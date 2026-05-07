//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.DetroitDiesel.pas
// CONTENTS       : Detroit Diesel engine-OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Detroit Diesel (a Daimler Truck NA brand) is an
//                  engine OEM — appears as the ECM on Freightliner
//                  Cascadia / Western Star and select Mercedes-Benz
//                  trucks (BlueTec rebadged DD13 / DD15 / DD16).
//                  Resolve via TOBDOEMRegistry.FindByKey('DDC').
//                  Coverage targets DD13 / DD15 / DD16 with the
//                  GHG17 emissions package.
//------------------------------------------------------------------------------
unit OBD.OEM.DetroitDiesel;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionDetroitDiesel = class(TOBDOEMExtensionBase)
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
    function ApplicableToECUSupplier(const SupplierID: string): Boolean; override;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers, OBD.OEM.Catalog.Loader, OBD.OEM.DTC.Loader;

function TOBDOEMExtensionDetroitDiesel.ManufacturerKey: string;
begin Result := 'DDC'; end;

function TOBDOEMExtensionDetroitDiesel.DisplayName: string;
begin Result := 'Detroit Diesel Corp. (engine OEM)'; end;

function TOBDOEMExtensionDetroitDiesel.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  // Engine OEM — no WMI. Use ApplicableToECUSupplier instead.
  Result := False;
end;

function TOBDOEMExtensionDetroitDiesel.ApplicableToECUSupplier(
  const SupplierID: string): Boolean;
var
  Norm: string;
begin
  // J1939 PGN 65259 'Make' returns 'DETROIT' or 'DDC' on Daimler
  // Truck NA powertrain ECUs. Some older MCM-1 modules emit
  // 'DETROITDDC' as a single token. Match case-insensitively.
  Norm := UpperCase(Trim(SupplierID));
  Result := (Norm = 'DETROIT') or (Norm = 'DDC') or (Norm = 'DETROITDDC');
end;

procedure TOBDOEMExtensionDetroitDiesel.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'engine_mcm',  'Detroit MCM (DD13/DD15/DD16)'),
    ECU(J1939_ADDR_TRANSMISSION_1,   'transmission','Detroit DT12 — automated manual transmission'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'atd1',        'GHG17 Aftertreatment Control 1 (DPF)'),
    ECU(J1939_ADDR_AFTERTREATMENT_2, 'atd2',        'GHG17 Aftertreatment Control 2 (SCR)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Detroit MCM spare part'),
    DID($F189, 'sw_version_number',         'MCM firmware version'),
    DID($F18A, 'system_supplier_identifier','ECU supplier ID'),
    DID($F197, 'system_name',               'Detroit MCM long name'),
    DID($F1A0, 'detroit_engine_serial',     'Detroit engine serial number'),
    DID($F1A1, 'detroit_calibration_id',    'Detroit calibration ID'),
    DID($F1A2, 'detroit_emissions_family',  'EPA/CARB emissions family code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration (DDDL)'),
    Routine($0205, 'dt12_clutch_calibration','DT12 clutch wear calibration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('detroit.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionDetroitDiesel.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionDetroitDiesel.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionDetroitDiesel.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionDetroitDiesel.DtcCatalogFileName: string;
begin Result := 'dtc-detroit.json'; end;

function TOBDOEMExtensionDetroitDiesel.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F1A0, $F1A1, $F1A2:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'detroit_engine_serial';
                    $F1A1: 'detroit_calibration_id';
                    $F1A2: 'detroit_emissions_family';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionDetroitDiesel.Create);

end.
