//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Cummins.pas
// CONTENTS       : Cummins engine-OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Cummins is an engine manufacturer — it doesn't
//                  ship a vehicle, so VIN routing always returns
//                  False. The extension is resolved by name via
//                  TOBDOEMRegistry.FindByKey('CUMMINS') once the
//                  caller has identified the engine OEM through
//                  the J1939 component-identification DID
//                  (PGN 65259 / SPN 586) or via INSITE on a
//                  PACCAR / Volvo Trucks / Freightliner / RAM HD
//                  chassis. Coverage targets the X15 / L9 / B6.7 /
//                  ISX15 / ISL9 ranges.
//------------------------------------------------------------------------------
unit OBD.OEM.Cummins;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionCummins = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionCummins.ManufacturerKey: string;
begin Result := 'CUMMINS'; end;

function TOBDOEMExtensionCummins.DisplayName: string;
begin Result := 'Cummins Inc. (engine OEM)'; end;

function TOBDOEMExtensionCummins.ApplicableToVIN(const VIN: string): Boolean;
begin
  // Cummins doesn't issue WMIs — it ships engines into PACCAR /
  // Volvo Trucks / Freightliner / RAM HD chassis. Resolve via
  // TOBDOEMRegistry.FindByKey('CUMMINS') once the engine OEM is
  // detected from PGN 65259 or DID F18A.
  Result := False;
end;

procedure TOBDOEMExtensionCummins.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // The Cummins ECM presents itself as J1939 source 0 (Engine #1)
  // plus an aftertreatment controller at 66 / 67 for the DPF +
  // SCR boxes on Tier-4 / Euro-VI engines.
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'engine_ecm',     'Cummins ECM (X15 / L9 / B6.7)'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'atd1',           'Aftertreatment Control 1 (DPF)'),
    ECU(J1939_ADDR_AFTERTREATMENT_2, 'atd2',           'Aftertreatment Control 2 (SCR / DEF)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Cummins ESN service part number'),
    DID($F189, 'sw_version_number',         'CM ECM firmware version'),
    DID($F18A, 'system_supplier_identifier','ECU supplier ID (always Cummins for the ECM)'),
    DID($F197, 'system_name',               'Cummins ECM long name'),
    DID($F1A0, 'cummins_engine_serial',     'Cummins engine serial number (ESN)'),
    DID($F1A1, 'cummins_calibration_id',    'Cummins calibration package ID')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration (INSITE)'),
    Routine($0205, 'def_quality_test',      'DEF / urea quality test'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('cummins.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionCummins.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionCummins.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-Tier-4 ISC / ISX modules accept the ISO 14229 textbook
  // two's-complement at Level 1 (verified against community INSITE
  // captures). Modern ECMs use a proprietary 8-byte seed-key —
  // replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionCummins.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionCummins.DtcCatalogFileName: string;
begin Result := 'dtc-cummins.json'; end;

function TOBDOEMExtensionCummins.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F1A0:
      if Length(Payload) > 0 then
      begin
        Result := Format('cummins_engine_serial = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A1:
      if Length(Payload) > 0 then
      begin
        Result := Format('cummins_calibration_id = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionCummins.Create);

end.
