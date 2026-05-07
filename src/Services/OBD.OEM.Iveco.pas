//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Iveco.pas
// CONTENTS       : Iveco S.p.A. heavy-commercial OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Iveco (Italian commercial-vehicle maker, now
//                  part of Iveco Group following the CNH Industrial
//                  spin-off) covers the Daily van + Eurocargo
//                  light truck + S-Way / T-Way heavy truck +
//                  Iveco Bus ranges. Dealer toolchain is Iveco
//                  EASY (Electronic Application System).
//------------------------------------------------------------------------------
unit OBD.OEM.Iveco;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionIveco = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionIveco.ManufacturerKey: string;
begin Result := 'IVECO'; end;

function TOBDOEMExtensionIveco.DisplayName: string;
begin Result := 'Iveco S.p.A. (Iveco Group)'; end;

function TOBDOEMExtensionIveco.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Iveco Italy (Suzzara / Brescia / Turin / Foggia): ZCF.
  // Iveco Spain (Valladolid Daily): VCF.
  // Iveco Latin America (Sete Lagoas Brazil): 9BD share — but that
  // collides with Stellantis-FCA Brazil. Skip.
  Result := (WMI = 'ZCF') or (WMI = 'VCF');
end;

procedure TOBDOEMExtensionIveco.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'fpt_engine',    'FPT Engine ECU (Cursor / NEF / S-FE)'),
    ECU(J1939_ADDR_TRANSMISSION_1,   'eurotronic',    'EuroTronic / HI-TRONIX AMT'),
    ECU(J1939_ADDR_BRAKES_SYSTEM,    'ebs',           'EBS — Knorr-Bremse'),
    ECU(J1939_ADDR_INSTRUMENT_CLUSTER,'cluster',      'Driver Information Display'),
    ECU(J1939_ADDR_CAB_PRIMARY,      'vcm',           'VCM — Vehicle Control Module'),
    ECU(J1939_ADDR_BODY_PRIMARY,     'bcm',           'BCM — Body Computer'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'acm',           'ACM — Aftertreatment Control'),
    ECU($7E5, 'evcc',           'eDaily EV Charge Controller')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Iveco service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'iveco_model_code',          'Iveco model code (e.g. Daily, S-Way)'),
    DID($F1A2, 'iveco_emissions_pkg',       'Euro 6e / OBD-VI emissions package code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration (EASY)'),
    Routine($0205, 'eurotronic_calibration','EuroTronic / HI-TRONIX clutch calibration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('iveco.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionIveco.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionIveco.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionIveco.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionIveco.DtcCatalogFileName: string;
begin Result := 'dtc-iveco.json'; end;

function TOBDOEMExtensionIveco.DecodeDID(const DID: Word;
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
                    $F1A0: 'iveco_model_code';
                    $F1A2: 'iveco_emissions_pkg';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionIveco.Create);

end.
