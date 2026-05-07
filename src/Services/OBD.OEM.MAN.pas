//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.MAN.pas
// CONTENTS       : MAN Truck & Bus heavy-duty OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : MAN Truck & Bus SE (Munich + Salzgitter,
//                  Germany) is the truck-and-bus brand of the VW
//                  Group's Traton subsidiary alongside Scania.
//                  Dealer toolchain is MAN-cats II / MANTS.
//                  The catalog targets the TGX / TGS / TGM / TGL
//                  range with the D2676 / D3876 / D26 / D08
//                  engines.
//------------------------------------------------------------------------------
unit OBD.OEM.MAN;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionMAN = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionMAN.ManufacturerKey: string;
begin Result := 'MAN'; end;

function TOBDOEMExtensionMAN.DisplayName: string;
begin Result := 'MAN Truck & Bus SE (Traton Group)'; end;

function TOBDOEMExtensionMAN.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // MAN Munich + Salzgitter + Steyr (AT): WMA. Latin America (now-
  // defunct VW Truck & Bus / MAN Latin America merger): 9BW (Brazil).
  Result :=
    (WMI = 'WMA') or (WMI = '9BW');
end;

procedure TOBDOEMExtensionMAN.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'edc',            'EDC — Engine Control (D26 / D08)'),
    ECU(J1939_ADDR_TRANSMISSION_1,   'tipmatic',       'TipMatic / TraXon transmission control'),
    ECU(J1939_ADDR_BRAKES_SYSTEM,    'ebs',            'EBS — Electronic Braking'),
    ECU(J1939_ADDR_RETARDER_ENGINE,  'retarder',       'MAN PriTarder retarder controller'),
    ECU(J1939_ADDR_INSTRUMENT_CLUSTER,'zbr',           'ZBR — Central Vehicle Computer (cluster)'),
    ECU(J1939_ADDR_CAB_PRIMARY,      'fhrr',           'FHRR — Driver assist controller'),
    ECU(J1939_ADDR_BODY_PRIMARY,     'bws',            'BWS — Body computer'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'acm',            'ACM — Aftertreatment Control')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'MAN service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'man_chassis_code',          'MAN chassis code (e.g. TGX, TGS, TGE)'),
    DID($F1A2, 'man_engine_serial',         'MAN engine serial number'),
    DID($F1B0, 'man_factory_options',       'MAN factory option block')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration (MAN-cats)'),
    Routine($0205, 'tipmatic_calibration',  'TipMatic clutch / gear-position calibration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('man.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionMAN.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionMAN.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionMAN.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMAN.DtcCatalogFileName: string;
begin Result := 'dtc-man.json'; end;

function TOBDOEMExtensionMAN.DecodeDID(const DID: Word;
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
                    $F1A0: 'man_chassis_code';
                    $F1A2: 'man_engine_serial';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMAN.Create);

end.
