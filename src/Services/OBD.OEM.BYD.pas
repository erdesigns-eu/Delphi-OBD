//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.BYD.pas
// CONTENTS       : BYD Auto OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : BYD ('Build Your Dreams') is the largest plug-in
//                  EV maker in the world; the catalog targets the
//                  e-Platform 3.0 / Han / Tang / Atto 3 range with
//                  Blade-battery diagnostics. Dealer toolchain is
//                  proprietary BYD Star Diagnostic; the catalog
//                  surfaces what's reachable over standard ISO
//                  14229 UDS-on-CAN (most BYD models support both
//                  the proprietary protocol and ISO 15765-4 OBD-II
//                  for emissions / EV state-of-charge reads).
//------------------------------------------------------------------------------
unit OBD.OEM.BYD;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionBYD = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionBYD.ManufacturerKey: string;
begin Result := 'BYD'; end;

function TOBDOEMExtensionBYD.DisplayName: string;
begin Result := 'BYD Auto Co. Ltd.'; end;

function TOBDOEMExtensionBYD.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // BYD passenger (Xi'an / Shenzhen): L6T.
  // BYD Auto (Changsha plant): LGX.
  // BYD Brazil (Camaçari assembly): 8GA.
  Result :=
    (WMI = 'L6T') or (WMI = 'LGX') or (WMI = '8GA');
end;

procedure TOBDOEMExtensionBYD.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // BYD e-Platform 3.0 ECU map. The high-voltage stack lives at
  // 0x782 / 0x792 (BMS + charge controller), the EV motor inverter
  // at 0x710, and the cluster at 0x720.
  ECUs := [
    ECU($7E0, 'vcu',          'VCU — Vehicle Control Unit'),
    ECU($710, 'motor',        'Drive Motor Inverter'),
    ECU($720, 'cluster',      'Instrument Cluster'),
    ECU($782, 'bms',          'BMS — Blade Battery Management'),
    ECU($792, 'charge',       'Charge Port + AC/DC charger'),
    ECU($762, 'body',         'BCM — Body Control'),
    ECU($768, 'climate',      'Climate Control / Heat Pump'),
    ECU($774, 'iboost',       'iBooster — electronic brake assist'),
    ECU($724, 'dipilot',      'DiPilot — driver assistance')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'BYD service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'byd_model_code',            'BYD model code (e.g. SC1, EA1)'),
    DID($F1A2, 'byd_battery_pack_id',       'Blade battery pack ID')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('byd.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionBYD.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionBYD.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionBYD.DtcCatalogFileName: string;
begin Result := 'dtc-byd.json'; end;

function TOBDOEMExtensionBYD.DecodeDID(const DID: Word;
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
                    $F1A0: 'byd_model_code';
                    $F1A2: 'byd_battery_pack_id';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionBYD.Create);

end.
