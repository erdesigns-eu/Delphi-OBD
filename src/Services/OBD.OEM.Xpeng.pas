//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Xpeng.pas
// CONTENTS       : XPENG (Xpeng Motors) OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Xpeng (Guangzhou + Zhaoqing) is a Chinese
//                  smart-EV OEM known for its XNGP / NGP highway
//                  autonomous-driving stack and self-developed
//                  Xpilot ADAS computer. The catalog targets the
//                  P5 / P7 / G3 / G6 / G9 / X9 range.
//------------------------------------------------------------------------------
unit OBD.OEM.Xpeng;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionXpeng = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionXpeng.ManufacturerKey: string;
begin Result := 'XPENG'; end;

function TOBDOEMExtensionXpeng.DisplayName: string;
begin Result := 'Xpeng Motors (XPeng Inc.)'; end;

function TOBDOEMExtensionXpeng.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Xpeng: LJY (Zhaoqing plant), LMZ (Guangzhou plant).
  Result := (WMI = 'LJY') or (WMI = 'LMZ');
end;

procedure TOBDOEMExtensionXpeng.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'vcu',          'VCU — Vehicle Control Unit'),
    ECU($710, 'motor_front',  'Front Motor Inverter'),
    ECU($712, 'motor_rear',   'Rear Motor Inverter'),
    ECU($782, 'bms',          'BMS — Battery Management'),
    ECU($792, 'charge',       'Charge Port + AC/DC charger'),
    ECU($720, 'cluster',      'Instrument Cluster'),
    ECU($724, 'xpilot',       'XPILOT — Xpeng ADAS computer'),
    ECU($762, 'bcm',          'BCM — Body Control'),
    ECU($768, 'climate',      'Climate Control / Heat Pump'),
    ECU($770, 'xui',          'Xmart OS — Cabin computer')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Xpeng service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'xpeng_model_code',          'Xpeng model code (e.g. P7, G6, X9)'),
    DID($F1A2, 'xpeng_xpilot_version',     'XPILOT software version')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('xpeng.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionXpeng.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionXpeng.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionXpeng.DtcCatalogFileName: string;
begin Result := 'dtc-xpeng.json'; end;

function TOBDOEMExtensionXpeng.DecodeDID(const DID: Word;
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
                    $F1A0: 'xpeng_model_code';
                    $F1A2: 'xpeng_xpilot_version';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionXpeng.Create);

end.
