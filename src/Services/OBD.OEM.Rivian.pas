//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Rivian.pas
// CONTENTS       : Rivian Automotive OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Rivian (Normal IL plant) builds the R1T pickup,
//                  R1S SUV, and EDV electric delivery van for
//                  Amazon. The catalog targets the consumer R1
//                  range; no public service docs exist (Rivian
//                  pushes service via the Service Toolbox cloud
//                  app), so the entries here are derived from the
//                  community reverse-engineering effort.
//------------------------------------------------------------------------------
unit OBD.OEM.Rivian;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionRivian = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
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

function TOBDOEMExtensionRivian.ManufacturerKey: string;
begin Result := 'RIVIAN'; end;

function TOBDOEMExtensionRivian.DisplayName: string;
begin Result := 'Rivian Automotive, Inc.'; end;

function TOBDOEMExtensionRivian.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Rivian Normal IL plant (R1T / R1S / EDV): 7PD.
  Result := (WMI = '7PD');
end;

procedure TOBDOEMExtensionRivian.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // Quad-motor R1T / R1S has four independent inverters at
  // 0x710-0x713 (one per wheel). Tri-motor variants use 0x710 +
  // 0x712 (front) + 0x714 (single rear-drive unit).
  ECUs := [
    ECU($7E0, 'vcu',          'VCU — Vehicle Control Unit'),
    ECU($710, 'motor_fl',     'Front-Left Motor Inverter'),
    ECU($711, 'motor_fr',     'Front-Right Motor Inverter'),
    ECU($712, 'motor_rl',     'Rear-Left Motor Inverter'),
    ECU($713, 'motor_rr',     'Rear-Right Motor Inverter'),
    ECU($782, 'bms',          'BMS — Pack (Standard / Large / Max)'),
    ECU($792, 'charge',       'Charge Port + on-board AC/DC charger'),
    ECU($720, 'cluster',      'Driver Display Cluster'),
    ECU($770, 'central',      'Central Gateway / IVI'),
    ECU($724, 'driver_plus',  'Driver+ ADAS computer')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'rivian_model_code',         'Rivian model code (R1T / R1S / EDV)'),
    DID($F1A2, 'rivian_drivetrain',         'Rivian drivetrain (Quad / Dual / Performance Dual)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('rivian.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionRivian.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionRivian.DtcCatalogFileName: string;
begin Result := 'dtc-rivian.json'; end;

function TOBDOEMExtensionRivian.DecodeDID(const DID: Word;
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
                    $F1A0: 'rivian_model_code';
                    $F1A2: 'rivian_drivetrain';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionRivian.Create);

end.
