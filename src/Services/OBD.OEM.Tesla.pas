//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Tesla.pas
// CONTENTS       : Tesla OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Tesla diverges from the ISO 14229 mainstream:
//                  the diagnostic protocol used by service-mode and
//                  Toolbox is largely proprietary, with Tesla-specific
//                  CAN broadcast frames and a dealer-only HTTP API.
//                  OBD-II emissions data IS supported per CARB
//                  requirements, but most "diagnostic" reads are
//                  vendor-specific. The catalog ships the minimal
//                  set needed to identify a Tesla and read its OBD
//                  PIDs; deeper coverage requires the Tesla Toolbox
//                  SDK which is NDA-protected.
//------------------------------------------------------------------------------
unit OBD.OEM.Tesla;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionTesla = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionTesla.ManufacturerKey: string;
begin Result := 'TESLA'; end;

function TOBDOEMExtensionTesla.DisplayName: string;
begin Result := 'Tesla, Inc.'; end;

function TOBDOEMExtensionTesla.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Tesla Fremont (US): 5YJ (Models S/X/Y/3).
  // Tesla Shanghai: LRW (Model 3 / Y for China + Europe).
  // Tesla Berlin: XP7 (Model Y for Europe).
  // Tesla Austin: 7SA (Model Y / Cybertruck).
  // Some early VINs used 7SAY for the Y; collapse to 7SA prefix.
  Result :=
    (WMI = '5YJ') or (WMI = 'LRW') or
    (WMI = 'XP7') or (WMI = '7SA');
end;

procedure TOBDOEMExtensionTesla.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // Tesla CAN-bus mapping (Tesla Service Tech mode community
  // reference). The ECU inventory is much larger in reality (HV
  // controller, BMS_master + BMS_slave x 4, motor inverters,
  // gateway, autopilot…); the starter ships the most-queried set.
  ECUs := [
    ECU($7DF, 'gateway',        'OBD-II broadcast (functional)'),
    ECU($7E0, 'powertrain',     'Powertrain Controller'),
    ECU($7E1, 'gateway_p',      'Vehicle Gateway'),
    ECU($782, 'bms',            'BMS — Battery Management System'),
    ECU($724, 'autopilot',      'Autopilot ECU (FSD/HW3/HW4)'),
    ECU($762, 'driver_door',    'Driver-side body controller'),
    ECU($772, 'cabin',          'Cabin / cluster (touchscreen IHU)'),
    ECU($792, 'charging',       'Charge Port Controller')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F1A0, 'tesla_firmware_version',    'Tesla firmware version (string)'),
    DID($F1A2, 'tesla_hardware_id',         'Tesla hardware platform (HW1/2.5/3/4)'),
    DID($F197, 'system_name',               'ECU long name')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase')
  ];

  MergeCatalogJSON('tesla.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionTesla.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionTesla.DtcCatalogFileName: string;
begin Result := 'dtc-tesla.json'; end;

function TOBDOEMExtensionTesla.DecodeDID(const DID: Word;
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
                    $F1A0: 'tesla_firmware_version';
                    $F1A2: 'tesla_hardware_id';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionTesla.Create);

end.
