//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.VolvoTrucks.pas
// CONTENTS       : Volvo Group truck OEM extension
//                  (Volvo Trucks + Mack Trucks + Renault Trucks)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Volvo Group / AB Volvo is the truck-and-bus
//                  business — separate from Volvo Cars (Geely-
//                  owned) covered in OBD.OEM.Volvo. The Group's
//                  brands share the EMS / EMC architecture used
//                  here: Volvo Trucks (Sweden + NRV/USA), Mack
//                  Trucks (Macungie/USA), Renault Trucks (Lyon),
//                  and the now-defunct UD Trucks (sold to Isuzu).
//                  The dealer toolchain is Volvo Tech Tool / Premium
//                  Tech Tool (PTT).
//------------------------------------------------------------------------------
unit OBD.OEM.VolvoTrucks;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionVolvoTrucks = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionVolvoTrucks.ManufacturerKey: string;
begin Result := 'VOLVOTR'; end;

function TOBDOEMExtensionVolvoTrucks.DisplayName: string;
begin Result := 'Volvo Group (Volvo Trucks / Mack / Renault Trucks)'; end;

function TOBDOEMExtensionVolvoTrucks.ApplicableToVIN(
  const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Volvo Trucks: 4V4 (NRV New River Valley VA), YV2 (Tuve Sweden).
  // Mack Trucks (Volvo-owned, Macungie PA): 1M1, 1M2, 4V5, 4V1.
  // Renault Trucks (Volvo-owned, Lyon Bourg-en-Bresse): VG6, VF6.
  // UD Trucks (Volvo-owned 2007-2021, now Isuzu): not claimed
  // here — historical.
  Result :=
    (WMI = '4V4') or (WMI = 'YV2') or (WMI = '4V2') or
    (WMI = '1M1') or (WMI = '1M2') or
    (WMI = '4V5') or (WMI = '4V1') or
    (WMI = 'VG6') or (WMI = 'VF6');
end;

procedure TOBDOEMExtensionVolvoTrucks.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'engine_emc',  'Volvo / Mack Engine Control (EMS / EMC)'),
    ECU(J1939_ADDR_TRANSMISSION_1,   'i_shift',     'Volvo I-Shift / Mack mDRIVE transmission'),
    ECU(J1939_ADDR_BRAKES_SYSTEM,    'abs',         'EBS — electronic braking'),
    ECU(J1939_ADDR_INSTRUMENT_CLUSTER,'cluster',    'MID — Message Information Display'),
    ECU(J1939_ADDR_CAB_PRIMARY,      'mid_140',     'MID 140 — instrumentation control'),
    ECU(J1939_ADDR_BODY_PRIMARY,     'mid_144',     'MID 144 — vehicle ECU (VECU)'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'atd',         'Aftertreatment (DPF + SCR)'),
    ECU(J1939_ADDR_TIRE_PRESSURE,    'tpms',        'Tire-pressure monitoring')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Volvo Group service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'volvo_truck_chassis_code',  'Volvo / Mack chassis code (e.g. VNL, FH16, Anthem)'),
    DID($F1A2, 'volvo_truck_emissions_pkg', 'EPA / EU emissions package code')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration (PTT)'),
    Routine($0205, 'i_shift_clutch_calibration','I-Shift / mDRIVE clutch calibration'),
    Routine($0F02, 'tpms_relearn',          'TPMS relearn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('volvotrucks.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionVolvoTrucks.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionVolvoTrucks.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionVolvoTrucks.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionVolvoTrucks.DtcCatalogFileName: string;
begin Result := 'dtc-volvotrucks.json'; end;

function TOBDOEMExtensionVolvoTrucks.DecodeDID(const DID: Word;
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
                    $F1A0: 'volvo_truck_chassis_code';
                    $F1A2: 'volvo_truck_emissions_pkg';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionVolvoTrucks.Create);

end.
