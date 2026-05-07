//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Scania.pas
// CONTENTS       : Scania heavy-duty OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Scania (now part of Traton, the VW Group truck-
//                  holdings sub-brand alongside MAN). Dealer
//                  toolchain is SDP3 (Scania Diagnostic Programmer
//                  3) plus XCom for ECU programming. The catalog
//                  targets the modern S-, R-, P-, G-, and L-series
//                  with the SCR-only (no EGR) DC09 / DC13 / DC16
//                  engines.
//------------------------------------------------------------------------------
unit OBD.OEM.Scania;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionScania = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionScania.ManufacturerKey: string;
begin Result := 'SCANIA'; end;

function TOBDOEMExtensionScania.DisplayName: string;
begin Result := 'Scania AB (Traton Group)'; end;

function TOBDOEMExtensionScania.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Scania Sweden: VLU (Södertälje), YS2 (Södertälje passenger /
  //   coach), XLE (NL Zwolle export). Scania Brazil (São Bernardo
  //   do Campo): 9BS. Scania South America: 9BS.
  Result :=
    (WMI = 'VLU') or (WMI = 'YS2') or (WMI = 'XLE') or (WMI = '9BS');
end;

procedure TOBDOEMExtensionScania.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'ems',            'EMS — DC09 / DC13 / DC16 engine controller'),
    ECU(J1939_ADDR_TRANSMISSION_1,   'opc',            'OPC — Opticruise transmission'),
    ECU(J1939_ADDR_BRAKES_SYSTEM,    'ebs',            'EBS — Electronic Braking System'),
    ECU(J1939_ADDR_RETARDER_ENGINE,  'retarder',       'Scania retarder controller'),
    ECU(J1939_ADDR_INSTRUMENT_CLUSTER,'icl',            'ICL — Instrument Cluster'),
    ECU(J1939_ADDR_CAB_PRIMARY,      'coo',            'COO — Coordinator (vehicle gateway)'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'acm',            'ACM — Aftertreatment Control Module'),
    ECU(J1939_ADDR_FORWARD_RADAR,    'awd',            'AWD — Adaptive cruise / forward radar')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Scania part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'scania_chassis_number',     'Scania chassis number'),
    DID($F1A2, 'scania_specification_code', 'Scania specification / option code'),
    DID($F1A4, 'scania_engine_serial',      'Scania engine serial number')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration (SDP3)'),
    Routine($0205, 'opticruise_calibration','Opticruise clutch / gear-position calibration'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('scania.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionScania.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionScania.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionScania.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionScania.DtcCatalogFileName: string;
begin Result := 'dtc-scania.json'; end;

function TOBDOEMExtensionScania.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'scania_chassis_number';
                    $F1A2: 'scania_specification_code';
                    $F1A4: 'scania_engine_serial';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionScania.Create);

end.
