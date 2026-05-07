//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.HyundaiKia.pas
// CONTENTS       : Hyundai / Kia / Genesis OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : GDS / KDS dealer tools target the standard
//                  ISO 15765-4 powertrain range plus Hyundai-private
//                  body / cluster / immobilizer addresses. The
//                  extension also covers Genesis (Hyundai's premium
//                  brand) since it shares the GDS toolchain.
//------------------------------------------------------------------------------
unit OBD.OEM.HyundaiKia;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  /// <summary>HMG (Hyundai Motor Group) session negotiator. GDS uses
  /// a 1500 ms tester-present interval which most pre-2018 ECUs
  /// require to keep the extended session alive.</summary>
  TOBDHyundaiKiaSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionHyundaiKia = class(TOBDOEMExtensionBase)
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

function TOBDHyundaiKiaSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin Result := 1500; end;

function TOBDHyundaiKiaSessionNegotiator.DisplayName: string;
begin Result := 'Hyundai/Kia GDS / KDS'; end;

function TOBDOEMExtensionHyundaiKia.ManufacturerKey: string;
begin Result := 'HMG'; end;

function TOBDOEMExtensionHyundaiKia.DisplayName: string;
begin Result := 'Hyundai Motor Group (Hyundai / Kia / Genesis)'; end;

function TOBDOEMExtensionHyundaiKia.ApplicableToVIN(
  const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Hyundai Korea: KMH (passenger), KM8 (SUV/CUV), KMF (Truck).
  // Hyundai US-built: 5NPE (Sonata Alabama), 5NMS (Santa Fe).
  // Kia Korea: KNA (passenger), KND (SUV), KNH (truck), KNB.
  // Kia US-built: 5XX (Optima Georgia), 5XY (Sorento), KNDJP (Soul).
  // Genesis: KMTC (Genesis G80 Alabama), KMHKR.
  Result :=
    // Hyundai
    (WMI = 'KMH') or (WMI = 'KM8') or (WMI = 'KMF') or (WMI = 'KMT') or
    (WMI = '5NP') or (WMI = '5NM') or (WMI = '5NX') or
    // Kia
    (WMI = 'KNA') or (WMI = 'KND') or (WMI = 'KNH') or (WMI = 'KNB') or
    (WMI = '5XX') or (WMI = '5XY') or (WMI = 'KNF') or
    // Genesis (Hyundai sub-brand)
    (WMI = 'KMK');
end;

procedure TOBDOEMExtensionHyundaiKia.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'Engine ECU (EMS)'),
    ECU($7E1, 'transmission',   'Transmission Control (TCU)'),
    ECU($7E2, 'ems_secondary',  'Secondary engine controller (V-engines)'),
    ECU($7D1, 'abs',            'ABS / VDC / ESC'),
    ECU($7D2, 'srs',            'SRS / Airbag'),
    ECU($780, 'cluster',        'Instrument Cluster'),
    ECU($7A0, 'bcm',            'BCM — Body Control'),
    ECU($7B0, 'icm',            'Integrated Central Module (ICM)'),
    ECU($7C0, 'hvac',           'Climate Control'),
    ECU($7E5, 'evcc',            'Electric-vehicle charge controller (EV)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'HMG service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F193, 'hmg_rom_id',                'HMG ROM ID (calibration tag)'),
    DID($F1A0, 'hmg_calibration_id',        'HMG calibration ID'),
    DID($F1B0, 'hmg_vehicle_option_code',   'HMG vehicle-option code (model trim)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'crank_relearn',         'Crankshaft position relearn'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F01, 'sjb_relearn',           'Smart Junction Box relearn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('hmg.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionHyundaiKia.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHyundaiKiaSessionNegotiator.Create; end;

procedure TOBDOEMExtensionHyundaiKia.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  PUBLIC_MASK: array[0..3] of Byte = ($48, $4D, $43, $00);  // 'HMC'
var
  Mask: TBytes;
begin
  SetLength(Mask, Length(PUBLIC_MASK));
  Move(PUBLIC_MASK[0], Mask[0], Length(PUBLIC_MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(Mask,
    'HMG community XOR-mask placeholder', 'community-pr', False));
end;

procedure TOBDOEMExtensionHyundaiKia.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionHyundaiKia.DtcCatalogFileName: string;
begin Result := 'dtc-hmg.json'; end;

function TOBDOEMExtensionHyundaiKia.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F193, $F1A0, $F1B0:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F193: 'hmg_rom_id';
                    $F1A0: 'hmg_calibration_id';
                    $F1B0: 'hmg_vehicle_option_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionHyundaiKia.Create);

end.
