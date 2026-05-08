//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Renault.pas
// CONTENTS       : Renault Group OEM extension
//                  (Renault, Dacia, Alpine, Renault Korea)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : CLIP / CAN Clip is the Renault dealer tool.
//                  Renault Group is the Renault-Nissan-Mitsubishi
//                  alliance member; the Renault-side ECUs sit on
//                  the standard ISO 15765-4 powertrain range plus
//                  CLIP-specific 0x76x body modules.
//------------------------------------------------------------------------------
unit OBD.OEM.Renault;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionRenault = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionRenault.ManufacturerKey: string;
begin Result := 'RNLT'; end;

function TOBDOEMExtensionRenault.DisplayName: string;
begin Result := 'Renault Group (Renault / Dacia / Alpine)'; end;

function TOBDOEMExtensionRenault.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Renault: VF1 (Boulogne-Billancourt passenger), VF2 (commercial),
  //   VS5 (Valladolid Spain), VR1 (Tangier Morocco), 3W2 (Mexico).
  // Dacia is delegated to OBD.OEM.Dacia (UU1 / UU3 / LBR / LRY).
  // Alpine (sports brand): VFA, VFD (Dieppe).
  // Renault Korea (formerly Renault Samsung Motors): KNM.
  Result :=
    // Renault SA
    (WMI = 'VF1') or (WMI = 'VF2') or (WMI = 'VS5') or
    (WMI = 'VR1') or (WMI = '3W2') or
    // Alpine
    (WMI = 'VFA') or (WMI = 'VFD') or
    // Renault Korea
    (WMI = 'KNM');
end;

procedure TOBDOEMExtensionRenault.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // CLIP ECU map. Renault uses 0x76x for body / cluster modules
  // (UCH, BCM, instrument cluster) plus the standard 0x7E0/0x7E1
  // for powertrain.
  ECUs := [
    ECU($7E0, 'engine',         'EMS — Engine Management'),
    ECU($7E1, 'transmission',   'TCU — Transmission Control'),
    ECU($760, 'uch',            'UCH — Unité Centrale Habitacle (BCM)'),
    ECU($762, 'cluster',        'Instrument Cluster (TdB)'),
    ECU($763, 'abs',            'ABS / ESP'),
    ECU($765, 'srs',            'SRS / Airbag'),
    ECU($768, 'climate',        'Climate Control'),
    ECU($772, 'pas',            'PAS — Power-Assisted Steering'),
    ECU($785, 'evcc',           'EV Charge Controller (Zoe / Megane E-Tech)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Renault service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'renault_calibration_id',    'Renault calibration ID'),
    DID($F1A2, 'renault_market_code',       'Renault market / fleet code'),
    DID($F1B0, 'renault_options_block',     'Renault factory options block')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0205, 'tps_relearn',           'Throttle-pedal learning (CLIP)'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F01, 'auto_clutch_relearn',   'AMT clutch-touchpoint relearn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('renault.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionRenault.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  MASK: array[0..3] of Byte = ($52, $4E, $4C, $54); // 'RNLT'
var
  M: TBytes;
begin
  // Pre-2014 Renault EMS modules accepted a 4-byte XOR mask at
  // Level 1 (CLIP community documentation). Modern CLIP uses
  // a proprietary algorithm — replace at startup.
  SetLength(M, Length(MASK));
  Move(MASK[0], M[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(M,
    'Renault CLIP community XOR-mask placeholder', 'community-pr', False));
end;

procedure TOBDOEMExtensionRenault.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionRenault.DtcCatalogFileName: string;
begin Result := 'dtc-renault.json'; end;

function TOBDOEMExtensionRenault.DecodeDID(const DID: Word;
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
                    $F1A0: 'renault_calibration_id';
                    $F1A2: 'renault_market_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionRenault.Create);

end.
