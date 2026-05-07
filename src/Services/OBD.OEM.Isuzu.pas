//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Isuzu.pas
// CONTENTS       : Isuzu Motors OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Isuzu Motors (Japan) — light commercial (D-Max
//                  pickup, Mu-X SUV) + medium-duty trucks (NPR /
//                  NQR / FRR / FTR / FVR Series) + heavy-duty
//                  Forward / Giga. Dealer toolchain is IDSS-II
//                  (Isuzu Diagnostic Service System II).
//------------------------------------------------------------------------------
unit OBD.OEM.Isuzu;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionIsuzu = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionIsuzu.ManufacturerKey: string;
begin Result := 'ISUZU'; end;

function TOBDOEMExtensionIsuzu.DisplayName: string;
begin Result := 'Isuzu Motors Ltd.'; end;

function TOBDOEMExtensionIsuzu.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Isuzu Japan Fujisawa (D-Max / Mu-X / Trooper): JAA, JAB.
  // Isuzu commercial truck Japan: JAL, JAN, JAH (Forward / Giga).
  // Isuzu USA (commercial truck Charlotte MI): 5RY, 4GD.
  // Note: WMI 'JAC' was historically used by Isuzu but JAC Motors
  // (separate Chinese OEM) also uses similar prefixes — not claimed
  // here to avoid the conflict.
  Result :=
    (WMI = 'JAA') or (WMI = 'JAB') or (WMI = 'JAL') or (WMI = 'JAN') or
    (WMI = 'JAH') or (WMI = '5RY') or (WMI = '4GD');
end;

procedure TOBDOEMExtensionIsuzu.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU(J1939_ADDR_ENGINE_1,         'engine',    'Isuzu engine ECM (4HK1 / 6HK1 / 6WG1 / RZ4E)'),
    ECU(J1939_ADDR_TRANSMISSION_1,   'transmission','Aisin / MZW / Smoother automated transmission'),
    ECU(J1939_ADDR_BRAKES_SYSTEM,    'abs',       'ABS / ESC'),
    ECU(J1939_ADDR_INSTRUMENT_CLUSTER,'cluster',  'Combination Meter'),
    ECU(J1939_ADDR_CAB_PRIMARY,      'cab',       'Cab Body Computer'),
    ECU(J1939_ADDR_AFTERTREATMENT_1, 'atd',       'Aftertreatment (DPF + SCR)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Isuzu service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'isuzu_model_code',          'Isuzu chassis code (e.g. RT85, NPR)'),
    DID($F1A2, 'isuzu_engine_code',         'Isuzu engine code (e.g. 4HK1, 6HK1, RZ4E)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'forced_dpf_regen',      'Forced parked DPF regeneration (IDSS-II)'),
    Routine($0205, 'idle_relearn',          'Idle re-learn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('isuzu.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionIsuzu.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionIsuzu.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionIsuzu.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionIsuzu.DtcCatalogFileName: string;
begin Result := 'dtc-isuzu.json'; end;

function TOBDOEMExtensionIsuzu.DecodeDID(const DID: Word;
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
                    $F1A0: 'isuzu_model_code';
                    $F1A2: 'isuzu_engine_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionIsuzu.Create);

end.
