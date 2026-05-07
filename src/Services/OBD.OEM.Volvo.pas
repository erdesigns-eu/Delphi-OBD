//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Volvo.pas
// CONTENTS       : Volvo Cars OEM extension (Geely-owned).
//                  Note: Volvo Trucks (Volvo Group / AB Volvo) is
//                  separate and uses J1939; that's a heavy-duty
//                  extension, not covered here.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : VIDA / DiCE is the Volvo Cars dealer toolchain.
//                  Volvo CMA / SPA / SPA2 platforms (XC60, XC90,
//                  S60/V60, EX30, EX90) all share the same
//                  electronic architecture; the catalog targets
//                  that range. Heartbeat is longer (5 seconds)
//                  because VIDA's extended session keeps modules
//                  awake on the request side.
//------------------------------------------------------------------------------
unit OBD.OEM.Volvo;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  /// <summary>VIDA-style negotiator. Tester present interval is
  /// 5000 ms — longer than the ISO default — to match VIDA's
  /// extended-session heartbeat.</summary>
  TOBDVolvoSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionVolvo = class(TOBDOEMExtensionBase)
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

function TOBDVolvoSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin Result := 5000; end;

function TOBDVolvoSessionNegotiator.DisplayName: string;
begin Result := 'Volvo VIDA / DiCE'; end;

function TOBDOEMExtensionVolvo.ManufacturerKey: string;
begin Result := 'VOLVO'; end;

function TOBDOEMExtensionVolvo.DisplayName: string;
begin Result := 'Volvo Cars (Geely)'; end;

function TOBDOEMExtensionVolvo.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Volvo Sweden: YV1 (passenger sedan/wagon, Torslanda + Ghent),
  //   YV4 (SUV — XC40/60/90).
  // Volvo China: LYV (Daqing / Chengdu, Zeekr-shared), LVS, LVY.
  // Volvo South Carolina (SC USA, SPA-S60): 7JR.
  Result :=
    (WMI = 'YV1') or (WMI = 'YV4') or
    (WMI = 'LYV') or (WMI = 'LVS') or (WMI = 'LVY') or
    (WMI = '7JR');
end;

procedure TOBDOEMExtensionVolvo.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  ECUs := [
    ECU($7E0, 'engine',         'EMS — Engine Management'),
    ECU($7E1, 'transmission',   'TCM — Aisin / Geartronic'),
    ECU($740, 'cem',            'CEM — Central Electronic Module'),
    ECU($742, 'cluster',        'DIM — Driver Information Module'),
    ECU($760, 'srs',            'SRS / Airbag'),
    ECU($762, 'abs',            'ABS / DSTC'),
    ECU($765, 'pdm',            'PDM — Power Door Module'),
    ECU($768, 'climate',        'CCM — Climate Control'),
    ECU($770, 'sensus',         'Sensus / IHU — Infotainment'),
    ECU($785, 'evcc',           'EV Charge Controller (XC40/EX30/EX90)')
  ];

  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Volvo service part number'),
    DID($F189, 'sw_version_number',         'ECU software version'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($F1A0, 'volvo_struct_week',         'Volvo build week (YYWW)'),
    DID($F1A2, 'volvo_factory_code',        'Volvo factory code'),
    DID($F1B0, 'volvo_pno_code',            'Volvo PNO code (option block)')
  ];

  Routines := [
    Routine($0203, 'reset_adaptations',     'Reset adaptive learning'),
    Routine($0204, 'electronic_parking_brake_service', 'EPB service-mode toggle'),
    Routine($0F00, 'sas_calibration',       'Steering-angle sensor reset'),
    Routine($0F02, 'rdc_relearn',           'TPMS / RDC sensor relearn'),
    Routine($FF00, 'erase_memory',          'Pre-flash erase'),
    Routine($FF02, 'verify_checksum',       'Post-flash checksum verification')
  ];

  MergeCatalogJSON('volvo.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

function TOBDOEMExtensionVolvo.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDVolvoSessionNegotiator.Create; end;

procedure TOBDOEMExtensionVolvo.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionVolvo.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionVolvo.DtcCatalogFileName: string;
begin Result := 'dtc-volvo.json'; end;

function TOBDOEMExtensionVolvo.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1B0:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'volvo_struct_week';
                    $F1A2: 'volvo_factory_code';
                    $F1B0: 'volvo_pno_code';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionVolvo.Create);

end.
