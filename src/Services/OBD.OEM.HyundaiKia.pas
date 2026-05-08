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
    procedure BuildExtendedCatalog(
      var CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var Adaptations: TArray<TOBDOEMAdaptation>;
      var ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var LivePIDs: TArray<TOBDOEMLivePID>;
      var DtcExtended: TArray<TOBDDtcExtendedDataRecord>); override;
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

function TOBDOEMExtensionHyundaiKia.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in hmg.json.
  Result := VINMatchesCatalog('hmg.json', VIN);
end;
procedure TOBDOEMExtensionHyundaiKia.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are hmg.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('hmg.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionHyundaiKia.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('hmg.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
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
