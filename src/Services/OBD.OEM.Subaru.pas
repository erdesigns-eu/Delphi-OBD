//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Subaru.pas
// CONTENTS       : Subaru OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : SSM4 (Subaru Select Monitor 4) is the dealer
//                  tool. Powertrain ECUs use the standard ISO 15765-4
//                  range; AWD-specific modules (AT/AWD controller,
//                  Active Torque Vectoring) live in 0x7Ex.
//------------------------------------------------------------------------------
unit OBD.OEM.Subaru;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionSubaru = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionSubaru.ManufacturerKey: string;
begin Result := 'SUBARU'; end;

function TOBDOEMExtensionSubaru.DisplayName: string;
begin Result := 'Subaru Corporation'; end;

function TOBDOEMExtensionSubaru.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in subaru.json.
  Result := VINMatchesCatalog('subaru.json', VIN);
end;
procedure TOBDOEMExtensionSubaru.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are subaru.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('subaru.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionSubaru.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('subaru.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
procedure TOBDOEMExtensionSubaru.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  MASK: array[0..1] of Byte = ($53, $42); // 'SB'
var
  Mask: TBytes;
begin
  // Pre-2012 ECUs documented in the OpenECU community ship a 2-byte
  // byte-rotate algorithm at Level 1. Replace at startup.
  SetLength(Mask, Length(MASK));
  Move(MASK[0], Mask[0], Length(MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyByteRotate.Create(
    1, 4, Mask, 'Subaru SSM community byte-rotate placeholder',
    'community-pr', False));
end;

procedure TOBDOEMExtensionSubaru.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionSubaru.DtcCatalogFileName: string;
begin Result := 'dtc-subaru.json'; end;

function TOBDOEMExtensionSubaru.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0:
      if Length(Payload) > 0 then
      begin
        Result := Format('subaru_chassis_code = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionSubaru.Create);

end.
