//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Cummins.pas
// CONTENTS       : Cummins engine-OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Cummins is an engine manufacturer — it doesn't
//                  ship a vehicle, so VIN routing always returns
//                  False. The extension is resolved by name via
//                  TOBDOEMRegistry.FindByKey('CUMMINS') once the
//                  caller has identified the engine OEM through
//                  the J1939 component-identification DID
//                  (PGN 65259 / SPN 586) or via INSITE on a
//                  PACCAR / Volvo Trucks / Freightliner / RAM HD
//                  chassis. Coverage targets the X15 / L9 / B6.7 /
//                  ISX15 / ISL9 ranges.
//------------------------------------------------------------------------------
unit OBD.OEM.Cummins;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionCummins = class(TOBDOEMExtensionBase)
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
    function ApplicableToECUSupplier(const SupplierID: string): Boolean; override;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers, OBD.OEM.Catalog.Loader, OBD.OEM.DTC.Loader;

function TOBDOEMExtensionCummins.ManufacturerKey: string;
begin Result := 'CUMMINS'; end;

function TOBDOEMExtensionCummins.DisplayName: string;
begin Result := 'Cummins Inc. (engine OEM)'; end;

function TOBDOEMExtensionCummins.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in cummins.json.
  Result := VINMatchesCatalog('cummins.json', VIN);
end;
function TOBDOEMExtensionCummins.ApplicableToECUSupplier(
  const SupplierID: string): Boolean;
var
  Norm: string;
begin
  // J1939 PGN 65259 'Make' returns 'CUMMINS' for the ECM. Some
  // older Cummins ECMs respond with 'CMI' (Cummins Inc) on F18A;
  // accept both. Match is case-insensitive and ignores whitespace.
  Norm := UpperCase(Trim(SupplierID));
  Result := (Norm = 'CUMMINS') or (Norm = 'CMI');
end;

procedure TOBDOEMExtensionCummins.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are cummins.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('cummins.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionCummins.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('cummins.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
function TOBDOEMExtensionCummins.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDHDSessionNegotiator.Create; end;

procedure TOBDOEMExtensionCummins.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-Tier-4 ISC / ISX modules accept the ISO 14229 textbook
  // two's-complement at Level 1 (verified against community INSITE
  // captures). Modern ECMs use a proprietary 8-byte seed-key —
  // replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionCummins.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionCummins.DtcCatalogFileName: string;
begin Result := 'dtc-cummins.json'; end;

function TOBDOEMExtensionCummins.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F1A0:
      if Length(Payload) > 0 then
      begin
        Result := Format('cummins_engine_serial = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A1:
      if Length(Payload) > 0 then
      begin
        Result := Format('cummins_calibration_id = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionCummins.Create);

end.
