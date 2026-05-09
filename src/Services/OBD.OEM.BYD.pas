//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.BYD.pas
// CONTENTS       : BYD Auto OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : BYD ('Build Your Dreams') is the largest plug-in
//                  EV maker in the world; the catalog targets the
//                  e-Platform 3.0 / Han / Tang / Atto 3 range with
//                  Blade-battery diagnostics. Dealer toolchain is
//                  proprietary BYD Star Diagnostic; the catalog
//                  surfaces what's reachable over standard ISO
//                  14229 UDS-on-CAN (most BYD models support both
//                  the proprietary protocol and ISO 15765-4 OBD-II
//                  for emissions / EV state-of-charge reads).
//------------------------------------------------------------------------------
unit OBD.OEM.BYD;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionBYD = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var
        Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure BuildExtendedCatalog(
      var
        CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var
        Adaptations: TArray<TOBDOEMAdaptation>;
      var
        ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var
        LivePIDs: TArray<TOBDOEMLivePID>;
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

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMExtensionBYD.ManufacturerKey: string;
begin
  Result := 'BYD';
end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionBYD.DisplayName: string;
begin
  Result := 'BYD Auto Co. Ltd.';
end;

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TOBDOEMExtensionBYD.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in byd.json.
  Result := VINMatchesCatalog('byd.json', VIN);
end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBYD.BuildCatalog(
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are byd.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('byd.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


//------------------------------------------------------------------------------
// BUILD EXTENDED CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBYD.BuildExtendedCatalog(
  var
    CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var
    Adaptations: TArray<TOBDOEMAdaptation>;
  var
    ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var
    LivePIDs: TArray<TOBDOEMLivePID>;
  var
    DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('byd.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

//------------------------------------------------------------------------------
// SEED DEFAULT SEED KEY ALGORITHMS
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBYD.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBYD.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

//------------------------------------------------------------------------------
// DTC CATALOG FILE NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionBYD.DtcCatalogFileName: string;
begin
  Result := 'dtc-byd.json';
end;

//------------------------------------------------------------------------------
// DECODE DID
//------------------------------------------------------------------------------
function TOBDOEMExtensionBYD.DecodeDID(const DID: Word;
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
                    $F1A0: 'byd_model_code';
                    $F1A2: 'byd_battery_pack_id';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionBYD.Create);

end.
