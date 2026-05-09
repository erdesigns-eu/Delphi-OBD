//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Lucid.pas
// CONTENTS       : Lucid Group OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Lucid Group (Casa Grande, Arizona — AMP-1 plant)
//                  builds the Air sedan and Gravity SUV. The
//                  catalog targets the dual- / tri-motor Air and
//                  the upcoming Gravity. Lucid runs proprietary
//                  cloud-connected diagnostics; the catalog
//                  surfaces what's reachable over standard
//                  ISO 14229 UDS-on-CAN.
//------------------------------------------------------------------------------
unit OBD.OEM.Lucid;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionLucid = class(TOBDOEMExtensionBase)
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
function TOBDOEMExtensionLucid.ManufacturerKey: string;
begin
  Result := 'LUCID';
end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionLucid.DisplayName: string;
begin
  Result := 'Lucid Group, Inc.';
end;

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TOBDOEMExtensionLucid.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in lucid.json.
  Result := VINMatchesCatalog('lucid.json', VIN);
end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionLucid.BuildCatalog(
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are lucid.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('lucid.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


//------------------------------------------------------------------------------
// BUILD EXTENDED CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionLucid.BuildExtendedCatalog(
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
  MergeExtendedCatalogJSON('lucid.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionLucid.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

//------------------------------------------------------------------------------
// DTC CATALOG FILE NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionLucid.DtcCatalogFileName: string;
begin
  Result := 'dtc-lucid.json';
end;

//------------------------------------------------------------------------------
// DECODE DID
//------------------------------------------------------------------------------
function TOBDOEMExtensionLucid.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4, $F1A6:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'lucid_model_code';
                    $F1A2: 'lucid_drivetrain';
                    $F1A4: 'lucid_battery_pack';
                    $F1A6: 'lucid_software_release';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionLucid.Create);

end.
