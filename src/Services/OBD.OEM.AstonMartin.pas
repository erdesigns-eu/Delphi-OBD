//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.AstonMartin.pas
// CONTENTS       : Aston Martin Lagonda OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Aston Martin Lagonda Ltd. — Gaydon (sports cars,
//                  Vantage / DB12 / Vanquish) + St Athan (DBX SUV) +
//                  Gaydon FAB-1 (Valhalla mid-engine PHEV). The AMR
//                  diagnostic stack uses Bosch MED + Continental
//                  Mercedes-AMG-derived ECUs (M177 V8 twin-turbo
//                  shared with AMG; V12 N/A is the in-house unit).
//                  Catalog covers DB11 through Vanquish + DBX +
//                  Valhalla.
//------------------------------------------------------------------------------
unit OBD.OEM.AstonMartin;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionAstonMartin = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionAstonMartin.ManufacturerKey: string;
begin Result := 'ASTON_MARTIN'; end;

function TOBDOEMExtensionAstonMartin.DisplayName: string;
begin Result := 'Aston Martin Lagonda Ltd.'; end;

function TOBDOEMExtensionAstonMartin.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in aston-martin.json.
  Result := VINMatchesCatalog('aston-martin.json', VIN);
end;
procedure TOBDOEMExtensionAstonMartin.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are aston-martin.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('aston-martin.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionAstonMartin.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('aston-martin.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
procedure TOBDOEMExtensionAstonMartin.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionAstonMartin.DtcCatalogFileName: string;
begin Result := 'dtc-aston-martin.json'; end;

function TOBDOEMExtensionAstonMartin.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4, $F1A6, $F1A8:
      if Length(Payload) > 0 then
      begin
        Result := Format('aml_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionAstonMartin.Create);

end.
