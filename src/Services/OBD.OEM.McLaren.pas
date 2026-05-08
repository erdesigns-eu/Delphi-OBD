//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.McLaren.pas
// CONTENTS       : McLaren Automotive OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : McLaren Automotive Ltd. — McLaren Production Centre
//                  (MPC), Woking, England. Mid-engine carbon-tub
//                  construction (Monocell / MonoCell II / II-J /
//                  III) shared across the line. Engines are the
//                  Ricardo-built M838T / M840T / M840TE 4.0 V8
//                  twin-turbo (720S / 750S / 765LT / GT) and the new
//                  M630 3.0 V6 hybrid (Artura / 750S derivatives).
//                  Transmissions are the Graziano 7-DCT.
//------------------------------------------------------------------------------
unit OBD.OEM.McLaren;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionMcLaren = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionMcLaren.ManufacturerKey: string;
begin Result := 'MCLAREN'; end;

function TOBDOEMExtensionMcLaren.DisplayName: string;
begin Result := 'McLaren Automotive Ltd.'; end;

function TOBDOEMExtensionMcLaren.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in mclaren.json.
  Result := VINMatchesCatalog('mclaren.json', VIN);
end;
procedure TOBDOEMExtensionMcLaren.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are mclaren.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('mclaren.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionMcLaren.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('mclaren.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
procedure TOBDOEMExtensionMcLaren.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMcLaren.DtcCatalogFileName: string;
begin Result := 'dtc-mclaren.json'; end;

function TOBDOEMExtensionMcLaren.DecodeDID(const DID: Word;
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
        Result := Format('mcl_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMcLaren.Create);

end.
