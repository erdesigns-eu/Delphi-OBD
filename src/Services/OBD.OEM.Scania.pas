//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Scania.pas
// CONTENTS       : Scania heavy-duty OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Scania (now part of Traton, the VW Group truck-
//                  holdings sub-brand alongside MAN). Dealer
//                  toolchain is SDP3 (Scania Diagnostic Programmer
//                  3) plus XCom for ECU programming. The catalog
//                  targets the modern S-, R-, P-, G-, and L-series
//                  with the SCR-only (no EGR) DC09 / DC13 / DC16
//                  engines.
//------------------------------------------------------------------------------
unit OBD.OEM.Scania;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC,
  OBD.OEM.HD;

type
  TOBDOEMExtensionScania = class(TOBDOEMExtensionBase)
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

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMExtensionScania.ManufacturerKey: string;
begin
  Result := 'SCANIA';
end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionScania.DisplayName: string;
begin
  Result := 'Scania AB (Traton Group)';
end;

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TOBDOEMExtensionScania.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in scania.json.
  Result := VINMatchesCatalog('scania.json', VIN);
end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionScania.BuildCatalog(
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are scania.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('scania.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


//------------------------------------------------------------------------------
// BUILD EXTENDED CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionScania.BuildExtendedCatalog(
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
  MergeExtendedCatalogJSON('scania.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

//------------------------------------------------------------------------------
// CREATE SESSION NEGOTIATOR
//------------------------------------------------------------------------------
function TOBDOEMExtensionScania.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDHDSessionNegotiator.Create;
end;

//------------------------------------------------------------------------------
// SEED DEFAULT SEED KEY ALGORITHMS
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionScania.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionScania.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

//------------------------------------------------------------------------------
// DTC CATALOG FILE NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionScania.DtcCatalogFileName: string;
begin
  Result := 'dtc-scania.json';
end;

//------------------------------------------------------------------------------
// DECODE DID
//------------------------------------------------------------------------------
function TOBDOEMExtensionScania.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1A4:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = "%s"',
          [string(case DID of
                    $F1A0: 'scania_chassis_number';
                    $F1A2: 'scania_specification_code';
                    $F1A4: 'scania_engine_serial';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionScania.Create);

end.
