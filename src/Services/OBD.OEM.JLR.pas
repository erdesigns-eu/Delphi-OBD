//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.JLR.pas
// CONTENTS       : Jaguar Land Rover OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : JLR (Tata Motors-owned since 2008) ships its
//                  own diagnostic stack (SDD / Pathfinder, more
//                  recently the Topix Cloud workshop platform).
//                  Catalog targets the Range Rover / Range Rover
//                  Sport / Velar / Evoque / Discovery / Defender /
//                  Jaguar XF / F-Pace / I-Pace / F-Type range.
//------------------------------------------------------------------------------
unit OBD.OEM.JLR;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  TOBDOEMExtensionJLR = class(TOBDOEMExtensionBase)
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
function TOBDOEMExtensionJLR.ManufacturerKey: string;
begin
  Result := 'JLR';
end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionJLR.DisplayName: string;
begin
  Result := 'Jaguar Land Rover Limited (Tata)';
end;

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TOBDOEMExtensionJLR.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in jlr.json.
  Result := VINMatchesCatalog('jlr.json', VIN);
end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionJLR.BuildCatalog(
  var
    DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are jlr.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('jlr.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


//------------------------------------------------------------------------------
// BUILD EXTENDED CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionJLR.BuildExtendedCatalog(
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
  MergeExtendedCatalogJSON('jlr.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

//------------------------------------------------------------------------------
// SEED DEFAULT SEED KEY ALGORITHMS
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionJLR.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionJLR.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

//------------------------------------------------------------------------------
// DTC CATALOG FILE NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionJLR.DtcCatalogFileName: string;
begin
  Result := 'dtc-jlr.json';
end;

//------------------------------------------------------------------------------
// DECODE DID
//------------------------------------------------------------------------------
function TOBDOEMExtensionJLR.DecodeDID(const DID: Word;
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
                    $F1A0: 'jlr_model_code';
                    $F1A2: 'jlr_assembly_plant';
                  else  'unknown'; end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionJLR.Create);

end.
