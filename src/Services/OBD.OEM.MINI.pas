//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.MINI.pas
// CONTENTS       : MINI (BMW Group sub-brand) OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : MINI is a BMW Group sub-brand but uses its own
//                  WMIs (WMW Oxford UK + SAW China BMW Brilliance).
//                  Architecture overlaps with BMW E-Sys / ISTA but
//                  the model codes (R56, F56, F60, J01, J05) differ.
//                  Inherits the BMW session-negotiation lineage
//                  (1500 ms heartbeat, security access required for
//                  coding writes).
//------------------------------------------------------------------------------
unit OBD.OEM.MINI;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  /// <summary>MINI uses BMW E-Sys / ISTA, so the same negotiator
  /// applies (security access required for both extended +
  /// programming sessions; 1500 ms heartbeat for older R-series
  /// DMEs).</summary>
  TOBDMINISessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean; override;
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionMINI = class(TOBDOEMExtensionBase)
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

function TOBDMINISessionNegotiator.RequiresSecurityAccess(
  SessionType: TOBDSessionType): Boolean;
begin
  Result := SessionType in [sstExtendedDiagnostic, sstProgramming,
                            sstOEMSpecific1, sstOEMSpecific2];
end;

function TOBDMINISessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin Result := 1500; end;

function TOBDMINISessionNegotiator.DisplayName: string;
begin Result := 'MINI (BMW E-Sys / ISTA)'; end;

function TOBDOEMExtensionMINI.ManufacturerKey: string;
begin Result := 'MINI'; end;

function TOBDOEMExtensionMINI.DisplayName: string;
begin Result := 'MINI (BMW Group sub-brand)'; end;

function TOBDOEMExtensionMINI.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in mini.json.
  Result := VINMatchesCatalog('mini.json', VIN);
end;
procedure TOBDOEMExtensionMINI.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are mini.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('mini.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionMINI.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('mini.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
function TOBDOEMExtensionMINI.CreateSessionNegotiator: IOBDSessionNegotiator;
begin Result := TOBDMINISessionNegotiator.Create; end;

procedure TOBDOEMExtensionMINI.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  PUBLIC_MASK: array[0..3] of Byte = ($A5, $5A, $C3, $3C);
var
  Mask: TBytes;
begin
  // MINI inherits BMW E-Sys lineage; same XOR-mask placeholder
  // applies until the production NDA algorithm is registered.
  SetLength(Mask, Length(PUBLIC_MASK));
  Move(PUBLIC_MASK[0], Mask[0], Length(PUBLIC_MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(Mask,
    'MINI (BMW E-Sys lineage) XOR-mask placeholder', 'community-pr', False));
end;

procedure TOBDOEMExtensionMINI.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMINI.DtcCatalogFileName: string;
begin Result := 'dtc-mini.json'; end;

function TOBDOEMExtensionMINI.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A4:
      if Length(Payload) > 0 then
      begin
        Result := Format('mini_chassis_code = "%s"',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMINI.Create);

end.
