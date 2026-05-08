//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.RollsRoyce.pas
// CONTENTS       : Rolls-Royce Motor Cars (BMW Group) OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Rolls-Royce Motor Cars Ltd. — Goodwood, England.
//                  BMW Group sub-brand; uses BMW E-Sys / ISTA stack
//                  with Rolls-Royce-specific extensions. Catalog
//                  covers Phantom (RR1 / 8th-gen) + Ghost (RR21) +
//                  Cullinan (RR31) + Spectre (RR23 EV) + Black Badge
//                  trims.
//------------------------------------------------------------------------------
unit OBD.OEM.RollsRoyce;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  /// <summary>Rolls-Royce inherits the BMW Group session-negotiation
  /// lineage: security access required for extended + programming;
  /// 1500 ms heartbeat for older DMEs.</summary>
  TOBDRRSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean; override;
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionRollsRoyce = class(TOBDOEMExtensionBase)
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

function TOBDRRSessionNegotiator.RequiresSecurityAccess(
  SessionType: TOBDSessionType): Boolean;
begin
  Result := SessionType in [sstExtendedDiagnostic, sstProgramming,
                            sstOEMSpecific1, sstOEMSpecific2];
end;

function TOBDRRSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin Result := 1500; end;

function TOBDRRSessionNegotiator.DisplayName: string;
begin Result := 'Rolls-Royce (BMW E-Sys / ISTA)'; end;

function TOBDOEMExtensionRollsRoyce.ManufacturerKey: string;
begin Result := 'ROLLS_ROYCE'; end;

function TOBDOEMExtensionRollsRoyce.DisplayName: string;
begin Result := 'Rolls-Royce Motor Cars Ltd.'; end;

function TOBDOEMExtensionRollsRoyce.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in rolls-royce.json.
  Result := VINMatchesCatalog('rolls-royce.json', VIN);
end;
function TOBDOEMExtensionRollsRoyce.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDRRSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionRollsRoyce.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are rolls-royce.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('rolls-royce.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionRollsRoyce.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('rolls-royce.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
procedure TOBDOEMExtensionRollsRoyce.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionRollsRoyce.DtcCatalogFileName: string;
begin Result := 'dtc-rolls-royce.json'; end;

function TOBDOEMExtensionRollsRoyce.DecodeDID(const DID: Word;
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
        Result := Format('rr_meta_%.4x = "%s"',
          [DID, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionRollsRoyce.Create);

end.
