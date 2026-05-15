//------------------------------------------------------------------------------
//  OBD.OEM.RollsRoyce
//
//  Rolls-Royce Motor Cars Ltd. (Goodwood, England) OEM
//  extension. BMW Group sub-brand; uses the BMW E-Sys / ISTA
//  stack with Rolls-Royce-specific extensions. Catalogue covers
//  Phantom (RR1 / 8th-gen) + Ghost (RR21) + Cullinan (RR31) +
//  Spectre (RR23 EV) + Black Badge trims.
//
//  Catalogue + DTC overlay live in
//  <c>catalogs/rolls-royce.json</c> and
//  <c>catalogs/dtc-rolls-royce.json</c> on top of the ISO
//  15031-6 baseline.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.RollsRoyce;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Rolls-Royce session negotiator — BMW Group
  /// lineage.</summary>
  TOBDRRSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    /// <summary><c>True</c> for extended / programming
    /// sessions.</summary>
    function RequiresSecurityAccess(
      ASessionType: TOBDSessionType): Boolean; override;
    /// <summary>1500 ms heartbeat.</summary>
    function DefaultTesterPresentMs: Cardinal; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>Rolls-Royce OEM extension.</summary>
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
    function DecodeDID(const DID: Word;
      const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers,
  OBD.OEM.Catalog.Loader,
  OBD.OEM.DTC.Loader;

{ TOBDRRSessionNegotiator }

function TOBDRRSessionNegotiator.RequiresSecurityAccess(
  ASessionType: TOBDSessionType): Boolean;
begin
  Result := ASessionType in [sstExtendedDiagnostic, sstProgramming,
                             sstOEMSpecific1, sstOEMSpecific2];
end;

function TOBDRRSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 1500;
end;

function TOBDRRSessionNegotiator.DisplayName: string;
begin
  Result := 'Rolls-Royce (BMW E-Sys / ISTA)';
end;

{ TOBDOEMExtensionRollsRoyce }

function TOBDOEMExtensionRollsRoyce.ManufacturerKey: string;
begin
  Result := 'ROLLS_ROYCE';
end;

function TOBDOEMExtensionRollsRoyce.DisplayName: string;
begin
  Result := 'Rolls-Royce Motor Cars Ltd.';
end;

function TOBDOEMExtensionRollsRoyce.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('rolls-royce.json', VIN);
end;

function TOBDOEMExtensionRollsRoyce.CreateSessionNegotiator:
  IOBDSessionNegotiator;
begin
  Result := TOBDRRSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionRollsRoyce.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
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

procedure TOBDOEMExtensionRollsRoyce.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionRollsRoyce.DtcCatalogFileName: string;
begin
  Result := 'dtc-rolls-royce.json';
end;

function TOBDOEMExtensionRollsRoyce.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
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
