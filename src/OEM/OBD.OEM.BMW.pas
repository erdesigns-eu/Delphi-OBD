//------------------------------------------------------------------------------
//  OBD.OEM.BMW
//
//  Bayerische Motoren Werke OEM extension. Wires the BMW
//  catalogue (<c>catalogs/bmw.json</c>) and DTC overlay
//  (<c>catalogs/dtc-bmw.json</c>) into the base framework and
//  publishes the BMW E-Sys / ISTA session negotiator.
//
//  E-Sys / ISTA choreography requires SecurityAccess after the
//  session-control reply for any coding or programming write.
//  This negotiator declares the requirement for the extended,
//  programming and both OEM-specific session types; the actual
//  seed/key dance lives in the seed-key registry. Tester-present
//  cadence is 1500 ms — E-series DMEs drop sessions at the
//  default 2000 ms.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.BMW;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>BMW E-Sys / ISTA session negotiator. Inherits the
  /// stock ISO 14229 plan and overrides the cadence + privilege
  /// gating.</summary>
  TOBDBMWSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    /// <summary><c>True</c> for extended / programming / OEM-
    /// specific sessions.</summary>
    function RequiresSecurityAccess(
      ASessionType: TOBDSessionType): Boolean; override;
    /// <summary>1500 ms heartbeat.</summary>
    function DefaultTesterPresentMs: Cardinal; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>BMW OEM extension.</summary>
  TOBDOEMExtensionBMW = class(TOBDOEMExtensionBase)
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
    procedure SeedDefaultSeedKeyAlgorithms(
      Reg: TOBDSeedKeyRegistry); override;
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

{ TOBDBMWSessionNegotiator }

function TOBDBMWSessionNegotiator.RequiresSecurityAccess(
  ASessionType: TOBDSessionType): Boolean;
begin
  Result := ASessionType in [sstExtendedDiagnostic, sstProgramming,
                             sstOEMSpecific1, sstOEMSpecific2];
end;

function TOBDBMWSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  // E-series DMEs occasionally drop sessions at the stock
  // 2000 ms heartbeat; ISTA picks 1500 ms.
  Result := 1500;
end;

function TOBDBMWSessionNegotiator.DisplayName: string;
begin
  Result := 'BMW E-Sys / ISTA';
end;

{ TOBDOEMExtensionBMW }

function TOBDOEMExtensionBMW.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDBMWSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionBMW.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  // Public placeholder XOR mask used by the bimmer-utility
  // tutorial flow against a simulated DME. Real E-Sys algorithms
  // ship inside ISTA and are NDA-protected; production users
  // replace this registration via RegisterAlgorithm.
  PUBLIC_MASK: array[0..3] of Byte = ($A5, $5A, $C3, $3C);
var
  Mask: TBytes;
begin
  SetLength(Mask, Length(PUBLIC_MASK));
  Move(PUBLIC_MASK[0], Mask[0], Length(PUBLIC_MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(Mask,
    'BMW community XOR-mask placeholder',
    'community-pr', False));
end;

procedure TOBDOEMExtensionBMW.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionBMW.DtcCatalogFileName: string;
begin
  Result := 'dtc-bmw.json';
end;

function TOBDOEMExtensionBMW.ManufacturerKey: string;
begin
  Result := 'BMW';
end;

function TOBDOEMExtensionBMW.DisplayName: string;
begin
  Result := 'Bayerische Motoren Werke';
end;

function TOBDOEMExtensionBMW.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('bmw.json', VIN);
end;

procedure TOBDOEMExtensionBMW.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('bmw.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionBMW.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('bmw.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

function TOBDOEMExtensionBMW.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Voltage: Word;
  Mileage: Cardinal;
begin
  case DID of
    $D051:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Payload[0] shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V',
          [Voltage / 1000.0]);
        Exit;
      end;
    $D050:
      if Length(Payload) >= 4 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8) or
                    Cardinal(Payload[3]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionBMW.Create);

end.
