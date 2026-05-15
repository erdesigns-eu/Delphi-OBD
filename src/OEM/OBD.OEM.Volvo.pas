//------------------------------------------------------------------------------
//  OBD.OEM.Volvo
//
//  Volvo Cars (Geely) OEM extension. Covers Volvo + Polestar
//  legacy SPA / SPA2 platforms. Catalogue + DTC overlay in
//  <c>catalogs/volvo.json</c> + <c>catalogs/dtc-volvo.json</c>.
//
//  TOBDVolvoSessionNegotiator picks a 5000 ms tester-present
//  cadence — Volvo VIDA / DiCE keep extended sessions alive at
//  that interval.
//
//  Seed-key starter is the KWP2000 two's-complement accepted by
//  pre-SPA2 ECUs; modern VIDA uses NDA crypto which production
//  callers register via RegisterAlgorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Volvo;

interface

uses
  System.SysUtils,
  OBD.OEM,
  OBD.OEM.Session,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>Volvo VIDA / DiCE session negotiator.</summary>
  TOBDVolvoSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    /// <summary>5000 ms heartbeat.</summary>
    function DefaultTesterPresentMs: Cardinal; override;
    /// <summary>Display label.</summary>
    function DisplayName: string; override;
  end;

  /// <summary>Volvo OEM extension.</summary>
  TOBDOEMExtensionVolvo = class(TOBDOEMExtensionBase)
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

{ TOBDVolvoSessionNegotiator }

function TOBDVolvoSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 5000;
end;

function TOBDVolvoSessionNegotiator.DisplayName: string;
begin
  Result := 'Volvo VIDA / DiCE';
end;

{ TOBDOEMExtensionVolvo }

function TOBDOEMExtensionVolvo.ManufacturerKey: string;
begin
  Result := 'VOLVO';
end;

function TOBDOEMExtensionVolvo.DisplayName: string;
begin
  Result := 'Volvo Cars (Geely)';
end;

function TOBDOEMExtensionVolvo.ApplicableToVIN(
  const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('volvo.json', VIN);
end;

function TOBDOEMExtensionVolvo.CreateSessionNegotiator:
  IOBDSessionNegotiator;
begin
  Result := TOBDVolvoSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionVolvo.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('volvo.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionVolvo.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('volvo.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionVolvo.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionVolvo.SeedDefaultDtcCatalog(
  Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionVolvo.DtcCatalogFileName: string;
begin
  Result := 'dtc-volvo.json';
end;

function TOBDOEMExtensionVolvo.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  FieldName: string;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s',
          [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F1A0, $F1A2, $F1B0:
      if Length(Payload) > 0 then
      begin
        case DID of
          $F1A0: FieldName := 'volvo_struct_week';
          $F1A2: FieldName := 'volvo_factory_code';
          $F1B0: FieldName := 'volvo_pno_code';
        else
          FieldName := 'unknown';
        end;
        Result := Format('%s = "%s"',
          [FieldName, TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionVolvo.Create);

end.
