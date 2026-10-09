// ------------------------------------------------------------------------------
// ERD.OEM.Geely
//
// Geely Auto / Lynk & Co OEM extension. Catalogue + DTC overlay
// in <c>catalogs/geely.json</c> + <c>catalogs/dtc-geely.json</c>.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-05-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.OEM.Geely;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.OEM,
  ERD.OEM.Session,
  ERD.OEM.SeedKey,
  ERD.OEM.DTC;

type
  /// <summary>Geely OEM extension.</summary>
  TOBDOEMExtensionGeely = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure BuildExtendedCatalog(var CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var Adaptations: TArray<TOBDOEMAdaptation>;
      var ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var LivePIDs: TArray<TOBDOEMLivePID>;
      var DtcExtended: TArray<TOBDDtcExtendedDataRecord>); override;
    procedure SeedDefaultSeedKeyAlgorithms(Reg: TOBDSeedKeyRegistry); override;
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); override;
    function DtcCatalogFileName: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word; const Payload: TBytes): string;
      override;
  end;

implementation

uses
  ERD.OEM.Helpers,
  ERD.OEM.Catalog.Loader,
  ERD.OEM.DTC.Loader;

function TOBDOEMExtensionGeely.ManufacturerKey: string;
begin
  Result := 'GEELY';
end;

function TOBDOEMExtensionGeely.DisplayName: string;
begin
  Result := 'Geely Auto / Lynk & Co';
end;

function TOBDOEMExtensionGeely.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('geely.json', VIN);
end;

procedure TOBDOEMExtensionGeely.BuildCatalog
  (var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>; var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('geely.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionGeely.BuildExtendedCatalog(var CodingBlocks
  : TArray<TOBDOEMCodingBlock>; var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('geely.json', CodingBlocks, Adaptations,
    ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionGeely.SeedDefaultSeedKeyAlgorithms
  (Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    IOBDSeedKeyAlgorithm(TOBDSeedKeyKWP2000TwosComplement.Create()));
end;

procedure TOBDOEMExtensionGeely.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionGeely.DtcCatalogFileName: string;
begin
  Result := 'dtc-geely.json';
end;

function TOBDOEMExtensionGeely.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  FieldName: string;
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
        case DID of
          $F1A0:
            FieldName := 'geely_platform_code';
          $F1A2:
            FieldName := 'geely_market_code';
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

TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionGeely.Create);

end.
