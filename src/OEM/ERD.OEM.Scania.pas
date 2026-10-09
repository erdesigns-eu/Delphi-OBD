// ------------------------------------------------------------------------------
// ERD.OEM.Scania
//
// Scania AB (Traton Group) OEM extension. Catalogue + DTC
// overlay in <c>catalogs/scania.json</c> +
// <c>catalogs/dtc-scania.json</c>.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-05-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.OEM.Scania;

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
  ERD.OEM.DTC,
  ERD.OEM.HD;

type
  /// <summary>Scania OEM extension.</summary>
  TOBDOEMExtensionScania = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionScania.ManufacturerKey: string;
begin
  Result := 'SCANIA';
end;

function TOBDOEMExtensionScania.DisplayName: string;
begin
  Result := 'Scania AB (Traton Group)';
end;

function TOBDOEMExtensionScania.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := VINMatchesCatalog('scania.json', VIN);
end;

procedure TOBDOEMExtensionScania.BuildCatalog
  (var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>; var ECUs: TArray<TOBDOEMECU>);
begin
  MergeCatalogJSON('scania.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionScania.BuildExtendedCatalog(var CodingBlocks
  : TArray<TOBDOEMCodingBlock>; var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('scania.json', CodingBlocks, Adaptations,
    ActuatorTests, LivePIDs, DtcExtended);
end;

procedure TOBDOEMExtensionScania.SeedDefaultSeedKeyAlgorithms
  (Reg: TOBDSeedKeyRegistry);
begin
  Reg.RegisterAlgorithm($01,
    IOBDSeedKeyAlgorithm(TOBDSeedKeyKWP2000TwosComplement.Create()));
end;

procedure TOBDOEMExtensionScania.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionScania.DtcCatalogFileName: string;
begin
  Result := 'dtc-scania.json';
end;

function TOBDOEMExtensionScania.DecodeDID(const DID: Word;
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
    $F1A0, $F1A2, $F1A4:
      if Length(Payload) > 0 then
      begin
        case DID of
          $F1A0:
            FieldName := 'scania_chassis_number';
          $F1A2:
            FieldName := 'scania_specification_code';
          $F1A4:
            FieldName := 'scania_engine_serial';
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

TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionScania.Create);

end.
