// ------------------------------------------------------------------------------
// ERD.Service.Inspection.Component
//
// Non-visual component wrapper for the inspection-rules catalogue evaluator.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// 2026-10-12  ERD  Empty CatalogFile means the default file; the IDE path is
//                  no longer streamed into the DFM.
// ------------------------------------------------------------------------------

unit ERD.Service.Inspection.Component;

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
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  ERD.Service.DriveCycle.Types,
  ERD.Service.Inspection;

type
  /// <summary>Event fired after the inspection catalogue is reloaded.</summary>
  TOBDInspectionRulesReloadedEvent = procedure(Sender: TObject) of object;

  /// <summary>Component wrapper for periodic-inspection OBD rules.</summary>
  TOBDInspectionRules = class(TComponent)
  strict private
    FCatalogFile: string;
    FCountry: string;
    FAutoLoad: Boolean;
    FOnReloaded: TOBDInspectionRulesReloadedEvent;
    procedure SetCatalogFile(const AValue: string);
  protected
    /// <summary>Auto-loads the catalogue after streaming when enabled.</summary>
    procedure Loaded; override;
  public
    /// <summary>Constructs the component with AutoLoad enabled.</summary>
    constructor Create(AOwner: TComponent); override;

    /// <summary>Reloads the catalogue file.</summary>
    procedure Reload;

    /// <summary>Returns loaded country codes.</summary>
    function CountryCodes: TArray<string>;

    /// <summary>Looks up a rule by country code.</summary>
    function TryGetRule(const ACountryCode: string;
      out ARule: TOBDInspectionRule): Boolean;

    /// <summary>Evaluates a vehicle snapshot for the Country property.</summary>
    function Evaluate(const AReadiness: TArray<TOBDMonitorReadiness>;
      AMILOn: Boolean; AConfirmedDTCCount: Integer;
      AFuelType: TOBDInspectionFuelType; AFirstRegistration: TDateTime)
      : TOBDInspectionResult;
  published
    /// <summary>JSON catalogue file. Empty (the default) uses
    /// catalogs/inspection/rules.json next to the executable.</summary>
    property CatalogFile: string read FCatalogFile write SetCatalogFile;
    /// <summary>ISO 3166-1 alpha-2 country code used by Evaluate.</summary>
    property Country: string read FCountry write FCountry;
    /// <summary>Reload automatically after component streaming.</summary>
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default True;
    /// <summary>Raised after Reload completes.</summary>
    property OnReloaded: TOBDInspectionRulesReloadedEvent read FOnReloaded
      write FOnReloaded;
  end;

implementation

constructor TOBDInspectionRules.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoLoad := True;
end;

procedure TOBDInspectionRules.SetCatalogFile(const AValue: string);
begin
  FCatalogFile := AValue;
  if AValue <> '' then
    TOBDInspectionCatalog.CatalogFile := AValue;
end;

procedure TOBDInspectionRules.Loaded;
begin
  inherited;
  if FAutoLoad and not(csDesigning in ComponentState) then
    Reload;
end;

procedure TOBDInspectionRules.Reload;
begin
  if FCatalogFile <> '' then
    TOBDInspectionCatalog.CatalogFile := FCatalogFile;
  TOBDInspectionCatalog.Reload;
  if Assigned(FOnReloaded) then
    FOnReloaded(Self);
end;

function TOBDInspectionRules.CountryCodes: TArray<string>;
begin
  Result := TOBDInspectionCatalog.CountryCodes;
end;

function TOBDInspectionRules.TryGetRule(const ACountryCode: string;
  out ARule: TOBDInspectionRule): Boolean;
begin
  Result := TOBDInspectionCatalog.TryGetRule(ACountryCode, ARule);
end;

function TOBDInspectionRules.Evaluate(
  const AReadiness: TArray<TOBDMonitorReadiness>; AMILOn: Boolean;
  AConfirmedDTCCount: Integer; AFuelType: TOBDInspectionFuelType;
  AFirstRegistration: TDateTime): TOBDInspectionResult;
begin
  Result := TOBDInspectionCatalog.EvaluateCountry(FCountry, AReadiness,
    AMILOn, AConfirmedDTCCount, AFuelType, AFirstRegistration);
end;

end.
