//------------------------------------------------------------------------------
//  OBD.Service.DriveCycle.Catalog.Component
//
//  TOBDDriveCycleCatalogComp - non-visual component wrapping
//  the static TOBDDriveCycleCatalog. CatalogDir + AutoLoad
//  published; Reload + RegisteredMonitors exposed.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Service.DriveCycle.Catalog.Component;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Service.DriveCycle.Types,
  OBD.Service.DriveCycle.Catalog;

type
  TOBDDriveCycleCatalogReloadedEvent = procedure(Sender: TObject) of object;

  TOBDDriveCycleCatalogComp = class(TComponent)
  strict private
    FCatalogDir: string;
    FAutoLoad:   Boolean;
    FOnReloaded: TOBDDriveCycleCatalogReloadedEvent;
    procedure SetCatalogDir(const AValue: string);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Reload;
    function RegisteredMonitors: TArray<TOBDMonitor>;
    function TryGetCycle(AMonitor: TOBDMonitor;
      out AOut: TOBDDriveCycle): Boolean;
    procedure RegisterCycle(const ACycle: TOBDDriveCycle);
  published
    property CatalogDir: string
      read FCatalogDir write SetCatalogDir;
    property AutoLoad: Boolean
      read FAutoLoad write FAutoLoad default True;
    property OnReloaded: TOBDDriveCycleCatalogReloadedEvent
      read FOnReloaded write FOnReloaded;
  end;

implementation

constructor TOBDDriveCycleCatalogComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoLoad := True;
end;

procedure TOBDDriveCycleCatalogComp.SetCatalogDir(const AValue: string);
begin
  FCatalogDir := AValue;
  if AValue <> '' then
    TOBDDriveCycleCatalog.CatalogDir := AValue;
end;

procedure TOBDDriveCycleCatalogComp.Loaded;
begin
  inherited;
  if FAutoLoad and not (csDesigning in ComponentState) then
    Reload;
end;

procedure TOBDDriveCycleCatalogComp.Reload;
begin
  TOBDDriveCycleCatalog.Reload;
  if Assigned(FOnReloaded) then FOnReloaded(Self);
end;

function TOBDDriveCycleCatalogComp.RegisteredMonitors: TArray<TOBDMonitor>;
begin
  Result := TOBDDriveCycleCatalog.RegisteredMonitors;
end;

function TOBDDriveCycleCatalogComp.TryGetCycle(AMonitor: TOBDMonitor;
  out AOut: TOBDDriveCycle): Boolean;
begin
  Result := TOBDDriveCycleCatalog.TryGetCycle(AMonitor, AOut);
end;

procedure TOBDDriveCycleCatalogComp.RegisterCycle(
  const ACycle: TOBDDriveCycle);
begin
  TOBDDriveCycleCatalog.RegisterCycle(ACycle);
end;

end.
