//------------------------------------------------------------------------------
//  OBD.Service.EVBattery.Catalog.Component
//
//  TOBDEVBatteryCatalogComp - non-visual component wrapping
//  the static TOBDEVBatteryCatalog. CatalogDir + AutoLoad
//  published; Reload + VendorKeys + TryGet exposed.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Service.EVBattery.Catalog.Component;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Service.EVBattery.Types,
  OBD.Service.EVBattery.Catalog;

type
  TOBDEVBatteryCatalogReloadedEvent = procedure(Sender: TObject) of object;

  TOBDEVBatteryCatalogComp = class(TComponent)
  strict private
    FCatalogDir: string;
    FAutoLoad:   Boolean;
    FOnReloaded: TOBDEVBatteryCatalogReloadedEvent;
    procedure SetCatalogDir(const AValue: string);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Reload;
    function VendorKeys: TArray<string>;
    function TryGet(const AVendor: string;
      out AOut: TOBDEVBatteryVendorCatalog): Boolean;
    procedure Register(const ACatalog: TOBDEVBatteryVendorCatalog);
  published
    property CatalogDir: string
      read FCatalogDir write SetCatalogDir;
    property AutoLoad: Boolean
      read FAutoLoad write FAutoLoad default True;
    property OnReloaded: TOBDEVBatteryCatalogReloadedEvent
      read FOnReloaded write FOnReloaded;
  end;

implementation

constructor TOBDEVBatteryCatalogComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoLoad := True;
end;

procedure TOBDEVBatteryCatalogComp.SetCatalogDir(const AValue: string);
begin
  FCatalogDir := AValue;
  if AValue <> '' then
    TOBDEVBatteryCatalog.CatalogDir := AValue;
end;

procedure TOBDEVBatteryCatalogComp.Loaded;
begin
  inherited;
  if FAutoLoad and not (csDesigning in ComponentState) then
    Reload;
end;

procedure TOBDEVBatteryCatalogComp.Reload;
begin
  TOBDEVBatteryCatalog.Reload;
  if Assigned(FOnReloaded) then FOnReloaded(Self);
end;

function TOBDEVBatteryCatalogComp.VendorKeys: TArray<string>;
begin
  Result := TOBDEVBatteryCatalog.VendorKeys;
end;

function TOBDEVBatteryCatalogComp.TryGet(const AVendor: string;
  out AOut: TOBDEVBatteryVendorCatalog): Boolean;
begin
  Result := TOBDEVBatteryCatalog.TryGet(AVendor, AOut);
end;

procedure TOBDEVBatteryCatalogComp.Register(
  const ACatalog: TOBDEVBatteryVendorCatalog);
begin
  TOBDEVBatteryCatalog.Register(ACatalog);
end;

end.
