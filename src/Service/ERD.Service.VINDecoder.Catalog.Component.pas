// ------------------------------------------------------------------------------
// ERD.Service.VINDecoder.Catalog.Component
//
// TOBDVINCatalog - non-visual component wrapping the static
// TOBDVINDecoder catalogue. Drop on a form, set CatalogDir
// (or accept the default <exe>/catalogs/), and the catalogues
// will load before the first decode. AutoLoad fires the load
// on streaming so the IDE Object Inspector picks up vendor
// counts at design time.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT - see LICENSE
// ------------------------------------------------------------------------------

unit ERD.Service.VINDecoder.Catalog.Component;

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
  ERD.Service.VINDecoder;

type
  TOBDVINCatalogReloadedEvent = procedure(Sender: TObject) of object;

  TOBDVINCatalog = class(TComponent)
  strict private
    FCatalogDir: string;
    FAutoLoad: Boolean;
    FOnReloaded: TOBDVINCatalogReloadedEvent;
    procedure SetCatalogDir(const AValue: string);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>Force a (re)load of every VIN catalogue
    /// (regions / countries / WMIs / plants / VDS rules) from
    /// <see cref="CatalogDir"/>.</summary>
    procedure Reload;
  published
    /// <summary>Override the default catalog dir
    /// (<exe>/catalogs/). Empty = leave the global setting
    /// untouched.</summary>
    property CatalogDir: string read FCatalogDir write SetCatalogDir;

    /// <summary>When True (default) the IDE Loaded handler
    /// fires <see cref="Reload"/> immediately after the .dfm
    /// streams. Off if your catalog files only become
    /// reachable later in the host's startup.</summary>
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default True;

    property OnReloaded: TOBDVINCatalogReloadedEvent read FOnReloaded
      write FOnReloaded;
  end;

implementation

constructor TOBDVINCatalog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoLoad := True;
end;

procedure TOBDVINCatalog.SetCatalogDir(const AValue: string);
begin
  FCatalogDir := AValue;
  if AValue <> '' then
    TOBDVINDecoder.CatalogDir := AValue;
end;

procedure TOBDVINCatalog.Loaded;
begin
  inherited;
  if FAutoLoad and not(csDesigning in ComponentState) then
    Reload;
end;

procedure TOBDVINCatalog.Reload;
begin
  if FCatalogDir = '' then
    TOBDVINDecoder.LoadCatalogs(TOBDVINDecoder.CatalogDir)
  else
    TOBDVINDecoder.LoadCatalogs(FCatalogDir);
  if Assigned(FOnReloaded) then
    FOnReloaded(Self);
end;

end.
