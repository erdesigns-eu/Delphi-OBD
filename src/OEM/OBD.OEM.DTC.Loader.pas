//------------------------------------------------------------------------------
//  OBD.OEM.DTC.Loader
//
//  Resolves a DTC-catalogue JSON file by name through the shared
//  OEM catalogue search path and merges its entries into a
//  <see cref="TOBDDtcCatalog"/>. Mirrors the resolution policy of
//  <c>OBD.OEM.Catalog.Loader</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial port from v1 OBD.OEM.DTC.Loader.
//------------------------------------------------------------------------------

unit OBD.OEM.DTC.Loader;

interface

uses
  System.SysUtils,
  OBD.OEM.DTC,
  OBD.OEM.Catalog.Loader;

/// <summary>
///   Loads <c>FileName</c> through the standard catalogue search
///   path and merges its entries into <c>Cat</c>. Silently no-ops
///   when the file isn't found or fails to parse, so deployments
///   without a catalogues folder still work.
/// </summary>
/// <param name="FileName">Catalogue file name.</param>
/// <param name="Cat">Target catalogue to merge into.</param>
procedure MergeDtcCatalog(const FileName: string; Cat: TOBDDtcCatalog);

implementation

procedure MergeDtcCatalog(const FileName: string; Cat: TOBDDtcCatalog);
var
  Path: string;
begin
  if Cat = nil then
    Exit;
  Path := ResolveCatalogPath(FileName);
  if Path = '' then
    Exit;
  try
    Cat.LoadFromFile(Path);
  except
    // A malformed catalogue must not break application startup.
    // Production deployments validate catalogues at build time.
  end;
end;

end.
