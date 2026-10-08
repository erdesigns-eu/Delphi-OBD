//------------------------------------------------------------------------------
//  ERD.OEM.DTC.Loader
//
//  Resolves a DTC-catalogue JSON file by name through the shared
//  OEM catalogue search path and merges its entries into a
//  <see cref="TOBDDtcCatalog"/>. Mirrors the resolution policy of
//  <c>ERD.OEM.Catalog.Loader</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.OEM.DTC.Loader;

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
  ERD.OEM.DTC,
  ERD.OEM.Catalog.Loader;

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
