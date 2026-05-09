//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.DTC.Loader.pas
// CONTENTS       : Resolves a DTC catalog JSON by name through the
//                  shared catalog search path and merges it into a
//                  TOBDDtcCatalog. Mirrors OBD.OEM.Catalog.Loader.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.DTC.Loader;

interface

uses
  System.SysUtils,
  OBD.OEM.DTC, OBD.OEM.Catalog.Loader;

/// <summary>
///   Load <c>FileName</c> through the standard catalog search
///   path and merge its entries into <c>Cat</c>. Silently no-ops when
///   the file isn't found so deployments without the catalog folder
///   still work.
/// </summary>
procedure MergeDtcCatalog(const FileName: string; Cat: TOBDDtcCatalog);

implementation

//------------------------------------------------------------------------------
// MERGE DTC CATALOG
//------------------------------------------------------------------------------
procedure MergeDtcCatalog(const FileName: string; Cat: TOBDDtcCatalog);
var
  Path: string;
begin
  if Cat = nil then Exit;
  Path := ResolveCatalogPath(FileName);
  if Path = '' then Exit;
  try
    Cat.LoadFromFile(Path);
  except
    // Same policy as the JSON DID loader: a malformed catalog must
    // not break startup. Higher-level audit code (production
    // pipelines) should validate catalogs at build time.
  end;
end;

end.
