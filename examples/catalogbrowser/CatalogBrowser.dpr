//------------------------------------------------------------------------------
// PROGRAM        : CatalogBrowser
// CONTENTS       : VCL example that walks every shipped OEM catalog
//                  and lets the user browse ECUs / DIDs / Routines /
//                  Coding Blocks / Adaptations / Actuator Tests /
//                  Live PIDs / DTC Extended Data side-by-side.
//                  Dogfoods the catalog loader + JSON catalog API.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Form is constructed programmatically (no .dfm)
//                  to keep the example self-contained — drop the
//                  two files into a Delphi VCL project, point the
//                  library path at src/Services + Packages, and
//                  build. Catalogs are discovered by walking the
//                  catalogs/ directory beside the example so the
//                  binary works wherever the repo lives.
//------------------------------------------------------------------------------
program CatalogBrowser;

uses
  Vcl.Forms,
  CatalogBrowser.MainForm in 'CatalogBrowser.MainForm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi-OBD Catalog Browser';
  Application.CreateForm(TCatalogBrowserForm, CatalogBrowserForm);
  Application.Run;
end.
