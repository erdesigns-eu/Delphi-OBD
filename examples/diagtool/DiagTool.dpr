//------------------------------------------------------------------------------
// PROGRAM        : DiagTool
// CONTENTS       : VCL reference diagnostic tool — connection wizard,
//                  OEM auto-detect, DTC reader, live-data dashboard,
//                  DID browser, routine runner. Demonstrates how to
//                  compose the framework into a shipping tool.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The form is constructed programmatically (no
//                  .dfm) to keep the example self-contained — drop
//                  the two files into a Delphi VCL project, add
//                  the framework to the library path, and build.
//------------------------------------------------------------------------------
program DiagTool;

uses
  Vcl.Forms,
  DiagTool.MainForm in 'DiagTool.MainForm.pas',
  // Make every shipped OEM extension self-register:
  OBD.OEM.VW, OBD.OEM.BMW, OBD.OEM.Mercedes,
  OBD.OEM.Ford, OBD.OEM.GM, OBD.OEM.Stellantis,
  OBD.OEM.Toyota, OBD.OEM.Honda, OBD.OEM.HyundaiKia,
  OBD.OEM.Nissan, OBD.OEM.Subaru, OBD.OEM.Mazda,
  OBD.OEM.Renault, OBD.OEM.Volvo, OBD.OEM.Tesla,
  OBD.OEM.Suzuki, OBD.OEM.Mitsubishi,
  OBD.OEM.Cummins, OBD.OEM.DetroitDiesel, OBD.OEM.PACCAR,
  OBD.OEM.VolvoTrucks, OBD.OEM.Scania, OBD.OEM.MAN,
  OBD.OEM.BYD, OBD.OEM.Geely, OBD.OEM.NIO,
  OBD.OEM.Xpeng, OBD.OEM.GreatWall;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi-OBD Diagnostic Tool';
  Application.CreateForm(TDiagToolMainForm, DiagToolMainForm);
  Application.Run;
end.
