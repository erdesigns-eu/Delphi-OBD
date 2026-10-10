//------------------------------------------------------------------------------
//  OBDStudioDashboard - sample 18
//
//  Workshop dashboard designed in the form designer from the
//  OBD Dashboard palette page. The form and its controls are in
//  DashboardMain.pas / DashboardMain.dfm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

program OBDStudioDashboard;

uses
  Vcl.Forms,
  DashboardMain in 'DashboardMain.pas' {frmDashboard};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'OBD Studio - Dashboard';
  Application.CreateForm(TfrmDashboard, frmDashboard);
  Application.Run;
end.
