program OBDStudioPersonal;

//------------------------------------------------------------------------------
//  OBD-Studio Personal
//
//  Free (for personal, non-commercial use) OBD-II diagnostics for Windows,
//  built with the Delphi-OBD components. See README.md and EULA.md.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  untPersonalPage in 'untPersonalPage.pas',
  untSettings in 'untSettings.pas',
  untSession in 'untSession.pas',
  untGarage in 'untGarage.pas',
  untWelcome in 'untWelcome.pas' {frmWelcome},
  untPageOverview in 'untPageOverview.pas' {frmPageOverview: TFrame},
  untPageCodes in 'untPageCodes.pas' {frmPageCodes: TFrame},
  untPageReadiness in 'untPageReadiness.pas' {frmPageReadiness: TFrame},
  untPageLive in 'untPageLive.pas' {frmPageLive: TFrame},
  untPageDashboard in 'untPageDashboard.pas' {frmPageDashboard: TFrame},
  untPageTests in 'untPageTests.pas' {frmPageTests: TFrame},
  untPageInfo in 'untPageInfo.pas' {frmPageInfo: TFrame},
  untPageFuel in 'untPageFuel.pas' {frmPageFuel: TFrame},
  untPagePerformance in 'untPagePerformance.pas' {frmPagePerformance: TFrame},
  untPageEV in 'untPageEV.pas' {frmPageEV: TFrame},
  untPageAdapter in 'untPageAdapter.pas' {frmPageAdapter: TFrame},
  untPageRecordings in 'untPageRecordings.pas' {frmPageRecordings: TFrame},
  untPageConsole in 'untPageConsole.pas' {frmPageConsole: TFrame},
  untPageSettings in 'untPageSettings.pas' {frmPageSettings: TFrame},
  untPageVehicles in 'untPageVehicles.pas' {frmPageVehicles: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'OBD-Studio Personal';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
