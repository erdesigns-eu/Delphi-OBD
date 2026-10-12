program OBDStudioPro;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  untProPage in 'untProPage.pas',
  untSimulator in '..\obd-studio\untSimulator.pas',
  untPageEcuNetwork in 'untPageEcuNetwork.pas' {frmEcuNetwork: TFrame},
  untPageService in 'untPageService.pas' {frmService: TFrame},
  untPageCoding in 'untPageCoding.pas' {frmCoding: TFrame},
  untPageFlash in 'untPageFlash.pas' {frmFlash: TFrame},
  untPageMemory in 'untPageMemory.pas' {frmMemory: TFrame},
  untPageRadio in 'untPageRadio.pas' {frmRadio: TFrame},
  untPageSecurity in 'untPageSecurity.pas' {frmSecurity: TFrame},
  untPageDyno in 'untPageDyno.pas' {frmDyno: TFrame},
  untPagePerformance in 'untPagePerformance.pas' {frmPerformance: TFrame},
  untPageMeasurement in 'untPageMeasurement.pas' {frmMeasurement: TFrame},
  untPageBus in 'untPageBus.pas' {frmBus: TFrame},
  untPageEvBattery in 'untPageEvBattery.pas' {frmEvBattery: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'OBD Studio Pro';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
