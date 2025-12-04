program FTDIDashboard;

uses
  Vcl.Forms,
  Examples.FTDI.FTDIModule in 'FTDIModule.pas' {FTDIDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFTDIDashboardForm, FTDIDashboardForm);
  Application.Run;
end.
