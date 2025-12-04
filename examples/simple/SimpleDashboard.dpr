program SimpleDashboard;

uses
  Vcl.Forms,
  Examples.Simple.SimpleDashboard in 'SimpleDashboard.pas' {SimpleDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSimpleDashboardForm, SimpleDashboardForm);
  Application.Run;
end.
