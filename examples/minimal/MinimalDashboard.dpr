program MinimalDashboard;

uses
  Vcl.Forms,
  Examples.Minimal.MinimalDashboard in 'MinimalDashboard.pas' {MILDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMILDashboardForm, MILDashboardForm);
  Application.Run;
end.
