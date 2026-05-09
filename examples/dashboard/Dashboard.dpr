program Dashboard;

uses
  Vcl.Forms,
  Examples.Dashboard.Form in 'DashboardForm.pas' {DashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'OBD-II Reference Dashboard';
  Application.CreateForm(TDashboardForm, DashboardForm);
  Application.Run;
end.
