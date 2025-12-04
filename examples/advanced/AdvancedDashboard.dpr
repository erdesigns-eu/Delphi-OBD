program AdvancedDashboard;

uses
  Vcl.Forms,
  Examples.Advanced.AdvancedDashboard in 'AdvancedDashboard.pas' {AdvancedDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAdvancedDashboardForm, AdvancedDashboardForm);
  Application.Run;
end.
