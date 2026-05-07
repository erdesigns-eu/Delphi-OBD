program MobileDashboard;

uses
  System.StartUpCopy,
  FMX.Forms,
  Examples.Mobile.Dashboard in 'MobileDashboardForm.pas' {MobileDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMobileDashboardForm, MobileDashboardForm);
  Application.Run;
end.
