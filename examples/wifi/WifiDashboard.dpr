program WifiDashboard;

uses
  Vcl.Forms,
  Examples.Wifi.WifiModule in 'WifiModule.pas' {WifiDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWifiDashboardForm, WifiDashboardForm);
  Application.Run;
end.
