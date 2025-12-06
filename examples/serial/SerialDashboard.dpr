program SerialDashboard;

uses
  Vcl.Forms,
  Examples.Serial.SerialModule in 'SerialModule.pas' {SerialDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSerialDashboardForm, SerialDashboardForm);
  Application.Run;
end.
