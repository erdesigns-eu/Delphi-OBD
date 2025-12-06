program BluetoothDashboard;

uses
  Vcl.Forms,
  Examples.Bluetooth.BluetoothModule in 'BluetoothModule.pas' {BluetoothDashboardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBluetoothDashboardForm, BluetoothDashboardForm);
  Application.Run;
end.
