program ECUFlashingExample;

uses
  Vcl.Forms,
  ECUFlashingExampleMain in 'ECUFlashingExampleMain.pas' {ECUFlashingForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TECUFlashingForm, ECUFlashingForm);
  Application.Run;
end.
