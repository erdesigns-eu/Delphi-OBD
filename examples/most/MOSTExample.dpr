program MOSTExample;

uses
  Vcl.Forms,
  MOSTExampleMain in 'MOSTExampleMain.pas' {MOSTForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMOSTForm, MOSTForm);
  Application.Run;
end.
