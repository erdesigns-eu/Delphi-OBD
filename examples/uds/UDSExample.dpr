program UDSExample;

uses
  Vcl.Forms,
  UDSExampleMain in 'UDSExampleMain.pas' {UDSForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TUDSForm, UDSForm);
  Application.Run;
end.
