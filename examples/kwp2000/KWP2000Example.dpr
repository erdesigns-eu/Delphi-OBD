program KWP2000Example;

uses
  Vcl.Forms,
  KWP2000ExampleMain in 'KWP2000ExampleMain.pas' {KWP2000Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TKWP2000Form, KWP2000Form);
  Application.Run;
end.
