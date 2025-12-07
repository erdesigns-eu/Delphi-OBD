program LINExample;

uses
  Vcl.Forms,
  LINExampleMain in 'LINExampleMain.pas' {LINForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLINForm, LINForm);
  Application.Run;
end.
