program TachographExample;

uses
  Vcl.Forms,
  TachographExampleMain in 'TachographExampleMain.pas' {TachographForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTachographForm, TachographForm);
  Application.Run;
end.
