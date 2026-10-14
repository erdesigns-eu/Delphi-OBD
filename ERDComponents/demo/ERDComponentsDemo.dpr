program ERDComponentsDemo;
uses Vcl.Forms, Showcase in 'Showcase.pas' {MainForm};
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TShowcaseForm, MainForm);
  Application.Run;
end.
