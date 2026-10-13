program ERDComponentsDemo;
uses Vcl.Forms, Showcase in 'Showcase.pas';
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TShowcaseForm, MainForm);
  Application.Run;
end.
