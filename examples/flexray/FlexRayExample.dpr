program FlexRayExample;

uses
  Vcl.Forms,
  FlexRayExampleMain in 'FlexRayExampleMain.pas' {FlexRayForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFlexRayForm, FlexRayForm);
  Application.Run;
end.
