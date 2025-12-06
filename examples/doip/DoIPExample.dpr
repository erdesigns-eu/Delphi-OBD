program DoIPExample;

uses
  Vcl.Forms,
  DoIPMainForm in 'DoIPMainForm.pas' {FormDoIPMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDoIPMain, FormDoIPMain);
  Application.Run;
end.
