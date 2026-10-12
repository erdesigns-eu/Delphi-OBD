program OBDStudio;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'OBD Studio';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
