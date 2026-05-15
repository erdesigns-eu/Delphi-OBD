//------------------------------------------------------------------------------
//  OBD.Design.Forms.LiveTest
//
//  TOBDLiveTestDlg — modal "test something at design time" dialog
//  shared by the connection, adapter and protocol component
//  editors. Header text identifies what the host is testing; the
//  body memo collects log lines emitted by the test action; the
//  footer shows a status banner ("Running…", "OK", "Failed").
//
//  The dialog is intentionally generic — each component editor
//  passes a callback that performs the test and writes lines to
//  the dialog. Threading is inside the callback (tests typically
//  run async and signal completion through the supplied write
//  routine).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Forms.LiveTest;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics;

type
  TOBDLiveTestStatus = (ltsRunning, ltsOK, ltsFail);

  /// <summary>Action signature passed by component editors.
  /// <para>The action receives a <c>WriteLine</c> callback for
  /// streaming log messages and a <c>SetStatus</c> callback for
  /// flipping the footer banner.</para></summary>
  TOBDLiveTestAction = reference to procedure(
    const AWriteLine: TProc<string>;
    const ASetStatus: TProc<TOBDLiveTestStatus>);

  /// <summary>Modal live-test dialog.</summary>
  TOBDLiveTestDlg = class(TForm)
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblTarget: TLabel;
    memLog: TMemo;
    pnlFooter: TPanel;
    pnlStatus: TPanel;
    lblStatus: TLabel;
    btnClose: TButton;
    btnRun: TButton;
    procedure btnRunClick(Sender: TObject);
  strict private
    FAction: TOBDLiveTestAction;
    procedure ApplyStatus(AStatus: TOBDLiveTestStatus);
    procedure WriteLine(const ALine: string);
  public
    /// <summary>Configures the dialog and runs the action once
    /// modally. Title / target are shown in the header; the
    /// action is invoked when the user clicks "Run".</summary>
    class procedure Show(const ATitle, ATarget: string;
      const AAction: TOBDLiveTestAction); static;
  end;

implementation

{$R *.dfm}

{ ---- TOBDLiveTestDlg --------------------------------------------------------- }

procedure TOBDLiveTestDlg.WriteLine(const ALine: string);
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    memLog.Lines.Add(ALine)
  else
    TThread.Queue(nil,
      procedure
      begin
        memLog.Lines.Add(ALine);
      end);
end;

procedure TOBDLiveTestDlg.ApplyStatus(AStatus: TOBDLiveTestStatus);
begin
  case AStatus of
    ltsRunning:
      begin
        pnlStatus.Color := $00DDDDDD;
        lblStatus.Caption := 'Running…';
      end;
    ltsOK:
      begin
        pnlStatus.Color := $00B5DEAA;  // soft green
        lblStatus.Caption := 'OK';
      end;
    ltsFail:
      begin
        pnlStatus.Color := $00A2A2DE;  // soft red
        lblStatus.Caption := 'Failed';
      end;
  end;
end;

procedure TOBDLiveTestDlg.btnRunClick(Sender: TObject);
begin
  if not Assigned(FAction) then Exit;
  btnRun.Enabled := False;
  try
    memLog.Lines.Clear;
    ApplyStatus(ltsRunning);
    FAction(WriteLine,
      procedure(AStatus: TOBDLiveTestStatus)
      begin
        if TThread.CurrentThread.ThreadID = MainThreadID then
          ApplyStatus(AStatus)
        else
          TThread.Queue(nil,
            procedure
            begin
              ApplyStatus(AStatus);
            end);
      end);
  finally
    btnRun.Enabled := True;
  end;
end;

class procedure TOBDLiveTestDlg.Show(const ATitle, ATarget: string;
  const AAction: TOBDLiveTestAction);
var
  Dlg: TOBDLiveTestDlg;
begin
  Dlg := TOBDLiveTestDlg.Create(nil);
  try
    Dlg.lblTitle.Caption := ATitle;
    Dlg.lblTarget.Caption := ATarget;
    Dlg.FAction := AAction;
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

end.
