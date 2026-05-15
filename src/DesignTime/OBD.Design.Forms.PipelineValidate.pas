//------------------------------------------------------------------------------
//  OBD.Design.Forms.PipelineValidate
//
//  TOBDPipelineValidateDlg — modal report shown by the
//  <c>TOBDFlashPipeline</c> component editor's "Validate
//  configuration" verb. Renders a list of design-time issues
//  (missing protocol, missing voltage gate, AutoExecute true,
//  missing audit log) and offers to open the safety guide.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Forms.PipelineValidate;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  /// <summary>Severity of a single issue line.</summary>
  TOBDPipelineIssueLevel = (pilInfo, pilWarning, pilError);

  /// <summary>Modal validation report.</summary>
  TOBDPipelineValidateDlg = class(TForm)
    pnlHeader: TPanel;
    lblComponent: TLabel;
    lblHeader: TLabel;
    lstIssues: TListBox;
    pnlButtons: TPanel;
    btnClose: TButton;
    btnOpenGuide: TButton;
    procedure btnOpenGuideClick(Sender: TObject);
  end;

  /// <summary>One issue line.</summary>
  TOBDPipelineIssue = record
    Level: TOBDPipelineIssueLevel;
    Text: string;
  end;

/// <summary>Show the validation report for a pipeline component
/// against <c>AIssues</c>. <c>AComponentName</c> is shown in the
/// header.</summary>
procedure ShowPipelineValidation(const AComponentName: string;
  const AIssues: array of TOBDPipelineIssue);

implementation

{$R *.dfm}

uses
  Winapi.ShellAPI,
  Winapi.Windows;

procedure TOBDPipelineValidateDlg.btnOpenGuideClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open',
    'https://github.com/erdesigns-eu/delphi-obd/blob/main/docs/flashing-safety.md',
    nil, nil, SW_SHOWNORMAL);
end;

procedure ShowPipelineValidation(const AComponentName: string;
  const AIssues: array of TOBDPipelineIssue);
const
  PREFIX: array[TOBDPipelineIssueLevel] of string = (
    '   ',
    '!  ',
    'X  ');
var
  Dlg: TOBDPipelineValidateDlg;
  I: Integer;
begin
  Dlg := TOBDPipelineValidateDlg.Create(nil);
  try
    Dlg.lblComponent.Caption := AComponentName;
    Dlg.lstIssues.Items.BeginUpdate;
    try
      Dlg.lstIssues.Items.Clear;
      if Length(AIssues) = 0 then
        Dlg.lstIssues.Items.Add('   No configuration issues detected.')
      else
        for I := Low(AIssues) to High(AIssues) do
          Dlg.lstIssues.Items.Add(
            PREFIX[AIssues[I].Level] + AIssues[I].Text);
    finally
      Dlg.lstIssues.Items.EndUpdate;
    end;
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

end.
