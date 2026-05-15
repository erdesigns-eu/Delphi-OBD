//------------------------------------------------------------------------------
//  OBD.Design.Forms.InitCommands
//
//  TOBDInitCommandsDlg — modal editor invoked by the
//  <see cref="OBD.Design.Editors.TOBDAdapterInitCommandsProperty"/>
//  custom property editor. Two-pane layout:
//
//    - Left list: a curated set of common ELM327 / OBDLink AT and
//      ST commands, double-click to insert at the caret.
//    - Right memo: the actual init-script the host is editing.
//      One command per line; lines starting with `//` or `;` are
//      treated as comments by the runtime engine.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Forms.InitCommands;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  /// <summary>Modal init-script editor.</summary>
  TOBDInitCommandsDlg = class(TForm)
    pnlPalette: TPanel;
    lblPalette: TLabel;
    lstPalette: TListBox;
    splPane: TSplitter;
    pnlEditor: TPanel;
    lblEditor: TLabel;
    memScript: TMemo;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lstPaletteDblClick(Sender: TObject);
  end;

/// <summary>Show the editor against <c>AScript</c>. Returns
/// <c>True</c> when the host clicked OK, in which case
/// <c>AScript</c> is updated in-place.</summary>
function EditInitCommands(AScript: TStrings): Boolean;

implementation

{$R *.dfm}

const
  PALETTE: array[0..15] of record Cmd, Description: string end = (
    (Cmd: 'ATZ';     Description: 'Reset chip'),
    (Cmd: 'ATE0';    Description: 'Echo off'),
    (Cmd: 'ATL0';    Description: 'Linefeed off'),
    (Cmd: 'ATS0';    Description: 'Spaces off'),
    (Cmd: 'ATH1';    Description: 'Headers on'),
    (Cmd: 'ATAT2';   Description: 'Adaptive timing on (aggressive)'),
    (Cmd: 'ATSP0';   Description: 'Auto protocol detect'),
    (Cmd: 'ATSP6';   Description: 'Force protocol 6 (CAN 11-bit / 500k)'),
    (Cmd: 'ATSP7';   Description: 'Force protocol 7 (CAN 29-bit / 500k)'),
    (Cmd: 'ATCAF1';  Description: 'Auto-format ISO-TP frames'),
    (Cmd: 'ATCAF0';  Description: 'Disable ISO-TP auto-format'),
    (Cmd: 'ATSH';    Description: 'Set CAN header (e.g. ATSH 7E0)'),
    (Cmd: 'ATFCSH';  Description: 'Set flow-control source header'),
    (Cmd: 'STDI';    Description: 'STN device id (OBDLink-only)'),
    (Cmd: 'STIX';    Description: 'STN ext device id (OBDLink-only)'),
    (Cmd: 'STBR';    Description: 'STN baud-rate (OBDLink-only)')
  );

procedure TOBDInitCommandsDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  lstPalette.Items.BeginUpdate;
  try
    lstPalette.Items.Clear;
    for I := Low(PALETTE) to High(PALETTE) do
      lstPalette.Items.Add(
        Format('%-8s  %s', [PALETTE[I].Cmd, PALETTE[I].Description]));
  finally
    lstPalette.Items.EndUpdate;
  end;
end;

procedure TOBDInitCommandsDlg.lstPaletteDblClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lstPalette.ItemIndex;
  if Idx < 0 then Exit;
  // Insert just the command (not the description) at the caret.
  memScript.SelText := PALETTE[Idx].Cmd + sLineBreak;
  memScript.SetFocus;
end;

function EditInitCommands(AScript: TStrings): Boolean;
var
  Dlg: TOBDInitCommandsDlg;
begin
  Result := False;
  if AScript = nil then Exit;
  Dlg := TOBDInitCommandsDlg.Create(nil);
  try
    Dlg.memScript.Lines.Assign(AScript);
    if Dlg.ShowModal = mrOk then
    begin
      AScript.Assign(Dlg.memScript.Lines);
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

end.
