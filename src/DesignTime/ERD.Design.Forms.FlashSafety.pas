// ------------------------------------------------------------------------------
// ERD.Design.Forms.FlashSafety
//
// TOBDFlashSafetyDlg — modal warning dialog shown by the
// destructive component editors (Flasher, UDSTransfer,
// FlashPipeline) and by the pipeline validation report. Reminds
// the host of the safety contract before they wire one of these
// onto a form, and shows the full flashing safety guide below the
// reminder. The guide text is compiled into the design-time
// package (ERD.Design.SafetyGuide, generated from
// <c>docs/flashing-safety.md</c>), so no browser or internet
// connection is needed.
//
// This form is never registered on the palette — it is owned by
// the design-time package and only constructed from a component
// editor verb.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-05-10  ERD  Initial implementation.
// 2026-10-12  ERD  Show the built-in safety guide in the dialog
//                  instead of opening it online.
// ------------------------------------------------------------------------------

unit ERD.Design.Forms.FlashSafety;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.PngImage;

type
  /// <summary>Modal flashing-safety reminder with the full, built-in
  /// flashing safety guide.</summary>
  TOBDFlashSafetyDlg = class(TForm)
    pnlBody: TPanel;
    pnlButtons: TPanel;
    btnClose: TButton;
    lblWarning: TLabel;
    memGuide: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

  /// <summary>Show the flashing-safety reminder and the full safety
  /// guide in a modal dialog.</summary>
procedure ShowFlashSafetyDialog;

implementation

{$R *.dfm}

uses
  ERD.Design.SafetyGuide;

procedure TOBDFlashSafetyDlg.FormCreate(Sender: TObject);
begin
  memGuide.Lines.Text := OBDFlashSafetyGuideText;
end;

procedure ShowFlashSafetyDialog;
var
  Dlg: TOBDFlashSafetyDlg;
begin
  Dlg := TOBDFlashSafetyDlg.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

end.
