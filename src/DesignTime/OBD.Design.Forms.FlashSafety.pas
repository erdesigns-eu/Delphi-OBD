//------------------------------------------------------------------------------
//  OBD.Design.Forms.FlashSafety
//
//  TOBDFlashSafetyDlg — modal warning dialog shown by the
//  destructive component editors (Flasher, UDSTransfer,
//  FlashPipeline). Reminds the host of the safety contract
//  before they wire one of these onto a form, and offers to open
//  <c>docs/flashing-safety.md</c> in the host's default browser.
//
//  This form is never registered on the palette — it is owned by
//  the design-time package and only constructed from a component
//  editor verb.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Forms.FlashSafety;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.PngImage;

type
  /// <summary>Modal flashing-safety reminder.</summary>
  TOBDFlashSafetyDlg = class(TForm)
    pnlBody: TPanel;
    pnlButtons: TPanel;
    btnClose: TButton;
    btnOpenGuide: TButton;
    lblWarning: TLabel;
    procedure btnOpenGuideClick(Sender: TObject);
  end;

/// <summary>Convenience launcher.</summary>
procedure ShowFlashSafetyDialog;

implementation

{$R *.dfm}

uses
  Winapi.ShellAPI,
  Winapi.Windows;

procedure TOBDFlashSafetyDlg.btnOpenGuideClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open',
    'https://github.com/erdesigns-eu/delphi-obd/blob/main/docs/flashing-safety.md',
    nil, nil, SW_SHOWNORMAL);
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
