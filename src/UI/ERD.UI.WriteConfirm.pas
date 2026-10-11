//------------------------------------------------------------------------------
//  ERD.UI.WriteConfirm
//
//  TOBDWriteConfirm - OBD Studio Pro inline confirmation strip for guarded
//  ECU writes, including the mandatory backup-first variant and keyboard use.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.WriteConfirm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Interactive part under the pointer.</summary>
  TOBDWriteConfirmHit = (
    /// <summary>No button is hit.</summary>
    wchNone,
    /// <summary>The cancel button is hit.</summary>
    wchCancel,
    /// <summary>The primary button is hit.</summary>
    wchPrimary);

  /// <summary>OBD Studio Pro inline write confirmation strip.</summary>
  TOBDWriteConfirm = class(TOBDCustomControl)
  strict private
    FBackupDone: Boolean;
    FTitle: string;
    FBackupRequiredTitle: string;
    FBackupText: string;
    FBackupRequiredText: string;
    FConfirmCaption: string;
    FBackupCaption: string;
    FCancelCaption: string;
    FHotPart: TOBDWriteConfirmHit;
    FDownPart: TOBDWriteConfirmHit;
    FPrimaryRect: TRect;
    FCancelRect: TRect;
    FOnConfirm: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnBackup: TNotifyEvent;
    procedure SetBackupDone(AValue: Boolean);
    procedure SetTitle(const AValue: string);
    procedure SetBackupRequiredTitle(const AValue: string);
    procedure SetBackupText(const AValue: string);
    procedure SetBackupRequiredText(const AValue: string);
    procedure SetConfirmCaption(const AValue: string);
    procedure SetBackupCaption(const AValue: string);
    procedure SetCancelCaption(const AValue: string);
    procedure SetHotPart(AValue: TOBDWriteConfirmHit);
    procedure SetDownPart(AValue: TOBDWriteConfirmHit);
    function StripHeight: Integer;
    function PrimaryCaption: string;
    function ButtonWidth96(const ACaption: string): Integer;
    procedure LayoutFallbackButtons;
    function ButtonState(APart: TOBDWriteConfirmHit): TOBDControlState;
    procedure DoPrimary;
    procedure DoCancel;
    procedure PaintText(var P: TOBDPainter; const R: TRect; AColor: TColor);
    procedure PaintButtons(var P: TOBDPainter; const R: TRect);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the inline confirmation strip.</summary>
    /// <param name="ACanvas">Canvas to paint to.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks button hover state.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Presses a painted button.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Releases and invokes a painted button.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Handles Enter for the primary action and Escape for cancel.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Requests Enter and Escape key messages.</summary>
    /// <param name="Message">Dialog-code message.</param>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    /// <summary>Creates the strip with backup completed.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Repaints the strip after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the button or background at a point.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Hit part.</returns>
    function HitTest(X, Y: Integer): TOBDWriteConfirmHit;
    /// <summary>Current primary-button rectangle.</summary>
    /// <returns>Client rectangle.</returns>
    function PrimaryButtonRect: TRect;
    /// <summary>Current cancel-button rectangle.</summary>
    /// <returns>Client rectangle.</returns>
    function CancelButtonRect: TRect;
  published
    /// <summary>True shows the ready-to-write variant; False asks for a backup.</summary>
    property BackupDone: Boolean read FBackupDone write SetBackupDone
      default True;
    /// <summary>Ready variant title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Backup-required variant title.</summary>
    property BackupRequiredTitle: string read FBackupRequiredTitle
      write SetBackupRequiredTitle;
    /// <summary>Ready variant backup status line.</summary>
    property BackupText: string read FBackupText write SetBackupText;
    /// <summary>Backup-required explanatory line.</summary>
    property BackupRequiredText: string read FBackupRequiredText
      write SetBackupRequiredText;
    /// <summary>Primary caption when backup is done.</summary>
    property ConfirmCaption: string read FConfirmCaption write SetConfirmCaption;
    /// <summary>Primary caption when a backup is still required.</summary>
    property BackupCaption: string read FBackupCaption write SetBackupCaption;
    /// <summary>Cancel button caption.</summary>
    property CancelCaption: string read FCancelCaption write SetCancelCaption;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the ready primary button is invoked.</summary>
    property OnConfirm: TNotifyEvent read FOnConfirm write FOnConfirm;
    /// <summary>Fires when Cancel or Escape is invoked.</summary>
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    /// <summary>Fires when the backup-required primary button is invoked.</summary>
    property OnBackup: TNotifyEvent read FOnBackup write FOnBackup;
  end;

implementation

constructor TOBDWriteConfirm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  FBackupDone := True;
  FTitle := 'Write 3 changes to 09 Central electrics?';
  FBackupRequiredTitle := 'Make a backup before writing';
  FBackupText := 'Backup saved 18:51 ' + #$00B7 + ' CODING_09_2026-10-11.json';
  FBackupRequiredText := 'Writing is enabled once the current coding is saved.';
  FConfirmCaption := 'Write coding';
  FBackupCaption := 'Make backup';
  FCancelCaption := 'Cancel';
  Width := 720;
  Height := StripHeight;
end;

procedure TOBDWriteConfirm.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetBackupDone(AValue: Boolean);
begin
  if FBackupDone = AValue then
    Exit;
  FBackupDone := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetBackupRequiredTitle(const AValue: string);
begin
  if FBackupRequiredTitle = AValue then
    Exit;
  FBackupRequiredTitle := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetBackupText(const AValue: string);
begin
  if FBackupText = AValue then
    Exit;
  FBackupText := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetBackupRequiredText(const AValue: string);
begin
  if FBackupRequiredText = AValue then
    Exit;
  FBackupRequiredText := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetConfirmCaption(const AValue: string);
begin
  if FConfirmCaption = AValue then
    Exit;
  FConfirmCaption := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetBackupCaption(const AValue: string);
begin
  if FBackupCaption = AValue then
    Exit;
  FBackupCaption := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetCancelCaption(const AValue: string);
begin
  if FCancelCaption = AValue then
    Exit;
  FCancelCaption := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetHotPart(AValue: TOBDWriteConfirmHit);
begin
  if FHotPart = AValue then
    Exit;
  FHotPart := AValue;
  Invalidate;
end;

procedure TOBDWriteConfirm.SetDownPart(AValue: TOBDWriteConfirmHit);
begin
  if FDownPart = AValue then
    Exit;
  FDownPart := AValue;
  Invalidate;
end;

function TOBDWriteConfirm.StripHeight: Integer;
begin
  Result := ScaleValue(Metrics.Button + 28);
end;

function TOBDWriteConfirm.PrimaryCaption: string;
begin
  if FBackupDone then
    Result := FConfirmCaption
  else
    Result := FBackupCaption;
end;

function TOBDWriteConfirm.ButtonWidth96(const ACaption: string): Integer;
begin
  Result := 28 + Length(ACaption) * 7;
  if Result < 72 then
    Result := 72;
end;

procedure TOBDWriteConfirm.LayoutFallbackButtons;
var
  H, Y, W1, W2, X: Integer;
begin
  H := ScaleValue(Metrics.Button);
  Y := (Height - H) div 2;
  W1 := ScaleValue(ButtonWidth96(PrimaryCaption));
  W2 := ScaleValue(ButtonWidth96(FCancelCaption));
  X := Width - ScaleValue(16);
  FPrimaryRect := Rect(X - W1, Y, X, Y + H);
  Dec(X, W1 + ScaleValue(8));
  FCancelRect := Rect(X - W2, Y, X, Y + H);
end;

function TOBDWriteConfirm.ButtonState(APart: TOBDWriteConfirmHit)
  : TOBDControlState;
begin
  if not Enabled then
    Result := cstDisabled
  else if FDownPart = APart then
    Result := cstPressed
  else if FHotPart = APart then
    Result := cstHover
  else
    Result := cstNormal;
end;

procedure TOBDWriteConfirm.DoPrimary;
begin
  if FBackupDone then
  begin
    if Assigned(FOnConfirm) then
      FOnConfirm(Self);
  end
  else if Assigned(FOnBackup) then
    FOnBackup(Self);
end;

procedure TOBDWriteConfirm.DoCancel;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

function TOBDWriteConfirm.HitTest(X, Y: Integer): TOBDWriteConfirmHit;
begin
  if FPrimaryRect.IsEmpty or FCancelRect.IsEmpty then
    LayoutFallbackButtons;
  if PtInRect(FPrimaryRect, Point(X, Y)) then
    Result := wchPrimary
  else if PtInRect(FCancelRect, Point(X, Y)) then
    Result := wchCancel
  else
    Result := wchNone;
end;

function TOBDWriteConfirm.PrimaryButtonRect: TRect;
begin
  if FPrimaryRect.IsEmpty then
    LayoutFallbackButtons;
  Result := FPrimaryRect;
end;

function TOBDWriteConfirm.CancelButtonRect: TRect;
begin
  if FCancelRect.IsEmpty then
    LayoutFallbackButtons;
  Result := FCancelRect;
end;

procedure TOBDWriteConfirm.PaintText(var P: TOBDPainter; const R: TRect;
  AColor: TColor);
var
  Mid, MaxW: Integer;
  Caption, Line: string;
begin
  Mid := R.Top + R.Height div 2;
  if FBackupDone then
  begin
    Caption := FTitle;
    Line := FBackupText;
  end
  else
  begin
    Caption := FBackupRequiredTitle;
    Line := FBackupRequiredText;
  end;
  MaxW := FCancelRect.Left - (R.Left + P.S(44)) - P.S(16);
  if MaxW < P.S(80) then
    MaxW := R.Width - P.S(88);
  if FBackupDone then
    P.Glyph(glWrite, R.Left + P.S(24), Mid, P.AccentText, 1)
  else
    P.Glyph(glWrite, R.Left + P.S(24), Mid, AColor, 1);
  P.Text(R.Left + P.S(44), Mid - P.S(9), Caption, OBD_TEXT_HEADING,
    Palette.ForegroundText, twBold, taLeftJustify, MaxW);
  if FBackupDone then
  begin
    P.GlyphCheck(R.Left + P.S(50), Mid + P.S(10), Palette.Success, 0.65);
    P.Text(R.Left + P.S(60), Mid + P.S(10), Line, OBD_TEXT_SMALL,
      Palette.ForegroundText, twRegular, taLeftJustify, MaxW - P.S(16));
  end
  else
    P.Text(R.Left + P.S(44), Mid + P.S(10), Line, OBD_TEXT_SMALL,
      Palette.ForegroundText, twRegular, taLeftJustify, MaxW);
end;

procedure TOBDWriteConfirm.PaintButtons(var P: TOBDPainter; const R: TRect);
var
  H, Y, X, BW: Integer;
begin
  H := ScaleValue(Metrics.Button);
  Y := R.Top + (R.Height - H) div 2;
  X := R.Right - P.S(16);
  BW := P.ButtonWidth(PrimaryCaption, glNone);
  FPrimaryRect := Rect(X - BW, Y, X, Y + H);
  P.Button(FPrimaryRect, PrimaryCaption, bkPrimary, ButtonState(wchPrimary),
    Focused and (FHotPart = wchPrimary), glNone);
  Dec(X, BW + P.S(8));
  BW := P.ButtonWidth(FCancelCaption, glNone);
  FCancelRect := Rect(X - BW, Y, X, Y + H);
  P.Button(FCancelRect, FCancelCaption, bkSecondary, ButtonState(wchCancel),
    Focused and (FHotPart = wchCancel), glNone);
end;

procedure TOBDWriteConfirm.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  R: TRect;
  Color, Outline, Fill: TColor;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    R := Rect(0, 0, Width, Height);
    if FBackupDone then
      Color := Palette.Accent
    else
      Color := Palette.Warning;
    Fill := P.Tint(Color, 0.12);
    Outline := OBDMixColor(Color, Palette.GaugeFace, 0.5);
    P.FillRect(R, Fill);
    P.FrameRect(R, Outline);
    P.FillRect(Rect(0, 0, P.S(4), Height), Color);
    PaintButtons(P, R);
    PaintText(P, R, Color);
    if Focused and (FHotPart = wchNone) then
      P.FocusRing(0, 0, Width, Height);
  finally
    P.Free;
  end;
end;

procedure TOBDWriteConfirm.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not Enabled then
    SetHotPart(wchNone)
  else
    SetHotPart(HitTest(X, Y));
end;

procedure TOBDWriteConfirm.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Hit: TOBDWriteConfirmHit;
begin
  inherited;
  if CanFocus and not Focused then
    SetFocus;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  Hit := HitTest(X, Y);
  SetHotPart(Hit);
  SetDownPart(Hit);
  if Hit <> wchNone then
    MouseCapture := True;
end;

procedure TOBDWriteConfirm.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Hit, Down: TOBDWriteConfirmHit;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  Hit := HitTest(X, Y);
  Down := FDownPart;
  MouseCapture := False;
  SetDownPart(wchNone);
  SetHotPart(Hit);
  if (Down <> wchNone) and (Down = Hit) then
  begin
    if Hit = wchPrimary then
      DoPrimary
    else if Hit = wchCancel then
      DoCancel;
  end;
end;

procedure TOBDWriteConfirm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not Enabled then
    Exit;
  case Key of
    VK_RETURN:
      DoPrimary;
    VK_ESCAPE:
      DoCancel;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDWriteConfirm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTALLKEYS;
end;

procedure TOBDWriteConfirm.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FDownPart = wchNone then
    SetHotPart(wchNone);
end;

end.
