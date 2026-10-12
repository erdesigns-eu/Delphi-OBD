//------------------------------------------------------------------------------
//  ERD.UI.Memo
//
//  TOBDMemo - themed multi-line text box for notes, customer complaints,
//  repair remarks and raw logs.
//
//  A borderless VCL memo sits inside the same frame TOBDEdit paints (one
//  pixel divider line, two pixel orange line while focused), in the type
//  scale of the other controls. The Windows scroll bar is replaced by the
//  themed overlay bar on the right; it follows typing, the caret, the
//  mouse wheel and selection drags, and it can be dragged or clicked.
//  Mono switches to Consolas for logs and frames.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Memo;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Themed multi-line edit with the themed scroll bar.</summary>
  TOBDMemo = class(TOBDCustomControl)
  strict private
    FMemo: TMemo;
    FMono: Boolean;
    FFirstLine: Integer;
    FLineCount: Integer;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    FOnChange: TNotifyEvent;
    function GetLines: TStrings;
    procedure SetLines(AValue: TStrings);
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(AValue: Boolean);
    function GetMaxLength: Integer;
    procedure SetMaxLength(AValue: Integer);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(AValue: Boolean);
    function GetWantTabs: Boolean;
    procedure SetWantTabs(AValue: Boolean);
    function GetWantReturns: Boolean;
    procedure SetWantReturns(AValue: Boolean);
    function GetMemoTextHint: string;
    procedure SetMemoTextHint(const AValue: string);
    procedure SetMono(AValue: Boolean);
    function BarWidth: Integer;
    function BarRect: TRect;
    function VisibleLines: Integer;
    function NeedsBar: Boolean;
    procedure SetBarHot(AValue: Boolean);
    procedure ScrollToLine(ALine: Integer);
    procedure LayoutInner;
    procedure SyncInnerLook;
    procedure InnerChange(Sender: TObject);
    procedure InnerFocusChange(Sender: TObject);
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the frame and the scroll bar.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Lays the memo out in the frame.</summary>
    procedure Resize; override;
    /// <summary>Focuses the memo or starts a bar drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Drags the thumb and tracks hover over the bar.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a thumb drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Scrolls three lines per notch when the wheel reaches the
    /// frame.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when the memo scrolled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Fires <see cref="OnChange"/>.</summary>
    procedure Change; virtual;
  public
    /// <summary>Creates a 240 x 96 memo.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Applies the density padding and bar width.</summary>
    procedure DensityChanged; override;
    /// <summary>Focuses the text.</summary>
    procedure SetFocus; override;
    /// <summary>True when the text has focus.</summary>
    /// <returns>Focus flag.</returns>
    function Focused: Boolean; override;
    /// <summary>Selects all text.</summary>
    procedure SelectAll;
    /// <summary>Removes all text.</summary>
    procedure Clear;
    /// <summary>Re-reads the scroll position of the text; called by the
    /// inner memo after anything that can scroll it.</summary>
    procedure SyncScroll;
    /// <summary>All text as one string.</summary>
    property Text: string read GetText write SetText;
  published
    /// <summary>The lines of text.</summary>
    property Lines: TStrings read GetLines write SetLines;
    /// <summary>Blocks typing; text can still be selected and copied.
    /// </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly
      default False;
    /// <summary>Maximum number of characters; 0 = no limit.</summary>
    property MaxLength: Integer read GetMaxLength write SetMaxLength
      default 0;
    /// <summary>Wraps long lines at the right edge.</summary>
    property WordWrap: Boolean read GetWordWrap write SetWordWrap
      default True;
    /// <summary>Tab inserts a tab instead of moving focus.</summary>
    property WantTabs: Boolean read GetWantTabs write SetWantTabs
      default False;
    /// <summary>Enter starts a new line instead of pressing the default
    /// button.</summary>
    property WantReturns: Boolean read GetWantReturns write SetWantReturns
      default True;
    /// <summary>Grey prompt shown while the memo is empty.</summary>
    property TextHint: string read GetMemoTextHint write SetMemoTextHint;
    /// <summary>Consolas instead of Segoe UI, for logs and frames.
    /// </summary>
    property Mono: Boolean read FMono write SetMono default False;
    /// <summary>Desktop or tablet padding.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires on the main thread when the text changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  ERD.UI.ScrollBar;

type
  // Memo that tells its owner after anything that can scroll it, and
  // takes the wheel itself (it has no Windows scroll bar).
  TOBDInnerMemo = class(TMemo)
  protected
    procedure WndProc(var Message: TMessage); override;
  end;

procedure TOBDInnerMemo.WndProc(var Message: TMessage);
var
  Delta, Steps: Integer;
begin
  if Message.Msg = WM_MOUSEWHEEL then
  begin
    Delta := SmallInt(HiWord(Message.WParam));
    Steps := -MulDiv(Delta, 3, WHEEL_DELTA);
    if (Steps = 0) and (Delta <> 0) then
      Steps := -Sign(Delta);
    Perform(EM_LINESCROLL, 0, Steps);
    Message.Result := 0;
  end
  else
    inherited WndProc(Message);
  case Message.Msg of
    WM_MOUSEWHEEL, WM_VSCROLL, WM_KEYDOWN, WM_CHAR, WM_SIZE, WM_SETTEXT,
    WM_PASTE, WM_CUT, WM_CLEAR, WM_UNDO, WM_MOUSEMOVE, WM_LBUTTONUP,
    EM_REPLACESEL, EM_SETSEL, EM_LINESCROLL, EM_SCROLLCARET:
      if Owner is TOBDMemo then
        TOBDMemo(Owner).SyncScroll;
  end;
end;

{ TOBDMemo }

constructor TOBDMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 240;
  Height := 96;
  TabStop := False;
  FMemo := TOBDInnerMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.BorderStyle := bsNone;
  FMemo.ScrollBars := ssNone;
  FMemo.WordWrap := True;
  FMemo.ParentFont := False;
  FMemo.ParentColor := False;
  FMemo.TabStop := True;
  FMemo.OnChange := InnerChange;
  FMemo.OnEnter := InnerFocusChange;
  FMemo.OnExit := InnerFocusChange;
  LayoutInner;
end;

function TOBDMemo.GetLines: TStrings;
begin
  Result := FMemo.Lines;
end;

procedure TOBDMemo.SetLines(AValue: TStrings);
begin
  FMemo.Lines.Assign(AValue);
end;

function TOBDMemo.GetText: string;
begin
  Result := FMemo.Text;
end;

procedure TOBDMemo.SetText(const AValue: string);
begin
  FMemo.Text := AValue;
end;

function TOBDMemo.GetReadOnly: Boolean;
begin
  Result := FMemo.ReadOnly;
end;

procedure TOBDMemo.SetReadOnly(AValue: Boolean);
begin
  FMemo.ReadOnly := AValue;
end;

function TOBDMemo.GetMaxLength: Integer;
begin
  Result := FMemo.MaxLength;
end;

procedure TOBDMemo.SetMaxLength(AValue: Integer);
begin
  FMemo.MaxLength := AValue;
end;

function TOBDMemo.GetWordWrap: Boolean;
begin
  Result := FMemo.WordWrap;
end;

procedure TOBDMemo.SetWordWrap(AValue: Boolean);
begin
  FMemo.WordWrap := AValue;
end;

function TOBDMemo.GetWantTabs: Boolean;
begin
  Result := FMemo.WantTabs;
end;

procedure TOBDMemo.SetWantTabs(AValue: Boolean);
begin
  FMemo.WantTabs := AValue;
end;

function TOBDMemo.GetWantReturns: Boolean;
begin
  Result := FMemo.WantReturns;
end;

procedure TOBDMemo.SetWantReturns(AValue: Boolean);
begin
  FMemo.WantReturns := AValue;
end;

function TOBDMemo.GetMemoTextHint: string;
begin
  Result := FMemo.TextHint;
end;

procedure TOBDMemo.SetMemoTextHint(const AValue: string);
begin
  FMemo.TextHint := AValue;
end;

procedure TOBDMemo.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  LayoutInner;
end;

procedure TOBDMemo.SetFocus;
begin
  if FMemo.CanFocus then
    FMemo.SetFocus
  else
    inherited SetFocus;
end;

function TOBDMemo.Focused: Boolean;
begin
  Result := FMemo.Focused;
end;

procedure TOBDMemo.SelectAll;
begin
  FMemo.SelectAll;
end;

procedure TOBDMemo.Clear;
begin
  FMemo.Clear;
end;

procedure TOBDMemo.InnerChange(Sender: TObject);
begin
  SyncScroll;
  Change;
end;

procedure TOBDMemo.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDMemo.InnerFocusChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDMemo.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FMemo.Enabled := Enabled;
  SyncInnerLook;
  Invalidate;
end;

procedure TOBDMemo.SyncInnerLook;
var
  Back, Ink: TColor;
begin
  if Enabled then
  begin
    Back := Palette.GaugeFace;
    Ink := Palette.ForegroundText;
  end
  else
  begin
    Back := Palette.Background;
    Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
  end;
  if FMemo.Color <> Back then
    FMemo.Color := Back;
  if FMemo.Font.Color <> Ink then
    FMemo.Font.Color := Ink;
end;

function TOBDMemo.BarWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(16)
  else
    Result := ScaleValue(10);
end;

function TOBDMemo.BarRect: TRect;
var
  F: Integer;
begin
  F := ScaleValue(2);
  Result := Rect(Width - F - BarWidth, F, Width - F, Height - F);
end;

procedure TOBDMemo.LayoutInner;
var
  FontH, Pad, PadY: Integer;
begin
  if FMemo = nil then
    Exit;
  if FMono and not SameText(FMemo.Font.Name, 'Consolas') then
    FMemo.Font.Name := 'Consolas'
  else if not FMono and not SameText(FMemo.Font.Name, 'Segoe UI') then
    FMemo.Font.Name := 'Segoe UI';
  FontH := -Round(OBD_TEXT_BODY * ScaleValue(96) / 96);
  if FMemo.Font.Height <> FontH then
    FMemo.Font.Height := FontH;
  if Density = dnTablet then
  begin
    Pad := ScaleValue(12);
    PadY := ScaleValue(10);
  end
  else
  begin
    Pad := ScaleValue(10);
    PadY := ScaleValue(6);
  end;
  // The bar strip is always reserved so the text does not re-wrap when
  // the bar appears.
  FMemo.SetBounds(Pad, PadY, Max(0, Width - Pad - BarWidth - ScaleValue(4)),
    Max(0, Height - 2 * PadY));
  SyncInnerLook;
end;

procedure TOBDMemo.Resize;
begin
  inherited Resize;
  LayoutInner;
  Invalidate;
end;

procedure TOBDMemo.DensityChanged;
begin
  LayoutInner;
  inherited DensityChanged;
end;

function TOBDMemo.VisibleLines: Integer;
begin
  Result := Max(1, FMemo.Height div Max(1, OBDFontLineHeight(FMemo.Font)));
end;

function TOBDMemo.NeedsBar: Boolean;
begin
  Result := FLineCount > VisibleLines;
end;

procedure TOBDMemo.SyncScroll;
var
  First, Count: Integer;
begin
  if (FMemo = nil) or (csDestroying in ComponentState) or
    not FMemo.HandleAllocated then
    Exit;
  First := FMemo.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
  Count := FMemo.Perform(EM_GETLINECOUNT, 0, 0);
  if (First = FFirstLine) and (Count = FLineCount) then
    Exit;
  FFirstLine := First;
  FLineCount := Count;
  Invalidate;
end;

procedure TOBDMemo.ScrollToLine(ALine: Integer);
var
  First: Integer;
begin
  if not FMemo.HandleAllocated then
    Exit;
  ALine := EnsureRange(ALine, 0, Max(0, FLineCount - VisibleLines));
  First := FMemo.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
  if ALine <> First then
    FMemo.Perform(EM_LINESCROLL, 0, ALine - First);
  SyncScroll;
end;

procedure TOBDMemo.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

procedure TOBDMemo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FBarDragging then
    SetBarHot(False);
end;

procedure TOBDMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Thumb: TRect;
begin
  if (Button = mbLeft) and FMemo.CanFocus then
    FMemo.SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not NeedsBar or
    not PtInRect(BarRect, Point(X, Y)) then
    Exit;
  Thumb := OBDScrollThumbRect(BarRect, True, 0, FLineCount - 1,
    VisibleLines, FFirstLine, FBarHot, False);
  if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
  begin
    FBarDragging := True;
    FBarDragOffset := Y - Thumb.Top;
    Invalidate;
  end
  else if Y < Thumb.Top then
    ScrollToLine(FFirstLine - VisibleLines)
  else
    ScrollToLine(FFirstLine + VisibleLines);
end;

procedure TOBDMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Bar, Thumb: TRect;
  Travel: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FBarDragging then
  begin
    Bar := BarRect;
    Thumb := OBDScrollThumbRect(Bar, True, 0, FLineCount - 1, VisibleLines,
      FFirstLine, True, True);
    Travel := Bar.Height - Thumb.Height;
    if Travel > 0 then
      ScrollToLine(Round((Y - FBarDragOffset - Bar.Top) / Travel *
        Max(0, FLineCount - VisibleLines)));
    Exit;
  end;
  SetBarHot(NeedsBar and PtInRect(BarRect, Point(X, Y)));
end;

procedure TOBDMemo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FBarDragging then
  begin
    FBarDragging := False;
    SetBarHot(PtInRect(BarRect, Point(X, Y)));
    Invalidate;
  end;
end;

function TOBDMemo.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result or not NeedsBar then
    Exit;
  ScrollToLine(FFirstLine - MulDiv(WheelDelta, 3, WHEEL_DELTA));
  Result := True;
end;

procedure TOBDMemo.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  SyncInnerLook;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(Rect(0, 0, Width, Height), FMemo.Focused and not IsPreview,
      Enabled);
    if NeedsBar then
      OBDPaintScrollBar(P, BarRect, True, 0, FLineCount - 1, VisibleLines,
        FFirstLine, FBarHot, FBarDragging);
  finally
    P.Free;
  end;
end;

end.
