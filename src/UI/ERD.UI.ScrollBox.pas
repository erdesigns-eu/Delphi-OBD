//------------------------------------------------------------------------------
//  ERD.UI.ScrollBox
//
//  TOBDScrollBox - container that scrolls its children vertically with
//  the themed overlay scroll bar (OBDPaintScrollBar) instead of the
//  Windows one, so settings pages and long forms match the rest of OBD
//  Studio.
//
//  Drop controls on it like on a TScrollBox. Top-aligned children stack
//  and scroll (a column of collapsible cards is the typical use);
//  non-aligned children scroll with them. The bar appears when the
//  children reach below the visible area; drag it, click the track, turn
//  the mouse wheel, or tab to a control below the fold (it is scrolled
//  into view). Children paint their background in the scroll box colour.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.ScrollBox;

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
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Vertically scrolling container with the themed scroll bar.
  /// </summary>
  TOBDScrollBox = class(TOBDCustomControl, IOBDSurface)
  strict private
    FScrollY: Integer;
    FBarShown: Boolean;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    FLineStep: Integer;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function NeedsBar: Boolean;
    function BarWidth: Integer;
    function BarRect: TRect;
    function ThumbRect: TRect;
    procedure SetBarHot(AValue: Boolean);
    procedure SetLineStep(AValue: Integer);
    procedure ShiftChildren(ANewScroll: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged);
      message CM_FOCUSCHANGED;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest);
      message CM_DESIGNHITTEST;
  protected
    /// <summary>Paints the scroll bar; the children paint themselves.
    /// </summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Moves the area aligned children use up by the scroll
    /// position and keeps the scroll bar strip free.</summary>
    /// <param name="Rect">Client area, adjusted in place.</param>
    procedure AdjustClientRect(var Rect: TRect); override;
    /// <summary>Aligns the children, then keeps the scroll position in
    /// range of the new content height.</summary>
    /// <param name="AControl">Control that triggered the alignment.</param>
    /// <param name="Rect">Area to align in.</param>
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    /// <summary>Starts a thumb drag or pages on the track.</summary>
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
    /// <summary>Scrolls <see cref="LineStep"/> pixels per wheel notch.
    /// </summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when the box scrolled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    /// <summary>Creates a 240 x 200 scroll box.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Writes the children at their unscrolled positions.
    /// </summary>
    /// <param name="Writer">Form writer.</param>
    procedure WriteState(Writer: TWriter); override;
    /// <summary>The colour children paint their background in.</summary>
    /// <returns>Effective background of the scroll box.</returns>
    function SurfaceColor: TColor;
    /// <summary>Scrolls to a position, clamped to the content.</summary>
    /// <param name="APosition">Pixels scrolled from the top.</param>
    procedure ScrollTo(APosition: Integer);
    /// <summary>Scrolls just enough to show a child (or a control inside
    /// a child) completely.</summary>
    /// <param name="AControl">Control to show.</param>
    procedure ScrollInView(AControl: TControl);
    /// <summary>Pixels scrolled from the top.</summary>
    property ScrollPosition: Integer read FScrollY write ScrollTo;
  published
    /// <summary>Pixels (96 DPI) per mouse-wheel notch.</summary>
    property LineStep: Integer read FLineStep write SetLineStep default 48;
    /// <summary>Space around the children.</summary>
    property Padding;
    /// <summary>Desktop or tablet scroll bar width.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
  end;

implementation

uses
  ERD.UI.ScrollBar;

{ TOBDScrollBox }

constructor TOBDScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FLineStep := 48;
  Width := 240;
  Height := 200;
end;

function TOBDScrollBox.SurfaceColor: TColor;
begin
  Result := EffectiveBackground;
end;

procedure TOBDScrollBox.SetLineStep(AValue: Integer);
begin
  FLineStep := Max(1, AValue);
end;

function TOBDScrollBox.BarWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(16)
  else
    Result := ScaleValue(10);
end;

function TOBDScrollBox.ContentHeight: Integer;
var
  I, B: Integer;
  C: TControl;
begin
  // Bottom of the lowest child, unscrolled, plus the bottom padding.
  Result := 0;
  for I := 0 to ControlCount - 1 do
  begin
    C := Controls[I];
    if not C.Visible and not (csDesigning in ComponentState) then
      Continue;
    B := C.Top + C.Height;
    if C.AlignWithMargins then
      Inc(B, C.Margins.Bottom);
    Result := Max(Result, B);
  end;
  if Result > 0 then
    Result := Result + FScrollY + Padding.Bottom;
end;

function TOBDScrollBox.MaxScroll: Integer;
begin
  Result := Max(0, ContentHeight - Height);
end;

function TOBDScrollBox.NeedsBar: Boolean;
begin
  // Decided by the last alignment pass, which also reserved the strip.
  Result := FBarShown;
end;

function TOBDScrollBox.BarRect: TRect;
begin
  Result := Rect(Width - BarWidth, 0, Width, Height);
end;

function TOBDScrollBox.ThumbRect: TRect;
begin
  Result := OBDScrollThumbRect(BarRect, True, 0, ContentHeight - 1, Height,
    FScrollY, FBarHot, FBarDragging);
end;

procedure TOBDScrollBox.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if FBarShown then
    Dec(Rect.Right, BarWidth);
  if Rect.Right < Rect.Left then
    Rect.Right := Rect.Left;
  OffsetRect(Rect, 0, -FScrollY);
end;

procedure TOBDScrollBox.AlignControls(AControl: TControl; var Rect: TRect);
var
  Pass: Integer;
  Shown: Boolean;
begin
  inherited AlignControls(AControl, Rect);
  // The children may have grown, shrunk or gone away: show or hide the
  // bar strip and stay in range, re-aligning in this same pass (a nested
  // Realign is swallowed while the alignment runs).
  for Pass := 1 to 2 do
  begin
    Shown := ContentHeight > Height;
    if (Shown = FBarShown) and (FScrollY <= MaxScroll) then
      Break;
    FBarShown := Shown;
    if not Shown then
      ShiftChildren(0)
    else if FScrollY > MaxScroll then
      ShiftChildren(MaxScroll);
    Rect := ClientRect;
    AdjustClientRect(Rect);
    inherited AlignControls(AControl, Rect);
  end;
  Invalidate;
end;

procedure TOBDScrollBox.ShiftChildren(ANewScroll: Integer);
var
  I, Delta: Integer;
  C: TControl;
begin
  Delta := ANewScroll - FScrollY;
  if Delta = 0 then
    Exit;
  FScrollY := ANewScroll;
  // Aligned children follow the shifted client area when the alignment
  // runs; the others move here.
  for I := 0 to ControlCount - 1 do
  begin
    C := Controls[I];
    if C.Align = alNone then
      C.Top := C.Top - Delta;
  end;
end;

procedure TOBDScrollBox.ScrollTo(APosition: Integer);
begin
  APosition := EnsureRange(APosition, 0, MaxScroll);
  if APosition = FScrollY then
    Exit;
  DisableAlign;
  try
    ShiftChildren(APosition);
  finally
    EnableAlign;
  end;
  Realign;
  Invalidate;
end;

procedure TOBDScrollBox.ScrollInView(AControl: TControl);
var
  TopY, BottomY, Margin: Integer;
  P: TPoint;
begin
  if (AControl = nil) or (AControl = Self) or not HandleAllocated or
    not ContainsControl(AControl) then
    Exit;
  if AControl.Parent = Self then
    P := AControl.BoundsRect.TopLeft
  else if (AControl.Parent <> nil) and AControl.Parent.HandleAllocated then
    P := ScreenToClient(AControl.Parent.ClientToScreen(
      AControl.BoundsRect.TopLeft))
  else
    Exit;
  Margin := ScaleValue(8);
  TopY := P.Y;
  BottomY := P.Y + AControl.Height;
  if TopY < 0 then
    ScrollTo(FScrollY + TopY - Margin)
  else if BottomY > Height then
    ScrollTo(FScrollY + Min(TopY - Margin, BottomY - Height + Margin));
end;

procedure TOBDScrollBox.WriteState(Writer: TWriter);
var
  Saved: Integer;
begin
  // The form file keeps the unscrolled layout.
  Saved := FScrollY;
  if Saved <> 0 then
    ScrollTo(0);
  try
    inherited WriteState(Writer);
  finally
    if Saved <> 0 then
      ScrollTo(Saved);
  end;
end;

procedure TOBDScrollBox.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

procedure TOBDScrollBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FBarDragging then
    SetBarHot(False);
end;

procedure TOBDScrollBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  // Tabbing to a control below (or above) the visible area shows it.
  if (Message.Sender <> nil) and (Message.Sender <> Self) and
    ContainsControl(Message.Sender) then
    ScrollInView(Message.Sender);
end;

procedure TOBDScrollBox.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  // The bar works in the form designer too, so content below the fold
  // can be reached.
  if NeedsBar and PtInRect(BarRect, Point(Message.XPos, Message.YPos)) then
    Message.Result := 1
  else
    Message.Result := 0;
end;

procedure TOBDScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Thumb: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not NeedsBar or
    not PtInRect(BarRect, Point(X, Y)) then
    Exit;
  Thumb := ThumbRect;
  if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
  begin
    FBarDragging := True;
    FBarDragOffset := Y - Thumb.Top;
    Invalidate;
  end
  else if Y < Thumb.Top then
    ScrollTo(FScrollY - Height)
  else
    ScrollTo(FScrollY + Height);
end;

procedure TOBDScrollBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Bar, Thumb: TRect;
  Travel: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FBarDragging then
  begin
    Bar := BarRect;
    Thumb := ThumbRect;
    Travel := Bar.Height - Thumb.Height;
    if Travel > 0 then
      ScrollTo(Round((Y - FBarDragOffset - Bar.Top) / Travel * MaxScroll));
    Exit;
  end;
  SetBarHot(NeedsBar and PtInRect(BarRect, Point(X, Y)));
end;

procedure TOBDScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
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

function TOBDScrollBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result or not NeedsBar then
    Exit;
  ScrollTo(FScrollY - MulDiv(WheelDelta, ScaleValue(FLineStep), WHEEL_DELTA));
  Result := True;
end;

procedure TOBDScrollBox.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  if not NeedsBar then
    Exit;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    OBDPaintScrollBar(P, BarRect, True, 0, ContentHeight - 1, Height,
      FScrollY, FBarHot, FBarDragging);
  finally
    P.Free;
  end;
end;

end.
