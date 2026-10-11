//------------------------------------------------------------------------------
//  ERD.UI.Splitter
//
//  TOBDSplitter - themed drop-in for TSplitter. Align it next to an
//  aligned control (alLeft / alRight / alTop / alBottom, like TSplitter);
//  dragging it resizes that control live. The bar paints in the surface
//  colour it sits on with a small grip, a divider line on hover and an
//  orange line while it is dragged.
//
//  The thickness follows the density (6 px desktop, 12 px tablet, so it
//  is easy to grab with a finger) unless Thickness is set. MinSize keeps
//  the resized control and the area on the other side from collapsing.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Splitter;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Themed splitter bar that resizes the aligned control next
  /// to it.</summary>
  TOBDSplitter = class(TOBDCustomControl)
  strict private
    FControl: TControl;
    FMinSize: Integer;
    FThickness: Integer;
    FHover: Boolean;
    FDragging: Boolean;
    FApplying: Boolean;
    FDownPos: TPoint;
    FStartSize: Integer;
    FMaxSize: Integer;
    FOnMoved: TNotifyEvent;
    procedure SetMinSize(AValue: Integer);
    procedure SetThickness(AValue: Integer);
    function IsVertical: Boolean;
    function FindControl: TControl;
    function AvailableSize: Integer;
    procedure ApplyThickness;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the grip, the hover line and the drag line.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Follows a new Align with the cursor and the thickness.
    /// </summary>
    procedure RequestAlign; override;
    /// <summary>Applies the thickness after the form is loaded.</summary>
    procedure Loaded; override;
    /// <summary>Starts a drag when the resized control is found.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Resizes the control while dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a drag and fires <see cref="OnMoved"/>.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    /// <summary>Creates a left-aligned splitter.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Applies the density thickness.</summary>
    procedure DensityChanged; override;
    /// <summary>Control the splitter resizes: the visible control right
    /// before it in the align order (left of an alLeft splitter, above
    /// an alTop splitter, and so on), or nil.</summary>
    /// <returns>Resized control or nil.</returns>
    function ResizedControl: TControl;
    /// <summary>True while the bar is dragged (TControl.Dragging is the
    /// drag-and-drop state).</summary>
    property Sizing: Boolean read FDragging;
  published
    /// <summary>Side the splitter sits on; the resized control has the
    /// same alignment.</summary>
    property Align default alLeft;
    /// <summary>Smallest size, in 96-DPI pixels, of the resized control
    /// and of the space left on the other side.</summary>
    property MinSize: Integer read FMinSize write SetMinSize default 60;
    /// <summary>Bar thickness in 96-DPI pixels; 0 follows the density
    /// (6 px desktop, 12 px tablet).</summary>
    property Thickness: Integer read FThickness write SetThickness default 0;
    /// <summary>Desktop or tablet thickness.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires on the main thread after a drag changed the size.
    /// </summary>
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  end;

implementation

const
  THICKNESS_DESKTOP = 6;
  THICKNESS_TABLET = 12;

{ TOBDSplitter }

constructor TOBDSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FMinSize := 60;
  Align := alLeft;
  Width := THICKNESS_DESKTOP;
  Cursor := crHSplit;
  TabStop := False;
end;

function TOBDSplitter.IsVertical: Boolean;
begin
  // A vertical bar (alLeft / alRight) resizes widths.
  Result := Align in [alLeft, alRight];
end;

procedure TOBDSplitter.SetMinSize(AValue: Integer);
begin
  FMinSize := Max(0, AValue);
end;

procedure TOBDSplitter.SetThickness(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FThickness = AValue then
    Exit;
  FThickness := AValue;
  ApplyThickness;
end;

procedure TOBDSplitter.ApplyThickness;
var
  T: Integer;
begin
  if FApplying or (csLoading in ComponentState) then
    Exit;
  if FThickness > 0 then
    T := ScaleValue(FThickness)
  else if Density = dnTablet then
    T := ScaleValue(THICKNESS_TABLET)
  else
    T := ScaleValue(THICKNESS_DESKTOP);
  FApplying := True;
  try
    if IsVertical and (Width <> T) then
      Width := T
    else if (Align in [alTop, alBottom]) and (Height <> T) then
      Height := T;
  finally
    FApplying := False;
  end;
end;

procedure TOBDSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if FApplying then
    Exit;
  if Align in [alTop, alBottom] then
    Cursor := crVSplit
  else if IsVertical then
    Cursor := crHSplit;
  ApplyThickness;
end;

procedure TOBDSplitter.Loaded;
begin
  inherited Loaded;
  ApplyThickness;
end;

procedure TOBDSplitter.DensityChanged;
begin
  ApplyThickness;
  inherited DensityChanged;
end;

function TOBDSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  C: TControl;
  R: TRect;
begin
  Result := nil;
  if Parent = nil then
    Exit;
  // A point just outside the edge the resized control touches.
  case Align of
    alLeft:
      P := Point(Left - 1, Top + Height div 2);
    alRight:
      P := Point(Left + Width, Top + Height div 2);
    alTop:
      P := Point(Left + Width div 2, Top - 1);
    alBottom:
      P := Point(Left + Width div 2, Top + Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    C := Parent.Controls[I];
    if (C = Self) or not C.Visible or (C.Align <> Align) then
      Continue;
    R := C.BoundsRect;
    // Margins of AlignWithMargins controls belong to them.
    if C.AlignWithMargins then
    begin
      Dec(R.Left, C.Margins.Left);
      Dec(R.Top, C.Margins.Top);
      Inc(R.Right, C.Margins.Right);
      Inc(R.Bottom, C.Margins.Bottom);
    end;
    if R.Width = 0 then
      Inc(R.Right);
    if R.Height = 0 then
      Inc(R.Bottom);
    if PtInRect(R, P) then
      Exit(C);
  end;
end;

function TOBDSplitter.ResizedControl: TControl;
begin
  Result := FindControl;
end;

function TOBDSplitter.AvailableSize: Integer;
var
  I: Integer;
  C: TControl;
begin
  // Parent client size minus every other control stacked on the same
  // axis; what is left is shared by the resized control and the area on
  // the other side.
  if IsVertical then
    Result := Parent.ClientWidth
  else
    Result := Parent.ClientHeight;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    C := Parent.Controls[I];
    if (C = FControl) or not C.Visible then
      Continue;
    if IsVertical and (C.Align in [alLeft, alRight]) then
      Dec(Result, C.Width)
    else if not IsVertical and (C.Align in [alTop, alBottom]) then
      Dec(Result, C.Height);
  end;
end;

procedure TOBDSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not Enabled then
    Exit;
  FControl := FindControl;
  if FControl = nil then
    Exit;
  FDownPos := Mouse.CursorPos;
  if IsVertical then
    FStartSize := FControl.Width
  else
    FStartSize := FControl.Height;
  FMaxSize := Max(ScaleValue(FMinSize), AvailableSize - ScaleValue(FMinSize));
  FDragging := True;
  Invalidate;
end;

procedure TOBDSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pos: TPoint;
  Delta, NewSize: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if not FDragging or (FControl = nil) then
    Exit;
  // Screen coordinates: the splitter itself moves while it is dragged.
  Pos := Mouse.CursorPos;
  if IsVertical then
    Delta := Pos.X - FDownPos.X
  else
    Delta := Pos.Y - FDownPos.Y;
  if Align in [alRight, alBottom] then
    Delta := -Delta;
  NewSize := EnsureRange(FStartSize + Delta, ScaleValue(FMinSize), FMaxSize);
  if IsVertical and (FControl.Width <> NewSize) then
    FControl.Width := NewSize
  else if not IsVertical and (FControl.Height <> NewSize) then
    FControl.Height := NewSize
  else
    Exit;
  // Paint the resized layout now instead of when the drag pauses.
  if (Parent <> nil) and Parent.HandleAllocated then
    RedrawWindow(Parent.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or
      RDW_ALLCHILDREN);
end;

procedure TOBDSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Moved: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not FDragging then
    Exit;
  FDragging := False;
  Moved := False;
  if FControl <> nil then
    if IsVertical then
      Moved := FControl.Width <> FStartSize
    else
      Moved := FControl.Height <> FStartSize;
  FControl := nil;
  Invalidate;
  if Moved and Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TOBDSplitter.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHover := True;
  Invalidate;
end;

procedure TOBDSplitter.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHover := False;
  Invalidate;
end;

procedure TOBDSplitter.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Line, Dot: TColor;
  I: Integer;
  D, CX, CY, Gap: Single;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if FDragging then
      Line := P.AccentText
    else if FHover and Enabled then
      Line := Palette.NeutralLight
    else
      Line := clNone;
    if Line <> clNone then
      if IsVertical then
        P.FillRect(Rect(Width div 2 - P.S(1), 0, Width div 2 + P.S(1),
          Height), Line)
      else
        P.FillRect(Rect(0, Height div 2 - P.S(1), Width, Height div 2 +
          P.S(1)), Line);
    // Grip: three dots across the middle of the bar.
    if FDragging then
      Dot := P.AccentText
    else if FHover and Enabled then
      Dot := Palette.ForegroundText
    else
      Dot := Palette.Subtle;
    D := P.SF(3);
    Gap := P.SF(5);
    for I := -1 to 1 do
    begin
      if IsVertical then
      begin
        CX := Width / 2;
        CY := Height / 2 + I * Gap;
      end
      else
      begin
        CX := Width / 2 + I * Gap;
        CY := Height / 2;
      end;
      P.Ellipse(CX - D / 2, CY - D / 2, D, D, Dot, clNone);
    end;
  finally
    P.Free;
  end;
end;

end.
