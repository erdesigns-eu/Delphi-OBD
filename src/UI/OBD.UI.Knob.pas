//------------------------------------------------------------------------------
//  OBD.UI.Knob
//
//  TOBDKnob — rotary input control. Drag the knob clockwise to
//  increase the value, counter-clockwise to decrease it. Mouse
//  wheel steps by <c>Step</c>; double-click resets to <c>Default</c>.
//
//  Painted with the standard VCL <c>TCanvas</c> on top of
//  <c>TCustomControl</c> — Windows owns the drawing surface, no
//  third-party renderer in the chain.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Knob.
//------------------------------------------------------------------------------

unit OBD.UI.Knob;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  Winapi.Windows,
  Winapi.Messages;

const
  /// <summary>Lowest value (default 0).</summary>
  KNOB_DEFAULT_MIN         = 0.0;
  /// <summary>Highest value (default 100).</summary>
  KNOB_DEFAULT_MAX         = 100.0;
  /// <summary>Step size for the mouse wheel (default 1).</summary>
  KNOB_DEFAULT_STEP        = 1.0;
  /// <summary>Sweep start angle, degrees from north
  /// (default 135 — 7.30 on a clock face).</summary>
  KNOB_DEFAULT_START_ANGLE = 135;
  /// <summary>Sweep angular extent in degrees (default 270 —
  /// three-quarter arc).</summary>
  KNOB_DEFAULT_SWEEP_ANGLE = 270;

type
  /// <summary>Fires when the value changes. Main thread.</summary>
  TOBDKnobChangeEvent = procedure(Sender: TObject;
    AValue: Single) of object;

  /// <summary>
  ///   Drag-to-rotate input control.
  /// </summary>
  /// <remarks>
  ///   Set <c>Min</c> / <c>Max</c> / <c>Step</c>, optionally set
  ///   <c>StartAngle</c> + <c>SweepAngle</c> for a partial arc.
  ///   <c>Caption</c> and <c>ShowValue</c> drive the body text.
  ///   <c>OnChange</c> fires whenever the value moves.
  /// </remarks>
  TOBDKnob = class(TCustomControl)
  strict private
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FStep: Single;
    FDefault: Single;
    FStartAngle: Integer;
    FSweepAngle: Integer;
    FBodyColor: TColor;
    FRingColor: TColor;
    FActiveRingColor: TColor;
    FIndicatorColor: TColor;
    FTextColor: TColor;
    FCaption: string;
    FShowValue: Boolean;
    FDragging: Boolean;
    FDragAnchorAngle: Single;
    FDragAnchorValue: Single;
    FOnChange: TOBDKnobChangeEvent;
    procedure SetMin(AValue: Single);
    procedure SetMax(AValue: Single);
    procedure SetValue(AValue: Single);
    procedure SetStep(AValue: Single);
    procedure SetStartAngle(AValue: Integer);
    procedure SetSweepAngle(AValue: Integer);
    procedure SetColor(AIndex: Integer; AValue: TColor);
    procedure SetCaption(const AValue: string);
    procedure SetShowValue(AValue: Boolean);
    function ClampValue(AValue: Single): Single;
    function ValueToAngle(AValue: Single): Single;
    function AngleToValue(AAngle: Single): Single;
    function PointToAngle(const APoint: TPoint): Single;
    procedure CMMouseEnter(var Message: TMessage);
      message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage);
      message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Constructs the knob with the published defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Minimum value. Default <c>0</c>.</summary>
    property Min: Single read FMin write SetMin;
    /// <summary>Maximum value. Default <c>100</c>.</summary>
    property Max: Single read FMax write SetMax;
    /// <summary>Current value. Clamped to
    /// <c>[Min, Max]</c>.</summary>
    property Value: Single read FValue write SetValue;
    /// <summary>Step size for mouse-wheel + arrow-key
    /// adjustments.</summary>
    property Step: Single read FStep write SetStep;
    /// <summary>Value restored on double-click. Default
    /// <c>Min</c>.</summary>
    property Default_: Single read FDefault write FDefault;
    /// <summary>Sweep start angle (degrees, 0 = north). Default
    /// <c>135</c>.</summary>
    property StartAngle: Integer read FStartAngle write SetStartAngle
      default KNOB_DEFAULT_START_ANGLE;
    /// <summary>Sweep extent (degrees). Default <c>270</c>.</summary>
    property SweepAngle: Integer read FSweepAngle write SetSweepAngle
      default KNOB_DEFAULT_SWEEP_ANGLE;
    /// <summary>Body fill colour.</summary>
    property BodyColor: TColor index 0 read FBodyColor write SetColor;
    /// <summary>Inactive-arc colour.</summary>
    property RingColor: TColor index 1 read FRingColor write SetColor;
    /// <summary>Active-arc colour (from <c>StartAngle</c> to
    /// the current value).</summary>
    property ActiveRingColor: TColor index 2 read FActiveRingColor
      write SetColor;
    /// <summary>Pointer-line colour.</summary>
    property IndicatorColor: TColor index 3 read FIndicatorColor
      write SetColor;
    /// <summary>Caption / value text colour.</summary>
    property TextColor: TColor index 4 read FTextColor write SetColor;
    /// <summary>Optional caption (rendered above the value).</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Show the numeric value inside the knob. Default
    /// <c>True</c>.</summary>
    property ShowValue: Boolean read FShowValue write SetShowValue
      default True;
    /// <summary>Fires on every value change.</summary>
    property OnChange: TOBDKnobChangeEvent read FOnChange write FOnChange;

    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

constructor TOBDKnob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks,
    csCaptureMouse];
  Width := 96;
  Height := 96;
  TabStop := True;
  FMin := KNOB_DEFAULT_MIN;
  FMax := KNOB_DEFAULT_MAX;
  FValue := FMin;
  FDefault := FMin;
  FStep := KNOB_DEFAULT_STEP;
  FStartAngle := KNOB_DEFAULT_START_ANGLE;
  FSweepAngle := KNOB_DEFAULT_SWEEP_ANGLE;
  FBodyColor := TColor($00282828);
  FRingColor := TColor($003A3A3A);
  FActiveRingColor := TColor($001F8FE6);
  FIndicatorColor := clWhite;
  FTextColor := clWhite;
  FShowValue := True;
end;

procedure TOBDKnob.SetMin(AValue: Single);
begin
  if SameValue(FMin, AValue) then
    Exit;
  FMin := AValue;
  if FMax < FMin then
    FMax := FMin;
  if FDefault < FMin then
    FDefault := FMin;
  FValue := ClampValue(FValue);
  Invalidate;
end;

procedure TOBDKnob.SetMax(AValue: Single);
begin
  if SameValue(FMax, AValue) then
    Exit;
  FMax := AValue;
  if FMin > FMax then
    FMin := FMax;
  if FDefault > FMax then
    FDefault := FMax;
  FValue := ClampValue(FValue);
  Invalidate;
end;

procedure TOBDKnob.SetValue(AValue: Single);
var
  Clamped: Single;
begin
  Clamped := ClampValue(AValue);
  if SameValue(FValue, Clamped) then
    Exit;
  FValue := Clamped;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self, FValue);
end;

procedure TOBDKnob.SetStep(AValue: Single);
begin
  if AValue <= 0 then
    AValue := 1;
  FStep := AValue;
end;

procedure TOBDKnob.SetStartAngle(AValue: Integer);
begin
  if FStartAngle = AValue then
    Exit;
  FStartAngle := AValue;
  Invalidate;
end;

procedure TOBDKnob.SetSweepAngle(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1
  else if AValue > 360 then
    AValue := 360;
  if FSweepAngle = AValue then
    Exit;
  FSweepAngle := AValue;
  Invalidate;
end;

procedure TOBDKnob.SetColor(AIndex: Integer; AValue: TColor);
begin
  case AIndex of
    0: FBodyColor := AValue;
    1: FRingColor := AValue;
    2: FActiveRingColor := AValue;
    3: FIndicatorColor := AValue;
    4: FTextColor := AValue;
  end;
  Invalidate;
end;

procedure TOBDKnob.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

procedure TOBDKnob.SetShowValue(AValue: Boolean);
begin
  if FShowValue = AValue then
    Exit;
  FShowValue := AValue;
  Invalidate;
end;

function TOBDKnob.ClampValue(AValue: Single): Single;
begin
  if AValue < FMin then
    Result := FMin
  else if AValue > FMax then
    Result := FMax
  else
    Result := AValue;
end;

function TOBDKnob.ValueToAngle(AValue: Single): Single;
var
  Frac: Single;
begin
  if SameValue(FMax, FMin) then
    Frac := 0
  else
    Frac := (AValue - FMin) / (FMax - FMin);
  Result := FStartAngle + Frac * FSweepAngle;
end;

function TOBDKnob.AngleToValue(AAngle: Single): Single;
var
  Delta: Single;
  Frac: Single;
begin
  Delta := AAngle - FStartAngle;
  while Delta < 0 do
    Delta := Delta + 360;
  while Delta > 360 do
    Delta := Delta - 360;
  if Delta > FSweepAngle then
  begin
    // Snap to the nearer endpoint when the user drags past the
    // arc boundary.
    if Delta - FSweepAngle > (360 - FSweepAngle) / 2 then
      Delta := 0
    else
      Delta := FSweepAngle;
  end;
  Frac := Delta / FSweepAngle;
  Result := FMin + Frac * (FMax - FMin);
end;

function TOBDKnob.PointToAngle(const APoint: TPoint): Single;
var
  Cx: Single;
  Cy: Single;
  Dx: Single;
  Dy: Single;
  RadAngle: Single;
begin
  Cx := Width / 2;
  Cy := Height / 2;
  Dx := APoint.X - Cx;
  Dy := APoint.Y - Cy;
  RadAngle := ArcTan2(Dx, -Dy);                // 0 = north, +CW
  Result := RadAngle * (180 / PI);
  if Result < 0 then
    Result := Result + 360;
end;

procedure TOBDKnob.Paint;
var
  R: TRect;
  Diameter: Integer;
  CenterX: Integer;
  CenterY: Integer;
  Radius: Integer;
  Pointer_: TPoint;
  PointerAngle: Single;
  Body: TRect;
  CaptionR: TRect;
  ValueR: TRect;
  ValueStr: string;
  ArcRect: TRect;
  StartRad: Double;
  EndRad: Double;
  StartX, StartY, EndX, EndY: Integer;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);

  Diameter := Min(R.Width, R.Height) - 8;
  CenterX := R.Left + R.Width div 2;
  CenterY := R.Top + R.Height div 2;
  Radius := Diameter div 2;

  // Outer inactive ring.
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FRingColor;
  Canvas.Pen.Width := 4;
  Canvas.Ellipse(CenterX - Radius, CenterY - Radius,
                 CenterX + Radius, CenterY + Radius);

  // Active sweep from StartAngle to PointerAngle.
  PointerAngle := ValueToAngle(FValue);
  StartRad := DegToRad(90 - FStartAngle);
  EndRad := DegToRad(90 - PointerAngle);
  Canvas.Pen.Color := FActiveRingColor;
  ArcRect := Rect(CenterX - Radius, CenterY - Radius,
                  CenterX + Radius, CenterY + Radius);
  StartX := CenterX + Round(Radius * Cos(StartRad));
  StartY := CenterY - Round(Radius * Sin(StartRad));
  EndX := CenterX + Round(Radius * Cos(EndRad));
  EndY := CenterY - Round(Radius * Sin(EndRad));
  Canvas.Arc(ArcRect.Left, ArcRect.Top, ArcRect.Right, ArcRect.Bottom,
             EndX, EndY, StartX, StartY);

  // Knob body (filled circle inside the ring).
  Body := Rect(CenterX - Radius + 6, CenterY - Radius + 6,
               CenterX + Radius - 6, CenterY + Radius - 6);
  Canvas.Brush.Color := FBodyColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psClear;
  Canvas.Ellipse(Body);
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;

  // Pointer line from the centre to just inside the ring.
  Canvas.Pen.Color := FIndicatorColor;
  Canvas.Pen.Width := 3;
  Pointer_.X := CenterX + Round((Radius - 8) * Cos(EndRad));
  Pointer_.Y := CenterY - Round((Radius - 8) * Sin(EndRad));
  Canvas.MoveTo(CenterX, CenterY);
  Canvas.LineTo(Pointer_.X, Pointer_.Y);
  Canvas.Pen.Width := 1;

  // Caption + value text.
  Canvas.Font.Assign(Self.Font);
  Canvas.Font.Color := FTextColor;
  Canvas.Brush.Style := bsClear;
  if FCaption <> '' then
  begin
    CaptionR := Rect(R.Left, CenterY - 18, R.Right, CenterY - 4);
    DrawText(Canvas.Handle, PChar(FCaption), Length(FCaption),
      CaptionR, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
  if FShowValue then
  begin
    ValueR := Rect(R.Left, CenterY + 2, R.Right, CenterY + 22);
    ValueStr := FormatFloat('0.##', FValue);
    DrawText(Canvas.Handle, PChar(ValueStr), Length(ValueStr),
      ValueR, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TOBDKnob.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  FDragging := True;
  FDragAnchorAngle := PointToAngle(Point(X, Y));
  FDragAnchorValue := FValue;
  if CanFocus then
    SetFocus;
end;

procedure TOBDKnob.MouseMove(Shift: TShiftState; X: Integer;
  Y: Integer);
var
  CurrentAngle: Single;
  Delta: Single;
  ValueRange: Single;
  NewValue: Single;
begin
  inherited;
  if not FDragging then
    Exit;
  CurrentAngle := PointToAngle(Point(X, Y));
  Delta := CurrentAngle - FDragAnchorAngle;
  while Delta < -180 do
    Delta := Delta + 360;
  while Delta > 180 do
    Delta := Delta - 360;
  ValueRange := FMax - FMin;
  if ValueRange = 0 then
    Exit;
  NewValue := FDragAnchorValue + (Delta / FSweepAngle) * ValueRange;
  SetValue(NewValue);
end;

procedure TOBDKnob.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    FDragging := False;
end;

function TOBDKnob.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if WheelDelta > 0 then
    SetValue(FValue + FStep)
  else
    SetValue(FValue - FStep);
  Result := True;
end;

procedure TOBDKnob.DblClick;
begin
  inherited;
  SetValue(FDefault);
end;

procedure TOBDKnob.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT, VK_DOWN: SetValue(FValue - FStep);
    VK_RIGHT, VK_UP:  SetValue(FValue + FStep);
    VK_HOME:          SetValue(FMin);
    VK_END:           SetValue(FMax);
  end;
end;

procedure TOBDKnob.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TOBDKnob.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FDragging then
    FDragging := False;
end;

end.
