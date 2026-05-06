//------------------------------------------------------------------------------
// UNIT           : OBD.Knob.pas
// CONTENTS       : Rotary input knob component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Drag-to-rotate input control. Mouse wheel increments by
//                  Step. Drag captures the cursor and maps angle → value
//                  along the configured StartAngle..StartAngle+SweepAngle
//                  arc.
//------------------------------------------------------------------------------
unit OBD.Knob;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  Vcl.Controls, Vcl.Graphics, WinApi.Windows, Winapi.Messages,
  System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers;

const
  KNOB_DEFAULT_MIN: Single = 0;
  KNOB_DEFAULT_MAX: Single = 100;
  KNOB_DEFAULT_STEP: Single = 1;
  KNOB_DEFAULT_START_ANGLE = 135;
  KNOB_DEFAULT_SWEEP = 270;
  KNOB_DEFAULT_BACKGROUND   = $00181818;
  KNOB_DEFAULT_BODY         = $00282828;
  KNOB_DEFAULT_RING         = $003A3A3A;
  KNOB_DEFAULT_ACTIVE_RING  = TColor($001F8FE6);
  KNOB_DEFAULT_INDICATOR    = clWhite;
  KNOB_DEFAULT_TEXT         = clWhite;

type
  TOBDKnobChangeEvent = procedure(Sender: TObject; const Value: Single) of object;

  /// <summary>
  ///   Rotary input. Drag the knob clockwise to increase, counter-clockwise
  ///   to decrease; the mouse wheel steps by <c>Step</c>.
  /// </summary>
  TOBDKnob = class(TOBDCustomControl)
  private
    FMin, FMax, FValue, FStep: Single;
    FStartAngle, FSweepAngle: Single;
    FBackgroundColor: TColor;
    FBodyColor: TColor;
    FRingColor: TColor;
    FActiveRingColor: TColor;
    FIndicatorColor: TColor;
    FTextColor: TColor;
    FCaption: string;
    FShowValue: Boolean;
    FDragging: Boolean;
    FOnChange: TOBDKnobChangeEvent;

    procedure SetMin(const AValue: Single);
    procedure SetMax(const AValue: Single);
    procedure SetValue(const AValue: Single);
    procedure SetStep(const AValue: Single);
    procedure SetStartAngle(const AValue: Single);
    procedure SetSweepAngle(const AValue: Single);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetBodyColor(const AValue: TColor);
    procedure SetRingColor(const AValue: TColor);
    procedure SetActiveRingColor(const AValue: TColor);
    procedure SetIndicatorColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
    procedure SetCaption(const AValue: string);
    procedure SetShowValue(const AValue: Boolean);

    function ValueToFraction(const AValue: Single): Single;
    function PointToValue(const X, Y: Integer): Single;
    procedure ApplyValueFromMouse(X, Y: Integer);
    function NormalizeAngle(A: Single): Single;
    function SnapToStep(const AValue: Single): Single;

  protected
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    /// <summary>Increment / decrement step for the wheel and snapping.</summary>
    property Step: Single read FStep write SetStep;
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property SweepAngle: Single read FSweepAngle write SetSweepAngle;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default KNOB_DEFAULT_BACKGROUND;
    property BodyColor: TColor read FBodyColor write SetBodyColor default KNOB_DEFAULT_BODY;
    property RingColor: TColor read FRingColor write SetRingColor default KNOB_DEFAULT_RING;
    property ActiveRingColor: TColor read FActiveRingColor write SetActiveRingColor default KNOB_DEFAULT_ACTIVE_RING;
    property IndicatorColor: TColor read FIndicatorColor write SetIndicatorColor default KNOB_DEFAULT_INDICATOR;
    property TextColor: TColor read FTextColor write SetTextColor default KNOB_DEFAULT_TEXT;
    property Caption: string read FCaption write SetCaption;
    property ShowValue: Boolean read FShowValue write SetShowValue default True;

    property OnChange: TOBDKnobChangeEvent read FOnChange write FOnChange;
  end;

implementation

constructor TOBDKnob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := KNOB_DEFAULT_MIN;
  FMax := KNOB_DEFAULT_MAX;
  FValue := KNOB_DEFAULT_MIN;
  FStep := KNOB_DEFAULT_STEP;
  FStartAngle := KNOB_DEFAULT_START_ANGLE;
  FSweepAngle := KNOB_DEFAULT_SWEEP;
  FBackgroundColor := KNOB_DEFAULT_BACKGROUND;
  FBodyColor := KNOB_DEFAULT_BODY;
  FRingColor := KNOB_DEFAULT_RING;
  FActiveRingColor := KNOB_DEFAULT_ACTIVE_RING;
  FIndicatorColor := KNOB_DEFAULT_INDICATOR;
  FTextColor := KNOB_DEFAULT_TEXT;
  FShowValue := True;
  FCaption := '';

  TabStop := True;
  Width := 96;
  Height := 96;
end;

//------------------------------------------------------------------------------
// SETTERS
//------------------------------------------------------------------------------
procedure TOBDKnob.SetMin(const AValue: Single);
begin
  if FMin <> AValue then
  begin
    FMin := AValue;
    if FMax < FMin then FMax := FMin;
    if FValue < FMin then SetValue(FMin) else Invalidate;
  end;
end;

procedure TOBDKnob.SetMax(const AValue: Single);
begin
  if FMax <> AValue then
  begin
    FMax := AValue;
    if FMin > FMax then FMin := FMax;
    if FValue > FMax then SetValue(FMax) else Invalidate;
  end;
end;

procedure TOBDKnob.SetValue(const AValue: Single);
var Clamped: Single;
begin
  Clamped := SnapToStep(AValue);
  if Clamped < FMin then Clamped := FMin;
  if Clamped > FMax then Clamped := FMax;
  if FValue = Clamped then Exit;
  FValue := Clamped;
  Invalidate;
  if Assigned(FOnChange) then FOnChange(Self, FValue);
end;

procedure TOBDKnob.SetStep(const AValue: Single);
begin if (AValue > 0) and (FStep <> AValue) then begin FStep := AValue; Invalidate; end; end;

procedure TOBDKnob.SetStartAngle(const AValue: Single);
begin if FStartAngle <> AValue then begin FStartAngle := AValue; Invalidate; end; end;

procedure TOBDKnob.SetSweepAngle(const AValue: Single);
begin if FSweepAngle <> AValue then begin FSweepAngle := AValue; Invalidate; end; end;

procedure TOBDKnob.SetBackgroundColor(const AValue: TColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Invalidate; end; end;

procedure TOBDKnob.SetBodyColor(const AValue: TColor);
begin if FBodyColor <> AValue then begin FBodyColor := AValue; Invalidate; end; end;

procedure TOBDKnob.SetRingColor(const AValue: TColor);
begin if FRingColor <> AValue then begin FRingColor := AValue; Invalidate; end; end;

procedure TOBDKnob.SetActiveRingColor(const AValue: TColor);
begin if FActiveRingColor <> AValue then begin FActiveRingColor := AValue; Invalidate; end; end;

procedure TOBDKnob.SetIndicatorColor(const AValue: TColor);
begin if FIndicatorColor <> AValue then begin FIndicatorColor := AValue; Invalidate; end; end;

procedure TOBDKnob.SetTextColor(const AValue: TColor);
begin if FTextColor <> AValue then begin FTextColor := AValue; Invalidate; end; end;

procedure TOBDKnob.SetCaption(const AValue: string);
begin if FCaption <> AValue then begin FCaption := AValue; Invalidate; end; end;

procedure TOBDKnob.SetShowValue(const AValue: Boolean);
begin if FShowValue <> AValue then begin FShowValue := AValue; Invalidate; end; end;

//------------------------------------------------------------------------------
// MATH
//------------------------------------------------------------------------------
function TOBDKnob.ValueToFraction(const AValue: Single): Single;
var Span: Single;
begin
  Span := FMax - FMin;
  if Span <= 0 then Exit(0);
  Result := (AValue - FMin) / Span;
  if Result < 0 then Result := 0;
  if Result > 1 then Result := 1;
end;

function TOBDKnob.NormalizeAngle(A: Single): Single;
begin
  // Wrap into [0, 360).
  while A < 0 do A := A + 360;
  while A >= 360 do A := A - 360;
  Result := A;
end;

function TOBDKnob.SnapToStep(const AValue: Single): Single;
begin
  if FStep <= 0 then Exit(AValue);
  Result := Round((AValue - FMin) / FStep) * FStep + FMin;
end;

function TOBDKnob.PointToValue(const X, Y: Integer): Single;
var
  Cx, Cy, Dx, Dy: Single;
  ScreenAngle, RelAngle: Single;
begin
  Cx := Width / 2;
  Cy := Height / 2;
  Dx := X - Cx;
  Dy := Y - Cy;
  ScreenAngle := RadToDeg(ArcTan2(Dy, Dx));   // -180..180, 0 = right, +90 = down
  ScreenAngle := NormalizeAngle(ScreenAngle); // 0..360
  RelAngle := NormalizeAngle(ScreenAngle - FStartAngle);
  if RelAngle > FSweepAngle then
  begin
    // Pin to the nearer end of the arc when the cursor is in the dead zone.
    if RelAngle - FSweepAngle < 360 - RelAngle then
      RelAngle := FSweepAngle
    else
      RelAngle := 0;
  end;
  Result := FMin + (RelAngle / FSweepAngle) * (FMax - FMin);
end;

procedure TOBDKnob.ApplyValueFromMouse(X, Y: Integer);
begin
  SetValue(PointToValue(X, Y));
end;

//------------------------------------------------------------------------------
// MOUSE
//------------------------------------------------------------------------------
procedure TOBDKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not Focused then SetFocus;
  if Button = mbLeft then
  begin
    FDragging := True;
    MouseCapture := True;
    ApplyValueFromMouse(X, Y);
  end;
end;

procedure TOBDKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDragging then ApplyValueFromMouse(X, Y);
end;

procedure TOBDKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDragging then
  begin
    FDragging := False;
    MouseCapture := False;
  end;
end;

function TOBDKnob.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  WHEEL_LINE_DELTA = 120;
begin
  SetValue(FValue + (WheelDelta div WHEEL_LINE_DELTA) * FStep);
  Result := True;
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDKnob.PaintSkia(Canvas: ISkCanvas);
var
  Cx, Cy, R, RingThick, BodyR, IndicatorR: Single;
  Frac, AngleRad: Single;
  ArcRect: TRectF;
  Paint: ISkPaint;
  Font: ISkFont;
  Lbl: string;
  W: Single;
begin
  if (Width <= 4) or (Height <= 4) then Exit;

  Cx := Width / 2;
  Cy := Height / 2;
  R := System.Math.Min(Width, Height) / 2 - 4;
  if R < 8 then Exit;

  RingThick := System.Math.Max(3, R * 0.10);
  BodyR := R - RingThick - 4;
  IndicatorR := System.Math.Max(2, R * 0.06);

  // Background.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FBackgroundColor);
  Canvas.DrawCircle(Cx, Cy, R, Paint);

  // Ring background.
  ArcRect := RectF(Cx - R + RingThick / 2, Cy - R + RingThick / 2,
                   Cx + R - RingThick / 2, Cy + R - RingThick / 2);
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := RingThick;
  Paint.StrokeCap := TSkStrokeCap.Round;
  Paint.Color := SafeColorRefToSkColor(FRingColor);
  Canvas.DrawArc(ArcRect, FStartAngle, FSweepAngle, False, Paint);

  // Active arc.
  Frac := ValueToFraction(FValue);
  if Frac > 0 then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := RingThick;
    Paint.StrokeCap := TSkStrokeCap.Round;
    Paint.Color := SafeColorRefToSkColor(FActiveRingColor);
    Canvas.DrawArc(ArcRect, FStartAngle, Frac * FSweepAngle, False, Paint);
  end;

  // Knob body.
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FBodyColor);
  Canvas.DrawCircle(Cx, Cy, BodyR, Paint);

  // Indicator dot near the body edge at value position.
  AngleRad := DegToRad(FStartAngle + Frac * FSweepAngle);
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FIndicatorColor);
  Canvas.DrawCircle(
    Cx + Cos(AngleRad) * (BodyR - IndicatorR - 4),
    Cy + Sin(AngleRad) * (BodyR - IndicatorR - 4),
    IndicatorR, Paint);

  // Caption + numeric value.
  Font := TSkFont.Create(TSkTypeface.MakeDefault, System.Math.Max(10, R * 0.18));
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := SafeColorRefToSkColor(FTextColor);

  if FShowValue then
  begin
    Lbl := FormatFloat('0.##', FValue);
    W := Font.MeasureText(Lbl, Paint);
    Canvas.DrawSimpleText(Lbl, Cx - W / 2, Cy + Font.Size / 3, Font, Paint);
  end;

  if FCaption <> '' then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeDefault, System.Math.Max(9, R * 0.13));
    W := Font.MeasureText(FCaption, Paint);
    Canvas.DrawSimpleText(FCaption, Cx - W / 2, Cy - BodyR + Font.Size + 2,
      Font, Paint);
  end;
end;

end.
