//------------------------------------------------------------------------------
// UNIT           : OBD.Knob.FMX.pas
// CONTENTS       : FMX binding for the rotary input knob
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Knob.FMX;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.Knob;

const
  KNOBFMX_DEFAULT_MIN: Single = 0;
  KNOBFMX_DEFAULT_MAX: Single = 100;
  KNOBFMX_DEFAULT_STEP: Single = 1;
  KNOBFMX_DEFAULT_START_ANGLE = 135;
  KNOBFMX_DEFAULT_SWEEP = 270;
  KNOBFMX_DEFAULT_BACKGROUND  = $FF181818;
  KNOBFMX_DEFAULT_BODY        = $FF282828;
  KNOBFMX_DEFAULT_RING        = $FF3A3A3A;
  KNOBFMX_DEFAULT_ACTIVE_RING = $FFE68F1F;
  KNOBFMX_DEFAULT_INDICATOR   = TAlphaColors.White;
  KNOBFMX_DEFAULT_TEXT        = TAlphaColors.White;

type
  TOBDKnobChangeEventFMX = procedure(Sender: TObject; const Value: Single) of object;

  TOBDKnobFMX = class(TSkPaintBox)
  private
    FMin, FMax, FValue, FStep: Single;
    FStartAngle, FSweepAngle: Single;
    FBackgroundColor: TAlphaColor;
    FBodyColor: TAlphaColor;
    FRingColor: TAlphaColor;
    FActiveRingColor: TAlphaColor;
    FIndicatorColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FCaption: string;
    FShowValue: Boolean;
    FDragging: Boolean;
    FOnChange: TOBDKnobChangeEventFMX;

    procedure SetMin(const AValue: Single);
    procedure SetMax(const AValue: Single);
    procedure SetValue(const AValue: Single);
    procedure SetStep(const AValue: Single);
    procedure SetStartAngle(const AValue: Single);
    procedure SetSweepAngle(const AValue: Single);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetBodyColor(const AValue: TAlphaColor);
    procedure SetRingColor(const AValue: TAlphaColor);
    procedure SetActiveRingColor(const AValue: TAlphaColor);
    procedure SetIndicatorColor(const AValue: TAlphaColor);
    procedure SetTextColor(const AValue: TAlphaColor);
    procedure SetCaption(const AValue: string);
    procedure SetShowValue(const AValue: Boolean);

    function PointToValue(const X, Y: Single): Single;
    function NormalizeAngle(A: Single): Single;
    function SnapToStep(const AValue: Single): Single;

    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property Step: Single read FStep write SetStep;
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property SweepAngle: Single read FSweepAngle write SetSweepAngle;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default KNOBFMX_DEFAULT_BACKGROUND;
    property BodyColor: TAlphaColor read FBodyColor write SetBodyColor default KNOBFMX_DEFAULT_BODY;
    property RingColor: TAlphaColor read FRingColor write SetRingColor default KNOBFMX_DEFAULT_RING;
    property ActiveRingColor: TAlphaColor read FActiveRingColor write SetActiveRingColor default KNOBFMX_DEFAULT_ACTIVE_RING;
    property IndicatorColor: TAlphaColor read FIndicatorColor write SetIndicatorColor default KNOBFMX_DEFAULT_INDICATOR;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default KNOBFMX_DEFAULT_TEXT;
    property Caption: string read FCaption write SetCaption;
    property ShowValue: Boolean read FShowValue write SetShowValue default True;
    property OnChange: TOBDKnobChangeEventFMX read FOnChange write FOnChange;
  end;

implementation

constructor TOBDKnobFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := KNOBFMX_DEFAULT_MIN;
  FMax := KNOBFMX_DEFAULT_MAX;
  FValue := KNOBFMX_DEFAULT_MIN;
  FStep := KNOBFMX_DEFAULT_STEP;
  FStartAngle := KNOBFMX_DEFAULT_START_ANGLE;
  FSweepAngle := KNOBFMX_DEFAULT_SWEEP;
  FBackgroundColor := KNOBFMX_DEFAULT_BACKGROUND;
  FBodyColor := KNOBFMX_DEFAULT_BODY;
  FRingColor := KNOBFMX_DEFAULT_RING;
  FActiveRingColor := KNOBFMX_DEFAULT_ACTIVE_RING;
  FIndicatorColor := KNOBFMX_DEFAULT_INDICATOR;
  FTextColor := KNOBFMX_DEFAULT_TEXT;
  FShowValue := True;

  CanFocus := True;
  Width := 96;
  Height := 96;
  OnDraw := HandleDraw;
end;

function TOBDKnobFMX.NormalizeAngle(A: Single): Single;
begin
  while A < 0 do A := A + 360;
  while A >= 360 do A := A - 360;
  Result := A;
end;

function TOBDKnobFMX.SnapToStep(const AValue: Single): Single;
begin
  if FStep <= 0 then Exit(AValue);
  Result := Round((AValue - FMin) / FStep) * FStep + FMin;
end;

function TOBDKnobFMX.PointToValue(const X, Y: Single): Single;
var
  Cx, Cy, Dx, Dy: Single;
  ScreenAngle, RelAngle: Single;
begin
  Cx := Width / 2; Cy := Height / 2;
  Dx := X - Cx; Dy := Y - Cy;
  ScreenAngle := RadToDeg(ArcTan2(Dy, Dx));
  ScreenAngle := NormalizeAngle(ScreenAngle);
  RelAngle := NormalizeAngle(ScreenAngle - FStartAngle);
  if RelAngle > FSweepAngle then
  begin
    if RelAngle - FSweepAngle < 360 - RelAngle then RelAngle := FSweepAngle
    else RelAngle := 0;
  end;
  Result := FMin + (RelAngle / FSweepAngle) * (FMax - FMin);
end;

procedure TOBDKnobFMX.SetMin(const AValue: Single);
begin
  if FMin <> AValue then
  begin
    FMin := AValue;
    if FMax < FMin then FMax := FMin;
    if FValue < FMin then SetValue(FMin) else Redraw;
  end;
end;

procedure TOBDKnobFMX.SetMax(const AValue: Single);
begin
  if FMax <> AValue then
  begin
    FMax := AValue;
    if FMin > FMax then FMin := FMax;
    if FValue > FMax then SetValue(FMax) else Redraw;
  end;
end;

procedure TOBDKnobFMX.SetValue(const AValue: Single);
var Clamped: Single;
begin
  Clamped := SnapToStep(AValue);
  if Clamped < FMin then Clamped := FMin;
  if Clamped > FMax then Clamped := FMax;
  if FValue = Clamped then Exit;
  FValue := Clamped;
  Redraw;
  if Assigned(FOnChange) then FOnChange(Self, FValue);
end;

procedure TOBDKnobFMX.SetStep(const AValue: Single);
begin if (AValue > 0) and (FStep <> AValue) then begin FStep := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetStartAngle(const AValue: Single);
begin if FStartAngle <> AValue then begin FStartAngle := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetSweepAngle(const AValue: Single);
begin if FSweepAngle <> AValue then begin FSweepAngle := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetBodyColor(const AValue: TAlphaColor);
begin if FBodyColor <> AValue then begin FBodyColor := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetRingColor(const AValue: TAlphaColor);
begin if FRingColor <> AValue then begin FRingColor := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetActiveRingColor(const AValue: TAlphaColor);
begin if FActiveRingColor <> AValue then begin FActiveRingColor := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetIndicatorColor(const AValue: TAlphaColor);
begin if FIndicatorColor <> AValue then begin FIndicatorColor := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetTextColor(const AValue: TAlphaColor);
begin if FTextColor <> AValue then begin FTextColor := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetCaption(const AValue: string);
begin if FCaption <> AValue then begin FCaption := AValue; Redraw; end; end;
procedure TOBDKnobFMX.SetShowValue(const AValue: Boolean);
begin if FShowValue <> AValue then begin FShowValue := AValue; Redraw; end; end;

procedure TOBDKnobFMX.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if not IsFocused then SetFocus;
  if Button = TMouseButton.mbLeft then
  begin
    FDragging := True;
    Capture;     // FMX equivalent of MouseCapture := True
    SetValue(PointToValue(X, Y));
  end;
end;

procedure TOBDKnobFMX.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FDragging then SetValue(PointToValue(X, Y));
end;

procedure TOBDKnobFMX.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if FDragging then
  begin
    FDragging := False;
    ReleaseCapture;
  end;
end;

procedure TOBDKnobFMX.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
const
  WHEEL_LINE_DELTA = 120;
begin
  inherited;
  SetValue(FValue + (WheelDelta div WHEEL_LINE_DELTA) * FStep);
  Handled := True;
end;

procedure TOBDKnobFMX.HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  State: TOBDKnobRenderState;
begin
  State.Width := ADest.Width;
  State.Height := ADest.Height;
  State.Min := FMin;
  State.Max := FMax;
  State.Value := FValue;
  State.StartAngle := FStartAngle;
  State.SweepAngle := FSweepAngle;
  State.BackgroundColor := FBackgroundColor;
  State.BodyColor := FBodyColor;
  State.RingColor := FRingColor;
  State.ActiveRingColor := FActiveRingColor;
  State.IndicatorColor := FIndicatorColor;
  State.TextColor := FTextColor;
  State.Caption := FCaption;
  State.ShowValue := FShowValue;
  RenderKnob(ACanvas, State);
end;

end.
