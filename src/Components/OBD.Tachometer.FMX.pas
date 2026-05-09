//------------------------------------------------------------------------------
// UNIT           : OBD.Tachometer.FMX.pas
// CONTENTS       : FMX binding for the tachometer
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Lives in `Packages/RunTime.FMX.dpk`. Mirrors the VCL
//                  `TOBDTachometer` API with `TAlphaColor` colours and
//                  delegates rendering to `OBD.Render.Tachometer`.
//------------------------------------------------------------------------------
unit OBD.Tachometer.FMX;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  System.Diagnostics,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.Tachometer;

const
  TCFMX_DEFAULT_MIN: Single = 0;
  TCFMX_DEFAULT_MAX: Single = 8000;
  TCFMX_DEFAULT_REDLINE: Single = 6500;
  TCFMX_DEFAULT_SHIFT_POINT: Single = 6000;
  TCFMX_DEFAULT_MAJOR_TICK = 1000;
  TCFMX_DEFAULT_MINOR_TICK = 200;
  TCFMX_DEFAULT_TICK_LABEL_DIVISOR = 1000;
  TCFMX_DEFAULT_START_ANGLE = 135;
  TCFMX_DEFAULT_SWEEP = 270;
  TCFMX_DEFAULT_BACKGROUND = $FF181818;
  TCFMX_DEFAULT_RING = $FF282828;
  TCFMX_DEFAULT_BORDER = $FF404040;
  TCFMX_DEFAULT_TICK = $FFC0C0C0;
  TCFMX_DEFAULT_REDLINE_COLOR = $FFE63333;
  TCFMX_DEFAULT_NEEDLE = $FFE63333;
  TCFMX_DEFAULT_TEXT = TAlphaColors.White;
  TCFMX_DEFAULT_SHIFT_OFF = $FF282828;
  TCFMX_DEFAULT_SHIFT_ON = $FFE63300;
  TCFMX_DEFAULT_ANIM_DURATION = 200;

type
  TOBDTachometerFMX = class(TSkPaintBox)
  private
    FStopwatch: TStopwatch;
    FAnimationStartMs: Int64;
    FAnimationStartValue: Single;
    FDisplayValue: Single;

    FMin, FMax, FValue: Single;
    FRedlineFrom, FShiftPoint: Single;
    FStartAngle, FSweepAngle: Single;
    FMajorTickInterval, FMinorTickInterval, FTickLabelDivisor: Single;

    FBackgroundColor: TAlphaColor;
    FRingColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FTickColor: TAlphaColor;
    FRedlineColor: TAlphaColor;
    FNeedleColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FShiftLightColorOff: TAlphaColor;
    FShiftLightColorOn: TAlphaColor;

    FCaption: string;
    FUnits: string;
    FShowShiftLight: Boolean;

    FAnimationEnabled: Boolean;
    FAnimationDurationMs: Integer;

    procedure SetMin(const AValue: Single);
    procedure SetMax(const AValue: Single);
    procedure SetValue(const AValue: Single);
    procedure SetRedlineFrom(const AValue: Single);
    procedure SetShiftPoint(const AValue: Single);
    procedure SetStartAngle(const AValue: Single);
    procedure SetSweepAngle(const AValue: Single);
    procedure SetMajorTickInterval(const AValue: Single);
    procedure SetMinorTickInterval(const AValue: Single);
    procedure SetTickLabelDivisor(const AValue: Single);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetRingColor(const AValue: TAlphaColor);
    procedure SetBorderColor(const AValue: TAlphaColor);
    procedure SetTickColor(const AValue: TAlphaColor);
    procedure SetRedlineColor(const AValue: TAlphaColor);
    procedure SetNeedleColor(const AValue: TAlphaColor);
    procedure SetTextColor(const AValue: TAlphaColor);
    procedure SetShiftLightColorOff(const AValue: TAlphaColor);
    procedure SetShiftLightColorOn(const AValue: TAlphaColor);
    procedure SetCaption(const AValue: string);
    procedure SetUnits(const AValue: string);
    procedure SetShowShiftLight(const AValue: Boolean);
    procedure SetAnimationEnabled(const AValue: Boolean);
    procedure SetAnimationDurationMs(const AValue: Integer);

    function EaseOutCubic(T: Single): Single; inline;
    procedure UpdateAnimationValue;

    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  public
    constructor Create(AOwner: TComponent); override;
    function ShiftLightActive: Boolean;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property RedlineFrom: Single read FRedlineFrom write SetRedlineFrom;
    property ShiftPoint: Single read FShiftPoint write SetShiftPoint;
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property SweepAngle: Single read FSweepAngle write SetSweepAngle;
    property MajorTickInterval: Single read FMajorTickInterval write SetMajorTickInterval;
    property MinorTickInterval: Single read FMinorTickInterval write SetMinorTickInterval;
    property TickLabelDivisor: Single read FTickLabelDivisor write SetTickLabelDivisor;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TCFMX_DEFAULT_BACKGROUND;
    property RingColor: TAlphaColor read FRingColor write SetRingColor default TCFMX_DEFAULT_RING;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default TCFMX_DEFAULT_BORDER;
    property TickColor: TAlphaColor read FTickColor write SetTickColor default TCFMX_DEFAULT_TICK;
    property RedlineColor: TAlphaColor read FRedlineColor write SetRedlineColor default TCFMX_DEFAULT_REDLINE_COLOR;
    property NeedleColor: TAlphaColor read FNeedleColor write SetNeedleColor default TCFMX_DEFAULT_NEEDLE;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default TCFMX_DEFAULT_TEXT;
    property ShiftLightColorOff: TAlphaColor read FShiftLightColorOff write SetShiftLightColorOff default TCFMX_DEFAULT_SHIFT_OFF;
    property ShiftLightColorOn: TAlphaColor read FShiftLightColorOn write SetShiftLightColorOn default TCFMX_DEFAULT_SHIFT_ON;
    property Caption: string read FCaption write SetCaption;
    property Units: string read FUnits write SetUnits;
    property ShowShiftLight: Boolean read FShowShiftLight write SetShowShiftLight default True;
    property AnimationEnabled: Boolean read FAnimationEnabled write SetAnimationEnabled default True;
    property AnimationDurationMs: Integer read FAnimationDurationMs write SetAnimationDurationMs default TCFMX_DEFAULT_ANIM_DURATION;
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDTachometerFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStopwatch := TStopwatch.StartNew;

  FMin := TCFMX_DEFAULT_MIN;
  FMax := TCFMX_DEFAULT_MAX;
  FValue := TCFMX_DEFAULT_MIN;
  FDisplayValue := TCFMX_DEFAULT_MIN;
  FAnimationStartValue := TCFMX_DEFAULT_MIN;
  FRedlineFrom := TCFMX_DEFAULT_REDLINE;
  FShiftPoint := TCFMX_DEFAULT_SHIFT_POINT;

  FStartAngle := TCFMX_DEFAULT_START_ANGLE;
  FSweepAngle := TCFMX_DEFAULT_SWEEP;
  FMajorTickInterval := TCFMX_DEFAULT_MAJOR_TICK;
  FMinorTickInterval := TCFMX_DEFAULT_MINOR_TICK;
  FTickLabelDivisor := TCFMX_DEFAULT_TICK_LABEL_DIVISOR;

  FBackgroundColor := TCFMX_DEFAULT_BACKGROUND;
  FRingColor := TCFMX_DEFAULT_RING;
  FBorderColor := TCFMX_DEFAULT_BORDER;
  FTickColor := TCFMX_DEFAULT_TICK;
  FRedlineColor := TCFMX_DEFAULT_REDLINE_COLOR;
  FNeedleColor := TCFMX_DEFAULT_NEEDLE;
  FTextColor := TCFMX_DEFAULT_TEXT;
  FShiftLightColorOff := TCFMX_DEFAULT_SHIFT_OFF;
  FShiftLightColorOn := TCFMX_DEFAULT_SHIFT_ON;

  FCaption := 'RPM';
  FUnits := 'x1000';
  FShowShiftLight := True;

  FAnimationEnabled := True;
  FAnimationDurationMs := TCFMX_DEFAULT_ANIM_DURATION;

  Width := 240;
  Height := 240;

  OnDraw := HandleDraw;
end;

//------------------------------------------------------------------------------
// EASE OUT CUBIC
//------------------------------------------------------------------------------
function TOBDTachometerFMX.EaseOutCubic(T: Single): Single;
begin
  T := 1 - T;
  Result := 1 - (T * T * T);
end;

//------------------------------------------------------------------------------
// UPDATE ANIMATION VALUE
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.UpdateAnimationValue;
var Elapsed: Int64; Progress: Single;
begin
  if not FAnimationEnabled then
  begin
    FDisplayValue := FValue;
    Exit;
  end;
  if FAnimationDurationMs <= 0 then
  begin
    FDisplayValue := FValue;
    Exit;
  end;
  Elapsed := FStopwatch.ElapsedMilliseconds - FAnimationStartMs;
  if Elapsed >= FAnimationDurationMs then
    FDisplayValue := FValue
  else
  begin
    Progress := Elapsed / FAnimationDurationMs;
    FDisplayValue := FAnimationStartValue +
      (FValue - FAnimationStartValue) * EaseOutCubic(Progress);
  end;
end;

//------------------------------------------------------------------------------
// SHIFT LIGHT ACTIVE
//------------------------------------------------------------------------------
function TOBDTachometerFMX.ShiftLightActive: Boolean;
begin
  Result := FShowShiftLight and (FDisplayValue >= FShiftPoint);
end;

//------------------------------------------------------------------------------
// SET MIN
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetMin(const AValue: Single);
begin
  if FMin <> AValue then
  begin
    FMin := AValue;
    if FMax < FMin then FMax := FMin;
    if FValue < FMin then FValue := FMin;
    Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET MAX
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetMax(const AValue: Single);
begin
  if FMax <> AValue then
  begin
    FMax := AValue;
    if FMin > FMax then FMin := FMax;
    if FValue > FMax then FValue := FMax;
    Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET VALUE
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetValue(const AValue: Single);
var
  Clamped: Single;
begin
  Clamped := AValue;
  if Clamped < FMin then Clamped := FMin;
  if Clamped > FMax then Clamped := FMax;
  if FValue = Clamped then Exit;

  if FAnimationEnabled and not (csDesigning in ComponentState) then
  begin
    UpdateAnimationValue;
    FAnimationStartValue := FDisplayValue;
    FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
  end
  else
    FDisplayValue := Clamped;

  FValue := Clamped;
  Redraw;
end;

//------------------------------------------------------------------------------
// SET REDLINE FROM
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetRedlineFrom(const AValue: Single);
begin
  if FRedlineFrom <> AValue then begin FRedlineFrom := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHIFT POINT
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetShiftPoint(const AValue: Single);
begin
  if FShiftPoint <> AValue then begin FShiftPoint := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET START ANGLE
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetStartAngle(const AValue: Single);
begin
  if FStartAngle <> AValue then begin FStartAngle := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SWEEP ANGLE
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetSweepAngle(const AValue: Single);
begin
  if FSweepAngle <> AValue then begin FSweepAngle := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET MAJOR TICK INTERVAL
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetMajorTickInterval(const AValue: Single);
begin
  if (AValue > 0) and (FMajorTickInterval <> AValue) then begin FMajorTickInterval := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET MINOR TICK INTERVAL
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetMinorTickInterval(const AValue: Single);
begin
  if (AValue > 0) and (FMinorTickInterval <> AValue) then begin FMinorTickInterval := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET TICK LABEL DIVISOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetTickLabelDivisor(const AValue: Single);
begin
  if (AValue > 0) and (FTickLabelDivisor <> AValue) then begin FTickLabelDivisor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET RING COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetRingColor(const AValue: TAlphaColor);
begin
  if FRingColor <> AValue then begin FRingColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetBorderColor(const AValue: TAlphaColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET TICK COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetTickColor(const AValue: TAlphaColor);
begin
  if FTickColor <> AValue then begin FTickColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET REDLINE COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetRedlineColor(const AValue: TAlphaColor);
begin
  if FRedlineColor <> AValue then begin FRedlineColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET NEEDLE COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetNeedleColor(const AValue: TAlphaColor);
begin
  if FNeedleColor <> AValue then begin FNeedleColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetTextColor(const AValue: TAlphaColor);
begin
  if FTextColor <> AValue then begin FTextColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHIFT LIGHT COLOR OFF
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetShiftLightColorOff(const AValue: TAlphaColor);
begin
  if FShiftLightColorOff <> AValue then begin FShiftLightColorOff := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHIFT LIGHT COLOR ON
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetShiftLightColorOn(const AValue: TAlphaColor);
begin
  if FShiftLightColorOn <> AValue then begin FShiftLightColorOn := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then begin FCaption := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET UNITS
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetUnits(const AValue: string);
begin
  if FUnits <> AValue then begin FUnits := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW SHIFT LIGHT
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetShowShiftLight(const AValue: Boolean);
begin
  if FShowShiftLight <> AValue then begin FShowShiftLight := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET ANIMATION ENABLED
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetAnimationEnabled(const AValue: Boolean);
begin
  if FAnimationEnabled <> AValue then
  begin
    FAnimationEnabled := AValue;
    if not AValue then FDisplayValue := FValue;
    Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET ANIMATION DURATION MS
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.SetAnimationDurationMs(const AValue: Integer);
begin
  if (AValue >= 0) and (FAnimationDurationMs <> AValue) then begin FAnimationDurationMs := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// HANDLE DRAW
//------------------------------------------------------------------------------
procedure TOBDTachometerFMX.HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  State: TOBDTachometerRenderState;
begin
  UpdateAnimationValue;

  State.Width := ADest.Width;
  State.Height := ADest.Height;
  State.Min := FMin;
  State.Max := FMax;
  State.DisplayValue := FDisplayValue;
  State.RedlineFrom := FRedlineFrom;
  State.ShiftPoint := FShiftPoint;
  State.StartAngle := FStartAngle;
  State.SweepAngle := FSweepAngle;
  State.MajorTickInterval := FMajorTickInterval;
  State.MinorTickInterval := FMinorTickInterval;
  State.TickLabelDivisor := FTickLabelDivisor;
  State.BackgroundColor := FBackgroundColor;
  State.RingColor := FRingColor;
  State.BorderColor := FBorderColor;
  State.TickColor := FTickColor;
  State.RedlineColor := FRedlineColor;
  State.NeedleColor := FNeedleColor;
  State.TextColor := FTextColor;
  State.ShiftLightColorOff := FShiftLightColorOff;
  State.ShiftLightColorOn := FShiftLightColorOn;
  State.Caption := FCaption;
  State.Units := FUnits;
  State.ShowShiftLight := FShowShiftLight;

  RenderTachometer(ACanvas, State);
end;

end.
