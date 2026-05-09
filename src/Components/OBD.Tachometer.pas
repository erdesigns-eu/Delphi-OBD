//------------------------------------------------------------------------------
// UNIT           : OBD.Tachometer.pas
// CONTENTS       : Tachometer component (analog-style RPM gauge with
//                  redline arc and optional shift light)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Focused dashboard tachometer. For a fully customisable
//                  gauge with bands, captions, and gradient scales use
//                  TOBDCircularGauge — this one trades configurability for
//                  a clean RPM-specific API (redline + shift light).
//------------------------------------------------------------------------------
unit OBD.Tachometer;

interface

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.Types, System.Math,
  System.UITypes, Vcl.Controls, Vcl.Graphics, Vcl.Themes, WinApi.Windows,
  Winapi.Messages, System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.Render.Tachometer;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  TC_DEFAULT_MIN: Single = 0;
  TC_DEFAULT_MAX: Single = 8000;
  TC_DEFAULT_REDLINE: Single = 6500;
  TC_DEFAULT_SHIFT_POINT: Single = 6000;
  TC_DEFAULT_MAJOR_TICK = 1000;
  TC_DEFAULT_MINOR_TICK = 200;
  TC_DEFAULT_TICK_LABEL_DIVISOR = 1000;
  TC_DEFAULT_START_ANGLE = 135;
  TC_DEFAULT_SWEEP = 270;
  TC_DEFAULT_BACKGROUND_COLOR = $00181818;
  TC_DEFAULT_RING_COLOR = $00282828;
  TC_DEFAULT_BORDER_COLOR = $00404040;
  TC_DEFAULT_TICK_COLOR = $00C0C0C0;
  TC_DEFAULT_REDLINE_COLOR = TColor($003333E6);   // RGB E63333 → BGR 3333E6
  TC_DEFAULT_NEEDLE_COLOR = TColor($003333E6);
  TC_DEFAULT_TEXT_COLOR = clWhite;
  TC_DEFAULT_SHIFT_LIGHT_COLOR_OFF = $00282828;
  TC_DEFAULT_SHIFT_LIGHT_COLOR_ON = TColor($000033E6); // amber-ish
  TC_DEFAULT_ANIM_DURATION = 200;

//------------------------------------------------------------------------------
// CLASS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Analog-style tachometer.
  /// </summary>
  TOBDTachometer = class(TOBDCustomControl)
  private
    FStopwatch: TStopwatch;
    FAnimationStartMs: Int64;
    FAnimationStartValue: Single;
    FDisplayValue: Single;

    FMin: Single;
    FMax: Single;
    FValue: Single;
    FRedlineFrom: Single;
    FShiftPoint: Single;

    FStartAngle: Single;
    FSweepAngle: Single;

    FMajorTickInterval: Single;
    FMinorTickInterval: Single;
    FTickLabelDivisor: Single;

    FBackgroundColor: TColor;
    FRingColor: TColor;
    FBorderColor: TColor;
    FTickColor: TColor;
    FRedlineColor: TColor;
    FNeedleColor: TColor;
    FTextColor: TColor;
    FShiftLightColorOff: TColor;
    FShiftLightColorOn: TColor;

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
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetRingColor(const AValue: TColor);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetTickColor(const AValue: TColor);
    procedure SetRedlineColor(const AValue: TColor);
    procedure SetNeedleColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
    procedure SetShiftLightColorOff(const AValue: TColor);
    procedure SetShiftLightColorOn(const AValue: TColor);
    procedure SetCaption(const AValue: string);
    procedure SetUnits(const AValue: string);
    procedure SetShowShiftLight(const AValue: Boolean);
    procedure SetAnimationEnabled(const AValue: Boolean);
    procedure SetAnimationDurationMs(const AValue: Integer);

    function ValueToFraction(const AValue: Single): Single;
    function EaseOutCubic(T: Single): Single; inline;

  protected
    procedure UpdateAnimationValue;
    procedure PaintSkia(Canvas: ISkCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   True while the current eased <c>Value</c> is at or above
    ///   <c>ShiftPoint</c>. Useful for driving an external LED or beeper.
    /// </summary>
    function ShiftLightActive: Boolean;

    procedure Assign(Source: TPersistent); override;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    /// <summary>
    ///   RPM at which the redline arc starts.
    /// </summary>
    property RedlineFrom: Single read FRedlineFrom write SetRedlineFrom;
    /// <summary>
    ///   RPM at which the shift light fires.
    /// </summary>
    property ShiftPoint: Single read FShiftPoint write SetShiftPoint;

    property StartAngle: Single read FStartAngle write SetStartAngle;
    property SweepAngle: Single read FSweepAngle write SetSweepAngle;

    property MajorTickInterval: Single read FMajorTickInterval write SetMajorTickInterval;
    property MinorTickInterval: Single read FMinorTickInterval write SetMinorTickInterval;
    /// <summary>
    ///   Numeric divisor applied to tick labels (1000 → "0..8" instead of "0..8000").
    /// </summary>
    property TickLabelDivisor: Single read FTickLabelDivisor write SetTickLabelDivisor;

    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default TC_DEFAULT_BACKGROUND_COLOR;
    property RingColor: TColor read FRingColor write SetRingColor default TC_DEFAULT_RING_COLOR;
    property BorderColor: TColor read FBorderColor write SetBorderColor default TC_DEFAULT_BORDER_COLOR;
    property TickColor: TColor read FTickColor write SetTickColor default TC_DEFAULT_TICK_COLOR;
    property RedlineColor: TColor read FRedlineColor write SetRedlineColor default TC_DEFAULT_REDLINE_COLOR;
    property NeedleColor: TColor read FNeedleColor write SetNeedleColor default TC_DEFAULT_NEEDLE_COLOR;
    property TextColor: TColor read FTextColor write SetTextColor default TC_DEFAULT_TEXT_COLOR;
    property ShiftLightColorOff: TColor read FShiftLightColorOff write SetShiftLightColorOff default TC_DEFAULT_SHIFT_LIGHT_COLOR_OFF;
    property ShiftLightColorOn: TColor read FShiftLightColorOn write SetShiftLightColorOn default TC_DEFAULT_SHIFT_LIGHT_COLOR_ON;

    property Caption: string read FCaption write SetCaption;
    property Units: string read FUnits write SetUnits;
    property ShowShiftLight: Boolean read FShowShiftLight write SetShowShiftLight default True;

    property AnimationEnabled: Boolean read FAnimationEnabled write SetAnimationEnabled default True;
    property AnimationDurationMs: Integer read FAnimationDurationMs write SetAnimationDurationMs default TC_DEFAULT_ANIM_DURATION;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTachometer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStopwatch := TStopwatch.StartNew;

  FMin := TC_DEFAULT_MIN;
  FMax := TC_DEFAULT_MAX;
  FValue := TC_DEFAULT_MIN;
  FDisplayValue := TC_DEFAULT_MIN;
  FAnimationStartValue := TC_DEFAULT_MIN;
  FRedlineFrom := TC_DEFAULT_REDLINE;
  FShiftPoint := TC_DEFAULT_SHIFT_POINT;

  FStartAngle := TC_DEFAULT_START_ANGLE;
  FSweepAngle := TC_DEFAULT_SWEEP;
  FMajorTickInterval := TC_DEFAULT_MAJOR_TICK;
  FMinorTickInterval := TC_DEFAULT_MINOR_TICK;
  FTickLabelDivisor := TC_DEFAULT_TICK_LABEL_DIVISOR;

  FBackgroundColor := TC_DEFAULT_BACKGROUND_COLOR;
  FRingColor := TC_DEFAULT_RING_COLOR;
  FBorderColor := TC_DEFAULT_BORDER_COLOR;
  FTickColor := TC_DEFAULT_TICK_COLOR;
  FRedlineColor := TC_DEFAULT_REDLINE_COLOR;
  FNeedleColor := TC_DEFAULT_NEEDLE_COLOR;
  FTextColor := TC_DEFAULT_TEXT_COLOR;
  FShiftLightColorOff := TC_DEFAULT_SHIFT_LIGHT_COLOR_OFF;
  FShiftLightColorOn := TC_DEFAULT_SHIFT_LIGHT_COLOR_ON;

  FCaption := 'RPM';
  FUnits := 'x1000';
  FShowShiftLight := True;

  FAnimationEnabled := True;
  FAnimationDurationMs := TC_DEFAULT_ANIM_DURATION;

  Width := 240;
  Height := 240;
end;

//------------------------------------------------------------------------------
// EASING + FRACTION
//------------------------------------------------------------------------------
function TOBDTachometer.EaseOutCubic(T: Single): Single;
begin
  T := 1 - T;
  Result := 1 - (T * T * T);
end;

//------------------------------------------------------------------------------
// VALUE TO FRACTION
//------------------------------------------------------------------------------
function TOBDTachometer.ValueToFraction(const AValue: Single): Single;
var
  Span: Single;
begin
  Span := FMax - FMin;
  if Span <= 0 then Exit(0);
  Result := (AValue - FMin) / Span;
  if Result < 0 then Result := 0;
  if Result > 1 then Result := 1;
end;

//------------------------------------------------------------------------------
// SHIFT LIGHT
//------------------------------------------------------------------------------
function TOBDTachometer.ShiftLightActive: Boolean;
begin
  Result := FShowShiftLight and (FDisplayValue >= FShiftPoint);
end;

//------------------------------------------------------------------------------
// UPDATE ANIMATION VALUE
//------------------------------------------------------------------------------
procedure TOBDTachometer.UpdateAnimationValue;
var
  Elapsed: Int64;
  Progress: Single;
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
// SETTERS
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetMin(const AValue: Single);
begin
  if FMin <> AValue then
  begin
    FMin := AValue;
    if FMax < FMin then FMax := FMin;
    if FValue < FMin then FValue := FMin;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET MAX
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetMax(const AValue: Single);
begin
  if FMax <> AValue then
  begin
    FMax := AValue;
    if FMin > FMax then FMin := FMax;
    if FValue > FMax then FValue := FMax;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET VALUE
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetValue(const AValue: Single);
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
  Invalidate;
end;

//------------------------------------------------------------------------------
// SET REDLINE FROM
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetRedlineFrom(const AValue: Single);
begin
  if FRedlineFrom <> AValue then begin FRedlineFrom := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHIFT POINT
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetShiftPoint(const AValue: Single);
begin
  if FShiftPoint <> AValue then begin FShiftPoint := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET START ANGLE
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetStartAngle(const AValue: Single);
begin
  if FStartAngle <> AValue then begin FStartAngle := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SWEEP ANGLE
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetSweepAngle(const AValue: Single);
begin
  if FSweepAngle <> AValue then begin FSweepAngle := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET MAJOR TICK INTERVAL
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetMajorTickInterval(const AValue: Single);
begin
  if (FMajorTickInterval <> AValue) and (AValue > 0) then begin FMajorTickInterval := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET MINOR TICK INTERVAL
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetMinorTickInterval(const AValue: Single);
begin
  if (FMinorTickInterval <> AValue) and (AValue > 0) then begin FMinorTickInterval := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET TICK LABEL DIVISOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetTickLabelDivisor(const AValue: Single);
begin
  if (FTickLabelDivisor <> AValue) and (AValue > 0) then begin FTickLabelDivisor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET RING COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetRingColor(const AValue: TColor);
begin
  if FRingColor <> AValue then begin FRingColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetBorderColor(const AValue: TColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET TICK COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetTickColor(const AValue: TColor);
begin
  if FTickColor <> AValue then begin FTickColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET REDLINE COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetRedlineColor(const AValue: TColor);
begin
  if FRedlineColor <> AValue then begin FRedlineColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET NEEDLE COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetNeedleColor(const AValue: TColor);
begin
  if FNeedleColor <> AValue then begin FNeedleColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetTextColor(const AValue: TColor);
begin
  if FTextColor <> AValue then begin FTextColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHIFT LIGHT COLOR OFF
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetShiftLightColorOff(const AValue: TColor);
begin
  if FShiftLightColorOff <> AValue then begin FShiftLightColorOff := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHIFT LIGHT COLOR ON
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetShiftLightColorOn(const AValue: TColor);
begin
  if FShiftLightColorOn <> AValue then begin FShiftLightColorOn := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then begin FCaption := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET UNITS
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetUnits(const AValue: string);
begin
  if FUnits <> AValue then begin FUnits := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW SHIFT LIGHT
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetShowShiftLight(const AValue: Boolean);
begin
  if FShowShiftLight <> AValue then begin FShowShiftLight := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET ANIMATION ENABLED
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetAnimationEnabled(const AValue: Boolean);
begin
  if FAnimationEnabled <> AValue then
  begin
    FAnimationEnabled := AValue;
    if not AValue then FDisplayValue := FValue;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET ANIMATION DURATION MS
//------------------------------------------------------------------------------
procedure TOBDTachometer.SetAnimationDurationMs(const AValue: Integer);
begin
  if (FAnimationDurationMs <> AValue) and (AValue >= 0) then begin FAnimationDurationMs := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// PAINT SKIA
//------------------------------------------------------------------------------
procedure TOBDTachometer.PaintSkia(Canvas: ISkCanvas);
var
  State: TOBDTachometerRenderState;
begin
  UpdateAnimationValue;

  // Marshal published properties into the framework-neutral state record.
  // Painting itself lives in OBD.Render.Tachometer so the FMX wrapper
  // calls the same renderer.
  State.Width := Width;
  State.Height := Height;
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
  State.BackgroundColor := SafeColorRefToSkColor(FBackgroundColor);
  State.RingColor := SafeColorRefToSkColor(FRingColor);
  State.BorderColor := SafeColorRefToSkColor(FBorderColor);
  State.TickColor := SafeColorRefToSkColor(FTickColor);
  State.RedlineColor := SafeColorRefToSkColor(FRedlineColor);
  State.NeedleColor := SafeColorRefToSkColor(FNeedleColor);
  State.TextColor := SafeColorRefToSkColor(FTextColor);
  State.ShiftLightColorOff := SafeColorRefToSkColor(FShiftLightColorOff);
  State.ShiftLightColorOn := SafeColorRefToSkColor(FShiftLightColorOn);
  State.Caption := FCaption;
  State.Units := FUnits;
  State.ShowShiftLight := FShowShiftLight;

  RenderTachometer(Canvas, State);
end;


//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTachometer.Assign(Source: TPersistent);
var
  Other: TOBDTachometer;
begin
  inherited;
  if Source is TOBDTachometer then
  begin
    Other := TOBDTachometer(Source);
    FMin := Other.FMin;
    FMax := Other.FMax;
    FValue := Other.FValue;
    FDisplayValue := Other.FDisplayValue;
    FRedlineFrom := Other.FRedlineFrom;
    FShiftPoint := Other.FShiftPoint;
    FStartAngle := Other.FStartAngle;
    FSweepAngle := Other.FSweepAngle;
    FMajorTickInterval := Other.FMajorTickInterval;
    FMinorTickInterval := Other.FMinorTickInterval;
    FTickLabelDivisor := Other.FTickLabelDivisor;
    FBackgroundColor := Other.FBackgroundColor;
    FRingColor := Other.FRingColor;
    FBorderColor := Other.FBorderColor;
    FTickColor := Other.FTickColor;
    FRedlineColor := Other.FRedlineColor;
    FNeedleColor := Other.FNeedleColor;
    FTextColor := Other.FTextColor;
    FShiftLightColorOff := Other.FShiftLightColorOff;
    FShiftLightColorOn := Other.FShiftLightColorOn;
    FCaption := Other.FCaption;
    FUnits := Other.FUnits;
    FShowShiftLight := Other.FShowShiftLight;
    FAnimationEnabled := Other.FAnimationEnabled;
    FAnimationDurationMs := Other.FAnimationDurationMs;
    Invalidate;
  end;
end;

end.
