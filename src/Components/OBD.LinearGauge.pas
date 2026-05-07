//------------------------------------------------------------------------------
// UNIT           : OBD.LinearGauge.pas
// CONTENTS       : Linear (bar) gauge component with Skia rendering
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Horizontal or vertical bar gauge, suitable for boost,
//                  fuel level, coolant temperature, oil pressure and
//                  similar single-axis readouts. Renders a rounded-rect
//                  bar with a configurable horizontal/vertical fill
//                  gradient inside a styled background. Value transitions
//                  are eased over time with the same wall-clock pattern
//                  TOBDCircularGauge uses.
//------------------------------------------------------------------------------
unit OBD.LinearGauge;

interface

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.Types, System.UITypes,
  Vcl.Controls, Vcl.Graphics, Vcl.Themes, WinApi.Windows, Winapi.Messages,
  System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers,
  OBD.Render.LinearGauge;

type
  // Re-export the renderer types so existing callers that only `uses
  // OBD.LinearGauge` continue to compile.
  TOBDLinearGaugeOrientation = OBD.Render.LinearGauge.TOBDLinearGaugeOrientation;
  TOBDLinearGaugeDirection = OBD.Render.LinearGauge.TOBDLinearGaugeDirection;

const
  loHorizontal = OBD.Render.LinearGauge.loHorizontal;
  loVertical   = OBD.Render.LinearGauge.loVertical;
  ldNormal     = OBD.Render.LinearGauge.ldNormal;
  ldReversed   = OBD.Render.LinearGauge.ldReversed;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>Default minimum value.</summary>
  LG_DEFAULT_MIN: Single = 0;
  /// <summary>Default maximum value.</summary>
  LG_DEFAULT_MAX: Single = 100;
  /// <summary>Default value-transition duration (milliseconds).</summary>
  LG_DEFAULT_ANIM_DURATION = 350;
  /// <summary>Default corner radius for the bar and frame.</summary>
  LG_DEFAULT_CORNER_RADIUS = 6;
  /// <summary>Default border thickness.</summary>
  LG_DEFAULT_BORDER_WIDTH = 1;
  /// <summary>Default outer padding inside the control bounds.</summary>
  LG_DEFAULT_PADDING = 6;
  /// <summary>Default background fill (matches the rest of the touch UI chrome).</summary>
  LG_DEFAULT_BACKGROUND_COLOR = $00181818;
  /// <summary>Default frame / border colour.</summary>
  LG_DEFAULT_BORDER_COLOR = $00404040;
  /// <summary>Default bar gradient start (cool side — green).</summary>
  LG_DEFAULT_BAR_FROM_COLOR = $0033C033;
  /// <summary>Default bar gradient end (hot side — yellow-red).</summary>
  LG_DEFAULT_BAR_TO_COLOR = $001F8FE6;
  /// <summary>Default text colour.</summary>
  LG_DEFAULT_TEXT_COLOR = clWhite;

//------------------------------------------------------------------------------
// CLASS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Linear (bar) gauge — VCL binding around the framework-neutral
  ///   <c>OBD.Render.LinearGauge</c> renderer.
  /// </summary>
  TOBDLinearGauge = class(TOBDCustomControl)
  private
    FStopwatch: TStopwatch;
    FAnimationStartMs: Int64;
    FAnimationStartValue: Single;
    FDisplayValue: Single;

    FMin: Single;
    FMax: Single;
    FValue: Single;
    FOrientation: TOBDLinearGaugeOrientation;
    FDirection: TOBDLinearGaugeDirection;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FBarColorFrom: TColor;
    FBarColorTo: TColor;
    FCornerRadius: Integer;
    FPadding: Integer;
    FCaption: string;
    FUnits: string;
    FShowValue: Boolean;
    FTextColor: TColor;
    FAnimationEnabled: Boolean;
    FAnimationDurationMs: Integer;

    procedure SetMin(const AValue: Single);
    procedure SetMax(const AValue: Single);
    procedure SetValue(const AValue: Single);
    procedure SetOrientation(const AValue: TOBDLinearGaugeOrientation);
    procedure SetDirection(const AValue: TOBDLinearGaugeDirection);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetBarColorFrom(const AValue: TColor);
    procedure SetBarColorTo(const AValue: TColor);
    procedure SetCornerRadius(const AValue: Integer);
    procedure SetPadding(const AValue: Integer);
    procedure SetCaption(const AValue: string);
    procedure SetUnits(const AValue: string);
    procedure SetShowValue(const AValue: Boolean);
    procedure SetTextColor(const AValue: TColor);
    procedure SetAnimationEnabled(const AValue: Boolean);
    procedure SetAnimationDurationMs(const AValue: Integer);

    /// <summary>
    ///   Easing function used during value transitions — keeps motion soft
    ///   without depending on an external animation framework.
    /// </summary>
    function EaseOutCubic(T: Single): Single; inline;
    /// <summary>
    ///   Map a value to a 0..1 fraction inside [Min..Max] (clamped).
    /// </summary>
    function ValueToFraction(const AValue: Single): Single;

  protected
    procedure UpdateAnimationValue;
    procedure PaintSkia(Canvas: ISkCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Minimum value.</summary>
    property Min: Single read FMin write SetMin;
    /// <summary>Maximum value.</summary>
    property Max: Single read FMax write SetMax;
    /// <summary>Current value (clamped into [Min..Max]).</summary>
    property Value: Single read FValue write SetValue;
    /// <summary>Bar orientation — horizontal or vertical.</summary>
    property Orientation: TOBDLinearGaugeOrientation read FOrientation write SetOrientation default loHorizontal;
    /// <summary>Direction of fill growth.</summary>
    property Direction: TOBDLinearGaugeDirection read FDirection write SetDirection default ldNormal;
    /// <summary>Background fill colour for the unfilled bar.</summary>
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default LG_DEFAULT_BACKGROUND_COLOR;
    /// <summary>Outline colour drawn around the bar.</summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default LG_DEFAULT_BORDER_COLOR;
    /// <summary>Outline thickness in pixels (0 disables the border).</summary>
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default LG_DEFAULT_BORDER_WIDTH;
    /// <summary>Gradient start colour for the filled portion.</summary>
    property BarColorFrom: TColor read FBarColorFrom write SetBarColorFrom default LG_DEFAULT_BAR_FROM_COLOR;
    /// <summary>Gradient end colour for the filled portion.</summary>
    property BarColorTo: TColor read FBarColorTo write SetBarColorTo default LG_DEFAULT_BAR_TO_COLOR;
    /// <summary>Corner radius for the bar and frame.</summary>
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default LG_DEFAULT_CORNER_RADIUS;
    /// <summary>Inner padding between the control edge and the bar.</summary>
    property Padding: Integer read FPadding write SetPadding default LG_DEFAULT_PADDING;
    /// <summary>Optional caption rendered above (horizontal) or beside (vertical) the bar.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Optional units suffix appended to the value text.</summary>
    property Units: string read FUnits write SetUnits;
    /// <summary>If true, render the numeric value next to the bar.</summary>
    property ShowValue: Boolean read FShowValue write SetShowValue default True;
    /// <summary>Colour used for caption + value text.</summary>
    property TextColor: TColor read FTextColor write SetTextColor default LG_DEFAULT_TEXT_COLOR;
    /// <summary>Whether value transitions are animated.</summary>
    property AnimationEnabled: Boolean read FAnimationEnabled write SetAnimationEnabled default True;
    /// <summary>Animation duration in milliseconds.</summary>
    property AnimationDurationMs: Integer read FAnimationDurationMs write SetAnimationDurationMs default LG_DEFAULT_ANIM_DURATION;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDLinearGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStopwatch := TStopwatch.StartNew;

  FMin := LG_DEFAULT_MIN;
  FMax := LG_DEFAULT_MAX;
  FValue := LG_DEFAULT_MIN;
  FDisplayValue := LG_DEFAULT_MIN;
  FAnimationStartValue := LG_DEFAULT_MIN;

  FOrientation := loHorizontal;
  FDirection := ldNormal;
  FBackgroundColor := LG_DEFAULT_BACKGROUND_COLOR;
  FBorderColor := LG_DEFAULT_BORDER_COLOR;
  FBorderWidth := LG_DEFAULT_BORDER_WIDTH;
  FBarColorFrom := LG_DEFAULT_BAR_FROM_COLOR;
  FBarColorTo := LG_DEFAULT_BAR_TO_COLOR;
  FCornerRadius := LG_DEFAULT_CORNER_RADIUS;
  FPadding := LG_DEFAULT_PADDING;
  FShowValue := True;
  FTextColor := LG_DEFAULT_TEXT_COLOR;
  FAnimationEnabled := True;
  FAnimationDurationMs := LG_DEFAULT_ANIM_DURATION;

  Width := 240;
  Height := 64;
end;

//------------------------------------------------------------------------------
// EASING
//------------------------------------------------------------------------------
function TOBDLinearGauge.EaseOutCubic(T: Single): Single;
begin
  T := 1 - T;
  Result := 1 - (T * T * T);
end;

//------------------------------------------------------------------------------
// VALUE → FRACTION
//------------------------------------------------------------------------------
function TOBDLinearGauge.ValueToFraction(const AValue: Single): Single;
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
// UPDATE ANIMATION VALUE
//------------------------------------------------------------------------------
procedure TOBDLinearGauge.UpdateAnimationValue;
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
procedure TOBDLinearGauge.SetMin(const AValue: Single);
begin
  if FMin <> AValue then
  begin
    FMin := AValue;
    if FMax < FMin then FMax := FMin;
    if FValue < FMin then FValue := FMin;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetMax(const AValue: Single);
begin
  if FMax <> AValue then
  begin
    FMax := AValue;
    if FMin > FMax then FMin := FMax;
    if FValue > FMax then FValue := FMax;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetValue(const AValue: Single);
var
  Clamped: Single;
begin
  Clamped := AValue;
  if Clamped < FMin then Clamped := FMin;
  if Clamped > FMax then Clamped := FMax;

  if FValue = Clamped then Exit;

  if FAnimationEnabled and not (csDesigning in ComponentState) then
  begin
    // Snapshot the current eased position so the animation resumes from
    // wherever the bar happens to be drawn right now, even mid-transition.
    UpdateAnimationValue;
    FAnimationStartValue := FDisplayValue;
    FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
  end
  else
    FDisplayValue := Clamped;

  FValue := Clamped;
  Invalidate;
end;

procedure TOBDLinearGauge.SetOrientation(const AValue: TOBDLinearGaugeOrientation);
begin
  if FOrientation <> AValue then
  begin
    FOrientation := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetDirection(const AValue: TOBDLinearGaugeDirection);
begin
  if FDirection <> AValue then
  begin
    FDirection := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetBorderColor(const AValue: TColor);
begin
  if FBorderColor <> AValue then
  begin
    FBorderColor := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetBorderWidth(const AValue: Integer);
begin
  if (FBorderWidth <> AValue) and (AValue >= 0) then
  begin
    FBorderWidth := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetBarColorFrom(const AValue: TColor);
begin
  if FBarColorFrom <> AValue then
  begin
    FBarColorFrom := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetBarColorTo(const AValue: TColor);
begin
  if FBarColorTo <> AValue then
  begin
    FBarColorTo := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetCornerRadius(const AValue: Integer);
begin
  if (FCornerRadius <> AValue) and (AValue >= 0) then
  begin
    FCornerRadius := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetPadding(const AValue: Integer);
begin
  if (FPadding <> AValue) and (AValue >= 0) then
  begin
    FPadding := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetUnits(const AValue: string);
begin
  if FUnits <> AValue then
  begin
    FUnits := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetShowValue(const AValue: Boolean);
begin
  if FShowValue <> AValue then
  begin
    FShowValue := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetTextColor(const AValue: TColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetAnimationEnabled(const AValue: Boolean);
begin
  if FAnimationEnabled <> AValue then
  begin
    FAnimationEnabled := AValue;
    if not AValue then FDisplayValue := FValue;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetAnimationDurationMs(const AValue: Integer);
begin
  if (FAnimationDurationMs <> AValue) and (AValue >= 0) then
  begin
    FAnimationDurationMs := AValue;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// PAINT SKIA
//------------------------------------------------------------------------------
procedure TOBDLinearGauge.PaintSkia(Canvas: ISkCanvas);
var
  State: TOBDLinearGaugeRenderState;
begin
  // Refresh the eased display value before drawing.
  UpdateAnimationValue;

  // Marshal published properties into the framework-neutral state
  // record. The actual painting lives in OBD.Render.LinearGauge so the
  // FMX wrapper can call the same renderer.
  State.Width := Width;
  State.Height := Height;
  State.Min := FMin;
  State.Max := FMax;
  State.DisplayValue := FDisplayValue;
  State.Orientation := FOrientation;
  State.Direction := FDirection;
  State.BackgroundColor := SafeColorRefToSkColor(FBackgroundColor);
  State.BorderColor := SafeColorRefToSkColor(FBorderColor);
  State.BarColorFrom := SafeColorRefToSkColor(FBarColorFrom);
  State.BarColorTo := SafeColorRefToSkColor(FBarColorTo);
  State.TextColor := SafeColorRefToSkColor(FTextColor);
  State.OuterClearColor := SafeColorRefToSkColor(LG_DEFAULT_BACKGROUND_COLOR);
  State.BorderWidth := FBorderWidth;
  State.CornerRadius := FCornerRadius;
  State.Padding := FPadding;
  State.Caption := FCaption;
  State.Units := FUnits;
  State.ShowValue := FShowValue;

  RenderLinearGauge(Canvas, State);
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDLinearGauge.Assign(Source: TPersistent);
var
  Other: TOBDLinearGauge;
begin
  inherited;
  if Source is TOBDLinearGauge then
  begin
    Other := TOBDLinearGauge(Source);
    FMin := Other.FMin;
    FMax := Other.FMax;
    FValue := Other.FValue;
    FDisplayValue := Other.FDisplayValue;
    FOrientation := Other.FOrientation;
    FDirection := Other.FDirection;
    FBackgroundColor := Other.FBackgroundColor;
    FBorderColor := Other.FBorderColor;
    FBorderWidth := Other.FBorderWidth;
    FBarColorFrom := Other.FBarColorFrom;
    FBarColorTo := Other.FBarColorTo;
    FCornerRadius := Other.FCornerRadius;
    FPadding := Other.FPadding;
    FCaption := Other.FCaption;
    FUnits := Other.FUnits;
    FShowValue := Other.FShowValue;
    FTextColor := Other.FTextColor;
    FAnimationEnabled := Other.FAnimationEnabled;
    FAnimationDurationMs := Other.FAnimationDurationMs;
    Invalidate;
  end;
end;

end.
