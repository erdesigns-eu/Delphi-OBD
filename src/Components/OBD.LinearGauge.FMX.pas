//------------------------------------------------------------------------------
// UNIT           : OBD.LinearGauge.FMX.pas
// CONTENTS       : FMX binding for the linear gauge
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : FMX-side binding around `OBD.Render.LinearGauge`. The
//                  rendering logic is shared with the VCL component
//                  (`OBD.LinearGauge`); this unit only handles property
//                  storage, the FMX paint event, and (eventual)
//                  cross-platform value-transition timing via TTimer.
//
//                  Lives in `Packages/RunTime.FMX.dpk`. Do NOT add this
//                  unit to the VCL `RunTime.dpk` — Skia.FMX and Vcl.Skia
//                  conflict at link time.
//------------------------------------------------------------------------------
unit OBD.LinearGauge.FMX;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  System.Diagnostics,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.LinearGauge;

const
  LGFMX_DEFAULT_MIN: Single = 0;
  LGFMX_DEFAULT_MAX: Single = 100;
  LGFMX_DEFAULT_ANIM_DURATION = 350;
  LGFMX_DEFAULT_BACKGROUND = $FF181818;
  LGFMX_DEFAULT_BORDER = $FF404040;
  LGFMX_DEFAULT_BAR_FROM = $FF33C033;
  LGFMX_DEFAULT_BAR_TO = $FF1F8FE6;
  LGFMX_DEFAULT_TEXT = TAlphaColors.White;

type
  /// <summary>
  ///   Linear (bar) gauge for FMX. Drop on a form, set
  ///   <c>Min</c>/<c>Max</c>/<c>Value</c>; FMX layout drives the size.
  /// </summary>
  TOBDLinearGaugeFMX = class(TSkPaintBox)
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
    FBackgroundColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FBarColorFrom: TAlphaColor;
    FBarColorTo: TAlphaColor;
    FTextColor: TAlphaColor;
    FBorderWidth: Single;
    FCornerRadius: Single;
    FPadding: Single;
    FCaption: string;
    FUnits: string;
    FShowValue: Boolean;
    FAnimationEnabled: Boolean;
    FAnimationDurationMs: Integer;

    procedure SetMin(const AValue: Single);
    procedure SetMax(const AValue: Single);
    procedure SetValue(const AValue: Single);
    procedure SetOrientation(const AValue: TOBDLinearGaugeOrientation);
    procedure SetDirection(const AValue: TOBDLinearGaugeDirection);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetBorderColor(const AValue: TAlphaColor);
    procedure SetBarColorFrom(const AValue: TAlphaColor);
    procedure SetBarColorTo(const AValue: TAlphaColor);
    procedure SetTextColor(const AValue: TAlphaColor);
    procedure SetBorderWidth(const AValue: Single);
    procedure SetCornerRadius(const AValue: Single);
    procedure SetPadding(const AValue: Single);
    procedure SetCaption(const AValue: string);
    procedure SetUnits(const AValue: string);
    procedure SetShowValue(const AValue: Boolean);
    procedure SetAnimationEnabled(const AValue: Boolean);
    procedure SetAnimationDurationMs(const AValue: Integer);

    function EaseOutCubic(T: Single): Single; inline;
    procedure UpdateAnimationValue;

    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property Orientation: TOBDLinearGaugeOrientation read FOrientation write SetOrientation default loHorizontal;
    property Direction: TOBDLinearGaugeDirection read FDirection write SetDirection default ldNormal;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default LGFMX_DEFAULT_BACKGROUND;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default LGFMX_DEFAULT_BORDER;
    property BarColorFrom: TAlphaColor read FBarColorFrom write SetBarColorFrom default LGFMX_DEFAULT_BAR_FROM;
    property BarColorTo: TAlphaColor read FBarColorTo write SetBarColorTo default LGFMX_DEFAULT_BAR_TO;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default LGFMX_DEFAULT_TEXT;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property CornerRadius: Single read FCornerRadius write SetCornerRadius;
    property Padding: Single read FPadding write SetPadding;
    property Caption: string read FCaption write SetCaption;
    property Units: string read FUnits write SetUnits;
    property ShowValue: Boolean read FShowValue write SetShowValue default True;
    property AnimationEnabled: Boolean read FAnimationEnabled write SetAnimationEnabled default True;
    property AnimationDurationMs: Integer read FAnimationDurationMs write SetAnimationDurationMs default LGFMX_DEFAULT_ANIM_DURATION;
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDLinearGaugeFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStopwatch := TStopwatch.StartNew;

  FMin := LGFMX_DEFAULT_MIN;
  FMax := LGFMX_DEFAULT_MAX;
  FValue := LGFMX_DEFAULT_MIN;
  FDisplayValue := LGFMX_DEFAULT_MIN;
  FAnimationStartValue := LGFMX_DEFAULT_MIN;

  FOrientation := loHorizontal;
  FDirection := ldNormal;
  FBackgroundColor := LGFMX_DEFAULT_BACKGROUND;
  FBorderColor := LGFMX_DEFAULT_BORDER;
  FBarColorFrom := LGFMX_DEFAULT_BAR_FROM;
  FBarColorTo := LGFMX_DEFAULT_BAR_TO;
  FTextColor := LGFMX_DEFAULT_TEXT;
  FBorderWidth := 1;
  FCornerRadius := 6;
  FPadding := 6;
  FShowValue := True;
  FAnimationEnabled := True;
  FAnimationDurationMs := LGFMX_DEFAULT_ANIM_DURATION;

  Width := 240;
  Height := 64;

  // FMX exposes painting through the OnDraw event on TSkPaintBox.
  OnDraw := HandleDraw;
end;

//------------------------------------------------------------------------------
// EASE OUT CUBIC
//------------------------------------------------------------------------------
function TOBDLinearGaugeFMX.EaseOutCubic(T: Single): Single;
begin
  T := 1 - T;
  Result := 1 - (T * T * T);
end;

//------------------------------------------------------------------------------
// UPDATE ANIMATION VALUE
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.UpdateAnimationValue;
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
procedure TOBDLinearGaugeFMX.SetMin(const AValue: Single);
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
procedure TOBDLinearGaugeFMX.SetMax(const AValue: Single);
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
procedure TOBDLinearGaugeFMX.SetValue(const AValue: Single);
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
// SET ORIENTATION
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetOrientation(const AValue: TOBDLinearGaugeOrientation);
begin
  if FOrientation <> AValue then begin FOrientation := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET DIRECTION
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetDirection(const AValue: TOBDLinearGaugeDirection);
begin
  if FDirection <> AValue then begin FDirection := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetBorderColor(const AValue: TAlphaColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BAR COLOR FROM
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetBarColorFrom(const AValue: TAlphaColor);
begin
  if FBarColorFrom <> AValue then begin FBarColorFrom := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BAR COLOR TO
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetBarColorTo(const AValue: TAlphaColor);
begin
  if FBarColorTo <> AValue then begin FBarColorTo := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetTextColor(const AValue: TAlphaColor);
begin
  if FTextColor <> AValue then begin FTextColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER WIDTH
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetBorderWidth(const AValue: Single);
begin
  if (AValue >= 0) and (FBorderWidth <> AValue) then begin FBorderWidth := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET CORNER RADIUS
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetCornerRadius(const AValue: Single);
begin
  if (AValue >= 0) and (FCornerRadius <> AValue) then begin FCornerRadius := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET PADDING
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetPadding(const AValue: Single);
begin
  if (AValue >= 0) and (FPadding <> AValue) then begin FPadding := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then begin FCaption := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET UNITS
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetUnits(const AValue: string);
begin
  if FUnits <> AValue then begin FUnits := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW VALUE
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetShowValue(const AValue: Boolean);
begin
  if FShowValue <> AValue then begin FShowValue := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET ANIMATION ENABLED
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.SetAnimationEnabled(const AValue: Boolean);
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
procedure TOBDLinearGaugeFMX.SetAnimationDurationMs(const AValue: Integer);
begin
  if (FAnimationDurationMs <> AValue) and (AValue >= 0) then begin FAnimationDurationMs := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// HANDLE DRAW
//------------------------------------------------------------------------------
procedure TOBDLinearGaugeFMX.HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  State: TOBDLinearGaugeRenderState;
begin
  UpdateAnimationValue;

  // Same marshalling shape as the VCL binder — colours are already
  // TAlphaColor in FMX so no conversion needed.
  State.Width := ADest.Width;
  State.Height := ADest.Height;
  State.Min := FMin;
  State.Max := FMax;
  State.DisplayValue := FDisplayValue;
  State.Orientation := FOrientation;
  State.Direction := FDirection;
  State.BackgroundColor := FBackgroundColor;
  State.BorderColor := FBorderColor;
  State.BarColorFrom := FBarColorFrom;
  State.BarColorTo := FBarColorTo;
  State.TextColor := FTextColor;
  State.OuterClearColor := FBackgroundColor;
  State.BorderWidth := FBorderWidth;
  State.CornerRadius := FCornerRadius;
  State.Padding := FPadding;
  State.Caption := FCaption;
  State.Units := FUnits;
  State.ShowValue := FShowValue;

  RenderLinearGauge(ACanvas, State);
end;

end.
