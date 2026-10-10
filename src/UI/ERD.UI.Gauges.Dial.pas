//------------------------------------------------------------------------------
//  ERD.UI.Gauges.Dial
//
//  TOBDDialGauge - round dial for RPM, speed, coolant temperature,
//  boost / vacuum and any other single channel that reads best as a
//  needle.
//
//  Layout (all sizes scale with the control and the screen DPI):
//    - face with a rim, warning / alarm bands along the scale;
//    - major and minor ticks at "nice" steps in the display unit,
//      with a x1000 multiplier for large ranges such as RPM;
//    - caption above the hub, large digital readout in the gap at
//      the bottom of the sweep, unit and data-state text below it;
//    - needle coloured by alert level, parked and greyed when there
//      is no data or the value is stale.
//
//  Arcs and the needle are anti-aliased with GDI+; text uses the VCL
//  canvas, which stays sharper at small sizes.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  TOBDDialGauge on the channel-bound gauge base.
//------------------------------------------------------------------------------

unit ERD.UI.Gauges.Dial;

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.GDIP,
  ERD.UI.Control,
  ERD.UI.Units,
  ERD.UI.Gauges.Types,
  ERD.UI.Gauges.Base;

type
  /// <summary>Round needle gauge bound to one data channel.</summary>
  /// <remarks>
  /// <para>Defaults to engine RPM (PID <c>$0C</c>, 0..8000 rpm,
  /// warning from 6000, alarm from 6800). Change <c>Channel.PID</c>,
  /// <c>Min</c>, <c>Max</c>, <c>Caption</c> and <c>Alerts</c> for
  /// any other channel.</para>
  /// <para>Angles use the GDI+ convention: 0 degrees at 3 o'clock,
  /// positive clockwise. The default 135 / 270 leaves the gap at the
  /// bottom for the readout.</para>
  /// </remarks>
  TOBDDialGauge = class(TOBDGaugeBase)
  strict private
    FStartAngle: Integer;
    FSweepAngle: Integer;
    FShowReadout: Boolean;
    FShowTickLabels: Boolean;
    procedure SetStartAngle(AValue: Integer);
    procedure SetSweepAngle(AValue: Integer);
    procedure SetShowReadout(AValue: Boolean);
    procedure SetShowTickLabels(AValue: Boolean);
    function AngleOf(AMetric: Double): Single;
    function PointAt(ACX, ACY, ARadius, AAngle: Single): TPointF;
    procedure DrawScale(AGraphics: TGPGraphics; ACX, ACY, ARadius: Single);
    procedure DrawTickLabels(ACanvas: TCanvas; ACX, ACY, ARadius: Single;
      out AMultiplier: string);
    procedure DrawTexts(ACanvas: TCanvas; ACX, ACY, ARadius: Single;
      const AMultiplier: string);
    procedure DrawNeedle(AGraphics: TGPGraphics; ACX, ACY, ARadius: Single);
  protected
    /// <summary>Paints face, scale, labels, readout and needle.
    /// </summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates an RPM dial.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Adds the dial geometry to the base settings.</summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Restores base settings and the dial geometry.</summary>
    /// <param name="AObject">Source object.</param>
    procedure LoadSettings(AObject: TJSONObject); override;
  published
    /// <summary>Angle of the scale minimum, degrees clockwise from
    /// 3 o'clock.</summary>
    property StartAngle: Integer read FStartAngle write SetStartAngle
      default 135;
    /// <summary>Angular length of the scale in degrees (30..360).
    /// </summary>
    property SweepAngle: Integer read FSweepAngle write SetSweepAngle
      default 270;
    /// <summary>Shows the digital readout under the hub.</summary>
    property ShowReadout: Boolean read FShowReadout write SetShowReadout
      default True;
    /// <summary>Shows numbers at the major ticks.</summary>
    property ShowTickLabels: Boolean read FShowTickLabels
      write SetShowTickLabels default True;
  end;

implementation

constructor TOBDDialGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 220;
  Height := 220;
  FStartAngle := 135;
  FSweepAngle := 270;
  FShowReadout := True;
  FShowTickLabels := True;
  Caption := 'RPM';
  &Unit := 'rpm';
  Min := 0;
  Max := 8000;
  Channel.PID := $0C;
  Alerts.SetHigh(6000, 6800);
end;

procedure TOBDDialGauge.SetStartAngle(AValue: Integer);
begin
  if FStartAngle = AValue then
    Exit;
  FStartAngle := AValue;
  Invalidate;
end;

procedure TOBDDialGauge.SetSweepAngle(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 30, 360);
  if FSweepAngle = AValue then
    Exit;
  FSweepAngle := AValue;
  Invalidate;
end;

procedure TOBDDialGauge.SetShowReadout(AValue: Boolean);
begin
  if FShowReadout = AValue then
    Exit;
  FShowReadout := AValue;
  Invalidate;
end;

procedure TOBDDialGauge.SetShowTickLabels(AValue: Boolean);
begin
  if FShowTickLabels = AValue then
    Exit;
  FShowTickLabels := AValue;
  Invalidate;
end;

function TOBDDialGauge.AngleOf(AMetric: Double): Single;
begin
  Result := FStartAngle + NormaliseValue(Min, Max, AMetric) * FSweepAngle;
end;

function TOBDDialGauge.PointAt(ACX, ACY, ARadius, AAngle: Single): TPointF;
var
  R: Double;
begin
  R := DegToRad(AAngle);
  Result.X := ACX + ARadius * Cos(R);
  Result.Y := ACY + ARadius * Sin(R);
end;

procedure TOBDDialGauge.DrawScale(AGraphics: TGPGraphics;
  ACX, ACY, ARadius: Single);
var
  Brush: TGPSolidBrush;
  Pen: TGPPen;
  Zone: TOBDGaugeZone;
  BandR, A0, A1, T, DMin, DMax, Step, MinorStep: Double;
  P0, P1: TPointF;
  Conv: TOBDUnitConversion;
  Major: Boolean;
  Guard: Integer;
begin
  // Face and rim.
  Brush := TGPSolidBrush.Create(ColorToARGB(Palette.GaugeFace));
  try
    AGraphics.FillEllipse(Brush, ACX - ARadius, ACY - ARadius, ARadius * 2,
      ARadius * 2);
  finally
    Brush.Free;
  end;
  Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(2));
  try
    AGraphics.DrawEllipse(Pen, ACX - ARadius, ACY - ARadius, ARadius * 2,
      ARadius * 2);
  finally
    Pen.Free;
  end;

  // Track and alert bands just inside the rim.
  BandR := ARadius * 0.9;
  Pen := TGPPen.Create(ColorToARGB(Palette.Subtle, 90), ARadius * 0.06);
  try
    AGraphics.DrawArc(Pen, ACX - BandR, ACY - BandR, BandR * 2, BandR * 2,
      FStartAngle, FSweepAngle);
  finally
    Pen.Free;
  end;
  for Zone in EffectiveZones do
  begin
    A0 := AngleOf(Zone.StartValue);
    A1 := AngleOf(Zone.EndValue);
    if A1 - A0 < 0.5 then
      Continue;
    Pen := TGPPen.Create(ColorToARGB(Zone.Color), ARadius * 0.06);
    try
      AGraphics.DrawArc(Pen, ACX - BandR, ACY - BandR, BandR * 2, BandR * 2,
        A0, A1 - A0);
    finally
      Pen.Free;
    end;
  end;

  // Ticks at nice steps in the display unit.
  Conv := Conversion;
  DMin := Conv.ToDisplay(Min);
  DMax := Conv.ToDisplay(Max);
  if DMax <= DMin then
    Exit;
  Step := NiceTickStep(DMax - DMin, IfThen(ARadius > ScaleValue(80), 8, 5));
  MinorStep := Step / 5;
  T := Ceil(DMin / MinorStep - 1E-9) * MinorStep;
  Guard := 0;
  Pen := TGPPen.Create(ColorToARGB(Palette.GaugeTick), 1);
  try
    while (T <= DMax + MinorStep * 1E-6) and (Guard < 500) do
    begin
      Inc(Guard);
      Major := Abs(T / Step - Round(T / Step)) < 1E-6;
      A0 := AngleOf(Conv.FromDisplay(T));
      if Major then
      begin
        Pen.SetWidth(System.Math.Max(2, ScaleValue(2)));
        P0 := PointAt(ACX, ACY, ARadius * 0.97, A0);
        P1 := PointAt(ACX, ACY, ARadius * 0.82, A0);
      end
      else
      begin
        Pen.SetWidth(1);
        P0 := PointAt(ACX, ACY, ARadius * 0.97, A0);
        P1 := PointAt(ACX, ACY, ARadius * 0.89, A0);
      end;
      AGraphics.DrawLine(Pen, P0.X, P0.Y, P1.X, P1.Y);
      T := T + MinorStep;
    end;
  finally
    Pen.Free;
  end;
end;

procedure TOBDDialGauge.DrawTickLabels(ACanvas: TCanvas;
  ACX, ACY, ARadius: Single; out AMultiplier: string);
var
  Conv: TOBDUnitConversion;
  DMin, DMax, Step, T, Shown: Double;
  Divisor: Double;
  P: TPointF;
  S: string;
  Decimals, Guard: Integer;
begin
  AMultiplier := '';
  if not FShowTickLabels then
    Exit;
  Conv := Conversion;
  DMin := Conv.ToDisplay(Min);
  DMax := Conv.ToDisplay(Max);
  if DMax <= DMin then
    Exit;
  Step := NiceTickStep(DMax - DMin, IfThen(ARadius > ScaleValue(80), 8, 5));
  Divisor := 1;
  if (Step >= 1000) or (System.Math.Max(Abs(DMin), Abs(DMax)) >= 2000) then
  begin
    Divisor := 1000;
    AMultiplier := 'x1000';
  end;
  Decimals := 0;
  if Abs(Step / Divisor - Round(Step / Divisor)) > 1E-6 then
    Decimals := 1;
  ACanvas.Font.Name := Font.Name;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Height := -System.Math.Max(8, Round(ARadius * 0.13));
  ACanvas.Font.Color := Palette.GaugeLabel;
  ACanvas.Brush.Style := bsClear;
  T := Ceil(DMin / Step - 1E-9) * Step;
  Guard := 0;
  while (T <= DMax + Step * 1E-6) and (Guard < 100) do
  begin
    Inc(Guard);
    Shown := T / Divisor;
    S := OBDFormatNumber(Shown, Decimals);
    P := PointAt(ACX, ACY, ARadius * 0.66, AngleOf(Conv.FromDisplay(T)));
    ACanvas.TextOut(Round(P.X) - ACanvas.TextWidth(S) div 2,
      Round(P.Y) - ACanvas.TextHeight(S) div 2, S);
    T := T + Step;
  end;
end;

procedure TOBDDialGauge.DrawTexts(ACanvas: TCanvas; ACX, ACY, ARadius: Single;
  const AMultiplier: string);
var
  R: TRect;
  S: string;
  TextTop: Integer;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Name := Font.Name;

  // Caption (and the x1000 multiplier) above the hub.
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := Palette.GaugeLabel;
  R := Rect(Round(ACX - ARadius * 0.5), Round(ACY - ARadius * 0.42),
    Round(ACX + ARadius * 0.5), Round(ACY - ARadius * 0.22));
  S := Caption;
  if AMultiplier <> '' then
    S := Trim(S + ' ' + AMultiplier);
  if S <> '' then
  begin
    FitFont(ACanvas, S, R.Width, R.Height);
    DrawCentred(ACanvas, R, S);
  end;

  if not FShowReadout then
    Exit;

  // Big readout in the gap at the bottom of the sweep.
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := ValueColor(EffectiveForeground);
  TextTop := Round(ACY + ARadius * 0.28);
  R := Rect(Round(ACX - ARadius * 0.55), TextTop, Round(ACX + ARadius * 0.55),
    TextTop + Round(ARadius * 0.32));
  S := ReadoutText;
  FitFont(ACanvas, S, R.Width, R.Height);
  DrawCentred(ACanvas, R, S);

  // Unit, or the data-state text when not live.
  ACanvas.Font.Style := [];
  R := Rect(Round(ACX - ARadius * 0.5), R.Bottom, Round(ACX + ARadius * 0.5),
    R.Bottom + Round(ARadius * 0.16));
  S := StateText;
  if S <> '' then
  begin
    ACanvas.Font.Style := [fsBold];
    if DataState = dstOutOfRange then
      ACanvas.Font.Color := Palette.Danger
    else
      ACanvas.Font.Color := Palette.Subtle;
  end
  else
  begin
    S := DisplayUnit;
    ACanvas.Font.Color := Palette.GaugeLabel;
  end;
  if S <> '' then
  begin
    FitFont(ACanvas, S, R.Width, R.Height);
    DrawCentred(ACanvas, R, S);
  end;
end;

procedure TOBDDialGauge.DrawNeedle(AGraphics: TGPGraphics;
  ACX, ACY, ARadius: Single);
var
  Pen: TGPPen;
  Brush: TGPSolidBrush;
  Angle: Single;
  Tip, Tail: TPointF;
  Col: TColor;
  HubR: Single;
begin
  Angle := FStartAngle + PaintFraction * FSweepAngle;
  Tip := PointAt(ACX, ACY, ARadius * 0.84, Angle);
  Tail := PointAt(ACX, ACY, ARadius * 0.14, Angle + 180);
  Col := ValueColor(Palette.GaugeNeedle);
  Pen := TGPPen.Create(ColorToARGB(Col), System.Math.Max(2, ARadius * 0.035));
  try
    Pen.SetStartCap(LineCapRound);
    Pen.SetEndCap(LineCapRound);
    AGraphics.DrawLine(Pen, Tail.X, Tail.Y, Tip.X, Tip.Y);
  finally
    Pen.Free;
  end;
  HubR := System.Math.Max(3, ARadius * 0.07);
  Brush := TGPSolidBrush.Create(ColorToARGB(Col));
  try
    AGraphics.FillEllipse(Brush, ACX - HubR, ACY - HubR, HubR * 2, HubR * 2);
  finally
    Brush.Free;
  end;
end;

procedure TOBDDialGauge.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  CX, CY, Radius: Single;
  Multiplier: string;
begin
  CX := Width / 2;
  CY := Height / 2;
  Radius := System.Math.Min(Width, Height) / 2 - ScaleValue(4);
  if Radius < ScaleValue(10) then
    Exit;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawScale(Graphics, CX, CY, Radius);
  finally
    Graphics.Free;
  end;

  DrawTickLabels(ACanvas, CX, CY, Radius, Multiplier);
  DrawTexts(ACanvas, CX, CY, Radius, Multiplier);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawNeedle(Graphics, CX, CY, Radius);
  finally
    Graphics.Free;
  end;
end;

procedure TOBDDialGauge.SaveSettings(AObject: TJSONObject);
begin
  inherited SaveSettings(AObject);
  AObject.AddPair('startAngle', TJSONNumber.Create(FStartAngle));
  AObject.AddPair('sweepAngle', TJSONNumber.Create(FSweepAngle));
end;

procedure TOBDDialGauge.LoadSettings(AObject: TJSONObject);
var
  I: Integer;
begin
  inherited LoadSettings(AObject);
  I := FStartAngle;
  if OBDJsonReadInt(AObject, 'startAngle', I) then
    StartAngle := I;
  I := FSweepAngle;
  if OBDJsonReadInt(AObject, 'sweepAngle', I) then
    SweepAngle := I;
end;

end.
