//------------------------------------------------------------------------------
//  OBD.UI.Gauges.DialExtended
//
//  Circular-dial variants that add a second visual element on
//  top of TOBDCircularGauge:
//
//    TOBDDualNeedleGauge — primary value needle plus a thinner
//                          setpoint / target needle. Useful for
//                          "current vs commanded" pairs (boost
//                          actual vs commanded, AFR vs lambda
//                          target, throttle vs pedal demand).
//    TOBDMinMaxGauge     — primary needle plus two small tick
//                          markers on the dial recording the
//                          lowest and highest values seen since
//                          the last ResetMinMax call.
//
//  Both inherit theme / HiDPI / zone / LiveData / LiveBindings
//  behaviour for free from TOBDGaugeBase.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.DialExtended;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Base,
  OBD.UI.Gauges.Dial;

type
  /// <summary>Circular gauge with a primary value needle plus a
  /// thinner setpoint needle. <c>SetpointValue</c> can be wired
  /// from a second LiveData source (host glue) or set directly
  /// at runtime.</summary>
  TOBDDualNeedleGauge = class(TOBDCircularGauge)
  strict private
    FSetpointValue: Double;
    FSetpointColor: TColor;
    FShowSetpoint:  Boolean;
    procedure SetSetpointValue(AValue: Double);
    procedure SetSetpointColor(AValue: TColor);
    procedure SetShowSetpoint(AValue: Boolean);
  protected
    procedure PaintExtras(AGraphics: TGPGraphics;
      const ABounds: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Secondary value the gauge draws as a thinner
    /// needle. Default 0.</summary>
    property SetpointValue: Double
      read FSetpointValue write SetSetpointValue;
    /// <summary>Setpoint-needle colour. Default cyan-ish accent
    /// so it reads distinctly against the primary needle.</summary>
    property SetpointColor: TColor
      read FSetpointColor write SetSetpointColor default $00C08040;
    /// <summary>Show the setpoint needle. Default True.</summary>
    property ShowSetpoint: Boolean
      read FShowSetpoint write SetShowSetpoint default True;
  end;

  /// <summary>Circular gauge that records and renders the lowest
  /// and highest values seen since the last
  /// <c>ResetMinMax</c>. Two small tick markers sit just inside
  /// the bezel at the recorded angles.</summary>
  TOBDMinMaxGauge = class(TOBDCircularGauge)
  strict private
    FHaveStats: Boolean;
    FMinSeen:   Double;
    FMaxSeen:   Double;
    FMarkerColor: TColor;
    FShowMinMax:  Boolean;
    procedure SetMarkerColor(AValue: TColor);
    procedure SetShowMinMax(AValue: Boolean);
    procedure ObserveSample(AValue: Double);
  protected
    procedure PaintExtras(AGraphics: TGPGraphics;
      const ABounds: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Clear the recorded min / max trace. Next
    /// displayed value seeds both.</summary>
    procedure ResetMinMax;
    /// <summary>Lowest value seen since the last reset (or 0 if
    /// no samples observed yet).</summary>
    function MinSeen: Double;
    /// <summary>Highest value seen since the last reset (or 0 if
    /// no samples observed yet).</summary>
    function MaxSeen: Double;
  published
    /// <summary>Colour of the min / max marker triangles.
    /// Default subtle grey.</summary>
    property MarkerColor: TColor
      read FMarkerColor write SetMarkerColor default $00808080;
    /// <summary>Show the markers. Default True.</summary>
    property ShowMinMax: Boolean
      read FShowMinMax write SetShowMinMax default True;
  end;

implementation

{ ---- TOBDDualNeedleGauge -------------------------------------------------- }

constructor TOBDDualNeedleGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSetpointValue := 0;
  FSetpointColor := $00C08040;
  FShowSetpoint  := True;
end;

procedure TOBDDualNeedleGauge.SetSetpointValue(AValue: Double);
begin
  if SameValue(FSetpointValue, AValue) then Exit;
  FSetpointValue := AValue;
  Repaint;
end;

procedure TOBDDualNeedleGauge.SetSetpointColor(AValue: TColor);
begin
  if FSetpointColor = AValue then Exit;
  FSetpointColor := AValue;
  Repaint;
end;

procedure TOBDDualNeedleGauge.SetShowSetpoint(AValue: Boolean);
begin
  if FShowSetpoint = AValue then Exit;
  FShowSetpoint := AValue;
  Repaint;
end;

procedure TOBDDualNeedleGauge.PaintExtras(AGraphics: TGPGraphics;
  const ABounds: TRectF);
var
  Cx, Cy, R, NLen: Single;
  Angle: Single;
  Pen: TGPPen;
  NX, NY: Single;
  ClampedSP: Double;
begin
  if not FShowSetpoint then Exit;
  // Clamp into Min..Max so the needle never escapes the dial.
  ClampedSP := Clamp(FSetpointValue);

  Cx := ABounds.X + ABounds.Width  / 2;
  Cy := ABounds.Y + ABounds.Height / 2;
  R  := System.Math.Min(ABounds.Width, ABounds.Height) / 2;
  // Shorter than the primary needle to keep the two visually
  // distinct even when they overlap.
  NLen := R - ScaleValue(28);
  Angle := DegToRad(ValueToAngle(ClampedSP));
  NX := Cx + NLen * Cos(Angle);
  NY := Cy + NLen * Sin(Angle);

  Pen := TGPPen.Create(ColorToARGB(FSetpointColor), ScaleValue(2));
  Pen.SetStartCap(LineCapRound);
  Pen.SetEndCap(LineCapRound);
  try
    AGraphics.DrawLine(Pen, Cx, Cy, NX, NY);
  finally
    Pen.Free;
  end;
end;

{ ---- TOBDMinMaxGauge ------------------------------------------------------ }

constructor TOBDMinMaxGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarkerColor := $00808080;
  FShowMinMax  := True;
  FHaveStats   := False;
end;

procedure TOBDMinMaxGauge.SetMarkerColor(AValue: TColor);
begin
  if FMarkerColor = AValue then Exit;
  FMarkerColor := AValue;
  Repaint;
end;

procedure TOBDMinMaxGauge.SetShowMinMax(AValue: Boolean);
begin
  if FShowMinMax = AValue then Exit;
  FShowMinMax := AValue;
  Repaint;
end;

procedure TOBDMinMaxGauge.ResetMinMax;
begin
  FHaveStats := False;
  FMinSeen := 0;
  FMaxSeen := 0;
  Repaint;
end;

function TOBDMinMaxGauge.MinSeen: Double;
begin
  if FHaveStats then Result := FMinSeen else Result := 0;
end;

function TOBDMinMaxGauge.MaxSeen: Double;
begin
  if FHaveStats then Result := FMaxSeen else Result := 0;
end;

procedure TOBDMinMaxGauge.ObserveSample(AValue: Double);
begin
  if not FHaveStats then
  begin
    FMinSeen := AValue;
    FMaxSeen := AValue;
    FHaveStats := True;
  end
  else
  begin
    if AValue < FMinSeen then FMinSeen := AValue;
    if AValue > FMaxSeen then FMaxSeen := AValue;
  end;
end;

procedure TOBDMinMaxGauge.PaintExtras(AGraphics: TGPGraphics;
  const ABounds: TRectF);
var
  Cx, Cy, R, RIn, ROut: Single;

  procedure DrawMarker(AVal: Double);
  var
    Angle: Single;
    Pen: TGPPen;
    X1, Y1, X2, Y2: Single;
  begin
    Angle := DegToRad(ValueToAngle(AVal));
    X1 := Cx + RIn  * Cos(Angle);
    Y1 := Cy + RIn  * Sin(Angle);
    X2 := Cx + ROut * Cos(Angle);
    Y2 := Cy + ROut * Sin(Angle);
    Pen := TGPPen.Create(ColorToARGB(FMarkerColor), ScaleValue(3));
    Pen.SetStartCap(LineCapRound);
    Pen.SetEndCap(LineCapRound);
    try
      AGraphics.DrawLine(Pen, X1, Y1, X2, Y2);
    finally
      Pen.Free;
    end;
  end;

begin
  // Observe samples through the paint pipeline so the trace
  // tracks DisplayValue (post-animation, post-clamp).
  ObserveSample(DisplayValue);
  if not FShowMinMax or not FHaveStats then Exit;

  Cx := ABounds.X + ABounds.Width  / 2;
  Cy := ABounds.Y + ABounds.Height / 2;
  R  := System.Math.Min(ABounds.Width, ABounds.Height) / 2;
  // Sit the markers just inside the bezel, outside the tick band.
  RIn  := R - ScaleValue(6);
  ROut := R - ScaleValue(2);

  DrawMarker(FMinSeen);
  if not SameValue(FMinSeen, FMaxSeen) then
    DrawMarker(FMaxSeen);
end;

end.
