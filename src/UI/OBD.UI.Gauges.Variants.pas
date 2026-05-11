//------------------------------------------------------------------------------
//  OBD.UI.Gauges.Variants
//
//  Three non-dial gauge variants that share the
//  TOBDGaugeBase contract:
//
//    TOBDDigitalGauge     — pure numeric readout (no needle).
//                           Caption / value / unit stacked
//                           vertically; optional min/max/avg
//                           overlays.
//    TOBDBarSegmentGauge  — LED-bar / VU-meter. N segments
//                           light from left to right (or
//                           bottom to top) as value rises.
//    TOBDDeltaGauge       — centre-zero horizontal bar. Value
//                           > 0 fills right; value < 0 fills
//                           left. Visualises signed signals
//                           (yaw, lat-accel, fuel trim).
//
//  All three are theme-aware, HiDPI-aware, LiveData- and
//  LiveBindings-friendly via inheritance from TOBDGaugeBase.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.Variants;

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
  OBD.UI.Gauges.Linear;

type
  /// <summary>Big-number digital readout. No needle, no dial.
  /// Optional min/max/avg memory shown in small text under
  /// the main value.</summary>
  TOBDDigitalGauge = class(TOBDGaugeBase)
  strict private
    FShowMinMax:  Boolean;
    FShowAvg:     Boolean;
    FValueFont:   TFont;
    FCaptionFont: TFont;
    FMinSeen:     Double;
    FMaxSeen:     Double;
    FAvgRunning:  Double;
    FAvgSamples:  Int64;
    FHaveStats:   Boolean;
    procedure SetShowMinMax(AValue: Boolean);
    procedure SetShowAvg(AValue: Boolean);
    procedure SetValueFont(AValue: TFont);
    procedure SetCaptionFont(AValue: TFont);
    procedure FontChanged(Sender: TObject);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Clears the min / max / avg memory.</summary>
    procedure ResetStats;
  published
    /// <summary>Show "min: X  max: Y" line under the value.
    /// Default True.</summary>
    property ShowMinMax: Boolean
      read FShowMinMax write SetShowMinMax default True;
    /// <summary>Show "avg: Z" line under the value.
    /// Default False.</summary>
    property ShowAvg: Boolean
      read FShowAvg write SetShowAvg default False;
    /// <summary>Font for the headline numeric value.</summary>
    property ValueFont: TFont read FValueFont write SetValueFont;
    /// <summary>Font for Caption + stats text. Falls through
    /// to the inherited Font when nil.</summary>
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
  end;

  /// <summary>LED-bar / VU-meter. N segments light from the
  /// "Min" end toward the "Max" end as value rises. Each
  /// segment uses the zone colour at its midpoint, falling
  /// back to the accent colour.</summary>
  TOBDBarSegmentGauge = class(TOBDGaugeBase)
  strict private
    FSegmentCount: Integer;
    FGapPixels:    Integer;
    FOrientation:  TOBDLinearOrientation;
    procedure SetSegmentCount(AValue: Integer);
    procedure SetGapPixels(AValue: Integer);
    procedure SetOrientation(AValue: TOBDLinearOrientation);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Number of segments along the bar. Default 16.</summary>
    property SegmentCount: Integer
      read FSegmentCount write SetSegmentCount default 16;
    /// <summary>Gap (pixels at 96-DPI) between adjacent
    /// segments. Default 2.</summary>
    property GapPixels: Integer
      read FGapPixels write SetGapPixels default 2;
    /// <summary>Bar orientation. Default horizontal.</summary>
    property Orientation: TOBDLinearOrientation
      read FOrientation write SetOrientation default loHorizontal;
  end;

  /// <summary>Centre-zero horizontal bar. Value > 0 fills the
  /// right half; value < 0 fills the left half. Range is
  /// implicitly symmetric: <c>Min = -Range</c>, <c>Max = +Range</c>,
  /// where <c>Range = TOBDGaugeBase.Max</c> (the absolute
  /// magnitude). Hosts set <c>Range</c> via Max — leave Min at
  /// the inverse for symmetric behaviour.</summary>
  TOBDDeltaGauge = class(TOBDGaugeBase)
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ ---- TOBDDigitalGauge ---------------------------------------------------- }

constructor TOBDDigitalGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 180;
  Height := 90;
  FShowMinMax := True;
  FShowAvg    := False;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 28;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := FontChanged;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := FontChanged;
  ResetStats;
end;

destructor TOBDDigitalGauge.Destroy;
begin
  FValueFont.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDDigitalGauge.SetShowMinMax(AValue: Boolean);
begin if FShowMinMax <> AValue then begin FShowMinMax := AValue; Repaint; end; end;

procedure TOBDDigitalGauge.SetShowAvg(AValue: Boolean);
begin if FShowAvg <> AValue then begin FShowAvg := AValue; Repaint; end; end;

procedure TOBDDigitalGauge.SetValueFont(AValue: TFont);
begin FValueFont.Assign(AValue); end;

procedure TOBDDigitalGauge.SetCaptionFont(AValue: TFont);
begin FCaptionFont.Assign(AValue); end;

procedure TOBDDigitalGauge.FontChanged(Sender: TObject);
begin Repaint; end;

procedure TOBDDigitalGauge.ResetStats;
begin
  FMinSeen := 0;
  FMaxSeen := 0;
  FAvgRunning := 0;
  FAvgSamples := 0;
  FHaveStats := False;
  Repaint;
end;

procedure TOBDDigitalGauge.PaintControl(ACanvas: TCanvas);
var
  S:        string;
  R:        TRect;
  CapH, ValH, MinMaxH: Integer;
  Graphics: TGPGraphics;
  StripBrush: TGPSolidBrush;
  StripRect: TGPRectF;
  Norm: Single;
  ZoneCol: TColor;
begin
  // Update running stats from DisplayValue.
  if not FHaveStats then
  begin
    FMinSeen := DisplayValue;
    FMaxSeen := DisplayValue;
    FAvgRunning := DisplayValue;
    FAvgSamples := 1;
    FHaveStats := True;
  end
  else
  begin
    if DisplayValue < FMinSeen then FMinSeen := DisplayValue;
    if DisplayValue > FMaxSeen then FMaxSeen := DisplayValue;
    Inc(FAvgSamples);
    FAvgRunning := FAvgRunning +
      (DisplayValue - FAvgRunning) / FAvgSamples;
  end;

  R := ClientRect;
  ACanvas.Brush.Style := bsClear;

  // Caption (top).
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  CapH := ACanvas.TextHeight('Mg');
  if Caption <> '' then
    ACanvas.TextOut(ScaleValue(8), ScaleValue(4), Caption);

  // Main value (centred).
  ACanvas.Font := FValueFont;
  ZoneCol := CurrentZoneColor;
  if ZoneCol = clNone then ZoneCol := EffectiveForeground;
  ACanvas.Font.Color := ZoneCol;
  S := FormatValue(DisplayValue);
  ValH := ACanvas.TextHeight('0');
  ACanvas.TextOut(
    (R.Width  - ACanvas.TextWidth(S)) div 2,
    CapH + ScaleValue(2),
    S);

  // Min / max / avg row.
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  MinMaxH := 0;
  if FShowMinMax then
  begin
    S := Format('min: %s   max: %s',
      [FormatValue(FMinSeen), FormatValue(FMaxSeen)]);
    ACanvas.TextOut(
      (R.Width - ACanvas.TextWidth(S)) div 2,
      CapH + ScaleValue(2) + ValH + ScaleValue(2),
      S);
    MinMaxH := ACanvas.TextHeight(S);
  end;
  if FShowAvg then
  begin
    S := Format('avg: %s', [FormatValue(FAvgRunning)]);
    ACanvas.TextOut(
      (R.Width - ACanvas.TextWidth(S)) div 2,
      CapH + ScaleValue(2) + ValH + ScaleValue(2) + MinMaxH +
        ScaleValue(2),
      S);
  end;

  // Zone strip beneath everything.
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    StripRect.X := ScaleValue(8);
    StripRect.Y := R.Height - ScaleValue(6);
    StripRect.Width := R.Width - ScaleValue(16);
    StripRect.Height := ScaleValue(3);
    StripBrush := TGPSolidBrush.Create(ColorToARGB(EffectiveBorder));
    try
      Graphics.FillRectangle(StripBrush, StripRect);
    finally
      StripBrush.Free;
    end;
    Norm := Single(NormalisedDisplay);
    StripRect.Width := StripRect.Width * Norm;
    StripBrush := TGPSolidBrush.Create(ColorToARGB(EffectiveAccent));
    try
      Graphics.FillRectangle(StripBrush, StripRect);
    finally
      StripBrush.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDBarSegmentGauge ------------------------------------------------- }

constructor TOBDBarSegmentGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 220;
  Height := 60;
  FSegmentCount := 16;
  FGapPixels    := 2;
  FOrientation  := loHorizontal;
end;

procedure TOBDBarSegmentGauge.SetSegmentCount(AValue: Integer);
begin
  if AValue < 2 then AValue := 2;
  if AValue > 128 then AValue := 128;
  if FSegmentCount = AValue then Exit;
  FSegmentCount := AValue; Repaint;
end;

procedure TOBDBarSegmentGauge.SetGapPixels(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FGapPixels = AValue then Exit;
  FGapPixels := AValue; Repaint;
end;

procedure TOBDBarSegmentGauge.SetOrientation(AValue: TOBDLinearOrientation);
begin
  if FOrientation = AValue then Exit;
  FOrientation := AValue; Repaint;
end;

procedure TOBDBarSegmentGauge.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pad, Gap, SegLen: Integer;
  TrackX, TrackY, TrackW, TrackH: Integer;
  I: Integer;
  SegRect: TGPRectF;
  Brush: TGPSolidBrush;
  ActiveTo: Integer;
  SegV: Double;
  SegColor: TColor;
  Norm: Single;
  CapH: Integer;
begin
  Pad := ScaleValue(8);
  Gap := ScaleValue(FGapPixels);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := Self.Font;
  ACanvas.Font.Color := EffectiveForeground;
  CapH := IfThen(Caption <> '', ACanvas.TextHeight('Mg'), 0);
  if Caption <> '' then
    ACanvas.TextOut(Pad, Pad, Caption);

  if FOrientation = loHorizontal then
  begin
    TrackX := Pad;
    TrackY := Pad + CapH + ScaleValue(4);
    TrackW := Width  - 2 * Pad;
    TrackH := Height - TrackY - Pad;
  end
  else
  begin
    TrackX := Pad;
    TrackY := Pad + CapH + ScaleValue(4);
    TrackW := ScaleValue(24);
    TrackH := Height - TrackY - Pad;
    if (Width - 2 * Pad) < TrackW then TrackW := Width - 2 * Pad;
    TrackX := (Width - TrackW) div 2;
  end;

  Norm := Single(NormalisedDisplay);
  ActiveTo := Round(Norm * FSegmentCount);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    if FOrientation = loHorizontal then
    begin
      SegLen := (TrackW - (FSegmentCount - 1) * Gap) div FSegmentCount;
      for I := 0 to FSegmentCount - 1 do
      begin
        SegRect.X := TrackX + I * (SegLen + Gap);
        SegRect.Y := TrackY;
        SegRect.Width := SegLen;
        SegRect.Height := TrackH;
        // Pick zone colour at the segment's centre value.
        SegV := Min + (I + 0.5) / FSegmentCount * (Max - Min);
        SegColor := ResolveZone(Zones, SegV).Color;
        if SegColor = clNone then SegColor := EffectiveAccent;
        if I >= ActiveTo then SegColor := Palette.NeutralLight;
        Brush := TGPSolidBrush.Create(ColorToARGB(SegColor));
        try
          Graphics.FillRectangle(Brush, SegRect);
        finally
          Brush.Free;
        end;
      end;
    end
    else
    begin
      SegLen := (TrackH - (FSegmentCount - 1) * Gap) div FSegmentCount;
      for I := 0 to FSegmentCount - 1 do
      begin
        SegRect.X := TrackX;
        SegRect.Y := TrackY + TrackH - SegLen - I * (SegLen + Gap);
        SegRect.Width := TrackW;
        SegRect.Height := SegLen;
        SegV := Min + (I + 0.5) / FSegmentCount * (Max - Min);
        SegColor := ResolveZone(Zones, SegV).Color;
        if SegColor = clNone then SegColor := EffectiveAccent;
        if I >= ActiveTo then SegColor := Palette.NeutralLight;
        Brush := TGPSolidBrush.Create(ColorToARGB(SegColor));
        try
          Graphics.FillRectangle(Brush, SegRect);
        finally
          Brush.Free;
        end;
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDDeltaGauge ------------------------------------------------------ }

constructor TOBDDeltaGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 240;
  Height := 60;
  Min := -100;
  Max :=  100;
end;

procedure TOBDDeltaGauge.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pad, CapH, Track, TrackY, ValY: Integer;
  CenterX: Integer;
  TrackRect, FillRect, ZoneRect: TGPRectF;
  Pen:   TGPPen;
  Brush: TGPSolidBrush;
  Norm:  Single;
  ColorVal: TColor;
  Z: TOBDGaugeZone;
  ValStr: string;
begin
  Pad := ScaleValue(8);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := Self.Font;
  ACanvas.Font.Color := EffectiveForeground;
  CapH := IfThen(Caption <> '', ACanvas.TextHeight('Mg'), 0);
  if Caption <> '' then
    ACanvas.TextOut(Pad, Pad, Caption);

  TrackY := Pad + CapH + ScaleValue(4);
  Track  := ScaleValue(14);
  CenterX := Width div 2;

  TrackRect.X := Pad;
  TrackRect.Y := TrackY;
  TrackRect.Width  := Width - 2 * Pad;
  TrackRect.Height := Track;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    // Track background.
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.FillRectangle(Brush, TrackRect);
      Graphics.DrawRectangle(Pen, TrackRect);
    finally
      Pen.Free;
      Brush.Free;
    end;

    // Zone overlays (signed-aware).
    for Z in Zones do
    begin
      var ZX0 := TrackRect.X +
        Single(NormaliseValue(Min, Max, System.Math.Max(Min, Z.StartValue))) *
        TrackRect.Width;
      var ZX1 := TrackRect.X +
        Single(NormaliseValue(Min, Max, System.Math.Min(Max, Z.EndValue))) *
        TrackRect.Width;
      ZoneRect.X := ZX0;
      ZoneRect.Y := TrackRect.Y;
      ZoneRect.Width := ZX1 - ZX0;
      ZoneRect.Height := TrackRect.Height;
      Brush := TGPSolidBrush.Create(ColorToARGB(Z.Color, 96));
      try Graphics.FillRectangle(Brush, ZoneRect);
      finally
        Brush.Free;
      end;
    end;

    // Centre-zero fill — from CenterX outward.
    Norm := Single(NormalisedDisplay);   //  0..1 in clamped scale
    ColorVal := CurrentZoneColor;
    if ColorVal = clNone then ColorVal := EffectiveAccent;
    if DisplayValue >= 0 then
    begin
      // Fill from CenterX to right.
      var NormPos := Single(NormaliseValue(0, Max, DisplayValue));
      FillRect.X := CenterX;
      FillRect.Y := TrackRect.Y;
      FillRect.Width := NormPos * (TrackRect.X + TrackRect.Width - CenterX);
      FillRect.Height := TrackRect.Height;
    end
    else
    begin
      var NormNeg := Single(NormaliseValue(Min, 0, DisplayValue));
      // NormNeg is 0 at Min, 1 at zero. Fill from CenterX leftward.
      FillRect.X := CenterX - (1 - NormNeg) * (CenterX - TrackRect.X);
      FillRect.Y := TrackRect.Y;
      FillRect.Width := CenterX - FillRect.X;
      FillRect.Height := TrackRect.Height;
    end;
    Brush := TGPSolidBrush.Create(ColorToARGB(ColorVal));
    try Graphics.FillRectangle(Brush, FillRect);
    finally
      Brush.Free;
    end;

    // Centre-zero line.
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(2));
    try
      Graphics.DrawLine(Pen, Single(CenterX), TrackRect.Y - 2,
                              Single(CenterX), TrackRect.Y + TrackRect.Height + 2);
    finally
      Pen.Free;
    end;

    // Silence unused-var hint.
    Norm := Norm;
  finally
    Graphics.Free;
  end;

  // Numeric value.
  ValStr := FormatValue(DisplayValue);
  ValY := TrackY + Track + ScaleValue(4);
  ACanvas.Font := Self.Font;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(
    (Width - ACanvas.TextWidth(ValStr)) div 2, ValY, ValStr);
end;

end.
