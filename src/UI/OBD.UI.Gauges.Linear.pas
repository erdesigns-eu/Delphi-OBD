//------------------------------------------------------------------------------
//  OBD.UI.Gauges.Linear
//
//  TOBDLinearGauge — horizontal or vertical bar gauge. Same
//  value / range / zone / animation contract as the dial
//  family; paints a filled bar with optional tick marks +
//  caption + numeric readout.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.Linear;

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
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Base;

type
  TOBDLinearOrientation = (loHorizontal, loVertical);

  TOBDLinearGauge = class(TOBDGaugeBase)
  strict private
    FOrientation: TOBDLinearOrientation;
    FShowValueText: Boolean;
    procedure SetOrientation(AValue: TOBDLinearOrientation);
    procedure SetShowValueText(AValue: Boolean);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Bar orientation. Default horizontal.</summary>
    property Orientation: TOBDLinearOrientation
      read FOrientation write SetOrientation default loHorizontal;
    /// <summary>Print the numeric value + unit beside the bar.
    /// Default True.</summary>
    property ShowValueText: Boolean
      read FShowValueText write SetShowValueText default True;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

constructor TOBDLinearGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 220;
  Height := 60;
  FOrientation := loHorizontal;
  FShowValueText := True;
end;

procedure TOBDLinearGauge.SetOrientation(AValue: TOBDLinearOrientation);
begin
  if FOrientation = AValue then Exit;
  FOrientation := AValue; Repaint;
end;

procedure TOBDLinearGauge.SetShowValueText(AValue: Boolean);
begin
  if FShowValueText = AValue then Exit;
  FShowValueText := AValue; Repaint;
end;

procedure TOBDLinearGauge.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Track, Fill: TGPRectF;
  TrackPen:   TGPPen;
  TrackBrush, FillBrush: TGPSolidBrush;
  ClientPad:  Integer;
  CapHeight:  Integer;
  ValHeight:  Integer;
  TrackThick: Integer;
  Norm:       Single;
  ColorVal:   TColor;
  Cap:        string;
  ValStr:     string;
  X1, Y1, X2, Y2: Integer;
  Z:          TOBDGaugeZone;
  ZRect:      TGPRectF;
begin
  ClientPad := ScaleValue(8);
  CapHeight := IfThen(Caption <> '', ACanvas.TextHeight('Mg'), 0);
  ValHeight := IfThen(FShowValueText, ACanvas.TextHeight('Mg'), 0);
  TrackThick := ScaleValue(14);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    if FOrientation = loHorizontal then
    begin
      // Caption top, track centre, value-text right.
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := EffectiveForeground;
      if Caption <> '' then
        ACanvas.TextOut(ClientPad, ClientPad, Caption);

      Y1 := ClientPad + CapHeight + ScaleValue(4);
      X1 := ClientPad;
      X2 := Width - ClientPad;
      if FShowValueText then
        X2 := X2 - ACanvas.TextWidth(FormatValue(Max)) - ScaleValue(8);

      Track.X := X1;
      Track.Y := Y1;
      Track.Width  := X2 - X1;
      Track.Height := TrackThick;

      // Track background.
      TrackBrush := TGPSolidBrush.Create(
        ColorToARGB(Palette.NeutralLight));
      TrackPen := TGPPen.Create(ColorToARGB(EffectiveBorder),
        ScaleValue(1));
      try
        Graphics.FillRectangle(TrackBrush, Track);
        Graphics.DrawRectangle(TrackPen, Track);
      finally
        TrackPen.Free; TrackBrush.Free;
      end;

      // Zone strips (drawn on top of track background).
      for Z in Zones do
      begin
        if (Z.EndValue <= Min) or (Z.StartValue >= Max) then Continue;
        ZRect.X := Track.X +
          Single(NormaliseValue(Min, Max, System.Math.Max(Min, Z.StartValue))) *
          Track.Width;
        ZRect.Y := Track.Y;
        ZRect.Width :=
          Single(NormaliseValue(Min, Max, System.Math.Min(Max, Z.EndValue))) *
          Track.Width - (ZRect.X - Track.X);
        ZRect.Height := Track.Height;
        var ZBrush := TGPSolidBrush.Create(
          ColorToARGB(Z.Color, 96));
        try Graphics.FillRectangle(ZBrush, ZRect);
        finally
          ZBrush.Free;
        end;
      end;

      // Filled portion = current display value.
      Norm := Single(NormalisedDisplay);
      Fill := Track;
      Fill.Width := Norm * Track.Width;
      ColorVal := CurrentZoneColor;
      if ColorVal = clNone then ColorVal := Palette.Accent;
      FillBrush := TGPSolidBrush.Create(ColorToARGB(ColorVal));
      try Graphics.FillRectangle(FillBrush, Fill);
      finally
        FillBrush.Free;
      end;

      // Numeric value to the right of the bar.
      if FShowValueText then
      begin
        ValStr := FormatValue(DisplayValue);
        ACanvas.TextOut(
          Round(Track.X + Track.Width) + ScaleValue(8),
          Round(Track.Y) - ScaleValue(2),
          ValStr);
      end;
    end
    else
    begin
      // Vertical bar — fills from bottom up.
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font := Self.Font;
      ACanvas.Font.Color := EffectiveForeground;
      if Caption <> '' then
        ACanvas.TextOut(ClientPad, ClientPad, Caption);

      Y1 := ClientPad + CapHeight + ScaleValue(4);
      Y2 := Height - ClientPad - ValHeight - ScaleValue(2);

      Track.X := Width / 2 - TrackThick / 2;
      Track.Y := Y1;
      Track.Width := TrackThick;
      Track.Height := Y2 - Y1;

      TrackBrush := TGPSolidBrush.Create(
        ColorToARGB(Palette.NeutralLight));
      TrackPen := TGPPen.Create(ColorToARGB(EffectiveBorder),
        ScaleValue(1));
      try
        Graphics.FillRectangle(TrackBrush, Track);
        Graphics.DrawRectangle(TrackPen, Track);
      finally
        TrackPen.Free; TrackBrush.Free;
      end;

      // Zones (vertical strips).
      for Z in Zones do
      begin
        if (Z.EndValue <= Min) or (Z.StartValue >= Max) then Continue;
        var TopNorm := Single(NormaliseValue(Min, Max,
          System.Math.Min(Max, Z.EndValue)));
        var BotNorm := Single(NormaliseValue(Min, Max,
          System.Math.Max(Min, Z.StartValue)));
        ZRect.X := Track.X;
        ZRect.Width := Track.Width;
        ZRect.Y := Track.Y + (1 - TopNorm) * Track.Height;
        ZRect.Height := (TopNorm - BotNorm) * Track.Height;
        var ZBrush := TGPSolidBrush.Create(ColorToARGB(Z.Color, 96));
        try Graphics.FillRectangle(ZBrush, ZRect);
        finally
          ZBrush.Free;
        end;
      end;

      // Fill from bottom up.
      Norm := Single(NormalisedDisplay);
      Fill := Track;
      Fill.Y := Track.Y + (1 - Norm) * Track.Height;
      Fill.Height := Norm * Track.Height;
      ColorVal := CurrentZoneColor;
      if ColorVal = clNone then ColorVal := Palette.Accent;
      FillBrush := TGPSolidBrush.Create(ColorToARGB(ColorVal));
      try Graphics.FillRectangle(FillBrush, Fill);
      finally
        FillBrush.Free;
      end;

      // Numeric value below the bar.
      if FShowValueText then
      begin
        ValStr := FormatValue(DisplayValue);
        ACanvas.TextOut(
          Round(Width / 2 - ACanvas.TextWidth(ValStr) / 2),
          Round(Track.Y + Track.Height) + ScaleValue(4),
          ValStr);
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

end.
