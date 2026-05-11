//------------------------------------------------------------------------------
//  OBD.UI.Insights
//
//  Driver-insight widgets:
//
//    TOBDDriverScoreWidget  Smooth / moderate / aggressive
//                           score from accel histograms.
//    TOBDEcoScoreWidget     Eco score with brake / accel /
//                           idle breakdown.
//    TOBDTripSummaryCard    Distance / duration / avg speed
//                           / fuel summary card.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Insights;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>Driver smoothness score (0..100). Hosts feed
  /// the live score; the widget renders a coloured arc + the
  /// numeric value.</summary>
  TOBDDriverScoreWidget = class(TOBDCustomControl)
  strict private
    FScore:       Double;
    FCaptionFont: TFont;
    FValueFont:   TFont;
    procedure SetScore(AValue: Double);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  ScoreColor: TColor;
    function  ScoreText: string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Score: Double read FScore write SetScore;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>Eco score with breakdown bars (brake / accel /
  /// idle). All four values are in 0..100; the visual
  /// renders the headline + three small bars.</summary>
  TOBDEcoScoreWidget = class(TOBDCustomControl)
  strict private
    FOverall: Double;
    FBrake:   Double;
    FAccel:   Double;
    FIdle:    Double;
    FCaptionFont: TFont;
    FValueFont:   TFont;
    procedure SetOverall(AValue: Double);
    procedure SetBrake(AValue: Double);
    procedure SetAccel(AValue: Double);
    procedure SetIdle(AValue: Double);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure DrawBar(ACanvas: TCanvas; AGraphics: TGPGraphics;
      const ARect: TRect; const ALabel: string; AValue: Double);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property OverallScore: Double
      read FOverall write SetOverall;
    property BrakeScore: Double read FBrake write SetBrake;
    property AccelScore: Double read FAccel write SetAccel;
    property IdleScore:  Double read FIdle  write SetIdle;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>Trip summary: distance / duration / avg speed /
  /// fuel-used readout card.</summary>
  TOBDTripSummaryCard = class(TOBDCustomControl)
  strict private
    FDistanceKm:   Double;
    FDurationMin:  Double;
    FAvgSpeedKmh:  Double;
    FFuelL:        Double;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    procedure SetDistanceKm(AValue: Double);
    procedure SetDurationMin(AValue: Double);
    procedure SetAvgSpeedKmh(AValue: Double);
    procedure SetFuelL(AValue: Double);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property DistanceKm:  Double read FDistanceKm  write SetDistanceKm;
    property DurationMin: Double read FDurationMin write SetDurationMin;
    property AvgSpeedKmh: Double read FAvgSpeedKmh write SetAvgSpeedKmh;
    property FuelL:       Double read FFuelL       write SetFuelL;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

{ ---- TOBDDriverScoreWidget ------------------------------------- }

constructor TOBDDriverScoreWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 160;
  Height := 160;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 10;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 22;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDDriverScoreWidget.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDDriverScoreWidget.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDriverScoreWidget.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDDriverScoreWidget.SetScore(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FScore, AValue) then Exit;
  FScore := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDriverScoreWidget.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDDriverScoreWidget.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

function TOBDDriverScoreWidget.ScoreColor: TColor;
begin
  if FScore >= 80 then Result := Palette.Success
  else if FScore >= 60 then Result := EffectiveAccent
  else if FScore >= 40 then Result := Palette.Warning
  else Result := Palette.Danger;
end;

function TOBDDriverScoreWidget.ScoreText: string;
begin
  if FScore >= 80 then Result := 'smooth'
  else if FScore >= 60 then Result := 'moderate'
  else if FScore >= 40 then Result := 'aggressive'
  else Result := 'erratic';
end;

procedure TOBDDriverScoreWidget.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Cx, Cy, R: Single;
  Sweep: Single;
  S: string;
  ScoreCol: TColor;
begin
  Cx := Width  / 2;
  Cy := Height / 2;
  R  := System.Math.Min(Width, Height) / 2 - ScaleValue(10);
  if R <= 0 then Exit;
  ScoreCol := ScoreColor;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    Pen := TGPPen.Create(ColorToARGB(Palette.NeutralLight),
      ScaleValue(8));
    try
      Graphics.DrawArc(Pen, Cx - R, Cy - R, R * 2, R * 2,
        135, 270);
    finally
      Pen.Free;
    end;
    Sweep := Single(FScore / 100) * 270;
    Pen := TGPPen.Create(ColorToARGB(ScoreCol), ScaleValue(8));
    try
      Graphics.DrawArc(Pen, Cx - R, Cy - R, R * 2, R * 2,
        135, Sweep);
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := ScoreCol;
  S := IntToStr(Round(FScore));
  ACanvas.TextOut(
    Round(Cx - ACanvas.TextWidth(S) / 2),
    Round(Cy - ACanvas.TextHeight(S) / 2),
    S);
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  S := ScoreText;
  ACanvas.TextOut(
    Round(Cx - ACanvas.TextWidth(S) / 2),
    Round(Cy + R * 0.55), S);
end;

{ ---- TOBDEcoScoreWidget --------------------------------------- }

constructor TOBDEcoScoreWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 240;
  Height := 140;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 16;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDEcoScoreWidget.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDEcoScoreWidget.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDEcoScoreWidget.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure ClampToHundred(var AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
end;

procedure TOBDEcoScoreWidget.SetOverall(AValue: Double);
begin
  ClampToHundred(AValue);
  if SameValue(FOverall, AValue) then Exit;
  FOverall := AValue; NotifyBindings; Repaint;
end;

procedure TOBDEcoScoreWidget.SetBrake(AValue: Double);
begin
  ClampToHundred(AValue);
  if SameValue(FBrake, AValue) then Exit;
  FBrake := AValue; NotifyBindings; Repaint;
end;

procedure TOBDEcoScoreWidget.SetAccel(AValue: Double);
begin
  ClampToHundred(AValue);
  if SameValue(FAccel, AValue) then Exit;
  FAccel := AValue; NotifyBindings; Repaint;
end;

procedure TOBDEcoScoreWidget.SetIdle(AValue: Double);
begin
  ClampToHundred(AValue);
  if SameValue(FIdle, AValue) then Exit;
  FIdle := AValue; NotifyBindings; Repaint;
end;

procedure TOBDEcoScoreWidget.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDEcoScoreWidget.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDEcoScoreWidget.DrawBar(ACanvas: TCanvas;
  AGraphics: TGPGraphics; const ARect: TRect;
  const ALabel: string; AValue: Double);
var
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Track, Fill: TGPRectF;
  Pad: Integer;
  Col: TColor;
begin
  Pad := ScaleValue(2);
  Track.X := ARect.Left + ScaleValue(64);
  Track.Y := ARect.Top + Pad;
  Track.Width  := ARect.Right - Track.X - ScaleValue(48);
  Track.Height := ARect.Height - 2 * Pad;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(ARect.Left,
    ARect.Top + (ARect.Height - ACanvas.TextHeight('M')) div 2,
    ALabel);

  if AValue >= 75 then Col := Palette.Success
  else if AValue >= 50 then Col := EffectiveAccent
  else if AValue >= 25 then Col := Palette.Warning
  else Col := Palette.Danger;

  Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
  try
    AGraphics.FillRectangle(Brush, Track);
  finally
    Brush.Free;
  end;
  Fill := Track;
  Fill.Width := Single(AValue / 100) * Track.Width;
  Brush := TGPSolidBrush.Create(ColorToARGB(Col));
  try
    AGraphics.FillRectangle(Brush, Fill);
  finally
    Brush.Free;
  end;
  Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
  try
    AGraphics.DrawRectangle(Pen, Track);
  finally
    Pen.Free;
  end;
  ACanvas.Font.Color := Col;
  ACanvas.TextOut(ARect.Right - ScaleValue(40),
    ARect.Top + (ARect.Height - ACanvas.TextHeight('M')) div 2,
    Format('%.0f', [AValue]));
end;

procedure TOBDEcoScoreWidget.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pad, RowH: Integer;
  HeadlineY: Integer;
  R: TRect;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveAccent;
  ACanvas.TextOut(Pad, Pad,
    Format('Eco %.0f', [FOverall]));
  HeadlineY := Pad + ACanvas.TextHeight('Mg') + ScaleValue(6);
  RowH := ScaleValue(20);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    R := Rect(Pad, HeadlineY,
      Width - Pad, HeadlineY + RowH);
    DrawBar(ACanvas, Graphics, R, 'brake', FBrake);
    R := Rect(Pad, HeadlineY + RowH,
      Width - Pad, HeadlineY + 2 * RowH);
    DrawBar(ACanvas, Graphics, R, 'accel', FAccel);
    R := Rect(Pad, HeadlineY + 2 * RowH,
      Width - Pad, HeadlineY + 3 * RowH);
    DrawBar(ACanvas, Graphics, R, 'idle',  FIdle);
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDTripSummaryCard -------------------------------------- }

constructor TOBDTripSummaryCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 280;
  Height := 130;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 12;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDTripSummaryCard.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDTripSummaryCard.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDTripSummaryCard.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDTripSummaryCard.SetDistanceKm(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FDistanceKm, AValue) then Exit;
  FDistanceKm := AValue; NotifyBindings; Repaint;
end;

procedure TOBDTripSummaryCard.SetDurationMin(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FDurationMin, AValue) then Exit;
  FDurationMin := AValue; NotifyBindings; Repaint;
end;

procedure TOBDTripSummaryCard.SetAvgSpeedKmh(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FAvgSpeedKmh, AValue) then Exit;
  FAvgSpeedKmh := AValue; NotifyBindings; Repaint;
end;

procedure TOBDTripSummaryCard.SetFuelL(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FFuelL, AValue) then Exit;
  FFuelL := AValue; NotifyBindings; Repaint;
end;

procedure TOBDTripSummaryCard.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDTripSummaryCard.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDTripSummaryCard.PaintControl(ACanvas: TCanvas);
var
  Pad, Y: Integer;

  procedure Row(const ALabel, AValue: string);
  begin
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y, ALabel);
    ACanvas.Font := FValueFont;
    ACanvas.Font.Color := EffectiveAccent;
    ACanvas.TextOut(Pad + ScaleValue(120), Y, AValue);
    Inc(Y, ACanvas.TextHeight('Mg') + ScaleValue(2));
  end;

begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(10);
  Y := Pad;
  Row('distance',  Format('%.1f km',     [FDistanceKm]));
  Row('duration',  Format('%.0f min',    [FDurationMin]));
  Row('avg speed', Format('%.1f km/h',   [FAvgSpeedKmh]));
  Row('fuel',      Format('%.2f L',      [FFuelL]));
end;

end.
