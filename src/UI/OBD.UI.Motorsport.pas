//------------------------------------------------------------------------------
//  OBD.UI.Motorsport
//
//  Motorsport / track visuals:
//
//    TOBDLapTrackMap      Track outline + current-position
//                         marker + start/finish line.
//    TOBDHUDOverlay       Borderless layered always-on-top
//                         overlay window for HUD displays.
//    TOBDPredictiveLap    Predicted lap time vs personal-best
//                         delta readout.
//    TOBDGForceVisualiser XY g-vector with traction-circle
//                         trail of the last N samples.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Motorsport;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Data.Bind.Components,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>One waypoint on the track polyline.</summary>
  TOBDTrackPoint = record
    X, Y: Single;     // normalised 0..1
  end;

  /// <summary>Track outline + position marker + start line.
  /// Host pushes the track shape once at session start, then
  /// updates <see cref="PositionFraction"/> as the car moves
  /// along the lap.</summary>
  TOBDLapTrackMap = class(TOBDCustomControl)
  strict private
    FTrack:    TArray<TOBDTrackPoint>;
    FPosition: Single;       // 0..1
    procedure SetPositionFraction(AValue: Single);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadTrack(const APoints: array of TOBDTrackPoint);
    procedure Clear;
  published
    /// <summary>Position along the lap (0..1, wraps).</summary>
    property PositionFraction: Single
      read FPosition write SetPositionFraction;
  end;

  /// <summary>Borderless always-on-top transparent host window
  /// for HUD overlays. Hosts dock their gauge / lamp / score
  /// widgets as children; this form just provides the always-
  /// on-top borderless framing.</summary>
  TOBDHUDOverlay = class(TForm)
  strict private
    FOpacity: Byte;
    FLockedTopmost: Boolean;
    procedure SetOpacityValue(AValue: Byte);
    procedure SetLockedTopmost(AValue: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Window opacity 0..255. Default 220.</summary>
    property OverlayOpacity: Byte
      read FOpacity write SetOpacityValue default 220;
    /// <summary>When True (default) the form re-asserts its
    /// always-on-top status if the host clobbers it.</summary>
    property LockedTopmost: Boolean
      read FLockedTopmost write SetLockedTopmost default True;
  end;

  /// <summary>Predictive lap-time widget. Current lap timer +
  /// predicted final + delta-from-PB lozenge (green / red).
  /// </summary>
  TOBDPredictiveLap = class(TOBDCustomControl)
  strict private
    FCurrentMs:   Cardinal;
    FPredictedMs: Cardinal;
    FBestMs:      Cardinal;
    FDeltaMs:     Integer;
    FCaptionFont: TFont;
    FValueFont:   TFont;
    procedure SetCurrentMs(AValue: Cardinal);
    procedure SetPredictedMs(AValue: Cardinal);
    procedure SetBestMs(AValue: Cardinal);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure RecomputeDelta;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure UpdateSample(ACurrentMs, APredictedMs: Cardinal);
    property DeltaMs: Integer read FDeltaMs;
  published
    property CurrentMs:   Cardinal read FCurrentMs   write SetCurrentMs;
    property PredictedMs: Cardinal read FPredictedMs write SetPredictedMs;
    property BestMs:      Cardinal read FBestMs      write SetBestMs;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property ValueFont:   TFont read FValueFont write SetValueFont;
  end;

  /// <summary>XY g-vector visualiser with a fading trail of
  /// the last <see cref="TrailCapacity"/> samples. Renders a
  /// circle (traction circle) with the live vector ending in
  /// a dot.</summary>
  TOBDGForceVisualiser = class(TOBDCustomControl)
  strict private
    FXg, FYg:        Double;
    FMaxG:           Double;
    FTrail:          TList<TPointF>;
    FTrailCapacity:  Integer;
    procedure SetXg(AValue: Double);
    procedure SetYg(AValue: Double);
    procedure SetMaxG(AValue: Double);
    procedure SetTrailCapacity(AValue: Integer);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Pushes one (lateral g, longitudinal g) sample.
    /// </summary>
    procedure PushSample(AXg, AYg: Double);
    procedure ClearTrail;
  published
    /// <summary>Lateral g. Positive = right turn.</summary>
    property Xg: Double read FXg write SetXg;
    /// <summary>Longitudinal g. Positive = acceleration.
    /// </summary>
    property Yg: Double read FYg write SetYg;
    /// <summary>Traction-circle radius in g. Default 1.4.
    /// </summary>
    property MaxG: Double read FMaxG write SetMaxG;
    property TrailCapacity: Integer
      read FTrailCapacity write SetTrailCapacity default 60;
  end;

implementation

{ ---- TOBDLapTrackMap --------------------------------------------------- }

constructor TOBDLapTrackMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 280;
  Height := 200;
end;

procedure TOBDLapTrackMap.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDLapTrackMap.SetPositionFraction(AValue: Single);
begin
  if AValue < 0 then AValue := AValue - Trunc(AValue);
  while AValue >= 1.0 do AValue := AValue - 1.0;
  if SameValue(FPosition, AValue) then Exit;
  FPosition := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDLapTrackMap.LoadTrack(
  const APoints: array of TOBDTrackPoint);
var I: Integer;
begin
  SetLength(FTrack, Length(APoints));
  for I := 0 to High(APoints) do FTrack[I] := APoints[I];
  NotifyBindings;
  Repaint;
end;

procedure TOBDLapTrackMap.Clear;
begin
  SetLength(FTrack, 0);
  NotifyBindings;
  Repaint;
end;

procedure TOBDLapTrackMap.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Brush: TGPSolidBrush;
  Pad: Integer;
  PlotW, PlotH: Integer;
  Pts: array of TGPPointF;
  I, MarkIdx: Integer;
  MarkX, MarkY: Single;
begin
  if Length(FTrack) < 2 then Exit;
  Pad := ScaleValue(8);
  PlotW := Width  - 2 * Pad;
  PlotH := Height - 2 * Pad;
  SetLength(Pts, Length(FTrack));
  for I := 0 to High(FTrack) do
  begin
    Pts[I].X := Pad + FTrack[I].X * PlotW;
    Pts[I].Y := Pad + FTrack[I].Y * PlotH;
  end;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Pen := TGPPen.Create(ColorToARGB(EffectiveAccent), ScaleValue(2));
  Pen.SetLineJoin(LineJoinRound);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.DrawLines(Pen, PGPPointF(@Pts[0]), Length(Pts));

    MarkIdx := System.Math.Min(
      Trunc(FPosition * (Length(FTrack) - 1)),
      Length(FTrack) - 1);
    if MarkIdx < 0 then MarkIdx := 0;
    MarkX := Pts[MarkIdx].X;
    MarkY := Pts[MarkIdx].Y;
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Danger));
    try
      Graphics.FillEllipse(Brush,
        MarkX - ScaleValue(6), MarkY - ScaleValue(6),
        ScaleValue(12), ScaleValue(12));
    finally
      Brush.Free;
    end;
  finally
    Pen.Free;
    Graphics.Free;
  end;
end;

{ ---- TOBDHUDOverlay ---------------------------------------------------- }

constructor TOBDHUDOverlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Borderless layered always-on-top form. The VCL's
  // AlphaBlend + AlphaBlendValue properties wire the
  // SetLayeredWindowAttributes call internally as soon as
  // the handle is created, so we just set them here. The
  // explicit WS_EX_LAYERED in CreateParams covers the early-
  // handle case before AlphaBlend has run its own attribute
  // application — also gives us WS_EX_TOOLWINDOW so the
  // overlay doesn't appear in the taskbar.
  BorderStyle   := bsNone;
  FormStyle     := fsStayOnTop;
  AlphaBlend    := True;
  FOpacity      := 220;
  AlphaBlendValue := FOpacity;
  FLockedTopmost := True;
  Color         := clBlack;
end;

procedure TOBDHUDOverlay.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_LAYERED or
    WS_EX_TOOLWINDOW;
end;

procedure TOBDHUDOverlay.SetOpacityValue(AValue: Byte);
begin
  if FOpacity = AValue then Exit;
  FOpacity := AValue;
  // AlphaBlendValue setter re-applies the Win32 layered
  // attributes when the handle exists; safe to set before the
  // form is shown too (will be applied on handle creation).
  AlphaBlendValue := AValue;
end;

procedure TOBDHUDOverlay.SetLockedTopmost(AValue: Boolean);
begin
  if FLockedTopmost = AValue then Exit;
  FLockedTopmost := AValue;
  if FLockedTopmost then FormStyle := fsStayOnTop
  else                   FormStyle := fsNormal;
end;

{ ---- TOBDPredictiveLap ------------------------------------------------- }

constructor TOBDPredictiveLap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 260;
  Height := 100;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 20;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDPredictiveLap.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDPredictiveLap.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDPredictiveLap.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDPredictiveLap.RecomputeDelta;
begin
  if (FBestMs > 0) and (FPredictedMs > 0) then
    FDeltaMs := Integer(FPredictedMs) - Integer(FBestMs)
  else
    FDeltaMs := 0;
end;

procedure TOBDPredictiveLap.SetCurrentMs(AValue: Cardinal);
begin
  if FCurrentMs = AValue then Exit;
  FCurrentMs := AValue; NotifyBindings; Repaint;
end;

procedure TOBDPredictiveLap.SetPredictedMs(AValue: Cardinal);
begin
  if FPredictedMs = AValue then Exit;
  FPredictedMs := AValue;
  RecomputeDelta;
  NotifyBindings;
  Repaint;
end;

procedure TOBDPredictiveLap.SetBestMs(AValue: Cardinal);
begin
  if FBestMs = AValue then Exit;
  FBestMs := AValue;
  RecomputeDelta;
  NotifyBindings;
  Repaint;
end;

procedure TOBDPredictiveLap.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDPredictiveLap.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDPredictiveLap.UpdateSample(
  ACurrentMs, APredictedMs: Cardinal);
begin
  FCurrentMs   := ACurrentMs;
  FPredictedMs := APredictedMs;
  RecomputeDelta;
  NotifyBindings;
  Repaint;
end;

function FormatLapTime(AMs: Cardinal): string;
var
  M, S, H: Cardinal;
begin
  H := (AMs mod 1000) div 10;
  S := (AMs div 1000) mod 60;
  M := AMs div 60000;
  Result := Format('%d:%2.2d.%2.2d', [M, S, H]);
end;

procedure TOBDPredictiveLap.PaintControl(ACanvas: TCanvas);
var
  Pad: Integer;
  Pred, Cap: string;
  DeltaCap: string;
  Col: TColor;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(10);

  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(Pad, Pad, Format('current %s',
    [FormatLapTime(FCurrentMs)]));

  ACanvas.Font := FValueFont;
  Pred := 'predicted ' + FormatLapTime(FPredictedMs);
  ACanvas.Font.Color := EffectiveAccent;
  ACanvas.TextOut(Pad,
    Pad + ACanvas.TextHeight('Mg') + ScaleValue(2), Pred);

  if FDeltaMs <> 0 then
  begin
    if FDeltaMs < 0 then Col := Palette.Success
    else                 Col := Palette.Danger;
    if FDeltaMs < 0 then
      DeltaCap := Format('-%d.%2.2d',
        [Abs(FDeltaMs) div 1000, (Abs(FDeltaMs) mod 1000) div 10])
    else
      DeltaCap := Format('+%d.%2.2d',
        [FDeltaMs div 1000, (FDeltaMs mod 1000) div 10]);
    ACanvas.Font := FValueFont;
    ACanvas.Font.Color := Col;
    Cap := 'Δ ' + DeltaCap;
    ACanvas.TextOut(Width - Pad - ACanvas.TextWidth(Cap),
      Pad + ACanvas.TextHeight('Mg') + ScaleValue(2), Cap);
  end;
end;

{ ---- TOBDGForceVisualiser ---------------------------------------------- }

constructor TOBDGForceVisualiser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 200;
  Height := 200;
  FMaxG := 1.4;
  FTrail := TList<TPointF>.Create;
  FTrailCapacity := 60;
end;

destructor TOBDGForceVisualiser.Destroy;
begin
  FTrail.Free;
  inherited;
end;

procedure TOBDGForceVisualiser.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDGForceVisualiser.SetXg(AValue: Double);
begin
  if SameValue(FXg, AValue) then Exit;
  FXg := AValue; NotifyBindings; Repaint;
end;

procedure TOBDGForceVisualiser.SetYg(AValue: Double);
begin
  if SameValue(FYg, AValue) then Exit;
  FYg := AValue; NotifyBindings; Repaint;
end;

procedure TOBDGForceVisualiser.SetMaxG(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FMaxG, AValue) then Exit;
  FMaxG := AValue; Repaint;
end;

procedure TOBDGForceVisualiser.SetTrailCapacity(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 4096 then AValue := 4096;
  if FTrailCapacity = AValue then Exit;
  FTrailCapacity := AValue;
  while FTrail.Count > FTrailCapacity do FTrail.Delete(0);
  Repaint;
end;

procedure TOBDGForceVisualiser.PushSample(AXg, AYg: Double);
var P: TPointF;
begin
  FXg := AXg;
  FYg := AYg;
  P.X := AXg;
  P.Y := AYg;
  FTrail.Add(P);
  while FTrail.Count > FTrailCapacity do FTrail.Delete(0);
  NotifyBindings;
  Repaint;
end;

procedure TOBDGForceVisualiser.ClearTrail;
begin
  FTrail.Clear;
  Repaint;
end;

procedure TOBDGForceVisualiser.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Brush: TGPSolidBrush;
  Cx, Cy, R: Single;
  I: Integer;
  X, Y: Single;
  Alpha: Byte;
begin
  Cx := Width  / 2;
  Cy := Height / 2;
  R  := System.Math.Min(Width, Height) / 2 - ScaleValue(8);
  if R <= 0 then Exit;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.DrawEllipse(Pen, Cx - R, Cy - R, R * 2, R * 2);
      Graphics.DrawLine(Pen, Cx, Cy - R, Cx, Cy + R);
      Graphics.DrawLine(Pen, Cx - R, Cy, Cx + R, Cy);
    finally
      Pen.Free;
    end;

    // Trail (oldest = transparent, newest = solid).
    for I := 0 to FTrail.Count - 1 do
    begin
      X := Cx + Single(FTrail[I].X / FMaxG) * R;
      Y := Cy - Single(FTrail[I].Y / FMaxG) * R;
      Alpha := Byte(40 + Round(180 * I / System.Math.Max(FTrail.Count - 1, 1)));
      Brush := TGPSolidBrush.Create(
        ColorToARGB(EffectiveAccent, Alpha));
      try
        Graphics.FillEllipse(Brush,
          X - 2, Y - 2, 4, 4);
      finally
        Brush.Free;
      end;
    end;

    // Live vector.
    X := Cx + Single(FXg / FMaxG) * R;
    Y := Cy - Single(FYg / FMaxG) * R;
    Pen := TGPPen.Create(ColorToARGB(Palette.Danger),
      ScaleValue(2));
    try
      Graphics.DrawLine(Pen, Cx, Cy, X, Y);
    finally
      Pen.Free;
    end;
    Brush := TGPSolidBrush.Create(
      ColorToARGB(Palette.Danger));
    try
      Graphics.FillEllipse(Brush,
        X - 4, Y - 4, 8, 8);
    finally
      Brush.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

end.
