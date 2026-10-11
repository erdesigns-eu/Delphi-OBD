//------------------------------------------------------------------------------
//  ERD.UI.GMeter
//
//  TOBDGMeter - OBD Studio Pro friction-circle card. It plots lateral and
//  longitudinal acceleration on a g-g diagram: concentric rings with
//  labels, the cross axes, a fading trail of the most recent samples held
//  in a ring buffer, the current operating dot, a peak marker and a value
//  read-out. A compact mode shows only the circle.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.GMeter;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>One lateral / longitudinal acceleration sample in g.</summary>
  TOBDGSample = record
    /// <summary>Lateral acceleration in g; positive is to the right.
    /// </summary>
    Lateral: Double;
    /// <summary>Longitudinal acceleration in g; positive accelerates,
    /// negative brakes.</summary>
    Longitudinal: Double;
  end;

  /// <summary>Friction-circle (g-g) acceleration card.</summary>
  TOBDGMeter = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FTrail: TArray<TOBDGSample>;
    FMaxG: Double;
    FTrailLength: Integer;
    FPeakG: Double;
    FPeakSample: TOBDGSample;
    FCompact: Boolean;
    FHasPeak: Boolean;
    procedure SetTitle(const AValue: string);
    procedure SetMaxG(AValue: Double);
    procedure SetTrailLength(AValue: Integer);
    procedure SetCompact(AValue: Boolean);
    function UsePreviewData: Boolean;
    function HeaderHeight: Integer;
    function CurrentSample: TOBDGSample;
    procedure PreviewTrail(out ATrail: TArray<TOBDGSample>;
      out APeak: TOBDGSample);
    procedure PaintHeader(var P: TOBDPainter; const R: TRect);
    procedure PaintCircle(var P: TOBDPainter; const R: TRect);
    procedure PaintReadout(var P: TOBDPainter; ATop: Integer;
      const ACur, APeak: TOBDGSample; APeakG: Double);
  protected
    /// <summary>Paints the friction-circle card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Repaints after a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Creates the meter with preview defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Card face colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Adds a sample to the trail and updates the peak.
    /// </summary>
    /// <param name="ALateral">Lateral acceleration in g.</param>
    /// <param name="ALongitudinal">Longitudinal acceleration in g.</param>
    procedure AddSample(ALateral, ALongitudinal: Double);
    /// <summary>Clears the trail without touching the peak.</summary>
    procedure ClearTrail;
    /// <summary>Resets the recorded peak to zero.</summary>
    procedure ResetPeak;
    /// <summary>Largest combined g recorded since the last reset.
    /// </summary>
    property PeakG: Double read FPeakG;
  published
    /// <summary>Card title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Full-scale acceleration of the outer ring in g.</summary>
    property MaxG: Double read FMaxG write SetMaxG;
    /// <summary>Number of samples kept in the trail ring buffer.</summary>
    property TrailLength: Integer read FTrailLength write SetTrailLength;
    /// <summary>Hides the header and read-out, showing only the circle.
    /// </summary>
    property Compact: Boolean read FCompact write SetCompact;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
  end;

implementation

uses
  Winapi.GDIPAPI;

const
  /// <summary>Em height of the read-out values, 96-DPI pixels.</summary>
  OBD_GMETER_VALUE = 14;

{ TOBDGMeter }

constructor TOBDGMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'Friction circle';
  FMaxG := 1.0;
  FTrailLength := 48;
  FCompact := False;
  FPeakG := 0.98;
  FHasPeak := True;
  FPeakSample.Lateral := 0.0;
  FPeakSample.Longitudinal := -0.98;
  Width := 292;
  Height := 352;
end;

procedure TOBDGMeter.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDGMeter.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDGMeter.AddSample(ALateral, ALongitudinal: Double);
var
  S: TOBDGSample;
  Mag: Double;
  N: Integer;
begin
  S.Lateral := ALateral;
  S.Longitudinal := ALongitudinal;
  N := Length(FTrail);
  SetLength(FTrail, N + 1);
  FTrail[N] := S;
  if (FTrailLength > 0) and (Length(FTrail) > FTrailLength) then
    FTrail := Copy(FTrail, Length(FTrail) - FTrailLength, FTrailLength);
  Mag := Sqrt(Sqr(ALateral) + Sqr(ALongitudinal));
  if Mag > FPeakG then
  begin
    FPeakG := Mag;
    FPeakSample := S;
    FHasPeak := True;
  end;
  Invalidate;
end;

procedure TOBDGMeter.ClearTrail;
begin
  SetLength(FTrail, 0);
  Invalidate;
end;

procedure TOBDGMeter.ResetPeak;
begin
  FPeakG := 0;
  FHasPeak := False;
  Invalidate;
end;

procedure TOBDGMeter.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDGMeter.SetMaxG(AValue: Double);
begin
  if AValue < 0.1 then
    AValue := 0.1;
  if SameValue(FMaxG, AValue) then
    Exit;
  FMaxG := AValue;
  Invalidate;
end;

procedure TOBDGMeter.SetTrailLength(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FTrailLength = AValue then
    Exit;
  FTrailLength := AValue;
  if Length(FTrail) > FTrailLength then
    FTrail := Copy(FTrail, Length(FTrail) - FTrailLength, FTrailLength);
  Invalidate;
end;

procedure TOBDGMeter.SetCompact(AValue: Boolean);
begin
  if FCompact = AValue then
    Exit;
  FCompact := AValue;
  Invalidate;
end;

function TOBDGMeter.UsePreviewData: Boolean;
begin
  Result := IsPreview and (Length(FTrail) = 0);
end;

function TOBDGMeter.HeaderHeight: Integer;
begin
  if FCompact then
    Result := 0
  else
    Result := ScaleValue(Metrics.Head);
end;

function TOBDGMeter.CurrentSample: TOBDGSample;
begin
  if Length(FTrail) > 0 then
    Result := FTrail[High(FTrail)]
  else
  begin
    Result.Lateral := 0;
    Result.Longitudinal := 0;
  end;
end;

procedure TOBDGMeter.PreviewTrail(out ATrail: TArray<TOBDGSample>;
  out APeak: TOBDGSample);
const
  LAT: array[0..5] of Double = (0.02, 0.03, 0.05, 0.08, 0.10, 0.12);
  LON: array[0..5] of Double = (0.0, -0.25, -0.55, -0.8, -0.9, -0.94);
var
  I: Integer;
begin
  SetLength(ATrail, Length(LAT));
  for I := 0 to High(LAT) do
  begin
    ATrail[I].Lateral := LAT[I];
    ATrail[I].Longitudinal := LON[I];
  end;
  APeak.Lateral := 0.0;
  APeak.Longitudinal := -0.98;
end;

procedure TOBDGMeter.PaintHeader(var P: TOBDPainter; const R: TRect);
var
  X, Y, ChipW: Integer;
  ChipText: string;
begin
  Y := R.Top + R.Height div 2;
  P.Glyph(glGForce, R.Left + P.S(26), Y, P.AccentText, 1);
  X := R.Left + P.S(46);
  P.Text(X, Y, FTitle, OBD_TEXT_TITLE, Palette.ForegroundText, twBold,
    taLeftJustify, R.Width - P.S(140));
  ChipText := Format('PEAK %.2f g', [FPeakG]);
  ChipW := P.ChipWidth(ChipText);
  P.Chip(R.Right - P.S(16) - ChipW, Y - P.S(10), ChipText, P.AccentText, True);
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TOBDGMeter.PaintCircle(var P: TOBDPainter; const R: TRect);
var
  CX, CY, Radius, RR, Mag: Single;
  Trail: TArray<TOBDGSample>;
  Peak, Cur: TOBDGSample;
  Pts: array of TGPPointF;
  I: Integer;
  DX, DY: Single;
  RingG: Double;
begin
  CX := R.Left + R.Width / 2;
  CY := R.Top + R.Height / 2;
  Radius := System.Math.Min(R.Width, R.Height) / 2 - P.SF(18);
  if Radius < 8 then
    Exit;

  // Outer boundary and concentric rings.
  P.Ellipse(CX - Radius, CY - Radius, 2 * Radius, 2 * Radius,
    Palette.GaugeFace, Palette.NeutralDark, 1.5);
  RingG := FMaxG / 2;
  while RingG < FMaxG - 0.001 do
  begin
    RR := Radius * (RingG / FMaxG);
    P.Ellipse(CX - RR, CY - RR, 2 * RR, 2 * RR, clNone, Palette.NeutralLight);
    P.Text(Round(CX + P.S(4)), Round(CY - RR - P.S(8)),
      Format('%.1f g', [RingG]), OBD_TEXT_CAPS, Palette.GaugeLabel);
    RingG := RingG + FMaxG / 2;
  end;
  P.Text(Round(CX + P.S(4)), Round(CY - Radius + P.S(10)),
    Format('%.1f g', [FMaxG]), OBD_TEXT_CAPS, Palette.GaugeLabel);

  // Cross axes.
  P.HLine(Round(CX - Radius), Round(CY), Round(2 * Radius),
    Palette.NeutralLight);
  P.VLine(Round(CX), Round(CY - Radius), Round(2 * Radius),
    Palette.NeutralLight);
  P.Caps(Round(CX), R.Top + P.S(8), 'Braking', taCenter);
  P.Caps(Round(CX), R.Bottom - P.S(6), 'Accelerating', taCenter);

  if UsePreviewData then
    PreviewTrail(Trail, Peak)
  else
  begin
    Trail := FTrail;
    Peak := FPeakSample;
  end;

  // Trail polyline.
  if Length(Trail) >= 2 then
  begin
    SetLength(Pts, Length(Trail));
    for I := 0 to High(Trail) do
      Pts[I] := OBDPointF(CX + Trail[I].Lateral / FMaxG * Radius,
        CY + Trail[I].Longitudinal / FMaxG * Radius);
    P.Lines(Pts, OBDMixColor(Palette.Accent, Palette.GaugeFace, 0.5),
      P.SF(2));
  end;

  // Peak marker.
  if FHasPeak or UsePreviewData then
  begin
    DX := CX + Peak.Lateral / FMaxG * Radius;
    DY := CY + Peak.Longitudinal / FMaxG * Radius;
    P.Ellipse(DX - P.SF(5), DY - P.SF(5), P.SF(10), P.SF(10), clNone,
      P.AccentText, 1.5);
  end;

  // Current dot.
  if Length(Trail) > 0 then
    Cur := Trail[High(Trail)]
  else
    Cur := CurrentSample;
  Mag := Sqrt(Sqr(Cur.Lateral) + Sqr(Cur.Longitudinal));
  if (Length(Trail) > 0) or (Mag > 0) then
  begin
    DX := CX + Cur.Lateral / FMaxG * Radius;
    DY := CY + Cur.Longitudinal / FMaxG * Radius;
    P.Ellipse(DX - P.SF(7), DY - P.SF(7), P.SF(14), P.SF(14), Palette.Accent,
      Palette.GaugeFace, 2);
  end;
end;

procedure TOBDGMeter.PaintReadout(var P: TOBDPainter; ATop: Integer;
  const ACur, APeak: TOBDGSample; APeakG: Double);
var
  ColW, I, TX: Integer;
  Caption, Value: string;
  Col: TColor;
begin
  ColW := Width div 3;
  for I := 0 to 2 do
  begin
    TX := P.S(16) + I * ColW;
    case I of
      0:
        begin
          Caption := 'Longitudinal';
          Value := Format('%.2f g', [ACur.Longitudinal]);
          Col := P.AccentText;
        end;
      1:
        begin
          Caption := 'Lateral';
          Value := Format('%.2f g', [ACur.Lateral]);
          Col := Palette.ForegroundText;
        end;
    else
      begin
        Caption := 'Peak';
        Value := Format('%.2f g', [APeakG]);
        Col := Palette.ForegroundText;
      end;
    end;
    P.Caps(TX, ATop, Caption);
    P.Text(TX, ATop + P.S(22), Value, OBD_GMETER_VALUE, Col, twBold);
  end;
end;

procedure TOBDGMeter.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Head, CircleBottom, ReadoutTop: Integer;
  CircleR: TRect;
  Cur, Peak: TOBDGSample;
  PeakG: Double;
  Trail: TArray<TOBDGSample>;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    Head := HeaderHeight;
    if not FCompact then
      PaintHeader(P, Rect(0, 0, Width, Head));

    if FCompact then
      CircleBottom := Height
    else
      CircleBottom := Height - ScaleValue(52);
    CircleR := Rect(ScaleValue(8), Head + ScaleValue(8), Width - ScaleValue(8),
      CircleBottom - ScaleValue(4));
    PaintCircle(P, CircleR);

    if not FCompact then
    begin
      if UsePreviewData then
      begin
        PreviewTrail(Trail, Peak);
        Cur := Trail[High(Trail)];
        PeakG := 0.98;
      end
      else
      begin
        Cur := CurrentSample;
        Peak := FPeakSample;
        PeakG := FPeakG;
      end;
      ReadoutTop := CircleBottom + ScaleValue(6);
      PaintReadout(P, ReadoutTop, Cur, Peak, PeakG);
    end;
  finally
    P.Free;
  end;
end;

end.
