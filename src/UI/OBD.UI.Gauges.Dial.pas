//------------------------------------------------------------------------------
//  OBD.UI.Gauges.Dial
//
//  Dial-shaped gauges: circular full-sweep, tachometer (with
//  redline), partial-arc, and combo (dial + centred digital
//  readout).
//
//  All four share the same `DrawDialFrame` / `DrawNeedle`
//  helpers from TOBDDialBase. Subclasses override:
//    StartAngleDeg / SweepAngleDeg  scale-geometry hooks
//    PaintExtras                    optional overlay paint
//
//  Anti-aliased paint via GDI+ for smooth arcs / needles;
//  TCanvas for text + ticks (faster, sharper at small sizes).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.Dial;

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
  /// <summary>Internal base for circular / tachometer / arc /
  /// combo gauges. Owns the dial-frame paint pipeline.</summary>
  TOBDDialBase = class(TOBDGaugeBase)
  protected
    /// <summary>Angle (degrees, GDI+ convention: 0° at 3
    /// o'clock, sweeping clockwise) where the value scale
    /// starts.</summary>
    function StartAngleDeg: Single; virtual; abstract;
    /// <summary>Angular sweep of the value scale (degrees).</summary>
    function SweepAngleDeg: Single; virtual; abstract;
    /// <summary>Override to paint an overlay (e.g. combo's
    /// digital readout in the centre). Called after needle.</summary>
    procedure PaintExtras(AGraphics: TGPGraphics;
      const ABounds: TRectF); virtual;
    procedure PaintControl(ACanvas: TCanvas); override;

    function ValueToAngle(AValue: Double): Single;
    procedure DrawDialFrame(AGraphics: TGPGraphics;
      const ABounds: TRectF);
    procedure DrawZones(AGraphics: TGPGraphics;
      const ABounds: TRectF);
    procedure DrawTicksAndLabels(ACanvas: TCanvas;
      const ABounds: TRectF);
    procedure DrawNeedle(AGraphics: TGPGraphics;
      const ABounds: TRectF);
    procedure DrawCaption(ACanvas: TCanvas; const ABounds: TRectF);
  end;

  /// <summary>Classic round dial with the value scale
  /// sweeping from roughly 7 o'clock clockwise to 5 o'clock
  /// (~240° sweep).</summary>
  TOBDCircularGauge = class(TOBDDialBase)
  protected
    function StartAngleDeg: Single; override;
    function SweepAngleDeg: Single; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>Tachometer-styled circular gauge. Adds a default
  /// redline zone (top 12% of the range, red) and warning zone
  /// (preceding 8%, amber). Hosts can override
  /// <c>Redline</c> / <c>RedlineColor</c> /
  /// <c>WarningStart</c> at runtime.</summary>
  TOBDTachometer = class(TOBDCircularGauge)
  strict private
    FRedline:      Double;
    FRedlineColor: TColor;
    FWarningStart: Double;
    FWarningColor: TColor;
    procedure SetRedline(AValue: Double);
    procedure SetRedlineColor(AValue: TColor);
    procedure SetWarningStart(AValue: Double);
    procedure SetWarningColor(AValue: TColor);
    procedure RebuildZones;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Value above which the redline zone paints.
    /// Default 0.88 × Max.</summary>
    property Redline:      Double read FRedline      write SetRedline;
    /// <summary>Redline zone colour. Default red.</summary>
    property RedlineColor: TColor read FRedlineColor write SetRedlineColor default $003C3CFF;
    /// <summary>Lower bound of the amber warning zone.
    /// Default 0.80 × Max.</summary>
    property WarningStart: Double read FWarningStart write SetWarningStart;
    /// <summary>Warning zone colour. Default amber.</summary>
    property WarningColor: TColor read FWarningColor write SetWarningColor default $0000A8FF;
  end;

  /// <summary>Partial-arc dial — half-circle or 270° variant.
  /// Configurable start / sweep angles let hosts dial in any
  /// shape from quarter-arc to nearly full-circle.</summary>
  TOBDArcGauge = class(TOBDDialBase)
  strict private
    FStartAngle: Single;
    FSweepAngle: Single;
    procedure SetStartAngle(AValue: Single);
    procedure SetSweepAngle(AValue: Single);
  protected
    function StartAngleDeg: Single; override;
    function SweepAngleDeg: Single; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Where the scale begins, GDI+ angle convention
    /// (0° = 3 o'clock, clockwise). Default 135° (8 o'clock).</summary>
    property StartAngle: Single read FStartAngle write SetStartAngle;
    /// <summary>Angular sweep in degrees.
    /// Default 270°.</summary>
    property SweepAngle: Single read FSweepAngle write SetSweepAngle;
  end;

  /// <summary>Circular gauge with the value reprinted as a
  /// large digital readout in the centre — the modern-car
  /// cluster look.</summary>
  TOBDComboGauge = class(TOBDCircularGauge)
  strict private
    FCenterFont: TFont;
    procedure FontChange(Sender: TObject);
    procedure SetCenterFont(AValue: TFont);
  protected
    procedure PaintExtras(AGraphics: TGPGraphics;
      const ABounds: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Font for the centre digital readout. Defaults
    /// to a 2x-scale headline.</summary>
    property CenterFont: TFont read FCenterFont write SetCenterFont;
  end;

implementation

uses
  System.UITypes;

const
  // Default circular dial sweep: from 135° clockwise through
  // 360° back to 45°. GDI+ angles: 0 = 3 o'clock, increasing
  // clockwise. So Start=135 (8 o'clock), Sweep=270 (back to
  // 4 o'clock).
  DEFAULT_DIAL_START = 135;
  DEFAULT_DIAL_SWEEP = 270;

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

{ ---- TOBDDialBase --------------------------------------------------------- }

procedure TOBDDialBase.PaintExtras(AGraphics: TGPGraphics;
  const ABounds: TRectF);
begin
  // Optional override; no-op default.
end;

function TOBDDialBase.ValueToAngle(AValue: Double): Single;
begin
  Result := StartAngleDeg +
    Single(NormaliseValue(Min, Max, AValue)) * SweepAngleDeg;
end;

procedure TOBDDialBase.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Bounds:   TRectF;
  S:        Integer;
begin
  // Compute a square bounds centred in the client rect.
  S := Min(Width, Height);
  Bounds.X := (Width  - S) / 2;
  Bounds.Y := (Height - S) / 2;
  Bounds.Width  := S;
  Bounds.Height := S;

  // Shrink inside for tick / label headroom.
  Bounds.Inflate(-ScaleValue(8), -ScaleValue(8));

  // Background already filled by base; paint dial elements.
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    DrawDialFrame(Graphics, Bounds);
    DrawZones(Graphics, Bounds);
    DrawTicksAndLabels(ACanvas, Bounds);
    DrawNeedle(Graphics, Bounds);
    PaintExtras(Graphics, Bounds);
  finally
    Graphics.Free;
  end;
  DrawCaption(ACanvas, Bounds);
end;

procedure TOBDDialBase.DrawDialFrame(AGraphics: TGPGraphics;
  const ABounds: TRectF);
var
  Pen:    TGPPen;
  Brush:  TGPSolidBrush;
  FaceCol, BorderCol: TColor;
  Inner:  TGPRectF;
begin
  FaceCol   := Palette.GaugeFace;
  if Self.StyleBackground <> clDefault then FaceCol := Self.StyleBackground;
  BorderCol := EffectiveBorder;

  // Face fill.
  Brush := TGPSolidBrush.Create(ColorToARGB(FaceCol));
  try
    Inner.X := ABounds.X;
    Inner.Y := ABounds.Y;
    Inner.Width  := ABounds.Width;
    Inner.Height := ABounds.Height;
    AGraphics.FillEllipse(Brush, Inner);
  finally Brush.Free; end;

  // Border ring.
  Pen := TGPPen.Create(ColorToARGB(BorderCol), ScaleValue(2));
  try
    AGraphics.DrawEllipse(Pen, Inner);
  finally Pen.Free; end;
end;

procedure TOBDDialBase.DrawZones(AGraphics: TGPGraphics;
  const ABounds: TRectF);
var
  Z:       TOBDGaugeZone;
  StartA:  Single;
  SweepA:  Single;
  Pen:     TGPPen;
  Rect:    TGPRectF;
  Inset:   Single;
begin
  // Zones paint as thin coloured arcs just inside the bezel.
  if Length(Zones) = 0 then Exit;
  Inset := ScaleValue(6);
  Rect.X := ABounds.X + Inset;
  Rect.Y := ABounds.Y + Inset;
  Rect.Width  := ABounds.Width  - Inset * 2;
  Rect.Height := ABounds.Height - Inset * 2;
  for Z in Zones do
  begin
    StartA := ValueToAngle(Z.StartValue);
    SweepA := ValueToAngle(Z.EndValue) - StartA;
    if SweepA <= 0 then Continue;
    Pen := TGPPen.Create(ColorToARGB(Z.Color), ScaleValue(6));
    try
      AGraphics.DrawArc(Pen, Rect, StartA, SweepA);
    finally Pen.Free; end;
  end;
end;

procedure TOBDDialBase.DrawTicksAndLabels(ACanvas: TCanvas;
  const ABounds: TRectF);
var
  Cfg:    TOBDGaugeTickConfig;
  V:      Double;
  AngleDeg, AngleRad: Single;
  Cx, Cy, R, RIn, ROut, RLbl: Single;
  X1, Y1, X2, Y2, Lx, Ly: Single;
  TickColor: TColor;
  LabelColor: TColor;
  S: string;
  TextW, TextH: Integer;
  Step:   Double;
  M:      Integer;
begin
  Cfg := TickConfig;
  if Cfg.MajorInterval <= 0 then Exit;
  if SameValue(Min, Max) then Exit;

  Cx := ABounds.X + ABounds.Width  / 2;
  Cy := ABounds.Y + ABounds.Height / 2;
  R  := System.Math.Min(ABounds.Width, ABounds.Height) / 2;
  ROut := R - ScaleValue(10);
  RIn  := R - ScaleValue(20);
  RLbl := R - ScaleValue(34);

  TickColor  := Palette.GaugeTick;
  LabelColor := Palette.GaugeLabel;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Font := Self.Font;
  ACanvas.Font.Color := LabelColor;

  V := Min;
  while V <= Max + 1e-9 do
  begin
    AngleDeg := ValueToAngle(V);
    AngleRad := DegToRad(AngleDeg);
    X1 := Cx + RIn * Cos(AngleRad);
    Y1 := Cy + RIn * Sin(AngleRad);
    X2 := Cx + ROut * Cos(AngleRad);
    Y2 := Cy + ROut * Sin(AngleRad);
    ACanvas.Pen.Color := TickColor;
    ACanvas.Pen.Width := ScaleValue(2);
    ACanvas.MoveTo(Round(X1), Round(Y1));
    ACanvas.LineTo(Round(X2), Round(Y2));

    if Cfg.ShowLabels then
    begin
      S := FormatFloat('0.' + StringOfChar('0', Cfg.LabelDecimals), V);
      if Cfg.LabelDecimals = 0 then S := FormatFloat('0', V);
      TextW := ACanvas.TextWidth(S);
      TextH := ACanvas.TextHeight(S);
      Lx := Cx + RLbl * Cos(AngleRad) - TextW / 2;
      Ly := Cy + RLbl * Sin(AngleRad) - TextH / 2;
      ACanvas.TextOut(Round(Lx), Round(Ly), S);
    end;

    // Minor ticks.
    if (Cfg.MinorTicksPerMajor > 0) and (V + Cfg.MajorInterval <= Max + 1e-9) then
    begin
      Step := Cfg.MajorInterval / (Cfg.MinorTicksPerMajor + 1);
      for M := 1 to Cfg.MinorTicksPerMajor do
      begin
        AngleDeg := ValueToAngle(V + M * Step);
        AngleRad := DegToRad(AngleDeg);
        X1 := Cx + (RIn + ScaleValue(5)) * Cos(AngleRad);
        Y1 := Cy + (RIn + ScaleValue(5)) * Sin(AngleRad);
        X2 := Cx + ROut * Cos(AngleRad);
        Y2 := Cy + ROut * Sin(AngleRad);
        ACanvas.Pen.Width := ScaleValue(1);
        ACanvas.MoveTo(Round(X1), Round(Y1));
        ACanvas.LineTo(Round(X2), Round(Y2));
      end;
    end;

    V := V + Cfg.MajorInterval;
  end;
end;

procedure TOBDDialBase.DrawNeedle(AGraphics: TGPGraphics;
  const ABounds: TRectF);
var
  Cx, Cy, R: Single;
  Angle: Single;
  Pen:   TGPPen;
  Brush: TGPSolidBrush;
  HubR:  Single;
  NX, NY, NLen: Single;
  Color: TColor;
  Hub:   TGPRectF;
begin
  Cx := ABounds.X + ABounds.Width  / 2;
  Cy := ABounds.Y + ABounds.Height / 2;
  R  := System.Math.Min(ABounds.Width, ABounds.Height) / 2;
  NLen := R - ScaleValue(18);
  Angle := DegToRad(ValueToAngle(DisplayValue));
  NX := Cx + NLen * Cos(Angle);
  NY := Cy + NLen * Sin(Angle);

  Color := CurrentZoneColor;
  if Color = clNone then Color := Palette.GaugeNeedle;

  // Needle line.
  Pen := TGPPen.Create(ColorToARGB(Color), ScaleValue(3));
  Pen.SetStartCap(LineCapRound);
  Pen.SetEndCap(LineCapTriangle);
  try
    AGraphics.DrawLine(Pen, Cx, Cy, NX, NY);
  finally Pen.Free; end;

  // Hub.
  HubR := ScaleValue(8);
  Brush := TGPSolidBrush.Create(ColorToARGB(Palette.GaugeTick));
  try
    Hub.X := Cx - HubR;
    Hub.Y := Cy - HubR;
    Hub.Width := HubR * 2;
    Hub.Height := HubR * 2;
    AGraphics.FillEllipse(Brush, Hub);
  finally Brush.Free; end;
end;

procedure TOBDDialBase.DrawCaption(ACanvas: TCanvas;
  const ABounds: TRectF);
var
  S: string;
  Cx, Cy: Integer;
  TextW: Integer;
begin
  if Caption = '' then Exit;
  S := Caption;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := Self.Font;
  ACanvas.Font.Color := EffectiveForeground;
  TextW := ACanvas.TextWidth(S);
  Cx := Round(ABounds.X + ABounds.Width / 2 - TextW / 2);
  Cy := Round(ABounds.Y + ABounds.Height * 0.66);
  ACanvas.TextOut(Cx, Cy, S);
end;

{ ---- TOBDCircularGauge ---------------------------------------------------- }

constructor TOBDCircularGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
end;

function TOBDCircularGauge.StartAngleDeg: Single;
begin
  Result := DEFAULT_DIAL_START;
end;

function TOBDCircularGauge.SweepAngleDeg: Single;
begin
  Result := DEFAULT_DIAL_SWEEP;
end;

{ ---- TOBDTachometer ------------------------------------------------------- }

constructor TOBDTachometer.Create(AOwner: TComponent);
var Cfg: TOBDGaugeTickConfig;
begin
  inherited Create(AOwner);
  Min := 0;
  Max := 8000;
  &Unit := 'rpm';
  Caption := 'RPM';
  FRedlineColor := $003C3CFF;     //  red BGR
  FWarningColor := $0000A8FF;     //  amber BGR
  FWarningStart := 6400;          //  0.80 × 8000
  FRedline      := 7000;          //  0.88 × 8000
  // Override the inherited 0..100 tick config for an RPM scale.
  Cfg := DefaultTickConfig;
  Cfg.MajorInterval      := 1000;
  Cfg.MinorTicksPerMajor := 4;
  Cfg.LabelDecimals      := 0;
  SetTickConfig(Cfg);
  RebuildZones;
end;

procedure TOBDTachometer.SetRedline(AValue: Double);
begin
  if SameValue(FRedline, AValue) then Exit;
  FRedline := AValue; RebuildZones;
end;

procedure TOBDTachometer.SetRedlineColor(AValue: TColor);
begin
  if FRedlineColor = AValue then Exit;
  FRedlineColor := AValue; RebuildZones;
end;

procedure TOBDTachometer.SetWarningStart(AValue: Double);
begin
  if SameValue(FWarningStart, AValue) then Exit;
  FWarningStart := AValue; RebuildZones;
end;

procedure TOBDTachometer.SetWarningColor(AValue: TColor);
begin
  if FWarningColor = AValue then Exit;
  FWarningColor := AValue; RebuildZones;
end;

procedure TOBDTachometer.RebuildZones;
var Z: TOBDGaugeZones;
begin
  SetLength(Z, 2);
  Z[0] := MakeGaugeZone(FWarningStart, FRedline, FWarningColor);
  Z[1] := MakeGaugeZone(FRedline, Max, FRedlineColor);
  SetZones(Z);
end;

{ ---- TOBDArcGauge --------------------------------------------------------- }

constructor TOBDArcGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartAngle := DEFAULT_DIAL_START;
  FSweepAngle := DEFAULT_DIAL_SWEEP;
end;

function TOBDArcGauge.StartAngleDeg: Single;
begin
  Result := FStartAngle;
end;

function TOBDArcGauge.SweepAngleDeg: Single;
begin
  Result := FSweepAngle;
end;

procedure TOBDArcGauge.SetStartAngle(AValue: Single);
begin
  if SameValue(FStartAngle, AValue) then Exit;
  FStartAngle := AValue; Repaint;
end;

procedure TOBDArcGauge.SetSweepAngle(AValue: Single);
begin
  if SameValue(FSweepAngle, AValue) then Exit;
  FSweepAngle := AValue; Repaint;
end;

{ ---- TOBDComboGauge ------------------------------------------------------- }

constructor TOBDComboGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCenterFont := TFont.Create;
  FCenterFont.Name := 'Segoe UI';
  FCenterFont.Size := 18;
  FCenterFont.Style := [fsBold];
  FCenterFont.OnChange := FontChange;
end;

destructor TOBDComboGauge.Destroy;
begin
  FCenterFont.Free;
  inherited;
end;

procedure TOBDComboGauge.SetCenterFont(AValue: TFont);
begin
  FCenterFont.Assign(AValue);
end;

procedure TOBDComboGauge.FontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDComboGauge.PaintExtras(AGraphics: TGPGraphics;
  const ABounds: TRectF);
var
  S: string;
  C: TCanvas;
  W, H: Integer;
  Cx, Cy: Integer;
begin
  // Paint the digital readout via TCanvas (sharper text than
  // GDI+ at small sizes). Get the host canvas via the bitmap
  // already in use.
  // We work around the GDI+ Graphics by writing on the
  // underlying surface — the buffer canvas is exposed through
  // the inherited Paint pipeline.
  C := nil;  //  filled below — using parent canvas
  // PaintExtras runs INSIDE PaintControl which received the
  // bitmap canvas as ACanvas. We don't have a direct handle
  // here; instead, write via the TGPGraphics' DrawString
  // (GDI+) — slightly softer than TCanvas at small sizes but
  // works without canvas plumbing.
  var FontFamily := TGPFontFamily.Create(FCenterFont.Name);
  var GdiFont    := TGPFont.Create(FontFamily,
    FCenterFont.Size, FontStyleBold, UnitPoint);
  var Brush      := TGPSolidBrush.Create(
    ColorToARGB(EffectiveForeground));
  var Format     := TGPStringFormat.Create;
  try
    Format.SetAlignment(StringAlignmentCenter);
    Format.SetLineAlignment(StringAlignmentCenter);
    S := FormatValue(DisplayValue);
    var R: TGPRectF;
    R.X := ABounds.X;
    R.Y := ABounds.Y + ABounds.Height * 0.30;
    R.Width  := ABounds.Width;
    R.Height := ABounds.Height * 0.30;
    AGraphics.DrawString(S, Length(S), GdiFont, R, Format, Brush);
  finally
    Format.Free;
    Brush.Free;
    GdiFont.Free;
    FontFamily.Free;
  end;
end;

end.
