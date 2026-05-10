//------------------------------------------------------------------------------
//  OBD.UI.Telltales
//
//  Dashboard telltales — the small glyph lamps a real cluster
//  uses to surface emissions / safety / system status:
//
//    TOBDMILLamp        check-engine. Off / on / flashing.
//    TOBDDTCBadge       circular count badge with pulse on
//                       arrival. Drops next to a label.
//    TOBDReadinessLamp  per-monitor "supported / complete /
//                       incomplete" tri-state icon.
//    TOBDDashLamp       one component, many glyphs — Glyph enum
//                       picks the symbol (oil / battery / fuel
//                       / ABS / seatbelt / airbag / TPMS / ESP
//                       / ...). State is off / on / flashing.
//
//  All four inherit theme / HiDPI / VCL-Style behaviour from
//  TOBDGraphicControl. Every state mutation routes through
//  TBindings.Notify so a host that wired a LiveBinding sees
//  the refresh.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Telltales;

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
  Vcl.ExtCtrls,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>Tri-state lamp shared by MIL / DashLamp.</summary>
  TOBDTelltaleState = (tsOff, tsOn, tsFlashing);

  /// <summary>Readiness tri-state used by the readiness lamp.
  /// </summary>
  TOBDReadinessState = (
    /// <summary>Monitor not supported on this ECU.</summary>
    rsNotSupported,
    /// <summary>Supported but the drive-cycle hasn't completed
    /// the test.</summary>
    rsIncomplete,
    /// <summary>Supported and the test has completed.</summary>
    rsComplete
  );

  /// <summary>Symbol identifier for <see cref="TOBDDashLamp"/>.
  /// Maps to a built-in vector glyph. Extend by adding a new
  /// enum member and a paint case below.</summary>
  TOBDDashGlyph = (
    dgOil,        // engine oil-can
    dgBattery,    // battery / charging
    dgFuel,       // pump
    dgABS,        // ABS lozenge
    dgSeatbelt,   // seatbelt figure
    dgAirbag,     // airbag passenger
    dgTPMS,       // tyre pressure horseshoe
    dgESP,        // stability control
    dgEngineTemp, // coolant thermometer
    dgHandbrake   // handbrake bracket
  );

  /// <summary>Common lamp base — paints a rounded body in the
  /// state colour, owns the flash timer, exposes a virtual
  /// glyph-paint hook so subclasses draw their symbol on top.
  /// </summary>
  TOBDTelltaleBase = class(TOBDGraphicControl)
  strict private
    FState:         TOBDTelltaleState;
    FOnColor:       TColor;
    FOffColor:      TColor;
    FFlashPeriodMs: Cardinal;
    FFlashOn:       Boolean;
    FTimer:         TTimer;
    procedure SetState(AValue: TOBDTelltaleState);
    procedure SetOnColor(AValue: TColor);
    procedure SetOffColor(AValue: TColor);
    procedure SetFlashPeriodMs(AValue: Cardinal);
    procedure HandleTimer(Sender: TObject);
  protected
    /// <summary>Override to paint the symbol on top of the lamp
    /// body. <paramref name="ABounds"/> is the inner area
    /// (already inset for the bezel). <paramref name="AColor"/>
    /// is the glyph stroke / fill colour the subclass should
    /// use.</summary>
    procedure PaintGlyph(AGraphics: TGPGraphics;
      const ABounds: TGPRectF; AColor: TColor); virtual;
    /// <summary>Returns the colour the lamp body should paint
    /// in based on State + theme fallback.</summary>
    function  EffectiveLitColor: TColor; virtual;
    /// <summary>True when the lamp is currently showing the lit
    /// glyph (On, or Flashing+visible-tick).</summary>
    function  IsLit: Boolean;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure NotifyBindings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Off / On / Flashing. Default <c>tsOff</c>.</summary>
    property State: TOBDTelltaleState read FState write SetState
      default tsOff;
    /// <summary>Body colour when lit. Default uses theme.
    /// </summary>
    property OnColor:  TColor read FOnColor  write SetOnColor
      default clDefault;
    /// <summary>Body colour when dark. Default uses theme.
    /// </summary>
    property OffColor: TColor read FOffColor write SetOffColor
      default clDefault;
    /// <summary>Full flash cycle (on+off). Default 600 ms.
    /// </summary>
    property FlashPeriodMs: Cardinal
      read FFlashPeriodMs write SetFlashPeriodMs default 600;
  end;

  /// <summary>Malfunction Indicator Lamp. Off / On / Flashing.
  /// Host code typically maps the lamp's <c>State</c> from the
  /// MIL status read via <c>TOBDVehicleHealth</c>:
  /// <code>
  ///   MIL.State := IfThen(Status.MILOn, tsOn, tsOff);
  /// </code>
  /// Flashing isn't reported by Mode 01 PID 01 itself; hosts
  /// that want the catalyst-damage flash convention can set
  /// <c>tsFlashing</c> when they detect misfire DTCs alongside
  /// <c>MILOn</c>.</summary>
  TOBDMILLamp = class(TOBDTelltaleBase)
  protected
    procedure PaintGlyph(AGraphics: TGPGraphics;
      const ABounds: TGPRectF; AColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>Circular badge with a count. Pulses for a few
  /// frames when <c>Count</c> changes (drawing attention to a
  /// new DTC arriving). Set <c>Count</c> from a host's
  /// OnDTCs handler.</summary>
  TOBDDTCBadge = class(TOBDGraphicControl)
  strict private
    FCount:           Integer;
    FPulseOnChange:   Boolean;
    FBadgeColor:      TColor;
    FFont:            TFont;
    FPulseTimer:      TTimer;
    FPulseFrame:      Integer;
    procedure SetCount(AValue: Integer);
    procedure SetPulseOnChange(AValue: Boolean);
    procedure SetBadgeColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure HandlePulseTick(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Number shown inside the badge. Negative values
    /// hide the badge entirely (lets a host represent "unknown".)
    /// </summary>
    property Count: Integer read FCount write SetCount default 0;
    /// <summary>Pulse animation on each Count change. Default
    /// True.</summary>
    property PulseOnChange: Boolean
      read FPulseOnChange write SetPulseOnChange default True;
    /// <summary>Badge fill colour. Default theme danger
    /// (typically red).</summary>
    property BadgeColor: TColor
      read FBadgeColor write SetBadgeColor default clDefault;
    /// <summary>Font for the count number.</summary>
    property NumberFont: TFont read FFont write SetFont;
  end;

  /// <summary>Per-monitor readiness lamp. Three-state:
  /// not-supported / incomplete / complete. Use one per readiness
  /// monitor row alongside its label.</summary>
  TOBDReadinessLamp = class(TOBDGraphicControl)
  strict private
    FState:    TOBDReadinessState;
    procedure SetState(AValue: TOBDReadinessState);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Not-supported / Incomplete / Complete. Default
    /// <c>rsNotSupported</c>.</summary>
    property State: TOBDReadinessState read FState write SetState
      default rsNotSupported;
  end;

  /// <summary>Single dashboard glyph driven by a <c>Glyph</c>
  /// enum. Reuses the telltale lit / dark / flash body and adds
  /// a stylised mono-line vector for the picked symbol.</summary>
  TOBDDashLamp = class(TOBDTelltaleBase)
  strict private
    FGlyph: TOBDDashGlyph;
    procedure SetGlyph(AValue: TOBDDashGlyph);
  protected
    procedure PaintGlyph(AGraphics: TGPGraphics;
      const ABounds: TGPRectF; AColor: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Symbol shown inside the lamp body.</summary>
    property Glyph: TOBDDashGlyph read FGlyph write SetGlyph
      default dgOil;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

function RectF(AX, AY, AW, AH: Single): TGPRectF; inline;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Width  := AW;
  Result.Height := AH;
end;

// Inline tick-mark helper — kept above the readiness-lamp code
// so it's resolved by the time PaintControl needs it.
procedure DrawCheckMark(AGraphics: TGPGraphics; APen: TGPPen;
  const ABounds: TGPRectF);
begin
  AGraphics.DrawLine(APen,
    ABounds.X + ABounds.Width * 0.25,
    ABounds.Y + ABounds.Height * 0.55,
    ABounds.X + ABounds.Width * 0.45,
    ABounds.Y + ABounds.Height * 0.75);
  AGraphics.DrawLine(APen,
    ABounds.X + ABounds.Width * 0.45,
    ABounds.Y + ABounds.Height * 0.75,
    ABounds.X + ABounds.Width * 0.75,
    ABounds.Y + ABounds.Height * 0.30);
end;

{ ---- TOBDTelltaleBase ----------------------------------------------------- }

constructor TOBDTelltaleBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 32;
  Height := 32;
  FState         := tsOff;
  FOnColor       := clDefault;
  FOffColor      := clDefault;
  FFlashPeriodMs := 600;
  FFlashOn       := True;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
end;

destructor TOBDTelltaleBase.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TOBDTelltaleBase.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDTelltaleBase.SetState(AValue: TOBDTelltaleState);
begin
  if FState = AValue then Exit;
  FState := AValue;
  FTimer.Enabled := (FState = tsFlashing) and
                    not (csDesigning in ComponentState);
  if FTimer.Enabled then
    FTimer.Interval := System.Math.Max(50, FFlashPeriodMs div 2);
  FFlashOn := True;
  NotifyBindings;
  Repaint;
end;

procedure TOBDTelltaleBase.SetOnColor(AValue: TColor);
begin
  if FOnColor = AValue then Exit;
  FOnColor := AValue; Repaint;
end;

procedure TOBDTelltaleBase.SetOffColor(AValue: TColor);
begin
  if FOffColor = AValue then Exit;
  FOffColor := AValue; Repaint;
end;

procedure TOBDTelltaleBase.SetFlashPeriodMs(AValue: Cardinal);
begin
  if AValue < 100 then AValue := 100;
  if FFlashPeriodMs = AValue then Exit;
  FFlashPeriodMs := AValue;
  if FTimer.Enabled then
    FTimer.Interval := FFlashPeriodMs div 2;
end;

procedure TOBDTelltaleBase.HandleTimer(Sender: TObject);
begin
  FFlashOn := not FFlashOn;
  Repaint;
end;

function TOBDTelltaleBase.IsLit: Boolean;
begin
  Result := (FState = tsOn) or
            ((FState = tsFlashing) and FFlashOn);
end;

function TOBDTelltaleBase.EffectiveLitColor: TColor;
begin
  if FOnColor <> clDefault then Result := FOnColor
  else                         Result := Palette.Warning;
end;

procedure TOBDTelltaleBase.PaintGlyph(AGraphics: TGPGraphics;
  const ABounds: TGPRectF; AColor: TColor);
begin
  // Default = no glyph (subclass paints one).
end;

procedure TOBDTelltaleBase.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen:   TGPPen;
  Brush: TGPSolidBrush;
  Body:  TGPRectF;
  Inner: TGPRectF;
  BodyCol, BorderCol, GlyphCol, OffCol: TColor;
  Pad: Single;
begin
  if FOffColor <> clDefault then OffCol := FOffColor
  else                           OffCol := Palette.NeutralLight;
  BorderCol := EffectiveBorder;
  if IsLit then
  begin
    BodyCol  := EffectiveLitColor;
    // The lit glyph is shown against the body — pick a glyph
    // colour that contrasts. Black on amber / red is the
    // industry-standard reading.
    GlyphCol := clBlack;
  end
  else
  begin
    BodyCol  := OffCol;
    GlyphCol := EffectiveForeground;
  end;

  Pad := ScaleValue(2);
  Body.X := Pad;
  Body.Y := Pad;
  Body.Width  := Width  - 2 * Pad;
  Body.Height := Height - 2 * Pad;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Rounded body fill — approximate with an ellipse when the
    // bounds are roughly square; otherwise use a rounded rect.
    Brush := TGPSolidBrush.Create(ColorToARGB(BodyCol));
    try
      Graphics.FillEllipse(Brush, Body);
    finally
      Brush.Free;
    end;

    // Bezel.
    Pen := TGPPen.Create(ColorToARGB(BorderCol), ScaleValue(1));
    try
      Graphics.DrawEllipse(Pen, Body);
    finally
      Pen.Free;
    end;

    // Glyph paint area — inset slightly so the symbol doesn't
    // clip the bezel.
    Inner.X := Body.X + Body.Width  * 0.18;
    Inner.Y := Body.Y + Body.Height * 0.18;
    Inner.Width  := Body.Width  * 0.64;
    Inner.Height := Body.Height * 0.64;
    PaintGlyph(Graphics, Inner, GlyphCol);
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDMILLamp ---------------------------------------------------------- }

constructor TOBDMILLamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 40;
  Height := 32;
end;

// MIL is amber by convention; the base's EffectiveLitColor
// already returns Palette.Warning when no explicit OnColor was
// set, which is exactly the ISO 2575 amber the dashboard uses.
// No override needed.

procedure TOBDMILLamp.PaintGlyph(AGraphics: TGPGraphics;
  const ABounds: TGPRectF; AColor: TColor);
var
  Pen: TGPPen;
  Cx, Cy, W, H: Single;
  Path: TGPGraphicsPath;
begin
  // Engine outline + ignition mark — the ISO 2575 check-engine
  // glyph in a single stroke.
  Cx := ABounds.X + ABounds.Width  / 2;
  Cy := ABounds.Y + ABounds.Height / 2;
  W  := ABounds.Width  * 0.85;
  H  := ABounds.Height * 0.55;
  Pen := TGPPen.Create(ColorToARGB(AColor), ScaleValue(2));
  Pen.SetStartCap(LineCapRound);
  Pen.SetEndCap(LineCapRound);
  Path := TGPGraphicsPath.Create;
  try
    // Rounded engine block.
    Path.AddRectangle(RectF(Cx - W / 2, Cy - H / 2, W, H));
    // Crank-bump on top.
    Path.AddRectangle(
      RectF(Cx - W * 0.15, Cy - H / 2 - H * 0.25, W * 0.30, H * 0.25));
    // Output shaft.
    Path.AddRectangle(
      RectF(Cx + W / 2, Cy - H * 0.18, W * 0.12, H * 0.36));
    AGraphics.DrawPath(Pen, Path);

    // Lightning bolt — stylised, three line segments inside.
    AGraphics.DrawLine(Pen,
      Cx - W * 0.05, Cy - H * 0.10,
      Cx - W * 0.20, Cy + H * 0.05);
    AGraphics.DrawLine(Pen,
      Cx - W * 0.20, Cy + H * 0.05,
      Cx + W * 0.00, Cy + H * 0.05);
    AGraphics.DrawLine(Pen,
      Cx + W * 0.00, Cy + H * 0.05,
      Cx - W * 0.10, Cy + H * 0.20);
  finally
    Path.Free;
    Pen.Free;
  end;
end;

{ ---- TOBDDTCBadge --------------------------------------------------------- }

constructor TOBDDTCBadge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 32;
  Height := 32;
  FCount := 0;
  FPulseOnChange := True;
  FBadgeColor := clDefault;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;
  FFont.Style := [fsBold];
  FFont.Color := clWhite;
  FFont.OnChange := HandleFontChange;
  FPulseTimer := TTimer.Create(Self);
  FPulseTimer.Enabled := False;
  FPulseTimer.Interval := 40;
  FPulseTimer.OnTimer := HandlePulseTick;
  FPulseFrame := 0;
end;

destructor TOBDDTCBadge.Destroy;
begin
  FPulseTimer.Free;
  FFont.Free;
  inherited;
end;

procedure TOBDDTCBadge.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDTCBadge.SetCount(AValue: Integer);
begin
  if FCount = AValue then Exit;
  FCount := AValue;
  NotifyBindings;
  // Trigger pulse animation if requested and not at design-time.
  if FPulseOnChange and not (csDesigning in ComponentState) then
  begin
    FPulseFrame := 0;
    FPulseTimer.Enabled := True;
  end;
  Repaint;
end;

procedure TOBDDTCBadge.SetPulseOnChange(AValue: Boolean);
begin
  if FPulseOnChange = AValue then Exit;
  FPulseOnChange := AValue;
  if not FPulseOnChange then
  begin
    FPulseTimer.Enabled := False;
    FPulseFrame := 0;
    Repaint;
  end;
end;

procedure TOBDDTCBadge.SetBadgeColor(AValue: TColor);
begin
  if FBadgeColor = AValue then Exit;
  FBadgeColor := AValue; Repaint;
end;

procedure TOBDDTCBadge.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDDTCBadge.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDDTCBadge.HandlePulseTick(Sender: TObject);
const
  PULSE_FRAMES = 10;
begin
  Inc(FPulseFrame);
  if FPulseFrame >= PULSE_FRAMES then
  begin
    FPulseFrame := 0;
    FPulseTimer.Enabled := False;
  end;
  Repaint;
end;

procedure TOBDDTCBadge.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Body, Halo: TGPRectF;
  Col: TColor;
  S: string;
  Pad: Single;
  Scale: Single;
  W: Integer;
begin
  if FCount < 0 then Exit;                 // hidden state
  if FBadgeColor <> clDefault then Col := FBadgeColor
  else                              Col := Palette.Danger;

  Pad := ScaleValue(2);
  Body.X := Pad;
  Body.Y := Pad;
  Body.Width  := Width  - 2 * Pad;
  Body.Height := Height - 2 * Pad;

  // Pulse halo: scale a translucent disc up + down over the
  // configured frame count.
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);

    if FPulseTimer.Enabled then
    begin
      Scale := 1.0 + 0.5 *
        (1.0 - Abs(FPulseFrame / 10.0 - 0.5) * 2.0);
      Brush := TGPSolidBrush.Create(ColorToARGB(Col, 64));
      try
        Halo.X := Body.X - Body.Width  * (Scale - 1.0) / 2;
        Halo.Y := Body.Y - Body.Height * (Scale - 1.0) / 2;
        Halo.Width  := Body.Width  * Scale;
        Halo.Height := Body.Height * Scale;
        Graphics.FillEllipse(Brush, Halo);
      finally
        Brush.Free;
      end;
    end;

    // Body fill.
    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      Graphics.FillEllipse(Brush, Body);
    finally
      Brush.Free;
    end;
  finally
    Graphics.Free;
  end;

  // Count via TCanvas for sharper small-pixel text.
  S := IntToStr(FCount);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  W := ACanvas.TextWidth(S);
  ACanvas.TextOut(
    (Width  - W) div 2,
    (Height - ACanvas.TextHeight('0')) div 2,
    S);
end;

{ ---- TOBDReadinessLamp ---------------------------------------------------- }

constructor TOBDReadinessLamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 24;
  Height := 24;
  FState := rsNotSupported;
end;

procedure TOBDReadinessLamp.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDReadinessLamp.SetState(AValue: TOBDReadinessState);
begin
  if FState = AValue then Exit;
  FState := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDReadinessLamp.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Brush: TGPSolidBrush;
  Body: TGPRectF;
  Col: TColor;
  Pad: Single;
begin
  Pad := ScaleValue(2);
  Body.X := Pad;
  Body.Y := Pad;
  Body.Width  := Width  - 2 * Pad;
  Body.Height := Height - 2 * Pad;

  case FState of
    rsNotSupported: Col := Palette.NeutralLight;
    rsIncomplete:   Col := Palette.Warning;
    rsComplete:     Col := Palette.Success;
  else
    Col := Palette.NeutralLight;
  end;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      Graphics.FillEllipse(Brush, Body);
    finally
      Brush.Free;
    end;

    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawEllipse(Pen, Body);
    finally
      Pen.Free;
    end;

    // Inner mark: a tick for Complete, a dash for Incomplete,
    // an X for NotSupported.
    Pen := TGPPen.Create(ColorToARGB(clBlack), ScaleValue(2));
    Pen.SetStartCap(LineCapRound);
    Pen.SetEndCap(LineCapRound);
    try
      case FState of
        rsComplete:
          begin
            DrawCheckMark(Graphics, Pen, Body);
          end;
        rsIncomplete:
          begin
            Graphics.DrawLine(Pen,
              Body.X + Body.Width  * 0.30, Body.Y + Body.Height * 0.50,
              Body.X + Body.Width  * 0.70, Body.Y + Body.Height * 0.50);
          end;
        rsNotSupported:
          begin
            Graphics.DrawLine(Pen,
              Body.X + Body.Width  * 0.30, Body.Y + Body.Height * 0.30,
              Body.X + Body.Width  * 0.70, Body.Y + Body.Height * 0.70);
            Graphics.DrawLine(Pen,
              Body.X + Body.Width  * 0.70, Body.Y + Body.Height * 0.30,
              Body.X + Body.Width  * 0.30, Body.Y + Body.Height * 0.70);
          end;
      end;
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDDashLamp --------------------------------------------------------- }

constructor TOBDDashLamp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 36;
  Height := 32;
  FGlyph := dgOil;
end;

procedure TOBDDashLamp.SetGlyph(AValue: TOBDDashGlyph);
begin
  if FGlyph = AValue then Exit;
  FGlyph := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDDashLamp.PaintGlyph(AGraphics: TGPGraphics;
  const ABounds: TGPRectF; AColor: TColor);
var
  Pen: TGPPen;
  Brush: TGPSolidBrush;
  Cx, Cy, W, H: Single;
begin
  Cx := ABounds.X + ABounds.Width  / 2;
  Cy := ABounds.Y + ABounds.Height / 2;
  W  := ABounds.Width;
  H  := ABounds.Height;

  Pen := TGPPen.Create(ColorToARGB(AColor), ScaleValue(2));
  Pen.SetStartCap(LineCapRound);
  Pen.SetEndCap(LineCapRound);
  Brush := TGPSolidBrush.Create(ColorToARGB(AColor));
  try
    case FGlyph of
      dgOil:
        begin
          // Oil-can spout + drip.
          AGraphics.DrawLine(Pen, Cx - W * 0.35, Cy,
                                   Cx + W * 0.05, Cy);
          AGraphics.DrawLine(Pen, Cx + W * 0.05, Cy,
                                   Cx + W * 0.35, Cy - H * 0.20);
          AGraphics.DrawArc(Pen,
            RectF(Cx - W * 0.10, Cy - H * 0.10, W * 0.20, H * 0.40),
            0, 180);
          // Drip.
          AGraphics.FillEllipse(Brush,
            Cx + W * 0.32, Cy + H * 0.10, W * 0.10, H * 0.15);
        end;
      dgBattery:
        begin
          AGraphics.DrawRectangle(Pen,
            RectF(Cx - W * 0.40, Cy - H * 0.25, W * 0.80, H * 0.50));
          AGraphics.DrawLine(Pen,
            Cx - W * 0.20, Cy, Cx - W * 0.05, Cy);
          AGraphics.DrawLine(Pen, Cx + W * 0.10, Cy - H * 0.08,
                                   Cx + W * 0.10, Cy + H * 0.08);
          AGraphics.DrawLine(Pen, Cx + W * 0.02, Cy, Cx + W * 0.18, Cy);
        end;
      dgFuel:
        begin
          AGraphics.DrawRectangle(Pen,
            RectF(Cx - W * 0.30, Cy - H * 0.40, W * 0.50, H * 0.80));
          AGraphics.DrawLine(Pen, Cx + W * 0.20, Cy - H * 0.15,
                                   Cx + W * 0.35, Cy - H * 0.05);
          AGraphics.DrawLine(Pen, Cx + W * 0.35, Cy - H * 0.05,
                                   Cx + W * 0.35, Cy + H * 0.10);
        end;
      dgABS:
        begin
          AGraphics.DrawEllipse(Pen,
            RectF(Cx - W * 0.40, Cy - H * 0.40, W * 0.80, H * 0.80));
          // Letters approximated as three short bars.
          AGraphics.DrawLine(Pen, Cx - W * 0.20, Cy - H * 0.10,
                                   Cx - W * 0.20, Cy + H * 0.10);
          AGraphics.DrawLine(Pen, Cx + W * 0.00, Cy - H * 0.10,
                                   Cx + W * 0.00, Cy + H * 0.10);
          AGraphics.DrawLine(Pen, Cx + W * 0.20, Cy - H * 0.10,
                                   Cx + W * 0.20, Cy + H * 0.10);
        end;
      dgSeatbelt:
        begin
          // Belt-and-figure simplified to a chevron.
          AGraphics.DrawLine(Pen, Cx - W * 0.30, Cy - H * 0.30,
                                   Cx + W * 0.05, Cy + H * 0.30);
          AGraphics.DrawLine(Pen, Cx + W * 0.30, Cy - H * 0.30,
                                   Cx + W * 0.05, Cy + H * 0.30);
        end;
      dgAirbag:
        begin
          AGraphics.FillEllipse(Brush,
            Cx, Cy - H * 0.05, W * 0.25, H * 0.25);
          AGraphics.DrawArc(Pen,
            RectF(Cx - W * 0.30, Cy - H * 0.10, W * 0.20, H * 0.40),
            -45, 90);
        end;
      dgTPMS:
        begin
          // Horseshoe + exclamation.
          AGraphics.DrawArc(Pen,
            RectF(Cx - W * 0.40, Cy - H * 0.30, W * 0.80, H * 0.60),
            45, 270);
          AGraphics.DrawLine(Pen, Cx, Cy - H * 0.05,
                                   Cx, Cy + H * 0.15);
          AGraphics.FillEllipse(Brush,
            Cx - W * 0.04, Cy + H * 0.20, W * 0.08, W * 0.08);
        end;
      dgESP:
        begin
          AGraphics.DrawEllipse(Pen,
            RectF(Cx - W * 0.20, Cy - H * 0.20, W * 0.40, H * 0.40));
          AGraphics.DrawArc(Pen,
            RectF(Cx - W * 0.40, Cy - H * 0.35, W * 0.30, H * 0.70),
            -30, 60);
          AGraphics.DrawArc(Pen,
            RectF(Cx + W * 0.10, Cy - H * 0.35, W * 0.30, H * 0.70),
            150, 60);
        end;
      dgEngineTemp:
        begin
          // Thermometer.
          AGraphics.DrawLine(Pen, Cx, Cy - H * 0.35,
                                   Cx, Cy + H * 0.15);
          AGraphics.FillEllipse(Brush,
            Cx - W * 0.12, Cy + H * 0.10, W * 0.24, W * 0.24);
          // Wavy water at the base.
          AGraphics.DrawLine(Pen, Cx - W * 0.40, Cy + H * 0.35,
                                   Cx + W * 0.40, Cy + H * 0.35);
        end;
      dgHandbrake:
        begin
          AGraphics.DrawEllipse(Pen,
            RectF(Cx - W * 0.40, Cy - H * 0.40, W * 0.80, H * 0.80));
          AGraphics.DrawLine(Pen, Cx, Cy - H * 0.10,
                                   Cx, Cy + H * 0.20);
          AGraphics.DrawLine(Pen, Cx - W * 0.20, Cy + H * 0.05,
                                   Cx + W * 0.20, Cy + H * 0.05);
        end;
    end;
  finally
    Brush.Free;
    Pen.Free;
  end;
end;

end.
