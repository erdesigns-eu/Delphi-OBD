//------------------------------------------------------------------------------
//  OBD.UI.Gauges.Specialised
//
//  Domain-specific gauges layered on top of the dial / bar
//  primitives. Each ships with sensible defaults so a host
//  drops one on a form, sets one or two properties, and gets
//  a production-looking widget out of the box:
//
//    TOBDBoostGauge        centre-zero dial. Vacuum (blue) on
//                          the left, boost (red) on the right.
//                          Default range -1.0..+2.0 bar.
//    TOBDAFRGauge          AFR dial with stoich centre line and
//                          lean / rich amber zones. Default
//                          range 10..18, stoich 14.7.
//    TOBDStateOfChargeBar  horizontal SOC bar with a charging
//                          arrow + time-to-full readout. Bound
//                          to <see cref="TOBDEVBattery"/> at
//                          host-glue level.
//    TOBDRegenIndicator    vertical centre-zero power flow bar.
//                          Positive = regen (up, green),
//                          negative = power draw (down, blue).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.Specialised;

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
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Base,
  OBD.UI.Gauges.Dial;

type
  /// <summary>Centre-zero boost / vacuum gauge. Range is
  /// <c>Min</c>..<c>Max</c> on the inherited gauge (override
  /// for kPa-based dashboards, etc.). The default geometry
  /// covers a typical turbo passenger car: -1.0 bar vacuum to
  /// +2.0 bar boost with a 1.5 bar warning band.</summary>
  TOBDBoostGauge = class(TOBDCircularGauge)
  strict private
    FVacuumColor:  TColor;
    FBoostColor:   TColor;
    FWarningStart: Double;
    procedure SetVacuumColor(AValue: TColor);
    procedure SetBoostColor(AValue: TColor);
    procedure SetWarningStart(AValue: Double);
    procedure RebuildZones;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Vacuum-side zone colour (Min..0).
    /// Default blue.</summary>
    property VacuumColor: TColor
      read FVacuumColor write SetVacuumColor default $00C08040;
    /// <summary>Boost-side zone colour (0..WarningStart).
    /// Default amber.</summary>
    property BoostColor: TColor
      read FBoostColor write SetBoostColor default $0000A8FF;
    /// <summary>Boost value at which the warning band starts;
    /// the band runs from <c>WarningStart</c> to
    /// <c>Max</c> in <see cref="BoostColor"/>'s red sibling.
    /// Default 1.5 bar.</summary>
    property WarningStart: Double
      read FWarningStart write SetWarningStart;
  end;

  /// <summary>Air-fuel-ratio dial. Centred on
  /// <see cref="Stoich"/> (14.7 by default for gasoline) with
  /// lean and rich warning bands at the dial extremes.</summary>
  TOBDAFRGauge = class(TOBDCircularGauge)
  strict private
    FStoich:    Double;
    FLeanStart: Double;
    FRichStart: Double;
    FLeanColor: TColor;
    FRichColor: TColor;
    procedure SetStoich(AValue: Double);
    procedure SetLeanStart(AValue: Double);
    procedure SetRichStart(AValue: Double);
    procedure SetLeanColor(AValue: TColor);
    procedure SetRichColor(AValue: TColor);
    procedure RebuildZones;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Stoichiometric AFR for the fuel. Default 14.7
    /// (E10 gasoline). Hosts running diesel set ~14.5,
    /// E85 ~9.7.</summary>
    property Stoich: Double read FStoich write SetStoich;
    /// <summary>AFR at which the lean band begins. Above this
    /// value the gauge paints the lean warning colour.
    /// Default 16.0.</summary>
    property LeanStart: Double read FLeanStart write SetLeanStart;
    /// <summary>AFR at which the rich band begins. Below this
    /// value the gauge paints the rich warning colour.
    /// Default 13.0.</summary>
    property RichStart: Double read FRichStart write SetRichStart;
    /// <summary>Lean-band colour. Default amber.</summary>
    property LeanColor: TColor read FLeanColor write SetLeanColor
      default $0000A8FF;
    /// <summary>Rich-band colour. Default red.</summary>
    property RichColor: TColor read FRichColor write SetRichColor
      default $003C3CFF;
  end;

  /// <summary>Big horizontal state-of-charge bar with optional
  /// charging arrow and time-to-full readout. Use one as the
  /// top-of-dashboard SOC indicator on an EV cluster.</summary>
  TOBDStateOfChargeBar = class(TOBDCustomControl)
  strict private
    FSOC:           Double;
    FCharging:      Boolean;
    FTimeToFullMs:  Int64;
    FShowReadout:   Boolean;
    FLowThreshold:  Double;
    FCaptionFont:   TFont;
    FValueFont:     TFont;
    procedure SetSOC(AValue: Double);
    procedure SetCharging(AValue: Boolean);
    procedure SetTimeToFullMs(AValue: Int64);
    procedure SetShowReadout(AValue: Boolean);
    procedure SetLowThreshold(AValue: Double);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  FormatTimeToFull: string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>State of charge in percent (0..100). Clamped.
    /// </summary>
    property SOC: Double read FSOC write SetSOC;
    /// <summary>True while the pack is charging. Drives the
    /// charging arrow + time-to-full visibility.</summary>
    property Charging: Boolean
      read FCharging write SetCharging default False;
    /// <summary>Estimated milliseconds until 100 %. 0 hides the
    /// readout.</summary>
    property TimeToFullMs: Int64
      read FTimeToFullMs write SetTimeToFullMs;
    /// <summary>Show the percent / time-to-full readout next
    /// to the bar. Default True.</summary>
    property ShowReadout: Boolean
      read FShowReadout write SetShowReadout default True;
    /// <summary>SOC at which the bar turns the warning colour.
    /// Default 20 %.</summary>
    property LowThreshold: Double
      read FLowThreshold write SetLowThreshold;
    /// <summary>Big SOC-percent font.</summary>
    property ValueFont: TFont read FValueFont write SetValueFont;
    /// <summary>Caption + time-to-full font.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
  end;

  /// <summary>Vertical centre-zero power-flow bar. Positive
  /// <see cref="Power"/> fills upward in the regen colour
  /// (green by default); negative <see cref="Power"/> fills
  /// downward in the discharge colour (blue).</summary>
  TOBDRegenIndicator = class(TOBDCustomControl)
  strict private
    FPower:       Double;
    FMaxPower:    Double;
    FMaxRegen:    Double;
    FRegenColor:  TColor;
    FDrawColor:   TColor;
    procedure SetPower(AValue: Double);
    procedure SetMaxPower(AValue: Double);
    procedure SetMaxRegen(AValue: Double);
    procedure SetRegenColor(AValue: TColor);
    procedure SetDrawColor(AValue: TColor);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Instantaneous power. Positive = regen,
    /// negative = motor draw. Unit conventionally kW.</summary>
    property Power: Double read FPower write SetPower;
    /// <summary>Magnitude of the maximum discharge the bar can
    /// show. Default 200.</summary>
    property MaxPower: Double read FMaxPower write SetMaxPower;
    /// <summary>Magnitude of the maximum regen the bar can
    /// show. Default 80.</summary>
    property MaxRegen: Double read FMaxRegen write SetMaxRegen;
    /// <summary>Regen colour. Default green.</summary>
    property RegenColor: TColor
      read FRegenColor write SetRegenColor default clDefault;
    /// <summary>Discharge colour. Default theme accent.</summary>
    property DischargeColor: TColor
      read FDrawColor write SetDrawColor default clDefault;
  end;

implementation

{ ---- TOBDBoostGauge ------------------------------------------------------ }

constructor TOBDBoostGauge.Create(AOwner: TComponent);
var Cfg: TOBDGaugeTickConfig;
begin
  inherited Create(AOwner);
  Min := -1.0;
  Max :=  2.0;
  &Unit := 'bar';
  Caption := 'Boost';
  Decimals := 1;
  FVacuumColor  := $00C08040;       // BGR-blue
  FBoostColor   := $0000A8FF;       // BGR-amber
  FWarningStart := 1.5;
  Cfg := DefaultTickConfig;
  Cfg.MajorInterval      := 0.5;
  Cfg.MinorTicksPerMajor := 4;
  Cfg.LabelDecimals      := 1;
  SetTickConfig(Cfg);
  RebuildZones;
end;

procedure TOBDBoostGauge.SetVacuumColor(AValue: TColor);
begin
  if FVacuumColor = AValue then Exit;
  FVacuumColor := AValue; RebuildZones;
end;

procedure TOBDBoostGauge.SetBoostColor(AValue: TColor);
begin
  if FBoostColor = AValue then Exit;
  FBoostColor := AValue; RebuildZones;
end;

procedure TOBDBoostGauge.SetWarningStart(AValue: Double);
begin
  if SameValue(FWarningStart, AValue) then Exit;
  FWarningStart := AValue; RebuildZones;
end;

procedure TOBDBoostGauge.RebuildZones;
var Z: TOBDGaugeZones;
begin
  // Three coloured bands: vacuum, boost OK, boost warning. The
  // red warning band uses the danger palette colour so a host
  // theme switch keeps the warning recognisable.
  SetLength(Z, 3);
  Z[0] := MakeGaugeZone(Min,            0,             FVacuumColor);
  Z[1] := MakeGaugeZone(0,              FWarningStart, FBoostColor);
  Z[2] := MakeGaugeZone(FWarningStart,  Max,           Palette.Danger);
  SetZones(Z);
end;

{ ---- TOBDAFRGauge -------------------------------------------------------- }

constructor TOBDAFRGauge.Create(AOwner: TComponent);
var Cfg: TOBDGaugeTickConfig;
begin
  inherited Create(AOwner);
  Min := 10.0;
  Max := 18.0;
  &Unit := 'AFR';
  Caption := 'AFR';
  Decimals := 1;
  FStoich    := 14.7;
  FLeanStart := 16.0;
  FRichStart := 13.0;
  FLeanColor := $0000A8FF;
  FRichColor := $003C3CFF;
  Cfg := DefaultTickConfig;
  Cfg.MajorInterval      := 1.0;
  Cfg.MinorTicksPerMajor := 4;
  Cfg.LabelDecimals      := 0;
  SetTickConfig(Cfg);
  RebuildZones;
end;

procedure TOBDAFRGauge.SetStoich(AValue: Double);
begin
  if SameValue(FStoich, AValue) then Exit;
  FStoich := AValue; RebuildZones;
end;

procedure TOBDAFRGauge.SetLeanStart(AValue: Double);
begin
  if SameValue(FLeanStart, AValue) then Exit;
  FLeanStart := AValue; RebuildZones;
end;

procedure TOBDAFRGauge.SetRichStart(AValue: Double);
begin
  if SameValue(FRichStart, AValue) then Exit;
  FRichStart := AValue; RebuildZones;
end;

procedure TOBDAFRGauge.SetLeanColor(AValue: TColor);
begin
  if FLeanColor = AValue then Exit;
  FLeanColor := AValue; RebuildZones;
end;

procedure TOBDAFRGauge.SetRichColor(AValue: TColor);
begin
  if FRichColor = AValue then Exit;
  FRichColor := AValue; RebuildZones;
end;

procedure TOBDAFRGauge.RebuildZones;
var Z: TOBDGaugeZones;
begin
  SetLength(Z, 2);
  // Below RichStart = rich, above LeanStart = lean. The middle
  // band is intentionally left unzoned (gauge face colour) so
  // the stoich centre reads as the "OK" target.
  Z[0] := MakeGaugeZone(Min,        FRichStart, FRichColor);
  Z[1] := MakeGaugeZone(FLeanStart, Max,        FLeanColor);
  SetZones(Z);
end;

{ ---- TOBDStateOfChargeBar ------------------------------------------------ }

constructor TOBDStateOfChargeBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 56;
  FSOC          := 0;
  FCharging     := False;
  FTimeToFullMs := 0;
  FShowReadout  := True;
  FLowThreshold := 20.0;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 16;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
end;

destructor TOBDStateOfChargeBar.Destroy;
begin
  FValueFont.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDStateOfChargeBar.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDStateOfChargeBar.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDStateOfChargeBar.SetSOC(AValue: Double);
begin
  if AValue < 0   then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FSOC, AValue) then Exit;
  FSOC := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDStateOfChargeBar.SetCharging(AValue: Boolean);
begin
  if FCharging = AValue then Exit;
  FCharging := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDStateOfChargeBar.SetTimeToFullMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FTimeToFullMs = AValue then Exit;
  FTimeToFullMs := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDStateOfChargeBar.SetShowReadout(AValue: Boolean);
begin
  if FShowReadout = AValue then Exit;
  FShowReadout := AValue; Repaint;
end;

procedure TOBDStateOfChargeBar.SetLowThreshold(AValue: Double);
begin
  if AValue < 0   then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FLowThreshold, AValue) then Exit;
  FLowThreshold := AValue; Repaint;
end;

procedure TOBDStateOfChargeBar.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDStateOfChargeBar.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

function TOBDStateOfChargeBar.FormatTimeToFull: string;
var
  Mins, Hours: Int64;
begin
  if FTimeToFullMs <= 0 then Exit('');
  Mins := FTimeToFullMs div 60000;
  if Mins < 60 then
    Result := Format('%d min to full', [Mins])
  else
  begin
    Hours := Mins div 60;
    Mins  := Mins mod 60;
    Result := Format('%dh %dmin to full', [Hours, Mins]);
  end;
end;

procedure TOBDStateOfChargeBar.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Track, Fill: TGPRectF;
  Pad, BarH, ReadoutW: Integer;
  BarCol: TColor;
  PercentText, EtaText: string;
  TextY: Integer;
  ArrowX1, ArrowY1, ArrowX2, ArrowY2, ArrowY3: Single;
begin
  Pad := ScaleValue(8);
  if FShowReadout then
    ReadoutW := ScaleValue(120)
  else
    ReadoutW := 0;

  BarH := System.Math.Min(Height - 2 * Pad, ScaleValue(24));
  Track.X := Pad;
  Track.Y := (Height - BarH) / 2;
  Track.Width  := Width - 2 * Pad - ReadoutW;
  Track.Height := BarH;
  if Track.Width < 1 then Exit;

  if FSOC <= FLowThreshold then
    BarCol := Palette.Danger
  else if FCharging then
    BarCol := Palette.Success
  else
    BarCol := EffectiveAccent;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Track background.
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;

    // Fill.
    Fill := Track;
    Fill.Width := Single(Track.Width * FSOC / 100.0);
    Brush := TGPSolidBrush.Create(ColorToARGB(BarCol));
    try
      Graphics.FillRectangle(Brush, Fill);
    finally
      Brush.Free;
    end;

    // Bezel.
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawRectangle(Pen, Track);
    finally
      Pen.Free;
    end;

    // Charging-arrow overlay — a chunky triangle inside the
    // bar, halfway along, when the pack is taking energy.
    if FCharging then
    begin
      ArrowX1 := Track.X + Track.Width * 0.5 - ScaleValue(6);
      ArrowY1 := Track.Y + Track.Height * 0.75;
      ArrowX2 := Track.X + Track.Width * 0.5 + ScaleValue(6);
      ArrowY2 := Track.Y + Track.Height * 0.75;
      ArrowY3 := Track.Y + Track.Height * 0.25;
      Brush := TGPSolidBrush.Create(ColorToARGB(clWhite, 220));
      try
        var Pts: array[0..2] of TGPPointF;
        Pts[0].X := ArrowX1;  Pts[0].Y := ArrowY1;
        Pts[1].X := ArrowX2;  Pts[1].Y := ArrowY2;
        Pts[2].X := Track.X + Track.Width * 0.5;
        Pts[2].Y := ArrowY3;
        Graphics.FillPolygon(Brush, PGPPointF(@Pts[0]), 3);
      finally
        Brush.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;

  if FShowReadout then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := FValueFont;
    ACanvas.Font.Color := EffectiveForeground;
    PercentText := Format('%.0f %%', [FSOC]);
    TextY := (Height - ACanvas.TextHeight(PercentText)) div 2;
    ACanvas.TextOut(
      Width - ReadoutW + ScaleValue(4),
      TextY - ScaleValue(6),
      PercentText);
    if FCharging then
    begin
      ACanvas.Font := FCaptionFont;
      ACanvas.Font.Color := Palette.Success;
      EtaText := FormatTimeToFull;
      if EtaText <> '' then
        ACanvas.TextOut(
          Width - ReadoutW + ScaleValue(4),
          TextY + ACanvas.TextHeight(PercentText) - ScaleValue(2),
          EtaText);
    end;
  end;
end;

{ ---- TOBDRegenIndicator -------------------------------------------------- }

constructor TOBDRegenIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 40;
  Height := 160;
  FPower      := 0;
  FMaxPower   := 200;
  FMaxRegen   := 80;
  FRegenColor := clDefault;
  FDrawColor  := clDefault;
end;

procedure TOBDRegenIndicator.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDRegenIndicator.SetPower(AValue: Double);
begin
  if SameValue(FPower, AValue) then Exit;
  FPower := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDRegenIndicator.SetMaxPower(AValue: Double);
begin
  if AValue < 1 then AValue := 1;
  if SameValue(FMaxPower, AValue) then Exit;
  FMaxPower := AValue; Repaint;
end;

procedure TOBDRegenIndicator.SetMaxRegen(AValue: Double);
begin
  if AValue < 1 then AValue := 1;
  if SameValue(FMaxRegen, AValue) then Exit;
  FMaxRegen := AValue; Repaint;
end;

procedure TOBDRegenIndicator.SetRegenColor(AValue: TColor);
begin
  if FRegenColor = AValue then Exit;
  FRegenColor := AValue; Repaint;
end;

procedure TOBDRegenIndicator.SetDrawColor(AValue: TColor);
begin
  if FDrawColor = AValue then Exit;
  FDrawColor := AValue; Repaint;
end;

procedure TOBDRegenIndicator.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Track, Fill: TGPRectF;
  Pad: Integer;
  CenterY: Single;
  FillH: Single;
  Col: TColor;
begin
  Pad := ScaleValue(4);
  Track.X := Pad;
  Track.Y := Pad;
  Track.Width  := Width  - 2 * Pad;
  Track.Height := Height - 2 * Pad;
  if (Track.Width < 1) or (Track.Height < 1) then Exit;
  CenterY := Track.Y + Track.Height / 2;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Track background.
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;

    // Fill — direction depends on sign.
    if FPower > 0 then
    begin
      Col := ResolveColor(FRegenColor, Palette.Success);
      FillH := System.Math.Min(FPower / FMaxRegen, 1.0) *
                 (Track.Height / 2);
      Fill.X := Track.X;
      Fill.Y := CenterY - FillH;
      Fill.Width  := Track.Width;
      Fill.Height := FillH;
    end
    else if FPower < 0 then
    begin
      Col := ResolveColor(FDrawColor, EffectiveAccent);
      FillH := System.Math.Min(Abs(FPower) / FMaxPower, 1.0) *
                 (Track.Height / 2);
      Fill.X := Track.X;
      Fill.Y := CenterY;
      Fill.Width  := Track.Width;
      Fill.Height := FillH;
    end
    else
    begin
      Col := EffectiveAccent;
      Fill.Width := 0; Fill.Height := 0;
    end;

    if Fill.Height > 0 then
    begin
      Brush := TGPSolidBrush.Create(ColorToARGB(Col));
      try
        Graphics.FillRectangle(Brush, Fill);
      finally
        Brush.Free;
      end;
    end;

    // Centre line.
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawLine(Pen,
        Track.X,  CenterY,
        Track.X + Track.Width, CenterY);
    finally
      Pen.Free;
    end;

    // Bezel.
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawRectangle(Pen, Track);
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

end.
