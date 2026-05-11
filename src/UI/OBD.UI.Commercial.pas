//------------------------------------------------------------------------------
//  OBD.UI.Commercial
//
//  Commercial / heavy-duty visuals:
//
//    TOBDMarineTach    High-RPM single tach (0..6000 default)
//                      with hours-meter readout.
//    TOBDPTOMeter      PTO speed gauge (rpm) with 540 / 1000
//                      reference markers (agricultural).
//    TOBDDPFStatus     Diesel particulate filter soot load %
//                      bar + regen-active lamp.
//    TOBDAdBlueLevel   DEF tank fill bar with low-warning
//                      threshold.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Commercial;

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
  OBD.UI.Control,
  OBD.UI.Gauges.Types,
  OBD.UI.Gauges.Base,
  OBD.UI.Gauges.Dial;

type
  /// <summary>Marine / industrial tach. Default scale 0..6000
  /// rpm with an integrated hours-meter readout in the lower
  /// segment.</summary>
  TOBDMarineTach = class(TOBDCircularGauge)
  strict private
    FHours: Double;
    FCaptionFont: TFont;
    procedure SetHours(AValue: Double);
    procedure SetCaptionFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
  protected
    procedure PaintExtras(AGraphics: TGPGraphics;
      const ABounds: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Total run-time in hours. Default 0.</summary>
    property Hours: Double read FHours write SetHours;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
  end;

  /// <summary>PTO speed gauge with reference markers at 540
  /// and 1000 rpm (the agricultural standards).</summary>
  TOBDPTOMeter = class(TOBDCircularGauge)
  strict private
    procedure SeedZones;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>Diesel-particulate-filter status panel. Soot-
  /// load percentage bar with a regen-active indicator that
  /// pulses when active.</summary>
  TOBDDPFStatus = class(TOBDCustomControl)
  strict private
    FSootPercent: Double;
    FRegenActive: Boolean;
    FCaptionFont: TFont;
    FValueFont:   TFont;
    procedure SetSootPercent(AValue: Double);
    procedure SetRegenActive(AValue: Boolean);
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
    /// <summary>Soot load %. Clamped 0..100.</summary>
    property SootPercent: Double
      read FSootPercent write SetSootPercent;
    /// <summary>True while a regen cycle is running.</summary>
    property RegenActive: Boolean
      read FRegenActive write SetRegenActive default False;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>DEF / AdBlue tank gauge. Vertical bar with a
  /// red sliver below <see cref="LowThreshold"/>.</summary>
  TOBDAdBlueLevel = class(TOBDCustomControl)
  strict private
    FLevelPercent: Double;
    FLowThreshold: Double;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    procedure SetLevelPercent(AValue: Double);
    procedure SetLowThreshold(AValue: Double);
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
    /// <summary>Fill % (0..100).</summary>
    property LevelPercent: Double
      read FLevelPercent write SetLevelPercent;
    /// <summary>Warning threshold (%). Default 15.</summary>
    property LowThreshold: Double
      read FLowThreshold write SetLowThreshold;
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

{ ---- TOBDMarineTach --------------------------------------------------- }

constructor TOBDMarineTach.Create(AOwner: TComponent);
var Cfg: TOBDGaugeTickConfig;
begin
  inherited Create(AOwner);
  Min := 0;
  Max := 6000;
  &Unit := 'rpm';
  Caption := 'Marine RPM';
  Cfg := DefaultTickConfig;
  Cfg.MajorInterval := 1000;
  Cfg.MinorTicksPerMajor := 4;
  Cfg.LabelDecimals := 0;
  SetTickConfig(Cfg);
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
end;

destructor TOBDMarineTach.Destroy;
begin
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDMarineTach.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDMarineTach.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDMarineTach.SetHours(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FHours, AValue) then Exit;
  FHours := AValue;
  Repaint;
end;

procedure TOBDMarineTach.PaintExtras(AGraphics: TGPGraphics;
  const ABounds: TRectF);
var
  C: TCanvas;
  S: string;
  W: Integer;
begin
  C := Canvas;
  C.Brush.Style := bsClear;
  C.Font := FCaptionFont;
  C.Font.Color := EffectiveForeground;
  S := Format('%.1f h', [FHours]);
  W := C.TextWidth(S);
  C.TextOut(
    Round(ABounds.X + (ABounds.Width  - W) / 2),
    Round(ABounds.Y + ABounds.Height * 0.78),
    S);
end;

{ ---- TOBDPTOMeter ----------------------------------------------------- }

constructor TOBDPTOMeter.Create(AOwner: TComponent);
var Cfg: TOBDGaugeTickConfig;
begin
  inherited Create(AOwner);
  Min := 0;
  Max := 1500;
  &Unit := 'rpm';
  Caption := 'PTO';
  Cfg := DefaultTickConfig;
  Cfg.MajorInterval := 200;
  Cfg.MinorTicksPerMajor := 1;
  Cfg.LabelDecimals := 0;
  SetTickConfig(Cfg);
  SeedZones;
end;

procedure TOBDPTOMeter.SeedZones;
var Z: TOBDGaugeZones;
begin
  // 540 and 1000 rpm reference bands (each ±10 rpm wide).
  SetLength(Z, 2);
  Z[0] := MakeGaugeZone(530, 550, Palette.Success);
  Z[1] := MakeGaugeZone(990, 1010, Palette.Success);
  SetZones(Z);
end;

{ ---- TOBDDPFStatus ---------------------------------------------------- }

constructor TOBDDPFStatus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 280;
  Height := 90;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 14;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDDPFStatus.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDDPFStatus.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDPFStatus.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDDPFStatus.SetSootPercent(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FSootPercent, AValue) then Exit;
  FSootPercent := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDPFStatus.SetRegenActive(AValue: Boolean);
begin
  if FRegenActive = AValue then Exit;
  FRegenActive := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDPFStatus.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDDPFStatus.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDDPFStatus.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad: Integer;
  Track, Fill: TGPRectF;
  Col: TColor;
  ValStr: string;
begin
  Pad := ScaleValue(10);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(Pad, Pad, 'DPF soot load');

  Track.X := Pad;
  Track.Y := Pad + ACanvas.TextHeight('Mg') + ScaleValue(4);
  Track.Width  := Width - 2 * Pad - ScaleValue(80);
  Track.Height := ScaleValue(14);

  if FSootPercent > 80 then Col := Palette.Danger
  else if FSootPercent > 60 then Col := Palette.Warning
  else Col := Palette.Success;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;
    Fill := Track;
    Fill.Width := Single(FSootPercent / 100) * Track.Width;
    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      Graphics.FillRectangle(Brush, Fill);
    finally
      Brush.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.DrawRectangle(Pen, Track);
    finally
      Pen.Free;
    end;

    if FRegenActive then
    begin
      Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Warning));
      try
        Graphics.FillEllipse(Brush,
          Width - Pad - ScaleValue(60),
          Track.Y, ScaleValue(14), ScaleValue(14));
      finally
        Brush.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;

  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := Col;
  ValStr := Format('%.0f %%', [FSootPercent]);
  ACanvas.TextOut(Width - Pad - ACanvas.TextWidth(ValStr),
    Round(Track.Y - ScaleValue(2)), ValStr);

  if FRegenActive then
  begin
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := Palette.Warning;
    ACanvas.TextOut(Pad, Height - Pad -
      ACanvas.TextHeight('Mg'), 'regen active');
  end;
end;

{ ---- TOBDAdBlueLevel -------------------------------------------------- }

constructor TOBDAdBlueLevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 60;
  Height := 180;
  FLevelPercent := 100;
  FLowThreshold := 15;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 8;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 11;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDAdBlueLevel.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDAdBlueLevel.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDAdBlueLevel.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDAdBlueLevel.SetLevelPercent(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FLevelPercent, AValue) then Exit;
  FLevelPercent := AValue; NotifyBindings; Repaint;
end;

procedure TOBDAdBlueLevel.SetLowThreshold(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FLowThreshold, AValue) then Exit;
  FLowThreshold := AValue; Repaint;
end;

procedure TOBDAdBlueLevel.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDAdBlueLevel.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDAdBlueLevel.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad: Integer;
  Track, Fill: TGPRectF;
  Col: TColor;
  Val: string;
begin
  Pad := ScaleValue(6);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(Pad, Pad, 'DEF');

  Track.X := Pad;
  Track.Y := Pad + ACanvas.TextHeight('Mg') + ScaleValue(2);
  Track.Width := Width - 2 * Pad;
  Track.Height := Height - Track.Y - ScaleValue(28) - Pad;

  if FLevelPercent <= FLowThreshold then Col := Palette.Danger
  else Col := Palette.Success;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;
    Fill := Track;
    Fill.Height := Single(FLevelPercent / 100) * Track.Height;
    Fill.Y := Track.Y + (Track.Height - Fill.Height);
    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      Graphics.FillRectangle(Brush, Fill);
    finally
      Brush.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.DrawRectangle(Pen, Track);
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;

  Val := Format('%.0f %%', [FLevelPercent]);
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := Col;
  ACanvas.TextOut(
    (Width - ACanvas.TextWidth(Val)) div 2,
    Height - Pad - ACanvas.TextHeight(Val),
    Val);
end;

end.
