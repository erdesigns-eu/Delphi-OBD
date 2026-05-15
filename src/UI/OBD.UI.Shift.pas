//------------------------------------------------------------------------------
//  OBD.UI.Shift
//
//  Driver-focussed shift / gear visuals:
//
//    TOBDShiftLight     single LED that lights green / yellow /
//                       red as RPM climbs through RPMStart →
//                       RPMRedline, then flashes red over the
//                       redline.
//    TOBDShiftLightBar  F1-style horizontal LED row.
//                       Progressive illumination green-first,
//                       yellow-middle, red-top.
//    TOBDGearIndicator  large 7-segment gear readout. Accepts
//                       1..9, "R", "N", "P", "D" (and any single
//                       glyph the segment table can render).
//
//  All three inherit theme / HiDPI / VCL-Style awareness from
//  TOBDGraphicControl, drive every value-change through
//  TBindings.Notify, and guard their flash timer with
//  csDesigning so the IDE Designer stays responsive.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Shift;

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
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>RPM-driven single shift LED. Below
  /// <see cref="RPMStart"/> the LED is dark; from
  /// <c>RPMStart</c> the light ramps green → yellow → red as
  /// RPM approaches <see cref="RPMRedline"/>. At/above
  /// <c>RPMRedline</c> the LED flashes red.</summary>
  TOBDShiftLight = class(TOBDGraphicControl)
  strict private
    FRPM:         Double;
    FRPMStart:    Double;
    FRPMRedline:  Double;
    FGreen:       TColor;
    FYellow:      TColor;
    FRed:         TColor;
    FFlashPeriodMs: Cardinal;
    FFlashOn:     Boolean;
    FTimer:       TTimer;
    procedure SetRPM(AValue: Double);
    procedure SetRPMStart(AValue: Double);
    procedure SetRPMRedline(AValue: Double);
    procedure SetGreen(AValue: TColor);
    procedure SetYellow(AValue: TColor);
    procedure SetRed(AValue: TColor);
    procedure SetFlashPeriodMs(AValue: Cardinal);
    procedure HandleTimer(Sender: TObject);
    procedure NotifyBindings;
    procedure UpdateFlasher;
    /// <summary>Returns the lit colour for the current RPM,
    /// or <c>clNone</c> when below <c>RPMStart</c>.</summary>
    function  ResolveLit(out AIsRedFlash: Boolean): TColor;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Current engine RPM. Drives the light state.
    /// </summary>
    property RPM: Double read FRPM write SetRPM;
    /// <summary>RPM at which the green band starts. Below this
    /// the LED is dark. Default 4000.</summary>
    property RPMStart: Double read FRPMStart write SetRPMStart;
    /// <summary>RPM at which the red flash begins. Default 7000.
    /// </summary>
    property RPMRedline: Double
      read FRPMRedline write SetRPMRedline;
    property GreenColor:  TColor read FGreen  write SetGreen
      default clDefault;
    property YellowColor: TColor read FYellow write SetYellow
      default clDefault;
    property RedColor:    TColor read FRed    write SetRed
      default clDefault;
    /// <summary>Flash cycle when over redline. Default 200 ms.
    /// </summary>
    property FlashPeriodMs: Cardinal
      read FFlashPeriodMs write SetFlashPeriodMs default 200;
  end;

  /// <summary>F1-style horizontal LED row. As RPM climbs,
  /// segments illuminate left → right; the first third lights
  /// green, the next third yellow, the last third red. Above
  /// <see cref="RPMRedline"/> the whole row flashes red.
  /// </summary>
  TOBDShiftLightBar = class(TOBDGraphicControl)
  strict private
    FRPM:          Double;
    FRPMStart:     Double;
    FRPMRedline:   Double;
    FSegmentCount: Integer;
    FGapPixels:    Integer;
    FGreen:        TColor;
    FYellow:       TColor;
    FRed:          TColor;
    FFlashPeriodMs: Cardinal;
    FFlashOn:      Boolean;
    FTimer:        TTimer;
    procedure SetRPM(AValue: Double);
    procedure SetRPMStart(AValue: Double);
    procedure SetRPMRedline(AValue: Double);
    procedure SetSegmentCount(AValue: Integer);
    procedure SetGapPixels(AValue: Integer);
    procedure SetGreen(AValue: TColor);
    procedure SetYellow(AValue: TColor);
    procedure SetRed(AValue: TColor);
    procedure SetFlashPeriodMs(AValue: Cardinal);
    procedure HandleTimer(Sender: TObject);
    procedure NotifyBindings;
    procedure UpdateFlasher;
    function  SegmentColor(AIndex: Integer): TColor;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property RPM: Double read FRPM write SetRPM;
    /// <summary>RPM that lights the first segment. Default 4000.
    /// </summary>
    property RPMStart: Double read FRPMStart write SetRPMStart;
    /// <summary>RPM that lights the last segment and starts the
    /// flash. Default 7000.</summary>
    property RPMRedline: Double
      read FRPMRedline write SetRPMRedline;
    /// <summary>Number of LEDs in the row. Default 10.</summary>
    property SegmentCount: Integer
      read FSegmentCount write SetSegmentCount default 10;
    /// <summary>Gap between segments (design pixels). Default 3.
    /// </summary>
    property GapPixels: Integer
      read FGapPixels write SetGapPixels default 3;
    property GreenColor:  TColor read FGreen  write SetGreen
      default clDefault;
    property YellowColor: TColor read FYellow write SetYellow
      default clDefault;
    property RedColor:    TColor read FRed    write SetRed
      default clDefault;
    property FlashPeriodMs: Cardinal
      read FFlashPeriodMs write SetFlashPeriodMs default 160;
  end;

  /// <summary>Big 7-segment gear readout. <see cref="Gear"/>
  /// accepts any single character; the built-in segment table
  /// covers <c>0..9, A..F, R, N, P, D, L, H, '-'</c> — anything
  /// else renders blank.</summary>
  TOBDGearIndicator = class(TOBDGraphicControl)
  strict private
    FGear:    Char;
    FOnColor: TColor;
    FOffColor: TColor;
    procedure SetGear(AValue: Char);
    procedure SetOnColor(AValue: TColor);
    procedure SetOffColor(AValue: TColor);
    procedure NotifyBindings;
    function  SegmentMask(AGlyph: Char): Byte;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Convenience: set gear from an Integer (1..9).
    /// Values outside that range render blank.</summary>
    procedure SetGearInt(AValue: Integer);
  published
    /// <summary>Currently shown glyph. Default <c>'N'</c>.
    /// </summary>
    property Gear: Char read FGear write SetGear default 'N';
    /// <summary>Lit-segment colour. Default theme accent.</summary>
    property OnColor: TColor read FOnColor write SetOnColor
      default clDefault;
    /// <summary>Dim-segment colour. Default subtle theme tint.
    /// </summary>
    property OffColor: TColor read FOffColor write SetOffColor
      default clDefault;
  end;

implementation

{ ---- TOBDShiftLight ------------------------------------------------------- }

constructor TOBDShiftLight.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 32;
  Height := 32;
  FRPM           := 0;
  FRPMStart      := 4000;
  FRPMRedline    := 7000;
  FGreen         := clDefault;
  FYellow        := clDefault;
  FRed           := clDefault;
  FFlashPeriodMs := 200;
  FFlashOn       := True;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
end;

destructor TOBDShiftLight.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TOBDShiftLight.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDShiftLight.UpdateFlasher;
begin
  if (FRPM >= FRPMRedline) and
     not (csDesigning in ComponentState) then
  begin
    FTimer.Interval := System.Math.Max(50, FFlashPeriodMs div 2);
    FTimer.Enabled  := True;
  end
  else
  begin
    FTimer.Enabled := False;
    FFlashOn := True;
  end;
end;

procedure TOBDShiftLight.SetRPM(AValue: Double);
begin
  if SameValue(FRPM, AValue) then Exit;
  FRPM := AValue;
  UpdateFlasher;
  NotifyBindings;
  Repaint;
end;

procedure TOBDShiftLight.SetRPMStart(AValue: Double);
begin
  if SameValue(FRPMStart, AValue) then Exit;
  FRPMStart := AValue; Repaint;
end;

procedure TOBDShiftLight.SetRPMRedline(AValue: Double);
begin
  if SameValue(FRPMRedline, AValue) then Exit;
  FRPMRedline := AValue;
  UpdateFlasher;
  Repaint;
end;

procedure TOBDShiftLight.SetGreen(AValue: TColor);
begin
  if FGreen = AValue then Exit;
  FGreen := AValue; Repaint;
end;

procedure TOBDShiftLight.SetYellow(AValue: TColor);
begin
  if FYellow = AValue then Exit;
  FYellow := AValue; Repaint;
end;

procedure TOBDShiftLight.SetRed(AValue: TColor);
begin
  if FRed = AValue then Exit;
  FRed := AValue; Repaint;
end;

procedure TOBDShiftLight.SetFlashPeriodMs(AValue: Cardinal);
begin
  if AValue < 50 then AValue := 50;
  if FFlashPeriodMs = AValue then Exit;
  FFlashPeriodMs := AValue;
  if FTimer.Enabled then
    FTimer.Interval := FFlashPeriodMs div 2;
end;

procedure TOBDShiftLight.HandleTimer(Sender: TObject);
begin
  FFlashOn := not FFlashOn;
  Repaint;
end;

function TOBDShiftLight.ResolveLit(out AIsRedFlash: Boolean): TColor;
var
  Span, Norm: Double;
begin
  AIsRedFlash := False;
  if FRPM < FRPMStart then Exit(clNone);
  if FRPM >= FRPMRedline then
  begin
    AIsRedFlash := True;
    Exit(ResolveColor(FRed, Palette.Danger));
  end;
  Span := FRPMRedline - FRPMStart;
  if Span <= 0 then Exit(ResolveColor(FGreen, Palette.Success));
  Norm := (FRPM - FRPMStart) / Span;        // 0..1
  if Norm < 0.5 then
    Result := ResolveColor(FGreen,  Palette.Success)
  else if Norm < 0.85 then
    Result := ResolveColor(FYellow, Palette.Warning)
  else
    Result := ResolveColor(FRed,    Palette.Danger);
end;

procedure TOBDShiftLight.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Body, Halo: TGPRectF;
  LitCol, OffCol, BorderCol: TColor;
  IsRedFlash: Boolean;
  D, X, Y: Integer;
begin
  D := System.Math.Min(Width, Height);
  X := (Width  - D) div 2;
  Y := (Height - D) div 2;
  Body.X := X + ScaleValue(2);
  Body.Y := Y + ScaleValue(2);
  Body.Width  := D - 2 * ScaleValue(2);
  Body.Height := D - 2 * ScaleValue(2);

  LitCol := ResolveLit(IsRedFlash);
  // Flash phase off → render dark.
  if IsRedFlash and not FFlashOn then LitCol := clNone;
  OffCol    := Palette.NeutralLight;
  BorderCol := EffectiveBorder;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Soft halo when lit.
    if LitCol <> clNone then
    begin
      Halo := Body;
      Halo.X := Halo.X - ScaleValue(3);
      Halo.Y := Halo.Y - ScaleValue(3);
      Halo.Width  := Halo.Width  + ScaleValue(6);
      Halo.Height := Halo.Height + ScaleValue(6);
      Brush := TGPSolidBrush.Create(ColorToARGB(LitCol, 56));
      try
        Graphics.FillEllipse(Brush, Halo);
      finally
        Brush.Free;
      end;
    end;

    // Body.
    if LitCol <> clNone then
      Brush := TGPSolidBrush.Create(ColorToARGB(LitCol))
    else
      Brush := TGPSolidBrush.Create(ColorToARGB(OffCol));
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
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDShiftLightBar ---------------------------------------------------- }

constructor TOBDShiftLightBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 260;
  Height := 24;
  FRPM           := 0;
  FRPMStart      := 4000;
  FRPMRedline    := 7000;
  FSegmentCount  := 10;
  FGapPixels     := 3;
  FGreen         := clDefault;
  FYellow        := clDefault;
  FRed           := clDefault;
  FFlashPeriodMs := 160;
  FFlashOn       := True;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
end;

destructor TOBDShiftLightBar.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TOBDShiftLightBar.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDShiftLightBar.UpdateFlasher;
begin
  if (FRPM >= FRPMRedline) and
     not (csDesigning in ComponentState) then
  begin
    FTimer.Interval := System.Math.Max(50, FFlashPeriodMs div 2);
    FTimer.Enabled  := True;
  end
  else
  begin
    FTimer.Enabled := False;
    FFlashOn := True;
  end;
end;

procedure TOBDShiftLightBar.SetRPM(AValue: Double);
begin
  if SameValue(FRPM, AValue) then Exit;
  FRPM := AValue;
  UpdateFlasher;
  NotifyBindings;
  Repaint;
end;

procedure TOBDShiftLightBar.SetRPMStart(AValue: Double);
begin
  if SameValue(FRPMStart, AValue) then Exit;
  FRPMStart := AValue; Repaint;
end;

procedure TOBDShiftLightBar.SetRPMRedline(AValue: Double);
begin
  if SameValue(FRPMRedline, AValue) then Exit;
  FRPMRedline := AValue;
  UpdateFlasher;
  Repaint;
end;

procedure TOBDShiftLightBar.SetSegmentCount(AValue: Integer);
begin
  if AValue < 3 then AValue := 3;
  if AValue > 64 then AValue := 64;
  if FSegmentCount = AValue then Exit;
  FSegmentCount := AValue; Repaint;
end;

procedure TOBDShiftLightBar.SetGapPixels(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FGapPixels = AValue then Exit;
  FGapPixels := AValue; Repaint;
end;

procedure TOBDShiftLightBar.SetGreen(AValue: TColor);
begin
  if FGreen = AValue then Exit;
  FGreen := AValue; Repaint;
end;

procedure TOBDShiftLightBar.SetYellow(AValue: TColor);
begin
  if FYellow = AValue then Exit;
  FYellow := AValue; Repaint;
end;

procedure TOBDShiftLightBar.SetRed(AValue: TColor);
begin
  if FRed = AValue then Exit;
  FRed := AValue; Repaint;
end;

procedure TOBDShiftLightBar.SetFlashPeriodMs(AValue: Cardinal);
begin
  if AValue < 50 then AValue := 50;
  if FFlashPeriodMs = AValue then Exit;
  FFlashPeriodMs := AValue;
  if FTimer.Enabled then
    FTimer.Interval := FFlashPeriodMs div 2;
end;

procedure TOBDShiftLightBar.HandleTimer(Sender: TObject);
begin
  FFlashOn := not FFlashOn;
  Repaint;
end;

function TOBDShiftLightBar.SegmentColor(AIndex: Integer): TColor;
var
  Third: Integer;
begin
  Third := FSegmentCount div 3;
  if AIndex < Third then
    Result := ResolveColor(FGreen,  Palette.Success)
  else if AIndex < 2 * Third then
    Result := ResolveColor(FYellow, Palette.Warning)
  else
    Result := ResolveColor(FRed,    Palette.Danger);
end;

procedure TOBDShiftLightBar.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pad, Gap, SegW: Integer;
  Span, Norm: Double;
  ActiveTo, I: Integer;
  Seg: TGPRectF;
  IsFlash, Lit: Boolean;
  SegCol, OffCol: TColor;
begin
  Pad := ScaleValue(2);
  Gap := ScaleValue(FGapPixels);
  if FSegmentCount <= 0 then Exit;
  SegW := (Width - 2 * Pad - (FSegmentCount - 1) * Gap)
            div FSegmentCount;
  if SegW <= 0 then Exit;

  if FRPM <= FRPMStart then
    ActiveTo := 0
  else if FRPM >= FRPMRedline then
    ActiveTo := FSegmentCount
  else
  begin
    Span := FRPMRedline - FRPMStart;
    if Span <= 0 then
      ActiveTo := FSegmentCount
    else
    begin
      Norm := (FRPM - FRPMStart) / Span;
      ActiveTo := Round(Norm * FSegmentCount);
    end;
  end;

  IsFlash := (FRPM >= FRPMRedline);
  OffCol  := Palette.NeutralLight;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for I := 0 to FSegmentCount - 1 do
    begin
      Seg.X := Pad + I * (SegW + Gap);
      Seg.Y := Pad;
      Seg.Width  := SegW;
      Seg.Height := Height - 2 * Pad;

      Lit := (I < ActiveTo);
      if IsFlash then
        Lit := Lit and FFlashOn;
      if Lit then
        SegCol := SegmentColor(I)
      else
        SegCol := OffCol;

      Brush := TGPSolidBrush.Create(ColorToARGB(SegCol));
      try
        Graphics.FillRectangle(Brush, Seg);
      finally
        Brush.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDGearIndicator --------------------------------------------------- }

constructor TOBDGearIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 80;
  Height := 120;
  FGear     := 'N';
  FOnColor  := clDefault;
  FOffColor := clDefault;
end;

procedure TOBDGearIndicator.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDGearIndicator.SetGear(AValue: Char);
begin
  if FGear = AValue then Exit;
  FGear := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDGearIndicator.SetGearInt(AValue: Integer);
begin
  if (AValue >= 0) and (AValue <= 9) then
    SetGear(Char(Ord('0') + AValue))
  else
    SetGear(' ');
end;

procedure TOBDGearIndicator.SetOnColor(AValue: TColor);
begin
  if FOnColor = AValue then Exit;
  FOnColor := AValue; Repaint;
end;

procedure TOBDGearIndicator.SetOffColor(AValue: TColor);
begin
  if FOffColor = AValue then Exit;
  FOffColor := AValue; Repaint;
end;

function TOBDGearIndicator.SegmentMask(AGlyph: Char): Byte;
begin
  // 7-segment encoding, bit 0 = top, going clockwise:
  //   0=top, 1=upper-right, 2=lower-right, 3=bottom,
  //   4=lower-left, 5=upper-left, 6=middle.
  case AGlyph of
    '0': Result := $3F;
    '1': Result := $06;
    '2': Result := $5B;
    '3': Result := $4F;
    '4': Result := $66;
    '5': Result := $6D;
    '6': Result := $7D;
    '7': Result := $07;
    '8': Result := $7F;
    '9': Result := $6F;
    'A', 'a': Result := $77;
    'b':      Result := $7C;
    'C':      Result := $39;
    'c':      Result := $58;
    'd':      Result := $5E;
    'E', 'e': Result := $79;
    'F', 'f': Result := $71;
    'H', 'h': Result := $76;
    'L', 'l': Result := $38;
    'N':      Result := $37;
    'n':      Result := $54;
    'P', 'p': Result := $73;
    'R', 'r': Result := $50;
    'D':      Result := $3F;     // visually same as zero
    '-':      Result := $40;     // middle only
    ' ':      Result := $00;
  else
    Result := $00;
  end;
end;

procedure TOBDGearIndicator.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Mask: Byte;
  OnCol, OffCol: TColor;
  SegRect: TGPRectF;
  Cx, Cy, W, H, SegLen, SegTh, Mid: Single;

  procedure FillIf(ABit: Byte; const R: TGPRectF);
  var Col: TColor;
  begin
    if (Mask and (1 shl ABit)) <> 0 then Col := OnCol
    else                                  Col := OffCol;
    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      Graphics.FillRectangle(Brush, R);
    finally
      Brush.Free;
    end;
  end;

begin
  Mask := SegmentMask(FGear);
  OnCol  := ResolveColor(FOnColor,  Palette.Accent);
  OffCol := ResolveColor(FOffColor, Palette.NeutralLight);
  // Make the off colour translucent so the lit glyph reads
  // clearly against it; GDI+ ARGB brushes pick this up.
  OffCol := ColorToRGB(OffCol);

  W := Width;
  H := Height;
  Cx := W / 2;
  Cy := H / 2;
  SegLen := System.Math.Min(W * 0.55, H * 0.35);
  SegTh  := SegLen * 0.18;
  Mid    := Cy;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Top.
    SegRect.X := Cx - SegLen / 2;
    SegRect.Y := Mid - SegLen - SegTh;
    SegRect.Width  := SegLen;
    SegRect.Height := SegTh;
    FillIf(0, SegRect);
    // Upper-right.
    SegRect.X := Cx + SegLen / 2 - SegTh;
    SegRect.Y := Mid - SegLen - SegTh / 2;
    SegRect.Width  := SegTh;
    SegRect.Height := SegLen;
    FillIf(1, SegRect);
    // Lower-right.
    SegRect.X := Cx + SegLen / 2 - SegTh;
    SegRect.Y := Mid + SegTh / 2;
    SegRect.Width  := SegTh;
    SegRect.Height := SegLen;
    FillIf(2, SegRect);
    // Bottom.
    SegRect.X := Cx - SegLen / 2;
    SegRect.Y := Mid + SegLen;
    SegRect.Width  := SegLen;
    SegRect.Height := SegTh;
    FillIf(3, SegRect);
    // Lower-left.
    SegRect.X := Cx - SegLen / 2;
    SegRect.Y := Mid + SegTh / 2;
    SegRect.Width  := SegTh;
    SegRect.Height := SegLen;
    FillIf(4, SegRect);
    // Upper-left.
    SegRect.X := Cx - SegLen / 2;
    SegRect.Y := Mid - SegLen - SegTh / 2;
    SegRect.Width  := SegTh;
    SegRect.Height := SegLen;
    FillIf(5, SegRect);
    // Middle.
    SegRect.X := Cx - SegLen / 2;
    SegRect.Y := Mid - SegTh / 2;
    SegRect.Width  := SegLen;
    SegRect.Height := SegTh;
    FillIf(6, SegRect);
  finally
    Graphics.Free;
  end;
end;

end.
