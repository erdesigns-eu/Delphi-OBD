//------------------------------------------------------------------------------
//  OBD.UI.Branding
//
//  Branding / utility visuals:
//
//    TOBDOEMBadge        Vendor logo card. Manual SetBrand or
//                        host-provided VIN → OEM mapping.
//    TOBDDigitalCluster  Retro 7-segment N-digit cluster.
//    TOBDBluetoothSignal RSSI bars for BT adapter.
//    TOBDWiFiSignal      RSSI bars for WiFi adapter.
//    TOBDGPSAccuracy     HDOP + sat-count indicator.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Branding;

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
  OBD.UI.Control;

type
  /// <summary>Vendor / OEM badge card. Hosts set the brand
  /// label + colour via <see cref="SetBrand"/>; the visual
  /// renders a centred name in a coloured card.</summary>
  TOBDOEMBadge = class(TOBDCustomControl)
  strict private
    FBrandName: string;
    FAccent:    TColor;
    FFont:      TFont;
    procedure SetBrandName(const AValue: string);
    procedure SetAccent(AValue: TColor);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>One-shot brand setter (name + accent colour).
    /// </summary>
    procedure SetBrand(const AName: string; AColor: TColor);
  published
    /// <summary>Brand display name shown on the card.</summary>
    property BrandName: string
      read FBrandName write SetBrandName;
    /// <summary>Card accent / fill colour. <c>clDefault</c>
    /// uses the active theme accent.</summary>
    property AccentColor: TColor
      read FAccent write SetAccent default clDefault;
    /// <summary>Font used for the brand name.</summary>
    property BrandFont: TFont read FFont write SetFontA;
  end;

  /// <summary>Multi-digit 7-segment cluster. Set
  /// <see cref="DigitCount"/> + <see cref="Text"/>; non-
  /// segmentable characters render blank.</summary>
  TOBDDigitalCluster = class(TOBDCustomControl)
  strict private
    FDigitCount: Integer;
    FText:       string;
    FOnColor:    TColor;
    FOffColor:   TColor;
    procedure SetDigitCount(AValue: Integer);
    procedure SetText(const AValue: string);
    procedure SetOnColor(AValue: TColor);
    procedure SetOffColor(AValue: TColor);
    procedure NotifyBindings;
    function  SegmentMask(AGlyph: Char): Byte;
    procedure DrawDigit(AGraphics: TGPGraphics;
      AX, AY, AW, AH: Single; AMask: Byte);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Number of digits in the cluster. Default 4.
    /// </summary>
    property DigitCount: Integer
      read FDigitCount write SetDigitCount default 4;
    /// <summary>Display text. Right-justified into
    /// <see cref="DigitCount"/> digits; unsegmentable
    /// characters render as blank.</summary>
    property Text: string read FText write SetText;
    /// <summary>Colour for lit segments. <c>clDefault</c> uses
    /// the theme accent.</summary>
    property OnColor: TColor read FOnColor write SetOnColor
      default clDefault;
    /// <summary>Colour for unlit segments. <c>clDefault</c>
    /// uses a dim theme tone.</summary>
    property OffColor: TColor read FOffColor write SetOffColor
      default clDefault;
  end;

  /// <summary>RSSI signal-strength bars. Shared by
  /// Bluetooth + Wi-Fi + GPS — the only difference is the
  /// metric and label.</summary>
  TOBDSignalBarsBase = class(TOBDCustomControl)
  strict private
    FBarCount:  Integer;
    FActive:    Integer;
    FCaption:   string;
    FFont:      TFont;
    procedure SetBarCount(AValue: Integer);
    procedure SetActive(AValue: Integer);
    procedure SetCaption(const AValue: string);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Total number of bars in the indicator.
    /// Default 4.</summary>
    property BarCount: Integer
      read FBarCount write SetBarCount default 4;
    /// <summary>Number of bars currently lit (0..BarCount).
    /// </summary>
    property ActiveBars: Integer
      read FActive write SetActive default 0;
    /// <summary>Caption rendered next to the bars.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Font used for the caption.</summary>
    property LabelFont: TFont read FFont write SetFontA;
  end;

  /// <summary>Bluetooth RSSI bars. <see cref="RssiDbm"/>
  /// (typically -100..-30 dBm) maps onto the 4-bar scale.
  /// </summary>
  TOBDBluetoothSignal = class(TOBDSignalBarsBase)
  strict private
    FRssiDbm: Integer;
    procedure SetRssiDbm(AValue: Integer);
    procedure UpdateBars;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Current Bluetooth RSSI in dBm (typically
    /// -100..-30). Default -100 (no signal).</summary>
    property RssiDbm: Integer read FRssiDbm write SetRssiDbm
      default -100;
  end;

  /// <summary>Wi-Fi RSSI bars.</summary>
  TOBDWiFiSignal = class(TOBDSignalBarsBase)
  strict private
    FRssiDbm: Integer;
    procedure SetRssiDbm(AValue: Integer);
    procedure UpdateBars;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Current Wi-Fi RSSI in dBm (typically
    /// -100..-30). Default -100 (no signal).</summary>
    property RssiDbm: Integer read FRssiDbm write SetRssiDbm
      default -100;
  end;

  /// <summary>GPS accuracy indicator: HDOP + sat-count.
  /// Renders as a small panel with a coloured grade.</summary>
  TOBDGPSAccuracy = class(TOBDCustomControl)
  strict private
    FHDOP:     Double;
    FSatCount: Integer;
    FFont:     TFont;
    procedure SetHDOP(AValue: Double);
    procedure SetSatCount(AValue: Integer);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  GradeColor: TColor;
    function  GradeText: string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Horizontal dilution of precision (lower is
    /// better; &lt;2 is "excellent", &gt;5 is "poor").</summary>
    property HDOP: Double read FHDOP write SetHDOP;
    /// <summary>Satellite count contributing to the fix.
    /// Default 0.</summary>
    property SatCount: Integer
      read FSatCount write SetSatCount default 0;
    /// <summary>Font used for the readout labels.</summary>
    property LabelFont: TFont read FFont write SetFontA;
  end;

implementation

{ ---- TOBDOEMBadge --------------------------------------------------- }

constructor TOBDOEMBadge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 160;
  Height := 60;
  FAccent := clDefault;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 14;
  FFont.Style := [fsBold];
  FFont.Color := clWhite;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDOEMBadge.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDOEMBadge.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDOEMBadge.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDOEMBadge.SetBrandName(const AValue: string);
begin
  if FBrandName = AValue then Exit;
  FBrandName := AValue; NotifyBindings; Repaint;
end;

procedure TOBDOEMBadge.SetAccent(AValue: TColor);
begin
  if FAccent = AValue then Exit;
  FAccent := AValue; Repaint;
end;

procedure TOBDOEMBadge.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDOEMBadge.SetBrand(const AName: string; AColor: TColor);
begin
  FBrandName := AName;
  FAccent    := AColor;
  NotifyBindings;
  Repaint;
end;

procedure TOBDOEMBadge.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Card: TGPRectF;
  Pad: Integer;
  Col: TColor;
  TextW, TextH: Integer;
begin
  Pad := ScaleValue(4);
  Card.X := Pad;
  Card.Y := Pad;
  Card.Width  := Width - 2 * Pad;
  Card.Height := Height - 2 * Pad;
  Col := ResolveColor(FAccent, EffectiveAccent);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Brush := TGPSolidBrush.Create(ColorToARGB(Col));
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.FillRectangle(Brush, Card);
  finally
    Brush.Free;
    Graphics.Free;
  end;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  TextW := ACanvas.TextWidth(FBrandName);
  TextH := ACanvas.TextHeight(FBrandName);
  ACanvas.TextOut(
    Round(Card.X + (Card.Width  - TextW) / 2),
    Round(Card.Y + (Card.Height - TextH) / 2),
    FBrandName);
end;

{ ---- TOBDDigitalCluster -------------------------------------------- }

constructor TOBDDigitalCluster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 200;
  Height := 60;
  FDigitCount := 4;
  FText := '----';
  FOnColor := clDefault;
  FOffColor := clDefault;
end;

procedure TOBDDigitalCluster.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDigitalCluster.SetDigitCount(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if AValue > 16 then AValue := 16;
  if FDigitCount = AValue then Exit;
  FDigitCount := AValue; Repaint;
end;

procedure TOBDDigitalCluster.SetText(const AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue; NotifyBindings; Repaint;
end;

procedure TOBDDigitalCluster.SetOnColor(AValue: TColor);
begin
  if FOnColor = AValue then Exit;
  FOnColor := AValue; Repaint;
end;

procedure TOBDDigitalCluster.SetOffColor(AValue: TColor);
begin
  if FOffColor = AValue then Exit;
  FOffColor := AValue; Repaint;
end;

function TOBDDigitalCluster.SegmentMask(AGlyph: Char): Byte;
begin
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
    '-': Result := $40;
    ' ': Result := $00;
  else
    Result := $00;
  end;
end;

procedure TOBDDigitalCluster.DrawDigit(AGraphics: TGPGraphics;
  AX, AY, AW, AH: Single; AMask: Byte);
var
  Brush: TGPSolidBrush;
  OnCol, OffCol: TColor;
  SegLen, SegTh: Single;
  CY: Single;

  procedure Fill(ABit: Byte; const R: TGPRectF);
  var Col: TColor;
  begin
    if (AMask and (1 shl ABit)) <> 0 then Col := OnCol
    else                                  Col := OffCol;
    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      AGraphics.FillRectangle(Brush, R);
    finally
      Brush.Free;
    end;
  end;

  function R(X1, Y1, X2, Y2: Single): TGPRectF;
  begin
    Result.X := X1;
    Result.Y := Y1;
    Result.Width  := X2 - X1;
    Result.Height := Y2 - Y1;
  end;

begin
  OnCol := ResolveColor(FOnColor, Palette.Accent);
  OffCol := ResolveColor(FOffColor, Palette.NeutralLight);
  SegLen := AW * 0.7;
  SegTh  := AW * 0.14;
  CY := AY + AH / 2;

  Fill(0, R(AX + AW * 0.15, AY, AX + AW * 0.85, AY + SegTh));
  Fill(1, R(AX + AW * 0.85 - SegTh, AY + SegTh, AX + AW * 0.85, CY));
  Fill(2, R(AX + AW * 0.85 - SegTh, CY, AX + AW * 0.85, AY + AH - SegTh));
  Fill(3, R(AX + AW * 0.15, AY + AH - SegTh, AX + AW * 0.85, AY + AH));
  Fill(4, R(AX + AW * 0.15, CY, AX + AW * 0.15 + SegTh, AY + AH - SegTh));
  Fill(5, R(AX + AW * 0.15, AY + SegTh, AX + AW * 0.15 + SegTh, CY));
  Fill(6, R(AX + AW * 0.15, CY - SegTh / 2, AX + AW * 0.85, CY + SegTh / 2));
end;

procedure TOBDDigitalCluster.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  I: Integer;
  W, H, X, Y, Pad: Single;
  Disp: string;
  Glyph: Char;
begin
  Pad := ScaleValue(4);
  W := (Width - 2 * Pad - (FDigitCount - 1) * Pad) / FDigitCount;
  H := Height - 2 * Pad;
  if (W <= 0) or (H <= 0) then Exit;

  Disp := FText;
  while Length(Disp) < FDigitCount do Disp := ' ' + Disp;
  if Length(Disp) > FDigitCount then
    Disp := Copy(Disp, Length(Disp) - FDigitCount + 1, FDigitCount);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Y := Pad;
    for I := 0 to FDigitCount - 1 do
    begin
      X := Pad + I * (W + Pad);
      Glyph := Disp[I + 1];
      DrawDigit(Graphics, X, Y, W, H, SegmentMask(Glyph));
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDSignalBarsBase -------------------------------------------- }

constructor TOBDSignalBarsBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 80;
  Height := 32;
  FBarCount := 4;
  FActive   := 0;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 8;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDSignalBarsBase.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDSignalBarsBase.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDSignalBarsBase.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDSignalBarsBase.SetBarCount(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if AValue > 8 then AValue := 8;
  if FBarCount = AValue then Exit;
  FBarCount := AValue; Repaint;
end;

procedure TOBDSignalBarsBase.SetActive(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if AValue > FBarCount then AValue := FBarCount;
  if FActive = AValue then Exit;
  FActive := AValue; NotifyBindings; Repaint;
end;

procedure TOBDSignalBarsBase.SetCaption(const AValue: string);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue; Repaint;
end;

procedure TOBDSignalBarsBase.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDSignalBarsBase.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pad, BarsAreaW, BarsAreaH, BarW, BarGap: Integer;
  I: Integer;
  X, Y, H: Single;
  Col: TColor;
begin
  Pad := ScaleValue(4);
  BarsAreaW := Width - 2 * Pad;
  if FCaption <> '' then BarsAreaW := BarsAreaW - ScaleValue(40);
  BarsAreaH := Height - 2 * Pad;
  if BarsAreaW < FBarCount then Exit;
  BarGap := ScaleValue(2);
  BarW := (BarsAreaW - (FBarCount - 1) * BarGap) div FBarCount;
  if BarW < 2 then BarW := 2;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for I := 0 to FBarCount - 1 do
    begin
      X := Pad + I * (BarW + BarGap);
      H := BarsAreaH * (0.3 + 0.7 * (I + 1) / FBarCount);
      Y := Pad + (BarsAreaH - H);
      if I < FActive then Col := EffectiveAccent
      else                Col := Palette.NeutralLight;
      Brush := TGPSolidBrush.Create(ColorToARGB(Col));
      try
        Graphics.FillRectangle(Brush, X, Y, BarW, H);
      finally
        Brush.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;

  if FCaption <> '' then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := FFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Width - Pad - ACanvas.TextWidth(FCaption),
      (Height - ACanvas.TextHeight(FCaption)) div 2, FCaption);
  end;
end;

{ ---- TOBDBluetoothSignal ------------------------------------------- }

constructor TOBDBluetoothSignal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption  := 'BT';
  FRssiDbm := -100;
  UpdateBars;
end;

procedure TOBDBluetoothSignal.SetRssiDbm(AValue: Integer);
begin
  if FRssiDbm = AValue then Exit;
  FRssiDbm := AValue;
  UpdateBars;
end;

procedure TOBDBluetoothSignal.UpdateBars;
var Bars: Integer;
begin
  // -100..-90 = 0, -90..-80 = 1, -80..-70 = 2, -70..-60 = 3,
  // > -60 = 4
  if FRssiDbm >= -60 then Bars := 4
  else if FRssiDbm >= -70 then Bars := 3
  else if FRssiDbm >= -80 then Bars := 2
  else if FRssiDbm >= -90 then Bars := 1
  else Bars := 0;
  ActiveBars := Bars;
end;

{ ---- TOBDWiFiSignal ------------------------------------------------ }

constructor TOBDWiFiSignal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption  := 'WiFi';
  FRssiDbm := -100;
  UpdateBars;
end;

procedure TOBDWiFiSignal.SetRssiDbm(AValue: Integer);
begin
  if FRssiDbm = AValue then Exit;
  FRssiDbm := AValue;
  UpdateBars;
end;

procedure TOBDWiFiSignal.UpdateBars;
var Bars: Integer;
begin
  if FRssiDbm >= -55 then Bars := 4
  else if FRssiDbm >= -65 then Bars := 3
  else if FRssiDbm >= -75 then Bars := 2
  else if FRssiDbm >= -85 then Bars := 1
  else Bars := 0;
  ActiveBars := Bars;
end;

{ ---- TOBDGPSAccuracy ----------------------------------------------- }

constructor TOBDGPSAccuracy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 140;
  Height := 36;
  FHDOP := 99.9;
  FSatCount := 0;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDGPSAccuracy.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDGPSAccuracy.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDGPSAccuracy.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDGPSAccuracy.SetHDOP(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FHDOP, AValue) then Exit;
  FHDOP := AValue; NotifyBindings; Repaint;
end;

procedure TOBDGPSAccuracy.SetSatCount(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FSatCount = AValue then Exit;
  FSatCount := AValue; NotifyBindings; Repaint;
end;

procedure TOBDGPSAccuracy.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

function TOBDGPSAccuracy.GradeColor: TColor;
begin
  if FHDOP <= 1.0 then Result := Palette.Success
  else if FHDOP <= 2.5 then Result := EffectiveAccent
  else if FHDOP <= 5.0 then Result := Palette.Warning
  else Result := Palette.Danger;
end;

function TOBDGPSAccuracy.GradeText: string;
begin
  if FHDOP <= 1.0 then Result := 'excellent'
  else if FHDOP <= 2.5 then Result := 'good'
  else if FHDOP <= 5.0 then Result := 'fair'
  else if FHDOP <= 10.0 then Result := 'poor'
  else Result := 'no fix';
end;

procedure TOBDGPSAccuracy.PaintControl(ACanvas: TCanvas);
var
  Pad: Integer;
  S: string;
begin
  Pad := ScaleValue(8);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveForeground;
  S := Format('GPS  HDOP %.1f  sats %d',
    [FHDOP, FSatCount]);
  ACanvas.TextOut(Pad, Pad, S);
  ACanvas.Font.Color := GradeColor;
  ACanvas.TextOut(Pad,
    Pad + ACanvas.TextHeight('Mg') + ScaleValue(2),
    GradeText);
end;

end.
