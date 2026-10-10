//------------------------------------------------------------------------------
//  ERD.UI.Gauges.Bar
//
//  TOBDBarGauge - horizontal or vertical bar for one channel.
//
//  Covers the channels a workshop reads as "how full": throttle
//  position, engine load, fuel level, battery voltage. CentreZero
//  fills from zero towards the value, which is how short- and
//  long-term fuel trims are read (lean to the right, rich to the
//  left).
//
//  Layout:
//    horizontal  caption top-left, large readout top-right, the bar
//                underneath with alert bands, min / max labels below;
//    vertical    caption on top, readout below it, the bar filling
//                the rest, min / max labels beside the bar.
//  Without data the bar is empty and shows "NO DATA"; stale values
//  are greyed; out-of-range values pin to the end and say OVER /
//  UNDER.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.Gauges.Bar;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Units,
  ERD.UI.Gauges.Types,
  ERD.UI.Gauges.Base;

type
  /// <summary>Direction of a bar gauge.</summary>
  TOBDBarOrientation = (
    /// <summary>Fills left to right.</summary>
    bgoHorizontal,
    /// <summary>Fills bottom to top.</summary>
    bgoVertical);

  /// <summary>Bar gauge bound to one data channel.</summary>
  /// <remarks>Defaults to throttle position (PID <c>$11</c>,
  /// 0..100 percent). For fuel trims use PID <c>$06</c>..<c>$09</c>,
  /// <c>Min = -25</c>, <c>Max = 25</c> and
  /// <c>CentreZero = True</c>.</remarks>
  TOBDBarGauge = class(TOBDGaugeBase)
  strict private
    FOrientation: TOBDBarOrientation;
    FCentreZero: Boolean;
    FShowReadout: Boolean;
    procedure SetOrientation(AValue: TOBDBarOrientation);
    procedure SetCentreZero(AValue: Boolean);
    procedure SetShowReadout(AValue: Boolean);
    function Baseline: Double;
    procedure PaintBar(ACanvas: TCanvas; const ABar: TRect);
    procedure PaintHorizontal(ACanvas: TCanvas);
    procedure PaintVertical(ACanvas: TCanvas);
  protected
    /// <summary>Paints the bar in the selected orientation.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Preview value: two thirds of the way from the
    /// baseline to the maximum.</summary>
    /// <returns>Preview value in the metric unit.</returns>
    function PreviewValue: Double; override;
  public
    /// <summary>Creates a throttle-position bar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Adds orientation and centre-zero to the base settings.
    /// </summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Restores base settings, orientation and centre-zero.
    /// </summary>
    /// <param name="AObject">Source object.</param>
    procedure LoadSettings(AObject: TJSONObject); override;
  published
    /// <summary>Horizontal or vertical.</summary>
    property Orientation: TOBDBarOrientation read FOrientation
      write SetOrientation default bgoHorizontal;
    /// <summary>Fill from zero (clamped to the range) instead of from
    /// <c>Min</c>. Use for signed channels such as fuel trims.
    /// </summary>
    property CentreZero: Boolean read FCentreZero write SetCentreZero
      default False;
    /// <summary>Shows the numeric readout next to the caption.
    /// </summary>
    property ShowReadout: Boolean read FShowReadout write SetShowReadout
      default True;
  end;

implementation

constructor TOBDBarGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 240;
  Height := 84;
  FOrientation := bgoHorizontal;
  FCentreZero := False;
  FShowReadout := True;
  Caption := 'Throttle';
  &Unit := '%';
  Min := 0;
  Max := 100;
  Channel.PID := $11;
end;

procedure TOBDBarGauge.SetOrientation(AValue: TOBDBarOrientation);
begin
  if FOrientation = AValue then
    Exit;
  FOrientation := AValue;
  Invalidate;
end;

procedure TOBDBarGauge.SetCentreZero(AValue: Boolean);
begin
  if FCentreZero = AValue then
    Exit;
  FCentreZero := AValue;
  Invalidate;
end;

procedure TOBDBarGauge.SetShowReadout(AValue: Boolean);
begin
  if FShowReadout = AValue then
    Exit;
  FShowReadout := AValue;
  Invalidate;
end;

function TOBDBarGauge.Baseline: Double;
begin
  if FCentreZero then
    Result := Clamp(0)
  else
    Result := Min;
end;

function TOBDBarGauge.PreviewValue: Double;
begin
  Result := Baseline + (Max - Baseline) * 0.66;
end;

procedure TOBDBarGauge.PaintBar(ACanvas: TCanvas; const ABar: TRect);
var
  Zone: TOBDGaugeZone;
  F0, F1: Double;
  R, Fill: TRect;
  Strip: Integer;
  S: string;

  function At(AFraction: Double): Integer;
  begin
    if FOrientation = bgoHorizontal then
      Result := ABar.Left + Round(AFraction * ABar.Width)
    else
      Result := ABar.Bottom - Round(AFraction * ABar.Height);
  end;

begin
  // Track.
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Palette.GaugeFace;
  ACanvas.Pen.Color := EffectiveBorder;
  ACanvas.Pen.Width := 1;
  ACanvas.Rectangle(ABar);

  // Alert bands as a thin strip along the track edge.
  Strip := System.Math.Max(3, ScaleValue(4));
  for Zone in EffectiveZones do
  begin
    F0 := NormaliseValue(Min, Max, Zone.StartValue);
    F1 := NormaliseValue(Min, Max, Zone.EndValue);
    if F1 <= F0 then
      Continue;
    if FOrientation = bgoHorizontal then
      R := Rect(At(F0), ABar.Bottom - Strip, At(F1), ABar.Bottom)
    else
      R := Rect(ABar.Right - Strip, At(F1), ABar.Right, At(F0));
    ACanvas.Brush.Color := Zone.Color;
    ACanvas.FillRect(R);
  end;

  // Fill from the baseline to the value.
  if DataState <> dstNoData then
  begin
    F0 := NormaliseValue(Min, Max, Baseline);
    F1 := PaintFraction;
    if FOrientation = bgoHorizontal then
      Fill := Rect(At(System.Math.Min(F0, F1)), ABar.Top + 1,
        At(System.Math.Max(F0, F1)), ABar.Bottom - Strip)
    else
      Fill := Rect(ABar.Left + 1, At(System.Math.Max(F0, F1)),
        ABar.Right - Strip, At(System.Math.Min(F0, F1)));
    if (Fill.Width > 0) and (Fill.Height > 0) then
    begin
      ACanvas.Brush.Color := ValueColor(EffectiveAccent);
      ACanvas.FillRect(Fill);
    end;
  end;

  // Zero marker for centre-zero bars.
  if FCentreZero and (Min < 0) and (Max > 0) then
  begin
    ACanvas.Pen.Color := EffectiveForeground;
    ACanvas.Pen.Width := System.Math.Max(1, ScaleValue(2));
    F0 := NormaliseValue(Min, Max, 0);
    if FOrientation = bgoHorizontal then
    begin
      ACanvas.MoveTo(At(F0), ABar.Top);
      ACanvas.LineTo(At(F0), ABar.Bottom);
    end
    else
    begin
      ACanvas.MoveTo(ABar.Left, At(F0));
      ACanvas.LineTo(ABar.Right, At(F0));
    end;
    ACanvas.Pen.Width := 1;
  end;

  // State text over the track when there is nothing live to show.
  S := StateText;
  if S <> '' then
  begin
    ACanvas.Font.Style := [fsBold];
    if DataState = dstOutOfRange then
      ACanvas.Font.Color := Palette.Danger
    else
      ACanvas.Font.Color := Palette.Subtle;
    FitFont(ACanvas, S, ABar.Width - ScaleValue(4),
      System.Math.Max(8, ABar.Height - ScaleValue(4)));
    DrawCentred(ACanvas, ABar, S);
  end;
end;

procedure TOBDBarGauge.PaintHorizontal(ACanvas: TCanvas);
var
  Pad, HeadH, LabelH: Integer;
  Head, Bar, R: TRect;
  S: string;
begin
  Pad := ScaleValue(6);
  HeadH := System.Math.Max(ScaleValue(16), Round(Height * 0.42));
  LabelH := System.Math.Max(ScaleValue(12), Round(Height * 0.16));
  Head := Rect(Pad, Pad, Width - Pad, Pad + HeadH);
  Bar := Rect(Pad, Head.Bottom + ScaleValue(2), Width - Pad,
    Height - Pad - LabelH);
  if Bar.Height < ScaleValue(6) then
    Bar.Bottom := Bar.Top + ScaleValue(6);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Name := Font.Name;

  // Readout (right) first, so the caption can use what is left.
  R := Head;
  if FShowReadout then
  begin
    S := ReadoutText;
    if DisplayUnit <> '' then
      S := S + ' ' + DisplayUnit;
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Color := ValueColor(EffectiveForeground);
    FitFont(ACanvas, S, Head.Width div 2, Head.Height);
    ACanvas.TextOut(Head.Right - ACanvas.TextWidth(S),
      Head.Top + (Head.Height - ACanvas.TextHeight(S)) div 2, S);
    R.Right := Head.Right - ACanvas.TextWidth(S) - Pad;
  end;
  if Caption <> '' then
  begin
    ACanvas.Font.Style := [];
    ACanvas.Font.Color := Palette.GaugeLabel;
    FitFont(ACanvas, Caption, System.Math.Max(10, R.Width),
      System.Math.Max(8, Round(Head.Height * 0.7)));
    ACanvas.TextOut(R.Left, R.Top + (R.Height - ACanvas.TextHeight(Caption))
      div 2, Caption);
  end;

  PaintBar(ACanvas, Bar);

  // Min / max labels under the bar.
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := Palette.GaugeLabel;
  ACanvas.Font.Height := -LabelH;
  S := FormatDisplay(Min);
  ACanvas.TextOut(Bar.Left, Bar.Bottom + 1, S);
  S := FormatDisplay(Max);
  ACanvas.TextOut(Bar.Right - ACanvas.TextWidth(S), Bar.Bottom + 1, S);
end;

procedure TOBDBarGauge.PaintVertical(ACanvas: TCanvas);
var
  Pad, CapH, ReadH, LabelW: Integer;
  Cap, ReadBox, Bar: TRect;
  S: string;
begin
  Pad := ScaleValue(6);
  CapH := System.Math.Max(ScaleValue(14), Round(Height * 0.09));
  ReadH := 0;
  if FShowReadout then
    ReadH := System.Math.Max(ScaleValue(18), Round(Height * 0.13));
  Cap := Rect(Pad, Pad, Width - Pad, Pad + CapH);
  ReadBox := Rect(Pad, Cap.Bottom, Width - Pad, Cap.Bottom + ReadH);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Name := Font.Name;
  if Caption <> '' then
  begin
    ACanvas.Font.Style := [];
    ACanvas.Font.Color := Palette.GaugeLabel;
    FitFont(ACanvas, Caption, Cap.Width, Cap.Height);
    DrawCentred(ACanvas, Cap, Caption);
  end;
  if FShowReadout then
  begin
    S := ReadoutText;
    if DisplayUnit <> '' then
      S := S + ' ' + DisplayUnit;
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Color := ValueColor(EffectiveForeground);
    FitFont(ACanvas, S, ReadBox.Width, ReadBox.Height);
    DrawCentred(ACanvas, ReadBox, S);
  end;

  // Bar centred, labels on its left.
  ACanvas.Font.Style := [];
  ACanvas.Font.Height := -System.Math.Max(ScaleValue(10), CapH - 2);
  LabelW := System.Math.Max(ACanvas.TextWidth(FormatDisplay(Min)),
    ACanvas.TextWidth(FormatDisplay(Max))) + ScaleValue(4);
  Bar := Rect(Pad + LabelW, ReadBox.Bottom + ScaleValue(4),
    Width - Pad, Height - Pad);
  if Bar.Width > ScaleValue(60) then
  begin
    Bar.Left := Bar.Left + (Bar.Width - ScaleValue(60)) div 2;
    Bar.Right := Bar.Left + ScaleValue(60);
  end;
  if Bar.Height < ScaleValue(10) then
    Bar.Top := Bar.Bottom - ScaleValue(10);

  PaintBar(ACanvas, Bar);

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := Palette.GaugeLabel;
  S := FormatDisplay(Max);
  ACanvas.TextOut(Bar.Left - ACanvas.TextWidth(S) - ScaleValue(3), Bar.Top, S);
  S := FormatDisplay(Min);
  ACanvas.TextOut(Bar.Left - ACanvas.TextWidth(S) - ScaleValue(3),
    Bar.Bottom - ACanvas.TextHeight(S), S);
end;

procedure TOBDBarGauge.PaintControl(ACanvas: TCanvas);
begin
  if (Width < ScaleValue(20)) or (Height < ScaleValue(20)) then
    Exit;
  if FOrientation = bgoHorizontal then
    PaintHorizontal(ACanvas)
  else
    PaintVertical(ACanvas);
end;

procedure TOBDBarGauge.SaveSettings(AObject: TJSONObject);
begin
  inherited SaveSettings(AObject);
  OBDJsonWriteBool(AObject, 'vertical', FOrientation = bgoVertical);
  OBDJsonWriteBool(AObject, 'centreZero', FCentreZero);
end;

procedure TOBDBarGauge.LoadSettings(AObject: TJSONObject);
var
  B: Boolean;
begin
  inherited LoadSettings(AObject);
  B := FOrientation = bgoVertical;
  if OBDJsonReadBool(AObject, 'vertical', B) then
  begin
    if B then
      Orientation := bgoVertical
    else
      Orientation := bgoHorizontal;
  end;
  B := FCentreZero;
  if OBDJsonReadBool(AObject, 'centreZero', B) then
    CentreZero := B;
end;

end.
