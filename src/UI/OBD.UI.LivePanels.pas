//------------------------------------------------------------------------------
//  OBD.UI.LivePanels
//
//  Card-style live-data visuals:
//
//    TOBDPidPanel        single-PID card. Caption (top), big
//                        numeric value + unit (centre), tiny
//                        spark trace (right). Auto-binds to
//                        TOBDLiveData + PID just like the
//                        gauge family.
//    TOBDFuelTrimDisplay four-bar fuel-trim widget. Shows
//                        STFT / LTFT for bank 1 + bank 2 with
//                        healthy (-10..+10), lean (>10), and
//                        rich (&lt; -10) coloured zones.
//
//  Theme- / HiDPI- / VCL-Style-aware via TOBDCustomControl.
//  Every state mutation calls TBindings.Notify so a host that
//  wired a LiveBinding refreshes automatically.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.LivePanels;

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
  OBD.Types,
  OBD.Service.LiveData;

type
  /// <summary>Single-PID live-data card. Drop on a form, point
  /// <c>LiveData</c> at the service component and set
  /// <c>PID</c> — the panel auto-subscribes, formats the value
  /// using <c>Decimals</c> + <c>Unit</c>, and builds a small
  /// sparkline of the last <c>SparkCapacity</c> samples.
  /// Hosts that drive the value directly (no LiveData)
  /// assign <c>Value</c> from their own glue and leave the
  /// binding properties unset.</summary>
  TOBDPidPanel = class(TOBDCustomControl)
  strict private
    FCaption:       string;
    FValue:         Double;
    FUnit:          string;
    FDecimals:      Byte;
    FLiveData:      TOBDLiveData;
    FPID:           Byte;
    FShowSpark:     Boolean;
    FSparkBuffer:   TArray<Double>;
    FSparkCount:    Integer;
    FSparkCapacity: Integer;
    FSparkNext:     Integer;
    FCaptionFont:   TFont;
    FValueFont:     TFont;
    procedure SetCaption(const AValue: string);
    procedure SetValue(AValue: Double);
    procedure SetUnit(const AValue: string);
    procedure SetDecimals(AValue: Byte);
    procedure SetLiveData(AValue: TOBDLiveData);
    procedure SetPID(AValue: Byte);
    procedure SetShowSpark(AValue: Boolean);
    procedure SetSparkCapacity(AValue: Integer);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure HandleLiveValue(Sender: TObject;
      const AValue: TOBDPIDValue);
    procedure SubscribeToLiveData;
    procedure UnsubscribeFromLiveData;
    procedure PushSparkSample(AValue: Double);
    procedure NotifyBindings;
    function  FormatValue: string;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Clears the sparkline trace.</summary>
    procedure ResetSpark;
  published
    /// <summary>PID label shown at the top of the card.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Current value. Drives the big readout + spark.
    /// </summary>
    property Value: Double read FValue write SetValue;
    /// <summary>Unit suffix (e.g. "°C", "rpm").</summary>
    property &Unit: string read FUnit write SetUnit;
    /// <summary>Decimal places on the displayed value.
    /// Default 0.</summary>
    property Decimals: Byte read FDecimals write SetDecimals
      default 0;
    /// <summary>Source service. When set together with
    /// <see cref="PID"/> the panel subscribes itself.</summary>
    property LiveData: TOBDLiveData
      read FLiveData write SetLiveData;
    /// <summary>PID the panel subscribes to on
    /// <see cref="LiveData"/>.</summary>
    property PID: Byte read FPID write SetPID;
    /// <summary>Show the inline spark on the right of the
    /// card. Default True.</summary>
    property ShowSpark: Boolean
      read FShowSpark write SetShowSpark default True;
    /// <summary>Ring-buffer capacity for the spark.
    /// Default 32.</summary>
    property SparkCapacity: Integer
      read FSparkCapacity write SetSparkCapacity default 32;
    /// <summary>Caption font. Falls through to component
    /// <c>Font</c> when not set.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Big numeric-value font.</summary>
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>Four-bar fuel-trim display covering bank 1 +
  /// bank 2 STFT and LTFT. Each bar is centre-zero so the
  /// sign of the trim is obvious at a glance; coloured zones
  /// flag healthy (-10..+10), lean (&gt;+10, amber), and
  /// rich (&lt; -10, red) regions.</summary>
  TOBDFuelTrimDisplay = class(TOBDCustomControl)
  strict private
    FSTFT1: Double;
    FLTFT1: Double;
    FSTFT2: Double;
    FLTFT2: Double;
    FHealthyLimit: Double;
    FRange:        Double;
    FShowBank2:    Boolean;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    procedure SetSTFT1(AValue: Double);
    procedure SetLTFT1(AValue: Double);
    procedure SetSTFT2(AValue: Double);
    procedure SetLTFT2(AValue: Double);
    procedure SetHealthyLimit(AValue: Double);
    procedure SetRange(AValue: Double);
    procedure SetShowBank2(AValue: Boolean);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure PaintBar(ACanvas: TCanvas; const ARect: TRect;
      const ALabel: string; AValue: Double);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Short-term fuel trim, bank 1 (%). Range
    /// <c>-Range..+Range</c>; values outside clamp to edges.
    /// </summary>
    property STFTBank1: Double read FSTFT1 write SetSTFT1;
    /// <summary>Long-term fuel trim, bank 1 (%).</summary>
    property LTFTBank1: Double read FLTFT1 write SetLTFT1;
    /// <summary>Short-term fuel trim, bank 2 (%). Ignored when
    /// <see cref="ShowBank2"/> is False.</summary>
    property STFTBank2: Double read FSTFT2 write SetSTFT2;
    /// <summary>Long-term fuel trim, bank 2 (%).</summary>
    property LTFTBank2: Double read FLTFT2 write SetLTFT2;
    /// <summary>Magnitude of the healthy band. Trims with
    /// <c>Abs(Value) &lt;= HealthyLimit</c> paint green.
    /// Default 10 (%).</summary>
    property HealthyLimit: Double
      read FHealthyLimit write SetHealthyLimit;
    /// <summary>Magnitude of the visible scale. Trims clamp at
    /// <c>±Range</c>. Default 25 (%).</summary>
    property Range: Double read FRange write SetRange;
    /// <summary>Show the bank-2 row. Default True. Single-bank
    /// engines should set this False to halve the height.
    /// </summary>
    property ShowBank2: Boolean
      read FShowBank2 write SetShowBank2 default True;
    /// <summary>Caption font (bank / direction labels).</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Value font (numeric readout under each bar).
    /// </summary>
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

implementation

{ ---- TOBDPidPanel -------------------------------------------------------- }

constructor TOBDPidPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 160;
  Height := 60;
  FCaption  := '';
  FValue    := 0;
  FUnit     := '';
  FDecimals := 0;
  FShowSpark := True;
  FSparkCapacity := 32;
  SetLength(FSparkBuffer, FSparkCapacity);
  FSparkCount := 0;
  FSparkNext  := 0;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 18;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDPidPanel.Destroy;
begin
  UnsubscribeFromLiveData;
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDPidPanel.Loaded;
begin
  inherited;
  SubscribeToLiveData;
end;

procedure TOBDPidPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLiveData) then
    FLiveData := nil;
end;

procedure TOBDPidPanel.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDPidPanel.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDPidPanel.SetCaption(const AValue: string);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue; Repaint;
end;

procedure TOBDPidPanel.SetValue(AValue: Double);
begin
  if SameValue(FValue, AValue) then Exit;
  FValue := AValue;
  PushSparkSample(AValue);
  NotifyBindings;
  Repaint;
end;

procedure TOBDPidPanel.SetUnit(const AValue: string);
begin
  if FUnit = AValue then Exit;
  FUnit := AValue; Repaint;
end;

procedure TOBDPidPanel.SetDecimals(AValue: Byte);
begin
  if FDecimals = AValue then Exit;
  FDecimals := AValue; Repaint;
end;

procedure TOBDPidPanel.SetLiveData(AValue: TOBDLiveData);
begin
  if FLiveData = AValue then Exit;
  UnsubscribeFromLiveData;
  if FLiveData <> nil then FLiveData.RemoveFreeNotification(Self);
  FLiveData := AValue;
  if FLiveData <> nil then FLiveData.FreeNotification(Self);
  SubscribeToLiveData;
end;

procedure TOBDPidPanel.SetPID(AValue: Byte);
begin
  if FPID = AValue then Exit;
  UnsubscribeFromLiveData;
  FPID := AValue;
  SubscribeToLiveData;
end;

procedure TOBDPidPanel.SetShowSpark(AValue: Boolean);
begin
  if FShowSpark = AValue then Exit;
  FShowSpark := AValue; Repaint;
end;

procedure TOBDPidPanel.SetSparkCapacity(AValue: Integer);
begin
  if AValue < 4 then AValue := 4;
  if AValue > 1024 then AValue := 1024;
  if FSparkCapacity = AValue then Exit;
  FSparkCapacity := AValue;
  SetLength(FSparkBuffer, FSparkCapacity);
  FSparkCount := 0;
  FSparkNext  := 0;
  Repaint;
end;

procedure TOBDPidPanel.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDPidPanel.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDPidPanel.SubscribeToLiveData;
begin
  if (FLiveData <> nil) and not (csDesigning in ComponentState) then
    FLiveData.Subscribe(FPID, HandleLiveValue);
end;

procedure TOBDPidPanel.UnsubscribeFromLiveData;
begin
  if FLiveData <> nil then
    FLiveData.Unsubscribe(FPID, HandleLiveValue);
end;

procedure TOBDPidPanel.HandleLiveValue(Sender: TObject;
  const AValue: TOBDPIDValue);
begin
  // TOBDLiveData dispatches on the main thread; safe to mutate
  // VCL state directly.
  if not IsNan(AValue.Value) then
    Value := AValue.Value;
  if (AValue.Unit_ <> '') and (FUnit = '') then
    SetUnit(AValue.Unit_);
end;

procedure TOBDPidPanel.PushSparkSample(AValue: Double);
begin
  if FSparkCapacity <= 0 then Exit;
  FSparkBuffer[FSparkNext] := AValue;
  FSparkNext := (FSparkNext + 1) mod FSparkCapacity;
  if FSparkCount < FSparkCapacity then Inc(FSparkCount);
end;

procedure TOBDPidPanel.ResetSpark;
begin
  FSparkCount := 0;
  FSparkNext  := 0;
  Repaint;
end;

function TOBDPidPanel.FormatValue: string;
var FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  if FDecimals = 0 then
    Result := FormatFloat('0', FValue, FS)
  else
    Result := FormatFloat('0.' + StringOfChar('0', FDecimals),
      FValue, FS);
  if FUnit <> '' then
    Result := Result + ' ' + FUnit;
end;

procedure TOBDPidPanel.PaintControl(ACanvas: TCanvas);
var
  R: TRect;
  Pad: Integer;
  SparkW: Integer;
  ValueStr: string;
  ValueX, ValueY: Integer;
  // Sparkline locals.
  Graphics: TGPGraphics;
  Pen: TGPPen;
  I, Idx, Start: Integer;
  Points: array of TGPPointF;
  SMin, SMax, Span, V: Double;
  PlotX, PlotY, PlotW, PlotH: Integer;
begin
  R   := ClientRect;
  Pad := ScaleValue(8);
  if FShowSpark and (FSparkCount >= 2) then
    SparkW := System.Math.Min(R.Width div 3, ScaleValue(80))
  else
    SparkW := 0;

  ACanvas.Brush.Style := bsClear;

  // Caption.
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  if FCaption <> '' then
    ACanvas.TextOut(Pad, ScaleValue(4), FCaption);

  // Big value.
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveAccent;
  ValueStr := FormatValue;
  ValueX := Pad;
  ValueY := ScaleValue(4) + ACanvas.TextHeight('Mg') + ScaleValue(2);
  ACanvas.TextOut(ValueX, ValueY, ValueStr);

  // Spark (right edge).
  if (SparkW > 0) and (FSparkCount >= 2) then
  begin
    PlotX := R.Width  - SparkW - Pad;
    PlotY := Pad + ScaleValue(2);
    PlotW := SparkW;
    PlotH := R.Height - PlotY - Pad;

    if FSparkCount < FSparkCapacity then Start := 0
    else                                  Start := FSparkNext;
    // Find range.
    Idx := (Start) mod FSparkCapacity;
    SMin := FSparkBuffer[Idx];
    SMax := SMin;
    for I := 1 to FSparkCount - 1 do
    begin
      Idx := (Start + I) mod FSparkCapacity;
      V := FSparkBuffer[Idx];
      if V < SMin then SMin := V;
      if V > SMax then SMax := V;
    end;
    if SameValue(SMin, SMax) then
    begin
      SMin := SMin - 0.5;
      SMax := SMax + 0.5;
    end;
    Span := SMax - SMin;

    SetLength(Points, FSparkCount);
    for I := 0 to FSparkCount - 1 do
    begin
      Idx := (Start + I) mod FSparkCapacity;
      Points[I].X := PlotX + I / (FSparkCount - 1) * PlotW;
      Points[I].Y := PlotY + PlotH -
        Single((FSparkBuffer[Idx] - SMin) / Span) * PlotH;
    end;

    Graphics := TGPGraphics.Create(ACanvas.Handle);
    Pen := TGPPen.Create(ColorToARGB(EffectiveAccent),
      ScaleValue(2));
    Pen.SetStartCap(LineCapRound);
    Pen.SetEndCap(LineCapRound);
    Pen.SetLineJoin(LineJoinRound);
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      Graphics.DrawLines(Pen, PGPPointF(@Points[0]), FSparkCount);
    finally
      Pen.Free;
      Graphics.Free;
    end;
  end;
end;

{ ---- TOBDFuelTrimDisplay ------------------------------------------------- }

constructor TOBDFuelTrimDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 140;
  FSTFT1 := 0;
  FLTFT1 := 0;
  FSTFT2 := 0;
  FLTFT2 := 0;
  FHealthyLimit := 10;
  FRange := 25;
  FShowBank2 := True;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 10;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDFuelTrimDisplay.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDFuelTrimDisplay.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDFuelTrimDisplay.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDFuelTrimDisplay.SetSTFT1(AValue: Double);
begin
  if SameValue(FSTFT1, AValue) then Exit;
  FSTFT1 := AValue; NotifyBindings; Repaint;
end;

procedure TOBDFuelTrimDisplay.SetLTFT1(AValue: Double);
begin
  if SameValue(FLTFT1, AValue) then Exit;
  FLTFT1 := AValue; NotifyBindings; Repaint;
end;

procedure TOBDFuelTrimDisplay.SetSTFT2(AValue: Double);
begin
  if SameValue(FSTFT2, AValue) then Exit;
  FSTFT2 := AValue; NotifyBindings; Repaint;
end;

procedure TOBDFuelTrimDisplay.SetLTFT2(AValue: Double);
begin
  if SameValue(FLTFT2, AValue) then Exit;
  FLTFT2 := AValue; NotifyBindings; Repaint;
end;

procedure TOBDFuelTrimDisplay.SetHealthyLimit(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FHealthyLimit, AValue) then Exit;
  FHealthyLimit := AValue; Repaint;
end;

procedure TOBDFuelTrimDisplay.SetRange(AValue: Double);
begin
  if AValue < 1 then AValue := 1;
  if SameValue(FRange, AValue) then Exit;
  FRange := AValue; Repaint;
end;

procedure TOBDFuelTrimDisplay.SetShowBank2(AValue: Boolean);
begin
  if FShowBank2 = AValue then Exit;
  FShowBank2 := AValue; Repaint;
end;

procedure TOBDFuelTrimDisplay.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDFuelTrimDisplay.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDFuelTrimDisplay.PaintBar(ACanvas: TCanvas;
  const ARect: TRect; const ALabel: string; AValue: Double);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Track, Fill, Healthy, Lean, Rich: TGPRectF;
  CenterX: Single;
  Clamped, Norm: Double;
  ValStr: string;
  ValW: Integer;
  LabelW: Integer;
  Col: TColor;
  TrackTop, TrackH: Integer;
  PadX: Integer;
begin
  // Layout: small label on the left (~40 px), bar fills middle,
  // numeric readout on the right (~50 px). Bar is centre-zero
  // horizontally.
  PadX := ScaleValue(4);
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  LabelW := System.Math.Max(ScaleValue(40),
    ACanvas.TextWidth(ALabel) + ScaleValue(4));

  TrackTop := ARect.Top + (ARect.Height - ScaleValue(14)) div 2;
  TrackH   := ScaleValue(14);

  ACanvas.TextOut(
    ARect.Left + PadX,
    ARect.Top + (ARect.Height - ACanvas.TextHeight('M')) div 2,
    ALabel);

  Track.X := ARect.Left + LabelW + PadX;
  Track.Y := TrackTop;
  Track.Width  := ARect.Right - Track.X - ScaleValue(60) - PadX;
  Track.Height := TrackH;
  if Track.Width < 1 then Exit;

  CenterX := Track.X + Track.Width / 2;

  // Clamp to ±Range.
  Clamped := AValue;
  if Clamped >  FRange then Clamped :=  FRange;
  if Clamped < -FRange then Clamped := -FRange;
  Norm := Clamped / FRange;       // -1..+1

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Zone backgrounds (rich-left, healthy-middle, lean-right).
    Rich.X := Track.X;
    Rich.Y := Track.Y;
    Rich.Width  := Track.Width / 2 *
      (1.0 - FHealthyLimit / FRange);
    Rich.Height := Track.Height;

    Healthy.X := Rich.X + Rich.Width;
    Healthy.Y := Track.Y;
    Healthy.Width  := Track.Width *
      (FHealthyLimit / FRange);
    Healthy.Height := Track.Height;

    Lean.X := Healthy.X + Healthy.Width;
    Lean.Y := Track.Y;
    Lean.Width  := Track.Width / 2 *
      (1.0 - FHealthyLimit / FRange);
    Lean.Height := Track.Height;

    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Danger, 64));
    try
      Graphics.FillRectangle(Brush, Rich);
    finally
      Brush.Free;
    end;
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Success, 64));
    try
      Graphics.FillRectangle(Brush, Healthy);
    finally
      Brush.Free;
    end;
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Warning, 64));
    try
      Graphics.FillRectangle(Brush, Lean);
    finally
      Brush.Free;
    end;

    // Centre-zero fill bar.
    if Abs(Clamped) <= FHealthyLimit then
      Col := Palette.Success
    else if Clamped > 0 then
      Col := Palette.Warning
    else
      Col := Palette.Danger;

    if Norm >= 0 then
    begin
      Fill.X := CenterX;
      Fill.Y := Track.Y;
      Fill.Width  := Single(Norm) * Track.Width / 2;
      Fill.Height := Track.Height;
    end
    else
    begin
      Fill.X := CenterX + Single(Norm) * Track.Width / 2;
      Fill.Y := Track.Y;
      Fill.Width  := -Single(Norm) * Track.Width / 2;
      Fill.Height := Track.Height;
    end;

    Brush := TGPSolidBrush.Create(ColorToARGB(Col));
    try
      Graphics.FillRectangle(Brush, Fill);
    finally
      Brush.Free;
    end;

    // Centre line.
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawLine(Pen,
        CenterX, Track.Y - 1,
        CenterX, Track.Y + Track.Height + 1);
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

  // Numeric readout.
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveForeground;
  ValStr := Format('%+.1f %%', [AValue]);
  ValW := ACanvas.TextWidth(ValStr);
  ACanvas.TextOut(
    ARect.Right - ValW - PadX,
    ARect.Top + (ARect.Height - ACanvas.TextHeight(ValStr)) div 2,
    ValStr);
end;

procedure TOBDFuelTrimDisplay.PaintControl(ACanvas: TCanvas);
var
  R, Row: TRect;
  RowCount, RowH: Integer;
begin
  R := ClientRect;
  ACanvas.Brush.Style := bsClear;
  if FShowBank2 then RowCount := 4 else RowCount := 2;
  RowH := R.Height div RowCount;

  Row := R;
  Row.Bottom := Row.Top + RowH;
  PaintBar(ACanvas, Row, 'STFT B1', FSTFT1);
  Row.Top := Row.Bottom; Row.Bottom := Row.Top + RowH;
  PaintBar(ACanvas, Row, 'LTFT B1', FLTFT1);
  if FShowBank2 then
  begin
    Row.Top := Row.Bottom; Row.Bottom := Row.Top + RowH;
    PaintBar(ACanvas, Row, 'STFT B2', FSTFT2);
    Row.Top := Row.Bottom; Row.Bottom := Row.Top + RowH;
    PaintBar(ACanvas, Row, 'LTFT B2', FLTFT2);
  end;
end;

end.
