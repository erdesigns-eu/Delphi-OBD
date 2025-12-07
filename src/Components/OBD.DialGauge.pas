//------------------------------------------------------------------------------
// UNIT           : OBD.DialGauge.pas
// CONTENTS       : Dial/Speedometer gauge with digital display
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.DialGauge;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Diagnostics,
  Vcl.Controls, WinApi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Themes,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Animation,
  OBD.CustomControl.AnimationManager;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  DEFAULT_MIN = 0;
  DEFAULT_MAX = 200;
  DEFAULT_REDLINE = 160;
  DEFAULT_NEEDLE_COLOR = $00DF7000;
  DEFAULT_DIGITAL_COLOR = $0000FF00;
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 800;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Dial Gauge - Automotive-style speedometer/tachometer with digital display
  /// </summary>
  TOBDDialGauge = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FRedLine: Single;
    FShowRedLine: Boolean;
    FDigitalDisplay: Boolean;
    FDigitalColor: TColor;
    FDigitalFormat: string;
    FNeedleColor: TColor;
    FAnimationEnabled: Boolean;
    FAnimationDuration: Cardinal;
    FAnimationType: TOBDCustomControlAnimationType;
    FAnimationValue: Single;
    FAnimationStartValue: Single;
    FUnit: string;
    FShowMajorTicks: Boolean;
    FShowMinorTicks: Boolean;
    FMajorTickStep: Single;
    FMinorTickStep: Single;
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetValue(Value: Single);
    procedure SetRedLine(Value: Single);
    procedure SetShowRedLine(Value: Boolean);
    procedure SetDigitalDisplay(Value: Boolean);
    procedure SetDigitalColor(Value: TColor);
    procedure SetDigitalFormat(Value: string);
    procedure SetNeedleColor(Value: TColor);
    procedure SetUnit(Value: string);
    procedure SetShowMajorTicks(Value: Boolean);
    procedure SetShowMinorTicks(Value: Boolean);
    procedure SetMajorTickStep(Value: Single);
    procedure SetMinorTickStep(Value: Single);
  protected
    procedure InvalidateBackground; virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // IOBDAnimatable interface
    procedure AnimationTick(ElapsedMs: Int64);
    function IsAnimating: Boolean;
    function GetFramesPerSecond: Integer;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property RedLine: Single read FRedLine write SetRedLine;
    property ShowRedLine: Boolean read FShowRedLine write SetShowRedLine default True;
    property DigitalDisplay: Boolean read FDigitalDisplay write SetDigitalDisplay default True;
    property DigitalColor: TColor read FDigitalColor write SetDigitalColor default DEFAULT_DIGITAL_COLOR;
    property DigitalFormat: string read FDigitalFormat write SetDigitalFormat;
    property NeedleColor: TColor read FNeedleColor write SetNeedleColor default DEFAULT_NEEDLE_COLOR;
    property &Unit: string read FUnit write SetUnit;
    property ShowMajorTicks: Boolean read FShowMajorTicks write SetShowMajorTicks default True;
    property ShowMinorTicks: Boolean read FShowMinorTicks write SetShowMinorTicks default True;
    property MajorTickStep: Single read FMajorTickStep write SetMajorTickStep;
    property MinorTickStep: Single read FMinorTickStep write SetMinorTickStep;
    property AnimationEnabled: Boolean read FAnimationEnabled write FAnimationEnabled default DEFAULT_ANIMATION_ENABLED;
    property AnimationDuration: Cardinal read FAnimationDuration write FAnimationDuration default DEFAULT_ANIMATION_DURATION;
  end;

implementation

uses
  System.Math;

procedure TOBDDialGauge.SetMin(Value: Single);
begin
  if Value > FMax then Value := FMax;
  if (FMin <> Value) then
  begin
    FMin := Value;
    if FValue < FMin then FValue := FMin;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetMax(Value: Single);
begin
  if Value < FMin then Value := FMin;
  if (FMax <> Value) then
  begin
    FMax := Value;
    if FValue > FMax then FValue := FMax;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetValue(Value: Single);
begin
  if (FValue <> Value) then
  begin
    if (Value < FMin) then Value := FMin;
    if (Value > FMax) then Value := FMax;
    FValue := Value;
    
    if FAnimationEnabled and not (csDesigning in ComponentState) then
    begin
      FAnimationStartValue := FAnimationValue;
      FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
      AnimationManager.CheckAnimationState;
    end
    else
      FAnimationValue := FValue;
    
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetRedLine(Value: Single);
begin
  if (FRedLine <> Value) then
  begin
    FRedLine := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetShowRedLine(Value: Boolean);
begin
  if (FShowRedLine <> Value) then
  begin
    FShowRedLine := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetDigitalDisplay(Value: Boolean);
begin
  if (FDigitalDisplay <> Value) then
  begin
    FDigitalDisplay := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetDigitalColor(Value: TColor);
begin
  if (FDigitalColor <> Value) then
  begin
    FDigitalColor := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetDigitalFormat(Value: string);
begin
  if (FDigitalFormat <> Value) then
  begin
    FDigitalFormat := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetNeedleColor(Value: TColor);
begin
  if (FNeedleColor <> Value) then
  begin
    FNeedleColor := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetUnit(Value: string);
begin
  if (FUnit <> Value) then
  begin
    FUnit := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetShowMajorTicks(Value: Boolean);
begin
  if (FShowMajorTicks <> Value) then
  begin
    FShowMajorTicks := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetShowMinorTicks(Value: Boolean);
begin
  if (FShowMinorTicks <> Value) then
  begin
    FShowMinorTicks := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetMajorTickStep(Value: Single);
begin
  if (FMajorTickStep <> Value) and (Value > 0) then
  begin
    FMajorTickStep := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.SetMinorTickStep(Value: Single);
begin
  if (FMinorTickStep <> Value) and (Value > 0) then
  begin
    FMinorTickStep := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDDialGauge.InvalidateBackground;
begin
  if not Assigned(FRenderLock) then
    Exit;
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
    BuildBackgroundSnapshot;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDDialGauge.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  Font: ISkFont;
  CenterX, CenterY, Radius: Single;
  StartAngle, SweepAngle: Single;
  I, TickCount: Integer;
  TickValue, TickAngle: Single;
  TickStart, TickEnd: TPointF;
  LabelPos: TPointF;
  LabelText: string;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));
  
  CenterX := Width / 2;
  CenterY := Height / 2;
  Radius := Min(Width, Height) / 2 - 20;
  
  // Draw outer circle
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 3;
  Paint.Color := SafeColorRefToSkColor(clGray);
  Canvas.DrawCircle(CenterX, CenterY, Radius, Paint);
  
  StartAngle := 135;  // 7 o'clock position
  SweepAngle := 270;  // Three-quarter circle
  
  // Draw red line arc if enabled
  if FShowRedLine and (FRedLine > FMin) and (FRedLine <= FMax) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 8;
    Paint.Color := SafeColorRefToSkColor(clRed);
    
    var RedLineAngle := StartAngle + ((FRedLine - FMin) / (FMax - FMin)) * SweepAngle;
    var RedLineSweep := (StartAngle + SweepAngle) - RedLineAngle;
    
    Canvas.DrawArc(
      TRectF.Create(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius),
      RedLineAngle, RedLineSweep, False, Paint);
  end;
  
  // Draw minor ticks
  if FShowMinorTicks and (FMinorTickStep > 0) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 1;
    Paint.Color := SafeColorRefToSkColor(clGray);
    
    TickCount := Round((FMax - FMin) / FMinorTickStep);
    for I := 0 to TickCount do
    begin
      TickValue := FMin + (I * FMinorTickStep);
      if (FMajorTickStep > 0) and (Frac((TickValue - FMin) / FMajorTickStep) = 0) then
        Continue;
        
      TickAngle := DegToRad(StartAngle + ((TickValue - FMin) / (FMax - FMin)) * SweepAngle);
      TickStart := PointF(CenterX + Cos(TickAngle) * (Radius - 10), 
                         CenterY + Sin(TickAngle) * (Radius - 10));
      TickEnd := PointF(CenterX + Cos(TickAngle) * Radius,
                       CenterY + Sin(TickAngle) * Radius);
      Canvas.DrawLine(TickStart, TickEnd, Paint);
    end;
  end;
  
  // Draw major ticks and labels
  if FShowMajorTicks and (FMajorTickStep > 0) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 2;
    Paint.Color := SafeColorRefToSkColor(clWhite);
    
    Font := TSkFont.Create(TSkTypeface.MakeDefault, 12);
    var TextPaint := TSkPaint.Create;
    TextPaint.AntiAlias := True;
    TextPaint.Color := SafeColorRefToSkColor(clWhite);
    
    TickCount := Round((FMax - FMin) / FMajorTickStep);
    for I := 0 to TickCount do
    begin
      TickValue := FMin + (I * FMajorTickStep);
      TickAngle := DegToRad(StartAngle + ((TickValue - FMin) / (FMax - FMin)) * SweepAngle);
      
      TickStart := PointF(CenterX + Cos(TickAngle) * (Radius - 15),
                         CenterY + Sin(TickAngle) * (Radius - 15));
      TickEnd := PointF(CenterX + Cos(TickAngle) * Radius,
                       CenterY + Sin(TickAngle) * Radius);
      Canvas.DrawLine(TickStart, TickEnd, Paint);
      
      LabelText := FormatFloat('0', TickValue);
      LabelPos := PointF(CenterX + Cos(TickAngle) * (Radius - 30),
                        CenterY + Sin(TickAngle) * (Radius - 30));
      Canvas.DrawSimpleText(LabelText, LabelPos.X, LabelPos.Y, Font, TextPaint);
    end;
  end;
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

function TOBDDialGauge.AcquireBackgroundSnapshot: ISkImage;
begin
  TMonitor.Enter(FRenderLock);
  try
    if not Assigned(FBackgroundSnapshot) then
      BuildBackgroundSnapshot;
    Result := FBackgroundSnapshot;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDDialGauge.PaintSkia(Canvas: ISkCanvas);
var
  BackgroundImage: ISkImage;
  Paint: ISkPaint;
  Font: ISkFont;
  CenterX, CenterY, Radius: Single;
  NeedleAngle: Single;
  NeedleEnd: TPointF;
  DisplayValue: Single;
  DigitalText: string;
  StartAngle, SweepAngle: Single;
begin
  try
    BackgroundImage := AcquireBackgroundSnapshot;
    if Assigned(BackgroundImage) then
      Canvas.DrawImage(BackgroundImage, 0, 0)
    else
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
    
    CenterX := Width / 2;
    CenterY := Height / 2;
    Radius := Min(Width, Height) / 2 - 20;
    
    StartAngle := 135;
    SweepAngle := 270;
    
    DisplayValue := FAnimationValue;
    
    // Draw needle
    NeedleAngle := DegToRad(StartAngle + ((DisplayValue - FMin) / (FMax - FMin)) * SweepAngle);
    NeedleEnd := PointF(CenterX + Cos(NeedleAngle) * (Radius - 25),
                       CenterY + Sin(NeedleAngle) * (Radius - 25));
    
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 3;
    Paint.Color := SafeColorRefToSkColor(FNeedleColor);
    Canvas.DrawLine(PointF(CenterX, CenterY), NeedleEnd, Paint);
    
    // Draw center circle
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FNeedleColor);
    Canvas.DrawCircle(CenterX, CenterY, 5, Paint);
    
    // Draw digital display
    if FDigitalDisplay then
    begin
      try
        DigitalText := Format(FDigitalFormat, [DisplayValue]);
      except
        DigitalText := FormatFloat('0.#', DisplayValue);
      end;
      
      if FUnit <> '' then
        DigitalText := DigitalText + ' ' + FUnit;
      
      Font := TSkFont.Create(TSkTypeface.MakeDefault, 24, 1, 0);
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Color := SafeColorRefToSkColor(FDigitalColor);
      Canvas.DrawSimpleText(DigitalText, CenterX, CenterY + 40, Font, Paint);
    end;
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
  end;
end;

procedure TOBDDialGauge.AnimationTick(ElapsedMs: Int64);
var
  CurrentMs, Elapsed: Int64;
  AnimationProgress, EasedProgress, InterpolatedValue: Single;
  EasingFunction: TOBDCustomControlAnimationEasingFunction;
begin
  if not FAnimationEnabled then
    Exit;
  
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  
  EasingFunction := GetEasingFunction(FAnimationType);
  if Elapsed < FAnimationDuration then
  begin
    AnimationProgress := Elapsed / FAnimationDuration;
    EasedProgress := EasingFunction(AnimationProgress);
    InterpolatedValue := FAnimationStartValue + (FValue - FAnimationStartValue) * EasedProgress;
    FAnimationValue := InterpolatedValue;
  end
  else
    FAnimationValue := FValue;
  
  Redraw;
  Invalidate;
end;

function TOBDDialGauge.IsAnimating: Boolean;
var
  CurrentMs, Elapsed: Int64;
begin
  if not FAnimationEnabled then
    Exit(False);
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  Result := (Elapsed < FAnimationDuration) and (FAnimationStartValue <> FValue);
end;

function TOBDDialGauge.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

procedure TOBDDialGauge.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDDialGauge.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

constructor TOBDDialGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FRedLine := DEFAULT_REDLINE;
  FShowRedLine := True;
  FDigitalDisplay := True;
  FDigitalColor := DEFAULT_DIGITAL_COLOR;
  FDigitalFormat := '%.0f';
  FNeedleColor := DEFAULT_NEEDLE_COLOR;
  FAnimationEnabled := DEFAULT_ANIMATION_ENABLED;
  FAnimationDuration := DEFAULT_ANIMATION_DURATION;
  FAnimationType := anQuartEaseInOut;
  FAnimationValue := FValue;
  FAnimationStartValue := FValue;
  FShowMajorTicks := True;
  FShowMinorTicks := True;
  FMajorTickStep := 20;
  FMinorTickStep := 10;
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
  
  if not (csDesigning in ComponentState) then
    AnimationManager.RegisterControl(Self);
  
  Width := 250;
  Height := 250;
end;

destructor TOBDDialGauge.Destroy;
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.UnregisterControl(Self);
  FRenderLock.Free;
  inherited Destroy;
end;

end.
