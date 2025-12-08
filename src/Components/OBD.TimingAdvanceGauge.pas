//------------------------------------------------------------------------------
// UNIT           : OBD.TimingAdvanceGauge.pas
// CONTENTS       : Timing advance gauge visualization
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.TimingAdvanceGauge;

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
  DEFAULT_MIN = -10.0;  // Retard
  DEFAULT_MAX = 50.0;   // Advance
  DEFAULT_OPTIMAL_MIN = 10.0;
  DEFAULT_OPTIMAL_MAX = 20.0;
  DEFAULT_NEEDLE_COLOR = $00DF7000;
  DEFAULT_RETARD_COLOR = $000080FF;  // Red
  DEFAULT_OPTIMAL_COLOR = $0000FF00; // Green
  DEFAULT_ADVANCE_COLOR = $0000FFFF; // Yellow
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 500;

type
  /// <summary>
  ///   Color settings for timing advance gauge zones
  /// </summary>
  TOBDTimingAdvanceGaugeColors = class(TPersistent)
  private
    FOwner: TComponent;
    FNeedle: TColor;
    FRetard: TColor;
    FOptimal: TColor;
    FAdvance: TColor;
    FBackground: TColor;
    FBorder: TColor;
    FText: TColor;
    FOnChange: TNotifyEvent;
    procedure SetNeedle(Value: TColor);
    procedure SetRetard(Value: TColor);
    procedure SetOptimal(Value: TColor);
    procedure SetAdvance(Value: TColor);
    procedure SetBackground(Value: TColor);
    procedure SetBorder(Value: TColor);
    procedure SetText(Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Needle: TColor read FNeedle write SetNeedle default DEFAULT_NEEDLE_COLOR;
    property Retard: TColor read FRetard write SetRetard default DEFAULT_RETARD_COLOR;
    property Optimal: TColor read FOptimal write SetOptimal default DEFAULT_OPTIMAL_COLOR;
    property Advance: TColor read FAdvance write SetAdvance default DEFAULT_ADVANCE_COLOR;
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Border: TColor read FBorder write SetBorder default clGray;
    property Text: TColor read FText write SetText default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Animation settings for timing advance gauge
  /// </summary>
  TOBDTimingAdvanceGaugeAnimation = class(TPersistent)
  private
    FOwner: TComponent;
    FEnabled: Boolean;
    FDuration: Cardinal;
    FEasingType: TOBDCustomControlAnimationType;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(Value: Boolean);
    procedure SetDuration(Value: Cardinal);
    procedure SetEasingType(Value: TOBDCustomControlAnimationType);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default DEFAULT_ANIMATION_ENABLED;
    property Duration: Cardinal read FDuration write SetDuration default DEFAULT_ANIMATION_DURATION;
    property EasingType: TOBDCustomControlAnimationType read FEasingType write SetEasingType default acEaseInOutQuad;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Timing Advance Gauge - Visualizes ignition timing with optimal zone highlighting
  /// </summary>
  TOBDTimingAdvanceGauge = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FOptimalMin: Single;
    FOptimalMax: Single;
    FShowOptimalZone: Boolean;
    FColors: TOBDTimingAdvanceGaugeColors;
    FAnimation: TOBDTimingAdvanceGaugeAnimation;
    FAnimationValue: Single;
    FAnimationStartValue: Single;
    FShowDigitalDisplay: Boolean;
    FShowMajorTicks: Boolean;
    FShowMinorTicks: Boolean;
    FMajorTickStep: Single;
    FMinorTickStep: Single;
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetValue(Value: Single);
    procedure SetOptimalMin(Value: Single);
    procedure SetOptimalMax(Value: Single);
    procedure SetShowOptimalZone(Value: Boolean);
    procedure SetColors(Value: TOBDTimingAdvanceGaugeColors);
    procedure SetAnimation(Value: TOBDTimingAdvanceGaugeAnimation);
    procedure SetShowDigitalDisplay(Value: Boolean);
    procedure SetShowMajorTicks(Value: Boolean);
    procedure SetShowMinorTicks(Value: Boolean);
    procedure SetMajorTickStep(Value: Single);
    procedure SetMinorTickStep(Value: Single);
    procedure OnColorsChanged(Sender: TObject);
    procedure OnAnimationChanged(Sender: TObject);
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
    property OptimalMin: Single read FOptimalMin write SetOptimalMin;
    property OptimalMax: Single read FOptimalMax write SetOptimalMax;
    property ShowOptimalZone: Boolean read FShowOptimalZone write SetShowOptimalZone default True;
    property Colors: TOBDTimingAdvanceGaugeColors read FColors write SetColors;
    property Animation: TOBDTimingAdvanceGaugeAnimation read FAnimation write SetAnimation;
    property ShowDigitalDisplay: Boolean read FShowDigitalDisplay write SetShowDigitalDisplay default True;
    property ShowMajorTicks: Boolean read FShowMajorTicks write SetShowMajorTicks default True;
    property ShowMinorTicks: Boolean read FShowMinorTicks write SetShowMinorTicks default True;
    property MajorTickStep: Single read FMajorTickStep write SetMajorTickStep;
    property MinorTickStep: Single read FMinorTickStep write SetMinorTickStep;
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDTimingAdvanceGaugeColors
//------------------------------------------------------------------------------

constructor TOBDTimingAdvanceGaugeColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FNeedle := DEFAULT_NEEDLE_COLOR;
  FRetard := DEFAULT_RETARD_COLOR;
  FOptimal := DEFAULT_OPTIMAL_COLOR;
  FAdvance := DEFAULT_ADVANCE_COLOR;
  FBackground := clWhite;
  FBorder := clGray;
  FText := clBlack;
end;

procedure TOBDTimingAdvanceGaugeColors.Assign(Source: TPersistent);
begin
  if Source is TOBDTimingAdvanceGaugeColors then
  begin
    FNeedle := TOBDTimingAdvanceGaugeColors(Source).Needle;
    FRetard := TOBDTimingAdvanceGaugeColors(Source).Retard;
    FOptimal := TOBDTimingAdvanceGaugeColors(Source).Optimal;
    FAdvance := TOBDTimingAdvanceGaugeColors(Source).Advance;
    FBackground := TOBDTimingAdvanceGaugeColors(Source).Background;
    FBorder := TOBDTimingAdvanceGaugeColors(Source).Border;
    FText := TOBDTimingAdvanceGaugeColors(Source).Text;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDTimingAdvanceGaugeColors.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDTimingAdvanceGaugeColors.SetNeedle(Value: TColor);
begin
  if FNeedle <> Value then
  begin
    FNeedle := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeColors.SetRetard(Value: TColor);
begin
  if FRetard <> Value then
  begin
    FRetard := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeColors.SetOptimal(Value: TColor);
begin
  if FOptimal <> Value then
  begin
    FOptimal := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeColors.SetAdvance(Value: TColor);
begin
  if FAdvance <> Value then
  begin
    FAdvance := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeColors.SetBorder(Value: TColor);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeColors.SetText(Value: TColor);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDTimingAdvanceGaugeAnimation
//------------------------------------------------------------------------------

constructor TOBDTimingAdvanceGaugeAnimation.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FEasingType := acEaseInOutQuad;
end;

procedure TOBDTimingAdvanceGaugeAnimation.Assign(Source: TPersistent);
begin
  if Source is TOBDTimingAdvanceGaugeAnimation then
  begin
    FEnabled := TOBDTimingAdvanceGaugeAnimation(Source).Enabled;
    FDuration := TOBDTimingAdvanceGaugeAnimation(Source).Duration;
    FEasingType := TOBDTimingAdvanceGaugeAnimation(Source).EasingType;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDTimingAdvanceGaugeAnimation.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDTimingAdvanceGaugeAnimation.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeAnimation.SetDuration(Value: Cardinal);
begin
  if FDuration <> Value then
  begin
    FDuration := Value;
    Changed;
  end;
end;

procedure TOBDTimingAdvanceGaugeAnimation.SetEasingType(Value: TOBDCustomControlAnimationType);
begin
  if FEasingType <> Value then
  begin
    FEasingType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDTimingAdvanceGauge
//------------------------------------------------------------------------------

constructor TOBDTimingAdvanceGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FStopwatch := TStopwatch.StartNew;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FValue := 0;
  FOptimalMin := DEFAULT_OPTIMAL_MIN;
  FOptimalMax := DEFAULT_OPTIMAL_MAX;
  FShowOptimalZone := True;
  FAnimationValue := 0;
  FAnimationStartValue := 0;
  FAnimationStartMs := 0;
  FShowDigitalDisplay := True;
  FShowMajorTicks := True;
  FShowMinorTicks := True;
  FMajorTickStep := 10.0;
  FMinorTickStep := 2.0;
  
  FColors := TOBDTimingAdvanceGaugeColors.Create(Self);
  FColors.OnChange := OnColorsChanged;
  
  FAnimation := TOBDTimingAdvanceGaugeAnimation.Create(Self);
  FAnimation.OnChange := OnAnimationChanged;
  
  Width := 300;
  Height := 300;
end;

destructor TOBDTimingAdvanceGauge.Destroy;
begin
  FAnimation.Free;
  FColors.Free;
  FRenderLock.Free;
  inherited;
end;

procedure TOBDTimingAdvanceGauge.Loaded;
begin
  inherited;
  InvalidateBackground;
end;

procedure TOBDTimingAdvanceGauge.Resize;
begin
  inherited;
  InvalidateBackground;
end;

procedure TOBDTimingAdvanceGauge.InvalidateBackground;
begin
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
  finally
    TMonitor.Exit(FRenderLock);
  end;
  Invalidate;
end;

procedure TOBDTimingAdvanceGauge.OnColorsChanged(Sender: TObject);
begin
  InvalidateBackground;
end;

procedure TOBDTimingAdvanceGauge.OnAnimationChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDTimingAdvanceGauge.SetMin(Value: Single);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetMax(Value: Single);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetValue(Value: Single);
begin
  Value := EnsureRange(Value, FMin, FMax);
  if FValue <> Value then
  begin
    if FAnimation.Enabled then
    begin
      FAnimationStartValue := FAnimationValue;
      FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
      FValue := Value;
      TOBDCustomControlAnimationManager.Instance.RegisterControl(Self);
    end
    else
    begin
      FValue := Value;
      FAnimationValue := Value;
      Invalidate;
    end;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetOptimalMin(Value: Single);
begin
  if FOptimalMin <> Value then
  begin
    FOptimalMin := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetOptimalMax(Value: Single);
begin
  if FOptimalMax <> Value then
  begin
    FOptimalMax := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetShowOptimalZone(Value: Boolean);
begin
  if FShowOptimalZone <> Value then
  begin
    FShowOptimalZone := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetColors(Value: TOBDTimingAdvanceGaugeColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDTimingAdvanceGauge.SetAnimation(Value: TOBDTimingAdvanceGaugeAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TOBDTimingAdvanceGauge.SetShowDigitalDisplay(Value: Boolean);
begin
  if FShowDigitalDisplay <> Value then
  begin
    FShowDigitalDisplay := Value;
    Invalidate;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetShowMajorTicks(Value: Boolean);
begin
  if FShowMajorTicks <> Value then
  begin
    FShowMajorTicks := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetShowMinorTicks(Value: Boolean);
begin
  if FShowMinorTicks <> Value then
  begin
    FShowMinorTicks := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetMajorTickStep(Value: Single);
begin
  if FMajorTickStep <> Value then
  begin
    FMajorTickStep := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.SetMinorTickStep(Value: Single);
begin
  if FMinorTickStep <> Value then
  begin
    FMinorTickStep := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDTimingAdvanceGauge.AnimationTick(ElapsedMs: Int64);
var
  Progress: Single;
  EasingFunc: TOBDCustomControlEasingFunction;
begin
  if not FAnimation.Enabled then Exit;
  
  Progress := (ElapsedMs - FAnimationStartMs) / FAnimation.Duration;
  if Progress >= 1.0 then
  begin
    FAnimationValue := FValue;
    TOBDCustomControlAnimationManager.Instance.UnregisterControl(Self);
    Invalidate;
    Exit;
  end;
  
  EasingFunc := GetEasingFunction(FAnimation.EasingType);
  FAnimationValue := FAnimationStartValue + (FValue - FAnimationStartValue) * EasingFunc(Progress);
  Invalidate;
end;

function TOBDTimingAdvanceGauge.IsAnimating: Boolean;
begin
  Result := FAnimation.Enabled and (FAnimationValue <> FValue);
end;

function TOBDTimingAdvanceGauge.GetFramesPerSecond: Integer;
begin
  Result := 60;
end;

function TOBDTimingAdvanceGauge.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDTimingAdvanceGauge.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  CenterX, CenterY, Radius: Single;
  Angle, StartAngle, EndAngle: Single;
  TickValue, X1, Y1, X2, Y2: Single;
  TextRect: TRectF;
  Font: ISkFont;
  RetardAngle, OptimalStartAngle, OptimalEndAngle, AdvanceAngle: Single;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  
  CenterX := Width / 2;
  CenterY := Height / 2;
  Radius := Min(Width, Height) / 2 - 20;
  
  // Background
  Paint := TSkPaint.Create;
  Paint.Color := FColors.Background;
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawCircle(CenterX, CenterY, Radius, Paint);
  
  // Border
  Paint.Color := FColors.Border;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2;
  Canvas.DrawCircle(CenterX, CenterY, Radius, Paint);
  
  StartAngle := 135;
  EndAngle := 45;
  
  // Calculate zone angles
  RetardAngle := 0;
  if FMin < 0 then
    RetardAngle := (Abs(FMin) / (FMax - FMin)) * (EndAngle - StartAngle + 360);
  
  OptimalStartAngle := StartAngle + ((FOptimalMin - FMin) / (FMax - FMin)) * (EndAngle - StartAngle + 360);
  OptimalEndAngle := StartAngle + ((FOptimalMax - FMin) / (FMax - FMin)) * (EndAngle - StartAngle + 360);
  AdvanceAngle := (EndAngle - StartAngle + 360) - (OptimalEndAngle - StartAngle);
  
  // Draw retard zone (red)
  if RetardAngle > 0 then
  begin
    Paint.Color := FColors.Retard;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 10;
    Canvas.DrawArc(RectF(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius),
                    StartAngle, RetardAngle, False, Paint);
  end;
  
  // Draw optimal zone (green)
  if FShowOptimalZone then
  begin
    Paint.Color := FColors.Optimal;
    Canvas.DrawArc(RectF(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius),
                    OptimalStartAngle, OptimalEndAngle - OptimalStartAngle, False, Paint);
  end;
  
  // Draw advance zone (yellow)
  Paint.Color := FColors.Advance;
  Canvas.DrawArc(RectF(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius),
                  OptimalEndAngle, AdvanceAngle, False, Paint);
  
  // Draw ticks and labels
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Normal), 12);
  Paint.Color := FColors.Text;
  Paint.StrokeWidth := 2;
  
  if FShowMajorTicks then
  begin
    TickValue := FMin;
    while TickValue <= FMax do
    begin
      // Calculate angle for this tick
      Angle := StartAngle + (TickValue - FMin) / (FMax - FMin) * (EndAngle - StartAngle + 360);
      if Angle >= 360 then Angle := Angle - 360;
      Angle := Angle * Pi / 180;
      
      X1 := CenterX + (Radius - 15) * Cos(Angle);
      Y1 := CenterY + (Radius - 15) * Sin(Angle);
      X2 := CenterX + Radius * Cos(Angle);
      Y2 := CenterY + Radius * Sin(Angle);
      Canvas.DrawLine(X1, Y1, X2, Y2, Paint);
      
      // Draw label
      TextRect := RectF(X1 - 20, Y1 - 10, X1 + 20, Y1 + 10);
      Canvas.DrawSimpleText(FormatFloat('0', TickValue), TextRect.CenterPoint.X, TextRect.CenterPoint.Y,
                           Font, Paint, TSkTextAlign.Center);
      
      TickValue := TickValue + FMajorTickStep;
    end;
  end;
  
  // Draw labels
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 14);
  TextRect := RectF(CenterX - 60, CenterY + 40, CenterX + 60, CenterY + 60);
  Canvas.DrawSimpleText('TIMING ADVANCE', TextRect.CenterPoint.X, TextRect.CenterPoint.Y,
                       Font, Paint, TSkTextAlign.Center);
  
  TextRect := RectF(CenterX - 40, CenterY + 55, CenterX + 40, CenterY + 75);
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Normal), 12);
  Canvas.DrawSimpleText('(Degrees)', TextRect.CenterPoint.X, TextRect.CenterPoint.Y,
                       Font, Paint, TSkTextAlign.Center);
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

procedure TOBDTimingAdvanceGauge.PaintSkia(Canvas: ISkCanvas);
var
  Background: ISkImage;
  Paint: ISkPaint;
  CenterX, CenterY, Radius: Single;
  Angle, StartAngle, EndAngle: Single;
  NeedleLength, X1, Y1, X2, Y2: Single;
  Font: ISkFont;
  TextRect: TRectF;
begin
  Background := AcquireBackgroundSnapshot;
  if Assigned(Background) then
    Canvas.DrawImage(Background, 0, 0);
  
  CenterX := Width / 2;
  CenterY := Height / 2;
  Radius := Min(Width, Height) / 2 - 20;
  
  // Draw needle
  Paint := TSkPaint.Create;
  Paint.Color := FColors.Needle;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 3;
  Paint.StrokeCap := TSkStrokeCap.Round;
  
  StartAngle := 135;
  EndAngle := 45;
  
  // Calculate needle angle based on value
  Angle := StartAngle + (FAnimationValue - FMin) / (FMax - FMin) * (EndAngle - StartAngle + 360);
  if Angle >= 360 then Angle := Angle - 360;
  Angle := Angle * Pi / 180;
  
  NeedleLength := Radius - 10;
  X1 := CenterX;
  Y1 := CenterY;
  X2 := CenterX + NeedleLength * Cos(Angle);
  Y2 := CenterY + NeedleLength * Sin(Angle);
  Canvas.DrawLine(X1, Y1, X2, Y2, Paint);
  
  // Draw center dot
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawCircle(CenterX, CenterY, 5, Paint);
  
  // Draw digital display
  if FShowDigitalDisplay then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 24);
    Paint.Color := FColors.Needle;
    TextRect := RectF(CenterX - 50, CenterY - 30, CenterX + 50, CenterY - 5);
    Canvas.DrawSimpleText(FormatFloat('0.0°', FAnimationValue), TextRect.CenterPoint.X, TextRect.CenterPoint.Y,
                         Font, Paint, TSkTextAlign.Center);
  end;
end;

end.
