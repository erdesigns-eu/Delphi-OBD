//------------------------------------------------------------------------------
// UNIT           : OBD.BoostGauge.pas
// CONTENTS       : Boost/Vacuum gauge with positive/negative range
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.BoostGauge;

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
  DEFAULT_MIN = -15.0;  // Vacuum (negative pressure)
  DEFAULT_MAX = 30.0;   // Boost (positive pressure)
  DEFAULT_ZERO_MARK_POSITION = 0.0;
  DEFAULT_NEEDLE_COLOR = $00DF7000;
  DEFAULT_VACUUM_COLOR = $000080FF;  // Red for vacuum
  DEFAULT_BOOST_COLOR = $0000FF00;   // Green for boost
  DEFAULT_REDLINE_COLOR = $000000FF; // Red for danger zone
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 500;

type
  /// <summary>
  ///   Color settings for boost gauge zones
  /// </summary>
  TOBDBoostGaugeColors = class(TPersistent)
  private
    FOwner: TComponent;
    FNeedle: TColor;
    FVacuum: TColor;
    FBoost: TColor;
    FRedLine: TColor;
    FBackground: TColor;
    FBorder: TColor;
    FText: TColor;
    FOnChange: TNotifyEvent;
    procedure SetNeedle(Value: TColor);
    procedure SetVacuum(Value: TColor);
    procedure SetBoost(Value: TColor);
    procedure SetRedLine(Value: TColor);
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
    property Vacuum: TColor read FVacuum write SetVacuum default DEFAULT_VACUUM_COLOR;
    property Boost: TColor read FBoost write SetBoost default DEFAULT_BOOST_COLOR;
    property RedLine: TColor read FRedLine write SetRedLine default DEFAULT_REDLINE_COLOR;
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Border: TColor read FBorder write SetBorder default clGray;
    property Text: TColor read FText write SetText default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Animation settings for boost gauge
  /// </summary>
  TOBDBoostGaugeAnimation = class(TPersistent)
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
  ///   Boost Gauge - Specialized gauge for boost/vacuum pressure with negative values
  /// </summary>
  TOBDBoostGauge = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FZeroMarkPosition: Single;
    FRedLineValue: Single;
    FShowRedLine: Boolean;
    FShowZeroMark: Boolean;
    FColors: TOBDBoostGaugeColors;
    FAnimation: TOBDBoostGaugeAnimation;
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
    procedure SetZeroMarkPosition(Value: Single);
    procedure SetRedLineValue(Value: Single);
    procedure SetShowRedLine(Value: Boolean);
    procedure SetShowZeroMark(Value: Boolean);
    procedure SetColors(Value: TOBDBoostGaugeColors);
    procedure SetAnimation(Value: TOBDBoostGaugeAnimation);
    procedure SetUnit(Value: string);
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
    property ZeroMarkPosition: Single read FZeroMarkPosition write SetZeroMarkPosition;
    property RedLineValue: Single read FRedLineValue write SetRedLineValue;
    property ShowRedLine: Boolean read FShowRedLine write SetShowRedLine default True;
    property ShowZeroMark: Boolean read FShowZeroMark write SetShowZeroMark default True;
    property Colors: TOBDBoostGaugeColors read FColors write SetColors;
    property Animation: TOBDBoostGaugeAnimation read FAnimation write SetAnimation;
    property UnitLabel: string read FUnit write SetUnit;
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
// TOBDBoostGaugeColors
//------------------------------------------------------------------------------

constructor TOBDBoostGaugeColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FNeedle := DEFAULT_NEEDLE_COLOR;
  FVacuum := DEFAULT_VACUUM_COLOR;
  FBoost := DEFAULT_BOOST_COLOR;
  FRedLine := DEFAULT_REDLINE_COLOR;
  FBackground := clWhite;
  FBorder := clGray;
  FText := clBlack;
end;

procedure TOBDBoostGaugeColors.Assign(Source: TPersistent);
begin
  if Source is TOBDBoostGaugeColors then
  begin
    FNeedle := TOBDBoostGaugeColors(Source).Needle;
    FVacuum := TOBDBoostGaugeColors(Source).Vacuum;
    FBoost := TOBDBoostGaugeColors(Source).Boost;
    FRedLine := TOBDBoostGaugeColors(Source).RedLine;
    FBackground := TOBDBoostGaugeColors(Source).Background;
    FBorder := TOBDBoostGaugeColors(Source).Border;
    FText := TOBDBoostGaugeColors(Source).Text;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDBoostGaugeColors.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDBoostGaugeColors.SetNeedle(Value: TColor);
begin
  if FNeedle <> Value then
  begin
    FNeedle := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeColors.SetVacuum(Value: TColor);
begin
  if FVacuum <> Value then
  begin
    FVacuum := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeColors.SetBoost(Value: TColor);
begin
  if FBoost <> Value then
  begin
    FBoost := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeColors.SetRedLine(Value: TColor);
begin
  if FRedLine <> Value then
  begin
    FRedLine := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeColors.SetBorder(Value: TColor);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeColors.SetText(Value: TColor);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDBoostGaugeAnimation
//------------------------------------------------------------------------------

constructor TOBDBoostGaugeAnimation.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FEasingType := acEaseInOutQuad;
end;

procedure TOBDBoostGaugeAnimation.Assign(Source: TPersistent);
begin
  if Source is TOBDBoostGaugeAnimation then
  begin
    FEnabled := TOBDBoostGaugeAnimation(Source).Enabled;
    FDuration := TOBDBoostGaugeAnimation(Source).Duration;
    FEasingType := TOBDBoostGaugeAnimation(Source).EasingType;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDBoostGaugeAnimation.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDBoostGaugeAnimation.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeAnimation.SetDuration(Value: Cardinal);
begin
  if FDuration <> Value then
  begin
    FDuration := Value;
    Changed;
  end;
end;

procedure TOBDBoostGaugeAnimation.SetEasingType(Value: TOBDCustomControlAnimationType);
begin
  if FEasingType <> Value then
  begin
    FEasingType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDBoostGauge
//------------------------------------------------------------------------------

constructor TOBDBoostGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FStopwatch := TStopwatch.StartNew;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FValue := 0;
  FZeroMarkPosition := DEFAULT_ZERO_MARK_POSITION;
  FRedLineValue := 25.0;
  FShowRedLine := True;
  FShowZeroMark := True;
  FAnimationValue := 0;
  FAnimationStartValue := 0;
  FAnimationStartMs := 0;
  FUnit := 'PSI';
  FShowMajorTicks := True;
  FShowMinorTicks := True;
  FMajorTickStep := 5.0;
  FMinorTickStep := 1.0;
  
  FColors := TOBDBoostGaugeColors.Create(Self);
  FColors.OnChange := OnColorsChanged;
  
  FAnimation := TOBDBoostGaugeAnimation.Create(Self);
  FAnimation.OnChange := OnAnimationChanged;
  
  Width := 300;
  Height := 300;
end;

destructor TOBDBoostGauge.Destroy;
begin
  FAnimation.Free;
  FColors.Free;
  FRenderLock.Free;
  inherited;
end;

procedure TOBDBoostGauge.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDBoostGauge.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDBoostGauge.InvalidateBackground;
begin
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDBoostGauge.OnColorsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDBoostGauge.OnAnimationChanged(Sender: TObject);
begin
  Redraw;
  Invalidate;
end;

procedure TOBDBoostGauge.SetMin(Value: Single);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetMax(Value: Single);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetValue(Value: Single);
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
      Redraw;
      Invalidate;
    end;
  end;
end;

procedure TOBDBoostGauge.SetZeroMarkPosition(Value: Single);
begin
  if FZeroMarkPosition <> Value then
  begin
    FZeroMarkPosition := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetRedLineValue(Value: Single);
begin
  if FRedLineValue <> Value then
  begin
    FRedLineValue := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetShowRedLine(Value: Boolean);
begin
  if FShowRedLine <> Value then
  begin
    FShowRedLine := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetShowZeroMark(Value: Boolean);
begin
  if FShowZeroMark <> Value then
  begin
    FShowZeroMark := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetColors(Value: TOBDBoostGaugeColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDBoostGauge.SetAnimation(Value: TOBDBoostGaugeAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TOBDBoostGauge.SetUnit(Value: string);
begin
  if FUnit <> Value then
  begin
    FUnit := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetShowMajorTicks(Value: Boolean);
begin
  if FShowMajorTicks <> Value then
  begin
    FShowMajorTicks := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetShowMinorTicks(Value: Boolean);
begin
  if FShowMinorTicks <> Value then
  begin
    FShowMinorTicks := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetMajorTickStep(Value: Single);
begin
  if FMajorTickStep <> Value then
  begin
    FMajorTickStep := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.SetMinorTickStep(Value: Single);
begin
  if FMinorTickStep <> Value then
  begin
    FMinorTickStep := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBoostGauge.AnimationTick(ElapsedMs: Int64);
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
    Redraw;
    Invalidate;
    Exit;
  end;
  
  EasingFunc := GetEasingFunction(FAnimation.EasingType);
  FAnimationValue := FAnimationStartValue + (FValue - FAnimationStartValue) * EasingFunc(Progress);
  Redraw;
  Invalidate;
end;

function TOBDBoostGauge.IsAnimating: Boolean;
begin
  Result := FAnimation.Enabled and (FAnimationValue <> FValue);
end;

function TOBDBoostGauge.GetFramesPerSecond: Integer;
begin
  Result := 60;
end;

function TOBDBoostGauge.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDBoostGauge.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  CenterX, CenterY, Radius: Single;
  Angle, StartAngle, EndAngle: Single;
  TickValue, X1, Y1, X2, Y2: Single;
  TextRect: TRectF;
  Font: ISkFont;
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
  
  // Draw vacuum zone (negative values) - left half
  if FMin < 0 then
  begin
    Paint.Color := FColors.Vacuum;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 10;
    Canvas.DrawArc(RectF(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius),
                    StartAngle, 90, False, Paint);
  end;
  
  // Draw boost zone (positive values) - right half
  if FMax > 0 then
  begin
    Paint.Color := FColors.Boost;
    Canvas.DrawArc(RectF(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius),
                    -45, 90, False, Paint);
  end;
  
  // Draw redline zone
  if FShowRedLine and (FRedLineValue > 0) then
  begin
    Paint.Color := FColors.RedLine;
    Paint.StrokeWidth := 12;
    Canvas.DrawArc(RectF(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius),
                    -45, 30, False, Paint);
  end;
  
  // Draw zero mark
  if FShowZeroMark then
  begin
    Paint.Color := FColors.Text;
    Paint.StrokeWidth := 3;
    Angle := -90 * Pi / 180;
    X1 := CenterX + (Radius - 20) * Cos(Angle);
    Y1 := CenterY + (Radius - 20) * Sin(Angle);
    X2 := CenterX + (Radius - 5) * Cos(Angle);
    Y2 := CenterY + (Radius - 5) * Sin(Angle);
    Canvas.DrawLine(X1, Y1, X2, Y2, Paint);
  end;
  
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
  
  // Draw unit label
  if FUnit <> '' then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 14);
    TextRect := RectF(CenterX - 40, CenterY + 40, CenterX + 40, CenterY + 60);
    Canvas.DrawSimpleText(FUnit, TextRect.CenterPoint.X, TextRect.CenterPoint.Y,
                         Font, Paint, TSkTextAlign.Center);
  end;
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

procedure TOBDBoostGauge.PaintSkia(Canvas: ISkCanvas);
var
  Background: ISkImage;
  Paint: ISkPaint;
  CenterX, CenterY, Radius: Single;
  Angle, StartAngle, EndAngle: Single;
  NeedleLength, X1, Y1, X2, Y2: Single;
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
end;

end.
