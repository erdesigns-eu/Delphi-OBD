//------------------------------------------------------------------------------
// UNIT           : OBD.FuelTrimGraph.pas
// CONTENTS       : Fuel trim graph (STFT vs LTFT) visualization
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.FuelTrimGraph;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections,
  Vcl.Controls, WinApi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Themes,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Animation,
  OBD.CustomControl.AnimationManager;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  DEFAULT_MIN = -25.0;  // Fuel trim percentage
  DEFAULT_MAX = 25.0;
  DEFAULT_STFT_COLOR = $00FF0000;  // Blue
  DEFAULT_LTFT_COLOR = $000000FF;  // Red
  DEFAULT_MAX_SAMPLES = 100;
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 300;

type
  /// <summary>
  ///   Data point for fuel trim values
  /// </summary>
  TFuelTrimDataPoint = record
    STFT: Single;  // Short Term Fuel Trim
    LTFT: Single;  // Long Term Fuel Trim
  end;

  /// <summary>
  ///   Color settings for fuel trim graph
  /// </summary>
  TOBDFuelTrimGraphColors = class(TPersistent)
  private
    FOwner: TComponent;
    FSTFT: TColor;
    FLTFT: TColor;
    FZeroLine: TColor;
    FGrid: TColor;
    FBackground: TColor;
    FBorder: TColor;
    FText: TColor;
    FOnChange: TNotifyEvent;
    procedure SetSTFT(Value: TColor);
    procedure SetLTFT(Value: TColor);
    procedure SetZeroLine(Value: TColor);
    procedure SetGrid(Value: TColor);
    procedure SetBackground(Value: TColor);
    procedure SetBorder(Value: TColor);
    procedure SetText(Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property STFT: TColor read FSTFT write SetSTFT default DEFAULT_STFT_COLOR;
    property LTFT: TColor read FLTFT write SetLTFT default DEFAULT_LTFT_COLOR;
    property ZeroLine: TColor read FZeroLine write SetZeroLine default clGray;
    property Grid: TColor read FGrid write SetGrid default clSilver;
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Border: TColor read FBorder write SetBorder default clGray;
    property Text: TColor read FText write SetText default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Animation settings for fuel trim graph
  /// </summary>
  TOBDFuelTrimGraphAnimation = class(TPersistent)
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
  ///   Fuel Trim Graph - Displays STFT vs LTFT over time
  /// </summary>
  TOBDFuelTrimGraph = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FDataPoints: TList<TFuelTrimDataPoint>;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FMin: Single;
    FMax: Single;
    FMaxSamples: Integer;
    FShowGrid: Boolean;
    FShowZeroLine: Boolean;
    FShowLegend: Boolean;
    FColors: TOBDFuelTrimGraphColors;
    FAnimation: TOBDFuelTrimGraphAnimation;
    FAnimatedSTFT: Single;
    FAnimatedLTFT: Single;
    FTargetSTFT: Single;
    FTargetLTFT: Single;
    FStartSTFT: Single;
    FStartLTFT: Single;
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetMaxSamples(Value: Integer);
    procedure SetShowGrid(Value: Boolean);
    procedure SetShowZeroLine(Value: Boolean);
    procedure SetShowLegend(Value: Boolean);
    procedure SetColors(Value: TOBDFuelTrimGraphColors);
    procedure SetAnimation(Value: TOBDFuelTrimGraphAnimation);
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
    // Data management
    procedure AddDataPoint(ASTFT, ALTFT: Single);
    procedure ClearData;
    function GetDataPointCount: Integer;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property MaxSamples: Integer read FMaxSamples write SetMaxSamples default DEFAULT_MAX_SAMPLES;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property ShowZeroLine: Boolean read FShowZeroLine write SetShowZeroLine default True;
    property ShowLegend: Boolean read FShowLegend write SetShowLegend default True;
    property Colors: TOBDFuelTrimGraphColors read FColors write SetColors;
    property Animation: TOBDFuelTrimGraphAnimation read FAnimation write SetAnimation;
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDFuelTrimGraphColors
//------------------------------------------------------------------------------

constructor TOBDFuelTrimGraphColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FSTFT := DEFAULT_STFT_COLOR;
  FLTFT := DEFAULT_LTFT_COLOR;
  FZeroLine := clGray;
  FGrid := clSilver;
  FBackground := clWhite;
  FBorder := clGray;
  FText := clBlack;
end;

procedure TOBDFuelTrimGraphColors.Assign(Source: TPersistent);
begin
  if Source is TOBDFuelTrimGraphColors then
  begin
    FSTFT := TOBDFuelTrimGraphColors(Source).STFT;
    FLTFT := TOBDFuelTrimGraphColors(Source).LTFT;
    FZeroLine := TOBDFuelTrimGraphColors(Source).ZeroLine;
    FGrid := TOBDFuelTrimGraphColors(Source).Grid;
    FBackground := TOBDFuelTrimGraphColors(Source).Background;
    FBorder := TOBDFuelTrimGraphColors(Source).Border;
    FText := TOBDFuelTrimGraphColors(Source).Text;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDFuelTrimGraphColors.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDFuelTrimGraphColors.SetSTFT(Value: TColor);
begin
  if FSTFT <> Value then
  begin
    FSTFT := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphColors.SetLTFT(Value: TColor);
begin
  if FLTFT <> Value then
  begin
    FLTFT := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphColors.SetZeroLine(Value: TColor);
begin
  if FZeroLine <> Value then
  begin
    FZeroLine := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphColors.SetGrid(Value: TColor);
begin
  if FGrid <> Value then
  begin
    FGrid := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphColors.SetBorder(Value: TColor);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphColors.SetText(Value: TColor);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDFuelTrimGraphAnimation
//------------------------------------------------------------------------------

constructor TOBDFuelTrimGraphAnimation.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FEasingType := acEaseInOutQuad;
end;

procedure TOBDFuelTrimGraphAnimation.Assign(Source: TPersistent);
begin
  if Source is TOBDFuelTrimGraphAnimation then
  begin
    FEnabled := TOBDFuelTrimGraphAnimation(Source).Enabled;
    FDuration := TOBDFuelTrimGraphAnimation(Source).Duration;
    FEasingType := TOBDFuelTrimGraphAnimation(Source).EasingType;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDFuelTrimGraphAnimation.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDFuelTrimGraphAnimation.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphAnimation.SetDuration(Value: Cardinal);
begin
  if FDuration <> Value then
  begin
    FDuration := Value;
    Changed;
  end;
end;

procedure TOBDFuelTrimGraphAnimation.SetEasingType(Value: TOBDCustomControlAnimationType);
begin
  if FEasingType <> Value then
  begin
    FEasingType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDFuelTrimGraph
//------------------------------------------------------------------------------

constructor TOBDFuelTrimGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FDataPoints := TList<TFuelTrimDataPoint>.Create;
  FStopwatch := TStopwatch.StartNew;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FMaxSamples := DEFAULT_MAX_SAMPLES;
  FShowGrid := True;
  FShowZeroLine := True;
  FShowLegend := True;
  FAnimatedSTFT := 0;
  FAnimatedLTFT := 0;
  FTargetSTFT := 0;
  FTargetLTFT := 0;
  FStartSTFT := 0;
  FStartLTFT := 0;
  FAnimationStartMs := 0;
  
  FColors := TOBDFuelTrimGraphColors.Create(Self);
  FColors.OnChange := OnColorsChanged;
  
  FAnimation := TOBDFuelTrimGraphAnimation.Create(Self);
  FAnimation.OnChange := OnAnimationChanged;
  
  Width := 400;
  Height := 300;
end;

destructor TOBDFuelTrimGraph.Destroy;
begin
  FAnimation.Free;
  FColors.Free;
  FDataPoints.Free;
  FRenderLock.Free;
  inherited;
end;

procedure TOBDFuelTrimGraph.Loaded;
begin
  inherited;
  InvalidateBackground;
end;

procedure TOBDFuelTrimGraph.Resize;
begin
  inherited;
  InvalidateBackground;
end;

procedure TOBDFuelTrimGraph.InvalidateBackground;
begin
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
  finally
    TMonitor.Exit(FRenderLock);
  end;
  Invalidate;
end;

procedure TOBDFuelTrimGraph.OnColorsChanged(Sender: TObject);
begin
  InvalidateBackground;
end;

procedure TOBDFuelTrimGraph.OnAnimationChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDFuelTrimGraph.SetMin(Value: Single);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDFuelTrimGraph.SetMax(Value: Single);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDFuelTrimGraph.SetMaxSamples(Value: Integer);
begin
  if FMaxSamples <> Value then
  begin
    FMaxSamples := Value;
    // Remove excess samples if needed
    TMonitor.Enter(FRenderLock);
    try
      while FDataPoints.Count > FMaxSamples do
        FDataPoints.Delete(0);
    finally
      TMonitor.Exit(FRenderLock);
    end;
    Invalidate;
  end;
end;

procedure TOBDFuelTrimGraph.SetShowGrid(Value: Boolean);
begin
  if FShowGrid <> Value then
  begin
    FShowGrid := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDFuelTrimGraph.SetShowZeroLine(Value: Boolean);
begin
  if FShowZeroLine <> Value then
  begin
    FShowZeroLine := Value;
    InvalidateBackground;
  end;
end;

procedure TOBDFuelTrimGraph.SetShowLegend(Value: Boolean);
begin
  if FShowLegend <> Value then
  begin
    FShowLegend := Value;
    Invalidate;
  end;
end;

procedure TOBDFuelTrimGraph.SetColors(Value: TOBDFuelTrimGraphColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDFuelTrimGraph.SetAnimation(Value: TOBDFuelTrimGraphAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TOBDFuelTrimGraph.AddDataPoint(ASTFT, ALTFT: Single);
var
  DataPoint: TFuelTrimDataPoint;
begin
  ASTFT := EnsureRange(ASTFT, FMin, FMax);
  ALTFT := EnsureRange(ALTFT, FMin, FMax);
  
  if FAnimation.Enabled then
  begin
    FStartSTFT := FAnimatedSTFT;
    FStartLTFT := FAnimatedLTFT;
    FTargetSTFT := ASTFT;
    FTargetLTFT := ALTFT;
    FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
    TOBDCustomControlAnimationManager.Instance.RegisterControl(Self);
  end
  else
  begin
    FAnimatedSTFT := ASTFT;
    FAnimatedLTFT := ALTFT;
  end;
  
  DataPoint.STFT := ASTFT;
  DataPoint.LTFT := ALTFT;
  
  TMonitor.Enter(FRenderLock);
  try
    FDataPoints.Add(DataPoint);
    if FDataPoints.Count > FMaxSamples then
      FDataPoints.Delete(0);
  finally
    TMonitor.Exit(FRenderLock);
  end;
  
  Invalidate;
end;

procedure TOBDFuelTrimGraph.ClearData;
begin
  TMonitor.Enter(FRenderLock);
  try
    FDataPoints.Clear;
  finally
    TMonitor.Exit(FRenderLock);
  end;
  Invalidate;
end;

function TOBDFuelTrimGraph.GetDataPointCount: Integer;
begin
  TMonitor.Enter(FRenderLock);
  try
    Result := FDataPoints.Count;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDFuelTrimGraph.AnimationTick(ElapsedMs: Int64);
var
  Progress: Single;
  EasingFunc: TOBDCustomControlEasingFunction;
begin
  if not FAnimation.Enabled then Exit;
  
  Progress := (ElapsedMs - FAnimationStartMs) / FAnimation.Duration;
  if Progress >= 1.0 then
  begin
    FAnimatedSTFT := FTargetSTFT;
    FAnimatedLTFT := FTargetLTFT;
    TOBDCustomControlAnimationManager.Instance.UnregisterControl(Self);
    Invalidate;
    Exit;
  end;
  
  EasingFunc := GetEasingFunction(FAnimation.EasingType);
  FAnimatedSTFT := FStartSTFT + (FTargetSTFT - FStartSTFT) * EasingFunc(Progress);
  FAnimatedLTFT := FStartLTFT + (FTargetLTFT - FStartLTFT) * EasingFunc(Progress);
  Invalidate;
end;

function TOBDFuelTrimGraph.IsAnimating: Boolean;
begin
  Result := FAnimation.Enabled and 
            ((FAnimatedSTFT <> FTargetSTFT) or (FAnimatedLTFT <> FTargetLTFT));
end;

function TOBDFuelTrimGraph.GetFramesPerSecond: Integer;
begin
  Result := 60;
end;

function TOBDFuelTrimGraph.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDFuelTrimGraph.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  GraphRect: TRectF;
  I: Integer;
  Y, GridStep: Single;
  Font: ISkFont;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  
  GraphRect := RectF(50, 10, Width - 10, Height - 30);
  
  // Background
  Paint := TSkPaint.Create;
  Paint.Color := FColors.Background;
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawRect(GraphRect, Paint);
  
  // Border
  Paint.Color := FColors.Border;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2;
  Canvas.DrawRect(GraphRect, Paint);
  
  // Grid
  if FShowGrid then
  begin
    Paint.Color := FColors.Grid;
    Paint.StrokeWidth := 1;
    GridStep := (GraphRect.Height) / 10;
    for I := 1 to 9 do
    begin
      Y := GraphRect.Top + I * GridStep;
      Canvas.DrawLine(GraphRect.Left, Y, GraphRect.Right, Y, Paint);
    end;
  end;
  
  // Zero line
  if FShowZeroLine then
  begin
    Y := GraphRect.Top + (GraphRect.Height / 2);
    Paint.Color := FColors.ZeroLine;
    Paint.StrokeWidth := 2;
    Canvas.DrawLine(GraphRect.Left, Y, GraphRect.Right, Y, Paint);
  end;
  
  // Y-axis labels
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Normal), 10);
  Paint.Color := FColors.Text;
  Paint.Style := TSkPaintStyle.Fill;
  
  for I := 0 to 4 do
  begin
    Y := GraphRect.Top + (GraphRect.Height * I / 4);
    Canvas.DrawSimpleText(FormatFloat('0', FMax - (FMax - FMin) * I / 4), 
                         10, Y, Font, Paint, TSkTextAlign.Left);
  end;
  
  // Labels
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 12);
  Canvas.DrawSimpleText('Fuel Trim (%)', GraphRect.CenterPoint.X, Height - 10,
                       Font, Paint, TSkTextAlign.Center);
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

procedure TOBDFuelTrimGraph.PaintSkia(Canvas: ISkCanvas);
var
  Background: ISkImage;
  Paint: ISkPaint;
  GraphRect: TRectF;
  I: Integer;
  X, Y, PrevX, PrevY: Single;
  DataPoint: TFuelTrimDataPoint;
  Path: ISkPath;
  Font: ISkFont;
  LegendY: Single;
begin
  Background := AcquireBackgroundSnapshot;
  if Assigned(Background) then
    Canvas.DrawImage(Background, 0, 0);
  
  GraphRect := RectF(50, 10, Width - 10, Height - 30);
  
  TMonitor.Enter(FRenderLock);
  try
    if FDataPoints.Count < 2 then Exit;
    
    // Draw STFT line
    Paint := TSkPaint.Create;
    Paint.Color := FColors.STFT;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 2;
    Paint.AntiAlias := True;
    
    Path := TSkPath.Create;
    for I := 0 to FDataPoints.Count - 1 do
    begin
      DataPoint := FDataPoints[I];
      X := GraphRect.Left + (GraphRect.Width * I / (FMaxSamples - 1));
      Y := GraphRect.Top + GraphRect.Height * (1 - (DataPoint.STFT - FMin) / (FMax - FMin));
      
      if I = 0 then
        Path.MoveTo(X, Y)
      else
        Path.LineTo(X, Y);
    end;
    Canvas.DrawPath(Path, Paint);
    
    // Draw LTFT line
    Paint.Color := FColors.LTFT;
    Path := TSkPath.Create;
    for I := 0 to FDataPoints.Count - 1 do
    begin
      DataPoint := FDataPoints[I];
      X := GraphRect.Left + (GraphRect.Width * I / (FMaxSamples - 1));
      Y := GraphRect.Top + GraphRect.Height * (1 - (DataPoint.LTFT - FMin) / (FMax - FMin));
      
      if I = 0 then
        Path.MoveTo(X, Y)
      else
        Path.LineTo(X, Y);
    end;
    Canvas.DrawPath(Path, Paint);
    
  finally
    TMonitor.Exit(FRenderLock);
  end;
  
  // Draw legend
  if FShowLegend then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Normal), 11);
    Paint.Style := TSkPaintStyle.Fill;
    LegendY := 25;
    
    // STFT legend
    Paint.Color := FColors.STFT;
    Canvas.DrawRect(RectF(GraphRect.Right - 120, LegendY - 5, GraphRect.Right - 100, LegendY + 5), Paint);
    Paint.Color := FColors.Text;
    Canvas.DrawSimpleText(Format('STFT: %.1f%%', [FAnimatedSTFT]), GraphRect.Right - 95, LegendY,
                         Font, Paint, TSkTextAlign.Left);
    
    // LTFT legend
    LegendY := LegendY + 20;
    Paint.Color := FColors.LTFT;
    Canvas.DrawRect(RectF(GraphRect.Right - 120, LegendY - 5, GraphRect.Right - 100, LegendY + 5), Paint);
    Paint.Color := FColors.Text;
    Canvas.DrawSimpleText(Format('LTFT: %.1f%%', [FAnimatedLTFT]), GraphRect.Right - 95, LegendY,
                         Font, Paint, TSkTextAlign.Left);
  end;
end;

end.
