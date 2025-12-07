//------------------------------------------------------------------------------
// UNIT           : OBD.SegmentedGauge.pas
// CONTENTS       : LED-style segmented bar gauge (like VU meter)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.SegmentedGauge;

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
  DEFAULT_SEGMENTS = 20;
  DEFAULT_SEGMENT_SPACING = 2;
  DEFAULT_MIN = 0;
  DEFAULT_MAX = 100;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  TSegmentStyle = (ssBar, ssCircle, ssDiamond, ssRectangle);
  TSegmentOrientation = (soHorizontal, soVertical);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Segmented LED-style gauge - VU meter appearance
  /// </summary>
  TOBDSegmentedGauge = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FSegmentCount: Integer;
    FSegmentSpacing: Integer;
    FSegmentStyle: TSegmentStyle;
    FOrientation: TSegmentOrientation;
    FColorLow: TColor;
    FColorMid: TColor;
    FColorHigh: TColor;
    FColorOff: TColor;
    FLowThreshold: Single;
    FHighThreshold: Single;
    FAnimationEnabled: Boolean;
    FAnimationDuration: Cardinal;
    FAnimationValue: Single;
    FAnimationStartValue: Single;
    FShowPeakHold: Boolean;
    FPeakValue: Single;
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetValue(Value: Single);
    procedure SetSegmentCount(Value: Integer);
    procedure SetSegmentSpacing(Value: Integer);
    procedure SetSegmentStyle(Value: TSegmentStyle);
    procedure SetOrientation(Value: TSegmentOrientation);
    procedure SetColorLow(Value: TColor);
    procedure SetColorMid(Value: TColor);
    procedure SetColorHigh(Value: TColor);
    procedure SetColorOff(Value: TColor);
    procedure SetLowThreshold(Value: Single);
    procedure SetHighThreshold(Value: Single);
    procedure SetShowPeakHold(Value: Boolean);
  protected
    procedure InvalidateBackground; virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    function GetSegmentColor(SegmentIndex: Integer): TColor;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetPeak;
    // IOBDAnimatable interface
    procedure AnimationTick(ElapsedMs: Int64);
    function IsAnimating: Boolean;
    function GetFramesPerSecond: Integer;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property SegmentCount: Integer read FSegmentCount write SetSegmentCount default DEFAULT_SEGMENTS;
    property SegmentSpacing: Integer read FSegmentSpacing write SetSegmentSpacing default DEFAULT_SEGMENT_SPACING;
    property SegmentStyle: TSegmentStyle read FSegmentStyle write SetSegmentStyle default ssBar;
    property Orientation: TSegmentOrientation read FOrientation write SetOrientation default soVertical;
    property ColorLow: TColor read FColorLow write SetColorLow default clLime;
    property ColorMid: TColor read FColorMid write SetColorMid default clYellow;
    property ColorHigh: TColor read FColorHigh write SetColorHigh default clRed;
    property ColorOff: TColor read FColorOff write SetColorOff default $00202020;
    property LowThreshold: Single read FLowThreshold write SetLowThreshold;
    property HighThreshold: Single read FHighThreshold write SetHighThreshold;
    property ShowPeakHold: Boolean read FShowPeakHold write SetShowPeakHold default False;
    property AnimationEnabled: Boolean read FAnimationEnabled write FAnimationEnabled default True;
    property AnimationDuration: Cardinal read FAnimationDuration write FAnimationDuration default 500;
  end;

implementation

uses
  System.Math;

procedure TOBDSegmentedGauge.SetMin(Value: Single);
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

procedure TOBDSegmentedGauge.SetMax(Value: Single);
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

procedure TOBDSegmentedGauge.SetValue(Value: Single);
begin
  if (FValue <> Value) then
  begin
    if (Value < FMin) then Value := FMin;
    if (Value > FMax) then Value := FMax;
    FValue := Value;
    
    // Update peak hold
    if FShowPeakHold and (FValue > FPeakValue) then
      FPeakValue := FValue;
    
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

procedure TOBDSegmentedGauge.SetSegmentCount(Value: Integer);
begin
  if (FSegmentCount <> Value) and (Value >= 3) and (Value <= 100) then
  begin
    FSegmentCount := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetSegmentSpacing(Value: Integer);
begin
  if (FSegmentSpacing <> Value) and (Value >= 0) then
  begin
    FSegmentSpacing := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetSegmentStyle(Value: TSegmentStyle);
begin
  if (FSegmentStyle <> Value) then
  begin
    FSegmentStyle := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetOrientation(Value: TSegmentOrientation);
begin
  if (FOrientation <> Value) then
  begin
    FOrientation := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetColorLow(Value: TColor);
begin
  if (FColorLow <> Value) then
  begin
    FColorLow := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetColorMid(Value: TColor);
begin
  if (FColorMid <> Value) then
  begin
    FColorMid := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetColorHigh(Value: TColor);
begin
  if (FColorHigh <> Value) then
  begin
    FColorHigh := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetColorOff(Value: TColor);
begin
  if (FColorOff <> Value) then
  begin
    FColorOff := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetLowThreshold(Value: Single);
begin
  if (FLowThreshold <> Value) then
  begin
    FLowThreshold := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetHighThreshold(Value: Single);
begin
  if (FHighThreshold <> Value) then
  begin
    FHighThreshold := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.SetShowPeakHold(Value: Boolean);
begin
  if (FShowPeakHold <> Value) then
  begin
    FShowPeakHold := Value;
    if not FShowPeakHold then
      FPeakValue := FMin;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSegmentedGauge.InvalidateBackground;
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

function TOBDSegmentedGauge.GetSegmentColor(SegmentIndex: Integer): TColor;
var
  SegmentValue: Single;
begin
  SegmentValue := (SegmentIndex / FSegmentCount) * 100;
  
  if SegmentValue <= FLowThreshold then
    Result := FColorLow
  else if SegmentValue <= FHighThreshold then
    Result := FColorMid
  else
    Result := FColorHigh;
end;

procedure TOBDSegmentedGauge.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  I: Integer;
  SegmentWidth, SegmentHeight: Single;
  SegmentRect: TRectF;
  X, Y: Single;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));
  
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  
  if FOrientation = soVertical then
  begin
    SegmentHeight := (Height - (FSegmentCount - 1) * FSegmentSpacing) / FSegmentCount;
    SegmentWidth := Width;
    
    for I := 0 to FSegmentCount - 1 do
    begin
      Y := Height - ((I + 1) * SegmentHeight) - (I * FSegmentSpacing);
      SegmentRect := TRectF.Create(0, Y, SegmentWidth, Y + SegmentHeight);
      
      Paint.Color := SafeColorRefToSkColor(FColorOff);
      
      case FSegmentStyle of
        ssBar:
          Canvas.DrawRect(SegmentRect, Paint);
        ssCircle:
          Canvas.DrawCircle(SegmentRect.CenterPoint.X, SegmentRect.CenterPoint.Y, 
                           Min(SegmentRect.Width, SegmentRect.Height) / 2, Paint);
        ssDiamond:
          begin
            var Path := TSkPathBuilder.Create;
            Path.MoveTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Top));
            Path.LineTo(PointF(SegmentRect.Right, SegmentRect.CenterPoint.Y));
            Path.LineTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Bottom));
            Path.LineTo(PointF(SegmentRect.Left, SegmentRect.CenterPoint.Y));
            Path.Close;
            Canvas.DrawPath(Path.Detach, Paint);
          end;
        ssRectangle:
          Canvas.DrawRoundRect(SegmentRect, 2, 2, Paint);
      end;
    end;
  end
  else // Horizontal
  begin
    SegmentWidth := (Width - (FSegmentCount - 1) * FSegmentSpacing) / FSegmentCount;
    SegmentHeight := Height;
    
    for I := 0 to FSegmentCount - 1 do
    begin
      X := I * (SegmentWidth + FSegmentSpacing);
      SegmentRect := TRectF.Create(X, 0, X + SegmentWidth, SegmentHeight);
      
      Paint.Color := SafeColorRefToSkColor(FColorOff);
      
      case FSegmentStyle of
        ssBar:
          Canvas.DrawRect(SegmentRect, Paint);
        ssCircle:
          Canvas.DrawCircle(SegmentRect.CenterPoint.X, SegmentRect.CenterPoint.Y,
                           Min(SegmentRect.Width, SegmentRect.Height) / 2, Paint);
        ssDiamond:
          begin
            var Path := TSkPathBuilder.Create;
            Path.MoveTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Top));
            Path.LineTo(PointF(SegmentRect.Right, SegmentRect.CenterPoint.Y));
            Path.LineTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Bottom));
            Path.LineTo(PointF(SegmentRect.Left, SegmentRect.CenterPoint.Y));
            Path.Close;
            Canvas.DrawPath(Path.Detach, Paint);
          end;
        ssRectangle:
          Canvas.DrawRoundRect(SegmentRect, 2, 2, Paint);
      end;
    end;
  end;
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

function TOBDSegmentedGauge.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDSegmentedGauge.PaintSkia(Canvas: ISkCanvas);
var
  BackgroundImage: ISkImage;
  Paint: ISkPaint;
  DisplayValue: Single;
  LitSegments, PeakSegment: Integer;
  I: Integer;
  SegmentWidth, SegmentHeight: Single;
  SegmentRect: TRectF;
  X, Y: Single;
begin
  try
    BackgroundImage := AcquireBackgroundSnapshot;
    if Assigned(BackgroundImage) then
      Canvas.DrawImage(BackgroundImage, 0, 0)
    else
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
    
    DisplayValue := FAnimationValue;
    LitSegments := Round((DisplayValue - FMin) / (FMax - FMin) * FSegmentCount);
    
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    
    if FOrientation = soVertical then
    begin
      SegmentHeight := (Height - (FSegmentCount - 1) * FSegmentSpacing) / FSegmentCount;
      SegmentWidth := Width;
      
      for I := 0 to LitSegments - 1 do
      begin
        Y := Height - ((I + 1) * SegmentHeight) - (I * FSegmentSpacing);
        SegmentRect := TRectF.Create(0, Y, SegmentWidth, Y + SegmentHeight);
        
        Paint.Color := SafeColorRefToSkColor(GetSegmentColor(I));
        
        case FSegmentStyle of
          ssBar:
            Canvas.DrawRect(SegmentRect, Paint);
          ssCircle:
            Canvas.DrawCircle(SegmentRect.CenterPoint.X, SegmentRect.CenterPoint.Y,
                             Min(SegmentRect.Width, SegmentRect.Height) / 2, Paint);
          ssDiamond:
            begin
              var Path := TSkPathBuilder.Create;
              Path.MoveTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Top));
              Path.LineTo(PointF(SegmentRect.Right, SegmentRect.CenterPoint.Y));
              Path.LineTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Bottom));
              Path.LineTo(PointF(SegmentRect.Left, SegmentRect.CenterPoint.Y));
              Path.Close;
              Canvas.DrawPath(Path.Detach, Paint);
            end;
          ssRectangle:
            Canvas.DrawRoundRect(SegmentRect, 2, 2, Paint);
        end;
      end;
      
      // Draw peak hold
      if FShowPeakHold and (FPeakValue > FMin) then
      begin
        PeakSegment := Round((FPeakValue - FMin) / (FMax - FMin) * FSegmentCount) - 1;
        if (PeakSegment >= 0) and (PeakSegment < FSegmentCount) then
        begin
          Y := Height - ((PeakSegment + 1) * SegmentHeight) - (PeakSegment * FSegmentSpacing);
          SegmentRect := TRectF.Create(0, Y, SegmentWidth, Y + SegmentHeight);
          Paint.Color := SafeColorRefToSkColor(clWhite);
          Canvas.DrawRect(SegmentRect, Paint);
        end;
      end;
    end
    else // Horizontal
    begin
      SegmentWidth := (Width - (FSegmentCount - 1) * FSegmentSpacing) / FSegmentCount;
      SegmentHeight := Height;
      
      for I := 0 to LitSegments - 1 do
      begin
        X := I * (SegmentWidth + FSegmentSpacing);
        SegmentRect := TRectF.Create(X, 0, X + SegmentWidth, SegmentHeight);
        
        Paint.Color := SafeColorRefToSkColor(GetSegmentColor(I));
        
        case FSegmentStyle of
          ssBar:
            Canvas.DrawRect(SegmentRect, Paint);
          ssCircle:
            Canvas.DrawCircle(SegmentRect.CenterPoint.X, SegmentRect.CenterPoint.Y,
                             Min(SegmentRect.Width, SegmentRect.Height) / 2, Paint);
          ssDiamond:
            begin
              var Path := TSkPathBuilder.Create;
              Path.MoveTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Top));
              Path.LineTo(PointF(SegmentRect.Right, SegmentRect.CenterPoint.Y));
              Path.LineTo(PointF(SegmentRect.CenterPoint.X, SegmentRect.Bottom));
              Path.LineTo(PointF(SegmentRect.Left, SegmentRect.CenterPoint.Y));
              Path.Close;
              Canvas.DrawPath(Path.Detach, Paint);
            end;
          ssRectangle:
            Canvas.DrawRoundRect(SegmentRect, 2, 2, Paint);
        end;
      end;
      
      // Draw peak hold
      if FShowPeakHold and (FPeakValue > FMin) then
      begin
        PeakSegment := Round((FPeakValue - FMin) / (FMax - FMin) * FSegmentCount) - 1;
        if (PeakSegment >= 0) and (PeakSegment < FSegmentCount) then
        begin
          X := PeakSegment * (SegmentWidth + FSegmentSpacing);
          SegmentRect := TRectF.Create(X, 0, X + SegmentWidth, SegmentHeight);
          Paint.Color := SafeColorRefToSkColor(clWhite);
          Canvas.DrawRect(SegmentRect, Paint);
        end;
      end;
    end;
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
  end;
end;

procedure TOBDSegmentedGauge.ResetPeak;
begin
  FPeakValue := FMin;
  Redraw;
  Invalidate;
end;

procedure TOBDSegmentedGauge.AnimationTick(ElapsedMs: Int64);
var
  CurrentMs, Elapsed: Int64;
  AnimationProgress, EasedProgress, InterpolatedValue: Single;
  EasingFunction: TOBDCustomControlAnimationEasingFunction;
begin
  if not FAnimationEnabled then
    Exit;
  
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  
  EasingFunction := GetEasingFunction(anQuartEaseOut);
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

function TOBDSegmentedGauge.IsAnimating: Boolean;
var
  CurrentMs, Elapsed: Int64;
begin
  if not FAnimationEnabled then
    Exit(False);
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  Result := (Elapsed < FAnimationDuration) and (FAnimationStartValue <> FValue);
end;

function TOBDSegmentedGauge.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

procedure TOBDSegmentedGauge.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDSegmentedGauge.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

constructor TOBDSegmentedGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FSegmentCount := DEFAULT_SEGMENTS;
  FSegmentSpacing := DEFAULT_SEGMENT_SPACING;
  FSegmentStyle := ssBar;
  FOrientation := soVertical;
  FColorLow := clLime;
  FColorMid := clYellow;
  FColorHigh := clRed;
  FColorOff := $00202020;
  FLowThreshold := 60;
  FHighThreshold := 85;
  FAnimationEnabled := True;
  FAnimationDuration := 500;
  FAnimationValue := FValue;
  FAnimationStartValue := FValue;
  FShowPeakHold := False;
  FPeakValue := FMin;
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
  
  if not (csDesigning in ComponentState) then
    AnimationManager.RegisterControl(Self);
  
  Width := 40;
  Height := 200;
end;

destructor TOBDSegmentedGauge.Destroy;
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.UnregisterControl(Self);
  FRenderLock.Free;
  inherited Destroy;
end;

end.
