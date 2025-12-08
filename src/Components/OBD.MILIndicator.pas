//------------------------------------------------------------------------------
// UNIT           : OBD.MILIndicator.pas
// CONTENTS       : Malfunction Indicator Lamp (Check Engine Light) with Skia rendering
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.MILIndicator;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Diagnostics, System.Types,
  System.UITypes, Vcl.Controls, WinApi.Windows, Winapi.Messages, Vcl.Graphics,
  Vcl.Themes, System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Animation,
  OBD.CustomControl.AnimationManager;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  MARGIN_FROM_BORDER = 4;
  
  DEFAULT_MIL_OFF_COLOR = $00404040;
  DEFAULT_MIL_ON_COLOR = $0000FFFF;      // Amber/Yellow
  DEFAULT_MIL_WARNING_COLOR = $000080FF;  // Orange
  DEFAULT_MIL_CRITICAL_COLOR = $000000FF; // Red
  DEFAULT_BACKGROUND_COLOR = $00000000;   // Black
  
  DEFAULT_SHOW_DTC_COUNT = True;
  DEFAULT_BLINK_INTERVAL = 500;  // milliseconds
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 300;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   MIL indicator state
  /// </summary>
  TMILState = (msOff, msOn, msBlinking);
  
  /// <summary>
  ///   MIL severity level
  /// </summary>
  TMILSeverity = (msNormal, msWarning, msCritical);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   MIL indicator colors
  /// </summary>
  TOBDMILColors = class(TPersistent)
  private
    FOffColor: TColor;
    FOnColor: TColor;
    FWarningColor: TColor;
    FCriticalColor: TColor;
    FBackgroundColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetOffColor(Value: TColor);
    procedure SetOnColor(Value: TColor);
    procedure SetWarningColor(Value: TColor);
    procedure SetCriticalColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property OffColor: TColor read FOffColor write SetOffColor default DEFAULT_MIL_OFF_COLOR;
    property OnColor: TColor read FOnColor write SetOnColor default DEFAULT_MIL_ON_COLOR;
    property WarningColor: TColor read FWarningColor write SetWarningColor default DEFAULT_MIL_WARNING_COLOR;
    property CriticalColor: TColor read FCriticalColor write SetCriticalColor default DEFAULT_MIL_CRITICAL_COLOR;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default DEFAULT_BACKGROUND_COLOR;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   MIL indicator animation
  /// </summary>
  TOBDMILAnimation = class(TPersistent)
  private
    FEnabled: Boolean;
    FDuration: Cardinal;
    FType: TOBDCustomControlAnimationType;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(Value: Boolean);
    procedure SetDuration(Value: Cardinal);
    procedure SetType(Value: TOBDCustomControlAnimationType);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default DEFAULT_ANIMATION_ENABLED;
    property Duration: Cardinal read FDuration write SetDuration default DEFAULT_ANIMATION_DURATION;
    property &Type: TOBDCustomControlAnimationType read FType write SetType default anQuartEaseOut;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Malfunction Indicator Lamp (Check Engine Light) component
  /// </summary>
  TOBDMILIndicator = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FBlinkTimer: Int64;
    
    FState: TMILState;
    FSeverity: TMILSeverity;
    FDTCCount: Integer;
    FShowDTCCount: Boolean;
    FBlinkInterval: Cardinal;
    FBlinkState: Boolean;
    
    FColors: TOBDMILColors;
    FAnimation: TOBDMILAnimation;
    
    FAnimationProgress: Single;
    FAnimationStartValue: Single;
    FAnimationTargetValue: Single;
    
    procedure SetState(Value: TMILState);
    procedure SetSeverity(Value: TMILSeverity);
    procedure SetDTCCount(Value: Integer);
    procedure SetShowDTCCount(Value: Boolean);
    procedure SetBlinkInterval(Value: Cardinal);
    procedure SetColors(Value: TOBDMILColors);
    procedure SetAnimation(Value: TOBDMILAnimation);
  protected
    procedure InvalidateBackground; virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    procedure DrawEngineIcon(Canvas: ISkCanvas; CenterX, CenterY, Size: Single; Alpha: Single = 1.0);
    function GetCurrentColor: TColor;
    procedure SettingsChanged(Sender: TObject);
    procedure AnimationChanged(Sender: TObject);
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    
    // IOBDAnimatable interface
    procedure AnimationTick(ElapsedMs: Int64);
    function IsAnimating: Boolean;
    function GetFramesPerSecond: Integer;
  published
    property State: TMILState read FState write SetState default msOff;
    property Severity: TMILSeverity read FSeverity write SetSeverity default msNormal;
    property DTCCount: Integer read FDTCCount write SetDTCCount default 0;
    property ShowDTCCount: Boolean read FShowDTCCount write SetShowDTCCount default DEFAULT_SHOW_DTC_COUNT;
    property BlinkInterval: Cardinal read FBlinkInterval write SetBlinkInterval default DEFAULT_BLINK_INTERVAL;
    property Colors: TOBDMILColors read FColors write SetColors;
    property Animation: TOBDMILAnimation read FAnimation write SetAnimation;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDMILColors Implementation
//------------------------------------------------------------------------------

constructor TOBDMILColors.Create;
begin
  inherited Create;
  FOffColor := DEFAULT_MIL_OFF_COLOR;
  FOnColor := DEFAULT_MIL_ON_COLOR;
  FWarningColor := DEFAULT_MIL_WARNING_COLOR;
  FCriticalColor := DEFAULT_MIL_CRITICAL_COLOR;
  FBackgroundColor := DEFAULT_BACKGROUND_COLOR;
end;

procedure TOBDMILColors.SetOffColor(Value: TColor);
begin
  if FOffColor <> Value then
  begin
    FOffColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILColors.SetOnColor(Value: TColor);
begin
  if FOnColor <> Value then
  begin
    FOnColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILColors.SetWarningColor(Value: TColor);
begin
  if FWarningColor <> Value then
  begin
    FWarningColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILColors.SetCriticalColor(Value: TColor);
begin
  if FCriticalColor <> Value then
  begin
    FCriticalColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILColors.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILColors.Assign(Source: TPersistent);
begin
  if Source is TOBDMILColors then
  begin
    FOffColor := (Source as TOBDMILColors).OffColor;
    FOnColor := (Source as TOBDMILColors).OnColor;
    FWarningColor := (Source as TOBDMILColors).WarningColor;
    FCriticalColor := (Source as TOBDMILColors).CriticalColor;
    FBackgroundColor := (Source as TOBDMILColors).BackgroundColor;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDMILAnimation Implementation
//------------------------------------------------------------------------------

constructor TOBDMILAnimation.Create;
begin
  inherited Create;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FType := anQuartEaseOut;
end;

procedure TOBDMILAnimation.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILAnimation.SetDuration(Value: Cardinal);
begin
  if (FDuration <> Value) and (Value >= 10) then
  begin
    FDuration := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILAnimation.SetType(Value: TOBDCustomControlAnimationType);
begin
  if FType <> Value then
  begin
    FType := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDMILAnimation.Assign(Source: TPersistent);
begin
  if Source is TOBDMILAnimation then
  begin
    FEnabled := (Source as TOBDMILAnimation).Enabled;
    FDuration := (Source as TOBDMILAnimation).Duration;
    FType := (Source as TOBDMILAnimation).&Type;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDMILIndicator Implementation
//------------------------------------------------------------------------------

constructor TOBDMILIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FState := msOff;
  FSeverity := msNormal;
  FDTCCount := 0;
  FShowDTCCount := DEFAULT_SHOW_DTC_COUNT;
  FBlinkInterval := DEFAULT_BLINK_INTERVAL;
  FBlinkState := False;
  
  FColors := TOBDMILColors.Create;
  FColors.OnChange := SettingsChanged;
  
  FAnimation := TOBDMILAnimation.Create;
  FAnimation.OnChange := AnimationChanged;
  
  FAnimationProgress := 1.0;
  FAnimationStartValue := 0.0;
  FAnimationTargetValue := 0.0;
  
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
  FBlinkTimer := 0;
  
  if not (csDesigning in ComponentState) then
    AnimationManager.RegisterControl(Self);
  
  Width := 120;
  Height := 120;
end;

destructor TOBDMILIndicator.Destroy;
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.UnregisterControl(Self);
  FRenderLock.Free;
  FColors.Free;
  FAnimation.Free;
  inherited Destroy;
end;

procedure TOBDMILIndicator.SetState(Value: TMILState);
begin
  if FState <> Value then
  begin
    FState := Value;
    
    if FAnimation.Enabled and not (csDesigning in ComponentState) then
    begin
      FAnimationStartValue := FAnimationTargetValue;
      FAnimationTargetValue := IfThen(FState = msOff, 0.0, 1.0);
      FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
      FAnimationProgress := 0.0;
      AnimationManager.CheckAnimationState;
    end;
    
    FBlinkTimer := FStopwatch.ElapsedMilliseconds;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDMILIndicator.SetSeverity(Value: TMILSeverity);
begin
  if FSeverity <> Value then
  begin
    FSeverity := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDMILIndicator.SetDTCCount(Value: Integer);
begin
  if (FDTCCount <> Value) and (Value >= 0) then
  begin
    FDTCCount := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDMILIndicator.SetShowDTCCount(Value: Boolean);
begin
  if FShowDTCCount <> Value then
  begin
    FShowDTCCount := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDMILIndicator.SetBlinkInterval(Value: Cardinal);
begin
  if (FBlinkInterval <> Value) and (Value >= 100) then
  begin
    FBlinkInterval := Value;
  end;
end;

procedure TOBDMILIndicator.SetColors(Value: TOBDMILColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDMILIndicator.SetAnimation(Value: TOBDMILAnimation);
begin
  FAnimation.Assign(Value);
end;

function TOBDMILIndicator.GetCurrentColor: TColor;
begin
  if FState = msOff then
    Result := FColors.OffColor
  else
  begin
    case FSeverity of
      msWarning: Result := FColors.WarningColor;
      msCritical: Result := FColors.CriticalColor;
    else
      Result := FColors.OnColor;
    end;
  end;
end;

procedure TOBDMILIndicator.InvalidateBackground;
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

procedure TOBDMILIndicator.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  
  // Draw background
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FColors.BackgroundColor);
  Canvas.DrawRect(TRectF.Create(0, 0, Width, Height), Paint);
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

function TOBDMILIndicator.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDMILIndicator.DrawEngineIcon(Canvas: ISkCanvas; CenterX, CenterY, Size: Single; Alpha: Single);
var
  Paint: ISkPaint;
  Path: ISkPathBuilder;
  IconColor: TColor;
  AlphaValue: Byte;
  Font: ISkFont;
  BlockSize, GapSize: Single;
begin
  IconColor := GetCurrentColor;
  AlphaValue := Round(Alpha * 255);
  
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(IconColor) and $00FFFFFF or (AlphaValue shl 24);
  
  BlockSize := Size * 0.15;
  GapSize := Size * 0.08;
  
  // Draw simplified engine block icon
  Path := TSkPathBuilder.Create;
  
  // Main engine block (rectangle)
  Path.AddRect(TRectF.Create(
    CenterX - Size * 0.35,
    CenterY - Size * 0.25,
    CenterX + Size * 0.35,
    CenterY + Size * 0.35
  ));
  
  Canvas.DrawPath(Path.Detach, Paint);
  
  // Draw cylinder blocks on top
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawRect(TRectF.Create(
    CenterX - Size * 0.25,
    CenterY - Size * 0.35,
    CenterX - Size * 0.05,
    CenterY - Size * 0.2
  ), Paint);
  
  Canvas.DrawRect(TRectF.Create(
    CenterX + Size * 0.05,
    CenterY - Size * 0.35,
    CenterX + Size * 0.25,
    CenterY - Size * 0.2
  ), Paint);
  
  // Draw "CHECK ENGINE" text
  if FShowDTCCount and (FDTCCount > 0) then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeDefault, 12, 1, 0);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(IconColor) and $00FFFFFF or (AlphaValue shl 24);
    Canvas.DrawSimpleText(Format('DTC: %d', [FDTCCount]), CenterX, CenterY + Size * 0.5, Font, Paint);
  end;
end;

procedure TOBDMILIndicator.PaintSkia(Canvas: ISkCanvas);
var
  BackgroundImage: ISkImage;
  CenterX, CenterY, IconSize, Alpha: Single;
  CurrentMs: Int64;
begin
  try
    BackgroundImage := AcquireBackgroundSnapshot;
    if Assigned(BackgroundImage) then
      Canvas.DrawImage(BackgroundImage, 0, 0)
    else
      Canvas.Clear(SafeColorRefToSkColor(FColors.BackgroundColor));
    
    CenterX := Width / 2;
    CenterY := Height / 2;
    IconSize := Min(Width, Height) * 0.6;
    
    // Calculate alpha based on state and animation
    Alpha := 1.0;
    
    if FState = msBlinking then
    begin
      CurrentMs := FStopwatch.ElapsedMilliseconds;
      if ((CurrentMs - FBlinkTimer) div FBlinkInterval) mod 2 = 0 then
        Alpha := 1.0
      else
        Alpha := 0.0;
    end
    else if FAnimation.Enabled and (FAnimationProgress < 1.0) then
    begin
      Alpha := FAnimationStartValue + (FAnimationTargetValue - FAnimationStartValue) * 
               EaseValue(FAnimationProgress, FAnimation.&Type);
    end
    else
    begin
      Alpha := IfThen(FState = msOff, 0.0, 1.0);
    end;
    
    if Alpha > 0.0 then
      DrawEngineIcon(Canvas, CenterX, CenterY, IconSize, Alpha);
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(FColors.BackgroundColor));
  end;
end;

procedure TOBDMILIndicator.AnimationTick(ElapsedMs: Int64);
var
  CurrentMs, Elapsed: Int64;
begin
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  
  // Handle fade animation
  if FAnimation.Enabled and (FAnimationProgress < 1.0) then
  begin
    Elapsed := CurrentMs - FAnimationStartMs;
    if Elapsed < FAnimation.Duration then
      FAnimationProgress := Elapsed / FAnimation.Duration
    else
      FAnimationProgress := 1.0;
  end;
  
  // Always redraw if blinking
  if FState = msBlinking then
    Redraw;
  
  if (FAnimationProgress < 1.0) or (FState = msBlinking) then
    Invalidate;
end;

function TOBDMILIndicator.IsAnimating: Boolean;
begin
  if FState = msBlinking then
    Exit(True);
  if not FAnimation.Enabled then
    Exit(False);
  Result := FAnimationProgress < 1.0;
end;

function TOBDMILIndicator.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

procedure TOBDMILIndicator.SettingsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDMILIndicator.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.CheckAnimationState;
  Redraw;
  Invalidate;
end;

procedure TOBDMILIndicator.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDMILIndicator.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDMILIndicator.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TOBDMILIndicator then
  begin
    FState := (Source as TOBDMILIndicator).State;
    FSeverity := (Source as TOBDMILIndicator).Severity;
    FDTCCount := (Source as TOBDMILIndicator).DTCCount;
    FShowDTCCount := (Source as TOBDMILIndicator).ShowDTCCount;
    FBlinkInterval := (Source as TOBDMILIndicator).BlinkInterval;
    FColors.Assign((Source as TOBDMILIndicator).Colors);
    FAnimation.Assign((Source as TOBDMILIndicator).Animation);
  end;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

end.
