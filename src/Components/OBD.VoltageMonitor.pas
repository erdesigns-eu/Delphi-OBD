//------------------------------------------------------------------------------
// UNIT           : OBD.VoltageMonitor.pas
// CONTENTS       : Battery voltage monitor with warning indicators
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.VoltageMonitor;

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
  DEFAULT_MIN = 8.0;
  DEFAULT_MAX = 16.0;
  DEFAULT_NORMAL_MIN = 12.5;
  DEFAULT_NORMAL_MAX = 14.8;
  DEFAULT_WARNING_LOW = 11.5;
  DEFAULT_CRITICAL_LOW = 10.5;
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 500;

type
  /// <summary>
  ///   Voltage status levels
  /// </summary>
  TOBDVoltageStatus = (vsUnknown, vsCritical, vsLow, vsNormal, vsHigh);

  /// <summary>
  ///   Color settings for voltage monitor
  /// </summary>
  TOBDVoltageMonitorColors = class(TPersistent)
  private
    FOwner: TComponent;
    FCritical: TColor;
    FLow: TColor;
    FNormal: TColor;
    FHigh: TColor;
    FBackground: TColor;
    FBorder: TColor;
    FText: TColor;
    FOnChange: TNotifyEvent;
    procedure SetCritical(Value: TColor);
    procedure SetLow(Value: TColor);
    procedure SetNormal(Value: TColor);
    procedure SetHigh(Value: TColor);
    procedure SetBackground(Value: TColor);
    procedure SetBorder(Value: TColor);
    procedure SetText(Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Critical: TColor read FCritical write SetCritical default clRed;
    property Low: TColor read FLow write SetLow default $000080FF;  // Orange
    property Normal: TColor read FNormal write SetNormal default $0000FF00;  // Green
    property High: TColor read FHigh write SetHigh default $0000FFFF;  // Yellow
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Border: TColor read FBorder write SetBorder default clGray;
    property Text: TColor read FText write SetText default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Animation settings for voltage monitor
  /// </summary>
  TOBDVoltageMonitorAnimation = class(TPersistent)
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
  ///   Voltage Monitor - Battery voltage gauge with warning indicators for ECU flashing
  /// </summary>
  TOBDVoltageMonitor = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FNormalMin: Single;
    FNormalMax: Single;
    FWarningLow: Single;
    FCriticalLow: Single;
    FShowWarningIndicator: Boolean;
    FShowDigitalDisplay: Boolean;
    FColors: TOBDVoltageMonitorColors;
    FAnimation: TOBDVoltageMonitorAnimation;
    FAnimationValue: Single;
    FAnimationStartValue: Single;
    FShowStatusText: Boolean;
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetValue(Value: Single);
    procedure SetNormalMin(Value: Single);
    procedure SetNormalMax(Value: Single);
    procedure SetWarningLow(Value: Single);
    procedure SetCriticalLow(Value: Single);
    procedure SetShowWarningIndicator(Value: Boolean);
    procedure SetShowDigitalDisplay(Value: Boolean);
    procedure SetColors(Value: TOBDVoltageMonitorColors);
    procedure SetAnimation(Value: TOBDVoltageMonitorAnimation);
    procedure SetShowStatusText(Value: Boolean);
    procedure OnColorsChanged(Sender: TObject);
    procedure OnAnimationChanged(Sender: TObject);
    function GetVoltageStatus: TOBDVoltageStatus;
    function GetStatusColor: TColor;
    function GetStatusText: string;
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
    // Status
    property VoltageStatus: TOBDVoltageStatus read GetVoltageStatus;
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property NormalMin: Single read FNormalMin write SetNormalMin;
    property NormalMax: Single read FNormalMax write SetNormalMax;
    property WarningLow: Single read FWarningLow write SetWarningLow;
    property CriticalLow: Single read FCriticalLow write SetCriticalLow;
    property ShowWarningIndicator: Boolean read FShowWarningIndicator write SetShowWarningIndicator default True;
    property ShowDigitalDisplay: Boolean read FShowDigitalDisplay write SetShowDigitalDisplay default True;
    property ShowStatusText: Boolean read FShowStatusText write SetShowStatusText default True;
    property Colors: TOBDVoltageMonitorColors read FColors write SetColors;
    property Animation: TOBDVoltageMonitorAnimation read FAnimation write SetAnimation;
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDVoltageMonitorColors
//------------------------------------------------------------------------------

constructor TOBDVoltageMonitorColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FCritical := clRed;
  FLow := $000080FF;  // Orange
  FNormal := $0000FF00;  // Green
  FHigh := $0000FFFF;  // Yellow
  FBackground := clWhite;
  FBorder := clGray;
  FText := clBlack;
end;

procedure TOBDVoltageMonitorColors.Assign(Source: TPersistent);
begin
  if Source is TOBDVoltageMonitorColors then
  begin
    FCritical := TOBDVoltageMonitorColors(Source).Critical;
    FLow := TOBDVoltageMonitorColors(Source).Low;
    FNormal := TOBDVoltageMonitorColors(Source).Normal;
    FHigh := TOBDVoltageMonitorColors(Source).High;
    FBackground := TOBDVoltageMonitorColors(Source).Background;
    FBorder := TOBDVoltageMonitorColors(Source).Border;
    FText := TOBDVoltageMonitorColors(Source).Text;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDVoltageMonitorColors.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDVoltageMonitorColors.SetCritical(Value: TColor);
begin
  if FCritical <> Value then
  begin
    FCritical := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorColors.SetLow(Value: TColor);
begin
  if FLow <> Value then
  begin
    FLow := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorColors.SetNormal(Value: TColor);
begin
  if FNormal <> Value then
  begin
    FNormal := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorColors.SetHigh(Value: TColor);
begin
  if FHigh <> Value then
  begin
    FHigh := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorColors.SetBorder(Value: TColor);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorColors.SetText(Value: TColor);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDVoltageMonitorAnimation
//------------------------------------------------------------------------------

constructor TOBDVoltageMonitorAnimation.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FEasingType := acEaseInOutQuad;
end;

procedure TOBDVoltageMonitorAnimation.Assign(Source: TPersistent);
begin
  if Source is TOBDVoltageMonitorAnimation then
  begin
    FEnabled := TOBDVoltageMonitorAnimation(Source).Enabled;
    FDuration := TOBDVoltageMonitorAnimation(Source).Duration;
    FEasingType := TOBDVoltageMonitorAnimation(Source).EasingType;
    Changed;
  end
  else
    inherited;
end;

procedure TOBDVoltageMonitorAnimation.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDVoltageMonitorAnimation.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorAnimation.SetDuration(Value: Cardinal);
begin
  if FDuration <> Value then
  begin
    FDuration := Value;
    Changed;
  end;
end;

procedure TOBDVoltageMonitorAnimation.SetEasingType(Value: TOBDCustomControlAnimationType);
begin
  if FEasingType <> Value then
  begin
    FEasingType := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// TOBDVoltageMonitor
//------------------------------------------------------------------------------

constructor TOBDVoltageMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FStopwatch := TStopwatch.StartNew;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FValue := 12.6;
  FNormalMin := DEFAULT_NORMAL_MIN;
  FNormalMax := DEFAULT_NORMAL_MAX;
  FWarningLow := DEFAULT_WARNING_LOW;
  FCriticalLow := DEFAULT_CRITICAL_LOW;
  FShowWarningIndicator := True;
  FShowDigitalDisplay := True;
  FShowStatusText := True;
  FAnimationValue := 12.6;
  FAnimationStartValue := 12.6;
  FAnimationStartMs := 0;
  
  FColors := TOBDVoltageMonitorColors.Create(Self);
  FColors.OnChange := OnColorsChanged;
  
  FAnimation := TOBDVoltageMonitorAnimation.Create(Self);
  FAnimation.OnChange := OnAnimationChanged;
  
  Width := 300;
  Height := 200;
end;

destructor TOBDVoltageMonitor.Destroy;
begin
  FAnimation.Free;
  FColors.Free;
  FRenderLock.Free;
  inherited;
end;

procedure TOBDVoltageMonitor.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDVoltageMonitor.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDVoltageMonitor.InvalidateBackground;
begin
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

procedure TOBDVoltageMonitor.OnColorsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDVoltageMonitor.OnAnimationChanged(Sender: TObject);
begin
  Redraw;
  Invalidate;
end;

procedure TOBDVoltageMonitor.SetMin(Value: Single);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    InvalidateBackground;
  Redraw;
  Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetMax(Value: Single);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    InvalidateBackground;
  Redraw;
  Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetValue(Value: Single);
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

procedure TOBDVoltageMonitor.SetNormalMin(Value: Single);
begin
  if FNormalMin <> Value then
  begin
    FNormalMin := Value;
    InvalidateBackground;
  Redraw;
  Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetNormalMax(Value: Single);
begin
  if FNormalMax <> Value then
  begin
    FNormalMax := Value;
    InvalidateBackground;
  Redraw;
  Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetWarningLow(Value: Single);
begin
  if FWarningLow <> Value then
  begin
    FWarningLow := Value;
    InvalidateBackground;
  Redraw;
  Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetCriticalLow(Value: Single);
begin
  if FCriticalLow <> Value then
  begin
    FCriticalLow := Value;
    InvalidateBackground;
  Redraw;
  Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetShowWarningIndicator(Value: Boolean);
begin
  if FShowWarningIndicator <> Value then
  begin
    FShowWarningIndicator := Value;
    Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetShowDigitalDisplay(Value: Boolean);
begin
  if FShowDigitalDisplay <> Value then
  begin
    FShowDigitalDisplay := Value;
    Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetShowStatusText(Value: Boolean);
begin
  if FShowStatusText <> Value then
  begin
    FShowStatusText := Value;
    Invalidate;
  end;
end;

procedure TOBDVoltageMonitor.SetColors(Value: TOBDVoltageMonitorColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDVoltageMonitor.SetAnimation(Value: TOBDVoltageMonitorAnimation);
begin
  FAnimation.Assign(Value);
end;

function TOBDVoltageMonitor.GetVoltageStatus: TOBDVoltageStatus;
begin
  if FAnimationValue < FCriticalLow then
    Result := vsCritical
  else if FAnimationValue < FWarningLow then
    Result := vsLow
  else if (FAnimationValue >= FNormalMin) and (FAnimationValue <= FNormalMax) then
    Result := vsNormal
  else if FAnimationValue > FNormalMax then
    Result := vsHigh
  else
    Result := vsUnknown;
end;

function TOBDVoltageMonitor.GetStatusColor: TColor;
begin
  case GetVoltageStatus of
    vsCritical: Result := FColors.Critical;
    vsLow:      Result := FColors.Low;
    vsNormal:   Result := FColors.Normal;
    vsHigh:     Result := FColors.High;
  else
    Result := FColors.Text;
  end;
end;

function TOBDVoltageMonitor.GetStatusText: string;
begin
  case GetVoltageStatus of
    vsCritical: Result := 'CRITICAL - DO NOT FLASH';
    vsLow:      Result := 'LOW - CHARGING RECOMMENDED';
    vsNormal:   Result := 'NORMAL - OK TO FLASH';
    vsHigh:     Result := 'HIGH - CHECK CHARGING SYSTEM';
  else
    Result := 'UNKNOWN';
  end;
end;

procedure TOBDVoltageMonitor.AnimationTick(ElapsedMs: Int64);
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
  Redraw;
  Invalidate;
end;

function TOBDVoltageMonitor.IsAnimating: Boolean;
begin
  Result := FAnimation.Enabled and (FAnimationValue <> FValue);
end;

function TOBDVoltageMonitor.GetFramesPerSecond: Integer;
begin
  Result := 60;
end;

function TOBDVoltageMonitor.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDVoltageMonitor.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  BarRect: TRectF;
  CriticalWidth, WarningWidth, NormalWidth, HighWidth: Single;
  Font: ISkFont;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  
  // Background
  Paint := TSkPaint.Create;
  Paint.Color := FColors.Background;
  Paint.Style := TSkPaintStyle.Fill;
  Canvas.DrawRect(RectF(0, 0, Width, Height), Paint);
  
  // Draw voltage scale bar
  BarRect := RectF(20, Height - 80, Width - 20, Height - 50);
  
  // Calculate zone widths
  CriticalWidth := (FCriticalLow - FMin) / (FMax - FMin) * BarRect.Width;
  WarningWidth := (FWarningLow - FCriticalLow) / (FMax - FMin) * BarRect.Width;
  NormalWidth := (FNormalMax - FWarningLow) / (FMax - FMin) * BarRect.Width;
  HighWidth := (FMax - FNormalMax) / (FMax - FMin) * BarRect.Width;
  
  // Draw zones
  Paint.Style := TSkPaintStyle.Fill;
  
  // Critical zone
  Paint.Color := FColors.Critical;
  Canvas.DrawRect(RectF(BarRect.Left, BarRect.Top, 
                       BarRect.Left + CriticalWidth, BarRect.Bottom), Paint);
  
  // Warning zone
  Paint.Color := FColors.Low;
  Canvas.DrawRect(RectF(BarRect.Left + CriticalWidth, BarRect.Top,
                       BarRect.Left + CriticalWidth + WarningWidth, BarRect.Bottom), Paint);
  
  // Normal zone
  Paint.Color := FColors.Normal;
  Canvas.DrawRect(RectF(BarRect.Left + CriticalWidth + WarningWidth, BarRect.Top,
                       BarRect.Left + CriticalWidth + WarningWidth + NormalWidth, BarRect.Bottom), Paint);
  
  // High zone
  Paint.Color := FColors.High;
  Canvas.DrawRect(RectF(BarRect.Left + CriticalWidth + WarningWidth + NormalWidth, BarRect.Top,
                       BarRect.Right, BarRect.Bottom), Paint);
  
  // Border
  Paint.Color := FColors.Border;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2;
  Canvas.DrawRect(BarRect, Paint);
  
  // Scale labels
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Normal), 10);
  Paint.Color := FColors.Text;
  Paint.Style := TSkPaintStyle.Fill;
  
  Canvas.DrawSimpleText(FormatFloat('0.0', FMin), BarRect.Left, BarRect.Bottom + 15,
                       Font, Paint, TSkTextAlign.Left);
  Canvas.DrawSimpleText(FormatFloat('0.0', FMax), BarRect.Right, BarRect.Bottom + 15,
                       Font, Paint, TSkTextAlign.Right);
  
  // Title
  Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 14);
  Canvas.DrawSimpleText('BATTERY VOLTAGE', Width / 2, 20,
                       Font, Paint, TSkTextAlign.Center);
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

procedure TOBDVoltageMonitor.PaintSkia(Canvas: ISkCanvas);
var
  Background: ISkImage;
  Paint: ISkPaint;
  BarRect: TRectF;
  IndicatorX: Single;
  Font: ISkFont;
  StatusColor: TColor;
begin
  Background := AcquireBackgroundSnapshot;
  if Assigned(Background) then
    Canvas.DrawImage(Background, 0, 0);
  
  BarRect := RectF(20, Height - 80, Width - 20, Height - 50);
  
  // Draw voltage indicator
  Paint := TSkPaint.Create;
  StatusColor := GetStatusColor;
  Paint.Color := StatusColor;
  Paint.Style := TSkPaintStyle.Fill;
  
  IndicatorX := BarRect.Left + (FAnimationValue - FMin) / (FMax - FMin) * BarRect.Width;
  Canvas.DrawRect(RectF(IndicatorX - 3, BarRect.Top - 10, IndicatorX + 3, BarRect.Bottom + 10), Paint);
  
  // Draw digital display
  if FShowDigitalDisplay then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 48);
    Paint.Color := StatusColor;
    Canvas.DrawSimpleText(FormatFloat('0.00', FAnimationValue) + 'V', Width / 2, Height / 2 - 20,
                         Font, Paint, TSkTextAlign.Center);
  end;
  
  // Draw status text
  if FShowStatusText then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeFromName('Segoe UI', TSkFontStyle.Bold), 12);
    Paint.Color := StatusColor;
    Canvas.DrawSimpleText(GetStatusText, Width / 2, Height / 2 + 20,
                         Font, Paint, TSkTextAlign.Center);
  end;
  
  // Draw warning indicator
  if FShowWarningIndicator and (GetVoltageStatus in [vsCritical, vsLow]) then
  begin
    Paint.Color := FColors.Critical;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 3;
    Canvas.DrawRect(RectF(5, 5, Width - 5, Height - 5), Paint);
  end;
end;

end.
