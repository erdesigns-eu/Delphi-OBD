//------------------------------------------------------------------------------
// UNIT           : OBD.SevenSegmentDisplay.pas
// CONTENTS       : Seven-segment LED display component with Skia rendering
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.SevenSegmentDisplay;

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
  DEFAULT_DIGIT_COUNT = 4;
  DEFAULT_DIGIT_SPACING = 8;
  DEFAULT_SEGMENT_WIDTH = 8;
  DEFAULT_SEGMENT_SPACING = 2;
  
  DEFAULT_ON_COLOR = $000080FF;        // Bright blue
  DEFAULT_OFF_COLOR = $00202020;       // Dark gray
  DEFAULT_BACKGROUND_COLOR = $00000000; // Black
  
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 300;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Seven segment display style
  /// </summary>
  TSegmentStyle = (ssClassic, ssModern, ssItalic);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Seven segment display color properties
  /// </summary>
  TOBDSevenSegmentColors = class(TPersistent)
  private
    FOnColor: TColor;
    FOffColor: TColor;
    FBackgroundColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetOnColor(Value: TColor);
    procedure SetOffColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property OnColor: TColor read FOnColor write SetOnColor default DEFAULT_ON_COLOR;
    property OffColor: TColor read FOffColor write SetOffColor default DEFAULT_OFF_COLOR;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default DEFAULT_BACKGROUND_COLOR;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Seven segment display animation properties
  /// </summary>
  TOBDSevenSegmentAnimation = class(TPersistent)
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
  ///   Seven-segment LED display component
  /// </summary>
  TOBDSevenSegmentDisplay = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    
    FDigitCount: Integer;
    FValue: string;
    FDigitSpacing: Integer;
    FSegmentWidth: Integer;
    FSegmentSpacing: Integer;
    FStyle: TSegmentStyle;
    FShowLeadingZeros: Boolean;
    FDecimalPlaces: Integer;
    
    FColors: TOBDSevenSegmentColors;
    FAnimation: TOBDSevenSegmentAnimation;
    
    FAnimationValue: string;
    FAnimationStartValue: string;
    FAnimationProgress: Single;
    
    procedure SetDigitCount(Value: Integer);
    procedure SetValue(const Value: string);
    procedure SetDigitSpacing(Value: Integer);
    procedure SetSegmentWidth(Value: Integer);
    procedure SetSegmentSpacing(Value: Integer);
    procedure SetStyle(Value: TSegmentStyle);
    procedure SetShowLeadingZeros(Value: Boolean);
    procedure SetDecimalPlaces(Value: Integer);
    procedure SetColors(Value: TOBDSevenSegmentColors);
    procedure SetAnimation(Value: TOBDSevenSegmentAnimation);
  protected
    procedure InvalidateBackground; virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    procedure DrawSegment(Canvas: ISkCanvas; X, Y, Width, Height: Single; 
      Horizontal: Boolean; IsOn: Boolean; Alpha: Single = 1.0);
    procedure DrawDigit(Canvas: ISkCanvas; X, Y, Width, Height: Single; 
      Digit: Char; Alpha: Single = 1.0);
    function GetSegmentPattern(Digit: Char): Byte;
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
    
    // Utility methods
    procedure SetNumericValue(const Value: Extended);
    procedure SetIntegerValue(const Value: Integer);
  published
    property DigitCount: Integer read FDigitCount write SetDigitCount default DEFAULT_DIGIT_COUNT;
    property Value: string read FValue write SetValue;
    property DigitSpacing: Integer read FDigitSpacing write SetDigitSpacing default DEFAULT_DIGIT_SPACING;
    property SegmentWidth: Integer read FSegmentWidth write SetSegmentWidth default DEFAULT_SEGMENT_WIDTH;
    property SegmentSpacing: Integer read FSegmentSpacing write SetSegmentSpacing default DEFAULT_SEGMENT_SPACING;
    property Style: TSegmentStyle read FStyle write SetStyle default ssClassic;
    property ShowLeadingZeros: Boolean read FShowLeadingZeros write SetShowLeadingZeros default False;
    property DecimalPlaces: Integer read FDecimalPlaces write SetDecimalPlaces default 0;
    property Colors: TOBDSevenSegmentColors read FColors write SetColors;
    property Animation: TOBDSevenSegmentAnimation read FAnimation write SetAnimation;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDSevenSegmentColors Implementation
//------------------------------------------------------------------------------

constructor TOBDSevenSegmentColors.Create;
begin
  inherited Create;
  FOnColor := DEFAULT_ON_COLOR;
  FOffColor := DEFAULT_OFF_COLOR;
  FBackgroundColor := DEFAULT_BACKGROUND_COLOR;
end;

procedure TOBDSevenSegmentColors.SetOnColor(Value: TColor);
begin
  if FOnColor <> Value then
  begin
    FOnColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDSevenSegmentColors.SetOffColor(Value: TColor);
begin
  if FOffColor <> Value then
  begin
    FOffColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDSevenSegmentColors.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDSevenSegmentColors.Assign(Source: TPersistent);
begin
  if Source is TOBDSevenSegmentColors then
  begin
    FOnColor := (Source as TOBDSevenSegmentColors).OnColor;
    FOffColor := (Source as TOBDSevenSegmentColors).OffColor;
    FBackgroundColor := (Source as TOBDSevenSegmentColors).BackgroundColor;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDSevenSegmentAnimation Implementation
//------------------------------------------------------------------------------

constructor TOBDSevenSegmentAnimation.Create;
begin
  inherited Create;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FType := anQuartEaseOut;
end;

procedure TOBDSevenSegmentAnimation.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDSevenSegmentAnimation.SetDuration(Value: Cardinal);
begin
  if (FDuration <> Value) and (Value >= 10) then
  begin
    FDuration := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDSevenSegmentAnimation.SetType(Value: TOBDCustomControlAnimationType);
begin
  if FType <> Value then
  begin
    FType := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDSevenSegmentAnimation.Assign(Source: TPersistent);
begin
  if Source is TOBDSevenSegmentAnimation then
  begin
    FEnabled := (Source as TOBDSevenSegmentAnimation).Enabled;
    FDuration := (Source as TOBDSevenSegmentAnimation).Duration;
    FType := (Source as TOBDSevenSegmentAnimation).&Type;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDSevenSegmentDisplay Implementation
//------------------------------------------------------------------------------

constructor TOBDSevenSegmentDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FDigitCount := DEFAULT_DIGIT_COUNT;
  FValue := '0';
  FDigitSpacing := DEFAULT_DIGIT_SPACING;
  FSegmentWidth := DEFAULT_SEGMENT_WIDTH;
  FSegmentSpacing := DEFAULT_SEGMENT_SPACING;
  FStyle := ssClassic;
  FShowLeadingZeros := False;
  FDecimalPlaces := 0;
  
  FColors := TOBDSevenSegmentColors.Create;
  FColors.OnChange := SettingsChanged;
  
  FAnimation := TOBDSevenSegmentAnimation.Create;
  FAnimation.OnChange := AnimationChanged;
  
  FAnimationValue := FValue;
  FAnimationStartValue := FValue;
  FAnimationProgress := 1.0;
  
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
  
  if not (csDesigning in ComponentState) then
    AnimationManager.RegisterControl(Self);
  
  Width := 200;
  Height := 60;
end;

destructor TOBDSevenSegmentDisplay.Destroy;
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.UnregisterControl(Self);
  FRenderLock.Free;
  FColors.Free;
  FAnimation.Free;
  inherited Destroy;
end;

procedure TOBDSevenSegmentDisplay.SetDigitCount(Value: Integer);
begin
  if (FDigitCount <> Value) and (Value >= 1) and (Value <= 16) then
  begin
    FDigitCount := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    
    if FAnimation.Enabled and not (csDesigning in ComponentState) then
    begin
      FAnimationStartValue := FAnimationValue;
      FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
      FAnimationProgress := 0.0;
      AnimationManager.CheckAnimationState;
    end
    else
      FAnimationValue := FValue;
    
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetDigitSpacing(Value: Integer);
begin
  if (FDigitSpacing <> Value) and (Value >= 0) then
  begin
    FDigitSpacing := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetSegmentWidth(Value: Integer);
begin
  if (FSegmentWidth <> Value) and (Value >= 2) then
  begin
    FSegmentWidth := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetSegmentSpacing(Value: Integer);
begin
  if (FSegmentSpacing <> Value) and (Value >= 0) then
  begin
    FSegmentSpacing := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetStyle(Value: TSegmentStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetShowLeadingZeros(Value: Boolean);
begin
  if FShowLeadingZeros <> Value then
  begin
    FShowLeadingZeros := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetDecimalPlaces(Value: Integer);
begin
  if (FDecimalPlaces <> Value) and (Value >= 0) and (Value <= 4) then
  begin
    FDecimalPlaces := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDSevenSegmentDisplay.SetColors(Value: TOBDSevenSegmentColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDSevenSegmentDisplay.SetAnimation(Value: TOBDSevenSegmentAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TOBDSevenSegmentDisplay.SetNumericValue(const Value: Extended);
var
  FormatStr: string;
begin
  if FDecimalPlaces > 0 then
    FormatStr := '%.' + IntToStr(FDecimalPlaces) + 'f'
  else
    FormatStr := '%.0f';
  SetValue(Format(FormatStr, [Value]));
end;

procedure TOBDSevenSegmentDisplay.SetIntegerValue(const Value: Integer);
begin
  SetValue(IntToStr(Value));
end;

function TOBDSevenSegmentDisplay.GetSegmentPattern(Digit: Char): Byte;
begin
  // 7-segment encoding: gfedcba (bit 0 = segment a, bit 6 = segment g)
  case Digit of
    '0': Result := $3F; // abcdef
    '1': Result := $06; // bc
    '2': Result := $5B; // abdeg
    '3': Result := $4F; // abcdg
    '4': Result := $66; // bcfg
    '5': Result := $6D; // acdfg
    '6': Result := $7D; // acdefg
    '7': Result := $07; // abc
    '8': Result := $7F; // abcdefg
    '9': Result := $6F; // abcdfg
    '-': Result := $40; // g only
    ' ': Result := $00; // all off
  else
    Result := $00;
  end;
end;

procedure TOBDSevenSegmentDisplay.DrawSegment(Canvas: ISkCanvas; X, Y, Width, Height: Single;
  Horizontal: Boolean; IsOn: Boolean; Alpha: Single);
var
  Paint: ISkPaint;
  Path: ISkPathBuilder;
  SegColor: TColor;
  AlphaValue: Byte;
begin
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  
  if IsOn then
    SegColor := FColors.OnColor
  else
    SegColor := FColors.OffColor;
  
  AlphaValue := Round(Alpha * 255);
  Paint.Color := SafeColorRefToSkColor(SegColor) and $00FFFFFF or (AlphaValue shl 24);
  
  Path := TSkPathBuilder.Create;
  
  if Horizontal then
  begin
    // Horizontal segment
    Path.MoveTo(PointF(X + FSegmentWidth/2, Y));
    Path.LineTo(PointF(X + Width - FSegmentWidth/2, Y));
    Path.LineTo(PointF(X + Width, Y + FSegmentWidth/2));
    Path.LineTo(PointF(X + Width - FSegmentWidth/2, Y + FSegmentWidth));
    Path.LineTo(PointF(X + FSegmentWidth/2, Y + FSegmentWidth));
    Path.LineTo(PointF(X, Y + FSegmentWidth/2));
  end
  else
  begin
    // Vertical segment
    Path.MoveTo(PointF(X + FSegmentWidth/2, Y));
    Path.LineTo(PointF(X + FSegmentWidth, Y + FSegmentWidth/2));
    Path.LineTo(PointF(X + FSegmentWidth, Y + Height - FSegmentWidth/2));
    Path.LineTo(PointF(X + FSegmentWidth/2, Y + Height));
    Path.LineTo(PointF(X, Y + Height - FSegmentWidth/2));
    Path.LineTo(PointF(X, Y + FSegmentWidth/2));
  end;
  
  Path.Close;
  Canvas.DrawPath(Path.Detach, Paint);
end;

procedure TOBDSevenSegmentDisplay.DrawDigit(Canvas: ISkCanvas; X, Y, Width, Height: Single;
  Digit: Char; Alpha: Single);
var
  Pattern: Byte;
  SegHeight, SegWidth: Single;
begin
  Pattern := GetSegmentPattern(Digit);
  SegHeight := (Height - 3 * FSegmentWidth - 2 * FSegmentSpacing) / 2;
  SegWidth := Width - 2 * FSegmentWidth;
  
  // Segment a (top)
  DrawSegment(Canvas, X + FSegmentWidth, Y, SegWidth, 0, True, (Pattern and $01) <> 0, Alpha);
  
  // Segment b (top right)
  DrawSegment(Canvas, X + Width - FSegmentWidth, Y + FSegmentWidth + FSegmentSpacing, 
    0, SegHeight, False, (Pattern and $02) <> 0, Alpha);
  
  // Segment c (bottom right)
  DrawSegment(Canvas, X + Width - FSegmentWidth, 
    Y + FSegmentWidth * 2 + FSegmentSpacing * 2 + SegHeight, 
    0, SegHeight, False, (Pattern and $04) <> 0, Alpha);
  
  // Segment d (bottom)
  DrawSegment(Canvas, X + FSegmentWidth, Y + Height - FSegmentWidth, 
    SegWidth, 0, True, (Pattern and $08) <> 0, Alpha);
  
  // Segment e (bottom left)
  DrawSegment(Canvas, X, Y + FSegmentWidth * 2 + FSegmentSpacing * 2 + SegHeight, 
    0, SegHeight, False, (Pattern and $10) <> 0, Alpha);
  
  // Segment f (top left)
  DrawSegment(Canvas, X, Y + FSegmentWidth + FSegmentSpacing, 
    0, SegHeight, False, (Pattern and $20) <> 0, Alpha);
  
  // Segment g (middle)
  DrawSegment(Canvas, X + FSegmentWidth, 
    Y + FSegmentWidth + FSegmentSpacing + SegHeight, 
    SegWidth, 0, True, (Pattern and $40) <> 0, Alpha);
end;

procedure TOBDSevenSegmentDisplay.InvalidateBackground;
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

procedure TOBDSevenSegmentDisplay.BuildBackgroundSnapshot;
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

function TOBDSevenSegmentDisplay.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDSevenSegmentDisplay.PaintSkia(Canvas: ISkCanvas);
var
  BackgroundImage: ISkImage;
  DisplayText: string;
  I, StartIdx: Integer;
  DigitWidth, DigitHeight, TotalWidth, X, Y: Single;
  CurrentAlpha, PrevAlpha: Single;
begin
  try
    BackgroundImage := AcquireBackgroundSnapshot;
    if Assigned(BackgroundImage) then
      Canvas.DrawImage(BackgroundImage, 0, 0)
    else
      Canvas.Clear(SafeColorRefToSkColor(FColors.BackgroundColor));
    
    // Prepare display text
    DisplayText := FAnimationValue;
    if FShowLeadingZeros then
      DisplayText := DisplayText.PadLeft(FDigitCount, '0')
    else
      DisplayText := DisplayText.PadLeft(FDigitCount, ' ');
    
    if DisplayText.Length > FDigitCount then
      DisplayText := DisplayText.Substring(DisplayText.Length - FDigitCount);
    
    // Calculate digit dimensions
    DigitHeight := Height - 2 * MARGIN_FROM_BORDER;
    DigitWidth := DigitHeight * 0.6; // Classic 7-segment aspect ratio
    TotalWidth := FDigitCount * DigitWidth + (FDigitCount - 1) * FDigitSpacing;
    
    X := (Width - TotalWidth) / 2;
    Y := MARGIN_FROM_BORDER;
    
    // Draw digits
    CurrentAlpha := FAnimationProgress;
    PrevAlpha := 1.0 - CurrentAlpha;
    
    for I := 1 to FDigitCount do
    begin
      if I <= DisplayText.Length then
      begin
        // Draw current digit
        DrawDigit(Canvas, X, Y, DigitWidth, DigitHeight, DisplayText[I], CurrentAlpha);
        
        // Draw previous digit (fade out) during animation
        if (FAnimationProgress < 1.0) and (I <= FAnimationStartValue.Length) then
          DrawDigit(Canvas, X, Y, DigitWidth, DigitHeight, FAnimationStartValue[I], PrevAlpha);
      end;
      X := X + DigitWidth + FDigitSpacing;
    end;
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(FColors.BackgroundColor));
  end;
end;

procedure TOBDSevenSegmentDisplay.AnimationTick(ElapsedMs: Int64);
var
  CurrentMs, Elapsed: Int64;
  Progress: Single;
  EasingFunction: TOBDCustomControlAnimationEasingFunction;
begin
  if not FAnimation.Enabled then
    Exit;
  
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  
  EasingFunction := GetEasingFunction(FAnimation.&Type);
  
  if Elapsed < FAnimation.Duration then
  begin
    Progress := Elapsed / FAnimation.Duration;
    FAnimationProgress := EasingFunction(Progress);
    FAnimationValue := FValue;
  end
  else
  begin
    FAnimationProgress := 1.0;
    FAnimationValue := FValue;
  end;
  
  Redraw;
  Invalidate;
end;

function TOBDSevenSegmentDisplay.IsAnimating: Boolean;
var
  CurrentMs, Elapsed: Int64;
begin
  if not FAnimation.Enabled then
    Exit(False);
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  Result := (Elapsed < FAnimation.Duration);
end;

function TOBDSevenSegmentDisplay.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

procedure TOBDSevenSegmentDisplay.SettingsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDSevenSegmentDisplay.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.CheckAnimationState;
  if not FAnimation.Enabled then
    FAnimationValue := FValue;
  Redraw;
  Invalidate;
end;

procedure TOBDSevenSegmentDisplay.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDSevenSegmentDisplay.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDSevenSegmentDisplay.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TOBDSevenSegmentDisplay then
  begin
    FDigitCount := (Source as TOBDSevenSegmentDisplay).DigitCount;
    FValue := (Source as TOBDSevenSegmentDisplay).Value;
    FDigitSpacing := (Source as TOBDSevenSegmentDisplay).DigitSpacing;
    FSegmentWidth := (Source as TOBDSevenSegmentDisplay).SegmentWidth;
    FSegmentSpacing := (Source as TOBDSevenSegmentDisplay).SegmentSpacing;
    FStyle := (Source as TOBDSevenSegmentDisplay).Style;
    FShowLeadingZeros := (Source as TOBDSevenSegmentDisplay).ShowLeadingZeros;
    FDecimalPlaces := (Source as TOBDSevenSegmentDisplay).DecimalPlaces;
    FColors.Assign((Source as TOBDSevenSegmentDisplay).Colors);
    FAnimation.Assign((Source as TOBDSevenSegmentDisplay).Animation);
  end;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

end.
