//------------------------------------------------------------------------------
// UNIT           : OBD.ConnectorIndicator.pas
// CONTENTS       : OBD-II 16-pin connector indicator with Skia rendering  
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.ConnectorIndicator;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Diagnostics, System.Types,
  System.UITypes, System.Generics.Collections, Vcl.Controls, WinApi.Windows, 
  Winapi.Messages, Vcl.Graphics, Vcl.Themes, System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Animation,
  OBD.CustomControl.AnimationManager;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  MARGIN_FROM_BORDER = 4;
  DEFAULT_PIN_SIZE = 20;
  DEFAULT_PIN_SPACING = 4;
  DEFAULT_CORNER_RADIUS = 8;
  
  DEFAULT_CONNECTOR_COLOR = $00404040;
  DEFAULT_PIN_INACTIVE_COLOR = $00808080;
  DEFAULT_PIN_ACTIVE_COLOR = $0000FF00;
  DEFAULT_PIN_SIGNAL_COLOR = $000080FF;
  DEFAULT_PIN_POWER_COLOR = $000000FF;
  
  DEFAULT_SHOW_LABELS = True;
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 500;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD-II pin state
  /// </summary>
  TOBDPinState = (psInactive, psActive, psSignal, psPower);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD-II connector pin information
  /// </summary>
  TOBDConnectorPin = class(TPersistent)
  private
    FPinNumber: Integer;
    FState: TOBDPinState;
    FLabel: string;
    FDescription: string;
    FOnChange: TNotifyEvent;
    procedure SetState(Value: TOBDPinState);
    procedure SetLabel(const Value: string);
    procedure SetDescription(const Value: string);
  public
    constructor Create(APinNumber: Integer); virtual;
    procedure Assign(Source: TPersistent); override;
    property PinNumber: Integer read FPinNumber;
  published
    property State: TOBDPinState read FState write SetState default psInactive;
    property &Label: string read FLabel write SetLabel;
    property Description: string read FDescription write SetDescription;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   OBD-II connector colors
  /// </summary>
  TOBDConnectorColors = class(TPersistent)
  private
    FConnectorColor: TColor;
    FPinInactiveColor: TColor;
    FPinActiveColor: TColor;
    FPinSignalColor: TColor;
    FPinPowerColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetConnectorColor(Value: TColor);
    procedure SetPinInactiveColor(Value: TColor);
    procedure SetPinActiveColor(Value: TColor);
    procedure SetPinSignalColor(Value: TColor);
    procedure SetPinPowerColor(Value: TColor);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property ConnectorColor: TColor read FConnectorColor write SetConnectorColor default DEFAULT_CONNECTOR_COLOR;
    property PinInactiveColor: TColor read FPinInactiveColor write SetPinInactiveColor default DEFAULT_PIN_INACTIVE_COLOR;
    property PinActiveColor: TColor read FPinActiveColor write SetPinActiveColor default DEFAULT_PIN_ACTIVE_COLOR;
    property PinSignalColor: TColor read FPinSignalColor write SetPinSignalColor default DEFAULT_PIN_SIGNAL_COLOR;
    property PinPowerColor: TColor read FPinPowerColor write SetPinPowerColor default DEFAULT_PIN_POWER_COLOR;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   OBD-II connector animation
  /// </summary>
  TOBDConnectorAnimation = class(TPersistent)
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
  ///   OBD-II 16-pin connector indicator component
  /// </summary>
  TOBDConnectorIndicator = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    
    FPins: TObjectList<TOBDConnectorPin>;
    FPinSize: Integer;
    FPinSpacing: Integer;
    FCornerRadius: Integer;
    FShowLabels: Boolean;
    
    FColors: TOBDConnectorColors;
    FAnimation: TOBDConnectorAnimation;
    
    FHoveredPin: Integer;
    FAnimationProgress: TDictionary<Integer, Single>;
    FAnimationStartTime: TDictionary<Integer, Int64>;
    
    function GetPin(Index: Integer): TOBDConnectorPin;
    procedure SetPinSize(Value: Integer);
    procedure SetPinSpacing(Value: Integer);
    procedure SetCornerRadius(Value: Integer);
    procedure SetShowLabels(Value: Boolean);
    procedure SetColors(Value: TOBDConnectorColors);
    procedure SetAnimation(Value: TOBDConnectorAnimation);
  protected
    procedure InitializePins;
    procedure InvalidateBackground; virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    procedure DrawConnectorShape(Canvas: ISkCanvas; Rect: TRectF);
    procedure DrawPin(Canvas: ISkCanvas; X, Y: Single; Pin: TOBDConnectorPin; Alpha: Single = 1.0);
    function GetPinRect(PinNumber: Integer): TRectF;
    function GetPinColor(Pin: TOBDConnectorPin): TColor;
    procedure PinChanged(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure AnimationChanged(Sender: TObject);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
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
    
    // Pin access
    property Pins[Index: Integer]: TOBDConnectorPin read GetPin;
    procedure SetPinState(PinNumber: Integer; State: TOBDPinState);
    procedure SetAllPinsState(State: TOBDPinState);
  published
    property PinSize: Integer read FPinSize write SetPinSize default DEFAULT_PIN_SIZE;
    property PinSpacing: Integer read FPinSpacing write SetPinSpacing default DEFAULT_PIN_SPACING;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default DEFAULT_CORNER_RADIUS;
    property ShowLabels: Boolean read FShowLabels write SetShowLabels default DEFAULT_SHOW_LABELS;
    property Colors: TOBDConnectorColors read FColors write SetColors;
    property Animation: TOBDConnectorAnimation read FAnimation write SetAnimation;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDConnectorPin Implementation
//------------------------------------------------------------------------------

constructor TOBDConnectorPin.Create(APinNumber: Integer);
begin
  inherited Create;
  FPinNumber := APinNumber;
  FState := psInactive;
  
  // Set default labels based on standard OBD-II pinout
  case APinNumber of
    2: begin FLabel := 'J1850+'; FDescription := 'J1850 Bus+'; end;
    4: begin FLabel := 'GND'; FDescription := 'Chassis ground'; end;
    5: begin FLabel := 'GND'; FDescription := 'Signal ground'; end;
    6: begin FLabel := 'CAN H'; FDescription := 'CAN High'; end;
    7: begin FLabel := 'K'; FDescription := 'K-Line'; end;
    10: begin FLabel := 'J1850-'; FDescription := 'J1850 Bus-'; end;
    14: begin FLabel := 'CAN L'; FDescription := 'CAN Low'; end;
    15: begin FLabel := 'L'; FDescription := 'L-Line'; end;
    16: begin FLabel := '+12V'; FDescription := 'Battery positive'; end;
  else
    FLabel := IntToStr(APinNumber);
    FDescription := 'Manufacturer discretion';
  end;
end;

procedure TOBDConnectorPin.SetState(Value: TOBDPinState);
begin
  if FState <> Value then
  begin
    FState := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorPin.SetLabel(const Value: string);
begin
  if FLabel <> Value then
  begin
    FLabel := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorPin.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorPin.Assign(Source: TPersistent);
begin
  if Source is TOBDConnectorPin then
  begin
    FState := (Source as TOBDConnectorPin).State;
    FLabel := (Source as TOBDConnectorPin).&Label;
    FDescription := (Source as TOBDConnectorPin).Description;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDConnectorColors Implementation
//------------------------------------------------------------------------------

constructor TOBDConnectorColors.Create;
begin
  inherited Create;
  FConnectorColor := DEFAULT_CONNECTOR_COLOR;
  FPinInactiveColor := DEFAULT_PIN_INACTIVE_COLOR;
  FPinActiveColor := DEFAULT_PIN_ACTIVE_COLOR;
  FPinSignalColor := DEFAULT_PIN_SIGNAL_COLOR;
  FPinPowerColor := DEFAULT_PIN_POWER_COLOR;
end;

procedure TOBDConnectorColors.SetConnectorColor(Value: TColor);
begin
  if FConnectorColor <> Value then
  begin
    FConnectorColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorColors.SetPinInactiveColor(Value: TColor);
begin
  if FPinInactiveColor <> Value then
  begin
    FPinInactiveColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorColors.SetPinActiveColor(Value: TColor);
begin
  if FPinActiveColor <> Value then
  begin
    FPinActiveColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorColors.SetPinSignalColor(Value: TColor);
begin
  if FPinSignalColor <> Value then
  begin
    FPinSignalColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorColors.SetPinPowerColor(Value: TColor);
begin
  if FPinPowerColor <> Value then
  begin
    FPinPowerColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorColors.Assign(Source: TPersistent);
begin
  if Source is TOBDConnectorColors then
  begin
    FConnectorColor := (Source as TOBDConnectorColors).ConnectorColor;
    FPinInactiveColor := (Source as TOBDConnectorColors).PinInactiveColor;
    FPinActiveColor := (Source as TOBDConnectorColors).PinActiveColor;
    FPinSignalColor := (Source as TOBDConnectorColors).PinSignalColor;
    FPinPowerColor := (Source as TOBDConnectorColors).PinPowerColor;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDConnectorAnimation Implementation
//------------------------------------------------------------------------------

constructor TOBDConnectorAnimation.Create;
begin
  inherited Create;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FType := anQuartEaseOut;
end;

procedure TOBDConnectorAnimation.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorAnimation.SetDuration(Value: Cardinal);
begin
  if (FDuration <> Value) and (Value >= 10) then
  begin
    FDuration := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorAnimation.SetType(Value: TOBDCustomControlAnimationType);
begin
  if FType <> Value then
  begin
    FType := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDConnectorAnimation.Assign(Source: TPersistent);
begin
  if Source is TOBDConnectorAnimation then
  begin
    FEnabled := (Source as TOBDConnectorAnimation).Enabled;
    FDuration := (Source as TOBDConnectorAnimation).Duration;
    FType := (Source as TOBDConnectorAnimation).&Type;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDConnectorIndicator Implementation
//------------------------------------------------------------------------------

constructor TOBDConnectorIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FPins := TObjectList<TOBDConnectorPin>.Create(True);
  FAnimationProgress := TDictionary<Integer, Single>.Create;
  FAnimationStartTime := TDictionary<Integer, Int64>.Create;
  
  FPinSize := DEFAULT_PIN_SIZE;
  FPinSpacing := DEFAULT_PIN_SPACING;
  FCornerRadius := DEFAULT_CORNER_RADIUS;
  FShowLabels := DEFAULT_SHOW_LABELS;
  FHoveredPin := -1;
  
  FColors := TOBDConnectorColors.Create;
  FColors.OnChange := SettingsChanged;
  
  FAnimation := TOBDConnectorAnimation.Create;
  FAnimation.OnChange := AnimationChanged;
  
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
  
  InitializePins;
  
  if not (csDesigning in ComponentState) then
    AnimationManager.RegisterControl(Self);
  
  Width := 250;
  Height := 150;
end;

destructor TOBDConnectorIndicator.Destroy;
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.UnregisterControl(Self);
  FRenderLock.Free;
  FPins.Free;
  FAnimationProgress.Free;
  FAnimationStartTime.Free;
  FColors.Free;
  FAnimation.Free;
  inherited Destroy;
end;

procedure TOBDConnectorIndicator.InitializePins;
var
  I: Integer;
  Pin: TOBDConnectorPin;
begin
  FPins.Clear;
  for I := 1 to 16 do
  begin
    Pin := TOBDConnectorPin.Create(I);
    Pin.OnChange := PinChanged;
    FPins.Add(Pin);
    FAnimationProgress.Add(I, 1.0);
  end;
end;

function TOBDConnectorIndicator.GetPin(Index: Integer): TOBDConnectorPin;
begin
  if (Index >= 1) and (Index <= 16) then
    Result := FPins[Index - 1]
  else
    raise Exception.CreateFmt('Invalid pin number: %d (must be 1-16)', [Index]);
end;

procedure TOBDConnectorIndicator.SetPinState(PinNumber: Integer; State: TOBDPinState);
begin
  if (PinNumber >= 1) and (PinNumber <= 16) then
  begin
    Pins[PinNumber].State := State;
    if FAnimation.Enabled and not (csDesigning in ComponentState) then
    begin
      FAnimationStartTime.AddOrSetValue(PinNumber, FStopwatch.ElapsedMilliseconds);
      FAnimationProgress.AddOrSetValue(PinNumber, 0.0);
      AnimationManager.CheckAnimationState;
    end;
  end;
end;

procedure TOBDConnectorIndicator.SetAllPinsState(State: TOBDPinState);
var
  I: Integer;
begin
  for I := 1 to 16 do
    SetPinState(I, State);
end;

procedure TOBDConnectorIndicator.SetPinSize(Value: Integer);
begin
  if (FPinSize <> Value) and (Value >= 10) and (Value <= 50) then
  begin
    FPinSize := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectorIndicator.SetPinSpacing(Value: Integer);
begin
  if (FPinSpacing <> Value) and (Value >= 0) then
  begin
    FPinSpacing := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectorIndicator.SetCornerRadius(Value: Integer);
begin
  if (FCornerRadius <> Value) and (Value >= 0) then
  begin
    FCornerRadius := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectorIndicator.SetShowLabels(Value: Boolean);
begin
  if FShowLabels <> Value then
  begin
    FShowLabels := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDConnectorIndicator.SetColors(Value: TOBDConnectorColors);
begin
  FColors.Assign(Value);
end;

procedure TOBDConnectorIndicator.SetAnimation(Value: TOBDConnectorAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TOBDConnectorIndicator.InvalidateBackground;
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

procedure TOBDConnectorIndicator.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  ConnectorRect: TRectF;
  PinWidth, PinHeight: Single;
begin
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  Canvas.Clear(SafeColorRefToSkColor(FColors.ConnectorColor));
  
  // Calculate connector dimensions
  PinWidth := (8 * FPinSize + 7 * FPinSpacing);
  PinHeight := (2 * FPinSize + FPinSpacing);
  
  ConnectorRect := TRectF.Create(
    (Width - PinWidth) / 2 - MARGIN_FROM_BORDER * 2,
    (Height - PinHeight) / 2 - MARGIN_FROM_BORDER * 2,
    (Width + PinWidth) / 2 + MARGIN_FROM_BORDER * 2,
    (Height + PinHeight) / 2 + MARGIN_FROM_BORDER * 2
  );
  
  // Draw connector shape
  DrawConnectorShape(Canvas, ConnectorRect);
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

procedure TOBDConnectorIndicator.DrawConnectorShape(Canvas: ISkCanvas; Rect: TRectF);
var
  Paint: ISkPaint;
begin
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FColors.ConnectorColor);
  Canvas.DrawRoundRect(Rect, FCornerRadius, FCornerRadius, Paint);
  
  // Draw border
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 2;
  Paint.Color := SafeColorRefToSkColor(clBlack);
  Canvas.DrawRoundRect(Rect, FCornerRadius, FCornerRadius, Paint);
end;

function TOBDConnectorIndicator.GetPinColor(Pin: TOBDConnectorPin): TColor;
begin
  case Pin.State of
    psActive: Result := FColors.PinActiveColor;
    psSignal: Result := FColors.PinSignalColor;
    psPower: Result := FColors.PinPowerColor;
  else
    Result := FColors.PinInactiveColor;
  end;
end;

function TOBDConnectorIndicator.GetPinRect(PinNumber: Integer): TRectF;
var
  Row, Col: Integer;
  X, Y, PinWidth, PinHeight: Single;
begin
  PinWidth := 8 * FPinSize + 7 * FPinSpacing;
  PinHeight := 2 * FPinSize + FPinSpacing;
  
  // OBD-II connector layout: Row 1 = pins 1-8, Row 2 = pins 9-16
  if PinNumber <= 8 then
  begin
    Row := 0;
    Col := PinNumber - 1;
  end
  else
  begin
    Row := 1;
    Col := PinNumber - 9;
  end;
  
  X := (Width - PinWidth) / 2 + Col * (FPinSize + FPinSpacing);
  Y := (Height - PinHeight) / 2 + Row * (FPinSize + FPinSpacing);
  
  Result := TRectF.Create(X, Y, X + FPinSize, Y + FPinSize);
end;

procedure TOBDConnectorIndicator.DrawPin(Canvas: ISkCanvas; X, Y: Single; 
  Pin: TOBDConnectorPin; Alpha: Single);
var
  Paint: ISkPaint;
  Font: ISkFont;
  PinColor: TColor;
  AlphaValue: Byte;
  PinRect: TRectF;
begin
  PinRect := TRectF.Create(X, Y, X + FPinSize, Y + FPinSize);
  
  // Draw pin circle
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  
  PinColor := GetPinColor(Pin);
  AlphaValue := Round(Alpha * 255);
  Paint.Color := SafeColorRefToSkColor(PinColor) and $00FFFFFF or (AlphaValue shl 24);
  
  Canvas.DrawCircle(PinRect.CenterPoint.X, PinRect.CenterPoint.Y, FPinSize / 2, Paint);
  
  // Draw pin border
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color := SafeColorRefToSkColor(clBlack) and $00FFFFFF or (AlphaValue shl 24);
  Canvas.DrawCircle(PinRect.CenterPoint.X, PinRect.CenterPoint.Y, FPinSize / 2, Paint);
  
  // Draw pin label
  if FShowLabels and (Pin.&Label <> '') then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeDefault, 8);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(clWhite) and $00FFFFFF or (AlphaValue shl 24);
    Canvas.DrawSimpleText(Pin.&Label, PinRect.CenterPoint.X, PinRect.CenterPoint.Y + 3, Font, Paint);
  end;
end;

function TOBDConnectorIndicator.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDConnectorIndicator.PaintSkia(Canvas: ISkCanvas);
var
  BackgroundImage: ISkImage;
  I: Integer;
  Pin: TOBDConnectorPin;
  PinRect: TRectF;
  Alpha: Single;
begin
  try
    BackgroundImage := AcquireBackgroundSnapshot;
    if Assigned(BackgroundImage) then
      Canvas.DrawImage(BackgroundImage, 0, 0)
    else
      Canvas.Clear(SafeColorRefToSkColor(FColors.ConnectorColor));
    
    // Draw all pins
    for I := 0 to FPins.Count - 1 do
    begin
      Pin := FPins[I];
      PinRect := GetPinRect(Pin.PinNumber);
      
      // Get animation alpha
      if FAnimationProgress.ContainsKey(Pin.PinNumber) then
        Alpha := FAnimationProgress[Pin.PinNumber]
      else
        Alpha := 1.0;
      
      DrawPin(Canvas, PinRect.Left, PinRect.Top, Pin, Alpha);
    end;
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(FColors.ConnectorColor));
  end;
end;

procedure TOBDConnectorIndicator.AnimationTick(ElapsedMs: Int64);
var
  I: Integer;
  Pin: TOBDConnectorPin;
  CurrentMs, Elapsed: Int64;
  Progress: Single;
  EasingFunction: TOBDCustomControlAnimationEasingFunction;
begin
  if not FAnimation.Enabled then
    Exit;
  
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  EasingFunction := GetEasingFunction(FAnimation.&Type);
  
  for I := 0 to FPins.Count - 1 do
  begin
    Pin := FPins[I];
    if FAnimationStartTime.ContainsKey(Pin.PinNumber) then
    begin
      Elapsed := CurrentMs - FAnimationStartTime[Pin.PinNumber];
      
      if Elapsed < FAnimation.Duration then
      begin
        Progress := Elapsed / FAnimation.Duration;
        Progress := EasingFunction(Progress);
        FAnimationProgress.AddOrSetValue(Pin.PinNumber, Progress);
      end
      else
      begin
        FAnimationProgress.AddOrSetValue(Pin.PinNumber, 1.0);
        FAnimationStartTime.Remove(Pin.PinNumber);
      end;
    end;
  end;
  
  Redraw;
  Invalidate;
end;

function TOBDConnectorIndicator.IsAnimating: Boolean;
begin
  if not FAnimation.Enabled then
    Exit(False);
  Result := FAnimationStartTime.Count > 0;
end;

function TOBDConnectorIndicator.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

procedure TOBDConnectorIndicator.PinChanged(Sender: TObject);
begin
  Redraw;
  Invalidate;
end;

procedure TOBDConnectorIndicator.SettingsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDConnectorIndicator.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.CheckAnimationState;
  Redraw;
  Invalidate;
end;

procedure TOBDConnectorIndicator.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  PinRect: TRectF;
  Point: TPointF;
begin
  inherited;
  Point := PointF(X, Y);
  FHoveredPin := -1;
  
  for I := 1 to 16 do
  begin
    PinRect := GetPinRect(I);
    if PinRect.Contains(Point) then
    begin
      FHoveredPin := I;
      Hint := Format('Pin %d: %s', [I, Pins[I].Description]);
      Break;
    end;
  end;
end;

procedure TOBDConnectorIndicator.MouseLeave;
begin
  inherited;
  FHoveredPin := -1;
end;

procedure TOBDConnectorIndicator.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDConnectorIndicator.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDConnectorIndicator.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;
  if Source is TOBDConnectorIndicator then
  begin
    FPinSize := (Source as TOBDConnectorIndicator).PinSize;
    FPinSpacing := (Source as TOBDConnectorIndicator).PinSpacing;
    FCornerRadius := (Source as TOBDConnectorIndicator).CornerRadius;
    FShowLabels := (Source as TOBDConnectorIndicator).ShowLabels;
    FColors.Assign((Source as TOBDConnectorIndicator).Colors);
    FAnimation.Assign((Source as TOBDConnectorIndicator).Animation);
    
    for I := 0 to 15 do
      FPins[I].Assign((Source as TOBDConnectorIndicator).FPins[I]);
  end;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

end.
