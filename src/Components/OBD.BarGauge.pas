//------------------------------------------------------------------------------
// UNIT           : OBD.BarGauge.pas
// CONTENTS       : Bar/Level gauge component with Skia rendering
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.BarGauge;

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
  MARGIN_FROM_BORDER = 2;
  DEFAULT_MIN = 0;
  DEFAULT_MAX = 100;
  DEFAULT_BAR_WIDTH = 40;
  DEFAULT_CORNER_RADIUS = 4;
  DEFAULT_FILL_COLOR = $0000AA00;
  DEFAULT_EMPTY_COLOR = $00D0D0D0;
  DEFAULT_BORDER_COLOR = $00808080;
  DEFAULT_BORDER_WIDTH = 2;
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 800;
  DEFAULT_ANIMATION_TYPE = anQuartEaseInOut;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  TBarOrientation = (boHorizontal, boVertical);
  TFillDirection = (fdLeftToRight, fdRightToLeft, fdBottomToTop, fdTopToBottom);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Bar Gauge fill properties
  /// </summary>
  TOBDBarGaugeFill = class(TPersistent)
  private
    FColor: TColor;
    FGradientEndColor: TColor;
    FUseGradient: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetGradientEndColor(Value: TColor);
    procedure SetUseGradient(Value: Boolean);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default DEFAULT_FILL_COLOR;
    property GradientEndColor: TColor read FGradientEndColor write SetGradientEndColor default DEFAULT_FILL_COLOR;
    property UseGradient: Boolean read FUseGradient write SetUseGradient default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Bar Gauge background properties
  /// </summary>
  TOBDBarGaugeBackground = class(TPersistent)
  private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default DEFAULT_EMPTY_COLOR;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Bar Gauge border properties
  /// </summary>
  TOBDBarGaugeBorder = class(TPersistent)
  private
    FColor: TColor;
    FWidth: Integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetWidth(Value: Integer);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default DEFAULT_BORDER_COLOR;
    property Width: Integer read FWidth write SetWidth default DEFAULT_BORDER_WIDTH;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Bar Gauge value label properties
  /// </summary>
  TOBDBarGaugeValueLabel = class(TPersistent)
  private
    FVisible: Boolean;
    FFont: TFont;
    FFormat: string;
    FOnChange: TNotifyEvent;
    procedure SetVisible(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetFormat(Value: string);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Font: TFont read FFont write SetFont;
    property Format: string read FFormat write SetFormat;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Bar Gauge zone item
  /// </summary>
  TOBDBarGaugeZoneItem = class(TCollectionItem)
  private
    FFrom: Single;
    FTo: Single;
    FColor: TColor;
    procedure SetFrom(Value: Single);
    procedure SetTo(Value: Single);
    procedure SetColor(Value: TColor);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property From: Single read FFrom write SetFrom;
    property &To: Single read FTo write SetTo;
    property Color: TColor read FColor write SetColor default clGreen;
  end;

  /// <summary>
  ///   Bar Gauge zones collection
  /// </summary>
  TOBDBarGaugeZones = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDBarGaugeZoneItem;
    procedure SetItem(Index: Integer; const Value: TOBDBarGaugeZoneItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    function Add: TOBDBarGaugeZoneItem;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TOBDBarGaugeZoneItem read GetItem write SetItem;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Bar Gauge animation properties
  /// </summary>
  TOBDBarGaugeAnimation = class(TPersistent)
  private
    FEnabled: Boolean;
    FDuration: Cardinal;
    FType: TOBDCustomControlAnimationType;
    FValue: Single;
    FStartTime: Cardinal;
    FStartValue: Single;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(Value: Boolean);
    procedure SetDuration(Value: Cardinal);
  protected
    property Value: Single read FValue write FValue;
    property StartTime: Cardinal read FStartTime write FStartTime;
    property StartValue: Single read FStartValue write FStartValue;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default DEFAULT_ANIMATION_ENABLED;
    property Duration: Cardinal read FDuration write SetDuration default DEFAULT_ANIMATION_DURATION;
    property &Type: TOBDCustomControlAnimationType read FType write FType default DEFAULT_ANIMATION_TYPE;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Bar Gauge Component
  /// </summary>
  TOBDBarGauge = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FOrientation: TBarOrientation;
    FFillDirection: TFillDirection;
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FBarWidth: Integer;
    FCornerRadius: Single;
    FFill: TOBDBarGaugeFill;
    FBackground: TOBDBarGaugeBackground;
    FBorder: TOBDBarGaugeBorder;
    FValueLabel: TOBDBarGaugeValueLabel;
    FZones: TOBDBarGaugeZones;
    FAnimation: TOBDBarGaugeAnimation;
    procedure SetOrientation(Value: TBarOrientation);
    procedure SetFillDirection(Value: TFillDirection);
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetValue(Value: Single);
    procedure SetBarWidth(Value: Integer);
    procedure SetCornerRadius(Value: Single);
    procedure SetFill(Value: TOBDBarGaugeFill);
    procedure SetBackground(Value: TOBDBarGaugeBackground);
    procedure SetBorder(Value: TOBDBarGaugeBorder);
    procedure SetValueLabel(Value: TOBDBarGaugeValueLabel);
    procedure SetZones(Value: TOBDBarGaugeZones);
    procedure SetAnimation(Value: TOBDBarGaugeAnimation);
  protected
    procedure InvalidateBackground; virtual;
    procedure PaintBar(Canvas: ISkCanvas); virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    function GetZoneColor(Value: Single): TColor;
    procedure SettingsChanged(Sender: TObject);
    procedure FillSettingsChanged(Sender: TObject);
    procedure AnimationChanged(Sender: TObject);
    procedure Resize; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // IOBDAnimatable interface
    procedure AnimationTick(ElapsedMs: Int64);
    function IsAnimating: Boolean;
    function GetFramesPerSecond: Integer;
  published
    property Orientation: TBarOrientation read FOrientation write SetOrientation default boVertical;
    property FillDirection: TFillDirection read FFillDirection write SetFillDirection default fdBottomToTop;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property BarWidth: Integer read FBarWidth write SetBarWidth default DEFAULT_BAR_WIDTH;
    property CornerRadius: Single read FCornerRadius write SetCornerRadius;
    property Fill: TOBDBarGaugeFill read FFill write SetFill;
    property Background: TOBDBarGaugeBackground read FBackground write SetBackground;
    property Border: TOBDBarGaugeBorder read FBorder write SetBorder;
    property ValueLabel: TOBDBarGaugeValueLabel read FValueLabel write SetValueLabel;
    property Zones: TOBDBarGaugeZones read FZones write SetZones;
    property Animation: TOBDBarGaugeAnimation read FAnimation write SetAnimation;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// TOBDBarGaugeFill Implementation
//------------------------------------------------------------------------------

procedure TOBDBarGaugeFill.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDBarGaugeFill.SetGradientEndColor(Value: TColor);
begin
  if (FGradientEndColor <> Value) then
  begin
    FGradientEndColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDBarGaugeFill.SetUseGradient(Value: Boolean);
begin
  if (FUseGradient <> Value) then
  begin
    FUseGradient := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDBarGaugeFill.Create;
begin
  inherited Create;
  FColor := DEFAULT_FILL_COLOR;
  FGradientEndColor := DEFAULT_FILL_COLOR;
  FUseGradient := False;
end;

procedure TOBDBarGaugeFill.Assign(Source: TPersistent);
begin
  if (Source is TOBDBarGaugeFill) then
  begin
    FColor := (Source as TOBDBarGaugeFill).Color;
    FGradientEndColor := (Source as TOBDBarGaugeFill).GradientEndColor;
    FUseGradient := (Source as TOBDBarGaugeFill).UseGradient;
    if Assigned(OnChange) then OnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDBarGaugeBackground Implementation
//------------------------------------------------------------------------------

procedure TOBDBarGaugeBackground.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDBarGaugeBackground.Create;
begin
  inherited Create;
  FColor := DEFAULT_EMPTY_COLOR;
end;

procedure TOBDBarGaugeBackground.Assign(Source: TPersistent);
begin
  if (Source is TOBDBarGaugeBackground) then
  begin
    FColor := (Source as TOBDBarGaugeBackground).Color;
    if Assigned(OnChange) then OnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDBarGaugeBorder Implementation
//------------------------------------------------------------------------------

procedure TOBDBarGaugeBorder.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDBarGaugeBorder.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    FWidth := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDBarGaugeBorder.Create;
begin
  inherited Create;
  FColor := DEFAULT_BORDER_COLOR;
  FWidth := DEFAULT_BORDER_WIDTH;
end;

procedure TOBDBarGaugeBorder.Assign(Source: TPersistent);
begin
  if (Source is TOBDBarGaugeBorder) then
  begin
    FColor := (Source as TOBDBarGaugeBorder).Color;
    FWidth := (Source as TOBDBarGaugeBorder).Width;
    if Assigned(OnChange) then OnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDBarGaugeValueLabel Implementation
//------------------------------------------------------------------------------

procedure TOBDBarGaugeValueLabel.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDBarGaugeValueLabel.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TOBDBarGaugeValueLabel.SetFormat(Value: string);
begin
  if (FFormat <> Value) then
  begin
    FFormat := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDBarGaugeValueLabel.FontChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

constructor TOBDBarGaugeValueLabel.Create;
begin
  inherited Create;
  FVisible := True;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFormat := '%.0f';
end;

destructor TOBDBarGaugeValueLabel.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TOBDBarGaugeValueLabel.Assign(Source: TPersistent);
begin
  if (Source is TOBDBarGaugeValueLabel) then
  begin
    FVisible := (Source as TOBDBarGaugeValueLabel).Visible;
    FFont.Assign((Source as TOBDBarGaugeValueLabel).Font);
    FFormat := (Source as TOBDBarGaugeValueLabel).Format;
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDBarGaugeZoneItem Implementation
//------------------------------------------------------------------------------

procedure TOBDBarGaugeZoneItem.SetFrom(Value: Single);
begin
  if (FFrom <> Value) then
  begin
    FFrom := Value;
    if FFrom > FTo then FTo := Value;
    Changed(False);
  end;
end;

procedure TOBDBarGaugeZoneItem.SetTo(Value: Single);
begin
  if (FTo <> Value) then
  begin
    FTo := Value;
    Changed(False);
  end;
end;

procedure TOBDBarGaugeZoneItem.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

constructor TOBDBarGaugeZoneItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFrom := 0;
  FTo := 0;
  FColor := clGreen;
end;

procedure TOBDBarGaugeZoneItem.Assign(Source: TPersistent);
begin
  if (Source is TOBDBarGaugeZoneItem) then
  begin
    FFrom := (Source as TOBDBarGaugeZoneItem).From;
    FTo := (Source as TOBDBarGaugeZoneItem).&To;
    FColor := (Source as TOBDBarGaugeZoneItem).Color;
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDBarGaugeZones Implementation
//------------------------------------------------------------------------------

function TOBDBarGaugeZones.GetItem(Index: Integer): TOBDBarGaugeZoneItem;
begin
  Result := TOBDBarGaugeZoneItem(inherited Items[Index]);
end;

procedure TOBDBarGaugeZones.SetItem(Index: Integer; const Value: TOBDBarGaugeZoneItem);
begin
  inherited SetItem(Index, Value);
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TOBDBarGaugeZones.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TOBDBarGaugeZones.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDBarGaugeZoneItem);
end;

function TOBDBarGaugeZones.Add: TOBDBarGaugeZoneItem;
begin
  Result := TOBDBarGaugeZoneItem(inherited Add);
end;

procedure TOBDBarGaugeZones.Assign(Source: TPersistent);
var
  ZonesSource: TOBDBarGaugeZones;
  I: Integer;
begin
  if (Source is TOBDBarGaugeZones) then
  begin
    ZonesSource := TOBDBarGaugeZones(Source);
    Clear;
    for I := 0 to ZonesSource.Count - 1 do
      Add.Assign(ZonesSource.Items[I]);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDBarGaugeAnimation Implementation
//------------------------------------------------------------------------------

procedure TOBDBarGaugeAnimation.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDBarGaugeAnimation.SetDuration(Value: Cardinal);
begin
  if (FDuration <> Value) and (Value >= 10) then
  begin
    FDuration := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDBarGaugeAnimation.Create;
begin
  inherited Create;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FType := DEFAULT_ANIMATION_TYPE;
end;

procedure TOBDBarGaugeAnimation.Assign(Source: TPersistent);
begin
  if (Source is TOBDBarGaugeAnimation) then
  begin
    FEnabled := (Source as TOBDBarGaugeAnimation).Enabled;
    FDuration := (Source as TOBDBarGaugeAnimation).Duration;
    FType := (Source as TOBDBarGaugeAnimation).&Type;
  end else
    inherited;
end;


//------------------------------------------------------------------------------
// TOBDBarGauge Implementation
//------------------------------------------------------------------------------

procedure TOBDBarGauge.SetOrientation(Value: TBarOrientation);
begin
  if (FOrientation <> Value) then
  begin
    FOrientation := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBarGauge.SetFillDirection(Value: TFillDirection);
begin
  if (FFillDirection <> Value) then
  begin
    FFillDirection := Value;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBarGauge.SetMin(Value: Single);
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

procedure TOBDBarGauge.SetMax(Value: Single);
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

procedure TOBDBarGauge.SetValue(Value: Single);
begin
  if (FValue <> Value) then
  begin
    if (Value < FMin) then Value := FMin;
    if (Value > FMax) then Value := FMax;
    FValue := Value;
    
    if Animation.Enabled and not (csDesigning in ComponentState) then
    begin
      Animation.StartValue := Animation.Value;
      FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
      AnimationManager.CheckAnimationState;
    end
    else
      Animation.Value := FValue;
    
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBarGauge.SetBarWidth(Value: Integer);
begin
  if (FBarWidth <> Value) and (Value >= 10) then
  begin
    FBarWidth := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBarGauge.SetCornerRadius(Value: Single);
begin
  if (FCornerRadius <> Value) and (Value >= 0) then
  begin
    FCornerRadius := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDBarGauge.SetFill(Value: TOBDBarGaugeFill);
begin
  FFill.Assign(Value);
end;

procedure TOBDBarGauge.SetBackground(Value: TOBDBarGaugeBackground);
begin
  FBackground.Assign(Value);
end;

procedure TOBDBarGauge.SetBorder(Value: TOBDBarGaugeBorder);
begin
  FBorder.Assign(Value);
end;

procedure TOBDBarGauge.SetValueLabel(Value: TOBDBarGaugeValueLabel);
begin
  FValueLabel.Assign(Value);
end;

procedure TOBDBarGauge.SetZones(Value: TOBDBarGaugeZones);
begin
  FZones.Assign(Value);
end;

procedure TOBDBarGauge.SetAnimation(Value: TOBDBarGaugeAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TOBDBarGauge.InvalidateBackground;
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

function TOBDBarGauge.GetZoneColor(Value: Single): TColor;
var
  I: Integer;
begin
  Result := FFill.Color;
  for I := 0 to FZones.Count - 1 do
  begin
    if (Value >= FZones.Items[I].From) and (Value <= FZones.Items[I].&To) then
    begin
      Result := FZones.Items[I].Color;
      Break;
    end;
  end;
end;

procedure TOBDBarGauge.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  BarRect: TRectF;
begin
  if not Assigned(FBackground) or not Assigned(FBorder) then
    Exit;
    
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));
  
  // Calculate bar rectangle
  if FOrientation = boVertical then
  begin
    BarRect := TRectF.Create(
      (Width - FBarWidth) / 2,
      MARGIN_FROM_BORDER,
      (Width + FBarWidth) / 2,
      Height - MARGIN_FROM_BORDER);
  end
  else
  begin
    BarRect := TRectF.Create(
      MARGIN_FROM_BORDER,
      (Height - FBarWidth) / 2,
      Width - MARGIN_FROM_BORDER,
      (Height + FBarWidth) / 2);
  end;
  
  // Draw background bar
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FBackground.Color);
  
  if FCornerRadius > 0 then
    Canvas.DrawRoundRect(BarRect, FCornerRadius, FCornerRadius, Paint)
  else
    Canvas.DrawRect(BarRect, Paint);
  
  // Draw border
  if (FBorder.Color <> clNone) and (FBorder.Width > 0) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := FBorder.Width;
    Paint.Color := SafeColorRefToSkColor(FBorder.Color);
    
    if FCornerRadius > 0 then
      Canvas.DrawRoundRect(BarRect, FCornerRadius, FCornerRadius, Paint)
    else
      Canvas.DrawRect(BarRect, Paint);
  end;
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

function TOBDBarGauge.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDBarGauge.PaintBar(Canvas: ISkCanvas);
var
  Paint: ISkPaint;
  Font: ISkFont;
  BackgroundImage: ISkImage;
  BarRect, FillRect: TRectF;
  FillRatio, DisplayValue: Single;
  ValueText: string;
  TextRect: TRectF;
  FillColor: TColor;
begin
  try
    if not Assigned(FBackground) or not Assigned(FBorder) or not Assigned(FFill) then
    begin
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
      Exit;
    end;
    
    BackgroundImage := AcquireBackgroundSnapshot;
    if Assigned(BackgroundImage) then
      Canvas.DrawImage(BackgroundImage, 0, 0)
    else
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
    
    if FMax <= FMin then
      Exit;
    
    // Calculate bar rectangle
    if FOrientation = boVertical then
    begin
      BarRect := TRectF.Create(
        (Width - FBarWidth) / 2,
        MARGIN_FROM_BORDER,
        (Width + FBarWidth) / 2,
        Height - MARGIN_FROM_BORDER);
    end
    else
    begin
      BarRect := TRectF.Create(
        MARGIN_FROM_BORDER,
        (Height - FBarWidth) / 2,
        Width - MARGIN_FROM_BORDER,
        (Height + FBarWidth) / 2);
    end;
    
    // Calculate fill ratio
    if Animation.Enabled and not (csDesigning in ComponentState) then
    begin
      FillRatio := (Animation.Value - FMin) / (FMax - FMin);
      DisplayValue := Animation.Value;
    end
    else
    begin
      FillRatio := (FValue - FMin) / (FMax - FMin);
      DisplayValue := FValue;
    end;
    
    // Get zone color if zones are defined
    FillColor := GetZoneColor(DisplayValue);
    
    // Calculate fill rectangle based on direction
    case FFillDirection of
      fdLeftToRight:
        FillRect := TRectF.Create(
          BarRect.Left,
          BarRect.Top,
          BarRect.Left + (BarRect.Width * FillRatio),
          BarRect.Bottom);
      fdRightToLeft:
        FillRect := TRectF.Create(
          BarRect.Right - (BarRect.Width * FillRatio),
          BarRect.Top,
          BarRect.Right,
          BarRect.Bottom);
      fdBottomToTop:
        FillRect := TRectF.Create(
          BarRect.Left,
          BarRect.Bottom - (BarRect.Height * FillRatio),
          BarRect.Right,
          BarRect.Bottom);
      fdTopToBottom:
        FillRect := TRectF.Create(
          BarRect.Left,
          BarRect.Top,
          BarRect.Right,
          BarRect.Top + (BarRect.Height * FillRatio));
    end;
    
    // Draw fill
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    
    if FFill.UseGradient then
    begin
      if FOrientation = boVertical then
        Paint.Shader := TSkShader.MakeGradientLinear(
          TPointF.Create(FillRect.Left, FillRect.Bottom),
          TPointF.Create(FillRect.Left, FillRect.Top),
          [SafeColorRefToSkColor(FFill.Color), SafeColorRefToSkColor(FFill.GradientEndColor)],
          nil, TSkTileMode.Clamp)
      else
        Paint.Shader := TSkShader.MakeGradientLinear(
          TPointF.Create(FillRect.Left, FillRect.Top),
          TPointF.Create(FillRect.Right, FillRect.Top),
          [SafeColorRefToSkColor(FFill.Color), SafeColorRefToSkColor(FFill.GradientEndColor)],
          nil, TSkTileMode.Clamp);
    end
    else
      Paint.Color := SafeColorRefToSkColor(FillColor);
    
    if FCornerRadius > 0 then
      Canvas.DrawRoundRect(FillRect, FCornerRadius, FCornerRadius, Paint)
    else
      Canvas.DrawRect(FillRect, Paint);
    
    // Draw value label
    if FValueLabel.Visible then
    begin
      try
        ValueText := Format(FValueLabel.Format, [DisplayValue]);
      except
        ValueText := FormatFloat('0.##', DisplayValue);
      end;
      Font := CreateSkFont(FValueLabel.Font);
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Color := SafeColorRefToSkColor(FValueLabel.Font.Color);
      Paint.Style := TSkPaintStyle.Fill;
      
      Canvas.DrawSimpleText(
        ValueText,
        BarRect.Left + (BarRect.Width / 2),
        BarRect.Top + (BarRect.Height / 2) + (Font.Size / 3),
        Font,
        Paint);
    end;
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
  end;
end;

procedure TOBDBarGauge.PaintSkia(Canvas: ISkCanvas);
begin
  try
    PaintBar(Canvas);
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
  end;
end;

procedure TOBDBarGauge.SettingsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDBarGauge.FillSettingsChanged(Sender: TObject);
begin
  Redraw;
  Invalidate;
end;

procedure TOBDBarGauge.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.CheckAnimationState;
  if not Animation.Enabled then
    Animation.Value := FValue;
  Redraw;
  Invalidate;
end;

procedure TOBDBarGauge.AnimationTick(ElapsedMs: Int64);
var
  CurrentMs, Elapsed: Int64;
  AnimationProgress, EasedProgress, InterpolatedValue: Single;
  EasingFunction: TOBDCustomControlAnimationEasingFunction;
begin
  if not Animation.Enabled then
    Exit;
  
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  
  EasingFunction := GetEasingFunction(Animation.&Type);
  if Elapsed < Animation.Duration then
  begin
    AnimationProgress := Elapsed / Animation.Duration;
    EasedProgress := EasingFunction(AnimationProgress);
    InterpolatedValue := Animation.StartValue + (FValue - Animation.StartValue) * EasedProgress;
    Animation.Value := InterpolatedValue;
  end
  else
    Animation.Value := FValue;
  
  Redraw;
  Invalidate;
end;

function TOBDBarGauge.IsAnimating: Boolean;
var
  CurrentMs, Elapsed: Int64;
begin
  if not Animation.Enabled then
    Exit(False);
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  Result := (Elapsed < Animation.Duration) and (Animation.StartValue <> FValue);
end;

function TOBDBarGauge.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

procedure TOBDBarGauge.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDBarGauge.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDBarGauge.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_COLORCHANGED: InvalidateBackground;
  end;
end;

constructor TOBDBarGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FOrientation := boVertical;
  FFillDirection := fdBottomToTop;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FBarWidth := DEFAULT_BAR_WIDTH;
  FCornerRadius := DEFAULT_CORNER_RADIUS;
  FFill := TOBDBarGaugeFill.Create;
  FFill.OnChange := FillSettingsChanged;
  FBackground := TOBDBarGaugeBackground.Create;
  FBackground.OnChange := SettingsChanged;
  FBorder := TOBDBarGaugeBorder.Create;
  FBorder.OnChange := SettingsChanged;
  FValueLabel := TOBDBarGaugeValueLabel.Create;
  FValueLabel.OnChange := SettingsChanged;
  FZones := TOBDBarGaugeZones.Create(Self);
  FZones.OnChange := FillSettingsChanged;
  FAnimation := TOBDBarGaugeAnimation.Create;
  FAnimation.OnChange := AnimationChanged;
  FAnimation.Value := FValue;
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
  
  if not (csDesigning in ComponentState) then
    AnimationManager.RegisterControl(Self);
  
  Width := 100;
  Height := 200;
end;

destructor TOBDBarGauge.Destroy;
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.UnregisterControl(Self);
  FRenderLock.Free;
  FFill.Free;
  FBackground.Free;
  FBorder.Free;
  FValueLabel.Free;
  FZones.Free;
  FAnimation.Free;
  inherited Destroy;
end;

procedure TOBDBarGauge.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TOBDBarGauge) then
  begin
    FOrientation := (Source as TOBDBarGauge).Orientation;
    FFillDirection := (Source as TOBDBarGauge).FillDirection;
    FMin := (Source as TOBDBarGauge).Min;
    FMax := (Source as TOBDBarGauge).Max;
    FBarWidth := (Source as TOBDBarGauge).BarWidth;
    FCornerRadius := (Source as TOBDBarGauge).CornerRadius;
    FFill.Assign((Source as TOBDBarGauge).Fill);
    FBackground.Assign((Source as TOBDBarGauge).Background);
    FBorder.Assign((Source as TOBDBarGauge).Border);
    FValueLabel.Assign((Source as TOBDBarGauge).ValueLabel);
    FZones.Assign((Source as TOBDBarGauge).Zones);
    FAnimation.Assign((Source as TOBDBarGauge).Animation);
  end;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

end.
