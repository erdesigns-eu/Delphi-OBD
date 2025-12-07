//------------------------------------------------------------------------------
// UNIT           : OBD.LinearGauge.pas
// CONTENTS       : Linear gauge component with Skia rendering
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : OpenAI Assistant (based on ERDesigns pattern)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.LinearGauge;

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
  /// <summary>
  ///   Margin from the border
  /// </summary>
  MARGIN_FROM_BORDER = 4;

  /// <summary>
  ///   Default min/max values
  /// </summary>
  DEFAULT_MIN = 0;
  DEFAULT_MAX = 100;

  /// <summary>
  ///   Default tick steps
  /// </summary>
  DEFAULT_MINOR_STEP = 1;
  DEFAULT_MAJOR_STEP = 10;
  DEFAULT_MINOR_LENGTH = 5;
  DEFAULT_MAJOR_LENGTH = 10;

  /// <summary>
  ///   Default scale height (for horizontal) or width (for vertical)
  /// </summary>
  DEFAULT_SCALE_SIZE = 20;

  /// <summary>
  ///   Default colors
  /// </summary>
  DEFAULT_BACKGROUND_FROM = $00FBF5F7;
  DEFAULT_BACKGROUND_TO = $00BBAEAC;
  DEFAULT_BORDER_COLOR = $00776F6F;
  DEFAULT_BORDER_WIDTH = 2;
  DEFAULT_SCALE_COLOR = $00E0E0E0;
  DEFAULT_SLIDER_COLOR = $00DF7000;
  DEFAULT_SLIDER_SIZE = 14;

  /// <summary>
  ///   Animation defaults
  /// </summary>
  DEFAULT_ANIMATION_ENABLED = True;
  DEFAULT_ANIMATION_DURATION = 1000;
  DEFAULT_ANIMATION_TYPE = anQuartEaseInOut;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Gauge orientation
  /// </summary>
  TGaugeOrientation = (goHorizontal, goVertical);

  /// <summary>
  ///   Scale position
  /// </summary>
  TScalePosition = (spTop, spBottom, spLeft, spRight, spNone);

  /// <summary>
  ///   Slider shape
  /// </summary>
  TSliderShape = (ssRectangle, ssTriangle, ssCircle, ssLine);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Linear Gauge background properties
  /// </summary>
  TOBDLinearGaugeBackground = class(TPersistent)
  private
    FFromColor: TColor;
    FToColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BACKGROUND_FROM;
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BACKGROUND_TO;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Linear Gauge border properties
  /// </summary>
  TOBDLinearGaugeBorder = class(TPersistent)
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
  ///   Linear Gauge scale properties
  /// </summary>
  TOBDLinearGaugeScale = class(TPersistent)
  private
    FColor: TColor;
    FSize: Integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetSize(Value: Integer);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default DEFAULT_SCALE_COLOR;
    property Size: Integer read FSize write SetSize default DEFAULT_SCALE_SIZE;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Linear Gauge tick properties
  /// </summary>
  TOBDLinearGaugeTick = class(TPersistent)
  private
    FStep: Single;
    FLength: Single;
    FWidth: Single;
    FShowLabel: Boolean;
    FColor: TColor;
    FFont: TFont;
    FDivider: Single;
    FOffset: Single;
    FOnChange: TNotifyEvent;
    procedure SetStep(Value: Single);
    procedure SetLength(Value: Single);
    procedure SetWidth(Value: Single);
    procedure SetShowLabel(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetDivider(Value: Single);
    procedure SetOffset(Value: Single);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Step: Single read FStep write SetStep;
    property Length: Single read FLength write SetLength;
    property Width: Single read FWidth write SetWidth;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property Divider: Single read FDivider write SetDivider;
    property Offset: Single read FOffset write SetOffset;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Linear Gauge major ticks
  /// </summary>
  TOBDLinearGaugeMajorTicks = class(TOBDLinearGaugeTick)
  public
    constructor Create; override;
  end;

  /// <summary>
  ///   Linear Gauge minor ticks
  /// </summary>
  TOBDLinearGaugeMinorTicks = class(TOBDLinearGaugeTick)
  public
    constructor Create; override;
  end;

  /// <summary>
  ///   Linear Gauge slider properties
  /// </summary>
  TOBDLinearGaugeSlider = class(TPersistent)
  private
    FColor: TColor;
    FSize: Single;
    FShape: TSliderShape;
    FBorderColor: TColor;
    FBorderWidth: Single;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetSize(Value: Single);
    procedure SetShape(Value: TSliderShape);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default DEFAULT_SLIDER_COLOR;
    property Size: Single read FSize write SetSize;
    property Shape: TSliderShape read FShape write SetShape default ssTriangle;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Linear Gauge caption
  /// </summary>
  TOBDLinearGaugeCaption = class(TPersistent)
  private
    FCaption: string;
    FFont: TFont;
    FOffset: Single;
    FOnChange: TNotifyEvent;
    procedure SetCaption(Value: string);
    procedure SetFont(Value: TFont);
    procedure SetOffset(Value: Single);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
    property Offset: Single read FOffset write SetOffset;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Linear Gauge animation
  /// </summary>
  TOBDLinearGaugeAnimation = class(TPersistent)
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
  ///   Linear Gauge gradient scale item
  /// </summary>
  TOBDLinearGaugeGradientScaleItem = class(TCollectionItem)
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
    property Color: TColor read FColor write SetColor default clHighlight;
  end;

  /// <summary>
  ///   Linear Gauge gradient scale collection
  /// </summary>
  TOBDLinearGaugeGradientScaleItems = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDLinearGaugeGradientScaleItem;
    procedure SetItem(Index: Integer; const Value: TOBDLinearGaugeGradientScaleItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    function Add: TOBDLinearGaugeGradientScaleItem;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TOBDLinearGaugeGradientScaleItem read GetItem write SetItem;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Linear Gauge Component
  /// </summary>
  TOBDLinearGauge = class(TOBDCustomControl, IOBDAnimatable)
  private
    FBackgroundSnapshot: ISkImage;
    FRenderLock: TObject;
    FAnimationStartMs: Int64;
    FStopwatch: TStopwatch;
    FOrientation: TGaugeOrientation;
    FScalePosition: TScalePosition;
    FMin: Single;
    FMax: Single;
    FValue: Single;
    FBackground: TOBDLinearGaugeBackground;
    FBorder: TOBDLinearGaugeBorder;
    FScale: TOBDLinearGaugeScale;
    FMajorTicks: TOBDLinearGaugeMajorTicks;
    FMinorTicks: TOBDLinearGaugeMinorTicks;
    FSlider: TOBDLinearGaugeSlider;
    FStartCaption: TOBDLinearGaugeCaption;
    FEndCaption: TOBDLinearGaugeCaption;
    FAnimation: TOBDLinearGaugeAnimation;
    FGradientScaleItems: TOBDLinearGaugeGradientScaleItems;
    procedure SetOrientation(Value: TGaugeOrientation);
    procedure SetScalePosition(Value: TScalePosition);
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetValue(Value: Single);
    procedure SetBackground(Value: TOBDLinearGaugeBackground);
    procedure SetBorder(Value: TOBDLinearGaugeBorder);
    procedure SetScale(Value: TOBDLinearGaugeScale);
    procedure SetMajorTicks(Value: TOBDLinearGaugeMajorTicks);
    procedure SetMinorTicks(Value: TOBDLinearGaugeMinorTicks);
    procedure SetSlider(Value: TOBDLinearGaugeSlider);
    procedure SetStartCaption(Value: TOBDLinearGaugeCaption);
    procedure SetEndCaption(Value: TOBDLinearGaugeCaption);
    procedure SetAnimation(Value: TOBDLinearGaugeAnimation);
    procedure SetGradientScaleItems(Value: TOBDLinearGaugeGradientScaleItems);
  protected
    procedure InvalidateBackground; virtual;
    procedure PaintSlider(Canvas: ISkCanvas); virtual;
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure BuildBackgroundSnapshot;
    function AcquireBackgroundSnapshot: ISkImage;
    procedure SettingsChanged(Sender: TObject);
    procedure SliderSettingsChanged(Sender: TObject);
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
    property Orientation: TGaugeOrientation read FOrientation write SetOrientation default goHorizontal;
    property ScalePosition: TScalePosition read FScalePosition write SetScalePosition default spBottom;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Value: Single read FValue write SetValue;
    property Background: TOBDLinearGaugeBackground read FBackground write SetBackground;
    property Border: TOBDLinearGaugeBorder read FBorder write SetBorder;
    property Scale: TOBDLinearGaugeScale read FScale write SetScale;
    property MajorTicks: TOBDLinearGaugeMajorTicks read FMajorTicks write SetMajorTicks;
    property MinorTicks: TOBDLinearGaugeMinorTicks read FMinorTicks write SetMinorTicks;
    property Slider: TOBDLinearGaugeSlider read FSlider write SetSlider;
    property StartCaption: TOBDLinearGaugeCaption read FStartCaption write SetStartCaption;
    property EndCaption: TOBDLinearGaugeCaption read FEndCaption write SetEndCaption;
    property Animation: TOBDLinearGaugeAnimation read FAnimation write SetAnimation;
    property GradientScale: TOBDLinearGaugeGradientScaleItems read FGradientScaleItems write SetGradientScaleItems;
  end;

implementation

uses
  System.Math;


//------------------------------------------------------------------------------
// TOBDLinearGaugeBackground Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeBackground.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    FFromColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeBackground.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    FToColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDLinearGaugeBackground.Create;
begin
  inherited Create;
  FFromColor := DEFAULT_BACKGROUND_FROM;
  FToColor := DEFAULT_BACKGROUND_TO;
end;

procedure TOBDLinearGaugeBackground.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeBackground) then
  begin
    FFromColor := (Source as TOBDLinearGaugeBackground).FromColor;
    FToColor := (Source as TOBDLinearGaugeBackground).ToColor;
    if Assigned(OnChange) then OnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeBorder Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeBorder.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeBorder.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    FWidth := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDLinearGaugeBorder.Create;
begin
  inherited Create;
  FColor := DEFAULT_BORDER_COLOR;
  FWidth := DEFAULT_BORDER_WIDTH;
end;

procedure TOBDLinearGaugeBorder.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeBorder) then
  begin
    FColor := (Source as TOBDLinearGaugeBorder).Color;
    FWidth := (Source as TOBDLinearGaugeBorder).Width;
    if Assigned(OnChange) then OnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeScale Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeScale.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeScale.SetSize(Value: Integer);
begin
  if (FSize <> Value) and (Value >= 5) then
  begin
    FSize := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDLinearGaugeScale.Create;
begin
  inherited Create;
  FColor := DEFAULT_SCALE_COLOR;
  FSize := DEFAULT_SCALE_SIZE;
end;

procedure TOBDLinearGaugeScale.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeScale) then
  begin
    FColor := (Source as TOBDLinearGaugeScale).Color;
    FSize := (Source as TOBDLinearGaugeScale).Size;
    if Assigned(OnChange) then OnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeTick Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeTick.SetStep(Value: Single);
begin
  if (FStep <> Value) and (Value >= 1) then
  begin
    FStep := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeTick.SetLength(Value: Single);
begin
  if (FLength <> Value) and (Value >= 2) then
  begin
    FLength := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeTick.SetWidth(Value: Single);
begin
  if (FWidth <> Value) and (Value >= 1) then
  begin
    FWidth := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeTick.SetShowLabel(Value: Boolean);
begin
  if (FShowLabel <> Value) then
  begin
    FShowLabel := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeTick.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeTick.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TOBDLinearGaugeTick.SetDivider(Value: Single);
begin
  if (FDivider <> Value) and (Value >= 0) then
  begin
    FDivider := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeTick.SetOffset(Value: Single);
begin
  if (FOffset <> Value) and (Value >= 0) then
  begin
    FOffset := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeTick.FontChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

constructor TOBDLinearGaugeTick.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FWidth := 1;
  FColor := clBlack;
  FDivider := 0;
  FOffset := 2;
end;

destructor TOBDLinearGaugeTick.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TOBDLinearGaugeTick.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeTick) then
  begin
    FStep := (Source as TOBDLinearGaugeTick).Step;
    FLength := (Source as TOBDLinearGaugeTick).Length;
    FWidth := (Source as TOBDLinearGaugeTick).Width;
    FShowLabel := (Source as TOBDLinearGaugeTick).ShowLabel;
    FColor := (Source as TOBDLinearGaugeTick).Color;
    FFont.Assign((Source as TOBDLinearGaugeTick).Font);
    FDivider := (Source as TOBDLinearGaugeTick).Divider;
    FOffset := (Source as TOBDLinearGaugeTick).Offset;
  end else
    inherited;
end;

constructor TOBDLinearGaugeMajorTicks.Create;
begin
  inherited Create;
  FStep := DEFAULT_MAJOR_STEP;
  FLength := DEFAULT_MAJOR_LENGTH;
  FShowLabel := True;
end;

constructor TOBDLinearGaugeMinorTicks.Create;
begin
  inherited Create;
  FStep := DEFAULT_MINOR_STEP;
  FLength := DEFAULT_MINOR_LENGTH;
  FShowLabel := False;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeSlider Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeSlider.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeSlider.SetSize(Value: Single);
begin
  if (FSize <> Value) and (Value >= 5) then
  begin
    FSize := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeSlider.SetShape(Value: TSliderShape);
begin
  if (FShape <> Value) then
  begin
    FShape := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeSlider.SetBorderColor(Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeSlider.SetBorderWidth(Value: Single);
begin
  if (FBorderWidth <> Value) and (Value >= 0) then
  begin
    FBorderWidth := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDLinearGaugeSlider.Create;
begin
  inherited Create;
  FColor := DEFAULT_SLIDER_COLOR;
  FSize := DEFAULT_SLIDER_SIZE;
  FShape := ssTriangle;
  FBorderColor := clBlack;
  FBorderWidth := 1;
end;

procedure TOBDLinearGaugeSlider.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeSlider) then
  begin
    FColor := (Source as TOBDLinearGaugeSlider).Color;
    FSize := (Source as TOBDLinearGaugeSlider).Size;
    FShape := (Source as TOBDLinearGaugeSlider).Shape;
    FBorderColor := (Source as TOBDLinearGaugeSlider).BorderColor;
    FBorderWidth := (Source as TOBDLinearGaugeSlider).BorderWidth;
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeCaption Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeCaption.SetCaption(Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TOBDLinearGaugeCaption.SetOffset(Value: Single);
begin
  if (FOffset <> Value) then
  begin
    FOffset := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeCaption.FontChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

constructor TOBDLinearGaugeCaption.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FOffset := 5;
end;

destructor TOBDLinearGaugeCaption.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TOBDLinearGaugeCaption.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeCaption) then
  begin
    FCaption := (Source as TOBDLinearGaugeCaption).Caption;
    FFont.Assign((Source as TOBDLinearGaugeCaption).Font);
    FOffset := (Source as TOBDLinearGaugeCaption).Offset;
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeAnimation Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeAnimation.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDLinearGaugeAnimation.SetDuration(Value: Cardinal);
begin
  if (FDuration <> Value) and (Value >= 10) then
  begin
    FDuration := Value;
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

constructor TOBDLinearGaugeAnimation.Create;
begin
  inherited Create;
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FType := DEFAULT_ANIMATION_TYPE;
end;

procedure TOBDLinearGaugeAnimation.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeAnimation) then
  begin
    FEnabled := (Source as TOBDLinearGaugeAnimation).Enabled;
    FDuration := (Source as TOBDLinearGaugeAnimation).Duration;
    FType := (Source as TOBDLinearGaugeAnimation).&Type;
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeGradientScaleItem Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGaugeGradientScaleItem.SetFrom(Value: Single);
begin
  if (FFrom <> Value) then
  begin
    FFrom := Value;
    // Don't auto-adjust To value - let user fix it
    Changed(False);
  end;
end;

procedure TOBDLinearGaugeGradientScaleItem.SetTo(Value: Single);
begin
  if (FTo <> Value) then
  begin
    FTo := Value;
    Changed(False);
  end;
end;

procedure TOBDLinearGaugeGradientScaleItem.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

constructor TOBDLinearGaugeGradientScaleItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFrom := 0;
  FTo := 0;
  FColor := clHighlight;
end;

procedure TOBDLinearGaugeGradientScaleItem.Assign(Source: TPersistent);
begin
  if (Source is TOBDLinearGaugeGradientScaleItem) then
  begin
    FFrom := (Source as TOBDLinearGaugeGradientScaleItem).From;
    FTo := (Source as TOBDLinearGaugeGradientScaleItem).&To;
    FColor := (Source as TOBDLinearGaugeGradientScaleItem).Color;
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDLinearGaugeGradientScaleItems Implementation
//------------------------------------------------------------------------------

function TOBDLinearGaugeGradientScaleItems.GetItem(Index: Integer): TOBDLinearGaugeGradientScaleItem;
begin
  Result := TOBDLinearGaugeGradientScaleItem(inherited Items[Index]);
end;

procedure TOBDLinearGaugeGradientScaleItems.SetItem(Index: Integer; const Value: TOBDLinearGaugeGradientScaleItem);
begin
  inherited SetItem(Index, Value);
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TOBDLinearGaugeGradientScaleItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TOBDLinearGaugeGradientScaleItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDLinearGaugeGradientScaleItem);
end;

function TOBDLinearGaugeGradientScaleItems.Add: TOBDLinearGaugeGradientScaleItem;
begin
  Result := TOBDLinearGaugeGradientScaleItem(inherited Add);
end;

procedure TOBDLinearGaugeGradientScaleItems.Assign(Source: TPersistent);
var
  Items: TOBDLinearGaugeGradientScaleItems;
  I: Integer;
begin
  if (Source is TOBDLinearGaugeGradientScaleItems) then
  begin
    Items := TOBDLinearGaugeGradientScaleItems(Source);
    Clear;
    for I := 0 to Items.Count - 1 do
      Add.Assign(Items.Items[I]);
  end else
    inherited;
end;


//------------------------------------------------------------------------------
// TOBDLinearGauge Implementation
//------------------------------------------------------------------------------

procedure TOBDLinearGauge.SetOrientation(Value: TGaugeOrientation);
begin
  if (FOrientation <> Value) then
  begin
    FOrientation := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetScalePosition(Value: TScalePosition);
begin
  if (FScalePosition <> Value) then
  begin
    FScalePosition := Value;
    InvalidateBackground;
    Redraw;
    Invalidate;
  end;
end;

procedure TOBDLinearGauge.SetMin(Value: Single);
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

procedure TOBDLinearGauge.SetMax(Value: Single);
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

procedure TOBDLinearGauge.SetValue(Value: Single);
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

procedure TOBDLinearGauge.SetBackground(Value: TOBDLinearGaugeBackground);
begin
  FBackground.Assign(Value);
end;

procedure TOBDLinearGauge.SetBorder(Value: TOBDLinearGaugeBorder);
begin
  FBorder.Assign(Value);
end;

procedure TOBDLinearGauge.SetScale(Value: TOBDLinearGaugeScale);
begin
  FScale.Assign(Value);
end;

procedure TOBDLinearGauge.SetMajorTicks(Value: TOBDLinearGaugeMajorTicks);
begin
  FMajorTicks.Assign(Value);
end;

procedure TOBDLinearGauge.SetMinorTicks(Value: TOBDLinearGaugeMinorTicks);
begin
  FMinorTicks.Assign(Value);
end;

procedure TOBDLinearGauge.SetSlider(Value: TOBDLinearGaugeSlider);
begin
  FSlider.Assign(Value);
end;

procedure TOBDLinearGauge.SetStartCaption(Value: TOBDLinearGaugeCaption);
begin
  FStartCaption.Assign(Value);
end;

procedure TOBDLinearGauge.SetEndCaption(Value: TOBDLinearGaugeCaption);
begin
  FEndCaption.Assign(Value);
end;

procedure TOBDLinearGauge.SetAnimation(Value: TOBDLinearGaugeAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TOBDLinearGauge.SetGradientScaleItems(Value: TOBDLinearGaugeGradientScaleItems);
begin
  FGradientScaleItems.Assign(Value);
end;

procedure TOBDLinearGauge.InvalidateBackground;
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

procedure TOBDLinearGauge.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  Font: ISkFont;
  ScaleRect, BgRect, ZoneRect: TRectF;
  TotalTicks, TickIndex, I: Integer;
  TickValue, TickPos: Single;
  StartPoint, EndPoint: TPointF;
  NumberStr: string;
  NumberPos: TPointF;
  ZoneStart, ZoneEnd: Single;
begin
  if not Assigned(FBackground) or not Assigned(FBorder) or
     not Assigned(FScale) or not Assigned(FMajorTicks) or
     not Assigned(FMinorTicks) or not Assigned(FStartCaption) or
     not Assigned(FEndCaption) or not Assigned(FGradientScaleItems) then
    Exit;
    
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));
  
  // Calculate scale rectangle based on orientation
  if FOrientation = goHorizontal then
  begin
    ScaleRect := TRectF.Create(
      MARGIN_FROM_BORDER,
      (Height - FScale.Size) / 2,
      Width - MARGIN_FROM_BORDER,
      (Height + FScale.Size) / 2);
  end
  else
  begin
    ScaleRect := TRectF.Create(
      (Width - FScale.Size) / 2,
      MARGIN_FROM_BORDER,
      (Width + FScale.Size) / 2,
      Height - MARGIN_FROM_BORDER);
  end;
  
  // Draw background gradient
  if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    if FOrientation = goHorizontal then
      Paint.Shader := TSkShader.MakeGradientLinear(
        TPointF.Create(0, 0),
        TPointF.Create(Width, 0),
        [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
        nil, TSkTileMode.Clamp)
    else
      Paint.Shader := TSkShader.MakeGradientLinear(
        TPointF.Create(0, Height),
        TPointF.Create(0, 0),
        [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
        nil, TSkTileMode.Clamp);
    Canvas.DrawRect(TRectF.Create(0, 0, Width, Height), Paint);
  end;
  
  // Draw gradient scale zones
  for I := 0 to GradientScale.Count - 1 do
  begin
    ZoneStart := ((GradientScale.Items[I].From - FMin) / (FMax - FMin));
    ZoneEnd := ((GradientScale.Items[I].&To - FMin) / (FMax - FMin));
    
    if FOrientation = goHorizontal then
    begin
      ZoneRect := TRectF.Create(
        ScaleRect.Left + (ScaleRect.Width * ZoneStart),
        ScaleRect.Top,
        ScaleRect.Left + (ScaleRect.Width * ZoneEnd),
        ScaleRect.Bottom);
    end
    else
    begin
      ZoneRect := TRectF.Create(
        ScaleRect.Left,
        ScaleRect.Bottom - (ScaleRect.Height * ZoneEnd),
        ScaleRect.Right,
        ScaleRect.Bottom - (ScaleRect.Height * ZoneStart));
    end;
    
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(GradientScale.Items[I].Color);
    Canvas.DrawRect(ZoneRect, Paint);
  end;
  
  // Draw scale bar
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(FScale.Color);
  Canvas.DrawRect(ScaleRect, Paint);
  
  // Draw border
  if (Border.Color <> clNone) and (Border.Width > 0) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := Border.Width;
    Paint.Color := SafeColorRefToSkColor(Border.Color);
    Canvas.DrawRect(ScaleRect, Paint);
  end;
  
  // Draw minor ticks
  if FMinorTicks.Step > 0 then
  begin
    TotalTicks := Round((FMax - FMin) / FMinorTicks.Step);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := FMinorTicks.Width;
    Paint.Color := SafeColorRefToSkColor(FMinorTicks.Color);
    
    for TickIndex := 0 to TotalTicks do
    begin
      if (TickIndex mod Round(FMajorTicks.Step)) = 0 then
        Continue;
        
      TickValue := FMin + (FMinorTicks.Step * TickIndex);
      TickPos := (TickValue - FMin) / (FMax - FMin);
      
      if FOrientation = goHorizontal then
      begin
        StartPoint := TPointF.Create(ScaleRect.Left + (ScaleRect.Width * TickPos), ScaleRect.Bottom);
        EndPoint := TPointF.Create(StartPoint.X, StartPoint.Y + FMinorTicks.Length);
      end
      else
      begin
        StartPoint := TPointF.Create(ScaleRect.Right, ScaleRect.Bottom - (ScaleRect.Height * TickPos));
        EndPoint := TPointF.Create(StartPoint.X + FMinorTicks.Length, StartPoint.Y);
      end;
      
      Canvas.DrawLine(StartPoint, EndPoint, Paint);
    end;
  end;
  
  // Draw major ticks
  if FMajorTicks.Step > 0 then
  begin
    TotalTicks := Round((FMax - FMin) / FMajorTicks.Step);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := FMajorTicks.Width;
    Paint.Color := SafeColorRefToSkColor(FMajorTicks.Color);
    
    for TickIndex := 0 to TotalTicks do
    begin
      TickValue := FMin + (FMajorTicks.Step * TickIndex);
      TickPos := (TickValue - FMin) / (FMax - FMin);
      
      if FOrientation = goHorizontal then
      begin
        StartPoint := TPointF.Create(ScaleRect.Left + (ScaleRect.Width * TickPos), ScaleRect.Bottom);
        EndPoint := TPointF.Create(StartPoint.X, StartPoint.Y + FMajorTicks.Length);
      end
      else
      begin
        StartPoint := TPointF.Create(ScaleRect.Right, ScaleRect.Bottom - (ScaleRect.Height * TickPos));
        EndPoint := TPointF.Create(StartPoint.X + FMajorTicks.Length, StartPoint.Y);
      end;
      
      Canvas.DrawLine(StartPoint, EndPoint, Paint);
      
      // Draw labels
      if FMajorTicks.ShowLabel then
      begin
        Font := CreateSkFont(FMajorTicks.Font);
        Paint := TSkPaint.Create;
        Paint.AntiAlias := True;
        Paint.Color := SafeColorRefToSkColor(FMajorTicks.Font.Color);
        Paint.Style := TSkPaintStyle.Fill;
        
        if (FMajorTicks.Divider > 0) then
          NumberStr := FloatToStr(TickValue / FMajorTicks.Divider)
        else
          NumberStr := FloatToStr(TickValue);
        
        if FOrientation = goHorizontal then
          NumberPos := TPointF.Create(StartPoint.X, EndPoint.Y + Font.Size + 5)
        else
          NumberPos := TPointF.Create(EndPoint.X + 5, StartPoint.Y + (Font.Size / 3));
        
        Canvas.DrawSimpleText(NumberStr, NumberPos.X, NumberPos.Y, Font, Paint);
      end;
    end;
  end;
  
  // Draw captions
  if (FStartCaption.Caption <> '') then
  begin
    Font := CreateSkFont(FStartCaption.Font);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FStartCaption.Font.Color);
    Paint.Style := TSkPaintStyle.Fill;
    
    if FOrientation = goHorizontal then
      Canvas.DrawSimpleText(FStartCaption.Caption, FStartCaption.Offset, Height / 2, Font, Paint)
    else
      Canvas.DrawSimpleText(FStartCaption.Caption, Width / 2, Height - FStartCaption.Offset, Font, Paint);
  end;
  
  if (FEndCaption.Caption <> '') then
  begin
    Font := CreateSkFont(FEndCaption.Font);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FEndCaption.Font.Color);
    Paint.Style := TSkPaintStyle.Fill;
    
    if FOrientation = goHorizontal then
      Canvas.DrawSimpleText(FEndCaption.Caption, Width - FEndCaption.Offset, Height / 2, Font, Paint)
    else
      Canvas.DrawSimpleText(FEndCaption.Caption, Width / 2, FEndCaption.Offset, Font, Paint);
  end;
  
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

function TOBDLinearGauge.AcquireBackgroundSnapshot: ISkImage;
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

procedure TOBDLinearGauge.PaintSlider(Canvas: ISkCanvas);
var
  Paint: ISkPaint;
  BackgroundImage: ISkImage;
  SliderPos, ValueRatio: Single;
  ScaleRect: TRectF;
  SliderPath: ISkPath;
  PathBuilder: ISkPathBuilder;
  CenterPoint: TPointF;
begin
  try
    if not Assigned(FBackground) or not Assigned(FBorder) or 
       not Assigned(FScale) or not Assigned(FSlider) then
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
    
    // Calculate scale rectangle
    if FOrientation = goHorizontal then
    begin
      ScaleRect := TRectF.Create(
        MARGIN_FROM_BORDER,
        (Height - FScale.Size) / 2,
        Width - MARGIN_FROM_BORDER,
        (Height + FScale.Size) / 2);
    end
    else
    begin
      ScaleRect := TRectF.Create(
        (Width - FScale.Size) / 2,
        MARGIN_FROM_BORDER,
        (Width + FScale.Size) / 2,
        Height - MARGIN_FROM_BORDER);
    end;
    
    // Calculate slider position
    if Animation.Enabled and not (csDesigning in ComponentState) then
      ValueRatio := (Animation.Value - FMin) / (FMax - FMin)
    else
      ValueRatio := (FValue - FMin) / (FMax - FMin);
    
    if FOrientation = goHorizontal then
    begin
      SliderPos := ScaleRect.Left + (ScaleRect.Width * ValueRatio);
      CenterPoint := TPointF.Create(SliderPos, ScaleRect.Top + (ScaleRect.Height / 2));
    end
    else
    begin
      SliderPos := ScaleRect.Bottom - (ScaleRect.Height * ValueRatio);
      CenterPoint := TPointF.Create(ScaleRect.Left + (ScaleRect.Width / 2), SliderPos);
    end;
    
    // Draw slider based on shape
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FSlider.Color);
    
    case FSlider.Shape of
      ssRectangle:
        begin
          if FOrientation = goHorizontal then
            Canvas.DrawRect(TRectF.Create(CenterPoint.X - (FSlider.Size / 2), ScaleRect.Top,
              CenterPoint.X + (FSlider.Size / 2), ScaleRect.Bottom), Paint)
          else
            Canvas.DrawRect(TRectF.Create(ScaleRect.Left, CenterPoint.Y - (FSlider.Size / 2),
              ScaleRect.Right, CenterPoint.Y + (FSlider.Size / 2)), Paint);
        end;
      ssTriangle:
        begin
          PathBuilder := TSkPathBuilder.Create;
          if FOrientation = goHorizontal then
          begin
            PathBuilder.MoveTo(PointF(CenterPoint.X, ScaleRect.Top - 2));
            PathBuilder.LineTo(PointF(CenterPoint.X - FSlider.Size / 2, ScaleRect.Top - FSlider.Size - 2));
            PathBuilder.LineTo(PointF(CenterPoint.X + FSlider.Size / 2, ScaleRect.Top - FSlider.Size - 2));
          end
          else
          begin
            PathBuilder.MoveTo(PointF(ScaleRect.Right + 2, CenterPoint.Y));
            PathBuilder.LineTo(PointF(ScaleRect.Right + FSlider.Size + 2, CenterPoint.Y - FSlider.Size / 2));
            PathBuilder.LineTo(PointF(ScaleRect.Right + FSlider.Size + 2, CenterPoint.Y + FSlider.Size / 2));
          end;
          PathBuilder.Close;
          SliderPath := PathBuilder.Detach;
          Canvas.DrawPath(SliderPath, Paint);
        end;
      ssCircle:
        Canvas.DrawCircle(CenterPoint.X, CenterPoint.Y, FSlider.Size / 2, Paint);
      ssLine:
        begin
          Paint.Style := TSkPaintStyle.Stroke;
          Paint.StrokeWidth := FSlider.Size / 2;
          if FOrientation = goHorizontal then
            Canvas.DrawLine(CenterPoint.X, ScaleRect.Top, CenterPoint.X, ScaleRect.Bottom, Paint)
          else
            Canvas.DrawLine(ScaleRect.Left, CenterPoint.Y, ScaleRect.Right, CenterPoint.Y, Paint);
        end;
    end;
    
    // Draw slider border
    if (FSlider.BorderColor <> clNone) and (FSlider.BorderWidth > 0) then
    begin
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.StrokeWidth := FSlider.BorderWidth;
      Paint.Color := SafeColorRefToSkColor(FSlider.BorderColor);
      
      case FSlider.Shape of
        ssCircle:
          Canvas.DrawCircle(CenterPoint.X, CenterPoint.Y, (FSlider.Size / 2) - (FSlider.BorderWidth / 2), Paint);
        ssTriangle:
          if Assigned(SliderPath) then
            Canvas.DrawPath(SliderPath, Paint);
      end;
    end;
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
  end;
end;

procedure TOBDLinearGauge.PaintSkia(Canvas: ISkCanvas);
begin
  try
    PaintSlider(Canvas);
  except
    on E: Exception do
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
  end;
end;

procedure TOBDLinearGauge.SettingsChanged(Sender: TObject);
begin
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDLinearGauge.SliderSettingsChanged(Sender: TObject);
begin
  Redraw;
  Invalidate;
end;

procedure TOBDLinearGauge.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.CheckAnimationState;
  if not Animation.Enabled then
    Animation.Value := FValue;
  Redraw;
  Invalidate;
end;

procedure TOBDLinearGauge.AnimationTick(ElapsedMs: Int64);
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

function TOBDLinearGauge.IsAnimating: Boolean;
var
  CurrentMs, Elapsed: Int64;
begin
  if not Animation.Enabled then
    Exit(False);
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  Result := (Elapsed < Animation.Duration) and (Animation.StartValue <> FValue);
end;

function TOBDLinearGauge.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

procedure TOBDLinearGauge.Resize;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDLinearGauge.Loaded;
begin
  inherited;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

procedure TOBDLinearGauge.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_COLORCHANGED: InvalidateBackground;
  end;
end;

constructor TOBDLinearGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderLock := TObject.Create;
  FOrientation := goHorizontal;
  FScalePosition := spBottom;
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FBackground := TOBDLinearGaugeBackground.Create;
  FBackground.OnChange := SettingsChanged;
  FBorder := TOBDLinearGaugeBorder.Create;
  FBorder.OnChange := SettingsChanged;
  FScale := TOBDLinearGaugeScale.Create;
  FScale.OnChange := SettingsChanged;
  FMajorTicks := TOBDLinearGaugeMajorTicks.Create;
  FMajorTicks.OnChange := SettingsChanged;
  FMinorTicks := TOBDLinearGaugeMinorTicks.Create;
  FMinorTicks.OnChange := SettingsChanged;
  FSlider := TOBDLinearGaugeSlider.Create;
  FSlider.OnChange := SliderSettingsChanged;
  FStartCaption := TOBDLinearGaugeCaption.Create;
  FStartCaption.OnChange := SettingsChanged;
  FEndCaption := TOBDLinearGaugeCaption.Create;
  FEndCaption.OnChange := SettingsChanged;
  FAnimation := TOBDLinearGaugeAnimation.Create;
  FAnimation.OnChange := AnimationChanged;
  FAnimation.Value := FValue;
  FGradientScaleItems := TOBDLinearGaugeGradientScaleItems.Create(Self);
  FGradientScaleItems.OnChange := SettingsChanged;
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;
  
  if not (csDesigning in ComponentState) then
    AnimationManager.RegisterControl(Self);
  
  Width := 300;
  Height := 80;
end;

destructor TOBDLinearGauge.Destroy;
begin
  if not (csDesigning in ComponentState) then
    AnimationManager.UnregisterControl(Self);
  FRenderLock.Free;
  FBackground.Free;
  FBorder.Free;
  FScale.Free;
  FMajorTicks.Free;
  FMinorTicks.Free;
  FSlider.Free;
  FStartCaption.Free;
  FEndCaption.Free;
  FAnimation.Free;
  FGradientScaleItems.Free;
  inherited Destroy;
end;

procedure TOBDLinearGauge.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TOBDLinearGauge) then
  begin
    FOrientation := (Source as TOBDLinearGauge).Orientation;
    FScalePosition := (Source as TOBDLinearGauge).ScalePosition;
    FMin := (Source as TOBDLinearGauge).Min;
    FMax := (Source as TOBDLinearGauge).Max;
    FBackground.Assign((Source as TOBDLinearGauge).Background);
    FBorder.Assign((Source as TOBDLinearGauge).Border);
    FScale.Assign((Source as TOBDLinearGauge).Scale);
    FMajorTicks.Assign((Source as TOBDLinearGauge).MajorTicks);
    FMinorTicks.Assign((Source as TOBDLinearGauge).MinorTicks);
    FSlider.Assign((Source as TOBDLinearGauge).Slider);
    FStartCaption.Assign((Source as TOBDLinearGauge).StartCaption);
    FEndCaption.Assign((Source as TOBDLinearGauge).EndCaption);
    FAnimation.Assign((Source as TOBDLinearGauge).Animation);
    FGradientScaleItems.Assign((Source as TOBDLinearGauge).GradientScale);
  end;
  InvalidateBackground;
  Redraw;
  Invalidate;
end;

end.
