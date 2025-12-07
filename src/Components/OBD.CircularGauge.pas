//------------------------------------------------------------------------------
// UNIT           : OBD.CircularGauge.pas
// CONTENTS       : Circular gauge component with Skia rendering
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 22/03/2024
// UPDATED        : 06/12/2025 - Refactored for direct Skia rendering
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.CircularGauge;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Diagnostics, Vcl.Controls, 
  WinApi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Themes, System.Skia, Vcl.Skia, System.Types,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Animation,
  OBD.CustomControl.AnimationManager;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Margin from the border (Otherwise the gauge border is flattened on the side)
  /// </summary>
  MARGIN_FROM_BORDER = 2;

  /// <summary>
  ///   Default start angle: Starting from bottom left
  /// </summary>
  DEFAULT_START_ANGLE = 135;
  /// <summary>
  ///   Default end angle: Ending at bottom right
  /// </summary>
  DEFAULT_END_ANGLE = 225;
  /// <summary>
  ///   Minimum starting angle
  /// </summary>
  MIN_START_ANGLE: Single = 0;
  /// <summary>
  ///   Minimum ending angle
  /// </summary>
  MIN_END_ANGLE: Single = 0;
  /// <summary>
  ///   Maximum starting angle, allowing a full circle
  /// </summary>
  MAX_START_ANGLE: Single = 360;
  /// <summary>
  ///   Maximum ending angle, allowing a full circle
  /// </summary>
  MAX_END_ANGLE: Single = 360;

  /// <summary>
  ///   Min value
  /// </summary>
  DEFAULT_MIN = 0;
  /// <summary>
  ///   Max value
  /// </summary>
  DEFAULT_MAX = 100;

  /// <summary>
  ///   Default minor tick step
  /// </summary>
  DEFAULT_MINOR_STEP = 1;
  /// <summary>
  ///   Default major tick step
  /// </summary>
  DEFAULT_MAJOR_STEP = 10;
  /// <summary>
  ///   Default minor tick length
  /// </summary>
  DEFAULT_MINOR_LENGTH = 5;
  /// <summary>
  ///   Default major tick length
  /// </summary>
  DEFAULT_MAJOR_LENGTH = 15;

  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = $00FBF5F7;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00BBAEAC;

  /// <summary>
  ///   Default border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00918888;
  /// <summary>
  ///   Default border to color
  /// </summary>
  DEFAULT_BORDER_TO = $00776F6F;
  /// <summary>
  ///   Default border width
  /// </summary>
  DEFAULT_BORDER_WIDTH = 6;

  /// <summary>
  ///   Default needle length
  /// </summary>
  DEFAULT_NEEDLE_LENGTH = 0.7;
  /// <summary>
  ///   Default needle width
  /// </summary>
  DEFAULT_NEEDLE_WIDTH = 7;
  /// <summary>
  ///   Default needle color
  /// </summary>
  DEFAULT_NEEDLE_COLOR = $00DF7000;
  /// <summary>
  ///   Default needle border width
  /// </summary>
  DEFAULT_NEEDLE_BORDER_WIDTH = 1;
  /// <summary>
  ///   Default needle border color
  /// </summary>
  DEFAULT_NEEDLE_BORDER_COLOR = $00575757;
  /// <summary>
  ///   Default needle center size
  /// </summary>
  DEFAULT_NEEDLE_CENTER_SIZE = 20;
  /// <summary>
  ///   Default needle center color
  /// </summary>
  DEFAULT_NEEDLE_CENTER_COLOR = $00918888;
  /// <summary>
  ///   Default needle center border width
  /// </summary>
  DEFAULT_NEEDLE_CENTER_BORDER_WIDTH = 2;
  /// <summary>
  ///   Default needle center border color
  /// </summary>
  DEFAULT_NEEDLE_CENTER_BORDER_COLOR = $00776F6F;

  /// <summary>
  ///   Default animation enabled
  /// </summary>
  DEFAULT_ANIMATION_ENABLED = True;
  /// <summary>
  ///   Default animation duration
  /// </summary>
  DEFAULT_ANIMATION_DURATION = 1500;
  /// <summary>
  ///   Default animation type
  /// </summary>
  DEFAULT_ANIMATION_TYPE = anQuartEaseInOut;

  /// <summary>
  ///   Default gradient scale size
  /// </summary>
  DEFAULT_GRADIENT_SCALE_SIZE = 10;
  /// <summary>
  ///   Default gradient scale width
  /// </summary>
  DEFAULT_GRADIENT_SCALE_COLOR = clHighlight;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Circular Gauge background properties
  /// </summary>
  TOBDCircularGaugeBackground = class(TPersistent)
  private
    /// <summary>
    ///   Gradient from color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    FToColor: TColor;

    /// <summary>
    ///   Set gradient from color
    /// </summary
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set gradient to color
    /// </summary
    procedure SetToColor(Value: TColor);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Gradient from color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BACKGROUND_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BACKGROUND_TO;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge border properties
  /// </summary>
  TOBDCircularGaugeBorder = class(TPersistent)
  private
    /// <summary>
    ///   Gradient from color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    FToColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FWidth: Integer;

    /// <summary>
    ///   Set gradient from color
    /// </summary
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set gradient to color
    /// </summary
    procedure SetToColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetWidth(Value: Integer);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Gradient from color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_TO;
    /// <summary>
    ///   Border width
    /// </summary>
    property Width: Integer read FWidth write SetWidth default DEFAULT_BORDER_WIDTH;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge tick properties
  /// </summary>
  TOBDCircularGaugeTick = class(TPersistent)
  private
    /// <summary>
    ///   Tick step
    /// </summary>
    FStep: Single;
    /// <summary>
    ///   Tick line length
    /// </summary>
    FLength: Single;
    /// <summary>
    ///   Tick line width
    /// </summary>
    FWidth: Single;
    /// <summary>
    ///   Do we need to draw a label for this tick?
    /// </summary>
    FShowLabel: Boolean;
    /// <summary>
    ///   Tick line color
    /// </summary>
    FColor: TColor;
    /// <summary>
    ///   Tick label font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Tick value label divider
    /// </summary>
    FDivider: Single;
    /// <summary>
    ///   Tick offset (space from border)
    /// </summary>
    FOffset: Single;

    /// <summary>
    ///   Set tick step
    /// </summary>
    procedure SetStep(Value: Single);
    /// <summary>
    ///   Set tick line length
    /// </summary>
    procedure SetLength(Value: Single);
    /// <summary>
    ///   Set tick line width
    /// </summary>
    procedure SetWidth(Value: Single);
    /// <summary>
    ///   Set show tick label
    /// </summary>
    procedure SetShowLabel(Value: Boolean);
    /// <summary>
    ///   Set tick line color
    /// </summary>
    procedure SetColor(Value: TColor);
    /// <summary>
    ///   Set tick label font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set tick value label divider
    /// </summary>
    procedure SetDivider(Value: Single);
    /// <summary>
    ///   Set tick offset
    /// </summary>
    procedure SetOffset(Value: Single);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Font changed event handler
    /// </summary>
    procedure FontChanged(Sender: TObject);
    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Tick step
    /// </summary>
    property Step: Single read FStep write SetStep;
    /// <summary>
    ///   Tick line length
    /// </summary>
    property Length: Single read FLength write SetLength;
    /// <summary>
    ///   Tick line width
    /// </summary>
    property Width: Single read FWidth write SetWidth;
    /// <summary>
    ///   Do we need to draw a label for this tick?
    /// </summary>
    property ShowLabel: Boolean read FShowLabel write SetShowLabel;
    /// <summary>
    ///   Tick line color
    /// </summary>
    property Color: TColor read FColor write SetColor;
    /// <summary>
    ///   Tick label font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Tick value label divider
    /// </summary>
    property Divider: Single read FDivider write SetDivider;
    /// <summary>
    ///   Tick offset (space from border)
    /// </summary>
    property Offset: Single read FOffset write SetOffset;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge major tick properties
  /// </summary>
  TOBDCircularGaugeMajorTicks = class(TOBDCircularGaugeTick)
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; override;
  end;

  /// <summary>
  ///   Circular Gauge minor tick properties
  /// </summary>
  TOBDCircularGaugeMinorTicks = class(TOBDCircularGaugeTick)
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; override;
  end;

  /// <summary>
  ///   Circular Gauge needle properties
  /// </summary>
  TOBDCircularGaugeNeedle = class(TPersistent)
  private
    /// <summary>
    ///   Length of the needle
    /// </summary>
    FLength: Single;
    /// <summary>
    ///   Width of the needle
    /// </summary>
    FWidth: Single;
    /// <summary>
    ///   Needle fill color
    /// </summary>
    FColor: TColor;
    /// <summary>
    ///   Needle border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Needle border width
    /// </summary>
    FBorderWidth: Single;
    /// <summary>
    ///   Center color
    /// </summary>
    FCenterColor: TColor;
    /// <summary>
    ///   Center size (Width/Height)
    /// </summary>
    FCenterSize: Single;
    /// <summary>
    ///   Center border color
    /// </summary>
    FCenterBorderColor: TColor;
    /// <summary>
    ///   Center border size
    /// </summary>
    FCenterBorderWidth: Single;

    /// <summary>
    ///   Set needle length
    /// </summary>
    procedure SetLength(Value: Single);
    /// <summary>
    ///   Set needle width
    /// </summary>
    procedure SetWidth(Value: Single);
    /// <summary>
    ///   Set needle color
    /// </summary>
    procedure SetColor(Value: TColor);
    /// <summary>
    ///   Set needle border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set needle border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
    /// <summary>
    ///   Set center color
    /// </summary>
    procedure SetCenterColor(Value: TColor);
    /// <summary>
    ///   Set center size
    /// </summary>
    procedure SetCenterSize(Value: Single);
    /// <summary>
    ///   Set center border color
    /// </summary>
    procedure SetCenterBorderColor(Value: TColor);
    /// <summary>
    ///   Set center border width
    /// </summary>
    procedure SetCenterBorderWidth(Value: Single);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Length of the needle
    /// </summary>
    property Length: Single read FLength write SetLength;
    /// <summary>
    ///   Width of the needle
    /// </summary>
    property Width: Single read FWidth write SetWidth;
    /// <summary>
    ///   Needle fill color
    /// </summary>
    property Color: TColor read FColor write SetColor default DEFAULT_NEEDLE_COLOR;
    /// <summary>
    ///   Needle border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_NEEDLE_BORDER_COLOR;
    /// <summary>
    ///   Needle border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    /// <summary>
    ///   Center color
    /// </summary>
    property CenterColor: TColor read FCenterColor write SetCenterColor default DEFAULT_NEEDLE_CENTER_COLOR;
    /// <summary>
    ///   Center size (Width/Height)
    /// </summary>
    property CenterSize: Single read FCenterSize write SetCenterSize;
    /// <summary>
    ///   Center border color
    /// </summary>
    property CenterBorderColor: TColor read FCenterBorderColor write SetCenterBorderColor default DEFAULT_NEEDLE_CENTER_BORDER_COLOR;
    /// <summary>
    ///   Center border size
    /// </summary>
    property CenterBorderWidth: Single read FCenterBorderWidth write SetCenterBorderWidth;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge caption
  /// </summary>
  TOBDCircularGaugeCaption = class(TPersistent)
  private
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: string;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Offset
    /// </summary>
    FOffset: Single;

    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: string);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set offset
    /// </summary>
    procedure SetOffset(Value: Single);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Font changed handler
    /// </summary>
    procedure FontChanged(Sender: TObject);
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Offset
    /// </summary>
    property Offset: Single read FOffset write SetOffset;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge animation
  /// </summary>
  TOBDCircularGaugeAnimation = class(TPersistent)
  private
    /// <summary>
    ///   Is animation enabled
    /// </summary>
    FEnabled: Boolean;
    /// <summary>
    ///   Duration of the animation
    /// </summary>
    FDuration: Cardinal;
    /// <summary>
    ///   Type of animation
    /// </summary>
    FType: TOBDCustomControlAnimationType;

    /// <summary>
    ///   Set enabled
    /// </summary>
    procedure SetEnabled(Value: Boolean);
    /// <summary>
    ///   Set duration
    /// </summary>
    procedure SetDuration(Value: Cardinal);
  private
    /// <summary>
    ///   Value used for animation
    /// </summary>
    FValue: Single;
    /// <summary>
    ///   Start time of the animation
    /// </summary>
    FStartTime: Cardinal;
    /// <summary>
    ///   Start value of the animation
    /// </summary>
    FStartValue: Single;
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Value used for animation
    /// </summary>
    property Value: Single read FValue write FValue;
    /// <summary>
    ///   Start time of the animation
    /// </summary>
    property StartTime: Cardinal read FStartTime write FStartTime;
    /// <summary>
    ///   Start value of the animation
    /// </summary>
    property StartValue: Single read FStartValue write FStartValue;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Is animation enabled
    /// </summary>
    property Enabled: Boolean read FEnabled write SetEnabled default DEFAULT_ANIMATION_ENABLED;
    /// <summary>
    ///   Animation duration
    /// </summary>
    property Duration: Cardinal read FDuration write SetDuration default DEFAULT_ANIMATION_DURATION;
    /// <summary>
    ///   Type of animation
    /// </summary>
    property &Type: TOBDCustomControlAnimationType read FType write FType default DEFAULT_ANIMATION_TYPE;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge gradient scale item
  /// </summary>
  TOBDCircularGaugeGradientScaleItem = class(TCollectionItem)
  private
    /// <summary>
    ///   From value
    /// </summary>
    FFrom: Single;
    /// <summary>
    ///   To value
    /// </summary>
    FTo: Single;
    /// <summary>
    ///   Color
    /// </summary>
    FColor: TColor;
    /// <summary>
    ///   Size
    /// </summary>
    FSize: Single;

    /// <summary>
    ///   Set from value
    /// </summary>
    procedure SetFrom(Value: Single);
    /// <summary>
    ///   Set to value
    /// </summary>
    procedure SetTo(Value: Single);
    /// <summary>
    ///   Set color
    /// </summary>
    procedure SetColor(Value: TColor);
    /// <summary>
    ///   Set size
    /// </summary>
    procedure SetSize(Value: Single);
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Collection: TCollection); override;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   From value
    /// </summary>
    property From: Single read FFrom write SetFrom;
    /// <summary>
    ///   To value
    /// </summary>
    property &To: Single read FTo write SetTo;
    /// <summary>
    ///   Color
    /// </summary>
    property Color: TColor read FColor write SetColor default DEFAULT_GRADIENT_SCALE_COLOR;
    /// <summary>
    ///   Size
    /// </summary>
    property Size: Single read FSize write SetSize;
  end;

  /// <summary>
  ///   Circular Gauge gradient scale collection
  /// </summary>
  TOBDCircularGaugeGradientScaleItems = class(TOwnedCollection)
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;

    /// <summary>
    ///   Get Item
    /// </summary>
    function GetItem(Index: Integer): TOBDCircularGaugeGradientScaleItem;
    /// <summary>
    ///   Set Item
    /// </summary>
    procedure SetItem(Index: Integer; const Value: TOBDCircularGaugeGradientScaleItem);
  protected
    /// <summary>
    ///   Item changed handler
    /// </summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TPersistent); virtual;

    /// <summary>
    ///   Add new item
    /// </summary>
    function Add: TOBDCircularGaugeGradientScaleItem;
    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Items
    /// </summary>
    property Items[Index: Integer]: TOBDCircularGaugeGradientScaleItem read GetItem write SetItem;
  published
    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge Component
  /// </summary>
  TOBDCircularGauge = class(TOBDCustomControl, IOBDAnimatable)
  private
    /// <summary>
    ///   Cached Skia snapshot of the static gauge background for reuse between frames.
    /// </summary>
    FBackgroundSnapshot: ISkImage;
    /// <summary>
    ///   Render lock to coordinate background and needle updates across threads.
    /// </summary>
    FRenderLock: TObject;
    /// <summary>
    ///   Animation start time in milliseconds
    /// </summary>
    FAnimationStartMs: Int64;
    /// <summary>
    ///   Stopwatch for high-resolution timing
    /// </summary>
    FStopwatch: TStopwatch;
  private
    /// <summary>
    ///   Start angle
    /// </summary>
    FStartAngle: Single;
    /// <summary>
    ///   End angle
    /// </summary>
    FEndAngle: Single;
    /// <summary>
    ///   Min
    /// </summary>
    FMin: Single;
    /// <summary>
    ///   Max
    /// </summary>
    FMax: Single;
    /// <summary>
    ///   Value
    /// </summary>
    FValue: Single;
    /// <summary>
    ///   Gauge background
    /// </summary>
    FBackground: TOBDCircularGaugeBackground;
    /// <summary>
    ///   Gauge border
    /// </summary>
    FBorder: TOBDCircularGaugeBorder;
    /// <summary>
    ///   Major ticks
    /// </summary>
    FMajorTicks: TOBDCircularGaugeMajorTicks;
    /// <summary>
    ///   Minor ticks
    /// </summary>
    FMinorTicks: TOBDCircularGaugeMinorTicks;
    /// <summary>
    ///   Needle
    /// </summary>
    FNeedle: TOBDCircularGaugeNeedle;
    /// <summary>
    ///   Top caption
    /// </summary>
    FTopCaption: TOBDCircularGaugeCaption;
    /// <summary>
    ///   Bottom caption
    /// </summary>
    FBottomCaption: TOBDCircularGaugeCaption;
    /// <summary>
    ///   Animation
    /// </summary>
    FAnimation: TOBDCircularGaugeAnimation;
    /// <summary>
    ///   Gradient scale items
    /// </summary>
    FGradientScaleItems: TOBDCircularGaugeGradientScaleItems;

    /// <summary>
    ///   Set start angle
    /// </summary>
    procedure SetStartAngle(Value: Single);
    /// <summary>
    ///   Set end angle
    /// </summary>
    procedure SetEndAngle(Value: Single);
    /// <summary>
    ///   Set min
    /// </summary>
    procedure SetMin(Value: Single);
    /// <summary>
    ///   Set max
    /// </summary>
    procedure SetMax(Value: Single);
    /// <summary>
    ///   Set value
    /// </summary>
    procedure SetValue(Value: Single);
    /// <summary>
    ///   Set background properties
    /// </summary>
    procedure SetBackground(Value: TOBDCircularGaugeBackground);
    /// <summary>
    ///   Set border properties
    /// </summary>
    procedure SetBorder(Value: TOBDCircularGaugeBorder);
    /// <summary>
    ///   Set major ticks
    /// </summary>
    procedure SetMajorTicks(Value: TOBDCircularGaugeMajorTicks);
    /// <summary>
    ///   Set minor ticks
    /// </summary>
    procedure SetMinorTicks(Value: TOBDCircularGaugeMinorTicks);
    /// <summary>
    ///   Set needle
    /// </summary>
    procedure SetNeedle(Value: TOBDCircularGaugeNeedle);
    /// <summary>
    ///   Set top caption
    /// </summary>
    procedure SetTopCaption(Value: TOBDCircularGaugeCaption);
    /// <summary>
    ///   Set bottom caption
    /// </summary>
    procedure SetBottomCaption(Value: TOBDCircularGaugeCaption);
    /// <summary>
    ///   Set animation
    /// </summary>
    procedure SetAnimation(Value: TOBDCircularGaugeAnimation);
    /// <summary>
    ///   Set gradient scale items
    /// </summary>
    procedure SetGradientScaleItems(Value: TOBDCircularGaugeGradientScaleItems);
  protected
    /// <summary>
    ///   Invalidate background (Repaint background buffer)
    /// </summary>
    procedure InvalidateBackground; virtual;
    /// <summary>
    ///   Paint the needle on the provided Skia canvas
    /// </summary>
    procedure PaintNeedle(Canvas: ISkCanvas); virtual;
    /// <summary>
    ///   Override PaintSkia to render gauge with Skia
    /// </summary>
    procedure PaintSkia(Canvas: ISkCanvas); override;
    /// <summary>
    ///   Builds the static background snapshot while the render lock is held.
    /// </summary>
    procedure BuildBackgroundSnapshot;
    /// <summary>
    ///   Retrieves the cached background snapshot, constructing it when missing in a thread-safe way.
    /// </summary>
    function AcquireBackgroundSnapshot: ISkImage;
    /// <summary>
    ///   On change handler
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   On needle change handler
    /// </summary>
    procedure NeedleSettingsChanged(Sender: TObject);
    /// <summary>
    ///   On animation changed handler
    /// </summary>
    procedure AnimationChanged(Sender: TObject);
  protected
    /// <summary>
    ///   Override Resize method
    /// </summary>
    procedure Resize; override;
    /// <summary>
    ///   Override Loaded method
    /// </summary>
    procedure Loaded; override;
    /// <summary>
    ///   Override WndProc method
    /// </summary>
    procedure WndProc(var Message: TMessage); override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    // IOBDAnimatable interface methods
    /// <summary>
    ///   Called on each animation tick
    /// </summary>
    procedure AnimationTick(ElapsedMs: Int64);
    /// <summary>
    ///   Returns true if the control has active animations
    /// </summary>
    function IsAnimating: Boolean;
    /// <summary>
    ///   Get the desired frames per second for this control
    /// </summary>
    function GetFramesPerSecond: Integer;
  published
    /// <summary>
    ///   Start angle
    /// </summary>
    property StartAngle: Single read FStartAngle write SetStartAngle;
    /// <summary>
    ///   End angle
    /// </summary>
    property EndAngle: Single read FEndAngle write SetEndAngle;
    /// <summary>
    ///   Min
    /// </summary>
    property Min: Single read FMin write SetMin;
    /// <summary>
    ///   Max
    /// </summary>
    property Max: Single read FMax write SetMax;
    /// <summary>
    ///   Value
    /// </summary>
    property Value: Single read FValue write SetValue;
    /// <summary>
    ///   Gauge background
    /// </summary>
    property Background: TOBDCircularGaugeBackground read FBackground write SetBackground;
    /// <summary>
    ///   Gauge border
    /// </summary>
    property Border: TOBDCircularGaugeBorder read FBorder write SetBorder;
    /// <summary>
    ///   Major ticks
    /// </summary>
    property MajorTicks: TOBDCircularGaugeMajorTicks read FMajorTicks write SetMajorTicks;
    /// <summary>
    ///   Minor ticks
    /// </summary>
    property MinorTicks: TOBDCircularGaugeMinorTicks read FMinorTicks write SetMinorTicks;
    /// <summary>
    ///   Needle
    /// </summary>
    property Needle: TOBDCircularGaugeNeedle read FNeedle write SetNeedle;
    /// <summary>
    ///   Top caption
    /// </summary>
    property TopCaption: TOBDCircularGaugeCaption read FTopCaption write SetTopCaption;
    /// <summary>
    ///   Bottom caption
    /// </summary>
    property BottomCaption: TOBDCircularGaugeCaption read FBottomCaption write SetBottomCaption;
    /// <summary>
    ///   Animation
    /// </summary>
    property Animation: TOBDCircularGaugeAnimation read FAnimation write SetAnimation;
    /// <summary>
    ///   Gradient scale items
    /// </summary>
    property GradientScale: TOBDCircularGaugeGradientScaleItems read FGradientScaleItems write SetGradientScaleItems;
  end;

implementation

uses
  System.Math;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeBackground.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set the new from color
    FFromColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeBackground.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set the new to color
    FToColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeBackground.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BACKGROUND_FROM;
  FToColor := DEFAULT_BACKGROUND_TO;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDCircularGaugeBackground) then
  begin
    FFromColor := (Source as TOBDCircularGaugeBackground).FromColor;
    FToColor := (Source as TOBDCircularGaugeBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeBorder.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set the new from color
    FFromColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeBorder.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set the new to color
    FToColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeBorder.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    // Set the new width
    FWidth := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeBorder.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BORDER_FROM;
  FToColor := DEFAULT_BORDER_TO;
  FWidth := DEFAULT_BORDER_WIDTH;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDCircularGaugeBorder) then
  begin
    FFromColor := (Source as TOBDCircularGaugeBorder).FromColor;
    FToColor := (Source as TOBDCircularGaugeBorder).ToColor;
    FWidth := (Source as TOBDCircularGaugeBorder).Width;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET STEP
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.SetStep(Value: Single);
begin
  if (FStep <> Value) and (Value >= 1) then
  begin
    // Set new step
    FStep := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET LENGTH
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.SetLength(Value: Single);
begin
  if (FLength <> Value) and (Value >= 2) then
  begin
    // Set new length
    FLength := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.SetWidth(Value: Single);
begin
  if (FWidth <> Value) and (Value >= 1) then
  begin
    // Set new width
    FWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET SHOW LABEL
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.SetShowLabel(Value: Boolean);
begin
  if (FShowLabel <> Value) then
  begin
    // Set new show label
    FShowLabel := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    // Set new color
    FColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.SetFont(Value: TFont);
begin
  // Assign font
  FFont.Assign(Font);
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// SET DIVIDER
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.SetDivider(Value: Single);
begin
  if (FDivider <> Value) and (Value >= 0) then
  begin
    // Set new divider
    FDivider := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TOBDCircularGaugeTick.SetOffset(Value: Single);
begin
  if (FOffset <> Value) and (Value >= 0) then
  begin
    // Set new offset
    FOffset := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeTick.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create label font
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  // Set defaults
  FWidth := 1;
  FColor := clBlack;
  FDivider := 0;
  FOffset := 2;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDCircularGaugeTick.Destroy;
begin
  // Free label font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// FONT CHANGED
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.FontChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeTick.Assign(Source: TPersistent);
begin
  if (Source is TOBDCircularGaugeTick) then
  begin
    FStep := (Source as TOBDCircularGaugeTick).Step;
    FLength := (Source as TOBDCircularGaugeTick).Length;
    FWidth := (Source as TOBDCircularGaugeTick).Width;
    FShowLabel := (Source as TOBDCircularGaugeTick).ShowLabel;
    FColor := (Source as TOBDCircularGaugeTick).Color;
    FFont.Assign((Source as TOBDCircularGaugeTick).Font);
    FDivider := (Source as TOBDCircularGaugeTick).Divider;
    FOffset := (Source as TOBDCircularGaugeTick).Offset;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeMajorTicks.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FStep := DEFAULT_MAJOR_STEP;
  FLength := DEFAULT_MAJOR_LENGTH;
  FShowLabel := True;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeMinorTicks.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FStep := DEFAULT_MINOR_STEP;
  FLength := DEFAULT_MINOR_LENGTH;
  FShowLabel := False;
end;

//------------------------------------------------------------------------------
// SET NEEDLE LENGTH
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetLength(Value: Single);
begin
  if (FLength <> Value) and (Value >= 0.1) and (Value <= 1) then
  begin
    // Set new length
    FLength := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET NEEDLE WIDTH
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetWidth(Value: Single);
begin
  if (FWidth <> Value) and (Value >= 1) then
  begin
    // Set new width
    FWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET NEEDLE COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    // Set new color
    FColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetBorderColor(Value: TColor);
begin
 if (FBorderColor <> Value) then
  begin
    // Set new border color
    FBorderColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER WIDTH
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetBorderWidth(Value: Single);
begin
  if (FBorderWidth <> Value) and (Value >= 0) then
  begin
    // Set new border width
    FBorderWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CENTER COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetCenterColor(Value: TColor);
begin
  if (FCenterColor <> Value) then
  begin
    // Set new center color
    FCenterColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CENTER SIZE
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetCenterSize(Value: Single);
begin
  if (FCenterSize <> Value) and (Value >= 0) then
  begin
    // Set new center size
    FCenterSize := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CENTER BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetCenterBorderColor(Value: TColor);
begin
  if (FCenterBorderColor <> Value) then
  begin
    // Set new center border color
    FCenterBorderColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CENTER BORDER WIDTH
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.SetCenterBorderWidth(Value: Single);
begin
  if (FCenterBorderWidth <> Value) and (Value >= 0) then
  begin
    // Set new center border width
    FCenterBorderWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeNeedle.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FLength := DEFAULT_NEEDLE_LENGTH;
  FWidth := DEFAULT_NEEDLE_WIDTH;
  FColor := DEFAULT_NEEDLE_COLOR;
  FBorderColor := DEFAULT_NEEDLE_BORDER_COLOR;
  FBorderWidth := DEFAULT_NEEDLE_BORDER_WIDTH;
  FCenterColor := DEFAULT_NEEDLE_CENTER_COLOR;
  FCenterSize  := DEFAULT_NEEDLE_CENTER_SIZE;
  FCenterBorderColor := DEFAULT_NEEDLE_CENTER_BORDER_COLOR;
  FCenterBorderWidth := DEFAULT_NEEDLE_CENTER_BORDER_WIDTH;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeNeedle.Assign(Source: TPersistent);
begin
  if (Source is TOBDCircularGaugeNeedle) then
  begin
    Flength := (Source as TOBDCircularGaugeNeedle).Length;
    FWidth := (Source as TOBDCircularGaugeNeedle).Width;
    FColor := (Source as TOBDCircularGaugeNeedle).Color;
    FBorderColor := (Source as TOBDCircularGaugeNeedle).BorderColor;
    FBorderWidth := (Source as TOBDCircularGaugeNeedle).BorderWidth;
    FCenterColor := (Source as TOBDCircularGaugeNeedle).CenterColor;
    FCenterSize  := (Source as TOBDCircularGaugeNeedle).CenterSize;
    FCenterBorderColor := (Source as TOBDCircularGaugeNeedle).CenterBorderColor;
    FCenterBorderWidth := (Source as TOBDCircularGaugeNeedle).CenterBorderWidth;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeCaption.SetCaption(Value: string);
begin
  if (FCaption <> Value) then
  begin
    // Set new caption
    FCaption := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeCaption.SetFont(Value: TFont);
begin
  // Assign font
  FFont.Assign(Value);
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// SET OFFSET
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeCaption.SetOffset(Value: Single);
begin
  if (FOffset <> Value) then
  begin
    // Set new offset
    FOffset := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// FONT CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeCaption.FontChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeCaption.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDCircularGaugeCaption.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeCaption.Assign(Source: TPersistent);
begin
  if (Source is TOBDCircularGaugeCaption) then
  begin
    FCaption := (Source as TOBDCircularGaugeCaption).Caption;
    FFont.Assign((Source as TOBDCircularGaugeCaption).Font);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET ENABLED
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeAnimation.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    // Set enabled
    FEnabled := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DURATION
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeAnimation.SetDuration(Value: Cardinal);
begin
  if (FDuration <> Value) and (Value >= 10) then
  begin
    // Set duration
    FDuration := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeAnimation.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FType := DEFAULT_ANIMATION_TYPE;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeAnimation.Assign(Source: TPersistent);
begin
  if (Source is TOBDCircularGaugeAnimation) then
  begin
    FEnabled := (Source as TOBDCircularGaugeAnimation).Enabled;
    FDuration := (Source as TOBDCircularGaugeAnimation).Duration;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM VALUE
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItem.SetFrom(Value: Single);
begin
  if (FFrom <> Value) then
  begin
    // Set from value
    FFrom := Value;
    // Set to value
    if FFrom > FTo then FTo := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET TO VALUE
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItem.SetTo(Value: Single);
begin
  if (FTo <> Value) then
  begin
    // Set to value
    FTo := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItem.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    // Set color
    FColor := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET SIZE
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItem.SetSize(Value: Single);
begin
  if (FSize <> Value) then
  begin
    // Set size
    FSize := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeGradientScaleItem.Create(Collection: TCollection);
begin
  // Call inherited constructor
  inherited Create(Collection);
  // Set defaults
  FFrom := 0;
  FTo := 0;
  FColor := DEFAULT_GRADIENT_SCALE_COLOR;
  FSize := DEFAULT_GRADIENT_SCALE_SIZE;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItem.Assign(Source: TPersistent);
begin
  if (Source is TOBDCircularGaugeGradientScaleItem) then
  begin
    FFrom := (Source as TOBDCircularGaugeGradientScaleItem).From;
    FTo := (Source as TOBDCircularGaugeGradientScaleItem).&To;
    FColor := (Source as TOBDCircularGaugeGradientScaleItem).Color;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// GET ITEM
//------------------------------------------------------------------------------
function TOBDCircularGaugeGradientScaleItems.GetItem(Index: Integer): TOBDCircularGaugeGradientScaleItem;
begin
  Result := TOBDCircularGaugeGradientScaleItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------
// SET ITEM
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItems.SetItem(Index: Integer; const Value: TOBDCircularGaugeGradientScaleItem);
begin
  // Call inherited setitem
  inherited SetItem(Index, Value);
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// ITEM CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItems.Update(Item: TCollectionItem);
begin
  // Call inherited update
  inherited Update(Item);
  // Notify changes
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGaugeGradientScaleItems.Create(AOwner: TPersistent);
begin
  // Call inherited constructor
  inherited Create(AOwner, TOBDCircularGaugeGradientScaleItem);
end;

//------------------------------------------------------------------------------
// ADD ITEM
//------------------------------------------------------------------------------
function TOBDCircularGaugeGradientScaleItems.Add: TOBDCircularGaugeGradientScaleItem;
begin
  Result := TOBDCircularGaugeGradientScaleItem(inherited Add);
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGaugeGradientScaleItems.Assign(Source: TPersistent);
var
  Items: TOBDCircularGaugeGradientScaleItems;
  I : Integer;
begin
  if (Source is TOBDCircularGaugeGradientScaleItems)  then
  begin
    Items := TOBDCircularGaugeGradientScaleItems(Source);
    Clear;
    for I := 0 to Items.Count - 1 do Add.Assign(Items.Items[I]);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET START ANGLE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetStartAngle(Value: Single);
begin
  // Validate and clamp angle to valid range
  if Value < MIN_START_ANGLE then Value := MIN_START_ANGLE;
  if Value > MAX_START_ANGLE then Value := MAX_START_ANGLE;
  
  if (FStartAngle <> Value) then
  begin
    // Set new start angle
    FStartAngle := Value;
    // Invalidate the background buffer
    InvalidateBackground;
    // Redraw Skia
    Redraw;
    // Invalidate buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET END ANGLE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetEndAngle(Value: Single);
begin
  // Validate and clamp angle to valid range
  if Value < MIN_END_ANGLE then Value := MIN_END_ANGLE;
  if Value > MAX_END_ANGLE then Value := MAX_END_ANGLE;
  
  if (FEndAngle <> Value) then
  begin
    // Set new end angle
    FEndAngle := Value;
    // Invalidate the background buffer
    InvalidateBackground;
    // Redraw Skia
    Redraw;
    // Invalidate buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET MIN
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetMin(Value: Single);
begin
  // Ensure Min is not greater than Max
  if Value > FMax then Value := FMax;
  
  if (FMin <> Value) then
  begin
    // Set new min
    FMin := Value;
    // Clamp current value to new range
    if FValue < FMin then
      FValue := FMin;
    // Invalidate the background buffer
    InvalidateBackground;
    // Redraw Skia
    Redraw;
    // Invalidate buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET MAX
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetMax(Value: Single);
begin
  // Ensure Max is not less than Min
  if Value < FMin then Value := FMin;
  
  if (FMax <> Value) then
  begin
    // Set new max
    FMax := Value;
    // Clamp current value to new range
    if FValue > FMax then
      FValue := FMax;
    // Invalidate the background buffer
    InvalidateBackground;
    // Redraw Skia
    Redraw;
    // Invalidate buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET VALUE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetValue(Value: Single);
begin
  if (FValue <> Value) then
  begin
    if (Value < FMin) then Value := FMin;
    if (Value > FMax) then Value := FMax;
    
    // Set value
    FValue := Value;
    
    // Handle animation
    if Animation.Enabled and not (csDesigning in ComponentState) then
    begin
      // Animation is enabled: set up animation parameters
      Animation.StartValue := Animation.Value;  // Start from current animated position
      FAnimationStartMs := FStopwatch.ElapsedMilliseconds;
      // Notify animation manager to start timer
      AnimationManager.CheckAnimationState;
    end
    else
    begin
      // Animation is disabled or at design time: update immediately
      Animation.Value := FValue;
    end;
    
    // Redraw Skia
    Redraw;
    // Invalidate buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetBackground(Value: TOBDCircularGaugeBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetBorder(Value: TOBDCircularGaugeBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET MAJOR TICKS
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetMajorTicks(Value: TOBDCircularGaugeMajorTicks);
begin
  FMajorTicks.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET MINOR TICKS
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetMinorTicks(Value: TOBDCircularGaugeMinorTicks);
begin
  FMinorTicks.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET NEEDLE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetNeedle(Value: TOBDCircularGaugeNeedle);
begin
  FNeedle.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET TOP CAPTION
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetTopCaption(Value: TOBDCircularGaugeCaption);
begin
  FTopCaption.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BOTTOM CAPTION
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetBottomCaption(Value: TOBDCircularGaugeCaption);
begin
  FBottomCaption.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ANIMATION
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetAnimation(Value: TOBDCircularGaugeAnimation);
begin
  FAnimation.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET GRADIENT SCALE ITEMS
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetGradientScaleItems(Value: TOBDCircularGaugeGradientScaleItems);
begin
  FGradientScaleItems.Assign(Value);
end;

//------------------------------------------------------------------------------
// INVALIDATE BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.InvalidateBackground;
begin
  // Safety check: FRenderLock might not be initialized yet during parent constructor
  if not Assigned(FRenderLock) then
    Exit;
    
  // Clear and rebuild the cached background snapshot under the render lock
  TMonitor.Enter(FRenderLock);
  try
    FBackgroundSnapshot := nil;
    BuildBackgroundSnapshot;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

//------------------------------------------------------------------------------
// BUILD BACKGROUND SNAPSHOT
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.BuildBackgroundSnapshot;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  Font: ISkFont;
  Size, X, Y, SweepAngle: Single;
  GaugeRect, CaptionRect, InnerArcRect, OuterArcRect: TRectF;
  TotalTicks, TickIndex, I: Integer;
  AnglePerTick, CurrentAngle, InnerRadius, OuterRadius: Single;
  StartPoint, EndPoint: TPointF;
  NumberAngle, NumberRadius: Single;
  NumberStr: string;
  LowValueAngle, HighValueAngle: Single;
  ArcPath: ISkPath;
  PathBuilder: ISkPathBuilder;
begin
  // Safety check: ensure all required objects are initialized
  // This prevents access violations if called during constructor
  if not Assigned(FBackground) or not Assigned(FBorder) or
     not Assigned(FMajorTicks) or not Assigned(FMinorTicks) or
     not Assigned(FTopCaption) or not Assigned(FBottomCaption) or
     not Assigned(FGradientScaleItems) then
    Exit;
    
  // Allocate a Skia surface that holds the static gauge background
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;

  // Clear using the active style color so Skia draws over a themed backdrop without GDI bridging
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

  // Calculate gauge size and position based on the control's aspect ratio
  Size := System.Math.Min(ClientWidth - (MARGIN_FROM_BORDER * 2), ClientHeight - (MARGIN_FROM_BORDER * 2));
  X := (Width - Size) / 2;
  Y := (Height - Size) / 2;
  GaugeRect := TRectF.Create(X, Y, X + Size, Y + Size);

  // Draw the background gradient ellipse when colors are configured
  if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(GaugeRect.Left, GaugeRect.Top),
      TPointF.Create(GaugeRect.Left, GaugeRect.Bottom),
      [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawOval(GaugeRect, Paint);
  end;

  // Draw gradient scale slices using Skia paths for each range
  for I := 0 to GradientScale.Count - 1 do
  begin
    InnerRadius := Size / 2 - (GradientScale.Items[I].Size + Border.Width);
    OuterRadius := Size / 2;

    InnerArcRect := TRectF.Create(
      X + (GradientScale.Items[I].Size + Border.Width),
      Y + (GradientScale.Items[I].Size + Border.Width),
      X + (GradientScale.Items[I].Size + Border.Width) + (InnerRadius * 2),
      Y + (GradientScale.Items[I].Size + Border.Width) + (InnerRadius * 2));
    OuterArcRect := TRectF.Create(X, Y, X + (OuterRadius * 2), Y + (OuterRadius * 2));

    PathBuilder := TSkPathBuilder.Create;
    LowValueAngle := ((GradientScale.Items[I].From - FMin) / (FMax - FMin)) * ((EndAngle + 180) - StartAngle) + StartAngle;
    HighValueAngle := ((GradientScale.Items[I].&To - FMin) / (FMax - FMin)) * ((FEndAngle + 180) - FStartAngle) + FStartAngle;

    SweepAngle := HighValueAngle - LowValueAngle;
    if SweepAngle < 0 then
      SweepAngle := SweepAngle + 360;

    PathBuilder.AddArc(OuterArcRect, LowValueAngle, SweepAngle);
    PathBuilder.AddArc(InnerArcRect, LowValueAngle + SweepAngle, -SweepAngle);
    PathBuilder.Close;
    ArcPath := PathBuilder.Detach;

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(GradientScale.Items[I].Color);
    Canvas.DrawPath(ArcPath, Paint);
  end;

  // Draw the gauge border with an inset stroke to respect the configured width
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) and (Border.Width > 0) then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := Border.Width;
    Paint.StrokeJoin := TSkStrokeJoin.Round;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(GaugeRect.Left, GaugeRect.Top),
      TPointF.Create(GaugeRect.Left, GaugeRect.Bottom),
      [SafeColorRefToSkColor(Border.FromColor), SafeColorRefToSkColor(Border.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawOval(TRectF.Create(GaugeRect.Left + (Border.Width / 2), GaugeRect.Top + (Border.Width / 2), GaugeRect.Right - (Border.Width / 2), GaugeRect.Bottom - (Border.Width / 2)), Paint);
  end;

  // Calculate the amount of minor ticks we need to draw
  TotalTicks := Round((FMax - FMin) / FMinorTicks.Step);
  AnglePerTick := ((180 + FEndAngle) - FStartAngle) / TotalTicks;
  InnerRadius := (Size / 2) - FBorder.Width - FMinorTicks.Length - FMinorTicks.Offset;
  OuterRadius := (Size / 2) - FBorder.Width - FMinorTicks.Offset;

  // Draw minor ticks
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := FMinorTicks.Width;
  Paint.Color := SafeColorRefToSkColor(FMinorTicks.Color);
  for TickIndex := 0 to TotalTicks do
  begin
    if (TickIndex mod Round(FMajorTicks.Step)) = 0 then
      Continue;
    CurrentAngle := DegToRad(FStartAngle + (AnglePerTick * TickIndex));
    StartPoint := TPointF.Create(X + (Size / 2) + (Cos(CurrentAngle) * InnerRadius), Y + (Size / 2) + (Sin(CurrentAngle) * InnerRadius));
    EndPoint := TPointF.Create(X + (Size / 2) + (Cos(CurrentAngle) * OuterRadius), Y + (Size / 2) + (Sin(CurrentAngle) * OuterRadius));
    Canvas.DrawLine(StartPoint, EndPoint, Paint);
  end;

  // Draw labels for minor ticks when enabled
  if FMinorTicks.ShowLabel then
  begin
    Font := CreateSkFont(FMinorTicks.Font);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FMinorTicks.Font.Color);
    Paint.Style := TSkPaintStyle.Fill;

    NumberRadius := OuterRadius - FMinorTicks.Length - 10;
    for TickIndex := 0 to TotalTicks do
    begin
      if (TickIndex mod Round(FMajorTicks.Step)) = 0 then
        Continue;

      NumberAngle := DegToRad(FStartAngle + (AnglePerTick * TickIndex));
      StartPoint := TPointF.Create(X + (Size / 2) + (Cos(NumberAngle) * NumberRadius), Y + (Size / 2) + (Sin(NumberAngle) * NumberRadius));

      if (FMinorTicks.Divider > 0) then
        NumberStr := FloatToStr((FMin + (FMinorTicks.Step * TickIndex)) / FMinorTicks.Divider)
      else
        NumberStr := FloatToStr(FMin + (FMinorTicks.Step * TickIndex));

      Canvas.DrawSimpleText(NumberStr, StartPoint.X, StartPoint.Y + (Font.Size / 3), Font, Paint);
    end;
  end;

  // Calculate the amount of major ticks we need to draw
  TotalTicks := Round((FMax - FMin) / FMajorTicks.Step);
  AnglePerTick := ((180 + FEndAngle) - FStartAngle) / TotalTicks;
  InnerRadius := (Size / 2) - FBorder.Width - FMajorTicks.Length - FMajorTicks.Offset;
  OuterRadius := (Size / 2) - FBorder.Width - FMajorTicks.Offset;

  // Draw major ticks
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := FMajorTicks.Width;
  Paint.Color := SafeColorRefToSkColor(FMajorTicks.Color);
  for TickIndex := 0 to TotalTicks do
  begin
    CurrentAngle := DegToRad(FStartAngle + (AnglePerTick * TickIndex));
    StartPoint := TPointF.Create(X + (Size / 2) + (Cos(CurrentAngle) * InnerRadius), Y + (Size / 2) + (Sin(CurrentAngle) * InnerRadius));
    EndPoint := TPointF.Create(X + (Size / 2) + (Cos(CurrentAngle) * OuterRadius), Y + (Size / 2) + (Sin(CurrentAngle) * OuterRadius));
    Canvas.DrawLine(StartPoint, EndPoint, Paint);
  end;

  // Draw labels for major ticks when enabled
  if FMajorTicks.ShowLabel then
  begin
    Font := CreateSkFont(FMajorTicks.Font);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FMajorTicks.Font.Color);
    Paint.Style := TSkPaintStyle.Fill;

    NumberRadius := OuterRadius - FMajorTicks.Length - 10;
    for TickIndex := 0 to TotalTicks do
    begin
      NumberAngle := DegToRad(FStartAngle + (AnglePerTick * TickIndex));
      StartPoint := TPointF.Create(X + (Size / 2) + (Cos(NumberAngle) * NumberRadius), Y + (Size / 2) + (Sin(NumberAngle) * NumberRadius));

      if (FMajorTicks.Divider > 0) then
        NumberStr := FloatToStr((FMin + (FMajorTicks.Step * TickIndex)) / FMajorTicks.Divider)
      else
        NumberStr := FloatToStr(FMin + (FMajorTicks.Step * TickIndex));

      Canvas.DrawSimpleText(NumberStr, StartPoint.X, StartPoint.Y + (Font.Size / 3), Font, Paint);
    end;
  end;

  // Draw top caption when provided
  if (FTopCaption.Caption <> '') then
  begin
    Font := CreateSkFont(FTopCaption.Font);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FTopCaption.Font.Color);
    Paint.Style := TSkPaintStyle.Fill;

    CaptionRect := TRectF.Create(0, 0, Width, (Height / 2) + FTopCaption.Offset);
    Canvas.DrawSimpleText(
      FTopCaption.Caption,
      CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) / 2),
      CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) / 2),
      Font,
      Paint);
  end;

  // Draw bottom caption when provided
  if (FBottomCaption.Caption <> '') then
  begin
    Font := CreateSkFont(FBottomCaption.Font);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := SafeColorRefToSkColor(FBottomCaption.Font.Color);
    Paint.Style := TSkPaintStyle.Fill;

    CaptionRect := TRectF.Create(0, (Height / 2), Width, (Height / 2) + FBottomCaption.Offset);
    Canvas.DrawSimpleText(
      FBottomCaption.Caption,
      CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) / 2),
      CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) / 2),
      Font,
      Paint);
  end;

  // Persist the Skia surface into the reusable background snapshot
  FBackgroundSnapshot := Surface.MakeImageSnapshot;
end;

//------------------------------------------------------------------------------
// ACQUIRE BACKGROUND SNAPSHOT
//------------------------------------------------------------------------------
function TOBDCircularGauge.AcquireBackgroundSnapshot: ISkImage;
begin
  // Lazily rebuild the background snapshot while guarding concurrent access
  TMonitor.Enter(FRenderLock);
  try
    if not Assigned(FBackgroundSnapshot) then
      BuildBackgroundSnapshot;
    Result := FBackgroundSnapshot;
  finally
    TMonitor.Exit(FRenderLock);
  end;
end;

//------------------------------------------------------------------------------
// PAINT NEEDLE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.PaintNeedle(Canvas: ISkCanvas);
var
  Paint: ISkPaint;
  BackgroundImage: ISkImage;
  BasePoint, LeftPoint, RightPoint, TipPoint: TPointF;
  NeedleLength: Single;
  ValueAngle, BaseAngleLeft, BaseAngleRight, X, Y, Size: Single;
  NeedlePath: ISkPath;
  PathBuilder: ISkPathBuilder;
begin
  try
    // Safety check: ensure all required objects are initialized before painting
    // This prevents access violations if paint is triggered during constructor
    if not Assigned(FBackground) or not Assigned(FBorder) or not Assigned(FNeedle) then
    begin
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
      Exit;
    end;
    
    // Draw the cached background image first
    BackgroundImage := AcquireBackgroundSnapshot;
    if Assigned(BackgroundImage) then
      Canvas.DrawImage(BackgroundImage, 0, 0)
    else
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));

    // Validate value range before calculations to prevent division by zero
    if FMax <= FMin then
      Exit;

    // Gauge's center position
    Size := System.Math.Min(ClientWidth, ClientHeight);
    if Size <= 0 then
      Exit;
      
    X := (Width - Size) / 2;
    Y := (Height - Size) / 2;
    BasePoint := TPointF.Create(X + Size / 2, Y + Size / 2);

    // Needle properties
    NeedleLength := Size / 2 * FNeedle.Length;

    // Calculate the needle's angle based on the current value
    // At design time, always use FValue since animations don't run in the IDE
    if Animation.Enabled and not (csDesigning in ComponentState) then
      ValueAngle := ((Animation.Value - FMin) / (FMax - FMin)) * ((180 + FEndAngle) - FStartAngle) + FStartAngle
    else
      ValueAngle := ((FValue - FMin) / (FMax - FMin)) * ((180 + FEndAngle) - FStartAngle) + FStartAngle;
    ValueAngle := DegToRad(ValueAngle);

    // Calculate points for the needle
    TipPoint := TPointF.Create(BasePoint.X + Cos(ValueAngle) * NeedleLength, BasePoint.Y + Sin(ValueAngle) * NeedleLength);

    // Calculate base angles for a wider base
    BaseAngleLeft := ValueAngle + Pi / 2;
    BaseAngleRight := ValueAngle - Pi / 2;
    LeftPoint := TPointF.Create(BasePoint.X + Cos(BaseAngleLeft) * (FNeedle.Width / 2), BasePoint.Y + Sin(BaseAngleLeft) * (FNeedle.Width / 2));
    RightPoint := TPointF.Create(BasePoint.X + Cos(BaseAngleRight) * (FNeedle.Width / 2), BasePoint.Y + Sin(BaseAngleRight) * (FNeedle.Width / 2));

    // Build and fill the needle path
    PathBuilder := TSkPathBuilder.Create;
    PathBuilder.MoveTo(LeftPoint);
    PathBuilder.LineTo(TipPoint);
    PathBuilder.LineTo(RightPoint);
    PathBuilder.Close;
    NeedlePath := PathBuilder.Detach;

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FNeedle.Color);
    Canvas.DrawPath(NeedlePath, Paint);

    if (FNeedle.BorderColor <> clNone) and (FNeedle.BorderWidth > 0) then
    begin
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.StrokeWidth := FNeedle.BorderWidth;
      Paint.StrokeJoin := TSkStrokeJoin.Round;
      Paint.Color := SafeColorRefToSkColor(FNeedle.BorderColor);
      Canvas.DrawPath(NeedlePath, Paint);
    end;

    // Draw the needle center with fill and optional stroke
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(FNeedle.CenterColor);
    Canvas.DrawCircle(BasePoint.X, BasePoint.Y, FNeedle.CenterSize / 2, Paint);

    if (FNeedle.CenterBorderColor <> clNone) and (FNeedle.CenterBorderWidth > 0) then
    begin
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.StrokeWidth := FNeedle.CenterBorderWidth;
      Paint.Color := SafeColorRefToSkColor(FNeedle.CenterBorderColor);
      Canvas.DrawCircle(BasePoint.X, BasePoint.Y, (FNeedle.CenterSize / 2) - (FNeedle.CenterBorderWidth / 2), Paint);
    end;
    // Canvas is rendering directly to the control - no conversion needed!
  except
    on E: Exception do
    begin
      // On error, clear canvas with background color
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
    end;
  end;
end;

//------------------------------------------------------------------------------
// PAINT SKIA
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.PaintSkia(Canvas: ISkCanvas);
begin
  try
    // Paint the gauge with needle directly to the provided canvas
    PaintNeedle(Canvas);
  except
    on E: Exception do
    begin
      // On rendering error, clear canvas with background color as fallback
      Canvas.Clear(SafeColorRefToSkColor(Self.Color));
      // In debug mode, you could log the error here
      // OutputDebugString(PChar('CircularGauge render error: ' + E.Message));
    end;
  end;
end;

//------------------------------------------------------------------------------
// ON CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SettingsChanged(Sender: TObject);
begin
  // Invalidate the background
  InvalidateBackground;
  // Redraw Skia
  Redraw;
  // Invalidate the buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// ON NEEDLE CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.NeedleSettingsChanged(Sender: TObject);
begin
  // Redraw Skia
  Redraw;
  // Invalidate the buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// ON ANIMATION CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    // Notify the animation manager to check animation state
    AnimationManager.CheckAnimationState;
  end;
  
  // If animation is disabled, immediately set Animation.Value to current value
  if not Animation.Enabled then
    Animation.Value := FValue;
  
  // Redraw Skia
  Redraw;
  // Invalidate the buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// ANIMATION TICK (IOBDAnimatable interface)
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.AnimationTick(ElapsedMs: Int64);
var
  CurrentMs, Elapsed: Int64;
  AnimationProgress, EasedProgress, InterpolatedValue: Single;
  EasingFunction: TOBDCustomControlAnimationEasingFunction;
begin
  if not Animation.Enabled then
    Exit;

  // Get current time from stopwatch
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  // Calculate elapsed time since animation start
  Elapsed := CurrentMs - FAnimationStartMs;

  // Get the easing function
  EasingFunction := GetEasingFunction(Animation.&Type);
  if Elapsed < Animation.Duration then
  begin
    // Calculate the animation progress
    AnimationProgress := Elapsed / Animation.Duration;
    // Apply easing function to AnimationProgress
    EasedProgress := EasingFunction(AnimationProgress);
    // Calculate the interpolated value using the eased progress
    InterpolatedValue := Animation.StartValue + (FValue - Animation.StartValue) * EasedProgress;
    // Update the animation value
    Animation.Value := InterpolatedValue;
  end
  else
  begin
    // Directly set to target value when animation duration has passed
    Animation.Value := FValue;
  end;

  // Redraw Skia
  Redraw;
  // Trigger a repaint to display the updated needle position
  Invalidate;
end;

//------------------------------------------------------------------------------
// IS ANIMATING (IOBDAnimatable interface)
//------------------------------------------------------------------------------
function TOBDCircularGauge.IsAnimating: Boolean;
var
  CurrentMs, Elapsed: Int64;
begin
  if not Animation.Enabled then
    Exit(False);
    
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  Elapsed := CurrentMs - FAnimationStartMs;
  Result := (Elapsed < Animation.Duration) and (Animation.StartValue <> FValue);
end;

//------------------------------------------------------------------------------
// GET FRAMES PER SECOND (IOBDAnimatable interface)
//------------------------------------------------------------------------------
function TOBDCircularGauge.GetFramesPerSecond: Integer;
begin
  Result := FramesPerSecond;
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.Resize;
begin
  // Call inherited Resize
  inherited;
  // Invalidate the background
  InvalidateBackground;
  // Redraw Skia
  Redraw;
  // Invalidate the buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.Loaded;
begin
  // Call inherited Loaded
  inherited;
  // Invalidate the background
  InvalidateBackground;
  // Redraw Skia
  Redraw;
  // Invalidate the buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// WND PROC
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.WndProc(var Message: TMessage);
begin
  // Call inherited WndProc
  inherited;
  // Handle message
  case Message.Msg of
    // Color changed
    CM_COLORCHANGED: InvalidateBackground;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCircularGauge.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Initialize render lock for snapshot coordination
  FRenderLock := TObject.Create;
  // Set default start angle
  FStartAngle := DEFAULT_START_ANGLE;
  // Set default end angle
  FEndAngle := DEFAULT_END_ANGLE;
  // Set min
  FMin := DEFAULT_MIN;
  // Set max
  FMax := DEFAULT_MAX;
  // Create gauge background properties
  FBackground := TOBDCircularGaugeBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create gauge border properties
  FBorder := TOBDCircularGaugeBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create major tick properties
  FMajorTicks := TOBDCircularGaugeMajorTicks.Create;
  FMajorTicks.OnChange := SettingsChanged;
  // Create minor tick properties
  FMinorTicks := TOBDCircularGaugeMinorTicks.Create;
  FminorTicks.OnChange := SettingsChanged;
  // Create needle properties
  FNeedle := TOBDCircularGaugeNeedle.Create;
  FNeedle.OnChange := NeedleSettingsChanged;
  // Create top caption properties
  FTopCaption := TOBDCircularGaugeCaption.Create;
  FToPCaption.OnChange := SettingsChanged;
  // Create bottom caption
  FBottomCaption := TOBDCircularGaugeCaption.Create;
  FBottomCaption.OnChange := SettingsChanged;
  // Create animation properties
  FAnimation := TOBDCircularGaugeAnimation.Create;
  FAnimation.OnChange := AnimationChanged;
  FAnimation.Value := FValue;
  // Create gradient scale items
  FGradientScaleItems := TOBDCircularGaugeGradientScaleItems.Create(Self);
  FGradientScaleItems.OnChange := SettingsChanged;

  // Initialize stopwatch for high-resolution timing
  FStopwatch := TStopwatch.StartNew;
  FAnimationStartMs := 0;

  if not (csDesigning in ComponentState) then
  begin
    // Register with the animation manager
    AnimationManager.RegisterControl(Self);
  end;

  // Set default dimensions
  Width := 176;
  Height := 176;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDCircularGauge.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    // Unregister from the animation manager
    AnimationManager.UnregisterControl(Self);
  end;
  // Release render lock
  FRenderLock.Free;
  // Free gauge background properties
  FBackground.Free;
  // Free gauge border properties
  FBorder.Free;
  // Free major tick properties
  FMajorTicks.Free;
  // Free minor tick properties
  FMinorTicks.Free;
  // Free needle properties
  FNeedle.Free;
  // Free top caption
  FTopCaption.Free;
  // Free bottom caption
  FBottomCaption.Free;
  // Free animation properties
  FAnimation.Free;
  // Free gradient scale items
  FGradientScaleItems.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDCircularGauge) then
  begin
    FStartAngle := (Source as TOBDCircularGauge).StartAngle;
    FEndAngle   := (Source as TOBDCircularGauge).EndAngle;
    FBackground.Assign((Source as TOBDCircularGauge).Background);
    FBorder.Assign((Source as TOBDCircularGauge).Border);
    FMajorTicks.Assign((Source as TOBDCircularGauge).MajorTicks);
    FMinorTicks.Assign((Source as TOBDCircularGauge).MinorTicks);
    FNeedle.Assign((Source as TOBDCircularGauge).Needle);
    FTopCaption.Assign((Source as TOBDCircularGauge).TopCaption);
    FBottomCaption.Assign((Source as TOBDCircularGauge).BottomCaption);
    FAnimation.Assign((Source as TOBDCircularGauge).Animation);
    FGradientScaleItems.Assign((Source as TOBDCircularGauge).GradientScale);
  end;
  // Invalidate background
  InvalidateBackground;
  // Redraw Skia
  Redraw;
  // Invalidate the buffer
  Invalidate;
end;

end.
