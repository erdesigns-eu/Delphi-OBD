//------------------------------------------------------------------------------
// UNIT           : OBD.CircularGauge.pas
// CONTENTS       : Circular gauge component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 22/03/2024
//------------------------------------------------------------------------------
unit OBD.CircularGauge;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes,

  OBD.CustomControl, OBD.CustomControl.Common, OBD.CustomControl.Animation;

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
  DEFAULT_BACKGROUND_FROM = $00E4E4E4;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00CDCDCD;

  /// <summary>
  ///   Default border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00D2D2D2;
  /// <summary>
  ///   Default border to color
  /// </summary>
  DEFAULT_BORDER_TO = $009D9D9D;
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
  DEFAULT_NEEDLE_CENTER_COLOR = $009D9D9D;
  /// <summary>
  ///   Default needle center border width
  /// </summary>
  DEFAULT_NEEDLE_CENTER_BORDER_WIDTH = 2;
  /// <summary>
  ///   Default needle center border color
  /// </summary>
  DEFAULT_NEEDLE_CENTER_BORDER_COLOR = $00575757;

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
  TOBDCircularGauge = class(TOBDCustomControl)
  private
    /// <summary>
    ///   Background Buffer
    /// </summary>
    FBackgroundBuffer: TBitmap;
    /// <summary>
    ///   Window handle needed for the timer
    /// </summary>
    FWindowHandle: THandle;
    /// <summary>
    ///   Handle of the animation timer
    /// </summary>
    FTimerHandle: THandle;
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
    ///   Paint the needle
    /// </summary>
    procedure PaintNeedle; virtual;
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintBuffer; override;
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
    /// <summary>
    ///   Timer proc handler
    /// </summary>
    procedure AnimationTimerProc(var Msg: TMessage);
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
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math;

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
  if (FStartAngle <> Value) and (Value >= MIN_START_ANGLE) and (Value <= MAX_START_ANGLE) then
  begin
    // Set new start angle
    FStartAngle := Value;
    // Invalidate the background buffer
    InvalidateBackground;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET END ANGLE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetEndAngle(Value: Single);
begin
  if (FEndAngle <> Value) and (Value >= MIN_END_ANGLE) and (Value <= MAX_END_ANGLE) then
  begin
    // Set new end angle
    FEndAngle := Value;
    // Invalidate the background buffer
    InvalidateBackground;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET MIN
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetMin(Value: Single);
begin
  if (FMin <> Value) and (Value <= FMax) then
  begin
    // Set new min
    FMin := Value;
    // Invalidate the background buffer
    InvalidateBackground;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET MAX
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SetMax(Value: Single);
begin
  if (FMax <> Value) and (Value >= FMin) then
  begin
    // Set new max
    FMax := Value;
    // Invalidate the background buffer
    InvalidateBackground;
    // Invalidate buffer
    InvalidateBuffer;
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
    // Set the start value (for animation)
    Animation.StartValue := FValue;
    // Set value
    FValue := Value;
    // Set animation start time
    Animation.StartTime := GetTickCount;
    // Invalidate buffer
    InvalidateBuffer;
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
var
  SS: TCustomStyleServices;
  Graphics: TGPGraphics;
  Size, X, Y, SweepAngle: Single;
  GaugeRect, CaptionRect: TGPRectF;
  Brush: TGPBrush;
  Pen: TGPPen;
  TotalTicks, TickIndex, I: Integer;
  AnglePerTick, CurrentAngle, InnerRadius, OuterRadius: Single;
  StartPoint, EndPoint: TGPPointF;
  NumberAngle, NumberRadius: Single;
  NumberStr: WideString;
  NumberPoint: TGPPointF;
  Font: TGPFont;
  FontBrush: TGPSolidBrush;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;
  ArcPath: TGPGraphicsPath;
  InnerArcRect, OuterArcRect: TGPRectF;
  LowValueAngle, HighValueAngle: single;
begin
  // Update the size of the background buffer
  FBackgroundBuffer.SetSize(Width, Height);

  // If VCL styles is available and enabled, then draw the VCL Style background
  // so it matches the active style background like on the Form or a Panel.
  if TStyleManager.IsCustomStyleActive then
  begin
    SS := StyleServices;
    // Draw the styled background
    SS.DrawElement(FBackgroundBuffer.Canvas.Handle, SS.GetElementDetails(twWindowRoot), Rect(0, 0, Width, Height));
  end else
  // Otherwise fill the background with the color.
  with FBackgroundBuffer.Canvas do
  begin
    // Use the component color
    Brush.Color := Self.Color;
    // Use a solid brush
    Brush.Style := bsSolid;
    // Fill the background with the component color
    FillRect(Rect(0, 0, Width, Height));
  end;

  // Initialize GDI+ Graphics object
  Graphics := TGPGraphics.Create(FBackgroundBuffer.Canvas.Handle);
  try
    // Set smoothing mode to high-quality
    Graphics.SetSmoothingMode(SmoothingModeHighQuality);
    // Set compositing quality to high-quality
    Graphics.SetCompositingQuality(CompositingQualityHighQuality);

    // Calculate gauge size and position based on control's aspect ratio
    Size := System.Math.Min(ClientWidth - (MARGIN_FROM_BORDER * 2), ClientHeight - (MARGIN_FROM_BORDER * 2));
    X := (Width - Size) / 2;
    Y := (Height - Size) / 2;

    // Draw the backround
    if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
    begin
      // Get the rectangle for the gauge
      GaugeRect := MakeRect(X, Y, Size, Size);
      // Create the background brush
      Brush := TGPLinearGradientBrush.Create(GaugeRect, SafeColorRefToARGB(Background.FromColor), SafeColorRefToARGB(Background.ToColor), LinearGradientModeVertical);
      try
        // Fill the gauge background
        Graphics.FillEllipse(Brush, GaugeRect);
      finally
        // Free the background brush object
        Brush.Free;
      end;
    end;

    // Draw gradient scale items
    for I := 0 to GradientScale.Count -1 do
    begin
      // Calculate inner and outer radius
      InnerRadius := Size / 2 - (GradientScale.Items[I].Size + Border.Width);
      OuterRadius := Size / 2;

      // Calculate arc rects
      InnerArcRect := MakeRect(X + (GradientScale.Items[I].Size + Border.Width), Y + (GradientScale.Items[I].Size + Border.Width), InnerRadius * 2, InnerRadius * 2);
      OuterArcRect := MakeRect(X, Y, OuterRadius * 2, OuterRadius * 2);

      // Create the arc
      ArcPath := TGPGraphicsPath.Create;
      try
        LowValueAngle := ((GradientScale.Items[I].From - FMin) / (FMax - FMin)) * ((EndAngle + 180) - StartAngle) + StartAngle;
        HighValueAngle := ((GradientScale.Items[I].&To - FMin) / (FMax - FMin)) * ((FEndAngle + 180) - FStartAngle) + FStartAngle;

        // Calculate sweep angle, which is the angle from LowValueAngle to HighValueAngle
        SweepAngle := HighValueAngle - LowValueAngle;
        // Ensure sweep angle is not negative
        if SweepAngle < 0 then SweepAngle := SweepAngle + 360;

        // Add the outer arc to the path
        ArcPath.AddArc(OuterArcRect, LowValueAngle, SweepAngle);
        // Since we're drawing the inner arc in the opposite direction,
        // the start angle is the end angle of the outer arc,
        // and the sweep angle is negative.
        ArcPath.AddArc(InnerArcRect, LowValueAngle + SweepAngle, -SweepAngle);
        // Close the figure
        ArcPath.CloseFigure;

        Brush := TGPSolidBrush.Create(SafeColorRefToARGB(GradientScale.Items[I].Color));
        try
          Graphics.FillPath(Brush, ArcPath);
        finally
          Brush.Free;
        end;
      finally
        ArcPath.Free;
      end;
    end;

    // Draw the border
    if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) and (Border.Width > 0) then
    begin
      // Create the border brush
      Brush := TGPLinearGradientBrush.Create(GaugeRect, SafeColorRefToARGB(Border.FromColor), SafeColorRefToARGB(Border.ToColor), LinearGradientModeVertical);
      // Create the border pen
      Pen := TGPPen.Create(Brush, Border.Width);
      Pen.SetAlignment(PenAlignmentInset);
      try
        // Draw the gauge border
        Graphics.DrawEllipse(Pen, GaugeRect);
      finally
        // Free the background brush object
        Brush.Free;
        // Free the background pen object
        Pen.Free;
      end;
    end;

    // Calculate the amount of minor ticks we need to draw
    TotalTicks := Round((FMax - FMin) / FMinorTicks.Step);
    // Adjust the AnglePerTick calculation to account for the described angle definitions
    AnglePerTick := ((180 + FEndAngle) - FStartAngle) / TotalTicks;
    // Calculate inner radius
    InnerRadius := (Size / 2) - FBorder.Width - FMinorTicks.Length - FMinorTicks.Offset;
    // Calculate outer radius
    OuterRadius := (Size / 2) - FBorder.Width - FMinorTicks.Offset;
    // Create the pen for the minor ticks
    Pen := TGPPen.Create(SafeColorRefToARGB(FMinorTicks.Color), FMinorTicks.Width);
    try
      for TickIndex := 0 to TotalTicks do
      begin
        // Skip if we need to draw a major tick here
        if (TickIndex mod Round(FMajorTicks.Step)) = 0 then Continue;
        // Calculate current angle
        CurrentAngle := FStartAngle + (AnglePerTick * TickIndex);
        // Convert degrees to radians for Sin and Cos functions
        CurrentAngle := DegToRad(CurrentAngle);
        // Calculate the start and end points
        StartPoint.X := X + (Size / 2) + (Cos(CurrentAngle) * InnerRadius);
        StartPoint.Y := Y + (Size / 2) + (Sin(CurrentAngle) * InnerRadius);
        EndPoint.X := X + (Size / 2) + (Cos(CurrentAngle) * OuterRadius);
        EndPoint.Y := Y + (Size / 2) + (Sin(CurrentAngle) * OuterRadius);
        // Draw the tick
        Graphics.DrawLine(Pen, StartPoint, EndPoint);
      end;
    finally
      // Free the minor tick pen object
      Pen.Free;
    end;

    // Draw labels for minor ticks
    if FMinorTicks.ShowLabel then
    begin
      FontFamily := TGPFontFamily.Create(FMinorTicks.Font.Name);
      Font := TGPFont.Create(FontFamily, FMinorTicks.Font.Size, FontStyle(FMinorTicks.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(FMinorTicks.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetAlignment(StringAlignmentCenter);
      StringFormat.SetLineAlignment(StringAlignmentCenter);
      try
        // Adjust the radius for number positioning based on your design needs
        NumberRadius := OuterRadius - FMinorTicks.Length - 10;
        for TickIndex := 0 to TotalTicks do
        begin
          // Skip if we need to draw a major tick here
          if (TickIndex mod Round(FMajorTicks.Step)) = 0 then Continue;
          // Calculate the angle for the number
          NumberAngle := FStartAngle + (AnglePerTick * TickIndex);
          // Convert degrees to radians for Sin and Cos functions
          NumberAngle := DegToRad(NumberAngle);
          // Determine the position for the number
          NumberPoint.X := X + (Size / 2) + (Cos(NumberAngle) * NumberRadius);
          NumberPoint.Y := Y + (Size / 2) + (Sin(NumberAngle) * NumberRadius);
          // Convert tick value to string
          if (FMinorTicks.Divider > 0) then
            NumberStr := WideString(FloatToStr((FMin + (FMinorTicks.Step * TickIndex)) / FMinorTicks.Divider))
          else
            NumberStr := WideString(FloatToStr(FMin + (FMinorTicks.Step * TickIndex)));
          // Draw the number at the calculated position
          Graphics.DrawString(NumberStr, -1, Font, NumberPoint, StringFormat, FontBrush);
        end;
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;
    end;

    // Calculate the amount of major ticks we need to draw
    TotalTicks := Round((FMax - FMin) / FMajorTicks.Step);
    // Adjust the AnglePerTick calculation to account for the described angle definitions
    AnglePerTick := ((180 + FEndAngle) - FStartAngle) / TotalTicks;
    // Calculate inner radius
    InnerRadius := (Size / 2) - FBorder.Width - FMajorTicks.Length - FMajorTicks.Offset;
    // Calculate outer radius
    OuterRadius := (Size / 2) - FBorder.Width - FMajorTicks.Offset;
    // Create the pen for the minor ticks
    Pen := TGPPen.Create(SafeColorRefToARGB(FMajorTicks.Color), FMajorTicks.Width);
    try
      for TickIndex := 0 to TotalTicks do
      begin
        // Calculate current angle
        CurrentAngle := FStartAngle + (AnglePerTick * TickIndex);
        // Convert degrees to radians for Sin and Cos functions
        CurrentAngle := DegToRad(CurrentAngle);
        // Calculate the start and end points
        StartPoint.X := X + (Size / 2) + (Cos(CurrentAngle) * InnerRadius);
        StartPoint.Y := Y + (Size / 2) + (Sin(CurrentAngle) * InnerRadius);
        EndPoint.X := X + (Size / 2) + (Cos(CurrentAngle) * OuterRadius);
        EndPoint.Y := Y + (Size / 2) + (Sin(CurrentAngle) * OuterRadius);
        // Draw the tick
        Graphics.DrawLine(Pen, StartPoint, EndPoint);
      end;
    finally
      // Free the minor tick pen object
      Pen.Free;
    end;

    // Draw labels for major ticks
    if FMajorTicks.ShowLabel then
    begin
      FontFamily := TGPFontFamily.Create(FMajorTicks.Font.Name);
      Font := TGPFont.Create(FontFamily, FMajorTicks.Font.Size, FontStyle(FMajorTicks.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(FMajorTicks.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetAlignment(StringAlignmentCenter);
      StringFormat.SetLineAlignment(StringAlignmentCenter);
      try
        // Adjust the radius for number positioning based on your design needs
        NumberRadius := OuterRadius - FMajorTicks.Length - 10;
        for TickIndex := 0 to TotalTicks do
        begin
          // Calculate the angle for the number
          NumberAngle := FStartAngle + (AnglePerTick * TickIndex);
          // Convert degrees to radians for Sin and Cos functions
          NumberAngle := DegToRad(NumberAngle);
          // Determine the position for the number
          NumberPoint.X := X + (Size / 2) + (Cos(NumberAngle) * NumberRadius);
          NumberPoint.Y := Y + (Size / 2) + (Sin(NumberAngle) * NumberRadius);
          // Convert tick value to string
          if (FMajorTicks.Divider > 0) then
            NumberStr := WideString(FloatToStr((FMin + (FMajorTicks.Step * TickIndex)) / FMajorTicks.Divider))
          else
            NumberStr := WideString(FloatToStr(FMin + (FMajorTicks.Step * TickIndex)));
          // Draw the number at the calculated position
          Graphics.DrawString(NumberStr, -1, Font, NumberPoint, StringFormat, FontBrush);
        end;
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;
    end;

    // Draw top caption
    if (FTopCaption.Caption <> '') then
    begin
      FontFamily := TGPFontFamily.Create(FTopCaption.Font.Name);
      Font := TGPFont.Create(FontFamily, FTopCaption.Font.Size, FontStyle(FTopCaption.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(FTopCaption.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetAlignment(StringAlignmentCenter);
      StringFormat.SetLineAlignment(StringAlignmentCenter);
      try
        CaptionRect := MakeRect(0, 0, Width, (Height / 2) + FTopCaption.Offset);
        Graphics.DrawString(FTopCaption.Caption, -1, Font, CaptionRect, StringFormat, FontBrush);
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;
    end;

    // Draw bottom caption
    if (FBottomCaption.Caption <> '') then
    begin
      FontFamily := TGPFontFamily.Create(FBottomCaption.Font.Name);
      // TODO: update the font styles
      Font := TGPFont.Create(FontFamily, FBottomCaption.Font.Size, FontStyle(FBottomCaption.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(FBottomCaption.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetAlignment(StringAlignmentCenter);
      StringFormat.SetLineAlignment(StringAlignmentCenter);
      try
        CaptionRect := MakeRect(0, (Height / 2), Width, (Height / 2) + FBottomCaption.Offset);
        Graphics.DrawString(FBottomCaption.Caption, -1, Font, CaptionRect, StringFormat, FontBrush);
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;
    end;
  finally
    // Free the GDI+ Graphics object
    Graphics.Free;
  end;
end;

//------------------------------------------------------------------------------
// PAINT NEEDLE
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.PaintNeedle;
var
  Graphics: TGPGraphics;
  Brush: TGPBrush;
  Pen: TGPPen;
  BasePoint, LeftPoint, RightPoint, TipPoint: TGPPointF;
  NeedleLength: Single;
  ValueAngle, BaseAngleLeft, BaseAngleRight, X, Y, Size: Single;
  NeedlePath: TGPGraphicsPath;
  CenterRect: TGPRectF;
begin
  // Initialize GDI+ Graphics object
  Graphics := TGPGraphics.Create(Buffer.Canvas.Handle);
  try
    // Set smoothing mode to high-quality
    Graphics.SetSmoothingMode(SmoothingModeHighQuality);
    // Set compositing quality to high-quality
    Graphics.SetCompositingQuality(CompositingQualityHighQuality);

    // Gauge's center position
    Size := System.Math.Min(ClientWidth, ClientHeight);
    X := (Width - Size) / 2;
    Y := (Height - Size) / 2;
    BasePoint.X := X + Size / 2;
    BasePoint.Y := Y + Size / 2;

    // Needle properties
    NeedleLength := Size / 2 * FNeedle.Length;

    // Calculate the needle's angle based on the current value
    if Animation.Enabled then
      ValueAngle := ((Animation.Value - FMin) / (FMax - FMin)) * ((180 + FEndAngle) - FStartAngle) + FStartAngle
    else
      ValueAngle := ((FValue - FMin) / (FMax - FMin)) * ((180 + FEndAngle) - FStartAngle) + FStartAngle;
    ValueAngle := DegToRad(ValueAngle);

    // Calculate points for the needle
    TipPoint.X := BasePoint.X + Cos(ValueAngle) * NeedleLength;
    TipPoint.Y := BasePoint.Y + Sin(ValueAngle) * NeedleLength;

    // Calculate base angles for a wider base
    BaseAngleLeft := ValueAngle + Pi / 2;
    BaseAngleRight := ValueAngle - Pi / 2;
    LeftPoint.X := BasePoint.X + Cos(BaseAngleLeft) * (FNeedle.Width / 2);
    LeftPoint.Y := BasePoint.Y + Sin(BaseAngleLeft) * (FNeedle.Width / 2);
    RightPoint.X := BasePoint.X + Cos(BaseAngleRight) * (FNeedle.Width / 2);
    RightPoint.Y := BasePoint.Y + Sin(BaseAngleRight) * (FNeedle.Width / 2);

    // Create the needle shape (path)
    NeedlePath := TGPGraphicsPath.Create;
    try
      NeedlePath.StartFigure;
      NeedlePath.AddLine(LeftPoint, TipPoint);
      NeedlePath.AddLine(TipPoint, RightPoint);
      NeedlePath.AddLine(RightPoint, LeftPoint);
      NeedlePath.CloseFigure;

      // Create the brush for the needle
      Brush := TGPSolidBrush.Create(SafeColorRefToARGB(FNeedle.Color));
      // Create the pen for the needle
      Pen := TGPPen.Create(SafeColorRefToARGB(FNeedle.BorderColor), FNeedle.BorderWidth);
      try
        // Fill the needle shape
        Graphics.FillPath(Brush, NeedlePath);
        // Draw the border
        if (FNeedle.BorderColor <> clNone) and (FNeedle.BorderWidth > 0) then
        Graphics.DrawPath(Pen, NeedlePath);
      finally
        Brush.Free;
        Pen.Free;
      end;
    finally
      NeedlePath.Free;
    end;

    // Create the brush for the needle center
    Brush := TGPSolidBrush.Create(SafeColorRefToARGB(Fneedle.CenterColor));
    // Create the pen for the needle center
    Pen := TGPPen.Create(SafeColorRefToARGB(FNeedle.CenterBorderColor), FNeedle.CenterBorderWidth);
    try
      CenterRect := MakeRect(BasePoint.X - FNeedle.CenterSize / 2, BasePoint.Y - FNeedle.CenterSize / 2, FNeedle.CenterSize, FNeedle.CenterSize);
      // Fill the needle center
      Graphics.FillEllipse(Brush, CenterRect);
      // Draw the border
      if (FNeedle.CenterBorderColor <> clNone) and (FNeedle.CenterBorderWidth > 0) then
      Graphics.DrawEllipse(Pen, CenterRect);
    finally
      Brush.Free;
      Pen.Free;
    end;
  finally
    // Free the GDI+ Graphics object
    Graphics.Free;
  end;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.PaintBuffer;
begin
  // Call inherited PaintBuffer
  inherited;
  // Copy the background buffer to the main buffer, by buffering the background
  // and only updating the background buffer when the background is changed
  // allows us to just copy the background buffer, which speeds up our PaintBuffer
  // resulting in less CPU consumption and allowing higher framerates.
  BitBlt(Buffer.Canvas.Handle, 0, 0, Width, Height, FBackgroundBuffer.Canvas.Handle, 0,  0, SRCCOPY);
  // Paint the needle on the buffer.
  PaintNeedle;
end;

//------------------------------------------------------------------------------
// ON CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.SettingsChanged(Sender: TObject);
begin
  // Invalidate the background
  InvalidateBackground;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// ON NEEDLE CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.NeedleSettingsChanged(Sender: TObject);
begin
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// ON ANIMATION CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    // Kill the timer
    if (FTimerHandle <> 0) then KillTimer(Handle, FTimerHandle);
    // Create new timer
    if Animation.Enabled then FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FramesPerSecond, nil);
  end;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// ANIMATION TIMER MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.AnimationTimerProc(var Msg: TMessage);
var
  CurrentTime, Elapsed: Cardinal;
  AnimationProgress, EasedProgress, InterpolatedValue: Single;
  EasingFunction: TOBDCustomControlAnimationEasingFunction;
begin
  if Msg.Msg = WM_TIMER then
  begin
    // Set current time
    CurrentTime := GetTickCount;
    // Calculate elapsed time
    Elapsed := CurrentTime - Animation.StartTime;

    // Get the easing function
    EasingFunction := GetEasingFunction(Animation.&Type);
    if Elapsed < Animation.Duration then
    begin
      // Calulate the animation progress
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

    // Trigger a repaint to display the updated needle position
    InvalidateBuffer;
  end else
    // Pass message to default message handler
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
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
  // Invalidate the buffer
  InvalidateBuffer;
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
  // Invalidate the buffer
  InvalidateBuffer;
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
  // Create background buffer
  FBackgroundBuffer := TBitmap.Create;
  // Set the background buffer pixel format
  FBackgroundBuffer.PixelFormat := pf32bit;
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

  if not (csDesigning in ComponentState) then
  begin
    // Allocate window handle for the timer
    FWindowHandle := AllocateHWnd(AnimationTimerProc);
    // Create new timer
    if Animation.Enabled then FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FramesPerSecond, nil);
  end;

  // Set default dimensions
  Width := 201;
  Height := 201;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDCircularGauge.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    // Kill the timer
    if (FTimerHandle <> 0) then KillTimer(FWindowHandle, FTimerHandle);
    // Deallocate window handle
    DeallocateHWnd(FWindowHandle);
  end;
  // Free background buffer
  FBackgroundBuffer.Free;
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
  // Invalidate the buffer
  InvalidateBuffer;
end;

end.
