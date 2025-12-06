//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Header.pas
// CONTENTS       : Header component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/03/2024
//------------------------------------------------------------------------------
unit OBD.Touch.Header;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Skia, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Themes, Vcl.ExtCtrls, Skia.Vcl,

  OBD.CustomControl.Helpers, OBD.CustomControl.Constants;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 50;
  /// <summary>
  ///   Default corner radius
  /// </summary>
  DEFAULT_CORNER = 5;

  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = clWhite;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00D7D7D7;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Header Tab Change event
  /// </summary>
  TOBDTouchHeaderTabChangeEvent = procedure(Sender: TObject; TabIndex: Integer) of object;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Header background properties
  /// </summary>
  TOBDTouchHeaderBackground = class(TPersistent)
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
  ///   Touch Header border properties
  /// </summary>
  TOBDTouchHeaderBorder = class(TPersistent)
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
    ///   Border height
    /// </summary>
    FHeight: Integer;

    /// <summary>
    ///   Set gradient from color
    /// </summary
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set gradient to color
    /// </summary
    procedure SetToColor(Value: TColor);
    /// <summary>
    ///   Set border height
    /// </summary>
    procedure SetHeight(Value: Integer);
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
    ///   Border height
    /// </summary>
    property Height: Integer read FHeight write SetHeight default DEFAULT_BORDER_HEIGHT;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header button color
  /// </summary>
  TOBDTouchHeaderButtonColor = class(TPersistent)
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
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_TO;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header button
  /// </summary>
  TOBDTouchHeaderButton = class(TPersistent)
  private
    /// <summary>
    ///   Button Rect
    /// </summary>
    FButtonRect: TRect;
    /// <summary>
    ///   Button state (0 = normal, 1 = hot, 2 = pressed)
    /// </summary>
    FButtonState: Integer;
  private
    /// <summary>
    ///   Normal button color
    /// </summary>
    FNormalColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Hot button color
    /// </summary>
    FHotColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Pressed button color
    /// </summary>
    FPressedColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Disabled button color
    /// </summary>
    FDisabledColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Enabled
    /// </summary>
    FEnabled: Boolean;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Width
    /// </summary>
    FWidth: Integer;
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;
    /// <summary>
    ///   Image
    /// </summary>
    FImage: TPicture;

    /// <summary>
    ///   Set normal button color
    /// </summary>
    procedure SetNormalColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set hot button color
    /// </summary>
    procedure SetHotColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set pressed button color
    /// </summary>
    procedure SetPressedColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set disabled button color
    /// </summary>
    procedure SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set enabled
    /// </summary>
    procedure SetEnabled(Value: Boolean);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set width
    /// </summary>
    procedure SetWidth(Value: Integer);
    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
    /// <summary>
    ///   Set image
    /// </summary>
    procedure SetImage(Value: TPicture);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed hanlder
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   Button Rect
    /// </summary>
    property ButtonRect: TRect read FButtonRect write FButtonRect;
    /// <summary>
    ///   Button state (0 = normal, 1 = hot, 2 = pressed)
    /// </summary>
    property ButtonState: Integer read FButtonState write FButtonState;
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
    ///   Normal button color
    /// </summary>
    property NormalColor: TOBDTouchHeaderButtonColor read FNormalColor write SetNormalColor;
    /// <summary>
    ///   Hot button color
    /// </summary>
    property HotColor: TOBDTouchHeaderButtonColor read FHotColor write SetHotColor;
    /// <summary>
    ///   Pressed button color
    /// </summary>
    property PressedColor: TOBDTouchHeaderButtonColor read FPressedColor write SetPressedColor;
    /// <summary>
    ///   Disabled button color
    /// </summary>
    property DisabledColor: TOBDTouchHeaderButtonColor read FDisabledColor write SetDisabledColor;
    /// <summary>
    ///   Enabled
    /// </summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Width
    /// </summary>
    property Width: Integer read FWidth write SetWidth default DEFAULT_BACK_BUTTON_WIDTH;
    /// <summary>
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_BACK_BUTTON_BORDER_COLOR;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    /// <summary>
    ///   Image
    /// </summary>
    property Image: TPicture read FImage write SetImage;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header action button
  /// </summary>
  TOBDTouchHeaderActionButton = class(TOBDTouchHeaderButton)
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; override;
  end;

  /// <summary>
  ///   Touch Header button
  /// </summary>
  TOBDTouchHeaderCaption = class(TPersistent)
  private
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;

    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed hanlder
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
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
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header Tab Item
  /// </summary>
  TOBDTouchHeaderTabItem = class(TCollectionItem)
  private
    /// <summary>
    ///   Tab Rect
    /// </summary>
    FTabRect: TRect;
    /// <summary>
    ///   Tab state (0 = normal, 1 = hot, 2 = pressed)
    /// </summary>
    FTabState: Integer;
  private
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   Image
    /// </summary>
    FImage: TPicture;
    /// <summary>
    ///   Width
    /// </summary>
    FWidth: Integer;
    /// <summary>
    ///   Enabled
    /// </summary>
    FEnabled: Boolean;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;

    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set image
    /// </summary>
    procedure SetImage(Value: TPicture);
    /// <summary>
    ///   Set width
    /// </summary>
    procedure SetWidth(Value: Integer);
    /// <summary>
    ///   Set enabled
    /// </summary>
    procedure SetEnabled(Value: Boolean);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
  protected
    /// <summary>
    ///   Tab rect
    /// </summary>
    property TabRect: TRect read FTabRect write FTabRect;
    /// <summary>
    ///   Tab state
    /// </summary>
    property TabState: Integer read FTabState write FTabState;
  protected
    /// <summary>
    ///   Image changed handler
    /// </summary>
    procedure ImageChanged(Sender: TObject);
    /// <summary>
    ///   Override get displayname function
    /// </summary>
    function GetDisplayName : String; override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Assign
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   Image
    /// </summary>
    property Image: TPicture read FImage write SetImage;
    /// <summary>
    ///   Width
    /// </summary>
    property Width: Integer read FWidth write SetWidth default DEFAULT_TAB_WIDTH;
    /// <summary>
    ///   Enabled
    /// </summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  /// <summary>
  ///   Touch Header Tab Collection
  /// </summary>
  TOBDTouchHeaderTabCollection = class(TOwnedCollection)
  private
    /// <summary>
    ///   On Change event
    /// </summary>
    FOnChange: TNotifyEvent;

    /// <summary>
    ///   Get tab item
    /// </summary>
    function GetItem(AIndex: Integer): TOBDTouchHeaderTabItem;
    /// <summary>
    ///   Set tab item
    /// </summary>
    procedure SetItem(AIndex: Integer; const Value: TOBDTouchHeaderTabItem);
  protected
    /// <summary>
    ///   Override Item update handler
    /// </summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TPersistent); virtual;

    /// <summary>
    ///   Add tab item
    /// </summary>
    function Add: TOBDTouchHeaderTabItem;
    /// <summary>
    ///   Assign
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Items
    /// </summary>
    property Items[AIndex: Integer]: TOBDTouchHeaderTabItem read GetItem write SetItem;
    /// <summary>
    ///   On Change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header tab
  /// </summary>
  TOBDTouchHeaderTab = class(TPersistent)
  private
    /// <summary>
    ///   Normal tab color
    /// </summary>
    FNormalColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Hot tab color
    /// </summary>
    FHotColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Pressed tab color
    /// </summary>
    FPressedColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Disabled tab color
    /// </summary>
    FDisabledColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Active tab color
    /// </summary>
    FActiveColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;

    /// <summary>
    ///   Set normal tab color
    /// </summary>
    procedure SetNormalColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set hot tab color
    /// </summary>
    procedure SetHotColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set pressed tab color
    /// </summary>
    procedure SetPressedColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set disabled tab color
    /// </summary>
    procedure SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set active tab color
    /// </summary>
    procedure SetActiveColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed hanlder
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
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
    ///   Normal button color
    /// </summary>
    property NormalColor: TOBDTouchHeaderButtonColor read FNormalColor write SetNormalColor;
    /// <summary>
    ///   Hot button color
    /// </summary>
    property HotColor: TOBDTouchHeaderButtonColor read FHotColor write SetHotColor;
    /// <summary>
    ///   Pressed button color
    /// </summary>
    property PressedColor: TOBDTouchHeaderButtonColor read FPressedColor write SetPressedColor;
    /// <summary>
    ///   Disabled button color
    /// </summary>
    property DisabledColor: TOBDTouchHeaderButtonColor read FDisabledColor write SetDisabledColor;
    /// <summary>
    ///   Active tab color
    /// </summary>
    property ActiveColor: TOBDTouchHeaderButtonColor read FActiveColor write SetActiveColor;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_BORDER_TO;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header vattery indicator
  /// </summary>
  TOBDTouchHeaderBatteryIndicator = class(TPersistent)
  private
    /// <summary>
    ///   Indicator size
    /// </summary>
    FSize: Integer;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;
    /// <summary>
    ///   Show label
    /// </summary>
    FShowLabel: Boolean;
    /// <summary>
    ///   Battery percentage
    /// </summary>
    FPercentage: Single;
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;
    /// <summary>
    ///   Color for percentage (0-25)
    /// </summary>
    FColor25: TColor;
    /// <summary>
    ///   Color for percentage (26-50)
    /// </summary>
    FColor50: TColor;
    /// <summary>
    ///   Color for percentage (51-75)
    /// </summary>
    FColor75: TColor;
    /// <summary>
    ///   Color for percentage (76-100)
    /// </summary>
    FColor100: TColor;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;

    /// <summary>
    ///   Set indicator size
    /// </summary>
    procedure SetSize(Value: Integer);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
    /// <summary>
    ///   Set show label
    /// </summary>
    procedure SetShowLabel(Value: Boolean);
    /// <summary>
    ///   Set percentage
    /// </summary>
    procedure SetPercentage(Value: Single);
    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
    /// <summary>
    ///   Set color for percentage (0-25)
    /// </summary>
    procedure SetColor25(Value: TColor);
    /// <summary>
    ///   Set color for percentage (26-50)
    /// </summary>
    procedure SetColor50(Value: TColor);
    /// <summary>
    ///   Set color for percentage (51-75)
    /// </summary>
    procedure SetColor75(Value: TColor);
    /// <summary>
    ///   Set color for percentage (76-100)
    /// </summary>
    procedure SetColor100(Value: TColor);
    /// <summary>
    ///   Font
    /// </summary>
    procedure SetFont(Value: TFont);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed hanlder
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
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
    ///   Indicator size
    /// </summary>
    property Size: Integer read FSize write SetSize default 32;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    ///   Show label
    /// </summary>
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;
    /// <summary>
    ///   Battery percentage
    /// </summary>
    property Percentage: Single read FPercentage write SetPercentage; 
    /// <summary>
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_BACK_BUTTON_BORDER_COLOR;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    /// <summary>
    ///   Color for percentage (0-25)
    /// </summary>
    property Color25: TColor read FColor25 write SetColor25 default DEFAULT_BATTERY_COLOR_25;
    /// <summary>
    ///   Color for percentage (26-50)
    /// </summary>
    property Color50: TColor read FColor50 write SetColor50 default DEFAULT_BATTERY_COLOR_50;
    /// <summary>
    ///   Color for percentage (51-75)
    /// </summary>
    property Color75: TColor read FColor75 write SetColor75 default DEFAULT_BATTERY_COLOR_75;
    /// <summary>
    ///   Color for percentage (76-100)
    /// </summary>
    property Color100: TColor read FColor100 write SetColor100 default DEFAULT_BATTERY_COLOR_100;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
  
    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header Component
  /// </summary>
  TOBDTouchHeader = class(TCustomControl)
  private
    /// <summary>
    ///   Class constructor
    /// </summary>
    class constructor Create;
    /// <summary>
    ///   Class destructor
    /// </summary>
    class destructor Destroy;
  private
    /// <summary>
    ///   Buffer (This is the canvas we draw on)
    /// </summary>
    FBuffer: TBitmap;
    /// <summary>
    ///   Update rect (Invalidated rectangle)
    /// </summary>
    FUpdateRect: TRect;
  private
    /// <summary>
    ///   Background
    /// </summary>
    FBackground: TOBDTouchHeaderBackground;
    /// <summary>
    ///   Border
    /// </summary>
    FBorder: TOBDTouchHeaderBorder;
    /// <summary>
    ///   Back button
    /// </summary>
    FBackButton: TOBDTouchHeaderButton;
    /// <summary>
    ///   Action button
    /// </summary>
    FActionButton: TOBDTouchHeaderActionButton;
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TOBDTouchHeaderCaption;
    /// <summary>
    ///   Tabs
    /// </summary>
    FTabs: TOBDTouchHeaderTabCollection;
    /// <summary>
    ///   Tab settings
    /// </summary>
    FTab: TOBDTouchHeaderTab;
    /// <summary>
    ///   Tab index (active tab)
    /// </summary>
    FTabIndex: Integer;
    /// <summary>
    ///   Battery indicator
    /// </summary>
    FBatteryIndicator: TOBDTouchHeaderBatteryIndicator;

    /// <summary>
    ///   Back button click event
    /// </summary>
    FOnBackButtonClick: TNotifyEvent;
    /// <summary>
    ///   Action button click event
    /// </summary>
    FOnActionButtonClick: TNotifyEvent;
    /// <summary>
    ///   Tab index change event
    /// </summary>
    FOnTabChange: TOBDTouchHeaderTabChangeEvent;

    /// <summary>
    ///   Set background
    /// </summary>
    procedure SetBackground(Value: TOBDTouchHeaderBackground);
    /// <summary>
    ///   Set border
    /// </summary>
    procedure SetBorder(Value: TOBDTouchHeaderBorder);
    /// <summary>
    ///   Set back button
    /// </summary>
    procedure SetBackButton(Value: TOBDTouchHeaderButton);
    /// <summary>
    ///   Set action button
    /// </summary>
    procedure SetActionButton(Value: TOBDTouchHeaderActionButton);
    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TOBDTouchHeaderCaption);
    /// <summary>
    ///   Set tabs
    /// </summary>
    procedure SetTabs(Value: TOBDTouchHeaderTabCollection);
    /// <summary>
    ///   Set tab settings
    /// </summary>
    procedure SetTab(Value: TOBDTouchHeaderTab);
    /// <summary>
    ///   Set tab index (active tab)
    /// </summary>
    procedure SetTabIndex(Value: Integer);
    /// <summary>
    ///   Set battery indicator
    /// </summary>
    procedure SetBatteryIndicator(Value: TOBDTouchHeaderBatteryIndicator);
  private
    /// <summary>
    ///   WM_PAINT message handler
    /// </summary>
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    /// <summary>
    ///   WM_ERASEBKGND message handler
    /// </summary>
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    /// <summary>
    ///   WM_KILLFOCUS message handler
    /// </summary>
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    /// <summary>
    ///   CM_ENABLEDCHANGED message handler
    /// </summary>
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    /// <summary>
    ///   CM_MOUSELEAVE message handler
    /// </summary>
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>
    ///   Buffer (This is the canvas we draw on)
    /// </summary>
    property Buffer: TBitmap read FBuffer;

    /// <summary>
    ///   Override CreateParams method
    /// </summary>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>
    ///   Override Paint method
    /// </summary>
    procedure Paint; override;
    /// <summary>
    ///   Override Resize method
    /// </summary>
    procedure Resize; override;
    /// <summary>
    ///   Override Loaded method
    /// </summary>
    procedure Loaded; override;
    /// <summary>
    ///   Override UpdateStyleElements method
    /// </summary>
    procedure UpdateStyleElements; override;
    /// <summary>
    ///   Override MouseDown method
    /// </summary>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>
    ///   Override MouseUp method
    /// </summary>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>
    ///   Override MouseMove method
    /// </summary>
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  protected
    /// <summary>
    ///   Settings changed handler
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintBuffer; virtual;
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
    ///   Override Repaint method
    /// </summary>
    procedure Repaint; override;
    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Background
    /// </summary>
    property Background: TOBDTouchHeaderBackground read FBackground write SetBackground;
    /// <summary>
    ///   Border
    /// </summary>
    property Border: TOBDTouchHeaderBorder read FBorder write SetBorder;
    /// <summary>
    ///   Back button
    /// </summary>
    property BackButton: TOBDTouchHeaderButton read FBackButton write SetBackButton;
    /// <summary>
    ///   Action button
    /// </summary>
    property ActionButton: TOBDTouchHeaderActionButton read FActionButton write SetActionButton;
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TOBDTouchHeaderCaption read FCaption write SetCaption;
    /// <summary>
    ///   Tabs
    /// </summary>
    property Tabs: TOBDTouchHeaderTabCollection read FTabs write SetTabs;
    /// <summary>
    ///   Tab settings
    /// </summary>
    property Tab: TOBDTouchHeaderTab read FTab write SetTab;
    /// <summary>
    ///   Tab index (active tab)
    /// </summary>
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    /// <summary>
    ///   Battery indicator
    /// </summary>
    property BatteryIndicator: TOBDTouchHeaderBatteryIndicator read FBatteryIndicator write SetBatteryIndicator;

    /// <summary>
    ///   Back button click event
    /// </summary>
    property OnBackButtonClick: TNotifyEvent read FOnBackButtonClick write FOnBackButtonClick;
    /// <summary>
    ///   Action button click event
    /// </summary>
    property OnActionButtonClick: TNotifyEvent read FOnActionButtonClick write FOnActionButtonClick;
    /// <summary>
    ///   Tab index change event
    /// </summary>
    property OnTabChange: TOBDTouchHeaderTabChangeEvent read FOnTabChange write FOnTabChange;
  published
    /// <summary>
    ///   Component alignment (inherited)
    /// </summary>
    property Align default alTop;
    /// <summary>
    ///   Component anchors (inherited)
    /// </summary>
    property Anchors;
    /// <summary>
    ///   Component color (inherited)
    /// </summary>
    property Color;
  end;

implementation

uses
  System.Skia, Skia.Vcl;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBackground.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderBackground.SetToColor(Value: TColor);
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
constructor TOBDTouchHeaderBackground.Create;
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
procedure TOBDTouchHeaderBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBackground) then
  begin
    FFromColor := (Source as TOBDTouchHeaderBackground).FromColor;
    FToColor := (Source as TOBDTouchHeaderBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBorder.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderBorder.SetToColor(Value: TColor);
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
procedure TOBDTouchHeaderBorder.SetHeight(Value: Integer);
begin
  if (FHeight <> Value) and (Value >= 0) then
  begin
    // Set the new height
    FHeight := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderBorder.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BORDER_FROM;
  FToColor := DEFAULT_BORDER_TO;
  FHeight := DEFAULT_BORDER_HEIGHT;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBorder) then
  begin
    FFromColor := (Source as TOBDTouchHeaderBorder).FromColor;
    FToColor := (Source as TOBDTouchHeaderBorder).ToColor;
    FHeight := (Source as TOBDTouchHeaderBorder).Height;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButtonColor.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderButtonColor.SetToColor(Value: TColor);
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
constructor TOBDTouchHeaderButtonColor.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButtonColor.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButtonColor) then
  begin
    FFromColor := (Source as TOBDTouchHeaderButtonColor).FromColor;
    FToColor := (Source as TOBDTouchHeaderButtonColor).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET NORMAL COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetNormalColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assig nnormal color
  FNormalColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET HOT COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetHotColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign hot color
  FHotColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PRESSED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetPressedColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign pressed color
  FPressedColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET DISABLED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign disabled color
  FDisabledColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ENABLED
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    // Set enabled status
    FEnabled := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetCaption(Value: TCaption);
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
procedure TOBDTouchHeaderButton.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    // Set new width
    FWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetBorderColor(Value: TColor);
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
procedure TOBDTouchHeaderButton.SetBorderWidth(Value: Single);
begin
  if (FBorderWidth <> Value) and (Value >= 1) then
  begin
    // Set new border width
    FBorderWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET IMAGE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetImage(Value: TPicture);
begin
  // Set new image
  FImage.Assign(Value);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderButton.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create Normal button color
  FNormalColor := TOBDTouchHeaderButtonColor.Create;
  FNormalColor.OnChange := SettingsChanged;
  FNormalColor.FromColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_FROM;
  FNormalColor.ToColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_TO;
  // Create Hot button color
  FHotColor := TOBDTouchHeaderButtonColor.Create;
  FHotColor.OnChange := SettingsChanged;
  FHotColor.FromColor := DEFAULT_BACK_BUTTON_HOT_COLOR_FROM;
  FHotColor.ToColor := DEFAULT_BACK_BUTTON_HOT_COLOR_TO;
  // Create Pressed button color
  FPressedColor := TOBDTouchHeaderButtonColor.Create;
  FPressedColor.OnChange := SettingsChanged;
  FPressedColor.FromColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_FROM;
  FPressedColor.ToColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_TO;
  // Create Disabled button color
  FDisabledColor := TOBDTouchHeaderButtonColor.Create;
  FDisabledColor.OnChange := SettingsChanged;
  FDisabledColor.FromColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_FROM;
  FDisabledColor.ToColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_TO;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Create image
  FImage := TPicture.Create;
  FImage.OnChange := SettingsChanged;
  // Set defaults
  FEnabled := True;
  FVisible := True;
  FBorderColor := DEFAULT_BACK_BUTTON_BORDER_COLOR;
  FBorderWidth := 1;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderButton.Destroy;
begin
  // Free normal button color
  FNormalColor.Free;
  // Free hot button color
  FHotColor.Free;
  // Free pressed button color
  FPressedColor.Free;
  // Free disabled button color
  FDisabledColor.Free;
  // Free font
  FFont.Free;
  // Free image
  FImage.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButton) then
  begin
    FNormalColor.Assign((Source as TOBDTouchHeaderButton).NormalColor);
    FHotColor.Assign((Source as TOBDTouchHeaderButton).HotColor);
    FPressedColor.Assign((Source as TOBDTouchHeaderButton).PressedColor);
    FDisabledColor.Assign((Source as TOBDTouchHeaderButton).DisabledColor);
    FEnabled := (Source as TOBDTouchHeaderButton).Enabled;
    FVisible := (Source as TOBDTouchHeaderButton).Visible;
    FCaption := (Source as TOBDTouchHeaderButton).Caption;
    FFont.Assign((Source as TOBDTouchHeaderButton).Font);
    FWidth := (Source as TOBDTouchHeaderButton).Width;
    FBorderColor := (Source as TOBDTouchHeaderButton).BorderColor;
    FBorderWidth := (Source as TOBDTouchHeaderButton).BorderWidth;
    FImage.Assign((Source as TOBDTouchHeaderButton).Image);
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderActionButton.Create;
begin
  // Call inherited constructor
  inherited Create;
  FWidth    := DEFAULT_ACTION_BUTTON_WIDTH;
  FCaption  := DEFAULT_ACTION_BUTTON_TEXT;
  FNormalColor.FFromColor   := DEFAULT_ACTION_BUTTON_NORMAL_COLOR_FROM;
  FNormalColor.FToColor     := DEFAULT_ACTION_BUTTON_NORMAL_COLOR_TO;
  FHotColor.FFromColor      := DEFAULT_ACTION_BUTTON_HOT_COLOR_FROM;
  FHotColor.FToColor        := DEFAULT_ACTION_BUTTON_HOT_COLOR_TO;
  FPressedColor.FFromColor  := DEFAULT_ACTION_BUTTON_PRESSED_COLOR_FROM;
  FPressedColor.FToColor    := DEFAULT_ACTION_BUTTON_PRESSED_COLOR_TO;
  FDisabledColor.FFromColor := DEFAULT_ACTION_BUTTON_DISABLED_COLOR_FROM;
  FDisabledColor.FToColor   := DEFAULT_ACTION_BUTTON_DISABLED_COLOR_TO;
  FBorderColor              := DEFAULT_ACTION_BUTTON_BORDER_COLOR;
  FFont.Color := clWhite;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.SetCaption(Value: TCaption);
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
procedure TOBDTouchHeaderCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderCaption.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  FFont.Size := DEFAULT_CAPTION_FONT_SIZE;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderCaption.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderCaption) then
  begin
    FCaption := (Source as TOBDTouchHeaderCaption).Caption;
    FFont.Assign((Source as TOBDTouchHeaderCaption).Font);
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetCaption(Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET IMAGE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetImage(Value: TPicture);
begin
  FImage.Assign(Value);
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET ENABLED
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// IMAGE CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.ImageChanged(Sender: TObject);
begin
  Changed(False);
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDTouchHeaderTabItem.GetDisplayName: string;
begin
  Result := FCaption;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderTabItem.Create(Collection: TCollection);
begin
  // Call inherited constructor
  inherited Create(Collection);
  // Create image
  FImage := TPicture.Create;
  FImage.OnChange := ImageChanged;
  // Set defaults
  FCaption := Format('Tab %d', [Index]);
  FWidth := DEFAULT_TAB_WIDTH;
  FEnabled := True;
  FVisible := True;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderTabItem.Destroy;
begin
  // Free image
  FImage.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.Assign(Source: TPersistent);
begin
  if (Source is TOBDTouchHeaderTabItem) then
  begin
    FCaption := (Source as TOBDTouchHeaderTabItem).Caption;
    FImage.Assign((Source as TOBDTouchHeaderTabItem).Image);
    FWidth := (Source as TOBDTouchHeaderTabItem).Width;
    FEnabled := (Source as TOBDTouchHeaderTabItem).Enabled;
    FVisible := (Source as TOBDTouchHeaderTabItem).Visible;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// GET TAB ITEM
//------------------------------------------------------------------------------
function TOBDTouchHeaderTabCollection.GetItem(AIndex: Integer): TOBDTouchHeaderTabItem;
begin
  Result := TOBDTouchHeaderTabItem(inherited Items[AIndex]);
end;

//------------------------------------------------------------------------------
// SET TAB ITEM
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabCollection.SetItem(AIndex: Integer; const Value: TOBDTouchHeaderTabItem);
begin
  // Call inherited set item
  inherited SetItem(AIndex, Value);
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// ITEM UPDATE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabCollection.Update(Item: TCollectionItem);
begin
  // Call inherited update
  inherited Update(Item);
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderTabCollection.Create(AOwner: TPersistent);
begin
  // Call inherited constructor
  inherited Create(AOwner, TOBDTouchHeaderTabItem);
end;

//------------------------------------------------------------------------------
// ADD TAB ITEM
//------------------------------------------------------------------------------
function TOBDTouchHeaderTabCollection.Add: TOBDTouchHeaderTabItem;
begin
  // Create new tab item
  Result := TOBDTouchHeaderTabItem(inherited Add);
  // Set new tab item caption
  Result.Caption := Format('Tab %d', [NextID]);
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabCollection.Assign(Source: TPersistent);
var
  L: TOBDTouchHeaderTabCollection;
  I: Integer;
begin
  if (Source is TOBDTouchHeaderTabCollection) then
  begin
    // Cast the list as TOBDTouchHeaderTabCollection
    L := TOBDTouchHeaderTabCollection(Source);
    // Clear the items
    Clear;
    // Add the items
    for I := 0 to L.Count - 1 do Add.Assign(L.Items[I]);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET NORMAL COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetNormalColor(Value: TOBDTouchHeaderButtonColor);
begin
  FNormalColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET HOT COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetHotColor(Value: TOBDTouchHeaderButtonColor);
begin
  FHotColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PRESSED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetPressedColor(Value: TOBDTouchHeaderButtonColor);
begin
  FPressedColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET DISABLED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
begin
  FDisabledColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ACTIVE COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetActiveColor(Value: TOBDTouchHeaderButtonColor);
begin
  FActiveColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetBorderColor(Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    // Set new border color
    FBorderColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetBorderWidth(Value: Single);
begin
 if (FBorderWidth <> Value) then
  begin
    // Set new border width
    FBorderWidth := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderTab.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create Normal tab color
  FNormalColor := TOBDTouchHeaderButtonColor.Create;
  FNormalColor.OnChange := SettingsChanged;
  FNormalColor.FromColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_FROM;
  FNormalColor.ToColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_TO;
  // Create Hot tab color
  FHotColor := TOBDTouchHeaderButtonColor.Create;
  FHotColor.OnChange := SettingsChanged;
  FHotColor.FromColor := DEFAULT_BACK_BUTTON_HOT_COLOR_FROM;
  FHotColor.ToColor := DEFAULT_BACK_BUTTON_HOT_COLOR_TO;
  // Create Pressed tab color
  FPressedColor := TOBDTouchHeaderButtonColor.Create;
  FPressedColor.OnChange := SettingsChanged;
  FPressedColor.FromColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_FROM;
  FPressedColor.ToColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_TO;
  // Create Disabled tab color
  FDisabledColor := TOBDTouchHeaderButtonColor.Create;
  FDisabledColor.OnChange := SettingsChanged;
  FDisabledColor.FromColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_FROM;
  FDisabledColor.ToColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_TO;
  // Create Active tab color
  FActiveColor := TOBDTouchHeaderButtonColor.Create;
  FActiveColor.OnChange := SettingsChanged;
  FActiveColor.FromColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_TO;
  FActiveColor.ToColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_FROM;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Set defaults
  FBorderColor := DEFAULT_BACK_BUTTON_BORDER_COLOR;
  FBorderWidth := 1;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderTab.Destroy;
begin
  // Free normal color
  FNormalColor.Free;
  // Free hot color
  FHotColor.Free;
  // Free pressed color
  FPressedColor.Free;
  // Free disabled color
  FDisabledColor.Free;
  // Free active color
  FActiveColor.Free;
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButton) then
  begin
    FNormalColor.Assign((Source as TOBDTouchHeaderTab).NormalColor);
    FHotColor.Assign((Source as TOBDTouchHeaderTab).HotColor);
    FPressedColor.Assign((Source as TOBDTouchHeaderTab).PressedColor);
    FDisabledColor.Assign((Source as TOBDTouchHeaderTab).DisabledColor);
    FActiveColor.Assign((Source as TOBDTouchHeaderTab).ActiveColor);
    FFont.Assign((Source as TOBDTouchHeaderTab).Font);
    FBorderColor := (Source as TOBDTouchHeaderTab).BorderColor;
    FBorderWidth := (Source as TOBDTouchHeaderTab).BorderWidth;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetSize(Value: Integer);
begin
  if (FSize <> Value) and (FSize >= 16) then
  begin
    // Set new size
    FSize := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BSHOW LABEL
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetShowLabel(Value: Boolean);
begin
  if (FShowLabel <> Value) then
  begin
    // Set new showlabel
    FShowLabel := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET PERCENTAGE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetPercentage(Value: Single);
begin
  if (FPercentage <> Value) and (Value >= 0) and (Value <= 100) then
  begin
    // Set new percentage
    FPercentage := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchHeaderBatteryIndicator.SetBorderWidth(Value: Single);
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
// SET COLOR 25
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor25(Value: TColor);
begin
  if (FColor25 <> Value) then
  begin
    // Set new color
    FColor25 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR 50
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor50(Value: TColor);
begin
  if (FColor50 <> Value) then
  begin
    // Set new color
    FColor50 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR 75
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor75(Value: TColor);
begin
  if (FColor75 <> Value) then
  begin
    // Set new color
    FColor75 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR 100
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor100(Value: TColor);
begin
  if (FColor100 <> Value) then
  begin
    // Set new color
    FColor100 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderBatteryIndicator.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  FFont.Size := 7;
  // Set defaults
  FSize := 32;
  FVisible := True;
  FShowLabel := True;
  FPercentage := 100;
  FBorderColor := DEFAULT_BACK_BUTTON_BORDER_COLOR;
  FBorderWidth := 1;
  FColor25 := DEFAULT_BATTERY_COLOR_25;
  FColor50 := DEFAULT_BATTERY_COLOR_50;
  FColor75 := DEFAULT_BATTERY_COLOR_75;
  FColor100 := DEFAULT_BATTERY_COLOR_100;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderBatteryIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBatteryIndicator) then
  begin
    FSize := (Source as TOBDTouchHeaderBatteryIndicator).Size;
    FVisible := (Source as TOBDTouchHeaderBatteryIndicator).Visible;
    FShowLabel := (Source as TOBDTouchHeaderBatteryIndicator).ShowLabel;
    FPercentage := (Source as TOBDTouchHeaderBatteryIndicator).Percentage;
    FBorderColor := (Source as TOBDTouchHeaderBatteryIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchHeaderBatteryIndicator).BorderWidth;
    FColor25 := (Source as TOBDTouchHeaderBatteryIndicator).Color25;
    FColor50 := (Source as TOBDTouchHeaderBatteryIndicator).Color50;
    FColor75 := (Source as TOBDTouchHeaderBatteryIndicator).Color75;
    FColor100 := (Source as TOBDTouchHeaderBatteryIndicator).Color100;
    FFont.Assign((Source as TOBDTouchHeaderBatteryIndicator).Font);
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CLASS CONSTRUCTOR
//------------------------------------------------------------------------------
class constructor TOBDTouchHeader.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TOBDTouchHeader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// CLASS DESTRUCTOR
//------------------------------------------------------------------------------
class destructor TOBDTouchHeader.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TOBDTouchHeader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBackground(Value: TOBDTouchHeaderBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBorder(Value: TOBDTouchHeaderBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BACK BUTTON
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBackButton(Value: TOBDTouchHeaderButton);
begin
  FBackButton.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ACTION BUTTON
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetActionButton(Value: TOBDTouchHeaderActionButton);
begin
  FActionButton.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetCaption(Value: TOBDTouchHeaderCaption);
begin
  FCaption.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET TABS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetTabs(Value: TOBDTouchHeaderTabCollection);
begin
  FTabs.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET TAB SETTINGS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetTab(Value: TOBDTouchHeaderTab);
begin
  FTab.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET TAB INDEX
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetTabIndex(Value: Integer);
begin
  if (FTabIndex <> Value) and (Value >= -1) and (Value < FTabs.Count) then
  begin
    FTabIndex := Value;
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BATTERY INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBatteryIndicator(Value: TOBDTouchHeaderBatteryIndicator);
begin
  FBatteryIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// WM_PAINT MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  // Retrieve the invalidated rectangle
  if not GetUpdateRect(Handle, FUpdateRect, False) then
  // If no update region, default to the entire client area
  FUpdateRect := Rect(0, 0, Width, Height);
end;

//------------------------------------------------------------------------------
// WM_ERASEBKGND MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  // Set the handled flag
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------
// WM_KILLFOCUS MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.WMKillFocus(var Message: TWMKillFocus);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mousedown
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Reset back button state to normal
  if BackButton.Visible and BackButton.Enabled and (BackButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset action button state to normal
  if ActionButton.Visible and ActionButton.Enabled and (ActionButton.ButtonState <> 0) then
  begin
    ActionButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset tab state to normal
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and (FTabs.Items[I].TabState <> 0) then
  begin
    FTabs.Items[I].TabState := 0;
    NeedRedraw := True;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// CM_ENABLEDCHANGED MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.CMEnabledChanged(var Message: TMessage);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Reset back button state to normal
  if BackButton.Visible and BackButton.Enabled and (BackButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset action button state to normal
  if ActionButton.Visible and ActionButton.Enabled and (ActionButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset tab state to normal
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and (FTabs.Items[I].TabState <> 0) then
  begin
    FTabs.Items[I].TabState := 0;
    NeedRedraw := True;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// CM_MOUSELEAVE MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.CMMouseLeave(var Message: TMessage);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Reset back button state to normal
  if BackButton.Visible and BackButton.Enabled and (BackButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset action button state to normal
  if ActionButton.Visible and ActionButton.Enabled and (ActionButton.ButtonState <> 0) then
  begin
    ActionButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset tab state to normal
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and (FTabs.Items[I].TabState <> 0) then
  begin
    FTabs.Items[I].TabState := 0;
    NeedRedraw := True;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Adjust window style to avoid unnecessary redraws on size changes,
  // optimizing performance for custom drawing.
  with Params do Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Paint;
var
  X, Y, W, H: Integer;
begin
  // Call inherited Paint
  inherited;

  // Draw the buffer to the component canvas
  X := FUpdateRect.Left;
  Y := FUpdateRect.Top;
  W := FUpdateRect.Right - FUpdateRect.Left;
  H := FUpdateRect.Bottom - FUpdateRect.Top;

  if (W <> 0) and (H <> 0) then
    // Only update invalidated part
    BitBlt(Canvas.Handle, X, Y, W, H, FBuffer.Canvas.Handle, X,  Y, SRCCOPY)
  else
    // Repaint the whole buffer to the surface
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FBuffer.Canvas.Handle, X,  Y, SRCCOPY);
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Resize;
begin
  // Call inherited Resize
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Loaded;
begin
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
end;

//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// MOUSE DOWN HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mousedown
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Check if we are over the back button
  if BackButton.Visible and BackButton.Enabled then
  begin
    if PtInRect(BackButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to pressed
      if (BackButton.ButtonState < 2) then
      begin
        BackButton.ButtonState := 2;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (BackButton.ButtonState = 1) then
      begin
        BackButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over the action button
  if ActionButton.Visible and ActionButton.Enabled then
  begin
    if PtInRect(ActionButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to pressed
      if (ActionButton.ButtonState < 2) then
      begin
        ActionButton.ButtonState := 2;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (ActionButton.ButtonState = 1) then
      begin
        ActionButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over a tab
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and FTabs.Items[I].Enabled then
  begin
    if PtInRect(FTabs.Items[I].TabRect, Point(X, Y)) then
    begin
      // Set tab state to pressed
      if (FTabs.Items[I].TabState < 2) then
      begin
        FTabs.Items[I].TabState := 2;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset tab state to normal
      if (FTabs.Items[I].TabState =  1) then
      begin
        FTabs.Items[I].TabState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// MOUSE UP HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mouseup
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Check if we are over the back button
  if BackButton.Visible and BackButton.Enabled then
  begin
    if PtInRect(BackButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (BackButton.ButtonState = 2) then
      begin
        BackButton.ButtonState := 1;
        NeedRedraw := True;
        // If the button state is pressed, fire a click event
        if Assigned(FOnBackButtonClick) then FOnBackButtonClick(Self);
      end;
    end else
    begin
      // Reset button state to normal
      if (BackButton.ButtonState <> 0) then
      begin
        BackButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over the action button
  if ActionButton.Visible and ActionButton.Enabled then
  begin
    if PtInRect(ActionButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (ActionButton.ButtonState = 2) then
      begin
        ActionButton.ButtonState := 1;
        NeedRedraw := True;
        // If the button state is pressed, fire a click event
        if Assigned(FOnActionButtonClick) then FOnActionButtonClick(Self);
      end;
    end else
    begin
      // Reset button state to normal
      if (ActionButton.ButtonState <> 0) then
      begin
        ActionButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over a tab
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and FTabs.Items[I].Enabled then
  begin
    if PtInRect(FTabs.Items[I].TabRect, Point(X, Y)) then
    begin
      // Set tab state to hot
      if (FTabs.Items[I].TabState = 2) then
      begin
        FTabs.Items[I].TabState := 1;
        NeedRedraw := True;
        // Update the tabindex
        FTabIndex := I;
        // If the tab state is pressed, fire a click event
        if Assigned(OnTabChange) then OnTabChange(Self, I);
      end;
    end else
    begin
      // Reset tab state to normal
      if (FTabs.Items[I].TabState <> 0) then
      begin
        FTabs.Items[I].TabState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// MOUSE MOVE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mousemove
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Check if we are over the back button
  if BackButton.Visible and BackButton.Enabled then
  begin
    if PtInRect(BackButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (BackButton.ButtonState = 0) then
      begin
        BackButton.ButtonState := 1;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (BackButton.ButtonState = 1) then
      begin
        BackButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over the action button
  if ActionButton.Visible and ActionButton.Enabled then
  begin
    if PtInRect(ActionButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (ActionButton.ButtonState = 0) then
      begin
        ActionButton.ButtonState := 1;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (ActionButton.ButtonState = 1) then
      begin
        ActionButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over a tab
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and FTabs.Items[I].Enabled then
  begin
    if PtInRect(FTabs.Items[I].TabRect, Point(X, Y)) then
    begin
      // Set tab state to hot
      if (FTabs.Items[I].TabState = 0) then
      begin
        FTabs.Items[I].TabState := 1;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset tab state to normal
      if (FTabs.Items[I].TabState = 1) then
      begin
        FTabs.Items[I].TabState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SettingsChanged(Sender: TObject);
begin
  // Reset tab index
  if (FTabIndex > FTabs.Count) then FTabIndex := FTabs.Count -1;
  if (FTabs.Count > 0) and (FTabIndex = -1) then FTabIndex := 0;
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.PaintBuffer;

  type
    TTabOverlay = record
      Caption: string;
      Rect: TRect;
      Image: TGraphic;
      Visible: Boolean;
    end;
  function GraphicToSkImage(const Graphic: TGraphic): ISkImage;
  var
    Bitmap: TBitmap;
  begin
    Result := nil;
    if not Assigned(Graphic) then
      Exit;

    Bitmap := TBitmap.Create;
    try
      Bitmap.Assign(Graphic);
      Result := TSkImage.MakeFromBitmap(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
  var
    Surface: ISkSurface;
    Canvas: ISkCanvas;
    Paint: ISkPaint;
    TabPath, GlarePath: ISkPath;
    BackgroundRect, BorderRect: TRectF;
    ButtonRect, TabRect, BatteryRect: TRectF;
    TabOverlays: TArray<TTabOverlay>;
    BackCaptionRect, ActionCaptionRect, CaptionRect, BatteryCaptionRect: TRect;
    TotalTabWidth, I, FirstVisibleTab, LastVisibleTab: Integer;
    TabsVisible, IsFirstTab, IsLastTab: Boolean;
    BackHasImage, ActionHasImage, BatteryShowLabel: Boolean;
    X, Y, W, H, TX: Single;
    BodyFromColor, BodyToColor: TColor;
  begin
    // Ensure the back-buffer matches the control dimensions before drawing
    Buffer.SetSize(Width, Height);

    // Allocate a Skia surface for fast CPU-backed drawing
    Surface := TSkSurface.MakeRasterN32Premul(Width, Height);
    Canvas := Surface.Canvas;
    Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

    // Paint the background gradient when both colors are provided
    if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
    begin
      BackgroundRect := TRectF.Create(0.0, 0.0, Width + 0.0, Height + 0.0);
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(BackgroundRect.Left, BackgroundRect.Top),
        TSkPoint.Create(BackgroundRect.Left, BackgroundRect.Bottom),
        [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawRect(BackgroundRect, Paint);
    end;

    // Draw the border strip when enabled
    if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
    begin
      BorderRect := TRectF.Create(0.0, 0.0, Width + 0.0, Border.Height + 0.0);
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(BorderRect.Left, BorderRect.Top),
        TSkPoint.Create(BorderRect.Left, BorderRect.Bottom),
        [SafeColorRefToSkColor(Border.FromColor), SafeColorRefToSkColor(Border.ToColor)],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawRect(BorderRect, Paint);
    end;

    // Draw the back button
    BackHasImage := False;
    if BackButton.Visible then
    begin
      X := 8;
      H := Height - 16;
      Y := ((Height / 2) - (H / 2)) - (Border.Height / 2);
      ButtonRect := TRectF.Create(X, Y, X + BackButton.Width, Y + H);
      BackButton.ButtonRect := TRect.Create(Round(X), Round(Y), Round(X + ButtonRect.Width), Round(Y + ButtonRect.Height));
      CaptionRect := Rect(Round(X + 6), Round(Y), Round(X + ButtonRect.Width), Round(Y + ButtonRect.Height));

      if not BackButton.Enabled then
      begin
        BodyFromColor := BackButton.DisabledColor.FromColor;
        BodyToColor := BackButton.DisabledColor.ToColor;
      end else
      begin
        case BackButton.ButtonState of
          1: begin BodyFromColor := BackButton.HotColor.FromColor; BodyToColor := BackButton.HotColor.ToColor; end;
          2: begin BodyFromColor := BackButton.PressedColor.FromColor; BodyToColor := BackButton.PressedColor.ToColor; end;
        else
          begin BodyFromColor := BackButton.NormalColor.FromColor; BodyToColor := BackButton.NormalColor.ToColor; end;
        end;
      end;

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Top),
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Bottom),
        [SafeColorRefToSkColor(BodyFromColor), SafeColorRefToSkColor(BodyToColor)],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawPath(CreateBackButtonPath(ButtonRect, DEFAULT_CORNER), Paint);

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Top),
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Bottom),
        [$4BFFFFFF, $1EFFFFFF],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawPath(CreateGlareBackButtonPath(ButtonRect, DEFAULT_CORNER), Paint);

      if BackButton.BorderColor <> clNone then
      begin
        Paint := TSkPaint.Create;
        Paint.AntiAlias := True;
        Paint.Style := TSkPaintStyle.Stroke;
        Paint.StrokeWidth := BackButton.BorderWidth;
        Paint.Color := SafeColorRefToSkColor(BackButton.BorderColor);
        Canvas.DrawPath(CreateBackButtonPath(ButtonRect, DEFAULT_CORNER), Paint);
      end;

      BackCaptionRect := CaptionRect;
      BackHasImage := Assigned(BackButton.Image.Graphic);
    end;

    // Draw the action button
    ActionHasImage := False;
    if ActionButton.Visible then
    begin
      if BackButton.Visible then
        X := 8 + BackButton.Width + 8
      else
        X := 8;
      H := Height - 16;
      Y := ((Height / 2) - (H / 2)) - (Border.Height / 2);
      ButtonRect := TRectF.Create(X, Y, X + ActionButton.Width, Y + H);
      ActionButton.ButtonRect := TRect.Create(Round(X), Round(Y), Round(X + ButtonRect.Width), Round(Y + ButtonRect.Height));
      CaptionRect := Rect(Round(X), Round(Y), Round(X + ButtonRect.Width), Round(Y + ButtonRect.Height));

      if not ActionButton.Enabled then
      begin
        BodyFromColor := ActionButton.DisabledColor.FromColor;
        BodyToColor := ActionButton.DisabledColor.ToColor;
      end else
      begin
        case ActionButton.ButtonState of
          1: begin BodyFromColor := ActionButton.HotColor.FromColor; BodyToColor := ActionButton.HotColor.ToColor; end;
          2: begin BodyFromColor := ActionButton.PressedColor.FromColor; BodyToColor := ActionButton.PressedColor.ToColor; end;
        else
          begin BodyFromColor := ActionButton.NormalColor.FromColor; BodyToColor := ActionButton.NormalColor.ToColor; end;
        end;
      end;

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Top),
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Bottom),
        [SafeColorRefToSkColor(BodyFromColor), SafeColorRefToSkColor(BodyToColor)],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawPath(CreateRoundRectPath(ButtonRect, DEFAULT_CORNER), Paint);

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Top),
        TSkPoint.Create(ButtonRect.Left, ButtonRect.Bottom),
        [$4BFFFFFF, $1EFFFFFF],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawPath(CreateGlareRoundRectPath(ButtonRect, DEFAULT_CORNER), Paint);

      if ActionButton.BorderColor <> clNone then
      begin
        Paint := TSkPaint.Create;
        Paint.AntiAlias := True;
        Paint.Style := TSkPaintStyle.Stroke;
        Paint.StrokeWidth := ActionButton.BorderWidth;
        Paint.Color := SafeColorRefToSkColor(ActionButton.BorderColor);
        Canvas.DrawPath(CreateRoundRectPath(ButtonRect, DEFAULT_CORNER), Paint);
      end;

      ActionCaptionRect := CaptionRect;
      ActionHasImage := Assigned(ActionButton.Image.Graphic);
    end;

    // Tab layout calculations
    TotalTabWidth := 0;
    FirstVisibleTab := -1;
    LastVisibleTab  := -1;
    for I := 0 to FTabs.Count -1 do
    begin
      if FTabs.Items[I].Visible then
      begin
        Inc(TotalTabWidth, FTabs.Items[I].Width);
        if FirstVisibleTab = -1 then FirstVisibleTab := I;
        LastVisibleTab := I;
      end;
    end;

    TabsVisible := (TotalTabWidth > 0);
    SetLength(TabOverlays, FTabs.Count);

    if TabsVisible then
    begin
      H := Height - 16;
      TX := 16;
      if BackButton.Visible then TX := TX + BackButton.Width + 8;
      if ActionButton.Visible then TX := TX + ActionButton.Width + 8;
      W := Width - (TX * 2);
      TX := TX + ((W / 2) - (TotalTabWidth / 2));

      for I := 0 to FTabs.Count -1 do
      begin
        IsFirstTab := I = FirstVisibleTab;
        IsLastTab  := I = LastVisibleTab;
        Y := ((Height / 2) - (H / 2)) - (Border.Height / 2);
        if not FTabs.Items[I].Visible then Continue;

        TabRect := TRectF.Create(TX, Y, TX + FTabs.Items[I].Width, Y + H);
        FTabs.Items[I].TabRect := Rect(Round(TX), Round(Y), Round(TX + FTabs.Items[I].Width), Round(Y + H));
        TabOverlays[I].Caption := FTabs.Items[I].Caption;
        TabOverlays[I].Rect := FTabs.Items[I].TabRect;
        TabOverlays[I].Image := FTabs.Items[I].Image.Graphic;
        TabOverlays[I].Visible := True;

        if IsFirstTab and (not IsLastTab) then
        begin
          TabPath := CreateTabLeftPath(TabRect, DEFAULT_CORNER);
          GlarePath := CreateGlareTabLeftPath(TabRect, DEFAULT_CORNER);
        end else if (not IsFirstTab) and IsLastTab then
        begin
          TabPath := CreateTabRightPath(TabRect, DEFAULT_CORNER);
          GlarePath := CreateGlareTabRightPath(TabRect, DEFAULT_CORNER);
        end else if (not IsFirstTab) and (not IsLastTab) then
        begin
          TabPath := CreateTabCenterPath(TabRect);
          GlarePath := CreateGlareTabCenterPath(TabRect);
        end else
        begin
          TabPath := CreateRoundRectPath(TabRect, DEFAULT_CORNER);
          GlarePath := CreateGlareRoundRectPath(TabRect, DEFAULT_CORNER);
        end;

        Paint := TSkPaint.Create;
        Paint.AntiAlias := True;
        if not FTabs.Items[I].Enabled then
          Paint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(TabRect.Left, TabRect.Top), TSkPoint.Create(TabRect.Left, TabRect.Bottom),
            [SafeColorRefToSkColor(Tab.DisabledColor.FromColor), SafeColorRefToSkColor(Tab.DisabledColor.ToColor)], nil, TSkTileMode.Clamp)
        else if TabIndex = I then
          Paint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(TabRect.Left, TabRect.Top), TSkPoint.Create(TabRect.Left, TabRect.Bottom),
            [SafeColorRefToSkColor(Tab.ActiveColor.FromColor), SafeColorRefToSkColor(Tab.ActiveColor.ToColor)], nil, TSkTileMode.Clamp)
        else
          case FTabs.Items[I].TabState of
            1: Paint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(TabRect.Left, TabRect.Top), TSkPoint.Create(TabRect.Left, TabRect.Bottom),
                  [SafeColorRefToSkColor(Tab.HotColor.FromColor), SafeColorRefToSkColor(Tab.HotColor.ToColor)], nil, TSkTileMode.Clamp);
            2: Paint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(TabRect.Left, TabRect.Top), TSkPoint.Create(TabRect.Left, TabRect.Bottom),
                  [SafeColorRefToSkColor(Tab.PressedColor.FromColor), SafeColorRefToSkColor(Tab.PressedColor.ToColor)], nil, TSkTileMode.Clamp);
          else
            Paint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(TabRect.Left, TabRect.Top), TSkPoint.Create(TabRect.Left, TabRect.Bottom),
                  [SafeColorRefToSkColor(Tab.NormalColor.FromColor), SafeColorRefToSkColor(Tab.NormalColor.ToColor)], nil, TSkTileMode.Clamp);
          end;
        Canvas.DrawPath(TabPath, Paint);

        Paint := TSkPaint.Create;
        Paint.AntiAlias := True;
        Paint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(TabRect.Left, TabRect.Top), TSkPoint.Create(TabRect.Left, TabRect.Bottom), [$4BFFFFFF, $1EFFFFFF], nil, TSkTileMode.Clamp);
        Canvas.DrawPath(GlarePath, Paint);

        if Tab.BorderColor <> clNone then
        begin
          Paint := TSkPaint.Create;
          Paint.AntiAlias := True;
          Paint.Style := TSkPaintStyle.Stroke;
          Paint.StrokeWidth := Tab.BorderWidth;
          Paint.Color := SafeColorRefToSkColor(Tab.BorderColor);
          Canvas.DrawPath(TabPath, Paint);
        end;

        TX := TX + (FTabs.Items[I].Width - Tab.BorderWidth);
      end;
    end;

    // Draw the battery indicator
    BatteryShowLabel := False;
    if BatteryIndicator.Visible then
    begin
      X := (Width - 8) - BatteryIndicator.Size;
      Y := (Height / 2) - (BatteryIndicator.Size / 2);
      BatteryRect := TRectF.Create(X, Y, X + BatteryIndicator.Size, Y + BatteryIndicator.Size);
      CaptionRect := Rect(Round(X), Round(Y + 2), Round(X + BatteryIndicator.Size - 2), Round(Y + BatteryIndicator.Size - 2));

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.StrokeWidth := BatteryIndicator.BorderWidth;
      Paint.Color := SafeColorRefToSkColor(BatteryIndicator.BorderColor);
      Canvas.DrawPath(CreateBatteryPath(BatteryRect, DEFAULT_CORNER), Paint);

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Fill;
      if (BatteryIndicator.Percentage <= 25) then
        Paint.Color := SafeColorRefToSkColor(BatteryIndicator.Color25)
      else if (BatteryIndicator.Percentage <= 50) then
        Paint.Color := SafeColorRefToSkColor(BatteryIndicator.Color50)
      else if (BatteryIndicator.Percentage <= 75) then
        Paint.Color := SafeColorRefToSkColor(BatteryIndicator.Color75)
      else
        Paint.Color := SafeColorRefToSkColor(BatteryIndicator.Color100);
      Canvas.DrawPath(CreateBatteryPercentagePath(BatteryRect, BatteryIndicator.Percentage), Paint);

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(BatteryRect.Left, BatteryRect.Top),
        TSkPoint.Create(BatteryRect.Left, BatteryRect.Bottom),
        [$4BFFFFFF, $1EFFFFFF],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawPath(CreateGlareBatteryPercentagePath(BatteryRect, BatteryIndicator.Percentage), Paint);

      BatteryCaptionRect := CaptionRect;
      BatteryShowLabel := BatteryIndicator.ShowLabel;
    end;

    // Draw text and icons using Skia so the buffer stays Skia-driven end-to-end
    if BackButton.Visible then
    begin
      if BackHasImage and Assigned(BackButton.Image.Graphic) then
      begin
        var BackImage := GraphicToSkImage(BackButton.Image.Graphic);
        if Assigned(BackImage) then
          Canvas.DrawImageRect(BackImage,
            TRectF.Create(
              BackCaptionRect.Left + ((BackCaptionRect.Width - BackImage.Width) / 2),
              BackCaptionRect.Top + ((BackCaptionRect.Height - BackImage.Height) / 2),
              BackCaptionRect.Left + ((BackCaptionRect.Width - BackImage.Width) / 2) + BackImage.Width,
              BackCaptionRect.Top + ((BackCaptionRect.Height - BackImage.Height) / 2) + BackImage.Height
            ),
            TSkSamplingOptions.HighQuality);
      end else
        DrawSkTextCentered(Canvas, BackButton.Caption, BackButton.Font, TRectF.Create(BackCaptionRect), BackButton.Font.Color);
    end;

    if ActionButton.Visible then
    begin
      if ActionHasImage and Assigned(ActionButton.Image.Graphic) then
      begin
        var ActionImage := GraphicToSkImage(ActionButton.Image.Graphic);
        if Assigned(ActionImage) then
          Canvas.DrawImageRect(ActionImage,
            TRectF.Create(
              ActionCaptionRect.Left + ((ActionCaptionRect.Width - ActionImage.Width) / 2),
              ActionCaptionRect.Top + ((ActionCaptionRect.Height - ActionImage.Height) / 2),
              ActionCaptionRect.Left + ((ActionCaptionRect.Width - ActionImage.Width) / 2) + ActionImage.Width,
              ActionCaptionRect.Top + ((ActionCaptionRect.Height - ActionImage.Height) / 2) + ActionImage.Height
            ),
            TSkSamplingOptions.HighQuality);
      end else
        DrawSkTextCentered(Canvas, ActionButton.Caption, ActionButton.Font, TRectF.Create(ActionCaptionRect), ActionButton.Font.Color);
    end;

    if TabsVisible then
    begin
      for I := 0 to FTabs.Count -1 do
      begin
        if not TabOverlays[I].Visible then Continue;
        if Assigned(TabOverlays[I].Image) then
        begin
          var TabImage := GraphicToSkImage(TabOverlays[I].Image);
          if Assigned(TabImage) then
            Canvas.DrawImageRect(TabImage,
              TRectF.Create(
                TabOverlays[I].Rect.Left + ((TabOverlays[I].Rect.Width - TabImage.Width) / 2),
                TabOverlays[I].Rect.Top + ((TabOverlays[I].Rect.Height - TabImage.Height) / 2),
                TabOverlays[I].Rect.Left + ((TabOverlays[I].Rect.Width - TabImage.Width) / 2) + TabImage.Width,
                TabOverlays[I].Rect.Top + ((TabOverlays[I].Rect.Height - TabImage.Height) / 2) + TabImage.Height
              ),
              TSkSamplingOptions.HighQuality);
        end else
          DrawSkTextCentered(Canvas, TabOverlays[I].Caption, Tab.Font, TRectF.Create(TabOverlays[I].Rect), Tab.Font.Color);
      end;
    end else if (Caption.Caption <> '') then
      DrawSkTextCentered(Canvas, Caption.Caption, Caption.Font, RectF(0, 0, Width, Height), Caption.Font.Color);

    if BatteryIndicator.Visible and BatteryShowLabel then
      DrawSkTextCentered(Canvas, Format('%d%%', [Round(BatteryIndicator.Percentage)]), BatteryIndicator.Font, TRectF.Create(BatteryCaptionRect), BatteryIndicator.Font.Color);

    // Copy the Skia surface into the component buffer
    Surface.MakeImageSnapshot.ToBitmap(Buffer);
  end;
//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderBackground.Create;
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
procedure TOBDTouchHeaderBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBackground) then
  begin
    FFromColor := (Source as TOBDTouchHeaderBackground).FromColor;
    FToColor := (Source as TOBDTouchHeaderBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBorder.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderBorder.SetToColor(Value: TColor);
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
procedure TOBDTouchHeaderBorder.SetHeight(Value: Integer);
begin
  if (FHeight <> Value) and (Value >= 0) then
  begin
    // Set the new height
    FHeight := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderBorder.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BORDER_FROM;
  FToColor := DEFAULT_BORDER_TO;
  FHeight := DEFAULT_BORDER_HEIGHT;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBorder) then
  begin
    FFromColor := (Source as TOBDTouchHeaderBorder).FromColor;
    FToColor := (Source as TOBDTouchHeaderBorder).ToColor;
    FHeight := (Source as TOBDTouchHeaderBorder).Height;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButtonColor.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderButtonColor.SetToColor(Value: TColor);
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
constructor TOBDTouchHeaderButtonColor.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButtonColor.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButtonColor) then
  begin
    FFromColor := (Source as TOBDTouchHeaderButtonColor).FromColor;
    FToColor := (Source as TOBDTouchHeaderButtonColor).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET NORMAL COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetNormalColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assig nnormal color
  FNormalColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET HOT COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetHotColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign hot color
  FHotColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PRESSED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetPressedColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign pressed color
  FPressedColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET DISABLED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign disabled color
  FDisabledColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ENABLED
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    // Set enabled status
    FEnabled := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetCaption(Value: TCaption);
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
procedure TOBDTouchHeaderButton.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    // Set new width
    FWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetBorderColor(Value: TColor);
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
procedure TOBDTouchHeaderButton.SetBorderWidth(Value: Single);
begin
  if (FBorderWidth <> Value) and (Value >= 1) then
  begin
    // Set new border width
    FBorderWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET IMAGE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetImage(Value: TPicture);
begin
  // Set new image
  FImage.Assign(Value);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderButton.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create Normal button color
  FNormalColor := TOBDTouchHeaderButtonColor.Create;
  FNormalColor.OnChange := SettingsChanged;
  FNormalColor.FromColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_FROM;
  FNormalColor.ToColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_TO;
  // Create Hot button color
  FHotColor := TOBDTouchHeaderButtonColor.Create;
  FHotColor.OnChange := SettingsChanged;
  FHotColor.FromColor := DEFAULT_BACK_BUTTON_HOT_COLOR_FROM;
  FHotColor.ToColor := DEFAULT_BACK_BUTTON_HOT_COLOR_TO;
  // Create Pressed button color
  FPressedColor := TOBDTouchHeaderButtonColor.Create;
  FPressedColor.OnChange := SettingsChanged;
  FPressedColor.FromColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_FROM;
  FPressedColor.ToColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_TO;
  // Create Disabled button color
  FDisabledColor := TOBDTouchHeaderButtonColor.Create;
  FDisabledColor.OnChange := SettingsChanged;
  FDisabledColor.FromColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_FROM;
  FDisabledColor.ToColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_TO;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Create image
  FImage := TPicture.Create;
  FImage.OnChange := SettingsChanged;
  // Set defaults
  FEnabled := True;
  FVisible := True;
  FBorderColor := DEFAULT_BACK_BUTTON_BORDER_COLOR;
  FBorderWidth := 1;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderButton.Destroy;
begin
  // Free normal button color
  FNormalColor.Free;
  // Free hot button color
  FHotColor.Free;
  // Free pressed button color
  FPressedColor.Free;
  // Free disabled button color
  FDisabledColor.Free;
  // Free font
  FFont.Free;
  // Free image
  FImage.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButton) then
  begin
    FNormalColor.Assign((Source as TOBDTouchHeaderButton).NormalColor);
    FHotColor.Assign((Source as TOBDTouchHeaderButton).HotColor);
    FPressedColor.Assign((Source as TOBDTouchHeaderButton).PressedColor);
    FDisabledColor.Assign((Source as TOBDTouchHeaderButton).DisabledColor);
    FEnabled := (Source as TOBDTouchHeaderButton).Enabled;
    FVisible := (Source as TOBDTouchHeaderButton).Visible;
    FCaption := (Source as TOBDTouchHeaderButton).Caption;
    FFont.Assign((Source as TOBDTouchHeaderButton).Font);
    FWidth := (Source as TOBDTouchHeaderButton).Width;
    FBorderColor := (Source as TOBDTouchHeaderButton).BorderColor;
    FBorderWidth := (Source as TOBDTouchHeaderButton).BorderWidth;
    FImage.Assign((Source as TOBDTouchHeaderButton).Image);
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderActionButton.Create;
begin
  // Call inherited constructor
  inherited Create;
  FWidth    := DEFAULT_ACTION_BUTTON_WIDTH;
  FCaption  := DEFAULT_ACTION_BUTTON_TEXT;
  FNormalColor.FFromColor   := DEFAULT_ACTION_BUTTON_NORMAL_COLOR_FROM;
  FNormalColor.FToColor     := DEFAULT_ACTION_BUTTON_NORMAL_COLOR_TO;
  FHotColor.FFromColor      := DEFAULT_ACTION_BUTTON_HOT_COLOR_FROM;
  FHotColor.FToColor        := DEFAULT_ACTION_BUTTON_HOT_COLOR_TO;
  FPressedColor.FFromColor  := DEFAULT_ACTION_BUTTON_PRESSED_COLOR_FROM;
  FPressedColor.FToColor    := DEFAULT_ACTION_BUTTON_PRESSED_COLOR_TO;
  FDisabledColor.FFromColor := DEFAULT_ACTION_BUTTON_DISABLED_COLOR_FROM;
  FDisabledColor.FToColor   := DEFAULT_ACTION_BUTTON_DISABLED_COLOR_TO;
  FBorderColor              := DEFAULT_ACTION_BUTTON_BORDER_COLOR;
  FFont.Color := clWhite;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.SetCaption(Value: TCaption);
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
procedure TOBDTouchHeaderCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderCaption.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  FFont.Size := DEFAULT_CAPTION_FONT_SIZE;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderCaption.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderCaption) then
  begin
    FCaption := (Source as TOBDTouchHeaderCaption).Caption;
    FFont.Assign((Source as TOBDTouchHeaderCaption).Font);
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetCaption(Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET IMAGE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetImage(Value: TPicture);
begin
  FImage.Assign(Value);
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET ENABLED
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// IMAGE CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.ImageChanged(Sender: TObject);
begin
  Changed(False);
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDTouchHeaderTabItem.GetDisplayName: string;
begin
  Result := FCaption;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderTabItem.Create(Collection: TCollection);
begin
  // Call inherited constructor
  inherited Create(Collection);
  // Create image
  FImage := TPicture.Create;
  FImage.OnChange := ImageChanged;
  // Set defaults
  FCaption := Format('Tab %d', [Index]);
  FWidth := DEFAULT_TAB_WIDTH;
  FEnabled := True;
  FVisible := True;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderTabItem.Destroy;
begin
  // Free image
  FImage.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabItem.Assign(Source: TPersistent);
begin
  if (Source is TOBDTouchHeaderTabItem) then
  begin
    FCaption := (Source as TOBDTouchHeaderTabItem).Caption;
    FImage.Assign((Source as TOBDTouchHeaderTabItem).Image);
    FWidth := (Source as TOBDTouchHeaderTabItem).Width;
    FEnabled := (Source as TOBDTouchHeaderTabItem).Enabled;
    FVisible := (Source as TOBDTouchHeaderTabItem).Visible;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// GET TAB ITEM
//------------------------------------------------------------------------------
function TOBDTouchHeaderTabCollection.GetItem(AIndex: Integer): TOBDTouchHeaderTabItem;
begin
  Result := TOBDTouchHeaderTabItem(inherited Items[AIndex]);
end;

//------------------------------------------------------------------------------
// SET TAB ITEM
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabCollection.SetItem(AIndex: Integer; const Value: TOBDTouchHeaderTabItem);
begin
  // Call inherited set item
  inherited SetItem(AIndex, Value);
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// ITEM UPDATE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabCollection.Update(Item: TCollectionItem);
begin
  // Call inherited update
  inherited Update(Item);
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderTabCollection.Create(AOwner: TPersistent);
begin
  // Call inherited constructor
  inherited Create(AOwner, TOBDTouchHeaderTabItem);
end;

//------------------------------------------------------------------------------
// ADD TAB ITEM
//------------------------------------------------------------------------------
function TOBDTouchHeaderTabCollection.Add: TOBDTouchHeaderTabItem;
begin
  // Create new tab item
  Result := TOBDTouchHeaderTabItem(inherited Add);
  // Set new tab item caption
  Result.Caption := Format('Tab %d', [NextID]);
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTabCollection.Assign(Source: TPersistent);
var
  L: TOBDTouchHeaderTabCollection;
  I: Integer;
begin
  if (Source is TOBDTouchHeaderTabCollection) then
  begin
    // Cast the list as TOBDTouchHeaderTabCollection
    L := TOBDTouchHeaderTabCollection(Source);
    // Clear the items
    Clear;
    // Add the items
    for I := 0 to L.Count - 1 do Add.Assign(L.Items[I]);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET NORMAL COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetNormalColor(Value: TOBDTouchHeaderButtonColor);
begin
  FNormalColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET HOT COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetHotColor(Value: TOBDTouchHeaderButtonColor);
begin
  FHotColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PRESSED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetPressedColor(Value: TOBDTouchHeaderButtonColor);
begin
  FPressedColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET DISABLED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
begin
  FDisabledColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ACTIVE COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetActiveColor(Value: TOBDTouchHeaderButtonColor);
begin
  FActiveColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetBorderColor(Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    // Set new border color
    FBorderColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SetBorderWidth(Value: Single);
begin
 if (FBorderWidth <> Value) then
  begin
    // Set new border width
    FBorderWidth := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderTab.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create Normal tab color
  FNormalColor := TOBDTouchHeaderButtonColor.Create;
  FNormalColor.OnChange := SettingsChanged;
  FNormalColor.FromColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_FROM;
  FNormalColor.ToColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_TO;
  // Create Hot tab color
  FHotColor := TOBDTouchHeaderButtonColor.Create;
  FHotColor.OnChange := SettingsChanged;
  FHotColor.FromColor := DEFAULT_BACK_BUTTON_HOT_COLOR_FROM;
  FHotColor.ToColor := DEFAULT_BACK_BUTTON_HOT_COLOR_TO;
  // Create Pressed tab color
  FPressedColor := TOBDTouchHeaderButtonColor.Create;
  FPressedColor.OnChange := SettingsChanged;
  FPressedColor.FromColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_FROM;
  FPressedColor.ToColor := DEFAULT_BACK_BUTTON_PRESSED_COLOR_TO;
  // Create Disabled tab color
  FDisabledColor := TOBDTouchHeaderButtonColor.Create;
  FDisabledColor.OnChange := SettingsChanged;
  FDisabledColor.FromColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_FROM;
  FDisabledColor.ToColor := DEFAULT_BACK_BUTTON_DISABLED_COLOR_TO;
  // Create Active tab color
  FActiveColor := TOBDTouchHeaderButtonColor.Create;
  FActiveColor.OnChange := SettingsChanged;
  FActiveColor.FromColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_TO;
  FActiveColor.ToColor := DEFAULT_BACK_BUTTON_NORMAL_COLOR_FROM;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Set defaults
  FBorderColor := DEFAULT_BACK_BUTTON_BORDER_COLOR;
  FBorderWidth := 1;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderTab.Destroy;
begin
  // Free normal color
  FNormalColor.Free;
  // Free hot color
  FHotColor.Free;
  // Free pressed color
  FPressedColor.Free;
  // Free disabled color
  FDisabledColor.Free;
  // Free active color
  FActiveColor.Free;
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderTab.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButton) then
  begin
    FNormalColor.Assign((Source as TOBDTouchHeaderTab).NormalColor);
    FHotColor.Assign((Source as TOBDTouchHeaderTab).HotColor);
    FPressedColor.Assign((Source as TOBDTouchHeaderTab).PressedColor);
    FDisabledColor.Assign((Source as TOBDTouchHeaderTab).DisabledColor);
    FActiveColor.Assign((Source as TOBDTouchHeaderTab).ActiveColor);
    FFont.Assign((Source as TOBDTouchHeaderTab).Font);
    FBorderColor := (Source as TOBDTouchHeaderTab).BorderColor;
    FBorderWidth := (Source as TOBDTouchHeaderTab).BorderWidth;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetSize(Value: Integer);
begin
  if (FSize <> Value) and (FSize >= 16) then
  begin
    // Set new size
    FSize := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BSHOW LABEL
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetShowLabel(Value: Boolean);
begin
  if (FShowLabel <> Value) then
  begin
    // Set new showlabel
    FShowLabel := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET PERCENTAGE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetPercentage(Value: Single);
begin
  if (FPercentage <> Value) and (Value >= 0) and (Value <= 100) then
  begin
    // Set new percentage
    FPercentage := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchHeaderBatteryIndicator.SetBorderWidth(Value: Single);
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
// SET COLOR 25
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor25(Value: TColor);
begin
  if (FColor25 <> Value) then
  begin
    // Set new color
    FColor25 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR 50
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor50(Value: TColor);
begin
  if (FColor50 <> Value) then
  begin
    // Set new color
    FColor50 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR 75
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor75(Value: TColor);
begin
  if (FColor75 <> Value) then
  begin
    // Set new color
    FColor75 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET COLOR 100
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetColor100(Value: TColor);
begin
  if (FColor100 <> Value) then
  begin
    // Set new color
    FColor100 := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderBatteryIndicator.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  FFont.Size := 7;
  // Set defaults
  FSize := 32;
  FVisible := True;
  FShowLabel := True;
  FPercentage := 100;
  FBorderColor := DEFAULT_BACK_BUTTON_BORDER_COLOR;
  FBorderWidth := 1;
  FColor25 := DEFAULT_BATTERY_COLOR_25;
  FColor50 := DEFAULT_BATTERY_COLOR_50;
  FColor75 := DEFAULT_BATTERY_COLOR_75;
  FColor100 := DEFAULT_BATTERY_COLOR_100;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderBatteryIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBatteryIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBatteryIndicator) then
  begin
    FSize := (Source as TOBDTouchHeaderBatteryIndicator).Size;
    FVisible := (Source as TOBDTouchHeaderBatteryIndicator).Visible;
    FShowLabel := (Source as TOBDTouchHeaderBatteryIndicator).ShowLabel;
    FPercentage := (Source as TOBDTouchHeaderBatteryIndicator).Percentage;
    FBorderColor := (Source as TOBDTouchHeaderBatteryIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchHeaderBatteryIndicator).BorderWidth;
    FColor25 := (Source as TOBDTouchHeaderBatteryIndicator).Color25;
    FColor50 := (Source as TOBDTouchHeaderBatteryIndicator).Color50;
    FColor75 := (Source as TOBDTouchHeaderBatteryIndicator).Color75;
    FColor100 := (Source as TOBDTouchHeaderBatteryIndicator).Color100;
    FFont.Assign((Source as TOBDTouchHeaderBatteryIndicator).Font);
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CLASS CONSTRUCTOR
//------------------------------------------------------------------------------
class constructor TOBDTouchHeader.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TOBDTouchHeader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// CLASS DESTRUCTOR
//------------------------------------------------------------------------------
class destructor TOBDTouchHeader.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TOBDTouchHeader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBackground(Value: TOBDTouchHeaderBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBorder(Value: TOBDTouchHeaderBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BACK BUTTON
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBackButton(Value: TOBDTouchHeaderButton);
begin
  FBackButton.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ACTION BUTTON
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetActionButton(Value: TOBDTouchHeaderActionButton);
begin
  FActionButton.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetCaption(Value: TOBDTouchHeaderCaption);
begin
  FCaption.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET TABS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetTabs(Value: TOBDTouchHeaderTabCollection);
begin
  FTabs.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET TAB SETTINGS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetTab(Value: TOBDTouchHeaderTab);
begin
  FTab.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET TAB INDEX
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetTabIndex(Value: Integer);
begin
  if (FTabIndex <> Value) and (Value >= -1) and (Value < FTabs.Count) then
  begin
    FTabIndex := Value;
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BATTERY INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBatteryIndicator(Value: TOBDTouchHeaderBatteryIndicator);
begin
  FBatteryIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// WM_PAINT MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  // Retrieve the invalidated rectangle
  if not GetUpdateRect(Handle, FUpdateRect, False) then
  // If no update region, default to the entire client area
  FUpdateRect := Rect(0, 0, Width, Height);
end;

//------------------------------------------------------------------------------
// WM_ERASEBKGND MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  // Set the handled flag
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------
// WM_KILLFOCUS MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.WMKillFocus(var Message: TWMKillFocus);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mousedown
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Reset back button state to normal
  if BackButton.Visible and BackButton.Enabled and (BackButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset action button state to normal
  if ActionButton.Visible and ActionButton.Enabled and (ActionButton.ButtonState <> 0) then
  begin
    ActionButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset tab state to normal
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and (FTabs.Items[I].TabState <> 0) then
  begin
    FTabs.Items[I].TabState := 0;
    NeedRedraw := True;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// CM_ENABLEDCHANGED MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.CMEnabledChanged(var Message: TMessage);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Reset back button state to normal
  if BackButton.Visible and BackButton.Enabled and (BackButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset action button state to normal
  if ActionButton.Visible and ActionButton.Enabled and (ActionButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset tab state to normal
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and (FTabs.Items[I].TabState <> 0) then
  begin
    FTabs.Items[I].TabState := 0;
    NeedRedraw := True;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// CM_MOUSELEAVE MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.CMMouseLeave(var Message: TMessage);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Reset back button state to normal
  if BackButton.Visible and BackButton.Enabled and (BackButton.ButtonState <> 0) then
  begin
    BackButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset action button state to normal
  if ActionButton.Visible and ActionButton.Enabled and (ActionButton.ButtonState <> 0) then
  begin
    ActionButton.ButtonState := 0;
    NeedRedraw := True;
  end;

  // Reset tab state to normal
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and (FTabs.Items[I].TabState <> 0) then
  begin
    FTabs.Items[I].TabState := 0;
    NeedRedraw := True;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Adjust window style to avoid unnecessary redraws on size changes,
  // optimizing performance for custom drawing.
  with Params do Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Paint;
var
  X, Y, W, H: Integer;
begin
  // Call inherited Paint
  inherited;

  // Draw the buffer to the component canvas
  X := FUpdateRect.Left;
  Y := FUpdateRect.Top;
  W := FUpdateRect.Right - FUpdateRect.Left;
  H := FUpdateRect.Bottom - FUpdateRect.Top;

  if (W <> 0) and (H <> 0) then
    // Only update invalidated part
    BitBlt(Canvas.Handle, X, Y, W, H, FBuffer.Canvas.Handle, X,  Y, SRCCOPY)
  else
    // Repaint the whole buffer to the surface
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FBuffer.Canvas.Handle, X,  Y, SRCCOPY);
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Resize;
begin
  // Call inherited Resize
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Loaded;
begin
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
end;

//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// MOUSE DOWN HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mousedown
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Check if we are over the back button
  if BackButton.Visible and BackButton.Enabled then
  begin
    if PtInRect(BackButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to pressed
      if (BackButton.ButtonState < 2) then
      begin
        BackButton.ButtonState := 2;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (BackButton.ButtonState = 1) then
      begin
        BackButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over the action button
  if ActionButton.Visible and ActionButton.Enabled then
  begin
    if PtInRect(ActionButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to pressed
      if (ActionButton.ButtonState < 2) then
      begin
        ActionButton.ButtonState := 2;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (ActionButton.ButtonState = 1) then
      begin
        ActionButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over a tab
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and FTabs.Items[I].Enabled then
  begin
    if PtInRect(FTabs.Items[I].TabRect, Point(X, Y)) then
    begin
      // Set tab state to pressed
      if (FTabs.Items[I].TabState < 2) then
      begin
        FTabs.Items[I].TabState := 2;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset tab state to normal
      if (FTabs.Items[I].TabState =  1) then
      begin
        FTabs.Items[I].TabState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// MOUSE UP HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mouseup
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Check if we are over the back button
  if BackButton.Visible and BackButton.Enabled then
  begin
    if PtInRect(BackButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (BackButton.ButtonState = 2) then
      begin
        BackButton.ButtonState := 1;
        NeedRedraw := True;
        // If the button state is pressed, fire a click event
        if Assigned(FOnBackButtonClick) then FOnBackButtonClick(Self);
      end;
    end else
    begin
      // Reset button state to normal
      if (BackButton.ButtonState <> 0) then
      begin
        BackButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over the action button
  if ActionButton.Visible and ActionButton.Enabled then
  begin
    if PtInRect(ActionButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (ActionButton.ButtonState = 2) then
      begin
        ActionButton.ButtonState := 1;
        NeedRedraw := True;
        // If the button state is pressed, fire a click event
        if Assigned(FOnActionButtonClick) then FOnActionButtonClick(Self);
      end;
    end else
    begin
      // Reset button state to normal
      if (ActionButton.ButtonState <> 0) then
      begin
        ActionButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over a tab
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and FTabs.Items[I].Enabled then
  begin
    if PtInRect(FTabs.Items[I].TabRect, Point(X, Y)) then
    begin
      // Set tab state to hot
      if (FTabs.Items[I].TabState = 2) then
      begin
        FTabs.Items[I].TabState := 1;
        NeedRedraw := True;
        // Update the tabindex
        FTabIndex := I;
        // If the tab state is pressed, fire a click event
        if Assigned(OnTabChange) then OnTabChange(Self, I);
      end;
    end else
    begin
      // Reset tab state to normal
      if (FTabs.Items[I].TabState <> 0) then
      begin
        FTabs.Items[I].TabState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// MOUSE MOVE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
var
  NeedRedraw: Boolean;
  I: Integer;
begin
  // Call inherited mousemove
  inherited;

  // Flag to indicate we need to redraw
  NeedRedraw := False;

  // Check if we are over the back button
  if BackButton.Visible and BackButton.Enabled then
  begin
    if PtInRect(BackButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (BackButton.ButtonState = 0) then
      begin
        BackButton.ButtonState := 1;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (BackButton.ButtonState = 1) then
      begin
        BackButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over the action button
  if ActionButton.Visible and ActionButton.Enabled then
  begin
    if PtInRect(ActionButton.ButtonRect, Point(X, Y)) then
    begin
      // Set button state to hot
      if (ActionButton.ButtonState = 0) then
      begin
        ActionButton.ButtonState := 1;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset button state to normal
      if (ActionButton.ButtonState = 1) then
      begin
        ActionButton.ButtonState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // Check if we are over a tab
  for I := 0 to FTabs.Count -1 do
  if FTabs.Items[I].Visible and FTabs.Items[I].Enabled then
  begin
    if PtInRect(FTabs.Items[I].TabRect, Point(X, Y)) then
    begin
      // Set tab state to hot
      if (FTabs.Items[I].TabState = 0) then
      begin
        FTabs.Items[I].TabState := 1;
        NeedRedraw := True;
      end;
    end else
    begin
      // Reset tab state to normal
      if (FTabs.Items[I].TabState = 1) then
      begin
        FTabs.Items[I].TabState := 0;
        NeedRedraw := True;
      end;
    end;
  end;

  // If we need to redraw, then update the buffer and invalidate
  if NeedRedraw then
  begin
    PaintBuffer;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SettingsChanged(Sender: TObject);
begin
  // Reset tab index
  if (FTabIndex > FTabs.Count) then FTabIndex := FTabs.Count -1;
  if (FTabs.Count > 0) and (FTabIndex = -1) then FTabIndex := 0;
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeader.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Prevent background erasure for smoother rendering and reduced flickering.
  ControlStyle := ControlStyle + [csOpaque];
  // Create Buffer
  FBuffer := TBitmap.Create;
  // Set the buffer pixel format
  FBuffer.PixelFormat := pf32bit;
  // Create background
  FBackground := TOBDTouchHeaderBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDTouchHeaderBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create back button
  FBackButton := TOBDTouchHeaderButton.Create;
  FBackButton.OnChange := SettingsChanged;
  FBackButton.FWidth   := DEFAULT_BACK_BUTTON_WIDTH;
  FBackButton.FCaption := DEFAULT_BACK_BUTTON_CAPTION;
  // Create action button
  FActionButton := TOBDTouchHeaderActionButton.Create;
  FActionButton.OnChange := SettingsChanged;
  // Create caption
  FCaption := TOBDTouchHeaderCaption.Create;
  FCaption.OnChange := SettingsChanged;
  // Create tabs
  FTabs := TOBDTouchHeaderTabCollection.Create(Self);
  FTabs.OnChange := SettingsChanged;
  // Create tab settings
  FTab := TOBDTouchHeaderTab.Create;
  FTab.OnChange := SettingsChanged;
  // Create battery indicator
  FBatteryIndicator := TOBDTouchHeaderBatteryIndicator.Create;
  FBatteryIndicator.OnChange := SettingsChanged; 
  // Set defaults
  FTabIndex := -1;
  Height := DEFAULT_HEIGHT;
  Align := alTop;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeader.Destroy;
begin
  // Free buffer
  FBuffer.Free;
  // Free background
  FBackground.Free;
  // Free border
  FBorder.Free;
  // Free back button
  FBackButton.Free;
  // Free action button
  FActionButton.Free;
  // Free caption
  FCaption.Free;
  // Free tabs
  FTabs.Free;
  // Free tab settings
  FTab.Free;
  // Free battery indicator
  FBatteryIndicator.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// REPAINT
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Repaint;
begin
  // Call inherited repaint
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchHeader) then
  begin
    FBackground.Assign((Source as TOBDTouchHeader).Background);
    FBorder.Assign((Source as TOBDTouchHeader).Border);
    FBackButton.Assign((Source as TOBDTouchHeader).BackButton);
    FActionButton.Assign((Source as TOBDTouchHeader).ActionButton);
    FCaption.Assign((Source as TOBDTouchHeader).Caption);
    FTabs.Assign((Source as TOBDTouchHeader).Tabs);
    FTabIndex := (Source as TOBDTouchHeader).TabIndex;
    FBatteryIndicator.Assign((Source as TOBDTouchHeader).BatteryIndicator);
  end;
end;

end.
