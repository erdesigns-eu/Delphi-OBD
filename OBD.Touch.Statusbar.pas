//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Statusbar.pas
// CONTENTS       : Statusbar component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 31/03/2024
//------------------------------------------------------------------------------
unit OBD.Touch.Statusbar;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Themes, Vcl.ExtCtrls,

  OBD.CustomControl.Common;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 25;

  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = $00EBECEC;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00EFEFEF;

  /// <summary>
  ///   Default border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00BFBFBF;
  /// <summary>
  ///   Default border to color
  /// </summary>
  DEFAULT_BORDER_TO = $00CBCCCD;
  /// <summary>
  ///   Default border height
  /// </summary>
  DEFAULT_BORDER_HEIGHT = 2;

  /// <summary>
  ///   Default battery indicator size
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_SIZE = 16;
  /// <summary>
  ///   Default battery indicator border color
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default battery indicator label format
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_LABEL_FORMAT = '%.1fV';
  /// <summary>
  ///   Default battery indicator from color
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default battery indicator to color
  /// </summary>
  DEFAULT_BATTERY_INDICATOR_TO_COLOR = $00C1C1C2;

  /// <summary>
  ///   Default VCI indicator size
  /// </summary>
  DEFAULT_VCI_INDICATOR_SIZE = 16;
  /// <summary>
  ///   Default VCI indicator border color
  /// </summary>
  DEFAULT_VCI_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default VCI indicator from color
  /// </summary>
  DEFAULT_VCI_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default VCI indicator to color
  /// </summary>
  DEFAULT_VCI_INDICATOR_TO_COLOR = $00C1C1C2;
  /// <summary>
  ///   Default VCI indicator caption
  /// </summary>
  DEFAULT_VCI_INDICATOR_CAPTION = 'Not connected';

  /// <summary>
  ///   Default Internet connection indicator size
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_SIZE = 12;
  /// <summary>
  ///   Default Internet connection border color
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default Internet connection from color
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default Internet connection to color
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_TO_COLOR = $00C1C1C2;
  /// <summary>
  ///   Default Internet connection caption
  /// </summary>
  DEFAULT_INTERNET_CONNECTION_INDICATOR_CAPTION = 'No internet access';

  /// <summary>
  ///   Default protocol indicator size
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_SIZE = 16;
  /// <summary>
  ///   Default protocol border color
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_BORDER_COLOR = clBlack;
  /// <summary>
  ///   Default protocol from color
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_FROM_COLOR = $00F5F5F5;
  /// <summary>
  ///   Default protocol to color
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_TO_COLOR = $00C1C1C2;
  /// <summary>
  ///   Default protocol caption
  /// </summary>
  DEFAULT_PROTOCOL_INDICATOR_CAPTION = 'Not connected';

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Statusbar background properties
  /// </summary>
  TOBDTouchStatusbarBackground = class(TPersistent)
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
  ///   Touch Statusbar border properties
  /// </summary>
  TOBDTouchStatusbarBorder = class(TPersistent)
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
  ///   Touch Statusbar battery indicator
  /// </summary>
  TOBDTouchStatusbarBatteryIndicator = class(TPersistent)
  private
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;
    /// <summary>
    ///   Size
    /// </summary>
    FSize: Single;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Voltage
    /// </summary>
    FVoltage: Single;
    /// <summary>
    ///   Format
    /// </summary>
    FFormat: string;
    /// <summary>
    ///   From color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   To color
    /// </summary>
    FToColor: TColor;

    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
    /// <summary>
    ///   Set size
    /// </summary>
    procedure SetSize(Value: Single);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set voltage
    /// </summary>
    procedure SetVoltage(Value: Single);
    /// <summary>
    ///   Set format
    /// </summary>
    procedure SetFormat(Value: string);
    /// <summary>
    ///   Set from color
    /// </summary>
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set to color
    /// </summary>
    procedure SetToColor(Value: TColor);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed handler
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
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_BATTERY_INDICATOR_BORDER_COLOR;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    /// <summary>
    ///   Size
    /// </summary>
    property Size: Single read FSize write SetSize;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Voltage
    /// </summary>
    property Voltage: Single read FVoltage write SetVoltage;
    /// <summary>
    ///   Format
    /// </summary>
    property Format: string read FFormat write SetFormat;
    /// <summary>
    ///   From color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BATTERY_INDICATOR_FROM_COLOR;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BATTERY_INDICATOR_TO_COLOR;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar VCI (Vehicle Connection Interface) indicator
  /// </summary>
  TOBDTouchStatusbarVciIndicator = class(TPersistent)
  private
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;
    /// <summary>
    ///   Size
    /// </summary>
    FSize: Single;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   From color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   To color
    /// </summary>
    FToColor: TColor;

    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
    /// <summary>
    ///   Set size
    /// </summary>
    procedure SetSize(Value: Single);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set from color
    /// </summary>
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set to color
    /// </summary>
    procedure SetToColor(Value: TColor);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed handler
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
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_VCI_INDICATOR_BORDER_COLOR;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    /// <summary>
    ///   Size
    /// </summary>
    property Size: Single read FSize write SetSize;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   From color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_VCI_INDICATOR_FROM_COLOR;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_VCI_INDICATOR_TO_COLOR;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar Internet connection indicator
  /// </summary>
  TOBDTouchStatusbarInternetConnectionIndicator = class(TPersistent)
  private
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;
    /// <summary>
    ///   Size
    /// </summary>
    FSize: Single;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   From color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   To color
    /// </summary>
    FToColor: TColor;

    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
    /// <summary>
    ///   Set size
    /// </summary>
    procedure SetSize(Value: Single);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set from color
    /// </summary>
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set to color
    /// </summary>
    procedure SetToColor(Value: TColor);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed handler
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
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_INTERNET_CONNECTION_INDICATOR_BORDER_COLOR;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    /// <summary>
    ///   Size
    /// </summary>
    property Size: Single read FSize write SetSize;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   From color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_INTERNET_CONNECTION_INDICATOR_FROM_COLOR;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_INTERNET_CONNECTION_INDICATOR_TO_COLOR;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar Protocol indicator
  /// </summary>
  TOBDTouchStatusbarProtocolIndicator = class(TPersistent)
  private
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;
    /// <summary>
    ///   Size
    /// </summary>
    FSize: Single;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   From color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   To color
    /// </summary>
    FToColor: TColor;

    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
    /// <summary>
    ///   Set size
    /// </summary>
    procedure SetSize(Value: Single);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set from color
    /// </summary>
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set to color
    /// </summary>
    procedure SetToColor(Value: TColor);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed handler
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
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_PROTOCOL_INDICATOR_BORDER_COLOR;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    /// <summary>
    ///   Size
    /// </summary>
    property Size: Single read FSize write SetSize;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   From color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_PROTOCOL_INDICATOR_FROM_COLOR;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_PROTOCOL_INDICATOR_TO_COLOR;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar Component
  /// </summary>
  TOBDTouchStatusbar = class(TCustomControl)
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
    FBackground: TOBDTouchStatusbarBackground;
    /// <summary>
    ///   Border
    /// </summary>
    FBorder: TOBDTouchStatusbarBorder;
    /// <summary>
    ///   Battery indicator
    /// </summary>
    FBatteryIndicator: TOBDTouchStatusbarBatteryIndicator;
    /// <summary>
    ///   VCI (Vehicle Connection Interface) indicator
    /// </summary>
    FVciIndicator: TOBDTouchStatusbarVciIndicator;
    /// <summary>
    ///   Internet connection indicator
    /// </summary>
    FInternetConnectionIndicator: TOBDTouchStatusbarInternetConnectionIndicator;
    /// <summary>
    ///   Protocol indicator
    /// </summary>
    FProtocolIndicator: TOBDTouchStatusbarProtocolIndicator;

    /// <summary>
    ///   Set background
    /// </summary>
    procedure SetBackground(Value: TOBDTouchStatusbarBackground);
    /// <summary>
    ///   Set border
    /// </summary>
    procedure SetBorder(Value: TOBDTouchStatusbarBorder);
    /// <summary>
    ///   Set battery indicator
    /// </summary>
    procedure SetBatteryIndicator(Value: TOBDTouchStatusbarBatteryIndicator);
    /// <summary>
    ///   Set VCI indicator
    /// </summary>
    procedure SetVciIndicator(Value: TOBDTouchStatusbarVciIndicator);
    /// <summary>
    ///   Set internet connection indicator
    /// </summary>
    procedure SetInternetConnectionIndicator(Value: TOBDTouchStatusbarInternetConnectionIndicator);
    /// <summary>
    ///   Set protocol indicator
    /// </summary>
    procedure SetProtocolIndicator(Value: TOBDTouchStatusbarProtocolIndicator);
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
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Background
    /// </summary>
    property Background: TOBDTouchStatusbarBackground read FBackground write SetBackground;
    /// <summary>
    ///   Border
    /// </summary>
    property Border: TOBDTouchStatusbarBorder read FBorder write SetBorder;
    /// <summary>
    ///   Battery indicator
    /// </summary>
    property BatteryIndicator: TOBDTouchStatusbarBatteryIndicator read FBatteryIndicator write SetBatteryIndicator;
    /// <summary>
    ///   VCI (Vehicle Connection Interface) indicator
    /// </summary>
    property VciIndicator: TOBDTouchStatusbarVciIndicator read FVciIndicator write SetVciIndicator;
    /// <summary>
    ///   Internet connection indicator
    /// </summary>
    property InternetConnectionIndicator: TOBDTouchStatusbarInternetConnectionIndicator read FInternetConnectionIndicator write SetInternetConnectionIndicator;
    /// <summary>
    ///   Protocol indicator
    /// </summary>
    property ProtocolIndicator: TOBDTouchStatusbarProtocolIndicator read FProtocolIndicator write SetProtocolIndicator;
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
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBackground.SetFromColor(Value: TColor);
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
procedure TOBDTouchStatusbarBackground.SetToColor(Value: TColor);
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
constructor TOBDTouchStatusbarBackground.Create;
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
procedure TOBDTouchStatusbarBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarBackground) then
  begin
    FFromColor := (Source as TOBDTouchStatusbarBackground).FromColor;
    FToColor := (Source as TOBDTouchStatusbarBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBorder.SetFromColor(Value: TColor);
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
procedure TOBDTouchStatusbarBorder.SetToColor(Value: TColor);
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
procedure TOBDTouchStatusbarBorder.SetHeight(Value: Integer);
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
constructor TOBDTouchStatusbarBorder.Create;
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
procedure TOBDTouchStatusbarBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarBorder) then
  begin
    FFromColor := (Source as TOBDTouchStatusbarBorder).FromColor;
    FToColor := (Source as TOBDTouchStatusbarBorder).ToColor;
    FHeight := (Source as TOBDTouchStatusbarBorder).Height;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchStatusbarBatteryIndicator.SetBorderWidth(Value: Single);
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
// SET SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetSize(Value: Single);
begin
  if (FSize <> Value) then
  begin
    // Set new size
    FSize := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET VOLTAGE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetVoltage(Value: Single);
begin
  if (FVoltage <> Value) and (Value >= 0) and (Value <= 100) then
  begin
    // Set new voltage
    FVoltage := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FORMAT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetFormat(Value: string);
begin
  if (FFormat <> Value) then
  begin
    // Set new format
    FFormat := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set new from color
    FFromColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set new to color
    FToColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarBatteryIndicator.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Set defaults
  FBorderColor := DEFAULT_BATTERY_INDICATOR_BORDER_COLOR;
  FBorderWidth := 1;
  FSize := DEFAULT_BATTERY_INDICATOR_SIZE;
  FVisible := True;
  FVoltage := 0;
  FFormat := DEFAULT_BATTERY_INDICATOR_LABEL_FORMAT;
  FFromColor := DEFAULT_BATTERY_INDICATOR_FROM_COLOR;
  FToColor := DEFAULT_BATTERY_INDICATOR_TO_COLOR;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchStatusbarBatteryIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBatteryIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarBatteryIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchStatusbarBatteryIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchStatusbarBatteryIndicator).BorderWidth;
    FSize := (Source as TOBDTouchStatusbarBatteryIndicator).Size;
    FVisible := (Source as TOBDTouchStatusbarBatteryIndicator).Visible;
    FVoltage := (Source as TOBDTouchStatusbarBatteryIndicator).Voltage;
    FFormat := (Source as TOBDTouchStatusbarBatteryIndicator).Format;
    FFromColor := (Source as TOBDTouchStatusbarBatteryIndicator).FromColor;
    FToColor := (Source as TOBDTouchStatusbarBatteryIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchStatusbarVciIndicator.SetBorderWidth(Value: Single);
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
// SET SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SetSize(Value: Single);
begin
  if (FSize <> Value) then
  begin
    // Set new size
    FSize := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FORMAT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SetCaption(Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    // Set new caption
    FCaption := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set new from color
    FFromColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set new to color
    FToColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarVciIndicator.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Set defaults
  FBorderColor := DEFAULT_VCI_INDICATOR_BORDER_COLOR;
  FBorderWidth := 1;
  FSize := DEFAULT_VCI_INDICATOR_SIZE;
  FVisible := True;
  FFromColor := DEFAULT_VCI_INDICATOR_FROM_COLOR;
  FToColor := DEFAULT_VCI_INDICATOR_TO_COLOR;
  FCaption := DEFAULT_VCI_INDICATOR_CAPTION;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchStatusbarVciIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarVciIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarVciIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchStatusbarVciIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchStatusbarVciIndicator).BorderWidth;
    FSize := (Source as TOBDTouchStatusbarVciIndicator).Size;
    FVisible := (Source as TOBDTouchStatusbarVciIndicator).Visible;
    FCaption:= (Source as TOBDTouchStatusbarVciIndicator).Caption;
    FFromColor := (Source as TOBDTouchStatusbarVciIndicator).FromColor;
    FToColor := (Source as TOBDTouchStatusbarVciIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetBorderWidth(Value: Single);
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
// SET SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetSize(Value: Single);
begin
  if (FSize <> Value) then
  begin
    // Set new size
    FSize := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FORMAT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetCaption(Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    // Set new caption
    FCaption := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set new from color
    FFromColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set new to color
    FToColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarInternetConnectionIndicator.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Set defaults
  FBorderColor := DEFAULT_INTERNET_CONNECTION_INDICATOR_BORDER_COLOR;
  FBorderWidth := 1;
  FSize := DEFAULT_INTERNET_CONNECTION_INDICATOR_SIZE;
  FVisible := True;
  FFromColor := DEFAULT_INTERNET_CONNECTION_INDICATOR_FROM_COLOR;
  FToColor := DEFAULT_INTERNET_CONNECTION_INDICATOR_TO_COLOR;
  FCaption := DEFAULT_INTERNET_CONNECTION_INDICATOR_CAPTION;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchStatusbarInternetConnectionIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarInternetConnectionIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarInternetConnectionIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchStatusbarInternetConnectionIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchStatusbarInternetConnectionIndicator).BorderWidth;
    FSize := (Source as TOBDTouchStatusbarInternetConnectionIndicator).Size;
    FVisible := (Source as TOBDTouchStatusbarInternetConnectionIndicator).Visible;
    FCaption:= (Source as TOBDTouchStatusbarInternetConnectionIndicator).Caption;
    FFromColor := (Source as TOBDTouchStatusbarInternetConnectionIndicator).FromColor;
    FToColor := (Source as TOBDTouchStatusbarInternetConnectionIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchStatusbarProtocolIndicator.SetBorderWidth(Value: Single);
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
// SET SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SetSize(Value: Single);
begin
  if (FSize <> Value) then
  begin
    // Set new size
    FSize := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FORMAT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SetCaption(Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    // Set new caption
    FCaption := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set new from color
    FFromColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set new to color
    FToColor := Value;
    // Notify change
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarProtocolIndicator.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Set defaults
  FBorderColor := DEFAULT_PROTOCOL_INDICATOR_BORDER_COLOR;
  FBorderWidth := 1;
  FSize := DEFAULT_PROTOCOL_INDICATOR_SIZE;
  FVisible := True;
  FFromColor := DEFAULT_PROTOCOL_INDICATOR_FROM_COLOR;
  FToColor := DEFAULT_PROTOCOL_INDICATOR_TO_COLOR;
  FCaption := DEFAULT_PROTOCOL_INDICATOR_CAPTION;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchStatusbarProtocolIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarProtocolIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarProtocolIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchStatusbarProtocolIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchStatusbarProtocolIndicator).BorderWidth;
    FSize := (Source as TOBDTouchStatusbarProtocolIndicator).Size;
    FVisible := (Source as TOBDTouchStatusbarProtocolIndicator).Visible;
    FCaption:= (Source as TOBDTouchStatusbarProtocolIndicator).Caption;
    FFromColor := (Source as TOBDTouchStatusbarProtocolIndicator).FromColor;
    FToColor := (Source as TOBDTouchStatusbarProtocolIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CLASS CONSTRUCTOR
//------------------------------------------------------------------------------
class constructor TOBDTouchStatusbar.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TOBDTouchStatusbar, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// CLASS DESTRUCTOR
//------------------------------------------------------------------------------
class destructor TOBDTouchStatusbar.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TOBDTouchStatusbar, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetBackground(Value: TOBDTouchStatusbarBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetBorder(Value: TOBDTouchStatusbarBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BATTERY INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetBatteryIndicator(Value: TOBDTouchStatusbarBatteryIndicator);
begin
  FBatteryIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET VCI INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetVciIndicator(Value: TOBDTouchStatusbarVciIndicator);
begin
  FVciIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET INTERNET CONNECTION INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetInternetConnectionIndicator(Value: TOBDTouchStatusbarInternetConnectionIndicator);
begin
  FInternetConnectionIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PROTOCOL INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetProtocolIndicator(Value: TOBDTouchStatusbarProtocolIndicator);
begin
  FProtocolIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// WM_PAINT MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WMPaint(var Msg: TWMPaint);
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
procedure TOBDTouchStatusbar.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  // Set the handled flag
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------
// WM_KILLFOCUS MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
end;

//------------------------------------------------------------------------------
// CM_ENABLEDCHANGED MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------
// CM_MOUSELEAVE MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Adjust window style to avoid unnecessary redraws on size changes,
  // optimizing performance for custom drawing.
  with Params do Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Paint;
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
procedure TOBDTouchStatusbar.Resize;
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
procedure TOBDTouchStatusbar.Loaded;
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
procedure TOBDTouchStatusbar.UpdateStyleElements;
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
procedure TOBDTouchStatusbar.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mousedown
  inherited;
end;

//------------------------------------------------------------------------------
// MOUSE UP HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mouseup
  inherited;
end;

//------------------------------------------------------------------------------
// MOUSE MOVE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mousemove
  inherited;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SettingsChanged(Sender: TObject);
begin
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.PaintBuffer;
var
  SS: TCustomStyleServices;
  Graphics: TGPGraphics;
  BackgroundRect, BorderRect: TGPRect;
  MeasureRect, CaptionRect, BatteryRect, VCIRect, InternetRect, ProtocolRect: TGPRectF;
  Brush: TGPBrush;
  Pen: TGPPen;
  BatteryPath, VciPath, InternetPath, ProtocolPath: TGPGraphicsPath;

  Font: TGPFont;
  FontBrush: TGPSolidBrush;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;

  X, Y, Z: Single;
  S: string;
begin
  // Update the size of the buffer
  Buffer.SetSize(Width, Height);

  // If VCL styles is available and enabled, then draw the VCL Style background
  // so it matches the active style background like on the Form or a Panel.
  if TStyleManager.IsCustomStyleActive then
  begin
    SS := StyleServices;
    // Draw the styled background
    SS.DrawElement(Buffer.Canvas.Handle, SS.GetElementDetails(twWindowRoot), Rect(0, 0, Width, Height));
  end else
  // Otherwise fill the background with the color.
  with Buffer.Canvas do
  begin
    // Use the component color
    Brush.Color := Self.Color;
    // Use a solid brush
    Brush.Style := bsSolid;
    // Fill the background with the component color
    FillRect(Rect(0, 0, Width, Height));
  end;

  // Initialize GDI+ Graphics object
  Graphics := TGPGraphics.Create(Buffer.Canvas.Handle);
  try
    // Set smoothing mode to high-quality
    Graphics.SetSmoothingMode(SmoothingModeHighQuality);
    // Set compositing quality to high-quality
    Graphics.SetCompositingQuality(CompositingQualityHighQuality);

    // Draw the backround
    if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
    begin
      // Get the rectangle for the background
      BackgroundRect := MakeRect(0, 0, Width, Height);
      // Create the background brush
      Brush := TGPLinearGradientBrush.Create(BackgroundRect, SafeColorRefToARGB(Background.FromColor), SafeColorRefToARGB(Background.ToColor), LinearGradientModeVertical);
      try
        // Fill the background
        Graphics.FillRectangle(Brush, BackgroundRect);
      finally
        // Free the background brush object
        Brush.Free;
      end;
    end;

    // Draw the border
    if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
    begin
      // Get the rectangle for the border
      BorderRect := MakeRect(0, Height - Border.Height, Width, Border.Height);
      // Create the border brush
      Brush := TGPLinearGradientBrush.Create(BackgroundRect, SafeColorRefToARGB(Border.FromColor), SafeColorRefToARGB(Border.ToColor), LinearGradientModeVertical);
      try
        // Fill the border
        Graphics.FillRectangle(Brush, BorderRect);
      finally
        // Free the border brush object
        Brush.Free;
      end;
    end;

    // Set the X position
    X := Width - 8;

    // Draw the battery indicator
    if BatteryIndicator.Visible then
    begin
      Y := ((Height - Border.Height) / 2) - (BatteryIndicator.Size / 2);

      // Create font for the label
      FontFamily := TGPFontFamily.Create(BatteryIndicator.Font.Name);
      Font := TGPFont.Create(FontFamily, BatteryIndicator.Font.Size, OBD.CustomControl.Common.FontStyle(BatteryIndicator.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(BatteryIndicator.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetFormatFlags(StringFormatFlagsNoWrap);
      StringFormat.SetAlignment(StringAlignmentFar);
      StringFormat.SetLineAlignment(StringAlignmentCenter);

      // Format the label
      S := Format(BatteryIndicator.Format, [BatteryIndicator.Voltage]);
      // Set the measure rect
      MeasureRect := MakeRect(0, 0, Width - 8.0, Height - Border.Height);
      // First measure the needed space for the label
      if Graphics.MeasureString(S, Length(S), Font, MeasureRect, StringFormat, CaptionRect) = Ok then
      // Draw the label
      try
        Graphics.DrawString(S, Length(S), Font, CaptionRect, StringFormat, FontBrush);
        X := X - (CaptionRect.Width + 4);
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;

      // Create battery indicator rect
      BatteryRect := MakeRect(X - BatteryIndicator.Size, Y, BatteryIndicator.Size, BatteryIndicator.Size);
      // Create battery indicator path
      BatteryPath := CreateVehicleBatteryPath(BatteryRect);
      // Create pen
      Pen := TGPPen.Create(SafeColorRefToARGB(BatteryIndicator.BorderColor), BatteryIndicator.BorderWidth);
      Pen.SetAlignment(PenAlignmentInset);
      // Create brush
      Brush := TGPLinearGradientBrush.Create(BatteryRect, SafeColorRefToARGB(BatteryIndicator.FromColor), SafeColorRefToARGB(BatteryIndicator.ToColor), LinearGradientModeVertical);
      try
        if (BatteryIndicator.FromColor <> clNone) and (BatteryIndicator.ToColor <> clNone) then
          Graphics.FillPath(Brush, BatteryPath);
        Graphics.DrawPath(Pen, BatteryPath);
      finally
        BatteryPath.Free;
        Pen.Free;
        Brush.Free;
      end;

      // Update the x position
      X := X - (BatteryIndicator.Size + 4);
    end;

    // Draw the VCI indicator
    if VciIndicator.Visible then
    begin
      // Update the Y position
      Y := ((Height - Border.Height) / 2) - (VciIndicator.Size / 2);

      // Create font for the label
      FontFamily := TGPFontFamily.Create(VciIndicator.Font.Name);
      Font := TGPFont.Create(FontFamily, VciIndicator.Font.Size, OBD.CustomControl.Common.FontStyle(VciIndicator.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(VciIndicator.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetFormatFlags(StringFormatFlagsNoWrap);
      StringFormat.SetAlignment(StringAlignmentFar);
      StringFormat.SetLineAlignment(StringAlignmentCenter);

      // Format the label
      S := VciIndicator.Caption;
      // Set the measure rect
      MeasureRect := MakeRect(0, 0.0, X, Height - Border.Height);
      // First measure the needed space for the label
      if Graphics.MeasureString(S, Length(S), Font, MeasureRect, StringFormat, CaptionRect) = Ok then
      // Draw the label
      try
        Graphics.DrawString(S, Length(S), Font, CaptionRect, StringFormat, FontBrush);
        X := X - (CaptionRect.Width + 2);
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;

      // Create vci indicator rect
      VciRect := MakeRect(X - VciIndicator.Size, Y, VciIndicator.Size, VciIndicator.Size);
      // Create vci indicator path
      VciPath := CreateJ1962Path(VciRect);
      // Create pen
      Pen := TGPPen.Create(SafeColorRefToARGB(VciIndicator.BorderColor), VciIndicator.BorderWidth);
      Pen.SetAlignment(PenAlignmentInset);
      // Create brush
      Brush := TGPLinearGradientBrush.Create(VciRect, SafeColorRefToARGB(VciIndicator.FromColor), SafeColorRefToARGB(VciIndicator.ToColor), LinearGradientModeVertical);
      try
        if (VciIndicator.FromColor <> clNone) and (VciIndicator.ToColor <> clNone) then
          Graphics.FillPath(Brush, VciPath);
        Graphics.DrawPath(Pen, VciPath);
      finally
        VciPath.Free;
        Pen.Free;
        Brush.Free;
      end;

      // Update the x position
      X := X - (VciIndicator.Size + 4);
    end;

    // Draw the internet connection indicator
    if InternetConnectionIndicator.Visible then
    begin
      // Update the Y position
      Y := ((Height - Border.Height) / 2) - (InternetConnectionIndicator.Size / 2);

      // Create font for the label
      FontFamily := TGPFontFamily.Create(InternetConnectionIndicator.Font.Name);
      Font := TGPFont.Create(FontFamily, InternetConnectionIndicator.Font.Size, OBD.CustomControl.Common.FontStyle(InternetConnectionIndicator.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(InternetConnectionIndicator.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetFormatFlags(StringFormatFlagsNoWrap);
      StringFormat.SetAlignment(StringAlignmentFar);
      StringFormat.SetLineAlignment(StringAlignmentCenter);

      // Format the label
      S := InternetConnectionIndicator.Caption;
      // Set the measure rect
      MeasureRect := MakeRect(0, 0.0, X, Height - Border.Height);
      // First measure the needed space for the label
      if Graphics.MeasureString(S, Length(S), Font, MeasureRect, StringFormat, CaptionRect) = Ok then
      // Draw the label
      try
        Graphics.DrawString(S, Length(S), Font, CaptionRect, StringFormat, FontBrush);
        X := X - CaptionRect.Width;
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;

      // Create internet connection indicator rect
      InternetRect := MakeRect(X - InternetConnectionIndicator.Size, Y, InternetConnectionIndicator.Size, InternetConnectionIndicator.Size);
      // Create internet connection indicator path
      InternetPath := CreateInternetGlobePath(InternetRect);
      // Create pen
      Pen := TGPPen.Create(SafeColorRefToARGB(InternetConnectionIndicator.BorderColor), InternetConnectionIndicator.BorderWidth);
      Pen.SetAlignment(PenAlignmentInset);
      Brush := TGPLinearGradientBrush.Create(InternetRect, SafeColorRefToARGB(InternetConnectionIndicator.FromColor), SafeColorRefToARGB(InternetConnectionIndicator.ToColor), LinearGradientModeVertical);
      try
        if (InternetConnectionIndicator.FromColor <> clNone) and (InternetConnectionIndicator.ToColor <> clNone) then
          Graphics.FillEllipse(Brush, InternetRect);
        Graphics.DrawPath(Pen, InternetPath);
      finally
        InternetPath.Free;
        Pen.Free;
        Brush.Free;
      end;

      X := X - (InternetConnectionIndicator.Size + 4);
    end;

    // Draw the protocol indicator
    if ProtocolIndicator.Visible then
    begin
      // Get the available width
      Z := X;
      // Update the Y position
      Y := ((Height - Border.Height) / 2) - (ProtocolIndicator.Size / 2);
      X := 8;

      // Create protocol indicator rect
      ProtocolRect := MakeRect(8, Y, ProtocolIndicator.Size, ProtocolIndicator.Size);
      // Create protocol indicator path
      ProtocolPath := CreateProtocolPath(ProtocolRect);
      // Create pen
      Pen := TGPPen.Create(SafeColorRefToARGB(ProtocolIndicator.BorderColor), ProtocolIndicator.BorderWidth);
      Pen.SetAlignment(PenAlignmentInset);
      Brush := TGPLinearGradientBrush.Create(ProtocolRect, SafeColorRefToARGB(ProtocolIndicator.FromColor), SafeColorRefToARGB(ProtocolIndicator.ToColor), LinearGradientModeVertical);
      try
        if (ProtocolIndicator.FromColor <> clNone) and (ProtocolIndicator.ToColor <> clNone) then
          Graphics.FillPath(Brush, ProtocolPath);
        Graphics.DrawPath(Pen, ProtocolPath);
      finally
        ProtocolPath.Free;
        Pen.Free;
        Brush.Free;
      end;

      // Create font for the label
      FontFamily := TGPFontFamily.Create(ProtocolIndicator.Font.Name);
      Font := TGPFont.Create(FontFamily, ProtocolIndicator.Font.Size, OBD.CustomControl.Common.FontStyle(ProtocolIndicator.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(ProtocolIndicator.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetAlignment(StringAlignmentNear);
      StringFormat.SetLineAlignment(StringAlignmentCenter);
      StringFormat.SetFormatFlags(StringFormatFlagsNoWrap);
      StringFormat.SetTrimming(StringTrimmingEllipsisCharacter);

      // Format the label
      S := ProtocolIndicator.Caption;
      // Set the measure rect
      MeasureRect := MakeRect(12 + ProtocolIndicator.Size, 0.0, Z - (12 + ProtocolIndicator.Size), Height - Border.Height);
      // First measure the needed space for the label
      if Graphics.MeasureString(S, Length(S), Font, MeasureRect, StringFormat, CaptionRect) = Ok then
      // Draw the label
      try
        Graphics.DrawString(S, Length(S), Font, CaptionRect, StringFormat, FontBrush);
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
      end;
    end;


  finally
    // Free GDI+ graphics object
    Graphics.Free;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbar.Create(AOwner: TComponent);
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
  FBackground := TOBDTouchStatusbarBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDTouchStatusbarBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create battery indicator
  FBatteryIndicator := TOBDTouchStatusbarBatteryIndicator.Create;
  FBatteryIndicator.OnChange := SettingsChanged;
  // Create vci indicator
  FVciIndicator := TOBDTouchStatusbarVciIndicator.Create;
  FVciIndicator.OnChange := SettingsChanged;
  // Create internet connection indicator
  FInternetConnectionIndicator := TOBDTouchStatusbarInternetConnectionIndicator.Create;
  FInternetConnectionIndicator.OnChange := SettingsChanged;
  // Create protocol indicator
  FProtocolIndicator := TOBDTouchStatusbarProtocolIndicator.Create;
  FProtocolIndicator.OnChange := SettingsChanged;
  // Set defaults
  Height := DEFAULT_HEIGHT;
  Align := alTop;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchStatusbar.Destroy;
begin
  // Free buffer
  FBuffer.Free;
  // Free background
  FBackground.Free;
  // Free border
  FBorder.Free;
  // Free battery indicator
  FBatteryIndicator.Free;
  // Free vci indicator
  FVciIndicator.Free;
  // Free internet connection indicator
  FInternetConnectionIndicator.Free;
  // Free protocol indicator
  FProtocolIndicator.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchStatusbar) then
  begin
    FBackground.Assign((Source as TOBDTouchStatusbar).Background);
    FBorder.Assign((Source as TOBDTouchStatusbar).Border);
    FBatteryIndicator.Assign((Source as TOBDTouchStatusbar).BatteryIndicator);
    FVciIndicator.Assign((Source as TOBDTouchStatusbar).VciIndicator);
    FInternetConnectionIndicator.Assign((Source as TOBDTouchStatusbar).InternetConnectionIndicator);
    FProtocolIndicator.Assign((Source as TOBDTouchStatusbar).ProtocolIndicator);
  end;
end;

end.
