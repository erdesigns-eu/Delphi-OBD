//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Subheader.pas
// CONTENTS       : Subheader component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 01/04/2024
//------------------------------------------------------------------------------
unit OBD.Touch.Subheader;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Math, System.Skia, Vcl.Controls,
  WinApi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  Vcl.Themes, Vcl.ExtCtrls,

  OBD.Connection.Component, OBD.Connection.Types,
  OBD.CustomControl.Helpers, OBD.CustomControl.Constants;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 25;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Subheader background properties
  /// </summary>
  TOBDTouchSubheaderBackground = class(TPersistent)
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
  ///   Touch Subheader border properties
  /// </summary>
  TOBDTouchSubheaderBorder = class(TPersistent)
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
  ///   Touch Subheader battery indicator
  /// </summary>
  TOBDTouchSubheaderBatteryIndicator = class(TPersistent)
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
  ///   Touch Subheader VCI (Vehicle Connection Interface) indicator
  /// </summary>
  TOBDTouchSubheaderVciIndicator = class(TPersistent)
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
  ///   Touch Subheader Internet connection indicator
  /// </summary>
  TOBDTouchSubheaderInternetConnectionIndicator = class(TPersistent)
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
  ///   Touch Subheader Protocol indicator
  /// </summary>
  TOBDTouchSubheaderProtocolIndicator = class(TPersistent)
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
  ///   Touch Subheader Component
  /// </summary>
  TOBDTouchSubheader = class(TCustomControl)
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
    FBackground: TOBDTouchSubheaderBackground;
    /// <summary>
    ///   Border
    /// </summary>
    FBorder: TOBDTouchSubheaderBorder;
    /// <summary>
    ///   Battery indicator
    /// </summary>
    FBatteryIndicator: TOBDTouchSubheaderBatteryIndicator;
    /// <summary>
    ///   VCI (Vehicle Connection Interface) indicator
    /// </summary>
    FVciIndicator: TOBDTouchSubheaderVciIndicator;
    /// <summary>
    ///   Internet connection indicator
    /// </summary>
    FInternetConnectionIndicator: TOBDTouchSubheaderInternetConnectionIndicator;
    /// <summary>
    ///   Protocol indicator
    /// </summary>
    FProtocolIndicator: TOBDTouchSubheaderProtocolIndicator;
    /// <summary>
    ///   Connection component used to synchronize indicator captions.
    /// </summary>
    FConnectionComponent: TOBDConnectionComponent;
    /// <summary>
    ///   Indicates whether indicator captions should be updated automatically from the connection component.
    /// </summary>
    FAutoApplyConnectionDetails: Boolean;

    /// <summary>
    ///   Set background
    /// </summary>
    procedure SetBackground(Value: TOBDTouchSubheaderBackground);
    /// <summary>
    ///   Set border
    /// </summary>
    procedure SetBorder(Value: TOBDTouchSubheaderBorder);
    /// <summary>
    ///   Set battery indicator
    /// </summary>
    procedure SetBatteryIndicator(Value: TOBDTouchSubheaderBatteryIndicator);
    /// <summary>
    ///   Set VCI indicator
    /// </summary>
    procedure SetVciIndicator(Value: TOBDTouchSubheaderVciIndicator);
    /// <summary>
    ///   Set internet connection indicator
    /// </summary>
    procedure SetInternetConnectionIndicator(Value: TOBDTouchSubheaderInternetConnectionIndicator);
    /// <summary>
    ///   Set protocol indicator
    /// </summary>
    procedure SetProtocolIndicator(Value: TOBDTouchSubheaderProtocolIndicator);
    /// <summary>
    ///   Set the connection component used for automatic indicator updates.
    /// </summary>
    procedure SetConnectionComponent(const Value: TOBDConnectionComponent);
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
    ///   Handle connection state changes and queue UI-safe indicator updates.
    /// </summary>
    procedure HandleConnectionStateChanged(Sender: TObject; const Connected: Boolean; const ConnectionType: TOBDConnectionType);
    /// <summary>
    ///   Apply connection captions to indicators on the UI thread.
    /// </summary>
    procedure ApplyConnectionDetails(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
    /// <summary>
    ///   Build a friendly caption for the current connection target.
    /// </summary>
    function BuildConnectionCaption(const ConnectionType: TOBDConnectionType): string;
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
    property Background: TOBDTouchSubheaderBackground read FBackground write SetBackground;
    /// <summary>
    ///   Border
    /// </summary>
    property Border: TOBDTouchSubheaderBorder read FBorder write SetBorder;
    /// <summary>
    ///   Battery indicator
    /// </summary>
    property BatteryIndicator: TOBDTouchSubheaderBatteryIndicator read FBatteryIndicator write SetBatteryIndicator;
    /// <summary>
    ///   VCI (Vehicle Connection Interface) indicator
    /// </summary>
    property VciIndicator: TOBDTouchSubheaderVciIndicator read FVciIndicator write SetVciIndicator;
    /// <summary>
    ///   Internet connection indicator
    /// </summary>
    property InternetConnectionIndicator: TOBDTouchSubheaderInternetConnectionIndicator read FInternetConnectionIndicator write SetInternetConnectionIndicator;
    /// <summary>
    ///   Protocol indicator
    /// </summary>
    property ProtocolIndicator: TOBDTouchSubheaderProtocolIndicator read FProtocolIndicator write SetProtocolIndicator;
    /// <summary>
    ///   Connection component used to automatically reflect connection details on the indicator captions.
    /// </summary>
    property ConnectionComponent: TOBDConnectionComponent read FConnectionComponent write SetConnectionComponent;
    /// <summary>
    ///   Enables or disables automatic indicator updates when the connection component changes state.
    /// </summary>
    property AutoApplyConnectionDetails: Boolean read FAutoApplyConnectionDetails write FAutoApplyConnectionDetails default True;
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
  System.Math;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBackground.SetFromColor(Value: TColor);
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
procedure TOBDTouchSubheaderBackground.SetToColor(Value: TColor);
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
constructor TOBDTouchSubheaderBackground.Create;
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
procedure TOBDTouchSubheaderBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchSubheaderBackground) then
  begin
    FFromColor := (Source as TOBDTouchSubheaderBackground).FromColor;
    FToColor := (Source as TOBDTouchSubheaderBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBorder.SetFromColor(Value: TColor);
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
procedure TOBDTouchSubheaderBorder.SetToColor(Value: TColor);
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
procedure TOBDTouchSubheaderBorder.SetHeight(Value: Integer);
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
constructor TOBDTouchSubheaderBorder.Create;
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
procedure TOBDTouchSubheaderBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchSubheaderBorder) then
  begin
    FFromColor := (Source as TOBDTouchSubheaderBorder).FromColor;
    FToColor := (Source as TOBDTouchSubheaderBorder).ToColor;
    FHeight := (Source as TOBDTouchSubheaderBorder).Height;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBatteryIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchSubheaderBatteryIndicator.SetBorderWidth(Value: Single);
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
procedure TOBDTouchSubheaderBatteryIndicator.SetSize(Value: Single);
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
procedure TOBDTouchSubheaderBatteryIndicator.SetVisible(Value: Boolean);
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
procedure TOBDTouchSubheaderBatteryIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET VOLTAGE
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBatteryIndicator.SetVoltage(Value: Single);
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
procedure TOBDTouchSubheaderBatteryIndicator.SetFormat(Value: string);
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
procedure TOBDTouchSubheaderBatteryIndicator.SetFromColor(Value: TColor);
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
procedure TOBDTouchSubheaderBatteryIndicator.SetToColor(Value: TColor);
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
procedure TOBDTouchSubheaderBatteryIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderBatteryIndicator.Create;
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
destructor TOBDTouchSubheaderBatteryIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBatteryIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchSubheaderBatteryIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchSubheaderBatteryIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchSubheaderBatteryIndicator).BorderWidth;
    FSize := (Source as TOBDTouchSubheaderBatteryIndicator).Size;
    FVisible := (Source as TOBDTouchSubheaderBatteryIndicator).Visible;
    FVoltage := (Source as TOBDTouchSubheaderBatteryIndicator).Voltage;
    FFormat := (Source as TOBDTouchSubheaderBatteryIndicator).Format;
    FFromColor := (Source as TOBDTouchSubheaderBatteryIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderBatteryIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderVciIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchSubheaderVciIndicator.SetBorderWidth(Value: Single);
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
procedure TOBDTouchSubheaderVciIndicator.SetSize(Value: Single);
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
procedure TOBDTouchSubheaderVciIndicator.SetVisible(Value: Boolean);
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
procedure TOBDTouchSubheaderVciIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FORMAT
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderVciIndicator.SetCaption(Value: TCaption);
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
procedure TOBDTouchSubheaderVciIndicator.SetFromColor(Value: TColor);
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
procedure TOBDTouchSubheaderVciIndicator.SetToColor(Value: TColor);
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
procedure TOBDTouchSubheaderVciIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderVciIndicator.Create;
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
destructor TOBDTouchSubheaderVciIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderVciIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchSubheaderVciIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchSubheaderVciIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchSubheaderVciIndicator).BorderWidth;
    FSize := (Source as TOBDTouchSubheaderVciIndicator).Size;
    FVisible := (Source as TOBDTouchSubheaderVciIndicator).Visible;
    FCaption:= (Source as TOBDTouchSubheaderVciIndicator).Caption;
    FFromColor := (Source as TOBDTouchSubheaderVciIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderVciIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetBorderWidth(Value: Single);
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
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetSize(Value: Single);
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
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetVisible(Value: Boolean);
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
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FORMAT
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetCaption(Value: TCaption);
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
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetFromColor(Value: TColor);
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
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetToColor(Value: TColor);
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
procedure TOBDTouchSubheaderInternetConnectionIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderInternetConnectionIndicator.Create;
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
destructor TOBDTouchSubheaderInternetConnectionIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderInternetConnectionIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchSubheaderInternetConnectionIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchSubheaderInternetConnectionIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchSubheaderInternetConnectionIndicator).BorderWidth;
    FSize := (Source as TOBDTouchSubheaderInternetConnectionIndicator).Size;
    FVisible := (Source as TOBDTouchSubheaderInternetConnectionIndicator).Visible;
    FCaption:= (Source as TOBDTouchSubheaderInternetConnectionIndicator).Caption;
    FFromColor := (Source as TOBDTouchSubheaderInternetConnectionIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderInternetConnectionIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderProtocolIndicator.SetBorderColor(Value: TColor);
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
procedure TOBDTouchSubheaderProtocolIndicator.SetBorderWidth(Value: Single);
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
procedure TOBDTouchSubheaderProtocolIndicator.SetSize(Value: Single);
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
procedure TOBDTouchSubheaderProtocolIndicator.SetVisible(Value: Boolean);
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
procedure TOBDTouchSubheaderProtocolIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET FORMAT
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderProtocolIndicator.SetCaption(Value: TCaption);
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
procedure TOBDTouchSubheaderProtocolIndicator.SetFromColor(Value: TColor);
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
procedure TOBDTouchSubheaderProtocolIndicator.SetToColor(Value: TColor);
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
procedure TOBDTouchSubheaderProtocolIndicator.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderProtocolIndicator.Create;
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
destructor TOBDTouchSubheaderProtocolIndicator.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderProtocolIndicator.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchSubheaderProtocolIndicator) then
  begin
    FBorderColor := (Source as TOBDTouchSubheaderProtocolIndicator).BorderColor;
    FBorderWidth := (Source as TOBDTouchSubheaderProtocolIndicator).BorderWidth;
    FSize := (Source as TOBDTouchSubheaderProtocolIndicator).Size;
    FVisible := (Source as TOBDTouchSubheaderProtocolIndicator).Visible;
    FCaption:= (Source as TOBDTouchSubheaderProtocolIndicator).Caption;
    FFromColor := (Source as TOBDTouchSubheaderProtocolIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderProtocolIndicator).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CLASS CONSTRUCTOR
//------------------------------------------------------------------------------
class constructor TOBDTouchSubheader.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TOBDTouchSubheader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// CLASS DESTRUCTOR
//------------------------------------------------------------------------------
class destructor TOBDTouchSubheader.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TOBDTouchSubheader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetBackground(Value: TOBDTouchSubheaderBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetBorder(Value: TOBDTouchSubheaderBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BATTERY INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetBatteryIndicator(Value: TOBDTouchSubheaderBatteryIndicator);
begin
  FBatteryIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET VCI INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetVciIndicator(Value: TOBDTouchSubheaderVciIndicator);
begin
  FVciIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET INTERNET CONNECTION INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetInternetConnectionIndicator(Value: TOBDTouchSubheaderInternetConnectionIndicator);
begin
  FInternetConnectionIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PROTOCOL INDICATOR
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetProtocolIndicator(Value: TOBDTouchSubheaderProtocolIndicator);
begin
  FProtocolIndicator.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET CONNECTION COMPONENT
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetConnectionComponent(const Value: TOBDConnectionComponent);
begin
  if FConnectionComponent = Value then
    Exit;

  if Assigned(FConnectionComponent) and (FConnectionComponent.OnConnectionStateChanged = HandleConnectionStateChanged) then
    FConnectionComponent.OnConnectionStateChanged := nil;

  FConnectionComponent := Value;

  if Assigned(FConnectionComponent) then
  begin
    FConnectionComponent.OnConnectionStateChanged := HandleConnectionStateChanged;
    if FAutoApplyConnectionDetails then
      HandleConnectionStateChanged(FConnectionComponent, FConnectionComponent.Connected, FConnectionComponent.ConnectionType);
  end;
end;

//------------------------------------------------------------------------------
// HANDLE CONNECTION STATE CHANGED
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.HandleConnectionStateChanged(Sender: TObject; const Connected: Boolean; const ConnectionType: TOBDConnectionType);
begin
  if not FAutoApplyConnectionDetails then
    Exit;

  TThread.Queue(nil,
    procedure
    begin
      ApplyConnectionDetails(Connected, ConnectionType);
    end);
end;

//------------------------------------------------------------------------------
// APPLY CONNECTION DETAILS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.ApplyConnectionDetails(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
var
  CaptionText: string;
  InternetText: string;
begin
  if not Assigned(FConnectionComponent) then
    Exit;

  if Connected then
  begin
    CaptionText := BuildConnectionCaption(ConnectionType);
    case ConnectionType of
      ctWiFi:
        InternetText := Format('%s:%d', [FConnectionComponent.IPAddress, FConnectionComponent.Port]);
      ctBluetooth:
        InternetText := FConnectionComponent.BluetoothAddress;
    else
      InternetText := '';
    end;
  end
  else
  begin
    CaptionText := 'Disconnected';
    InternetText := '';
  end;

  FVciIndicator.Caption := CaptionText;
  FInternetConnectionIndicator.Visible := Connected and (ConnectionType in [ctWiFi, ctBluetooth]);
  FInternetConnectionIndicator.Caption := InternetText;
  Invalidate;
end;

//------------------------------------------------------------------------------
// BUILD CONNECTION CAPTION
//------------------------------------------------------------------------------
function TOBDTouchSubheader.BuildConnectionCaption(const ConnectionType: TOBDConnectionType): string;
begin
  case ConnectionType of
    ctSerial:
      Result := Format('COM %s', [FConnectionComponent.SerialPort]);
    ctBluetooth:
      Result := 'Bluetooth';
    ctWiFi:
      Result := 'WiFi';
    ctFTDI:
      Result := 'FTDI';
  else
    Result := 'Unknown';
  end;
end;

//------------------------------------------------------------------------------
// WM_PAINT MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.WMPaint(var Msg: TWMPaint);
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
procedure TOBDTouchSubheader.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  // Set the handled flag
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Adjust window style to avoid unnecessary redraws on size changes,
  // optimizing performance for custom drawing.
  with Params do Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.Paint;
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
procedure TOBDTouchSubheader.Resize;
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
procedure TOBDTouchSubheader.Loaded;
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
procedure TOBDTouchSubheader.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SettingsChanged(Sender: TObject);
begin
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.PaintBuffer;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  BackgroundRect, BorderRect: TSkRect;
  MeasureRect: TRect;
  BatteryRect, VciRect, InternetRect, ProtocolRect: TRectF;
  BatteryCaptionRect, VciCaptionRect, InternetCaptionRect, ProtocolCaptionRect: TRect;
  BatteryLabelText, VciLabelText, InternetLabelText, ProtocolLabelText: string;
  HasBattery, HasVci, HasInternet, HasProtocol: Boolean;
  X, Y, Z: Single;
  TextSize: TSizeF;
begin
  // Update the size of the buffer
  Buffer.SetSize(Width, Height);

  // Allocate a Skia surface for the subheader drawing pass
  Surface := TSkSurface.MakeRasterN32Premul(Width, Height);
  Canvas := Surface.Canvas;

  // Clear using the active style's fill color so the Skia surface matches themed backgrounds
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

  // Draw the backround gradient when configured
  if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
  begin
    BackgroundRect := TSkRect.Create(0.0, 0.0, Width + 0.0, Height + 0.0);
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

  // Draw the border stripe when enabled
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
  begin
    BorderRect := TSkRect.Create(0.0, Height - Border.Height + 0.0, Width + 0.0, Height + 0.0);
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

  // Prepare layout state
  X := Width - 8;
  HasBattery := False;
  HasVci := False;
  HasInternet := False;
  HasProtocol := False;

  // Draw the battery indicator
  if BatteryIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (BatteryIndicator.Size / 2);
    BatteryLabelText := Format(BatteryIndicator.Format, [BatteryIndicator.Voltage]);

    TextSize := MeasureSkText(BatteryLabelText, BatteryIndicator.Font);
    MeasureRect := Rect(0, 0, Width - 8, Height - Border.Height);
    BatteryCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 4);

    BatteryRect := TRectF.Create(X - BatteryIndicator.Size, Y, X, Y + BatteryIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(BatteryRect.Left, BatteryRect.Top),
      TSkPoint.Create(BatteryRect.Left, BatteryRect.Bottom),
      [SafeColorRefToSkColor(BatteryIndicator.FromColor), SafeColorRefToSkColor(BatteryIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := BatteryIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(BatteryIndicator.BorderColor);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    X := X - (BatteryIndicator.Size + 4);
    HasBattery := True;
  end;

  // Draw the VCI indicator
  if VciIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (VciIndicator.Size / 2);
    VciLabelText := VciIndicator.Caption;

    TextSize := MeasureSkText(VciLabelText, VciIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    VciCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 2);

    VciRect := TRectF.Create(X - VciIndicator.Size, Y, X, Y + VciIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(VciRect.Left, VciRect.Top),
      TSkPoint.Create(VciRect.Left, VciRect.Bottom),
      [SafeColorRefToSkColor(VciIndicator.FromColor), SafeColorRefToSkColor(VciIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := VciIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(VciIndicator.BorderColor);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    X := X - (VciIndicator.Size + 4);
    HasVci := True;
  end;

  // Draw the internet connection indicator
  if InternetConnectionIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (InternetConnectionIndicator.Size / 2);
    InternetLabelText := InternetConnectionIndicator.Caption;

    TextSize := MeasureSkText(InternetLabelText, InternetConnectionIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    InternetCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - TextSize.cx;

    InternetRect := TRectF.Create(X - InternetConnectionIndicator.Size, Y, X, Y + InternetConnectionIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(InternetRect.Left, InternetRect.Top),
      TSkPoint.Create(InternetRect.Left, InternetRect.Bottom),
      [SafeColorRefToSkColor(InternetConnectionIndicator.FromColor), SafeColorRefToSkColor(InternetConnectionIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawOval(TSkRect.Create(InternetRect.Left, InternetRect.Top, InternetRect.Right, InternetRect.Bottom), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := InternetConnectionIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(InternetConnectionIndicator.BorderColor);
    Canvas.DrawPath(CreateInternetGlobePath(InternetRect), Paint);

    X := X - (InternetConnectionIndicator.Size + 4);
    HasInternet := True;
  end;

  // Draw the protocol indicator
  if ProtocolIndicator.Visible then
  begin
    Z := X;
    Y := ((Height - Border.Height) / 2) - (ProtocolIndicator.Size / 2);

    ProtocolRect := TRectF.Create(8, Y, 8 + ProtocolIndicator.Size, Y + ProtocolIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(ProtocolRect.Left, ProtocolRect.Top),
      TSkPoint.Create(ProtocolRect.Left, ProtocolRect.Bottom),
      [SafeColorRefToSkColor(ProtocolIndicator.FromColor), SafeColorRefToSkColor(ProtocolIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := ProtocolIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(ProtocolIndicator.BorderColor);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    ProtocolLabelText := FitTextToWidth(ProtocolIndicator.Caption, ProtocolIndicator.Font, Round(Z - (12 + ProtocolIndicator.Size)));
    TextSize := MeasureSkText(ProtocolLabelText, ProtocolIndicator.Font);
    MeasureRect := Rect(12 + ProtocolIndicator.Size, 0, Round(Z - (12 + ProtocolIndicator.Size)), Height - Border.Height);
    ProtocolCaptionRect := Rect(
      MeasureRect.Left,
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Left + Min(Ceil(TextSize.cx), MeasureRect.Right - MeasureRect.Left),
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    HasProtocol := True;
  end;

  // Render captions with Skia to keep text drawing inside the Skia pipeline
  if HasBattery then
  begin
    DrawSkTextCentered(Canvas, BatteryLabelText, BatteryIndicator.Font, TRectF.Create(BatteryCaptionRect), BatteryIndicator.Font.Color, TSkTextAlign.Right);
    DrawSkTextCentered(Canvas, '- +', BatteryIndicator.Font, BatteryRect, BatteryIndicator.Font.Color);
  end;

  if HasVci then
    DrawSkTextCentered(Canvas, VciLabelText, VciIndicator.Font, TRectF.Create(VciCaptionRect), VciIndicator.Font.Color, TSkTextAlign.Right);

  if HasInternet then
    DrawSkTextCentered(Canvas, InternetLabelText, InternetConnectionIndicator.Font, TRectF.Create(InternetCaptionRect), InternetConnectionIndicator.Font.Color, TSkTextAlign.Right);

  if HasProtocol then
    DrawSkTextCentered(Canvas, ProtocolLabelText, ProtocolIndicator.Font, TRectF.Create(ProtocolCaptionRect), ProtocolIndicator.Font.Color, TSkTextAlign.Left);

  // Copy the Skia surface into the component buffer
  Surface.MakeImageSnapshot.ToBitmap(Buffer);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheader.Create(AOwner: TComponent);
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
  FBackground := TOBDTouchSubheaderBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDTouchSubheaderBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create battery indicator
  FBatteryIndicator := TOBDTouchSubheaderBatteryIndicator.Create;
  FBatteryIndicator.OnChange := SettingsChanged;
  // Create vci indicator
  FVciIndicator := TOBDTouchSubheaderVciIndicator.Create;
  FVciIndicator.OnChange := SettingsChanged;
  // Create internet connection indicator
  FInternetConnectionIndicator := TOBDTouchSubheaderInternetConnectionIndicator.Create;
  FInternetConnectionIndicator.OnChange := SettingsChanged;
  // Create protocol indicator
  FProtocolIndicator := TOBDTouchSubheaderProtocolIndicator.Create;
  FProtocolIndicator.OnChange := SettingsChanged;
  // Enable automatic connection application by default
  FAutoApplyConnectionDetails := True;
  // Set defaults
  Height := DEFAULT_HEIGHT;
  Align := alTop;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchSubheader.Destroy;
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
  // Clear connection component reference
  if Assigned(FConnectionComponent) and (FConnectionComponent.OnConnectionStateChanged = HandleConnectionStateChanged) then
    FConnectionComponent.OnConnectionStateChanged := nil;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// REPAINT
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.Repaint;
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
procedure TOBDTouchSubheader.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchSubheader) then
  begin
    FBackground.Assign((Source as TOBDTouchSubheader).Background);
    FBorder.Assign((Source as TOBDTouchSubheader).Border);
    FBatteryIndicator.Assign((Source as TOBDTouchSubheader).BatteryIndicator);
    FVciIndicator.Assign((Source as TOBDTouchSubheader).VciIndicator);
    FInternetConnectionIndicator.Assign((Source as TOBDTouchSubheader).InternetConnectionIndicator);
    FProtocolIndicator.Assign((Source as TOBDTouchSubheader).ProtocolIndicator);
  end;
end;

end.
