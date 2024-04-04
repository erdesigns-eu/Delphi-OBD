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
  System.SysUtils, System.Classes, System.Types, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Themes, Vcl.ExtCtrls,

  OBD.CustomControl.Common, OBD.CustomControl.Constants;

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
  private
    /// <summary>
    ///   WM_PAINT message handler
    /// </summary>
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    /// <summary>
    ///   WM_ERASEBKGND message handler
    /// </summary>
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
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
