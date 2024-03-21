//------------------------------------------------------------------------------
// UNIT           : OBD.CircularGauge.pas
// CONTENTS       : Circular gauge component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 20/03/2024
//------------------------------------------------------------------------------
unit OBD.CircularGauge;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, OBD.CustomControl;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
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
  ///   Default gauge background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = $00E4E4E4;
  /// <summary>
  ///   Default gauge background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00CDCDCD;

  /// <summary>
  ///   Default gauge border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00D2D2D2;
  /// <summary>
  ///   Default gauge border to color
  /// </summary>
  DEFAULT_BORDER_TO = $009D9D9D;
  /// <summary>
  ///   Default gauge border width
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
  DEFAULT_NEEDLE_COLOR = clSkyBlue;
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
  ///   Circular Gauge Component
  /// </summary>
  TOBDCircularGauge = class(TOBDCustomControl)
  private
    /// <summary>
    ///   Background Buffer
    /// </summary>
    FBackgroundBuffer: TBitmap;
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
  protected
    /// <summary>
    ///   Override Resize method
    /// </summary>
    procedure Resize; override;
    /// <summary>
    ///   Override Loaded method
    /// </summary>
    procedure Loaded; override;
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
  end;

procedure Register;

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math;

//------------------------------------------------------------------------------
// CONVERT COLOR TO GDI+ COLOR
//------------------------------------------------------------------------------
function SafeColorRefToARGB(Color: TColor): DWORD;
var
  RGBColor: COLORREF;
begin
  // Convert TColor to a RGB color if it's a system color
  RGBColor := ColorToRGB(Color);
  // Now convert the RGB color to ARGB format expected by GDI+
  Result := (GetRValue(RGBColor) shl RedShift) or
            (GetGValue(RGBColor) shl GreenShift) or
            (GetBValue(RGBColor) shl BlueShift) or
            (AlphaMask);
end;

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
  if (FWidth <> Value) then
  begin
    // Set the enw width
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
    // Set value
    FValue := Value;
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
// INVALIDATE BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDCircularGauge.InvalidateBackground;
var
  SS: TCustomStyleServices;
  Graphics: TGPGraphics;
  Size, X, Y: Single;
  GaugeRect: TGPRectF;
  Brush: TGPBrush;
  Pen: TGPPen;

  TotalTicks, TickIndex: Integer;
  AnglePerTick, CurrentAngle, InnerRadius, OuterRadius: Single;
  StartPoint, EndPoint: TGPPointF;

  NumberAngle, NumberRadius: Single;
  NumberStr: WideString;
  NumberPoint: TGPPointF;
  Font: TGPFont;
  FontBrush: TGPSolidBrush;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;
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
    Size := System.Math.Min(ClientWidth, ClientHeight);
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

    // Draw the border
    if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
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
      Font := TGPFont.Create(FontFamily, FMinorTicks.Font.Size, FontStyleRegular, UnitPoint);
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
      Font := TGPFont.Create(FontFamily, FMajorTicks.Font.Size, FontStyleRegular, UnitPoint);
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
  // Copy the background buffer to the main buffer, by buffering the background
  // and only updating the background buffer when the background is changed
  // allows us to just copy the background buffer, which speeds up our PaintBuffer
  // resulting in less CPU consumption and allowing higher framerates.
  BitBlt(Buffer.Canvas.Handle, 0, 0, Width, Height, FBackgroundBuffer.Canvas.Handle, 0,  0, SRCCOPY);
  // Paint the needle on the buffer
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
  // Set default dimensions
  Width := 201;
  Height := 201;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDCircularGauge.Destroy;
begin
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
  end;
end;

//------------------------------------------------------------------------------
// REGISTER THE COMPONENT
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('ERDesigns OBD Visual', [TOBDCircularGauge]);
end;

end.
