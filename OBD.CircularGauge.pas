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
    ///   Gauge background
    /// </summary>
    FBackground: TOBDCircularGaugeBackground;
    /// <summary>
    ///   Gauge border
    /// </summary>
    FBorder: TOBDCircularGaugeBorder;

    /// <summary>
    ///   Set start angle
    /// </summary>
    procedure SetStartAngle(Value: Single);
    /// <summary>
    ///   Set end angle
    /// </summary>
    procedure SetEndAngle(Value: Single);
    /// <summary>
    ///   Set background properties
    /// </summary>
    procedure SetBackground(Value: TOBDCircularGaugeBackground);
    /// <summary>
    ///   Set border properties
    /// </summary>
    procedure SetBorder(Value: TOBDCircularGaugeBorder);
  protected
    /// <summary>
    ///   Invalidate background (Repaint background buffer)
    /// </summary>
    procedure InvalidateBackground; virtual;
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintBuffer; override;
    /// <summary>
    ///   On change handler
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
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
    ///   Gauge background
    /// </summary>
    property Background: TOBDCircularGaugeBackground read FBackground write SetBackground;
    /// <summary>
    ///   Gauge border
    /// </summary>
    property Border: TOBDCircularGaugeBorder read FBorder write SetBorder;
  end;

procedure Register;

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
  // Call inherited assign
  inherited;
  // Assign properties
  if (Source is TOBDCircularGaugeBackground) then
  begin
    FFromColor := (Source as TOBDCircularGaugeBackground).FromColor;
    FToColor := (Source as TOBDCircularGaugeBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
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
  // Call inherited assign
  inherited;
  // Assign properties
  if (Source is TOBDCircularGaugeBorder) then
  begin
    FFromColor := (Source as TOBDCircularGaugeBorder).FromColor;
    FToColor := (Source as TOBDCircularGaugeBorder).ToColor;
    FWidth := (Source as TOBDCircularGaugeBorder).Width;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
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
    Size := Min(ClientWidth, ClientHeight);
    X := (Width - Size) / 2;
    Y := (Height - Size) / 2;

    // Get the rectangle for the gauge
    GaugeRect := MakeRect(X, Y, Size, Size);
    // Create the background brush
    Brush := TGPLinearGradientBrush.Create(GaugeRect, ColorRefToARGB(Background.FromColor), ColorRefToARGB(Background.ToColor), LinearGradientModeVertical);
    try
      // Fill the gauge background
      Graphics.FillEllipse(Brush, GaugeRect);
    finally
      // Free the background brush object
      Brush.Free;
    end;

    // Create the border brush
    Brush := TGPLinearGradientBrush.Create(GaugeRect, ColorRefToARGB(Border.FromColor), ColorRefToARGB(Border.ToColor), LinearGradientModeVertical);
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

    // Draw the ticks
    // Draw the numbers?

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
  // Create gauge background properties
  FBackground := TOBDCircularGaugeBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create gauge border properties
  FBorder := TOBDCircularGaugeBorder.Create;
  FBorder.OnChange := SettingsChanged;
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
