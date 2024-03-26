//------------------------------------------------------------------------------
// UNIT           : OBD.LED.pas
// CONTENTS       : LED component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 24/03/2024
//------------------------------------------------------------------------------
unit OBD.LED;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes,

  OBD.CustomControl, OBD.CustomControl.Common;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
    /// <summary>
  ///   Margin from the border (Otherwise the led border is flattened on the side)
  /// </summary>
  MARGIN_FROM_BORDER = 2;

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
  DEFAULT_BORDER_WIDTH = 1;

  /// <summary>
  ///   Default grayed from color
  /// </summary>
  DEFAULT_GRAYED_FROM_COLOR = $00A3A3A3;
  /// <summary>
  ///   Default grayed to color
  /// </summary>
  DEFAULT_GRAYED_TO_COLOR = $00D4D4D4;
  /// <summary>
  ///   Default off from color
  /// </summary>
  DEFAULT_OFF_FROM_COLOR = $00974B00;
  /// <summary>
  ///   Default off to color
  /// </summary>
  DEFAULT_OFF_TO_COLOR = $00DF7000;
  /// <summary>
  ///   Default on from color
  /// </summary>
  DEFAULT_ON_FROM_COLOR = $00FF8000;
  /// <summary>
  ///   Default on to color
  /// </summary>
  DEFAULT_ON_TO_COLOR = $00FFAA55;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  TOBDLedState = (lsGrayed, lsOff, lsOn);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   LED border properties
  /// </summary>
  TOBDLedBorder = class(TPersistent)
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
  ///   LED color properties
  /// </summary>
  TOBDLedColor = class(TPersistent)
  private
    /// <summary>
    ///   From color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   To color
    /// </summary>
    FToColor: TColor;

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
    ///   From color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TOBDLedGrayedColor = class(TOBDLedColor)
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; override;
  published
    /// <summary>
    ///   From color
    /// </summary>
    property FromColor default DEFAULT_GRAYED_FROM_COLOR;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor default DEFAULT_GRAYED_TO_COLOR;
  end;

  TOBDLedOffColor = class(TOBDLedColor)
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; override;
  published
    /// <summary>
    ///   From color
    /// </summary>
    property FromColor default DEFAULT_OFF_FROM_COLOR;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor default DEFAULT_OFF_TO_COLOR;
  end;

  TOBDLedOnColor = class(TOBDLedColor)
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; override;
  published
    /// <summary>
    ///   From color
    /// </summary>
    property FromColor default DEFAULT_ON_FROM_COLOR;
    /// <summary>
    ///   To color
    /// </summary>
    property ToColor default DEFAULT_ON_TO_COLOR;
  end;

  /// <summary>
  ///   LED Component
  /// </summary>
  TOBDLed = class(TOBDCustomControl)
  private
    /// <summary>
    ///   Grayed buffer
    /// </summary>
    FGrayedBuffer: TBitmap;
    /// <summary>
    ///   Off buffer
    /// </summary>
    FOffBuffer: TBitmap;
    /// <summary>
    ///   On buffer
    /// </summary>
    FOnBuffer: TBitmap;
  private
    /// <summary>
    ///   State
    /// </summary>
    FState: TOBDLedState;
    /// <summary>
    ///   Border
    /// </summary>
    FBorder: TOBDLedBorder;
    /// <summary>
    ///   Grayed color
    /// </summary>
    FGrayedColor: TOBDLedGrayedColor;
    /// <summary>
    ///   Off color
    /// </summary>
    FOffColor: TOBDLedOffColor;
    /// <summary>
    ///   On color
    /// </summary>
    FOnColor: TOBDLedOnColor;

    /// <summary>
    ///   Set state
    /// </summary>
    procedure SetState(Value: TOBDLedState);
    /// <summary>
    ///   Set border
    /// </summary>
    procedure SetBorder(Value: TOBDLedBorder);
    /// <summary>
    ///   Set grayed color
    /// </summary>
    procedure SetGrayedColor(Value: TOBDLedGrayedColor);
    /// <summary>
    ///   Set off color
    /// </summary>
    procedure SetOffColor(Value: TOBDLedOffColor);
    /// <summary>
    ///   Set on color
    /// </summary>
    procedure SetOnColor(Value: TOBDLedOnColor);
  protected
    /// <summary>
    ///   Invalidate color buffers
    /// </summary>
    procedure InvalidateColors; virtual;
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
    ///   State
    /// </summary>
    property State: TOBDLedState read FState write SetState default lsGrayed;
    /// <summary>
    ///   Border
    /// </summary>
    property Border: TOBDLedBorder read FBorder write SetBorder;
    /// <summary>
    ///   Grayed color
    /// </summary>
    property GrayedColor: TOBDLedGrayedColor read FGrayedColor write SetGrayedColor;
    /// <summary>
    ///   Off color
    /// </summary>
    property OffColor: TOBDLedOffColor read FOffColor write SetOffColor;
    /// <summary>
    ///   On color
    /// </summary>
    property OnColor: TOBDLedOnColor read FOnColor write SetOnColor;
  end;

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDLedBorder.SetFromColor(Value: TColor);
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
procedure TOBDLedBorder.SetToColor(Value: TColor);
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
procedure TOBDLedBorder.SetWidth(Value: Integer);
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
constructor TOBDLedBorder.Create;
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
procedure TOBDLedBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDLedBorder) then
  begin
    FFromColor := (Source as TOBDLedBorder).FromColor;
    FToColor := (Source as TOBDLedBorder).ToColor;
    FWidth := (Source as TOBDLedBorder).Width;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDLedColor.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) and (Value <> clNone) then
  begin
    // Set new from color
    FFromColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDLedColor.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) and (Value <> clNone) then
  begin
    // Set new to color
    FToColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDLedColor.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDLedColor.Assign(Source: TPersistent);
begin
  if (Source is TOBDLedColor) then
  begin
    FFromColor := (Source as TOBDLedColor).FromColor;
    FToColor   := (Source as TOBDLedColor).ToColor;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDLedGrayedColor.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_GRAYED_FROM_COLOR;
  FToColor   := DEFAULT_GRAYED_TO_COLOR;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDLedOffColor.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_OFF_FROM_COLOR;
  FToColor := DEFAULT_OFF_TO_COLOR;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDLedOnColor.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_ON_FROM_COLOR;
  ToColor := DEFAULT_ON_TO_COLOR;
end;

//------------------------------------------------------------------------------
// SET STATE
//------------------------------------------------------------------------------
procedure TOBDLed.SetState(Value: TOBDLedState);
begin
  if (FState <> Value) then
  begin
    // Set new state
    FState := Value;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDLed.SetBorder(Value: TOBDLedBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET GRAYED COLOR
//------------------------------------------------------------------------------
procedure TOBDLed.SetGrayedColor(Value: TOBDLedGrayedColor);
begin
  FGrayedColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET OFF COLOR
//------------------------------------------------------------------------------
procedure TOBDLed.SetOffColor(Value: TOBDLedOffColor);
begin
  FOffColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ON COLOR
//------------------------------------------------------------------------------
procedure TOBDLed.SetOnColor(Value: TOBDLedOnColor);
begin
  FOnColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// INVALIDATE COLOR BUFFERS
//------------------------------------------------------------------------------
procedure TOBDLed.InvalidateColors;

  procedure PaintLed(Buffer: TBitmap; State: TOBDLedState);
  var
    SS: TCustomStyleServices;
    Size, X, Y, GW, GH, GX: Single;
    Graphics: TGPGraphics;
    BorderRect, LedRect, GlareRect: TGPRectF;
    Brush: TGPBrush;
    Pen: TGPPen;
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

      // Calculate the size of the led
      Size := System.Math.Min(ClientWidth - (MARGIN_FROM_BORDER * 2), ClientHeight - (MARGIN_FROM_BORDER * 2)) - (Border.Width * 2) - 2;
      // Calculate the horizontal position
      X := (Width / 2) - (Size / 2);
      // Calculate the vertical position
      Y := (Height / 2) - (Size / 2);
      // Get the rectangle for the border
      BorderRect := MakeRect(X, Y, Size, Size);

      // Draw the background - we use a clWindow color for the background,
      // this shines a bit through to offset the color and the border.
      Brush := TGPSolidBrush.Create(SafeColorRefToARGB(clWindow));
      try
        Graphics.FillEllipse(Brush, BorderRect);
      finally
        // Free brush object
        Brush.Free;
      end;

      // Get the rectangle for the led color
      LedRect := MakeRect(X + Border.Width + 2, Y + Border.Width + 2, Size - (Border.Width * 2) - 4,  Size - (Border.Width * 2) - 4);

      // Create the led brush
      case State of
        lsGrayed : Brush := TGPLinearGradientBrush.Create(LedRect, SafeColorRefToARGB(GrayedColor.FromColor), SafeColorRefToARGB(GrayedColor.ToColor), LinearGradientModeVertical);
        lsOff    : Brush := TGPLinearGradientBrush.Create(LedRect, SafeColorRefToARGB(OffColor.FromColor), SafeColorRefToARGB(OffColor.ToColor), LinearGradientModeVertical);
        lsOn     : Brush := TGPLinearGradientBrush.Create(LedRect, SafeColorRefToARGB(OnColor.FromColor), SafeColorRefToARGB(OnColor.ToColor), LinearGradientModeVertical);
      end;
      // Create the led pen
      case State of
        lsGrayed : Pen := TGPPen.Create(SafeColorRefToARGB(GrayedColor.FromColor), 1.5);
        lsOff    : Pen := TGPPen.Create(SafeColorRefToARGB(OffColor.FromColor), 1.5);
        lsOn     : Pen := TGPPen.Create(SafeColorRefToARGB(OnColor.FromColor), 1.5);
      end;
      Pen.SetAlignment(PenAlignmentInset);
      // Draw the led
      try
        Graphics.FillEllipse(Brush, LedRect);
        Graphics.DrawEllipse(Pen, LedRect);
      finally
        Brush.Free;
        Pen.Free;
      end;

      // Calculate glare width
      GW := Size * 0.75;
      // Calculate glare height
      GH := Size * 0.5;
      // Calculate the glare horizontal position
      GX := (Width / 2) - (GW / 2);

      // Get the rectangle for the glare
      GlareRect := MakeRect(GX, Y + + Border.Width + 2, GW, GH);
      // Create the glare brush (White with alpha 75 to White with alpha 30)
      Brush := TGPLinearGradientBrush.Create(GlareRect, MakeColor(75, 255, 255, 255), MakeColor(30, 255, 255, 255), LinearGradientModeVertical);
      // Draw the glare
      try
        Graphics.FillEllipse(Brush, GlareRect);
      finally
        Brush.Free;
      end;

      // Draw the border
      if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) and (Border.Width > 0) then
      begin
        // Create the border brush
        Brush := TGPLinearGradientBrush.Create(BorderRect, SafeColorRefToARGB(Border.FromColor), SafeColorRefToARGB(Border.ToColor), LinearGradientModeVertical);
        // Create the border pen
        Pen := TGPPen.Create(Brush, Border.Width);
        Pen.SetAlignment(PenAlignmentInset);
        try
          // Draw the gauge border
          Graphics.DrawEllipse(Pen, BorderRect);
        finally
          // Free the background brush object
          Brush.Free;
          // Free the background pen object
          Pen.Free;
        end;
      end;
    finally
      Graphics.Free;
    end;
  end;

begin
  // Paint the grayed led
  PaintLed(FGrayedBuffer, lsGrayed);
  // Paint the off led
  PaintLed(FOffBuffer, lsOff);
  // Paint the on led
  PaintLed(FOnBuffer, lsOn);
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDLed.PaintBuffer;
begin
  // Call inherited PaintBuffer
  inherited;
  // Copy the color buffer to the main buffer, by buffering the colors
  // and only updating the color buffers when the colors are changed
  // allows us to just copy the color buffer, which speeds up our PaintBuffer
  // resulting in less CPU consumption and allowing higher framerates.
  case State of
    lsGrayed : BitBlt(Buffer.Canvas.Handle, 0, 0, Width, Height, FGrayedBuffer.Canvas.Handle, 0, 0, SRCCOPY);
    lsOff    : BitBlt(Buffer.Canvas.Handle, 0, 0, Width, Height, FOffBuffer.Canvas.Handle, 0, 0, SRCCOPY);
    lsOn     : BitBlt(Buffer.Canvas.Handle, 0, 0, Width, Height, FOnBuffer.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

//------------------------------------------------------------------------------
// ON CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDLed.SettingsChanged(Sender: TObject);
begin
  // Invalidate background
  InvalidateColors;
  // Invalidate buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDLed.Resize;
begin
  // Call inherited Resize
  inherited;
  // Invalidate color buffers
  InvalidateColors;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDLed.Loaded;
begin
  // Call inherited Loaded
  inherited;
  // Invalidate color buffers
  InvalidateColors;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// WND PROC
//------------------------------------------------------------------------------
procedure TOBDLed.WndProc(var Message: TMessage);
begin
  // Call inherited WndProc
  inherited;
  // Handle message
  case Message.Msg of
    // Color changed
    CM_COLORCHANGED: InvalidateColors;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDLed.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Create grayed buffer
  FGrayedBuffer := TBitmap.Create;
  FGrayedBuffer.PixelFormat := pf32bit;
  // Create off buffer
  FOffBuffer := TBitmap.Create;
  FOffBuffer.PixelFormat := pf32bit;
  // Create on buffer
  FOnBuffer := TBitmap.Create;
  FOnBuffer.PixelFormat := pf32bit;
  // Create border
  FBorder := TOBDLedBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create grayed color
  FGrayedColor := TOBDLedGrayedColor.Create;
  FGrayedColor.OnChange := SettingsChanged;
  // Create off color
  FOffColor := TOBDLedOffColor.Create;
  FoffColor.OnChange := SettingsChanged;
  // Create on color
  FOnColor := TOBDLedOnColor.Create;
  FOnColor.OnChange := SettingsChanged;
  // Set defaults
  FState := lsGrayed;
  //
  Width := 25;
  Height := 25;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDLed.Destroy;
begin
  // Free grayed buffer
  FGrayedBuffer.Free;
  // Free off buffer
  FOffBuffer.Free;
  // Free on buffer
  FOnBuffer.Free;
  // Free border
  FBorder.Free;
  // Free grayed color
  FGrayedColor.Free;
  // Free off color
  FOffColor.Free;
  // Free on color
  FOnColor.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDLed.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDLed) then
  begin
    FBorder.Assign((Source as TOBDLed).Border);
    FState := (Source as TOBDLed).State;
    FGrayedColor.Assign((Source as TOBDLed).GrayedColor);
    FOffColor.Assign((Source as TOBDLed).OffColor);
    FOnColor.Assign((Source as TOBDLed).OnColor);
  end;
  // Invalidate color buffers
  InvalidateColors;
  // Invalidate the buffer
  InvalidateBuffer;
end;

end.
