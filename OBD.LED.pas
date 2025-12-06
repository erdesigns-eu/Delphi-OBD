//------------------------------------------------------------------------------
// UNIT           : OBD.LED.pas
// CONTENTS       : LED component with Skia rendering
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 24/03/2024
// UPDATED        : 06/12/2025 - Refactored for direct Skia rendering
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.LED;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Math, Vcl.Controls,
  WinApi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Themes, System.Skia, Skia.Vcl,

  OBD.CustomControl, OBD.CustomControl.Helpers;

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
    ///   Cached Skia snapshot for the grayed LED state
    /// </summary>
    FGrayedImage: ISkImage;
    /// <summary>
    ///   Cached Skia snapshot for the off LED state
    /// </summary>
    FOffImage: ISkImage;
    /// <summary>
    ///   Cached Skia snapshot for the on LED state
    /// </summary>
    FOnImage: ISkImage;
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
    /// <summary>
    ///   Invalidate cached Skia LED snapshots for all states
    /// </summary>
    procedure InvalidateColors; virtual;
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintSkia(Canvas: ISkCanvas); override;
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
  System.Math;

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
    Invalidate;
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

  function PaintLedImage(State: TOBDLedState): ISkImage;
  var
    BackgroundColor: TAlphaColor;
    Surface: ISkSurface;
    Canvas: ISkCanvas;
    Size, X, Y, GW, GH, GX: Single;
    BorderRect, LedRect, GlareRect: TRectF;
    BorderPaint, FillPaint, GlarePaint: ISkPaint;
    Colors: TArray<TAlphaColor>;
  begin
    // Resolve the background tint directly in Skia to avoid any GDI style drawing
    if TStyleManager.IsCustomStyleActive then
      BackgroundColor := SafeColorRefToSkColor(StyleServices.GetSystemColor(clWindow))
    else
      BackgroundColor := SafeColorRefToSkColor(Self.Color);

    // Allocate a dedicated Skia surface for the LED snapshot
    Surface := TSkSurface.MakeRasterN32Premul(Width, Height);
    Canvas := Surface.Canvas;

    // Clear the surface to the desired background tint
    Canvas.Clear(BackgroundColor);

    // Calculate the size of the LED body and center it within the control bounds
    Size := System.Math.Min(ClientWidth - (MARGIN_FROM_BORDER * 2), ClientHeight - (MARGIN_FROM_BORDER * 2)) - (Border.Width * 2) - 2;
    X := (Width / 2) - (Size / 2);
    Y := (Height / 2) - (Size / 2);
    BorderRect := TRectF.Create(X, Y, X + Size, Y + Size);

    // Paint the subtle background disk behind the LED for depth
    BorderPaint := TSkPaint.Create;
    BorderPaint.AntiAlias := True;
    BorderPaint.Style := TSkPaintStyle.Fill;
    BorderPaint.Color := SafeColorRefToSkColor(clWindow);
    Canvas.DrawOval(BorderRect, BorderPaint);

    // Prepare the main LED fill rectangle and choose the gradient palette by state
    LedRect := TRectF.Create(X + Border.Width + 2, Y + Border.Width + 2, X + Size - Border.Width - 2, Y + Size - Border.Width - 2);
    FillPaint := TSkPaint.Create;
    FillPaint.AntiAlias := True;
    FillPaint.Style := TSkPaintStyle.Fill;
    case State of
      lsGrayed : Colors := [SafeColorRefToSkColor(GrayedColor.FromColor), SafeColorRefToSkColor(GrayedColor.ToColor)];
      lsOff    : Colors := [SafeColorRefToSkColor(OffColor.FromColor), SafeColorRefToSkColor(OffColor.ToColor)];
      lsOn     : Colors := [SafeColorRefToSkColor(OnColor.FromColor), SafeColorRefToSkColor(OnColor.ToColor)];
    end;
    FillPaint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(LedRect.Left, LedRect.Top), TSkPoint.Create(LedRect.Left, LedRect.Bottom), Colors, nil, TSkTileMode.Clamp);
    Canvas.DrawOval(LedRect, FillPaint);

    // Add a thin stroke around the LED body to highlight the rim
    BorderPaint := TSkPaint.Create;
    BorderPaint.AntiAlias := True;
    BorderPaint.Style := TSkPaintStyle.Stroke;
    BorderPaint.StrokeWidth := 1.5;
    case State of
      lsGrayed : BorderPaint.Color := SafeColorRefToSkColor(GrayedColor.FromColor);
      lsOff    : BorderPaint.Color := SafeColorRefToSkColor(OffColor.FromColor);
      lsOn     : BorderPaint.Color := SafeColorRefToSkColor(OnColor.FromColor);
    end;
    Canvas.DrawOval(LedRect, BorderPaint);

    // Calculate the glare footprint and blend a translucent highlight across it
    GW := Size * 0.75;
    GH := Size * 0.5;
    GX := (Width / 2) - (GW / 2);
    GlareRect := TRectF.Create(GX, Y + Border.Width + 2, GX + GW, Y + Border.Width + 2 + GH);
    GlarePaint := TSkPaint.Create;
    GlarePaint.AntiAlias := True;
    GlarePaint.Style := TSkPaintStyle.Fill;
    GlarePaint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(GlareRect.Left, GlareRect.Top), TSkPoint.Create(GlareRect.Left, GlareRect.Bottom),
      [$4BFFFFFF, $1EFFFFFF], nil, TSkTileMode.Clamp);
    Canvas.DrawOval(GlareRect, GlarePaint);

    // Build the outer border ring gradient to frame the LED
    GW := Width - 1;
    GH := Height - 1;
    BorderRect := TRectF.Create(0, 0, GW, GH);
    BorderPaint := TSkPaint.Create;
    BorderPaint.AntiAlias := True;
    BorderPaint.Style := TSkPaintStyle.Stroke;
    BorderPaint.StrokeWidth := Border.Width;
    BorderPaint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(BorderRect.Left, BorderRect.Top), TSkPoint.Create(BorderRect.Left, BorderRect.Bottom),
      [SafeColorRefToSkColor(Border.FromColor), SafeColorRefToSkColor(Border.ToColor)], nil, TSkTileMode.Clamp);
    Canvas.DrawOval(BorderRect, BorderPaint);

    // Add an inner ring for subtle contrast and dimensionality
    BorderRect := TRectF.Create(1, 1, GW - 1, GH - 1);
    BorderPaint := TSkPaint.Create;
    BorderPaint.AntiAlias := True;
    BorderPaint.Style := TSkPaintStyle.Stroke;
    BorderPaint.StrokeWidth := Border.Width;
    BorderPaint.Shader := TSkShader.MakeLinearGradient(TSkPoint.Create(BorderRect.Left, BorderRect.Top), TSkPoint.Create(BorderRect.Left, BorderRect.Bottom),
      [SafeColorRefToSkColor(Border.ToColor), SafeColorRefToSkColor(Border.FromColor)], nil, TSkTileMode.Clamp);
    Canvas.DrawOval(BorderRect, BorderPaint);

    Result := Surface.MakeImageSnapshot;
  end;

begin
  // Rebuild all LED state snapshots using Skia-only drawing to remove GDI usage
  FGrayedImage := PaintLedImage(lsGrayed);
  FOffImage := PaintLedImage(lsOff);
  FOnImage := PaintLedImage(lsOn);
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDLed.PaintSkia(Canvas: ISkCanvas);
var
  Image: ISkImage;
begin
  // Select the matching Skia snapshot for the requested LED state
  case State of
    lsGrayed : Image := FGrayedImage;
    lsOff    : Image := FOffImage;
    lsOn     : Image := FOnImage;
  end;

  // If a snapshot is missing (e.g. after resize), rebuild and reselect
  if not Assigned(Image) then
  begin
    InvalidateColors;
    case State of
      lsGrayed : Image := FGrayedImage;
      lsOff    : Image := FOffImage;
      lsOn     : Image := FOnImage;
    end;
  end;

  // Draw the Skia snapshot directly to the canvas (zero-copy rendering)
  if Assigned(Image) then
    Canvas.DrawImage(Image, 0, 0)
  else
    Canvas.Clear(TAlphaColorRec.Null);
end;

//------------------------------------------------------------------------------
// ON CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDLed.SettingsChanged(Sender: TObject);
begin
  // Invalidate background
  InvalidateColors;
  // Invalidate buffer
  Invalidate;
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
  Invalidate;
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
  Invalidate;
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
  // Generate the initial Skia LED snapshots
  InvalidateColors;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDLed.Destroy;
begin
  // Release cached Skia snapshots
  FGrayedImage := nil;
  FOffImage := nil;
  FOnImage := nil;
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
  Invalidate;
end;

end.
