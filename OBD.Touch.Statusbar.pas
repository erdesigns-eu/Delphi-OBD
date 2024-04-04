//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Statusbar.pas
// CONTENTS       : Statusbar component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/04/2024
//------------------------------------------------------------------------------
unit OBD.Touch.Statusbar;

interface

uses
  System.SysUtils, System.Classes, System.Types, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Themes, Vcl.ExtCtrls,
  Vcl.Forms,

  OBD.CustomControl.Common, OBD.CustomControl.Constants;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 21;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Size grip dot shape type
  /// </summary>
  TOBDTouchStatusbarSizeGripDotShape = (dsSquare, dsCircle);

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
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_TO;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_FROM;
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
  ///   Touch Statusbar Sizegrip properties
  /// </summary>
  TOBDTouchStatusbarSizeGrip = class(TPersistent)
  private
    /// <summary>
    ///   Size Grip rect
    /// </summary>
    FSizeGripRect: TRect;
  private
    /// <summary>
    ///   Dot Color
    /// </summary>
    FDotColor: TColor;
    /// <summary>
    ///   Dot size
    /// </summary>
    FDotSize: Single;
    /// <summary>
    ///   Dot spacing
    /// </summary>
    FDotSpacing: Single;
    /// <summary>
    ///   Dot shape
    /// </summary>
    FDotShape: TOBDTouchStatusbarSizeGripDotShape;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;

    /// <summary>
    ///   Set dot color
    /// </summary
    procedure SetDotColor(Value: TColor);
    /// <summary>
    ///   Set dot size
    /// </summary>
    procedure SetDotSize(Value: Single);
    /// <summary>
    ///   Set dot spacing
    /// </summary>
    procedure SetDotSpacing(Value: Single);
    /// <summary>
    ///   Set dot shape
    /// </summary>
    procedure SetDotShape(Value: TOBDTouchStatusbarSizeGripDotShape);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Size Grip rect
    /// </summary>
    property SizeGripRect: TRect read FSizeGripRect write FSizeGripRect;
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
    ///   Dot Color
    /// </summary>
    property DotColor: TColor read FDotColor write SetDotColor default DEFAULT_SIZEGRIP_COLOR;
    /// <summary>
    ///   Dot size
    /// </summary>
    property DotSize: Single read FDotSize write SetDotSize;
    /// <summary>
    ///   Dot spacing
    /// </summary>
    property DotSpacing: Single read FDotSpacing write SetDotSpacing;
    /// <summary>
    ///   Dot shape
    /// </summary>
    property DotShape: TOBDTouchStatusbarSizeGripDotShape read FDotShape write SetDotShape default dsCircle;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;

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
    ///   Size Grip
    /// </summary>
    FSizeGrip: TOBDTouchStatusbarSizeGrip;

    /// <summary>
    ///   Set background
    /// </summary>
    procedure SetBackground(Value: TOBDTouchStatusbarBackground);
    /// <summary>
    ///   Set border
    /// </summary>
    procedure SetBorder(Value: TOBDTouchStatusbarBorder);
    /// <summary>
    ///   Set size grip
    /// </summary>
    procedure SetSizeGrip(Value: TOBDTouchStatusbarSizeGrip);
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
    /// <summary>
    ///   Override WndProc method
    /// </summary>
    procedure WndProc(var Message: TMessage); override;
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
    property Background: TOBDTouchStatusbarBackground read FBackground write SetBackground;
    /// <summary>
    ///   Border
    /// </summary>
    property Border: TOBDTouchStatusbarBorder read FBorder write SetBorder;
    /// <summary>
    ///   Size Grip
    /// </summary>
    property SizeGrip: TOBDTouchStatusbarSizeGrip read FSizeGrip write SetSizeGrip;
  published
    /// <summary>
    ///   Component alignment (inherited)
    /// </summary>
    property Align default alBottom;
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
  FFromColor := DEFAULT_BORDER_TO;
  FToColor := DEFAULT_BORDER_FROM;
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
// SET DOT COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotColor(Value: TColor);
begin
  if (FDotColor <> Value) then
  begin
    // Set the new dot color
    FDotColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DOT SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotSize(Value: Single);
begin
  if (FDotSize <> Value) and (Value >= 1) then
  begin
    // Set the new dot size
    FDotSize := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DOT SPACING
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotSpacing(Value: Single);
begin
  if (FDotSpacing <> Value) and (Value >= 1) then
  begin
    // Set the new dot spacing
    FDotSpacing := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DOT SHAPE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotShape(Value: TOBDTouchStatusbarSizeGripDotShape);
begin
  if (FDotShape <> Value) then
  begin
    // Set the new dot shape
    FDotShape := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarSizeGrip.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FDotColor := DEFAULT_SIZEGRIP_COLOR;
  FDotSize := 2;
  FDotSpacing := 1;
  FDotShape := dsCircle;
  FVisible := True;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarSizeGrip) then
  begin
    FDotColor := (Source as TOBDTouchStatusbarSizeGrip).DotColor;
    FDotSize := (Source as TOBDTouchStatusbarSizeGrip).DotSize;
    FDotSpacing := (Source as TOBDTouchStatusbarSizeGrip).DotSpacing;
    FDotShape := (Source as TOBDTouchStatusbarSizeGrip).DotShape;
    FVisible := (Source as TOBDTouchStatusbarSizeGrip).Visible;
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
// SET SIZE GRIP
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetSizeGrip(Value: TOBDTouchStatusbarSizeGrip);
begin
  FSizeGrip.Assign(Value);
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
// WND PROC HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WndProc(var Message: TMessage);
var
  P: TPoint;
  Form: TCustomForm;
begin
  // Call inherited WndProc
  inherited WndProc(Message);

  // Get the parent form
  Form := GetParentForm(Self);
  // If there is a parentform and the window state is normal
  if Assigned(Form) and (Form is TForm) and (Form.WindowState = wsNormal) then
  begin
    // Check if we are in the sizegrip rect
    if (Message.Msg = WM_NCHITTEST) and FSizeGrip.Visible then
    begin
      P := ScreenToClient(Mouse.CursorPos);
      if PtInRect(FSizeGrip.SizeGripRect, P) then
      begin
        Message.Result := HTBOTTOMRIGHT;
        Exit;
      end;
    end;

    // Handle the WM_NCLBUTTONDOWN message to start resizing
    if (Message.Msg = WM_NCLBUTTONDOWN) and (Message.WParam = HTBOTTOMRIGHT) then
    begin
      ReleaseCapture;
      Form.Perform(WM_SYSCOMMAND, SC_SIZE + 9, 0);
    end;
  end;
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
  BackgroundRect, BorderRect, SizeGripRect: TGPRectF;
  Brush: TGPBrush;
  SizeGripPath: TGPGraphicsPath;
  SizeGripWidth: Integer;
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
      BackgroundRect := MakeRect(0.0, 0, Width, Height);
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
      BorderRect := MakeRect(0.0, 0, Width, Border.Height);
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

    // Draw the Size Grip
    if SizeGrip.Visible then
    begin
      SizeGripRect := MakeRect(0.0, Border.Height, Width - 3, Height - Border.Height);
      SizeGripWidth := Ceil((SizeGrip.DotSize + SizeGrip.DotSpacing) * 3);
      FSizeGrip.SizeGripRect := Rect((Width - 3) - SizeGripWidth, Border.Height, Width - 3, Height);
      if SizeGrip.DotShape = dsSquare then
        SizeGripPath := CreateSizeGripPathSquare(SizeGripRect, SizeGrip.DotSize, SizeGrip.DotSpacing)
      else
        SizeGripPath := CreateSizeGripPathCircle(SizeGripRect, SizeGrip.DotSize, SizeGrip.DotSpacing);
      Brush := TGPSolidBrush.Create(SafeColorRefToARGB(SizeGrip.DotColor));
      try
        Graphics.FillPath(Brush, SizeGripPath);
      finally
        SizeGripPath.Free;
        Brush.Free;
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
  // Create size grip
  FSizegrip := TOBDTouchStatusbarSizeGrip.Create;
  FSizeGrip.OnChange := SettingsChanged;
  // Set defaults
  Height := DEFAULT_HEIGHT;
  Align := alBottom;
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
  // Free size grip
  FSizegrip.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// REPAINT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Repaint;
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
procedure TOBDTouchStatusbar.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchStatusbar) then
  begin
    FBackground.Assign((Source as TOBDTouchStatusbar).Background);
    FBorder.Assign((Source as TOBDTouchStatusbar).Border);
    FSizeGrip.Assign((Source as TOBDTouchStatusbar).SizeGrip);
  end;
end;

end.
