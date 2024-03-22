//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.pas
// CONTENTS       : Base Custom Control Component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 20/03/2024
// NOTE           : This component serves as a base for other visual components,
//                  it has a buffer and is drawn at a specified frame-rate, which
//                  makes it ideal for components like Gauges and Displays that
//                  need to be redrawn regularly.
//------------------------------------------------------------------------------
unit OBD.CustomControl;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, Vcl.ExtCtrls;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default frames per second
  /// </summary>
  DEFAULT_FPS = 30;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Base Custom Control Component
  /// </summary>
  TOBDCustomControl = class(TCustomControl)
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
    ///   Handle needed for the timer
    ///   Note: Using the components own handle throws an exception, so we need
    ///   to allocate our own handle so we can manage its lifecycle.
    /// </summary>
    FWindowHandle: THandle;
    /// <summary>
    ///   Buffer (This is the canvas we draw on)
    /// </summary>
    FBuffer: TBitmap;
    /// <summary>
    ///   Update rect (Invalidated rectangle)
    /// </summary>
    FUpdateRect: TRect;
    /// <summary>
    ///   Handle of the FPS timer
    /// </summary>
    FTimerHandle: THandle;
  private
    /// <summary>
    ///   Frames per second
    /// </summary>
    FFramesPerSecond: Integer;

    /// <summary>
    ///   Set frames per second
    /// </summary>
    procedure SetFramesPerSecond(Value: Integer);
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
    ///   Timer proc handler
    /// </summary>
    procedure TimerProc(var Msg: TMessage);
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintBuffer; virtual;
    /// <summary>
    ///   Invalidate buffer
    /// </summary>
    procedure InvalidateBuffer; virtual;
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
    ///   Frames per second
    /// </summary>
    property FramesPerSecond: Integer read FFramesPerSecond write SetFramesPerSecond default DEFAULT_FPS;
  published
    /// <summary>
    ///   Component alignment (inherited)
    /// </summary>
    property Align;
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

//------------------------------------------------------------------------------
// CLASS CONSTRUCTOR
//------------------------------------------------------------------------------
class constructor TOBDCustomControl.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TOBDCustomControl, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// CLASS DESTRUCTOR
//------------------------------------------------------------------------------
class destructor TOBDCustomControl.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TOBDCustomControl, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// SET FRAMES PER SECOND
//------------------------------------------------------------------------------
procedure TOBDCustomControl.SetFramesPerSecond(Value: Integer);
begin
  if (FFramesPerSecond <> Value) and (Value >= 1) and (Value < 100) then
  begin
    // Update the FPS
    FFramesPerSecond := Value;
    if not (csDesigning in ComponentState) then
    begin
      // Kill the timer
      if (FTimerHandle <> 0) then KillTimer(Handle, FTimerHandle);
      // Create new timer
      FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FFramesPerSecond, nil);
    end;
  end;
end;

//------------------------------------------------------------------------------
// WM_PAINT MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCustomControl.WMPaint(var Msg: TWMPaint);
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
procedure TOBDCustomControl.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  // Set the handled flag
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDCustomControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Adjust window style to avoid unnecessary redraws on size changes,
  // optimizing performance for custom drawing.
  with Params do Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Paint;
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
procedure TOBDCustomControl.Resize;
begin
  // Call inherited Resize
  inherited;
  // Update the size of the buffer
  FBuffer.SetSize(Width, Height);
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Loaded;
begin
  inherited;
  // Update the size of the buffer
  FBuffer.SetSize(Width, Height);
  // Create new timer
  if not (csDesigning in ComponentState) then FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FFramesPerSecond, nil);
end;

//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDCustomControl.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Invalidate buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// TIMER MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCustomControl.TimerProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_TIMER then
  begin
    // Trigger a repaint
    if (FTimerHandle <> 0) then Invalidate;
  end else
    // Pass message to default message handler
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDCustomControl.PaintBuffer;
begin
  // This is actually an abstract method, but for safety we add it like this.
end;

//------------------------------------------------------------------------------
// INVALIDATE BUFFER
//------------------------------------------------------------------------------
procedure TOBDCustomControl.InvalidateBuffer;
begin
  // Execute the Paint Buffer method. This method is an alternative for the
  // default invalidate method, because we are drawing to the component canvas
  // when the timer is fired (Fires x times per second).
  PaintBuffer;
  // When we are using this component in Design Mode (In the Delphi IDE)
  // we want to invalidate the control too, because the timer wont fire
  // in Design mode.
  if (csDesigning in ComponentState) then Invalidate;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCustomControl.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Prevent background erasure for smoother rendering and reduced flickering.
  ControlStyle := ControlStyle + [csOpaque];
  // Create Buffer
  FBuffer := TBitmap.Create;
  // Set the buffer pixel format
  FBuffer.PixelFormat := pf32bit;
  // Set initial FPS
  FFramesPerSecond := DEFAULT_FPS;
  // Allocate window handle for the timer
  if not (csDesigning in ComponentState) then FWindowHandle := AllocateHWnd(TimerProc);
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDCustomControl.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    // Kill the timer
    if (FTimerHandle <> 0) then KillTimer(FWindowHandle, FTimerHandle);
    // Deallocate window handle
    DeallocateHWnd(FWindowHandle);
  end;
  // Free buffer
  FBuffer.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDCustomControl) then
  begin
    FFramesPerSecond := (Source as TOBDCustomControl).FramesPerSecond;
  end;
end;

end.

