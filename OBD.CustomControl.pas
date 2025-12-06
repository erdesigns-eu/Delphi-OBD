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
    ///   Use legacy double-buffering mode (deprecated)
    ///   When False (default): Uses on-demand rendering for better performance
    ///   When True: Uses timer-based pre-rendering (legacy behavior, may cause unnecessary redraws)
    ///   Note: Skia rendering is already highly optimized. This property controls timing, not quality.
    /// </summary>
    FUseDoubleBuffering: Boolean;

    /// <summary>
    ///   Set frames per second
    /// </summary>
    procedure SetFramesPerSecond(Value: Integer);
    /// <summary>
    ///   Set use double buffering
    /// </summary>
    procedure SetUseDoubleBuffering(Value: Boolean);
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
    /// <summary>
    ///   Use legacy double-buffering mode (default: False for better performance)
    ///   When False: Renders on-demand when invalidated (efficient for most use cases)
    ///   When True: Pre-renders to buffer on timer ticks (legacy mode, less efficient)
    ///   Note: All rendering still uses TBitmap Buffer internally for VCL compatibility
    /// </summary>
    property UseDoubleBuffering: Boolean read FUseDoubleBuffering write SetUseDoubleBuffering default False;
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
// SET USE DOUBLE BUFFERING
//------------------------------------------------------------------------------
procedure TOBDCustomControl.SetUseDoubleBuffering(Value: Boolean);
begin
  if FUseDoubleBuffering <> Value then
  begin
    FUseDoubleBuffering := Value;
    // Resize buffer if needed
    if FUseDoubleBuffering then
      FBuffer.SetSize(Width, Height);
    // Invalidate to redraw with new buffering mode
    InvalidateBuffer;
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

  // In optimized mode (!UseDoubleBuffering), render on-demand during WM_PAINT
  if not FUseDoubleBuffering then
    PaintBuffer;

  // Copy Buffer to Canvas (this happens in both modes)
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
  // Update the size of the buffer only if double buffering is enabled
  if FUseDoubleBuffering then
    FBuffer.SetSize(Width, Height);
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Loaded;
begin
  inherited;
  // Update the size of the buffer only if double buffering is enabled
  if FUseDoubleBuffering then
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
  if FUseDoubleBuffering then
  begin
    // Legacy mode: Pre-render to buffer immediately, will be copied to Canvas on next Paint
    PaintBuffer;
    // In design mode or when timer is not running, trigger a repaint
    if (csDesigning in ComponentState) then
      Invalidate;
  end
  else
  begin
    // Optimized mode: Just schedule a repaint, PaintBuffer will be called during WM_PAINT
    Invalidate;
  end;
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
  // Disable double buffering by default for better Skia performance
  FUseDoubleBuffering := False;
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
    FUseDoubleBuffering := (Source as TOBDCustomControl).UseDoubleBuffering;
  end;
end;

end.

