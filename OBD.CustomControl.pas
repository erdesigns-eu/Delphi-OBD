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
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages, Vcl.Graphics;

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
  TOBDCustomControl = class(TCustomControl)
  private
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
    FTimerHandle: UINT_PTR;
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
    ///   WM_TIMER message handler
    /// </summary>
    procedure WMTimeStep(var Msg: TMessage); message WM_TIMER;
    /// <summary>
    ///   WM_PAINT message handler
    /// </summary>
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
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
  protected
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintBuffer; virtual; abstract;
    /// <summary>
    ///   Invalidate buffer
    /// </summary>
    procedure InvalidateBuffer; virtual;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TComponent); virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;
  published
    /// <summary>
    ///   Frames per second
    /// </summary>
    property FramesPerSecond: Integer read FFramesPerSecond write SetFramesPerSecond default DEFAULT_FPS;
  end;

implementation

//------------------------------------------------------------------------------
// SET FRAMES PER SECOND
//------------------------------------------------------------------------------
procedure TOBDCustomControl.SetFramesPerSecond(Value: Integer);
begin
  if (FFramesPerSecond <> Value) and (Value >= 1) and (Value < 100) then
  begin
    // Update the FPS
    FFramesPerSecond := Value;
    // Kill the timer
    if (FTimerHandle <> 0) then KillTimer(Handle, FTimerHandle);
    // Create new timer
    if not (csDesigning in ComponentState) then FTimerHandle := SetTimer(Handle, 1, 1000 div FFramesPerSecond, nil);
  end;
end;

//------------------------------------------------------------------------------
// WM_TIMER MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCustomControl.WMTimeStep(var Msg: TMessage);
begin
  // Trigger a repaint
  Invalidate;
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
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDCustomControl.CreateParams(var Params: TCreateParams);
begin
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
    BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, FBuffer.Canvas.Handle, X,  Y, SRCCOPY);
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Resize;
begin
  // Call inherited resize method
  inherited Resize;
  // Update the size of the buffer
  FBuffer.SetSize(Width, Height);
  // Invalidate the buffer
  InvalidateBuffer;
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
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDCustomControl.Destroy;
begin
  // Free buffer
  FBuffer.Free;
  // Kill the timer
  if FTimerHandle <> 0 then KillTimer(Handle, FTimerHandle);
  // Call inherited destructor
  inherited Destroy;
end;

end.

