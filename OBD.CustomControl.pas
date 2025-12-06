//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.pas
// CONTENTS       : Base Custom Control Component with Skia rendering support
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 20/03/2024
// UPDATED        : 06/12/2025 - Refactored for direct Skia rendering (no TBitmap buffer)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : This component serves as a base for Skia-rendered components.
//                  It provides timer-based rendering for animations and on-demand
//                  rendering for static content. Child classes override PaintSkia
//                  to render directly using Skia's canvas.
//------------------------------------------------------------------------------
unit OBD.CustomControl;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, System.Skia, Skia.Vcl;

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
    ///   WM_ERASEBKGND message handler
    /// </summary>
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
  protected
    /// <summary>
    ///   Override CreateParams method
    /// </summary>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>
    ///   Override Paint method - calls PaintSkia with Skia canvas
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
    ///   Paint using Skia canvas (override this in child classes)
    ///   This method is called with a Skia canvas that renders directly to the control
    /// </summary>
    procedure PaintSkia(Canvas: ISkCanvas); virtual;
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
    ///   Frames per second (for timer-based rendering during animations)
    ///   Set to 0 to disable timer and use on-demand rendering only
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
  if (FFramesPerSecond <> Value) and (Value >= 0) and (Value < 100) then
  begin
    // Update the FPS
    FFramesPerSecond := Value;
    if not (csDesigning in ComponentState) then
    begin
      // Kill existing timer
      if (FTimerHandle <> 0) then
      begin
        KillTimer(FWindowHandle, FTimerHandle);
        FTimerHandle := 0;
      end;
      // Create new timer only if FPS > 0
      if Value > 0 then
        FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FFramesPerSecond, nil);
    end;
  end;
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
  Surface: ISkSurface;
begin
  // Call inherited Paint
  inherited;

  // Create Skia surface directly from Canvas HDC for zero-copy rendering
  Surface := TSkSurface.MakeFromHDC(Canvas.Handle, Width, Height);
  if Assigned(Surface) then
  begin
    // Call child class to paint using Skia canvas
    PaintSkia(Surface.Canvas);
    // Flush ensures all drawing commands are executed
    Surface.Flush;
  end;
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Resize;
begin
  // Call inherited Resize
  inherited;
  // Trigger repaint with new size
  Invalidate;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Loaded;
begin
  inherited;
  // Create timer for animation support (if FPS > 0)
  if (FFramesPerSecond > 0) and not (csDesigning in ComponentState) then
    FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FFramesPerSecond, nil);
end;

//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDCustomControl.UpdateStyleElements;
begin
  // Call inherited
  inherited;
  // Trigger repaint with new style
  Invalidate;
end;

//------------------------------------------------------------------------------
// TIMER MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDCustomControl.TimerProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_TIMER then
  begin
    // Trigger a repaint (for animations)
    if (FTimerHandle <> 0) then Invalidate;
  end else
    // Pass message to default message handler
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

//------------------------------------------------------------------------------
// PAINT SKIA (Virtual method for child classes to override)
//------------------------------------------------------------------------------
procedure TOBDCustomControl.PaintSkia(Canvas: ISkCanvas);
begin
  // Default implementation: Clear with background color
  Canvas.Clear(TAlphaColorRec.Null);
  // Child classes should override this to provide their rendering
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDCustomControl.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Prevent background erasure for smoother rendering and reduced flickering
  ControlStyle := ControlStyle + [csOpaque];
  // Set initial FPS (for animation support)
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

