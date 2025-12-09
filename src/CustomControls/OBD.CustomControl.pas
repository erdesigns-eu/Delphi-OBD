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
//                  09/12/2025 - Added double buffering to prevent flickering
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : This component serves as a base for Skia-rendered components.
//                  It provides timer-based rendering for animations and on-demand
//                  rendering for static content. Child classes override PaintSkia
//                  to render directly using Skia's canvas. Double buffering is used
//                  to prevent flickering: content is rendered to a back buffer first,
//                  then copied atomically to the screen.
//------------------------------------------------------------------------------
unit OBD.CustomControl;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, System.Skia, Vcl.Skia, Vcl.ExtCtrls, System.UITypes, System.Types;

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
  TOBDCustomControl = class(TSkCustomControl)
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
    /// <summary>
    ///   Back buffer surface for double buffering (prevents flickering)
    /// </summary>
    FBackBuffer: ISkSurface;
    /// <summary>
    ///   Back buffer image snapshot
    /// </summary>
    FBackBufferImage: ISkImage;
    /// <summary>
    ///   Flag indicating the back buffer needs to be recreated
    /// </summary>
    FBackBufferInvalid: Boolean;
  private
    /// <summary>
    ///   Frames per second
    /// </summary>
    FFramesPerSecond: Integer;

    /// <summary>
    ///   Set frames per second
    /// </summary>
    procedure SetFramesPerSecond(Value: Integer);
    /// <summary>
    ///   Invalidate the back buffer (force recreation on next draw)
    /// </summary>
    procedure InvalidateBackBuffer;

  protected
    /// <summary>
    ///   Override Draw method from TSkCustomControl for Skia rendering
    /// </summary>
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    /// <summary>
    ///   Override Resize method
    /// </summary>
    procedure Resize; override;
    /// <summary>
    ///   Override Loaded method
    /// </summary>
    procedure Loaded; override;
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
// INVALIDATE BACK BUFFER
//------------------------------------------------------------------------------
procedure TOBDCustomControl.InvalidateBackBuffer;
begin
  FBackBufferInvalid := True;
end;

//------------------------------------------------------------------------------
// DRAW (Override from TSkCustomControl)
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  BufferCanvas: ISkCanvas;
begin
  // Recreate back buffer if size changed or first draw
  if not Assigned(FBackBuffer) or 
     (FBackBuffer.Width <> Width) or (FBackBuffer.Height <> Height) then
  begin
    // Create a new back buffer surface with current dimensions
    FBackBuffer := TSkSurface.MakeRaster(Width, Height);
    FBackBufferInvalid := False;
  end;
  
  // Always render to the back buffer first (prevents flickering)
  if Assigned(FBackBuffer) then
  begin
    // Get the canvas from the back buffer
    BufferCanvas := FBackBuffer.Canvas;
    // Render to the back buffer
    PaintSkia(BufferCanvas);
    // Create an immutable snapshot for atomic display
    FBackBufferImage := FBackBuffer.MakeImageSnapshot;
    
    // Draw the back buffer image to the screen (atomic operation, no flickering)
    if Assigned(FBackBufferImage) then
      ACanvas.DrawImage(FBackBufferImage, 0, 0);
  end;
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDCustomControl.Resize;
begin
  // Call inherited Resize
  inherited;
  // Invalidate back buffer so it gets recreated with new size
  InvalidateBackBuffer;
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
  // Set initial FPS (for animation support)
  FFramesPerSecond := DEFAULT_FPS;
  // Mark back buffer as invalid (needs to be created on first draw)
  FBackBufferInvalid := True;
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

