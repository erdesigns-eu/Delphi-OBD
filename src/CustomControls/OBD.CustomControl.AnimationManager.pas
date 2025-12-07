//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.AnimationManager.pas
// CONTENTS       : Shared Animation Manager for OBD Custom Controls
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// DESCRIPTION    : Centralized animation manager to optimize timer usage across
//                  multiple animating controls. Uses a single timer and
//                  TStopwatch for high-resolution timing.
//------------------------------------------------------------------------------
unit OBD.CustomControl.AnimationManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Diagnostics,
  Vcl.ExtCtrls, WinApi.Windows, WinApi.Messages;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Interface for controls that support animation
  /// </summary>
  IOBDAnimatable = interface
    ['{B5C8E1A2-9F7D-4B3E-8E5C-2D4A6C7B8E9F}']
    /// <summary>
    ///   Called on each animation tick
    /// </summary>
    procedure AnimationTick(ElapsedMs: Int64);
    /// <summary>
    ///   Returns true if the control has active animations
    /// </summary>
    function IsAnimating: Boolean;
    /// <summary>
    ///   Get the desired frames per second for this control
    /// </summary>
    function GetFramesPerSecond: Integer;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Centralized animation manager
  /// </summary>
  TOBDAnimationManager = class
  private
    /// <summary>
    ///   List of registered animating controls
    /// </summary>
    FAnimatingControls: TList<IOBDAnimatable>;
    /// <summary>
    ///   Stopwatch for high-resolution timing
    /// </summary>
    FStopwatch: TStopwatch;
    /// <summary>
    ///   Last tick time in milliseconds
    /// </summary>
    FLastTickMs: Int64;
    /// <summary>
    ///   Target frames per second
    /// </summary>
    FTargetFPS: Integer;
    /// <summary>
    ///   Window handle for timer messages
    /// </summary>
    FWindowHandle: THandle;
    /// <summary>
    ///   Timer handle
    /// </summary>
    FTimerHandle: THandle;

    /// <summary>
    ///   Timer tick handler
    /// </summary>
    procedure TimerProc(var Msg: TMessage);
    /// <summary>
    ///   Update timer interval based on registered controls
    /// </summary>
    procedure UpdateTimerInterval;
    /// <summary>
    ///   Start the animation timer
    /// </summary>
    procedure StartTimer;
    /// <summary>
    ///   Stop the animation timer
    /// </summary>
    procedure StopTimer;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Register a control for animation updates
    /// </summary>
    procedure RegisterControl(Control: IOBDAnimatable);
    /// <summary>
    ///   Unregister a control from animation updates
    /// </summary>
    procedure UnregisterControl(Control: IOBDAnimatable);
    /// <summary>
    ///   Check if any controls are animating and update timer state
    /// </summary>
    procedure CheckAnimationState;
    /// <summary>
    ///   Get the number of registered controls
    /// </summary>
    function GetControlCount: Integer;
  end;

/// <summary>
///   Get the global animation manager instance
/// </summary>
function AnimationManager: TOBDAnimationManager;

implementation

var
  GlobalAnimationManager: TOBDAnimationManager = nil;

//------------------------------------------------------------------------------
// GET GLOBAL ANIMATION MANAGER
//------------------------------------------------------------------------------
function AnimationManager: TOBDAnimationManager;
begin
  if GlobalAnimationManager = nil then
    GlobalAnimationManager := TOBDAnimationManager.Create;
  Result := GlobalAnimationManager;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDAnimationManager.Create;
begin
  inherited Create;
  FAnimatingControls := TList<IOBDAnimatable>.Create;
  FStopwatch := TStopwatch.Create;
  FLastTickMs := 0;
  FTargetFPS := 60; // Default to 60 FPS
  FWindowHandle := AllocateHWnd(TimerProc);
  FTimerHandle := 0;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDAnimationManager.Destroy;
begin
  StopTimer;
  if FWindowHandle <> 0 then
    DeallocateHWnd(FWindowHandle);
  FAnimatingControls.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// TIMER PROC
//------------------------------------------------------------------------------
procedure TOBDAnimationManager.TimerProc(var Msg: TMessage);
var
  Control: IOBDAnimatable;
  CurrentMs: Int64;
  ElapsedMs: Int64;
  I: Integer;
begin
  if Msg.Msg = WM_TIMER then
  begin
    // Get elapsed time since last tick
    CurrentMs := FStopwatch.ElapsedMilliseconds;
    ElapsedMs := CurrentMs - FLastTickMs;
    FLastTickMs := CurrentMs;

    // Update all animating controls
    // Use reverse iteration to handle removals during iteration
    for I := FAnimatingControls.Count - 1 downto 0 do
    begin
      if I < FAnimatingControls.Count then // Check bounds in case list changed
      begin
        Control := FAnimatingControls[I];
        if Control.IsAnimating then
          Control.AnimationTick(ElapsedMs);
      end;
    end;

    // Check if we should stop the timer (no more active animations)
    CheckAnimationState;
  end
  else
    DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

//------------------------------------------------------------------------------
// UPDATE TIMER INTERVAL
//------------------------------------------------------------------------------
procedure TOBDAnimationManager.UpdateTimerInterval;
var
  Control: IOBDAnimatable;
  MaxFPS: Integer;
begin
  MaxFPS := 30; // Default minimum
  
  // Find the highest FPS requirement
  for Control in FAnimatingControls do
  begin
    if Control.GetFramesPerSecond > MaxFPS then
      MaxFPS := Control.GetFramesPerSecond;
  end;

  // Cap at 60 FPS to avoid excessive CPU usage
  if MaxFPS > 60 then
    MaxFPS := 60;

  FTargetFPS := MaxFPS;

  // Restart timer with new interval if it's running
  if FTimerHandle <> 0 then
  begin
    StopTimer;
    StartTimer;
  end;
end;

//------------------------------------------------------------------------------
// START TIMER
//------------------------------------------------------------------------------
procedure TOBDAnimationManager.StartTimer;
begin
  if FTimerHandle = 0 then
  begin
    FStopwatch := TStopwatch.StartNew;
    FLastTickMs := 0;
    FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FTargetFPS, nil);
  end;
end;

//------------------------------------------------------------------------------
// STOP TIMER
//------------------------------------------------------------------------------
procedure TOBDAnimationManager.StopTimer;
begin
  if FTimerHandle <> 0 then
  begin
    KillTimer(FWindowHandle, FTimerHandle);
    FTimerHandle := 0;
    FStopwatch.Stop;
  end;
end;

//------------------------------------------------------------------------------
// REGISTER CONTROL
//------------------------------------------------------------------------------
procedure TOBDAnimationManager.RegisterControl(Control: IOBDAnimatable);
begin
  if not FAnimatingControls.Contains(Control) then
  begin
    FAnimatingControls.Add(Control);
    UpdateTimerInterval;
    CheckAnimationState;
  end;
end;

//------------------------------------------------------------------------------
// UNREGISTER CONTROL
//------------------------------------------------------------------------------
procedure TOBDAnimationManager.UnregisterControl(Control: IOBDAnimatable);
begin
  if FAnimatingControls.Contains(Control) then
  begin
    FAnimatingControls.Remove(Control);
    UpdateTimerInterval;
    CheckAnimationState;
  end;
end;

//------------------------------------------------------------------------------
// CHECK ANIMATION STATE
//------------------------------------------------------------------------------
procedure TOBDAnimationManager.CheckAnimationState;
var
  Control: IOBDAnimatable;
  HasActiveAnimations: Boolean;
begin
  HasActiveAnimations := False;

  // Check if any control is actively animating
  for Control in FAnimatingControls do
  begin
    if Control.IsAnimating then
    begin
      HasActiveAnimations := True;
      Break;
    end;
  end;

  // Start or stop timer based on animation state
  if HasActiveAnimations then
  begin
    if FTimerHandle = 0 then
      StartTimer;
  end
  else
  begin
    if FTimerHandle <> 0 then
      StopTimer;
  end;
end;

//------------------------------------------------------------------------------
// GET CONTROL COUNT
//------------------------------------------------------------------------------
function TOBDAnimationManager.GetControlCount: Integer;
begin
  Result := FAnimatingControls.Count;
end;

//------------------------------------------------------------------------------
// INITIALIZATION
//------------------------------------------------------------------------------
initialization

//------------------------------------------------------------------------------
// FINALIZATION
//------------------------------------------------------------------------------
finalization
  if GlobalAnimationManager <> nil then
  begin
    GlobalAnimationManager.Free;
    GlobalAnimationManager := nil;
  end;

end.
