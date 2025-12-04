//------------------------------------------------------------------------------
// UNIT           : OBD.Component.BindingHelpers.pas
// CONTENTS       : Shared helpers for safe component event binding
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : OpenAI Assistant
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/02/2025
//------------------------------------------------------------------------------
unit OBD.Component.BindingHelpers;

interface

uses
  System.Classes, System.SyncObjs;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Binding operation stage used to describe whether a handler is being
  ///   swapped or restored.
  /// </summary>
  TOBDBindingStage = (bsSwapping, bsRestoring);

  /// <summary>
  ///   Callback signature for observing binding changes performed through the
  ///   helper utilities.
  /// </summary>
  TOBDBindingNotification = reference to procedure(const Owner: TComponent;
    const BindingName: string; Stage: TOBDBindingStage; const HandlerAssigned: Boolean);

  /// <summary>
  ///   Helper record offering thread-safe utilities for swapping and restoring
  ///   component event handlers under a caller-supplied monitor lock.
  /// </summary>
  TOBDBindingHelpers = record
  public
    /// <summary>
    ///   Replace the target event handler with a new one while storing the
    ///   previous handler, protecting the swap with a monitor lock when
    ///   provided.
    /// </summary>
    class procedure SwapHandler<T>(const Lock: TObject; var TargetEvent: T; var StoredHandler: T;
      const NewHandler: T; const BindingName: string = ''; const Owner: TComponent = nil;
      const Notification: TOBDBindingNotification = nil); static;
    /// <summary>
    ///   Restore a previously stored handler to the target event while clearing
    ///   the stored reference under the monitor lock when supplied.
    /// </summary>
    class procedure RestoreHandler<T>(const Lock: TObject; var TargetEvent: T; var StoredHandler: T;
      const BindingName: string = ''; const Owner: TComponent = nil;
      const Notification: TOBDBindingNotification = nil); static;
  end;

implementation

//------------------------------------------------------------------------------
// SWAP HANDLER
//------------------------------------------------------------------------------
class procedure TOBDBindingHelpers.SwapHandler<T>(const Lock: TObject; var TargetEvent: T;
  var StoredHandler: T; const NewHandler: T; const BindingName: string = ''; const Owner: TComponent = nil;
  const Notification: TOBDBindingNotification = nil);
begin
  if Lock <> nil then
    TMonitor.Enter(Lock);
  try
    StoredHandler := TargetEvent;
    TargetEvent := NewHandler;
    if Assigned(Notification) then
      Notification(Owner, BindingName, bsSwapping, TMethod(TargetEvent).Code <> nil);
  finally
    if Lock <> nil then
      TMonitor.Exit(Lock);
  end;
end;

//------------------------------------------------------------------------------
// RESTORE HANDLER
//------------------------------------------------------------------------------
class procedure TOBDBindingHelpers.RestoreHandler<T>(const Lock: TObject; var TargetEvent: T;
  var StoredHandler: T; const BindingName: string = ''; const Owner: TComponent = nil;
  const Notification: TOBDBindingNotification = nil);
begin
  if Lock <> nil then
    TMonitor.Enter(Lock);
  try
    TargetEvent := StoredHandler;
    StoredHandler := Default(T);
    if Assigned(Notification) then
      Notification(Owner, BindingName, bsRestoring, TMethod(TargetEvent).Code <> nil);
  finally
    if Lock <> nil then
      TMonitor.Exit(Lock);
  end;
end;

end.
