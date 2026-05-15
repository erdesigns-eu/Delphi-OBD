//------------------------------------------------------------------------------
//  OBD.Logger
//
//  TOBDLogger — process-wide structured logger. Walks a list of
//  registered <see cref="IOBDLogSink"/> instances on every
//  <c>Log</c> call, applying a configurable minimum-level
//  filter. Reentrant; safe to call from any thread.
//
//  Usage:
//
//      var
//        FileSink: IOBDLogSink;
//      begin
//        FileSink := TOBDLogFileSink.Create('app.log');
//        TOBDLogger.Instance.RegisterSink(FileSink);
//        TOBDLogger.Instance.Info('Started', 'app');
//        ...
//      end;
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Logger.pas.
//------------------------------------------------------------------------------

unit OBD.Logger;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Logger.Sinks;

type
  /// <summary>
  ///   Process-wide singleton logger.
  /// </summary>
  /// <remarks>
  ///   The instance is lazy-constructed on first
  ///   <c>Instance</c> call and freed at unit finalisation.
  ///   Hosts that want a per-app instance can construct one
  ///   directly via <c>TOBDLogger.Create</c>.
  /// </remarks>
  TOBDLogger = class
  strict private
    class var FInstance: TOBDLogger;
  strict private
    FLock: TCriticalSection;
    FSinks: TList<IOBDLogSink>;
    FMinLevel: TOBDLogLevel;
  public
    /// <summary>Constructs an empty logger with
    /// <c>MinLevel = olInfo</c>.</summary>
    constructor Create;
    /// <summary>Frees the sink list (sink instances themselves
    /// follow their own ref-count rules).</summary>
    destructor Destroy; override;

    /// <summary>Singleton accessor.</summary>
    /// <returns>The shared logger instance.</returns>
    class function Instance: TOBDLogger; static;
    /// <summary>Releases the singleton (test hook).</summary>
    class procedure ReleaseInstance; static;

    /// <summary>Adds a sink. Duplicate adds are silently
    /// ignored.</summary>
    /// <param name="ASink">Sink to register.</param>
    procedure RegisterSink(const ASink: IOBDLogSink);

    /// <summary>Removes a sink reference.</summary>
    /// <param name="ASink">Sink to unregister.</param>
    procedure UnregisterSink(const ASink: IOBDLogSink);

    /// <summary>Number of currently-registered sinks.</summary>
    function SinkCount: Integer;

    /// <summary>Flushes every registered sink.</summary>
    procedure FlushAll;

    /// <summary>Writes an event to every registered sink whose
    /// minimum-level gate passes.</summary>
    /// <param name="ALevel">Severity.</param>
    /// <param name="AMessage">Body.</param>
    /// <param name="ACategory">Optional category tag.</param>
    procedure Log(ALevel: TOBDLogLevel; const AMessage: string;
      const ACategory: string = '');

    /// <summary>Shortcut for <c>Log(olDebug, ...)</c>.</summary>
    /// <param name="AMessage">Body.</param>
    /// <param name="ACategory">Optional category tag.</param>
    procedure Debug(const AMessage: string;
      const ACategory: string = '');
    /// <summary>Shortcut for <c>Log(olInfo, ...)</c>.</summary>
    /// <param name="AMessage">Body.</param>
    /// <param name="ACategory">Optional category tag.</param>
    procedure Info(const AMessage: string;
      const ACategory: string = '');
    /// <summary>Shortcut for <c>Log(olWarning, ...)</c>.</summary>
    /// <param name="AMessage">Body.</param>
    /// <param name="ACategory">Optional category tag.</param>
    procedure Warning(const AMessage: string;
      const ACategory: string = '');
    /// <summary>Shortcut for <c>Log(olError, ...)</c>.</summary>
    /// <param name="AMessage">Body.</param>
    /// <param name="ACategory">Optional category tag.</param>
    procedure Error(const AMessage: string;
      const ACategory: string = '');
    /// <summary>Shortcut for <c>Log(olCritical, ...)</c>.</summary>
    /// <param name="AMessage">Body.</param>
    /// <param name="ACategory">Optional category tag.</param>
    procedure Critical(const AMessage: string;
      const ACategory: string = '');

    /// <summary>Minimum severity that reaches the sinks. Events
    /// below this level are dropped before dispatch.</summary>
    property MinLevel: TOBDLogLevel read FMinLevel write FMinLevel;
  end;

implementation

constructor TOBDLogger.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FSinks := TList<IOBDLogSink>.Create;
  FMinLevel := olInfo;
end;

destructor TOBDLogger.Destroy;
begin
  FSinks.Free;
  FLock.Free;
  inherited;
end;

class function TOBDLogger.Instance: TOBDLogger;
begin
  if FInstance = nil then
    FInstance := TOBDLogger.Create;
  Result := FInstance;
end;

class procedure TOBDLogger.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDLogger.RegisterSink(const ASink: IOBDLogSink);
begin
  if ASink = nil then
    Exit;
  FLock.Enter;
  try
    if FSinks.IndexOf(ASink) < 0 then
      FSinks.Add(ASink);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDLogger.UnregisterSink(const ASink: IOBDLogSink);
begin
  if ASink = nil then
    Exit;
  FLock.Enter;
  try
    FSinks.Remove(ASink);
  finally
    FLock.Leave;
  end;
end;

function TOBDLogger.SinkCount: Integer;
begin
  FLock.Enter;
  try
    Result := FSinks.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDLogger.FlushAll;
var
  Sinks: TArray<IOBDLogSink>;
  S: IOBDLogSink;
begin
  FLock.Enter;
  try
    Sinks := FSinks.ToArray;
  finally
    FLock.Leave;
  end;
  for S in Sinks do
    S.Flush;
end;

procedure TOBDLogger.Log(ALevel: TOBDLogLevel; const AMessage: string;
  const ACategory: string);
var
  Event: TOBDLogEvent;
  Sinks: TArray<IOBDLogSink>;
  S: IOBDLogSink;
begin
  if ALevel < FMinLevel then
    Exit;
  Event := Default(TOBDLogEvent);
  Event.Level := ALevel;
  Event.Timestamp := Now;
  Event.Category := ACategory;
  Event.Message := AMessage;
  // Snapshot the sink list under the lock then dispatch without
  // it — sinks may take their own locks and we want to avoid
  // holding two at once.
  FLock.Enter;
  try
    Sinks := FSinks.ToArray;
  finally
    FLock.Leave;
  end;
  for S in Sinks do
  begin
    try
      S.Write(Event);
    except
      // A misbehaving sink must not break logging for the others.
    end;
  end;
end;

procedure TOBDLogger.Debug(const AMessage: string;
  const ACategory: string);
begin
  Log(olDebug, AMessage, ACategory);
end;

procedure TOBDLogger.Info(const AMessage: string;
  const ACategory: string);
begin
  Log(olInfo, AMessage, ACategory);
end;

procedure TOBDLogger.Warning(const AMessage: string;
  const ACategory: string);
begin
  Log(olWarning, AMessage, ACategory);
end;

procedure TOBDLogger.Error(const AMessage: string;
  const ACategory: string);
begin
  Log(olError, AMessage, ACategory);
end;

procedure TOBDLogger.Critical(const AMessage: string;
  const ACategory: string);
begin
  Log(olCritical, AMessage, ACategory);
end;

initialization

finalization
  TOBDLogger.ReleaseInstance;

end.
