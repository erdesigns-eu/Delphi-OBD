//------------------------------------------------------------------------------
//  OBD.Logger.Sinks
//
//  Log-sink interface + the small "ships with the package" sink
//  set: console (stdout / stderr split by severity), file (with
//  optional rotation), and an in-memory ring buffer useful for
//  test fixtures.
//
//  Hosts wire their own sinks by implementing
//  <see cref="IOBDLogSink"/> and registering through
//  <c>TOBDLogger.RegisterSink</c>. The visual TOBDLogViewer in
//  <c>OBD.UI.LogViewer</c> implements the same interface so a
//  drop-on-form log viewer can be added without extra glue.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Logger.Sinks.
//------------------------------------------------------------------------------

unit OBD.Logger.Sinks;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;

type
  /// <summary>Log severity level.</summary>
  TOBDLogLevel = (
    /// <summary>Verbose debug-only tracing.</summary>
    olDebug,
    /// <summary>Informational message.</summary>
    olInfo,
    /// <summary>Warning — recoverable anomaly.</summary>
    olWarning,
    /// <summary>Error — single-operation failure.</summary>
    olError,
    /// <summary>Critical — fatal / shutdown-worthy.</summary>
    olCritical
  );

  /// <summary>One log event passed to every registered sink.</summary>
  TOBDLogEvent = record
    /// <summary>Severity level.</summary>
    Level: TOBDLogLevel;
    /// <summary>Local timestamp at which the event was raised.</summary>
    Timestamp: TDateTime;
    /// <summary>Free-form category tag (e.g. <c>'Connection'</c>,
    /// <c>'UDS'</c>, <c>'Flash'</c>); empty when not categorised.</summary>
    Category: string;
    /// <summary>Message body.</summary>
    Message: string;
  end;

  /// <summary>
  ///   Sink contract — any host-supplied target that accepts log
  ///   events.
  /// </summary>
  /// <remarks>
  ///   Implementations may be reference-counted (e.g.
  ///   <c>TInterfacedObject</c> sinks) or component-attached
  ///   (e.g. a <c>TComponent</c> that exposes a no-op
  ///   <c>_AddRef</c> / <c>_Release</c>). The logger holds
  ///   interface references so RAII rules apply on the sink
  ///   side.
  /// </remarks>
  IOBDLogSink = interface
    ['{4DAA1F1B-9D2B-4A28-9B0F-2C8C5F5C7D60}']
    /// <summary>Writes one log event.</summary>
    /// <param name="AEvent">Event to record.</param>
    procedure Write(const AEvent: TOBDLogEvent);
    /// <summary>Flushes any pending output (file flush, queue
    /// drain, …).</summary>
    procedure Flush;
  end;

  /// <summary>
  ///   Routes events to stdout (debug / info) and stderr
  ///   (warning / error / critical).
  /// </summary>
  TOBDLogConsoleSink = class(TInterfacedObject, IOBDLogSink)
  strict private
    FLock: TCriticalSection;
  public
    /// <summary>Constructs the sink.</summary>
    constructor Create;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;
    /// <summary>Writes <c>AEvent</c> to the matching console
    /// stream.</summary>
    /// <param name="AEvent">Event to write.</param>
    procedure Write(const AEvent: TOBDLogEvent);
    /// <summary>Flushes both console streams.</summary>
    procedure Flush;
  end;

  /// <summary>
  ///   Appends events to a UTF-8 text file, one line per event.
  ///   When <see cref="MaxBytes"/> is non-zero the file is
  ///   rotated to <c>&lt;name&gt;.1</c>, <c>&lt;name&gt;.2</c>
  ///   …, dropping the oldest beyond <see cref="MaxBackups"/>.
  /// </summary>
  TOBDLogFileSink = class(TInterfacedObject, IOBDLogSink)
  strict private
    FPath: string;
    FMaxBytes: Int64;
    FMaxBackups: Integer;
    FStream: TFileStream;
    FLock: TCriticalSection;
    procedure OpenForAppend;
    procedure RotateIfNeeded(AAdditional: Integer);
  public
    /// <summary>Constructs the sink against <c>APath</c>.</summary>
    /// <param name="APath">Target file path.</param>
    /// <param name="AMaxBytes">Rotation threshold (0 = no
    /// rotation; default 10 MiB).</param>
    /// <param name="AMaxBackups">Backup-file count (default 5).</param>
    constructor Create(const APath: string;
      AMaxBytes: Int64 = 10 * 1024 * 1024;
      AMaxBackups: Integer = 5);
    /// <summary>Closes the file and frees state.</summary>
    destructor Destroy; override;
    /// <summary>Appends one event as a UTF-8 line.</summary>
    /// <param name="AEvent">Event to append.</param>
    procedure Write(const AEvent: TOBDLogEvent);
    /// <summary>Flushes the underlying stream to disk.</summary>
    procedure Flush;
    /// <summary>Configured rotation byte threshold.</summary>
    property MaxBytes: Int64 read FMaxBytes;
    /// <summary>Configured backup-file count.</summary>
    property MaxBackups: Integer read FMaxBackups;
  end;

  /// <summary>
  ///   Holds the last <c>Capacity</c> events in memory. Useful
  ///   from test fixtures and for "show me the last N log
  ///   lines" host UIs.
  /// </summary>
  TOBDLogMemorySink = class(TInterfacedObject, IOBDLogSink)
  strict private
    FBuffer: TList<TOBDLogEvent>;
    FCapacity: Integer;
    FLock: TCriticalSection;
  public
    /// <summary>Constructs the sink.</summary>
    /// <param name="ACapacity">Ring-buffer capacity. Default
    /// 256.</param>
    constructor Create(ACapacity: Integer = 256);
    /// <summary>Frees state.</summary>
    destructor Destroy; override;
    /// <summary>Stores one event.</summary>
    /// <param name="AEvent">Event to store.</param>
    procedure Write(const AEvent: TOBDLogEvent);
    /// <summary>No-op (memory sink has nothing to flush).</summary>
    procedure Flush;
    /// <summary>Snapshot of the current buffer in event order.</summary>
    /// <returns>Array of events.</returns>
    function Snapshot: TArray<TOBDLogEvent>;
    /// <summary>Number of currently-buffered events.</summary>
    function Count: Integer;
  end;

/// <summary>
///   Formats <c>AEvent</c> as a single text line
///   (<c>HH:MM:SS.zzz [LEVEL] [category] message</c>).
/// </summary>
/// <param name="AEvent">Event to format.</param>
/// <returns>Formatted line (no trailing newline).</returns>
function FormatLogLine(const AEvent: TOBDLogEvent): string;

/// <summary>Returns the short uppercase tag for a level
/// (<c>DEBUG</c> / <c>INFO</c> / <c>WARN</c> / <c>ERROR</c> /
/// <c>FATAL</c>).</summary>
/// <param name="ALevel">Severity.</param>
/// <returns>Tag string.</returns>
function LogLevelTag(ALevel: TOBDLogLevel): string;

implementation

uses
  System.IOUtils,
  System.DateUtils;

function LogLevelTag(ALevel: TOBDLogLevel): string;
begin
  case ALevel of
    olDebug:    Result := 'DEBUG';
    olInfo:     Result := 'INFO ';
    olWarning:  Result := 'WARN ';
    olError:    Result := 'ERROR';
    olCritical: Result := 'FATAL';
  else
    Result := '?????';
  end;
end;

function FormatLogLine(const AEvent: TOBDLogEvent): string;
var
  Cat: string;
begin
  if AEvent.Category <> '' then
    Cat := ' [' + AEvent.Category + ']'
  else
    Cat := '';
  Result := Format('%s [%s]%s %s', [
    FormatDateTime('hh:nn:ss.zzz', AEvent.Timestamp),
    LogLevelTag(AEvent.Level),
    Cat,
    AEvent.Message]);
end;

{ ---- TOBDLogConsoleSink --------------------------------------------------- }

constructor TOBDLogConsoleSink.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TOBDLogConsoleSink.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TOBDLogConsoleSink.Write(const AEvent: TOBDLogEvent);
var
  Line: string;
begin
  Line := FormatLogLine(AEvent);
  FLock.Enter;
  try
    if AEvent.Level >= olWarning then
      System.Writeln(ErrOutput, Line)
    else
      System.Writeln(Line);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDLogConsoleSink.Flush;
begin
  // System.Writeln auto-flushes on console handles.
end;

{ ---- TOBDLogFileSink ------------------------------------------------------ }

constructor TOBDLogFileSink.Create(const APath: string;
  AMaxBytes: Int64; AMaxBackups: Integer);
begin
  inherited Create;
  FPath := APath;
  FMaxBytes := AMaxBytes;
  FMaxBackups := AMaxBackups;
  FLock := TCriticalSection.Create;
  OpenForAppend;
end;

destructor TOBDLogFileSink.Destroy;
begin
  FreeAndNil(FStream);
  FLock.Free;
  inherited;
end;

procedure TOBDLogFileSink.OpenForAppend;
var
  Mode: Word;
begin
  if TFile.Exists(FPath) then
    Mode := fmOpenReadWrite or fmShareDenyWrite
  else
    Mode := fmCreate or fmShareDenyWrite;
  FStream := TFileStream.Create(FPath, Mode);
  FStream.Seek(0, soEnd);
end;

procedure TOBDLogFileSink.RotateIfNeeded(AAdditional: Integer);
var
  I: Integer;
  From_: string;
  To_: string;
begin
  if FMaxBytes <= 0 then
    Exit;
  if FStream.Size + AAdditional <= FMaxBytes then
    Exit;
  FreeAndNil(FStream);
  // Slide existing backups: .N-1 -> .N, dropping the oldest.
  for I := FMaxBackups downto 1 do
  begin
    From_ := FPath + '.' + IntToStr(I - 1);
    To_ := FPath + '.' + IntToStr(I);
    if I = 1 then
      From_ := FPath;
    if TFile.Exists(To_) then
      TFile.Delete(To_);
    if TFile.Exists(From_) then
      TFile.Move(From_, To_);
  end;
  OpenForAppend;
end;

procedure TOBDLogFileSink.Write(const AEvent: TOBDLogEvent);
var
  Line: TBytes;
begin
  Line := TEncoding.UTF8.GetBytes(FormatLogLine(AEvent) + sLineBreak);
  FLock.Enter;
  try
    RotateIfNeeded(Length(Line));
    FStream.WriteBuffer(Line[0], Length(Line));
  finally
    FLock.Leave;
  end;
end;

procedure TOBDLogFileSink.Flush;
begin
  FLock.Enter;
  try
    // TFileStream flushes on close; no separate sync API in the
    // RTL — host can call Free to force.
  finally
    FLock.Leave;
  end;
end;

{ ---- TOBDLogMemorySink ---------------------------------------------------- }

constructor TOBDLogMemorySink.Create(ACapacity: Integer);
begin
  inherited Create;
  if ACapacity < 1 then
    ACapacity := 1;
  FCapacity := ACapacity;
  FBuffer := TList<TOBDLogEvent>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TOBDLogMemorySink.Destroy;
begin
  FBuffer.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDLogMemorySink.Write(const AEvent: TOBDLogEvent);
begin
  FLock.Enter;
  try
    FBuffer.Add(AEvent);
    while FBuffer.Count > FCapacity do
      FBuffer.Delete(0);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDLogMemorySink.Flush;
begin
  // No-op.
end;

function TOBDLogMemorySink.Snapshot: TArray<TOBDLogEvent>;
begin
  FLock.Enter;
  try
    Result := FBuffer.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TOBDLogMemorySink.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FBuffer.Count;
  finally
    FLock.Leave;
  end;
end;

end.
