//------------------------------------------------------------------------------
// UNIT           : OBD.Logger.Sinks.pas
// CONTENTS       : Pluggable IOBDLogSink + bundled sink implementations
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Sinks let the logger fan out to multiple destinations
//                  without each consumer writing its own dispatch glue.
//                  Bundled implementations:
//
//                    TFileRotationSink   — size-based rotation
//                    TDailyRotationSink  — date-stamped file per day
//                    TJsonLineSink       — JSON-Lines for ELK / Splunk
//                    TConsoleSink        — stdout (CLI tools, CI)
//                    TInMemorySink       — buffer events in RAM (log viewer)
//------------------------------------------------------------------------------
unit OBD.Logger.Sinks;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.IOUtils,
  System.Generics.Collections, System.JSON, System.DateUtils;

type
  /// <summary>
  ///   Log severity. Mirrors <c>OBD.Logger.TLogLevel</c> — kept in sync
  ///   manually so the sinks don't drag the legacy unit into every
  ///   consumer.
  /// </summary>
  TOBDLogLevel = (lsDebug, lsInfo, lsWarning, lsError, lsCritical);

  /// <summary>
  ///   One log event handed to every sink.
  /// </summary>
  TOBDLogEvent = record
    Timestamp: TDateTime;
    Level: TOBDLogLevel;
    Source: string;        // optional logger / subsystem tag
    Message: string;
  end;

  /// <summary>
  ///   Implemented by anything that wants to receive log events.
  /// </summary>
  IOBDLogSink = interface
    ['{D8E1F1C6-3F65-4E5F-8A7C-9C7B5D2A5E10}']
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
  end;

//------------------------------------------------------------------------------
// FILE ROTATION (size-based)
//------------------------------------------------------------------------------
type
  TFileRotationSink = class(TInterfacedObject, IOBDLogSink)
  strict private
    FLock: TCriticalSection;
    FFilePath: string;
    FMaxBytes: Int64;
    FMaxBackups: Integer;
    procedure RotateIfNeeded;
    procedure Append(const S: string);
  public
    constructor Create(const AFilePath: string; AMaxBytes: Int64 = 10 * 1024 * 1024;
      AMaxBackups: Integer = 5);
    destructor Destroy; override;
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
    property FilePath: string read FFilePath;
  end;

//------------------------------------------------------------------------------
// DAILY ROTATION (one file per day)
//------------------------------------------------------------------------------
type
  TDailyRotationSink = class(TInterfacedObject, IOBDLogSink)
  strict private
    FLock: TCriticalSection;
    FDirectory: string;
    FBaseName: string;
    FExtension: string;
    function FileFor(const D: TDateTime): string;
  public
    constructor Create(const ADirectory, ABaseName: string;
      const AExtension: string = '.log');
    destructor Destroy; override;
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
  end;

//------------------------------------------------------------------------------
// JSON-LINES SINK
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   One JSON object per line. Compatible with Filebeat / Vector /
  ///   Splunk JSON ingest.
  /// </summary>
  TJsonLineSink = class(TInterfacedObject, IOBDLogSink)
  strict private
    FLock: TCriticalSection;
    FFilePath: string;
    function EventToJson(const Event: TOBDLogEvent): string;
  public
    constructor Create(const AFilePath: string);
    destructor Destroy; override;
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
  end;

//------------------------------------------------------------------------------
// CONSOLE
//------------------------------------------------------------------------------
type
  TConsoleSink = class(TInterfacedObject, IOBDLogSink)
  public
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
  end;

//------------------------------------------------------------------------------
// IN-MEMORY
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Bounded ring buffer of log events. Useful for the in-app log
  ///   viewer and for tests that need to assert what was logged.
  /// </summary>
  TInMemorySink = class(TInterfacedObject, IOBDLogSink)
  strict private
    FLock: TCriticalSection;
    FCapacity: Integer;
    FEvents: TList<TOBDLogEvent>;
    FOnEvent: TProc<TOBDLogEvent>;
  public
    constructor Create(ACapacity: Integer = 500);
    destructor Destroy; override;
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
    /// <summary>
    ///   Snapshot the buffer (oldest first).
    /// </summary>
    function Snapshot: TArray<TOBDLogEvent>;
    procedure ClearEvents;
    property Capacity: Integer read FCapacity;
    /// <summary>
    ///   Callback fired on every new event (after the buffer is updated).
    ///   Lets the in-app log viewer subscribe without polling.
    /// </summary>
    property OnEvent: TProc<TOBDLogEvent> read FOnEvent write FOnEvent;
  end;

//------------------------------------------------------------------------------
// HELPERS
//------------------------------------------------------------------------------
function LogLevelName(L: TOBDLogLevel): string;

implementation

//------------------------------------------------------------------------------
// LOG LEVEL NAME
//------------------------------------------------------------------------------
function LogLevelName(L: TOBDLogLevel): string;
begin
  case L of
    lsDebug:    Result := 'DEBUG';
    lsInfo:     Result := 'INFO';
    lsWarning:  Result := 'WARNING';
    lsError:    Result := 'ERROR';
    lsCritical: Result := 'CRITICAL';
  else          Result := 'UNKNOWN';
  end;
end;

//------------------------------------------------------------------------------
// FORMAT LINE
//------------------------------------------------------------------------------
function FormatLine(const Event: TOBDLogEvent): string;
begin
  if Event.Source <> '' then
    Result := Format('[%s] [%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Event.Timestamp),
       LogLevelName(Event.Level), Event.Source, Event.Message])
  else
    Result := Format('[%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Event.Timestamp),
       LogLevelName(Event.Level), Event.Message]);
end;

//------------------------------------------------------------------------------
// APPEND UTF8
//------------------------------------------------------------------------------
procedure AppendUtf8(const FilePath, Line: string);
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  ForceDirectories(TPath.GetDirectoryName(FilePath));
  Bytes := TEncoding.UTF8.GetBytes(Line + sLineBreak);
  if TFile.Exists(FilePath) then
  begin
    Stream := TFileStream.Create(FilePath, fmOpenWrite or fmShareDenyWrite);
    try Stream.Seek(0, soEnd); Stream.WriteBuffer(Bytes[0], Length(Bytes));
    finally Stream.Free; end;
  end
  else
  begin
    Stream := TFileStream.Create(FilePath, fmCreate or fmShareDenyWrite);
    try Stream.WriteBuffer(Bytes[0], Length(Bytes));
    finally Stream.Free; end;
  end;
end;

//==============================================================================
// TFileRotationSink
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TFileRotationSink.Create(const AFilePath: string;
  AMaxBytes: Int64; AMaxBackups: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FFilePath := AFilePath;
  FMaxBytes := AMaxBytes;
  FMaxBackups := AMaxBackups;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TFileRotationSink.Destroy;
begin
  FLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// ROTATE IF NEEDED
//------------------------------------------------------------------------------
procedure TFileRotationSink.RotateIfNeeded;
var
  I: Integer;
  Old, NewName: string;
begin
  if not TFile.Exists(FFilePath) then Exit;
  if TFile.GetSize(FFilePath) < FMaxBytes then Exit;

  Old := FFilePath + '.' + IntToStr(FMaxBackups);
  if TFile.Exists(Old) then TFile.Delete(Old);

  for I := FMaxBackups - 1 downto 1 do
  begin
    Old := FFilePath + '.' + IntToStr(I);
    NewName := FFilePath + '.' + IntToStr(I + 1);
    if TFile.Exists(Old) then TFile.Move(Old, NewName);
  end;

  TFile.Move(FFilePath, FFilePath + '.1');
end;

//------------------------------------------------------------------------------
// APPEND
//------------------------------------------------------------------------------
procedure TFileRotationSink.Append(const S: string);
begin
  AppendUtf8(FFilePath, S);
end;

//------------------------------------------------------------------------------
// WRITE
//------------------------------------------------------------------------------
procedure TFileRotationSink.Write(const Event: TOBDLogEvent);
begin
  FLock.Enter;
  try
    RotateIfNeeded;
    try Append(FormatLine(Event)); except {silently ignore I/O errors so logging never raises} end;
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// FLUSH
//------------------------------------------------------------------------------
procedure TFileRotationSink.Flush;
begin
  // Each Write closes the stream so no buffered data exists.
end;

//==============================================================================
// TDailyRotationSink
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TDailyRotationSink.Create(const ADirectory, ABaseName,
  AExtension: string);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FDirectory := ADirectory;
  FBaseName := ABaseName;
  FExtension := AExtension;
  ForceDirectories(FDirectory);
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TDailyRotationSink.Destroy;
begin
  FLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// FILE FOR
//------------------------------------------------------------------------------
function TDailyRotationSink.FileFor(const D: TDateTime): string;
begin
  Result := TPath.Combine(FDirectory,
    FBaseName + '-' + FormatDateTime('yyyymmdd', D) + FExtension);
end;

//------------------------------------------------------------------------------
// WRITE
//------------------------------------------------------------------------------
procedure TDailyRotationSink.Write(const Event: TOBDLogEvent);
begin
  FLock.Enter;
  try
    try AppendUtf8(FileFor(Event.Timestamp), FormatLine(Event)); except end;
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// FLUSH
//------------------------------------------------------------------------------
procedure TDailyRotationSink.Flush;
begin
end;

//==============================================================================
// TJsonLineSink
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TJsonLineSink.Create(const AFilePath: string);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FFilePath := AFilePath;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TJsonLineSink.Destroy;
begin
  FLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// EVENT TO JSON
//------------------------------------------------------------------------------
function TJsonLineSink.EventToJson(const Event: TOBDLogEvent): string;
var
  Obj: TJSONObject;
begin
  // ISO-8601 with milliseconds — what every JSON-log ingester expects.
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('ts',    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', Event.Timestamp));
    Obj.AddPair('level', LogLevelName(Event.Level));
    if Event.Source <> '' then Obj.AddPair('source', Event.Source);
    Obj.AddPair('msg',   Event.Message);
    Result := Obj.ToJSON;
  finally
    Obj.Free;
  end;
end;

//------------------------------------------------------------------------------
// WRITE
//------------------------------------------------------------------------------
procedure TJsonLineSink.Write(const Event: TOBDLogEvent);
begin
  FLock.Enter;
  try
    try AppendUtf8(FFilePath, EventToJson(Event)); except end;
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// FLUSH
//------------------------------------------------------------------------------
procedure TJsonLineSink.Flush;
begin
end;

//==============================================================================
// TConsoleSink
//==============================================================================

//------------------------------------------------------------------------------
// WRITE
//------------------------------------------------------------------------------
procedure TConsoleSink.Write(const Event: TOBDLogEvent);
begin
  // IsConsole is set by the runtime when {APPTYPE CONSOLE} is on; guard
  // so a GUI app accidentally registering this sink doesn't crash on
  // Writeln to a non-existent stdout.
  if IsConsole then
    System.Writeln(FormatLine(Event));
end;

//------------------------------------------------------------------------------
// FLUSH
//------------------------------------------------------------------------------
procedure TConsoleSink.Flush;
begin
end;

//==============================================================================
// TInMemorySink
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TInMemorySink.Create(ACapacity: Integer);
begin
  inherited Create;
  if ACapacity < 1 then ACapacity := 1;
  FLock := TCriticalSection.Create;
  FCapacity := ACapacity;
  FEvents := TList<TOBDLogEvent>.Create;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TInMemorySink.Destroy;
begin
  FEvents.Free;
  FLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// WRITE
//------------------------------------------------------------------------------
procedure TInMemorySink.Write(const Event: TOBDLogEvent);
var
  Cb: TProc<TOBDLogEvent>;
begin
  FLock.Enter;
  try
    while FEvents.Count >= FCapacity do FEvents.Delete(0);
    FEvents.Add(Event);
    Cb := FOnEvent;
  finally
    FLock.Leave;
  end;
  if Assigned(Cb) then
    try Cb(Event); except end;
end;

//------------------------------------------------------------------------------
// FLUSH
//------------------------------------------------------------------------------
procedure TInMemorySink.Flush;
begin
end;

//------------------------------------------------------------------------------
// SNAPSHOT
//------------------------------------------------------------------------------
function TInMemorySink.Snapshot: TArray<TOBDLogEvent>;
begin
  FLock.Enter;
  try Result := FEvents.ToArray; finally FLock.Leave; end;
end;

//------------------------------------------------------------------------------
// CLEAR EVENTS
//------------------------------------------------------------------------------
procedure TInMemorySink.ClearEvents;
begin
  FLock.Enter;
  try FEvents.Clear; finally FLock.Leave; end;
end;

end.
