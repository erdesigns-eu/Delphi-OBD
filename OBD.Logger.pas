//------------------------------------------------------------------------------
// UNIT           : OBD.Logger.pas
// CONTENTS       : Error Logging System with Severity Levels and Rotation
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.Logger;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.IOUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Log severity levels
  /// </summary>
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  /// <summary>
  ///   Log event handler
  /// </summary>
  TLogEvent = procedure(const Level: TLogLevel; const Message: string) of object;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Logger with file rotation and severity levels
  /// </summary>
  TOBDLogger = class
  private
    /// <summary>
    ///   Critical section for thread-safe logging
    /// </summary>
    FCriticalSection: TCriticalSection;
    /// <summary>
    ///   Log file path
    /// </summary>
    FLogFilePath: string;
    /// <summary>
    ///   Maximum log file size in bytes (default: 10MB)
    /// </summary>
    FMaxLogSize: Int64;
    /// <summary>
    ///   Number of backup log files to keep
    /// </summary>
    FMaxBackups: Integer;
    /// <summary>
    ///   Minimum log level to record
    /// </summary>
    FMinLogLevel: TLogLevel;
    /// <summary>
    ///   Debug mode flag
    /// </summary>
    FDebugMode: Boolean;
    /// <summary>
    ///   Event handler for log messages
    /// </summary>
    FOnLog: TLogEvent;

    /// <summary>
    ///   Get log level name as string
    /// </summary>
    function GetLevelName(const Level: TLogLevel): string;
    /// <summary>
    ///   Check if log rotation is needed
    /// </summary>
    procedure CheckRotation;
    /// <summary>
    ///   Perform log file rotation
    /// </summary>
    procedure RotateLogs;
    /// <summary>
    ///   Write message to log file
    /// </summary>
    procedure WriteToFile(const Message: string);

  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(const LogFilePath: string = '');
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Log a message with specified level
    /// </summary>
    procedure Log(const Level: TLogLevel; const Message: string); overload;
    /// <summary>
    ///   Log a formatted message with specified level
    /// </summary>
    procedure Log(const Level: TLogLevel; const Format: string; const Args: array of const); overload;

    /// <summary>
    ///   Log debug message (only if debug mode enabled)
    /// </summary>
    procedure Debug(const Message: string); overload;
    procedure Debug(const Format: string; const Args: array of const); overload;

    /// <summary>
    ///   Log info message
    /// </summary>
    procedure Info(const Message: string); overload;
    procedure Info(const Format: string; const Args: array of const); overload;

    /// <summary>
    ///   Log warning message
    /// </summary>
    procedure Warning(const Message: string); overload;
    procedure Warning(const Format: string; const Args: array of const); overload;

    /// <summary>
    ///   Log error message
    /// </summary>
    procedure Error(const Message: string); overload;
    procedure Error(const Format: string; const Args: array of const); overload;

    /// <summary>
    ///   Log critical message
    /// </summary>
    procedure Critical(const Message: string); overload;
    procedure Critical(const Format: string; const Args: array of const); overload;

    /// <summary>
    ///   Clear log file
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Maximum log file size (default: 10MB)
    /// </summary>
    property MaxLogSize: Int64 read FMaxLogSize write FMaxLogSize;
    /// <summary>
    ///   Number of backup files to keep (default: 5)
    /// </summary>
    property MaxBackups: Integer read FMaxBackups write FMaxBackups;
    /// <summary>
    ///   Minimum log level to record (default: llInfo)
    /// </summary>
    property MinLogLevel: TLogLevel read FMinLogLevel write FMinLogLevel;
    /// <summary>
    ///   Enable debug mode (logs debug messages)
    /// </summary>
    property DebugMode: Boolean read FDebugMode write FDebugMode;
    /// <summary>
    ///   Event handler for log messages
    /// </summary>
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

var
  /// <summary>
  ///   Global logger instance
  /// </summary>
  GlobalLogger: TOBDLogger;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDLogger.Create(const LogFilePath: string);
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  
  // Set default log file path
  if LogFilePath.IsEmpty then
    FLogFilePath := TPath.Combine(TPath.GetDocumentsPath, 'OBD', 'obd.log')
  else
    FLogFilePath := LogFilePath;

  // Create log directory if it doesn't exist
  ForceDirectories(TPath.GetDirectoryName(FLogFilePath));

  // Default settings
  FMaxLogSize := 10 * 1024 * 1024; // 10MB
  FMaxBackups := 5;
  FMinLogLevel := llInfo;
  FDebugMode := False;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDLogger.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// GET LEVEL NAME
//------------------------------------------------------------------------------
function TOBDLogger.GetLevelName(const Level: TLogLevel): string;
begin
  case Level of
    llDebug:    Result := 'DEBUG';
    llInfo:     Result := 'INFO';
    llWarning:  Result := 'WARNING';
    llError:    Result := 'ERROR';
    llCritical: Result := 'CRITICAL';
  else
    Result := 'UNKNOWN';
  end;
end;

//------------------------------------------------------------------------------
// CHECK ROTATION
//------------------------------------------------------------------------------
procedure TOBDLogger.CheckRotation;
var
  FileSize: Int64;
begin
  if not TFile.Exists(FLogFilePath) then
    Exit;

  FileSize := TFile.GetSize(FLogFilePath);
  if FileSize >= FMaxLogSize then
    RotateLogs;
end;

//------------------------------------------------------------------------------
// ROTATE LOGS
//------------------------------------------------------------------------------
procedure TOBDLogger.RotateLogs;
var
  I: Integer;
  OldFile, NewFile: string;
begin
  // Delete oldest backup if exists
  OldFile := FLogFilePath + '.' + IntToStr(FMaxBackups);
  if TFile.Exists(OldFile) then
    TFile.Delete(OldFile);

  // Rotate existing backups
  for I := FMaxBackups - 1 downto 1 do
  begin
    OldFile := FLogFilePath + '.' + IntToStr(I);
    NewFile := FLogFilePath + '.' + IntToStr(I + 1);
    if TFile.Exists(OldFile) then
      TFile.Move(OldFile, NewFile);
  end;

  // Rename current log to .1
  if TFile.Exists(FLogFilePath) then
    TFile.Move(FLogFilePath, FLogFilePath + '.1');
end;

//------------------------------------------------------------------------------
// WRITE TO FILE
//------------------------------------------------------------------------------
procedure TOBDLogger.WriteToFile(const Message: string);
var
  FileStream: TFileStream;
  Bytes: TBytes;
begin
  try
    // Check if rotation is needed
    CheckRotation;

    // Open file for appending (create if doesn't exist)
    if TFile.Exists(FLogFilePath) then
    begin
      FileStream := TFileStream.Create(FLogFilePath, fmOpenWrite or fmShareDenyWrite);
      // Seek to end of file for appending
      FileStream.Seek(0, soEnd);
    end
    else
    begin
      // Create new file
      FileStream := TFileStream.Create(FLogFilePath, fmCreate or fmShareDenyWrite);
    end;

    try

      // Write message
      Bytes := TEncoding.UTF8.GetBytes(Message + sLineBreak);
      FileStream.WriteBuffer(Bytes[0], Length(Bytes));
    finally
      FileStream.Free;
    end;
  except
    // Silently ignore file write errors to prevent logging from causing exceptions
  end;
end;

//------------------------------------------------------------------------------
// LOG
//------------------------------------------------------------------------------
procedure TOBDLogger.Log(const Level: TLogLevel; const Message: string);
var
  LogMessage: string;
  Timestamp: TDateTime;
begin
  // Check if level should be logged
  if Ord(Level) < Ord(FMinLogLevel) then
    Exit;

  // Skip debug messages if not in debug mode
  if (Level = llDebug) and not FDebugMode then
    Exit;

  FCriticalSection.Enter;
  try
    // Format log message with timestamp
    Timestamp := Now;
    LogMessage := Format('[%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Timestamp),
       GetLevelName(Level),
       Message]);

    // Write to file
    WriteToFile(LogMessage);

    // Trigger event if assigned
    if Assigned(FOnLog) then
      FOnLog(Level, Message);
  finally
    FCriticalSection.Leave;
  end;
end;

//------------------------------------------------------------------------------
// LOG (FORMATTED)
//------------------------------------------------------------------------------
procedure TOBDLogger.Log(const Level: TLogLevel; const Format: string; const Args: array of const);
begin
  Log(Level, System.SysUtils.Format(Format, Args));
end;

//------------------------------------------------------------------------------
// DEBUG
//------------------------------------------------------------------------------
procedure TOBDLogger.Debug(const Message: string);
begin
  Log(llDebug, Message);
end;

procedure TOBDLogger.Debug(const Format: string; const Args: array of const);
begin
  Log(llDebug, Format, Args);
end;

//------------------------------------------------------------------------------
// INFO
//------------------------------------------------------------------------------
procedure TOBDLogger.Info(const Message: string);
begin
  Log(llInfo, Message);
end;

procedure TOBDLogger.Info(const Format: string; const Args: array of const);
begin
  Log(llInfo, Format, Args);
end;

//------------------------------------------------------------------------------
// WARNING
//------------------------------------------------------------------------------
procedure TOBDLogger.Warning(const Message: string);
begin
  Log(llWarning, Message);
end;

procedure TOBDLogger.Warning(const Format: string; const Args: array of const);
begin
  Log(llWarning, Format, Args);
end;

//------------------------------------------------------------------------------
// ERROR
//------------------------------------------------------------------------------
procedure TOBDLogger.Error(const Message: string);
begin
  Log(llError, Message);
end;

procedure TOBDLogger.Error(const Format: string; const Args: array of const);
begin
  Log(llError, Format, Args);
end;

//------------------------------------------------------------------------------
// CRITICAL
//------------------------------------------------------------------------------
procedure TOBDLogger.Critical(const Message: string);
begin
  Log(llCritical, Message);
end;

procedure TOBDLogger.Critical(const Format: string; const Args: array of const);
begin
  Log(llCritical, Format, Args);
end;

//------------------------------------------------------------------------------
// CLEAR
//------------------------------------------------------------------------------
procedure TOBDLogger.Clear;
var
  I: Integer;
  FileName: string;
begin
  FCriticalSection.Enter;
  try
    // Delete main log file
    if TFile.Exists(FLogFilePath) then
      TFile.Delete(FLogFilePath);

    // Delete backup files
    for I := 1 to FMaxBackups do
    begin
      FileName := FLogFilePath + '.' + IntToStr(I);
      if TFile.Exists(FileName) then
        TFile.Delete(FileName);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

//------------------------------------------------------------------------------
// INITIALIZATION / FINALIZATION
//------------------------------------------------------------------------------
initialization
  GlobalLogger := TOBDLogger.Create;

finalization
  GlobalLogger.Free;

end.
