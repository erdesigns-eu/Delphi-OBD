// ------------------------------------------------------------------------------
// ERD.Recorder.CSV
//
// Live-data CSV logging and .obdlog CSV export helpers.
//
// TOBDCSVLogger records TOBDLiveData samples in a wide CSV shape
// suited to spreadsheet analysis. TOBDRecordingCSVExporter converts
// TOBDRecorder JSONL captures to a long CSV shape for offline review.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.Recorder.CSV;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF FPC}Math{$ELSE}System.Math{$ENDIF},
{$IFDEF MSWINDOWS}
{$IFDEF FPC}Windows{$ELSE}Winapi.Windows{$ENDIF},
{$ENDIF}
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  ERD.Types,
  ERD.Connection,
  ERD.Protocol.Types,
  ERD.Service.LiveData,
  ERD.Service.Catalog,
  ERD.Recorder;

const
  /// <summary>Default CSV field separator.</summary>
  OBD_CSV_DEFAULT_SEPARATOR = ',';
  /// <summary>Default decimal separator for numeric values.</summary>
  OBD_CSV_DEFAULT_DECIMAL_SEPARATOR = '.';
  /// <summary>Default timestamp format: ISO 8601 local time with milliseconds.</summary>
  OBD_CSV_DEFAULT_TIMESTAMP_FORMAT = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz';
  /// <summary>Default crash-loss bound for live CSV file syncing.</summary>
  OBD_CSV_DEFAULT_FLUSH_INTERVAL_MS = 1000;

type
  /// <summary>Raised on CSV writer, logger or export configuration errors.</summary>
  EOBDCSVError = class(EOBDConfig);

  /// <summary>
  /// Small RFC 4180 CSV writer with configurable separator, decimal
  /// separator, UTF-8 BOM output and CRLF line endings.
  /// </summary>
  TOBDCSVWriter = class
  strict private
    FStream: TStream;
    FOwnsStream: Boolean;
    FSeparator: Char;
    FDecimalSeparator: Char;
    FIncludeUTF8BOM: Boolean;
    FBOMWritten: Boolean;
    procedure ValidateConfig;
    procedure WriteBOMIfNeeded;
    procedure SetSeparator(AValue: Char);
    procedure SetDecimalSeparator(AValue: Char);
  public
    /// <summary>Creates a writer over an existing stream.</summary>
    /// <param name="AStream">Destination stream.</param>
    /// <param name="AOwnsStream">True when the writer owns the stream.</param>
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); overload;
    /// <summary>Creates or truncates a UTF-8 CSV file.</summary>
    /// <param name="AFileName">Destination file.</param>
    /// <param name="AIncludeUTF8BOM">Write an Excel-friendly UTF-8 BOM.</param>
    constructor Create(const AFileName: string;
      AIncludeUTF8BOM: Boolean = True); overload;
    /// <summary>Flushes and optionally frees the destination stream.</summary>
    destructor Destroy; override;

    /// <summary>Quotes one field using RFC 4180 rules.</summary>
    /// <param name="AValue">Field text.</param>
    /// <param name="ASeparator">Configured separator.</param>
    /// <returns>Escaped field text.</returns>
    class function QuoteField(const AValue: string;
      ASeparator: Char = OBD_CSV_DEFAULT_SEPARATOR): string; static;

    /// <summary>Builds one CSV line without a trailing line ending.</summary>
    /// <param name="AFields">Fields to write.</param>
    /// <param name="ASeparator">Configured separator.</param>
    /// <returns>CSV row text.</returns>
    class function BuildRow(const AFields: array of string;
      ASeparator: Char = OBD_CSV_DEFAULT_SEPARATOR): string; static;

    /// <summary>Formats a floating-point value with an invariant decimal separator.</summary>
    /// <param name="AValue">Value to format.</param>
    /// <param name="ADecimalSeparator">Decimal separator to use.</param>
    /// <returns>Formatted number.</returns>
    class function FormatNumber(AValue: Double;
      ADecimalSeparator: Char = OBD_CSV_DEFAULT_DECIMAL_SEPARATOR): string; static;

    /// <summary>Writes one CSV row followed by CRLF.</summary>
    /// <param name="AFields">Fields to write.</param>
    procedure WriteRow(const AFields: array of string);

    /// <summary>Flushes the underlying file handle where the platform exposes one.</summary>
    procedure Flush;

    /// <summary>Field separator. Default is comma.</summary>
    property Separator: Char read FSeparator write SetSeparator;
    /// <summary>Decimal separator used by <see cref="FormatNumber"/>. Default is dot.</summary>
    property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator;
    /// <summary>Write a UTF-8 BOM before the first row.</summary>
    property IncludeUTF8BOM: Boolean read FIncludeUTF8BOM write FIncludeUTF8BOM;
  end;

  /// <summary>Options used by <see cref="TOBDRecordingCSVExporter"/>.</summary>
  TOBDRecordingCSVExportOptions = record
    /// <summary>CSV field separator.</summary>
    Separator: Char;
    /// <summary>Numeric decimal separator.</summary>
    DecimalSeparator: Char;
    /// <summary>Write a UTF-8 BOM to stream/file exports.</summary>
    IncludeUTF8BOM: Boolean;
    /// <summary>Include the standard header row.</summary>
    IncludeHeader: Boolean;
    /// <summary>Timestamp format; empty uses ISO 8601 local time with milliseconds.</summary>
    TimestampFormat: string;
  end;

  /// <summary>
  /// Live-data CSV logger. Subscribe it to a <see cref="TOBDLiveData"/>
  /// instance or call <see cref="PushValue"/> from a simulator/test feed.
  /// Rows are wide: timestamp, elapsed seconds, then one column per
  /// configured PID.
  /// </summary>
  TOBDCSVLogger = class(TComponent)
  strict private
    FLiveData: TOBDLiveData;
    FFileName: string;
    FLoggedPIDs: string;
    FSeparator: Char;
    FDecimalSeparator: Char;
    FIncludeUTF8BOM: Boolean;
    FIncludeUnits: Boolean;
    FTimestampFormat: string;
    FIntervalMs: Cardinal;
    FActive: Boolean;
    FSubscribed: Boolean;
    FLock: TCriticalSection;
    FFileStream: TFileStream;
    FWriter: TOBDCSVWriter;
    FPIDs: TBytes;
    FNames: TArray<string>;
    FUnits: TArray<string>;
    FValues: TArray<string>;
    FSeen: TArray<Boolean>;
    FRowHasData: Boolean;
    FRowTimestamp: TDateTime;
    FStartTimestamp: TDateTime;
    FLastFlushTick: UInt64;
    FOnError: TOBDConnectionErrorEvent;
    procedure SetLiveData(AValue: TOBDLiveData);
    procedure SetSeparator(AValue: Char);
    procedure SetDecimalSeparator(AValue: Char);
    procedure SetLoggedPIDs(const AValue: string);
    procedure SubscribeLiveData;
    procedure UnsubscribeLiveData;
    procedure HandleValue(Sender: TObject; const AValue: TOBDPIDValue);
    procedure PrepareColumns;
    procedure WriteHeaderLocked;
    procedure ClearRowLocked;
    procedure BeginRowLocked(ATimestamp: TDateTime);
    procedure WriteCurrentRowLocked;
    procedure FlushIfDueLocked;
    function IndexOfPID(APID: Byte): Integer;
    function AllColumnsSeen: Boolean;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
  protected
    /// <summary>Clears the live-data reference when the component is destroyed.</summary>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    /// <summary>Creates the logger.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops logging and releases file handles.</summary>
    destructor Destroy; override;

    /// <summary>Starts a new CSV file and fixes the PID column set.</summary>
    procedure Start;
    /// <summary>Stops logging, writes any pending row and flushes the file.</summary>
    procedure Stop;
    /// <summary>Adds one PID to <see cref="LoggedPIDs"/>.</summary>
    /// <param name="APID">Mode 01 PID byte.</param>
    procedure AddPID(APID: Byte);
    /// <summary>Replaces <see cref="LoggedPIDs"/> from an array.</summary>
    /// <param name="APIDs">Mode 01 PID bytes.</param>
    procedure SetPIDs(const APIDs: TBytes);
    /// <summary>Clears the configured PID list.</summary>
    procedure ClearPIDs;
    /// <summary>Records one decoded value. Useful for test feeds and simulators.</summary>
    /// <param name="AValue">Live-data value snapshot.</param>
    procedure PushValue(const AValue: TOBDPIDValue);

    /// <summary>True while the CSV file is open.</summary>
    property Active: Boolean read FActive;
  published
    /// <summary>Live-data source. When assigned, Start subscribes to the configured PIDs.</summary>
    property LiveData: TOBDLiveData read FLiveData write SetLiveData;
    /// <summary>Destination CSV file.</summary>
    property FileName: string read FFileName write FFileName;
    /// <summary>Comma, semicolon or whitespace separated PID list (hex or decimal).</summary>
    property LoggedPIDs: string read FLoggedPIDs write SetLoggedPIDs;
    /// <summary>CSV field separator. Default is comma.</summary>
    property Separator: Char read FSeparator write SetSeparator default OBD_CSV_DEFAULT_SEPARATOR;
    /// <summary>Decimal separator for numbers. Default is dot.</summary>
    property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator default OBD_CSV_DEFAULT_DECIMAL_SEPARATOR;
    /// <summary>Write a UTF-8 BOM for Excel. Default is True.</summary>
    property IncludeUTF8BOM: Boolean read FIncludeUTF8BOM write FIncludeUTF8BOM default True;
    /// <summary>Append units to PID headers, for example Engine RPM [rpm].</summary>
    property IncludeUnits: Boolean read FIncludeUnits write FIncludeUnits default True;
    /// <summary>Timestamp format. Empty falls back to ISO 8601 local time with milliseconds.</summary>
    property TimestampFormat: string read FTimestampFormat write FTimestampFormat;
    /// <summary>Maximum row age before a partial row is emitted, in milliseconds.</summary>
    property IntervalMs: Cardinal read FIntervalMs write FIntervalMs default OBD_CSV_DEFAULT_FLUSH_INTERVAL_MS;
    /// <summary>Fires for transient file-write errors.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

  /// <summary>
  /// Converts <c>.obdlog</c> recorder captures to long-format CSV.
  /// </summary>
  TOBDRecordingCSVExporter = class
  public
    /// <summary>Exports a recorder file to a CSV file.</summary>
    /// <param name="ARecordingFile">Source <c>.obdlog</c> or <c>.obdlog.gz</c>.</param>
    /// <param name="ACSVFile">Destination CSV file.</param>
    /// <param name="AOptions">CSV export options.</param>
    class procedure Export(const ARecordingFile, ACSVFile: string;
      const AOptions: TOBDRecordingCSVExportOptions); static;

    /// <summary>Exports a recorder file to an existing stream.</summary>
    /// <param name="ARecordingFile">Source <c>.obdlog</c> or <c>.obdlog.gz</c>.</param>
    /// <param name="AStream">Destination stream.</param>
    /// <param name="AOptions">CSV export options.</param>
    class procedure ExportToStream(const ARecordingFile: string; AStream: TStream;
      const AOptions: TOBDRecordingCSVExportOptions); static;

    /// <summary>Exports parsed entries to an existing stream.</summary>
    /// <param name="AEntries">Entries to export.</param>
    /// <param name="AStream">Destination stream.</param>
    /// <param name="AOptions">CSV export options.</param>
    class procedure ExportEntriesToStream(const AEntries: array of TOBDLogEntry;
      AStream: TStream; const AOptions: TOBDRecordingCSVExportOptions); static;

    /// <summary>Exports parsed entries to strings, one CSV row per string.</summary>
    /// <param name="AEntries">Entries to export.</param>
    /// <param name="AStrings">Destination strings.</param>
    /// <param name="AOptions">CSV export options.</param>
    class procedure ExportEntriesToStrings(const AEntries: array of TOBDLogEntry;
      AStrings: TStrings; const AOptions: TOBDRecordingCSVExportOptions); static;

    /// <summary>Exports in-memory JSONL recording lines to a stream.</summary>
    /// <param name="ALines">JSONL lines written by TOBDRecorder.</param>
    /// <param name="AStream">Destination stream.</param>
    /// <param name="AOptions">CSV export options.</param>
    class procedure ExportLogLinesToStream(const ALines: TStrings; AStream: TStream;
      const AOptions: TOBDRecordingCSVExportOptions); static;

    /// <summary>Exports in-memory JSONL recording lines to strings.</summary>
    /// <param name="ALines">JSONL lines written by TOBDRecorder.</param>
    /// <param name="AStrings">Destination strings.</param>
    /// <param name="AOptions">CSV export options.</param>
    class procedure ExportLogLinesToStrings(const ALines, AStrings: TStrings;
      const AOptions: TOBDRecordingCSVExportOptions); static;
  end;

/// <summary>Returns the default recording CSV export options.</summary>
/// <returns>Default options.</returns>
function DefaultOBDRecordingCSVExportOptions: TOBDRecordingCSVExportOptions;

implementation

uses
{$IFDEF FPC}{$IFDEF UNIX}BaseUnix, Unix, {$ENDIF}{$ENDIF}
{$IFDEF FPC}DateUtils{$ELSE}System.DateUtils{$ENDIF},
  ERD.Replayer;

const
  OBD_CSV_CRLF = #13#10;
  OBD_CSV_UTF8_BOM0 = $EF;
  OBD_CSV_UTF8_BOM1 = $BB;
  OBD_CSV_UTF8_BOM2 = $BF;
  OBD_CSV_SECONDS_PER_DAY = 86400.0;
  OBD_CSV_MS_PER_DAY = 86400000.0;

function DefaultOBDRecordingCSVExportOptions: TOBDRecordingCSVExportOptions;
begin
  Result.Separator := OBD_CSV_DEFAULT_SEPARATOR;
  Result.DecimalSeparator := OBD_CSV_DEFAULT_DECIMAL_SEPARATOR;
  Result.IncludeUTF8BOM := True;
  Result.IncludeHeader := True;
  Result.TimestampFormat := OBD_CSV_DEFAULT_TIMESTAMP_FORMAT;
end;

procedure ValidateCSVConfig(ASeparator, ADecimalSeparator: Char);
begin
  if ASeparator = #0 then
    raise EOBDCSVError.Create('CSV separator cannot be empty');
  if ADecimalSeparator = #0 then
    raise EOBDCSVError.Create('CSV decimal separator cannot be empty');
  if ASeparator = ADecimalSeparator then
    raise EOBDCSVError.Create('CSV separator and decimal separator must differ');
  if CharInSet(ASeparator, [#10, #13, '"']) then
    raise EOBDCSVError.Create('CSV separator cannot be a quote or line break');
end;

function TimestampFormatOrDefault(const AFormat: string): string;
begin
  if AFormat <> '' then
    Result := AFormat
  else
    Result := OBD_CSV_DEFAULT_TIMESTAMP_FORMAT;
end;

function FormatCSVTimestamp(ATimestamp: TDateTime; const AFormat: string): string;
begin
  if ATimestamp = 0 then
    Result := ''
  else
    Result := FormatDateTime(TimestampFormatOrDefault(AFormat), ATimestamp);
end;

function PIDToText(APID: Byte): string;
begin
  Result := '0x' + IntToHex(APID, 2);
end;

function EntryKindText(AKind: TOBDLogEntryKind): string;
begin
  case AKind of
    leInfo:
      Result := 'info';
    leFrame:
      Result := 'frame';
    leResponse:
      Result := 'response';
    leNRC:
      Result := 'nrc';
    leError:
      Result := 'error';
  else
    Result := 'info';
  end;
end;

procedure ResolvePIDInfo(APID: Byte; out AName, AUnit: string);
var
  Info: TOBDPIDInfo;
begin
  AName := '';
  AUnit := '';
  if TOBDServiceCatalog.Default.TryGetPID(APID, Info) then
  begin
    AName := Info.Description;
    if AName = '' then
      AName := Info.Name;
    AUnit := Info.Decoder.Unit_;
  end;
  if AName <> '' then
    Exit;
  case APID of
    $04:
      begin AName := 'Engine load'; AUnit := '%'; end;
    $05:
      begin AName := 'Engine coolant temperature'; AUnit := 'C'; end;
    $0A:
      begin AName := 'Fuel pressure'; AUnit := 'kPa'; end;
    $0B:
      begin AName := 'Intake manifold pressure'; AUnit := 'kPa'; end;
    $0C:
      begin AName := 'Engine RPM'; AUnit := 'rpm'; end;
    $0D:
      begin AName := 'Vehicle speed'; AUnit := 'km/h'; end;
    $0E:
      begin AName := 'Timing advance'; AUnit := 'deg'; end;
    $0F:
      begin AName := 'Intake air temperature'; AUnit := 'C'; end;
    $10:
      begin AName := 'Mass air flow'; AUnit := 'g/s'; end;
    $11:
      begin AName := 'Throttle position'; AUnit := '%'; end;
    $1F:
      begin AName := 'Run time since engine start'; AUnit := 's'; end;
    $21:
      begin AName := 'Distance with MIL on'; AUnit := 'km'; end;
    $2F:
      begin AName := 'Fuel tank level'; AUnit := '%'; end;
    $33:
      begin AName := 'Barometric pressure'; AUnit := 'kPa'; end;
    $42:
      begin AName := 'Control module voltage'; AUnit := 'V'; end;
    $46:
      begin AName := 'Ambient air temperature'; AUnit := 'C'; end;
    $5C:
      begin AName := 'Engine oil temperature'; AUnit := 'C'; end;
    $5E:
      begin AName := 'Engine fuel rate'; AUnit := 'L/h'; end;
  else
    AName := 'PID ' + PIDToText(APID);
  end;
end;

function TryDecodePIDValue(APID: Byte; const ARaw: TBytes; out AName, AUnit: string;
  out AValue: Double): Boolean;
var
  Info: TOBDPIDInfo;
begin
  ResolvePIDInfo(APID, AName, AUnit);
  if TOBDServiceCatalog.Default.TryGetPID(APID, Info) and
    EvaluatePIDDecoder(Info.Decoder, ARaw, AValue) then
  begin
    if Info.Description <> '' then
      AName := Info.Description
    else if Info.Name <> '' then
      AName := Info.Name;
    AUnit := Info.Decoder.Unit_;
    Exit(True);
  end;
  Result := True;
  AValue := NaN;
  case APID of
    $04:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] * 100.0 / 255.0;
      end;
    $05:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] - 40;
      end;
    $0A:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] * 3;
      end;
    $0B:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0];
      end;
    $0C:
      begin
        if Length(ARaw) < 2 then Exit(False);
        AValue := ((ARaw[0] * 256) + ARaw[1]) / 4.0;
      end;
    $0D:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0];
      end;
    $0E:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := (ARaw[0] / 2.0) - 64.0;
      end;
    $0F:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] - 40;
      end;
    $10:
      begin
        if Length(ARaw) < 2 then Exit(False);
        AValue := ((ARaw[0] * 256) + ARaw[1]) / 100.0;
      end;
    $11:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] * 100.0 / 255.0;
      end;
    $1F:
      begin
        if Length(ARaw) < 2 then Exit(False);
        AValue := (ARaw[0] * 256) + ARaw[1];
      end;
    $21:
      begin
        if Length(ARaw) < 2 then Exit(False);
        AValue := (ARaw[0] * 256) + ARaw[1];
      end;
    $2F:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] * 100.0 / 255.0;
      end;
    $33:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0];
      end;
    $42:
      begin
        if Length(ARaw) < 2 then Exit(False);
        AValue := ((ARaw[0] * 256) + ARaw[1]) / 1000.0;
      end;
    $46:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] - 40;
      end;
    $5C:
      begin
        if Length(ARaw) < 1 then Exit(False);
        AValue := ARaw[0] - 40;
      end;
    $5E:
      begin
        if Length(ARaw) < 2 then Exit(False);
        AValue := ((ARaw[0] * 256) + ARaw[1]) * 0.05;
      end;
  else
    Result := False;
  end;
end;

function ParsePIDToken(const AToken: string): Byte;
var
  S: string;
  I, Code: Integer;
  Hex: Boolean;
begin
  S := Trim(AToken);
  if S = '' then
    raise EOBDCSVError.Create('Empty PID token');
  Hex := False;
  if (Length(S) > 1) and (S[1] = '$') then
  begin
    S := Copy(S, 2, MaxInt);
    Hex := True;
  end
  else if (Length(S) > 2) and (S[1] = '0') and CharInSet(S[2], ['x', 'X']) then
  begin
    S := Copy(S, 3, MaxInt);
    Hex := True;
  end
  else
    for I := 1 to Length(S) do
      if CharInSet(S[I], ['A' .. 'F', 'a' .. 'f']) then
        Hex := True;
  if Hex then
    Val('$' + S, I, Code)
  else
    Val(S, I, Code);
  if (Code <> 0) or (I < 0) or (I > 255) then
    raise EOBDCSVError.CreateFmt('Invalid PID token "%s"', [AToken]);
  Result := Byte(I);
end;

function ParsePIDList(const AText: string): TBytes;
var
  List: TList<Byte>;
  Token: string;
  I: Integer;
  PID: Byte;
  C: Char;
  procedure AddToken;
  begin
    if Trim(Token) = '' then
      Exit;
    PID := ParsePIDToken(Token);
    if List.IndexOf(PID) < 0 then
      List.Add(PID);
    Token := '';
  end;
begin
  List := TList<Byte>.Create;
  try
    Token := '';
    for I := 1 to Length(AText) do
    begin
      C := AText[I];
      if CharInSet(C, [',', ';', ' ', #9, #10, #13]) then
        AddToken
      else
        Token := Token + C;
    end;
    AddToken;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

{ ---- TOBDCSVWriter ---------------------------------------------------------- }

constructor TOBDCSVWriter.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  if AStream = nil then
    raise EArgumentNilException.Create('TOBDCSVWriter stream required');
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FSeparator := OBD_CSV_DEFAULT_SEPARATOR;
  FDecimalSeparator := OBD_CSV_DEFAULT_DECIMAL_SEPARATOR;
  FIncludeUTF8BOM := False;
end;

constructor TOBDCSVWriter.Create(const AFileName: string; AIncludeUTF8BOM: Boolean);
begin
  Create(TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite), True);
  FIncludeUTF8BOM := AIncludeUTF8BOM;
end;

destructor TOBDCSVWriter.Destroy;
begin
  try
    Flush;
  except
  end;
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

procedure TOBDCSVWriter.ValidateConfig;
begin
  ValidateCSVConfig(FSeparator, FDecimalSeparator);
end;

procedure TOBDCSVWriter.SetSeparator(AValue: Char);
begin
  if FSeparator = AValue then
    Exit;
  ValidateCSVConfig(AValue, FDecimalSeparator);
  FSeparator := AValue;
end;

procedure TOBDCSVWriter.SetDecimalSeparator(AValue: Char);
begin
  if FDecimalSeparator = AValue then
    Exit;
  ValidateCSVConfig(FSeparator, AValue);
  FDecimalSeparator := AValue;
end;

procedure TOBDCSVWriter.WriteBOMIfNeeded;
var
  Bom: array [0 .. 2] of Byte;
begin
  if FBOMWritten then
    Exit;
  FBOMWritten := True;
  if not FIncludeUTF8BOM then
    Exit;
  if (FStream <> nil) and (FStream.Position <> 0) then
    Exit;
  Bom[0] := OBD_CSV_UTF8_BOM0;
  Bom[1] := OBD_CSV_UTF8_BOM1;
  Bom[2] := OBD_CSV_UTF8_BOM2;
  FStream.WriteBuffer(Bom[0], Length(Bom));
end;

class function TOBDCSVWriter.QuoteField(const AValue: string;
  ASeparator: Char): string;
var
  I: Integer;
  NeedsQuote: Boolean;
begin
  NeedsQuote := False;
  for I := 1 to Length(AValue) do
    if (AValue[I] = ASeparator) or (AValue[I] = '"') or
      (AValue[I] = #13) or (AValue[I] = #10) then
    begin
      NeedsQuote := True;
      Break;
    end;
  if not NeedsQuote then
    Exit(AValue);
  Result := '"';
  for I := 1 to Length(AValue) do
  begin
    if AValue[I] = '"' then
      Result := Result + '""'
    else
      Result := Result + AValue[I];
  end;
  Result := Result + '"';
end;

class function TOBDCSVWriter.BuildRow(const AFields: array of string;
  ASeparator: Char): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AFields) do
  begin
    if I > 0 then
      Result := Result + ASeparator;
    Result := Result + QuoteField(AFields[I], ASeparator);
  end;
end;

class function TOBDCSVWriter.FormatNumber(AValue: Double;
  ADecimalSeparator: Char): string;
var
  Settings: TFormatSettings;
begin
  Settings := TFormatSettings.Invariant;
  Settings.DecimalSeparator := ADecimalSeparator;
  Result := FloatToStr(AValue, Settings);
end;

procedure TOBDCSVWriter.WriteRow(const AFields: array of string);
var
  Line: string;
  Bytes: TBytes;
begin
  ValidateConfig;
  WriteBOMIfNeeded;
  Line := BuildRow(AFields, FSeparator) + OBD_CSV_CRLF;
  Bytes := TEncoding.UTF8.GetBytes(Line);
  if Length(Bytes) > 0 then
    FStream.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure TOBDCSVWriter.Flush;
begin
  if FStream = nil then
    Exit;
  if FStream is TFileStream then
  begin
{$IFDEF MSWINDOWS}
    if not FlushFileBuffers(TFileStream(FStream).Handle) then
      RaiseLastOSError;
{$ELSE}
{$IFDEF FPC}{$IFDEF UNIX}
    if fpFsync(TFileStream(FStream).Handle) <> 0 then
      RaiseLastOSError;
{$ENDIF}{$ENDIF}
{$ENDIF}
  end;
end;

{ ---- TOBDCSVLogger ---------------------------------------------------------- }

constructor TOBDCSVLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FSeparator := OBD_CSV_DEFAULT_SEPARATOR;
  FDecimalSeparator := OBD_CSV_DEFAULT_DECIMAL_SEPARATOR;
  FIncludeUTF8BOM := True;
  FIncludeUnits := True;
  FTimestampFormat := OBD_CSV_DEFAULT_TIMESTAMP_FORMAT;
  FIntervalMs := OBD_CSV_DEFAULT_FLUSH_INTERVAL_MS;
end;

destructor TOBDCSVLogger.Destroy;
begin
  Stop;
  if FLiveData <> nil then
    FLiveData.RemoveFreeNotification(Self);
  FLock.Free;
  inherited;
end;

procedure TOBDCSVLogger.SetLiveData(AValue: TOBDLiveData);
begin
  if FLiveData = AValue then
    Exit;
  UnsubscribeLiveData;
  if FLiveData <> nil then
    FLiveData.RemoveFreeNotification(Self);
  FLiveData := AValue;
  if FLiveData <> nil then
    FLiveData.FreeNotification(Self);
  SubscribeLiveData;
end;

procedure TOBDCSVLogger.SetSeparator(AValue: Char);
begin
  if FSeparator = AValue then
    Exit;
  ValidateCSVConfig(AValue, FDecimalSeparator);
  FSeparator := AValue;
end;

procedure TOBDCSVLogger.SetDecimalSeparator(AValue: Char);
begin
  if FDecimalSeparator = AValue then
    Exit;
  ValidateCSVConfig(FSeparator, AValue);
  FDecimalSeparator := AValue;
end;

procedure TOBDCSVLogger.SetLoggedPIDs(const AValue: string);
begin
  if FActive then
    raise EOBDCSVError.Create('Cannot change LoggedPIDs while CSV logger is active');
  FLoggedPIDs := AValue;
end;

procedure TOBDCSVLogger.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLiveData) then
  begin
    UnsubscribeLiveData;
    FLiveData := nil;
  end;
end;

procedure TOBDCSVLogger.SubscribeLiveData;
var
  I: Integer;
begin
  if (FLiveData = nil) or not FActive or FSubscribed then
    Exit;
  for I := 0 to High(FPIDs) do
    FLiveData.Subscribe(FPIDs[I], HandleValue);
  FSubscribed := True;
end;

procedure TOBDCSVLogger.UnsubscribeLiveData;
var
  I: Integer;
begin
  if (FLiveData = nil) or not FSubscribed then
  begin
    FSubscribed := False;
    Exit;
  end;
  for I := 0 to High(FPIDs) do
    FLiveData.Unsubscribe(FPIDs[I], HandleValue);
  FSubscribed := False;
end;

procedure TOBDCSVLogger.PrepareColumns;
var
  I: Integer;
begin
  FPIDs := ParsePIDList(FLoggedPIDs);
  if Length(FPIDs) = 0 then
    raise EOBDCSVError.Create('TOBDCSVLogger requires at least one LoggedPIDs entry');
  SetLength(FNames, Length(FPIDs));
  SetLength(FUnits, Length(FPIDs));
  SetLength(FValues, Length(FPIDs));
  SetLength(FSeen, Length(FPIDs));
  for I := 0 to High(FPIDs) do
    ResolvePIDInfo(FPIDs[I], FNames[I], FUnits[I]);
  ClearRowLocked;
end;

procedure TOBDCSVLogger.Start;
begin
  if FActive then
    Stop;
  if FFileName = '' then
    raise EOBDCSVError.Create('TOBDCSVLogger FileName is required');
  ValidateCSVConfig(FSeparator, FDecimalSeparator);
  FLock.Enter;
  try
    try
      PrepareColumns;
      FFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
      FWriter := TOBDCSVWriter.Create(FFileStream, False);
      FWriter.Separator := FSeparator;
      FWriter.DecimalSeparator := FDecimalSeparator;
      FWriter.IncludeUTF8BOM := FIncludeUTF8BOM;
      FStartTimestamp := Now;
      FLastFlushTick := TThread.GetTickCount64;
      WriteHeaderLocked;
      FWriter.Flush;
      FActive := True;
    except
      FreeAndNil(FWriter);
      FreeAndNil(FFileStream);
      FActive := False;
      raise;
    end;
  finally
    FLock.Leave;
  end;
  SubscribeLiveData;
end;

procedure TOBDCSVLogger.Stop;
begin
  UnsubscribeLiveData;
  FLock.Enter;
  try
    if FActive and FRowHasData then
      WriteCurrentRowLocked;
    if FWriter <> nil then
    begin
      FWriter.Flush;
      FreeAndNil(FWriter);
    end;
    FreeAndNil(FFileStream);
    FActive := False;
    FRowHasData := False;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDCSVLogger.AddPID(APID: Byte);
begin
  if FActive then
    raise EOBDCSVError.Create('Cannot change PID list while CSV logger is active');
  if FLoggedPIDs <> '' then
    FLoggedPIDs := FLoggedPIDs + ',';
  FLoggedPIDs := FLoggedPIDs + PIDToText(APID);
end;

procedure TOBDCSVLogger.SetPIDs(const APIDs: TBytes);
var
  I: Integer;
begin
  if FActive then
    raise EOBDCSVError.Create('Cannot change PID list while CSV logger is active');
  FLoggedPIDs := '';
  for I := 0 to High(APIDs) do
  begin
    if I > 0 then
      FLoggedPIDs := FLoggedPIDs + ',';
    FLoggedPIDs := FLoggedPIDs + PIDToText(APIDs[I]);
  end;
end;

procedure TOBDCSVLogger.ClearPIDs;
begin
  if FActive then
    raise EOBDCSVError.Create('Cannot change PID list while CSV logger is active');
  FLoggedPIDs := '';
end;

function TOBDCSVLogger.IndexOfPID(APID: Byte): Integer;
begin
  for Result := 0 to High(FPIDs) do
    if FPIDs[Result] = APID then
      Exit;
  Result := -1;
end;

function TOBDCSVLogger.AllColumnsSeen: Boolean;
var
  I: Integer;
begin
  Result := Length(FSeen) > 0;
  for I := 0 to High(FSeen) do
    if not FSeen[I] then
      Exit(False);
end;

procedure TOBDCSVLogger.WriteHeaderLocked;
var
  Fields: TArray<string>;
  I: Integer;
begin
  SetLength(Fields, 2 + Length(FPIDs));
  Fields[0] := 'Timestamp';
  Fields[1] := 'ElapsedSeconds';
  for I := 0 to High(FPIDs) do
  begin
    Fields[I + 2] := FNames[I];
    if Fields[I + 2] = '' then
      Fields[I + 2] := 'PID ' + PIDToText(FPIDs[I]);
    if FIncludeUnits and (FUnits[I] <> '') then
      Fields[I + 2] := Fields[I + 2] + ' [' + FUnits[I] + ']';
  end;
  FWriter.WriteRow(Fields);
end;

procedure TOBDCSVLogger.ClearRowLocked;
var
  I: Integer;
begin
  for I := 0 to High(FValues) do
  begin
    FValues[I] := '';
    FSeen[I] := False;
  end;
  FRowHasData := False;
  FRowTimestamp := 0;
end;

procedure TOBDCSVLogger.BeginRowLocked(ATimestamp: TDateTime);
begin
  ClearRowLocked;
  FRowTimestamp := ATimestamp;
  FRowHasData := True;
end;

procedure TOBDCSVLogger.WriteCurrentRowLocked;
var
  Fields: TArray<string>;
  I: Integer;
  Elapsed: Double;
begin
  if not FRowHasData then
    Exit;
  SetLength(Fields, 2 + Length(FPIDs));
  Fields[0] := FormatCSVTimestamp(FRowTimestamp, FTimestampFormat);
  Elapsed := (FRowTimestamp - FStartTimestamp) * OBD_CSV_SECONDS_PER_DAY;
  if Elapsed < 0 then
    Elapsed := 0;
  Fields[1] := TOBDCSVWriter.FormatNumber(Elapsed, FDecimalSeparator);
  for I := 0 to High(FPIDs) do
    Fields[I + 2] := FValues[I];
  FWriter.WriteRow(Fields);
  ClearRowLocked;
  FlushIfDueLocked;
end;

procedure TOBDCSVLogger.FlushIfDueLocked;
var
  Tick: UInt64;
begin
  Tick := TThread.GetTickCount64;
  if (Tick - FLastFlushTick) >= OBD_CSV_DEFAULT_FLUSH_INTERVAL_MS then
  begin
    FWriter.Flush;
    FLastFlushTick := Tick;
  end;
end;

procedure TOBDCSVLogger.HandleValue(Sender: TObject; const AValue: TOBDPIDValue);
begin
  PushValue(AValue);
end;

procedure TOBDCSVLogger.PushValue(const AValue: TOBDPIDValue);
var
  Index: Integer;
  Now_: TDateTime;
  AgeMs: Double;
begin
  Now_ := Now;
  try
    FLock.Enter;
    try
      if not FActive then
        Exit;
      Index := IndexOfPID(AValue.PID);
      if Index < 0 then
        Exit;
      if FRowHasData then
      begin
        AgeMs := (Now_ - FRowTimestamp) * OBD_CSV_MS_PER_DAY;
        if FSeen[Index] or ((FIntervalMs > 0) and (AgeMs >= FIntervalMs)) then
          WriteCurrentRowLocked;
      end;
      if not FRowHasData then
        BeginRowLocked(Now_);
      if IsNan(AValue.Value) then
        FValues[Index] := ''
      else
        FValues[Index] := TOBDCSVWriter.FormatNumber(AValue.Value, FDecimalSeparator);
      FSeen[Index] := True;
      if AllColumnsSeen then
        WriteCurrentRowLocked;
    finally
      FLock.Leave;
    end;
  except
    on E: Exception do
      FireError(oeIO, E.Message);
  end;
end;

procedure TOBDCSVLogger.FireError(ACode: TOBDErrorCode; const AMessage: string);
var
  Handled: Boolean;
begin
  if not Assigned(FOnError) then
    Exit;
  Handled := False;
  FOnError(Self, ACode, AMessage, Handled);
end;

{ ---- TOBDRecordingCSVExporter ---------------------------------------------- }

procedure DecodeEntry(const AEntry: TOBDLogEntry; ADecimalSeparator: Char;
  out APidDid, ADecodedName, ADecodedValue, AUnit: string);
var
  PID: Byte;
  Raw: TBytes;
  Value: Double;
  Name, Unit_: string;
begin
  APidDid := '';
  ADecodedName := '';
  ADecodedValue := '';
  AUnit := '';
  if not AEntry.HasServiceID then
    Exit;
  if (AEntry.ServiceID = $41) and (Length(AEntry.Raw) >= 1) then
  begin
    PID := AEntry.Raw[0];
    APidDid := PIDToText(PID);
    Raw := Copy(AEntry.Raw, 1, Length(AEntry.Raw) - 1);
    if TryDecodePIDValue(PID, Raw, Name, Unit_, Value) then
    begin
      ADecodedName := Name;
      AUnit := Unit_;
      if not IsNan(Value) then
        ADecodedValue := TOBDCSVWriter.FormatNumber(Value, ADecimalSeparator);
    end;
  end
  else if (AEntry.ServiceID = $62) and (Length(AEntry.Raw) >= 2) then
    APidDid := '0x' + IntToHex((Word(AEntry.Raw[0]) shl 8) or AEntry.Raw[1], 4)
  else if (AEntry.ServiceID = $49) and (Length(AEntry.Raw) >= 1) then
    APidDid := PIDToText(AEntry.Raw[0]);
end;

procedure EntryToFields(const AEntry: TOBDLogEntry; const AStart: TDateTime;
  const AOptions: TOBDRecordingCSVExportOptions; out AFields: TArray<string>);
var
  Elapsed: Double;
  PidDid, Name, Value, Unit_: string;
begin
  SetLength(AFields, 10);
  AFields[0] := FormatCSVTimestamp(AEntry.Timestamp, AOptions.TimestampFormat);
  if (AStart <> 0) and (AEntry.Timestamp <> 0) then
  begin
    Elapsed := (AEntry.Timestamp - AStart) * OBD_CSV_SECONDS_PER_DAY;
    if Elapsed < 0 then
      Elapsed := 0;
    AFields[1] := TOBDCSVWriter.FormatNumber(Elapsed, AOptions.DecimalSeparator);
  end
  else if AEntry.ElapsedMs > 0 then
    AFields[1] := TOBDCSVWriter.FormatNumber(AEntry.ElapsedMs / 1000.0,
      AOptions.DecimalSeparator)
  else
    AFields[1] := '';
  AFields[2] := EntryKindText(AEntry.Kind);
  if AEntry.HasFrameID then
    AFields[3] := '0x' + IntToHex(AEntry.FrameID, 0)
  else
    AFields[3] := '';
  if AEntry.HasServiceID then
    AFields[4] := '0x' + IntToHex(AEntry.ServiceID, 2)
  else
    AFields[4] := '';
  DecodeEntry(AEntry, AOptions.DecimalSeparator, PidDid, Name, Value, Unit_);
  AFields[5] := PidDid;
  AFields[6] := BytesToHex(AEntry.Raw);
  AFields[7] := Name;
  AFields[8] := Value;
  AFields[9] := Unit_;
end;

procedure ValidateExportOptions(const AOptions: TOBDRecordingCSVExportOptions);
begin
  ValidateCSVConfig(AOptions.Separator, AOptions.DecimalSeparator);
end;

function FirstTimestamp(const AEntries: array of TOBDLogEntry): TDateTime;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AEntries) do
    if AEntries[I].Timestamp <> 0 then
      Exit(AEntries[I].Timestamp);
end;

class procedure TOBDRecordingCSVExporter.Export(const ARecordingFile,
  ACSVFile: string; const AOptions: TOBDRecordingCSVExportOptions);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(ACSVFile, fmCreate or fmShareDenyWrite);
  try
    ExportToStream(ARecordingFile, Stream, AOptions);
  finally
    Stream.Free;
  end;
end;

class procedure TOBDRecordingCSVExporter.ExportToStream(const ARecordingFile: string;
  AStream: TStream; const AOptions: TOBDRecordingCSVExportOptions);
var
  Entries: TArray<TOBDLogEntry>;
begin
  Entries := TOBDReplayer.LoadAll(ARecordingFile);
  ExportEntriesToStream(Entries, AStream, AOptions);
end;

class procedure TOBDRecordingCSVExporter.ExportEntriesToStream(
  const AEntries: array of TOBDLogEntry; AStream: TStream;
  const AOptions: TOBDRecordingCSVExportOptions);
var
  Writer: TOBDCSVWriter;
  Fields: TArray<string>;
  I: Integer;
  Start: TDateTime;
begin
  ValidateExportOptions(AOptions);
  Writer := TOBDCSVWriter.Create(AStream, False);
  try
    Writer.Separator := AOptions.Separator;
    Writer.DecimalSeparator := AOptions.DecimalSeparator;
    Writer.IncludeUTF8BOM := AOptions.IncludeUTF8BOM;
    if AOptions.IncludeHeader then
      Writer.WriteRow(['timestamp', 'elapsed', 'kind', 'ecu_header', 'service',
        'pid_did', 'raw_hex', 'decoded_name', 'decoded_value', 'unit']);
    Start := FirstTimestamp(AEntries);
    for I := 0 to High(AEntries) do
    begin
      EntryToFields(AEntries[I], Start, AOptions, Fields);
      Writer.WriteRow(Fields);
    end;
    Writer.Flush;
  finally
    Writer.Free;
  end;
end;

class procedure TOBDRecordingCSVExporter.ExportEntriesToStrings(
  const AEntries: array of TOBDLogEntry; AStrings: TStrings;
  const AOptions: TOBDRecordingCSVExportOptions);
var
  Fields: TArray<string>;
  I: Integer;
  Start: TDateTime;
begin
  if AStrings = nil then
    raise EArgumentNilException.Create('TOBDRecordingCSVExporter strings required');
  ValidateExportOptions(AOptions);
  AStrings.BeginUpdate;
  try
    if AOptions.IncludeHeader then
      AStrings.Add(TOBDCSVWriter.BuildRow(['timestamp', 'elapsed', 'kind',
        'ecu_header', 'service', 'pid_did', 'raw_hex', 'decoded_name',
        'decoded_value', 'unit'], AOptions.Separator));
    Start := FirstTimestamp(AEntries);
    for I := 0 to High(AEntries) do
    begin
      EntryToFields(AEntries[I], Start, AOptions, Fields);
      AStrings.Add(TOBDCSVWriter.BuildRow(Fields, AOptions.Separator));
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

class procedure TOBDRecordingCSVExporter.ExportLogLinesToStream(const ALines: TStrings;
  AStream: TStream; const AOptions: TOBDRecordingCSVExportOptions);
var
  Entries: TList<TOBDLogEntry>;
  Replayer: TOBDReplayer;
  Entry: TOBDLogEntry;
  I: Integer;
begin
  if ALines = nil then
    raise EArgumentNilException.Create('TOBDRecordingCSVExporter lines required');
  Entries := TList<TOBDLogEntry>.Create;
  Replayer := TOBDReplayer.Create(nil);
  try
    for I := 0 to ALines.Count - 1 do
    begin
      if Trim(ALines[I]) = '' then
        Continue;
      if not Replayer.ParseLine(ALines[I], Entry) then
        raise EOBDCSVError.CreateFmt('Malformed recording line %d', [I + 1]);
      Entries.Add(Entry);
    end;
    ExportEntriesToStream(Entries.ToArray, AStream, AOptions);
  finally
    Replayer.Free;
    Entries.Free;
  end;
end;

class procedure TOBDRecordingCSVExporter.ExportLogLinesToStrings(const ALines,
  AStrings: TStrings; const AOptions: TOBDRecordingCSVExportOptions);
var
  Entries: TList<TOBDLogEntry>;
  Replayer: TOBDReplayer;
  Entry: TOBDLogEntry;
  I: Integer;
begin
  if ALines = nil then
    raise EArgumentNilException.Create('TOBDRecordingCSVExporter lines required');
  Entries := TList<TOBDLogEntry>.Create;
  Replayer := TOBDReplayer.Create(nil);
  try
    for I := 0 to ALines.Count - 1 do
    begin
      if Trim(ALines[I]) = '' then
        Continue;
      if not Replayer.ParseLine(ALines[I], Entry) then
        raise EOBDCSVError.CreateFmt('Malformed recording line %d', [I + 1]);
      Entries.Add(Entry);
    end;
    ExportEntriesToStrings(Entries.ToArray, AStrings, AOptions);
  finally
    Replayer.Free;
    Entries.Free;
  end;
end;

end.
