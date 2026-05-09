//------------------------------------------------------------------------------
// UNIT           : OBD.Service.Recorder.pas
// CONTENTS       : Record and replay OBD adapter conversations
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : `.obdlog` is a flat newline-delimited text format —
//                  each record is `<elapsed_ms>\t<direction>\t<text>`.
//                  Direction is `S` (sent), `R` (received), `I` (info),
//                  or `E` (error). Storing both halves of the
//                  conversation makes the replay deterministic without a
//                  vehicle and useful as test fixtures (see
//                  `tests/Tests.RadioCode.Smoke` for the pattern).
//------------------------------------------------------------------------------
unit OBD.Service.Recorder;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Diagnostics,
  System.Generics.Collections, System.IOUtils;

type
  TOBDRecorderDirection = (rdSent, rdReceived, rdInfo, rdError);

  TOBDRecordedEntry = record
    ElapsedMs: Int64;
    Direction: TOBDRecorderDirection;
    Text: string;
  end;

  /// <summary>
  ///   Thread-safe append-only recorder. Hand `RecordSent` /
  ///   `RecordReceived` etc. to the connection's hooks and call
  ///   `SaveToFile` to persist a `.obdlog`.
  /// </summary>
  TOBDRecorder = class
  strict private
    FLock: TCriticalSection;
    FStopwatch: TStopwatch;
    FEntries: TList<TOBDRecordedEntry>;
    procedure AppendEntry(Direction: TOBDRecorderDirection; const Text: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;          // resets the timeline + clears entries
    procedure Stop;
    function IsRunning: Boolean;

    procedure RecordSent(const Text: string);
    procedure RecordReceived(const Text: string);
    procedure RecordInfo(const Text: string);
    procedure RecordError(const Text: string);

    function Snapshot: TArray<TOBDRecordedEntry>;
    function Count: Integer;
    procedure SaveToFile(const FilePath: string);
  end;

  /// <summary>
  ///   Replay a recorded conversation. Iterates through entries
  ///   honouring (or scaling) the original elapsed timing.
  /// </summary>
  TOBDReplayEvent = procedure(Sender: TObject; const Entry: TOBDRecordedEntry) of object;

  TOBDReplayer = class
  strict private
    FEntries: TArray<TOBDRecordedEntry>;
    FOnEntry: TOBDReplayEvent;
    FSpeed: Single;     // 1.0 = real-time, 0 = no delay (as fast as possible)
  public
    constructor Create;

    /// <summary>Load entries from a `.obdlog` file.</summary>
    procedure LoadFromFile(const FilePath: string);
    /// <summary>Load from memory.</summary>
    procedure LoadEntries(const AEntries: TArray<TOBDRecordedEntry>);

    /// <summary>
    ///   Synchronously walk every loaded entry, sleeping between them
    ///   according to the recorded ElapsedMs and the configured
    ///   <c>Speed</c> multiplier. Set <c>Speed</c> to 0 to fire all
    ///   entries back to back (useful for tests).
    /// </summary>
    procedure Run;

    function Count: Integer;
    function Entries: TArray<TOBDRecordedEntry>;
    /// <summary>Replay speed multiplier. 1.0 = real time. 0 = no delays.</summary>
    property Speed: Single read FSpeed write FSpeed;
    /// <summary>Fired for each entry as the replay progresses.</summary>
    property OnEntry: TOBDReplayEvent read FOnEntry write FOnEntry;
  end;

implementation

uses
  WinApi.Windows;

function DirectionLetter(D: TOBDRecorderDirection): Char;
begin
  case D of
    rdSent:     Result := 'S';
    rdReceived: Result := 'R';
    rdInfo:     Result := 'I';
    rdError:    Result := 'E';
  else          Result := '?';
  end;
end;

function LetterToDirection(C: Char): TOBDRecorderDirection;
begin
  case UpCase(C) of
    'S': Result := rdSent;
    'R': Result := rdReceived;
    'I': Result := rdInfo;
    'E': Result := rdError;
  else   Result := rdInfo;
  end;
end;

function EscapeText(const S: string): string;
begin
  // Tabs and newlines would corrupt the field-delimited line format.
  // Replace with conservative escapes — keep the format human-readable.
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function UnescapeText(const S: string): string;
var
  I: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(S) do
    begin
      if (S[I] = '\') and (I < Length(S)) then
      begin
        case S[I + 1] of
          '\': Builder.Append('\');
          't': Builder.Append(#9);
          'r': Builder.Append(#13);
          'n': Builder.Append(#10);
        else
          Builder.Append(S[I]);
          Builder.Append(S[I + 1]);
        end;
        Inc(I, 2);
      end
      else
      begin
        Builder.Append(S[I]);
        Inc(I);
      end;
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

//==============================================================================
// TOBDRecorder
//==============================================================================
constructor TOBDRecorder.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FEntries := TList<TOBDRecordedEntry>.Create;
end;

destructor TOBDRecorder.Destroy;
begin
  FEntries.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDRecorder.Start;
begin
  FLock.Enter;
  try
    FEntries.Clear;
    FStopwatch := TStopwatch.StartNew;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDRecorder.Stop;
begin
  FLock.Enter;
  try
    if FStopwatch.IsRunning then FStopwatch.Stop;
  finally
    FLock.Leave;
  end;
end;

function TOBDRecorder.IsRunning: Boolean;
begin
  FLock.Enter;
  try Result := FStopwatch.IsRunning; finally FLock.Leave; end;
end;

procedure TOBDRecorder.AppendEntry(Direction: TOBDRecorderDirection;
  const Text: string);
var
  Entry: TOBDRecordedEntry;
begin
  FLock.Enter;
  try
    Entry.ElapsedMs := FStopwatch.ElapsedMilliseconds;
    Entry.Direction := Direction;
    Entry.Text := Text;
    FEntries.Add(Entry);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDRecorder.RecordSent    (const Text: string); begin AppendEntry(rdSent,     Text); end;
procedure TOBDRecorder.RecordReceived(const Text: string); begin AppendEntry(rdReceived, Text); end;
procedure TOBDRecorder.RecordInfo    (const Text: string); begin AppendEntry(rdInfo,     Text); end;
procedure TOBDRecorder.RecordError   (const Text: string); begin AppendEntry(rdError,    Text); end;

function TOBDRecorder.Snapshot: TArray<TOBDRecordedEntry>;
begin
  FLock.Enter;
  try Result := FEntries.ToArray; finally FLock.Leave; end;
end;

function TOBDRecorder.Count: Integer;
begin
  FLock.Enter;
  try Result := FEntries.Count; finally FLock.Leave; end;
end;

procedure TOBDRecorder.SaveToFile(const FilePath: string);
var
  Output: TStringList;
  Entry: TOBDRecordedEntry;
  Snap: TArray<TOBDRecordedEntry>;
begin
  Snap := Snapshot;
  ForceDirectories(TPath.GetDirectoryName(FilePath));
  Output := TStringList.Create;
  try
    // First line is a magic header so loaders can sanity-check.
    Output.Add('# obdlog v1');
    for Entry in Snap do
      Output.Add(Format('%d'#9'%s'#9'%s',
        [Entry.ElapsedMs, DirectionLetter(Entry.Direction),
         EscapeText(Entry.Text)]));
    Output.SaveToFile(FilePath, TEncoding.UTF8);
  finally
    Output.Free;
  end;
end;

//==============================================================================
// TOBDReplayer
//==============================================================================
constructor TOBDReplayer.Create;
begin
  inherited Create;
  FSpeed := 1.0;
end;

procedure TOBDReplayer.LoadFromFile(const FilePath: string);
var
  Lines: TStringList;
  S: string;
  Parts: TArray<string>;
  Entry: TOBDRecordedEntry;
  Loaded: TList<TOBDRecordedEntry>;
begin
  Lines := TStringList.Create;
  Loaded := TList<TOBDRecordedEntry>.Create;
  try
    Lines.LoadFromFile(FilePath, TEncoding.UTF8);
    for S in Lines do
    begin
      if (S = '') or S.StartsWith('#') then Continue;
      Parts := S.Split([#9]);
      if Length(Parts) < 3 then Continue;
      Entry.ElapsedMs := StrToInt64Def(Parts[0], 0);
      if Length(Parts[1]) >= 1 then
        Entry.Direction := LetterToDirection(Parts[1][1])
      else
        Entry.Direction := rdInfo;
      Entry.Text := UnescapeText(Parts[2]);
      Loaded.Add(Entry);
    end;
    FEntries := Loaded.ToArray;
  finally
    Loaded.Free;
    Lines.Free;
  end;
end;

procedure TOBDReplayer.LoadEntries(const AEntries: TArray<TOBDRecordedEntry>);
begin
  FEntries := AEntries;
end;

procedure TOBDReplayer.Run;
var
  I: Integer;
  PrevElapsed, Delay: Int64;
begin
  PrevElapsed := 0;
  for I := 0 to High(FEntries) do
  begin
    if FSpeed > 0 then
    begin
      Delay := Round((FEntries[I].ElapsedMs - PrevElapsed) / FSpeed);
      if Delay > 0 then
        Sleep(Delay);
    end;
    PrevElapsed := FEntries[I].ElapsedMs;

    if Assigned(FOnEntry) then
      try FOnEntry(Self, FEntries[I]); except end;
  end;
end;

function TOBDReplayer.Count: Integer;
begin Result := Length(FEntries); end;

function TOBDReplayer.Entries: TArray<TOBDRecordedEntry>;
begin Result := FEntries; end;

end.
