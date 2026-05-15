//------------------------------------------------------------------------------
//  OBD.Replayer
//
//  TOBDReplayer — reads a `.obdlog` file written by
//  <see cref="OBD.Recorder.TOBDRecorder"/> and replays the
//  recorded events back through the same event interface a
//  bound application code subscribed to during the live capture.
//
//  Two playback modes:
//
//    rmAsFastAsPossible — emit every entry back-to-back. Useful
//                         for headless reprocessing / unit tests.
//    rmRealTime — match the wall-clock gaps between the
//                         original timestamps. Useful for
//                         demoing a captured session in a UI.
//
//  Async playback runs on a worker thread; events fire on the
//  main thread.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Replayer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.IOUtils,
  System.JSON,
  System.NetEncoding,
  System.Generics.Collections,
  System.ZLib,
  OBD.Types,
  OBD.Recorder;

type
  /// <summary>Playback mode.</summary>
  TOBDReplayMode = (rmAsFastAsPossible, rmRealTime);

  /// <summary>Fires for each replayed entry.</summary>
  TOBDReplayEntryEvent = procedure(Sender: TObject;
    const AEntry: TOBDLogEntry) of object;

  /// <summary>Replayer component. Drop on a form, point
  /// <c>FileName</c> at a <c>.obdlog</c>, hook
  /// <c>OnEntry</c>, call <c>Play</c> or
  /// <c>PlayAsync</c>.</summary>
  TOBDReplayer = class(TComponent)
  strict private
    FFileName: string;
    FMode: TOBDReplayMode;
    FStop: Boolean;
    FMaxGapMs: Cardinal;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnEntry: TOBDReplayEntryEvent;
    FOnComplete: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure FireEntry(const AEntry: TOBDLogEntry);
    procedure FireComplete;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Loads <c>FileName</c> into memory and replays
    /// it synchronously through <c>OnEntry</c>.</summary>
    procedure Play;
    /// <summary>Non-blocking <see cref="Play"/>. Fires
    /// <c>OnComplete</c> on success, <c>OnError</c> on
    /// failure.</summary>
    procedure PlayAsync;
    /// <summary>Cooperative cancel; the replayer stops at the
    /// next entry boundary.</summary>
    procedure Stop;

    /// <summary>Loads every entry into a flat array. Useful for
    /// offline analysis without going through the event
    /// interface.</summary>
    class function LoadAll(const AFileName: string): TArray<TOBDLogEntry>; static;

    /// <summary>Reads a `.obdlog` (plain or `.gz`) into a
    /// <c>TStringList</c>. Caller owns the returned list.
    /// Public so test fixtures and the redactor can reuse the
    /// gzip-aware loader without a private back door.</summary>
    function LoadLines(const APath: string): TStringList;
    /// <summary>Parses a single JSONL line into a
    /// <c>TOBDLogEntry</c>. Returns False on blank lines or
    /// malformed JSON. Public for the same reason as
    /// <c>LoadLines</c>.</summary>
    function ParseLine(const ALine: string;
      out AEntry: TOBDLogEntry): Boolean;
  published
    property FileName: string read FFileName write FFileName;
    /// <summary>Playback mode. Default
    /// <c>rmAsFastAsPossible</c>.</summary>
    property Mode: TOBDReplayMode read FMode write FMode
      default rmAsFastAsPossible;
    /// <summary>Maximum sleep between entries during
    /// <c>rmRealTime</c> playback, in ms. A captured pause longer
    /// than this is collapsed to the cap so a UI replay never
    /// stalls. Default 60000 (one minute).</summary>
    property MaxGapMs: Cardinal read FMaxGapMs write FMaxGapMs
      default 60000;
    /// <summary>Fires per replayed entry on the main thread.</summary>
    property OnEntry: TOBDReplayEntryEvent read FOnEntry write FOnEntry;
    /// <summary>Fires when playback finishes (main thread).</summary>
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    /// <summary>Fires on a parse / I/O error (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDReplayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FMaxGapMs := 60000;
end;

destructor TOBDReplayer.Destroy;
begin
  Stop;
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDReplayer.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDReplayer: playback already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDReplayer.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDReplayer.Stop;
begin
  FStop := True;
end;

function ParseHexNumber(const AText: string): UInt64;
var
  S: string;
begin
  S := Trim(AText);
  if (Length(S) >= 2) and (S[1] = '0') and CharInSet(S[2], ['x', 'X']) then
    Result := StrToInt64('$' + Copy(S, 3, MaxInt))
  else
    Result := StrToInt64(S);
end;

function TOBDReplayer.LoadLines(const APath: string): TStringList;
const
  GZIP_WINDOW_BITS = 15 + 16;
var
  FileStream: TFileStream;
  Decomp: TZDecompressionStream;
  Buf: TBytes;
  N: Integer;
  Reader: TStringStream;
begin
  Result := TStringList.Create;
  if SameText(ExtractFileExt(APath), '.gz') then
  begin
    FileStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
    try
      Decomp := TZDecompressionStream.Create(FileStream, GZIP_WINDOW_BITS);
      try
        // Drain into a memory buffer, then load as text — simpler
        // and correct than feeding TStringList.LoadFromStream a
        // decompression stream (which expects Seek support).
        SetLength(Buf, 0);
        var Chunk: TBytes;
        SetLength(Chunk, 16 * 1024);
        repeat
          N := Decomp.Read(Chunk[0], Length(Chunk));
          if N > 0 then
          begin
            var Old: Integer := Length(Buf);
            SetLength(Buf, Old + N);
            Move(Chunk[0], Buf[Old], N);
          end;
        until N <= 0;
        Reader := TStringStream.Create('', TEncoding.UTF8);
        try
          if Length(Buf) > 0 then
            Reader.WriteBuffer(Buf[0], Length(Buf));
          Reader.Position := 0;
          Result.LoadFromStream(Reader);
        finally
          Reader.Free;
        end;
      finally
        Decomp.Free;
      end;
    finally
      FileStream.Free;
    end;
  end
  else
    Result.LoadFromFile(APath, TEncoding.UTF8);
end;

function TOBDReplayer.ParseLine(const ALine: string;
  out AEntry: TOBDLogEntry): Boolean;
var
  Doc: TJSONValue;
  Obj: TJSONObject;
  V: TJSONValue;
  KindStr: string;
begin
  Result := False;
  AEntry := Default(TOBDLogEntry);
  if Trim(ALine) = '' then Exit;
  Doc := TJSONObject.ParseJSONValue(ALine);
  if not (Doc is TJSONObject) then
  begin
    if Doc <> nil then Doc.Free;
    Exit;
  end;
  try
    Obj := Doc as TJSONObject;
    V := Obj.GetValue('ts');
    if V is TJSONString then
      AEntry.Timestamp := ISO8601ToDate(V.Value);
    KindStr := '';
    V := Obj.GetValue('kind');
    if V is TJSONString then KindStr := V.Value;
    if      SameText(KindStr, 'frame')    then AEntry.Kind := leFrame
    else if SameText(KindStr, 'response') then AEntry.Kind := leResponse
    else if SameText(KindStr, 'nrc')      then AEntry.Kind := leNRC
    else if SameText(KindStr, 'error')    then AEntry.Kind := leError
    else                                       AEntry.Kind := leInfo;

    V := Obj.GetValue('elapsed_ms');
    if V is TJSONNumber then
      AEntry.ElapsedMs := Cardinal(TJSONNumber(V).AsInt64);

    V := Obj.GetValue('service_id');
    if V is TJSONString then
    begin
      AEntry.ServiceID := Byte(ParseHexNumber(V.Value));
      AEntry.HasServiceID := True;
    end;

    V := Obj.GetValue('raw');
    if V is TJSONString then
      AEntry.Raw := TNetEncoding.Base64.DecodeStringToBytes(V.Value);

    V := Obj.GetValue('id');
    if V is TJSONString then
    begin
      AEntry.FrameID := Cardinal(ParseHexNumber(V.Value));
      AEntry.HasFrameID := True;
    end;

    V := Obj.GetValue('extended');
    if V is TJSONBool then AEntry.Extended := TJSONBool(V).AsBoolean;

    V := Obj.GetValue('nrc');
    if V is TJSONString then
    begin
      AEntry.NRC := Byte(ParseHexNumber(V.Value));
      AEntry.HasNRC := True;
    end;

    V := Obj.GetValue('nrc_text');
    if V is TJSONString then AEntry.NRCText := V.Value;

    V := Obj.GetValue('message');
    if V is TJSONString then AEntry.Message := V.Value;

    Result := True;
  finally
    Doc.Free;
  end;
end;

procedure TOBDReplayer.Play;
var
  Lines: TStringList;
  I: Integer;
  Entry, Prev: TOBDLogEntry;
  Gap: Int64;
  HasPrev: Boolean;
begin
  if FFileName = '' then
    raise EOBDConfig.Create('TOBDReplayer: FileName not set');
  if not FileExists(FFileName) then
    raise EOBDConfig.CreateFmt('TOBDReplayer: file not found "%s"',
      [FFileName]);
  FStop := False;
  HasPrev := False;
  Lines := LoadLines(FFileName);
  try
    for I := 0 to Lines.Count - 1 do
    begin
      if FStop then Break;
      if not ParseLine(Lines[I], Entry) then Continue;
      if (FMode = rmRealTime) and HasPrev then
      begin
        Gap := MilliSecondsBetween(Entry.Timestamp, Prev.Timestamp);
        if Gap < 0 then Gap := 0;
        if Gap > Int64(FMaxGapMs) then Gap := FMaxGapMs;
        if Gap > 0 then Sleep(Gap);
      end;
      FireEntry(Entry);
      Prev := Entry;
      HasPrev := True;
    end;
  finally
    Lines.Free;
  end;
  if not FStop then FireComplete;
end;

procedure TOBDReplayer.PlayAsync;
var
  Self_: TOBDReplayer;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.Play;
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

class function TOBDReplayer.LoadAll(
  const AFileName: string): TArray<TOBDLogEntry>;
var
  Replayer: TOBDReplayer;
  Lines: TStringList;
  I: Integer;
  Acc: TList<TOBDLogEntry>;
  Entry: TOBDLogEntry;
begin
  Replayer := TOBDReplayer.Create(nil);
  Lines := Replayer.LoadLines(AFileName);
  Acc := TList<TOBDLogEntry>.Create;
  try
    for I := 0 to Lines.Count - 1 do
      if Replayer.ParseLine(Lines[I], Entry) then
        Acc.Add(Entry);
    Result := Acc.ToArray;
  finally
    Acc.Free;
    Lines.Free;
    Replayer.Free;
  end;
end;

procedure TOBDReplayer.FireEntry(const AEntry: TOBDLogEntry);
var
  Self_: TOBDReplayer;
  E: TOBDLogEntry;
begin
  if not Assigned(FOnEntry) then Exit;
  Self_ := Self; E := AEntry;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnEntry(Self_, E)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnEntry) then Self_.FOnEntry(Self_, E);
    end);
end;

procedure TOBDReplayer.FireComplete;
var
  Self_: TOBDReplayer;
begin
  if not Assigned(FOnComplete) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnComplete(Self_)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnComplete) then Self_.FOnComplete(Self_);
    end);
end;

procedure TOBDReplayer.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDReplayer; Code: TOBDErrorCode; Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
