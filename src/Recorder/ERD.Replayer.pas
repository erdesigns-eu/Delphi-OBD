// ------------------------------------------------------------------------------
// ERD.Replayer
//
// TOBDReplayer — reads a `.obdlog` file written by
// <see cref="ERD.Recorder.TOBDRecorder"/> and replays the
// recorded events back through the same event interface a
// bound application code subscribed to during the live capture.
//
// Two playback modes:
//
// rmAsFastAsPossible — emit every entry back-to-back. Useful
// for headless reprocessing / unit tests.
// rmRealTime — match the wall-clock gaps between the
// original timestamps. Useful for
// demoing a captured session in a UI.
//
// Async playback runs on a worker thread; events fire on the
// main thread.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-05-09  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.Replayer;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Async.Task,
{$IFDEF FPC}ERD.Compat.Functions, {$ENDIF}
  ERD.Connection,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF FPC}DateUtils{$ELSE}System.DateUtils{$ENDIF},
  System.IOUtils,
  System.JSON,
  System.NetEncoding,
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
{$IFDEF FPC}ZStream{$ELSE}System.ZLib{$ENDIF},
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Recorder;

type
  /// <summary>Playback mode.</summary>
  TOBDReplayMode = (rmAsFastAsPossible, rmRealTime);

  /// <summary>Fires for each replayed entry.</summary>
  TOBDReplayEntryEvent = procedure(Sender: TObject; const AEntry: TOBDLogEntry)
    of object;

  /// <summary>Replayer component. Drop on a form, point
  /// <c>FileName</c> at a <c>.obdlog</c>, hook
  /// <c>OnEntry</c>, call <c>Play</c> or
  /// <c>PlayAsync</c>.</summary>
  TOBDReplayer = class(TComponent)
  strict private
    FFileName: string;
    FMode: TOBDReplayMode;
    FStop: Boolean;
    FStopEvent: TEvent;
    FMaxDecodedBytes: Int64;
    FMaxLineBytes: Integer;
    FMaxEntries: Integer;
    FMaxGapMs: Cardinal;
    FOwnedTask: TOBDOwnedTask;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnEntry: TOBDReplayEntryEvent;
    FOnComplete: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure ScanLines(const APath: string; const AConsumer: TProc<string>;
      ARespectStop: Boolean);
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure FireEntry(const AEntry: TOBDLogEntry);
    procedure FireComplete;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Streams <c>FileName</c> with bounded decompression and replays
    /// it synchronously through <c>OnEntry</c>.</summary>
    procedure Play;
    /// <summary>Non-blocking <see cref="Play"/>. Fires
    /// <c>OnComplete</c> on success, <c>OnError</c> on
    /// failure.</summary>
    procedure PlayAsync;
    /// <summary>Cooperative cancel; the replayer stops at the
    /// next read boundary or interrupted timestamp wait.</summary>
    procedure Stop;

    /// <summary>Loads every entry into a flat array. Useful for
    /// offline analysis without going through the event
    /// interface.</summary>
    class function LoadAll(const AFileName: string)
      : TArray<TOBDLogEntry>; static;

    /// <summary>Reads a `.obdlog` (plain or `.gz`) into a
    /// <c>TStringList</c>. Caller owns the returned list.
    /// Public so test fixtures and the redactor can reuse the
    /// gzip-aware loader without a private back door.</summary>
    function LoadLines(const APath: string): TStringList;
    /// <summary>Parses a single JSONL line into a
    /// <c>TOBDLogEntry</c>. Returns False on blank lines or
    /// malformed JSON. Public for the same reason as
    /// <c>LoadLines</c>.</summary>
    function ParseLine(const ALine: string; out AEntry: TOBDLogEntry): Boolean;
  published
    /// <summary>Maximum decompressed bytes (default 256 MiB); bounded for plain and gzip logs.</summary>
    property MaxDecodedBytes: Int64 read FMaxDecodedBytes
      write FMaxDecodedBytes;
    /// <summary>Maximum bytes per JSONL line (default 1 MiB).</summary>
    property MaxLineBytes: Integer read FMaxLineBytes write FMaxLineBytes;
    /// <summary>Maximum nonblank input lines (default one million).</summary>
    property MaxEntries: Integer read FMaxEntries write FMaxEntries;
    property FileName: string read FFileName write FFileName;
    /// <summary>Playback mode. Default
    /// <c>rmAsFastAsPossible</c>.</summary>
    property Mode: TOBDReplayMode read FMode write FMode
      default rmAsFastAsPossible;
    /// <summary>Maximum sleep between entries during
    /// <c>rmRealTime</c> playback, in ms. A captured pause longer
    /// than this is collapsed to the cap so a UI replay never
    /// stalls. Default 60000 (one minute).</summary>
    property MaxGapMs: Cardinal read FMaxGapMs write FMaxGapMs default 60000;
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
  FOwnedTask := TOBDOwnedTask.Create;
  FStopEvent := TEvent.Create(nil, True, False, '');
  FMaxGapMs := 60000;
  FMaxDecodedBytes := 256 * 1024 * 1024;
  FMaxLineBytes := 1024 * 1024;
  FMaxEntries := 1000000;
end;

destructor TOBDReplayer.Destroy;
begin
  Stop;
  if FOwnedTask <> nil then
    FOwnedTask.Cancel;
  FreeAndNil(FOwnedTask);
  FStopEvent.Free;
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDReplayer.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('Async start requires the main thread');
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDReplayer: playback already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDReplayer.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDReplayer.Stop;
begin
  FStop := True;
  if FStopEvent <> nil then
    FStopEvent.SetEvent;
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

procedure TOBDReplayer.ScanLines(const APath: string;
  const AConsumer: TProc<string>; ARespectStop: Boolean);
var
  FileStream: TFileStream;
  Input: TStream;
  Bytes, LineBytes: TBytes;
  Count, I, Used, Entries, LineLimit, EntryLimit: Integer;
  Total, ByteLimit: Int64;
  Token: IOBDDispatchLifetime;
  procedure DeliverLine;
  var
    Text: string;
  begin
    if (Used > 0) and (LineBytes[Used - 1] = 13) then
      Dec(Used);
    Text := TEncoding.UTF8.GetString(LineBytes, 0, Used);
    if Trim(Text) <> '' then
    begin
      Inc(Entries);
      if Entries > EntryLimit then
        raise EOBDProtocolErr.Create('Replay entry limit exceeded');
      AConsumer(Text);
    end;
    Used := 0;
  end;

begin
  Token := FOwnedTask.Lifetime;
  LineLimit := FMaxLineBytes;
  EntryLimit := FMaxEntries;
  ByteLimit := FMaxDecodedBytes;
  if (LineLimit < 1) or (EntryLimit < 1) or (ByteLimit < 1) then
    raise EOBDConfig.Create('Replay limits must be positive');
  FileStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    Input := FileStream;
    if SameText(ExtractFileExt(APath), '.gz') then
{$IFDEF FPC}
      Input := TGZipDecompressionStream.Create(FileStream);
{$ELSE}
      Input := TZDecompressionStream.Create(FileStream, 31);
{$ENDIF}
    try
      SetLength(Bytes, 16384);
      SetLength(LineBytes, LineLimit);
      Used := 0;
      Entries := 0;
      Total := 0;
      repeat
        if ARespectStop and (Token.IsCancelled or FStop) then
          Break;
        Count := Input.Read(Bytes[0], Length(Bytes));
        Inc(Total, Count);
        if Total > ByteLimit then
          raise EOBDProtocolErr.Create
            ('Replay decompressed byte limit exceeded');
        for I := 0 to Count - 1 do
        begin
          if ARespectStop and (Token.IsCancelled or FStop) then
            Break;
          if Bytes[I] = 10 then
            DeliverLine
          else
          begin
            if Used = LineLimit then
              raise EOBDProtocolErr.Create('Replay line limit exceeded');
            LineBytes[Used] := Bytes[I];
            Inc(Used);
          end;
        end;
      until Count = 0;
      if (Used > 0) and not Token.IsCancelled and not(ARespectStop and FStop)
      then
        DeliverLine;
    finally
      if Input <> FileStream then
        Input.Free
    end;
  finally
    FileStream.Free
  end;
end;

function TOBDReplayer.LoadLines(const APath: string): TStringList;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    ScanLines(APath,
      procedure(Line: string)
      begin
        Lines.Add(Line)
      end, False);
    Result := Lines;
  except
    Lines.Free;
    raise
  end;
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
  AEntry := Default (TOBDLogEntry);
  if Trim(ALine) = '' then
    Exit;
  try
    Doc := TJSONObject.ParseJSONValue(ALine, True, True);
  except
    on E: Exception do
      Exit(False);
  end;
  if not(Doc is TJSONObject) then
  begin
    if Doc <> nil then
      Doc.Free;
    Exit;
  end;
  try
    Obj := Doc as TJSONObject;
    V := Obj.GetValue('ts');
    if V is TJSONString then
      AEntry.Timestamp := ISO8601ToDate(V.Value);
    KindStr := '';
    V := Obj.GetValue('kind');
    if V is TJSONString then
      KindStr := V.Value;
    if SameText(KindStr, 'frame') then
      AEntry.Kind := leFrame
    else if SameText(KindStr, 'response') then
      AEntry.Kind := leResponse
    else if SameText(KindStr, 'nrc') then
      AEntry.Kind := leNRC
    else if SameText(KindStr, 'error') then
      AEntry.Kind := leError
    else
      AEntry.Kind := leInfo;

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
    if V is TJSONBool then
      AEntry.Extended := TJSONBool(V).AsBoolean;

    V := Obj.GetValue('nrc');
    if V is TJSONString then
    begin
      AEntry.NRC := Byte(ParseHexNumber(V.Value));
      AEntry.HasNRC := True;
    end;

    V := Obj.GetValue('nrc_text');
    if V is TJSONString then
      AEntry.NRCText := V.Value;

    V := Obj.GetValue('message');
    if V is TJSONString then
      AEntry.Message := V.Value;

    Result := True;
  finally
    Doc.Free;
  end;
end;

procedure TOBDReplayer.Play;
var
  Prev: TOBDLogEntry;
  HasPrev: Boolean;
  Token: IOBDDispatchLifetime;
begin
  if FFileName = '' then
    raise EOBDConfig.Create('Replayer FileName not set');
  Token := FOwnedTask.Lifetime;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    FStop := False;
    FStopEvent.ResetEvent
  end;
  if FStop then
    Exit;
  HasPrev := False;
  ScanLines(FFileName,
    procedure(Line: string)
    var
      Entry: TOBDLogEntry;
      Gap: Int64;
    begin
      if not ParseLine(Line, Entry) then
        raise EOBDProtocolErr.Create('Replay contains malformed JSONL entry');
      if (FMode = rmRealTime) and HasPrev then
      begin
        Gap := MilliSecondsBetween(Entry.Timestamp, Prev.Timestamp);
        if Gap > Int64(FMaxGapMs) then
          Gap := FMaxGapMs;
        if (Gap > 0) and (FStopEvent.WaitFor(Cardinal(Gap)) = wrSignaled) then
          Exit;
      end;
      FireEntry(Entry);
      if Token.IsCancelled then
        Exit;
      Prev := Entry;
      HasPrev := True;
    end, True);
  if not Token.IsCancelled and not FStop then
    FireComplete;
end;

procedure TOBDReplayer.PlayAsync;
var
  Self_: TOBDReplayer;
begin
  GuardSingleAsync;
  try
    FStop := False;
    FStopEvent.ResetEvent;
    Self_ := Self;
    FOwnedTask.Start(
      procedure
      begin
        try
          try
            Self_.Play;
          except
            on E: Exception do
              Self_.FireError(oeIO, E.Message);
          end;
        finally
          Self_.ReleaseAsync;
        end;
      end);
  except
    ReleaseAsync;
    raise;
  end;
end;

class function TOBDReplayer.LoadAll(const AFileName: string)
  : TArray<TOBDLogEntry>;
var
  Replayer: TOBDReplayer;
  Acc: TList<TOBDLogEntry>;
begin
  Replayer := TOBDReplayer.Create(nil);
  try
    Acc := TList<TOBDLogEntry>.Create;
    try
      Replayer.ScanLines(AFileName,
        procedure(Line: string)
        var
          Entry: TOBDLogEntry;
        begin
          if not Replayer.ParseLine(Line, Entry) then
            raise EOBDProtocolErr.Create
              ('Replay contains malformed JSONL entry');
          Acc.Add(Entry);
        end, False);
      Result := Acc.ToArray;
    finally
      Acc.Free
    end;
  finally
    Replayer.Free
  end;
end;

procedure TOBDReplayer.FireEntry(const AEntry: TOBDLogEntry);
var
  Self_: TOBDReplayer;
  E: TOBDLogEntry;
begin
  if not Assigned(FOnEntry) then
    Exit;
  Self_ := Self;
  E := AEntry;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnEntry(Self_, E)
  else
    FOwnedTask.Post(
      procedure
      begin
        if Assigned(Self_.FOnEntry) then
          Self_.FOnEntry(Self_, E);
      end);
end;

procedure TOBDReplayer.FireComplete;
var
  Self_: TOBDReplayer;
begin
  if not Assigned(FOnComplete) then
    Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnComplete(Self_)
  else
    FOwnedTask.Post(
      procedure
      begin
        if Assigned(Self_.FOnComplete) then
          Self_.FOnComplete(Self_);
      end);
end;

procedure TOBDReplayer.FireError(ACode: TOBDErrorCode; const AMessage: string);
var
  Self_: TOBDReplayer;
  Code: TOBDErrorCode;
  Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    FOwnedTask.Post(
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
