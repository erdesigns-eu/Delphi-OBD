//------------------------------------------------------------------------------
// UNIT           : Tests.Logger.Sinks
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Logger.Sinks;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TLoggerSinksTests = class
  public
    [Test] procedure InMemorySink_AcceptsEventsAndCapsAtCapacity;
    [Test] procedure InMemorySink_OnEventCallbackFires;
    [Test] procedure JsonLineSink_WritesValidJson;
    [Test] procedure FileRotationSink_RotatesWhenSizeExceeded;
    [Test] procedure DailyRotationSink_NamesFileWithDate;
    [Test] procedure LogLevelName_ReturnsExpectedStrings;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.JSON, System.Classes,
  System.DateUtils,
  OBD.Logger.Sinks;

function MakeEvent(L: TOBDLogLevel; const Msg: string;
  const Source: string = ''): TOBDLogEvent;
begin
  Result.Timestamp := Now;
  Result.Level := L;
  Result.Source := Source;
  Result.Message := Msg;
end;

function ScratchPath(const Name: string): string;
begin
  Result := TPath.Combine(TPath.GetTempPath,
    Format('obdtests-%d-%s', [GetCurrentProcessId, Name]));
end;

{ TLoggerSinksTests }

procedure TLoggerSinksTests.InMemorySink_AcceptsEventsAndCapsAtCapacity;
var
  Sink: TInMemorySink;
  I: Integer;
  Snap: TArray<TOBDLogEvent>;
begin
  Sink := TInMemorySink.Create(3);
  try
    for I := 1 to 5 do
      Sink.Write(MakeEvent(lsInfo, 'msg ' + IntToStr(I)));
    Snap := Sink.Snapshot;
    Assert.AreEqual(3, Length(Snap), 'should cap at capacity');
    Assert.AreEqual('msg 3', Snap[0].Message, 'oldest two evicted');
    Assert.AreEqual('msg 5', Snap[2].Message);
  finally
    Sink.Free;
  end;
end;

procedure TLoggerSinksTests.InMemorySink_OnEventCallbackFires;
var
  Sink: TInMemorySink;
  Fired: Integer;
begin
  Fired := 0;
  Sink := TInMemorySink.Create;
  try
    Sink.OnEvent := procedure(E: TOBDLogEvent) begin Inc(Fired); end;
    Sink.Write(MakeEvent(lsInfo, 'one'));
    Sink.Write(MakeEvent(lsError, 'two'));
    Assert.AreEqual(2, Fired);
  finally
    Sink.Free;
  end;
end;

procedure TLoggerSinksTests.JsonLineSink_WritesValidJson;
var
  Path: string;
  Sink: TJsonLineSink;
  Lines: TStringList;
  Json: TJSONObject;
begin
  Path := ScratchPath('json.log');
  if TFile.Exists(Path) then TFile.Delete(Path);
  Sink := TJsonLineSink.Create(Path);
  try
    Sink.Write(MakeEvent(lsWarning, 'hello world', 'connection'));
  finally
    Sink.Free;
  end;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Path, TEncoding.UTF8);
    Assert.IsTrue(Lines.Count >= 1);
    Json := TJSONObject.ParseJSONValue(Lines[0]) as TJSONObject;
    try
      Assert.AreEqual('WARNING', Json.GetValue('level').Value);
      Assert.AreEqual('hello world', Json.GetValue('msg').Value);
      Assert.AreEqual('connection', Json.GetValue('source').Value);
    finally
      Json.Free;
    end;
  finally
    Lines.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

procedure TLoggerSinksTests.FileRotationSink_RotatesWhenSizeExceeded;
var
  Path, Backup1: string;
  Sink: TFileRotationSink;
  I: Integer;
begin
  Path := ScratchPath('rotation.log');
  Backup1 := Path + '.1';
  if TFile.Exists(Path) then TFile.Delete(Path);
  if TFile.Exists(Backup1) then TFile.Delete(Backup1);

  // Tiny cap so the second write rotates.
  Sink := TFileRotationSink.Create(Path, 64, 3);
  try
    for I := 1 to 20 do
      Sink.Write(MakeEvent(lsInfo, StringOfChar('x', 30)));
  finally
    Sink.Free;
  end;

  Assert.IsTrue(TFile.Exists(Path), 'current log must exist after rotation');
  Assert.IsTrue(TFile.Exists(Backup1), 'rotated .1 must exist');

  TFile.Delete(Path);
  TFile.Delete(Backup1);
end;

procedure TLoggerSinksTests.DailyRotationSink_NamesFileWithDate;
var
  Dir, Expected: string;
  Sink: TDailyRotationSink;
begin
  Dir := ScratchPath('daily');
  ForceDirectories(Dir);
  Expected := TPath.Combine(Dir,
    'svc-' + FormatDateTime('yyyymmdd', Now) + '.log');
  if TFile.Exists(Expected) then TFile.Delete(Expected);

  Sink := TDailyRotationSink.Create(Dir, 'svc');
  try
    Sink.Write(MakeEvent(lsInfo, 'today'));
  finally
    Sink.Free;
  end;

  Assert.IsTrue(TFile.Exists(Expected),
    'expected daily-rotated file at ' + Expected);
  TFile.Delete(Expected);
end;

procedure TLoggerSinksTests.LogLevelName_ReturnsExpectedStrings;
begin
  Assert.AreEqual('DEBUG',    LogLevelName(lsDebug));
  Assert.AreEqual('INFO',     LogLevelName(lsInfo));
  Assert.AreEqual('WARNING',  LogLevelName(lsWarning));
  Assert.AreEqual('ERROR',    LogLevelName(lsError));
  Assert.AreEqual('CRITICAL', LogLevelName(lsCritical));
end;

initialization
  TDUnitX.RegisterTestFixture(TLoggerSinksTests);

end.
