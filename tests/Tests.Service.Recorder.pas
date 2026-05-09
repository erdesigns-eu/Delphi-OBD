//------------------------------------------------------------------------------
// UNIT           : Tests.Service.Recorder
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Service.Recorder;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TRecorderTests = class
  public
    [Test] procedure RecorderCapturesEntriesInOrder;
    [Test] procedure SaveAndLoadRoundTrip;
    [Test] procedure ReplayFiresEntriesWithSpeedZero;
    [Test] procedure EscapesTabAndNewlineInText;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  OBD.Service.Recorder;

function ScratchFile(const Name: string): string;
begin
  Result := TPath.Combine(TPath.GetTempPath,
    Format('obdrec-%d-%s', [GetCurrentProcessId, Name]));
end;

procedure TRecorderTests.RecorderCapturesEntriesInOrder;
var
  R: TOBDRecorder;
  Entries: TArray<TOBDRecordedEntry>;
begin
  R := TOBDRecorder.Create;
  try
    R.Start;
    R.RecordSent('AT Z');
    R.RecordReceived('ELM327 v1.5');
    R.RecordError('NO DATA');
    R.Stop;

    Entries := R.Snapshot;
    Assert.AreEqual(3, Length(Entries));
    Assert.AreEqual(Ord(rdSent),     Ord(Entries[0].Direction));
    Assert.AreEqual('AT Z',          Entries[0].Text);
    Assert.AreEqual(Ord(rdReceived), Ord(Entries[1].Direction));
    Assert.AreEqual(Ord(rdError),    Ord(Entries[2].Direction));
  finally
    R.Free;
  end;
end;

procedure TRecorderTests.SaveAndLoadRoundTrip;
var
  R: TOBDRecorder;
  Replay: TOBDReplayer;
  Path: string;
  Loaded: TArray<TOBDRecordedEntry>;
begin
  Path := ScratchFile('roundtrip.obdlog');
  if TFile.Exists(Path) then TFile.Delete(Path);

  R := TOBDRecorder.Create;
  try
    R.Start;
    R.RecordSent('01 0C');
    R.RecordReceived('41 0C 1A F8');
    R.SaveToFile(Path);
  finally
    R.Free;
  end;

  Replay := TOBDReplayer.Create;
  try
    Replay.LoadFromFile(Path);
    Loaded := Replay.Entries;
    Assert.AreEqual(2, Length(Loaded));
    Assert.AreEqual('01 0C',         Loaded[0].Text);
    Assert.AreEqual('41 0C 1A F8',   Loaded[1].Text);
    Assert.AreEqual(Ord(rdSent),     Ord(Loaded[0].Direction));
    Assert.AreEqual(Ord(rdReceived), Ord(Loaded[1].Direction));
  finally
    Replay.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

procedure TRecorderTests.ReplayFiresEntriesWithSpeedZero;
var
  R: TOBDRecorder;
  Replay: TOBDReplayer;
  Path: string;
  Fired: Integer;
begin
  Path := ScratchFile('replay.obdlog');
  if TFile.Exists(Path) then TFile.Delete(Path);

  R := TOBDRecorder.Create;
  try
    R.Start;
    R.RecordSent('A');
    R.RecordReceived('B');
    R.RecordInfo('C');
    R.SaveToFile(Path);
  finally
    R.Free;
  end;

  Fired := 0;
  Replay := TOBDReplayer.Create;
  try
    Replay.LoadFromFile(Path);
    Replay.Speed := 0; // skip sleeps
    Replay.OnEntry := procedure(Sender: TObject; const Entry: TOBDRecordedEntry)
      begin Inc(Fired); end;
    Replay.Run;
    Assert.AreEqual(3, Fired);
  finally
    Replay.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

procedure TRecorderTests.EscapesTabAndNewlineInText;
var
  R: TOBDRecorder;
  Replay: TOBDReplayer;
  Path: string;
  Loaded: TArray<TOBDRecordedEntry>;
begin
  Path := ScratchFile('escape.obdlog');
  if TFile.Exists(Path) then TFile.Delete(Path);

  R := TOBDRecorder.Create;
  try
    R.Start;
    R.RecordReceived('line1' + #13#10 + 'line2' + #9 + 'tabbed');
    R.SaveToFile(Path);
  finally
    R.Free;
  end;

  Replay := TOBDReplayer.Create;
  try
    Replay.LoadFromFile(Path);
    Loaded := Replay.Entries;
    Assert.AreEqual(1, Length(Loaded));
    Assert.AreEqual('line1' + #13#10 + 'line2' + #9 + 'tabbed', Loaded[0].Text);
  finally
    Replay.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRecorderTests);

end.
