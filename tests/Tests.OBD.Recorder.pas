//------------------------------------------------------------------------------
//  Tests.OBD.Recorder
//
//  Phase 10 coverage: TOBDRecorder Append + file-format
//  round-trip; TOBDReplayer parse / play / cancel.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 10 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Recorder;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  System.Generics.Collections,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Recorder,
  OBD.Replayer;

type
  [TestFixture]
  TRecorderTests = class
  public
    [Test] procedure AppendThreeEntriesRoundTrip;
    [Test] procedure RawBytesPreserved;
    [Test] procedure CloseFlushesAndIsIdempotent;
  end;

  [TestFixture]
  TReplayerTests = class
  public
    [Test] procedure PlaysEveryEntry;
    [Test] procedure CancelStopsReplay;
    [Test] procedure PlayRaisesOnMissingFile;
    [Test] procedure ModeDefault;
  end;

implementation

procedure WriteThree(const APath: string);
var
  R: TOBDRecorder;
  E: TOBDLogEntry;
begin
  R := TOBDRecorder.Create(nil);
  try
    R.Open(APath);
    E := Default(TOBDLogEntry);
    E.Timestamp := EncodeDateTime(2026, 5, 9, 10, 0, 0, 0);
    E.Kind := leInfo;
    E.Message := 'session start';
    R.Append(E);

    E := Default(TOBDLogEntry);
    E.Timestamp := EncodeDateTime(2026, 5, 9, 10, 0, 1, 0);
    E.Kind := leResponse;
    E.ServiceID := $62;
    E.HasServiceID := True;
    E.Raw := TBytes.Create($F1, $90, $31, $48, $47);
    R.Append(E);

    E := Default(TOBDLogEntry);
    E.Timestamp := EncodeDateTime(2026, 5, 9, 10, 0, 2, 0);
    E.Kind := leNRC;
    E.ServiceID := $22;
    E.HasServiceID := True;
    E.NRC := $33;
    E.HasNRC := True;
    E.NRCText := 'security access denied';
    R.Append(E);
  finally
    R.Free;
  end;
end;

{ ---- Recorder -------------------------------------------------------------- }

procedure TRecorderTests.AppendThreeEntriesRoundTrip;
var
  Tmp: string;
  Entries: TArray<TOBDLogEntry>;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  try
    WriteThree(Tmp);
    Entries := TOBDReplayer.LoadAll(Tmp);
    Assert.AreEqual(3, Length(Entries));
    Assert.AreEqual(Ord(leInfo),     Ord(Entries[0].Kind));
    Assert.AreEqual(Ord(leResponse), Ord(Entries[1].Kind));
    Assert.AreEqual(Ord(leNRC),      Ord(Entries[2].Kind));
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TRecorderTests.RawBytesPreserved;
var
  Tmp: string;
  Entries: TArray<TOBDLogEntry>;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  try
    WriteThree(Tmp);
    Entries := TOBDReplayer.LoadAll(Tmp);
    Assert.AreEqual(5, Length(Entries[1].Raw));
    Assert.AreEqual($F1, Integer(Entries[1].Raw[0]));
    Assert.AreEqual($47, Integer(Entries[1].Raw[4]));
    Assert.AreEqual($62, Integer(Entries[1].ServiceID));
    Assert.AreEqual($33, Integer(Entries[2].NRC));
    Assert.AreEqual('security access denied', Entries[2].NRCText);
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TRecorderTests.CloseFlushesAndIsIdempotent;
var
  Tmp: string;
  R: TOBDRecorder;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  R := TOBDRecorder.Create(nil);
  try
    R.Open(Tmp);
    Assert.IsTrue(R.Active);
    R.Close;
    Assert.IsFalse(R.Active);
    R.Close; // idempotent
  finally
    R.Free;
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

{ ---- Replayer -------------------------------------------------------------- }

procedure TReplayerTests.PlaysEveryEntry;
var
  Tmp: string;
  Replayer: TOBDReplayer;
  Visited: TList<TOBDLogEntryKind>;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  Visited := TList<TOBDLogEntryKind>.Create;
  Replayer := TOBDReplayer.Create(nil);
  try
    WriteThree(Tmp);
    Replayer.FileName := Tmp;
    Replayer.Mode := rmAsFastAsPossible;
    Replayer.OnEntry :=
      procedure(Sender: TObject; const E: TOBDLogEntry)
      begin
        Visited.Add(E.Kind);
      end;
    Replayer.Play;
    Assert.AreEqual(3, Visited.Count);
    Assert.AreEqual(Ord(leInfo),     Ord(Visited[0]));
    Assert.AreEqual(Ord(leResponse), Ord(Visited[1]));
    Assert.AreEqual(Ord(leNRC),      Ord(Visited[2]));
  finally
    Replayer.Free;
    Visited.Free;
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TReplayerTests.CancelStopsReplay;
var
  Tmp: string;
  Replayer: TOBDReplayer;
  Count: Integer;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  Count := 0;
  Replayer := TOBDReplayer.Create(nil);
  try
    WriteThree(Tmp);
    Replayer.FileName := Tmp;
    Replayer.OnEntry :=
      procedure(Sender: TObject; const E: TOBDLogEntry)
      begin
        Inc(Count);
        if Count = 1 then Replayer.Stop;
      end;
    Replayer.Play;
    Assert.AreEqual(1, Count, 'Stop must halt at next entry boundary');
  finally
    Replayer.Free;
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TReplayerTests.PlayRaisesOnMissingFile;
var
  Replayer: TOBDReplayer;
begin
  Replayer := TOBDReplayer.Create(nil);
  try
    Replayer.FileName := 'C:\definitely-does-not-exist.obdlog';
    Assert.WillRaise(
      procedure begin Replayer.Play; end,
      EOBDConfig);
  finally
    Replayer.Free;
  end;
end;

procedure TReplayerTests.ModeDefault;
var
  Replayer: TOBDReplayer;
begin
  Replayer := TOBDReplayer.Create(nil);
  try
    Assert.AreEqual(Ord(rmAsFastAsPossible), Ord(Replayer.Mode));
  finally
    Replayer.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRecorderTests);
  TDUnitX.RegisterTestFixture(TReplayerTests);

end.
