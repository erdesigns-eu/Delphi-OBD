//------------------------------------------------------------------------------
//  Tests.OBD.Recorder
//
//  Coverage for the recorder / replayer: TOBDRecorder Append + file-format
//  round-trip; TOBDReplayer parse / play / cancel.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
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
  OBD.Protocol.Types,
  OBD.Recorder,
  OBD.Replayer,
  OBD.Recorder.ProtocolMock,
  OBD.Recorder.Redactor;

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
    [Test] procedure MaxGapMsDefault;
  end;

  [TestFixture]
  TGzipRoundTripTests = class
  public
    [Test] procedure GzipRecorderProducesReadableLog;
  end;

  [TestFixture]
  TProtocolMockTests = class
  public
    [Test] procedure MockFiresFrameResponseAndNRC;
  end;

  [TestFixture]
  TRedactorTests = class
  public
    [Test] procedure FilterDropsAndMutates;
    [Test] procedure ServiceIDPayloadWiperClearsRaw;
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

procedure TReplayerTests.MaxGapMsDefault;
var
  Replayer: TOBDReplayer;
begin
  Replayer := TOBDReplayer.Create(nil);
  try
    Assert.AreEqual(Cardinal(60000), Replayer.MaxGapMs);
    Replayer.MaxGapMs := 250;
    Assert.AreEqual(Cardinal(250), Replayer.MaxGapMs);
  finally
    Replayer.Free;
  end;
end;

{ ---- Gzip round-trip ------------------------------------------------------- }

procedure TGzipRoundTripTests.GzipRecorderProducesReadableLog;
var
  Tmp: string;
  Recorder: TOBDRecorder;
  E: TOBDLogEntry;
  Entries: TArray<TOBDLogEntry>;
  RawSize: Int64;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog.gz');
  Recorder := TOBDRecorder.Create(nil);
  try
    Recorder.Open(Tmp);
    Assert.IsTrue(Recorder.Compressed,
      '.gz extension must enable compression');
    E := Default(TOBDLogEntry);
    E.Timestamp := EncodeDateTime(2026, 5, 9, 10, 0, 0, 0);
    E.Kind := leResponse;
    E.ServiceID := $62;
    E.HasServiceID := True;
    E.Raw := TBytes.Create($F1, $90, $31, $48, $47);
    Recorder.Append(E);

    E := Default(TOBDLogEntry);
    E.Timestamp := EncodeDateTime(2026, 5, 9, 10, 0, 0, 100);
    E.Kind := leInfo;
    E.Message := 'second entry';
    Recorder.Append(E);
    Recorder.Close;

    RawSize := TFile.GetSize(Tmp);
    Assert.IsTrue(RawSize > 0, 'gzip file must not be empty');

    Entries := TOBDReplayer.LoadAll(Tmp);
    Assert.AreEqual(2, Length(Entries));
    Assert.AreEqual($62, Integer(Entries[0].ServiceID));
    Assert.AreEqual('second entry', Entries[1].Message);
  finally
    Recorder.Free;
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

{ ---- Protocol mock --------------------------------------------------------- }

procedure TProtocolMockTests.MockFiresFrameResponseAndNRC;
var
  Tmp: string;
  Recorder: TOBDRecorder;
  Mock: TOBDProtocolMock;
  E: TOBDLogEntry;
  FrameCount, RespCount, NRCCount: Integer;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  FrameCount := 0; RespCount := 0; NRCCount := 0;
  Recorder := TOBDRecorder.Create(nil);
  try
    Recorder.Open(Tmp);
    E := Default(TOBDLogEntry);
    E.Timestamp := Now;
    E.Kind := leFrame;
    E.FrameID := $7E8;
    E.HasFrameID := True;
    E.Raw := TBytes.Create($03, $41, $0C, $1A, $F8);
    Recorder.Append(E);

    E := Default(TOBDLogEntry);
    E.Timestamp := Now;
    E.Kind := leResponse;
    E.ServiceID := $41;
    E.HasServiceID := True;
    E.Raw := TBytes.Create($0C, $1A, $F8);
    E.ElapsedMs := 12;
    Recorder.Append(E);

    E := Default(TOBDLogEntry);
    E.Timestamp := Now;
    E.Kind := leNRC;
    E.ServiceID := $22;
    E.HasServiceID := True;
    E.NRC := $33;
    E.HasNRC := True;
    E.NRCText := 'security access denied';
    Recorder.Append(E);
    Recorder.Close;
  finally
    Recorder.Free;
  end;

  Mock := TOBDProtocolMock.Create(nil);
  try
    Mock.FileName := Tmp;
    Mock.OnFrame :=
      procedure(Sender: TObject; const AFrame: TOBDFrame)
      begin
        Inc(FrameCount);
        Assert.AreEqual(Cardinal($7E8), AFrame.Id);
        Assert.AreEqual(5, Length(AFrame.Payload));
      end;
    Mock.OnResponse :=
      procedure(Sender: TObject; const AResponse: TOBDResponse)
      begin
        Inc(RespCount);
        Assert.AreEqual($41, Integer(AResponse.ServiceID));
        Assert.AreEqual(Cardinal(12), AResponse.Elapsed);
      end;
    Mock.OnNRC :=
      procedure(Sender: TObject; const ARequest: TOBDRequest;
        ANRC: Byte; const AText: string)
      begin
        Inc(NRCCount);
        Assert.AreEqual($22, Integer(ARequest.ServiceID));
        Assert.AreEqual($33, Integer(ANRC));
      end;
    Mock.Run;
    Assert.AreEqual(1, FrameCount);
    Assert.AreEqual(1, RespCount);
    Assert.AreEqual(1, NRCCount);
  finally
    Mock.Free;
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

{ ---- Redactor -------------------------------------------------------------- }

procedure TRedactorTests.FilterDropsAndMutates;
var
  Src, Dst: string;
  Entries: TArray<TOBDLogEntry>;
  Written: Integer;
begin
  Src := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  Dst := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  try
    WriteThree(Src);
    Written := TOBDLogRedactor.Redact(Src, Dst,
      procedure(var AEntry: TOBDLogEntry; var AKeep: Boolean)
      begin
        if AEntry.Kind = leNRC then
          AKeep := False
        else if AEntry.Kind = leInfo then
          AEntry.Message := '[scrubbed]';
      end);
    Assert.AreEqual(2, Written);
    Entries := TOBDReplayer.LoadAll(Dst);
    Assert.AreEqual(2, Length(Entries));
    Assert.AreEqual('[scrubbed]', Entries[0].Message);
    Assert.AreEqual(Ord(leResponse), Ord(Entries[1].Kind));
  finally
    if TFile.Exists(Src) then TFile.Delete(Src);
    if TFile.Exists(Dst) then TFile.Delete(Dst);
  end;
end;

procedure TRedactorTests.ServiceIDPayloadWiperClearsRaw;
var
  Src, Dst: string;
  Entries: TArray<TOBDLogEntry>;
begin
  Src := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  Dst := TPath.Combine(TPath.GetTempPath, 'obdlog-' +
    TGUID.NewGuid.ToString + '.obdlog');
  try
    WriteThree(Src);
    TOBDLogRedactor.Redact(Src, Dst,
      TOBDLogRedactor.MakeServiceIDPayloadWiper([$62]));
    Entries := TOBDReplayer.LoadAll(Dst);
    Assert.AreEqual(3, Length(Entries));
    // The leResponse entry has ServiceID=0x62 — its raw must be empty.
    Assert.AreEqual(0, Length(Entries[1].Raw),
      'wiper must clear matching service-id payload');
    // Other entries left intact.
    Assert.AreEqual($33, Integer(Entries[2].NRC));
  finally
    if TFile.Exists(Src) then TFile.Delete(Src);
    if TFile.Exists(Dst) then TFile.Delete(Dst);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRecorderTests);
  TDUnitX.RegisterTestFixture(TReplayerTests);
  TDUnitX.RegisterTestFixture(TGzipRoundTripTests);
  TDUnitX.RegisterTestFixture(TProtocolMockTests);
  TDUnitX.RegisterTestFixture(TRedactorTests);

end.
