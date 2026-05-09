//------------------------------------------------------------------------------
//  Tests.OBD.Flashing.Phase9b
//
//  Phase 9b coverage: TOBDVoltageGate latch behaviour,
//  TOBDFlashCheckpoint round-trip + image hash mismatch detection,
//  TOBDFlashCheckList phase-runner ordering + abort-on-error.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 9b initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Flashing.Phase9b;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.UDS.Transfer,
  OBD.Flash.VoltageGate,
  OBD.Flash.Checkpoint,
  OBD.Flash.Phases;

type
  /// <summary>Voltage gate.</summary>
  [TestFixture]
  TVoltageGateTests = class
  public
    [Test] procedure RaisesWhenNoSourceConfigured;
    [Test] procedure LatchesAbortAfterHoldTime;
    [Test] procedure RecoversWhenVoltageReturns;
    [Test] procedure DefaultsArePerSpec;
  end;

  /// <summary>Checkpoint store.</summary>
  [TestFixture]
  TCheckpointTests = class
  public
    [Test] procedure RoundTripPreservesCursor;
    [Test] procedure MatchesImageDetectsTamperedImage;
    [Test] procedure LoadRejectsWrongVersion;
    [Test] procedure ImageHashSHA256Pinned;
  end;

  /// <summary>Phase / check list.</summary>
  [TestFixture]
  TFlashPhasesTests = class
  public
    [Test] procedure RunPhasePassesWhenAllChecksPass;
    [Test] procedure RunPhaseAbortsOnFirstErrorSeverityFail;
    [Test] procedure WarningsDoNotAbort;
    [Test] procedure VisitorReceivesEveryResult;
    [Test] procedure VoltageFloorHelperVector;
  end;

implementation

{ ---- Voltage gate ---------------------------------------------------------- }

procedure TVoltageGateTests.RaisesWhenNoSourceConfigured;
var
  G: TOBDVoltageGate;
begin
  G := TOBDVoltageGate.Create(nil);
  try
    G.PollIntervalMs := 30;
    G.HoldTimeMs := 10;
    G.MinimumVoltage := 12.0;
    // Without a source the polling thread will raise inside
    // ReadVoltage and emit OnAbort. Wait briefly then assert
    // the gate stopped.
    G.Start;
    Sleep(150);
    Assert.IsFalse(G.IsRunning,
      'gate must abort and stop when no source is configured');
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.LatchesAbortAfterHoldTime;
var
  G: TOBDVoltageGate;
  Aborted: Boolean;
begin
  G := TOBDVoltageGate.Create(nil);
  try
    G.MinimumVoltage := 12.0;
    G.PollIntervalMs := 20;
    G.HoldTimeMs := 60;
    G.SourceFunc := function: Double begin Result := 11.0; end;
    Aborted := False;
    G.OnAbort :=
      procedure(Sender: TObject; V: Double; R: string)
      begin
        Aborted := True;
      end;
    G.Start;
    Sleep(250);
    Assert.IsTrue(Aborted, 'OnAbort must latch within HoldTimeMs');
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.RecoversWhenVoltageReturns;
var
  G: TOBDVoltageGate;
  Aborted: Boolean;
  ReadCount: Integer;
begin
  G := TOBDVoltageGate.Create(nil);
  try
    G.MinimumVoltage := 12.0;
    G.PollIntervalMs := 20;
    G.HoldTimeMs := 200;
    Aborted := False;
    ReadCount := 0;
    G.SourceFunc :=
      function: Double
      begin
        Inc(ReadCount);
        // First two reads dip; rest are healthy. Hold time is
        // 200 ms, polls happen every 20 ms — the dip lasts ~40
        // ms so the latch must NOT fire.
        if ReadCount <= 2 then Result := 11.0 else Result := 13.5;
      end;
    G.OnAbort :=
      procedure(Sender: TObject; V: Double; R: string) begin
        Aborted := True;
      end;
    G.Start;
    Sleep(150);
    G.Stop;
    Assert.IsFalse(Aborted,
      'a transient dip shorter than HoldTimeMs must not latch');
  finally
    G.Free;
  end;
end;

procedure TVoltageGateTests.DefaultsArePerSpec;
var
  G: TOBDVoltageGate;
begin
  G := TOBDVoltageGate.Create(nil);
  try
    Assert.AreEqual(12.0, G.MinimumVoltage, 0.001);
    Assert.AreEqual(Cardinal(200), G.PollIntervalMs);
    Assert.AreEqual(Cardinal(1000), G.HoldTimeMs);
  finally
    G.Free;
  end;
end;

{ ---- Checkpoint ------------------------------------------------------------ }

procedure TCheckpointTests.RoundTripPreservesCursor;
var
  Tmp: string;
  Image: TBytes;
  Info, Loaded: TOBDFlashCheckpointInfo;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cp-' +
    TGUID.NewGuid.ToString + '.json');
  Image := TBytes.Create($AA, $BB, $CC, $DD, $EE, $FF);
  Info := Default(TOBDFlashCheckpointInfo);
  Info.SessionID := 'TEST';
  Info.ImageSha256 := TOBDFlashCheckpoint.ComputeImageHash(Image);
  Info.Cursor.Address := $40000000;
  Info.Cursor.TotalBytes := UInt32(Length(Image));
  Info.Cursor.BytesSent := 4;
  Info.Cursor.NextBSC := 5;
  Info.Cursor.MaxChunkBytes := 254;
  Info.Vendor := 'vag';
  Info.Module := 'engine';
  try
    TOBDFlashCheckpoint.Save(Tmp, Info);
    Loaded := TOBDFlashCheckpoint.Load(Tmp);
    Assert.AreEqual('TEST', Loaded.SessionID);
    Assert.AreEqual(Integer($40000000),
      Integer(Loaded.Cursor.Address));
    Assert.AreEqual(4,   Integer(Loaded.Cursor.BytesSent));
    Assert.AreEqual(5,   Integer(Loaded.Cursor.NextBSC));
    Assert.AreEqual(254, Integer(Loaded.Cursor.MaxChunkBytes));
    Assert.AreEqual('vag', Loaded.Vendor);
    Assert.AreEqual('engine', Loaded.Module);
    Assert.IsTrue(TOBDFlashCheckpoint.MatchesImage(Loaded, Image));
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TCheckpointTests.MatchesImageDetectsTamperedImage;
var
  Image, Tampered: TBytes;
  Info: TOBDFlashCheckpointInfo;
begin
  Image := TBytes.Create($00, $01, $02);
  Tampered := TBytes.Create($00, $01, $03);
  Info := Default(TOBDFlashCheckpointInfo);
  Info.ImageSha256 := TOBDFlashCheckpoint.ComputeImageHash(Image);
  Assert.IsTrue (TOBDFlashCheckpoint.MatchesImage(Info, Image));
  Assert.IsFalse(TOBDFlashCheckpoint.MatchesImage(Info, Tampered));
end;

procedure TCheckpointTests.LoadRejectsWrongVersion;
var
  Tmp: string;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cp-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{"version": 99, "session": "x"}', TEncoding.UTF8);
  try
    Assert.WillRaise(
      procedure begin TOBDFlashCheckpoint.Load(Tmp); end,
      EOBDProtocol);
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TCheckpointTests.ImageHashSHA256Pinned;
var
  Hash: TBytes;
begin
  // SHA-256("abc") = ba7816bf 8f01cfea 414140de 5dae2223
  //                  b00361a3 96177a9c b410ff61 f20015ad
  Hash := TOBDFlashCheckpoint.ComputeImageHash(
    TEncoding.ASCII.GetBytes('abc'));
  Assert.AreEqual(32, Length(Hash));
  Assert.AreEqual($BA, Integer(Hash[0]));
  Assert.AreEqual($AD, Integer(Hash[31]));
end;

{ ---- Flash phases ---------------------------------------------------------- }

procedure TFlashPhasesTests.RunPhasePassesWhenAllChecksPass;
var
  L: TOBDFlashCheckList;
  Err: TOBDCheckResult;
begin
  L := TOBDFlashCheckList.Create;
  try
    L.AddCheck(fpPreflight, csError, 'always-pass',
      function(out M: string): Boolean
      begin
        M := '';
        Result := True;
      end);
    Assert.IsTrue(L.RunPhase(fpPreflight, Err));
  finally
    L.Free;
  end;
end;

procedure TFlashPhasesTests.RunPhaseAbortsOnFirstErrorSeverityFail;
var
  L: TOBDFlashCheckList;
  Err: TOBDCheckResult;
begin
  L := TOBDFlashCheckList.Create;
  try
    L.AddCheck(fpPreflight, csError, 'fail-1',
      function(out M: string): Boolean
      begin
        M := 'reason 1';
        Result := False;
      end);
    L.AddCheck(fpPreflight, csError, 'fail-2',
      function(out M: string): Boolean
      begin
        M := 'reason 2';
        Result := False;
      end);
    Assert.IsFalse(L.RunPhase(fpPreflight, Err));
    Assert.AreEqual('fail-1', Err.Name,
      'first failing csError must be reported');
  finally
    L.Free;
  end;
end;

procedure TFlashPhasesTests.WarningsDoNotAbort;
var
  L: TOBDFlashCheckList;
  Err: TOBDCheckResult;
begin
  L := TOBDFlashCheckList.Create;
  try
    L.AddCheck(fpPreflight, csWarning, 'warn',
      function(out M: string): Boolean
      begin
        M := 'just a warning';
        Result := False;
      end);
    Assert.IsTrue(L.RunPhase(fpPreflight, Err));
  finally
    L.Free;
  end;
end;

procedure TFlashPhasesTests.VisitorReceivesEveryResult;
var
  L: TOBDFlashCheckList;
  Err: TOBDCheckResult;
  Visited: TList<string>;
begin
  L := TOBDFlashCheckList.Create;
  Visited := TList<string>.Create;
  try
    L.AddCheck(fpPreflight, csInfo, 'A',
      function(out M: string): Boolean
      begin M := ''; Result := True; end);
    L.AddCheck(fpPreflight, csInfo, 'B',
      function(out M: string): Boolean
      begin M := ''; Result := True; end);
    L.RunPhase(fpPreflight, Err,
      procedure(R: TOBDCheckResult) begin Visited.Add(R.Name); end);
    Assert.AreEqual(2, Visited.Count);
    Assert.AreEqual('A', Visited[0]);
    Assert.AreEqual('B', Visited[1]);
  finally
    Visited.Free;
    L.Free;
  end;
end;

procedure TFlashPhasesTests.VoltageFloorHelperVector;
var
  Func: TOBDFlashCheckFunc;
  Msg: string;
begin
  Func := TOBDFlashChecks.VoltageFloor(
    function: Double begin Result := 11.5; end, 12.0);
  Assert.IsFalse(Func(Msg));
  Assert.IsTrue(Pos('11.', Msg) > 0);
  Func := TOBDFlashChecks.VoltageFloor(
    function: Double begin Result := 13.0; end, 12.0);
  Assert.IsTrue(Func(Msg));
  Assert.AreEqual('', Msg);
end;

initialization
  TDUnitX.RegisterTestFixture(TVoltageGateTests);
  TDUnitX.RegisterTestFixture(TCheckpointTests);
  TDUnitX.RegisterTestFixture(TFlashPhasesTests);

end.
