//------------------------------------------------------------------------------
//  Tests.OBD.Utilities
//
//  Coverage for the host-utility surface: Logger sinks +
//  singleton, AttemptCounter, Nonce CSPRNG, StringHelpers.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.Utilities;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Logger.Sinks,
  OBD.Logger,
  OBD.Security.AttemptCounter,
  OBD.Security.Nonce,
  OBD.StringHelpers;

type
  /// <summary>DUnitX fixture for the utilities surface.</summary>
  [TestFixture]
  TUtilitiesTests = class
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;

    [Test] procedure Logger_RegisterAndUnregisterSink;
    [Test] procedure Logger_LevelFilterDropsBelowMin;
    [Test] procedure Logger_DispatchesToMemorySink;
    [Test] procedure Logger_MultipleSinksReceiveSameEvent;
    [Test] procedure LogFormat_FormatsLevelAndCategory;
    [Test] procedure MemorySink_RingBufferDropsOldest;

    [Test] procedure AttemptCounter_FailureRegistersAndCounts;
    [Test] procedure AttemptCounter_LocksOutAtThreshold;
    [Test] procedure AttemptCounter_SuccessResets;
    [Test] procedure AttemptCounter_UnlockSecondsZeroWhenUnlocked;

    [Test] procedure Nonce_BytesNonNegativeRaises;
    [Test] procedure Nonce_BytesLengthMatches;
    [Test] procedure Nonce_32AndDifferent64;
    [Test] procedure Nonce_HexEncodeMatchesBytes;

    [Test] procedure Strings_HexUpperRoundTrip;
    [Test] procedure Strings_HexLowerRoundTrip;
    [Test] procedure Strings_HexSeparatedSpaces;
    [Test] procedure Strings_HexToBytesIgnoresWhitespace;
    [Test] procedure Strings_HexToBytesRejectsOddLength;
    [Test] procedure Strings_PrintableAsciiReplacesNonPrint;
    [Test] procedure Strings_HexDumpFormatsLines;
  end;

implementation

procedure TUtilitiesTests.SetUp;
begin
  TOBDLogger.ReleaseInstance;
end;

procedure TUtilitiesTests.TearDown;
begin
  TOBDLogger.ReleaseInstance;
end;

{ ---- Logger -------------------------------------------------------------- }

procedure TUtilitiesTests.Logger_RegisterAndUnregisterSink;
var
  Sink: IOBDLogSink;
begin
  Sink := TOBDLogMemorySink.Create(8);
  TOBDLogger.Instance.RegisterSink(Sink);
  TOBDLogger.Instance.RegisterSink(Sink);                // duplicate
  Assert.AreEqual(1, TOBDLogger.Instance.SinkCount);
  TOBDLogger.Instance.UnregisterSink(Sink);
  Assert.AreEqual(0, TOBDLogger.Instance.SinkCount);
end;

procedure TUtilitiesTests.Logger_LevelFilterDropsBelowMin;
var
  Sink: TOBDLogMemorySink;
  ISink: IOBDLogSink;
begin
  Sink := TOBDLogMemorySink.Create(16);
  ISink := Sink;
  TOBDLogger.Instance.RegisterSink(ISink);
  TOBDLogger.Instance.MinLevel := olWarning;
  TOBDLogger.Instance.Info('drop');
  TOBDLogger.Instance.Warning('keep');
  Assert.AreEqual(1, Sink.Count);
end;

procedure TUtilitiesTests.Logger_DispatchesToMemorySink;
var
  Sink: TOBDLogMemorySink;
  ISink: IOBDLogSink;
  Snap: TArray<TOBDLogEvent>;
begin
  Sink := TOBDLogMemorySink.Create(16);
  ISink := Sink;
  TOBDLogger.Instance.RegisterSink(ISink);
  TOBDLogger.Instance.MinLevel := olDebug;
  TOBDLogger.Instance.Error('boom', 'cat');
  Snap := Sink.Snapshot;
  Assert.AreEqual(1, Length(Snap));
  Assert.AreEqual(Ord(olError), Ord(Snap[0].Level));
  Assert.AreEqual('cat', Snap[0].Category);
  Assert.AreEqual('boom', Snap[0].Message);
end;

procedure TUtilitiesTests.Logger_MultipleSinksReceiveSameEvent;
var
  S1, S2: TOBDLogMemorySink;
  I1, I2: IOBDLogSink;
begin
  S1 := TOBDLogMemorySink.Create(8);
  S2 := TOBDLogMemorySink.Create(8);
  I1 := S1;
  I2 := S2;
  TOBDLogger.Instance.RegisterSink(I1);
  TOBDLogger.Instance.RegisterSink(I2);
  TOBDLogger.Instance.MinLevel := olDebug;
  TOBDLogger.Instance.Info('fanout');
  Assert.AreEqual(1, S1.Count);
  Assert.AreEqual(1, S2.Count);
end;

procedure TUtilitiesTests.LogFormat_FormatsLevelAndCategory;
var
  E: TOBDLogEvent;
  Line: string;
begin
  E := Default(TOBDLogEvent);
  E.Level := olWarning;
  E.Timestamp := EncodeDate(2026, 5, 11) + EncodeTime(12, 0, 0, 0);
  E.Category := 'UDS';
  E.Message := 'NRC 0x33';
  Line := FormatLogLine(E);
  Assert.Contains(Line, '[WARN ]');
  Assert.Contains(Line, '[UDS]');
  Assert.Contains(Line, 'NRC 0x33');
end;

procedure TUtilitiesTests.MemorySink_RingBufferDropsOldest;
var
  S: TOBDLogMemorySink;
  E: TOBDLogEvent;
  I: Integer;
begin
  S := TOBDLogMemorySink.Create(3);
  try
    E := Default(TOBDLogEvent);
    E.Level := olInfo;
    for I := 1 to 5 do
    begin
      E.Message := IntToStr(I);
      S.Write(E);
    end;
    Assert.AreEqual(3, S.Count);
    Assert.AreEqual('3', S.Snapshot[0].Message);
    Assert.AreEqual('5', S.Snapshot[2].Message);
  finally
    S.Free;
  end;
end;

{ ---- AttemptCounter ----------------------------------------------------- }

procedure TUtilitiesTests.AttemptCounter_FailureRegistersAndCounts;
var
  C: TOBDAttemptCounter;
begin
  C := TOBDAttemptCounter.Create(3, 60);
  try
    Assert.AreEqual(0, C.FailureCount);
    C.RegisterFailure;
    C.RegisterFailure;
    Assert.AreEqual(2, C.FailureCount);
    Assert.IsFalse(C.IsLockedOut);
  finally
    C.Free;
  end;
end;

procedure TUtilitiesTests.AttemptCounter_LocksOutAtThreshold;
var
  C: TOBDAttemptCounter;
begin
  C := TOBDAttemptCounter.Create(3, 60);
  try
    C.RegisterFailure;
    C.RegisterFailure;
    C.RegisterFailure;
    Assert.IsTrue(C.IsLockedOut);
    Assert.IsTrue(C.SecondsUntilUnlock > 0);
  finally
    C.Free;
  end;
end;

procedure TUtilitiesTests.AttemptCounter_SuccessResets;
var
  C: TOBDAttemptCounter;
begin
  C := TOBDAttemptCounter.Create(2, 60);
  try
    C.RegisterFailure;
    C.RegisterFailure;
    Assert.IsTrue(C.IsLockedOut);
    C.RegisterSuccess;
    Assert.AreEqual(0, C.FailureCount);
    Assert.IsFalse(C.IsLockedOut);
  finally
    C.Free;
  end;
end;

procedure TUtilitiesTests.AttemptCounter_UnlockSecondsZeroWhenUnlocked;
var
  C: TOBDAttemptCounter;
begin
  C := TOBDAttemptCounter.Create(5, 60);
  try
    C.RegisterFailure;
    Assert.AreEqual(0, C.SecondsUntilUnlock);
  finally
    C.Free;
  end;
end;

{ ---- Nonce -------------------------------------------------------------- }

procedure TUtilitiesTests.Nonce_BytesNonNegativeRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      NonceBytes(0);
    end,
    EOBDNonce);
end;

procedure TUtilitiesTests.Nonce_BytesLengthMatches;
var
  B: TBytes;
begin
  B := NonceBytes(16);
  Assert.AreEqual(16, Length(B));
end;

procedure TUtilitiesTests.Nonce_32AndDifferent64;
var
  A, B: UInt64;
begin
  A := Nonce64;
  B := Nonce64;
  // Two consecutive 64-bit CSPRNG draws have a vanishing
  // probability of collision; if they collide here the test
  // is honest about a real backend regression.
  Assert.AreNotEqual(Integer(A and $FFFFFFFF), Integer(B and $FFFFFFFF));
end;

procedure TUtilitiesTests.Nonce_HexEncodeMatchesBytes;
var
  B: TBytes;
  S: string;
begin
  B := TBytes.Create($00, $AB, $CD, $EF);
  S := HexEncode(B);
  Assert.AreEqual('00abcdef', S);
end;

{ ---- StringHelpers ------------------------------------------------------ }

procedure TUtilitiesTests.Strings_HexUpperRoundTrip;
var
  B: TBytes;
begin
  B := TBytes.Create($12, $AB, $FF);
  Assert.AreEqual('12ABFF', BytesToHexUpper(B));
end;

procedure TUtilitiesTests.Strings_HexLowerRoundTrip;
var
  B: TBytes;
begin
  B := TBytes.Create($12, $AB, $FF);
  Assert.AreEqual('12abff', BytesToHexLower(B));
end;

procedure TUtilitiesTests.Strings_HexSeparatedSpaces;
var
  B: TBytes;
begin
  B := TBytes.Create($7E, $03, $22, $F1, $90);
  Assert.AreEqual('7E 03 22 F1 90', BytesToHexSeparated(B, ' '));
end;

procedure TUtilitiesTests.Strings_HexToBytesIgnoresWhitespace;
var
  B: TBytes;
begin
  B := HexToBytes('  7E 03 '#9'22 F1'#13#10'90 ');
  Assert.AreEqual(5, Length(B));
  Assert.AreEqual($7E, Integer(B[0]));
  Assert.AreEqual($90, Integer(B[4]));
end;

procedure TUtilitiesTests.Strings_HexToBytesRejectsOddLength;
begin
  Assert.WillRaise(
    procedure
    begin
      HexToBytes('ABC');
    end,
    EConvertError);
end;

procedure TUtilitiesTests.Strings_PrintableAsciiReplacesNonPrint;
var
  B: TBytes;
begin
  B := TBytes.Create(Ord('A'), $01, Ord('B'), $7F, Ord('C'));
  Assert.AreEqual('A.B.C', BytesToPrintableAscii(B, '.'));
end;

procedure TUtilitiesTests.Strings_HexDumpFormatsLines;
var
  Dump: string;
begin
  Dump := FormatHexDump(
    TBytes.Create(Ord('H'), Ord('i'), $00, $01), 4);
  Assert.Contains(Dump, '00000000');
  Assert.Contains(Dump, '48 69');
  Assert.Contains(Dump, 'Hi');
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilitiesTests);

end.
