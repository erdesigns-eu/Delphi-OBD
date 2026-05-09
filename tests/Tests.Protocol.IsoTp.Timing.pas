//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.IsoTp.Timing.pas
// CONTENTS       : Tests for OBD.Protocol.IsoTp.Timing
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Protocol.IsoTp.Timing;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TIsoTpTimingTests = class
  public
    /// <summary>Stmin byte zero is zero micros.</summary>
    [Test] procedure StminByteZeroIsZeroMicros;
    /// <summary>Stmin byte127 is127 milliseconds.</summary>
    [Test] procedure StminByte127Is127Milliseconds;
    /// <summary>Stmin byte f1 is hundred micros.</summary>
    [Test] procedure StminByteF1IsHundredMicros;
    /// <summary>Stmin byte f9 is nine hundred micros.</summary>
    [Test] procedure StminByteF9IsNineHundredMicros;
    /// <summary>Stmin reserved range raises.</summary>
    [Test] procedure StminReservedRangeRaises;
    /// <summary>Encode round trips milliseconds.</summary>
    [Test] procedure EncodeRoundTripsMilliseconds;
    /// <summary>Encode round trips microseconds.</summary>
    [Test] procedure EncodeRoundTripsMicroseconds;
    /// <summary>Encode rejects unrepresentable.</summary>
    [Test] procedure EncodeRejectsUnrepresentable;
    /// <summary>Compliant stream passes.</summary>
    [Test] procedure CompliantStreamPasses;
    /// <summary>Undershot gap flags violation.</summary>
    [Test] procedure UndershotGapFlagsViolation;
    /// <summary>Block size overrun flags violation.</summary>
    [Test] procedure BlockSizeOverrunFlagsViolation;
    /// <summary>Tolerance forgives small undershoot.</summary>
    [Test] procedure ToleranceForgivesSmallUndershoot;
    /// <summary>Reset after flow control.</summary>
    [Test] procedure ResetAfterFlowControl;
  end;

implementation

uses
  System.SysUtils, OBD.Protocol.IsoTp.Timing;

procedure TIsoTpTimingTests.StminByteZeroIsZeroMicros;
begin Assert.AreEqual(0, DecodeStminMicros($00)); end;

procedure TIsoTpTimingTests.StminByte127Is127Milliseconds;
begin Assert.AreEqual(127000, DecodeStminMicros($7F)); end;

procedure TIsoTpTimingTests.StminByteF1IsHundredMicros;
begin Assert.AreEqual(100, DecodeStminMicros($F1)); end;

procedure TIsoTpTimingTests.StminByteF9IsNineHundredMicros;
begin Assert.AreEqual(900, DecodeStminMicros($F9)); end;

procedure TIsoTpTimingTests.StminReservedRangeRaises;
begin
  Assert.WillRaise(procedure begin DecodeStminMicros($80); end, EOBDIsoTpTiming);
  Assert.WillRaise(procedure begin DecodeStminMicros($F0); end, EOBDIsoTpTiming);
  Assert.WillRaise(procedure begin DecodeStminMicros($FA); end, EOBDIsoTpTiming);
end;

procedure TIsoTpTimingTests.EncodeRoundTripsMilliseconds;
begin
  Assert.AreEqual($05, Integer(EncodeStminMicros(5000)));   // 5 ms
  Assert.AreEqual($7F, Integer(EncodeStminMicros(127000))); // 127 ms
end;

procedure TIsoTpTimingTests.EncodeRoundTripsMicroseconds;
begin
  Assert.AreEqual($F1, Integer(EncodeStminMicros(100)));
  Assert.AreEqual($F9, Integer(EncodeStminMicros(900)));
end;

procedure TIsoTpTimingTests.EncodeRejectsUnrepresentable;
begin
  Assert.WillRaise(procedure begin EncodeStminMicros(150); end, EOBDIsoTpTiming);
  Assert.WillRaise(procedure begin EncodeStminMicros(-1); end, EOBDIsoTpTiming);
  Assert.WillRaise(procedure begin EncodeStminMicros(200000); end, EOBDIsoTpTiming);
end;

function MkObs(Kind: TIsoTpFrameKind; T: Int64;
  IsTester: Boolean = True): TIsoTpFrameObservation;
begin
  Result.Kind := Kind;
  Result.TimestampMicros := T;
  Result.SenderIsTester := IsTester;
end;

procedure TIsoTpTimingTests.CompliantStreamPasses;
var
  Checker: TOBDIsoTpTimingChecker;
  Frames: TArray<TIsoTpFrameObservation>;
  R: TIsoTpTimingResult;
begin
  Checker := TOBDIsoTpTimingChecker.Create;
  try
    Checker.ApplyFlowControl($05, $00); // 5 ms STmin, unlimited BS
    Frames := [
      MkObs(iftFirst, 0),
      MkObs(iftFlowControl, 1000, False),
      MkObs(iftConsecutive, 2000),
      MkObs(iftConsecutive, 7100),  // gap 5100 us >= 5000 us
      MkObs(iftConsecutive, 12200)  // gap 5100 us
    ];
    R := Checker.Audit(Frames);
    Assert.IsTrue(R.Compliant, 'Compliant stream must not record violations');
  finally
    Checker.Free;
  end;
end;

procedure TIsoTpTimingTests.UndershotGapFlagsViolation;
var
  Checker: TOBDIsoTpTimingChecker;
  R: TIsoTpTimingResult;
begin
  Checker := TOBDIsoTpTimingChecker.Create;
  try
    Checker.ApplyFlowControl($05, $00); // 5 ms
    Checker.ToleranceMicros := 100;
    R := Checker.Audit([
      MkObs(iftFirst, 0),
      MkObs(iftFlowControl, 500, False),
      MkObs(iftConsecutive, 1000),
      MkObs(iftConsecutive, 2000)  // gap 1000us, way below 5000us
    ]);
    Assert.IsFalse(R.Compliant);
    Assert.AreEqual(1, Length(R.Violations));
    Assert.AreEqual(Ord(itvIntraGapTooSmall), Ord(R.Violations[0].Kind));
  finally
    Checker.Free;
  end;
end;

procedure TIsoTpTimingTests.BlockSizeOverrunFlagsViolation;
var
  Checker: TOBDIsoTpTimingChecker;
  R: TIsoTpTimingResult;
begin
  Checker := TOBDIsoTpTimingChecker.Create;
  try
    Checker.ApplyFlowControl($00, $02); // STmin=0, BS=2
    R := Checker.Audit([
      MkObs(iftFirst, 0),
      MkObs(iftFlowControl, 500, False),
      MkObs(iftConsecutive, 1000),
      MkObs(iftConsecutive, 2000),
      MkObs(iftConsecutive, 3000)  // 3rd CF after FC -> overrun
    ]);
    Assert.IsFalse(R.Compliant);
    Assert.IsTrue(Length(R.Violations) >= 1);
  finally
    Checker.Free;
  end;
end;

procedure TIsoTpTimingTests.ToleranceForgivesSmallUndershoot;
var
  Checker: TOBDIsoTpTimingChecker;
  R: TIsoTpTimingResult;
begin
  Checker := TOBDIsoTpTimingChecker.Create;
  try
    Checker.ApplyFlowControl($05, $00);
    Checker.ToleranceMicros := 500;
    R := Checker.Audit([
      MkObs(iftFirst, 0),
      MkObs(iftFlowControl, 500, False),
      MkObs(iftConsecutive, 1000),
      MkObs(iftConsecutive, 5800) // gap 4800us, 200us shy of 5000us — OK
    ]);
    Assert.IsTrue(R.Compliant);
  finally
    Checker.Free;
  end;
end;

procedure TIsoTpTimingTests.ResetAfterFlowControl;
var
  Checker: TOBDIsoTpTimingChecker;
  R: TIsoTpTimingResult;
begin
  Checker := TOBDIsoTpTimingChecker.Create;
  try
    Checker.ApplyFlowControl($00, $02); // BS=2
    R := Checker.Audit([
      MkObs(iftFirst, 0),
      MkObs(iftFlowControl, 1, False),
      MkObs(iftConsecutive, 100),
      MkObs(iftConsecutive, 200),
      MkObs(iftFlowControl, 300, False),  // resets the BS counter
      MkObs(iftConsecutive, 400),
      MkObs(iftConsecutive, 500)
    ]);
    Assert.IsTrue(R.Compliant);
  finally
    Checker.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TIsoTpTimingTests);

end.
