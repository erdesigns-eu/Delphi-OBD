//------------------------------------------------------------------------------
//  Tests.OBD.Connection.Retry
//
//  DUnitX coverage for TOBDRetryPolicy. Asserts the exponential
//  backoff curve, the MaxDelay clamp, and the jitter envelope.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Connection.Retry;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   Coverage for <c>TOBDRetryPolicy</c>.
  /// </summary>
  /// <remarks>
  ///   Jitter RNG is seeded for repeatable assertions.
  /// </remarks>
  [TestFixture]
  TRetryPolicyTests = class
  public
    /// <summary>Defaults match the documented values.</summary>
    [Test] procedure DefaultsAreDocumented;
    /// <summary>Without jitter the curve is exactly exponential.</summary>
    [Test] procedure ExponentialCurveWithoutJitter;
    /// <summary>The curve is clamped at MaxDelayMs.</summary>
    [Test] procedure CurveClampedAtMaxDelay;
    /// <summary>Jitter envelope: every sample lies within ±N% of the
    /// nominal delay.</summary>
    [Test] procedure JitterStaysInsideEnvelope;
    /// <summary>Attempt index zero raises.</summary>
    [Test] procedure AttemptZeroRaises;
    /// <summary>Assign copies every published property.</summary>
    [Test] procedure AssignCopiesAllFields;
  end;

implementation

uses
  System.SysUtils,
  OBD.Connection.Retry;

procedure TRetryPolicyTests.DefaultsAreDocumented;
var
  P: TOBDRetryPolicy;
begin
  P := TOBDRetryPolicy.Create;
  try
    Assert.IsTrue(P.Enabled);
    Assert.AreEqual<Cardinal>(3, P.MaxAttempts);
    Assert.AreEqual<Cardinal>(200, P.InitialDelayMs);
    Assert.AreEqual<Cardinal>(5000, P.MaxDelayMs);
    Assert.AreEqual(2.0, P.Multiplier, 1E-9);
    Assert.AreEqual<Byte>(10, P.JitterPercent);
  finally
    P.Free;
  end;
end;

procedure TRetryPolicyTests.ExponentialCurveWithoutJitter;
var
  P: TOBDRetryPolicy;
begin
  P := TOBDRetryPolicy.Create;
  try
    P.JitterPercent := 0;
    P.MaxDelayMs := 100000;
    Assert.AreEqual<Cardinal>(200,  P.DelayForAttempt(1));
    Assert.AreEqual<Cardinal>(400,  P.DelayForAttempt(2));
    Assert.AreEqual<Cardinal>(800,  P.DelayForAttempt(3));
    Assert.AreEqual<Cardinal>(1600, P.DelayForAttempt(4));
  finally
    P.Free;
  end;
end;

procedure TRetryPolicyTests.CurveClampedAtMaxDelay;
var
  P: TOBDRetryPolicy;
begin
  P := TOBDRetryPolicy.Create;
  try
    P.JitterPercent := 0;
    P.MaxDelayMs := 1000;
    Assert.AreEqual<Cardinal>(1000, P.DelayForAttempt(10));
  finally
    P.Free;
  end;
end;

procedure TRetryPolicyTests.JitterStaysInsideEnvelope;
var
  P: TOBDRetryPolicy;
  I, MinV, MaxV, Sample: Integer;
begin
  P := TOBDRetryPolicy.Create;
  try
    P.JitterPercent := 25;
    P.SeedJitter(12345);
    P.MaxDelayMs := 100000;
    // Nominal delay for attempt 3 = 200 * 2 * 2 = 800; envelope ±25% = ±200.
    MinV := MaxInt;
    MaxV := -MaxInt;
    for I := 0 to 99 do
    begin
      Sample := P.DelayForAttempt(3);
      if Sample < MinV then MinV := Sample;
      if Sample > MaxV then MaxV := Sample;
    end;
    Assert.IsTrue(MinV >= 600,
      Format('Jitter envelope lower bound violated: min %d', [MinV]));
    Assert.IsTrue(MaxV <= 1000,
      Format('Jitter envelope upper bound violated: max %d', [MaxV]));
  finally
    P.Free;
  end;
end;

procedure TRetryPolicyTests.AttemptZeroRaises;
var
  P: TOBDRetryPolicy;
begin
  P := TOBDRetryPolicy.Create;
  try
    Assert.WillRaise(
      procedure begin P.DelayForAttempt(0); end,
      EArgumentOutOfRangeException);
  finally
    P.Free;
  end;
end;

procedure TRetryPolicyTests.AssignCopiesAllFields;
var
  Source, Dest: TOBDRetryPolicy;
begin
  Source := TOBDRetryPolicy.Create;
  Dest := TOBDRetryPolicy.Create;
  try
    Source.Enabled := False;
    Source.MaxAttempts := 7;
    Source.InitialDelayMs := 12345;
    Source.MaxDelayMs := 42;
    Source.Multiplier := 3.5;
    Source.JitterPercent := 99;
    Dest.Assign(Source);
    Assert.IsFalse(Dest.Enabled);
    Assert.AreEqual<Cardinal>(7, Dest.MaxAttempts);
    Assert.AreEqual<Cardinal>(12345, Dest.InitialDelayMs);
    Assert.AreEqual<Cardinal>(42, Dest.MaxDelayMs);
    Assert.AreEqual(3.5, Dest.Multiplier, 1E-9);
    Assert.AreEqual<Byte>(99, Dest.JitterPercent);
  finally
    Source.Free;
    Dest.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRetryPolicyTests);

end.
