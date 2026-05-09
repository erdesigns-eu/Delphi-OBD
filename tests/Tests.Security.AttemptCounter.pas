//------------------------------------------------------------------------------
// UNIT           : Tests.Security.AttemptCounter
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Security.AttemptCounter;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TAttemptCounterTests = class
  public
    [Test] procedure FreeAttemptsAreNotLockedOut;
    [Test] procedure ExtraFailureLocksOut;
    [Test] procedure SuccessResetsTheCounter;
    [Test] procedure ResetClearsState;
    [Test] procedure LockoutCapsAtMaxLockoutSeconds;
    [Test] procedure DistinctIdentitiesDoNotInterfere;
  end;

implementation

uses
  System.SysUtils, OBD.Security.AttemptCounter;

//------------------------------------------------------------------------------
// FREE ATTEMPTS ARE NOT LOCKED OUT
//------------------------------------------------------------------------------
procedure TAttemptCounterTests.FreeAttemptsAreNotLockedOut;
var
  C: TOBDAttemptCounter;
  Wait: Integer;
  I: Integer;
begin
  C := TOBDAttemptCounter.Create;
  try
    C.FreeAttempts := 3;
    C.BaseLockoutSeconds := 30;
    for I := 1 to 3 do C.RegisterFailure('rad-001');
    Assert.IsTrue(C.IsAllowed('rad-001', Wait),
      'within free attempts must remain allowed');
  finally
    C.Free;
  end;
end;

//------------------------------------------------------------------------------
// EXTRA FAILURE LOCKS OUT
//------------------------------------------------------------------------------
procedure TAttemptCounterTests.ExtraFailureLocksOut;
var
  C: TOBDAttemptCounter;
  Wait: Integer;
  I: Integer;
begin
  C := TOBDAttemptCounter.Create;
  try
    C.FreeAttempts := 3;
    C.BaseLockoutSeconds := 60;
    for I := 1 to 4 do C.RegisterFailure('rad-001');
    Assert.IsFalse(C.IsAllowed('rad-001', Wait));
    Assert.IsTrue(Wait > 0, 'lockout remainder must be reported');
  finally
    C.Free;
  end;
end;

//------------------------------------------------------------------------------
// SUCCESS RESETS THE COUNTER
//------------------------------------------------------------------------------
procedure TAttemptCounterTests.SuccessResetsTheCounter;
var
  C: TOBDAttemptCounter;
  Wait: Integer;
  I: Integer;
begin
  C := TOBDAttemptCounter.Create;
  try
    C.FreeAttempts := 3;
    C.BaseLockoutSeconds := 60;
    for I := 1 to 5 do C.RegisterFailure('rad-001');
    Assert.IsFalse(C.IsAllowed('rad-001', Wait), 'should be locked out');
    C.RegisterSuccess('rad-001');
    Assert.IsTrue(C.IsAllowed('rad-001', Wait),
      'success must clear the lockout');
    Assert.AreEqual(0, C.State('rad-001').Failures);
  finally
    C.Free;
  end;
end;

//------------------------------------------------------------------------------
// RESET CLEARS STATE
//------------------------------------------------------------------------------
procedure TAttemptCounterTests.ResetClearsState;
var
  C: TOBDAttemptCounter;
  Wait: Integer;
begin
  C := TOBDAttemptCounter.Create;
  try
    C.FreeAttempts := 1;
    C.BaseLockoutSeconds := 30;
    C.RegisterFailure('rad-001');
    C.RegisterFailure('rad-001');
    Assert.IsFalse(C.IsAllowed('rad-001', Wait));
    C.Reset('rad-001');
    Assert.IsTrue(C.IsAllowed('rad-001', Wait));
  finally
    C.Free;
  end;
end;

//------------------------------------------------------------------------------
// LOCKOUT CAPS AT MAX LOCKOUT SECONDS
//------------------------------------------------------------------------------
procedure TAttemptCounterTests.LockoutCapsAtMaxLockoutSeconds;
var
  C: TOBDAttemptCounter;
  I: Integer;
  Wait: Integer;
  Cap: Integer;
begin
  C := TOBDAttemptCounter.Create;
  try
    Cap := 60;
    C.FreeAttempts := 0;
    C.BaseLockoutSeconds := 1;
    C.MaxLockoutSeconds := Cap;
    // 40 failures with base 1s and 2× growth would explode well past
    // an hour without the cap.
    for I := 1 to 40 do C.RegisterFailure('rad-001');
    C.IsAllowed('rad-001', Wait);
    Assert.IsTrue(Wait <= Cap + 1,
      Format('Wait %ds exceeded MaxLockoutSeconds %d', [Wait, Cap]));
  finally
    C.Free;
  end;
end;

//------------------------------------------------------------------------------
// DISTINCT IDENTITIES DO NOT INTERFERE
//------------------------------------------------------------------------------
procedure TAttemptCounterTests.DistinctIdentitiesDoNotInterfere;
var
  C: TOBDAttemptCounter;
  Wait: Integer;
  I: Integer;
begin
  C := TOBDAttemptCounter.Create;
  try
    C.FreeAttempts := 1;
    C.BaseLockoutSeconds := 60;
    for I := 1 to 5 do C.RegisterFailure('rad-001');
    Assert.IsFalse(C.IsAllowed('rad-001', Wait));
    Assert.IsTrue(C.IsAllowed('rad-002', Wait),
      'a second identity must not inherit the first one''s lockout');
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAttemptCounterTests);

end.
