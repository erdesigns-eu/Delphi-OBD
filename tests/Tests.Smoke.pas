//------------------------------------------------------------------------------
// UNIT           : Tests.Smoke
// CONTENTS       : Trivial smoke test — verifies the DUnitX harness builds,
//                  registers fixtures, and runs to completion.
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Delete this fixture once real test fixtures exist; until
//                  then it acts as the "is the test rig alive?" canary.
//------------------------------------------------------------------------------
unit Tests.Smoke;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSmokeTests = class
  public
    [Test]
    procedure HarnessIsAlive;
    [Test]
    [TestCase('OneAndOne',  '1,1,2')]
    [TestCase('TwoAndTwo',  '2,2,4')]
    [TestCase('SignBalance','-3,3,0')]
    procedure IntegerAdditionWorks(const A, B, Expected: Integer);
  end;

implementation

{ TSmokeTests }

//------------------------------------------------------------------------------
// HARNESS IS ALIVE
//------------------------------------------------------------------------------
procedure TSmokeTests.HarnessIsAlive;
begin
  Assert.Pass;
end;

//------------------------------------------------------------------------------
// INTEGER ADDITION WORKS
//------------------------------------------------------------------------------
procedure TSmokeTests.IntegerAdditionWorks(const A, B, Expected: Integer);
begin
  Assert.AreEqual(Expected, A + B);
end;

initialization
  TDUnitX.RegisterTestFixture(TSmokeTests);

end.
