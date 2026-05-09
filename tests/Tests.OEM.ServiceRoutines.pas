//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.ServiceRoutines.pas
// CONTENTS       : Tests for OBD.OEM.ServiceRoutines
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.ServiceRoutines;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TServiceRoutinesTests = class
  public
    /// <summary>
    ///   Registry has at least thirty.
    /// </summary>
    [Test] procedure RegistryHasAtLeastThirty;
    /// <summary>
    ///   Every entry has citation.
    /// </summary>
    [Test] procedure EveryEntryHasCitation;
    /// <summary>
    ///   Every entry has non empty key and name.
    /// </summary>
    [Test] procedure EveryEntryHasNonEmptyKeyAndName;
    /// <summary>
    ///   R i ds are non zero.
    /// </summary>
    [Test] procedure RIDsAreNonZero;
    /// <summary>
    ///   Sub function is valid u d s.
    /// </summary>
    [Test] procedure SubFunctionIsValidUDS;
    /// <summary>
    ///   Find is case insensitive.
    /// </summary>
    [Test] procedure FindIsCaseInsensitive;
    /// <summary>
    ///   Get by category returns maintenance.
    /// </summary>
    [Test] procedure GetByCategoryReturnsMaintenance;
    /// <summary>
    ///   Get by o e m returns b m w routines.
    /// </summary>
    [Test] procedure GetByOEMReturnsBMWRoutines;
    /// <summary>
    ///   Frame builder produces correct layout.
    /// </summary>
    [Test] procedure FrameBuilderProducesCorrectLayout;
    /// <summary>
    ///   Frame builder rejects bad sub function.
    /// </summary>
    [Test] procedure FrameBuilderRejectsBadSubFunction;
    /// <summary>
    ///   No duplicate keys.
    /// </summary>
    [Test] procedure NoDuplicateKeys;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.ServiceRoutines;

//------------------------------------------------------------------------------
// REGISTRY HAS AT LEAST THIRTY
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.RegistryHasAtLeastThirty;
begin
  Assert.IsTrue(TOBDServiceRoutineRegistry.Instance.Count >= 25,
    'Should have at least 25 routines, got ' +
    IntToStr(TOBDServiceRoutineRegistry.Instance.Count));
end;

//------------------------------------------------------------------------------
// EVERY ENTRY HAS CITATION
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.EveryEntryHasCitation;
var
  I: Integer;
  R: TOBDServiceRoutine;
begin
  for I := 0 to TOBDServiceRoutineRegistry.Instance.Count - 1 do
  begin
    R := TOBDServiceRoutineRegistry.Instance.Get(I);
    Assert.IsNotEmpty(R.Citation,
      'Routine ' + R.Key + ' missing citation');
  end;
end;

//------------------------------------------------------------------------------
// EVERY ENTRY HAS NON EMPTY KEY AND NAME
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.EveryEntryHasNonEmptyKeyAndName;
var
  I: Integer;
  R: TOBDServiceRoutine;
begin
  for I := 0 to TOBDServiceRoutineRegistry.Instance.Count - 1 do
  begin
    R := TOBDServiceRoutineRegistry.Instance.Get(I);
    Assert.IsNotEmpty(R.Key);
    Assert.IsNotEmpty(R.DisplayName);
  end;
end;

//------------------------------------------------------------------------------
// RIDS ARE NON ZERO
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.RIDsAreNonZero;
var
  I: Integer;
  R: TOBDServiceRoutine;
begin
  for I := 0 to TOBDServiceRoutineRegistry.Instance.Count - 1 do
  begin
    R := TOBDServiceRoutineRegistry.Instance.Get(I);
    Assert.IsTrue(R.RoutineIdentifier <> 0,
      R.Key + ' has zero RID');
  end;
end;

//------------------------------------------------------------------------------
// SUB FUNCTION IS VALID UDS
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.SubFunctionIsValidUDS;
var
  I: Integer;
  R: TOBDServiceRoutine;
begin
  for I := 0 to TOBDServiceRoutineRegistry.Instance.Count - 1 do
  begin
    R := TOBDServiceRoutineRegistry.Instance.Get(I);
    Assert.IsTrue(R.SubFunction in [$01, $02, $03],
      R.Key + ' has invalid sub-function');
  end;
end;

//------------------------------------------------------------------------------
// FIND IS CASE INSENSITIVE
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.FindIsCaseInsensitive;
var
  R: TOBDServiceRoutine;
begin
  Assert.IsTrue(TOBDServiceRoutineRegistry.Instance.Find('OIL_RESET_BMW', R));
  Assert.IsTrue(TOBDServiceRoutineRegistry.Instance.Find('Oil_Reset_BMW', R));
  Assert.IsTrue(TOBDServiceRoutineRegistry.Instance.Find('oil_reset_bmw', R));
  Assert.AreEqual('Oil Service Reset (BMW CBS)', R.DisplayName);
end;

//------------------------------------------------------------------------------
// GET BY CATEGORY RETURNS MAINTENANCE
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.GetByCategoryReturnsMaintenance;
var
  Routines: TArray<TOBDServiceRoutine>;
begin
  TOBDServiceRoutineRegistry.Instance.GetByCategory(srcMaintenance, Routines);
  Assert.IsTrue(Length(Routines) >= 5,
    'Maintenance category should have several entries');
end;

//------------------------------------------------------------------------------
// GET BY OEMRETURNS BMWROUTINES
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.GetByOEMReturnsBMWRoutines;
var
  Routines: TArray<TOBDServiceRoutine>;
  R: TOBDServiceRoutine;
  Found: Boolean;
begin
  TOBDServiceRoutineRegistry.Instance.GetByOEM('bmw', Routines);
  Assert.IsTrue(Length(Routines) >= 3, 'Expected several BMW routines');
  Found := False;
  for R in Routines do
    if R.Key = 'oil_reset_bmw' then Found := True;
  Assert.IsTrue(Found, 'BMW lookup should include oil_reset_bmw');
end;

//------------------------------------------------------------------------------
// FRAME BUILDER PRODUCES CORRECT LAYOUT
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.FrameBuilderProducesCorrectLayout;
var
  R: TOBDServiceRoutine;
  Frame: TBytes;
begin
  Assert.IsTrue(TOBDServiceRoutineRegistry.Instance.Find('sas_zero', R));
  Frame := BuildRoutineControlFrame(R);
  Assert.AreEqual($31, Integer(Frame[0]));
  Assert.AreEqual($01, Integer(Frame[1]));
  Assert.AreEqual($03, Integer(Frame[2])); // RID hi (0x0301)
  Assert.AreEqual($01, Integer(Frame[3])); // RID lo
  Assert.AreEqual(4, Length(Frame), 'No OptionRecord -> 4 bytes total');
end;

//------------------------------------------------------------------------------
// FRAME BUILDER REJECTS BAD SUB FUNCTION
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.FrameBuilderRejectsBadSubFunction;
var
  R: TOBDServiceRoutine;
begin
  R := Default(TOBDServiceRoutine);
  R.RoutineIdentifier := $0301;
  R.SubFunction := $99;
  Assert.WillRaise(
    procedure begin BuildRoutineControlFrame(R); end,
    EOBDServiceRoutine);
end;

//------------------------------------------------------------------------------
// NO DUPLICATE KEYS
//------------------------------------------------------------------------------
procedure TServiceRoutinesTests.NoDuplicateKeys;
var
  Seen: TArray<string>;
  I, J: Integer;
  R: TOBDServiceRoutine;
begin
  SetLength(Seen, TOBDServiceRoutineRegistry.Instance.Count);
  for I := 0 to TOBDServiceRoutineRegistry.Instance.Count - 1 do
  begin
    R := TOBDServiceRoutineRegistry.Instance.Get(I);
    for J := 0 to I - 1 do
      Assert.AreNotEqual(Seen[J], R.Key, 'Duplicate key: ' + R.Key);
    Seen[I] := R.Key;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceRoutinesTests);

end.
