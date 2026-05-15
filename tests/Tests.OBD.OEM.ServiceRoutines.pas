//------------------------------------------------------------------------------
//  Tests.OBD.OEM.ServiceRoutines
//
//  Coverage for the BuildRoutineControlFrame encoder. The
//  registry singleton is exercised separately via the
//  catalogue-loader fixtures.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.ServiceRoutines;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  OBD.OEM.ServiceRoutines;

type
  /// <summary>Tests for the RoutineControl frame encoder.</summary>
  [TestFixture]
  TServiceRoutinesTests = class
  public
    /// <summary>Start (SF=0x01) without option record produces
    /// <c>31 01 hi lo</c>.</summary>
    [Test] procedure Start_NoOptions;
    /// <summary>Stop (SF=0x02) produces <c>31 02 hi lo</c>.</summary>
    [Test] procedure Stop_NoOptions;
    /// <summary>RequestResults (SF=0x03) produces <c>31 03 hi
    /// lo</c>.</summary>
    [Test] procedure RequestResults_NoOptions;
    /// <summary>Option record bytes are appended after the
    /// RID.</summary>
    [Test] procedure Start_WithOptions_AppendsBytes;
    /// <summary>Invalid sub-function (0x04, 0x00) raises
    /// EOBDServiceRoutine.</summary>
    [Test] procedure InvalidSubFunction_Raises;
  end;

implementation

function MakeRoutine(ARid: Word; ASub: Byte;
  const AOptions: TBytes): TOBDServiceRoutine;
begin
  Result := Default(TOBDServiceRoutine);
  Result.Key := 'test_routine';
  Result.RoutineIdentifier := ARid;
  Result.SubFunction := ASub;
  Result.OptionRecord := AOptions;
end;

procedure TServiceRoutinesTests.Start_NoOptions;
var
  Frame: TBytes;
begin
  Frame := BuildRoutineControlFrame(
    MakeRoutine($1234, $01, nil));
  Assert.AreEqual(4, Length(Frame));
  Assert.AreEqual(Integer($31), Integer(Frame[0]));
  Assert.AreEqual(Integer($01), Integer(Frame[1]));
  Assert.AreEqual(Integer($12), Integer(Frame[2]));
  Assert.AreEqual(Integer($34), Integer(Frame[3]));
end;

procedure TServiceRoutinesTests.Stop_NoOptions;
var
  Frame: TBytes;
begin
  Frame := BuildRoutineControlFrame(
    MakeRoutine($1234, $02, nil));
  Assert.AreEqual(Integer($02), Integer(Frame[1]));
end;

procedure TServiceRoutinesTests.RequestResults_NoOptions;
var
  Frame: TBytes;
begin
  Frame := BuildRoutineControlFrame(
    MakeRoutine($1234, $03, nil));
  Assert.AreEqual(Integer($03), Integer(Frame[1]));
end;

procedure TServiceRoutinesTests.Start_WithOptions_AppendsBytes;
var
  Frame: TBytes;
begin
  Frame := BuildRoutineControlFrame(
    MakeRoutine($ABCD, $01, TBytes.Create($DE, $AD, $BE, $EF)));
  Assert.AreEqual(8, Length(Frame));
  Assert.AreEqual(Integer($AB), Integer(Frame[2]));
  Assert.AreEqual(Integer($CD), Integer(Frame[3]));
  Assert.AreEqual(Integer($DE), Integer(Frame[4]));
  Assert.AreEqual(Integer($AD), Integer(Frame[5]));
  Assert.AreEqual(Integer($BE), Integer(Frame[6]));
  Assert.AreEqual(Integer($EF), Integer(Frame[7]));
end;

procedure TServiceRoutinesTests.InvalidSubFunction_Raises;
begin
  Assert.WillRaise(
    procedure
    begin
      BuildRoutineControlFrame(MakeRoutine($1234, $04, nil));
    end,
    EOBDServiceRoutine);
  Assert.WillRaise(
    procedure
    begin
      BuildRoutineControlFrame(MakeRoutine($1234, $00, nil));
    end,
    EOBDServiceRoutine);
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceRoutinesTests);

end.
