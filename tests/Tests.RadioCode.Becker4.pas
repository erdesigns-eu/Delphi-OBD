//------------------------------------------------------------------------------
// UNIT           : Tests.RadioCode.Becker4
// CONTENTS       : Golden tests for TOBDRadioCodeBecker4
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Becker 4-digit calculator is a deterministic table lookup
//                  (serial → Database[serial]). Goldens here are taken straight
//                  from the published lookup table — drift in the algorithm or
//                  the table will be caught immediately.
//------------------------------------------------------------------------------
unit Tests.RadioCode.Becker4;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TBecker4Tests = class
  public
    [Test]
    [TestCase('Index_0',  '0000,1010')]
    [TestCase('Index_1',  '0001,1108')]
    [TestCase('Index_2',  '0002,1016')]
    [TestCase('Index_9',  '0009,1072')]
    [TestCase('Index_10', '0010,1090')]
    [TestCase('Index_19', '0019,0152')]
    procedure Calculate_ProducesExpectedCode(const Serial, Expected: string);

    /// <summary>
    ///   Calculate  is deterministic.
    /// </summary>
    [Test]
    procedure Calculate_IsDeterministic;

    [Test]
    [TestCase('TooShort',  '123')]
    [TestCase('TooLong',   '12345')]
    [TestCase('Letters',   'ABCD')]
    [TestCase('Mixed',     '12A4')]
    [TestCase('Empty',     '')]
    procedure Calculate_RejectsInvalidInput(const Serial: string);

    /// <summary>
    ///   Calculate  trims whitespace.
    /// </summary>
    [Test]
    procedure Calculate_TrimsWhitespace;
  end;

implementation

uses
  System.SysUtils,
  OBD.RadioCode.Becker4;

{ TBecker4Tests }

//------------------------------------------------------------------------------
// CALCULATE_PRODUCES EXPECTED CODE
//------------------------------------------------------------------------------
procedure TBecker4Tests.Calculate_ProducesExpectedCode(
  const Serial, Expected: string);
var
  Calc: TOBDRadioCodeBecker4;
  Output, Err: string;
begin
  Calc := TOBDRadioCodeBecker4.Create;
  try
    Assert.IsTrue(Calc.Calculate(Serial, Output, Err),
      Format('Calculate(%s) returned False with error "%s"', [Serial, Err]));
    Assert.AreEqual(Expected, Output);
  finally
    Calc.Free;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE_IS DETERMINISTIC
//------------------------------------------------------------------------------
procedure TBecker4Tests.Calculate_IsDeterministic;
var
  Calc: TOBDRadioCodeBecker4;
  Out1, Out2, Err: string;
begin
  // Same input must produce the same output across repeated calls — protects
  // against accidental statefulness in the calculator.
  Calc := TOBDRadioCodeBecker4.Create;
  try
    Calc.Calculate('1234', Out1, Err);
    Calc.Calculate('1234', Out2, Err);
    Assert.AreEqual(Out1, Out2);
    Assert.IsNotEmpty(Out1);
  finally
    Calc.Free;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE_REJECTS INVALID INPUT
//------------------------------------------------------------------------------
procedure TBecker4Tests.Calculate_RejectsInvalidInput(const Serial: string);
var
  Calc: TOBDRadioCodeBecker4;
  Output, Err: string;
begin
  Calc := TOBDRadioCodeBecker4.Create;
  try
    Assert.IsFalse(Calc.Calculate(Serial, Output, Err),
      Format('Calculate accepted invalid input "%s"', [Serial]));
    Assert.IsNotEmpty(Err);
  finally
    Calc.Free;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE_TRIMS WHITESPACE
//------------------------------------------------------------------------------
procedure TBecker4Tests.Calculate_TrimsWhitespace;
var
  Calc: TOBDRadioCodeBecker4;
  Output, Err: string;
begin
  // SanitizeInput on the base class is documented to strip whitespace.
  Calc := TOBDRadioCodeBecker4.Create;
  try
    Assert.IsTrue(Calc.Calculate(' 0000 ', Output, Err), Err);
    Assert.AreEqual('1010', Output);
  finally
    Calc.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TBecker4Tests);

end.
