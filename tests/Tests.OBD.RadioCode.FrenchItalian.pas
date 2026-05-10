//------------------------------------------------------------------------------
//  Tests.OBD.RadioCode.FrenchItalian
//
//  Pinned-vector tests for the French / Italian radio-code
//  algorithms that ship a real implementation (Peugeot, Renault,
//  Fiat-Daiichi, Fiat-VP). Vectors are computed by hand from the
//  algorithm spec and fixed here so a future refactor can't
//  silently regress.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.RadioCode.FrenchItalian;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.RadioCode.Types,
  OBD.RadioCode,
  OBD.RadioCode.FrenchItalian;

type
  [TestFixture]
  TPeugeotTests = class
  public
    [Test] procedure SuccessOnHandComputed1234;
    [Test] procedure RejectsNonDigit;
    [Test] procedure RejectsWrongLength;
  end;

  [TestFixture]
  TRenaultTests = class
  public
    [Test] procedure SuccessOnHandComputedC123;
    [Test] procedure RejectsA0Pair;
    [Test] procedure RejectsLeadingDigit;
  end;

  [TestFixture]
  TFiatDaiichiTests = class
  public
    [Test] procedure SuccessOnHandComputed1234;
    [Test] procedure SuccessOnAllZeroes;
  end;

  [TestFixture]
  TFiatVPTests = class
  public
    [Test] procedure SuccessOnHandComputed1234;
    [Test] procedure HighDigitClampsToZero;
  end;

implementation

{ ---- Peugeot --------------------------------------------------------------- }

procedure TPeugeotTests.SuccessOnHandComputed1234;
var
  C: TOBDRadioCodePeugeot;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodePeugeot.Create(nil);
  try
    R := C.Calculate('1234');
    // Hand trace:
    //   Out[0] = (1+1) mod 10 = 2
    //   Out[1] = (2+2) mod 10 = 4
    //   Out[2] = (3+3) mod 10 = 6
    //   Out[3] = (4+4) mod 10 = 8 -> 8>6 -> 2
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('2462', R.Code);
  finally
    C.Free;
  end;
end;

procedure TPeugeotTests.RejectsNonDigit;
var
  C: TOBDRadioCodePeugeot;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodePeugeot.Create(nil);
  try
    R := C.Calculate('12A4');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TPeugeotTests.RejectsWrongLength;
var
  C: TOBDRadioCodePeugeot;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodePeugeot.Create(nil);
  try
    R := C.Calculate('123');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

{ ---- Renault --------------------------------------------------------------- }

procedure TRenaultTests.SuccessOnHandComputedC123;
var
  C: TOBDRadioCodeRenault;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeRenault.Create(nil);
  try
    R := C.Calculate('C123');
    // Hand trace:
    //   X = ord('C')*10 + ord('1') - 698 = 670+49-698 = 21
    //   Y = ord('2')*10 + ord('3') + X - 528 = 500+51+21-528 = 44
    //   Z = (44*7) mod 100 = 308 mod 100 = 8
    //   C = (8 div 10) + (8 mod 10)*10 + ((259 mod 21) mod 100)*100
    //     = 0 + 80 + (4)*100 = 480
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('0480', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRenaultTests.RejectsA0Pair;
var
  C: TOBDRadioCodeRenault;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeRenault.Create(nil);
  try
    R := C.Calculate('A012');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TRenaultTests.RejectsLeadingDigit;
var
  C: TOBDRadioCodeRenault;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeRenault.Create(nil);
  try
    R := C.Calculate('1234');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

{ ---- Fiat Daiichi --------------------------------------------------------- }

procedure TFiatDaiichiTests.SuccessOnHandComputed1234;
var
  C: TOBDRadioCodeFiatDaiichi;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeFiatDaiichi.Create(nil);
  try
    R := C.Calculate('1234');
    // Hand trace:
    //   Out[3] = 10 - 1 = 9
    //   Out[2] =  9 - 2 = 7
    //   Out[1] =  9 - 3 = 6
    //   Out[0] =  9 - 4 = 5
    //   -> "5679"
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('5679', R.Code);
  finally
    C.Free;
  end;
end;

procedure TFiatDaiichiTests.SuccessOnAllZeroes;
var
  C: TOBDRadioCodeFiatDaiichi;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeFiatDaiichi.Create(nil);
  try
    R := C.Calculate('0000');
    Assert.IsTrue(R.Success);
    Assert.AreEqual('9999', R.Code);
  finally
    C.Free;
  end;
end;

{ ---- Fiat VP -------------------------------------------------------------- }

procedure TFiatVPTests.SuccessOnHandComputed1234;
var
  C: TOBDRadioCodeFiatVP;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeFiatVP.Create(nil);
  try
    R := C.Calculate('1234');
    // Hand trace:
    //   SN = [1,2,3,4]; OutputCode starts at 1111.
    //   + Third(4=4)*10  = +40   -> 1151
    //   + First(3=3)*1000 = +3000 -> 4151
    //   + Fourth(2=2)    = +2    -> 4153
    //   + Second(1=1)*100 = +100  -> 4253
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('4253', R.Code);
  finally
    C.Free;
  end;
end;

procedure TFiatVPTests.HighDigitClampsToZero;
var
  C: TOBDRadioCodeFiatVP;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeFiatVP.Create(nil);
  try
    // Serial 8888: Third(8)=0, First(8)=0, Fourth(8)=0, Second(8)=0
    // OutputCode = 1111 + 0 + 0 + 0 + 0 = 1111
    R := C.Calculate('8888');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('1111', R.Code);
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPeugeotTests);
  TDUnitX.RegisterTestFixture(TRenaultTests);
  TDUnitX.RegisterTestFixture(TFiatDaiichiTests);
  TDUnitX.RegisterTestFixture(TFiatVPTests);

end.
