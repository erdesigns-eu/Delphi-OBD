//------------------------------------------------------------------------------
//  Tests.OBD.RadioCode.Asian
//
//  Pinned-vector test for the Hyundai algorithm (the only Asian
//  vendor that ships a real bundled algorithm).
//------------------------------------------------------------------------------

unit Tests.OBD.RadioCode.Asian;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  OBD.RadioCode.Types,
  OBD.RadioCode,
  OBD.RadioCode.Asian;

type
  [TestFixture]
  THyundaiTests = class
  public
    [Test] procedure StandardOffsetIsAddPlus1212;
    [Test] procedure NineThousandPlusWrapsModuloTenThousand;
    [Test] procedure ModelHint2009UsesPlus1222;
    [Test] procedure RejectsNonDigit;
  end;

implementation

procedure THyundaiTests.StandardOffsetIsAddPlus1212;
var C: TOBDRadioCodeHyundai; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeHyundai.Create(nil);
  try
    R := C.Calculate('1234');
    // (1234 + 1212) mod 10000 = 2446
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('2446', R.Code);
  finally
    C.Free;
  end;
end;

procedure THyundaiTests.NineThousandPlusWrapsModuloTenThousand;
var C: TOBDRadioCodeHyundai; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeHyundai.Create(nil);
  try
    R := C.Calculate('9000');
    // (9000 + 1212) mod 10000 = 10212 mod 10000 = 212
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('0212', R.Code);
  finally
    C.Free;
  end;
end;

procedure THyundaiTests.ModelHint2009UsesPlus1222;
var C: TOBDRadioCodeHyundai; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeHyundai.Create(nil);
  try
    C.ModelHint := '2009-variant';
    R := C.Calculate('1234');
    // (1234 + 1222) mod 10000 = 2456
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('2456', R.Code);
    Assert.IsTrue(R.Variant.Contains('2009'));
  finally
    C.Free;
  end;
end;

procedure THyundaiTests.RejectsNonDigit;
var C: TOBDRadioCodeHyundai; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeHyundai.Create(nil);
  try
    R := C.Calculate('12A4');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(THyundaiTests);

end.
