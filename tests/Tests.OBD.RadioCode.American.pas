//------------------------------------------------------------------------------
//  Tests.OBD.RadioCode.American
//
//  Pinned-vector test for the Ford M-series algorithm. The
//  expected output was computed by hand from the JS reference
//  (OlegSmelov/ford-radio-codes/app.js) for M000001.
//------------------------------------------------------------------------------

unit Tests.OBD.RadioCode.American;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  OBD.RadioCode.Types,
  OBD.RadioCode,
  OBD.RadioCode.American;

type
  [TestFixture]
  TFordMTests = class
  public
    [Test] procedure SerialOneIsHandComputed9617;
    [Test] procedure LeadingMIsStripped;
    [Test] procedure RejectsNonDigitTail;
  end;

implementation

procedure TFordMTests.SerialOneIsHandComputed9617;
var
  C: TOBDRadioCodeFordM;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeFordM.Create(nil);
  try
    R := C.Calculate('000001');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('9617', R.Code,
      'M000001 should hand-compute to 9617 per the JS reference');
  finally
    C.Free;
  end;
end;

procedure TFordMTests.LeadingMIsStripped;
var
  C: TOBDRadioCodeFordM;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeFordM.Create(nil);
  try
    R := C.Calculate('M000001');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('9617', R.Code);
  finally
    C.Free;
  end;
end;

procedure TFordMTests.RejectsNonDigitTail;
var
  C: TOBDRadioCodeFordM;
  R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeFordM.Create(nil);
  try
    R := C.Calculate('M00000A');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFordMTests);

end.
