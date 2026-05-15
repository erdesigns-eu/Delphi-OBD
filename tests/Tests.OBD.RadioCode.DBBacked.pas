//------------------------------------------------------------------------------
//  Tests.OBD.RadioCode.DBBacked
//
//  Pinned-vector tests for the radio-code components whose
//  algorithm is a flat lookup against a shipped JSON database:
//    TOBDRadioCodeBecker4   (10,000 entries)
//    TOBDRadioCodeBecker5   (10,000 entries)
//    TOBDRadioCodeFordV     (999,999 entries)
//
//  The vectors below are pinned against the canonical JSON
//  shipped under catalogs/radio-code/. They run only when the
//  catalogues are reachable from the test executable's working
//  directory; if not, the test reports Pass with a skip message
//  so a missing-catalogue dev environment doesn't block the
//  build.
//------------------------------------------------------------------------------

unit Tests.OBD.RadioCode.DBBacked;

interface

uses
  System.SysUtils, System.IOUtils,
  DUnitX.TestFramework,
  OBD.RadioCode.Types,
  OBD.RadioCode,
  OBD.RadioCode.Aftermarket,
  OBD.RadioCode.FordV;

type
  [TestFixture]
  TRadioCodeDBBackedTests = class
  strict private
    function CatalogPresent(const AFileName: string): Boolean;
  public
    [Test] procedure Becker4_Index0Yields1010;
    [Test] procedure Becker4_Index9999Yields7532;
    [Test] procedure Becker4_RejectsThreeDigitInput;
    [Test] procedure Becker5_Index0Yields12111;
    [Test] procedure Becker5_Index1234Yields12434;
    [Test] procedure FordV_Serial1Yields5954;
    [Test] procedure FordV_Serial999999Yields4174;
    [Test] procedure FordV_AcceptsLeadingV;
    [Test] procedure FordV_RejectsOutOfRange;
  end;

implementation

function TRadioCodeDBBackedTests.CatalogPresent(const AFileName: string): Boolean;
begin
  Result := TFile.Exists(TPath.Combine(
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'catalogs'),
    'radio-code/' + AFileName));
end;

{ ---- Becker4 -------------------------------------------------------------- }

procedure TRadioCodeDBBackedTests.Becker4_Index0Yields1010;
var C: TOBDRadioCodeBecker4; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('becker4.json') then
  begin
    Assert.Pass('becker4.json not next to test exe; skipping vector');
    Exit;
  end;
  C := TOBDRadioCodeBecker4.Create(nil);
  try
    R := C.Calculate('0000');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('1010', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeDBBackedTests.Becker4_Index9999Yields7532;
var C: TOBDRadioCodeBecker4; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('becker4.json') then
  begin
    Assert.Pass('becker4.json not present; skipping');
    Exit;
  end;
  C := TOBDRadioCodeBecker4.Create(nil);
  try
    R := C.Calculate('9999');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('7532', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeDBBackedTests.Becker4_RejectsThreeDigitInput;
var C: TOBDRadioCodeBecker4; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeBecker4.Create(nil);
  try
    R := C.Calculate('999');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

{ ---- Becker5 -------------------------------------------------------------- }

procedure TRadioCodeDBBackedTests.Becker5_Index0Yields12111;
var C: TOBDRadioCodeBecker5; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('becker5.json') then
  begin
    Assert.Pass('becker5.json not present; skipping');
    Exit;
  end;
  C := TOBDRadioCodeBecker5.Create(nil);
  try
    R := C.Calculate('0000');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('12111', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeDBBackedTests.Becker5_Index1234Yields12434;
var C: TOBDRadioCodeBecker5; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('becker5.json') then
  begin
    Assert.Pass('becker5.json not present; skipping');
    Exit;
  end;
  C := TOBDRadioCodeBecker5.Create(nil);
  try
    R := C.Calculate('1234');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('12434', R.Code);
  finally
    C.Free;
  end;
end;

{ ---- FordV ---------------------------------------------------------------- }

procedure TRadioCodeDBBackedTests.FordV_Serial1Yields5954;
var C: TOBDRadioCodeFordV; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('ford-v.json') then
  begin
    Assert.Pass('ford-v.json not present; skipping');
    Exit;
  end;
  C := TOBDRadioCodeFordV.Create(nil);
  try
    R := C.Calculate('1');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('5954', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeDBBackedTests.FordV_Serial999999Yields4174;
var C: TOBDRadioCodeFordV; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('ford-v.json') then
  begin
    Assert.Pass('ford-v.json not present; skipping');
    Exit;
  end;
  C := TOBDRadioCodeFordV.Create(nil);
  try
    R := C.Calculate('999999');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('4174', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeDBBackedTests.FordV_AcceptsLeadingV;
var C: TOBDRadioCodeFordV; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('ford-v.json') then
  begin
    Assert.Pass('ford-v.json not present; skipping');
    Exit;
  end;
  C := TOBDRadioCodeFordV.Create(nil);
  try
    R := C.Calculate('V1');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('5954', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeDBBackedTests.FordV_RejectsOutOfRange;
var C: TOBDRadioCodeFordV; R: TOBDRadioCodeResult;
begin
  if not CatalogPresent('ford-v.json') then
  begin
    Assert.Pass('ford-v.json not present; skipping');
    Exit;
  end;
  C := TOBDRadioCodeFordV.Create(nil);
  try
    R := C.Calculate('1000000');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRadioCodeDBBackedTests);

end.
