//------------------------------------------------------------------------------
// UNIT           : Tests.RadioCode.Registry.pas
// CONTENTS       : Tests for OBD.RadioCode.Registry
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.RadioCode.Registry;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TRadioCodeRegistryTests = class
  public
    [Test] procedure RegistryHasAllEightPendingBrands;
    [Test] procedure FindIsCaseInsensitive;
    [Test] procedure UnknownBrandReturnsNil;
    [Test] procedure EachPendingBrandHasFalseDataAvailable;
    [Test] procedure PendingCalculatorRaisesOnCalculate;
    [Test] procedure PendingCalculatorRejectsValidate;
    [Test] procedure PendingCalculatorDescriptionIsNotEmpty;
    [Test] procedure RegisterDoesNotDuplicateOnSameKey;
    [Test] procedure DataNotesIsNotEmptyForPending;
  end;

implementation

uses
  System.SysUtils, System.Classes,
  OBD.RadioCode, OBD.RadioCode.Registry, OBD.RadioCode.Pending;

const
  ExpectedKeys: array[0..7] of string = (
    'pioneer', 'kenwood', 'jvc', 'sony',
    'philips', 'grundig', 'panasonic', 'continental_vdo'
  );

procedure TRadioCodeRegistryTests.RegistryHasAllEightPendingBrands;
var
  Key: string;
begin
  Assert.IsTrue(TOBDRadioCodeRegistry.Instance.Count >= Length(ExpectedKeys),
    'Registry should hold at least the eight pending brands');
  for Key in ExpectedKeys do
    Assert.IsNotNull(TOBDRadioCodeRegistry.Instance.Find(Key),
      'Expected brand not registered: ' + Key);
end;

procedure TRadioCodeRegistryTests.FindIsCaseInsensitive;
begin
  Assert.IsNotNull(TOBDRadioCodeRegistry.Instance.Find('PIONEER'));
  Assert.IsNotNull(TOBDRadioCodeRegistry.Instance.Find('Pioneer'));
  Assert.IsNotNull(TOBDRadioCodeRegistry.Instance.Find('pioneer'));
end;

procedure TRadioCodeRegistryTests.UnknownBrandReturnsNil;
begin
  Assert.IsNull(TOBDRadioCodeRegistry.Instance.Find('does_not_exist'));
end;

procedure TRadioCodeRegistryTests.EachPendingBrandHasFalseDataAvailable;
var
  Key: string;
begin
  for Key in ExpectedKeys do
    Assert.IsFalse(TOBDRadioCodeRegistry.Instance.Find(Key).DataAvailable,
      'DataAvailable should be False for ' + Key);
end;

procedure TRadioCodeRegistryTests.PendingCalculatorRaisesOnCalculate;
var
  Calc: IOBDRadioCode;
  Output, Err: string;
begin
  Calc := TOBDRadioCodeRegistry.Instance.Find('pioneer').CreateCalculator;
  Output := '';
  Err := '';
  Assert.WillRaise(
    procedure begin Calc.Calculate('1234567', Output, Err); end,
    EOBDRadioCodeDataMissing,
    'Pending calculator should raise EOBDRadioCodeDataMissing');
end;

procedure TRadioCodeRegistryTests.PendingCalculatorRejectsValidate;
var
  Calc: IOBDRadioCode;
  Err: string;
begin
  Calc := TOBDRadioCodeRegistry.Instance.Find('philips').CreateCalculator;
  Err := '';
  Assert.IsFalse(Calc.Validate('1234567890', Err),
    'Pending calculator must not claim validation success');
  Assert.IsNotEmpty(Err,
    'Validate failure must produce a human-readable message');
end;

procedure TRadioCodeRegistryTests.PendingCalculatorDescriptionIsNotEmpty;
var
  Calc: IOBDRadioCode;
begin
  Calc := TOBDRadioCodeRegistry.Instance.Find('sony').CreateCalculator;
  Assert.IsNotEmpty(Calc.GetDescription);
end;

procedure TRadioCodeRegistryTests.RegisterDoesNotDuplicateOnSameKey;
var
  CountBefore: Integer;
  Brand: TOBDRadioCodeBrand;
begin
  CountBefore := TOBDRadioCodeRegistry.Instance.Count;
  Brand := TOBDRadioCodeBrand.Create('pioneer', 'Pioneer (dup)', False, 'dup',
    function: IOBDRadioCode begin Result := nil; end);
  TOBDRadioCodeRegistry.Instance.Register(Brand);
  // Brand instance is freed by the registry on duplicate; count stays the same.
  Assert.AreEqual(CountBefore, TOBDRadioCodeRegistry.Instance.Count);
end;

procedure TRadioCodeRegistryTests.DataNotesIsNotEmptyForPending;
var
  Key: string;
begin
  for Key in ExpectedKeys do
    Assert.IsNotEmpty(TOBDRadioCodeRegistry.Instance.Find(Key).DataNotes,
      'DataNotes should explain the gap for ' + Key);
end;

initialization
  TDUnitX.RegisterTestFixture(TRadioCodeRegistryTests);

end.
