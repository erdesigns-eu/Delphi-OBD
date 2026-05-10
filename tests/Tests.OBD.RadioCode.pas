//------------------------------------------------------------------------------
//  Tests.OBD.RadioCode
//
//  DUnitX coverage for the radio-code foundation:
//  TOBDRadioCode base validators, Calculate flow, registry
//  round-trip. A tiny in-test calculator
//  (TFakeRadioCodeCalculator) exercises the contract without
//  pulling in a real vendor — vendor coverage lives in the
//  per-region test units.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.RadioCode;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  /// <summary>Throw-away calculator used by the tests in this
  /// unit. Brand 'fake'. Validates a 4-digit serial; the
  /// calculation echoes the input reversed.</summary>
  TFakeRadioCodeCalculator = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  [TestFixture]
  TRadioCodeBaseTests = class
  public
    [Test] procedure CalculateRejectsEmptyInputViaDefaultValidator;
    [Test] procedure CalculateUsesOverriddenValidator;
    [Test] procedure CalculateFillsBrandKeyAutomatically;
    [Test] procedure SanitizeUppersAndTrims;
  end;

  [TestFixture]
  TRadioCodeRegistryTests = class
  public
    [Setup]    procedure Setup;
    [Test]     procedure RegisterClassThenResolve;
    [Test]     procedure HasBrandReturnsTrueOnlyAfterRegister;
    [Test]     procedure AllReturnsSortedByDisplayName;
  end;

implementation

{ ---- TFakeRadioCodeCalculator ---------------------------------------------- }

function TFakeRadioCodeCalculator.BrandKey: string;
begin
  Result := 'fake';
end;

function TFakeRadioCodeCalculator.DisplayName: string;
begin
  Result := 'Fake Radio (test fixture)';
end;

function TFakeRadioCodeCalculator.Description: string;
begin
  Result :=
    'Test-only calculator. Input: 4 decimal digits. Output: ' +
    'the input reversed.';
end;

function TFakeRadioCodeCalculator.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
    and ValidateAllDigits(AInput, AReason);
end;

function TFakeRadioCodeCalculator.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  I: Integer;
  Reversed: string;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'fake / v1';
  SetLength(Reversed, Length(AInput));
  for I := 1 to Length(AInput) do
    Reversed[I] := AInput[Length(AInput) - I + 1];
  Result.Code := Reversed;
  Result.Success := True;
end;

/// Second fake to test sort order in the registry.
type
  TFakeRadioCodeAlphaCalculator = class(TFakeRadioCodeCalculator)
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
  end;

function TFakeRadioCodeAlphaCalculator.BrandKey: string;
begin
  Result := 'fake-alpha';
end;

function TFakeRadioCodeAlphaCalculator.DisplayName: string;
begin
  Result := 'Aaa Fake Radio (sorts first)';
end;

{ ---- TRadioCodeBaseTests --------------------------------------------------- }

procedure TRadioCodeBaseTests.CalculateRejectsEmptyInputViaDefaultValidator;
var
  Calc: TOBDRadioCode;
  R: TOBDRadioCodeResult;
begin
  Calc := TFakeRadioCodeCalculator.Create(nil);
  try
    R := Calc.Calculate('');
    Assert.IsFalse(R.Success);
    Assert.IsNotEmpty(R.Message);
  finally
    Calc.Free;
  end;
end;

procedure TRadioCodeBaseTests.CalculateUsesOverriddenValidator;
var
  Calc: TFakeRadioCodeCalculator;
  R: TOBDRadioCodeResult;
begin
  Calc := TFakeRadioCodeCalculator.Create(nil);
  try
    R := Calc.Calculate('ABCD');         // not all digits -> reject
    Assert.IsFalse(R.Success);

    R := Calc.Calculate('12345');        // wrong length -> reject
    Assert.IsFalse(R.Success);

    R := Calc.Calculate('1234');         // OK -> reverse
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('4321', R.Code);
  finally
    Calc.Free;
  end;
end;

procedure TRadioCodeBaseTests.CalculateFillsBrandKeyAutomatically;
var
  Calc: TFakeRadioCodeCalculator;
  R: TOBDRadioCodeResult;
begin
  Calc := TFakeRadioCodeCalculator.Create(nil);
  try
    R := Calc.Calculate('1234');
    Assert.AreEqual('fake', R.BrandKey);
  finally
    Calc.Free;
  end;
end;

procedure TRadioCodeBaseTests.SanitizeUppersAndTrims;
var
  Calc: TFakeRadioCodeCalculator;
  R: TOBDRadioCodeResult;
begin
  Calc := TFakeRadioCodeCalculator.Create(nil);
  try
    // Trim then validate-then-calculate path: '  1234  ' must
    // pass through SanitizeInput first.
    R := Calc.Calculate('  1234  ');
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('4321', R.Code);
  finally
    Calc.Free;
  end;
end;

{ ---- TRadioCodeRegistryTests ----------------------------------------------- }

procedure TRadioCodeRegistryTests.Setup;
begin
  // Registry is process-wide; ensure a clean slate by registering
  // the test fixtures explicitly.
  TOBDRadioCodeRegistry.Default.RegisterClass(TFakeRadioCodeCalculator);
  TOBDRadioCodeRegistry.Default.RegisterClass(TFakeRadioCodeAlphaCalculator);
end;

procedure TRadioCodeRegistryTests.RegisterClassThenResolve;
var
  C: TOBDRadioCodeClass;
begin
  Assert.IsTrue(TOBDRadioCodeRegistry.Default.TryResolve('fake', C));
  Assert.AreEqual('TFakeRadioCodeCalculator', C.ClassName);
end;

procedure TRadioCodeRegistryTests.HasBrandReturnsTrueOnlyAfterRegister;
begin
  Assert.IsTrue(TOBDRadioCodeRegistry.Default.HasBrand('fake'));
  Assert.IsFalse(TOBDRadioCodeRegistry.Default.HasBrand('not-registered'));
end;

procedure TRadioCodeRegistryTests.AllReturnsSortedByDisplayName;
var
  L: TArray<TOBDRadioCodeClass>;
  Idx, AlphaIdx, FakeIdx: Integer;
  Probe: TOBDRadioCode;
begin
  L := TOBDRadioCodeRegistry.Default.All;
  AlphaIdx := -1;
  FakeIdx := -1;
  for Idx := 0 to High(L) do
  begin
    Probe := L[Idx].Create(nil);
    try
      if Probe.BrandKey = 'fake-alpha' then AlphaIdx := Idx;
      if Probe.BrandKey = 'fake'       then FakeIdx  := Idx;
    finally
      Probe.Free;
    end;
  end;
  Assert.IsTrue(AlphaIdx >= 0, 'fake-alpha must be in the list');
  Assert.IsTrue(FakeIdx  >= 0, 'fake must be in the list');
  Assert.IsTrue(AlphaIdx < FakeIdx,
    '"Aaa Fake Radio" sorts before "Fake Radio"');
end;

initialization
  TDUnitX.RegisterTestFixture(TRadioCodeBaseTests);
  TDUnitX.RegisterTestFixture(TRadioCodeRegistryTests);

end.
