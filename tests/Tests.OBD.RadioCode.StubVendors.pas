//------------------------------------------------------------------------------
//  Tests.OBD.RadioCode.StubVendors
//
//  Smoke coverage for the radio-code components that ship as
//  validate-only stubs (no bundled algorithm; the host wires
//  OnCalculate). Covers a representative member from each
//  region plus a sweep that creates / frees every stub class
//  to catch regressions in the registration / validator chain.
//
//  Specifically:
//    European premium  - VW, BMW, Mercedes, Audi Concert, Mini,
//                        Porsche, SEAT, Skoda, Smart
//    British           - Jaguar, Land Rover, Saab, Opel
//    Volvo             - Volvo HU
//    Aftermarket stubs - Alpine, Blaupunkt, Clarion
//------------------------------------------------------------------------------

unit Tests.OBD.RadioCode.StubVendors;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  OBD.RadioCode.Types,
  OBD.RadioCode,
  OBD.RadioCode.EuropeanPremium,
  OBD.RadioCode.British,
  OBD.RadioCode.Volvo,
  OBD.RadioCode.Aftermarket;

type
  [TestFixture]
  TRadioCodeStubVendorsTests = class
  strict private
    FOnCalculateFired: Boolean;
    procedure HandleOnCalculate(Sender: TObject;
      const AInput: string; const AContext: TOBDRadioCodeContext;
      var AResult: TOBDRadioCodeResult);
  public
    [Setup] procedure Setup;

    // European premium - representative stubs
    [Test] procedure VW_NoOnCalculate_ReportsMissingAlgo;
    [Test] procedure BMW_OnCalculateFires;
    [Test] procedure Mercedes_RejectsBadShape;

    // British
    [Test] procedure Jaguar_NoOnCalculate_ReportsMissingAlgo;
    [Test] procedure Saab_OnCalculateFires;

    // Volvo
    [Test] procedure Volvo_RejectsBadShape;
    [Test] procedure Volvo_OnCalculateFires;

    // Aftermarket stubs
    [Test] procedure Alpine_NoOnCalculate_ReportsMissingAlgo;
    [Test] procedure Blaupunkt_RejectsBadShape;
    [Test] procedure Clarion_OnCalculateFires;

    // Sweep
    [Test] procedure AllStubVendors_RoundTripCreateFree;
  end;

implementation

procedure TRadioCodeStubVendorsTests.Setup;
begin
  FOnCalculateFired := False;
end;

procedure TRadioCodeStubVendorsTests.HandleOnCalculate(Sender: TObject;
  const AInput: string; const AContext: TOBDRadioCodeContext;
  var AResult: TOBDRadioCodeResult);
begin
  FOnCalculateFired := True;
  AResult.Success := True;
  AResult.Code    := 'TEST';
end;

{ ---- European premium ----------------------------------------------------- }

procedure TRadioCodeStubVendorsTests.VW_NoOnCalculate_ReportsMissingAlgo;
var C: TOBDRadioCodeVW; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeVW.Create(nil);
  try
    // Pass a syntactically valid VW serial so we get past
    // shape validation and exercise the "no algorithm" path.
    R := C.Calculate('1234567');
    Assert.IsFalse(R.Success);
    Assert.IsNotEmpty(R.Message);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeStubVendorsTests.BMW_OnCalculateFires;
var C: TOBDRadioCodeBMW; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeBMW.Create(nil);
  try
    C.OnCalculate := HandleOnCalculate;
    R := C.Calculate('1234567');
    Assert.IsTrue(FOnCalculateFired,
      'OnCalculate must fire for stub vendors with valid input');
    Assert.IsTrue(R.Success);
    Assert.AreEqual('TEST', R.Code);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeStubVendorsTests.Mercedes_RejectsBadShape;
var C: TOBDRadioCodeMercedes; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeMercedes.Create(nil);
  try
    R := C.Calculate('');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

{ ---- British -------------------------------------------------------------- }

procedure TRadioCodeStubVendorsTests.Jaguar_NoOnCalculate_ReportsMissingAlgo;
var C: TOBDRadioCodeJaguar; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeJaguar.Create(nil);
  try
    R := C.Calculate('123456');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeStubVendorsTests.Saab_OnCalculateFires;
var C: TOBDRadioCodeSaab; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeSaab.Create(nil);
  try
    C.OnCalculate := HandleOnCalculate;
    R := C.Calculate('1234');
    Assert.IsTrue(FOnCalculateFired);
    Assert.IsTrue(R.Success);
  finally
    C.Free;
  end;
end;

{ ---- Volvo ---------------------------------------------------------------- }

procedure TRadioCodeStubVendorsTests.Volvo_RejectsBadShape;
var C: TOBDRadioCodeVolvo; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeVolvo.Create(nil);
  try
    R := C.Calculate('!!');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeStubVendorsTests.Volvo_OnCalculateFires;
var C: TOBDRadioCodeVolvo; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeVolvo.Create(nil);
  try
    C.OnCalculate := HandleOnCalculate;
    R := C.Calculate('1234567');
    Assert.IsTrue(FOnCalculateFired);
    Assert.IsTrue(R.Success);
  finally
    C.Free;
  end;
end;

{ ---- Aftermarket ---------------------------------------------------------- }

procedure TRadioCodeStubVendorsTests.Alpine_NoOnCalculate_ReportsMissingAlgo;
var C: TOBDRadioCodeAlpine; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeAlpine.Create(nil);
  try
    R := C.Calculate('123456');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeStubVendorsTests.Blaupunkt_RejectsBadShape;
var C: TOBDRadioCodeBlaupunkt; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeBlaupunkt.Create(nil);
  try
    R := C.Calculate('123');
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TRadioCodeStubVendorsTests.Clarion_OnCalculateFires;
var C: TOBDRadioCodeClarion; R: TOBDRadioCodeResult;
begin
  C := TOBDRadioCodeClarion.Create(nil);
  try
    C.OnCalculate := HandleOnCalculate;
    R := C.Calculate('12345');
    Assert.IsTrue(FOnCalculateFired);
    Assert.IsTrue(R.Success);
  finally
    C.Free;
  end;
end;

{ ---- Sweep ---------------------------------------------------------------- }

procedure TRadioCodeStubVendorsTests.AllStubVendors_RoundTripCreateFree;

  procedure Roundtrip(const ACtor: TFunc<TOBDRadioCode>);
  var C: TOBDRadioCode;
  begin
    C := ACtor();
    try
      Assert.IsNotEmpty(C.BrandKey);
      Assert.IsNotEmpty(C.DisplayName);
    finally
      C.Free;
    end;
  end;

begin
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeVW.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeAudiConcert.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeBMW.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeMercedes.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeMini.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodePorsche.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeSEAT.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeSkoda.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeSmart.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeJaguar.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeLandRover.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeSaab.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeOpel.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeVolvo.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeAlpine.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeBlaupunkt.Create(nil) end);
  Roundtrip(function: TOBDRadioCode begin Result := TOBDRadioCodeClarion.Create(nil) end);
  Assert.Pass('17 stub-vendor classes round-tripped through Create + Free.');
end;

initialization
  TDUnitX.RegisterTestFixture(TRadioCodeStubVendorsTests);

end.
