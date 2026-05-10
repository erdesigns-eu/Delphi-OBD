//------------------------------------------------------------------------------
//  Tests.OBD.OEM.KeyAdaptation
//
//  Coverage for the key-adaptation safety scaffold.
//  Per-vendor protocol exchanges need a real ECU and are out
//  of unit-test scope; we test that destructive ops are
//  refused unless AutoExecute + OnConfirmExecute are wired,
//  and that ChassisCode / PIN validation fires.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.KeyAdaptation;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.OEM.KeyAdaptation.Types,
  OBD.OEM.KeyAdaptation.Base,
  OBD.OEM.KeyAdaptation.Ford,
  OBD.OEM.KeyAdaptation.HMG,
  OBD.OEM.KeyAdaptation.BMW,
  OBD.OEM.KeyAdaptation.Toyota;

type
  [TestFixture]
  TKeyAdaptSafetyTests = class
  strict private
    FConfirmCalled: Boolean;
    FConfirmAnswer: Boolean;
    FResultCaptured: TOBDKeyAdaptResult;
    procedure HandleConfirm(Sender: TObject; AOp: TOBDKeyAdaptOp;
      ASlotIndex: Byte; var ACanProceed: Boolean);
    procedure HandleResult(Sender: TObject;
      const AResult: TOBDKeyAdaptResult);
  public
    [Setup] procedure Setup;

    [Test] procedure AddKeyWithoutAutoExecuteRaises;
    [Test] procedure AddKeyWithoutOnConfirmRaises;
    [Test] procedure AddKeyHostRejectsConfirmation;
    [Test] procedure FordPINMustBe4Or8Chars;
    [Test] procedure HMGPINMustBe4Or6Digits;
    [Test] procedure BMWISNMustBe32HexChars;
    [Test] procedure ToyotaPINMustBe8Or16Chars;
  end;

implementation

procedure TKeyAdaptSafetyTests.HandleConfirm(Sender: TObject;
  AOp: TOBDKeyAdaptOp; ASlotIndex: Byte;
  var ACanProceed: Boolean);
begin
  FConfirmCalled := True;
  ACanProceed := FConfirmAnswer;
end;

procedure TKeyAdaptSafetyTests.HandleResult(Sender: TObject;
  const AResult: TOBDKeyAdaptResult);
begin
  FResultCaptured := AResult;
end;

procedure TKeyAdaptSafetyTests.Setup;
begin
  FConfirmCalled := False;
  FConfirmAnswer := True;
  FResultCaptured := Default(TOBDKeyAdaptResult);
end;

procedure TKeyAdaptSafetyTests.AddKeyWithoutAutoExecuteRaises;
var C: TOBDKeyAdaptationFord;
begin
  C := TOBDKeyAdaptationFord.Create(nil);
  try
    // AutoExecute defaults to False
    C.OnConfirmExecute := HandleConfirm;
    Assert.WillRaise(
      procedure begin C.AddKey end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKeyAdaptSafetyTests.AddKeyWithoutOnConfirmRaises;
var C: TOBDKeyAdaptationFord;
begin
  C := TOBDKeyAdaptationFord.Create(nil);
  try
    C.AutoExecute := True;
    // OnConfirmExecute deliberately not assigned
    Assert.WillRaise(
      procedure begin C.AddKey end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TKeyAdaptSafetyTests.AddKeyHostRejectsConfirmation;
var
  C: TOBDKeyAdaptationFord;
  R: TOBDKeyAdaptResult;
begin
  C := TOBDKeyAdaptationFord.Create(nil);
  try
    C.AutoExecute := True;
    C.OnConfirmExecute := HandleConfirm;
    C.OnResult := HandleResult;
    C.ChassisCode := 'p552';
    C.PIN := '12345678';
    FConfirmAnswer := False;
    R := C.AddKey;
    Assert.IsTrue(FConfirmCalled, 'OnConfirmExecute should fire');
    Assert.IsFalse(R.Success);
    Assert.IsTrue(R.Message.Contains('rejected'),
      'Result message should mention rejection; got: ' + R.Message);
    Assert.AreEqual(Ord(kaoAddKey), Ord(R.Op));
  finally
    C.Free;
  end;
end;

procedure TKeyAdaptSafetyTests.FordPINMustBe4Or8Chars;
var
  C: TOBDKeyAdaptationFord;
  R: TOBDKeyAdaptResult;
begin
  C := TOBDKeyAdaptationFord.Create(nil);
  try
    C.PIN := '123';  // wrong length
    R := C.CheckPin;
    Assert.IsFalse(R.Success);
    Assert.IsTrue(R.Message.Contains('4 or 8'));
    C.PIN := '1234';  // valid
    R := C.CheckPin;
    Assert.IsTrue(R.Success);
  finally
    C.Free;
  end;
end;

procedure TKeyAdaptSafetyTests.HMGPINMustBe4Or6Digits;
var
  C: TOBDKeyAdaptationHMG;
  R: TOBDKeyAdaptResult;
begin
  C := TOBDKeyAdaptationHMG.Create(nil);
  try
    C.PIN := '12345';  // wrong length
    R := C.CheckPin;
    Assert.IsFalse(R.Success);
    C.PIN := '123456';  // valid
    R := C.CheckPin;
    Assert.IsTrue(R.Success);
  finally
    C.Free;
  end;
end;

procedure TKeyAdaptSafetyTests.BMWISNMustBe32HexChars;
var
  C: TOBDKeyAdaptationBMW;
  R: TOBDKeyAdaptResult;
begin
  C := TOBDKeyAdaptationBMW.Create(nil);
  try
    C.PIN := 'too-short';
    R := C.CheckPin;
    Assert.IsFalse(R.Success);
    C.PIN := StringOfChar('A', 32);
    R := C.CheckPin;
    Assert.IsTrue(R.Success);
  finally
    C.Free;
  end;
end;

procedure TKeyAdaptSafetyTests.ToyotaPINMustBe8Or16Chars;
var
  C: TOBDKeyAdaptationToyota;
  R: TOBDKeyAdaptResult;
begin
  C := TOBDKeyAdaptationToyota.Create(nil);
  try
    C.PIN := '1234567';   // wrong
    R := C.CheckPin;
    Assert.IsFalse(R.Success);
    C.PIN := '12345678';  // master-key
    R := C.CheckPin;
    Assert.IsTrue(R.Success);
    C.PIN := StringOfChar('F', 16);  // smart-key cert
    R := C.CheckPin;
    Assert.IsTrue(R.Success);
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TKeyAdaptSafetyTests);

end.
