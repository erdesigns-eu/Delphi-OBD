//------------------------------------------------------------------------------
//  Tests.OBD.Service.VWRadioSAFE
//
//  Pinned-vector tests for the TOBDVWRadioSAFE skeleton. The
//  test fakes the host-supplied KWP1281 transport (OnReadEEPROM)
//  so the component's variant-map and bundled-decode behaviour
//  can be exercised in isolation, before the real KWP1281 codec
//  lands.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.VWRadioSAFE;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  OBD.Service.VWRadioSAFE;

type
  [TestFixture]
  TVWRadioSAFETests = class
  strict private
    FFakeBytes: TBytes;
    FFakeOk:    Boolean;
    FLastAddr:  Word;
    FLastLen:   Byte;
    procedure FakeReadEEPROM(Sender: TObject;
      AAddress: Word; ALength: Byte;
      out AData: TBytes; out ASuccess: Boolean;
      out AError: string);
  public
    [Setup] procedure Setup;

    [Test] procedure PremiumIV_DecodesFourASCIIDigits;
    [Test] procedure Gamma5_DecodesTwoBCDBytes;
    [Test] procedure NoTransport_FailsWithExplanation;
    [Test] procedure TransportFailure_PropagatesMessage;
    [Test] procedure ShortResponse_FailsCleanly;
    [Test] procedure CustomVariant_RequiresOnDecode;
  end;

implementation

procedure TVWRadioSAFETests.FakeReadEEPROM(Sender: TObject;
  AAddress: Word; ALength: Byte;
  out AData: TBytes; out ASuccess: Boolean;
  out AError: string);
begin
  FLastAddr := AAddress;
  FLastLen  := ALength;
  AData     := Copy(FFakeBytes, 0, ALength);
  ASuccess  := FFakeOk;
  AError    := '';
end;

procedure TVWRadioSAFETests.Setup;
begin
  FFakeBytes := nil;
  FFakeOk    := True;
  FLastAddr  := 0;
  FLastLen   := 0;
end;

procedure TVWRadioSAFETests.PremiumIV_DecodesFourASCIIDigits;
var C: TOBDVWRadioSAFE; R: TVWRadioSAFEResult;
begin
  FFakeBytes := TBytes.Create(Ord('1'), Ord('2'), Ord('3'), Ord('4'));
  C := TOBDVWRadioSAFE.Create(nil);
  try
    C.RadioVariant := svPremiumIV;
    C.OnReadEEPROM := FakeReadEEPROM;
    R := C.Extract;
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('1234', R.Code);
    Assert.AreEqual(Word($0083), FLastAddr);
    Assert.AreEqual(Byte(4),     FLastLen);
    Assert.AreEqual('31 32 33 34', R.RawHex);
  finally
    C.Free;
  end;
end;

procedure TVWRadioSAFETests.Gamma5_DecodesTwoBCDBytes;
var C: TOBDVWRadioSAFE; R: TVWRadioSAFEResult;
begin
  FFakeBytes := TBytes.Create($12, $34);
  C := TOBDVWRadioSAFE.Create(nil);
  try
    C.RadioVariant := svGamma5;
    C.OnReadEEPROM := FakeReadEEPROM;
    R := C.Extract;
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('1234', R.Code);
    Assert.AreEqual(Word($0014), FLastAddr);
    Assert.AreEqual(Byte(2),     FLastLen);
  finally
    C.Free;
  end;
end;

procedure TVWRadioSAFETests.NoTransport_FailsWithExplanation;
var C: TOBDVWRadioSAFE; R: TVWRadioSAFEResult;
begin
  C := TOBDVWRadioSAFE.Create(nil);
  try
    R := C.Extract;
    Assert.IsFalse(R.Success);
    Assert.IsTrue(R.Message.Contains('OnReadEEPROM'));
  finally
    C.Free;
  end;
end;

procedure TVWRadioSAFETests.TransportFailure_PropagatesMessage;
var C: TOBDVWRadioSAFE; R: TVWRadioSAFEResult;
begin
  FFakeBytes := nil;
  FFakeOk    := False;
  C := TOBDVWRadioSAFE.Create(nil);
  try
    C.RadioVariant := svPremiumIV;
    C.OnReadEEPROM := FakeReadEEPROM;
    R := C.Extract;
    Assert.IsFalse(R.Success);
    Assert.IsTrue(R.Message.Contains('OnReadEEPROM'));
  finally
    C.Free;
  end;
end;

procedure TVWRadioSAFETests.ShortResponse_FailsCleanly;
var C: TOBDVWRadioSAFE; R: TVWRadioSAFEResult;
begin
  FFakeBytes := TBytes.Create(Ord('1'));  // only 1 byte instead of 4
  C := TOBDVWRadioSAFE.Create(nil);
  try
    C.RadioVariant := svPremiumIV;
    C.OnReadEEPROM := FakeReadEEPROM;
    R := C.Extract;
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TVWRadioSAFETests.CustomVariant_RequiresOnDecode;
var C: TOBDVWRadioSAFE; R: TVWRadioSAFEResult;
begin
  C := TOBDVWRadioSAFE.Create(nil);
  try
    C.RadioVariant  := svCustom;
    C.CustomAddress := $0100;
    C.CustomLength  := 0;  // missing length too
    C.OnReadEEPROM  := FakeReadEEPROM;
    R := C.Extract;
    Assert.IsFalse(R.Success);
    Assert.IsTrue(R.Message.Contains('svCustom'));
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TVWRadioSAFETests);

end.
