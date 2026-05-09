//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.KeyAdaptation.HMG
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.KeyAdaptation.HMG;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  THMGKeyAdaptationTests = class
  public
    [Test] procedure RequestRoundTrip;
    [Test] procedure ResponseRoundTrip;
    [Test] procedure RequestRejectsBadVIN;
    [Test] procedure RequestRejectsBadPINLength;
    [Test] procedure RequestRejectsBadKeyIndex;
    [Test] procedure ResponseDecodeBadLengthRaises;
    [Test] procedure PlatformLookupReturnsKnown;
    [Test] procedure PlatformLookupUnknownIsCertificateRequired;
    [Test] procedure EGMPIsGatewayLocked;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.KeyAdaptation.HMG;

procedure THMGKeyAdaptationTests.RequestRoundTrip;
var
  In_, Out_: THMGKeyRegisterRequest;
  Bytes: TBytes;
begin
  In_.VIN := 'KMHE241CBKA000001';
  In_.Mode := hkmAddKey;
  In_.PIN := '1234';
  In_.KeyIndex := 2;
  Bytes := EncodeHMGKeyRegisterRequest(In_);
  Out_ := DecodeHMGKeyRegisterRequest(Bytes);
  Assert.AreEqual('KMHE241CBKA000001', Out_.VIN);
  Assert.AreEqual(Ord(hkmAddKey), Ord(Out_.Mode));
  Assert.AreEqual('1234', Out_.PIN);
  Assert.AreEqual(Integer(2), Integer(Out_.KeyIndex));
end;

procedure THMGKeyAdaptationTests.ResponseRoundTrip;
var
  In_, Out_: THMGKeyRegisterResponse;
  Bytes: TBytes;
begin
  In_.Mode := hkmReadCount;
  In_.Success := True;
  In_.KeyCount := 4;
  In_.StatusCode := $00;
  Bytes := EncodeHMGKeyRegisterResponse(In_);
  Out_ := DecodeHMGKeyRegisterResponse(Bytes);
  Assert.IsTrue(Out_.Success);
  Assert.AreEqual(Integer(4), Integer(Out_.KeyCount));
end;

procedure THMGKeyAdaptationTests.RequestRejectsBadVIN;
var Req: THMGKeyRegisterRequest;
begin
  Req.VIN := 'TOO-SHORT';
  Req.Mode := hkmAddKey;
  Req.PIN := '1234';
  Req.KeyIndex := 0;
  Assert.WillRaise(
    procedure begin EncodeHMGKeyRegisterRequest(Req); end, EOBDHMGKey);
end;

procedure THMGKeyAdaptationTests.RequestRejectsBadPINLength;
var Req: THMGKeyRegisterRequest;
begin
  Req.VIN := 'KMHE241CBKA000001';
  Req.Mode := hkmAddKey;
  Req.PIN := '12';     // too short
  Req.KeyIndex := 0;
  Assert.WillRaise(
    procedure begin EncodeHMGKeyRegisterRequest(Req); end, EOBDHMGKey);
end;

procedure THMGKeyAdaptationTests.RequestRejectsBadKeyIndex;
var Req: THMGKeyRegisterRequest;
begin
  Req.VIN := 'KMHE241CBKA000001';
  Req.Mode := hkmAddKey;
  Req.PIN := '1234';
  Req.KeyIndex := 8;   // out of range
  Assert.WillRaise(
    procedure begin EncodeHMGKeyRegisterRequest(Req); end, EOBDHMGKey);
end;

procedure THMGKeyAdaptationTests.ResponseDecodeBadLengthRaises;
begin
  Assert.WillRaise(
    procedure begin DecodeHMGKeyRegisterResponse(TBytes.Create($00)); end,
    EOBDHMGKey);
end;

procedure THMGKeyAdaptationTests.PlatformLookupReturnsKnown;
var P: THMGPlatformInfo;
begin
  P := FindHMGPlatform('rb');
  Assert.IsTrue(P.DisplayName.Contains('i20'));
  Assert.AreEqual(Ord(hpaOpenWithPIN), Ord(P.Access));
end;

procedure THMGKeyAdaptationTests.PlatformLookupUnknownIsCertificateRequired;
var P: THMGPlatformInfo;
begin
  P := FindHMGPlatform('made-up-platform');
  Assert.AreEqual(Ord(hpaCertificateRequired), Ord(P.Access));
end;

procedure THMGKeyAdaptationTests.EGMPIsGatewayLocked;
var P: THMGPlatformInfo;
begin
  P := FindHMGPlatform('ev_e_gmp');
  Assert.AreEqual(Ord(hpaGatewayLockedPostMY2020), Ord(P.Access));
end;

initialization
  TDUnitX.RegisterTestFixture(THMGKeyAdaptationTests);

end.
