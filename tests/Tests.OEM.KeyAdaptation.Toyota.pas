//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.KeyAdaptation.Toyota
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.KeyAdaptation.Toyota;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TToyotaKeyAdaptationTests = class
  public
    [Test] procedure RequestRoundTripWithMasterKey;
    [Test] procedure RequestRoundTripWithPIN;
    [Test] procedure RequestRequiresPINWhenNoMasterKey;
    [Test] procedure RequestPinTooLongRaises;
    [Test] procedure ResponseRoundTrip;
    [Test] procedure ResponseBadAddedKeyIdRaises;
    [Test] procedure CamryIsMasterKey;
    [Test] procedure NX300IsPin;
    [Test] procedure UnknownIsCertificateLocked;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.KeyAdaptation.Toyota;

procedure TToyotaKeyAdaptationTests.RequestRoundTripWithMasterKey;
var
  In_, Out_: TToyotaKeyRegisterRequest;
  Bytes: TBytes;
begin
  In_.VIN := 'JTDBR32E230012345';
  In_.Mode := tkmAddKey;
  In_.MasterKeyPresent := True;
  In_.PIN := '';
  Bytes := EncodeToyotaKeyRegisterRequest(In_);
  Out_ := DecodeToyotaKeyRegisterRequest(Bytes);
  Assert.IsTrue(Out_.MasterKeyPresent);
  Assert.AreEqual('', Out_.PIN);
end;

procedure TToyotaKeyAdaptationTests.RequestRoundTripWithPIN;
var
  In_, Out_: TToyotaKeyRegisterRequest;
  Bytes: TBytes;
begin
  In_.VIN := 'JTDBR32E230012345';
  In_.Mode := tkmAddKey;
  In_.MasterKeyPresent := False;
  In_.PIN := '987654';
  Bytes := EncodeToyotaKeyRegisterRequest(In_);
  Out_ := DecodeToyotaKeyRegisterRequest(Bytes);
  Assert.IsFalse(Out_.MasterKeyPresent);
  Assert.AreEqual('987654', Out_.PIN);
end;

procedure TToyotaKeyAdaptationTests.RequestRequiresPINWhenNoMasterKey;
var Req: TToyotaKeyRegisterRequest;
begin
  Req.VIN := 'JTDBR32E230012345';
  Req.Mode := tkmAddKey;
  Req.MasterKeyPresent := False;
  Req.PIN := '';
  Assert.WillRaise(
    procedure begin EncodeToyotaKeyRegisterRequest(Req); end,
    EOBDToyotaKey);
end;

procedure TToyotaKeyAdaptationTests.RequestPinTooLongRaises;
var Req: TToyotaKeyRegisterRequest;
begin
  Req.VIN := 'JTDBR32E230012345';
  Req.Mode := tkmAddKey;
  Req.MasterKeyPresent := False;
  Req.PIN := '12345678901234567';   // 17 chars
  Assert.WillRaise(
    procedure begin EncodeToyotaKeyRegisterRequest(Req); end,
    EOBDToyotaKey);
end;

procedure TToyotaKeyAdaptationTests.ResponseRoundTrip;
var
  In_, Out_: TToyotaKeyRegisterResponse;
  Bytes: TBytes;
begin
  In_.Mode := tkmAddKey;
  In_.Success := True;
  In_.KeyCount := 3;
  In_.AddedKeyId := TBytes.Create($AA, $BB, $CC, $DD);
  Bytes := EncodeToyotaKeyRegisterResponse(In_);
  Out_ := DecodeToyotaKeyRegisterResponse(Bytes);
  Assert.IsTrue(Out_.Success);
  Assert.AreEqual(Integer(3), Integer(Out_.KeyCount));
  Assert.AreEqual(Integer($AA), Integer(Out_.AddedKeyId[0]));
  Assert.AreEqual(Integer($DD), Integer(Out_.AddedKeyId[3]));
end;

procedure TToyotaKeyAdaptationTests.ResponseBadAddedKeyIdRaises;
var Resp: TToyotaKeyRegisterResponse;
begin
  Resp.Mode := tkmAddKey;
  Resp.Success := True;
  Resp.KeyCount := 1;
  Resp.AddedKeyId := TBytes.Create($00, $00);   // wrong length
  Assert.WillRaise(
    procedure begin EncodeToyotaKeyRegisterResponse(Resp); end,
    EOBDToyotaKey);
end;

procedure TToyotaKeyAdaptationTests.CamryIsMasterKey;
var P: TToyotaPlatformInfo;
begin
  P := FindToyotaPlatform('asv50');
  Assert.AreEqual(Ord(tpaMasterKey), Ord(P.Access));
end;

procedure TToyotaKeyAdaptationTests.NX300IsPin;
var P: TToyotaPlatformInfo;
begin
  P := FindToyotaPlatform('agz10');
  Assert.AreEqual(Ord(tpaPin), Ord(P.Access));
end;

procedure TToyotaKeyAdaptationTests.UnknownIsCertificateLocked;
var P: TToyotaPlatformInfo;
begin
  P := FindToyotaPlatform('unknown-chassis');
  Assert.AreEqual(Ord(tpaCertificateRequired), Ord(P.Access));
end;

initialization
  TDUnitX.RegisterTestFixture(TToyotaKeyAdaptationTests);

end.
