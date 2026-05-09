//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.KeyAdaptation.Toyota.pas
// CONTENTS       : Tests for OBD.OEM.KeyAdaptation.Toyota
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
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
    /// <summary>
    ///   Request round trip with master key.
    /// </summary>
    [Test] procedure RequestRoundTripWithMasterKey;
    /// <summary>
    ///   Request round trip with p i n.
    /// </summary>
    [Test] procedure RequestRoundTripWithPIN;
    /// <summary>
    ///   Request requires p i n when no master key.
    /// </summary>
    [Test] procedure RequestRequiresPINWhenNoMasterKey;
    /// <summary>
    ///   Request pin too long raises.
    /// </summary>
    [Test] procedure RequestPinTooLongRaises;
    /// <summary>
    ///   Response round trip.
    /// </summary>
    [Test] procedure ResponseRoundTrip;
    /// <summary>
    ///   Response bad added key id raises.
    /// </summary>
    [Test] procedure ResponseBadAddedKeyIdRaises;
    /// <summary>
    ///   Camry is master key.
    /// </summary>
    [Test] procedure CamryIsMasterKey;
    /// <summary>
    ///   N x300 is pin.
    /// </summary>
    [Test] procedure NX300IsPin;
    /// <summary>
    ///   Unknown is certificate locked.
    /// </summary>
    [Test] procedure UnknownIsCertificateLocked;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.KeyAdaptation.Toyota;

//------------------------------------------------------------------------------
// REQUEST ROUND TRIP WITH MASTER KEY
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// REQUEST ROUND TRIP WITH PIN
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// REQUEST REQUIRES PINWHEN NO MASTER KEY
//------------------------------------------------------------------------------
procedure TToyotaKeyAdaptationTests.RequestRequiresPINWhenNoMasterKey;
var
  Req: TToyotaKeyRegisterRequest;
begin
  Req.VIN := 'JTDBR32E230012345';
  Req.Mode := tkmAddKey;
  Req.MasterKeyPresent := False;
  Req.PIN := '';
  Assert.WillRaise(
    procedure begin EncodeToyotaKeyRegisterRequest(Req); end,
    EOBDToyotaKey);
end;

//------------------------------------------------------------------------------
// REQUEST PIN TOO LONG RAISES
//------------------------------------------------------------------------------
procedure TToyotaKeyAdaptationTests.RequestPinTooLongRaises;
var
  Req: TToyotaKeyRegisterRequest;
begin
  Req.VIN := 'JTDBR32E230012345';
  Req.Mode := tkmAddKey;
  Req.MasterKeyPresent := False;
  Req.PIN := '12345678901234567';   // 17 chars
  Assert.WillRaise(
    procedure begin EncodeToyotaKeyRegisterRequest(Req); end,
    EOBDToyotaKey);
end;

//------------------------------------------------------------------------------
// RESPONSE ROUND TRIP
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// RESPONSE BAD ADDED KEY ID RAISES
//------------------------------------------------------------------------------
procedure TToyotaKeyAdaptationTests.ResponseBadAddedKeyIdRaises;
var
  Resp: TToyotaKeyRegisterResponse;
begin
  Resp.Mode := tkmAddKey;
  Resp.Success := True;
  Resp.KeyCount := 1;
  Resp.AddedKeyId := TBytes.Create($00, $00);   // wrong length
  Assert.WillRaise(
    procedure begin EncodeToyotaKeyRegisterResponse(Resp); end,
    EOBDToyotaKey);
end;

//------------------------------------------------------------------------------
// CAMRY IS MASTER KEY
//------------------------------------------------------------------------------
procedure TToyotaKeyAdaptationTests.CamryIsMasterKey;
var
  P: TToyotaPlatformInfo;
begin
  P := FindToyotaPlatform('asv50');
  Assert.AreEqual(Ord(tpaMasterKey), Ord(P.Access));
end;

//------------------------------------------------------------------------------
// NX300 IS PIN
//------------------------------------------------------------------------------
procedure TToyotaKeyAdaptationTests.NX300IsPin;
var
  P: TToyotaPlatformInfo;
begin
  P := FindToyotaPlatform('agz10');
  Assert.AreEqual(Ord(tpaPin), Ord(P.Access));
end;

//------------------------------------------------------------------------------
// UNKNOWN IS CERTIFICATE LOCKED
//------------------------------------------------------------------------------
procedure TToyotaKeyAdaptationTests.UnknownIsCertificateLocked;
var
  P: TToyotaPlatformInfo;
begin
  P := FindToyotaPlatform('unknown-chassis');
  Assert.AreEqual(Ord(tpaCertificateRequired), Ord(P.Access));
end;

initialization
  TDUnitX.RegisterTestFixture(TToyotaKeyAdaptationTests);

end.
