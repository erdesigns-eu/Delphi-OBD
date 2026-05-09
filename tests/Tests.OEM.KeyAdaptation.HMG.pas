//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.KeyAdaptation.HMG.pas
// CONTENTS       : Tests for OBD.OEM.KeyAdaptation.HMG
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
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
    /// <summary>
    ///   Request round trip.
    /// </summary>
    [Test] procedure RequestRoundTrip;
    /// <summary>
    ///   Response round trip.
    /// </summary>
    [Test] procedure ResponseRoundTrip;
    /// <summary>
    ///   Request rejects bad v i n.
    /// </summary>
    [Test] procedure RequestRejectsBadVIN;
    /// <summary>
    ///   Request rejects bad p i n length.
    /// </summary>
    [Test] procedure RequestRejectsBadPINLength;
    /// <summary>
    ///   Request rejects bad key index.
    /// </summary>
    [Test] procedure RequestRejectsBadKeyIndex;
    /// <summary>
    ///   Response decode bad length raises.
    /// </summary>
    [Test] procedure ResponseDecodeBadLengthRaises;
    /// <summary>
    ///   Platform lookup returns known.
    /// </summary>
    [Test] procedure PlatformLookupReturnsKnown;
    /// <summary>
    ///   Platform lookup unknown is certificate required.
    /// </summary>
    [Test] procedure PlatformLookupUnknownIsCertificateRequired;
    /// <summary>
    ///   E g m p is gateway locked.
    /// </summary>
    [Test] procedure EGMPIsGatewayLocked;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.KeyAdaptation.HMG;

//------------------------------------------------------------------------------
// REQUEST ROUND TRIP
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// RESPONSE ROUND TRIP
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// REQUEST REJECTS BAD VIN
//------------------------------------------------------------------------------
procedure THMGKeyAdaptationTests.RequestRejectsBadVIN;
var
  Req: THMGKeyRegisterRequest;
begin
  Req.VIN := 'TOO-SHORT';
  Req.Mode := hkmAddKey;
  Req.PIN := '1234';
  Req.KeyIndex := 0;
  Assert.WillRaise(
    procedure begin EncodeHMGKeyRegisterRequest(Req); end, EOBDHMGKey);
end;

//------------------------------------------------------------------------------
// REQUEST REJECTS BAD PINLENGTH
//------------------------------------------------------------------------------
procedure THMGKeyAdaptationTests.RequestRejectsBadPINLength;
var
  Req: THMGKeyRegisterRequest;
begin
  Req.VIN := 'KMHE241CBKA000001';
  Req.Mode := hkmAddKey;
  Req.PIN := '12';     // too short
  Req.KeyIndex := 0;
  Assert.WillRaise(
    procedure begin EncodeHMGKeyRegisterRequest(Req); end, EOBDHMGKey);
end;

//------------------------------------------------------------------------------
// REQUEST REJECTS BAD KEY INDEX
//------------------------------------------------------------------------------
procedure THMGKeyAdaptationTests.RequestRejectsBadKeyIndex;
var
  Req: THMGKeyRegisterRequest;
begin
  Req.VIN := 'KMHE241CBKA000001';
  Req.Mode := hkmAddKey;
  Req.PIN := '1234';
  Req.KeyIndex := 8;   // out of range
  Assert.WillRaise(
    procedure begin EncodeHMGKeyRegisterRequest(Req); end, EOBDHMGKey);
end;

//------------------------------------------------------------------------------
// RESPONSE DECODE BAD LENGTH RAISES
//------------------------------------------------------------------------------
procedure THMGKeyAdaptationTests.ResponseDecodeBadLengthRaises;
begin
  Assert.WillRaise(
    procedure begin DecodeHMGKeyRegisterResponse(TBytes.Create($00)); end,
    EOBDHMGKey);
end;

//------------------------------------------------------------------------------
// PLATFORM LOOKUP RETURNS KNOWN
//------------------------------------------------------------------------------
procedure THMGKeyAdaptationTests.PlatformLookupReturnsKnown;
var
  P: THMGPlatformInfo;
begin
  P := FindHMGPlatform('rb');
  Assert.IsTrue(P.DisplayName.Contains('i20'));
  Assert.AreEqual(Ord(hpaOpenWithPIN), Ord(P.Access));
end;

//------------------------------------------------------------------------------
// PLATFORM LOOKUP UNKNOWN IS CERTIFICATE REQUIRED
//------------------------------------------------------------------------------
procedure THMGKeyAdaptationTests.PlatformLookupUnknownIsCertificateRequired;
var
  P: THMGPlatformInfo;
begin
  P := FindHMGPlatform('made-up-platform');
  Assert.AreEqual(Ord(hpaCertificateRequired), Ord(P.Access));
end;

//------------------------------------------------------------------------------
// EGMPIS GATEWAY LOCKED
//------------------------------------------------------------------------------
procedure THMGKeyAdaptationTests.EGMPIsGatewayLocked;
var
  P: THMGPlatformInfo;
begin
  P := FindHMGPlatform('ev_e_gmp');
  Assert.AreEqual(Ord(hpaGatewayLockedPostMY2020), Ord(P.Access));
end;

initialization
  TDUnitX.RegisterTestFixture(THMGKeyAdaptationTests);

end.
