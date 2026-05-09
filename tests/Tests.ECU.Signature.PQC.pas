//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Signature.PQC
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.ECU.Signature.PQC;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPQCSignatureTests = class
  public
    [Test] procedure EnvelopeRoundTrips;
    [Test] procedure EnvelopeWithEmptyKeyIdRoundTrips;
    [Test] procedure EnvelopeTruncatedAtSigLenRaises;
    [Test] procedure EnvelopeTruncatedAtSignatureRaises;
    [Test] procedure EnvelopeTooShortRaises;
    [Test] procedure VerifyAlgorithmMismatchRaises;
    [Test] procedure VerifyRaisesNotAvailableUntilBindingShips;
    [Test] procedure ConstructorRejectsUnknownAlgorithm;
    [Test] procedure ConstructorRejectsEmptyPublicKey;
    [Test] procedure AlgorithmNameMatchesEnum;
  end;

implementation

uses
  System.SysUtils, OBD.ECU.Signature, OBD.ECU.Signature.PQC;

procedure TPQCSignatureTests.EnvelopeRoundTrips;
var
  Env, Out_: TOBDPQCEnvelope;
  Bytes: TBytes;
begin
  Env := Default(TOBDPQCEnvelope);
  Env.Algorithm := pqcMlDsa65;
  Env.KeyId := TBytes.Create($AA, $BB, $CC, $DD);
  Env.Signature := TBytes.Create($01, $02, $03, $04, $05);
  Bytes := EncodePQCEnvelope(Env);
  Out_ := DecodePQCEnvelope(Bytes);
  Assert.AreEqual(Ord(pqcMlDsa65), Ord(Out_.Algorithm));
  Assert.AreEqual(4, Length(Out_.KeyId));
  Assert.AreEqual(5, Length(Out_.Signature));
  Assert.AreEqual($AA, Integer(Out_.KeyId[0]));
  Assert.AreEqual($05, Integer(Out_.Signature[4]));
end;

procedure TPQCSignatureTests.EnvelopeWithEmptyKeyIdRoundTrips;
var
  Env, Out_: TOBDPQCEnvelope;
  Bytes: TBytes;
begin
  Env := Default(TOBDPQCEnvelope);
  Env.Algorithm := pqcSlhDsaSha2128s;
  Env.Signature := TBytes.Create($FF);
  Bytes := EncodePQCEnvelope(Env);
  Out_ := DecodePQCEnvelope(Bytes);
  Assert.AreEqual(0, Length(Out_.KeyId));
  Assert.AreEqual(1, Length(Out_.Signature));
end;

procedure TPQCSignatureTests.EnvelopeTruncatedAtSigLenRaises;
var Bytes: TBytes;
begin
  Bytes := TBytes.Create($02, $00, $00, $00, $00); // missing one sig-len byte
  Assert.WillRaise(
    procedure begin DecodePQCEnvelope(Bytes); end,
    EOBDPQCSignature);
end;

procedure TPQCSignatureTests.EnvelopeTruncatedAtSignatureRaises;
var Bytes: TBytes;
begin
  // alg=2, keylen=0, siglen=4, but only 2 sig bytes follow
  Bytes := TBytes.Create($02, $00, $00, $00, $00, $04, $AA, $BB);
  Assert.WillRaise(
    procedure begin DecodePQCEnvelope(Bytes); end,
    EOBDPQCSignature);
end;

procedure TPQCSignatureTests.EnvelopeTooShortRaises;
begin
  Assert.WillRaise(
    procedure begin DecodePQCEnvelope(TBytes.Create($00, $00, $00)); end,
    EOBDPQCSignature);
end;

procedure TPQCSignatureTests.VerifyAlgorithmMismatchRaises;
var
  V: IFirmwareSignatureVerifier;
  Env: TOBDPQCEnvelope;
  EnvBytes: TBytes;
begin
  V := TOBDPQCSignatureVerifier.Create(pqcMlDsa65, TBytes.Create($01, $02));
  Env := Default(TOBDPQCEnvelope);
  Env.Algorithm := pqcSlhDsaSha2128s;
  Env.Signature := TBytes.Create($AA);
  EnvBytes := EncodePQCEnvelope(Env);
  Assert.WillRaise(
    procedure begin V.Verify(TBytes.Create($00), EnvBytes); end,
    EOBDPQCSignature);
end;

procedure TPQCSignatureTests.VerifyRaisesNotAvailableUntilBindingShips;
var
  V: IFirmwareSignatureVerifier;
  Env: TOBDPQCEnvelope;
  EnvBytes: TBytes;
begin
  V := TOBDPQCSignatureVerifier.Create(pqcMlDsa65, TBytes.Create($01, $02));
  Env := Default(TOBDPQCEnvelope);
  Env.Algorithm := pqcMlDsa65;
  Env.Signature := TBytes.Create($AA, $BB);
  EnvBytes := EncodePQCEnvelope(Env);
  Assert.WillRaise(
    procedure begin V.Verify(TBytes.Create($00), EnvBytes); end,
    EOBDPQCNotAvailable);
end;

procedure TPQCSignatureTests.ConstructorRejectsUnknownAlgorithm;
begin
  Assert.WillRaise(
    procedure
    var V: IFirmwareSignatureVerifier;
    begin
      V := TOBDPQCSignatureVerifier.Create(pqcUnknown, TBytes.Create($01));
    end,
    EOBDPQCSignature);
end;

procedure TPQCSignatureTests.ConstructorRejectsEmptyPublicKey;
begin
  Assert.WillRaise(
    procedure
    var V: IFirmwareSignatureVerifier;
    begin
      V := TOBDPQCSignatureVerifier.Create(pqcMlDsa65, nil);
    end,
    EOBDPQCSignature);
end;

procedure TPQCSignatureTests.AlgorithmNameMatchesEnum;
begin
  Assert.AreEqual('ML-DSA-65', PQCAlgorithmName(pqcMlDsa65));
  Assert.AreEqual('SLH-DSA-SHA2-128s', PQCAlgorithmName(pqcSlhDsaSha2128s));
  Assert.AreEqual('PQC-UNKNOWN', PQCAlgorithmName(pqcUnknown));
end;

initialization
  TDUnitX.RegisterTestFixture(TPQCSignatureTests);

end.
