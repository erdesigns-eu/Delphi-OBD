//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Signature.PQC.pas
// CONTENTS       : Tests for OBD.ECU.Signature.PQC
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
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
    /// <summary>
    ///   Envelope round trips.
    /// </summary>
    [Test] procedure EnvelopeRoundTrips;
    /// <summary>
    ///   Envelope with empty key id round trips.
    /// </summary>
    [Test] procedure EnvelopeWithEmptyKeyIdRoundTrips;
    /// <summary>
    ///   Envelope truncated at sig len raises.
    /// </summary>
    [Test] procedure EnvelopeTruncatedAtSigLenRaises;
    /// <summary>
    ///   Envelope truncated at signature raises.
    /// </summary>
    [Test] procedure EnvelopeTruncatedAtSignatureRaises;
    /// <summary>
    ///   Envelope too short raises.
    /// </summary>
    [Test] procedure EnvelopeTooShortRaises;
    /// <summary>
    ///   Verify algorithm mismatch raises.
    /// </summary>
    [Test] procedure VerifyAlgorithmMismatchRaises;
    /// <summary>
    ///   Verify raises not available until binding ships.
    /// </summary>
    [Test] procedure VerifyRaisesNotAvailableUntilBindingShips;
    /// <summary>
    ///   Constructor rejects unknown algorithm.
    /// </summary>
    [Test] procedure ConstructorRejectsUnknownAlgorithm;
    /// <summary>
    ///   Constructor rejects empty public key.
    /// </summary>
    [Test] procedure ConstructorRejectsEmptyPublicKey;
    /// <summary>
    ///   Algorithm name matches enum.
    /// </summary>
    [Test] procedure AlgorithmNameMatchesEnum;
  end;

implementation

uses
  System.SysUtils, OBD.ECU.Signature, OBD.ECU.Signature.PQC;

//------------------------------------------------------------------------------
// ENVELOPE ROUND TRIPS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// ENVELOPE WITH EMPTY KEY ID ROUND TRIPS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// ENVELOPE TRUNCATED AT SIG LEN RAISES
//------------------------------------------------------------------------------
procedure TPQCSignatureTests.EnvelopeTruncatedAtSigLenRaises;
var
  Bytes: TBytes;
begin
  Bytes := TBytes.Create($02, $00, $00, $00, $00); // missing one sig-len byte
  Assert.WillRaise(
    procedure begin DecodePQCEnvelope(Bytes); end,
    EOBDPQCSignature);
end;

//------------------------------------------------------------------------------
// ENVELOPE TRUNCATED AT SIGNATURE RAISES
//------------------------------------------------------------------------------
procedure TPQCSignatureTests.EnvelopeTruncatedAtSignatureRaises;
var
  Bytes: TBytes;
begin
  // alg=2, keylen=0, siglen=4, but only 2 sig bytes follow
  Bytes := TBytes.Create($02, $00, $00, $00, $00, $04, $AA, $BB);
  Assert.WillRaise(
    procedure begin DecodePQCEnvelope(Bytes); end,
    EOBDPQCSignature);
end;

//------------------------------------------------------------------------------
// ENVELOPE TOO SHORT RAISES
//------------------------------------------------------------------------------
procedure TPQCSignatureTests.EnvelopeTooShortRaises;
begin
  Assert.WillRaise(
    procedure begin DecodePQCEnvelope(TBytes.Create($00, $00, $00)); end,
    EOBDPQCSignature);
end;

//------------------------------------------------------------------------------
// VERIFY ALGORITHM MISMATCH RAISES
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// VERIFY RAISES NOT AVAILABLE UNTIL BINDING SHIPS
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// CONSTRUCTOR REJECTS UNKNOWN ALGORITHM
//------------------------------------------------------------------------------
procedure TPQCSignatureTests.ConstructorRejectsUnknownAlgorithm;
begin
  Assert.WillRaise(
    procedure
    var
      V: IFirmwareSignatureVerifier;
    begin
      V := TOBDPQCSignatureVerifier.Create(pqcUnknown, TBytes.Create($01));
    end,
    EOBDPQCSignature);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR REJECTS EMPTY PUBLIC KEY
//------------------------------------------------------------------------------
procedure TPQCSignatureTests.ConstructorRejectsEmptyPublicKey;
begin
  Assert.WillRaise(
    procedure
    var
      V: IFirmwareSignatureVerifier;
    begin
      V := TOBDPQCSignatureVerifier.Create(pqcMlDsa65, nil);
    end,
    EOBDPQCSignature);
end;

//------------------------------------------------------------------------------
// ALGORITHM NAME MATCHES ENUM
//------------------------------------------------------------------------------
procedure TPQCSignatureTests.AlgorithmNameMatchesEnum;
begin
  Assert.AreEqual('ML-DSA-65', PQCAlgorithmName(pqcMlDsa65));
  Assert.AreEqual('SLH-DSA-SHA2-128s', PQCAlgorithmName(pqcSlhDsaSha2128s));
  Assert.AreEqual('PQC-UNKNOWN', PQCAlgorithmName(pqcUnknown));
end;

initialization
  TDUnitX.RegisterTestFixture(TPQCSignatureTests);

end.
