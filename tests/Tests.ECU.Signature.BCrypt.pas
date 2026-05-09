//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Signature.BCrypt
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Exercises the BCrypt-backed verifier against real
//                  RSA + ECDSA test vectors generated with OpenSSL.
//                  Requires Windows + crypt32.dll + bcrypt.dll (always
//                  present on supported targets). The lint-only CI job
//                  on Ubuntu skips this unit at compile time via
//                  the {$IFDEF MSWINDOWS} guard.
//------------------------------------------------------------------------------
unit Tests.ECU.Signature.BCrypt;

{$IFDEF MSWINDOWS}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TBCryptVerifierTests = class
  public
    /// <summary>R s a  construct  recognises algorithm.</summary>
    [Test] procedure RSA_Construct_RecognisesAlgorithm;
    /// <summary>R s a  verify  accepts known good signature.</summary>
    [Test] procedure RSA_Verify_AcceptsKnownGoodSignature;
    /// <summary>R s a  verify  rejects tampered firmware.</summary>
    [Test] procedure RSA_Verify_RejectsTamperedFirmware;
    /// <summary>R s a  verify  rejects tampered signature.</summary>
    [Test] procedure RSA_Verify_RejectsTamperedSignature;
    /// <summary>R s a  verify  rejects empty firmware.</summary>
    [Test] procedure RSA_Verify_RejectsEmptyFirmware;
    /// <summary>R s a  verify  rejects empty signature.</summary>
    [Test] procedure RSA_Verify_RejectsEmptySignature;

    /// <summary>E c d s a  construct  recognises algorithm.</summary>
    [Test] procedure ECDSA_Construct_RecognisesAlgorithm;
    /// <summary>E c d s a  verify  accepts known good signature.</summary>
    [Test] procedure ECDSA_Verify_AcceptsKnownGoodSignature;
    /// <summary>E c d s a  verify  rejects tampered firmware.</summary>
    [Test] procedure ECDSA_Verify_RejectsTamperedFirmware;

    /// <summary>Construct  rejects empty der.</summary>
    [Test] procedure Construct_RejectsEmptyDer;
    /// <summary>Construct  rejects garbage der.</summary>
    [Test] procedure Construct_RejectsGarbageDer;
  end;

implementation

uses
  System.SysUtils, OBD.ECU.Signature.BCrypt;

const
{$I 'fixtures\test-fixtures.inc'}

function PubKeyRSA: TBytes;
begin
  SetLength(Result, TEST_RSA_PUB_DER_LEN);
  Move(TEST_RSA_PUB_DER[0], Result[0], TEST_RSA_PUB_DER_LEN);
end;

function PubKeyEC: TBytes;
begin
  SetLength(Result, TEST_EC_PUB_DER_LEN);
  Move(TEST_EC_PUB_DER[0], Result[0], TEST_EC_PUB_DER_LEN);
end;

function SigRSA: TBytes;
begin
  SetLength(Result, TEST_RSA_SIG_PKCS1_LEN);
  Move(TEST_RSA_SIG_PKCS1[0], Result[0], TEST_RSA_SIG_PKCS1_LEN);
end;

function SigEC: TBytes;
begin
  SetLength(Result, TEST_EC_SIG_LEN);
  Move(TEST_EC_SIG[0], Result[0], TEST_EC_SIG_LEN);
end;

function MessageBytes: TBytes;
begin
  Result := TEncoding.ASCII.GetBytes(TEST_MESSAGE_TEXT);
end;

procedure TBCryptVerifierTests.RSA_Construct_RecognisesAlgorithm;
var V: TOBDBCryptVerifier;
begin
  V := TOBDBCryptVerifier.Create(PubKeyRSA);
  try
    Assert.IsTrue(V.AlgorithmName.Contains('RSA'),
      'AlgorithmName should mention RSA, got ' + V.AlgorithmName);
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.RSA_Verify_AcceptsKnownGoodSignature;
var V: TOBDBCryptVerifier;
begin
  V := TOBDBCryptVerifier.Create(PubKeyRSA);
  try
    Assert.IsTrue(V.Verify(MessageBytes, SigRSA),
      'Pre-baked OpenSSL RSA-PKCS1-SHA256 signature must verify');
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.RSA_Verify_RejectsTamperedFirmware;
var
  V: TOBDBCryptVerifier;
  Tampered: TBytes;
begin
  V := TOBDBCryptVerifier.Create(PubKeyRSA);
  try
    Tampered := TEncoding.ASCII.GetBytes('hello WORLD');
    Assert.IsFalse(V.Verify(Tampered, SigRSA),
      'Modified firmware must NOT verify against original signature');
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.RSA_Verify_RejectsTamperedSignature;
var
  V: TOBDBCryptVerifier;
  BadSig: TBytes;
begin
  V := TOBDBCryptVerifier.Create(PubKeyRSA);
  try
    BadSig := SigRSA;
    BadSig[Length(BadSig) div 2] := BadSig[Length(BadSig) div 2] xor $FF;
    Assert.IsFalse(V.Verify(MessageBytes, BadSig),
      'Bit-flipped signature must NOT verify');
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.RSA_Verify_RejectsEmptyFirmware;
var V: TOBDBCryptVerifier; Empty: TBytes;
begin
  V := TOBDBCryptVerifier.Create(PubKeyRSA);
  try
    SetLength(Empty, 0);
    Assert.IsFalse(V.Verify(Empty, SigRSA));
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.RSA_Verify_RejectsEmptySignature;
var V: TOBDBCryptVerifier; Empty: TBytes;
begin
  V := TOBDBCryptVerifier.Create(PubKeyRSA);
  try
    SetLength(Empty, 0);
    Assert.IsFalse(V.Verify(MessageBytes, Empty));
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.ECDSA_Construct_RecognisesAlgorithm;
var V: TOBDBCryptVerifier;
begin
  V := TOBDBCryptVerifier.Create(PubKeyEC);
  try
    Assert.IsTrue(V.AlgorithmName.Contains('ECDSA'),
      'AlgorithmName should mention ECDSA, got ' + V.AlgorithmName);
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.ECDSA_Verify_AcceptsKnownGoodSignature;
var V: TOBDBCryptVerifier;
begin
  V := TOBDBCryptVerifier.Create(PubKeyEC);
  try
    Assert.IsTrue(V.Verify(MessageBytes, SigEC),
      'Pre-baked OpenSSL ECDSA-P256-SHA256 signature must verify');
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.ECDSA_Verify_RejectsTamperedFirmware;
var
  V: TOBDBCryptVerifier;
  Tampered: TBytes;
begin
  V := TOBDBCryptVerifier.Create(PubKeyEC);
  try
    Tampered := TEncoding.ASCII.GetBytes('hello WORLD');
    Assert.IsFalse(V.Verify(Tampered, SigEC));
  finally
    V.Free;
  end;
end;

procedure TBCryptVerifierTests.Construct_RejectsEmptyDer;
var Empty: TBytes;
begin
  SetLength(Empty, 0);
  Assert.WillRaise(
    procedure begin TOBDBCryptVerifier.Create(Empty).Free; end,
    EOBDBCryptError);
end;

procedure TBCryptVerifierTests.Construct_RejectsGarbageDer;
var Garbage: TBytes;
begin
  Garbage := TBytes.Create($AA, $BB, $CC, $DD, $EE, $FF);
  Assert.WillRaise(
    procedure begin TOBDBCryptVerifier.Create(Garbage).Free; end,
    EOBDBCryptError);
end;

initialization
  TDUnitX.RegisterTestFixture(TBCryptVerifierTests);

{$ELSE}
interface
implementation
{$ENDIF}

end.
