//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Signature.OpenSSL
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : These tests run only when libcrypto is on the
//                  build/test runner's PATH. Without OpenSSL the
//                  fixture tests skip; the availability + error tests
//                  still execute against whatever stub state.
//------------------------------------------------------------------------------
unit Tests.ECU.Signature.OpenSSL;

{$IFDEF MSWINDOWS}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TOpenSSLVerifierTests = class
  public
    [Test] procedure NotAvailable_ConstructRaises_When_LibcryptoMissing;
    [Test] procedure RSA_VerifiesKnownGoodSignature;
    [Test] procedure RSA_RejectsTamperedFirmware;
    [Test] procedure ECDSA_VerifiesKnownGoodSignature;
    [Test] procedure ECDSA_RejectsTamperedFirmware;
    [Test] procedure Construct_RejectsGarbageDer;
  end;

implementation

uses
  System.SysUtils, OBD.ECU.Signature.OpenSSL;

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

procedure TOpenSSLVerifierTests.NotAvailable_ConstructRaises_When_LibcryptoMissing;
begin
  // The negative branch only fires on a runner without OpenSSL on the
  // PATH. When libcrypto IS present, the constructor succeeds — that's
  // what every other test in this fixture checks. Either outcome is
  // valid, so we just exercise the dispatch.
  if not OpenSSLAvailable then
    Assert.WillRaise(
      procedure begin
        TOBDOpenSSLVerifier.Create(PubKeyRSA).Free;
      end,
      EOBDOpenSSLNotAvailable)
  else
    Assert.Pass('libcrypto is on PATH — negative branch skipped');
end;

procedure TOpenSSLVerifierTests.RSA_VerifiesKnownGoodSignature;
var V: TOBDOpenSSLVerifier;
begin
  if not OpenSSLAvailable then
  begin Assert.Pass('libcrypto not on PATH'); Exit; end;
  V := TOBDOpenSSLVerifier.Create(PubKeyRSA);
  try
    Assert.IsTrue(V.AlgorithmName.Contains('RSA'));
    Assert.IsTrue(V.Verify(MessageBytes, SigRSA));
  finally
    V.Free;
  end;
end;

procedure TOpenSSLVerifierTests.RSA_RejectsTamperedFirmware;
var
  V: TOBDOpenSSLVerifier;
  Tampered: TBytes;
begin
  if not OpenSSLAvailable then
  begin Assert.Pass('libcrypto not on PATH'); Exit; end;
  V := TOBDOpenSSLVerifier.Create(PubKeyRSA);
  try
    Tampered := TEncoding.ASCII.GetBytes('hello WORLD');
    Assert.IsFalse(V.Verify(Tampered, SigRSA));
  finally
    V.Free;
  end;
end;

procedure TOpenSSLVerifierTests.ECDSA_VerifiesKnownGoodSignature;
var V: TOBDOpenSSLVerifier;
begin
  if not OpenSSLAvailable then
  begin Assert.Pass('libcrypto not on PATH'); Exit; end;
  V := TOBDOpenSSLVerifier.Create(PubKeyEC);
  try
    Assert.IsTrue(V.AlgorithmName.Contains('ECDSA'));
    Assert.IsTrue(V.Verify(MessageBytes, SigEC));
  finally
    V.Free;
  end;
end;

procedure TOpenSSLVerifierTests.ECDSA_RejectsTamperedFirmware;
var
  V: TOBDOpenSSLVerifier;
  Tampered: TBytes;
begin
  if not OpenSSLAvailable then
  begin Assert.Pass('libcrypto not on PATH'); Exit; end;
  V := TOBDOpenSSLVerifier.Create(PubKeyEC);
  try
    Tampered := TEncoding.ASCII.GetBytes('hello WORLD');
    Assert.IsFalse(V.Verify(Tampered, SigEC));
  finally
    V.Free;
  end;
end;

procedure TOpenSSLVerifierTests.Construct_RejectsGarbageDer;
var Garbage: TBytes;
begin
  if not OpenSSLAvailable then
  begin Assert.Pass('libcrypto not on PATH'); Exit; end;
  Garbage := TBytes.Create($AA, $BB, $CC, $DD, $EE);
  Assert.WillRaise(
    procedure begin TOBDOpenSSLVerifier.Create(Garbage).Free; end,
    EOBDOpenSSLError);
end;

initialization
  TDUnitX.RegisterTestFixture(TOpenSSLVerifierTests);

{$ELSE}
interface
implementation
{$ENDIF}

end.
