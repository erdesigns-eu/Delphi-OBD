//------------------------------------------------------------------------------
// UNIT           : Tests.ECU.Signature
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.ECU.Signature;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSignatureTests = class
  public
    /// <summary>
    ///   Sha256  accepts known gold hash.
    /// </summary>
    [Test] procedure Sha256_AcceptsKnownGoldHash;
    /// <summary>
    ///   Sha256  rejects tampered firmware.
    /// </summary>
    [Test] procedure Sha256_RejectsTamperedFirmware;
    /// <summary>
    ///   Sha256  length mismatch rejected.
    /// </summary>
    [Test] procedure Sha256_LengthMismatchRejected;
    /// <summary>
    ///   Permissive  accepts anything.
    /// </summary>
    [Test] procedure Permissive_AcceptsAnything;
    /// <summary>
    ///   Compute sha256  empty has known value.
    /// </summary>
    [Test] procedure ComputeSha256_EmptyHasKnownValue;
  end;

implementation

uses
  System.SysUtils, OBD.ECU.Signature;

//------------------------------------------------------------------------------
// HEX TO BYTES
//------------------------------------------------------------------------------
function HexToBytes(const Hex: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(Hex) div 2);
  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(Hex, I * 2 + 1, 2));
end;

//------------------------------------------------------------------------------
// COMPUTE SHA256_EMPTY HAS KNOWN VALUE
//------------------------------------------------------------------------------
procedure TSignatureTests.ComputeSha256_EmptyHasKnownValue;
var
  Empty: TBytes;
  Hash: TBytes;
  Hex: string;
  I: Integer;
begin
  // SHA-256 of the empty string:
  // e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
  SetLength(Empty, 0);
  Hash := ComputeSha256(Empty);
  Hex := '';
  for I := 0 to High(Hash) do
    Hex := Hex + IntToHex(Hash[I], 2);
  Assert.AreEqual(
    'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855',
    Hex);
end;

//------------------------------------------------------------------------------
// SHA256_ACCEPTS KNOWN GOLD HASH
//------------------------------------------------------------------------------
procedure TSignatureTests.Sha256_AcceptsKnownGoldHash;
var
  Verifier: IFirmwareSignatureVerifier;
  Firmware, Signature: TBytes;
begin
  Firmware := TEncoding.ASCII.GetBytes('hello');
  // Pre-computed sha256 of "hello":
  Signature := HexToBytes(
    '2CF24DBA5FB0A30E26E83B2AC5B9E29E1B161E5C1FA7425E73043362938B9824');

  Verifier := TOBDSha256SignatureVerifier.Create;
  Assert.IsTrue(Verifier.Verify(Firmware, Signature));
end;

//------------------------------------------------------------------------------
// SHA256_REJECTS TAMPERED FIRMWARE
//------------------------------------------------------------------------------
procedure TSignatureTests.Sha256_RejectsTamperedFirmware;
var
  Verifier: IFirmwareSignatureVerifier;
  Firmware, Signature: TBytes;
begin
  Firmware := TEncoding.ASCII.GetBytes('hello WORLD');  // mutated
  Signature := HexToBytes(
    '2CF24DBA5FB0A30E26E83B2AC5B9E29E1B161E5C1FA7425E73043362938B9824');
  Verifier := TOBDSha256SignatureVerifier.Create;
  Assert.IsFalse(Verifier.Verify(Firmware, Signature));
end;

//------------------------------------------------------------------------------
// SHA256_LENGTH MISMATCH REJECTED
//------------------------------------------------------------------------------
procedure TSignatureTests.Sha256_LengthMismatchRejected;
var
  Verifier: IFirmwareSignatureVerifier;
  Firmware, Short: TBytes;
begin
  Firmware := TEncoding.ASCII.GetBytes('hello');
  Short := HexToBytes('2CF24DBA');  // truncated
  Verifier := TOBDSha256SignatureVerifier.Create;
  Assert.IsFalse(Verifier.Verify(Firmware, Short));
end;

//------------------------------------------------------------------------------
// PERMISSIVE_ACCEPTS ANYTHING
//------------------------------------------------------------------------------
procedure TSignatureTests.Permissive_AcceptsAnything;
var
  Verifier: IFirmwareSignatureVerifier;
  Firmware, Bogus: TBytes;
begin
  Firmware := TEncoding.ASCII.GetBytes('whatever');
  Bogus := TEncoding.ASCII.GetBytes('not actually a hash');
  Verifier := TOBDPermissiveSignatureVerifier.Create;
  Assert.IsTrue(Verifier.Verify(Firmware, Bogus),
    'Permissive verifier must accept anything (development only)');
end;

initialization
  TDUnitX.RegisterTestFixture(TSignatureTests);

end.
