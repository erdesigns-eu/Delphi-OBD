//------------------------------------------------------------------------------
// UNIT           : Tests.Tachograph.Signature.pas
// CONTENTS       : Tests for OBD.Tachograph.Signature
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Tachograph.Signature;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTachographSignatureTests = class
  public
    /// <summary>
    ///   Parses empty file as zero blocks.
    /// </summary>
    [Test] procedure ParsesEmptyFileAsZeroBlocks;
    /// <summary>
    ///   Parses single t l v.
    /// </summary>
    [Test] procedure ParsesSingleTLV;
    /// <summary>
    ///   Truncated declared length raises.
    /// </summary>
    [Test] procedure TruncatedDeclaredLengthRaises;
    /// <summary>
    ///   Verify chain succeeds when verifiers pass.
    /// </summary>
    [Test] procedure VerifyChainSucceedsWhenVerifiersPass;
    /// <summary>
    ///   Verify chain fails when signature block missing.
    /// </summary>
    [Test] procedure VerifyChainFailsWhenSignatureBlockMissing;
    /// <summary>
    ///   Verify chain fails when verifier returns false.
    /// </summary>
    [Test] procedure VerifyChainFailsWhenVerifierReturnsFalse;
    /// <summary>
    ///   Verify chain fails when verifier not configured.
    /// </summary>
    [Test] procedure VerifyChainFailsWhenVerifierNotConfigured;
  end;

implementation

uses
  System.SysUtils,
  OBD.ECU.Signature,
  OBD.Tachograph.Signature;

type
  TConfigurableVerifier = class(TInterfacedObject, IFirmwareSignatureVerifier)
  private
    FAccept: Boolean;
  public
    constructor Create(Accept: Boolean);
    function AlgorithmName: string;
    function Verify(const Firmware, Signature: TBytes): Boolean;
  end;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TConfigurableVerifier.Create(Accept: Boolean);
begin
  inherited Create;
  FAccept := Accept;
end;

//------------------------------------------------------------------------------
// ALGORITHM NAME
//------------------------------------------------------------------------------
function TConfigurableVerifier.AlgorithmName: string;
begin
  if FAccept then Result := 'TEST-ACCEPT' else Result := 'TEST-REJECT';
end;

//------------------------------------------------------------------------------
// VERIFY
//------------------------------------------------------------------------------
function TConfigurableVerifier.Verify(const Firmware, Signature: TBytes): Boolean;
begin
  Result := FAccept;
end;

//------------------------------------------------------------------------------
// MAKE BLOCK
//------------------------------------------------------------------------------
function MakeBlock(TagHi, TagLo: Byte; const Body: TBytes): TBytes;
var
  Out_: TBytes;
begin
  SetLength(Out_, 4 + Length(Body));
  Out_[0] := TagHi;
  Out_[1] := TagLo;
  Out_[2] := Byte(Length(Body) shr 8);
  Out_[3] := Byte(Length(Body) and $FF);
  if Length(Body) > 0 then
    Move(Body[0], Out_[4], Length(Body));
  Result := Out_;
end;

//------------------------------------------------------------------------------
// CONCAT BYTES
//------------------------------------------------------------------------------
function ConcatBytes(const Parts: array of TBytes): TBytes;
var
  Total, I, Off: Integer;
begin
  Total := 0;
  for I := 0 to High(Parts) do Inc(Total, Length(Parts[I]));
  SetLength(Result, Total);
  Off := 0;
  for I := 0 to High(Parts) do
    if Length(Parts[I]) > 0 then
    begin
      Move(Parts[I][0], Result[Off], Length(Parts[I]));
      Inc(Off, Length(Parts[I]));
    end;
end;

//------------------------------------------------------------------------------
// PARSES EMPTY FILE AS ZERO BLOCKS
//------------------------------------------------------------------------------
procedure TTachographSignatureTests.ParsesEmptyFileAsZeroBlocks;
var
  Checker: TOBDTachographSignatureChecker;
  Blocks: TArray<TDDDBlock>;
begin
  Checker := TOBDTachographSignatureChecker.Create;
  try
    Blocks := Checker.ParseBlocks(nil);
    Assert.AreEqual(0, Length(Blocks));
  finally
    Checker.Free;
  end;
end;

//------------------------------------------------------------------------------
// PARSES SINGLE TLV
//------------------------------------------------------------------------------
procedure TTachographSignatureTests.ParsesSingleTLV;
var
  Checker: TOBDTachographSignatureChecker;
  Blocks: TArray<TDDDBlock>;
  Body: TBytes;
begin
  Checker := TOBDTachographSignatureChecker.Create;
  try
    Body := TBytes.Create($AA, $BB, $CC);
    Blocks := Checker.ParseBlocks(MakeBlock($05, $01, Body));
    Assert.AreEqual(1, Length(Blocks));
    Assert.AreEqual(Word($0501), Blocks[0].Tag);
    Assert.AreEqual(3, Blocks[0].Length);
    Assert.AreEqual($AA, Integer(Blocks[0].Data[0]));
  finally
    Checker.Free;
  end;
end;

//------------------------------------------------------------------------------
// TRUNCATED DECLARED LENGTH RAISES
//------------------------------------------------------------------------------
procedure TTachographSignatureTests.TruncatedDeclaredLengthRaises;
var
  Checker: TOBDTachographSignatureChecker;
  Bad: TBytes;
begin
  Checker := TOBDTachographSignatureChecker.Create;
  try
    // tag=0x0501, declared len=0x0010, but no body bytes follow
    Bad := TBytes.Create($05, $01, $00, $10);
    Assert.WillRaise(
      procedure begin Checker.ParseBlocks(Bad); end,
      EOBDTachographSignature);
  finally
    Checker.Free;
  end;
end;

//------------------------------------------------------------------------------
// VERIFY CHAIN SUCCEEDS WHEN VERIFIERS PASS
//------------------------------------------------------------------------------
procedure TTachographSignatureTests.VerifyChainSucceedsWhenVerifiersPass;
var
  Checker: TOBDTachographSignatureChecker;
  File_: TBytes;
  R: TDDDChainResult;
begin
  File_ := ConcatBytes([
    MakeBlock($05, $01, TBytes.Create($AA)),  // overview
    MakeBlock($05, $0E, TBytes.Create($01)),  // signature
    MakeBlock($05, $04, TBytes.Create($BB)),  // activities
    MakeBlock($05, $0E, TBytes.Create($02))]); // signature

  Checker := TOBDTachographSignatureChecker.Create;
  try
    Checker.SetCardVerifier(TConfigurableVerifier.Create(True));
    Checker.SetVUVerifier(TConfigurableVerifier.Create(True));
    R := Checker.VerifyChain(File_);
    Assert.IsTrue(R.Verified, R.Reason);
    Assert.AreEqual(2, R.SignaturesVerified);
  finally
    Checker.Free;
  end;
end;

//------------------------------------------------------------------------------
// VERIFY CHAIN FAILS WHEN SIGNATURE BLOCK MISSING
//------------------------------------------------------------------------------
procedure TTachographSignatureTests.VerifyChainFailsWhenSignatureBlockMissing;
var
  Checker: TOBDTachographSignatureChecker;
  File_: TBytes;
  R: TDDDChainResult;
begin
  File_ := ConcatBytes([
    MakeBlock($05, $01, TBytes.Create($AA)),    // overview
    MakeBlock($05, $04, TBytes.Create($BB))]);  // activities — no signature between
  Checker := TOBDTachographSignatureChecker.Create;
  try
    Checker.SetCardVerifier(TConfigurableVerifier.Create(True));
    R := Checker.VerifyChain(File_);
    Assert.IsFalse(R.Verified);
    Assert.IsTrue(R.Reason.Contains('not followed by a signature'));
  finally
    Checker.Free;
  end;
end;

//------------------------------------------------------------------------------
// VERIFY CHAIN FAILS WHEN VERIFIER RETURNS FALSE
//------------------------------------------------------------------------------
procedure TTachographSignatureTests.VerifyChainFailsWhenVerifierReturnsFalse;
var
  Checker: TOBDTachographSignatureChecker;
  File_: TBytes;
  R: TDDDChainResult;
begin
  File_ := ConcatBytes([
    MakeBlock($05, $01, TBytes.Create($AA)),
    MakeBlock($05, $0E, TBytes.Create($DE, $AD))]);
  Checker := TOBDTachographSignatureChecker.Create;
  try
    Checker.SetCardVerifier(TConfigurableVerifier.Create(False));
    R := Checker.VerifyChain(File_);
    Assert.IsFalse(R.Verified);
    Assert.IsTrue(R.Reason.Contains('failed verification'));
    Assert.AreEqual(0, R.FirstFailureBlockIndex);
  finally
    Checker.Free;
  end;
end;

//------------------------------------------------------------------------------
// VERIFY CHAIN FAILS WHEN VERIFIER NOT CONFIGURED
//------------------------------------------------------------------------------
procedure TTachographSignatureTests.VerifyChainFailsWhenVerifierNotConfigured;
var
  Checker: TOBDTachographSignatureChecker;
  File_: TBytes;
  R: TDDDChainResult;
begin
  File_ := ConcatBytes([
    MakeBlock($05, $01, TBytes.Create($AA)),
    MakeBlock($05, $0E, TBytes.Create($01))]);
  Checker := TOBDTachographSignatureChecker.Create;
  try
    R := Checker.VerifyChain(File_);
    Assert.IsFalse(R.Verified);
    Assert.IsTrue(R.Reason.Contains('No verifier'));
  finally
    Checker.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTachographSignatureTests);

end.
