//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.SecOC.pas
// CONTENTS       : Tests for OBD.Protocol.SecOC
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Protocol.SecOC;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSecOCTests = class
  public
    /// <summary>Profile3 hmac round trip verifies.</summary>
    [Test] procedure Profile3HmacRoundTripVerifies;
    /// <summary>Freshness value changes mac.</summary>
    [Test] procedure FreshnessValueChangesMac;
    /// <summary>Payload flip fails verification.</summary>
    [Test] procedure PayloadFlipFailsVerification;
    /// <summary>Wrong key fails verification.</summary>
    [Test] procedure WrongKeyFailsVerification;
    /// <summary>Configurable truncation length.</summary>
    [Test] procedure ConfigurableTruncationLength;
    /// <summary>Profile1 raises until cmac binding ships.</summary>
    [Test] procedure Profile1RaisesUntilCmacBindingShips;
    /// <summary>Encode p d u layout matches spec.</summary>
    [Test] procedure EncodePDULayoutMatchesSpec;
    /// <summary>Empty key raises.</summary>
    [Test] procedure EmptyKeyRaises;
  end;

implementation

uses
  System.SysUtils, OBD.Protocol.SecOC;

function MakeProfile3Ctx(FV: UInt64; const Key: TBytes): TSecOCContext;
begin
  Result := Default(TSecOCContext);
  Result.Profile := secocProfile3;
  Result.KeyId := $0042;
  Result.Key := Key;
  Result.FreshnessValue := FV;
  Result.AuthenticatorBits := 32;
end;

procedure TSecOCTests.Profile3HmacRoundTripVerifies;
var
  Ctx: TSecOCContext;
  Payload, Mac: TBytes;
begin
  Ctx := MakeProfile3Ctx(1, TBytes.Create($AA, $BB, $CC, $DD));
  Payload := TBytes.Create($01, $02, $03, $04);
  Mac := SecOCComputeAuthenticator(Ctx, Payload);
  Assert.AreEqual(4, Length(Mac));
  Assert.IsTrue(SecOCVerifyAuthenticator(Ctx, Payload, Mac));
end;

procedure TSecOCTests.FreshnessValueChangesMac;
var
  Key, Payload, Mac1, Mac2: TBytes;
  C1, C2: TSecOCContext;
begin
  Key := TBytes.Create($AA, $BB, $CC, $DD);
  Payload := TBytes.Create($01, $02);
  C1 := MakeProfile3Ctx(1, Key);
  C2 := MakeProfile3Ctx(2, Key);
  Mac1 := SecOCComputeAuthenticator(C1, Payload);
  Mac2 := SecOCComputeAuthenticator(C2, Payload);
  Assert.IsFalse((Length(Mac1) = Length(Mac2))
    and CompareMem(@Mac1[0], @Mac2[0], Length(Mac1)),
    'Different FV must produce different MAC');
end;

procedure TSecOCTests.PayloadFlipFailsVerification;
var
  Ctx: TSecOCContext;
  Payload, Mac, Tampered: TBytes;
begin
  Ctx := MakeProfile3Ctx(1, TBytes.Create($AA, $BB));
  Payload := TBytes.Create($10, $20, $30);
  Mac := SecOCComputeAuthenticator(Ctx, Payload);
  Tampered := Copy(Payload);
  Tampered[1] := Tampered[1] xor $01;
  Assert.IsFalse(SecOCVerifyAuthenticator(Ctx, Tampered, Mac));
end;

procedure TSecOCTests.WrongKeyFailsVerification;
var
  Payload, Mac: TBytes;
  C1, C2: TSecOCContext;
begin
  C1 := MakeProfile3Ctx(1, TBytes.Create($AA, $BB));
  C2 := MakeProfile3Ctx(1, TBytes.Create($CC, $DD));
  Payload := TBytes.Create($01);
  Mac := SecOCComputeAuthenticator(C1, Payload);
  Assert.IsFalse(SecOCVerifyAuthenticator(C2, Payload, Mac));
end;

procedure TSecOCTests.ConfigurableTruncationLength;
var
  Ctx: TSecOCContext;
  Mac24, Mac64: TBytes;
begin
  Ctx := MakeProfile3Ctx(1, TBytes.Create($01, $02, $03));
  Ctx.AuthenticatorBits := 24;
  Mac24 := SecOCComputeAuthenticator(Ctx, TBytes.Create($AA));
  Ctx.AuthenticatorBits := 64;
  Mac64 := SecOCComputeAuthenticator(Ctx, TBytes.Create($AA));
  Assert.AreEqual(3, Length(Mac24));
  Assert.AreEqual(8, Length(Mac64));
  // MAC24 must equal the first 3 bytes of MAC64 (deterministic
  // truncation contract).
  Assert.IsTrue(CompareMem(@Mac24[0], @Mac64[0], 3));
end;

procedure TSecOCTests.Profile1RaisesUntilCmacBindingShips;
var
  Ctx: TSecOCContext;
begin
  Ctx := Default(TSecOCContext);
  Ctx.Profile := secocProfile1;
  Ctx.Key := TBytes.Create($00, $01, $02, $03, $04, $05, $06, $07,
                           $08, $09, $0A, $0B, $0C, $0D, $0E, $0F);
  Ctx.AuthenticatorBits := 24;
  Assert.WillRaise(
    procedure
    begin SecOCComputeAuthenticator(Ctx, TBytes.Create($AA)); end,
    EOBDSecOCAlgorithmNotAvailable);
end;

procedure TSecOCTests.EncodePDULayoutMatchesSpec;
var
  Ctx: TSecOCContext;
  PDU, Payload, Mac: TBytes;
begin
  Ctx := MakeProfile3Ctx($AABBCCDD, TBytes.Create($01));
  Payload := TBytes.Create($DE, $AD);
  Mac := TBytes.Create($CA, $FE);
  PDU := SecOCEncodePDU(Ctx, Payload, Mac);
  // Layout: KeyId(2) + FV(8 for profile 3) + Payload(2) + MAC(2) = 14
  Assert.AreEqual(14, Length(PDU));
  Assert.AreEqual($00, Integer(PDU[0]));
  Assert.AreEqual($42, Integer(PDU[1]));
  Assert.AreEqual($AA, Integer(PDU[6])); // first FV high byte
  Assert.AreEqual($DE, Integer(PDU[10])); // payload starts after FV
  Assert.AreEqual($CA, Integer(PDU[12])); // MAC follows payload
end;

procedure TSecOCTests.EmptyKeyRaises;
var
  Ctx: TSecOCContext;
begin
  Ctx := Default(TSecOCContext);
  Ctx.Profile := secocProfile3;
  Ctx.AuthenticatorBits := 32;
  Assert.WillRaise(
    procedure begin SecOCComputeAuthenticator(Ctx, TBytes.Create($00)); end,
    EOBDSecOC);
end;

initialization
  TDUnitX.RegisterTestFixture(TSecOCTests);

end.
