//------------------------------------------------------------------------------
// UNIT           : Tests.Security.Nonce
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Security.Nonce;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TNonceVaultTests = class
  public
    [Test] procedure IssueProducesUniqueValues;
    [Test] procedure IsValidTrueImmediatelyAfterIssue;
    [Test] procedure RedeemSucceedsThenRejectsReplay;
    [Test] procedure RedeemUnknownRaises;
    [Test] procedure RedeemAfterTtlRaisesExpired;
    [Test] procedure ResetClearsActiveAndUsed;
    [Test] procedure NonceLengthRespected;
    [Test] procedure ConstructRejectsZeroTtl;
    [Test] procedure ConstructRejectsTinyNonce;
  end;

implementation

uses
  System.SysUtils, System.Classes, OBD.Security.Nonce;

//------------------------------------------------------------------------------
// ISSUE PRODUCES UNIQUE VALUES
//------------------------------------------------------------------------------
procedure TNonceVaultTests.IssueProducesUniqueValues;
var
  V: TOBDNonceVault;
  N1, N2, N3: string;
begin
  V := TOBDNonceVault.Create(30, 16);
  try
    N1 := V.Issue;
    N2 := V.Issue;
    N3 := V.Issue;
    Assert.AreNotEqual(N1, N2);
    Assert.AreNotEqual(N2, N3);
    Assert.AreNotEqual(N1, N3);
    Assert.AreEqual(3, V.PendingCount);
  finally
    V.Free;
  end;
end;

//------------------------------------------------------------------------------
// IS VALID TRUE IMMEDIATELY AFTER ISSUE
//------------------------------------------------------------------------------
procedure TNonceVaultTests.IsValidTrueImmediatelyAfterIssue;
var V: TOBDNonceVault; N: string;
begin
  V := TOBDNonceVault.Create(30);
  try
    N := V.Issue;
    Assert.IsTrue(V.IsValid(N));
  finally
    V.Free;
  end;
end;

//------------------------------------------------------------------------------
// REDEEM SUCCEEDS THEN REJECTS REPLAY
//------------------------------------------------------------------------------
procedure TNonceVaultTests.RedeemSucceedsThenRejectsReplay;
var V: TOBDNonceVault; N: string;
begin
  V := TOBDNonceVault.Create(30);
  try
    N := V.Issue;
    V.Redeem(N);
    Assert.WillRaise(procedure begin V.Redeem(N); end, EOBDNonceReplay);
  finally
    V.Free;
  end;
end;

//------------------------------------------------------------------------------
// REDEEM UNKNOWN RAISES
//------------------------------------------------------------------------------
procedure TNonceVaultTests.RedeemUnknownRaises;
var
  V: TOBDNonceVault;
begin
  V := TOBDNonceVault.Create(30);
  try
    Assert.WillRaise(procedure begin V.Redeem('notarealnonce'); end,
      EOBDNonceUnknown);
  finally
    V.Free;
  end;
end;

//------------------------------------------------------------------------------
// REDEEM AFTER TTL RAISES EXPIRED
//------------------------------------------------------------------------------
procedure TNonceVaultTests.RedeemAfterTtlRaisesExpired;
var V: TOBDNonceVault; N: string;
begin
  V := TOBDNonceVault.Create(1);   // 1-second TTL
  try
    N := V.Issue;
    Sleep(1500);
    Assert.WillRaise(procedure begin V.Redeem(N); end, EOBDNonceError);
  finally
    V.Free;
  end;
end;

//------------------------------------------------------------------------------
// RESET CLEARS ACTIVE AND USED
//------------------------------------------------------------------------------
procedure TNonceVaultTests.ResetClearsActiveAndUsed;
var V: TOBDNonceVault; N: string;
begin
  V := TOBDNonceVault.Create(30);
  try
    N := V.Issue;
    V.Redeem(N);
    V.Reset;
    // After Reset, neither issued-but-unused nor previously-used nonces
    // should still be tracked.
    Assert.AreEqual(0, V.PendingCount);
    Assert.WillRaise(procedure begin V.Redeem(N); end, EOBDNonceUnknown);
  finally
    V.Free;
  end;
end;

//------------------------------------------------------------------------------
// NONCE LENGTH RESPECTED
//------------------------------------------------------------------------------
procedure TNonceVaultTests.NonceLengthRespected;
var V: TOBDNonceVault; N: string;
begin
  V := TOBDNonceVault.Create(30, 8);   // 8 bytes → 16 hex chars
  try
    N := V.Issue;
    Assert.AreEqual(16, Length(N));
  finally
    V.Free;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCT REJECTS ZERO TTL
//------------------------------------------------------------------------------
procedure TNonceVaultTests.ConstructRejectsZeroTtl;
begin
  Assert.WillRaise(
    procedure begin TOBDNonceVault.Create(0).Free; end,
    EArgumentException);
end;

//------------------------------------------------------------------------------
// CONSTRUCT REJECTS TINY NONCE
//------------------------------------------------------------------------------
procedure TNonceVaultTests.ConstructRejectsTinyNonce;
begin
  Assert.WillRaise(
    procedure begin TOBDNonceVault.Create(30, 4).Free; end,
    EArgumentException);
end;

initialization
  TDUnitX.RegisterTestFixture(TNonceVaultTests);

end.
