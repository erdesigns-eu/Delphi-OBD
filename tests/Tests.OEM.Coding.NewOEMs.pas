//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Coding.NewOEMs
// CONTENTS       : Round-trip + accessor tests for Toyota, Honda, HMG,
//                : Stellantis coding wrappers introduced in v3.80 / 4.4.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.Coding.NewOEMs;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TNewOEMCodingTests = class
  public
    [Test] procedure Toyota_HexRoundTrip;
    [Test] procedure Toyota_BitFlipPersists;
    [Test] procedure Honda_HexRoundTrip;
    [Test] procedure HMG_OutOfRangeByteRaises;
    [Test] procedure Stellantis_BitAndByteAccess;
    [Test] procedure Stellantis_ComputeChecksumRaisesForGap;
    [Test] procedure Stellantis_SetChecksumWritesTwoBytes;
    [Test] procedure ZeroLengthConstructionRaises;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM.Coding,
  OBD.OEM.Coding.Toyota,
  OBD.OEM.Coding.Honda,
  OBD.OEM.Coding.HMG,
  OBD.OEM.Coding.Stellantis;

procedure TNewOEMCodingTests.Toyota_HexRoundTrip;
var C: TOBDToyotaCustomize;
begin
  C := TOBDToyotaCustomize.CreateFromHex('0102030405');
  try
    Assert.AreEqual('0102030405', C.ToHex);
    Assert.AreEqual(5, C.ByteCount);
  finally C.Free; end;
end;

procedure TNewOEMCodingTests.Toyota_BitFlipPersists;
var C: TOBDToyotaCustomize;
begin
  C := TOBDToyotaCustomize.Create(1);
  try
    Assert.IsFalse(C.GetBit(0, 3));
    C.SetBit(0, 3, True);
    Assert.IsTrue(C.GetBit(0, 3));
    Assert.AreEqual($08, Integer(C.GetByte(0)));
  finally C.Free; end;
end;

procedure TNewOEMCodingTests.Honda_HexRoundTrip;
var H: TOBDHondaOptionByte;
begin
  H := TOBDHondaOptionByte.CreateFromHex('AABBCC');
  try
    Assert.AreEqual('AABBCC', H.ToHex);
    Assert.AreEqual($BB, Integer(H.GetByte(1)));
  finally H.Free; end;
end;

procedure TNewOEMCodingTests.HMG_OutOfRangeByteRaises;
var V: TOBDHMGVariantCoding;
begin
  V := TOBDHMGVariantCoding.Create(2);
  try
    Assert.WillRaise(
      procedure begin V.SetByte(99, $FF); end,
      EOBDCodingError);
  finally V.Free; end;
end;

procedure TNewOEMCodingTests.Stellantis_BitAndByteAccess;
var P: TOBDStellantisProxi;
begin
  P := TOBDStellantisProxi.Create(4);
  try
    P.SetByte(2, $80);
    Assert.IsTrue(P.GetBit(2, 7));
    Assert.IsFalse(P.GetBit(2, 6));
  finally P.Free; end;
end;

procedure TNewOEMCodingTests.Stellantis_ComputeChecksumRaisesForGap;
var P: TOBDStellantisProxi;
begin
  P := TOBDStellantisProxi.Create(4);
  try
    Assert.WillRaise(
      procedure begin P.ComputeChecksum; end,
      EOBDStellantisProxi);
  finally P.Free; end;
end;

procedure TNewOEMCodingTests.Stellantis_SetChecksumWritesTwoBytes;
var P: TOBDStellantisProxi;
begin
  P := TOBDStellantisProxi.Create(4);
  try
    P.SetChecksum($ABCD, 2);
    Assert.AreEqual($AB, Integer(P.GetByte(2)));
    Assert.AreEqual($CD, Integer(P.GetByte(3)));
  finally P.Free; end;
end;

procedure TNewOEMCodingTests.ZeroLengthConstructionRaises;
begin
  Assert.WillRaise(
    procedure
    var C: TOBDToyotaCustomize;
    begin C := TOBDToyotaCustomize.Create(0); C.Free; end,
    EOBDCodingError);
end;

initialization
  TDUnitX.RegisterTestFixture(TNewOEMCodingTests);

end.
