//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Coding.NewOEMs.pas
// CONTENTS       : Tests for Toyota/Honda/HMG/Stellantis coding wrappers
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
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
    /// <summary>
    ///   Toyota  hex round trip.
    /// </summary>
    [Test] procedure Toyota_HexRoundTrip;
    /// <summary>
    ///   Toyota  bit flip persists.
    /// </summary>
    [Test] procedure Toyota_BitFlipPersists;
    /// <summary>
    ///   Honda  hex round trip.
    /// </summary>
    [Test] procedure Honda_HexRoundTrip;
    /// <summary>
    ///   H m g  out of range byte raises.
    /// </summary>
    [Test] procedure HMG_OutOfRangeByteRaises;
    /// <summary>
    ///   Stellantis  bit and byte access.
    /// </summary>
    [Test] procedure Stellantis_BitAndByteAccess;
    /// <summary>
    ///   Stellantis  compute checksum raises for gap.
    /// </summary>
    [Test] procedure Stellantis_ComputeChecksumRaisesForGap;
    /// <summary>
    ///   Stellantis  set checksum writes two bytes.
    /// </summary>
    [Test] procedure Stellantis_SetChecksumWritesTwoBytes;
    /// <summary>
    ///   Zero length construction raises.
    /// </summary>
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

//------------------------------------------------------------------------------
// TOYOTA_HEX ROUND TRIP
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.Toyota_HexRoundTrip;
var
  C: TOBDToyotaCustomize;
begin
  C := TOBDToyotaCustomize.CreateFromHex('0102030405');
  try
    Assert.AreEqual('0102030405', C.ToHex);
    Assert.AreEqual(5, C.ByteCount);
  finally C.Free; end;
end;

//------------------------------------------------------------------------------
// TOYOTA_BIT FLIP PERSISTS
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.Toyota_BitFlipPersists;
var
  C: TOBDToyotaCustomize;
begin
  C := TOBDToyotaCustomize.Create(1);
  try
    Assert.IsFalse(C.GetBit(0, 3));
    C.SetBit(0, 3, True);
    Assert.IsTrue(C.GetBit(0, 3));
    Assert.AreEqual($08, Integer(C.GetByte(0)));
  finally C.Free; end;
end;

//------------------------------------------------------------------------------
// HONDA_HEX ROUND TRIP
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.Honda_HexRoundTrip;
var
  H: TOBDHondaOptionByte;
begin
  H := TOBDHondaOptionByte.CreateFromHex('AABBCC');
  try
    Assert.AreEqual('AABBCC', H.ToHex);
    Assert.AreEqual($BB, Integer(H.GetByte(1)));
  finally H.Free; end;
end;

//------------------------------------------------------------------------------
// HMG_OUT OF RANGE BYTE RAISES
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.HMG_OutOfRangeByteRaises;
var
  V: TOBDHMGVariantCoding;
begin
  V := TOBDHMGVariantCoding.Create(2);
  try
    Assert.WillRaise(
      procedure begin V.SetByte(99, $FF); end,
      EOBDCodingError);
  finally V.Free; end;
end;

//------------------------------------------------------------------------------
// STELLANTIS_BIT AND BYTE ACCESS
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.Stellantis_BitAndByteAccess;
var
  P: TOBDStellantisProxi;
begin
  P := TOBDStellantisProxi.Create(4);
  try
    P.SetByte(2, $80);
    Assert.IsTrue(P.GetBit(2, 7));
    Assert.IsFalse(P.GetBit(2, 6));
  finally P.Free; end;
end;

//------------------------------------------------------------------------------
// STELLANTIS_COMPUTE CHECKSUM RAISES FOR GAP
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.Stellantis_ComputeChecksumRaisesForGap;
var
  P: TOBDStellantisProxi;
begin
  P := TOBDStellantisProxi.Create(4);
  try
    Assert.WillRaise(
      procedure begin P.ComputeChecksum; end,
      EOBDStellantisProxi);
  finally P.Free; end;
end;

//------------------------------------------------------------------------------
// STELLANTIS_SET CHECKSUM WRITES TWO BYTES
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.Stellantis_SetChecksumWritesTwoBytes;
var
  P: TOBDStellantisProxi;
begin
  P := TOBDStellantisProxi.Create(4);
  try
    P.SetChecksum($ABCD, 2);
    Assert.AreEqual($AB, Integer(P.GetByte(2)));
    Assert.AreEqual($CD, Integer(P.GetByte(3)));
  finally P.Free; end;
end;

//------------------------------------------------------------------------------
// ZERO LENGTH CONSTRUCTION RAISES
//------------------------------------------------------------------------------
procedure TNewOEMCodingTests.ZeroLengthConstructionRaises;
begin
  Assert.WillRaise(
    procedure
    var
      C: TOBDToyotaCustomize;
    begin C := TOBDToyotaCustomize.Create(0); C.Free; end,
    EOBDCodingError);
end;

initialization
  TDUnitX.RegisterTestFixture(TNewOEMCodingTests);

end.
