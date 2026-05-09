//------------------------------------------------------------------------------
// UNIT           : Tests.Adapter.PassThrough.J2534v2.pas
// CONTENTS       : Tests for OBD.Adapter.PassThrough.J2534v2
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Adapter.PassThrough.J2534v2;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TJ2534v2Tests = class
  public
    /// <summary>Empty list serialises to four zero bytes.</summary>
    [Test] procedure EmptyListSerialisesToFourZeroBytes;
    /// <summary>Single entry serialises little endian.</summary>
    [Test] procedure SingleEntrySerialisesLittleEndian;
    /// <summary>Multiple entries preserve order.</summary>
    [Test] procedure MultipleEntriesPreserveOrder;
    /// <summary>Count reports length.</summary>
    [Test] procedure CountReportsLength;
    /// <summary>C a n f d data rate constant is0x8011.</summary>
    [Test] procedure CANFDDataRateConstantIs0x8011;
    /// <summary>Mixed format constant is0x800 b.</summary>
    [Test] procedure MixedFormatConstantIs0x800B;
  end;

implementation

uses
  System.SysUtils, OBD.Adapter.PassThrough.J2534v2;

procedure TJ2534v2Tests.EmptyListSerialisesToFourZeroBytes;
var
  L: TJ2534ConfigList;
  B: TBytes;
begin
  L := TJ2534ConfigList.Create;
  try
    B := L.ToBytes;
    Assert.AreEqual(4, Length(B));
    Assert.AreEqual(0, Integer(B[0]));
    Assert.AreEqual(0, Integer(B[1]));
    Assert.AreEqual(0, Integer(B[2]));
    Assert.AreEqual(0, Integer(B[3]));
  finally L.Free; end;
end;

procedure TJ2534v2Tests.SingleEntrySerialisesLittleEndian;
var
  L: TJ2534ConfigList;
  B: TBytes;
begin
  L := TJ2534ConfigList.Create;
  try
    L.Add(CFG_DATA_RATE, 500000);
    B := L.ToBytes;
    Assert.AreEqual(12, Length(B));
    // count = 1
    Assert.AreEqual(1, Integer(B[0]));
    // parameter = 0x00000001
    Assert.AreEqual($01, Integer(B[4]));
    Assert.AreEqual($00, Integer(B[5]));
    // value = 500000 = 0x0007A120 -> LE: 20 A1 07 00
    Assert.AreEqual($20, Integer(B[8]));
    Assert.AreEqual($A1, Integer(B[9]));
    Assert.AreEqual($07, Integer(B[10]));
    Assert.AreEqual($00, Integer(B[11]));
  finally L.Free; end;
end;

procedure TJ2534v2Tests.MultipleEntriesPreserveOrder;
var
  L: TJ2534ConfigList;
begin
  L := TJ2534ConfigList.Create;
  try
    L.Add(CFG_DATA_RATE, 500000);
    L.Add(CFG_CAN_FD_DATA_RATE, 2000000);
    L.Add(CFG_CAN_MIXED_FORMAT, 1);
    Assert.AreEqual(3, L.Count);
    Assert.AreEqual(4 + 3 * 8, Length(L.ToBytes));
  finally L.Free; end;
end;

procedure TJ2534v2Tests.CountReportsLength;
var
  L: TJ2534ConfigList;
begin
  L := TJ2534ConfigList.Create;
  try
    Assert.AreEqual(0, L.Count);
    L.Add(CFG_LOOPBACK, 0);
    Assert.AreEqual(1, L.Count);
  finally L.Free; end;
end;

procedure TJ2534v2Tests.CANFDDataRateConstantIs0x8011;
begin
  Assert.AreEqual(Cardinal($8011), CFG_CAN_FD_DATA_RATE);
end;

procedure TJ2534v2Tests.MixedFormatConstantIs0x800B;
begin
  Assert.AreEqual(Cardinal($800B), CFG_CAN_MIXED_FORMAT);
end;

initialization
  TDUnitX.RegisterTestFixture(TJ2534v2Tests);

end.
