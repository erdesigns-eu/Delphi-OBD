//------------------------------------------------------------------------------
// UNIT           : Tests.J1939.PGNs.pas
// CONTENTS       : Tests for OBD.J1939.PGNs
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.J1939.PGNs;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TJ1939PGNsTests = class
  public
    [Test] procedure SeedHasAtLeastFortyEntries;
    [Test] procedure NoDuplicatePGNIds;
    [Test] procedure EveryEntryHasMnemonicAndName;
    [Test] procedure EveryEntryHasSpecCitation;
    [Test] procedure FindDM1ReturnsCorrectMnemonic;
    [Test] procedure FindEEC1HasPriorityThree;
    [Test] procedure FindUnknownPGNReturnsZeroRecord;
    [Test] procedure RegisterReplacesExisting;
    [Test] procedure RegisterAddsNewEntry;
    [Test] procedure AllReturnsSortedAscending;
    [Test] procedure AddressClaimAndTransportProtocolDistinct;
  end;

implementation

uses
  System.SysUtils, OBD.J1939.PGNs;

procedure TJ1939PGNsTests.SeedHasAtLeastFortyEntries;
begin
  Assert.IsTrue(J1939PGNCount >= 40,
    'Expected >= 40 PGN entries, got ' + IntToStr(J1939PGNCount));
end;

procedure TJ1939PGNsTests.NoDuplicatePGNIds;
var
  All: TArray<TJ1939PGNDescriptor>;
  I: Integer;
begin
  All := J1939PGNAll;
  for I := 1 to High(All) do
    Assert.AreNotEqual(All[I - 1].PGN, All[I].PGN,
      Format('Duplicate PGN 0x%.4X between %s and %s',
        [All[I].PGN, All[I - 1].Mnemonic, All[I].Mnemonic]));
end;

procedure TJ1939PGNsTests.EveryEntryHasMnemonicAndName;
var D: TJ1939PGNDescriptor;
begin
  for D in J1939PGNAll do
  begin
    Assert.IsNotEmpty(D.Mnemonic, Format('PGN 0x%.4X has empty mnemonic', [D.PGN]));
    Assert.IsNotEmpty(D.Name, Format('PGN 0x%.4X has empty name', [D.PGN]));
  end;
end;

procedure TJ1939PGNsTests.EveryEntryHasSpecCitation;
var D: TJ1939PGNDescriptor;
begin
  for D in J1939PGNAll do
    Assert.IsNotEmpty(D.SpecSection,
      Format('PGN 0x%.4X (%s) missing spec section', [D.PGN, D.Mnemonic]));
end;

procedure TJ1939PGNsTests.FindDM1ReturnsCorrectMnemonic;
var D: TJ1939PGNDescriptor;
begin
  D := FindPGN($FECA);
  Assert.AreEqual('DM1', D.Mnemonic);
  Assert.IsTrue(D.Name.Contains('Active'));
end;

procedure TJ1939PGNsTests.FindEEC1HasPriorityThree;
var D: TJ1939PGNDescriptor;
begin
  D := FindPGN($F004);
  Assert.AreEqual('EEC1', D.Mnemonic);
  Assert.AreEqual(3, Integer(D.DefaultPriority));
end;

procedure TJ1939PGNsTests.FindUnknownPGNReturnsZeroRecord;
var D: TJ1939PGNDescriptor;
begin
  D := FindPGN($1234);
  Assert.AreEqual(UInt32(0), D.PGN);
end;

procedure TJ1939PGNsTests.RegisterReplacesExisting;
var
  Custom, Round: TJ1939PGNDescriptor;
begin
  Custom := FindPGN($FECA); // DM1
  Custom.Mnemonic := 'CUSTOM-DM1';
  RegisterJ1939PGN(Custom);
  Round := FindPGN($FECA);
  Assert.AreEqual('CUSTOM-DM1', Round.Mnemonic);
  // Restore so other tests don't see the mutation:
  Custom.Mnemonic := 'DM1';
  RegisterJ1939PGN(Custom);
end;

procedure TJ1939PGNsTests.RegisterAddsNewEntry;
var
  Before, After: Integer;
  D: TJ1939PGNDescriptor;
begin
  Before := J1939PGNCount;
  D.PGN := $9999;
  D.Mnemonic := 'TEST';
  D.Name := 'Test PGN for unit test';
  D.LengthBytes := 8;
  D.DefaultPriority := 6;
  D.TxRateMs := 1000;
  D.SpecSection := 'test only';
  RegisterJ1939PGN(D);
  After := J1939PGNCount;
  Assert.AreEqual(Before + 1, After);
  Assert.AreEqual('TEST', FindPGN($9999).Mnemonic);
end;

procedure TJ1939PGNsTests.AllReturnsSortedAscending;
var
  All: TArray<TJ1939PGNDescriptor>;
  I: Integer;
begin
  All := J1939PGNAll;
  for I := 1 to High(All) do
    Assert.IsTrue(All[I - 1].PGN < All[I].PGN,
      'PGN list must be ascending');
end;

procedure TJ1939PGNsTests.AddressClaimAndTransportProtocolDistinct;
begin
  Assert.AreEqual(UInt32($EE00), FindPGN($EE00).PGN);
  Assert.AreEqual('AC', FindPGN($EE00).Mnemonic);
  Assert.AreEqual('TP.CM', FindPGN($EC00).Mnemonic);
  Assert.AreEqual('TP.DT', FindPGN($EB00).Mnemonic);
end;

initialization
  TDUnitX.RegisterTestFixture(TJ1939PGNsTests);

end.
