//------------------------------------------------------------------------------
// UNIT           : Tests.OEM
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TOEMRegistryTests = class
  public
    [Test] procedure RegisterAndFindByKey;
    [Test] procedure FindByVIN_VW_MatchesWVW;
    [Test] procedure FindByVIN_BMW_MatchesWBA;
    [Test] procedure FindByVIN_NonOEMReturnsNil;
    [Test] procedure RegisterIsIdempotent;
    [Test] procedure UnregisterRemovesExtension;

    [Test] procedure VW_DecodeBatteryVoltage;
    [Test] procedure VW_DecodeVehicleSpeed;
    [Test] procedure VW_DecodeUnknownDIDFallsBackToHex;
    [Test] procedure BMW_DecodeMileage;

    [Test] procedure FindDID_LooksUpCatalogEntry;
    [Test] procedure FindRoutine_LooksUpCatalogEntry;
  end;

implementation

uses
  System.SysUtils, OBD.OEM, OBD.OEM.VW, OBD.OEM.BMW;

procedure TOEMRegistryTests.RegisterAndFindByKey;
var
  Ext: IOBDOEMExtension;
begin
  // The VW + BMW units self-register at unit init.
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Assert.IsNotNull(Pointer(Ext));
  Assert.AreEqual('VAG', Ext.ManufacturerKey);
end;

procedure TOEMRegistryTests.FindByVIN_VW_MatchesWVW;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByVIN('WVWZZZ1KZAW123456');
  Assert.IsNotNull(Pointer(Ext));
  Assert.AreEqual('VAG', Ext.ManufacturerKey);
end;

procedure TOEMRegistryTests.FindByVIN_BMW_MatchesWBA;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByVIN('WBA3C1C50DF136345');
  Assert.IsNotNull(Pointer(Ext));
  Assert.AreEqual('BMW', Ext.ManufacturerKey);
end;

procedure TOEMRegistryTests.FindByVIN_NonOEMReturnsNil;
var
  Ext: IOBDOEMExtension;
begin
  // 1FA = Ford USA — not in our shipped registry.
  Ext := TOBDOEMRegistry.FindByVIN('1FA1234567890ABCD');
  Assert.IsNull(Pointer(Ext));
end;

procedure TOEMRegistryTests.RegisterIsIdempotent;
var
  Same: IOBDOEMExtension;
  Before, After: Integer;
begin
  Same := TOBDOEMRegistry.FindByKey('VAG');
  Before := TOBDOEMRegistry.Count;
  TOBDOEMRegistry.RegisterExtension(Same);
  After := TOBDOEMRegistry.Count;
  Assert.AreEqual(Before, After,
    'Registering an already-registered extension must not duplicate it');
end;

procedure TOEMRegistryTests.UnregisterRemovesExtension;
var
  Ext: IOBDOEMExtension;
  Before: Integer;
begin
  Ext := TOBDOEMExtensionVW.Create;
  TOBDOEMRegistry.RegisterExtension(Ext);
  Before := TOBDOEMRegistry.Count;
  TOBDOEMRegistry.UnregisterExtension(Ext);
  Assert.AreEqual(Before - 1, TOBDOEMRegistry.Count);
end;

procedure TOEMRegistryTests.VW_DecodeBatteryVoltage;
var
  Ext: IOBDOEMExtension;
  Payload: TBytes;
begin
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  // 12345 mV = 12.345 V, big-endian.
  Payload := TBytes.Create($30, $39);  // 0x3039 = 12345
  Assert.AreEqual('battery_voltage = 12.345 V', Ext.DecodeDID($F405, Payload));
end;

procedure TOEMRegistryTests.VW_DecodeVehicleSpeed;
var
  Ext: IOBDOEMExtension;
  Payload: TBytes;
begin
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Payload := TBytes.Create($00, $7B);  // 123 km/h
  Assert.AreEqual('vehicle_speed = 123 km/h', Ext.DecodeDID($F40D, Payload));
end;

procedure TOEMRegistryTests.VW_DecodeUnknownDIDFallsBackToHex;
var
  Ext: IOBDOEMExtension;
  Payload: TBytes;
begin
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Payload := TBytes.Create($DE, $AD, $BE, $EF);
  Assert.AreEqual('DID 0x9999 = DE AD BE EF', Ext.DecodeDID($9999, Payload));
end;

procedure TOEMRegistryTests.BMW_DecodeMileage;
var
  Ext: IOBDOEMExtension;
  Payload: TBytes;
begin
  Ext := TOBDOEMRegistry.FindByKey('BMW');
  // 0x0001E240 = 123456 km
  Payload := TBytes.Create($00, $01, $E2, $40);
  Assert.AreEqual('mileage = 123456 km', Ext.DecodeDID($D050, Payload));
end;

procedure TOEMRegistryTests.FindDID_LooksUpCatalogEntry;
var
  Ext: IOBDOEMExtension;
  Entry: TOBDOEMDataIdentifier;
begin
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Assert.IsTrue(Ext.FindDID($F190, Entry));
  Assert.AreEqual('vin', Entry.Name);
  Assert.IsFalse(Ext.FindDID($DEAD, Entry),
    'unknown DID must return False');
end;

procedure TOEMRegistryTests.FindRoutine_LooksUpCatalogEntry;
var
  Ext: IOBDOEMExtension;
  Entry: TOBDOEMRoutine;
begin
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Assert.IsTrue(Ext.FindRoutine($FF00, Entry));
  Assert.AreEqual('erase_memory', Entry.Name);
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMRegistryTests);

end.
