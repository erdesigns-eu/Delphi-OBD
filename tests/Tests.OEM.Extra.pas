//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Extra
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Tests the Mercedes / Ford / GM / Stellantis extensions
//                  added in v3.2.
//------------------------------------------------------------------------------
unit Tests.OEM.Extra;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TOEMExtraRegistryTests = class
  public
    [Test] [TestCase('MB_WDD',     'WDD2042152F123456,MB')]
    [TestCase('MB_Sprinter',       'WDF9066131A123456,MB')]
    [TestCase('Ford_USA',          '1FAHP3F22JL123456,FORD')]
    [TestCase('Ford_TransitUK',    'WF0EXXTTREJ123456,FORD')]
    [TestCase('GM_Chevrolet_US',   '1G1JC5SHXG4123456,GM')]
    [TestCase('GM_Cadillac',       '1G6AB5RX5F0123456,GM')]
    [TestCase('Stellantis_Jeep',   '1J4FF48S81L123456,STLA')]
    [TestCase('Stellantis_Fiat',   'ZFA31200000123456,STLA')]
    [TestCase('Stellantis_Peugeot','VF36DRHE9HS123456,STLA')]
    procedure FindByVIN_RoutesToCorrectOEM(const VIN, ExpectedKey: string);

    [Test] procedure Mercedes_DecodeMileage;
    [Test] procedure Mercedes_DecodeProgrammingStatus;
    [Test] procedure Ford_DecodeBatteryVoltage;
    [Test] procedure Ford_DecodeFuelLevel;
    [Test] procedure GM_DecodeMileage;
    [Test] procedure Stellantis_DecodeProgrammingDate;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.Mercedes, OBD.OEM.Ford, OBD.OEM.GM, OBD.OEM.Stellantis;

procedure TOEMExtraRegistryTests.FindByVIN_RoutesToCorrectOEM(
  const VIN, ExpectedKey: string);
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByVIN(VIN);
  Assert.IsNotNull(Pointer(Ext),
    Format('No OEM matched VIN %s', [VIN]));
  Assert.AreEqual(ExpectedKey, Ext.ManufacturerKey);
end;

procedure TOEMExtraRegistryTests.Mercedes_DecodeMileage;
var
  Ext: IOBDOEMExtension;
  Payload: TBytes;
begin
  Ext := TOBDOEMRegistry.FindByKey('MB');
  // 0x012345 = 74565 km
  Payload := TBytes.Create($01, $23, $45);
  Assert.AreEqual('mileage = 74565 km', Ext.DecodeDID($0202, Payload));
end;

procedure TOEMExtraRegistryTests.Mercedes_DecodeProgrammingStatus;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByKey('MB');
  Assert.AreEqual('programming_status = OK',
    Ext.DecodeDID($F19E, TBytes.Create($00)));
  Assert.AreEqual('programming_status = INTERRUPTED',
    Ext.DecodeDID($F19E, TBytes.Create($01)));
  Assert.AreEqual('programming_status = CHECKSUM_INVALID',
    Ext.DecodeDID($F19E, TBytes.Create($02)));
end;

procedure TOEMExtraRegistryTests.Ford_DecodeBatteryVoltage;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByKey('FORD');
  // 12345 mV
  Assert.AreEqual('battery_voltage = 12.345 V',
    Ext.DecodeDID($DE02, TBytes.Create($30, $39)));
end;

procedure TOEMExtraRegistryTests.Ford_DecodeFuelLevel;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByKey('FORD');
  Assert.AreEqual('fuel_level = 75 %',
    Ext.DecodeDID($DE00, TBytes.Create($4B)));
end;

procedure TOEMExtraRegistryTests.GM_DecodeMileage;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByKey('GM');
  // 0x0001E240 = 123456 km, 4-byte big-endian
  Assert.AreEqual('mileage = 123456 km',
    Ext.DecodeDID($1981, TBytes.Create($00, $01, $E2, $40)));
end;

procedure TOEMExtraRegistryTests.Stellantis_DecodeProgrammingDate;
var Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByKey('STLA');
  // BCD-encoded YY MM DD = 25 03 14 → 2025-03-14
  Assert.AreEqual('programming_date = 2025-03-14',
    Ext.DecodeDID($F199, TBytes.Create($25, $03, $14)));
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMExtraRegistryTests);

end.
