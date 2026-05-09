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
    /// <summary>
    ///   Register and find by key.
    /// </summary>
    [Test] procedure RegisterAndFindByKey;
    /// <summary>
    ///   Find by v i n  v w  matches w v w.
    /// </summary>
    [Test] procedure FindByVIN_VW_MatchesWVW;
    /// <summary>
    ///   Find by v i n  b m w  matches w b a.
    /// </summary>
    [Test] procedure FindByVIN_BMW_MatchesWBA;
    /// <summary>
    ///   Find by v i n  non o e m returns nil.
    /// </summary>
    [Test] procedure FindByVIN_NonOEMReturnsNil;
    /// <summary>
    ///   Register is idempotent.
    /// </summary>
    [Test] procedure RegisterIsIdempotent;
    /// <summary>
    ///   Unregister removes extension.
    /// </summary>
    [Test] procedure UnregisterRemovesExtension;

    /// <summary>
    ///   V w  decode battery voltage.
    /// </summary>
    [Test] procedure VW_DecodeBatteryVoltage;
    /// <summary>
    ///   V w  decode vehicle speed.
    /// </summary>
    [Test] procedure VW_DecodeVehicleSpeed;
    /// <summary>
    ///   V w  decode unknown d i d falls back to hex.
    /// </summary>
    [Test] procedure VW_DecodeUnknownDIDFallsBackToHex;
    /// <summary>
    ///   B m w  decode mileage.
    /// </summary>
    [Test] procedure BMW_DecodeMileage;

    /// <summary>
    ///   Find d i d  looks up catalog entry.
    /// </summary>
    [Test] procedure FindDID_LooksUpCatalogEntry;
    /// <summary>
    ///   Find routine  looks up catalog entry.
    /// </summary>
    [Test] procedure FindRoutine_LooksUpCatalogEntry;
  end;

implementation

uses
  System.SysUtils, OBD.OEM, OBD.OEM.VW, OBD.OEM.BMW;

//------------------------------------------------------------------------------
// REGISTER AND FIND BY KEY
//------------------------------------------------------------------------------
procedure TOEMRegistryTests.RegisterAndFindByKey;
var
  Ext: IOBDOEMExtension;
begin
  // The VW + BMW units self-register at unit init.
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Assert.IsNotNull(Pointer(Ext));
  Assert.AreEqual('VAG', Ext.ManufacturerKey);
end;

//------------------------------------------------------------------------------
// FIND BY VIN_VW_MATCHES WVW
//------------------------------------------------------------------------------
procedure TOEMRegistryTests.FindByVIN_VW_MatchesWVW;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByVIN('WVWZZZ1KZAW123456');
  Assert.IsNotNull(Pointer(Ext));
  Assert.AreEqual('VAG', Ext.ManufacturerKey);
end;

//------------------------------------------------------------------------------
// FIND BY VIN_BMW_MATCHES WBA
//------------------------------------------------------------------------------
procedure TOEMRegistryTests.FindByVIN_BMW_MatchesWBA;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByVIN('WBA3C1C50DF136345');
  Assert.IsNotNull(Pointer(Ext));
  Assert.AreEqual('BMW', Ext.ManufacturerKey);
end;

//------------------------------------------------------------------------------
// FIND BY VIN_NON OEMRETURNS NIL
//------------------------------------------------------------------------------
procedure TOEMRegistryTests.FindByVIN_NonOEMReturnsNil;
var
  Ext: IOBDOEMExtension;
begin
  // 1FA = Ford USA — not in our shipped registry.
  Ext := TOBDOEMRegistry.FindByVIN('1FA1234567890ABCD');
  Assert.IsNull(Pointer(Ext));
end;

//------------------------------------------------------------------------------
// REGISTER IS IDEMPOTENT
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// UNREGISTER REMOVES EXTENSION
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// VW_DECODE BATTERY VOLTAGE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// VW_DECODE VEHICLE SPEED
//------------------------------------------------------------------------------
procedure TOEMRegistryTests.VW_DecodeVehicleSpeed;
var
  Ext: IOBDOEMExtension;
  Payload: TBytes;
begin
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Payload := TBytes.Create($00, $7B);  // 123 km/h
  Assert.AreEqual('vehicle_speed = 123 km/h', Ext.DecodeDID($F40D, Payload));
end;

//------------------------------------------------------------------------------
// VW_DECODE UNKNOWN DIDFALLS BACK TO HEX
//------------------------------------------------------------------------------
procedure TOEMRegistryTests.VW_DecodeUnknownDIDFallsBackToHex;
var
  Ext: IOBDOEMExtension;
  Payload: TBytes;
begin
  Ext := TOBDOEMRegistry.FindByKey('VAG');
  Payload := TBytes.Create($DE, $AD, $BE, $EF);
  Assert.AreEqual('DID 0x9999 = DE AD BE EF', Ext.DecodeDID($9999, Payload));
end;

//------------------------------------------------------------------------------
// BMW_DECODE MILEAGE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FIND DID_LOOKS UP CATALOG ENTRY
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// FIND ROUTINE_LOOKS UP CATALOG ENTRY
//------------------------------------------------------------------------------
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
