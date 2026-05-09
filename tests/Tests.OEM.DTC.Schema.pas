//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.DTC.Schema
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Phase A.5 tests for the v3.77 DTC schema extension.
//                  Verifies that:
//                  1. The new optional fields (Symptoms,
//                     RepairGuidance, MonitorType,
//                     FreezeFrameRelevant, RelatedDIDs,
//                     RelatedRoutines, OemBulletin) load correctly.
//                  2. Lookups by code resolve the rich metadata.
//                  3. Backward compatibility — old-format catalogs
//                     (without the new fields) still load.
//                  4. The shipped dtc-iso-15031.json catalog has the
//                     new fields populated for sample entries.
//------------------------------------------------------------------------------
unit Tests.OEM.DTC.Schema;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDtcSchemaExtensionTests = class
  public
    /// <summary>
    ///   Loads new fields from inline j s o n.
    /// </summary>
    [Test] procedure LoadsNewFieldsFromInlineJSON;
    /// <summary>
    ///   Backward compatible with old format.
    /// </summary>
    [Test] procedure BackwardCompatibleWithOldFormat;
    /// <summary>
    ///   Parses monitor type strings.
    /// </summary>
    [Test] procedure ParsesMonitorTypeStrings;
    /// <summary>
    ///   Shipped i s o15031 has monitor type.
    /// </summary>
    [Test] procedure ShippedISO15031HasMonitorType;
    /// <summary>
    ///   Shipped i s o15031 has related d i ds.
    /// </summary>
    [Test] procedure ShippedISO15031HasRelatedDIDs;
    /// <summary>
    ///   Shipped i s o15031 freeze frame on misfires.
    /// </summary>
    [Test] procedure ShippedISO15031FreezeFrameOnMisfires;
    /// <summary>
    ///   Sample p codes found by lookup.
    /// </summary>
    [Test] procedure SamplePCodesFoundByLookup;
    /// <summary>
    ///   Sample u codes found by lookup.
    /// </summary>
    [Test] procedure SampleUCodesFoundByLookup;
    /// <summary>
    ///   Related routines point at catalog routines.
    /// </summary>
    [Test] procedure RelatedRoutinesPointAtCatalogRoutines;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Classes,
  OBD.OEM.DTC, OBD.OEM.Catalog.Loader;

const
  DTC_WITH_NEW_FIELDS =
    '{' +
    '"version": 2,' +
    '"name": "test", "default_source": "ut",' +
    '"dtcs": [' +
    '  {' +
    '    "code": "P0301",' +
    '    "severity": "critical",' +
    '    "description": "Cylinder 1 misfire",' +
    '    "possible_causes": ["coil 1", "plug 1", "injector 1"],' +
    '    "symptoms": ["rough idle", "MIL on"],' +
    '    "repair_guidance": ["1) Read freeze frame.", "2) Swap coils."],' +
    '    "monitor_type": "continuous",' +
    '    "freeze_frame_relevant": true,' +
    '    "related_dids": ["ecm_cyl1_misfire", "ecm_lambda_b1"],' +
    '    "related_routines": ["ecm_misfire_adapt"],' +
    '    "oem_bulletin": "TSB 06-022"' +
    '  }' +
    ']' +
    '}';

  DTC_OLD_FORMAT =
    '{"dtcs": [{"code": "P0420", "description": "Cat efficiency"}]}';

//------------------------------------------------------------------------------
// LOADS NEW FIELDS FROM INLINE JSON
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.LoadsNewFieldsFromInlineJSON;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(DTC_WITH_NEW_FIELDS);
    Assert.IsTrue(Cat.FindByCode('P0301', Entry), 'P0301 should be present');
    Assert.AreEqual('Cylinder 1 misfire', Entry.Description);
    Assert.AreEqual<Cardinal>(2, Cardinal(Length(Entry.Symptoms)));
    Assert.AreEqual('rough idle', Entry.Symptoms[0]);
    Assert.AreEqual<Cardinal>(2, Cardinal(Length(Entry.RepairGuidance)));
    Assert.AreEqual<TOBDDtcMonitorType>(dmtContinuous, Entry.MonitorType);
    Assert.IsTrue(Entry.FreezeFrameRelevant, 'misfire should set FF flag');
    Assert.AreEqual<Cardinal>(2, Cardinal(Length(Entry.RelatedDIDs)));
    Assert.AreEqual('ecm_cyl1_misfire', Entry.RelatedDIDs[0]);
    Assert.AreEqual<Cardinal>(1, Cardinal(Length(Entry.RelatedRoutines)));
    Assert.AreEqual('ecm_misfire_adapt', Entry.RelatedRoutines[0]);
    Assert.AreEqual('TSB 06-022', Entry.OemBulletin);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// BACKWARD COMPATIBLE WITH OLD FORMAT
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.BackwardCompatibleWithOldFormat;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    // Old-format catalog without new fields must still load.
    Cat.LoadFromText(DTC_OLD_FORMAT);
    Assert.IsTrue(Cat.FindByCode('P0420', Entry));
    Assert.AreEqual('Cat efficiency', Entry.Description);
    // New fields default to empty / zero / unknown.
    Assert.AreEqual<Cardinal>(0, Cardinal(Length(Entry.Symptoms)));
    Assert.AreEqual<Cardinal>(0, Cardinal(Length(Entry.RepairGuidance)));
    Assert.AreEqual<TOBDDtcMonitorType>(dmtUnknown, Entry.MonitorType);
    Assert.IsFalse(Entry.FreezeFrameRelevant);
    Assert.AreEqual<Cardinal>(0, Cardinal(Length(Entry.RelatedDIDs)));
    Assert.AreEqual('', Entry.OemBulletin);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// PARSES MONITOR TYPE STRINGS
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.ParsesMonitorTypeStrings;
begin
  Assert.AreEqual<TOBDDtcMonitorType>(dmtContinuous,
    ParseMonitorType('continuous'));
  Assert.AreEqual<TOBDDtcMonitorType>(dmtNonContinuous,
    ParseMonitorType('non_continuous'));
  Assert.AreEqual<TOBDDtcMonitorType>(dmtNonContinuous,
    ParseMonitorType('noncontinuous'));
  Assert.AreEqual<TOBDDtcMonitorType>(dmtNonContinuous,
    ParseMonitorType('non-continuous'));
  Assert.AreEqual<TOBDDtcMonitorType>(dmtComprehensiveComponent,
    ParseMonitorType('comprehensive_component'));
  Assert.AreEqual<TOBDDtcMonitorType>(dmtUnknown,
    ParseMonitorType(''));
  Assert.AreEqual<TOBDDtcMonitorType>(dmtUnknown,
    ParseMonitorType('garbage'));
  Assert.AreEqual('continuous', FormatMonitorType(dmtContinuous));
  Assert.AreEqual('non_continuous', FormatMonitorType(dmtNonContinuous));
end;

// G9 (closed): use the loader's exported ResolveCatalogPath
// rather than re-implementing the search-path logic here. Keeps
// tests in lock-step with production.

//------------------------------------------------------------------------------
// LOAD SHIPPED DTC CATALOG
//------------------------------------------------------------------------------
procedure LoadShippedDtcCatalog(const FileName: string;
                                 Cat: TOBDDtcCatalog);
var
  Path: string;
begin
  Path := ResolveCatalogPath(FileName);
  if Path = '' then
    Assert.Pass(FileName + ' not on path; skipping');
  Cat.LoadFromFile(Path);
end;

//------------------------------------------------------------------------------
// SHIPPED ISO15031 HAS MONITOR TYPE
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.ShippedISO15031HasMonitorType;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    LoadShippedDtcCatalog('dtc-iso-15031.json', Cat);
    // P0420 catalyst efficiency — non-continuous monitor.
    Assert.IsTrue(Cat.FindByCode('P0420', Entry), 'P0420 should be in shipped catalog');
    Assert.AreEqual<TOBDDtcMonitorType>(dmtNonContinuous, Entry.MonitorType);
    // P0301 misfire — continuous monitor.
    Assert.IsTrue(Cat.FindByCode('P0301', Entry), 'P0301 should be in shipped catalog');
    Assert.AreEqual<TOBDDtcMonitorType>(dmtContinuous, Entry.MonitorType);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// SHIPPED ISO15031 HAS RELATED DIDS
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.ShippedISO15031HasRelatedDIDs;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
  Found: Boolean;
  S: string;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    LoadShippedDtcCatalog('dtc-iso-15031.json', Cat);
    Assert.IsTrue(Cat.FindByCode('P0301', Entry));
    // Should reference the per-cylinder misfire DID we added.
    Found := False;
    for S in Entry.RelatedDIDs do
      if S = 'ecm_cyl1_misfire' then Found := True;
    Assert.IsTrue(Found, 'P0301 should reference ecm_cyl1_misfire DID');
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// SHIPPED ISO15031 FREEZE FRAME ON MISFIRES
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.ShippedISO15031FreezeFrameOnMisfires;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    LoadShippedDtcCatalog('dtc-iso-15031.json', Cat);
    Assert.IsTrue(Cat.FindByCode('P0300', Entry), 'P0300 random misfire');
    Assert.IsTrue(Entry.FreezeFrameRelevant);
    Assert.IsTrue(Cat.FindByCode('P0301', Entry), 'P0301 cyl 1 misfire');
    Assert.IsTrue(Entry.FreezeFrameRelevant);
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// SAMPLE PCODES FOUND BY LOOKUP
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.SamplePCodesFoundByLookup;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    LoadShippedDtcCatalog('dtc-iso-15031.json', Cat);
    Assert.IsTrue(Cat.FindByCode('P0420', Entry));
    Assert.IsTrue(Cat.FindByCode('P0440', Entry));
    Assert.IsTrue(Cat.FindByCode('P244A', Entry));
    Assert.IsTrue(Cat.FindByCode('P204F', Entry));
    Assert.IsTrue(Cat.FindByCode('P1000', Entry));
    Assert.IsFalse(Cat.FindByCode('P0000', Entry));
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// SAMPLE UCODES FOUND BY LOOKUP
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.SampleUCodesFoundByLookup;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    LoadShippedDtcCatalog('dtc-iso-15031.json', Cat);
    Assert.IsTrue(Cat.FindByCode('U0100', Entry), 'Lost comm with ECM/PCM');
    Assert.AreEqual<TOBDDtcSeverity>(dtcSeverityCritical, Entry.Severity);
    Assert.IsTrue(Cat.FindByCode('U0001', Entry), 'High Speed CAN');
  finally
    Cat.Free;
  end;
end;

//------------------------------------------------------------------------------
// RELATED ROUTINES POINT AT CATALOG ROUTINES
//------------------------------------------------------------------------------
procedure TDtcSchemaExtensionTests.RelatedRoutinesPointAtCatalogRoutines;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
  Found: Boolean;
  S: string;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    LoadShippedDtcCatalog('dtc-iso-15031.json', Cat);
    // P244A DPF differential pressure references ecm_dpf_regen_force,
    // which is the routine name in our OEM catalogs.
    Assert.IsTrue(Cat.FindByCode('P244A', Entry));
    Found := False;
    for S in Entry.RelatedRoutines do
      if S = 'ecm_dpf_regen_force' then Found := True;
    Assert.IsTrue(Found,
      'P244A should suggest ecm_dpf_regen_force routine');
  finally
    Cat.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDtcSchemaExtensionTests);

end.
