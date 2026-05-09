//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.SupplierRouting
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.19 engine-OEM auto-
//                  routing — IOBDOEMExtension.ApplicableToECUSupplier
//                  + TOBDOEMRegistry.FindByECUSupplier.
//------------------------------------------------------------------------------
unit Tests.OEM.SupplierRouting;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSupplierRoutingTests = class
  public
    [Test] procedure CumminsClaimsCumminsAndCmi;
    [Test] procedure CumminsRejectsOtherSuppliers;
    [Test] procedure DetroitClaimsDetroitDdcDetroitDdc;
    [Test] procedure DetroitRejectsOtherSuppliers;
    [Test] procedure RegistryRoutesByCumminsId;
    [Test] procedure RegistryRoutesByDetroitId;
    [Test] procedure RegistryReturnsNilForUnknownSupplier;
    [Test] procedure RegistryHandlesEmptyString;
    [Test] procedure NonEngineOEMsReturnFalseByDefault;
    [Test] procedure SupplierMatchIsCaseInsensitive;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.Cummins, OBD.OEM.DetroitDiesel,
  OBD.OEM.VW, OBD.OEM.Toyota;

procedure TSupplierRoutingTests.CumminsClaimsCumminsAndCmi;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionCummins.Create;
  Assert.IsTrue(Ext.ApplicableToECUSupplier('CUMMINS'));
  Assert.IsTrue(Ext.ApplicableToECUSupplier('CMI'));
end;

procedure TSupplierRoutingTests.CumminsRejectsOtherSuppliers;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionCummins.Create;
  Assert.IsFalse(Ext.ApplicableToECUSupplier('DETROIT'));
  Assert.IsFalse(Ext.ApplicableToECUSupplier('VOLVO'));
  Assert.IsFalse(Ext.ApplicableToECUSupplier(''));
end;

procedure TSupplierRoutingTests.DetroitClaimsDetroitDdcDetroitDdc;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionDetroitDiesel.Create;
  Assert.IsTrue(Ext.ApplicableToECUSupplier('DETROIT'));
  Assert.IsTrue(Ext.ApplicableToECUSupplier('DDC'));
  Assert.IsTrue(Ext.ApplicableToECUSupplier('DETROITDDC'));
end;

procedure TSupplierRoutingTests.DetroitRejectsOtherSuppliers;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionDetroitDiesel.Create;
  Assert.IsFalse(Ext.ApplicableToECUSupplier('CUMMINS'));
  Assert.IsFalse(Ext.ApplicableToECUSupplier('PACCAR'));
end;

procedure TSupplierRoutingTests.RegistryRoutesByCumminsId;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByECUSupplier('CUMMINS');
  Assert.IsNotNull(Ext, 'Registry should route CUMMINS supplier ID');
  Assert.AreEqual('CUMMINS', Ext.ManufacturerKey);
end;

procedure TSupplierRoutingTests.RegistryRoutesByDetroitId;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByECUSupplier('DDC');
  Assert.IsNotNull(Ext, 'Registry should route DDC supplier ID');
  Assert.AreEqual('DDC', Ext.ManufacturerKey);
end;

procedure TSupplierRoutingTests.RegistryReturnsNilForUnknownSupplier;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMRegistry.FindByECUSupplier('UNKNOWN_SUPPLIER_NAME');
  Assert.IsNull(Ext);
end;

procedure TSupplierRoutingTests.RegistryHandlesEmptyString;
var
  Ext: IOBDOEMExtension;
begin
  // Empty-string short-circuit prevents accidental matches against
  // an extension that returns True on '' (none should, but the
  // registry guards anyway).
  Ext := TOBDOEMRegistry.FindByECUSupplier('');
  Assert.IsNull(Ext);
end;

procedure TSupplierRoutingTests.NonEngineOEMsReturnFalseByDefault;
var
  VW, Toyota: IOBDOEMExtension;
begin
  // Default base implementation returns False so the contract is
  // upward-compatible — only extensions that opt in respond.
  VW := TOBDOEMExtensionVW.Create;
  Toyota := TOBDOEMExtensionToyota.Create;
  Assert.IsFalse(VW.ApplicableToECUSupplier('VW'));
  Assert.IsFalse(VW.ApplicableToECUSupplier('VOLKSWAGEN'));
  Assert.IsFalse(Toyota.ApplicableToECUSupplier('TOYOTA'));
end;

procedure TSupplierRoutingTests.SupplierMatchIsCaseInsensitive;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionCummins.Create;
  Assert.IsTrue(Ext.ApplicableToECUSupplier('cummins'));
  Assert.IsTrue(Ext.ApplicableToECUSupplier('  Cummins  '));
end;

initialization
  TDUnitX.RegisterTestFixture(TSupplierRoutingTests);

end.
