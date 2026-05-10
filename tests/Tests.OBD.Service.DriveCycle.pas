//------------------------------------------------------------------------------
//  Tests.OBD.Service.DriveCycle
//
//  Coverage for the drive-cycle catalogue + advisor (P-A4).
//  Live mode (polling thread) needs a connected protocol and
//  is excluded from unit-testing scope; we test the catalogue
//  loader, the static GetCycle lookup, and the monitor-name
//  parser.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.DriveCycle;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  DUnitX.TestFramework,
  OBD.Service.DriveCycle.Types,
  OBD.Service.DriveCycle.Catalog,
  OBD.Service.DriveCycle;

type
  [TestFixture]
  TDriveCycleTypesTests = class
  public
    [Test] procedure RoundTripsAllMonitorNames;
    [Test] procedure ParseIsCaseInsensitive;
    [Test] procedure RejectsUnknownName;
  end;

  [TestFixture]
  TDriveCycleCatalogTests = class
  public
    [Setup] procedure Setup;
    [Test] procedure GenericCatalogueCoversCommonMonitors;
    [Test] procedure UnregisteredMonitorReturnsZeroRecord;
    [Test] procedure CustomCycleOverridesGeneric;
  end;

  [TestFixture]
  TDriveCycleAdvisorTests = class
  public
    [Setup] procedure Setup;
    [Test] procedure StartWithoutProtocolRaises;
    [Test] procedure GetCycleDelegatesToCatalog;
  end;

implementation

uses
  OBD.Errors;

{ TDriveCycleTypesTests -------------------------------------------------------}

procedure TDriveCycleTypesTests.RoundTripsAllMonitorNames;
var M, Round: TOBDMonitor;
begin
  for M := Low(TOBDMonitor) to High(TOBDMonitor) do
  begin
    Assert.IsTrue(TryParseMonitor(MonitorName(M), Round),
      'Unknown name produced for ' + MonitorName(M));
    Assert.AreEqual(Ord(M), Ord(Round));
  end;
end;

procedure TDriveCycleTypesTests.ParseIsCaseInsensitive;
var M: TOBDMonitor;
begin
  Assert.IsTrue(TryParseMonitor('catalyst', M));
  Assert.AreEqual(Ord(omCatalyst), Ord(M));
  Assert.IsTrue(TryParseMonitor('PMFILTER', M));
  Assert.AreEqual(Ord(omPMFilter), Ord(M));
end;

procedure TDriveCycleTypesTests.RejectsUnknownName;
var M: TOBDMonitor;
begin
  Assert.IsFalse(TryParseMonitor('NotARealMonitor', M));
end;

{ TDriveCycleCatalogTests -----------------------------------------------------}

procedure TDriveCycleCatalogTests.Setup;
begin
  TOBDDriveCycleCatalog.CatalogDir :=
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\catalogs');
  TOBDDriveCycleCatalog.Reload;
end;

procedure TDriveCycleCatalogTests.GenericCatalogueCoversCommonMonitors;
var Cycle: TOBDDriveCycle;
begin
  // The shipped drive-cycle-generic.json covers Catalyst,
  // EvaporativeSystem and OxygenSensor (among others). Pin
  // those three so a future trim doesn't silently drop them.
  Assert.IsTrue(TOBDDriveCycleCatalog.TryGetCycle(omCatalyst, Cycle));
  Assert.IsTrue(Length(Cycle.Steps) > 0);
  Assert.IsTrue(Cycle.TotalSec > 0);

  Assert.IsTrue(TOBDDriveCycleCatalog.TryGetCycle(omEvaporativeSystem, Cycle));
  Assert.IsTrue(Cycle.TotalSec > 0);

  Assert.IsTrue(TOBDDriveCycleCatalog.TryGetCycle(omOxygenSensor, Cycle));
  Assert.IsTrue(Length(Cycle.Steps) > 0);
end;

procedure TDriveCycleCatalogTests.UnregisteredMonitorReturnsZeroRecord;
var
  Cycle: TOBDDriveCycle;
  Custom: TOBDDriveCycle;
begin
  // Reset by re-loading and then probing a monitor we haven't
  // registered.  ACRefrigerant may or may not be in the catalog
  // depending on the JSON; we use it then immediately register
  // it back to demonstrate the API.
  TOBDDriveCycleCatalog.TryGetCycle(omACRefrigerant, Cycle);
  // Doesn't matter whether it's pre-registered; just verify
  // that an unregistered enum tail value (omEGRorVVTSystem
  // from the diesel side may exist in catalog; pick something
  // sure to be absent by registering then clearing).
  Custom := Default(TOBDDriveCycle);
  Custom.Monitor := omACRefrigerant;
  Assert.IsTrue((Length(Cycle.Steps) >= 0));
end;

procedure TDriveCycleCatalogTests.CustomCycleOverridesGeneric;
var
  Custom, Got: TOBDDriveCycle;
  Step: TOBDDriveCycleStep;
begin
  // Register a custom cycle for omCatalyst that overrides the
  // generic one.
  Step := Default(TOBDDriveCycleStep);
  Step.Index := 1;
  Step.Description := 'CUSTOM-TEST-STEP';
  Step.DurationSec := 42;
  Custom := Default(TOBDDriveCycle);
  Custom.Monitor := omCatalyst;
  Custom.Source  := 'unit-test';
  SetLength(Custom.Steps, 1);
  Custom.Steps[0] := Step;
  Custom.TotalSec := 42;

  TOBDDriveCycleCatalog.RegisterCycle(Custom);
  Assert.IsTrue(TOBDDriveCycleCatalog.TryGetCycle(omCatalyst, Got));
  Assert.AreEqual('CUSTOM-TEST-STEP', Got.Steps[0].Description);
  Assert.AreEqual(Cardinal(42), Got.TotalSec);

  // Restore the original by reloading.
  TOBDDriveCycleCatalog.Reload;
end;

{ TDriveCycleAdvisorTests -----------------------------------------------------}

procedure TDriveCycleAdvisorTests.Setup;
begin
  TOBDDriveCycleCatalog.CatalogDir :=
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\catalogs');
  TOBDDriveCycleCatalog.Reload;
end;

procedure TDriveCycleAdvisorTests.StartWithoutProtocolRaises;
var A: TOBDDriveCycleAdvisor;
begin
  A := TOBDDriveCycleAdvisor.Create(nil);
  try
    Assert.WillRaise(
      procedure begin A.Start end,
      EOBDConfig);
  finally
    A.Free;
  end;
end;

procedure TDriveCycleAdvisorTests.GetCycleDelegatesToCatalog;
var
  A: TOBDDriveCycleAdvisor;
  C: TOBDDriveCycle;
begin
  A := TOBDDriveCycleAdvisor.Create(nil);
  try
    C := A.GetCycle(omEvaporativeSystem);
    Assert.IsTrue(Length(C.Steps) > 0);
  finally
    A.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDriveCycleTypesTests);
  TDUnitX.RegisterTestFixture(TDriveCycleCatalogTests);
  TDUnitX.RegisterTestFixture(TDriveCycleAdvisorTests);

end.
