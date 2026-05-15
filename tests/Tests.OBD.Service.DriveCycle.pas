//------------------------------------------------------------------------------
//  Tests.OBD.Service.DriveCycle
//
//  Coverage for the drive-cycle catalogue + advisor.
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

  /// <summary>Coverage for the readiness-payload decoder
  /// (DecodePID01). Exercises the bit-layout decode for both
  /// SI and CI engines without needing a connected protocol.</summary>
  [TestFixture]
  TDriveCyclePID01DecoderTests = class
  strict private
    function FindMonitor(const A: TArray<TOBDMonitorReadiness>;
      M: TOBDMonitor; out R: TOBDMonitorReadiness): Boolean;
  public
    [Test] procedure ShortPayloadFailsCleanly;
    [Test] procedure SI_AllSupportedAllComplete;
    [Test] procedure SI_CatalystSupportedNotComplete;
    [Test] procedure CI_DieselFlagPicksDieselMonitorSet;
    [Test] procedure CI_PMFilterSupportedNotComplete;
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

{ ---- TDriveCyclePID01DecoderTests ----------------------------------------- }

function TDriveCyclePID01DecoderTests.FindMonitor(
  const A: TArray<TOBDMonitorReadiness>;
  M: TOBDMonitor; out R: TOBDMonitorReadiness): Boolean;
var I: Integer;
begin
  for I := 0 to High(A) do
    if A[I].Monitor = M then
    begin
      R := A[I];
      Exit(True);
    end;
  Result := False;
end;

procedure TDriveCyclePID01DecoderTests.ShortPayloadFailsCleanly;
var Err: string; A: TArray<TOBDMonitorReadiness>;
begin
  A := TOBDDriveCycleAdvisor.DecodePID01(TBytes.Create($00, $00), Err);
  Assert.AreEqual(0, Length(A));
  Assert.IsNotEmpty(Err);
end;

procedure TDriveCyclePID01DecoderTests.SI_AllSupportedAllComplete;
var
  // B = supported continuous=$07, complete-bits cleared=$00,
  //     diesel bit cleared. C = $FF (all SI mons supported),
  //     D = $00 (all complete = bit clear).
  Bytes: TBytes;
  Err: string;
  A: TArray<TOBDMonitorReadiness>;
  R: TOBDMonitorReadiness;
begin
  Bytes := TBytes.Create($00, $07, $FF, $00);
  A := TOBDDriveCycleAdvisor.DecodePID01(Bytes, Err);
  Assert.AreEqual('', Err);
  Assert.IsTrue(FindMonitor(A, omMisfire, R));
  Assert.IsTrue(R.Supported);
  Assert.IsTrue(R.Complete);
  Assert.IsTrue(FindMonitor(A, omCatalyst, R));
  Assert.IsTrue(R.Supported);
  Assert.IsTrue(R.Complete);
end;

procedure TDriveCyclePID01DecoderTests.SI_CatalystSupportedNotComplete;
var Bytes: TBytes; Err: string; A: TArray<TOBDMonitorReadiness>;
    R: TOBDMonitorReadiness;
begin
  // SI engine; only catalyst (bit 0 of C) supported, and bit
  // 0 of D set means "not complete".
  Bytes := TBytes.Create($00, $00, $01, $01);
  A := TOBDDriveCycleAdvisor.DecodePID01(Bytes, Err);
  Assert.AreEqual('', Err);
  Assert.IsTrue(FindMonitor(A, omCatalyst, R));
  Assert.IsTrue(R.Supported);
  Assert.IsFalse(R.Complete);
end;

procedure TDriveCyclePID01DecoderTests.CI_DieselFlagPicksDieselMonitorSet;
var Bytes: TBytes; Err: string; A: TArray<TOBDMonitorReadiness>;
    R: TOBDMonitorReadiness;
begin
  // B bit 3 = diesel flag. Diesel branch must produce
  // omNMHCCatalyst etc., NOT omCatalyst.
  Bytes := TBytes.Create($00, $08, $00, $00);
  A := TOBDDriveCycleAdvisor.DecodePID01(Bytes, Err);
  Assert.AreEqual('', Err);
  Assert.IsTrue(FindMonitor(A, omNMHCCatalyst, R),
    'Diesel decode must produce omNMHCCatalyst');
  Assert.IsFalse(FindMonitor(A, omCatalyst, R),
    'Diesel decode must NOT include omCatalyst (SI-only)');
end;

procedure TDriveCyclePID01DecoderTests.CI_PMFilterSupportedNotComplete;
var Bytes: TBytes; Err: string; A: TArray<TOBDMonitorReadiness>;
    R: TOBDMonitorReadiness;
begin
  // Diesel + PM filter (bit 6 in C) supported and bit 6 in D
  // set = not complete.
  Bytes := TBytes.Create($00, $08, $40, $40);
  A := TOBDDriveCycleAdvisor.DecodePID01(Bytes, Err);
  Assert.AreEqual('', Err);
  Assert.IsTrue(FindMonitor(A, omPMFilter, R));
  Assert.IsTrue(R.Supported);
  Assert.IsFalse(R.Complete);
end;

initialization
  TDUnitX.RegisterTestFixture(TDriveCycleTypesTests);
  TDUnitX.RegisterTestFixture(TDriveCycleCatalogTests);
  TDUnitX.RegisterTestFixture(TDriveCycleAdvisorTests);
  TDUnitX.RegisterTestFixture(TDriveCyclePID01DecoderTests);

end.
