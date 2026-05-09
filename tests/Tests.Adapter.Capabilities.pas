//------------------------------------------------------------------------------
// UNIT           : Tests.Adapter.Capabilities
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Adapter.Capabilities;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TAdapterCapabilitiesTests = class
  public
    [Test] procedure ELM327DoesNotClaimCANFD;
    [Test] procedure OBDLinkEXClaimsCANFD;
    [Test] procedure DoIPGatewayHasNoKLine;
    [Test] procedure UnknownAdapterReturnsFalse;
    [Test] procedure ResolveIsoTpFallsBackToSeven;
    [Test] procedure ResolveIsoTpReturnsSixtyTwoForCANFDAdapter;
    [Test] procedure RegisterIsCaseInsensitive;
    [Test] procedure SetToStringContainsCAN;
    [Test] procedure RegisterReplacesExisting;
  end;

implementation

uses
  System.SysUtils, OBD.Adapter.Capabilities;

procedure TAdapterCapabilitiesTests.ELM327DoesNotClaimCANFD;
begin
  Assert.IsTrue(AdapterSupports('elm327', acCAN));
  Assert.IsFalse(AdapterSupports('elm327', acCANFD));
end;

procedure TAdapterCapabilitiesTests.OBDLinkEXClaimsCANFD;
begin
  Assert.IsTrue(AdapterSupports('obdlink_ex', acCAN));
  Assert.IsTrue(AdapterSupports('obdlink_ex', acCANFD));
  Assert.IsTrue(AdapterSupports('obdlink_ex', acISOTPLargeFrame));
end;

procedure TAdapterCapabilitiesTests.DoIPGatewayHasNoKLine;
begin
  Assert.IsTrue(AdapterSupports('doip_gateway', acDoIP));
  Assert.IsFalse(AdapterSupports('doip_gateway', acKLine));
end;

procedure TAdapterCapabilitiesTests.UnknownAdapterReturnsFalse;
var Caps: TOBDAdapterCapabilities;
begin
  Assert.IsFalse(FindAdapterCapabilities('does-not-exist', Caps));
  Assert.IsFalse(AdapterSupports('does-not-exist', acCAN));
end;

procedure TAdapterCapabilitiesTests.ResolveIsoTpFallsBackToSeven;
begin
  Assert.AreEqual(7, ResolveIsoTpFrameBytes('elm327'));
  Assert.AreEqual(7, ResolveIsoTpFrameBytes('does-not-exist'));
end;

procedure TAdapterCapabilitiesTests.ResolveIsoTpReturnsSixtyTwoForCANFDAdapter;
begin
  Assert.AreEqual(62, ResolveIsoTpFrameBytes('obdlink_ex'));
end;

procedure TAdapterCapabilitiesTests.RegisterIsCaseInsensitive;
begin
  Assert.IsTrue(AdapterSupports('ELM327', acCAN));
  Assert.IsTrue(AdapterSupports('Elm327', acCAN));
end;

procedure TAdapterCapabilitiesTests.SetToStringContainsCAN;
var
  S: string;
begin
  S := CapabilitySetToString([acCAN, acISOTP, acVoltageMonitor]);
  Assert.IsTrue(S.Contains('CAN'));
  Assert.IsTrue(S.Contains('ISO-TP'));
end;

procedure TAdapterCapabilitiesTests.RegisterReplacesExisting;
var
  R: TOBDAdapterCapabilities;
begin
  R.AdapterKey := 'test_replace';
  R.DisplayName := 'first';
  R.CapSet := [acCAN];
  R.MaxIsoTpFrameBytes := 7;
  RegisterAdapterCapabilities(R);
  R.DisplayName := 'second';
  R.CapSet := [acCAN, acCANFD];
  R.MaxIsoTpFrameBytes := 62;
  RegisterAdapterCapabilities(R);
  Assert.IsTrue(AdapterSupports('test_replace', acCANFD));
end;

initialization
  TDUnitX.RegisterTestFixture(TAdapterCapabilitiesTests);

end.
