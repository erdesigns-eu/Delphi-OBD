//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Session
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.Session;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TStandardSessionTests = class
  public
    /// <summary>
    ///   Extended session emits header then10 03.
    /// </summary>
    [Test] procedure ExtendedSessionEmitsHeaderThen10_03;
    /// <summary>
    ///   Default session has no heartbeat.
    /// </summary>
    [Test] procedure DefaultSessionHasNoHeartbeat;
    /// <summary>
    ///   Non default session uses iso14229 heartbeat.
    /// </summary>
    [Test] procedure NonDefaultSessionUsesIso14229Heartbeat;
    /// <summary>
    ///   End session returns to10 01.
    /// </summary>
    [Test] procedure EndSessionReturnsTo10_01;
    /// <summary>
    ///   Programming requires security access.
    /// </summary>
    [Test] procedure ProgrammingRequiresSecurityAccess;
    /// <summary>
    ///   Extended does not require security access by default.
    /// </summary>
    [Test] procedure ExtendedDoesNotRequireSecurityAccessByDefault;
    /// <summary>
    ///   Zero ecu address omits header.
    /// </summary>
    [Test] procedure ZeroEcuAddressOmitsHeader;
  end;

  [TestFixture]
  TPerOEMSessionTests = class
  public
    /// <summary>
    ///   V w plan sets header and c r a.
    /// </summary>
    [Test] procedure VWPlanSetsHeaderAndCRA;
    /// <summary>
    ///   B m w requires security access for extended.
    /// </summary>
    [Test] procedure BMWRequiresSecurityAccessForExtended;
    /// <summary>
    ///   B m w heartbeat is1500ms.
    /// </summary>
    [Test] procedure BMWHeartbeatIs1500ms;
    /// <summary>
    ///   Mercedes appends f198 probe.
    /// </summary>
    [Test] procedure MercedesAppendsF198Probe;
    /// <summary>
    ///   Mercedes heartbeat is1500ms.
    /// </summary>
    [Test] procedure MercedesHeartbeatIs1500ms;
    /// <summary>
    ///   Ford prepends s t32 for programming.
    /// </summary>
    [Test] procedure FordPrependsST32ForProgramming;
    /// <summary>
    ///   Ford extended has no s t32.
    /// </summary>
    [Test] procedure FordExtendedHasNoST32;
    /// <summary>
    ///   G m prepends s p6.
    /// </summary>
    [Test] procedure GMPrependsSP6;
    /// <summary>
    ///   Stellantis appends f198 with empty expected.
    /// </summary>
    [Test] procedure StellantisAppendsF198WithEmptyExpected;
    /// <summary>
    ///   Extension resolves to o e m negotiator.
    /// </summary>
    [Test] procedure ExtensionResolvesToOEMNegotiator;
    /// <summary>
    ///   Session negotiator is cached across calls.
    /// </summary>
    [Test] procedure SessionNegotiatorIsCachedAcrossCalls;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.Session,
  OBD.OEM.VW, OBD.OEM.BMW, OBD.OEM.Mercedes,
  OBD.OEM.Ford, OBD.OEM.GM, OBD.OEM.Stellantis;

//------------------------------------------------------------------------------
// FIND UDSSTEP
//------------------------------------------------------------------------------
function FindUDSStep(const Plan: TOBDSessionPlan;
  const Prefix: TBytes): Boolean;
var
  Step: TOBDSessionStep;
  I: Integer;
  Match: Boolean;
begin
  for Step in Plan.Steps do
  begin
    if Step.Kind <> sskUDSRequest then Continue;
    if Length(Step.UDS) < Length(Prefix) then Continue;
    Match := True;
    for I := 0 to High(Prefix) do
      if Step.UDS[I] <> Prefix[I] then begin Match := False; Break; end;
    if Match then Exit(True);
  end;
  Result := False;
end;

//------------------------------------------------------------------------------
// FIND ATSTEP
//------------------------------------------------------------------------------
function FindATStep(const Plan: TOBDSessionPlan;
  const Cmd: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Plan.Steps) do
    if (Plan.Steps[I].Kind = sskATCommand) and
       SameText(Plan.Steps[I].AdapterCmd, Cmd) then
      Exit(I);
  Result := -1;
end;

//==============================================================================
// TStandardSessionTests
//==============================================================================

//------------------------------------------------------------------------------
// EXTENDED SESSION EMITS HEADER THEN10_03
//------------------------------------------------------------------------------
procedure TStandardSessionTests.ExtendedSessionEmitsHeaderThen10_03;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDStandardSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Assert.AreEqual(2, Length(Plan.Steps));
  Assert.AreEqual(Ord(sskATCommand), Ord(Plan.Steps[0].Kind));
  Assert.AreEqual('SH 7E0', Plan.Steps[0].AdapterCmd);
  Assert.AreEqual(Ord(sskUDSRequest), Ord(Plan.Steps[1].Kind));
  Assert.AreEqual(Byte($10), Plan.Steps[1].UDS[0]);
  Assert.AreEqual(Byte($03), Plan.Steps[1].UDS[1]);
  Assert.AreEqual(Byte($50), Plan.Steps[1].ExpectedResponse[0]);
end;

//------------------------------------------------------------------------------
// DEFAULT SESSION HAS NO HEARTBEAT
//------------------------------------------------------------------------------
procedure TStandardSessionTests.DefaultSessionHasNoHeartbeat;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDStandardSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstDefault, $7E0);
  Assert.AreEqual(Cardinal(0), Plan.TesterPresentMs);
end;

//------------------------------------------------------------------------------
// NON DEFAULT SESSION USES ISO14229 HEARTBEAT
//------------------------------------------------------------------------------
procedure TStandardSessionTests.NonDefaultSessionUsesIso14229Heartbeat;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDStandardSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Assert.AreEqual(Cardinal(2000), Plan.TesterPresentMs);
  Assert.AreEqual(2, Length(Plan.TesterPresentRequest));
  Assert.AreEqual(Byte($3E), Plan.TesterPresentRequest[0]);
  Assert.AreEqual(Byte($80), Plan.TesterPresentRequest[1]);
end;

//------------------------------------------------------------------------------
// END SESSION RETURNS TO10_01
//------------------------------------------------------------------------------
procedure TStandardSessionTests.EndSessionReturnsTo10_01;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDStandardSessionNegotiator.Create;
  Plan := N.EndSessionPlan($7E0);
  Assert.IsTrue(FindUDSStep(Plan, TBytes.Create($10, $01)),
    'EndSession plan must include a 10 01 request');
  Assert.AreEqual(Cardinal(0), Plan.TesterPresentMs);
end;

//------------------------------------------------------------------------------
// PROGRAMMING REQUIRES SECURITY ACCESS
//------------------------------------------------------------------------------
procedure TStandardSessionTests.ProgrammingRequiresSecurityAccess;
var
  N: IOBDSessionNegotiator;
begin
  N := TOBDStandardSessionNegotiator.Create;
  Assert.IsTrue(N.RequiresSecurityAccess(sstProgramming));
end;

//------------------------------------------------------------------------------
// EXTENDED DOES NOT REQUIRE SECURITY ACCESS BY DEFAULT
//------------------------------------------------------------------------------
procedure TStandardSessionTests.ExtendedDoesNotRequireSecurityAccessByDefault;
var
  N: IOBDSessionNegotiator;
begin
  N := TOBDStandardSessionNegotiator.Create;
  Assert.IsFalse(N.RequiresSecurityAccess(sstExtendedDiagnostic));
end;

//------------------------------------------------------------------------------
// ZERO ECU ADDRESS OMITS HEADER
//------------------------------------------------------------------------------
procedure TStandardSessionTests.ZeroEcuAddressOmitsHeader;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDStandardSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, 0);
  Assert.AreEqual(1, Length(Plan.Steps));
  Assert.AreEqual(Ord(sskUDSRequest), Ord(Plan.Steps[0].Kind));
end;

//==============================================================================
// TPerOEMSessionTests
//==============================================================================

//------------------------------------------------------------------------------
// VWPLAN SETS HEADER AND CRA
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.VWPlanSetsHeaderAndCRA;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDVWSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Assert.IsTrue(FindATStep(Plan, 'SH 7E0') >= 0, 'expected SH 7E0');
  Assert.IsTrue(FindATStep(Plan, 'CRA 7E8') >= 0, 'expected CRA 7E8');
  Assert.IsTrue(FindUDSStep(Plan, TBytes.Create($10, $03)));
end;

//------------------------------------------------------------------------------
// BMWREQUIRES SECURITY ACCESS FOR EXTENDED
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.BMWRequiresSecurityAccessForExtended;
var
  N: IOBDSessionNegotiator;
begin
  N := TOBDBMWSessionNegotiator.Create;
  Assert.IsTrue(N.RequiresSecurityAccess(sstExtendedDiagnostic));
  Assert.IsTrue(N.RequiresSecurityAccess(sstProgramming));
end;

//------------------------------------------------------------------------------
// BMWHEARTBEAT IS1500MS
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.BMWHeartbeatIs1500ms;
var
  N: IOBDSessionNegotiator;
begin
  N := TOBDBMWSessionNegotiator.Create;
  Assert.AreEqual(Cardinal(1500), N.DefaultTesterPresentMs);
end;

//------------------------------------------------------------------------------
// MERCEDES APPENDS F198 PROBE
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.MercedesAppendsF198Probe;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDMercedesSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Assert.IsTrue(FindUDSStep(Plan, TBytes.Create($22, $F1, $98)),
    'Mercedes plan must include a 22 F1 98 probe');
end;

//------------------------------------------------------------------------------
// MERCEDES HEARTBEAT IS1500MS
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.MercedesHeartbeatIs1500ms;
var
  N: IOBDSessionNegotiator;
begin
  N := TOBDMercedesSessionNegotiator.Create;
  Assert.AreEqual(Cardinal(1500), N.DefaultTesterPresentMs);
end;

//------------------------------------------------------------------------------
// FORD PREPENDS ST32 FOR PROGRAMMING
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.FordPrependsST32ForProgramming;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDFordSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstProgramming, $7E0);
  Assert.IsTrue(FindATStep(Plan, 'ST 32') >= 0,
    'Ford programming plan must prepend ST 32');
end;

//------------------------------------------------------------------------------
// FORD EXTENDED HAS NO ST32
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.FordExtendedHasNoST32;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDFordSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Assert.AreEqual(-1, FindATStep(Plan, 'ST 32'),
    'Ford extended-diagnostic should NOT carry the programming-only ST 32');
end;

//------------------------------------------------------------------------------
// GMPREPENDS SP6
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.GMPrependsSP6;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
begin
  N := TOBDGMSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Assert.IsTrue(FindATStep(Plan, 'SP 6') >= 0,
    'GM plan must lock to GMLAN protocol via SP 6');
end;

//------------------------------------------------------------------------------
// STELLANTIS APPENDS F198 WITH EMPTY EXPECTED
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.StellantisAppendsF198WithEmptyExpected;
var
  N: IOBDSessionNegotiator;
  Plan: TOBDSessionPlan;
  Step: TOBDSessionStep;
  Found: Boolean;
begin
  N := TOBDStellantisSessionNegotiator.Create;
  Plan := N.BeginSessionPlan(sstExtendedDiagnostic, $7E0);
  Found := False;
  for Step in Plan.Steps do
    if (Step.Kind = sskUDSRequest) and (Length(Step.UDS) = 3) and
       (Step.UDS[0] = $22) and (Step.UDS[1] = $F1) and (Step.UDS[2] = $98) then
    begin
      Found := True;
      Assert.AreEqual(0, Length(Step.ExpectedResponse),
        'Stellantis F198 probe must allow FCA NACKs (no expected prefix)');
    end;
  Assert.IsTrue(Found, 'Stellantis plan must include the F198 probe');
end;

//------------------------------------------------------------------------------
// EXTENSION RESOLVES TO OEMNEGOTIATOR
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.ExtensionResolvesToOEMNegotiator;
var
  Ext: IOBDOEMExtension;
begin
  Ext := TOBDOEMExtensionVW.Create;
  Assert.IsTrue(Pos('VAG', Ext.SessionNegotiator.DisplayName) > 0,
    'VW extension must return its OEM negotiator');

  Ext := TOBDOEMExtensionBMW.Create;
  Assert.IsTrue(Pos('BMW', Ext.SessionNegotiator.DisplayName) > 0);

  Ext := TOBDOEMExtensionMercedes.Create;
  Assert.IsTrue(Pos('Mercedes', Ext.SessionNegotiator.DisplayName) > 0);

  Ext := TOBDOEMExtensionFord.Create;
  Assert.IsTrue(Pos('Ford', Ext.SessionNegotiator.DisplayName) > 0);

  Ext := TOBDOEMExtensionGM.Create;
  Assert.IsTrue(Pos('GM', Ext.SessionNegotiator.DisplayName) > 0);

  Ext := TOBDOEMExtensionStellantis.Create;
  Assert.IsTrue(Pos('Stellantis', Ext.SessionNegotiator.DisplayName) > 0);
end;

//------------------------------------------------------------------------------
// SESSION NEGOTIATOR IS CACHED ACROSS CALLS
//------------------------------------------------------------------------------
procedure TPerOEMSessionTests.SessionNegotiatorIsCachedAcrossCalls;
var
  Ext: IOBDOEMExtension;
  A, B: IOBDSessionNegotiator;
begin
  Ext := TOBDOEMExtensionVW.Create;
  A := Ext.SessionNegotiator;
  B := Ext.SessionNegotiator;
  Assert.IsTrue(A = B, 'extensions must cache their session negotiator');
end;

initialization
  TDUnitX.RegisterTestFixture(TStandardSessionTests);
  TDUnitX.RegisterTestFixture(TPerOEMSessionTests);

end.
