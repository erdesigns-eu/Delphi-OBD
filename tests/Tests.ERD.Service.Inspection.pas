// ------------------------------------------------------------------------------
// Tests.ERD.Service.Inspection
//
// Unit tests for the periodic-inspection OBD rule catalogue and evaluator.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit Tests.ERD.Service.Inspection;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  DUnitX.TestFramework,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  System.IOUtils,
  ERD.Service.DriveCycle.Types,
  ERD.Service.Inspection;

type
  [TestFixture]
  TInspectionCatalogTests = class
  strict private
    function FindCatalogFile: string;
  public
    [Setup] procedure Setup;
    [Test] procedure LoadsNetherlandsRuleWithSources;
    [Test] procedure GBIgnoresReadinessButFailsMIL;
    [Test] procedure ReadinessAllowanceFailsWhenExceeded;
    [Test] procedure AppliesFromCanMakeRuleNotApplicable;
  end;

implementation

function TInspectionCatalogTests.FindCatalogFile: string;
var
  Current, Parent, Candidate: string;
begin
  Current := TPath.GetDirectoryName(ParamStr(0));
  while Current <> '' do
  begin
    Candidate := TPath.Combine(TPath.Combine(TPath.Combine(Current, 'catalogs'),
      'inspection'), 'rules.json');
    if TFile.Exists(Candidate) then
      Exit(Candidate);
    Parent := TPath.GetDirectoryName(Current);
    if Parent = Current then
      Break;
    Current := Parent;
  end;
  raise Exception.Create('Cannot locate inspection rule catalogue');
end;

procedure TInspectionCatalogTests.Setup;
begin
  TOBDInspectionCatalog.CatalogFile := FindCatalogFile;
  TOBDInspectionCatalog.Reload;
end;

procedure TInspectionCatalogTests.LoadsNetherlandsRuleWithSources;
var
  Rule: TOBDInspectionRule;
begin
  Assert.IsTrue(TOBDInspectionCatalog.TryGetRule('nl', Rule));
  Assert.AreEqual('NL', Rule.CountryCode);
  Assert.AreEqual('APK', Rule.InspectionName);
  Assert.IsFalse(Rule.Verified);
  Assert.IsTrue(Length(Rule.Sources) > 0);
end;

procedure TInspectionCatalogTests.GBIgnoresReadinessButFailsMIL;
var
  Readiness: TArray<TOBDMonitorReadiness>;
  Result_: TOBDInspectionResult;
begin
  SetLength(Readiness, 1);
  Readiness[0].Monitor := omCatalyst;
  Readiness[0].Supported := True;
  Readiness[0].Complete := False;

  Result_ := TOBDInspectionCatalog.EvaluateCountry('GB', Readiness, False, 0,
    iftPetrol, EncodeDate(2012, 1, 1));
  Assert.AreEqual(Ord(ivPass), Ord(Result_.Verdict));

  Result_ := TOBDInspectionCatalog.EvaluateCountry('GB', Readiness, True, 0,
    iftPetrol, EncodeDate(2012, 1, 1));
  Assert.AreEqual(Ord(ivFail), Ord(Result_.Verdict));
  Assert.AreEqual(Ord(irkMILOn), Ord(Result_.Reasons[0].Kind));
end;

procedure TInspectionCatalogTests.ReadinessAllowanceFailsWhenExceeded;
var
  Rule: TOBDInspectionRule;
  Readiness: TArray<TOBDMonitorReadiness>;
  Result_: TOBDInspectionResult;
begin
  Rule := Default(TOBDInspectionRule);
  Rule.CountryCode := 'ZZ';
  Rule.CountryName := 'Fixture';
  Rule.InspectionName := 'Fixture inspection';
  Rule.FuelTypes := [iftPetrol];
  Rule.MILOnFails := itsNo;
  Rule.DTCConfirmedFails := itsNo;
  Rule.ReadinessRequired := itsYes;
  Rule.MaxIncomplete[iftPetrol].Known := True;
  Rule.MaxIncomplete[iftPetrol].Count := 1;
  Rule.Verified := True;
  TOBDInspectionCatalog.RegisterRule(Rule);

  SetLength(Readiness, 2);
  Readiness[0].Monitor := omCatalyst;
  Readiness[0].Supported := True;
  Readiness[0].Complete := False;
  Readiness[1].Monitor := omOxygenSensor;
  Readiness[1].Supported := True;
  Readiness[1].Complete := False;

  Result_ := TOBDInspectionCatalog.EvaluateCountry('ZZ', Readiness, False, 0,
    iftPetrol, EncodeDate(2020, 1, 1));
  Assert.AreEqual(Ord(ivFail), Ord(Result_.Verdict));
  Assert.AreEqual(2, Result_.IncompleteMonitorCount);
  Assert.AreEqual(1, Result_.AllowedIncompleteMonitors);
end;

procedure TInspectionCatalogTests.AppliesFromCanMakeRuleNotApplicable;
var
  Result_: TOBDInspectionResult;
begin
  Result_ := TOBDInspectionCatalog.EvaluateCountry('GB', nil, False, 0,
    iftPetrol, EncodeDate(2001, 1, 1));
  Assert.AreEqual(Ord(ivNotApplicable), Ord(Result_.Verdict));
  Assert.AreEqual(Ord(irkBeforeAppliesFrom), Ord(Result_.Reasons[0].Kind));
end;

initialization
  TDUnitX.RegisterTestFixture(TInspectionCatalogTests);

end.
