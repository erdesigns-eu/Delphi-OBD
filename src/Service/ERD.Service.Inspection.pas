// ------------------------------------------------------------------------------
// ERD.Service.Inspection
//
// Loads national periodic-inspection OBD rules and evaluates a vehicle snapshot.
//
// The catalogue intentionally carries unverified research flags so host
// applications can show source and confidence alongside any pass/fail advisory.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - catalogs/inspection/rules.json
// - EU Directive 2014/45/EU periodic roadworthiness tests
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.Service.Inspection;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  ERD.JSON,
  ERD.Service.DriveCycle.Types,
  ERD.Types,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.IOUtils,
  System.JSON,
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF};

type
  /// <summary>Three-state value used when a national rule could not be verified.</summary>
  TOBDInspectionTriState = (itsUnknown, itsNo, itsYes);

  /// <summary>Fuel group used by the inspection rules evaluator.</summary>
  TOBDInspectionFuelType = (iftPetrol, iftDiesel, iftOther);

  /// <summary>Set of supported fuel groups for a country rule.</summary>
  TOBDInspectionFuelTypes = set of TOBDInspectionFuelType;

  /// <summary>Overall advisory verdict for a periodic inspection OBD check.</summary>
  TOBDInspectionVerdict = (ivPass, ivFail, ivUnknown, ivNotApplicable);

  /// <summary>Machine-readable reason code explaining an advisory verdict.</summary>
  TOBDInspectionReasonKind = (irkCountryUnknown, irkFuelNotCovered,
    irkBeforeAppliesFrom, irkMILOn, irkConfirmedDTCs, irkReadinessIncomplete,
    irkReadinessToleranceUnknown, irkRuleDataUnknown);

  /// <summary>Nullable integer allowance for incomplete readiness monitors.</summary>
  TOBDInspectionMonitorAllowance = record
    /// <summary>True when Count contains a confirmed catalogue value.</summary>
    Known: Boolean;
    /// <summary>Maximum incomplete supported monitors allowed by the rule.</summary>
    Count: Integer;
  end;

  /// <summary>One rule entry loaded from catalogs/inspection/rules.json.</summary>
  TOBDInspectionRule = record
    /// <summary>ISO 3166-1 alpha-2 country code.</summary>
    CountryCode: string;
    /// <summary>English country name.</summary>
    CountryName: string;
    /// <summary>Local inspection programme name, such as APK, MOT or NCT.</summary>
    InspectionName: string;
    /// <summary>Original ISO date text from the catalogue, or empty.</summary>
    AppliesFromText: string;
    /// <summary>True when AppliesFrom was parsed from the catalogue.</summary>
    HasAppliesFrom: Boolean;
    /// <summary>First registration date from which this rule applies.</summary>
    AppliesFrom: TDateTime;
    /// <summary>Fuel groups covered by this rule.</summary>
    FuelTypes: TOBDInspectionFuelTypes;
    /// <summary>Whether an illuminated emission MIL fails the inspection.</summary>
    MILOnFails: TOBDInspectionTriState;
    /// <summary>Whether a MIL bulb/commanded-on check is part of the rule.</summary>
    MILBulbCheck: TOBDInspectionTriState;
    /// <summary>Whether readiness monitors are evaluated.</summary>
    ReadinessRequired: TOBDInspectionTriState;
    /// <summary>Maximum incomplete monitor allowance per fuel group.</summary>
    MaxIncomplete: array [TOBDInspectionFuelType] of TOBDInspectionMonitorAllowance;
    /// <summary>Readiness monitors excluded by the national rule.</summary>
    MonitorsExcluded: TOBDMonitorSet;
    /// <summary>Whether confirmed emission DTCs fail the inspection.</summary>
    DTCConfirmedFails: TOBDInspectionTriState;
    /// <summary>Whether OBD replaces a tailpipe or smoke test.</summary>
    OBDReplacesTailpipe: TOBDInspectionTriState;
    /// <summary>Whether OBD supplements a tailpipe or smoke test.</summary>
    OBDSupplementsTailpipe: TOBDInspectionTriState;
    /// <summary>Free-text vehicle applicability note from the catalogue.</summary>
    Vehicles: string;
    /// <summary>Free-text research note from the catalogue.</summary>
    Notes: string;
    /// <summary>Official or supporting source URLs.</summary>
    Sources: TArray<string>;
    /// <summary>True only after ERDesigns has checked the rule against source material.</summary>
    Verified: Boolean;
    /// <summary>Date on which the source was last reviewed.</summary>
    Checked: string;
  end;

  /// <summary>Single machine-readable reason with English text for UI display.</summary>
  TOBDInspectionReason = record
    /// <summary>Reason kind.</summary>
    Kind: TOBDInspectionReasonKind;
    /// <summary>English explanation.</summary>
    Text: string;
  end;

  /// <summary>Evaluation result for one vehicle against one country rule.</summary>
  TOBDInspectionResult = record
    /// <summary>Computed advisory verdict.</summary>
    Verdict: TOBDInspectionVerdict;
    /// <summary>Reasons that contributed to the verdict.</summary>
    Reasons: TArray<TOBDInspectionReason>;
    /// <summary>Number of supported, non-excluded monitors that are incomplete.</summary>
    IncompleteMonitorCount: Integer;
    /// <summary>Allowed incomplete monitors, or -1 when unknown or not checked.</summary>
    AllowedIncompleteMonitors: Integer;
    /// <summary>Country code of the evaluated rule.</summary>
    CountryCode: string;
    /// <summary>Inspection programme name of the evaluated rule.</summary>
    InspectionName: string;
    /// <summary>Rule sources copied from the catalogue.</summary>
    Sources: TArray<string>;
    /// <summary>True only when the catalogue rule is owner-verified.</summary>
    Verified: Boolean;
  end;

  /// <summary>Process-wide catalogue and evaluator for inspection OBD rules.</summary>
  TOBDInspectionCatalog = class
  strict private
    class var FRules: TDictionary<string, TOBDInspectionRule>;
    class var FLoaded: Boolean;
    class var FCatalogFile: string;
    class var FLock: TCriticalSection;
    class procedure EnsureLoaded; static;
    class procedure LoadFile(const AFile: string); static;
  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>JSON catalogue file. Defaults to catalogs/inspection/rules.json next to the executable.</summary>
    class property CatalogFile: string read FCatalogFile write FCatalogFile;

    /// <summary>Reloads the catalogue from CatalogFile.</summary>
    class procedure Reload; static;

    /// <summary>Looks up a rule by ISO 3166-1 alpha-2 country code.</summary>
    class function TryGetRule(const ACountryCode: string;
      out ARule: TOBDInspectionRule): Boolean; static;

    /// <summary>Returns loaded country codes.</summary>
    class function CountryCodes: TArray<string>; static;

    /// <summary>Registers or replaces a rule at runtime.</summary>
    class procedure RegisterRule(const ARule: TOBDInspectionRule); static;

    /// <summary>Evaluates one vehicle snapshot against the named country rule.</summary>
    class function EvaluateCountry(const ACountryCode: string;
      const AReadiness: TArray<TOBDMonitorReadiness>; AMILOn: Boolean;
      AConfirmedDTCCount: Integer; AFuelType: TOBDInspectionFuelType;
      AFirstRegistration: TDateTime): TOBDInspectionResult; static;

    /// <summary>Evaluates one vehicle snapshot against an already loaded rule.</summary>
    class function Evaluate(const ARule: TOBDInspectionRule;
      const AReadiness: TArray<TOBDMonitorReadiness>; AMILOn: Boolean;
      AConfirmedDTCCount: Integer; AFuelType: TOBDInspectionFuelType;
      AFirstRegistration: TDateTime): TOBDInspectionResult; static;
  end;

/// <summary>Stable JSON/display name for an inspection fuel group.</summary>
function InspectionFuelTypeName(AFuelType: TOBDInspectionFuelType): string;

/// <summary>Parses a fuel group name from JSON.</summary>
function TryParseInspectionFuelType(const AName: string;
  out AFuelType: TOBDInspectionFuelType): Boolean;

/// <summary>Stable machine-readable name for a verdict.</summary>
function InspectionVerdictName(AVerdict: TOBDInspectionVerdict): string;

/// <summary>Stable machine-readable name for a reason kind.</summary>
function InspectionReasonKindName(AKind: TOBDInspectionReasonKind): string;

implementation

const
  FUEL_NAMES: array [TOBDInspectionFuelType] of string = ('petrol', 'diesel',
    'other');
  VERDICT_NAMES: array [TOBDInspectionVerdict] of string = ('pass', 'fail',
    'unknown', 'not_applicable');
  REASON_NAMES: array [TOBDInspectionReasonKind] of string = ('country_unknown',
    'fuel_not_covered', 'before_applies_from', 'mil_on', 'confirmed_dtcs',
    'readiness_incomplete', 'readiness_tolerance_unknown', 'rule_data_unknown');

function InspectionFuelTypeName(AFuelType: TOBDInspectionFuelType): string;
begin
  Result := FUEL_NAMES[AFuelType];
end;

function TryParseInspectionFuelType(const AName: string;
  out AFuelType: TOBDInspectionFuelType): Boolean;
var
  F: TOBDInspectionFuelType;
begin
  for F := Low(TOBDInspectionFuelType) to High(TOBDInspectionFuelType) do
    if SameText(FUEL_NAMES[F], AName) then
    begin
      AFuelType := F;
      Exit(True);
    end;
  Result := False;
end;

function InspectionVerdictName(AVerdict: TOBDInspectionVerdict): string;
begin
  Result := VERDICT_NAMES[AVerdict];
end;

function InspectionReasonKindName(AKind: TOBDInspectionReasonKind): string;
begin
  Result := REASON_NAMES[AKind];
end;

function NormalizeCountryCode(const ACode: string): string;
begin
  Result := UpperCase(Trim(ACode));
end;

function JSONTriState(AObj: TJSONObject; const AKey: string): TOBDInspectionTriState;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AKey);
  if V = nil then
    Exit(itsUnknown);
  if V is TJSONNull then
    Exit(itsUnknown);
  if SameText(V.Value, 'true') then
    Exit(itsYes);
  if SameText(V.Value, 'false') then
    Exit(itsNo);
  Result := itsUnknown;
end;

function JSONString(AObj: TJSONObject; const AKey: string): string;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AKey);
  if (V = nil) or (V is TJSONNull) then
    Exit('');
  Result := V.Value;
end;

function JSONBool(AObj: TJSONObject; const AKey: string): Boolean;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AKey);
  Result := (V <> nil) and not(V is TJSONNull) and SameText(V.Value, 'true');
end;

function JSONIntValue(AValue: TJSONValue; out AResult: Integer): Boolean;
begin
  Result := False;
  AResult := 0;
  if (AValue = nil) or (AValue is TJSONNull) then
    Exit;
  if AValue is TJSONNumber then
  begin
    AResult := TJSONNumber(AValue).AsInt;
    Exit(True);
  end;
  Result := TryStrToInt(AValue.Value, AResult);
end;

function TryParseISODate(const AText: string; out ADate: TDateTime): Boolean;
var
  Year, Month, Day: Integer;
begin
  Result := False;
  ADate := 0;
  if Length(AText) < 10 then
    Exit;
  Year := StrToIntDef(Copy(AText, 1, 4), 0);
  Month := StrToIntDef(Copy(AText, 6, 2), 0);
  Day := StrToIntDef(Copy(AText, 9, 2), 0);
  try
    ADate := EncodeDate(Year, Month, Day);
    Result := True;
  except
    Result := False;
  end;
end;

procedure AddReason(var AResult: TOBDInspectionResult;
  AKind: TOBDInspectionReasonKind; const AText: string);
var
  N: Integer;
begin
  N := Length(AResult.Reasons);
  SetLength(AResult.Reasons, N + 1);
  AResult.Reasons[N].Kind := AKind;
  AResult.Reasons[N].Text := AText;
end;

function CountIncomplete(const AReadiness: TArray<TOBDMonitorReadiness>;
  AExcluded: TOBDMonitorSet): Integer;
var
  R: TOBDMonitorReadiness;
begin
  Result := 0;
  for R in AReadiness do
    if R.Supported and not R.Complete and not(R.Monitor in AExcluded) then
      Inc(Result);
end;

class constructor TOBDInspectionCatalog.Create;
var
  Root: string;
begin
  FLock := TCriticalSection.Create;
  FRules := TDictionary<string, TOBDInspectionRule>.Create;
  Root := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'catalogs');
  FCatalogFile := TPath.Combine(TPath.Combine(Root, 'inspection'), 'rules.json');
end;

class destructor TOBDInspectionCatalog.Destroy;
begin
  FreeAndNil(FRules);
  FreeAndNil(FLock);
end;

class procedure TOBDInspectionCatalog.EnsureLoaded;
begin
  if FLoaded then
    Exit;
  Reload;
end;

class procedure TOBDInspectionCatalog.LoadFile(const AFile: string);
var
  Doc: TJSONObject;
  Arr, Fuels, Excluded, Sources: TJSONArray;
  Item, MaxObj: TJSONObject;
  Rule: TOBDInspectionRule;
  Fuel: TOBDInspectionFuelType;
  Monitor: TOBDMonitor;
  Value: TJSONValue;
  I, N: Integer;
begin
  if not TFile.Exists(AFile) then
    Exit;

  Doc := ParseOBDJSONObject(TFile.ReadAllText(AFile, TEncoding.UTF8));
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    if Arr = nil then
      Exit;

    for I := 0 to Arr.Count - 1 do
    begin
      Item := RequireOBDJSONObject(Arr.Items[I]);
      Rule := Default(TOBDInspectionRule);
      Rule.CountryCode := NormalizeCountryCode(Item.GetValue<string>('country_code', ''));
      if Rule.CountryCode = '' then
        Continue;
      Rule.CountryName := Item.GetValue<string>('country_name', '');
      Rule.InspectionName := Item.GetValue<string>('inspection_name', '');
      Rule.AppliesFromText := JSONString(Item, 'applies_from');
      Rule.HasAppliesFrom := TryParseISODate(Rule.AppliesFromText,
        Rule.AppliesFrom);
      Rule.MILOnFails := JSONTriState(Item, 'mil_on_fails');
      Rule.MILBulbCheck := JSONTriState(Item, 'mil_bulb_check');
      Rule.ReadinessRequired := JSONTriState(Item, 'readiness_required');
      Rule.DTCConfirmedFails := JSONTriState(Item, 'dtc_confirmed_fails');
      Rule.OBDReplacesTailpipe := JSONTriState(Item, 'obd_replaces_tailpipe');
      Rule.OBDSupplementsTailpipe := JSONTriState(Item,
        'obd_supplements_tailpipe');
      Rule.Vehicles := Item.GetValue<string>('vehicles', '');
      Rule.Notes := Item.GetValue<string>('notes', '');
      Rule.Verified := JSONBool(Item, 'verified');
      Rule.Checked := Item.GetValue<string>('checked', '');

      Fuels := Item.GetValue<TJSONArray>('fuel_types');
      if Fuels <> nil then
        for N := 0 to Fuels.Count - 1 do
          if TryParseInspectionFuelType(Fuels.Items[N].Value, Fuel) then
            Include(Rule.FuelTypes, Fuel);

      Value := Item.GetValue('max_incomplete_monitors');
      if Value is TJSONObject then
      begin
        MaxObj := TJSONObject(Value);
        for Fuel := Low(TOBDInspectionFuelType) to High(TOBDInspectionFuelType) do
        begin
          Value := MaxObj.GetValue(InspectionFuelTypeName(Fuel));
          if JSONIntValue(Value, N) then
          begin
            Rule.MaxIncomplete[Fuel].Known := True;
            Rule.MaxIncomplete[Fuel].Count := N;
          end;
        end;
      end;

      Excluded := Item.GetValue<TJSONArray>('monitors_excluded');
      if Excluded <> nil then
        for N := 0 to Excluded.Count - 1 do
          if TryParseMonitor(Excluded.Items[N].Value, Monitor) then
            Include(Rule.MonitorsExcluded, Monitor);

      Sources := Item.GetValue<TJSONArray>('sources');
      if Sources <> nil then
      begin
        SetLength(Rule.Sources, Sources.Count);
        for N := 0 to Sources.Count - 1 do
          Rule.Sources[N] := Sources.Items[N].Value;
      end;

      FRules.AddOrSetValue(Rule.CountryCode, Rule);
    end;
  finally
    Doc.Free;
  end;
end;

class procedure TOBDInspectionCatalog.Reload;
begin
  FLock.Enter;
  try
    FRules.Clear;
    LoadFile(FCatalogFile);
    FLoaded := True;
  finally
    FLock.Leave;
  end;
end;

class function TOBDInspectionCatalog.TryGetRule(const ACountryCode: string;
  out ARule: TOBDInspectionRule): Boolean;
begin
  EnsureLoaded;
  Result := FRules.TryGetValue(NormalizeCountryCode(ACountryCode), ARule);
  if not Result then
    ARule := Default(TOBDInspectionRule);
end;

class function TOBDInspectionCatalog.CountryCodes: TArray<string>;
var
  Key: string;
  Acc: TList<string>;
begin
  EnsureLoaded;
  Acc := TList<string>.Create;
  try
    for Key in FRules.Keys do
      Acc.Add(Key);
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class procedure TOBDInspectionCatalog.RegisterRule(const ARule: TOBDInspectionRule);
var
  Rule: TOBDInspectionRule;
begin
  EnsureLoaded;
  Rule := ARule;
  Rule.CountryCode := NormalizeCountryCode(Rule.CountryCode);
  if Rule.CountryCode = '' then
    raise EOBDConfig.Create('Inspection rule country code cannot be empty');
  FLock.Enter;
  try
    FRules.AddOrSetValue(Rule.CountryCode, Rule);
  finally
    FLock.Leave;
  end;
end;

class function TOBDInspectionCatalog.EvaluateCountry(const ACountryCode: string;
  const AReadiness: TArray<TOBDMonitorReadiness>; AMILOn: Boolean;
  AConfirmedDTCCount: Integer; AFuelType: TOBDInspectionFuelType;
  AFirstRegistration: TDateTime): TOBDInspectionResult;
var
  Rule: TOBDInspectionRule;
begin
  if not TryGetRule(ACountryCode, Rule) then
  begin
    Result := Default(TOBDInspectionResult);
    Result.Verdict := ivUnknown;
    Result.CountryCode := NormalizeCountryCode(ACountryCode);
    Result.AllowedIncompleteMonitors := -1;
    AddReason(Result, irkCountryUnknown,
      'No inspection OBD rule is loaded for this country.');
    Exit;
  end;
  Result := Evaluate(Rule, AReadiness, AMILOn, AConfirmedDTCCount, AFuelType,
    AFirstRegistration);
end;

class function TOBDInspectionCatalog.Evaluate(const ARule: TOBDInspectionRule;
  const AReadiness: TArray<TOBDMonitorReadiness>; AMILOn: Boolean;
  AConfirmedDTCCount: Integer; AFuelType: TOBDInspectionFuelType;
  AFirstRegistration: TDateTime): TOBDInspectionResult;
var
  Failed, Unknown, HasCheck: Boolean;
  Allowance: TOBDInspectionMonitorAllowance;
begin
  Result := Default(TOBDInspectionResult);
  Result.Verdict := ivUnknown;
  Result.AllowedIncompleteMonitors := -1;
  Result.CountryCode := ARule.CountryCode;
  Result.InspectionName := ARule.InspectionName;
  Result.Sources := Copy(ARule.Sources);
  Result.Verified := ARule.Verified;

  if not(AFuelType in ARule.FuelTypes) then
  begin
    Result.Verdict := ivNotApplicable;
    AddReason(Result, irkFuelNotCovered,
      'The catalogue rule does not cover this fuel type.');
    Exit;
  end;

  if ARule.HasAppliesFrom and (AFirstRegistration > 0) and
    (AFirstRegistration < ARule.AppliesFrom) then
  begin
    Result.Verdict := ivNotApplicable;
    AddReason(Result, irkBeforeAppliesFrom,
      'The vehicle was first registered before this OBD inspection rule applies.');
    Exit;
  end;

  Failed := False;
  Unknown := False;
  HasCheck := False;

  case ARule.MILOnFails of
    itsYes:
      begin
        HasCheck := True;
        if AMILOn then
        begin
          Failed := True;
          AddReason(Result, irkMILOn,
            'The emission malfunction indicator lamp is on.');
        end;
      end;
    itsUnknown:
      if AMILOn then
      begin
        Unknown := True;
        AddReason(Result, irkRuleDataUnknown,
          'The catalogue does not confirm how this inspection treats an illuminated MIL.');
      end;
  end;

  case ARule.DTCConfirmedFails of
    itsYes:
      begin
        HasCheck := True;
        if AConfirmedDTCCount > 0 then
        begin
          Failed := True;
          AddReason(Result, irkConfirmedDTCs,
            'The vehicle reports confirmed emission DTCs.');
        end;
      end;
    itsUnknown:
      if AConfirmedDTCCount > 0 then
      begin
        Unknown := True;
        AddReason(Result, irkRuleDataUnknown,
          'The catalogue does not confirm how this inspection treats confirmed DTCs.');
      end;
  end;

  Result.IncompleteMonitorCount := CountIncomplete(AReadiness,
    ARule.MonitorsExcluded);

  case ARule.ReadinessRequired of
    itsYes:
      begin
        HasCheck := True;
        Allowance := ARule.MaxIncomplete[AFuelType];
        if Allowance.Known then
        begin
          Result.AllowedIncompleteMonitors := Allowance.Count;
          if Result.IncompleteMonitorCount > Allowance.Count then
          begin
            Failed := True;
            AddReason(Result, irkReadinessIncomplete,
              'Too many supported readiness monitors are incomplete.');
          end;
        end
        else if Result.IncompleteMonitorCount > 0 then
        begin
          Unknown := True;
          AddReason(Result, irkReadinessToleranceUnknown,
            'Readiness is checked, but the national incomplete-monitor allowance is unknown.');
        end;
      end;
    itsUnknown:
      begin
        Unknown := True;
        AddReason(Result, irkRuleDataUnknown,
          'The catalogue does not confirm whether readiness monitors are checked.');
      end;
  end;

  if Failed then
    Result.Verdict := ivFail
  else if Unknown or not HasCheck then
    Result.Verdict := ivUnknown
  else
    Result.Verdict := ivPass;
end;

end.
