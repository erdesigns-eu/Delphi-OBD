//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Common.pas
// CONTENTS       : Unified coding / WriteDataByIdentifier API. The
//                  parallel of OBD.OEM.ServiceFunction (v3.25), but
//                  for SID 0x2E WriteDataByIdentifier flows: matches
//                  OEM-specific DID names ('fa_assembly',
//                  'as_built_code', 'mazda_as_built_code', 'fca_proxi'...)
//                  to a canonical TOBDCodingFunctionKind so a tool
//                  can call FindCodingFunction(OEM, cfVehicleOrder)
//                  once and have it work across every OEM extension
//                  that ships an FA / option / market block.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Common;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.OEM;

const
  /// <summary>UDS SID 0x2E — WriteDataByIdentifier.</summary>
  UDS_SID_WRITE_DATA_BY_IDENTIFIER = $2E;

type
  /// <summary>Canonical coding-function kinds. The set of writeable
  /// DIDs a coding tool typically exposes as one-tap actions.</summary>
  TOBDCodingFunctionKind = (
    cfUnknown,
    cfVehicleOrder,         // BMW FA / Bentley commission / SALAPA option codes
    cfAsBuiltCode,          // Ford / Mazda As-Built configuration block
    cfFcaProxi,             // FCA wiTech proxi alignment
    cfMarketRegion,         // Mazda / VW / GM market / region code
    cfStarlightPattern,     // Rolls-Royce Starlight Headliner pattern
    cfDaytimeRunningLights, // VW / VAG long-coding bit, GM RPO bit
    cfAutoLockUnlock,       // VW / Ford / GM convenience-lock toggles
    cfRearFogLamp,          // EU rear-fog lamp enable (NA cars)
    cfNeedleSweep,          // VAG cluster needle sweep on key-on
    cfAdaptiveCruiseEnable, // Latent ACC enable bit
    cfLaneAssistEnable,     // Latent lane-keep enable bit
    cfTpmsThreshold,        // TPMS warning threshold
    cfHeadlightCountry,     // Country-specific headlight beam pattern
    cfTraileMode,           // Trailer-mode enable / option block
    cfLanguageCode,         // Cluster + IVI language
    cfUnitsImperial,        // mph / mpg / Fahrenheit toggle
    cfTpmsCalibration,      // TPMS calibration block (writeable threshold)
    cfWindowComfort,        // One-touch window / comfort closing
    cfSofttopAuto           // Convertible / soft-top automatic operation
  );

  /// <summary>Resolved coding function — one writeable DID the OEM
  /// extension exposes that matches a canonical kind.</summary>
  TOBDCodingFunction = record
    Kind: TOBDCodingFunctionKind;
    DataIdentifier: Word;
    DidName: string;
    Description: string;
    EcuAddress: Word;
  end;

  /// <summary>Registry of canonical kind → DID-name-token mappings.</summary>
  TOBDCodingFunctionRegistry = class
  strict private class var
    FTokens: TObjectDictionary<TOBDCodingFunctionKind, TList<string>>;
    class procedure EnsureInitialized;
    class procedure RegisterTokens(const Kind: TOBDCodingFunctionKind;
      const Names: array of string);
  public
    /// <summary>True if <c>Name</c> matches a known token for <c>Kind</c>
    /// (case-insensitive substring match).</summary>
    class function NameMatchesKind(const Name: string;
      const Kind: TOBDCodingFunctionKind): Boolean;
    /// <summary>Best-guess canonical kind for an arbitrary DID
    /// name. Returns cfUnknown when no token matches.</summary>
    class function ClassifyName(const Name: string): TOBDCodingFunctionKind;
  end;

/// <summary>Find the first DID in the OEM extension's catalog that
/// maps to <c>Kind</c>. Returns <c>True</c> + populates <c>Func</c>;
/// returns <c>False</c> when no matching DID exists.</summary>
function FindCodingFunction(const Ext: IOBDOEMExtension;
  const Kind: TOBDCodingFunctionKind;
  out Func: TOBDCodingFunction): Boolean;

/// <summary>Discover every coding-classifiable DID on the OEM
/// extension. Useful for populating a "Coding" menu without
/// hard-coding which OEMs support what.</summary>
function ListCodingFunctions(
  const Ext: IOBDOEMExtension): TArray<TOBDCodingFunction>;

/// <summary>Build the WriteDataByIdentifier UDS frame
/// (<c>2E DID-hi DID-lo data...</c>).</summary>
function BuildWriteDataByIdentifier(const DID: Word;
  const Data: TBytes): TBytes;

/// <summary>Build a coding-write UDS frame for a resolved coding
/// function.</summary>
function BuildCodingFrame(const Func: TOBDCodingFunction;
  const Data: TBytes): TBytes;

/// <summary>Parse a positive WriteDataByIdentifier response
/// (<c>6E DID-hi DID-lo</c>). Returns <c>True</c> when the response
/// confirms the requested DID, <c>False</c> on negative response or
/// SID/DID mismatch.</summary>
function ParseCodingResponse(const Response: TBytes;
  const ExpectedDID: Word): Boolean;

/// <summary>Display label for a coding-function kind (used by UI).</summary>
function CodingFunctionKindName(const Kind: TOBDCodingFunctionKind): string;

implementation

class procedure TOBDCodingFunctionRegistry.EnsureInitialized;
begin
  if FTokens = nil then
  begin
    FTokens := TObjectDictionary<TOBDCodingFunctionKind, TList<string>>.Create([doOwnsValues]);
    RegisterTokens(cfVehicleOrder,
      ['fa_assembly', 'fa_write', 'salapa', 'commission_no',
       'commission_number', 'vehicle_order', 'bentley_commission']);
    RegisterTokens(cfAsBuiltCode,
      ['as_built', 'as_built_code', 'mazda_as_built',
       'ford_as_built', 'gm_rpo']);
    RegisterTokens(cfFcaProxi,
      ['witech_proxi', 'fca_proxi', 'proxi_align']);
    RegisterTokens(cfMarketRegion,
      ['market_code', 'region_code', 'mazda_market_code',
       'subaru_market_code', 'mitsu_market_code']);
    RegisterTokens(cfStarlightPattern,
      ['starlight_pattern', 'rr_starlight']);
    RegisterTokens(cfDaytimeRunningLights,
      ['drl', 'daytime_running_lights', 'daytime_lamp']);
    RegisterTokens(cfAutoLockUnlock,
      ['auto_lock', 'auto_unlock', 'comfort_lock']);
    RegisterTokens(cfRearFogLamp,
      ['rear_fog_lamp', 'rear_fog']);
    RegisterTokens(cfNeedleSweep,
      ['needle_sweep', 'staging_animation', 'cluster_sweep']);
    RegisterTokens(cfAdaptiveCruiseEnable,
      ['acc_enable', 'adaptive_cruise_enable']);
    RegisterTokens(cfLaneAssistEnable,
      ['lane_assist_enable', 'lks_enable', 'lka_enable']);
    RegisterTokens(cfTpmsThreshold,
      ['tpms_threshold', 'rdc_threshold']);
    RegisterTokens(cfHeadlightCountry,
      ['headlight_country', 'headlamp_country', 'beam_pattern']);
    RegisterTokens(cfTraileMode,
      ['trailer_mode_enable', 'trailer_option']);
    RegisterTokens(cfLanguageCode,
      ['language_code', 'cluster_language']);
    RegisterTokens(cfUnitsImperial,
      ['units_imperial', 'imperial_units', 'unit_system']);
    RegisterTokens(cfTpmsCalibration,
      ['tpms_calibration_block', 'rdc_calibration_block']);
    RegisterTokens(cfWindowComfort,
      ['window_comfort', 'one_touch_window']);
    RegisterTokens(cfSofttopAuto,
      ['softtop_auto', 'cabrio_top_auto', 'roof_auto']);
  end;
end;

class procedure TOBDCodingFunctionRegistry.RegisterTokens(
  const Kind: TOBDCodingFunctionKind; const Names: array of string);
var
  L: TList<string>;
  S: string;
begin
  L := TList<string>.Create;
  for S in Names do L.Add(LowerCase(S));
  FTokens.AddOrSetValue(Kind, L);
end;

class function TOBDCodingFunctionRegistry.NameMatchesKind(
  const Name: string; const Kind: TOBDCodingFunctionKind): Boolean;
var
  Tokens: TList<string>;
  Token, Lower: string;
begin
  EnsureInitialized;
  Result := False;
  if not FTokens.TryGetValue(Kind, Tokens) then Exit;
  Lower := LowerCase(Name);
  for Token in Tokens do
    if Pos(Token, Lower) > 0 then Exit(True);
end;

class function TOBDCodingFunctionRegistry.ClassifyName(
  const Name: string): TOBDCodingFunctionKind;
var
  Kind: TOBDCodingFunctionKind;
begin
  EnsureInitialized;
  for Kind := Succ(Low(TOBDCodingFunctionKind)) to High(TOBDCodingFunctionKind) do
    if NameMatchesKind(Name, Kind) then Exit(Kind);
  Result := cfUnknown;
end;

function CodingFunctionKindName(const Kind: TOBDCodingFunctionKind): string;
begin
  case Kind of
    cfVehicleOrder:         Result := 'Vehicle Order / FA / Commission';
    cfAsBuiltCode:          Result := 'As-Built Configuration Code';
    cfFcaProxi:             Result := 'FCA wiTech Proxi Alignment';
    cfMarketRegion:         Result := 'Market / Region Code';
    cfStarlightPattern:     Result := 'Rolls-Royce Starlight Pattern';
    cfDaytimeRunningLights: Result := 'Daytime Running Lights';
    cfAutoLockUnlock:       Result := 'Auto Lock / Unlock';
    cfRearFogLamp:          Result := 'Rear Fog Lamp';
    cfNeedleSweep:          Result := 'Cluster Needle-Sweep Animation';
    cfAdaptiveCruiseEnable: Result := 'Adaptive Cruise Latent Enable';
    cfLaneAssistEnable:     Result := 'Lane-Assist Latent Enable';
    cfTpmsThreshold:        Result := 'TPMS Warning Threshold';
    cfHeadlightCountry:     Result := 'Headlight Country / Beam Pattern';
    cfTraileMode:           Result := 'Trailer Mode';
    cfLanguageCode:         Result := 'Cluster / IVI Language';
    cfUnitsImperial:        Result := 'Imperial Units Toggle';
    cfTpmsCalibration:      Result := 'TPMS Calibration Block';
    cfWindowComfort:        Result := 'Comfort Window / One-Touch';
    cfSofttopAuto:          Result := 'Convertible Soft-Top Auto';
  else
    Result := 'Unknown Coding Function';
  end;
end;

function FindCodingFunction(const Ext: IOBDOEMExtension;
  const Kind: TOBDCodingFunctionKind;
  out Func: TOBDCodingFunction): Boolean;
var
  D: TOBDOEMDataIdentifier;
begin
  Func := Default(TOBDCodingFunction);
  Result := False;
  if Ext = nil then Exit;
  for D in Ext.DataIdentifiers do
    if TOBDCodingFunctionRegistry.NameMatchesKind(D.Name, Kind) then
    begin
      Func.Kind           := Kind;
      Func.DataIdentifier := D.DID;
      Func.DidName        := D.Name;
      Func.Description    := D.Description;
      Func.EcuAddress     := D.EcuAddress;
      Exit(True);
    end;
end;

function ListCodingFunctions(
  const Ext: IOBDOEMExtension): TArray<TOBDCodingFunction>;
var
  D: TOBDOEMDataIdentifier;
  Kind: TOBDCodingFunctionKind;
  Acc: TList<TOBDCodingFunction>;
  Func: TOBDCodingFunction;
begin
  Acc := TList<TOBDCodingFunction>.Create;
  try
    if Ext = nil then Exit(nil);
    for D in Ext.DataIdentifiers do
    begin
      Kind := TOBDCodingFunctionRegistry.ClassifyName(D.Name);
      if Kind = cfUnknown then Continue;
      Func := Default(TOBDCodingFunction);
      Func.Kind           := Kind;
      Func.DataIdentifier := D.DID;
      Func.DidName        := D.Name;
      Func.Description    := D.Description;
      Func.EcuAddress     := D.EcuAddress;
      Acc.Add(Func);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function BuildWriteDataByIdentifier(const DID: Word;
  const Data: TBytes): TBytes;
var
  HeaderLen, I: Integer;
begin
  HeaderLen := 3;
  SetLength(Result, HeaderLen + Length(Data));
  Result[0] := UDS_SID_WRITE_DATA_BY_IDENTIFIER;
  Result[1] := Byte(DID shr 8);
  Result[2] := Byte(DID and $FF);
  for I := 0 to High(Data) do
    Result[HeaderLen + I] := Data[I];
end;

function BuildCodingFrame(const Func: TOBDCodingFunction;
  const Data: TBytes): TBytes;
begin
  Result := BuildWriteDataByIdentifier(Func.DataIdentifier, Data);
end;

function ParseCodingResponse(const Response: TBytes;
  const ExpectedDID: Word): Boolean;
begin
  Result := (Length(Response) >= 3)
        and (Response[0] = UDS_SID_WRITE_DATA_BY_IDENTIFIER + $40)
        and (Response[1] = Byte(ExpectedDID shr 8))
        and (Response[2] = Byte(ExpectedDID and $FF));
end;

initialization

finalization
  if Assigned(TOBDCodingFunctionRegistry.FTokens) then
    TOBDCodingFunctionRegistry.FTokens.Free;

end.
