//------------------------------------------------------------------------------
// UNIT           : OBD.VIN.Constants.pas
// CONTENTS       : OBD VIN Constants (regions, countries, manufacturers, plants)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/04/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.VIN.Constants;

interface

uses
  System.SysUtils, System.Generics.Collections,

  OBD.VIN.Types;

//------------------------------------------------------------------------------
// CONSTANTS — VIN ALPHABETS (spec-defined, not data; never user-editable)
//------------------------------------------------------------------------------
const
  /// <summary>VIN-permitted characters (excludes I, O, Q, plus '0' wraps).</summary>
  ALPHABET_CHARS: array[0..32] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', '0'
  );

  /// <summary>VIN year-character cycle (60-year window starting 1980).</summary>
  YEAR_CHARS: array[0..59] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'V', 'W', 'X', 'Y', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'V', 'W', 'X', 'Y', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  );

//------------------------------------------------------------------------------
// LOOKUP TABLES — populated from JSON catalogs at unit init
//------------------------------------------------------------------------------
var
  /// <summary>VIN region table. Loaded from catalogs/vin-regions.json.</summary>
  VINRegions: TArray<TVINRegion>;
  /// <summary>VIN country table. Loaded from catalogs/vin-countries.json.</summary>
  VINCountries: TArray<TVINCountry>;
  /// <summary>WMI-to-manufacturer table. Loaded from catalogs/vin-wmi-manufacturers.json.</summary>
  VINManufacturers: TArray<TVINManufacturer>;

  /// <summary>WMI-prefix country lookup, computed from VINCountries +
  /// ALPHABET_CHARS.</summary>
  VINCountryMap: TDictionary<string, TVINCountry>;
  /// <summary>3-char WMI manufacturer lookup, computed from VINManufacturers.</summary>
  VINManufacturerMap: TDictionary<string, TVINManufacturer>;
  /// <summary>Year-character to model-year map.</summary>
  VINYearMap: TArray<TVINYear>;
  /// <summary>WMI+plant-char to plant location map. Loaded from
  /// catalogs/vin-plants.json.</summary>
  VINPlantLocationMap: TDictionary<string, TVINPlantLocation>;

implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

//------------------------------------------------------------------------------
// JSON LOADER HELPERS
//------------------------------------------------------------------------------
function LoadJsonObject(const FileName: string): TJSONObject;
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
begin
  Result := nil;
  Path := ResolveCatalogPath(FileName);
  if Path = '' then Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if Doc is TJSONObject then
    Result := Doc as TJSONObject
  else
    Doc.Free;
end;

procedure LoadRegions;
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  R: TVINRegion;
  S: string;
begin
  Doc := LoadJsonObject('vin-regions.json');
  if Doc = nil then Exit;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      S := Obj.GetValue<string>('range_start', '');
      if S = '' then Continue;
      R.RangeStart := S[1];
      S := Obj.GetValue<string>('range_end', '');
      if S = '' then Continue;
      R.RangeEnd := S[1];
      R.Name := Obj.GetValue<string>('name', '');
      VINRegions := VINRegions + [R];
    end;
  finally
    Doc.Free;
  end;
end;

procedure LoadCountries;
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  C: TVINCountry;
begin
  Doc := LoadJsonObject('vin-countries.json');
  if Doc = nil then Exit;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      C.RangeStart := Obj.GetValue<string>('range_start', '');
      C.RangeEnd   := Obj.GetValue<string>('range_end', '');
      C.Name       := Obj.GetValue<string>('name', '');
      C.Code       := Obj.GetValue<string>('iso_code', '');
      if (C.RangeStart = '') or (C.RangeEnd = '') then Continue;
      VINCountries := VINCountries + [C];
    end;
  finally
    Doc.Free;
  end;
end;

procedure LoadManufacturers;
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  M: TVINManufacturer;
begin
  Doc := LoadJsonObject('vin-wmi-manufacturers.json');
  if Doc = nil then Exit;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      M.Code := Obj.GetValue<string>('wmi', '');
      M.Name := Obj.GetValue<string>('name', '');
      if M.Code = '' then Continue;
      VINManufacturers := VINManufacturers + [M];
    end;
  finally
    Doc.Free;
  end;
end;

procedure LoadPlants;
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  P: TVINPlantLocation;
  Key: string;
begin
  Doc := LoadJsonObject('vin-plants.json');
  if Doc = nil then Exit;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      Key := Obj.GetValue<string>('key', '');
      if Key = '' then Continue;
      P.Code    := Obj.GetValue<string>('code', '');
      P.Name    := Obj.GetValue<string>('name', '');
      P.City    := Obj.GetValue<string>('city', '');
      P.Country := Obj.GetValue<string>('country', '');
      VINPlantLocationMap.AddOrSetValue(Key, P);
    end;
  finally
    Doc.Free;
  end;
end;

//------------------------------------------------------------------------------
// DERIVED MAPS — built from the loaded arrays
//------------------------------------------------------------------------------
procedure InitializeCountryMap;
var
  Country: TVINCountry;
  StartIndex, EndIndex, I, J, K: Integer;
  Key: string;
begin
  VINCountryMap := TDictionary<string, TVINCountry>.Create;
  for Country in VINCountries do
  begin
    StartIndex := -1;
    EndIndex   := -1;
    for I := Low(ALPHABET_CHARS) to High(ALPHABET_CHARS) do
    begin
      if ALPHABET_CHARS[I] = Country.RangeStart[1] then StartIndex := I;
      if ALPHABET_CHARS[I] = Country.RangeEnd[1]   then EndIndex   := I;
      if (StartIndex <> -1) and (EndIndex <> -1) then Break;
    end;
    if (StartIndex = -1) or (EndIndex = -1) then Continue;
    for I := StartIndex to EndIndex do
    for J := Low(ALPHABET_CHARS) to High(ALPHABET_CHARS) do
    begin
      if ALPHABET_CHARS[J] = Country.RangeStart[2] then StartIndex := J;
      if ALPHABET_CHARS[J] = Country.RangeEnd[2]   then EndIndex   := J;
      for K := StartIndex to EndIndex do
      begin
        Key := Country.RangeStart[1] + ALPHABET_CHARS[K];
        VINCountryMap.AddOrSetValue(Key, Country);
      end;
      Break;
    end;
  end;
end;

procedure InitializeManufacturerMap;
var
  Manufacturer: TVINManufacturer;
  ManufacturerCode: string;
  Character: Char;
begin
  VINManufacturerMap := TDictionary<string, TVINManufacturer>.Create;
  for Manufacturer in VINManufacturers do
  begin
    ManufacturerCode := Manufacturer.Code;
    if Length(ManufacturerCode) = 3 then
      VINManufacturerMap.AddOrSetValue(ManufacturerCode, Manufacturer)
    else if Length(ManufacturerCode) < 3 then
      for Character in ALPHABET_CHARS do
        if not VINManufacturerMap.ContainsKey(Manufacturer.Code + Character) then
          VINManufacturerMap.Add(Manufacturer.Code + Character, Manufacturer);
  end;
end;

procedure InitializeYearMap;
const
  StartYear: Integer = 1980;
var I: Integer;
begin
  SetLength(VINYearMap, Length(YEAR_CHARS));
  for I := Low(YEAR_CHARS) to High(YEAR_CHARS) do
  begin
    VINYearMap[I].Code := YEAR_CHARS[I];
    VINYearMap[I].Year := StartYear + I;
  end;
end;

//------------------------------------------------------------------------------
// INITIALIZATION
//------------------------------------------------------------------------------
initialization
  VINPlantLocationMap := TDictionary<string, TVINPlantLocation>.Create;
  LoadRegions;
  LoadCountries;
  LoadManufacturers;
  LoadPlants;
  InitializeCountryMap;
  InitializeManufacturerMap;
  InitializeYearMap;

finalization
  FreeAndNil(VINCountryMap);
  FreeAndNil(VINManufacturerMap);
  FreeAndNil(VINPlantLocationMap);

end.
