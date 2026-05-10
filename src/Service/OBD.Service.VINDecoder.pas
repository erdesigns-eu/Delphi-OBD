//------------------------------------------------------------------------------
//  OBD.Service.VINDecoder
//
//  TOBDVINDecoder — static class that decodes a 17-character VIN
//  into a fully populated <see cref="TOBDVINInfo"/>.
//
//  Algorithm (per ISO 3779 / ISO 3780 / SAE J853):
//
//    Position 1 .. 3   World Manufacturer Identifier (WMI)
//      [1]              first WMI char  -> region
//      [1..2]           first two chars -> country
//      [1..3]           full WMI        -> manufacturer
//    Position 4 .. 9   Vehicle Descriptor Section (VDS)
//      [9]              ISO 3779 transliteration check digit
//                       (mandatory for North-American VINs;
//                       filler character on most other markets)
//    Position 10..17  Vehicle Identifier Section (VIS)
//      [10]             year code -> 30-year cycle, two
//                       candidates per code over a 60-year window
//                       starting 1980
//      [11]             plant code -> per-WMI plant lookup
//      [12..17]         sequential production serial
//
//  VIN-permitted alphabet excludes I, O, Q to avoid confusion
//  with 1, 0. Lengths are checked up front; parsing aborts on
//  an invalid character with <c>Valid := False</c> and
//  <c>InvalidReason</c> populated.
//
//  Data tables (regions, countries, WMIs, plants) are loaded
//  from the JSON catalogues under <c>catalogs/vin/</c> on first
//  use. The loader is lazy and process-wide (single instance);
//  hosts can preload via <c>LoadCatalogs(ABaseDir)</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Algorithm
//                     re-derived from ISO 3779 / 3780 / SAE J853;
//                     spec-defined data tables imported from the
//                     v1 JSON catalogues.
//------------------------------------------------------------------------------

unit OBD.Service.VINDecoder;

interface

uses
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  OBD.Service.VINDecoder.Types;

type
  /// <summary>Static facade for VIN decoding. All methods are
  /// thread-safe after the catalogues have been loaded once.</summary>
  TOBDVINDecoder = class
  strict private
    class var FRegions:       TArray<TOBDVINRegion>;
    class var FCountries:     TArray<TOBDVINCountry>;
    class var FManufacturers: TDictionary<string, TOBDVINManufacturer>;
    class var FPlants:        TDictionary<string, TOBDVINPlantLocation>;
    class var FCatalogsLoaded: Boolean;
    class var FCatalogLock:    TObject;

    class procedure EnsureCatalogsLoaded; static;
    class function  ReadCatalogText(const AFile: string): string; static;
    class procedure LoadRegions(const ABaseDir: string); static;
    class procedure LoadCountries(const ABaseDir: string); static;
    class procedure LoadManufacturers(const ABaseDir: string); static;
    class procedure LoadPlants(const ABaseDir: string); static;
  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>Default catalog directory relative to the
    /// process's executable. Override
    /// via <see cref="CatalogDir"/> before any decode.</summary>
    class var CatalogDir: string;

    /// <summary>Pre-loads the catalogues from <c>ABaseDir</c> /
    /// <c>vin/</c>. Optional — first decode triggers a lazy
    /// load against <see cref="CatalogDir"/> if not already
    /// loaded.</summary>
    class procedure LoadCatalogs(const ABaseDir: string); static;

    /// <summary>True when a single character is in the ISO 3779
    /// VIN alphabet (excludes I, O, Q).</summary>
    class function IsVINChar(AChar: Char): Boolean; static;

    /// <summary>True when <c>AVIN</c> is exactly 17 characters
    /// of valid VIN-alphabet symbols. Does not check the check
    /// digit — see <c>IsCheckDigitValid</c> for that.</summary>
    class function IsValidShape(const AVIN: string): Boolean; static;

    /// <summary>Computes the ISO 3779 check digit (the character
    /// that should appear at position 9). Returns <c>#0</c> when
    /// the input shape is invalid.</summary>
    class function ComputeCheckDigit(const AVIN: string): Char; static;

    /// <summary>True when the check digit at position 9 matches
    /// the computed check digit. Many non-NA VINs leave the
    /// position as a filler — host code can choose to ignore
    /// the result on those.</summary>
    class function IsCheckDigitValid(const AVIN: string): Boolean; static;

    /// <summary>Resolves a year-code character into the two most
    /// likely calendar years (the VIN year alphabet repeats every
    /// 30 years). Hosts that have a hint about the era pick the
    /// closer one; <see cref="MostLikelyYear"/> does that
    /// automatically against the current calendar year.</summary>
    class function YearCandidates(AYearCode: Char): TArray<TOBDVINYear>; static;

    /// <summary>Picks the closer of two year candidates against
    /// <c>AReferenceYear</c> (defaults to the current calendar
    /// year). Returns 0 when the year code is unknown.</summary>
    class function MostLikelyYear(AYearCode: Char;
      AReferenceYear: Word = 0): Word; static;

    /// <summary>Looks up the region for the first WMI
    /// character.</summary>
    class function ResolveRegion(AFirstChar: Char): TOBDVINRegion; static;

    /// <summary>Looks up the country for the first two WMI
    /// characters.</summary>
    class function ResolveCountry(const AFirstTwo: string): TOBDVINCountry; static;

    /// <summary>Looks up the manufacturer for a 3-char WMI.</summary>
    class function ResolveManufacturer(const AWMI: string): TOBDVINManufacturer; static;

    /// <summary>Looks up the assembly plant for a WMI + plant
    /// code (4-char composite key).</summary>
    class function ResolvePlant(const AWMI: string;
      APlantCode: Char): TOBDVINPlantLocation; static;

    /// <summary>Best-effort feature decode from the VDS section.
    /// Currently a stub — returns
    /// <c>vtUnknown</c> + empty fields. Future work will populate
    /// from a per-manufacturer rules catalog.</summary>
    class function DetectFeatures(const AVIN: string): TOBDVINFeatures; static;

    /// <summary>Decodes <c>AVIN</c> end-to-end. Always returns a
    /// populated record; <c>Valid</c> + <c>InvalidReason</c>
    /// indicate whether the result is trustworthy.</summary>
    class function Decode(const AVIN: string): TOBDVINInfo; static;
  end;

implementation

uses
  System.IOUtils,
  System.JSON,
  Winapi.Windows;

const
  /// <summary>Position weights for the ISO 3779 check digit
  /// transliteration (positions 1..17, weight at index 8 is
  /// zero — the check digit's own slot).</summary>
  CHECK_DIGIT_WEIGHTS: array[1..17] of Byte = (
    8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2);

  /// <summary>VIN year-code alphabet, two cycles of 30. The
  /// catalogue covers a 60-year window starting in 1980.</summary>
  YEAR_ALPHABET: array[0..29] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N',
    'P', 'R', 'S', 'T', 'V', 'W', 'X', 'Y', '1', '2', '3', '4', '5',
    '6', '7', '8', '9');

  YEAR_BASE = 1980;

{ ---- helpers ---------------------------------------------------------------- }

function TransliterationValue(AChar: Char): Integer;
const
  // ISO 3779 check-digit letter values. I, O, Q are not in the
  // alphabet so they map to -1 (rejected).
  LETTER_VALUE: array['A'..'Z'] of ShortInt = (
    {A} 1, {B} 2, {C} 3, {D} 4, {E} 5, {F} 6, {G} 7, {H} 8,
    {I} -1, {J} 1, {K} 2, {L} 3, {M} 4, {N} 5, {O} -1, {P} 7,
    {Q} -1, {R} 9, {S} 2, {T} 3, {U} 4, {V} 5, {W} 6, {X} 7,
    {Y} 8, {Z} 9);
begin
  if (AChar >= '0') and (AChar <= '9') then
    Exit(Ord(AChar) - Ord('0'));
  if (AChar >= 'A') and (AChar <= 'Z') then
    Exit(LETTER_VALUE[AChar]);
  Result := -1;
end;

{ ---- TOBDVINDecoder --------------------------------------------------------- }

class constructor TOBDVINDecoder.Create;
begin
  FCatalogLock := TObject.Create;
  CatalogDir := TPath.Combine(
    TPath.GetDirectoryName(ParamStr(0)), 'catalogs');
end;

class destructor TOBDVINDecoder.Destroy;
begin
  FreeAndNil(FManufacturers);
  FreeAndNil(FPlants);
  FreeAndNil(FCatalogLock);
end;

class function TOBDVINDecoder.IsVINChar(AChar: Char): Boolean;
begin
  // I, O, Q excluded; full alphabet otherwise.
  Result := ((AChar >= '0') and (AChar <= '9')) or
            ((AChar >= 'A') and (AChar <= 'Z') and
             (AChar <> 'I') and (AChar <> 'O') and (AChar <> 'Q'));
end;

class function TOBDVINDecoder.IsValidShape(const AVIN: string): Boolean;
var
  C: Char;
begin
  if Length(AVIN) <> 17 then Exit(False);
  for C in AVIN do
    if not IsVINChar(C) then Exit(False);
  Result := True;
end;

class function TOBDVINDecoder.ComputeCheckDigit(const AVIN: string): Char;
var
  Sum, Idx, Value: Integer;
begin
  if not IsValidShape(AVIN) then Exit(#0);
  Sum := 0;
  for Idx := 1 to 17 do
  begin
    if Idx = 9 then Continue;
    Value := TransliterationValue(AVIN[Idx]);
    if Value < 0 then Exit(#0);
    Sum := Sum + Value * CHECK_DIGIT_WEIGHTS[Idx];
  end;
  Sum := Sum mod 11;
  if Sum = 10 then
    Result := 'X'
  else
    Result := Chr(Ord('0') + Sum);
end;

class function TOBDVINDecoder.IsCheckDigitValid(const AVIN: string): Boolean;
var
  Computed: Char;
begin
  Computed := ComputeCheckDigit(AVIN);
  Result := (Computed <> #0) and (AVIN[9] = Computed);
end;

class function TOBDVINDecoder.YearCandidates(
  AYearCode: Char): TArray<TOBDVINYear>;
var
  I: Integer;
  Candidates: TList<TOBDVINYear>;
  Y: TOBDVINYear;
begin
  Candidates := TList<TOBDVINYear>.Create;
  try
    for I := 0 to High(YEAR_ALPHABET) do
      if YEAR_ALPHABET[I] = AYearCode then
      begin
        Y.Code := AYearCode;
        Y.Year := YEAR_BASE + I;
        Candidates.Add(Y);
        Y.Year := YEAR_BASE + 30 + I;
        Candidates.Add(Y);
      end;
    Result := Candidates.ToArray;
  finally
    Candidates.Free;
  end;
end;

class function TOBDVINDecoder.MostLikelyYear(AYearCode: Char;
  AReferenceYear: Word = 0): Word;
var
  Cands: TArray<TOBDVINYear>;
  I: Integer;
  BestDelta, Delta: Integer;
  Ref: Integer;
begin
  Cands := YearCandidates(AYearCode);
  if Length(Cands) = 0 then Exit(0);
  if AReferenceYear = 0 then
    Ref := YearOf(Now) + 1   // build year is often a year ahead of cal year
  else
    Ref := AReferenceYear;
  Result := Cands[0].Year;
  BestDelta := Abs(Ref - Cands[0].Year);
  for I := 1 to High(Cands) do
  begin
    Delta := Abs(Ref - Cands[I].Year);
    if Delta < BestDelta then
    begin
      BestDelta := Delta;
      Result    := Cands[I].Year;
    end;
  end;
end;

class function TOBDVINDecoder.ResolveRegion(
  AFirstChar: Char): TOBDVINRegion;
var
  R: TOBDVINRegion;
begin
  EnsureCatalogsLoaded;
  Result := Default(TOBDVINRegion);
  for R in FRegions do
    if (AFirstChar >= R.RangeStart) and (AFirstChar <= R.RangeEnd) then
      Exit(R);
end;

class function TOBDVINDecoder.ResolveCountry(
  const AFirstTwo: string): TOBDVINCountry;
var
  C: TOBDVINCountry;
begin
  EnsureCatalogsLoaded;
  Result := Default(TOBDVINCountry);
  if Length(AFirstTwo) <> 2 then Exit;
  for C in FCountries do
    if (AFirstTwo >= C.RangeStart) and (AFirstTwo <= C.RangeEnd) then
      Exit(C);
end;

class function TOBDVINDecoder.ResolveManufacturer(
  const AWMI: string): TOBDVINManufacturer;
begin
  EnsureCatalogsLoaded;
  Result := Default(TOBDVINManufacturer);
  if Length(AWMI) <> 3 then Exit;
  if not FManufacturers.TryGetValue(UpperCase(AWMI), Result) then
    Result := Default(TOBDVINManufacturer);
end;

class function TOBDVINDecoder.ResolvePlant(const AWMI: string;
  APlantCode: Char): TOBDVINPlantLocation;
var
  Key: string;
begin
  EnsureCatalogsLoaded;
  Result := Default(TOBDVINPlantLocation);
  if Length(AWMI) <> 3 then Exit;
  Key := UpperCase(AWMI) + APlantCode;
  if not FPlants.TryGetValue(Key, Result) then
    Result := Default(TOBDVINPlantLocation);
end;

class function TOBDVINDecoder.DetectFeatures(
  const AVIN: string): TOBDVINFeatures;
begin
  // Stub. Future work: per-manufacturer rules catalogue
  // (data/catalogs/vin/vds-rules.json) keyed by WMI.
  Result := Default(TOBDVINFeatures);
  Result.VehicleType := vtUnknown;
end;

class function TOBDVINDecoder.Decode(const AVIN: string): TOBDVINInfo;
var
  Normalised: string;
begin
  Result := Default(TOBDVINInfo);
  Normalised := UpperCase(Trim(AVIN));
  Result.VIN := Normalised;
  if Length(Normalised) = 0 then
  begin
    Result.InvalidReason := 'VIN is empty';
    Exit;
  end;
  if Length(Normalised) <> 17 then
  begin
    Result.InvalidReason := Format('VIN must be exactly 17 characters (got %d)',
      [Length(Normalised)]);
    Exit;
  end;
  if not IsValidShape(Normalised) then
  begin
    Result.InvalidReason :=
      'VIN contains characters outside the ISO 3779 alphabet ' +
      '(I, O, Q are not allowed)';
    Exit;
  end;

  Result.WMI := Copy(Normalised, 1,  3);
  Result.VDS := Copy(Normalised, 4,  6);
  Result.VIS := Copy(Normalised, 10, 8);

  Result.Region       := ResolveRegion(Normalised[1]);
  Result.Country      := ResolveCountry(Copy(Normalised, 1, 2));
  Result.Manufacturer := ResolveManufacturer(Result.WMI);

  Result.CheckDigit      := Normalised[9];
  Result.CheckDigitValid := IsCheckDigitValid(Normalised);

  Result.YearCode       := Normalised[10];
  Result.YearCandidates := YearCandidates(Result.YearCode);
  Result.ModelYear      := MostLikelyYear(Result.YearCode);

  Result.PlantCode := Normalised[11];
  Result.Plant     := ResolvePlant(Result.WMI, Result.PlantCode);

  Result.Serial   := Copy(Normalised, 12, 6);
  Result.Features := DetectFeatures(Normalised);

  Result.Valid := True;
end;

{ ---- catalog loading ------------------------------------------------------- }

class procedure TOBDVINDecoder.LoadCatalogs(const ABaseDir: string);
begin
  TMonitor.Enter(FCatalogLock);
  try
    LoadRegions(ABaseDir);
    LoadCountries(ABaseDir);
    LoadManufacturers(ABaseDir);
    LoadPlants(ABaseDir);
    FCatalogsLoaded := True;
  finally
    TMonitor.Exit(FCatalogLock);
  end;
end;

class procedure TOBDVINDecoder.EnsureCatalogsLoaded;
begin
  if FCatalogsLoaded then Exit;
  LoadCatalogs(CatalogDir);
end;

class function TOBDVINDecoder.ReadCatalogText(const AFile: string): string;
begin
  if not TFile.Exists(AFile) then
    raise EOSError.CreateFmt('VIN catalogue missing: %s', [AFile]);
  Result := TFile.ReadAllText(AFile, TEncoding.UTF8);
end;

class procedure TOBDVINDecoder.LoadRegions(const ABaseDir: string);
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONObject;
  R: TOBDVINRegion;
  Acc: TList<TOBDVINRegion>;
  I: Integer;
begin
  Doc := TJSONObject.ParseJSONValue(
    ReadCatalogText(TPath.Combine(TPath.Combine(ABaseDir, 'vin'),
      'regions.json'))) as TJSONObject;
  Acc := TList<TOBDVINRegion>.Create;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    for I := 0 to Arr.Count - 1 do
    begin
      Item := Arr.Items[I] as TJSONObject;
      R.RangeStart := Item.GetValue<string>('range_start')[1];
      R.RangeEnd   := Item.GetValue<string>('range_end')[1];
      R.Name       := Item.GetValue<string>('name');
      Acc.Add(R);
    end;
    FRegions := Acc.ToArray;
  finally
    Acc.Free;
    Doc.Free;
  end;
end;

class procedure TOBDVINDecoder.LoadCountries(const ABaseDir: string);
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONObject;
  C: TOBDVINCountry;
  Acc: TList<TOBDVINCountry>;
  I: Integer;
begin
  Doc := TJSONObject.ParseJSONValue(
    ReadCatalogText(TPath.Combine(TPath.Combine(ABaseDir, 'vin'),
      'countries.json'))) as TJSONObject;
  Acc := TList<TOBDVINCountry>.Create;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    for I := 0 to Arr.Count - 1 do
    begin
      Item := Arr.Items[I] as TJSONObject;
      C.RangeStart := Item.GetValue<string>('range_start');
      C.RangeEnd   := Item.GetValue<string>('range_end');
      C.Name       := Item.GetValue<string>('name');
      C.ISOCode    := '';
      if Item.GetValue('iso_code') <> nil then
        C.ISOCode := Item.GetValue<string>('iso_code');
      Acc.Add(C);
    end;
    FCountries := Acc.ToArray;
  finally
    Acc.Free;
    Doc.Free;
  end;
end;

class procedure TOBDVINDecoder.LoadManufacturers(const ABaseDir: string);
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONObject;
  M: TOBDVINManufacturer;
  I: Integer;
begin
  if FManufacturers = nil then
    FManufacturers := TDictionary<string, TOBDVINManufacturer>.Create;
  FManufacturers.Clear;
  Doc := TJSONObject.ParseJSONValue(
    ReadCatalogText(TPath.Combine(TPath.Combine(ABaseDir, 'vin'),
      'wmi.json'))) as TJSONObject;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    for I := 0 to Arr.Count - 1 do
    begin
      Item := Arr.Items[I] as TJSONObject;
      M.WMI  := UpperCase(Item.GetValue<string>('wmi'));
      M.Name := Item.GetValue<string>('name');
      FManufacturers.AddOrSetValue(M.WMI, M);
    end;
  finally
    Doc.Free;
  end;
end;

class procedure TOBDVINDecoder.LoadPlants(const ABaseDir: string);
var
  Doc: TJSONObject;
  Arr: TJSONArray;
  Item: TJSONObject;
  P: TOBDVINPlantLocation;
  I: Integer;
begin
  if FPlants = nil then
    FPlants := TDictionary<string, TOBDVINPlantLocation>.Create;
  FPlants.Clear;
  Doc := TJSONObject.ParseJSONValue(
    ReadCatalogText(TPath.Combine(TPath.Combine(ABaseDir, 'vin'),
      'plants.json'))) as TJSONObject;
  try
    Arr := Doc.GetValue<TJSONArray>('entries');
    for I := 0 to Arr.Count - 1 do
    begin
      Item := Arr.Items[I] as TJSONObject;
      P.Key     := UpperCase(Item.GetValue<string>('key'));
      P.Code    := Item.GetValue<string>('code')[1];
      P.Name    := Item.GetValue<string>('name');
      P.City    := '';
      P.Country := '';
      if Item.GetValue('city') <> nil then
        P.City := Item.GetValue<string>('city');
      if Item.GetValue('country') <> nil then
        P.Country := Item.GetValue<string>('country');
      FPlants.AddOrSetValue(P.Key, P);
    end;
  finally
    Doc.Free;
  end;
end;

end.
