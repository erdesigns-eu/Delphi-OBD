//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Variants.pas
// CONTENTS       : Radio Code Regional Variant Management System
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Variants;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Geographic regions for radio code variants
  /// </summary>
  TRadioCodeRegion = (
    rcrUnknown,       // Unknown/Generic
    rcrNorthAmerica,  // USA, Canada, Mexico
    rcrEurope,        // European Union countries
    rcrAsia,          // Japan, South Korea, China
    rcrAustralia,     // Australia, New Zealand
    rcrMiddleEast,    // Middle Eastern markets
    rcrSouthAmerica,  // South American markets
    rcrAfrica         // African markets
  );

  /// <summary>
  ///   Model year range for algorithm variants
  /// </summary>
  TRadioCodeYearRange = record
    StartYear: Integer;
    EndYear: Integer;   // Use 9999 for current/ongoing
  end;

  /// <summary>
  ///   Security version for updated algorithms
  /// </summary>
  TRadioCodeSecurityVersion = (
    rcsvUnknown,      // Unknown version
    rcsvV1,           // Original version
    rcsvV2,           // Security update 1
    rcsvV3,           // Security update 2
    rcsvV4,           // Security update 3
    rcsvV5            // Latest version
  );

  /// <summary>
  ///   Radio code algorithm variant information
  /// </summary>
  TRadioCodeVariant = class
  private
    FVariantID: string;
    FDescription: string;
    FRegion: TRadioCodeRegion;
    FYearRange: TRadioCodeYearRange;
    FSecurityVersion: TRadioCodeSecurityVersion;
    FRadioModels: TStringList;
    FAlgorithmNotes: string;
    FIsDefault: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Unique identifier for this variant
    /// </summary>
    property VariantID: string read FVariantID write FVariantID;

    /// <summary>
    ///   Human-readable description
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    ///   Geographic region this variant applies to
    /// </summary>
    property Region: TRadioCodeRegion read FRegion write FRegion;

    /// <summary>
    ///   Model year range this variant applies to
    /// </summary>
    property YearRange: TRadioCodeYearRange read FYearRange write FYearRange;

    /// <summary>
    ///   Security version of the algorithm
    /// </summary>
    property SecurityVersion: TRadioCodeSecurityVersion read FSecurityVersion write FSecurityVersion;

    /// <summary>
    ///   List of radio model numbers that use this variant
    /// </summary>
    property RadioModels: TStringList read FRadioModels;

    /// <summary>
    ///   Additional notes about the algorithm
    /// </summary>
    property AlgorithmNotes: string read FAlgorithmNotes write FAlgorithmNotes;

    /// <summary>
    ///   Whether this is the default variant for the brand
    /// </summary>
    property IsDefault: Boolean read FIsDefault write FIsDefault;
  end;

  /// <summary>
  ///   Variant manager for a specific brand
  /// </summary>
  TRadioCodeVariantManager = class
  private
    FBrandName: string;
    FVariants: TObjectList<TRadioCodeVariant>;
    function GetVariantCount: Integer;
    function GetVariant(Index: Integer): TRadioCodeVariant;
  public
    constructor Create(const BrandName: string);
    destructor Destroy; override;

    /// <summary>
    ///   Add a new variant to the manager
    /// </summary>
    function AddVariant(const VariantID, Description: string;
      Region: TRadioCodeRegion; StartYear, EndYear: Integer;
      SecurityVersion: TRadioCodeSecurityVersion;
      const IsDefault: Boolean = False): TRadioCodeVariant;

    /// <summary>
    ///   Find variant by ID
    /// </summary>
    function FindVariant(const VariantID: string): TRadioCodeVariant;

    /// <summary>
    ///   Find best matching variant based on criteria
    /// </summary>
    function FindBestMatch(const Region: TRadioCodeRegion;
      const ModelYear: Integer;
      const RadioModel: string = ''): TRadioCodeVariant;

    /// <summary>
    ///   Get the default variant
    /// </summary>
    function GetDefaultVariant: TRadioCodeVariant;

    /// <summary>
    ///   Get list of all variants for a specific region
    /// </summary>
    procedure GetVariantsByRegion(const Region: TRadioCodeRegion;
      var Variants: TList<TRadioCodeVariant>);

    /// <summary>
    ///   Get list of all variants for a specific year
    /// </summary>
    procedure GetVariantsByYear(const ModelYear: Integer;
      var Variants: TList<TRadioCodeVariant>);

    /// <summary>
    ///   Brand name this manager handles
    /// </summary>
    property BrandName: string read FBrandName;

    /// <summary>
    ///   Number of variants registered
    /// </summary>
    property VariantCount: Integer read GetVariantCount;

    /// <summary>
    ///   Access variants by index
    /// </summary>
    property Variants[Index: Integer]: TRadioCodeVariant read GetVariant; default;
  end;

//------------------------------------------------------------------------------
// HELPER FUNCTIONS
//------------------------------------------------------------------------------

/// <summary>
///   Convert region enum to string
/// </summary>
function RegionToString(const Region: TRadioCodeRegion): string;

/// <summary>
///   Convert string to region enum
/// </summary>
function StringToRegion(const RegionStr: string): TRadioCodeRegion;

/// <summary>
///   Convert security version enum to string
/// </summary>
function SecurityVersionToString(const Version: TRadioCodeSecurityVersion): string;

/// <summary>
///   Convert string to security version enum
/// </summary>
function StringToSecurityVersion(const VersionStr: string): TRadioCodeSecurityVersion;

implementation

//------------------------------------------------------------------------------
// TRADIOCODEVARIANT - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TRadioCodeVariant.Create;
begin
  inherited Create;
  FRadioModels := TStringList.Create;
  FRadioModels.Sorted := True;
  FRadioModels.Duplicates := dupIgnore;
  FRegion := rcrUnknown;
  FSecurityVersion := rcsvUnknown;
  FYearRange.StartYear := 0;
  FYearRange.EndYear := 9999;
  FIsDefault := False;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANT - DESTRUCTOR
//------------------------------------------------------------------------------
destructor TRadioCodeVariant.Destroy;
begin
  FRadioModels.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TRadioCodeVariantManager.Create(const BrandName: string);
begin
  inherited Create;
  FBrandName := BrandName;
  FVariants := TObjectList<TRadioCodeVariant>.Create(True);
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - DESTRUCTOR
//------------------------------------------------------------------------------
destructor TRadioCodeVariantManager.Destroy;
begin
  FVariants.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - GET VARIANT COUNT
//------------------------------------------------------------------------------
function TRadioCodeVariantManager.GetVariantCount: Integer;
begin
  Result := FVariants.Count;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - GET VARIANT
//------------------------------------------------------------------------------
function TRadioCodeVariantManager.GetVariant(Index: Integer): TRadioCodeVariant;
begin
  if (Index >= 0) and (Index < FVariants.Count) then
    Result := FVariants[Index]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - ADD VARIANT
//------------------------------------------------------------------------------
function TRadioCodeVariantManager.AddVariant(const VariantID, Description: string;
  Region: TRadioCodeRegion; StartYear, EndYear: Integer;
  SecurityVersion: TRadioCodeSecurityVersion;
  const IsDefault: Boolean): TRadioCodeVariant;
begin
  Result := TRadioCodeVariant.Create;
  Result.VariantID := VariantID;
  Result.Description := Description;
  Result.Region := Region;
  Result.YearRange.StartYear := StartYear;
  Result.YearRange.EndYear := EndYear;
  Result.SecurityVersion := SecurityVersion;
  Result.IsDefault := IsDefault;
  FVariants.Add(Result);
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - FIND VARIANT
//------------------------------------------------------------------------------
function TRadioCodeVariantManager.FindVariant(const VariantID: string): TRadioCodeVariant;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FVariants.Count - 1 do
  begin
    if SameText(FVariants[I].VariantID, VariantID) then
    begin
      Result := FVariants[I];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - FIND BEST MATCH
//------------------------------------------------------------------------------
function TRadioCodeVariantManager.FindBestMatch(const Region: TRadioCodeRegion;
  const ModelYear: Integer; const RadioModel: string): TRadioCodeVariant;
var
  I: Integer;
  Variant: TRadioCodeVariant;
  BestScore: Integer;
  CurrentScore: Integer;
begin
  Result := nil;
  BestScore := 0;

  for I := 0 to FVariants.Count - 1 do
  begin
    Variant := FVariants[I];
    CurrentScore := 0;

    // Check region match
    if Variant.Region = Region then
      Inc(CurrentScore, 100)
    else if Variant.Region = rcrUnknown then
      Inc(CurrentScore, 10);

    // Check year range
    if (ModelYear >= Variant.YearRange.StartYear) and
       (ModelYear <= Variant.YearRange.EndYear) then
      Inc(CurrentScore, 50);

    // Check radio model match
    if (RadioModel <> '') and (Variant.RadioModels.IndexOf(RadioModel) >= 0) then
      Inc(CurrentScore, 75);

    // Default variant gets bonus if no better match
    if Variant.IsDefault then
      Inc(CurrentScore, 5);

    // Update best match
    if CurrentScore > BestScore then
    begin
      BestScore := CurrentScore;
      Result := Variant;
    end;
  end;

  // If no match found, return default variant
  if Result = nil then
    Result := GetDefaultVariant;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - GET DEFAULT VARIANT
//------------------------------------------------------------------------------
function TRadioCodeVariantManager.GetDefaultVariant: TRadioCodeVariant;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FVariants.Count - 1 do
  begin
    if FVariants[I].IsDefault then
    begin
      Result := FVariants[I];
      Break;
    end;
  end;

  // If no default is set, return first variant
  if (Result = nil) and (FVariants.Count > 0) then
    Result := FVariants[0];
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - GET VARIANTS BY REGION
//------------------------------------------------------------------------------
procedure TRadioCodeVariantManager.GetVariantsByRegion(
  const Region: TRadioCodeRegion; var Variants: TList<TRadioCodeVariant>);
var
  I: Integer;
begin
  if Variants = nil then
    Variants := TList<TRadioCodeVariant>.Create
  else
    Variants.Clear;

  for I := 0 to FVariants.Count - 1 do
  begin
    if (FVariants[I].Region = Region) or (FVariants[I].Region = rcrUnknown) then
      Variants.Add(FVariants[I]);
  end;
end;

//------------------------------------------------------------------------------
// TRADIOCODEVARIANTMANAGER - GET VARIANTS BY YEAR
//------------------------------------------------------------------------------
procedure TRadioCodeVariantManager.GetVariantsByYear(const ModelYear: Integer;
  var Variants: TList<TRadioCodeVariant>);
var
  I: Integer;
begin
  if Variants = nil then
    Variants := TList<TRadioCodeVariant>.Create
  else
    Variants.Clear;

  for I := 0 to FVariants.Count - 1 do
  begin
    if (ModelYear >= FVariants[I].YearRange.StartYear) and
       (ModelYear <= FVariants[I].YearRange.EndYear) then
      Variants.Add(FVariants[I]);
  end;
end;

//------------------------------------------------------------------------------
// HELPER FUNCTIONS - REGION TO STRING
//------------------------------------------------------------------------------
function RegionToString(const Region: TRadioCodeRegion): string;
begin
  case Region of
    rcrNorthAmerica: Result := 'North America';
    rcrEurope:       Result := 'Europe';
    rcrAsia:         Result := 'Asia';
    rcrAustralia:    Result := 'Australia';
    rcrMiddleEast:   Result := 'Middle East';
    rcrSouthAmerica: Result := 'South America';
    rcrAfrica:       Result := 'Africa';
  else
    Result := 'Unknown';
  end;
end;

//------------------------------------------------------------------------------
// HELPER FUNCTIONS - STRING TO REGION
//------------------------------------------------------------------------------
function StringToRegion(const RegionStr: string): TRadioCodeRegion;
begin
  if SameText(RegionStr, 'North America') or SameText(RegionStr, 'NA') then
    Result := rcrNorthAmerica
  else if SameText(RegionStr, 'Europe') or SameText(RegionStr, 'EU') then
    Result := rcrEurope
  else if SameText(RegionStr, 'Asia') then
    Result := rcrAsia
  else if SameText(RegionStr, 'Australia') or SameText(RegionStr, 'AU') then
    Result := rcrAustralia
  else if SameText(RegionStr, 'Middle East') or SameText(RegionStr, 'ME') then
    Result := rcrMiddleEast
  else if SameText(RegionStr, 'South America') or SameText(RegionStr, 'SA') then
    Result := rcrSouthAmerica
  else if SameText(RegionStr, 'Africa') then
    Result := rcrAfrica
  else
    Result := rcrUnknown;
end;

//------------------------------------------------------------------------------
// HELPER FUNCTIONS - SECURITY VERSION TO STRING
//------------------------------------------------------------------------------
function SecurityVersionToString(const Version: TRadioCodeSecurityVersion): string;
begin
  case Version of
    rcsvV1: Result := 'V1';
    rcsvV2: Result := 'V2';
    rcsvV3: Result := 'V3';
    rcsvV4: Result := 'V4';
    rcsvV5: Result := 'V5';
  else
    Result := 'Unknown';
  end;
end;

//------------------------------------------------------------------------------
// HELPER FUNCTIONS - STRING TO SECURITY VERSION
//------------------------------------------------------------------------------
function StringToSecurityVersion(const VersionStr: string): TRadioCodeSecurityVersion;
begin
  if SameText(VersionStr, 'V1') then
    Result := rcsvV1
  else if SameText(VersionStr, 'V2') then
    Result := rcsvV2
  else if SameText(VersionStr, 'V3') then
    Result := rcsvV3
  else if SameText(VersionStr, 'V4') then
    Result := rcsvV4
  else if SameText(VersionStr, 'V5') then
    Result := rcsvV5
  else
    Result := rcsvUnknown;
end;

end.
