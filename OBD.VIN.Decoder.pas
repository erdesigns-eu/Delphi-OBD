//------------------------------------------------------------------------------
// UNIT           : OBD.VIN.Decoder.pas
// CONTENTS       : OBD VIN Decoder
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/04/2024
//------------------------------------------------------------------------------
unit OBD.VIN.Decoder;

interface

uses
  System.SysUtils, System.RegularExpressions,

  OBD.VIN.Types, OBD.VIN.Constants;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD VIN Decoder (Interface)
  /// </summary>
  IOBDVinDecoder = interface
    ['{49CFB2D5-F3DE-4673-BE79-ED03D3B9B26B}']
    /// <summary>
    ///   Get the region from a WMI (World Manufacturer Identifier)
    /// </summary>
    function GetRegion(const WMI: string): TVINRegion;
    /// <summary>
    ///   Get the country from a WMI (World Manufacturer Identifier)
    /// </summary>
    function GetCountry(const WMI: string): TVINCountry;
    /// <summary>
    ///   Get the manufacturer from a WMI (World Manufacturer Identifier)
    /// </summary>
    function GetManufacturer(const WMI: string): TVINManufacturer;
    /// <summary>
    ///   Get the years from the year code
    /// </summary>
    function GetYears(const YearCode: Char): TArray<Integer>;
    /// <summary>
    ///   Validate a VIN
    /// </summary>
    function Validate(const VIN: string; var ErrorMessage: string): Boolean;
    /// <summary>
    ///   Parse a VIN into separate parts
    /// </summary>
    function Parse(const VIN: string): TVINParseResult;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD VIN Decoder (Class)
  /// </summary>
  TOBDVinDecoder = class(TInterfacedObject, IOBDVinDecoder)
  protected
    /// <summary>
    ///   Get the region from a WMI (World Manufacturer Identifier)
    /// </summary>
    function GetRegion(const WMI: string): TVINRegion;
    /// <summary>
    ///   Get the country from a WMI (World Manufacturer Identifier)
    /// </summary>
    function GetCountry(const WMI: string): TVINCountry;
    /// <summary>
    ///   Get the manufacturer from a WMI (World Manufacturer Identifier)
    /// </summary>
    function GetManufacturer(const WMI: string): TVINManufacturer;
    /// <summary>
    ///   Get the years from the year code
    /// </summary>
    function GetYears(const YearCode: Char): TArray<Integer>;
  public
    /// <summary>
    ///   Validate a VIN
    /// </summary>
    function Validate(const VIN: string; var ErrorMessage: string): Boolean;
    /// <summary>
    ///   Parse a VIN into separate parts
    /// </summary>
    function Parse(const VIN: string): TVINParseResult;
  end;

implementation

uses System.DateUtils;

//------------------------------------------------------------------------------
// GET REGION
//------------------------------------------------------------------------------
function TOBDVinDecoder.GetRegion(const WMI: string): TVINRegion;
const
  UnknownRegion: TVINRegion = (RangeStart: #0; RangeEnd: #0; Name: '');
var
  I: Integer;
  RegionCode: Char;
begin
  // Initialize result
  Result := UnknownRegion;
  // If the WMI is empty then exit here
  if (WMI = '') then Exit;
  // Extract the region code from the WMI
  RegionCode := WMI[1];
  // Loop over regions and find the matching region
  for I := Low(VINRegions) to High(VINRegions) do
  with VINRegions[I] do
  if (RegionCode >= RangeStart) and (RegionCode <= RangeEnd) then
  begin
    // Assign the found region to the result
    Result := VINRegions[I];
    // Break the loop because we found the region
    Break;
  end;
end;

//------------------------------------------------------------------------------
// GET COUNTRY
//------------------------------------------------------------------------------
function TOBDVinDecoder.GetCountry(const WMI: string): TVINCountry;
const
  UnknownCountry: TVINCountry = (RangeStart: ''; RangeEnd: ''; Name: ''; Code: '');
begin
  // Try to find the country in the Country Map
  if not VINCountryMap.TryGetValue(Copy(WMI, 1, 2), Result) then
  begin
    // If we couldnt find a match, return the unknown country
    Result := UnknownCountry;
  end;
end;

//------------------------------------------------------------------------------
// GET MANUFACTURER
//------------------------------------------------------------------------------
function TOBDVinDecoder.GetManufacturer(const WMI: string): TVINManufacturer;
const
  UnknownManufacturer: TVINManufacturer = (Code: ''; Name: '');
begin
  // Try the find the manufacturer in the manufacturer map
  if not VINManufacturerMap.TryGetValue(WMI, Result) then
  begin
    // If we couldnt find a match, return the unknown manufacturer
    Result := UnknownManufacturer;
  end;
end;

//------------------------------------------------------------------------------
// GET YEARS
//------------------------------------------------------------------------------
function TOBDVinDecoder.GetYears(const YearCode: Char): TArray<Integer>;
const
  START_YEAR = 1980;
var
  Index, CurrentYear, Candidate, I: Integer;
begin
  // Get the current year
  CurrentYear := YearOf(Now);
  // Try to find the year in the year map
  if VINYearMap.TryGetValue(YearCode, Index) then
  begin
    Candidate := START_YEAR + Index;
    I := 0;
    SetLength(Result, 0);
    repeat
      SetLength(Result, Length(Result) + 1);
      Result[I] := Candidate;
      Inc(I);
      Candidate := Candidate + Length(YEAR_CHARS);
    until Candidate > CurrentYear;
  end else
    // If yearCode not found, return an empty array
    SetLength(Result, 0);
end;

//------------------------------------------------------------------------------
// VALIDATE VIN
//------------------------------------------------------------------------------
function TOBDVinDecoder.Validate(const VIN: string; var ErrorMessage: string): Boolean;
begin
  // Initialize result
  Result := True;
  // Clear any previous error messages
  ErrorMessage := '';

  // Check if the VIN is exactly 17 characters long
  if Length(VIN) <> 17 then
  begin
    // Set the result (Succeeded?) to false
    Result := False;
    // Set the descriptive error message
    ErrorMessage := 'VIN must be 17 characters long';
    // Exit here because the VIN is invalid length
    Exit;
  end;

  // Check if the VIN contains only valid characters
  if not TRegEx.IsMatch(VIN, '^[A-HJ-NPR-Z0-9]{17}$') then
  begin
    // Set the result (Succeeded?) to false
    Result := False;
    // Set the descriptive error message
    ErrorMessage := 'VIN must contain only letters (excluding I, O, and Q) and numbers';
  end;
end;

//------------------------------------------------------------------------------
// PARSE VIN
//------------------------------------------------------------------------------
function TOBDVinDecoder.Parse(const VIN: string): TVINParseResult;
const
  UnknownVIN: TVINParseResult = (
    Region       : (RangeStart: #0; RangeEnd: #0; Name: '');
    Country      : (RangeStart: ''; RangeEnd: ''; Name: ''; Code: '');
    Manufacturer : (Code: ''; Name: '');
    Year         : [];
    WMI          : '';
    VDS          : '';
    VIS          : '';
    Valid        : False;
    ErrorMessage : ''
  );
var
  ErrorMessage: string;
  YearCode: Char;
begin
  // First check if the VIN is valid
  if not Validate(VIN, ErrorMessage) then
  begin
    // Set valid to false
    Result.Valid := False;
    // Assign the error message
    Result.ErrorMessage := ErrorMessage;
  end else
  begin
    // Set valid to true
    Result.Valid := True;
    // Extract the WMI
    Result.WMI := Copy(VIN, 1, 3);
    // Extract the VDS
    Result.VDS := Copy(VIN, 4, 6);
    // Extract the VIS
    Result.VIS := Copy(VIN, 10, 8);
    // Extract the year code
    YearCode := Copy(VIN, 10, 1)[1];
    // Parse the region
    Result.Region := GetRegion(Result.WMI);
    // Parse the country
    Result.Country := GetCountry(Result.WMI);
    // Parse the manufacturer
    Result.Manufacturer := GetManufacturer(Result.WMI);
    // Parse the year (possible years)
    Result.Year := GetYears(YearCode);
  end;
end;

end.
