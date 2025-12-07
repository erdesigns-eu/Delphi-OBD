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
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD VIN Decoder
  /// </summary>
  TOBDVinDecoder = class
  protected
    /// <summary>
    ///   Get the region from a WMI (World Manufacturer Identifier)
    /// </summary>
    class function GetRegion(const WMI: string): TVINRegion;
    /// <summary>
    ///   Get the country from a WMI (World Manufacturer Identifier)
    /// </summary>
    class function GetCountry(const WMI: string): TVINCountry;
    /// <summary>
    ///   Get the manufacturer from a WMI (World Manufacturer Identifier)
    /// </summary>
    class function GetManufacturer(const WMI: string): TVINManufacturer;
    /// <summary>
    ///   Get the years from the year code
    /// </summary>
    class function GetYears(const YearCode: Char): TArray<TVINYear>;
    /// <summary>
    ///   Get the plant location from the VIN
    /// </summary>
    class function GetPlantLocation(const WMI: string; const PlantCode: Char): TVINPlantLocation;
    /// <summary>
    ///   Calculate check digit for VIN validation (ISO 3779)
    /// </summary>
    class function CalculateCheckDigit(const VIN: string): Char;
    /// <summary>
    ///   Validate check digit (position 9)
    /// </summary>
    class function ValidateCheckDigit(const VIN: string): Boolean;
    /// <summary>
    ///   Get the most likely model year based on current date
    /// </summary>
    class function GetModelYear(const YearCode: Char): Integer;
    /// <summary>
    ///   Detect vehicle features from VIN (manufacturer-specific patterns)
    /// </summary>
    class function DetectFeatures(const VIN: string; const WMI: string; const VDS: string): TVINFeatures;
  public
    /// <summary>
    ///   Validate a VIN
    /// </summary>
    class function Validate(const VIN: string; var ErrorMessage: string): Boolean;
    /// <summary>
    ///   Parse a VIN into separate parts
    /// </summary>
    class function Parse(const VIN: string): TVINParseResult;
  end;

implementation

uses System.DateUtils;

//------------------------------------------------------------------------------
// GET REGION
//------------------------------------------------------------------------------
class function TOBDVinDecoder.GetRegion(const WMI: string): TVINRegion;
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
class function TOBDVinDecoder.GetCountry(const WMI: string): TVINCountry;
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
class function TOBDVinDecoder.GetManufacturer(const WMI: string): TVINManufacturer;
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
class function TOBDVinDecoder.GetYears(const YearCode: Char): TArray<TVINYear>;
var
  Index, CurrentYear, I: Integer;
begin
  Index := 0;
  Currentyear := YearOf(Now);
  for I := Low(VINYearMap) to High(VINYearMap) do
  begin
    if (VINYearMap[I].Code = YearCode) and (VINYearMap[I].Year <= CurrentYear) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Index] := VINYearMap[I];
      Inc(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------
// GET MODEL YEAR
//------------------------------------------------------------------------------
class function TOBDVinDecoder.GetModelYear(const YearCode: Char): Integer;
var
  Years: TArray<TVINYear>;
  CurrentYear, I: Integer;
  BestMatch: Integer;
begin
  // Get all possible years
  Years := GetYears(YearCode);
  
  if Length(Years) = 0 then
    Exit(0);
  
  // Current year
  CurrentYear := YearOf(Now);
  
  // Find the most recent year that's not in the future
  BestMatch := Years[0].Year;
  for I := 0 to High(Years) do
  begin
    if (Years[I].Year <= CurrentYear) and (Years[I].Year > BestMatch) then
      BestMatch := Years[I].Year;
  end;
  
  Result := BestMatch;
end;

//------------------------------------------------------------------------------
// GET PLANT LOCATION
//------------------------------------------------------------------------------
class function TOBDVinDecoder.GetPlantLocation(const WMI: string; const PlantCode: Char): TVINPlantLocation;
const
  UnknownPlant: TVINPlantLocation = (Code: #0; Name: ''; City: ''; Country: '');
var
  PlantKey: string;
begin
  // Try to find plant location in the map
  PlantKey := WMI + PlantCode;
  if not VINPlantLocationMap.TryGetValue(PlantKey, Result) then
  begin
    // If not found, return unknown plant
    Result := UnknownPlant;
    Result.Code := PlantCode;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE CHECK DIGIT (ISO 3779)
//------------------------------------------------------------------------------
class function TOBDVinDecoder.CalculateCheckDigit(const VIN: string): Char;
const
  // Transliteration table for letters to numbers
  TransliterationValues: array['A'..'Z'] of Integer = (
    1, 2, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 0, 7, 0, 9, 
    2, 3, 4, 5, 6, 7, 8, 9
  );
  // Position weights for check digit calculation
  Weights: array[1..17] of Integer = (
    8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2
  );
var
  I, Value, Sum: Integer;
  C: Char;
  Remainder: Integer;
begin
  Sum := 0;
  
  for I := 1 to 17 do
  begin
    C := UpCase(VIN[I]);
    
    // Get value for character
    if CharInSet(C, ['0'..'9']) then
      Value := Ord(C) - Ord('0')
    else if CharInSet(C, ['A'..'Z']) then
      Value := TransliterationValues[C]
    else
      Value := 0;
    
    // Multiply by weight and add to sum (skip position 9 - the check digit itself)
    if I <> 9 then
      Sum := Sum + (Value * Weights[I]);
  end;
  
  // Calculate remainder
  Remainder := Sum mod 11;
  
  // Convert to check digit (0-9 or X for 10)
  if Remainder = 10 then
    Result := 'X'
  else
    Result := Chr(Ord('0') + Remainder);
end;

//------------------------------------------------------------------------------
// VALIDATE CHECK DIGIT
//------------------------------------------------------------------------------
class function TOBDVinDecoder.ValidateCheckDigit(const VIN: string): Boolean;
var
  CalculatedDigit: Char;
begin
  Result := False;
  
  if Length(VIN) <> 17 then
    Exit;
  
  CalculatedDigit := CalculateCheckDigit(VIN);
  Result := (UpCase(VIN[9]) = CalculatedDigit);
end;

//------------------------------------------------------------------------------
// DETECT FEATURES FROM VIN
//------------------------------------------------------------------------------
class function TOBDVinDecoder.DetectFeatures(const VIN: string; const WMI: string; const VDS: string): TVINFeatures;
var
  Position4, Position5, Position6, Position7, Position8: Char;
begin
  // Initialize with defaults
  Result.VehicleType := vtUnknown;
  Result.EngineDisplacement := '';
  Result.EngineType := '';
  Result.BodyStyle := '';
  Result.DriveType := '';
  Result.Transmission := '';
  Result.RestraintSystem := '';
  Result.IsCommercial := False;
  Result.Notes := '';
  
  if Length(VDS) < 6 then
    Exit;
  
  // Extract VDS positions for analysis
  Position4 := VDS[1];  // Position 4 of VIN
  Position5 := VDS[2];  // Position 5 of VIN
  Position6 := VDS[3];  // Position 6 of VIN
  Position7 := VDS[4];  // Position 7 of VIN
  Position8 := VDS[5];  // Position 8 of VIN
  
  // Detect vehicle type based on common patterns
  // Ford patterns (1F*)
  if (WMI[1] = '1') and (WMI[2] = 'F') then
  begin
    case Position4 of
      'A', 'B': Result.VehicleType := vtPassengerCar;
      'C', 'D': Result.VehicleType := vtTruck;
      'E': Result.VehicleType := vtVan;
      'J', 'K', 'L': Result.VehicleType := vtSUV;
      'M': Result.VehicleType := vtTruck;
      'T': Result.VehicleType := vtTruck;
    end;
    Result.RestraintSystem := Position8;
  end
  // GM patterns (1G*)
  else if (WMI[1] = '1') and (WMI[2] = 'G') then
  begin
    case Position4 of
      '1': Result.VehicleType := vtPassengerCar;
      'C', 'D', 'E': Result.VehicleType := vtTruck;
      'K': Result.VehicleType := vtSUV;
      'T': Result.VehicleType := vtTruck;
      'W': Result.VehicleType := vtVan;
    end;
    
    // GM Drive type (position 6)
    case Position6 of
      'F': Result.DriveType := 'FWD';
      'R': Result.DriveType := 'RWD';
      'A', 'K': Result.DriveType := 'AWD';
    end;
  end
  // Toyota/Lexus patterns (4T*, 5T*, JT*)
  else if ((WMI[1] = '4') or (WMI[1] = '5') or (WMI[1] = 'J')) and (WMI[2] = 'T') then
  begin
    case Position4 of
      '1', '2', '3': Result.VehicleType := vtPassengerCar;
      '4', '5': Result.VehicleType := vtTruck;
      '6', '7': Result.VehicleType := vtSUV;
      'B': Result.VehicleType := vtVan;
      'E': Result.VehicleType := vtElectric;
    end;
    
    // Toyota engine detection (position 5)
    case Position5 of
      'A': Result.EngineType := '4-cylinder';
      'B': Result.EngineType := '4-cylinder Turbo';
      'C': Result.EngineType := '6-cylinder';
      'D': Result.EngineType := 'V6';
      'E': Result.EngineType := 'V8';
      'H': Result.EngineType := 'Hybrid';
    end;
  end
  // Honda/Acura patterns (1H*, JH*)
  else if ((WMI[1] = '1') or (WMI[1] = 'J')) and (WMI[2] = 'H') then
  begin
    case Position4 of
      'C', 'D', 'E', 'F', 'G': Result.VehicleType := vtPassengerCar;
      'J', 'K': Result.VehicleType := vtSUV;
      'T': Result.VehicleType := vtTruck;
    end;
    
    // Honda engine type
    if CharInSet(Position5, ['1', '2', '3', '4']) then
      Result.EngineType := '4-cylinder'
    else if CharInSet(Position5, ['5', '6']) then
      Result.EngineType := 'V6'
    else if Position5 = 'H' then
      Result.EngineType := 'Hybrid';
  end
  // BMW patterns (WB*)
  else if (WMI[1] = 'W') and (WMI[2] = 'B') then
  begin
    case Position4 of
      'A': Result.VehicleType := vtPassengerCar;
      'S', 'X': Result.VehicleType := vtSUV;
    end;
    
    Result.BodyStyle := 'BMW Series ' + Position5;
  end
  // Mercedes-Benz patterns (WD*)
  else if (WMI[1] = 'W') and (WMI[2] = 'D') then
  begin
    case Position4 of
      'B', 'C', 'D': Result.VehicleType := vtPassengerCar;
      'J': Result.VehicleType := vtSUV;
      'F': Result.VehicleType := vtVan;
    end;
  end
  // Volkswagen patterns (WV*, 3V*)
  else if ((WMI[1] = 'W') or (WMI[1] = '3')) and (WMI[2] = 'V') then
  begin
    case Position4 of
      'W', 'Z': Result.VehicleType := vtPassengerCar;
      '1', '2': Result.VehicleType := vtSUV;
    end;
  end;
  
  // Detect electric vehicles from VDS patterns
  if CharInSet(Position5, ['E', 'V']) or CharInSet(Position8, ['E']) then
  begin
    if Result.VehicleType = vtUnknown then
      Result.VehicleType := vtElectric;
    Result.Notes := 'Possible electric or plug-in hybrid vehicle';
  end;
  
  // Detect commercial vehicles
  if CharInSet(Position4, ['T', 'M']) or CharInSet(Position5, ['C', 'M']) then
  begin
    Result.IsCommercial := True;
  end;
end;

//------------------------------------------------------------------------------
// VALIDATE VIN
//------------------------------------------------------------------------------
class function TOBDVinDecoder.Validate(const VIN: string; var ErrorMessage: string): Boolean;
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
class function TOBDVinDecoder.Parse(const VIN: string): TVINParseResult;
//+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
//| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10| 11| 12| 13| 14| 15| 16| 17|
//+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
//|   WMI     |         VDS           |             VIS               |
//+-----------+-------------------+---+---+---+-----------------------+
const
  UnknownFeatures: TVINFeatures = (
    VehicleType       : vtUnknown;
    EngineDisplacement: '';
    EngineType        : '';
    BodyStyle         : '';
    DriveType         : '';
    Transmission      : '';
    RestraintSystem   : '';
    IsCommercial      : False;
    Notes             : ''
  );
  UnknownVIN: TVINParseResult = (
    Region          : (RangeStart: #0; RangeEnd: #0; Name: '');
    Country         : (RangeStart: ''; RangeEnd: ''; Name: ''; Code: '');
    Manufacturer    : (Code: ''; Name: '');
    Year            : [];
    WMI             : '';
    VDS             : '';
    VIS             : '';
    PlantLocation   : (Code: #0; Name: ''; City: ''; Country: '');
    CheckDigit      : #0;
    CheckDigitValid : False;
    ModelYear       : 0;
    SerialNumber    : '';
    Features        : (
      VehicleType       : vtUnknown;
      EngineDisplacement: '';
      EngineType        : '';
      BodyStyle         : '';
      DriveType         : '';
      Transmission      : '';
      RestraintSystem   : '';
      IsCommercial      : False;
      Notes             : ''
    );
    Valid           : False;
    ErrorMessage    : ''
  );
var
  ErrorMessage: string;
begin
  // First check if the VIN is valid
  if not Validate(VIN, ErrorMessage) then
  begin
    // Use the Unknown VIN record for the result
    Result := UnknownVIN;
    // Assign the error message
    Result.ErrorMessage := ErrorMessage;
  end else
  begin
    // Set valid to true
    Result.Valid := True;
    // Extract the WMI (positions 1-3)
    Result.WMI := Copy(VIN, 1, 3);
    // Extract the VDS (positions 4-9)
    Result.VDS := Copy(VIN, 4, 6);
    // Extract the VIS (positions 10-17)
    Result.VIS := Copy(VIN, 10, 8);
    // Extract check digit (position 9)
    Result.CheckDigit := VIN[9];
    // Validate check digit
    Result.CheckDigitValid := ValidateCheckDigit(VIN);
    // Extract plant code (position 11)
    Result.PlantLocation := GetPlantLocation(Result.WMI, VIN[11]);
    // Extract serial number (positions 12-17)
    Result.SerialNumber := Copy(VIN, 12, 6);
    // Parse the region
    Result.Region := GetRegion(Result.WMI);
    // Parse the country
    Result.Country := GetCountry(Result.WMI);
    // Parse the manufacturer
    Result.Manufacturer := GetManufacturer(Result.WMI);
    // Parse the year (possible years)
    Result.Year := GetYears(VIN[10]);
    // Get most likely model year
    Result.ModelYear := GetModelYear(VIN[10]);
    // Detect vehicle features from VIN
    Result.Features := DetectFeatures(VIN, Result.WMI, Result.VDS);
  end;
end;

end.
