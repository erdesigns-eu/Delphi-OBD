//------------------------------------------------------------------------------
// UNIT           : OBD.VIN.Types.pas
// CONTENTS       : OBD VIN Types
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/04/2024
//------------------------------------------------------------------------------
unit OBD.VIN.Types;

interface

//------------------------------------------------------------------------------
// RECORDS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   VIN Region
  /// </summary>
  TVINRegion = record
    /// <summary>
    ///   Range start
    /// </summary>
    RangeStart: Char;
    /// <summary>
    ///   Range end
    /// </summary>
    RangeEnd: Char;
    /// <summary>
    ///   Name of the region
    /// </summary>
    Name: string;
  end;

  /// <summary>
  ///   VIN Country
  /// </summary>
  TVINCountry = record
    /// <summary>
    ///   Range start
    /// </summary>
    RangeStart: string;
    /// <summary>
    ///   Range end
    /// </summary>
    RangeEnd: string;
    /// <summary>
    ///   Name of the country
    /// </summary>
    Name: string;
    /// <summary>
    ///   Code of the country
    /// </summary>
    Code: string;
  end;

  /// <summary>
  ///   VIN Manufacturer
  /// </summary>
  TVINManufacturer = record
    /// <summary>
    ///   Code
    /// </summary>
    Code: string;
    /// <summary>
    ///   Name of the manufacturer
    /// </summary>
    Name: string;
  end;

  /// <summary>
  ///   VIN Year
  /// </summary>
  TVINYear = record
    /// <summary>
    ///   Code
    /// </summary>
    Code: Char;
    /// <summary>
    ///   Year
    /// </summary>
    Year: Integer;
  end;

  /// <summary>
  ///   VIN Parse Result
  /// </summary>
  TVINParseResult = record
    /// <summary>
    ///   Region
    /// </summary>
    Region: TVINRegion;
    /// <summary>
    ///   Country
    /// </summary>
    Country: TVINCountry;
    /// <summary>
    ///   Manufacturer
    /// </summary>
    Manufacturer: TVINManufacturer;
    /// <summary>
    ///   Year (Array of possible years)
    /// </summary>
    Year: TArray<TVINYear>;
    /// <summary>
    ///   World Manufacturer Identifier
    /// </summary>
    WMI: string;
    /// <summary>
    ///   Vehicle Descriptor Section
    /// </summary>
    VDS: string;
    /// <summary>
    ///   Vehicle Identifier Section
    /// </summary>
    VIS: string;
    /// <summary>
    ///   Is the VIN valid?
    /// </summary>
    Valid: Boolean;
    /// <summary>
    ///   Error message (Set when VIN is not valid)
    /// </summary>
    ErrorMessage: string;
  end;

implementation

end.
