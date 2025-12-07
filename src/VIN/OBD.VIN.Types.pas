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
  ///   VIN Plant Location
  /// </summary>
  TVINPlantLocation = record
    /// <summary>
    ///   Plant code
    /// </summary>
    Code: Char;
    /// <summary>
    ///   Plant name
    /// </summary>
    Name: string;
    /// <summary>
    ///   City
    /// </summary>
    City: string;
    /// <summary>
    ///   Country
    /// </summary>
    Country: string;
  end;

  /// <summary>
  ///   Vehicle Type
  /// </summary>
  TVINVehicleType = (
    vtUnknown,
    vtPassengerCar,
    vtTruck,
    vtSUV,
    vtVan,
    vtBus,
    vtMotorcycle,
    vtElectric,
    vtHybrid
  );

  /// <summary>
  ///   VIN-based Vehicle Features
  /// </summary>
  TVINFeatures = record
    /// <summary>
    ///   Vehicle type detected from VDS
    /// </summary>
    VehicleType: TVINVehicleType;
    /// <summary>
    ///   Engine displacement (liters) if detectable
    /// </summary>
    EngineDisplacement: string;
    /// <summary>
    ///   Engine type description if detectable
    /// </summary>
    EngineType: string;
    /// <summary>
    ///   Body style if detectable
    /// </summary>
    BodyStyle: string;
    /// <summary>
    ///   Drive type (FWD, RWD, AWD, 4WD) if detectable
    /// </summary>
    DriveType: string;
    /// <summary>
    ///   Transmission type if detectable
    /// </summary>
    Transmission: string;
    /// <summary>
    ///   Safety/restraint system code
    /// </summary>
    RestraintSystem: string;
    /// <summary>
    ///   Is this a commercial/fleet vehicle?
    /// </summary>
    IsCommercial: Boolean;
    /// <summary>
    ///   Additional notes about detected features
    /// </summary>
    Notes: string;
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
    ///   Plant location
    /// </summary>
    PlantLocation: TVINPlantLocation;
    /// <summary>
    ///   Check digit (position 9)
    /// </summary>
    CheckDigit: Char;
    /// <summary>
    ///   Is check digit valid?
    /// </summary>
    CheckDigitValid: Boolean;
    /// <summary>
    ///   Model year (most likely year based on current date)
    /// </summary>
    ModelYear: Integer;
    /// <summary>
    ///   Sequential production number
    /// </summary>
    SerialNumber: string;
    /// <summary>
    ///   Vehicle features detected from VIN
    /// </summary>
    Features: TVINFeatures;
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
