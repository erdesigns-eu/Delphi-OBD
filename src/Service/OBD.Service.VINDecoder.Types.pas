//------------------------------------------------------------------------------
//  OBD.Service.VINDecoder.Types
//
//  Value types used by <see cref="OBD.Service.VINDecoder"/>.
//  Records are deliberately copy-cheap (no managed objects) so a
//  decoded VIN can be passed around freely without ownership
//  concerns.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Ported from v1's
//                     OBD.VIN.Types with the result record
//                     flattened to a single TOBDVINInfo carrying
//                     every decoded field.
//------------------------------------------------------------------------------

unit OBD.Service.VINDecoder.Types;

interface

type
  /// <summary>One ISO 3779 region (e.g. "Africa", "Europe"). Keyed
  /// by the first WMI character via the closed range
  /// <c>RangeStart</c>..<c>RangeEnd</c>.</summary>
  TOBDVINRegion = record
    /// <summary>Inclusive start of the region's first-WMI-char range.</summary>
    RangeStart: Char;
    /// <summary>Inclusive end of the region's first-WMI-char range.</summary>
    RangeEnd:   Char;
    /// <summary>Region name (e.g. "Europe", "North America").</summary>
    Name:       string;
  end;

  /// <summary>One ISO 3779 / SAE J853 country. Keyed by the first
  /// two WMI characters via the closed range
  /// <c>RangeStart</c>..<c>RangeEnd</c>.</summary>
  TOBDVINCountry = record
    /// <summary>Inclusive start of the country's two-char prefix range.</summary>
    RangeStart: string;
    /// <summary>Inclusive end of the country's two-char prefix range.</summary>
    RangeEnd:   string;
    /// <summary>Country name.</summary>
    Name:       string;
    /// <summary>ISO 3166-1 alpha-2 (or alpha-3 in a few legacy rows)
    /// country code; empty when the source row had none.</summary>
    ISOCode:    string;
  end;

  /// <summary>One ISO 3780 World Manufacturer Identifier. The
  /// <c>WMI</c> field carries the full 3-character prefix.</summary>
  TOBDVINManufacturer = record
    /// <summary>3-character WMI prefix.</summary>
    WMI:  string;
    /// <summary>Manufacturer name.</summary>
    Name: string;
  end;

  /// <summary>One model-year hypothesis. The VIN year-code is
  /// ambiguous on a 30-year cycle, so a decode produces up to two
  /// hypotheses (the catalog covers a 60-year window from 1980).</summary>
  TOBDVINYear = record
    /// <summary>Year-code character (position 10 in the VIN).</summary>
    Code: Char;
    /// <summary>Resolved calendar year.</summary>
    Year: Word;
  end;

  /// <summary>One assembly plant. Keyed by the 4-char composite
  /// of WMI (3) + plant character (1).</summary>
  TOBDVINPlantLocation = record
    /// <summary>4-char composite (`WMI` + plant char).</summary>
    Key:     string;
    /// <summary>Plant code (single char from the composite).</summary>
    Code:    Char;
    /// <summary>Plant name (e.g. "Wolfsburg Plant").</summary>
    Name:    string;
    /// <summary>City the plant sits in.</summary>
    City:    string;
    /// <summary>Country the plant sits in.</summary>
    Country: string;
  end;

  /// <summary>Vehicle type detected from the VDS section of the
  /// VIN. The detection is best-effort — a vendor with a
  /// non-standard VDS may decode as <c>vtUnknown</c>.</summary>
  TOBDVINVehicleType = (
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

  /// <summary>One vPIC-style VDS pattern. Decodes a single
  /// VDS character (or character class) at a fixed offset into
  /// one VIN feature element. Loaded from
  /// <c>catalogs/vin/vds-rules.json</c> via the
  /// <see cref="TOBDVINVDSSchema"/> records.
  /// <para>The <c>Match</c> field is a regex character class:
  /// <c>"A"</c> matches the literal A; <c>"[ABC]"</c> matches
  /// any of A, B, C; <c>"."</c> or <c>""</c> matches any
  /// character. Multi-char literals are matched verbatim.</para>
  /// </summary>
  TOBDVINVDSPattern = record
    /// <summary>0-based offset within the 6-char VDS (positions
    /// 4..9 of the full VIN -> offsets 0..5 here).</summary>
    Offset: Byte;
    /// <summary>Regex character class to match at <c>Offset</c>.
    /// Empty / "." / "*" all mean "any".</summary>
    Match:  string;
    /// <summary>Element name from the vPIC dictionary (e.g.
    /// <c>"BodyClass"</c>, <c>"DisplacementL"</c>,
    /// <c>"EngineModel"</c>, <c>"DriveType"</c>,
    /// <c>"TransmissionStyle"</c>, <c>"FuelTypePrimary"</c>,
    /// <c>"ElectrificationLevel"</c>, <c>"AirBagLocFront"</c>,
    /// <c>"GVWR"</c>).</summary>
    Field:  string;
    /// <summary>Decoded value for this match (e.g.
    /// <c>"Sedan/Saloon"</c>, <c>"2.0"</c>, <c>"FWD"</c>).</summary>
    Value:  string;
  end;

  /// <summary>WMI + year-range entry inside a
  /// <see cref="TOBDVINVDSSchema"/>. A schema applies when one
  /// of its WMIs matches AND (when present) the model year is
  /// inside the range.</summary>
  TOBDVINVDSSchemaWMI = record
    WMI:      string;   // 3-char prefix (no wildcards)
    YearFrom: Word;     // 0 = no lower bound
    YearTo:   Word;     // 0 = no upper bound
  end;

  /// <summary>One VinSchema from the vPIC model. Owns a list of
  /// applicable WMIs (with optional year ranges) and a list of
  /// patterns; multiple patterns inside one schema are
  /// AND-combined when matching a VIN.</summary>
  TOBDVINVDSSchema = record
    /// <summary>Free-text id from the source; useful for logs
    /// and citations (e.g. <c>"vPIC.VinSchemaId 12345"</c>).</summary>
    Id:       string;
    /// <summary>WMI + year-range entries this schema applies
    /// to.</summary>
    WMIs:     TArray<TOBDVINVDSSchemaWMI>;
    /// <summary>One-or-more pattern entries; each adds one
    /// decoded field to the result when matched.</summary>
    Patterns: TArray<TOBDVINVDSPattern>;
  end;

  /// <summary>Best-effort feature decode from the VDS section.
  /// Empty fields mean "not detected", not "absent".</summary>
  TOBDVINFeatures = record
    /// <summary>Detected vehicle type, or <c>vtUnknown</c>.</summary>
    VehicleType:        TOBDVINVehicleType;
    /// <summary>Engine displacement string (e.g. "2.0L"); empty
    /// when not detected.</summary>
    EngineDisplacement: string;
    /// <summary>Engine type / family description; empty when not
    /// detected.</summary>
    EngineType:         string;
    /// <summary>Body style description (sedan, coupe, …); empty
    /// when not detected.</summary>
    BodyStyle:          string;
    /// <summary>Drive type (FWD / RWD / AWD / 4WD); empty when
    /// not detected.</summary>
    DriveType:          string;
    /// <summary>Transmission type (manual / automatic / CVT /
    /// dual-clutch); empty when not detected.</summary>
    Transmission:       string;
    /// <summary>Restraint-system code (e.g. "1" airbags driver
    /// only, "2" full); empty when not detected.</summary>
    RestraintSystem:    string;
    /// <summary>True when the VDS suggests a fleet / commercial
    /// designation.</summary>
    IsCommercial:       Boolean;
  end;

  /// <summary>Full decoded result for a single VIN.</summary>
  TOBDVINInfo = record
    /// <summary>Original VIN string (uppercased and trimmed).</summary>
    VIN:             string;
    /// <summary>True when every section decoded without an
    /// internal error. Note: the VIN's check digit may still be
    /// reported invalid in <c>CheckDigitValid</c>.</summary>
    Valid:           Boolean;
    /// <summary>Reason populated when <c>Valid</c> is False.</summary>
    InvalidReason:   string;

    /// <summary>World Manufacturer Identifier (positions 1..3).</summary>
    WMI:             string;
    /// <summary>Vehicle Descriptor Section (positions 4..9).</summary>
    VDS:             string;
    /// <summary>Vehicle Identifier Section (positions 10..17).</summary>
    VIS:             string;

    /// <summary>Decoded region (from WMI[0]).</summary>
    Region:          TOBDVINRegion;
    /// <summary>Decoded country (from WMI[0..1]).</summary>
    Country:         TOBDVINCountry;
    /// <summary>Decoded manufacturer (from WMI[0..2]).</summary>
    Manufacturer:    TOBDVINManufacturer;

    /// <summary>Check-digit character (VIN position 9).</summary>
    CheckDigit:      Char;
    /// <summary>True when the check digit matches the spec
    /// calculation. North-American VINs require this; many
    /// non-NA VINs leave it as a filler character.</summary>
    CheckDigitValid: Boolean;

    /// <summary>Year-code character (VIN position 10).</summary>
    YearCode:        Char;
    /// <summary>All year hypotheses (typically two — VIN year
    /// codes cycle every 30 years).</summary>
    YearCandidates:  TArray<TOBDVINYear>;
    /// <summary>Most-likely model year against the current
    /// calendar year (the closest of <c>YearCandidates</c>).</summary>
    ModelYear:       Word;

    /// <summary>Plant code (VIN position 11).</summary>
    PlantCode:       Char;
    /// <summary>Decoded plant location (when registered).</summary>
    Plant:           TOBDVINPlantLocation;

    /// <summary>Sequential production number (VIN positions
    /// 12..17). Carried verbatim for hosts that need a numeric
    /// build sequence.</summary>
    Serial:          string;

    /// <summary>Best-effort feature decode from the VDS.</summary>
    Features:        TOBDVINFeatures;
  end;

implementation

end.
