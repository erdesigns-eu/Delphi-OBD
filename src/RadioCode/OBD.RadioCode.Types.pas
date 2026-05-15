//------------------------------------------------------------------------------
//  OBD.RadioCode.Types
//
//  Cross-cutting types for the Delphi-OBD radio-code subsystem.
//  Records are deliberately copy-cheap (no managed objects) so a
//  resolved calculator request / result can be passed around
//  freely without ownership concerns.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Region + variant
//                     model re-derived from the v1 design with
//                     a flatter shape (no class-owned variant
//                     manager — variants are immutable records).
//------------------------------------------------------------------------------

unit OBD.RadioCode.Types;

interface

type
  /// <summary>Geographic region for a calculator variant. The
  /// VinResolver uses VIN region to narrow the variant set when a
  /// brand has multiple algorithm versions per market.</summary>
  TOBDRadioCodeRegion = (
    rcrUnknown,
    rcrNorthAmerica,
    rcrEurope,
    rcrAsia,
    rcrAustralia,
    rcrMiddleEast,
    rcrSouthAmerica,
    rcrAfrica
  );

  /// <summary>Half-open model-year range. Use <c>EndYear = 9999</c>
  /// for "current and ongoing".</summary>
  TOBDRadioCodeYearRange = record
    StartYear: Word;
    EndYear:   Word;
  end;

  /// <summary>Algorithm-version step. Some brands rolled silent
  /// security updates that change the seed→key calculation; the
  /// variant model lets a calculator declare which version it
  /// implements.</summary>
  TOBDRadioCodeSecurityVersion = (
    rcsvUnknown,
    rcsvV1,
    rcsvV2,
    rcsvV3,
    rcsvV4
  );

  /// <summary>Optional context the resolver passes to a
  /// calculator. Any field that's left blank is filled from the
  /// VIN itself or from the brand's default variant.</summary>
  TOBDRadioCodeContext = record
    /// <summary>Brand identifier (e.g. <c>'volkswagen'</c>).
    /// Required.</summary>
    BrandKey:          string;
    /// <summary>Optional VIN — used by the resolver to derive
    /// region / model year automatically.</summary>
    VIN:               string;
    /// <summary>Override for the model year (0 = derive from VIN).</summary>
    ModelYearOverride: Word;
    /// <summary>Optional radio-model name hint (e.g.
    /// <c>'Concert'</c>, <c>'Symphony'</c>) for brands that ship
    /// multiple radios per car.</summary>
    ModelHint:         string;
    /// <summary>Override for the region (rcrUnknown = derive from
    /// VIN).</summary>
    RegionOverride:    TOBDRadioCodeRegion;
  end;

  /// <summary>One calculation result.</summary>
  TOBDRadioCodeResult = record
    /// <summary>True when the calculator produced a code.</summary>
    Success:    Boolean;
    /// <summary>Computed radio code (e.g. <c>'1234'</c>) — empty
    /// when <c>Success</c> is False.</summary>
    Code:       string;
    /// <summary>Diagnostic / error message when <c>Success</c> is
    /// False; empty otherwise.</summary>
    Message:    string;
    /// <summary>Brand key the calculator self-reported.</summary>
    BrandKey:   string;
    /// <summary>Variant the calculator self-reported (free-form
    /// human label e.g. <c>'Europe / 2003-2010 / V2'</c>).</summary>
    Variant:    string;
  end;

implementation

end.
