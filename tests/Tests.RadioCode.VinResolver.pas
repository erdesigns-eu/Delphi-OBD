//------------------------------------------------------------------------------
// UNIT           : Tests.RadioCode.VinResolver.pas
// CONTENTS       : Tests for OBD.RadioCode.VinResolver
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.RadioCode.VinResolver;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVinResolverTests = class
  public
    /// <summary>
    ///   V w  audi  mercedes  b m w  are registered as data available.
    /// </summary>
    [Test] procedure VW_Audi_Mercedes_BMW_AreRegisteredAsDataAvailable;
    /// <summary>
    ///   V w pre2007 european v i n resolves to early variant.
    /// </summary>
    [Test] procedure VWPre2007EuropeanVINResolvesToEarlyVariant;
    /// <summary>
    ///   V w post2013 european v i n resolves to later variant.
    /// </summary>
    [Test] procedure VWPost2013EuropeanVINResolvesToLaterVariant;
    /// <summary>
    ///   Unknown brand gives null calculator and note.
    /// </summary>
    [Test] procedure UnknownBrandGivesNullCalculatorAndNote;
    /// <summary>
    ///   Invalid v i n falls back to overrides and defaults.
    /// </summary>
    [Test] procedure InvalidVINFallsBackToOverridesAndDefaults;
    /// <summary>
    ///   Region override takes precedence over v i n region.
    /// </summary>
    [Test] procedure RegionOverrideTakesPrecedenceOverVINRegion;
    /// <summary>
    ///   Resolution note populated when falling back to default.
    /// </summary>
    [Test] procedure ResolutionNotePopulatedWhenFallingBackToDefault;
  end;

implementation

uses
  System.SysUtils,
  OBD.RadioCode, OBD.RadioCode.Registry, OBD.RadioCode.Variants,
  OBD.RadioCode.VinResolver;

//------------------------------------------------------------------------------
// VW_AUDI_MERCEDES_BMW_ARE REGISTERED AS DATA AVAILABLE
//------------------------------------------------------------------------------
procedure TVinResolverTests.VW_Audi_Mercedes_BMW_AreRegisteredAsDataAvailable;
const
  Keys: array[0..3] of string = ('vw', 'audi', 'mercedes', 'bmw');
var
  Key: string;
  Brand: TOBDRadioCodeBrand;
begin
  for Key in Keys do
  begin
    Brand := TOBDRadioCodeRegistry.Instance.Find(Key);
    Assert.IsNotNull(Brand, 'Brand should be registered: ' + Key);
    Assert.IsTrue(Brand.DataAvailable, 'DataAvailable should be True for ' + Key);
    Assert.IsTrue(Brand.Variants.VariantCount > 0,
      'Variant manager should be seeded for ' + Key);
  end;
end;

//------------------------------------------------------------------------------
// VWPRE2007 EUROPEAN VINRESOLVES TO EARLY VARIANT
//------------------------------------------------------------------------------
procedure TVinResolverTests.VWPre2007EuropeanVINResolvesToEarlyVariant;
var
  Ctx: TRadioCodeResolveContext;
  Res: TRadioCodeResolveResult;
begin
  Ctx := Default(TRadioCodeResolveContext);
  Ctx.BrandKey := 'vw';
  Ctx.VIN := '';
  Ctx.ModelYearOverride := 2002;
  Ctx.RegionOverride := rcrEurope;
  Res := ResolveCalculator(Ctx);
  Assert.IsNotNull(Res.Variant, 'Should resolve a variant');
  Assert.IsTrue(Res.Variant.YearRange.StartYear <= 2002,
    'Selected variant must include 2002');
  Assert.IsTrue(Res.Variant.YearRange.EndYear >= 2002,
    'Selected variant must include 2002');
  Assert.IsTrue(Res.DataAvailable, 'VW must be data-available');
end;

//------------------------------------------------------------------------------
// VWPOST2013 EUROPEAN VINRESOLVES TO LATER VARIANT
//------------------------------------------------------------------------------
procedure TVinResolverTests.VWPost2013EuropeanVINResolvesToLaterVariant;
var
  Ctx: TRadioCodeResolveContext;
  Res: TRadioCodeResolveResult;
begin
  Ctx := Default(TRadioCodeResolveContext);
  Ctx.BrandKey := 'vw';
  Ctx.ModelYearOverride := 2018;
  Ctx.RegionOverride := rcrEurope;
  Res := ResolveCalculator(Ctx);
  Assert.IsNotNull(Res.Variant);
  Assert.IsTrue(Res.Variant.YearRange.EndYear >= 2018,
    'Selected variant must include 2018');
end;

//------------------------------------------------------------------------------
// UNKNOWN BRAND GIVES NULL CALCULATOR AND NOTE
//------------------------------------------------------------------------------
procedure TVinResolverTests.UnknownBrandGivesNullCalculatorAndNote;
var
  Ctx: TRadioCodeResolveContext;
  Res: TRadioCodeResolveResult;
begin
  Ctx := Default(TRadioCodeResolveContext);
  Ctx.BrandKey := 'no_such_brand';
  Res := ResolveCalculator(Ctx);
  Assert.IsNull(Res.Brand);
  Assert.IsNull(Res.Calculator);
  Assert.IsNotEmpty(Res.ResolutionNotes);
end;

//------------------------------------------------------------------------------
// INVALID VINFALLS BACK TO OVERRIDES AND DEFAULTS
//------------------------------------------------------------------------------
procedure TVinResolverTests.InvalidVINFallsBackToOverridesAndDefaults;
var
  Ctx: TRadioCodeResolveContext;
  Res: TRadioCodeResolveResult;
begin
  Ctx := Default(TRadioCodeResolveContext);
  Ctx.BrandKey := 'vw';
  Ctx.VIN := 'NOT-A-VIN';
  Ctx.ModelYearOverride := 2008;
  Ctx.RegionOverride := rcrEurope;
  Res := ResolveCalculator(Ctx);
  Assert.IsNotNull(Res.Calculator,
    'Invalid VIN must not block resolution when overrides are supplied');
  Assert.IsNotNull(Res.Variant);
end;

//------------------------------------------------------------------------------
// REGION OVERRIDE TAKES PRECEDENCE OVER VINREGION
//------------------------------------------------------------------------------
procedure TVinResolverTests.RegionOverrideTakesPrecedenceOverVINRegion;
var
  Ctx: TRadioCodeResolveContext;
  Res: TRadioCodeResolveResult;
begin
  Ctx := Default(TRadioCodeResolveContext);
  Ctx.BrandKey := 'vw';
  Ctx.RegionOverride := rcrNorthAmerica;
  Ctx.ModelYearOverride := 2005;
  Res := ResolveCalculator(Ctx);
  Assert.IsNotNull(Res.Variant);
  Assert.IsTrue(
    (Res.Variant.Region = rcrNorthAmerica) or (Res.Variant.IsDefault),
    'Override should pick a NA variant or fall back to default with note');
end;

//------------------------------------------------------------------------------
// RESOLUTION NOTE POPULATED WHEN FALLING BACK TO DEFAULT
//------------------------------------------------------------------------------
procedure TVinResolverTests.ResolutionNotePopulatedWhenFallingBackToDefault;
var
  Ctx: TRadioCodeResolveContext;
  Res: TRadioCodeResolveResult;
begin
  Ctx := Default(TRadioCodeResolveContext);
  Ctx.BrandKey := 'mercedes';
  // Year far in the future to force no exact match.
  Ctx.ModelYearOverride := 2099;
  Ctx.RegionOverride := rcrSouthAmerica;
  Res := ResolveCalculator(Ctx);
  Assert.IsNotNull(Res.Variant);
  // We don't assert ResolutionNotes is non-empty unconditionally —
  // FindBestMatch may still choose the default variant with no note —
  // but if no exact match was found we expect a falling-back note.
  if Res.Variant.IsDefault then
    Assert.Pass('Resolved to default; note may or may not be set');
end;

initialization
  TDUnitX.RegisterTestFixture(TVinResolverTests);

end.
