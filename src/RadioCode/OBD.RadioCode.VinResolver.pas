//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.VinResolver.pas
// CONTENTS       : VIN-aware radio-code calculator resolver
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.RadioCode.VinResolver;

interface

uses
  System.SysUtils,

  OBD.RadioCode, OBD.RadioCode.Registry, OBD.RadioCode.Variants,
  OBD.VIN.Decoder, OBD.VIN.Types;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>Optional metadata supplied alongside the VIN. Any field
  /// left blank is filled from the VIN itself or from the brand's
  /// default variant.</summary>
  TRadioCodeResolveContext = record
    BrandKey: string;
    VIN: string;
    ModelYearOverride: Integer;  // 0 = use ModelYear from VIN
    ModelHint: string;           // optional radio-model name
    RegionOverride: TRadioCodeRegion; // rcrUnknown = derive from VIN
  end;

  /// <summary>Outcome of a resolution attempt.</summary>
  TRadioCodeResolveResult = record
    Calculator: IOBDRadioCode;
    Brand: TOBDRadioCodeBrand;     // nil if not found
    Variant: TRadioCodeVariant;    // nil if no variant manager populated
    DataAvailable: Boolean;        // shortcut: Brand <> nil and Brand.DataAvailable
    ResolutionNotes: string;       // e.g. 'fell back to default variant'
  end;

/// <summary>Map a TVINRegion.Name into the TRadioCodeRegion enum.</summary>
function MapVINRegionToRadioCodeRegion(const VinRegionName: string): TRadioCodeRegion;

/// <summary>Resolve a calculator for the given brand and VIN. Returns
/// a result record that callers should inspect — Calculator may be nil
/// when the brand isn't registered, or non-nil but with a stub when
/// the brand is data-pending.</summary>
function ResolveCalculator(const Ctx: TRadioCodeResolveContext): TRadioCodeResolveResult;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  OBD.RadioCode.VW.Advanced,
  OBD.RadioCode.Audi.Concert.Advanced,
  OBD.RadioCode.Mercedes.Advanced,
  OBD.RadioCode.BMW.Advanced;

function MapVINRegionToRadioCodeRegion(const VinRegionName: string): TRadioCodeRegion;
var
  N: string;
begin
  N := LowerCase(VinRegionName);
  if (N = '') then Exit(rcrUnknown);
  if Pos('europe', N) > 0 then Exit(rcrEurope);
  if Pos('north america', N) > 0 then Exit(rcrNorthAmerica);
  if Pos('asia', N) > 0 then Exit(rcrAsia);
  if Pos('oceania', N) > 0 then Exit(rcrAustralia);
  if Pos('africa', N) > 0 then Exit(rcrAfrica);
  if Pos('south america', N) > 0 then Exit(rcrSouthAmerica);
  if Pos('middle east', N) > 0 then Exit(rcrMiddleEast);
  Result := rcrUnknown;
end;

function ResolveCalculator(const Ctx: TRadioCodeResolveContext): TRadioCodeResolveResult;
var
  Brand: TOBDRadioCodeBrand;
  Parsed: TVINParseResult;
  Region: TRadioCodeRegion;
  Year: Integer;
  Variant: TRadioCodeVariant;
  Err: string;
begin
  Result := Default(TRadioCodeResolveResult);

  Brand := TOBDRadioCodeRegistry.Instance.Find(Ctx.BrandKey);
  Result.Brand := Brand;
  if Brand = nil then
  begin
    Result.ResolutionNotes := 'brand not registered: ' + Ctx.BrandKey;
    Exit;
  end;

  Result.DataAvailable := Brand.DataAvailable;
  Result.Calculator := Brand.CreateCalculator;

  // VIN parsing is best-effort: an invalid VIN doesn't fail the
  // resolution; we just fall back to overrides + brand defaults.
  Year := Ctx.ModelYearOverride;
  Region := Ctx.RegionOverride;

  if (Ctx.VIN <> '') and TOBDVinDecoder.Validate(Ctx.VIN, Err) then
  begin
    Parsed := TOBDVinDecoder.Parse(Ctx.VIN);
    if Year = 0 then
      Year := Parsed.ModelYear;
    if Region = rcrUnknown then
      Region := MapVINRegionToRadioCodeRegion(Parsed.Region.Name);
  end;

  Variant := Brand.Variants.FindBestMatch(Region, Year, Ctx.ModelHint);
  if Variant = nil then
  begin
    Variant := Brand.Variants.GetDefaultVariant;
    if Variant <> nil then
      Result.ResolutionNotes := 'no exact variant match; using brand default';
  end;
  Result.Variant := Variant;
end;

//------------------------------------------------------------------------------
// REGISTRATION OF DATA-AVAILABLE BRANDS
//------------------------------------------------------------------------------
function MakeFactoryVW: TOBDRadioCodeFactory;
begin
  Result := function: IOBDRadioCode
    begin
      Result := TOBDRadioCodeVWAdvanced.Create;
    end;
end;

function MakeFactoryAudiConcert: TOBDRadioCodeFactory;
begin
  Result := function: IOBDRadioCode
    begin
      Result := TOBDRadioCodeAudiConcertAdvanced.Create;
    end;
end;

function MakeFactoryMercedes: TOBDRadioCodeFactory;
begin
  Result := function: IOBDRadioCode
    begin
      Result := TOBDRadioCodeMercedesAdvanced.Create;
    end;
end;

function MakeFactoryBMW: TOBDRadioCodeFactory;
begin
  Result := function: IOBDRadioCode
    begin
      Result := TOBDRadioCodeBMWAdvanced.Create;
    end;
end;

procedure SeedVWVariants(Brand: TOBDRadioCodeBrand);
begin
  // Mirrors the variants TOBDRadioCodeVWAdvanced builds internally so
  // the registry-side resolver can discriminate without instantiating
  // the calculator. AlgorithmNotes here doc the underlying class.
  with Brand.Variants do
  begin
    AddVariant('VW_EU_GAMMA',  'VW Europe Gamma (1995-2000)',  rcrEurope, 1995, 2000, rcsvV1, True);
    AddVariant('VW_EU_BETA',   'VW Europe Beta (1998-2003)',   rcrEurope, 1998, 2003, rcsvV1);
    AddVariant('VW_EU_ALPHA',  'VW Europe Alpha (2000-2005)',  rcrEurope, 2000, 2005, rcsvV2);
    AddVariant('VW_EU_RCD',    'VW Europe RCD (2003-2012)',    rcrEurope, 2003, 2012, rcsvV2);
    AddVariant('VW_EU_RCD_NEW','VW Europe RCD/Composition (2013+)', rcrEurope, 2013, 9999, rcsvV3);
    AddVariant('VW_EU_RNS',    'VW Europe RNS Navigation (2005-2015)', rcrEurope, 2005, 2015, rcsvV2);
    AddVariant('VW_EU_RNS_NEW','VW Europe Discover Media/Pro (2016+)', rcrEurope, 2016, 9999, rcsvV4);
    AddVariant('VW_NA_STD',    'VW North America Standard (2000-2010)', rcrNorthAmerica, 2000, 2010, rcsvV1);
  end;
end;

procedure SeedAudiVariants(Brand: TOBDRadioCodeBrand);
begin
  with Brand.Variants do
  begin
    AddVariant('AUDI_CONCERT_1', 'Audi Concert I (pre-2003)',  rcrEurope, 1995, 2003, rcsvV1, True);
    AddVariant('AUDI_CONCERT_2', 'Audi Concert II/III (2003-2009)', rcrEurope, 2003, 2009, rcsvV2);
    AddVariant('AUDI_SYMPHONY',  'Audi Symphony I/II',         rcrEurope, 1996, 2007, rcsvV1);
    AddVariant('AUDI_RNS_E',     'Audi RNS-E Navigation',       rcrEurope, 2005, 2010, rcsvV2);
    AddVariant('AUDI_MMI_2G',    'Audi MMI 2G (2003-2009)',     rcrEurope, 2003, 2009, rcsvV2);
    AddVariant('AUDI_MMI_3G',    'Audi MMI 3G/3G+ (2008-2017)', rcrEurope, 2008, 2017, rcsvV3);
    AddVariant('AUDI_MIB',       'Audi MIB (2014+)',            rcrEurope, 2014, 9999, rcsvV4);
  end;
end;

procedure SeedMercedesVariants(Brand: TOBDRadioCodeBrand);
begin
  with Brand.Variants do
  begin
    // Becker BE-series prefixes documented across multiple Mercedes
    // service-info publications and community archives.
    AddVariant('MB_BECKER_BE',     'Mercedes Becker BE (1990-2000)', rcrEurope, 1990, 2000, rcsvV1, True);
    AddVariant('MB_BECKER_BE2',    'Mercedes Becker BE-2 (1998-2005)', rcrEurope, 1998, 2005, rcsvV2);
    AddVariant('MB_AUDIO_5',       'Mercedes Audio 5/10/20 (1998-2004)', rcrEurope, 1998, 2004, rcsvV1);
    AddVariant('MB_AUDIO_50_APS',  'Mercedes Audio 50 APS (2003-2014)', rcrEurope, 2003, 2014, rcsvV2);
    AddVariant('MB_COMMAND_NTG2',  'Mercedes COMAND NTG2 (2002-2008)', rcrEurope, 2002, 2008, rcsvV2);
    AddVariant('MB_COMMAND_NTG25', 'Mercedes COMAND NTG2.5 (2008-2014)', rcrEurope, 2008, 2014, rcsvV3);
    AddVariant('MB_COMMAND_NTG5',  'Mercedes COMAND NTG5 (2014-2018)', rcrEurope, 2014, 2018, rcsvV4);
    AddVariant('MB_MBUX',          'Mercedes MBUX (2018+)', rcrEurope, 2018, 9999, rcsvV5);
  end;
end;

procedure SeedBMWVariants(Brand: TOBDRadioCodeBrand);
begin
  with Brand.Variants do
  begin
    AddVariant('BMW_BUSINESS',    'BMW Business radio (E-series)', rcrEurope, 1990, 2007, rcsvV1, True);
    AddVariant('BMW_PROFESSIONAL','BMW Professional radio (E-series)', rcrEurope, 1995, 2007, rcsvV1);
    AddVariant('BMW_DSP',         'BMW DSP amplifier (E-series)', rcrEurope, 1995, 2010, rcsvV2);
    AddVariant('BMW_CCC',         'BMW CCC iDrive (E60/E90)',   rcrEurope, 2003, 2010, rcsvV2);
    AddVariant('BMW_CIC',         'BMW CIC iDrive (F-series transition)', rcrEurope, 2008, 2014, rcsvV3);
    AddVariant('BMW_NBT',         'BMW NBT (F-series)',         rcrEurope, 2012, 2017, rcsvV3);
    AddVariant('BMW_NBT_EVO',     'BMW NBT EVO',                rcrEurope, 2015, 2019, rcsvV4);
    AddVariant('BMW_ID5_ID6',     'BMW iDrive 5/6 (G-series)', rcrEurope, 2017, 2020, rcsvV4);
    AddVariant('BMW_ID7_ID8',     'BMW iDrive 7/8',             rcrEurope, 2018, 9999, rcsvV5);
  end;
end;

procedure RegisterDataAvailableBrands;
var
  VW, Audi, MB, BMW: TOBDRadioCodeBrand;
begin
  VW := TOBDRadioCodeBrand.Create('vw', 'Volkswagen', True, '', MakeFactoryVW);
  SeedVWVariants(VW);
  TOBDRadioCodeRegistry.Instance.Register(VW);

  Audi := TOBDRadioCodeBrand.Create('audi', 'Audi', True, '', MakeFactoryAudiConcert);
  SeedAudiVariants(Audi);
  TOBDRadioCodeRegistry.Instance.Register(Audi);

  MB := TOBDRadioCodeBrand.Create('mercedes', 'Mercedes-Benz', True, '', MakeFactoryMercedes);
  SeedMercedesVariants(MB);
  TOBDRadioCodeRegistry.Instance.Register(MB);

  BMW := TOBDRadioCodeBrand.Create('bmw', 'BMW', True, '', MakeFactoryBMW);
  SeedBMWVariants(BMW);
  TOBDRadioCodeRegistry.Instance.Register(BMW);
end;

initialization
  RegisterDataAvailableBrands;

end.
