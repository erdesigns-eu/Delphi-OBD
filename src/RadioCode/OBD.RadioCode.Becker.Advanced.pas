//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Becker.Advanced.pas
// CONTENTS       : Becker Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Becker.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants,
  OBD.RadioCode.Becker4, OBD.RadioCode.Becker5;

type
  /// <summary>
  ///   Becker Advanced Radio Code Calculator with Multiple Algorithm Variants
  ///   Consolidates: Becker4 (4-digit codes) and Becker5 (5-digit codes) lookup tables
  /// </summary>
  TOBDRadioCodeBeckerAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    FBecker4Calculator: TOBDRadioCodeBecker4;
    FBecker5Calculator: TOBDRadioCodeBecker5;
    
    procedure InitializeVariants;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string; override;

    /// <summary>
    ///   Set the variant to use
    /// </summary>
    procedure SetVariant(const VariantID: string); overload;
    procedure SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer); overload;

    /// <summary>
    ///   Get current variant information
    /// </summary>
    function GetCurrentVariant: TRadioCodeVariant;

    /// <summary>
    ///   Get all available variants
    /// </summary>
    function GetAvailableVariants: TRadioCodeVariantManager;

    /// <summary>
    ///   Radio Code input validator
    /// </summary>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;

    /// <summary>
    ///   Radio Code Calculator
    /// </summary>
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDRadioCodeBeckerAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Becker Advanced');
  FBecker4Calculator := TOBDRadioCodeBecker4.Create;
  FBecker5Calculator := TOBDRadioCodeBecker5.Create;
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDRadioCodeBeckerAdvanced.Destroy;
begin
  FBecker5Calculator.Free;
  FBecker4Calculator.Free;
  FVariantManager.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// INITIALIZE VARIANTS
//------------------------------------------------------------------------------
procedure TOBDRadioCodeBeckerAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Becker 4-digit Lookup Table (1995-2005)
  Variant := FVariantManager.AddVariant(
    'BECKER_4_DIGIT',
    'Becker 4-Digit Code Lookup (1995-2005)',
    rcrEurope,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Cascade');
  Variant.RadioModels.Add('Grand Prix');
  Variant.RadioModels.Add('Mexico');
  Variant.RadioModels.Add('Monza');
  Variant.RadioModels.Add('Indianapolis');
  Variant.AlgorithmNotes := 'Complete lookup table for 4-digit Becker codes (serials 0001-9999)';
  
  // Becker 5-digit Lookup Table (2006-2015)
  Variant := FVariantManager.AddVariant(
    'BECKER_5_DIGIT',
    'Becker 5-Digit Code Lookup (2006-2015)',
    rcrEurope,
    2006, 2015,
    rcsvV2,
    False
  );
  Variant.RadioModels.Add('DTM');
  Variant.RadioModels.Add('Online Pro');
  Variant.RadioModels.Add('Traffic Pro');
  Variant.RadioModels.Add('Cascade DTM');
  Variant.AlgorithmNotes := 'Complete lookup table for 5-digit Becker codes (serials 0001-9999)';
  
  // Becker 4-digit North America
  Variant := FVariantManager.AddVariant(
    'BECKER_4_NA',
    'Becker 4-Digit North America (1995-2005)',
    rcrNorthAmerica,
    1995, 2005,
    rcsvV1,
    False
  );
  Variant.RadioModels.Add('Cascade');
  Variant.RadioModels.Add('Mexico');
  Variant.AlgorithmNotes := 'North American market 4-digit variant';
  
  // Becker 5-digit North America
  Variant := FVariantManager.AddVariant(
    'BECKER_5_NA',
    'Becker 5-Digit North America (2006-2015)',
    rcrNorthAmerica,
    2006, 2015,
    rcsvV2,
    False
  );
  Variant.RadioModels.Add('DTM');
  Variant.RadioModels.Add('Traffic Pro');
  Variant.AlgorithmNotes := 'North American market 5-digit variant';
  
  // Becker 4-digit Asia
  Variant := FVariantManager.AddVariant(
    'BECKER_4_ASIA',
    'Becker 4-Digit Asia (2000-2010)',
    rcrAsia,
    2000, 2010,
    rcsvV1,
    False
  );
  Variant.RadioModels.Add('Mexico');
  Variant.RadioModels.Add('Indianapolis');
  Variant.AlgorithmNotes := 'Asian market 4-digit variant';
  
  // Becker 5-digit Asia
  Variant := FVariantManager.AddVariant(
    'BECKER_5_ASIA',
    'Becker 5-Digit Asia (2010+)',
    rcrAsia,
    2010, 9999,
    rcsvV2,
    False
  );
  Variant.RadioModels.Add('DTM');
  Variant.RadioModels.Add('Online Pro');
  Variant.AlgorithmNotes := 'Asian market 5-digit variant';
end;

//------------------------------------------------------------------------------
// GET DESCRIPTION
//------------------------------------------------------------------------------
function TOBDRadioCodeBeckerAdvanced.GetDescription: string;
begin
  Result := 'Advanced Becker Radio Code Calculator with multiple algorithm variants (4-digit and 5-digit lookup tables)';
end;

//------------------------------------------------------------------------------
// SET VARIANT
//------------------------------------------------------------------------------
procedure TOBDRadioCodeBeckerAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeBeckerAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

//------------------------------------------------------------------------------
// GET CURRENT VARIANT
//------------------------------------------------------------------------------
function TOBDRadioCodeBeckerAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

//------------------------------------------------------------------------------
// GET AVAILABLE VARIANTS
//------------------------------------------------------------------------------
function TOBDRadioCodeBeckerAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeBeckerAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Use4Digit: Boolean;
begin
  Result := True;
  ErrorMessage := '';
  
  Sanitized := SanitizeInput(Input);
  
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
  
  // Determine if using 4-digit or 5-digit variant
  Use4Digit := (Pos('4', FCurrentVariant.VariantID) > 0);
  
  if Use4Digit then
    Result := FBecker4Calculator.Validate(Sanitized, ErrorMessage)
  else
    Result := FBecker5Calculator.Validate(Sanitized, ErrorMessage);
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeBeckerAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Use4Digit: Boolean;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';
  
  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);
  
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
  
  // Determine if using 4-digit or 5-digit variant
  Use4Digit := (Pos('4', FCurrentVariant.VariantID) > 0);
  
  if Use4Digit then
    Result := FBecker4Calculator.Calculate(Sanitized, Output, ErrorMessage)
  else
    Result := FBecker5Calculator.Calculate(Sanitized, Output, ErrorMessage);
end;

end.
