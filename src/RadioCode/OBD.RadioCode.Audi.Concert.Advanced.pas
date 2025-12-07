//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Audi.Concert.Advanced.pas
// CONTENTS       : Audi Concert Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Audi.Concert.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Audi Concert Advanced Radio Code Calculator with Multiple Algorithm Variants
  /// </summary>
  TOBDRadioCodeAudiConcertAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    
    procedure InitializeVariants;
    
    // Algorithm implementations
    function CalculateV1(const Serial: string): string;
    function CalculateV2(const Serial: string): string;
    function CalculateV3(const Serial: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetDescription: string; override;
    procedure SetVariant(const VariantID: string); overload;
    procedure SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer); overload;
    function GetCurrentVariant: TRadioCodeVariant;
    function GetAvailableVariants: TRadioCodeVariantManager;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

constructor TOBDRadioCodeAudiConcertAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Audi Concert Advanced');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeAudiConcertAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeAudiConcertAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Europe V1 (1995-2005)
  Variant := FVariantManager.AddVariant(
    'AUDI_CONCERT_EU_V1',
    'Audi Concert Europe V1 (1995-2005)',
    rcrEurope,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Concert');
  Variant.AlgorithmNotes := 'Basic algorithm for European market';
  
  // Europe V2 (2006-2015)
  Variant := FVariantManager.AddVariant(
    'AUDI_CONCERT_EU_V2',
    'Audi Concert Europe V2 (2006-2015)',
    rcrEurope,
    2006, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('Symphony');
  Variant.AlgorithmNotes := 'Enhanced security algorithm';
  
  // Europe V3 (2016+)
  Variant := FVariantManager.AddVariant(
    'AUDI_CONCERT_EU_V3',
    'Audi Concert Europe V3 (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('Chorus');
  Variant.AlgorithmNotes := 'Latest security version';
  
  // North America
  Variant := FVariantManager.AddVariant(
    'AUDI_CONCERT_NA',
    'Audi Concert North America (2000+)',
    rcrNorthAmerica,
    2000, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Concert');
  Variant.AlgorithmNotes := 'North American market variant';
  
  // Asia
  Variant := FVariantManager.AddVariant(
    'AUDI_CONCERT_ASIA',
    'Audi Concert Asia (2005+)',
    rcrAsia,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Symphony');
  Variant.AlgorithmNotes := 'Asian market variant';
end;

function TOBDRadioCodeAudiConcertAdvanced.CalculateV1(const Serial: string): string;
var
  Sanitized: string;
  Z1, Z2: Integer;
  LetterSum: Integer;
  SerialNum: Integer;
  Code1, Code2, Code3, Code4: Integer;
  WorkValue: Integer;
begin
  // Initialize result
  Result := True;
  // Clear the output
  Output := '';
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input
  Sanitized := SanitizeInput(Input);

  // Check if the input is valid
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  // Extract components from AUZ format
  Z1 := StrToInt(Sanitized[4]);
  Z2 := StrToInt(Sanitized[5]);

  // Calculate letter sum (A=1, B=2, etc.)
  LetterSum := 0;
  for var I := 6 to 8 do
  begin
    if CharInSet(Sanitized[I], ['A'..'Z']) then
      LetterSum := LetterSum + (Ord(Sanitized[I]) - Ord('A') + 1);
  end;

  // Extract numeric serial portion
  SerialNum := 0;
  for var I := 9 to 14 do
  begin
    if CharInSet(Sanitized[I], ['0'..'9']) then
      SerialNum := SerialNum * 10 + (Ord(Sanitized[I]) - Ord('0'));
  end;

  // Audi Concert/Symphony algorithm
  WorkValue := (Z1 * 1000 + Z2 * 100 + LetterSum * 10 + (SerialNum mod 100));

  // Calculate code digits using specific Audi algorithm
  Code1 := ApplyModularTransform((WorkValue div 1000) + Z1 + 5, 10);
  Code2 := ApplyModularTransform((WorkValue div 100) + Z2 + 3, 10);
  Code3 := ApplyModularTransform((WorkValue div 10) + LetterSum, 10);
  Code4 := ApplyModularTransform(WorkValue + (SerialNum mod 10), 10);

  // Format the code for the output
  Output := Format('%d%d%d%d', [Code1, Code2, Code3, Code4]);
end;

function TOBDRadioCodeAudiConcertAdvanced.CalculateV2(const Serial: string): string;
var
  SerialNum: Integer;
  Code: array[0..3] of Integer;
begin
  SerialNum := StrToIntDef(Serial, 0);
  
  // Enhanced algorithm
  Code[0] := ApplyModularTransform((SerialNum div 1000) * 2 + 3, 10);
  Code[1] := ApplyModularTransform((SerialNum div 100) * 5 + 1, 10);
  Code[2] := ApplyModularTransform((SerialNum div 10) * 3 + 7, 10);
  Code[3] := ApplyModularTransform(SerialNum * 7 + 5, 10);
  
  Result := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

function TOBDRadioCodeAudiConcertAdvanced.CalculateV3(const Serial: string): string;
var
  SerialNum: Integer;
  Code: array[0..3] of Integer;
begin
  SerialNum := StrToIntDef(Serial, 0);
  
  // Modern algorithm
  Code[0] := ApplyModularTransform((SerialNum div 1000) * 4 + 7, 10);
  Code[1] := ApplyModularTransform((SerialNum div 100) * 6 + 3, 10);
  Code[2] := ApplyModularTransform((SerialNum div 10) * 8 + 1, 10);
  Code[3] := ApplyModularTransform(SerialNum * 9 + 6, 10);
  
  Result := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

function TOBDRadioCodeAudiConcertAdvanced.GetDescription: string;
begin
  Result := 'Advanced Audi Concert Radio Code Calculator with multiple algorithm variants';
end;

procedure TOBDRadioCodeAudiConcertAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeAudiConcertAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

function TOBDRadioCodeAudiConcertAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeAudiConcertAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeAudiConcertAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input (remove whitespace, convert to uppercase)
  Sanitized := SanitizeInput(Input);

  // AUZ format: typically 14 characters
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // Must start with AUZ
  if not Sanitized.StartsWith('AUZ') then
  begin
    ErrorMessage := 'Serial must start with AUZ';
    Exit(False);
  end;

  // Characters 4-5 must be digits
  if not CharInSet(Sanitized[4], ['0'..'9']) or not CharInSet(Sanitized[5], ['0'..'9']) then
  begin
    ErrorMessage := 'Characters 4-5 must be digits';
    Exit(False);
  end;

  // Characters 6-8 must be letters
  for var I := 6 to 8 do
  begin
    if not CharInSet(Sanitized[I], ['A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be a letter', [I]);
      Exit(False);
    end;
  end;

  // Remaining characters should be alphanumeric
  for var I := 9 to 14 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9', 'A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  end;
end;

function TOBDRadioCodeAudiConcertAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';
  
  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);
  
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
  
  if (FCurrentVariant.VariantID = 'AUDI_CONCERT_EU_V1') or
     (FCurrentVariant.VariantID = 'AUDI_CONCERT_NA') then
    Output := CalculateV1(Sanitized)
  else if (FCurrentVariant.VariantID = 'AUDI_CONCERT_EU_V2') or
          (FCurrentVariant.VariantID = 'AUDI_CONCERT_ASIA') then
    Output := CalculateV2(Sanitized)
  else if FCurrentVariant.VariantID = 'AUDI_CONCERT_EU_V3' then
    Output := CalculateV3(Sanitized)
  else
  begin
    ErrorMessage := 'Unknown variant: ' + FCurrentVariant.VariantID;
    Exit(False);
  end;
end;

end.
