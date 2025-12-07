//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.AlfaRomeo.Advanced.pas
// CONTENTS       : Alfa Romeo Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.AlfaRomeo.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Alfa Romeo Advanced Radio Code Calculator with Multiple Algorithm Variants
  /// </summary>
  TOBDRadioCodeAlfaRomeoAdvanced = class(TOBDRadioCode)
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

constructor TOBDRadioCodeAlfaRomeoAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Alfa Romeo Advanced');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeAlfaRomeoAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeAlfaRomeoAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Europe V1 (1995-2005)
  Variant := FVariantManager.AddVariant(
    'ALFAROMEO_EU_V1',
    'Alfa Romeo Europe V1 (1995-2005)',
    rcrEurope,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Blaupunkt');
  Variant.AlgorithmNotes := 'Basic algorithm for European market';
  
  // Europe V2 (2006-2015)
  Variant := FVariantManager.AddVariant(
    'ALFAROMEO_EU_V2',
    'Alfa Romeo Europe V2 (2006-2015)',
    rcrEurope,
    2006, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('Magneti Marelli');
  Variant.AlgorithmNotes := 'Enhanced security algorithm';
  
  // Europe V3 (2016+)
  Variant := FVariantManager.AddVariant(
    'ALFAROMEO_EU_V3',
    'Alfa Romeo Europe V3 (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('Model 3');
  Variant.AlgorithmNotes := 'Latest security version';
  
  // North America
  Variant := FVariantManager.AddVariant(
    'ALFAROMEO_NA',
    'Alfa Romeo North America (2000+)',
    rcrNorthAmerica,
    2000, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Blaupunkt');
  Variant.AlgorithmNotes := 'North American market variant';
  
  // Asia
  Variant := FVariantManager.AddVariant(
    'ALFAROMEO_ASIA',
    'Alfa Romeo Asia (2005+)',
    rcrAsia,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Magneti Marelli');
  Variant.AlgorithmNotes := 'Asian market variant';
end;

function TOBDRadioCodeAlfaRomeoAdvanced.CalculateV1(const Serial: string): string;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
  LetterValue: Integer;
  D1, D2, D3, D4: Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  if Length(Sanitized) = 4 then
  begin
    // Grundig format (4 digits)
    Serial := StrToIntDef(Sanitized, 0);
    D1 := (Serial div 1000) mod 10;
    D2 := (Serial div 100) mod 10;
    D3 := (Serial div 10) mod 10;
    D4 := Serial mod 10;

    Code[0] := ApplyModularTransform(D1 + D2 + 5, 10);
    Code[1] := ApplyModularTransform(D2 + D3 + 7, 10);
    Code[2] := ApplyModularTransform(D3 + D4 + 3, 10);
    Code[3] := ApplyModularTransform(D1 + D4 + 9, 10);
  end
  else
  begin
    // Blaupunkt format (letter + 6 digits)
    LetterValue := Ord(Sanitized[1]) - Ord('A') + 1;
    Serial := StrToIntDef(Copy(Sanitized, 2, 6), 0);

    Code[0] := ApplyModularTransform((Serial div 10000) + LetterValue, 10);
    Code[1] := ApplyModularTransform((Serial div 100) mod 100 + LetterValue * 2, 10);
    Code[2] := ApplyModularTransform((Serial mod 100) + LetterValue * 3, 10);
    Code[3] := ApplyModularTransform((Serial mod 10) + LetterValue * 5, 10);
  end;

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

function TOBDRadioCodeAlfaRomeoAdvanced.CalculateV2(const Serial: string): string;
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

function TOBDRadioCodeAlfaRomeoAdvanced.CalculateV3(const Serial: string): string;
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

function TOBDRadioCodeAlfaRomeoAdvanced.GetDescription: string;
begin
  Result := 'Advanced Alfa Romeo Radio Code Calculator with multiple algorithm variants';
end;

procedure TOBDRadioCodeAlfaRomeoAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeAlfaRomeoAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

function TOBDRadioCodeAlfaRomeoAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeAlfaRomeoAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeAlfaRomeoAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Support two formats: 4 digits (Grundig) or 7 chars (letter + 6 digits, Blaupunkt)
  if (Length(Sanitized) <> 4) and (Length(Sanitized) <> 7) then
  begin
    ErrorMessage := 'Serial must be 4 digits (Grundig) or 7 characters (Blaupunkt: letter + 6 digits)';
    Exit(False);
  end;

  if Length(Sanitized) = 4 then
  begin
    if not ValidateDigits(Sanitized, ErrorMessage) then
      Exit(False);
  end
  else // Length = 7
  begin
    if not CharInSet(Sanitized[1], ['A'..'Z']) then
    begin
      ErrorMessage := 'First character must be a letter for Blaupunkt models';
      Exit(False);
    end;

    for var I := 2 to 7 do
    begin
      if not CharInSet(Sanitized[I], ['0'..'9']) then
      begin
        ErrorMessage := Format('Character %d must be a digit', [I]);
        Exit(False);
      end;
    end;
  end;
end;

function TOBDRadioCodeAlfaRomeoAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if (FCurrentVariant.VariantID = 'ALFAROMEO_EU_V1') or
     (FCurrentVariant.VariantID = 'ALFAROMEO_NA') then
    Output := CalculateV1(Sanitized)
  else if (FCurrentVariant.VariantID = 'ALFAROMEO_EU_V2') or
          (FCurrentVariant.VariantID = 'ALFAROMEO_ASIA') then
    Output := CalculateV2(Sanitized)
  else if FCurrentVariant.VariantID = 'ALFAROMEO_EU_V3' then
    Output := CalculateV3(Sanitized)
  else
  begin
    ErrorMessage := 'Unknown variant: ' + FCurrentVariant.VariantID;
    Exit(False);
  end;
end;

end.
