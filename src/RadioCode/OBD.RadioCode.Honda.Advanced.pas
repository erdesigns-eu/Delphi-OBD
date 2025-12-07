//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Honda.Advanced.pas
// CONTENTS       : Honda Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Honda.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Honda Advanced Radio Code Calculator with Multiple Algorithm Variants
  /// </summary>
  TOBDRadioCodeHondaAdvanced = class(TOBDRadioCode)
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

constructor TOBDRadioCodeHondaAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Honda Advanced');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeHondaAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeHondaAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Europe V1 (1995-2005)
  Variant := FVariantManager.AddVariant(
    'HONDA_EU_V1',
    'Honda Europe V1 (1995-2005)',
    rcrEurope,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Alpine');
  Variant.AlgorithmNotes := 'Basic algorithm for European market';
  
  // Europe V2 (2006-2015)
  Variant := FVariantManager.AddVariant(
    'HONDA_EU_V2',
    'Honda Europe V2 (2006-2015)',
    rcrEurope,
    2006, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('Panasonic');
  Variant.AlgorithmNotes := 'Enhanced security algorithm';
  
  // Europe V3 (2016+)
  Variant := FVariantManager.AddVariant(
    'HONDA_EU_V3',
    'Honda Europe V3 (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('Model 3');
  Variant.AlgorithmNotes := 'Latest security version';
  
  // North America
  Variant := FVariantManager.AddVariant(
    'HONDA_NA',
    'Honda North America (2000+)',
    rcrNorthAmerica,
    2000, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Alpine');
  Variant.AlgorithmNotes := 'North American market variant';
  
  // Asia
  Variant := FVariantManager.AddVariant(
    'HONDA_ASIA',
    'Honda Asia (2005+)',
    rcrAsia,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Panasonic');
  Variant.AlgorithmNotes := 'Asian market variant';
end;

function TOBDRadioCodeHondaAdvanced.CalculateV1(const Serial: string): string;
var
  Sanitized: string;
  SerialDigits: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
  LetterValue: Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  // Get letter value (A=1, B=2, etc.)
  LetterValue := Ord(Sanitized[1]) - Ord('A') + 1;

  // Extract 7 digits
  SerialDigits := Copy(Sanitized, 2, 7);
  Serial := StrToInt(SerialDigits);

  // Honda algorithm
  Code[0] := ApplyModularTransform((Serial div 10000) + LetterValue + 4, 10);
  Code[1] := ApplyModularTransform((Serial div 1000) + LetterValue * 2, 10);
  Code[2] := ApplyModularTransform((Serial div 100) + LetterValue + 6, 10);
  Code[3] := ApplyModularTransform((Serial div 10) + LetterValue * 3, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

function TOBDRadioCodeHondaAdvanced.CalculateV2(const Serial: string): string;
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

function TOBDRadioCodeHondaAdvanced.CalculateV3(const Serial: string): string;
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

function TOBDRadioCodeHondaAdvanced.GetDescription: string;
begin
  Result := 'Advanced Honda Radio Code Calculator with multiple algorithm variants';
end;

procedure TOBDRadioCodeHondaAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeHondaAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

function TOBDRadioCodeHondaAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeHondaAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeHondaAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Honda serial: 8 characters (1 letter + 7 digits)
  if not ValidateLength(Sanitized, 8, ErrorMessage) then
    Exit(False);

  // First character must be a letter (U, L, or other prefix)
  if not CharInSet(Sanitized[1], ['A'..'Z']) then
  begin
    ErrorMessage := 'First character must be a letter (e.g., U, L)';
    Exit(False);
  end;

  // Remaining 7 characters must be digits
  for var I := 2 to 8 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9']) then
    begin
      ErrorMessage := Format('Character %d must be a digit', [I]);
      Exit(False);
    end;
  end;
end;

function TOBDRadioCodeHondaAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if (FCurrentVariant.VariantID = 'HONDA_EU_V1') or
     (FCurrentVariant.VariantID = 'HONDA_NA') then
    Output := CalculateV1(Sanitized)
  else if (FCurrentVariant.VariantID = 'HONDA_EU_V2') or
          (FCurrentVariant.VariantID = 'HONDA_ASIA') then
    Output := CalculateV2(Sanitized)
  else if FCurrentVariant.VariantID = 'HONDA_EU_V3' then
    Output := CalculateV3(Sanitized)
  else
  begin
    ErrorMessage := 'Unknown variant: ' + FCurrentVariant.VariantID;
    Exit(False);
  end;
end;

end.
