//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Nissan.Advanced.pas
// CONTENTS       : Nissan Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Nissan.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Nissan Advanced Radio Code Calculator with Multiple Algorithm Variants
  /// </summary>
  TOBDRadioCodeNissanAdvanced = class(TOBDRadioCode)
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

constructor TOBDRadioCodeNissanAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Nissan Advanced');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeNissanAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeNissanAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Europe V1 (1995-2005)
  Variant := FVariantManager.AddVariant(
    'NISSAN_EU_V1',
    'Nissan Europe V1 (1995-2005)',
    rcrEurope,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Clarion');
  Variant.AlgorithmNotes := 'Basic algorithm for European market';
  
  // Europe V2 (2006-2015)
  Variant := FVariantManager.AddVariant(
    'NISSAN_EU_V2',
    'Nissan Europe V2 (2006-2015)',
    rcrEurope,
    2006, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('Bose');
  Variant.AlgorithmNotes := 'Enhanced security algorithm';
  
  // Europe V3 (2016+)
  Variant := FVariantManager.AddVariant(
    'NISSAN_EU_V3',
    'Nissan Europe V3 (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('Model 3');
  Variant.AlgorithmNotes := 'Latest security version';
  
  // North America
  Variant := FVariantManager.AddVariant(
    'NISSAN_NA',
    'Nissan North America (2000+)',
    rcrNorthAmerica,
    2000, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Clarion');
  Variant.AlgorithmNotes := 'North American market variant';
  
  // Asia
  Variant := FVariantManager.AddVariant(
    'NISSAN_ASIA',
    'Nissan Asia (2005+)',
    rcrAsia,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Bose');
  Variant.AlgorithmNotes := 'Asian market variant';
end;

function TOBDRadioCodeNissanAdvanced.CalculateV1(const Serial: string): string;
var
  Sanitized: string;
  D1, D2, D3, D4: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  // Extract digits
  D1 := StrToInt(Sanitized[1]);
  D2 := StrToInt(Sanitized[2]);
  D3 := StrToInt(Sanitized[3]);
  D4 := StrToInt(Sanitized[4]);

  // Nissan BP algorithm
  Code[0] := ApplyModularTransform((D1 + 3) * 7, 10);
  Code[1] := ApplyModularTransform((D2 + 5) * 3, 10);
  Code[2] := ApplyModularTransform((D3 + 7) * 5, 10);
  Code[3] := ApplyModularTransform((D4 + 1) * 9, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

function TOBDRadioCodeNissanAdvanced.CalculateV2(const Serial: string): string;
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

function TOBDRadioCodeNissanAdvanced.CalculateV3(const Serial: string): string;
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

function TOBDRadioCodeNissanAdvanced.GetDescription: string;
begin
  Result := 'Advanced Nissan Radio Code Calculator with multiple algorithm variants';
end;

procedure TOBDRadioCodeNissanAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeNissanAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

function TOBDRadioCodeNissanAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeNissanAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeNissanAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Nissan codes are typically 4 digits
  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeNissanAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if (FCurrentVariant.VariantID = 'NISSAN_EU_V1') or
     (FCurrentVariant.VariantID = 'NISSAN_NA') then
    Output := CalculateV1(Sanitized)
  else if (FCurrentVariant.VariantID = 'NISSAN_EU_V2') or
          (FCurrentVariant.VariantID = 'NISSAN_ASIA') then
    Output := CalculateV2(Sanitized)
  else if FCurrentVariant.VariantID = 'NISSAN_EU_V3' then
    Output := CalculateV3(Sanitized)
  else
  begin
    ErrorMessage := 'Unknown variant: ' + FCurrentVariant.VariantID;
    Exit(False);
  end;
end;

end.
