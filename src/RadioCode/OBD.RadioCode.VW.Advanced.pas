//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.VW.Advanced.pas
// CONTENTS       : VW Radio Code Calculator with Regional Variants
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.VW.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   VW Radio Code Calculator - Advanced Multi-Variant
  /// </summary>
  TOBDRadioCodeVWAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    procedure InitializeVariants;
    function CalculateGamma(const Serial: string): string;
    function CalculateBeta(const Serial: string): string;
    function CalculateAlpha(const Serial: string): string;
    function CalculateRCD(const Serial: string): string;
    function CalculateRNS(const Serial: string): string;
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

constructor TOBDRadioCodeVWAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Volkswagen');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeVWAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeVWAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // European - Gamma Series (1995-2000)
  Variant := FVariantManager.AddVariant(
    'VW_EU_GAMMA',
    'VW Europe - Gamma Series (1995-2000)',
    rcrEurope,
    1995, 2000,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Gamma');
  Variant.RadioModels.Add('Gamma II');
  Variant.RadioModels.Add('Gamma III');
  Variant.AlgorithmNotes := 'Original Gamma series using simple VWZ calculation';

  // European - Beta Series (1998-2003)
  Variant := FVariantManager.AddVariant(
    'VW_EU_BETA',
    'VW Europe - Beta Series (1998-2003)',
    rcrEurope,
    1998, 2003,
    rcsvV1
  );
  Variant.RadioModels.Add('Beta');
  Variant.RadioModels.Add('Beta II');
  Variant.RadioModels.Add('Beta III');
  Variant.AlgorithmNotes := 'Beta series with slight variation from Gamma';

  // European - Alpha Series (2000-2005)
  Variant := FVariantManager.AddVariant(
    'VW_EU_ALPHA',
    'VW Europe - Alpha Series (2000-2005)',
    rcrEurope,
    2000, 2005,
    rcsvV2
  );
  Variant.RadioModels.Add('Alpha');
  Variant.RadioModels.Add('Alpha CC');
  Variant.AlgorithmNotes := 'Updated security with modified letter values';

  // European - RCD Series (2003-2012)
  Variant := FVariantManager.AddVariant(
    'VW_EU_RCD',
    'VW Europe - RCD Series (2003-2012)',
    rcrEurope,
    2003, 2012,
    rcsvV2
  );
  Variant.RadioModels.Add('RCD 200');
  Variant.RadioModels.Add('RCD 300');
  Variant.RadioModels.Add('RCD 310');
  Variant.RadioModels.Add('RCD 510');
  Variant.AlgorithmNotes := 'RCD series with enhanced security version 2';

  // European - RCD Series (2013+)
  Variant := FVariantManager.AddVariant(
    'VW_EU_RCD_NEW',
    'VW Europe - RCD Series (2013+)',
    rcrEurope,
    2013, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('RCD 330');
  Variant.RadioModels.Add('RCD 360');
  Variant.RadioModels.Add('Composition Media');
  Variant.AlgorithmNotes := 'Latest RCD with security version 3';

  // European - RNS Navigation (2005-2015)
  Variant := FVariantManager.AddVariant(
    'VW_EU_RNS',
    'VW Europe - RNS Navigation (2005-2015)',
    rcrEurope,
    2005, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('RNS 300');
  Variant.RadioModels.Add('RNS 310');
  Variant.RadioModels.Add('RNS 315');
  Variant.RadioModels.Add('RNS 510');
  Variant.AlgorithmNotes := 'Navigation systems with different algorithm';

  // European - RNS Navigation (2016+)
  Variant := FVariantManager.AddVariant(
    'VW_EU_RNS_NEW',
    'VW Europe - RNS Navigation (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV4
  );
  Variant.RadioModels.Add('Discover Media');
  Variant.RadioModels.Add('Discover Pro');
  Variant.AlgorithmNotes := 'Latest navigation with security version 4';

  // North American - Standard (2000-2010)
  Variant := FVariantManager.AddVariant(
    'VW_NA_STANDARD',
    'VW North America - Standard (2000-2010)',
    rcrNorthAmerica,
    2000, 2010,
    rcsvV1
  );
  Variant.RadioModels.Add('Premium 5');
  Variant.RadioModels.Add('Premium 6');
  Variant.AlgorithmNotes := 'North American variant similar to European RCD';

  // North American - Modern (2011+)
  Variant := FVariantManager.AddVariant(
    'VW_NA_MODERN',
    'VW North America - Modern (2011+)',
    rcrNorthAmerica,
    2011, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('Premium 8');
  Variant.RadioModels.Add('Discover Media (US)');
  Variant.AlgorithmNotes := 'Modern North American with enhanced security';

  // Chinese Market (2008+)
  Variant := FVariantManager.AddVariant(
    'VW_CN',
    'VW China (2008+)',
    rcrAsia,
    2008, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('RCD China');
  Variant.AlgorithmNotes := 'Chinese market variant with region lock';
end;

function TOBDRadioCodeVWAdvanced.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('VW Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'VW Radio Code Calculator (Regional Variants)';
end;

procedure TOBDRadioCodeVWAdvanced.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeVWAdvanced.SetVariant(const Region: TRadioCodeRegion;
  const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear);
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodeVWAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeVWAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeVWAdvanced.CalculateGamma(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  LetterValue: Integer;
  Digit: Integer;
begin
  Code := 0;
  
  // Skip VWZ prefix
  for I := 4 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['A'..'Z']) then
    begin
      LetterValue := Ord(Serial[I]) - Ord('A') + 1;
      Code := Code + LetterValue;
    end
    else if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + Digit;
    end;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeVWAdvanced.CalculateBeta(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  LetterValue: Integer;
  Digit: Integer;
begin
  Code := 0;
  
  for I := 4 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['A'..'Z']) then
    begin
      LetterValue := Ord(Serial[I]) - Ord('A') + 1;
      Code := Code + (LetterValue * I); // Position-weighted
    end
    else if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * I);
    end;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeVWAdvanced.CalculateAlpha(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  LetterValue: Integer;
  Digit: Integer;
  Multiplier: Integer;
begin
  Code := 0;
  Multiplier := 1;
  
  for I := 4 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['A'..'Z']) then
    begin
      // Modified letter values for Alpha
      LetterValue := ((Ord(Serial[I]) - Ord('A') + 1) * 2) mod 26;
      Code := Code + (LetterValue * Multiplier);
    end
    else if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * Multiplier);
    end;
    Multiplier := Multiplier + 1;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeVWAdvanced.CalculateRCD(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  LetterValue: Integer;
  Digit: Integer;
begin
  Code := 0;
  
  for I := 4 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['A'..'Z']) then
    begin
      LetterValue := Ord(Serial[I]) - Ord('A') + 1;
      Code := Code + (LetterValue * 3);
    end
    else if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * 7);
    end;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeVWAdvanced.CalculateRNS(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  LetterValue: Integer;
  Digit: Integer;
begin
  Code := 0;
  
  for I := 4 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['A'..'Z']) then
    begin
      LetterValue := Ord(Serial[I]) - Ord('A') + 1;
      Code := Code + (LetterValue * (I - 3));
    end
    else if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * (I - 3) * 2);
    end;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeVWAdvanced.Validate(const Input: string;
  var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);

  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  if not (Copy(Sanitized, 1, 3) = 'VWZ') then
  begin
    ErrorMessage := 'Serial must start with VWZ';
    Exit(False);
  end;

  Result := True;
end;

function TOBDRadioCodeVWAdvanced.Calculate(const Input: string;
  var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := False;
  Output := '';

  if not Validate(Input, ErrorMessage) then
    Exit(False);

  Sanitized := SanitizeInput(Input);

  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;

  try
    if Pos('GAMMA', FCurrentVariant.VariantID) > 0 then
      Output := CalculateGamma(Sanitized)
    else if Pos('BETA', FCurrentVariant.VariantID) > 0 then
      Output := CalculateBeta(Sanitized)
    else if Pos('ALPHA', FCurrentVariant.VariantID) > 0 then
      Output := CalculateAlpha(Sanitized)
    else if Pos('RNS', FCurrentVariant.VariantID) > 0 then
      Output := CalculateRNS(Sanitized)
    else
      Output := CalculateRCD(Sanitized); // Default to RCD

    Result := True;
  except
    on E: Exception do
    begin
      ErrorMessage := 'Calculation error: ' + E.Message;
      Result := False;
    end;
  end;
end;

end.
