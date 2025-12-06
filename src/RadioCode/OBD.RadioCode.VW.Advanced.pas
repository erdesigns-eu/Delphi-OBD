//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.VW.Advanced.pas
// CONTENTS       : VW Radio Code Calculator with Multiple Algorithms
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.VW.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   VW Radio Code Calculator with Multiple Algorithms
  /// </summary>
  TOBDRadioCodeVWAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    FRCDLookup: TDictionary<string, string>;
    procedure InitializeVariants;
    procedure InitializeLookupTables;
    function CalculateGamma(const Serial: string): string;
    function CalculateBeta(const Serial: string): string;
    function CalculateAlpha(const Serial: string): string;
    function CalculateRCD(const Serial: string): string;
    function CalculateRCDEnhanced(const Serial: string): string;
    function CalculateRNS(const Serial: string): string;
    function CalculateRNSLatest(const Serial: string): string;
    function LookupRCD(const Serial: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetDescription: string; override;
    procedure SetVariant(const VariantID: string); overload;
    procedure SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer); overload;
    function GetCurrentVariant: TRadioCodeVariant;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

constructor TOBDRadioCodeVWAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('VW');
  FRCDLookup := TDictionary<string, string>.Create;
  InitializeVariants;
  InitializeLookupTables;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeVWAdvanced.Destroy;
begin
  FRCDLookup.Free;
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeVWAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Gamma (1995-2000)
  Variant := FVariantManager.AddVariant(
    'VW_GAMMA',
    'VW Gamma (1995-2000)',
    rcrEurope,
    1995, 2000,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Gamma');
  Variant.AlgorithmNotes := 'Simple letter+digit sum algorithm';

  // Beta (1998-2003)
  Variant := FVariantManager.AddVariant(
    'VW_BETA',
    'VW Beta (1998-2003)',
    rcrEurope,
    1998, 2003,
    rcsvV1
  );
  Variant.RadioModels.Add('Beta');
  Variant.AlgorithmNotes := 'Position-weighted calculation';

  // Alpha (2000-2005)
  Variant := FVariantManager.AddVariant(
    'VW_ALPHA',
    'VW Alpha (2000-2005)',
    rcrEurope,
    2000, 2005,
    rcsvV2
  );
  Variant.RadioModels.Add('Alpha');
  Variant.AlgorithmNotes := 'Modified letter values (×2 mod 26)';

  // RCD (2003-2012)
  Variant := FVariantManager.AddVariant(
    'VW_RCD',
    'VW RCD (2003-2012)',
    rcrEurope,
    2003, 2012,
    rcsvV2
  );
  Variant.RadioModels.Add('RCD 200');
  Variant.RadioModels.Add('RCD 210');
  Variant.RadioModels.Add('RCD 300');
  Variant.RadioModels.Add('RCD 310');
  Variant.AlgorithmNotes := 'Letter×3 + Digit×7 algorithm';

  // RCD Enhanced (2013+)
  Variant := FVariantManager.AddVariant(
    'VW_RCD_ENHANCED',
    'VW RCD Enhanced (2013+)',
    rcrEurope,
    2013, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('RCD 510');
  Variant.AlgorithmNotes := 'Additional security layer with checksum';

  // RNS Navigation (2005-2015)
  Variant := FVariantManager.AddVariant(
    'VW_RNS',
    'VW RNS Navigation (2005-2015)',
    rcrEurope,
    2005, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('RNS 300');
  Variant.RadioModels.Add('RNS 310');
  Variant.RadioModels.Add('RNS 315');
  Variant.AlgorithmNotes := 'Navigation-specific algorithm';

  // RNS Latest (2016+)
  Variant := FVariantManager.AddVariant(
    'VW_RNS_LATEST',
    'VW RNS Latest (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('RNS 510');
  Variant.AlgorithmNotes := 'Enhanced navigation security';

  // Lookup Table
  Variant := FVariantManager.AddVariant(
    'VW_RCD_LOOKUP',
    'VW RCD Lookup Table',
    rcrEurope,
    2003, 9999,
    rcsvV2
  );
  Variant.AlgorithmNotes := 'Lookup table for known RCD serials';
end;

procedure TOBDRadioCodeVWAdvanced.InitializeLookupTables;
begin
  // RCD lookup - sample entries
  FRCDLookup.Add('VWZ1Z1A1234567', '1234');
  FRCDLookup.Add('VWZ2Z2B2345678', '5678');
  FRCDLookup.Add('VWZ3Z3C3456789', '9012');
  FRCDLookup.Add('VWZ4Z4D4567890', '3456');
  FRCDLookup.Add('VWZ5Z5E5678901', '7890');
end;

function TOBDRadioCodeVWAdvanced.CalculateGamma(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
  LetterValue: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if Serial[I].IsLetter then
    begin
      LetterValue := Ord(Serial[I].ToUpper) - Ord('A') + 1;
      Sum := Sum + LetterValue;
    end
    else if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + Digit;
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeVWAdvanced.CalculateBeta(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
  LetterValue: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if Serial[I].IsLetter then
    begin
      LetterValue := Ord(Serial[I].ToUpper) - Ord('A') + 1;
      Sum := Sum + (LetterValue * I);
    end
    else if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * I);
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeVWAdvanced.CalculateAlpha(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
  LetterValue: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if Serial[I].IsLetter then
    begin
      LetterValue := Ord(Serial[I].ToUpper) - Ord('A') + 1;
      LetterValue := (LetterValue * 2) mod 26;
      Sum := Sum + (LetterValue * I);
    end
    else if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * I);
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeVWAdvanced.CalculateRCD(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
  LetterValue: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if Serial[I].IsLetter then
    begin
      LetterValue := Ord(Serial[I].ToUpper) - Ord('A') + 1;
      Sum := Sum + (LetterValue * 3);
    end
    else if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * 7);
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeVWAdvanced.CalculateRCDEnhanced(const Serial: string): string;
var
  I, Sum, Checksum: Integer;
  Digit: Integer;
  LetterValue: Integer;
begin
  Sum := 0;
  Checksum := 0;
  for I := 1 to Length(Serial) do
  begin
    if Serial[I].IsLetter then
    begin
      LetterValue := Ord(Serial[I].ToUpper) - Ord('A') + 1;
      Sum := Sum + (LetterValue * 3);
      Checksum := Checksum + LetterValue;
    end
    else if TryStrToInt(Serial[I], Digit) then
    begin
      Sum := Sum + (Digit * 7);
      Checksum := Checksum + Digit;
    end;
  end;
  Sum := Sum + (Checksum mod 100);
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeVWAdvanced.CalculateRNS(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
  LetterValue: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if Serial[I].IsLetter then
    begin
      LetterValue := Ord(Serial[I].ToUpper) - Ord('A') + 1;
      Sum := Sum + (LetterValue * 5);
    end
    else if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * 9);
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeVWAdvanced.CalculateRNSLatest(const Serial: string): string;
var
  I, Sum, Checksum: Integer;
  Digit: Integer;
  LetterValue: Integer;
begin
  Sum := 0;
  Checksum := 0;
  for I := 1 to Length(Serial) do
  begin
    if Serial[I].IsLetter then
    begin
      LetterValue := Ord(Serial[I].ToUpper) - Ord('A') + 1;
      Sum := Sum + (LetterValue * 5);
      Checksum := Checksum + LetterValue;
    end
    else if TryStrToInt(Serial[I], Digit) then
    begin
      Sum := Sum + (Digit * 9);
      Checksum := Checksum + Digit;
    end;
  end;
  Sum := Sum + (Checksum * 3 mod 100);
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeVWAdvanced.LookupRCD(const Serial: string): string;
begin
  if FRCDLookup.ContainsKey(Serial) then
    Result := FRCDLookup[Serial]
  else
    Result := CalculateRCD(Serial);
end;

function TOBDRadioCodeVWAdvanced.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('VW Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'VW Radio Code Calculator (Advanced)';
end;

procedure TOBDRadioCodeVWAdvanced.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeVWAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodeVWAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeVWAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);
  
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);
    
  Result := True;
end;

function TOBDRadioCodeVWAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  if not Validate(Input, ErrorMessage) then
    Exit(False);
    
  Sanitized := SanitizeInput(Input);
  
  if FCurrentVariant = nil then
  begin
    ErrorMessage := 'No variant selected';
    Exit(False);
  end;
  
  if FCurrentVariant.VariantID = 'VW_GAMMA' then
    Output := CalculateGamma(Sanitized)
  else if FCurrentVariant.VariantID = 'VW_BETA' then
    Output := CalculateBeta(Sanitized)
  else if FCurrentVariant.VariantID = 'VW_ALPHA' then
    Output := CalculateAlpha(Sanitized)
  else if FCurrentVariant.VariantID = 'VW_RCD' then
    Output := CalculateRCD(Sanitized)
  else if FCurrentVariant.VariantID = 'VW_RCD_ENHANCED' then
    Output := CalculateRCDEnhanced(Sanitized)
  else if FCurrentVariant.VariantID = 'VW_RNS' then
    Output := CalculateRNS(Sanitized)
  else if FCurrentVariant.VariantID = 'VW_RNS_LATEST' then
    Output := CalculateRNSLatest(Sanitized)
  else if FCurrentVariant.VariantID = 'VW_RCD_LOOKUP' then
    Output := LookupRCD(Sanitized)
  else
    Output := CalculateRCD(Sanitized);
    
  Result := True;
end;

end.
