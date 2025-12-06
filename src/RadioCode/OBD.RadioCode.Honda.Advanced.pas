//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Honda.Advanced.pas
// CONTENTS       : Honda Radio Code Calculator with Multiple Algorithms
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Honda.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Honda Radio Code Calculator with Multiple Algorithms
  /// </summary>
  TOBDRadioCodeHondaAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    FAlpineLookup: TDictionary<string, string>;
    FPanasonicLookup: TDictionary<string, string>;
    procedure InitializeVariants;
    procedure InitializeLookupTables;
    function CalculateAlpineClassic(const Serial: string): string;
    function CalculateAlpineEnhanced(const Serial: string): string;
    function CalculatePanasonic(const Serial: string): string;
    function CalculateClarion(const Serial: string): string;
    function CalculatePremiumAudio(const Serial: string): string;
    function LookupAlpine(const Serial: string): string;
    function LookupPanasonic(const Serial: string): string;
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

constructor TOBDRadioCodeHondaAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Honda');
  FAlpineLookup := TDictionary<string, string>.Create;
  FPanasonicLookup := TDictionary<string, string>.Create;
  InitializeVariants;
  InitializeLookupTables;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeHondaAdvanced.Destroy;
begin
  FAlpineLookup.Free;
  FPanasonicLookup.Free;
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeHondaAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Alpine Classic (1998-2005)
  Variant := FVariantManager.AddVariant(
    'HONDA_ALPINE_CLASSIC',
    'Honda Alpine Classic (1998-2005)',
    rcrAsia,
    1998, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Alpine 39100-S84');
  Variant.RadioModels.Add('39100-SDA');
  Variant.AlgorithmNotes := 'Basic position multiplier algorithm';

  // Alpine Enhanced (2006-2012)
  Variant := FVariantManager.AddVariant(
    'HONDA_ALPINE_ENHANCED',
    'Honda Alpine Enhanced (2006-2012)',
    rcrAsia,
    2006, 2012,
    rcsvV2
  );
  Variant.RadioModels.Add('Alpine 39100-SHJ');
  Variant.RadioModels.Add('39100-SNV');
  Variant.AlgorithmNotes := 'Position * 2 multiplier with checksum';

  // Panasonic (2000-2010)
  Variant := FVariantManager.AddVariant(
    'HONDA_PANASONIC',
    'Honda Panasonic (2000-2010)',
    rcrAsia,
    2000, 2010,
    rcsvV1
  );
  Variant.RadioModels.Add('Panasonic CQ-EH');
  Variant.AlgorithmNotes := 'Alternating 7/3 weights algorithm';

  // Clarion (2002-2012)
  Variant := FVariantManager.AddVariant(
    'HONDA_CLARION',
    'Honda Clarion (2002-2012)',
    rcrAsia,
    2002, 2012,
    rcsvV1
  );
  Variant.RadioModels.Add('Clarion CX-');
  Variant.AlgorithmNotes := 'Doubled position multiplier';

  // Premium Audio (2010+)
  Variant := FVariantManager.AddVariant(
    'HONDA_PREMIUM',
    'Honda Premium Audio (2010+)',
    rcrAsia,
    2010, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Premium Audio System');
  Variant.AlgorithmNotes := 'Squared position weights for enhanced security';

  // Lookup Tables
  Variant := FVariantManager.AddVariant(
    'HONDA_ALPINE_LOOKUP',
    'Honda Alpine Lookup Table',
    rcrAsia,
    1998, 2012,
    rcsvV1
  );
  Variant.AlgorithmNotes := 'Lookup table for known Alpine serials';

  Variant := FVariantManager.AddVariant(
    'HONDA_PANASONIC_LOOKUP',
    'Honda Panasonic Lookup Table',
    rcrAsia,
    2000, 2010,
    rcsvV1
  );
  Variant.AlgorithmNotes := 'Lookup table for known Panasonic serials';
end;

procedure TOBDRadioCodeHondaAdvanced.InitializeLookupTables;
begin
  // Alpine lookup - sample entries
  FAlpineLookup.Add('U1234567', '1234');
  FAlpineLookup.Add('V2345678', '5678');
  FAlpineLookup.Add('W3456789', '9012');
  FAlpineLookup.Add('X4567890', '3456');
  FAlpineLookup.Add('Y5678901', '7890');

  // Panasonic lookup - sample entries
  FPanasonicLookup.Add('A1234567', '2345');
  FPanasonicLookup.Add('B2345678', '6789');
  FPanasonicLookup.Add('C3456789', '0123');
  FPanasonicLookup.Add('D4567890', '4567');
  FPanasonicLookup.Add('E5678901', '8901');
end;

function TOBDRadioCodeHondaAdvanced.CalculateAlpineClassic(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  // Skip first character (letter)
  for I := 2 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * (I - 1));
  end;
  Result := IntToStr(Sum mod 100000).PadLeft(5, '0');
end;

function TOBDRadioCodeHondaAdvanced.CalculateAlpineEnhanced(const Serial: string): string;
var
  I, Sum, Checksum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  Checksum := 0;
  for I := 2 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
    begin
      Sum := Sum + (Digit * (I - 1) * 2);
      Checksum := Checksum + Digit;
    end;
  end;
  Sum := Sum + (Checksum mod 100);
  Result := IntToStr(Sum mod 100000).PadLeft(5, '0');
end;

function TOBDRadioCodeHondaAdvanced.CalculatePanasonic(const Serial: string): string;
var
  I, Sum: Integer;
  Digit, Weight: Integer;
begin
  Sum := 0;
  for I := 2 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
    begin
      Weight := IfThen((I mod 2) = 0, 7, 3);
      Sum := Sum + (Digit * Weight);
    end;
  end;
  Result := IntToStr(Sum mod 100000).PadLeft(5, '0');
end;

function TOBDRadioCodeHondaAdvanced.CalculateClarion(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  for I := 2 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * (I - 1) * 2);
  end;
  Result := IntToStr(Sum mod 100000).PadLeft(5, '0');
end;

function TOBDRadioCodeHondaAdvanced.CalculatePremiumAudio(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  for I := 2 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * (I - 1) * (I - 1));
  end;
  Result := IntToStr(Sum mod 100000).PadLeft(5, '0');
end;

function TOBDRadioCodeHondaAdvanced.LookupAlpine(const Serial: string): string;
begin
  if FAlpineLookup.ContainsKey(Serial) then
    Result := FAlpineLookup[Serial]
  else
    Result := CalculateAlpineClassic(Serial);
end;

function TOBDRadioCodeHondaAdvanced.LookupPanasonic(const Serial: string): string;
begin
  if FPanasonicLookup.ContainsKey(Serial) then
    Result := FPanasonicLookup[Serial]
  else
    Result := CalculatePanasonic(Serial);
end;

function TOBDRadioCodeHondaAdvanced.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('Honda Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'Honda Radio Code Calculator (Advanced)';
end;

procedure TOBDRadioCodeHondaAdvanced.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeHondaAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodeHondaAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeHondaAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);
  
  if not ValidateLength(Sanitized, 8, ErrorMessage) then
    Exit(False);
    
  if not CharInSet(Sanitized[1], ['A'..'Z']) then
  begin
    ErrorMessage := 'Must start with a letter (A-Z)!';
    Exit(False);
  end;
    
  Result := True;
end;

function TOBDRadioCodeHondaAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if FCurrentVariant.VariantID = 'HONDA_ALPINE_CLASSIC' then
    Output := CalculateAlpineClassic(Sanitized)
  else if FCurrentVariant.VariantID = 'HONDA_ALPINE_ENHANCED' then
    Output := CalculateAlpineEnhanced(Sanitized)
  else if FCurrentVariant.VariantID = 'HONDA_PANASONIC' then
    Output := CalculatePanasonic(Sanitized)
  else if FCurrentVariant.VariantID = 'HONDA_CLARION' then
    Output := CalculateClarion(Sanitized)
  else if FCurrentVariant.VariantID = 'HONDA_PREMIUM' then
    Output := CalculatePremiumAudio(Sanitized)
  else if FCurrentVariant.VariantID = 'HONDA_ALPINE_LOOKUP' then
    Output := LookupAlpine(Sanitized)
  else if FCurrentVariant.VariantID = 'HONDA_PANASONIC_LOOKUP' then
    Output := LookupPanasonic(Sanitized)
  else
    Output := CalculateAlpineClassic(Sanitized);
    
  Result := True;
end;

end.
