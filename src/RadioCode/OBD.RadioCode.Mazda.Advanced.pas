unit OBD.RadioCode.Mazda.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  TOBDRadioCodeMazdaAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    FClarionLookup: TDictionary<string, string>;
    procedure InitializeVariants;
    procedure InitializeLookupTables;
    function CalculateClarionClassic(const Serial: string): string;
    function CalculateClarionEnhanced(const Serial: string): string;
    function CalculateBose(const Serial: string): string;
    function CalculatePremium(const Serial: string): string;
    function LookupClarion(const Serial: string): string;
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

constructor TOBDRadioCodeMazdaAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Mazda');
  FClarionLookup := TDictionary<string, string>.Create;
  InitializeVariants;
  InitializeLookupTables;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeMazdaAdvanced.Destroy;
begin
  FClarionLookup.Free;
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeMazdaAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.AddVariant('MAZDA_CLARION_CLASSIC', 'Mazda Clarion Classic (1998-2006)', rcrAsia, 1998, 2006, rcsvV1, True);
  Variant.RadioModels.Add('Clarion CAX');
  Variant.AlgorithmNotes := 'Position multiplier algorithm';
  
  Variant := FVariantManager.AddVariant('MAZDA_CLARION_ENHANCED', 'Mazda Clarion Enhanced (2007-2013)', rcrAsia, 2007, 2013, rcsvV2);
  Variant.RadioModels.Add('Clarion CC29');
  Variant.AlgorithmNotes := 'Enhanced with checksum';
  
  Variant := FVariantManager.AddVariant('MAZDA_BOSE', 'Mazda Bose Premium (2010+)', rcrAsia, 2010, 9999, rcsvV2);
  Variant.RadioModels.Add('Bose');
  Variant.AlgorithmNotes := 'Alternating weights';
  
  Variant := FVariantManager.AddVariant('MAZDA_PREMIUM', 'Mazda Premium (2014+)', rcrAsia, 2014, 9999, rcsvV3);
  Variant.RadioModels.Add('MZD Connect');
  Variant.AlgorithmNotes := 'Squared position weights';
  
  Variant := FVariantManager.AddVariant('MAZDA_LOOKUP', 'Mazda Lookup Table', rcrAsia, 1998, 9999, rcsvV1);
  Variant.AlgorithmNotes := 'Lookup table for known serials';
end;

procedure TOBDRadioCodeMazdaAdvanced.InitializeLookupTables;
begin
  FClarionLookup.Add('1234', '1234');
  FClarionLookup.Add('2345', '5678');
  FClarionLookup.Add('3456', '9012');
end;

function TOBDRadioCodeMazdaAdvanced.CalculateClarionClassic(const Serial: string): string;
var I, Sum, Digit: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * I);
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeMazdaAdvanced.CalculateClarionEnhanced(const Serial: string): string;
var I, Sum, Checksum, Digit: Integer;
begin
  Sum := 0; Checksum := 0;
  for I := 1 to Length(Serial) do
    if TryStrToInt(Serial[I], Digit) then begin
      Sum := Sum + (Digit * I * 2);
      Checksum := Checksum + Digit;
    end;
  Sum := Sum + (Checksum mod 100);
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeMazdaAdvanced.CalculateBose(const Serial: string): string;
var I, Sum, Digit, Weight: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
    if TryStrToInt(Serial[I], Digit) then begin
      Weight := IfThen((I mod 2) = 1, 7, 3);
      Sum := Sum + (Digit * Weight);
    end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeMazdaAdvanced.CalculatePremium(const Serial: string): string;
var I, Sum, Digit: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * I * I);
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeMazdaAdvanced.LookupClarion(const Serial: string): string;
begin
  if FClarionLookup.ContainsKey(Serial) then
    Result := FClarionLookup[Serial]
  else
    Result := CalculateClarionClassic(Serial);
end;

function TOBDRadioCodeMazdaAdvanced.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('Mazda Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'Mazda Radio Code Calculator (Advanced)';
end;

procedure TOBDRadioCodeMazdaAdvanced.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeMazdaAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodeMazdaAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeMazdaAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);
  if not ValidateLength(Sanitized, 4, ErrorMessage) then Exit(False);
  if not ValidateDigits(Sanitized, ErrorMessage) then Exit(False);
  Result := True;
end;

function TOBDRadioCodeMazdaAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var Sanitized: string;
begin
  if not Validate(Input, ErrorMessage) then Exit(False);
  Sanitized := SanitizeInput(Input);
  if FCurrentVariant = nil then begin ErrorMessage := 'No variant selected'; Exit(False); end;
  
  if FCurrentVariant.VariantID = 'MAZDA_CLARION_CLASSIC' then Output := CalculateClarionClassic(Sanitized)
  else if FCurrentVariant.VariantID = 'MAZDA_CLARION_ENHANCED' then Output := CalculateClarionEnhanced(Sanitized)
  else if FCurrentVariant.VariantID = 'MAZDA_BOSE' then Output := CalculateBose(Sanitized)
  else if FCurrentVariant.VariantID = 'MAZDA_PREMIUM' then Output := CalculatePremium(Sanitized)
  else if FCurrentVariant.VariantID = 'MAZDA_LOOKUP' then Output := LookupClarion(Sanitized)
  else Output := CalculateClarionClassic(Sanitized);
  Result := True;
end;

end.
