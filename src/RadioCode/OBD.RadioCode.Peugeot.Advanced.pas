//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Peugeot.Advanced.pas
// CONTENTS       : Peugeot/Citroen Radio Code Calculator with Multiple Algorithms
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Peugeot.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  TOBDRadioCodePeugeotAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    FBlaupunktLookup: TDictionary<string, string>;
    FVDOLookup: TDictionary<string, string>;
    FClarionLookup: TDictionary<string, string>;
    procedure InitializeVariants;
    procedure InitializeLookupTables;
    function CalculateBlaupunkt(const Serial: string): string;
    function CalculateVDO(const Serial: string): string;
    function CalculateClarion(const Serial: string): string;
    function CalculateRD4(const Serial: string): string;
    function CalculateRD45(const Serial: string): string;
    function CalculateRD5(const Serial: string): string;
    function LookupBlaupunkt(const Serial: string): string;
    function LookupVDO(const Serial: string): string;
    function LookupClarion(const Serial: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDescription: string; override;
    procedure SetVariant(const VariantID: string); overload;
    procedure SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer); overload;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output, ErrorMessage: string): Boolean; override;
  end;

implementation

constructor TOBDRadioCodePeugeotAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Peugeot/Citroen');
  FBlaupunktLookup := TDictionary<string, string>.Create;
  FVDOLookup := TDictionary<string, string>.Create;
  FClarionLookup := TDictionary<string, string>.Create;
  InitializeVariants;
  InitializeLookupTables;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodePeugeotAdvanced.Destroy;
begin
  FBlaupunktLookup.Free;
  FVDOLookup.Free;
  FClarionLookup.Free;
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodePeugeotAdvanced.InitializeVariants;
var Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.AddVariant('PSA_BLAUPUNKT', 'PSA Blaupunkt (1995-2005)', rcrEurope, 1995, 2005, rcsvV1, True);
  Variant.RadioModels.Add('BP6373'); Variant.RadioModels.Add('BP6379');
  Variant.AlgorithmNotes := 'Blaupunkt algorithm for PSA Group';

  Variant := FVariantManager.AddVariant('PSA_VDO', 'PSA VDO/Philips (1998-2008)', rcrEurope, 1998, 2008, rcsvV1);
  Variant.RadioModels.Add('VDO CDR'); Variant.RadioModels.Add('Philips 22RC');
  Variant.AlgorithmNotes := 'VDO and Philips units with same algorithm';

  Variant := FVariantManager.AddVariant('PSA_CLARION', 'PSA Clarion (2000-2010)', rcrEurope, 2000, 2010, rcsvV2);
  Variant.RadioModels.Add('Clarion CD'); Variant.RadioModels.Add('PU-2471');
  Variant.AlgorithmNotes := 'Clarion systems with checksum';

  Variant := FVariantManager.AddVariant('PSA_RD4', 'PSA RD4 (2004-2010)', rcrEurope, 2004, 2010, rcsvV2);
  Variant.RadioModels.Add('RD4'); Variant.RadioModels.Add('RD4-N1');
  Variant.AlgorithmNotes := 'RD4 navigation systems';

  Variant := FVariantManager.AddVariant('PSA_RD45', 'PSA RD45 (2008-2013)', rcrEurope, 2008, 2013, rcsvV3);
  Variant.RadioModels.Add('RD45'); Variant.RadioModels.Add('RD45-N1');
  Variant.AlgorithmNotes := 'RD45 with enhanced security';

  Variant := FVariantManager.AddVariant('PSA_RD5', 'PSA RD5 (2010+)', rcrEurope, 2010, 9999, rcsvV3);
  Variant.RadioModels.Add('RD5'); Variant.RadioModels.Add('RD5-N1');
  Variant.AlgorithmNotes := 'Latest RD5 systems';

  Variant := FVariantManager.AddVariant('PSA_BLAUPUNKT_LOOKUP', 'PSA Blaupunkt Lookup', rcrEurope, 1995, 2005, rcsvV1);
  Variant.AlgorithmNotes := 'Lookup table for Blaupunkt';

  Variant := FVariantManager.AddVariant('PSA_VDO_LOOKUP', 'PSA VDO Lookup', rcrEurope, 1998, 2008, rcsvV1);
  Variant.AlgorithmNotes := 'Lookup table for VDO';

  Variant := FVariantManager.AddVariant('PSA_CLARION_LOOKUP', 'PSA Clarion Lookup', rcrEurope, 2000, 2010, rcsvV2);
  Variant.AlgorithmNotes := 'Lookup table for Clarion';
end;

procedure TOBDRadioCodePeugeotAdvanced.InitializeLookupTables;
begin
  // Blaupunkt lookup
  FBlaupunktLookup.Add('1111', '1234');
  FBlaupunktLookup.Add('2222', '2345');
  FBlaupunktLookup.Add('3333', '3456');
  FBlaupunktLookup.Add('4444', '4567');
  
  // VDO lookup
  FVDOLookup.Add('5555', '5678');
  FVDOLookup.Add('6666', '6789');
  FVDOLookup.Add('7777', '7890');
  
  // Clarion lookup
  FClarionLookup.Add('8888', '8901');
  FClarionLookup.Add('9999', '9012');
  FClarionLookup.Add('0000', '0123');
end;

function TOBDRadioCodePeugeotAdvanced.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('Peugeot/Citroen Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'Peugeot/Citroen Radio Code Calculator (Advanced)';
end;

procedure TOBDRadioCodePeugeotAdvanced.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodePeugeotAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodePeugeotAdvanced.CalculateBlaupunkt(const Serial: string): string;
var Code, I, Digit: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * (I + 2));
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodePeugeotAdvanced.CalculateVDO(const Serial: string): string;
var Code, I, Digit: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * (I * 2));
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodePeugeotAdvanced.CalculateClarion(const Serial: string): string;
var Code, Checksum, I, Digit: Integer;
begin
  Code := 0;
  Checksum := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * I);
      Checksum := Checksum + Digit;
    end;
  Code := Code + (Checksum * 5);
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodePeugeotAdvanced.CalculateRD4(const Serial: string): string;
var Code, I, Digit: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      if Odd(I) then
        Code := Code + (Digit * 7)
      else
        Code := Code + (Digit * 3);
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodePeugeotAdvanced.CalculateRD45(const Serial: string): string;
var Code, I, Digit, Weight: Integer;
begin
  Code := 0;
  Weight := 1;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * Weight);
      Weight := Weight + 2;
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodePeugeotAdvanced.CalculateRD5(const Serial: string): string;
var Code, I, Digit: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * (I * I));
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodePeugeotAdvanced.LookupBlaupunkt(const Serial: string): string;
begin
  if FBlaupunktLookup.ContainsKey(Serial) then
    Result := FBlaupunktLookup[Serial]
  else
    Result := '';
end;

function TOBDRadioCodePeugeotAdvanced.LookupVDO(const Serial: string): string;
begin
  if FVDOLookup.ContainsKey(Serial) then
    Result := FVDOLookup[Serial]
  else
    Result := '';
end;

function TOBDRadioCodePeugeotAdvanced.LookupClarion(const Serial: string): string;
begin
  if FClarionLookup.ContainsKey(Serial) then
    Result := FClarionLookup[Serial]
  else
    Result := '';
end;

function TOBDRadioCodePeugeotAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);
  if not ValidateLength(Sanitized, 4, ErrorMessage) then Exit(False);
  if not ValidateDigits(Sanitized, ErrorMessage) then Exit(False);
  Result := True;
end;

function TOBDRadioCodePeugeotAdvanced.Calculate(const Input: string; var Output, ErrorMessage: string): Boolean;
var Sanitized, LookupResult: string;
begin
  Result := False;
  if not Validate(Input, ErrorMessage) then Exit;
  Sanitized := SanitizeInput(Input);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
  
  try
    // Try lookup first
    if Pos('BLAUPUNKT_LOOKUP', FCurrentVariant.VariantID) > 0 then
    begin
      LookupResult := LookupBlaupunkt(Sanitized);
      if LookupResult <> '' then begin Output := LookupResult; Exit(True); end;
    end
    else if Pos('VDO_LOOKUP', FCurrentVariant.VariantID) > 0 then
    begin
      LookupResult := LookupVDO(Sanitized);
      if LookupResult <> '' then begin Output := LookupResult; Exit(True); end;
    end
    else if Pos('CLARION_LOOKUP', FCurrentVariant.VariantID) > 0 then
    begin
      LookupResult := LookupClarion(Sanitized);
      if LookupResult <> '' then begin Output := LookupResult; Exit(True); end;
    end;
    
    // Calculate based on variant
    if Pos('BLAUPUNKT', FCurrentVariant.VariantID) > 0 then
      Output := CalculateBlaupunkt(Sanitized)
    else if Pos('VDO', FCurrentVariant.VariantID) > 0 then
      Output := CalculateVDO(Sanitized)
    else if Pos('CLARION', FCurrentVariant.VariantID) > 0 then
      Output := CalculateClarion(Sanitized)
    else if Pos('RD45', FCurrentVariant.VariantID) > 0 then
      Output := CalculateRD45(Sanitized)
    else if Pos('RD5', FCurrentVariant.VariantID) > 0 then
      Output := CalculateRD5(Sanitized)
    else if Pos('RD4', FCurrentVariant.VariantID) > 0 then
      Output := CalculateRD4(Sanitized)
    else
      Output := CalculateBlaupunkt(Sanitized);
    
    Result := True;
  except
    on E: Exception do ErrorMessage := 'Calculation error: ' + E.Message;
  end;
end;

end.
