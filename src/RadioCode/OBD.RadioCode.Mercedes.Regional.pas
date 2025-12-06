//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Mercedes.Regional.pas
// CONTENTS       : Mercedes Radio Code Calculator with Regional Variants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Mercedes.Regional;

interface

uses
  Winapi.Windows, System.SysUtils, OBD.RadioCode, OBD.RadioCode.Variants;

type
  TOBDRadioCodeMercedesRegional = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    procedure InitializeVariants;
    function CalculateBecker(const Serial: string): string;
    function CalculateAudio10(const Serial: string): string;
    function CalculateAudio20(const Serial: string): string;
    function CalculateCommand(const Serial: string): string;
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

constructor TOBDRadioCodeMercedesRegional.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Mercedes-Benz');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeMercedesRegional.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeMercedesRegional.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.AddVariant('MB_EU_BECKER', 'Mercedes Europe - Becker (1995-2005)', rcrEurope, 1995, 2005, rcsvV1, True);
  Variant.RadioModels.Add('BE1350'); Variant.RadioModels.Add('BE2010');
  Variant.AlgorithmNotes := 'Classic Becker algorithm';

  Variant := FVariantManager.AddVariant('MB_EU_AUDIO10', 'Mercedes Europe - Audio 10 (2000-2010)', rcrEurope, 2000, 2010, rcsvV2);
  Variant.RadioModels.Add('Audio 10'); Variant.RadioModels.Add('A1708200386');
  Variant.AlgorithmNotes := 'Audio 10 series algorithm';

  Variant := FVariantManager.AddVariant('MB_EU_AUDIO20', 'Mercedes Europe - Audio 20 (2005-2015)', rcrEurope, 2005, 2015, rcsvV2);
  Variant.RadioModels.Add('Audio 20'); Variant.RadioModels.Add('A2049003403');
  Variant.AlgorithmNotes := 'Audio 20 enhanced security';

  Variant := FVariantManager.AddVariant('MB_EU_COMMAND', 'Mercedes Europe - COMAND (2010+)', rcrEurope, 2010, 9999, rcsvV3);
  Variant.RadioModels.Add('COMAND APS'); Variant.RadioModels.Add('COMAND NTG5');
  Variant.AlgorithmNotes := 'COMAND navigation systems';

  Variant := FVariantManager.AddVariant('MB_NA', 'Mercedes North America (2000+)', rcrNorthAmerica, 2000, 9999, rcsvV2);
  Variant.RadioModels.Add('Audio Gateway'); Variant.AlgorithmNotes := 'North American variant';

  Variant := FVariantManager.AddVariant('MB_ASIA', 'Mercedes Asia (2005+)', rcrAsia, 2005, 9999, rcsvV2);
  Variant.AlgorithmNotes := 'Asian market variant';
end;

function TOBDRadioCodeMercedesRegional.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('Mercedes-Benz Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'Mercedes-Benz Radio Code Calculator';
end;

procedure TOBDRadioCodeMercedesRegional.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeMercedesRegional.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodeMercedesRegional.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeMercedesRegional.CalculateBecker(const Serial: string): string;
var Code, I, Val: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Val := StrToInt(Serial[I]);
      Code := Code + (Val * (I + 2));
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeMercedesRegional.CalculateAudio10(const Serial: string): string;
var Code, I, Val: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Val := StrToInt(Serial[I]);
      Code := Code + (Val * (I * 3));
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeMercedesRegional.CalculateAudio20(const Serial: string): string;
var Code, I, Val: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Val := StrToInt(Serial[I]);
      if Odd(I) then Code := Code + (Val * 7) else Code := Code + (Val * 5);
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeMercedesRegional.CalculateCommand(const Serial: string): string;
var Code, I, Val: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Val := StrToInt(Serial[I]);
      Code := Code + (Val * (I * I));
    end;
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeMercedesRegional.Validate(const Input: string; var ErrorMessage: string): Boolean;
var Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);
  if not ValidateLength(Sanitized, 14, ErrorMessage) then Exit(False);
  Result := True;
end;

function TOBDRadioCodeMercedesRegional.Calculate(const Input: string; var Output, ErrorMessage: string): Boolean;
var Sanitized: string;
begin
  Result := False;
  if not Validate(Input, ErrorMessage) then Exit;
  Sanitized := SanitizeInput(Input);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
  
  try
    if Pos('BECKER', FCurrentVariant.VariantID) > 0 then
      Output := CalculateBecker(Sanitized)
    else if Pos('AUDIO10', FCurrentVariant.VariantID) > 0 then
      Output := CalculateAudio10(Sanitized)
    else if Pos('AUDIO20', FCurrentVariant.VariantID) > 0 then
      Output := CalculateAudio20(Sanitized)
    else if Pos('COMMAND', FCurrentVariant.VariantID) > 0 then
      Output := CalculateCommand(Sanitized)
    else
      Output := CalculateBecker(Sanitized);
    Result := True;
  except
    on E: Exception do ErrorMessage := 'Calculation error: ' + E.Message;
  end;
end;

end.
