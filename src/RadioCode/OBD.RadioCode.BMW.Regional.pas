//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.BMW.Regional.pas
// CONTENTS       : BMW Radio Code Calculator with Regional Variants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.BMW.Regional;

interface

uses
  Winapi.Windows, System.SysUtils, OBD.RadioCode, OBD.RadioCode.Variants;

type
  TOBDRadioCodeBMWRegional = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    procedure InitializeVariants;
    function CalculateBusinessCD(const Serial: string): string;
    function CalculateProfessional(const Serial: string): string;
    function CalculateIDrive(const Serial: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDescription: string; override;
    procedure SetVariant(const VariantID: string); overload;
    procedure SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer); overload;
    function GetCurrentVariant: TRadioCodeVariant;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output, ErrorMessage: string): Boolean; override;
  end;

implementation

constructor TOBDRadioCodeBMWRegional.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('BMW');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeBMWRegional.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeBMWRegional.InitializeVariants;
var Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.AddVariant('BMW_EU_BUSINESS', 'BMW Europe - Business CD (1998-2008)', rcrEurope, 1998, 2008, rcsvV1, True);
  Variant.RadioModels.Add('Business CD'); Variant.RadioModels.Add('65126943431');
  
  Variant := FVariantManager.AddVariant('BMW_EU_PROF', 'BMW Europe - Professional (2004-2012)', rcrEurope, 2004, 2012, rcsvV2);
  Variant.RadioModels.Add('Professional'); Variant.RadioModels.Add('65129224887');
  
  Variant := FVariantManager.AddVariant('BMW_EU_IDRIVE', 'BMW Europe - iDrive (2008+)', rcrEurope, 2008, 9999, rcsvV3);
  Variant.RadioModels.Add('iDrive'); Variant.RadioModels.Add('NBT');
  
  Variant := FVariantManager.AddVariant('BMW_NA', 'BMW North America (2000+)', rcrNorthAmerica, 2000, 9999, rcsvV2);
  Variant := FVariantManager.AddVariant('BMW_ASIA', 'BMW Asia (2005+)', rcrAsia, 2005, 9999, rcsvV2);
end;

function TOBDRadioCodeBMWRegional.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('BMW Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'BMW Radio Code Calculator';
end;

procedure TOBDRadioCodeBMWRegional.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeBMWRegional.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear);
end;

function TOBDRadioCodeBMWRegional.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeBMWRegional.CalculateBusinessCD(const Serial: string): string;
var Code, I, Val: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Val := StrToInt(Serial[I]);
      Code := Code + (Val * I);
    end;
  Code := ApplyModularTransform(Code, 100000);
  Result := Format('%.5d', [Code]);
end;

function TOBDRadioCodeBMWRegional.CalculateProfessional(const Serial: string): string;
var Code, I, Val: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Val := StrToInt(Serial[I]);
      Code := Code + (Val * (I * 2));
    end;
  Code := ApplyModularTransform(Code, 100000);
  Result := Format('%.5d', [Code]);
end;

function TOBDRadioCodeBMWRegional.CalculateIDrive(const Serial: string): string;
var Code, I, Val: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Val := StrToInt(Serial[I]);
      Code := Code + (Val * (I * I));
    end;
  Code := ApplyModularTransform(Code, 100000);
  Result := Format('%.5d', [Code]);
end;

function TOBDRadioCodeBMWRegional.Validate(const Input: string; var ErrorMessage: string): Boolean;
var Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);
  if not ValidateLength(Sanitized, 7, ErrorMessage) then Exit(False);
  if not ValidateDigits(Sanitized, ErrorMessage) then Exit(False);
  Result := True;
end;

function TOBDRadioCodeBMWRegional.Calculate(const Input: string; var Output, ErrorMessage: string): Boolean;
var Sanitized: string;
begin
  Result := False;
  if not Validate(Input, ErrorMessage) then Exit;
  Sanitized := SanitizeInput(Input);
  if FCurrentVariant = nil then FCurrentVariant := FVariantManager.GetDefaultVariant;
  
  try
    if Pos('BUSINESS', FCurrentVariant.VariantID) > 0 then
      Output := CalculateBusinessCD(Sanitized)
    else if Pos('PROF', FCurrentVariant.VariantID) > 0 then
      Output := CalculateProfessional(Sanitized)
    else if Pos('IDRIVE', FCurrentVariant.VariantID) > 0 then
      Output := CalculateIDrive(Sanitized)
    else
      Output := CalculateBusinessCD(Sanitized);
    Result := True;
  except
    on E: Exception do ErrorMessage := 'Calculation error: ' + E.Message;
  end;
end;

end.
