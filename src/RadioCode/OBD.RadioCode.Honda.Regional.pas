//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Honda.Regional.pas
// CONTENTS       : Honda Radio Code Calculator with Regional Variants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Honda.Regional;

interface

uses
  Winapi.Windows, System.SysUtils, OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Honda Radio Code Calculator with Regional Variant Support
  /// </summary>
  TOBDRadioCodeHondaRegional = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    procedure InitializeVariants;
    function CalculateAlpine(const Serial: string): string;
    function CalculatePanasonic(const Serial: string): string;
    function CalculateClarion(const Serial: string): string;
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

constructor TOBDRadioCodeHondaRegional.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Honda');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeHondaRegional.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeHondaRegional.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Japanese Market - Alpine (1995-2005)
  Variant := FVariantManager.AddVariant(
    'HONDA_JP_ALPINE_OLD',
    'Honda Japan - Alpine (1995-2005)',
    rcrAsia,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('39100-S0A');
  Variant.RadioModels.Add('39100-S10');
  Variant.AlgorithmNotes := 'Original Alpine algorithm for Japanese market';

  // Japanese Market - Alpine (2006+)
  Variant := FVariantManager.AddVariant(
    'HONDA_JP_ALPINE_NEW',
    'Honda Japan - Alpine (2006+)',
    rcrAsia,
    2006, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('39100-SDA');
  Variant.RadioModels.Add('39100-SNA');
  Variant.RadioModels.Add('39100-SWA');
  Variant.AlgorithmNotes := 'Updated Alpine with enhanced security';

  // Japanese Market - Panasonic (2000+)
  Variant := FVariantManager.AddVariant(
    'HONDA_JP_PANASONIC',
    'Honda Japan - Panasonic (2000+)',
    rcrAsia,
    2000, 9999,
    rcsvV1
  );
  Variant.RadioModels.Add('39100-SEA');
  Variant.RadioModels.Add('39100-SHJ');
  Variant.AlgorithmNotes := 'Panasonic systems with different calculation';

  // Japanese Market - Clarion (1998-2010)
  Variant := FVariantManager.AddVariant(
    'HONDA_JP_CLARION',
    'Honda Japan - Clarion (1998-2010)',
    rcrAsia,
    1998, 2010,
    rcsvV1
  );
  Variant.RadioModels.Add('39100-S84');
  Variant.RadioModels.Add('39100-S9A');
  Variant.AlgorithmNotes := 'Clarion variant for Japanese domestic market';

  // North American - Alpine (1998-2008)
  Variant := FVariantManager.AddVariant(
    'HONDA_NA_ALPINE',
    'Honda North America - Alpine (1998-2008)',
    rcrNorthAmerica,
    1998, 2008,
    rcsvV1
  );
  Variant.RadioModels.Add('39100-S0A-A');
  Variant.RadioModels.Add('39100-SDA-A');
  Variant.AlgorithmNotes := 'North American Alpine variant';

  // North American - Modern (2009+)
  Variant := FVariantManager.AddVariant(
    'HONDA_NA_MODERN',
    'Honda North America - Modern (2009+)',
    rcrNorthAmerica,
    2009, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('39100-TK8');
  Variant.RadioModels.Add('39540-T0A');
  Variant.AlgorithmNotes := 'Modern North American with updated security';

  // European Market (2000-2010)
  Variant := FVariantManager.AddVariant(
    'HONDA_EU_CLASSIC',
    'Honda Europe - Classic (2000-2010)',
    rcrEurope,
    2000, 2010,
    rcsvV1
  );
  Variant.RadioModels.Add('39100-SMG');
  Variant.RadioModels.Add('39100-SNA-E');
  Variant.AlgorithmNotes := 'European market variant';

  // European Market (2011+)
  Variant := FVariantManager.AddVariant(
    'HONDA_EU_MODERN',
    'Honda Europe - Modern (2011+)',
    rcrEurope,
    2011, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('39100-T0A-E');
  Variant.RadioModels.Add('39101-T6A-E');
  Variant.AlgorithmNotes := 'Modern European with regional protection';

  // Australian Market (2000+)
  Variant := FVariantManager.AddVariant(
    'HONDA_AU',
    'Honda Australia (2000+)',
    rcrAustralia,
    2000, 9999,
    rcsvV1
  );
  Variant.RadioModels.Add('39100-SDA-AU');
  Variant.AlgorithmNotes := 'Australian market based on Japanese algorithm';
end;

function TOBDRadioCodeHondaRegional.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('Honda Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'Honda Radio Code Calculator (Regional Variants)';
end;

procedure TOBDRadioCodeHondaRegional.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeHondaRegional.SetVariant(const Region: TRadioCodeRegion;
  const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear);
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodeHondaRegional.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeHondaRegional.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeHondaRegional.CalculateAlpine(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  Digit: Integer;
  Letter: Char;
begin
  Code := 0;
  
  // Skip first letter
  Letter := Serial[1];
  Code := (Ord(Letter) - Ord('A') + 1) * 10;
  
  // Process digits
  for I := 2 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * I);
    end;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeHondaRegional.CalculatePanasonic(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  Digit: Integer;
  Letter: Char;
begin
  Code := 0;
  
  Letter := Serial[1];
  Code := (Ord(Letter) - Ord('A') + 1) * 15;
  
  for I := 2 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      // Panasonic uses different weighting
      if Odd(I) then
        Code := Code + (Digit * 5)
      else
        Code := Code + (Digit * 3);
    end;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeHondaRegional.CalculateClarion(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  Digit: Integer;
  Letter: Char;
begin
  Code := 0;
  
  Letter := Serial[1];
  Code := (Ord(Letter) - Ord('A') + 1) * 7;
  
  for I := 2 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * (I * 2));
    end;
  end;
  
  Code := ApplyModularTransform(Code, 10000);
  Result := Format('%.4d', [Code]);
end;

function TOBDRadioCodeHondaRegional.Validate(const Input: string;
  var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);

  if not ValidateLength(Sanitized, 8, ErrorMessage) then
    Exit(False);

  if not CharInSet(Sanitized[1], ['A'..'Z']) then
  begin
    ErrorMessage := 'Serial must start with a letter';
    Exit(False);
  end;

  Result := True;
end;

function TOBDRadioCodeHondaRegional.Calculate(const Input: string;
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
    if Pos('PANASONIC', FCurrentVariant.VariantID) > 0 then
      Output := CalculatePanasonic(Sanitized)
    else if Pos('CLARION', FCurrentVariant.VariantID) > 0 then
      Output := CalculateClarion(Sanitized)
    else
      Output := CalculateAlpine(Sanitized); // Default to Alpine

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
