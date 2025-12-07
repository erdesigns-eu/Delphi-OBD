//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Toyota.Advanced.pas
// CONTENTS       : Toyota Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Toyota.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Toyota Advanced Radio Code Calculator with Multiple Algorithm Variants
  /// </summary>
  TOBDRadioCodeToyotaAdvanced = class(TOBDRadioCode)
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

constructor TOBDRadioCodeToyotaAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Toyota Advanced');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeToyotaAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeToyotaAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Japanese Market - Fujitsu Ten (1995-2005)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_JP_FUJITSU_OLD',
    'Toyota Japan - Fujitsu Ten (1995-2005)',
    rcrAsia,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('08600');
  Variant.RadioModels.Add('08601');
  Variant.RadioModels.Add('08606');
  Variant.AlgorithmNotes := 'Original Fujitsu Ten algorithm using modulo 100000';

  // Japanese Market - Fujitsu Ten (2006-2012)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_JP_FUJITSU_NEW',
    'Toyota Japan - Fujitsu Ten (2006-2012)',
    rcrAsia,
    2006, 2012,
    rcsvV2
  );
  Variant.RadioModels.Add('08606');
  Variant.RadioModels.Add('08607');
  Variant.RadioModels.Add('86120');
  Variant.AlgorithmNotes := 'Updated Fujitsu Ten with enhanced security';

  // Japanese Market - Panasonic (2000-2010)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_JP_PANASONIC',
    'Toyota Japan - Panasonic (2000-2010)',
    rcrAsia,
    2000, 2010,
    rcsvV1
  );
  Variant.RadioModels.Add('CQ-');
  Variant.RadioModels.Add('CY-');
  Variant.AlgorithmNotes := 'Panasonic variant with different calculation method';

  // Japanese Market - Denso (2005+)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_JP_DENSO',
    'Toyota Japan - Denso (2005+)',
    rcrAsia,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('86100');
  Variant.RadioModels.Add('86120');
  Variant.RadioModels.Add('86140');
  Variant.AlgorithmNotes := 'Denso navigation systems with integrated security';

  // North American Market - Standard (2000-2010)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_NA_STANDARD',
    'Toyota North America - Standard (2000-2010)',
    rcrNorthAmerica,
    2000, 2010,
    rcsvV1
  );
  Variant.RadioModels.Add('A56805');
  Variant.RadioModels.Add('A56836');
  Variant.AlgorithmNotes := 'North American variant with region-specific modulo';

  // North American Market - JBL Premium (2005+)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_NA_JBL',
    'Toyota North America - JBL Premium (2005+)',
    rcrNorthAmerica,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('JBL');
  Variant.RadioModels.Add('86120-0C');
  Variant.AlgorithmNotes := 'Premium JBL systems with enhanced algorithm';

  // European Market (1998-2008)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_EU_CLASSIC',
    'Toyota Europe - Classic (1998-2008)',
    rcrEurope,
    1998, 2008,
    rcsvV1
  );
  Variant.RadioModels.Add('86120-0D');
  Variant.RadioModels.Add('86120-0E');
  Variant.AlgorithmNotes := 'European market variant similar to Japanese';

  // European Market (2009+)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_EU_MODERN',
    'Toyota Europe - Modern (2009+)',
    rcrEurope,
    2009, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('86140');
  Variant.RadioModels.Add('Touch & Go');
  Variant.AlgorithmNotes := 'Modern European systems with updated security';

  // Australian Market (2000+)
  Variant := FVariantManager.AddVariant(
    'TOYOTA_AU',
    'Toyota Australia (2000+)',
    rcrAustralia,
    2000, 9999,
    rcsvV1
  );
  Variant.RadioModels.Add('86120-3A');
  Variant.AlgorithmNotes := 'Australian market using Japanese algorithm base';
end;

function TOBDRadioCodeToyotaAdvanced.CalculateV1(const Serial: string): string;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  Serial := StrToInt(Sanitized);

  // Toyota algorithm
  Code[0] := ApplyModularTransform((Serial div 1000) + 3, 10);
  Code[1] := ApplyModularTransform((Serial div 100) + 7, 10);
  Code[2] := ApplyModularTransform((Serial div 10) + 1, 10);
  Code[3] := ApplyModularTransform(Serial + 5, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

function TOBDRadioCodeToyotaAdvanced.CalculateV2(const Serial: string): string;
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

function TOBDRadioCodeToyotaAdvanced.CalculateV3(const Serial: string): string;
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

function TOBDRadioCodeToyotaRegional.CalculateFujitsuTen(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  Digit: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * (I * 3));
    end;
  end;
  
  Code := ApplyModularTransform(Code, 100000);
  Result := Format('%.5d', [Code]);
end;

function TOBDRadioCodeToyotaRegional.CalculatePanasonic(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  Digit: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      // Panasonic uses alternating weights
      if Odd(I) then
        Code := Code + (Digit * 7)
      else
        Code := Code + (Digit * 3);
    end;
  end;
  
  Code := ApplyModularTransform(Code, 100000);
  Result := Format('%.5d', [Code]);
end;

function TOBDRadioCodeToyotaRegional.CalculateDenso(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  Digit: Integer;
  Multiplier: Integer;
begin
  Code := 0;
  Multiplier := 1;
  
  for I := 1 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      Code := Code + (Digit * Multiplier);
      Multiplier := Multiplier + 2;
    end;
  end;
  
  Code := ApplyModularTransform(Code, 100000);
  Result := Format('%.5d', [Code]);
end;

function TOBDRadioCodeToyotaRegional.CalculateJBL(const Serial: string): string;
var
  Code: Integer;
  I: Integer;
  Digit: Integer;
begin
  Code := 0;
  for I := 1 to Length(Serial) do
  begin
    if CharInSet(Serial[I], ['0'..'9']) then
    begin
      Digit := StrToInt(Serial[I]);
      // JBL premium uses squared position weights
      Code := Code + (Digit * (I * I));
    end;
  end;
  
  Code := ApplyModularTransform(Code, 100000);
  Result := Format('%.5d', [Code]);
end;

function TOBDRadioCodeToyotaAdvanced.GetDescription: string;
begin
  Result := 'Advanced Toyota Radio Code Calculator with multiple algorithm variants';
end;

procedure TOBDRadioCodeToyotaAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeToyotaAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

function TOBDRadioCodeToyotaAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeToyotaAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeToyotaAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Toyota serial format: 5 digits
  if not ValidateLength(Sanitized, 5, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeToyotaAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if (FCurrentVariant.VariantID = 'TOYOTA_EU_V1') or
     (FCurrentVariant.VariantID = 'TOYOTA_NA') then
    Output := CalculateV1(Sanitized)
  else if (FCurrentVariant.VariantID = 'TOYOTA_EU_V2') or
          (FCurrentVariant.VariantID = 'TOYOTA_ASIA') then
    Output := CalculateV2(Sanitized)
  else if FCurrentVariant.VariantID = 'TOYOTA_EU_V3' then
    Output := CalculateV3(Sanitized)
  else
  begin
    ErrorMessage := 'Unknown variant: ' + FCurrentVariant.VariantID;
    Exit(False);
  end;
end;

end.
