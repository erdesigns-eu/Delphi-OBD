//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Fiat.VP.Advanced.pas
// CONTENTS       : Fiat VP Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Fiat.VP.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Fiat VP Advanced Radio Code Calculator with Multiple Algorithm Variants
  /// </summary>
  TOBDRadioCodeFiatVPAdvanced = class(TOBDRadioCode)
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

constructor TOBDRadioCodeFiatVPAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Fiat VP Advanced');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeFiatVPAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeFiatVPAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Europe V1 (1995-2005)
  Variant := FVariantManager.AddVariant(
    'FIAT_VP_EU_V1',
    'Fiat VP Europe V1 (1995-2005)',
    rcrEurope,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('VP models');
  Variant.AlgorithmNotes := 'Basic algorithm for European market';
  
  // Europe V2 (2006-2015)
  Variant := FVariantManager.AddVariant(
    'FIAT_VP_EU_V2',
    'Fiat VP Europe V2 (2006-2015)',
    rcrEurope,
    2006, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('Model 2');
  Variant.AlgorithmNotes := 'Enhanced security algorithm';
  
  // Europe V3 (2016+)
  Variant := FVariantManager.AddVariant(
    'FIAT_VP_EU_V3',
    'Fiat VP Europe V3 (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('Model 3');
  Variant.AlgorithmNotes := 'Latest security version';
  
  // North America
  Variant := FVariantManager.AddVariant(
    'FIAT_VP_NA',
    'Fiat VP North America (2000+)',
    rcrNorthAmerica,
    2000, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('VP models');
  Variant.AlgorithmNotes := 'North American market variant';
  
  // Asia
  Variant := FVariantManager.AddVariant(
    'FIAT_VP_ASIA',
    'Fiat VP Asia (2005+)',
    rcrAsia,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Model 2');
  Variant.AlgorithmNotes := 'Asian market variant';
end;

function TOBDRadioCodeFiatVPAdvanced.CalculateV1(const Serial: string): string;
function GetFourthByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7: Result := 3;
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

  function GetThirdByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7: Result := 2;
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

  function GetSecondByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7: Result := 1;
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

  function GetFirstByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7,
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

var
  Sanitized: string;
  InputCode, OutputCode: Integer;
  SNArr: array[0..3] of Integer;
begin
  // Initialize result
  Result := True;
  // Clear the output
  Output := '';
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input
  Sanitized := SanitizeInput(Input);

  // Check if the input is valid
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  // Set the input code
  InputCode := StrToInt(Sanitized);

  // Calculate the code
  OutputCode := 1111;

  SNArr[0] := (InputCode div 1000) and $0F;
  SNArr[1] := (InputCode mod 1000) div 100 and $0F;
  SNArr[2] := (InputCode mod 100) div 10 and $0F;
  SNArr[3] := InputCode mod 10 and $0F;

  OutputCode := OutputCode + GetThirdByte(SNArr[3]) * 10;
  OutputCode := OutputCode + GetFirstByte(SNArr[2]) * 1000;
  OutputCode := OutputCode + GetFourthByte(SNArr[1]);
  OutputCode := OutputCode + GetSecondByte(SNArr[0]) * 100;

  // Format the code for the output
  Output := Format('%.*d', [4, OutputCode]);
end;

function TOBDRadioCodeFiatVPAdvanced.CalculateV2(const Serial: string): string;
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

function TOBDRadioCodeFiatVPAdvanced.CalculateV3(const Serial: string): string;
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

function TOBDRadioCodeFiatVPAdvanced.GetDescription: string;
begin
  Result := 'Advanced Fiat VP Radio Code Calculator with multiple algorithm variants';
end;

procedure TOBDRadioCodeFiatVPAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeFiatVPAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

function TOBDRadioCodeFiatVPAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeFiatVPAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeFiatVPAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input (remove whitespace, convert to uppercase)
  Sanitized := SanitizeInput(Input);

  // Validate length using helper method
  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);

  // Validate that all characters are digits using helper method
  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeFiatVPAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if (FCurrentVariant.VariantID = 'FIAT_VP_EU_V1') or
     (FCurrentVariant.VariantID = 'FIAT_VP_NA') then
    Output := CalculateV1(Sanitized)
  else if (FCurrentVariant.VariantID = 'FIAT_VP_EU_V2') or
          (FCurrentVariant.VariantID = 'FIAT_VP_ASIA') then
    Output := CalculateV2(Sanitized)
  else if FCurrentVariant.VariantID = 'FIAT_VP_EU_V3' then
    Output := CalculateV3(Sanitized)
  else
  begin
    ErrorMessage := 'Unknown variant: ' + FCurrentVariant.VariantID;
    Exit(False);
  end;
end;

end.
