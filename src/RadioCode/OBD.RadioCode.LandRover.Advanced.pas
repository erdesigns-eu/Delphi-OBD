//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.LandRover.Advanced.pas
// CONTENTS       : Land Rover Radio Code Calculator - Advanced Multi-Variant
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.LandRover.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Land Rover Advanced Radio Code Calculator with Multiple Algorithm Variants
  /// </summary>
  TOBDRadioCodeLandRoverAdvanced = class(TOBDRadioCode)
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

constructor TOBDRadioCodeLandRoverAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Land Rover Advanced');
  InitializeVariants;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeLandRoverAdvanced.Destroy;
begin
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeLandRoverAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // Europe V1 (1995-2005)
  Variant := FVariantManager.AddVariant(
    'LANDROVER_EU_V1',
    'Land Rover Europe V1 (1995-2005)',
    rcrEurope,
    1995, 2005,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('Premium');
  Variant.AlgorithmNotes := 'Basic algorithm for European market';
  
  // Europe V2 (2006-2015)
  Variant := FVariantManager.AddVariant(
    'LANDROVER_EU_V2',
    'Land Rover Europe V2 (2006-2015)',
    rcrEurope,
    2006, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('Meridian');
  Variant.AlgorithmNotes := 'Enhanced security algorithm';
  
  // Europe V3 (2016+)
  Variant := FVariantManager.AddVariant(
    'LANDROVER_EU_V3',
    'Land Rover Europe V3 (2016+)',
    rcrEurope,
    2016, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('Model 3');
  Variant.AlgorithmNotes := 'Latest security version';
  
  // North America
  Variant := FVariantManager.AddVariant(
    'LANDROVER_NA',
    'Land Rover North America (2000+)',
    rcrNorthAmerica,
    2000, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Premium');
  Variant.AlgorithmNotes := 'North American market variant';
  
  // Asia
  Variant := FVariantManager.AddVariant(
    'LANDROVER_ASIA',
    'Land Rover Asia (2005+)',
    rcrAsia,
    2005, 9999,
    rcsvV2
  );
  Variant.RadioModels.Add('Meridian');
  Variant.AlgorithmNotes := 'Asian market variant';
end;

function TOBDRadioCodeLandRoverAdvanced.CalculateV1(const Serial: string): string;
var
  Sanitized: string;
  SerialPart: string;
  Sum: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  // Extract alphanumeric part (skip LR/JG prefix)
  SerialPart := Copy(Sanitized, 3, 12);

  // Calculate checksum from alphanumeric characters
  Sum := 0;
  for var I := 1 to Length(SerialPart) do
  begin
    if CharInSet(SerialPart[I], ['0'..'9']) then
      Sum := Sum + (Ord(SerialPart[I]) - Ord('0'))
    else
      Sum := Sum + (Ord(SerialPart[I]) - Ord('A') + 10);
  end;

  // Land Rover algorithm (premium encoding)
  Code[0] := ApplyModularTransform(Sum div 100 + 7, 10);
  Code[1] := ApplyModularTransform(Sum div 10 + 5, 10);
  Code[2] := ApplyModularTransform(Sum + 9, 10);
  Code[3] := ApplyModularTransform(Sum * 3 + 11, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

function TOBDRadioCodeLandRoverAdvanced.CalculateV2(const Serial: string): string;
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

function TOBDRadioCodeLandRoverAdvanced.CalculateV3(const Serial: string): string;
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

function TOBDRadioCodeLandRoverAdvanced.GetDescription: string;
begin
  Result := 'Advanced Land Rover Radio Code Calculator with multiple algorithm variants';
end;

procedure TOBDRadioCodeLandRoverAdvanced.SetVariant(const VariantID: string);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindVariant(VariantID);
  if Variant <> nil then
    FCurrentVariant := Variant
  else
    raise Exception.CreateFmt('Variant "%s" not found', [VariantID]);
end;

procedure TOBDRadioCodeLandRoverAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
var
  Variant: TRadioCodeVariant;
begin
  Variant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if Variant <> nil then
    FCurrentVariant := Variant;
end;

function TOBDRadioCodeLandRoverAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeLandRoverAdvanced.GetAvailableVariants: TRadioCodeVariantManager;
begin
  Result := FVariantManager;
end;

function TOBDRadioCodeLandRoverAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Land Rover serial: 14 characters (LR + 12 alphanumeric)
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // Should start with 'LR' or 'JG' (Jaguar)
  if not ((Copy(Sanitized, 1, 2) = 'LR') or (Copy(Sanitized, 1, 2) = 'JG')) then
  begin
    ErrorMessage := 'Serial must start with LR (Land Rover) or JG (Jaguar)';
    Exit(False);
  end;

  // Remaining characters are alphanumeric
  for var I := 3 to 14 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9', 'A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  end;
end;

function TOBDRadioCodeLandRoverAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if (FCurrentVariant.VariantID = 'LANDROVER_EU_V1') or
     (FCurrentVariant.VariantID = 'LANDROVER_NA') then
    Output := CalculateV1(Sanitized)
  else if (FCurrentVariant.VariantID = 'LANDROVER_EU_V2') or
          (FCurrentVariant.VariantID = 'LANDROVER_ASIA') then
    Output := CalculateV2(Sanitized)
  else if FCurrentVariant.VariantID = 'LANDROVER_EU_V3' then
    Output := CalculateV3(Sanitized)
  else
  begin
    ErrorMessage := 'Unknown variant: ' + FCurrentVariant.VariantID;
    Exit(False);
  end;
end;

end.
