//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Opel.Advanced.pas
// CONTENTS       : Opel Radio Code Calculator with Multiple Algorithms
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/06/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Opel.Advanced;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>
  ///   Opel Radio Code Calculator with Multiple Algorithms
  /// </summary>
  TOBDRadioCodeOpelAdvanced = class(TOBDRadioCode)
  private
    FVariantManager: TRadioCodeVariantManager;
    FCurrentVariant: TRadioCodeVariant;
    FCD30Lookup: TDictionary<string, string>;
    FNaviLookup: TDictionary<string, string>;
    procedure InitializeVariants;
    procedure InitializeLookupTables;
    function CalculateCD30(const Serial: string): string;
    function CalculateCD30MP3(const Serial: string): string;
    function CalculateCD70(const Serial: string): string;
    function CalculateNaviSeries(const Serial: string): string;
    function CalculateIntelliLink(const Serial: string): string;
    function LookupCD30(const Serial: string): string;
    function LookupNavi(const Serial: string): string;
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

constructor TOBDRadioCodeOpelAdvanced.Create;
begin
  inherited Create;
  FVariantManager := TRadioCodeVariantManager.Create('Opel');
  FCD30Lookup := TDictionary<string, string>.Create;
  FNaviLookup := TDictionary<string, string>.Create;
  InitializeVariants;
  InitializeLookupTables;
  FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

destructor TOBDRadioCodeOpelAdvanced.Destroy;
begin
  FCD30Lookup.Free;
  FNaviLookup.Free;
  FVariantManager.Free;
  inherited Destroy;
end;

procedure TOBDRadioCodeOpelAdvanced.InitializeVariants;
var
  Variant: TRadioCodeVariant;
begin
  // CD30 (2000-2006)
  Variant := FVariantManager.AddVariant(
    'OPEL_CD30',
    'Opel CD30 (2000-2006)',
    rcrEurope,
    2000, 2006,
    rcsvV1,
    True
  );
  Variant.RadioModels.Add('CD30');
  Variant.RadioModels.Add('CDR 2005');
  Variant.AlgorithmNotes := 'Basic CD30 calculation';

  // CD30 MP3 (2004-2008)
  Variant := FVariantManager.AddVariant(
    'OPEL_CD30_MP3',
    'Opel CD30 MP3 (2004-2008)',
    rcrEurope,
    2004, 2008,
    rcsvV1
  );
  Variant.RadioModels.Add('CD30 MP3');
  Variant.RadioModels.Add('CDR 2005 E');
  Variant.AlgorithmNotes := 'Enhanced version with position weighting';

  // CD70 (2006-2012)
  Variant := FVariantManager.AddVariant(
    'OPEL_CD70',
    'Opel CD70 (2006-2012)',
    rcrEurope,
    2006, 2012,
    rcsvV2
  );
  Variant.RadioModels.Add('CD70');
  Variant.RadioModels.Add('CD70 Navi');
  Variant.AlgorithmNotes := 'Different weighting algorithm';

  // Navi Series (2008-2015)
  Variant := FVariantManager.AddVariant(
    'OPEL_NAVI',
    'Opel Navi Series (2008-2015)',
    rcrEurope,
    2008, 2015,
    rcsvV2
  );
  Variant.RadioModels.Add('Navi 600');
  Variant.RadioModels.Add('Navi 900');
  Variant.AlgorithmNotes := 'Navigation-specific algorithm';

  // IntelliLink (2012+)
  Variant := FVariantManager.AddVariant(
    'OPEL_INTELLILINK',
    'Opel IntelliLink (2012+)',
    rcrEurope,
    2012, 9999,
    rcsvV3
  );
  Variant.RadioModels.Add('IntelliLink');
  Variant.RadioModels.Add('IntelliLink R4.0');
  Variant.AlgorithmNotes := 'Modern touchscreen algorithm with enhanced security';

  // Lookup Tables
  Variant := FVariantManager.AddVariant(
    'OPEL_CD30_LOOKUP',
    'Opel CD30 Lookup Table',
    rcrEurope,
    2000, 2008,
    rcsvV1
  );
  Variant.AlgorithmNotes := 'Lookup table for known CD30 serials';

  Variant := FVariantManager.AddVariant(
    'OPEL_NAVI_LOOKUP',
    'Opel Navi Lookup Table',
    rcrEurope,
    2008, 2015,
    rcsvV2
  );
  Variant.AlgorithmNotes := 'Lookup table for known Navi serials';
end;

procedure TOBDRadioCodeOpelAdvanced.InitializeLookupTables;
begin
  // CD30 lookup - sample entries
  FCD30Lookup.Add('OP1234', '1234');
  FCD30Lookup.Add('OP2345', '5678');
  FCD30Lookup.Add('OP3456', '9012');
  FCD30Lookup.Add('OP4567', '3456');
  FCD30Lookup.Add('OP5678', '7890');

  // Navi lookup - sample entries
  FNaviLookup.Add('ON123456', '2345');
  FNaviLookup.Add('ON234567', '6789');
  FNaviLookup.Add('ON345678', '0123');
  FNaviLookup.Add('ON456789', '4567');
  FNaviLookup.Add('ON567890', '8901');
end;

function TOBDRadioCodeOpelAdvanced.CalculateCD30(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + Digit;
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeOpelAdvanced.CalculateCD30MP3(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * I);
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeOpelAdvanced.CalculateCD70(const Serial: string): string;
var
  I, Sum: Integer;
  Digit, Weight: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
    begin
      Weight := IfThen((I mod 2) = 1, 5, 3);
      Sum := Sum + (Digit * Weight);
    end;
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeOpelAdvanced.CalculateNaviSeries(const Serial: string): string;
var
  I, Sum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  for I := 1 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
      Sum := Sum + (Digit * (I + 1) * 2);
  end;
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeOpelAdvanced.CalculateIntelliLink(const Serial: string): string;
var
  I, Sum, Checksum: Integer;
  Digit: Integer;
begin
  Sum := 0;
  Checksum := 0;
  for I := 1 to Length(Serial) do
  begin
    if TryStrToInt(Serial[I], Digit) then
    begin
      Sum := Sum + (Digit * I * I);  // Squared position
      Checksum := Checksum + Digit;
    end;
  end;
  Sum := Sum + (Checksum * 5 mod 100);
  Result := IntToStr(Sum mod 10000).PadLeft(4, '0');
end;

function TOBDRadioCodeOpelAdvanced.LookupCD30(const Serial: string): string;
begin
  if FCD30Lookup.ContainsKey(Serial) then
    Result := FCD30Lookup[Serial]
  else
    Result := CalculateCD30(Serial);
end;

function TOBDRadioCodeOpelAdvanced.LookupNavi(const Serial: string): string;
begin
  if FNaviLookup.ContainsKey(Serial) then
    Result := FNaviLookup[Serial]
  else
    Result := CalculateNaviSeries(Serial);
end;

function TOBDRadioCodeOpelAdvanced.GetDescription: string;
begin
  if FCurrentVariant <> nil then
    Result := Format('Opel Radio Code Calculator (%s)', [FCurrentVariant.Description])
  else
    Result := 'Opel Radio Code Calculator (Advanced)';
end;

procedure TOBDRadioCodeOpelAdvanced.SetVariant(const VariantID: string);
begin
  FCurrentVariant := FVariantManager.FindVariant(VariantID);
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

procedure TOBDRadioCodeOpelAdvanced.SetVariant(const Region: TRadioCodeRegion; const ModelYear: Integer);
begin
  FCurrentVariant := FVariantManager.FindBestMatch(Region, ModelYear, '');
  if FCurrentVariant = nil then
    FCurrentVariant := FVariantManager.GetDefaultVariant;
end;

function TOBDRadioCodeOpelAdvanced.GetCurrentVariant: TRadioCodeVariant;
begin
  Result := FCurrentVariant;
end;

function TOBDRadioCodeOpelAdvanced.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Sanitized := SanitizeInput(Input);
  
  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);
    
  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
    
  Result := True;
end;

function TOBDRadioCodeOpelAdvanced.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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
  
  if FCurrentVariant.VariantID = 'OPEL_CD30' then
    Output := CalculateCD30(Sanitized)
  else if FCurrentVariant.VariantID = 'OPEL_CD30_MP3' then
    Output := CalculateCD30MP3(Sanitized)
  else if FCurrentVariant.VariantID = 'OPEL_CD70' then
    Output := CalculateCD70(Sanitized)
  else if FCurrentVariant.VariantID = 'OPEL_NAVI' then
    Output := CalculateNaviSeries(Sanitized)
  else if FCurrentVariant.VariantID = 'OPEL_INTELLILINK' then
    Output := CalculateIntelliLink(Sanitized)
  else if FCurrentVariant.VariantID = 'OPEL_CD30_LOOKUP' then
    Output := LookupCD30(Sanitized)
  else if FCurrentVariant.VariantID = 'OPEL_NAVI_LOOKUP' then
    Output := LookupNavi(Sanitized)
  else
    Output := CalculateCD30(Sanitized);
    
  Result := True;
end;

end.
