//------------------------------------------------------------------------------
//  OBD.RadioCode.EuropeanPremium
//
//  Vendor radio-code calculator components for the German /
//  European premium brands:
//
//    TOBDRadioCodeVW            Volkswagen group radios
//                               (Gamma / Beta / Alpha / RCD / RNS)
//    TOBDRadioCodeAudiConcert   Audi Concert / Symphony
//    TOBDRadioCodeBMW           BMW Business CD / Professional /
//                               Navigation / Modern
//    TOBDRadioCodeMercedes      Mercedes-Benz factory radios
//    TOBDRadioCodeMini          MINI (BMW group, 7-digit serial)
//    TOBDRadioCodePorsche       Porsche factory radios (PCM)
//    TOBDRadioCodeSEAT          SEAT (VW group, SEZ / VWZ prefix)
//    TOBDRadioCodeSkoda         Skoda (VW group, SKZ / VWZ prefix)
//    TOBDRadioCodeSmart         Smart (Mercedes, alphanumeric)
//
//  IMPORTANT: production radio-code algorithms for these vendors
//  are proprietary and licensed; this open-source distribution
//  does NOT bundle them. Each component validates the input
//  shape (length / prefix / character set per vendor) and then
//  fires <c>OnCalculate</c> for the host to supply the
//  calculation. Wire your own implementation, a licensed
//  code-service round-trip, or a network call.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Input validation
//                     re-derived from v1's vendor units; algorithm
//                     bodies intentionally not bundled (see note
//                     above).
//------------------------------------------------------------------------------

unit OBD.RadioCode.EuropeanPremium;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  /// <summary>Volkswagen group radios. Serial format:
  /// <c>VWZ + 11 alphanumeric</c> (14 total). Covers Gamma /
  /// Beta / Alpha / RCD / RNS — the host's <c>OnCalculate</c>
  /// handler picks the right algorithm by inspecting the
  /// serial.</summary>
  TOBDRadioCodeVW = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Audi Concert / Symphony. Serial: <c>AUZ + 11
  /// alphanumeric</c> (14 total).</summary>
  TOBDRadioCodeAudiConcert = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>BMW factory radios (Business CD / Professional /
  /// Navigation / Modern). Serial: 7 digits.</summary>
  TOBDRadioCodeBMW = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Mercedes-Benz factory radios. Serial: 14
  /// characters, leading letter (A / B / L / …).</summary>
  TOBDRadioCodeMercedes = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>MINI (BMW group). Serial: 7 digits, BMW Group
  /// standard.</summary>
  TOBDRadioCodeMini = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Porsche factory radios (PCM). Serial: <c>PO + 12
  /// alphanumeric</c> (14 total).</summary>
  TOBDRadioCodePorsche = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>SEAT (VW group). Serial: <c>SEZ</c> or <c>VWZ</c>
  /// + 11 alphanumeric (14 total).</summary>
  TOBDRadioCodeSEAT = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Skoda (VW group). Serial: <c>SKZ</c> or <c>VWZ</c>
  /// + 11 alphanumeric (14 total).</summary>
  TOBDRadioCodeSkoda = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Smart (Mercedes). Serial: 14 alphanumeric
  /// characters.</summary>
  TOBDRadioCodeSmart = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

{ ---- helpers ---------------------------------------------------------------- }

function ValidatePrefix(const AInput, APrefix: string;
  out AReason: string): Boolean;
begin
  Result := Copy(AInput, 1, Length(APrefix)) = APrefix;
  if not Result then
    AReason := Format('Serial must start with %s', [APrefix]);
end;

function ValidateAlphanumericRange(const AInput: string;
  AStart, AEnd: Integer; out AReason: string): Boolean;
var
  I: Integer;
begin
  AReason := '';
  for I := AStart to AEnd do
    if not CharInSet(AInput[I], ['0'..'9', 'A'..'Z']) then
    begin
      AReason := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  Result := True;
end;

function ValidateDigitRange(const AInput: string;
  AStart, AEnd: Integer; out AReason: string): Boolean;
var
  I: Integer;
begin
  AReason := '';
  for I := AStart to AEnd do
    if not CharInSet(AInput[I], ['0'..'9']) then
    begin
      AReason := Format('Character %d must be a digit', [I]);
      Exit(False);
    end;
  Result := True;
end;

{ ---- TOBDRadioCodeVW ------------------------------------------------------- }

function TOBDRadioCodeVW.BrandKey: string; begin Result := 'volkswagen'; end;
function TOBDRadioCodeVW.DisplayName: string; begin Result := 'Volkswagen group'; end;
function TOBDRadioCodeVW.Description: string;
begin
  Result :=
    'Volkswagen factory radios (Gamma / Beta / Alpha / RCD / RNS). ' +
    'Input: 14-character serial starting with VWZ. Algorithm not ' +
    'bundled — wire OnCalculate.';
end;

function TOBDRadioCodeVW.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidatePrefix(AInput, 'VWZ', AReason);
end;

{ ---- TOBDRadioCodeAudiConcert ---------------------------------------------- }

function TOBDRadioCodeAudiConcert.BrandKey: string; begin Result := 'audi-concert'; end;
function TOBDRadioCodeAudiConcert.DisplayName: string; begin Result := 'Audi Concert / Symphony'; end;
function TOBDRadioCodeAudiConcert.Description: string;
begin
  Result :=
    'Audi Concert / Symphony factory radios. Input: 14-character ' +
    'serial starting with AUZ; characters 4-5 must be digits. ' +
    'Algorithm not bundled — wire OnCalculate.';
end;

function TOBDRadioCodeAudiConcert.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidatePrefix(AInput, 'AUZ', AReason)
       and  ValidateDigitRange(AInput, 4, 5, AReason);
end;

{ ---- TOBDRadioCodeBMW ------------------------------------------------------ }

function TOBDRadioCodeBMW.BrandKey: string; begin Result := 'bmw'; end;
function TOBDRadioCodeBMW.DisplayName: string; begin Result := 'BMW'; end;
function TOBDRadioCodeBMW.Description: string;
begin
  Result :=
    'BMW factory radios (Business CD / Professional / Navigation / ' +
    'Modern). Input: 7-digit serial. Algorithm not bundled — wire ' +
    'OnCalculate.';
end;

function TOBDRadioCodeBMW.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 7, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

{ ---- TOBDRadioCodeMercedes ------------------------------------------------- }

function TOBDRadioCodeMercedes.BrandKey: string; begin Result := 'mercedes'; end;
function TOBDRadioCodeMercedes.DisplayName: string; begin Result := 'Mercedes-Benz'; end;
function TOBDRadioCodeMercedes.Description: string;
begin
  Result :=
    'Mercedes-Benz factory radios. Input: 14 alphanumeric characters ' +
    'starting with a letter (A / B / L / ...). Algorithm not bundled ' +
    '— wire OnCalculate.';
end;

function TOBDRadioCodeMercedes.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason);
  if not Result then Exit;
  if not CharInSet(AInput[1], ['A'..'Z']) then
  begin
    AReason := 'First character must be a letter';
    Exit(False);
  end;
  Result := ValidateAlphanumericRange(AInput, 2, 14, AReason);
end;

{ ---- TOBDRadioCodeMini ----------------------------------------------------- }

function TOBDRadioCodeMini.BrandKey: string; begin Result := 'mini'; end;
function TOBDRadioCodeMini.DisplayName: string; begin Result := 'MINI (BMW group)'; end;
function TOBDRadioCodeMini.Description: string;
begin
  Result :=
    'MINI factory radios. Input: 7-digit serial (BMW Group standard). ' +
    'Algorithm not bundled — wire OnCalculate.';
end;

function TOBDRadioCodeMini.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 7, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

{ ---- TOBDRadioCodePorsche -------------------------------------------------- }

function TOBDRadioCodePorsche.BrandKey: string; begin Result := 'porsche'; end;
function TOBDRadioCodePorsche.DisplayName: string; begin Result := 'Porsche (PCM)'; end;
function TOBDRadioCodePorsche.Description: string;
begin
  Result :=
    'Porsche factory radios (PCM). Input: 14-character serial starting ' +
    'with PO; remaining 12 alphanumeric. Algorithm not bundled — wire ' +
    'OnCalculate.';
end;

function TOBDRadioCodePorsche.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidatePrefix(AInput, 'PO', AReason)
       and  ValidateAlphanumericRange(AInput, 3, 14, AReason);
end;

{ ---- TOBDRadioCodeSEAT ----------------------------------------------------- }

function TOBDRadioCodeSEAT.BrandKey: string; begin Result := 'seat'; end;
function TOBDRadioCodeSEAT.DisplayName: string; begin Result := 'SEAT (VW group)'; end;
function TOBDRadioCodeSEAT.Description: string;
begin
  Result :=
    'SEAT factory radios. Input: 14-character serial starting with SEZ ' +
    'or VWZ; characters 4-5 must be digits. Algorithm not bundled — ' +
    'wire OnCalculate.';
end;

function TOBDRadioCodeSEAT.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason);
  if not Result then Exit;
  if (Copy(AInput, 1, 3) <> 'SEZ') and (Copy(AInput, 1, 3) <> 'VWZ') then
  begin
    AReason := 'Serial must start with SEZ or VWZ';
    Exit(False);
  end;
  Result := ValidateDigitRange(AInput, 4, 5, AReason);
end;

{ ---- TOBDRadioCodeSkoda ---------------------------------------------------- }

function TOBDRadioCodeSkoda.BrandKey: string; begin Result := 'skoda'; end;
function TOBDRadioCodeSkoda.DisplayName: string; begin Result := 'Skoda (VW group)'; end;
function TOBDRadioCodeSkoda.Description: string;
begin
  Result :=
    'Skoda factory radios. Input: 14-character serial starting with SKZ ' +
    'or VWZ; characters 4-5 must be digits. Algorithm not bundled — ' +
    'wire OnCalculate.';
end;

function TOBDRadioCodeSkoda.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason);
  if not Result then Exit;
  if (Copy(AInput, 1, 3) <> 'SKZ') and (Copy(AInput, 1, 3) <> 'VWZ') then
  begin
    AReason := 'Serial must start with SKZ or VWZ';
    Exit(False);
  end;
  Result := ValidateDigitRange(AInput, 4, 5, AReason);
end;

{ ---- TOBDRadioCodeSmart ---------------------------------------------------- }

function TOBDRadioCodeSmart.BrandKey: string; begin Result := 'smart'; end;
function TOBDRadioCodeSmart.DisplayName: string; begin Result := 'Smart (Mercedes)'; end;
function TOBDRadioCodeSmart.Description: string;
begin
  Result :=
    'Smart factory radios (Mercedes platform). Input: 14 alphanumeric ' +
    'characters. Algorithm not bundled — wire OnCalculate.';
end;

function TOBDRadioCodeSmart.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidateAlphanumericRange(AInput, 1, 14, AReason);
end;

{ ---- registration ---------------------------------------------------------- }

initialization
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeVW);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeAudiConcert);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeBMW);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeMercedes);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeMini);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodePorsche);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeSEAT);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeSkoda);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeSmart);

end.
