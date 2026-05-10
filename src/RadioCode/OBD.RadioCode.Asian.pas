//------------------------------------------------------------------------------
//  OBD.RadioCode.Asian
//
//  Vendor radio-code calculator components for Asian brands:
//
//    TOBDRadioCodeAcura       Honda group (1 letter + 7 digits)
//    TOBDRadioCodeHonda       Honda factory radios (1 letter + 7 digits)
//    TOBDRadioCodeHyundai     Hyundai (4 digits) — REAL algorithm
//    TOBDRadioCodeInfiniti    Nissan group (4 digits)
//    TOBDRadioCodeLexus       Toyota group (5 digits)
//    TOBDRadioCodeMazda       Mazda (6 digits)
//    TOBDRadioCodeMitsubishi  Mitsubishi (4 digits)
//    TOBDRadioCodeNissan      Nissan (4 digits)
//    TOBDRadioCodeSubaru      Subaru (5 digits)
//    TOBDRadioCodeSuzuki      Suzuki (4 digits)
//    TOBDRadioCodeToyota      Toyota (5 digits)
//
//  Hyundai ships a real algorithm; the rest are OnCalculate stubs
//  (algorithms proprietary or database-backed). See
//  docs/radio-code-algorithms.md.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Hyundai algorithm
//                     ((last4 + 1212) mod 10000) sourced from the
//                     MHH Auto community thread; verified across
//                     2002-2014 OEM units.
//------------------------------------------------------------------------------

unit OBD.RadioCode.Asian;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  TOBDRadioCodeAcura = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeHonda = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Hyundai factory radios (2002+). Input: 4-digit
  /// pre-code displayed when the radio shows <c>CODE</c>. The
  /// algorithm is a fixed offset; the 2009-only variant uses
  /// 1222 instead of 1212 — set <c>ModelHint = '2009-variant'</c>
  /// to use the variant.
  /// Algorithm <b>bundled</b> (community-documented).</summary>
  TOBDRadioCodeHyundai = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeInfiniti = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeLexus = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeMazda = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeMitsubishi = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeNissan = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeSubaru = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeSuzuki = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeToyota = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

{ ---- Acura ---------------------------------------------------------------- }
function TOBDRadioCodeAcura.BrandKey: string; begin Result := 'acura'; end;
function TOBDRadioCodeAcura.DisplayName: string; begin Result := 'Acura'; end;
function TOBDRadioCodeAcura.Description: string;
begin
  Result :=
    'Acura factory radios (Honda group). Input: 1 letter + 7 digits ' +
    'from the radio''s back label. Algorithm not bundled — Honda ' +
    'codes are typically database-backed; wire OnCalculate.';
end;
function TOBDRadioCodeAcura.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 8, AReason);
  if not Result then Exit;
  if not CharInSet(AInput[1], ['A'..'Z']) then
  begin
    AReason := 'First character must be a letter';
    Exit(False);
  end;
  Result := ValidateDigitRange(AInput, 2, 8, AReason);
end;

{ ---- Honda ---------------------------------------------------------------- }
function TOBDRadioCodeHonda.BrandKey: string; begin Result := 'honda'; end;
function TOBDRadioCodeHonda.DisplayName: string; begin Result := 'Honda'; end;
function TOBDRadioCodeHonda.Description: string;
begin
  Result :=
    'Honda factory radios. Input: 1 letter + 7 digits from the ' +
    'radio''s back label. Algorithm not bundled — Honda codes are ' +
    'typically database-backed; wire OnCalculate.';
end;
function TOBDRadioCodeHonda.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 8, AReason);
  if not Result then Exit;
  if not CharInSet(AInput[1], ['A'..'Z']) then
  begin
    AReason := 'First character must be a letter';
    Exit(False);
  end;
  Result := ValidateDigitRange(AInput, 2, 8, AReason);
end;

{ ---- Hyundai (REAL) ------------------------------------------------------- }
function TOBDRadioCodeHyundai.BrandKey: string; begin Result := 'hyundai'; end;
function TOBDRadioCodeHyundai.DisplayName: string; begin Result := 'Hyundai (2002+)'; end;
function TOBDRadioCodeHyundai.Description: string;
begin
  Result :=
    'Hyundai factory radios (2002+). Input: 4-digit pre-code shown ' +
    'on the unit when locked. Algorithm: (pre-code + 1212) mod 10000 ' +
    'on most years; (pre-code + 1222) mod 10000 on the 2009-only ' +
    'variant — set ModelHint = ''2009-variant'' for that.';
end;
function TOBDRadioCodeHyundai.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;
function TOBDRadioCodeHyundai.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  Pre, Off: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Pre := StrToInt(AInput);
  if SameText(AContext.ModelHint, '2009-variant') then
  begin
    Off := 1222;
    Result.Variant := '2009 variant (+1222)';
  end
  else
  begin
    Off := 1212;
    Result.Variant := 'standard (+1212)';
  end;
  Result.Code    := Format('%.4d', [(Pre + Off) mod 10000]);
  Result.Success := True;
end;

{ ---- Infiniti ------------------------------------------------------------- }
function TOBDRadioCodeInfiniti.BrandKey: string; begin Result := 'infiniti'; end;
function TOBDRadioCodeInfiniti.DisplayName: string; begin Result := 'Infiniti'; end;
function TOBDRadioCodeInfiniti.Description: string;
begin
  Result :=
    'Infiniti factory radios (Nissan group). Input: 4 digits. ' +
    'Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeInfiniti.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason) and ValidateAllDigits(AInput, AReason);
end;

{ ---- Lexus ---------------------------------------------------------------- }
function TOBDRadioCodeLexus.BrandKey: string; begin Result := 'lexus'; end;
function TOBDRadioCodeLexus.DisplayName: string; begin Result := 'Lexus'; end;
function TOBDRadioCodeLexus.Description: string;
begin
  Result :=
    'Lexus factory radios (Toyota group; ERC-style). Input: 5 digits. ' +
    'Algorithm not bundled — Toyota ERC is proprietary; wire OnCalculate.';
end;
function TOBDRadioCodeLexus.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 5, AReason) and ValidateAllDigits(AInput, AReason);
end;

{ ---- Mazda ---------------------------------------------------------------- }
function TOBDRadioCodeMazda.BrandKey: string; begin Result := 'mazda'; end;
function TOBDRadioCodeMazda.DisplayName: string; begin Result := 'Mazda'; end;
function TOBDRadioCodeMazda.Description: string;
begin
  Result :=
    'Mazda factory radios. Input: 6 digits. Algorithm not bundled — ' +
    'wire OnCalculate.';
end;
function TOBDRadioCodeMazda.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 6, AReason) and ValidateAllDigits(AInput, AReason);
end;

{ ---- Mitsubishi ----------------------------------------------------------- }
function TOBDRadioCodeMitsubishi.BrandKey: string; begin Result := 'mitsubishi'; end;
function TOBDRadioCodeMitsubishi.DisplayName: string; begin Result := 'Mitsubishi'; end;
function TOBDRadioCodeMitsubishi.Description: string;
begin
  Result :=
    'Mitsubishi factory radios. Input: 4 digits. Algorithm not ' +
    'bundled — wire OnCalculate.';
end;
function TOBDRadioCodeMitsubishi.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason) and ValidateAllDigits(AInput, AReason);
end;

{ ---- Nissan --------------------------------------------------------------- }
function TOBDRadioCodeNissan.BrandKey: string; begin Result := 'nissan'; end;
function TOBDRadioCodeNissan.DisplayName: string; begin Result := 'Nissan'; end;
function TOBDRadioCodeNissan.Description: string;
begin
  Result :=
    'Nissan factory radios. Input: 4 digits. Algorithm not bundled — ' +
    'wire OnCalculate.';
end;
function TOBDRadioCodeNissan.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason) and ValidateAllDigits(AInput, AReason);
end;

{ ---- Subaru --------------------------------------------------------------- }
function TOBDRadioCodeSubaru.BrandKey: string; begin Result := 'subaru'; end;
function TOBDRadioCodeSubaru.DisplayName: string; begin Result := 'Subaru'; end;
function TOBDRadioCodeSubaru.Description: string;
begin
  Result :=
    'Subaru factory radios (often Clarion-built). Input: 5 digits. ' +
    'Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeSubaru.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 5, AReason) and ValidateAllDigits(AInput, AReason);
end;

{ ---- Suzuki --------------------------------------------------------------- }
function TOBDRadioCodeSuzuki.BrandKey: string; begin Result := 'suzuki'; end;
function TOBDRadioCodeSuzuki.DisplayName: string; begin Result := 'Suzuki'; end;
function TOBDRadioCodeSuzuki.Description: string;
begin
  Result :=
    'Suzuki factory radios (PACR series; Blaupunkt-built). Input: ' +
    '4 digits. Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeSuzuki.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason) and ValidateAllDigits(AInput, AReason);
end;

{ ---- Toyota --------------------------------------------------------------- }
function TOBDRadioCodeToyota.BrandKey: string; begin Result := 'toyota'; end;
function TOBDRadioCodeToyota.DisplayName: string; begin Result := 'Toyota'; end;
function TOBDRadioCodeToyota.Description: string;
begin
  Result :=
    'Toyota factory radios (ERC system). Input: 5 digits. Algorithm ' +
    'not bundled — Toyota ERC is proprietary; wire OnCalculate.';
end;
function TOBDRadioCodeToyota.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 5, AReason) and ValidateAllDigits(AInput, AReason);
end;

initialization
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeAcura);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeHonda);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeHyundai);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeInfiniti);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeLexus);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeMazda);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeMitsubishi);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeNissan);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeSubaru);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeSuzuki);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeToyota);

end.
