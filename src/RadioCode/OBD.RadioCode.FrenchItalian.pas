//------------------------------------------------------------------------------
//  OBD.RadioCode.FrenchItalian
//
//  Vendor radio-code calculator components for French + Italian
//  brands (P-A1.3):
//
//    TOBDRadioCodeCitroen        Citroen radios (4 digits)
//    TOBDRadioCodePeugeot        Peugeot radios (4 digits)
//    TOBDRadioCodeRenault        Renault pre-2004 radios
//                                (1 letter + 3 digits, e.g. C123)
//    TOBDRadioCodeFiatDaiichi    Fiat / Lancia Daiichi-Pioneer radios
//                                (4 digits)
//    TOBDRadioCodeFiatVP         Fiat / Lancia VP-series radios
//                                (4 digits)
//    TOBDRadioCodeAlfaRomeo      Alfa Romeo radios (1 letter + 7 digits)
//    TOBDRadioCodeMaserati       Maserati radios (14 alphanum)
//
//  Algorithms NOT bundled. Each component validates the input
//  shape; the host wires <c>OnCalculate</c> to supply the
//  actual calculation. See docs/radio-code-algorithms.md.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Validation
//                     re-derived from the v1 vendor units.
//------------------------------------------------------------------------------

unit OBD.RadioCode.FrenchItalian;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  TOBDRadioCodeCitroen = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Peugeot factory radios. Input: 4-digit suffix of the
  /// barcode that starts with C7 (NOT the radio's serial number).
  /// Algorithm <b>bundled</b> — derived from the working
  /// open-source ERDesigns Peugeot calculator
  /// (https://github.com/erdesigns-eu/Peugeot-Calculator).</summary>
  TOBDRadioCodePeugeot = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Renault pre-2004 (Tuner List / Update List). Serial:
  /// one letter + three digits (e.g. <c>C123</c>). The pair
  /// <c>A0</c> is reserved (would divide-by-zero internally).
  /// Algorithm <b>bundled</b> — derived from the working
  /// open-source ERDesigns Renault calculator
  /// (https://github.com/erdesigns-eu/Renault-Calculator).</summary>
  TOBDRadioCodeRenault = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Fiat / Lancia Daiichi-Pioneer factory radios. Input:
  /// last 4 digits of the serial number. Algorithm <b>bundled</b>
  /// — derived from the working open-source ERDesigns
  /// Fiat-Daiichi calculator
  /// (https://github.com/erdesigns-eu/Fiat-Daiichi-Calculator).</summary>
  TOBDRadioCodeFiatDaiichi = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Fiat / Lancia Continental VP1 / VP2 factory radios.
  /// Input: last 4 digits of the serial number. Algorithm
  /// <b>bundled</b> — derived from the working open-source
  /// ERDesigns Fiat-VP1-VP2 calculator
  /// (https://github.com/erdesigns-eu/Fiat-VP1-VP2-Calculator).
  /// VP1 and VP2 use the same calculation.</summary>
  TOBDRadioCodeFiatVP = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeAlfaRomeo = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeMaserati = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

{ ---- Citroen --------------------------------------------------------------- }
function TOBDRadioCodeCitroen.BrandKey: string; begin Result := 'citroen'; end;
function TOBDRadioCodeCitroen.DisplayName: string; begin Result := 'Citroen'; end;
function TOBDRadioCodeCitroen.Description: string;
begin
  Result :=
    'Citroen factory radios. Input: 4-digit pre-code printed on the ' +
    'unit. Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeCitroen.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

{ ---- Peugeot --------------------------------------------------------------- }
function TOBDRadioCodePeugeot.BrandKey: string; begin Result := 'peugeot'; end;
function TOBDRadioCodePeugeot.DisplayName: string; begin Result := 'Peugeot'; end;
function TOBDRadioCodePeugeot.Description: string;
begin
  Result :=
    'Peugeot factory radios. Input: the 4 digits at the end of the ' +
    'barcode that starts with C7 (NOT the radio''s serial number). ' +
    'Algorithm bundled (ERDesigns Peugeot-Calculator).';
end;
function TOBDRadioCodePeugeot.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;
function TOBDRadioCodePeugeot.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  D: array[0..3] of Integer;
  Out_: array[0..3] of Integer;
  I: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'C7-barcode last-4 digits';
  for I := 0 to 3 do
  begin
    D[I]    := StrToInt(AInput[I + 1]);
    Out_[I] := (D[I] + (I + 1)) mod 10;
    if Out_[I] > 6 then
      Out_[I] := Out_[I] - 6;
  end;
  Result.Code    := Format('%d%d%d%d', [Out_[0], Out_[1], Out_[2], Out_[3]]);
  Result.Success := True;
end;

{ ---- Renault --------------------------------------------------------------- }
function TOBDRadioCodeRenault.BrandKey: string; begin Result := 'renault'; end;
function TOBDRadioCodeRenault.DisplayName: string; begin Result := 'Renault (pre-2004)'; end;
function TOBDRadioCodeRenault.Description: string;
begin
  Result :=
    'Renault Tuner List / Update List radios (pre-2004). Input: ' +
    'one letter + three digits (e.g. C123) — the last 4 of the ' +
    'pre-code. Algorithm bundled (ERDesigns Renault-Calculator).';
end;
function TOBDRadioCodeRenault.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason);
  if not Result then Exit;
  if not CharInSet(AInput[1], ['A'..'Z']) then
  begin
    AReason := 'First character must be a letter';
    Exit(False);
  end;
  Result := ValidateDigitRange(AInput, 2, 4, AReason);
  if not Result then Exit;
  if (AInput[1] = 'A') and (AInput[2] = '0') then
  begin
    AReason := 'Serial pair A0 is reserved';
    Exit(False);
  end;
end;
function TOBDRadioCodeRenault.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  X, Y, Z, C: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'pre-2004 (Tuner List / Update List)';
  // Magic constants 698 / 528 align the ASCII offsets so a serial
  // like "C123" produces the canonical pre-code → code mapping.
  X := Ord(AInput[1]) * 10 + Ord(AInput[2]) - 698;
  if X = 0 then X := 1;             // belt-and-braces; A0 already rejected
  Y := Ord(AInput[3]) * 10 + Ord(AInput[4]) + X - 528;
  Z := (Y * 7) mod 100;
  C := (Z div 10)
       + (Z mod 10) * 10
       + ((259 mod X) mod 100) * 100;
  Result.Code    := Format('%.4d', [C]);
  Result.Success := True;
end;

{ ---- Fiat Daiichi --------------------------------------------------------- }
function TOBDRadioCodeFiatDaiichi.BrandKey: string; begin Result := 'fiat-daiichi'; end;
function TOBDRadioCodeFiatDaiichi.DisplayName: string; begin Result := 'Fiat / Lancia Daiichi-Pioneer'; end;
function TOBDRadioCodeFiatDaiichi.Description: string;
begin
  Result :=
    'Fiat / Lancia Daiichi-Pioneer factory radios. Input: last 4 ' +
    'digits of the serial number. Algorithm bundled ' +
    '(ERDesigns Fiat-Daiichi-Calculator).';
end;
function TOBDRadioCodeFiatDaiichi.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;
function TOBDRadioCodeFiatDaiichi.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  D: array[0..3] of Integer;
  Out_: array[0..3] of Integer;
  I: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'Daiichi-Pioneer';
  for I := 0 to 3 do
    D[I] := StrToInt(AInput[I + 1]);
  // Each output digit is a fixed subtraction from the matching
  // input digit, written into the output in reverse order.
  Out_[3] := 10 - D[0];
  Out_[2] :=  9 - D[1];
  Out_[1] :=  9 - D[2];
  Out_[0] :=  9 - D[3];
  Result.Code    := Format('%d%d%d%d', [Out_[0], Out_[1], Out_[2], Out_[3]]);
  Result.Success := True;
end;

{ ---- Fiat VP1 / VP2 ------------------------------------------------------- }

function FiatVP_GetFirstByte(AInput: Integer): Integer;
begin
  if AInput > 10 then Exit(0);
  case AInput of
    6, 7, 8: Result := 0;
    9:       Result := 1;
  else
    Result := AInput;
  end;
end;

function FiatVP_GetSecondByte(AInput: Integer): Integer;
begin
  if AInput > 10 then Exit(0);
  case AInput of
    6, 7: Result := 1;
    8:    Result := 0;
    9:    Result := 1;
  else
    Result := AInput;
  end;
end;

function FiatVP_GetThirdByte(AInput: Integer): Integer;
begin
  if AInput > 10 then Exit(0);
  case AInput of
    6, 7: Result := 2;
    8:    Result := 0;
    9:    Result := 1;
  else
    Result := AInput;
  end;
end;

function FiatVP_GetFourthByte(AInput: Integer): Integer;
begin
  if AInput > 10 then Exit(0);
  case AInput of
    6, 7: Result := 3;
    8:    Result := 0;
    9:    Result := 1;
  else
    Result := AInput;
  end;
end;

function TOBDRadioCodeFiatVP.BrandKey: string; begin Result := 'fiat-vp'; end;
function TOBDRadioCodeFiatVP.DisplayName: string; begin Result := 'Fiat / Lancia Continental VP1 / VP2'; end;
function TOBDRadioCodeFiatVP.Description: string;
begin
  Result :=
    'Fiat / Lancia Continental VP1 / VP2 factory radios. Input: ' +
    'last 4 digits of the serial number. Both VP1 and VP2 use the ' +
    'same calculation. Algorithm bundled ' +
    '(ERDesigns Fiat-VP1-VP2-Calculator).';
end;
function TOBDRadioCodeFiatVP.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;
function TOBDRadioCodeFiatVP.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  Serial: Integer;
  SN: array[0..3] of Integer;
  OutputCode: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'VP1 / VP2 (Continental)';
  Serial := StrToInt(AInput);
  SN[0] := (Serial div 1000)              and $0F;
  SN[1] := (Serial mod 1000 div 100)      and $0F;
  SN[2] := (Serial mod 100  div 10)       and $0F;
  SN[3] := (Serial mod 10)                and $0F;
  OutputCode := 1111;
  Inc(OutputCode, FiatVP_GetThirdByte(SN[3])  * 10);
  Inc(OutputCode, FiatVP_GetFirstByte(SN[2])  * 1000);
  Inc(OutputCode, FiatVP_GetFourthByte(SN[1]));
  Inc(OutputCode, FiatVP_GetSecondByte(SN[0]) * 100);
  Result.Code    := Format('%.4d', [OutputCode]);
  Result.Success := True;
end;

{ ---- Alfa Romeo ----------------------------------------------------------- }
function TOBDRadioCodeAlfaRomeo.BrandKey: string; begin Result := 'alfa-romeo'; end;
function TOBDRadioCodeAlfaRomeo.DisplayName: string; begin Result := 'Alfa Romeo'; end;
function TOBDRadioCodeAlfaRomeo.Description: string;
begin
  Result :=
    'Alfa Romeo factory radios. Input: one letter + 7 digits. ' +
    'Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeAlfaRomeo.DoValidate(const AInput: string; out AReason: string): Boolean;
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

{ ---- Maserati ------------------------------------------------------------- }
function TOBDRadioCodeMaserati.BrandKey: string; begin Result := 'maserati'; end;
function TOBDRadioCodeMaserati.DisplayName: string; begin Result := 'Maserati'; end;
function TOBDRadioCodeMaserati.Description: string;
begin
  Result :=
    'Maserati factory radios. Input: 14 alphanumeric characters. ' +
    'Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeMaserati.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidateAlphanumericRange(AInput, 1, 14, AReason);
end;

initialization
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeCitroen);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodePeugeot);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeRenault);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeFiatDaiichi);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeFiatVP);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeAlfaRomeo);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeMaserati);

end.
