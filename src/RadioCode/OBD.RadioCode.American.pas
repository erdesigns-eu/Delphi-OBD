//------------------------------------------------------------------------------
//  OBD.RadioCode.American
//
//  Vendor radio-code calculator components for North-American
//  brands:
//
//    TOBDRadioCodeChrysler  Chrysler / Jeep / Dodge (4 digits)
//    TOBDRadioCodeFordM     Ford "M-prefix" series (6 digits;
//                            optional leading M). Database lookup
//                            ships separately (TOBDRadioCodeFordV
//                            in OBD.RadioCode.FordV).
//    TOBDRadioCodeGM        GM Delco Theftlock (4-digit pre-code).
//                            Real codes are dealer-issued; no
//                            public algorithm.
//    TOBDRadioCodeVisteon   Visteon Fiat-Stilo / Bravo (6 digits).
//                            Algorithm proprietary (PELock).
//
//  All four ship as OnCalculate stubs. The Ford "V-series" with
//  the bundled 100,000-entry serial->code database lives in the
//  separate <c>OBD.RadioCode.FordV</c> unit so a host
//  that doesn't need it doesn't drag the database in.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.RadioCode.American;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  TOBDRadioCodeChrysler = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Ford "M-prefix" series factory radios (M-radio
  /// 1996+). Input: 6-digit serial, optionally with a leading M
  /// (which is stripped). Algorithm <b>bundled</b> — re-derived
  /// from OlegSmelov/ford-radio-codes.</summary>
  TOBDRadioCodeFordM = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeGM = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeVisteon = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

{ ---- Chrysler ------------------------------------------------------------- }
function TOBDRadioCodeChrysler.BrandKey: string; begin Result := 'chrysler'; end;
function TOBDRadioCodeChrysler.DisplayName: string; begin Result := 'Chrysler / Jeep / Dodge'; end;
function TOBDRadioCodeChrysler.Description: string;
begin
  Result :=
    'Chrysler / Jeep / Dodge factory radios (TM9 / Panasonic). ' +
    'Input: 4-digit pre-code. Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeChrysler.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

{ ---- Ford M --------------------------------------------------------------- }
function TOBDRadioCodeFordM.BrandKey: string; begin Result := 'ford-m'; end;
function TOBDRadioCodeFordM.DisplayName: string; begin Result := 'Ford M-series'; end;
function TOBDRadioCodeFordM.Description: string;
begin
  Result :=
    'Ford "M-prefix" series factory radios (M-radio 1996+). Input: ' +
    '6-digit serial, optionally with a leading M (stripped). ' +
    'Algorithm bundled — substitution-table transform sourced from ' +
    'OlegSmelov/ford-radio-codes.';
end;
function TOBDRadioCodeFordM.DoValidate(const AInput: string; out AReason: string): Boolean;
var
  Stripped: string;
begin
  if (Length(AInput) > 0) and (AInput[1] = 'M') then
    Stripped := Copy(AInput, 2, Length(AInput) - 1)
  else
    Stripped := AInput;
  Result := ValidateLength(Stripped, 6, AReason)
       and  ValidateAllDigits(Stripped, AReason);
end;
function TOBDRadioCodeFordM.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
const
  // 10x10 substitution table from OlegSmelov/ford-radio-codes.
  LOOKUP: array[0..9, 0..9] of Byte = (
    (9, 5, 3, 4, 8, 7, 2, 6, 1, 0),
    (2, 1, 5, 6, 9, 3, 7, 0, 4, 8),
    (0, 4, 7, 3, 1, 9, 6, 5, 8, 2),
    (5, 6, 4, 1, 2, 8, 0, 9, 3, 7),
    (6, 3, 1, 2, 0, 5, 4, 8, 7, 9),
    (4, 0, 8, 7, 6, 1, 9, 3, 2, 5),
    (7, 8, 0, 5, 3, 2, 1, 4, 9, 6),
    (1, 9, 6, 8, 7, 4, 5, 2, 0, 3),
    (3, 2, 9, 0, 4, 6, 8, 7, 5, 1),
    (8, 7, 2, 9, 5, 0, 3, 1, 6, 4)
  );
var
  Stripped: string;
  N: array[0..6] of Byte;
  R: array[1..7] of Byte;
  Res: array[1..4] of Byte;
  XRes: array[1..4] of Integer;
  XRes1, XRes0: Integer;
  Code: array[0..3] of Byte;
  I: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'M-prefix substitution-table';

  // Strip leading M.
  if (Length(AInput) > 0) and (AInput[1] = 'M') then
    Stripped := Copy(AInput, 2, Length(AInput) - 1)
  else
    Stripped := AInput;

  // Reverse the 6 digits into N[0..5]; N[6] is fixed at 0.
  for I := 0 to 5 do
    N[I] := Ord(Stripped[6 - I]) - Ord('0');
  N[6] := 0;

  R[1] := LOOKUP[N[0], 5];
  R[2] := LOOKUP[N[1], 3];
  R[3] := LOOKUP[N[2], 8];
  R[4] := LOOKUP[N[3], 2];
  R[5] := LOOKUP[N[4], 1];
  R[6] := LOOKUP[N[5], 6];
  R[7] := LOOKUP[N[6], 9];

  Res[1] := ((LOOKUP[R[2], R[1]] + 1) * (LOOKUP[R[6], R[2]] + 1)
           + (LOOKUP[R[4], R[3]] + 1) * (LOOKUP[R[7], R[5]] + 1)
           +  LOOKUP[R[1], R[4]]) mod 10;
  Res[2] := ((LOOKUP[R[2], R[1]] + 1) * (LOOKUP[R[5], R[4]] + 1)
           + (LOOKUP[R[5], R[2]] + 1) * (LOOKUP[R[7], R[3]] + 1)
           +  LOOKUP[R[1], R[6]]) mod 10;
  Res[3] := ((LOOKUP[R[2], R[1]] + 1) * (LOOKUP[R[4], R[2]] + 1)
           + (LOOKUP[R[3], R[6]] + 1) * (LOOKUP[R[7], R[4]] + 1)
           +  LOOKUP[R[1], R[5]]) mod 10;
  Res[4] := ((LOOKUP[R[2], R[1]] + 1) * (LOOKUP[R[6], R[3]] + 1)
           + (LOOKUP[R[3], R[7]] + 1) * (LOOKUP[R[2], R[5]] + 1)
           +  LOOKUP[R[4], R[1]]) mod 10;

  XRes[1] := (LOOKUP[Res[1], 5] + 1) * (LOOKUP[Res[2], 1] + 1) + 105;
  XRes[2] := (LOOKUP[Res[2], 1] + 1) * (LOOKUP[Res[4], 0] + 1) + 102;
  XRes[3] := (LOOKUP[Res[1], 5] + 1) * (LOOKUP[Res[3], 8] + 1) + 103;
  XRes[4] := (LOOKUP[Res[3], 8] + 1) * (LOOKUP[Res[4], 0] + 1) + 108;

  // Code digits: position 3 from XRes[1], 2 from XRes[2], etc.
  for I := 1 to 4 do
  begin
    XRes1 := (XRes[I] div 10) mod 10;
    XRes0 :=  XRes[I] mod 10;
    Code[4 - I] := (XRes1 + XRes0 + R[1]) mod 10;
  end;
  Result.Code    := Format('%d%d%d%d',
    [Code[0], Code[1], Code[2], Code[3]]);
  Result.Success := True;
end;

{ ---- GM ------------------------------------------------------------------- }
function TOBDRadioCodeGM.BrandKey: string; begin Result := 'gm'; end;
function TOBDRadioCodeGM.DisplayName: string; begin Result := 'GM Delco (Theftlock)'; end;
function TOBDRadioCodeGM.Description: string;
begin
  Result :=
    'GM Delco Theftlock factory radios. The procedure is to extract ' +
    'a 6-digit ID from the radio (LOC mode) and call the GM dealer ' +
    'who looks up the 4-digit code. Input: 4-digit Theftlock code as ' +
    'returned by the dealer. No public algorithm — wire OnCalculate ' +
    'for any custom flow.';
end;
function TOBDRadioCodeGM.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

{ ---- Visteon ------------------------------------------------------------- }
function TOBDRadioCodeVisteon.BrandKey: string; begin Result := 'visteon'; end;
function TOBDRadioCodeVisteon.DisplayName: string; begin Result := 'Visteon (Fiat Stilo / Bravo)'; end;
function TOBDRadioCodeVisteon.Description: string;
begin
  Result :=
    'Visteon Fiat-Stilo / Bravo factory radios. Input: 6 digits. ' +
    'Algorithm proprietary (commercial only) — wire OnCalculate.';
end;
function TOBDRadioCodeVisteon.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 6, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

initialization
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeChrysler);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeFordM);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeGM);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeVisteon);

end.
