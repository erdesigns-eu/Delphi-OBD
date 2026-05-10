//------------------------------------------------------------------------------
//  OBD.RadioCode.American
//
//  Vendor radio-code calculator components for North-American
//  brands (P-A1.6):
//
//    TOBDRadioCodeChrysler  Chrysler / Jeep / Dodge (4 digits)
//    TOBDRadioCodeFordM     Ford "M-prefix" series (6 digits;
//                            optional leading M). Database lookup
//                            ships separately (TOBDRadioCodeFordV
//                            in OBD.RadioCode.FordVDatabase).
//    TOBDRadioCodeGM        GM Delco Theftlock (4-digit pre-code).
//                            Real codes are dealer-issued; no
//                            public algorithm.
//    TOBDRadioCodeVisteon   Visteon Fiat-Stilo / Bravo (6 digits).
//                            Algorithm proprietary (PELock).
//
//  All four ship as OnCalculate stubs. The Ford "V-series" with
//  the bundled 100,000-entry serial->code database lives in the
//  separate <c>OBD.RadioCode.FordVDatabase</c> unit so a host
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

  TOBDRadioCodeFordM = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
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
    'Ford "M-prefix" series factory radios. Input: 6 digits, ' +
    'optionally with a leading M (which is stripped before lookup). ' +
    'Algorithm not bundled — wire OnCalculate. A bundled-database ' +
    'variant (Ford V-series, 100,000 serial->code entries) ships as ' +
    'TOBDRadioCodeFordV in OBD.RadioCode.FordVDatabase.';
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
