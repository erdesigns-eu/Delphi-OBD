//------------------------------------------------------------------------------
//  OBD.RadioCode.British
//
//  Vendor radio-code calculator components for British brands
//  (P-A1.4):
//
//    TOBDRadioCodeJaguar     Jaguar factory radios (Alpine-supplied)
//    TOBDRadioCodeLandRover  Land Rover (Alpine / Visteon-supplied)
//    TOBDRadioCodeSaab       Saab YS / PH-series radios
//    TOBDRadioCodeOpel       Opel pre-CD30 (4-digit pre-code).
//                            CD30 / CD70 use EEPROM extraction —
//                            see docs/radio-code-algorithms.md.
//
//  No bundled algorithms. The British units largely use
//  Alpine / Visteon-supplied head units whose algorithms are
//  proprietary; CD30 / CD70 codes live in the radio's EEPROM
//  rather than being computed from the serial. Wire
//  <c>OnCalculate</c> with your own implementation.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Validation
//                     re-derived from the v1 vendor units.
//------------------------------------------------------------------------------

unit OBD.RadioCode.British;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  TOBDRadioCodeJaguar = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeLandRover = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeSaab = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeOpel = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

{ ---- Jaguar --------------------------------------------------------------- }
function TOBDRadioCodeJaguar.BrandKey: string; begin Result := 'jaguar'; end;
function TOBDRadioCodeJaguar.DisplayName: string; begin Result := 'Jaguar (Alpine)'; end;
function TOBDRadioCodeJaguar.Description: string;
begin
  Result :=
    'Jaguar factory radios (Alpine-supplied). Input: 14 alphanumeric ' +
    'characters. Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeJaguar.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidateAlphanumericRange(AInput, 1, 14, AReason);
end;

{ ---- Land Rover ----------------------------------------------------------- }
function TOBDRadioCodeLandRover.BrandKey: string; begin Result := 'land-rover'; end;
function TOBDRadioCodeLandRover.DisplayName: string; begin Result := 'Land Rover (Alpine / Visteon)'; end;
function TOBDRadioCodeLandRover.Description: string;
begin
  Result :=
    'Land Rover factory radios (Alpine / Visteon-supplied). Input: ' +
    '14 alphanumeric characters. Algorithm not bundled — wire ' +
    'OnCalculate.';
end;
function TOBDRadioCodeLandRover.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidateAlphanumericRange(AInput, 1, 14, AReason);
end;

{ ---- Saab ----------------------------------------------------------------- }
function TOBDRadioCodeSaab.BrandKey: string; begin Result := 'saab'; end;
function TOBDRadioCodeSaab.DisplayName: string; begin Result := 'Saab'; end;
function TOBDRadioCodeSaab.Description: string;
begin
  Result :=
    'Saab YS / PH-series factory radios. Input: 14 alphanumeric ' +
    'characters. Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeSaab.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 14, AReason)
       and  ValidateAlphanumericRange(AInput, 1, 14, AReason);
end;

{ ---- Opel ----------------------------------------------------------------- }
function TOBDRadioCodeOpel.BrandKey: string; begin Result := 'opel'; end;
function TOBDRadioCodeOpel.DisplayName: string; begin Result := 'Opel (pre-CD30)'; end;
function TOBDRadioCodeOpel.Description: string;
begin
  Result :=
    'Opel pre-CD30 factory radios. Input: 4-digit pre-code. CD30 / ' +
    'CD70 codes live in the radio''s EEPROM (24C32 / 95640) at a ' +
    'fixed offset and require a chip read — not a calculation. ' +
    'Algorithm not bundled — wire OnCalculate.';
end;
function TOBDRadioCodeOpel.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

initialization
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeJaguar);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeLandRover);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeSaab);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeOpel);

end.
