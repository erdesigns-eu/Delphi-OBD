//------------------------------------------------------------------------------
//  OBD.RadioCode.Volvo
//
//  P-A1.8: Volvo factory radios. The HU / SC-7xx code lives in
//  the head-unit's 24C01 EEPROM (no algorithm — must be read off
//  the chip). Component validates the 7-digit serial format and
//  fires <c>OnCalculate</c> for the host to wire its own EEPROM-
//  read flow or licensed code-service call.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.RadioCode.Volvo;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  TOBDRadioCodeVolvo = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

function TOBDRadioCodeVolvo.BrandKey: string; begin Result := 'volvo'; end;
function TOBDRadioCodeVolvo.DisplayName: string; begin Result := 'Volvo (HU / SC-7xx)'; end;
function TOBDRadioCodeVolvo.Description: string;
begin
  Result :=
    'Volvo HU / SC-7xx factory radios. Input: 7-digit serial. The ' +
    'code is stored in the head-unit''s 24C01 EEPROM and is not ' +
    'computed from the serial — host wires OnCalculate with its own ' +
    'EEPROM-read flow or a licensed code service.';
end;
function TOBDRadioCodeVolvo.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 7, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

initialization
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeVolvo);

end.
