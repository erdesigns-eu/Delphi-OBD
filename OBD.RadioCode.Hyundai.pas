//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Hyundai.pas
// CONTENTS       : Hyundai/Kia Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Hyundai;

interface

uses
  WinApi.Windows, System.SysUtils, OBD.RadioCode;

type
  TOBDRadioCodeHyundai = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeHyundai.GetDescription: string;
begin
  Result := 'Calculate radio code for Hyundai/Kia radios (Mobis, Blaupunkt).';
end;

function TOBDRadioCodeHyundai.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';
  Sanitized := SanitizeInput(Input);

  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeHyundai.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  D1, D2, D3, D4: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  D1 := StrToInt(Sanitized[1]);
  D2 := StrToInt(Sanitized[2]);
  D3 := StrToInt(Sanitized[3]);
  D4 := StrToInt(Sanitized[4]);

  Code[0] := ApplyModularTransform(D1 * 3 + D4 + 5, 10);
  Code[1] := ApplyModularTransform(D2 * 7 + D1 + 2, 10);
  Code[2] := ApplyModularTransform(D3 * 5 + D2 + 8, 10);
  Code[3] := ApplyModularTransform(D4 * 9 + D3 + 1, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
