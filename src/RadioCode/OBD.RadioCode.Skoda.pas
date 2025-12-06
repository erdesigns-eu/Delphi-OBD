//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Skoda.pas
// CONTENTS       : Skoda Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Skoda;

interface

uses
  WinApi.Windows, System.SysUtils, System.StrUtils, OBD.RadioCode;

type
  TOBDRadioCodeSkoda = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeSkoda.GetDescription: string;
begin
  Result := 'Calculate radio code for Skoda radios (similar to VW/Audi).';
end;

function TOBDRadioCodeSkoda.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';
  Sanitized := SanitizeInput(Input);

  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  if not Sanitized.StartsWith('SKZ') and not Sanitized.StartsWith('VWZ') then
  begin
    ErrorMessage := 'Serial must start with SKZ or VWZ';
    Exit(False);
  end;

  for var I := 4 to 5 do
    if not CharInSet(Sanitized[I], ['0'..'9']) then
    begin
      ErrorMessage := Format('Character %d must be a digit', [I]);
      Exit(False);
    end;

  for var I := 6 to 8 do
    if not CharInSet(Sanitized[I], ['A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be a letter', [I]);
      Exit(False);
    end;
end;

function TOBDRadioCodeSkoda.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Z1, Z2, SerialNum: Integer;
  LetterSum: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  Z1 := StrToInt(Sanitized[4]);
  Z2 := StrToInt(Sanitized[5]);

  LetterSum := (Ord(Sanitized[6]) - Ord('A') + 1) * 100 +
               (Ord(Sanitized[7]) - Ord('A') + 1) * 10 +
               (Ord(Sanitized[8]) - Ord('A') + 1);

  SerialNum := StrToInt(Copy(Sanitized, 9, 6));

  Code[0] := ApplyModularTransform(Z1 + Z2 + LetterSum + (SerialNum div 1000), 10);
  Code[1] := ApplyModularTransform(Z1 * 2 + Z2 + (SerialNum div 100), 10);
  Code[2] := ApplyModularTransform(LetterSum + (SerialNum div 10), 10);
  Code[3] := ApplyModularTransform(Z1 + Z2 * 2 + SerialNum, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
