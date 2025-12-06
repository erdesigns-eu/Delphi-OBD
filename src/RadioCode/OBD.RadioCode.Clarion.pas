//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Clarion.pas
// CONTENTS       : Clarion Radio Code Calculator (Universal Models)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Clarion;

interface

uses
  WinApi.Windows, System.SysUtils, OBD.RadioCode;

type
  TOBDRadioCodeClarion = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeClarion.GetDescription: string;
begin
  Result := 'Calculate radio code for Clarion universal radios.';
end;

function TOBDRadioCodeClarion.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';
  Sanitized := SanitizeInput(Input);

  if not ValidateLength(Sanitized, 5, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeClarion.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  Serial := StrToInt(Sanitized);

  Code[0] := ApplyModularTransform((Serial div 1000) + 6, 10);
  Code[1] := ApplyModularTransform((Serial div 100) * 4 + 2, 10);
  Code[2] := ApplyModularTransform((Serial div 10) + 9, 10);
  Code[3] := ApplyModularTransform(Serial * 3 + 5, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
