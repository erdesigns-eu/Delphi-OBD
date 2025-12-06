//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Blaupunkt.pas
// CONTENTS       : Blaupunkt Radio Code Calculator (Generic Models)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Blaupunkt;

interface

uses
  WinApi.Windows, System.SysUtils, OBD.RadioCode;

type
  TOBDRadioCodeBlaupunkt = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeBlaupunkt.GetDescription: string;
begin
  Result := 'Calculate radio code for Blaupunkt radios (generic models).';
end;

function TOBDRadioCodeBlaupunkt.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';
  Sanitized := SanitizeInput(Input);

  if not ((Length(Sanitized) = 7) and CharInSet(Sanitized[1], ['A'..'Z'])) then
  begin
    ErrorMessage := 'Blaupunkt serial must be 7 characters: 1 letter + 6 digits';
    Exit(False);
  end;

  for var I := 2 to 7 do
    if not CharInSet(Sanitized[I], ['0'..'9']) then
    begin
      ErrorMessage := Format('Character %d must be a digit', [I]);
      Exit(False);
    end;
end;

function TOBDRadioCodeBlaupunkt.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  LetterValue: Integer;
  Serial: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  LetterValue := Ord(Sanitized[1]) - Ord('A') + 1;
  Serial := StrToInt(Copy(Sanitized, 2, 6));

  Code[0] := ApplyModularTransform((Serial div 10000) + LetterValue * 3, 10);
  Code[1] := ApplyModularTransform((Serial div 1000) + LetterValue + 5, 10);
  Code[2] := ApplyModularTransform((Serial div 100) + LetterValue * 2, 10);
  Code[3] := ApplyModularTransform((Serial div 10) + LetterValue + 7, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
