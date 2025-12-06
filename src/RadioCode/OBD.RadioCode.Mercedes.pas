//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Mercedes.pas
// CONTENTS       : Mercedes-Benz Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Mercedes;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Mercedes-Benz Radio Code Calculator (Audio 10/20/30/50, Becker)
  /// </summary>
  TOBDRadioCodeMercedes = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeMercedes.GetDescription: string;
begin
  Result := 'Calculate radio code for Mercedes-Benz radio''s (Audio 10/20/30/50, Becker).';
end;

function TOBDRadioCodeMercedes.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Mercedes serial: typically 14 characters (letter + digits)
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // First character typically A, B, or L
  if not CharInSet(Sanitized[1], ['A'..'Z']) then
  begin
    ErrorMessage := 'First character must be a letter';
    Exit(False);
  end;

  // Remaining characters can be alphanumeric
  for var I := 2 to 14 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9', 'A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  end;
end;

function TOBDRadioCodeMercedes.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  LetterValue: Integer;
  NumericSum: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  // Letter value
  LetterValue := Ord(Sanitized[1]) - Ord('A') + 1;

  // Sum of all numeric values
  NumericSum := 0;
  for var I := 2 to 14 do
  begin
    if CharInSet(Sanitized[I], ['0'..'9']) then
      NumericSum := NumericSum + (Ord(Sanitized[I]) - Ord('0'))
    else
      NumericSum := NumericSum + (Ord(Sanitized[I]) - Ord('A') + 10);
  end;

  // Mercedes algorithm
  Code[0] := ApplyModularTransform((NumericSum div 100) + LetterValue * 7, 10);
  Code[1] := ApplyModularTransform((NumericSum div 10) + LetterValue * 3, 10);
  Code[2] := ApplyModularTransform(NumericSum + LetterValue * 5, 10);
  Code[3] := ApplyModularTransform((NumericSum * 2) + LetterValue, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
