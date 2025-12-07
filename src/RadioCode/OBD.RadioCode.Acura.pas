//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Acura.pas
// CONTENTS       : Acura Radio Code Calculator (Alpine/Panasonic)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Acura;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Acura Radio Code Calculator (Alpine/Panasonic models)
  /// </summary>
  /// <remarks>
  ///   Acura uses Honda-derived systems with 8-character serials (1 letter + 7 digits)
  ///   Algorithm similar to Honda with Acura-specific adjustments
  /// </remarks>
  TOBDRadioCodeAcura = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeAcura.GetDescription: string;
begin
  Result := 'Calculate radio code for Acura radio''s (Alpine, Panasonic, Navigation systems).';
end;

function TOBDRadioCodeAcura.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Acura serial: 8 characters (1 letter + 7 digits)
  if not ValidateLength(Sanitized, 8, ErrorMessage) then
    Exit(False);

  // First character must be a letter
  if not CharInSet(Sanitized[1], ['A'..'Z']) then
  begin
    ErrorMessage := 'First character must be a letter (e.g., U, L, T)';
    Exit(False);
  end;

  // Remaining 7 characters must be digits
  for var I := 2 to 8 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9']) then
    begin
      ErrorMessage := Format('Character %d must be a digit', [I]);
      Exit(False);
    end;
  end;
end;

function TOBDRadioCodeAcura.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  SerialDigits: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
  LetterValue: Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  // Get letter value (A=1, B=2, ..., Z=26)
  LetterValue := Ord(Sanitized[1]) - Ord('A') + 1;

  // Get serial number (last 7 digits)
  SerialDigits := Copy(Sanitized, 2, 7);
  Serial := StrToIntDef(SerialDigits, 0);

  // Acura algorithm (Honda-derived with modifications)
  Code[0] := ApplyModularTransform((Serial div 10000) + LetterValue, 10);
  Code[1] := ApplyModularTransform((Serial div 100) mod 100 + LetterValue * 2, 10);
  Code[2] := ApplyModularTransform((Serial mod 1000) + LetterValue * 3, 10);
  Code[3] := ApplyModularTransform((Serial mod 100) + LetterValue * 4, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
