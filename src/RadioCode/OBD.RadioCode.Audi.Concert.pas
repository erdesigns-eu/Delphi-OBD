//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Audi.Concert.pas
// CONTENTS       : Audi Concert Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Audi.Concert;

interface

uses
  WinApi.Windows, System.SysUtils, System.StrUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Audi Concert RadioCode Calculator
  ///   Supports Audi Concert and Symphony radios
  /// </summary>
  TOBDRadioCodeAudiConcert = class(TOBDRadioCode)
  public
    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string; override;
    /// <summary>
    ///   Radio Code input validator
    /// </summary>
    /// <param name="Input">
    ///   Audi serial number (format: AUZ series, typically 14 characters)
    /// </param>
    /// <param name="ErrorMessage">
    ///   A error message (Optional) that descibes why the input is invalid.
    /// </param>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    /// <summary>
    ///   Radio Code Calculator
    /// </summary>
    /// <param name="Input">
    ///   Audi serial number
    /// </param>
    /// <param name="Output">
    ///   The calculated radio code (4 digits).
    /// </param>
    /// <param name="ErrorMessage">
    ///   A error message (Optional) that descibes why the input is invalid.
    /// </param>
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

//------------------------------------------------------------------------------
// GET DESCRIPTION
//------------------------------------------------------------------------------
function TOBDRadioCodeAudiConcert.GetDescription: string;
begin
  Result := 'Calculate radio code for Audi Concert and Symphony radio''s (AUZ format).';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeAudiConcert.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input (remove whitespace, convert to uppercase)
  Sanitized := SanitizeInput(Input);

  // AUZ format: typically 14 characters
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // Must start with AUZ
  if not Sanitized.StartsWith('AUZ') then
  begin
    ErrorMessage := 'Serial must start with AUZ';
    Exit(False);
  end;

  // Characters 4-5 must be digits
  if not CharInSet(Sanitized[4], ['0'..'9']) or not CharInSet(Sanitized[5], ['0'..'9']) then
  begin
    ErrorMessage := 'Characters 4-5 must be digits';
    Exit(False);
  end;

  // Characters 6-8 must be letters
  for var I := 6 to 8 do
  begin
    if not CharInSet(Sanitized[I], ['A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be a letter', [I]);
      Exit(False);
    end;
  end;

  // Remaining characters should be alphanumeric
  for var I := 9 to 14 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9', 'A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeAudiConcert.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Z1, Z2: Integer;
  LetterSum: Integer;
  SerialNum: Integer;
  Code1, Code2, Code3, Code4: Integer;
  WorkValue: Integer;
begin
  // Initialize result
  Result := True;
  // Clear the output
  Output := '';
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input
  Sanitized := SanitizeInput(Input);

  // Check if the input is valid
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  // Extract components from AUZ format
  Z1 := StrToInt(Sanitized[4]);
  Z2 := StrToInt(Sanitized[5]);

  // Calculate letter sum (A=1, B=2, etc.)
  LetterSum := 0;
  for var I := 6 to 8 do
  begin
    if CharInSet(Sanitized[I], ['A'..'Z']) then
      LetterSum := LetterSum + (Ord(Sanitized[I]) - Ord('A') + 1);
  end;

  // Extract numeric serial portion
  SerialNum := 0;
  for var I := 9 to 14 do
  begin
    if CharInSet(Sanitized[I], ['0'..'9']) then
      SerialNum := SerialNum * 10 + (Ord(Sanitized[I]) - Ord('0'));
  end;

  // Audi Concert/Symphony algorithm
  WorkValue := (Z1 * 1000 + Z2 * 100 + LetterSum * 10 + (SerialNum mod 100));

  // Calculate code digits using specific Audi algorithm
  Code1 := ApplyModularTransform((WorkValue div 1000) + Z1 + 5, 10);
  Code2 := ApplyModularTransform((WorkValue div 100) + Z2 + 3, 10);
  Code3 := ApplyModularTransform((WorkValue div 10) + LetterSum, 10);
  Code4 := ApplyModularTransform(WorkValue + (SerialNum mod 10), 10);

  // Format the code for the output
  Output := Format('%d%d%d%d', [Code1, Code2, Code3, Code4]);
end;

end.
