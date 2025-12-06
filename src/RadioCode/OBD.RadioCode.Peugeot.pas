//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Peugeot.pas
// CONTENTS       : Peugeot Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 14/04/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Peugeot;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Peugeot RadioCode Calculator
  /// </summary>
  TOBDRadioCodePeugeot = class(TOBDRadioCode)
  public
    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string; override;
    /// <summary>
    ///   Radio Code input validator
    /// </summary>
    /// <param name="Input">
    ///   The serial number or any other needed input to calculate the
    ///   radio code.
    /// </param>
    ///  /// <param name="ErrorMessage">
    ///   A error message (Optional) that descibes why the input is invalid.
    /// </param>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    /// <summary>
    ///   Radio Code Calculator
    /// </summary>
    /// <param name="Input">
    ///   The serial number or any other needed input to calculate the
    ///   radio code.
    /// </param>
    /// <param name="Output">
    ///   The calculated radio code.
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
function TOBDRadioCodePeugeot.GetDescription: string;
begin
  Result := 'Calculate radio code for old Peugeot radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodePeugeot.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input (remove whitespace, convert to uppercase)
  Sanitized := SanitizeInput(Input);

  // Validate length using helper method
  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);

  // Validate that all characters are digits using helper method
  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodePeugeot.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  I, D1, D2, D3, D4: Integer;
  SNArr: array[0..3] of Integer;
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

  // Initialize variables
  D1 := StrToInt(Sanitized[1]);
  D2 := StrToInt(Sanitized[2]);
  D3 := StrToInt(Sanitized[3]);
  D4 := StrToInt(Sanitized[4]);

  // Perform calculations using helper method for modular arithmetic
  SNArr[0] := ApplyModularTransform(D1 + 1, 10);
  SNArr[1] := ApplyModularTransform(D2 + 2, 10);
  SNArr[2] := ApplyModularTransform(D3 + 3, 10);
  SNArr[3] := ApplyModularTransform(D4 + 4, 10);

  // Apply correction
  for I := 0 to 3 do
  begin
    if SNArr[I] > 6 then SNArr[I] := SNArr[I] - 6;
  end;

  // Format the code for the output
  Output := Format('%d%d%d%d', [SNArr[0], SNArr[1], SNArr[2], SNArr[3]]);
end;

end.
