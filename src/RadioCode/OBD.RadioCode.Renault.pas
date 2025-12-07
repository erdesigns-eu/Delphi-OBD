//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Renault.pas
// CONTENTS       : Renault Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 10/04/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Renault;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Renault RadioCode Calculator
  /// </summary>
  TOBDRadioCodeRenault = class(TOBDRadioCode)
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
function TOBDRadioCodeRenault.GetDescription: string;
begin
  Result := 'Calculate radio code for Renault radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeRenault.Validate(const Input: string; var ErrorMessage: string): Boolean;
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

  // Make sure the input starts with a letter
  if not CharInSet(Sanitized[1], ['A'..'Z']) then
  begin
    ErrorMessage := 'First character must be a letter!';
    Exit(False);
  end;

  // Make sure characters 2-4 are digits
  if not CharInSet(Sanitized[2], ['0'..'9']) or
     not CharInSet(Sanitized[3], ['0'..'9']) or
     not CharInSet(Sanitized[4], ['0'..'9']) then
  begin
    ErrorMessage := 'Characters 2-4 must be digits!';
    Exit(False);
  end;

  // The input can not start with A0
  if (Sanitized[1] = 'A') and (Sanitized[2] = '0') then
  begin
    ErrorMessage := 'Can not start with A0!';
    Exit(False);
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeRenault.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  X, Y, Z, C: Integer;
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

  // Calculate the code
  // Note: Convert char to digit value (Ord('0') = 48, so subtract 48 or use StrToInt)
  X := StrToInt(Sanitized[2]) + (Ord(Sanitized[1]) - Ord('A')) * 10 + 10;
  // Prevent division by zero using helper method
  if X = 0 then X := 1;
  
  Y := StrToInt(Sanitized[4]) + StrToInt(Sanitized[3]) * 10 + X;
  Z := ApplyModularTransform(Y * 7, 100);
  C := (Z div 10) + (Z mod 10) * 10 + ApplyModularTransform(ApplyModularTransform(259, X), 100) * 100;

  // Format the code for the output
  Output := Format('%.*d', [4, C]);
end;

end.

