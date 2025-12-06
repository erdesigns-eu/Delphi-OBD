//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.VW.RNS.pas
// CONTENTS       : VW RNS Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.VW.RNS;

interface

uses
  WinApi.Windows, System.SysUtils, System.StrUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD VW RNS RadioCode Calculator
  ///   Supports VW RNS navigation radios
  /// </summary>
  TOBDRadioCodeVWRNS = class(TOBDRadioCode)
  public
    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string; override;
    /// <summary>
    ///   Radio Code input validator
    /// </summary>
    /// <param name="Input">
    ///   RNS serial number (format: VWZ or similar 14-char format)
    /// </param>
    /// <param name="ErrorMessage">
    ///   A error message (Optional) that descibes why the input is invalid.
    /// </param>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    /// <summary>
    ///   Radio Code Calculator
    /// </summary>
    /// <param name="Input">
    ///   RNS serial number
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
function TOBDRadioCodeVWRNS.GetDescription: string;
begin
  Result := 'Calculate radio code for VW RNS navigation radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeVWRNS.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input (remove whitespace, convert to uppercase)
  Sanitized := SanitizeInput(Input);

  // RNS format: Similar to RCD, 14 characters
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // Must start with VWZ or similar prefix
  if not (Sanitized.StartsWith('VWZ') or Sanitized.StartsWith('RNS')) then
  begin
    ErrorMessage := 'Serial must start with VWZ or RNS';
    Exit(False);
  end;

  // Validate format: 3 letters + 2 digits + 3 letters + 6 digits
  // Characters 4-5 should be digits
  if not CharInSet(Sanitized[4], ['0'..'9']) or not CharInSet(Sanitized[5], ['0'..'9']) then
  begin
    ErrorMessage := 'Characters 4-5 must be digits';
    Exit(False);
  end;

  // Characters 6-8 should be letters
  for var I := 6 to 8 do
  begin
    if not CharInSet(Sanitized[I], ['A'..'Z', '0'..'9']) then
    begin
      ErrorMessage := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeVWRNS.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Part1, Part2: Integer;
  Code1, Code2, Code3, Code4: Integer;
  SerialDigits: string;
  CheckSum: Integer;
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

  // Extract numeric parts from serial
  SerialDigits := '';
  for var I := 1 to Length(Sanitized) do
  begin
    if CharInSet(Sanitized[I], ['0'..'9']) then
      SerialDigits := SerialDigits + Sanitized[I];
  end;

  // Need at least 8 digits for calculation
  if Length(SerialDigits) < 8 then
  begin
    ErrorMessage := 'Serial must contain at least 8 digits';
    Exit(False);
  end;

  // RNS algorithm - different from RCD
  Part1 := StrToInt(Copy(SerialDigits, 1, 4));
  Part2 := StrToInt(Copy(SerialDigits, 5, 4));

  // Calculate checksum
  CheckSum := ApplyModularTransform(Part1 + Part2, 10000);

  // Generate code digits
  Code1 := ApplyModularTransform(CheckSum div 1000, 10);
  Code2 := ApplyModularTransform((CheckSum div 100) + 3, 10);
  Code3 := ApplyModularTransform((CheckSum div 10) + 7, 10);
  Code4 := ApplyModularTransform(CheckSum + 9, 10);

  // Format the code for the output
  Output := Format('%d%d%d%d', [Code1, Code2, Code3, Code4]);
end;

end.
