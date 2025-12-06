//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.VW.RCD.pas
// CONTENTS       : VW RCD Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.VW.RCD;

interface

uses
  WinApi.Windows, System.SysUtils, System.StrUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD VW RCD RadioCode Calculator
  ///   Supports VW RCD radios with VWZ format serials
  /// </summary>
  TOBDRadioCodeVWRCD = class(TOBDRadioCode)
  public
    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string; override;
    /// <summary>
    ///   Radio Code input validator
    /// </summary>
    /// <param name="Input">
    ///   VWZ serial number (format: VWZ1Z2ABC1234567)
    /// </param>
    /// <param name="ErrorMessage">
    ///   A error message (Optional) that descibes why the input is invalid.
    /// </param>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    /// <summary>
    ///   Radio Code Calculator
    /// </summary>
    /// <param name="Input">
    ///   VWZ serial number
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
function TOBDRadioCodeVWRCD.GetDescription: string;
begin
  Result := 'Calculate radio code for VW RCD radio''s (VWZ format).';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeVWRCD.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Sanitize input (remove whitespace, convert to uppercase)
  Sanitized := SanitizeInput(Input);

  // VWZ format: VWZ + 1 digit + 1 digit + 3 letters + 7 digits = 14 characters
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // Must start with VWZ
  if not Sanitized.StartsWith('VWZ') then
  begin
    ErrorMessage := 'Serial must start with VWZ';
    Exit(False);
  end;

  // Characters 4-5 must be digits (Z1, Z2)
  if not CharInSet(Sanitized[4], ['0'..'9']) or not CharInSet(Sanitized[5], ['0'..'9']) then
  begin
    ErrorMessage := 'Characters 4-5 must be digits';
    Exit(False);
  end;

  // Characters 6-8 must be letters (ABC)
  for var I := 6 to 8 do
  begin
    if not CharInSet(Sanitized[I], ['A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be a letter', [I]);
      Exit(False);
    end;
  end;

  // Characters 9-14 must be digits (1234567)
  for var I := 9 to 14 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9']) then
    begin
      ErrorMessage := Format('Character %d must be a digit', [I]);
      Exit(False);
    end;
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeVWRCD.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Z1, Z2: Integer;
  A, B, C: Char;
  SerialNum: Integer;
  Code1, Code2, Code3, Code4: Integer;
  BaseValue: Integer;
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

  // Extract components from VWZ format
  Z1 := StrToInt(Sanitized[4]);
  Z2 := StrToInt(Sanitized[5]);
  A := Sanitized[6];
  B := Sanitized[7];
  C := Sanitized[8];
  SerialNum := StrToInt(Copy(Sanitized, 9, 6)); // Last 6 digits of the 7-digit serial

  // VW RCD algorithm
  // Base calculation using letter positions (A=1, B=2, etc.)
  BaseValue := (Ord(A) - Ord('A') + 1) * 100 + 
               (Ord(B) - Ord('A') + 1) * 10 + 
               (Ord(C) - Ord('A') + 1);

  // Calculate code digits using modular arithmetic and serial number
  Code1 := ApplyModularTransform(Z1 + Z2 + BaseValue + (SerialNum div 1000), 10);
  Code2 := ApplyModularTransform(Z1 * 2 + Z2 + (SerialNum div 100), 10);
  Code3 := ApplyModularTransform(BaseValue + (SerialNum div 10), 10);
  Code4 := ApplyModularTransform(Z1 + Z2 * 2 + SerialNum, 10);

  // Format the code for the output
  Output := Format('%d%d%d%d', [Code1, Code2, Code3, Code4]);
end;

end.
