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
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Make sure the input is 4 characters long
  if not (Length(Input) = 4) then
  begin
    ErrorMessage := 'Must be 4 characters long!';
    Exit(False);
  end;

  // Make sure the input starts with a letter
  if not CharInSet(Input[1], ['A'..'Z']) then
  begin
    ErrorMessage := 'First character must be a letter!';
    Exit(False);
  end;

  // Make sure the second character is a digit
  if not (CharInSet(Input[2], ['0'..'9'])) then
  begin
    ErrorMessage := 'Second character must be a digit!';
    Exit(False);
  end;

  // Make sure the third character is a digit
  if not (CharInSet(Input[3], ['0'..'9'])) then
  begin
    ErrorMessage := 'Third character must be a digit!';
    Exit(False);
  end;

  // Make sure the fourth character is a digit
  if not (CharInSet(Input[4], ['0'..'9'])) then
  begin
    ErrorMessage := 'Fourth character must be a digit!';
    Exit(False);
  end;

  // The input can not start with A0
  if (UpperCase(Input[1]) = 'A') and (UpperCase(Input[2]) = '0') then
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
  X, Y, Z, C: Integer;
begin
  // Initialize result
  Result := True;
  // Clear the output
  Output := '';
  // Clear the error message
  ErrorMessage := '';

  // Check if the input is valid
  if not Self.Validate(Input, ErrorMessage) then Exit(False);

  // Calculate the code
  X := Ord(Input[2]) + (Ord(Input[1])) * 10 - 698;
  Y := Ord(Input[4]) + (Ord(Input[3])) * 10 + X - 528;
  Z := (Y * 7) mod 100;
  C := (Z div 10) + (Z mod 10) * 10 + ((259 mod X) mod 100) * 100;

  // Format the code for the output
  Output := Format('%.*d', [4, C]);
end;

end.

