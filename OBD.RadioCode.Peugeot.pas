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
  if not CharInSet(Input[1], ['0'..'9']) then
  begin
    ErrorMessage := 'First character must be a digit!';
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
function TOBDRadioCodePeugeot.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  I, D1, D2, D3, D4: Integer;
  SNArr: array[0..3] of Integer;
begin
  // Initialize result
  Result := True;
  // Clear the output
  Output := '';
  // Clear the error message
  ErrorMessage := '';

  // Check if the input is valid
  if not Self.Validate(Input, ErrorMessage) then Exit(False);

  // Initialize variables
  D1 := StrToInt(Input[1]);
  D2 := StrToInt(Input[2]);
  D3 := StrToInt(Input[3]);
  D4 := StrToInt(Input[4]);

  // Perform calculations
  SNArr[0] := (D1 + 1) mod 10;
  SNArr[1] := (D2 + 2) mod 10;
  SNArr[2] := (D3 + 3) mod 10;
  SNArr[3] := (D4 + 4) mod 10;

  // Apply correction
  for I := 0 to 3 do
  begin
    if SNArr[I] > 6 then SNArr[I] := SNArr[I] - 6;
  end;

  // Format the code for the output
  Output := Format('%d%d%d%d', [SNArr[0], SNArr[1], SNArr[2], SNArr[3]]);
end;

end.
