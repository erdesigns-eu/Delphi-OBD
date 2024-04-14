//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Fiat.Daiichi.pas
// CONTENTS       : Fiat Daiichi Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 14/04/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Fiat.Daiichi;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Fiat Daiichi RadioCode Calculator
  /// </summary>
  TOBDRadioCodeFiatDaiichi = class(TOBDRadioCode)
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
function TOBDRadioCodeFiatDaiichi.GetDescription: string;
begin
  Result := 'Calculate radio code for Fiat Daiichi radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeFiatDaiichi.Validate(const Input: string; var ErrorMessage: string): Boolean;
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

  // Make sure the input starts with a digit
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
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeFiatDaiichi.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  I: Integer;
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

  // Initialize SNArr with default values
  FillChar(SNArr, SizeOf(SNArr), 0);

  // Iterate through each character in the input string
  for I := 1 to Length(Input) do
  begin
    // Update SNArr based on the character and index
    case I of
      1: SNArr[3] := 10 - StrToInt(Input[I]);
      2: SNArr[2] := 9 - StrToInt(Input[I]);
      3: SNArr[1] := 9 - StrToInt(Input[I]);
      4: SNArr[0] := 9 - StrToInt(Input[I]);
    end;
  end;

  // Format the code for the output
  Output := Format('%d%d%d%d', [SNArr[0], SNArr[1], SNArr[2], SNArr[3]]);
end;

end.
