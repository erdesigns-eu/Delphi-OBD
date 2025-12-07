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
unit OBD.RadioCode;

interface

uses
  Winapi.Windows, System.SysUtils;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD RadioCode (INTERFACE)
  /// </summary>
  IOBDRadioCode = interface
    ['{F2733032-6446-4E7B-9477-4AD9C67F1CB3}']
    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string;
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
    function Validate(const Input: string; var ErrorMessage: string): Boolean;
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
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD RadioCode (CLASS)
  /// </summary>
  TOBDRadioCode = class(TInterfacedObject, IOBDRadioCode)
  protected
    /// <summary>
    ///   Sanitize input by removing whitespace and converting to uppercase
    /// </summary>
    function SanitizeInput(const Input: string): string;
    /// <summary>
    ///   Validate input length
    /// </summary>
    function ValidateLength(const Input: string; const ExpectedLength: Integer; var ErrorMessage: string): Boolean;
    /// <summary>
    ///   Validate that input contains only digits
    /// </summary>
    function ValidateDigits(const Input: string; var ErrorMessage: string): Boolean;
    /// <summary>
    ///   Validate that input contains only letters
    /// </summary>
    function ValidateLetters(const Input: string; var ErrorMessage: string): Boolean;
    /// <summary>
    ///   Apply modular transform (useful for various radio code algorithms)
    /// </summary>
    function ApplyModularTransform(const Value: Integer; const Modulus: Integer): Integer;
  public
    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string; virtual; abstract;
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
    function Validate(const Input: string; var ErrorMessage: string): Boolean; virtual; abstract;
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
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; virtual; abstract;
  end;

implementation

//------------------------------------------------------------------------------
// SANITIZE INPUT
//------------------------------------------------------------------------------
function TOBDRadioCode.SanitizeInput(const Input: string): string;
var
  I: Integer;
begin
  Result := '';
  // Remove whitespace and convert to uppercase
  for I := 1 to Length(Input) do
  begin
    if not CharInSet(Input[I], [' ', #9, #10, #13]) then
      Result := Result + UpCase(Input[I]);
  end;
end;

//------------------------------------------------------------------------------
// VALIDATE LENGTH
//------------------------------------------------------------------------------
function TOBDRadioCode.ValidateLength(const Input: string; const ExpectedLength: Integer; var ErrorMessage: string): Boolean;
begin
  Result := Length(Input) = ExpectedLength;
  if not Result then
    ErrorMessage := Format('Input must be exactly %d characters long (received %d)', [ExpectedLength, Length(Input)]);
end;

//------------------------------------------------------------------------------
// VALIDATE DIGITS
//------------------------------------------------------------------------------
function TOBDRadioCode.ValidateDigits(const Input: string; var ErrorMessage: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Input) do
  begin
    if not CharInSet(Input[I], ['0'..'9']) then
    begin
      ErrorMessage := Format('Input must contain only digits (invalid character at position %d: ''%s'')', [I, Input[I]]);
      Exit(False);
    end;
  end;
end;

//------------------------------------------------------------------------------
// VALIDATE LETTERS
//------------------------------------------------------------------------------
function TOBDRadioCode.ValidateLetters(const Input: string; var ErrorMessage: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Input) do
  begin
    if not CharInSet(Input[I], ['A'..'Z', 'a'..'z']) then
    begin
      ErrorMessage := Format('Input must contain only letters (invalid character at position %d: ''%s'')', [I, Input[I]]);
      Exit(False);
    end;
  end;
end;

//------------------------------------------------------------------------------
// APPLY MODULAR TRANSFORM
//------------------------------------------------------------------------------
function TOBDRadioCode.ApplyModularTransform(const Value: Integer; const Modulus: Integer): Integer;
begin
  if Modulus = 0 then
    Result := 0
  else
    Result := Value mod Modulus;
end;

end.
