//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Fiat.VP.pas
// CONTENTS       : Fiat VP1/VP2 Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 10/04/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Fiat.VP;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Fiat VP1/VP2 RadioCode Calculator
  /// </summary>
  TOBDRadioCodeFiatVP = class(TOBDRadioCode)
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
function TOBDRadioCodeFiatVP.GetDescription: string;
begin
  Result := 'Calculate radio code for Fiat/Alfa-romeo VP1/VP2 radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeFiatVP.Validate(const Input: string; var ErrorMessage: string): Boolean;
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
function TOBDRadioCodeFiatVP.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;

  function GetFourthByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7: Result := 3;
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

  function GetThirdByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7: Result := 2;
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

  function GetSecondByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7: Result := 1;
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

  function GetFirstByte(Input: Integer): Integer;
  begin
    if (Input > 10) then Result := 0 else
    case Input of
      6,
      7,
      8: Result := 0;
      9: Result := 1;
    else
      Result := Input;
    end;
  end;

var
  Sanitized: string;
  InputCode, OutputCode: Integer;
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

  // Set the input code
  InputCode := StrToInt(Sanitized);

  // Calculate the code
  OutputCode := 1111;

  SNArr[0] := (InputCode div 1000) and $0F;
  SNArr[1] := (InputCode mod 1000) div 100 and $0F;
  SNArr[2] := (InputCode mod 100) div 10 and $0F;
  SNArr[3] := InputCode mod 10 and $0F;

  OutputCode := OutputCode + GetThirdByte(SNArr[3]) * 10;
  OutputCode := OutputCode + GetFirstByte(SNArr[2]) * 1000;
  OutputCode := OutputCode + GetFourthByte(SNArr[1]);
  OutputCode := OutputCode + GetSecondByte(SNArr[0]) * 100;

  // Format the code for the output
  Output := Format('%.*d', [4, OutputCode]);
end;

end.

