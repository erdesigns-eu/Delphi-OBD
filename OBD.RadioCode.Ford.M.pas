//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Ford.M.pas
// CONTENTS       : Ford Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 10/04/2024
//------------------------------------------------------------------------------
unit OBD.RadioCode.Ford.M;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Ford M RadioCode Calculator
  /// </summary>
  TOBDRadioCodeFordM = class(TOBDRadioCode)
  private const
    /// <summary>
    ///   Lookup table for calculating the code.
    /// </summary>
    Lookup: array[0..9, 0..9] of Integer = (
      (9, 5, 3, 4, 8, 7, 2, 6, 1, 0),
      (2, 1, 5, 6, 9, 3, 7, 0, 4, 8),
      (0, 4, 7, 3, 1, 9, 6, 5, 8, 2),
      (5, 6, 4, 1, 2, 8, 0, 9, 3, 7),
      (6, 3, 1, 2, 0, 5, 4, 8, 7, 9),
      (4, 0, 8, 7, 6, 1, 9, 3, 2, 5),
      (7, 8, 0, 5, 3, 2, 1, 4, 9, 6),
      (1, 9, 6, 8, 7, 4, 5, 2, 0, 3),
      (3, 2, 9, 0, 4, 6, 8, 7, 5, 1),
      (8, 7, 2, 9, 5, 0, 3, 1, 6, 4)
    );
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

uses System.StrUtils;

//------------------------------------------------------------------------------
// GET DESCRIPTION
//------------------------------------------------------------------------------
function TOBDRadioCodeFordM.GetDescription: string;
begin
  Result := 'Calculate radio code for Ford M radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeFordM.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  S: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';

  // Remove the leading V (optional)
  S := IfThen(Input.StartsWith('M', True), Copy(Input, 2, length(Input) - 1), Input);

  // Make sure the input is 5 characters long
  if not (Length(Input) = 5) then
  begin
    ErrorMessage := 'Must be 5 characters long!';
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

  // Make sure the fifth character is a digit
  if not (CharInSet(Input[4], ['0'..'9'])) then
  begin
    ErrorMessage := 'Fifth character must be a digit!';
    Exit(False);
  end;
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeFordM.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  S: string;
  N: array[0..5] of Integer;
  N1, N2, N3, N4, N5, N6, N7: Integer;
  R1, R2, R3, R4, R5, R6, R7: Integer;
  Res1, Res2, Res3, Res4: Integer;
  XRes1, XRes2, XRes3, XRes4: Integer;
  XRes10, XRes11, XRes20, XRes21, XRes30, XRes31, XRes40, XRes41: Integer;
begin
  // Initialize result
  Result := True;
  // Clear the output
  Output := '';
  // Clear the error message
  ErrorMessage := '';

  // Check if the input is valid
  if not Self.Validate(Input, ErrorMessage) then Exit(False);

  // Remove the leading V (optional)
  S := IfThen(Input.StartsWith('M', True), Copy(Input, 2, length(Input) - 1), Input);

  // Fill the N array
  N[0] := StrToInt(S[6]);
  N[1] := StrToInt(S[5]);
  N[2] := StrToInt(S[4]);
  N[3] := StrToInt(S[3]);
  N[4] := StrToInt(S[2]);
  N[5] := StrToInt(S[1]);

  // Set the N values
  N1 := N[0];
  N2 := N[1];
  N3 := N[2];
  N4 := N[3];
  N5 := N[4];
  N6 := N[5];
  N7 := 0;

  // Set the R values
  R1 := Lookup[N1, 5];
  R2 := Lookup[N2, 3];
  R3 := Lookup[N3, 8];
  R4 := Lookup[N4, 2];
  R5 := Lookup[N5, 1];
  R6 := Lookup[N6, 6];
  R7 := Lookup[N7, 9];

  // Calculate the code
  Res1 := ((Lookup[R2, R1] + 1) * (Lookup[R6, R2] + 1) + (Lookup[R4, R3] + 1) * (Lookup[R7, R5] + 1) + (Lookup[R1, R4])) mod 10;
  Res2 := ((Lookup[R2, R1] + 1) * (Lookup[R5, R4] + 1) + (Lookup[R5, R2] + 1) * (Lookup[R7, R3] + 1) + (Lookup[R1, R6])) mod 10;
  Res3 := ((Lookup[R2, R1] + 1) * (Lookup[R4, R2] + 1) + (Lookup[R3, R6] + 1) * (Lookup[R7, R4] + 1) + (Lookup[R1, R5])) mod 10;
  Res4 := ((Lookup[R2, R1] + 1) * (Lookup[R6, R3] + 1) + (Lookup[R3, R7] + 1) * (Lookup[R2, R5] + 1) + (Lookup[R4, R1])) mod 10;

  XRes1 := (Lookup[Res1, 5] + 1) * (Lookup[Res2, 1] + 1) + 105;
  XRes2 := (Lookup[Res2, 1] + 1) * (Lookup[Res4, 0] + 1) + 102;
  XRes3 := (Lookup[Res1, 5] + 1) * (Lookup[Res3, 8] + 1) + 103;
  XRes4 := (Lookup[Res3, 8] + 1) * (Lookup[Res4, 0] + 1) + 108;

  XRes11 := (XRes1 div 10) mod 10;
  XRes10 := (XRes1 mod 10);

  XRes21 := (XRes2 div 10) mod 10;
  XRes20 := (XRes2 mod 10);

  XRes31 := (XRes3 div 10) mod 10;
  XRes30 := (XRes3 mod 10);

  XRes41 := (XRes4 div 10) mod 10;
  XRes40 := (XRes4 mod 10);

  // Format the code for the output
  Output := Format('%d%d%d%d', [
    (XRes41 + XRes40 + R1) mod 10,
    (XRes31 + XRes30 + R1) mod 10,
    (XRes21 + XRes20 + R1) mod 10,
    (XRes11 + XRes10 + R1) mod 10
  ]);
end;

end.

