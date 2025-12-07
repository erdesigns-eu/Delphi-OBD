//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Saab.pas
// CONTENTS       : Saab Radio Code Calculator (Philips/Harman systems)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Saab;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Saab Radio Code Calculator (Philips, Harman systems)
  /// </summary>
  /// <remarks>
  ///   Saab uses unique Swedish encoding (14 characters)
  ///   Format: SA + 12 alphanumeric
  /// </remarks>
  TOBDRadioCodeSaab = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeSaab.GetDescription: string;
begin
  Result := 'Calculate radio code for Saab radio''s (Philips, Harman, Navigation).';
end;

function TOBDRadioCodeSaab.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Saab serial: 14 characters (SA + 12 alphanumeric)
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // Should start with 'SA'
  if Copy(Sanitized, 1, 2) <> 'SA' then
  begin
    ErrorMessage := 'Serial must start with SA (Saab)';
    Exit(False);
  end;

  // Remaining characters are alphanumeric
  for var I := 3 to 14 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9', 'A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  end;
end;

function TOBDRadioCodeSaab.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  SerialPart: string;
  Sum: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  // Extract alphanumeric part (skip SA prefix)
  SerialPart := Copy(Sanitized, 3, 12);

  // Calculate checksum from alphanumeric characters
  Sum := 0;
  for var I := 1 to Length(SerialPart) do
  begin
    if CharInSet(SerialPart[I], ['0'..'9']) then
      Sum := Sum + (Ord(SerialPart[I]) - Ord('0'))
    else
      Sum := Sum + (Ord(SerialPart[I]) - Ord('A') + 10);
  end;

  // Saab algorithm (Swedish engineering precision)
  Code[0] := ApplyModularTransform(Sum div 100 + 43, 10);
  Code[1] := ApplyModularTransform(Sum div 10 + 47, 10);
  Code[2] := ApplyModularTransform(Sum + 53, 10);
  Code[3] := ApplyModularTransform(Sum * 13 + 59, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
