//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.LandRover.pas
// CONTENTS       : Land Rover Radio Code Calculator (Visteon/Harman systems)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.LandRover;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Land Rover Radio Code Calculator (Visteon, Harman)
  /// </summary>
  /// <remarks>
  ///   Land Rover/Jaguar use similar systems with alphanumeric serials
  ///   Format: 14 characters (LR + 12 alphanumeric)
  /// </remarks>
  TOBDRadioCodeLandRover = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeLandRover.GetDescription: string;
begin
  Result := 'Calculate radio code for Land Rover/Jaguar radio''s (Visteon, Harman, Alpine).';
end;

function TOBDRadioCodeLandRover.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Land Rover serial: 14 characters (LR + 12 alphanumeric)
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // Should start with 'LR' or 'JG' (Jaguar)
  if not ((Copy(Sanitized, 1, 2) = 'LR') or (Copy(Sanitized, 1, 2) = 'JG')) then
  begin
    ErrorMessage := 'Serial must start with LR (Land Rover) or JG (Jaguar)';
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

function TOBDRadioCodeLandRover.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
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

  // Extract alphanumeric part (skip LR/JG prefix)
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

  // Land Rover algorithm (premium encoding)
  Code[0] := ApplyModularTransform(Sum div 100 + 7, 10);
  Code[1] := ApplyModularTransform(Sum div 10 + 5, 10);
  Code[2] := ApplyModularTransform(Sum + 9, 10);
  Code[3] := ApplyModularTransform(Sum * 3 + 11, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
