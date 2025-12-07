//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Smart.pas
// CONTENTS       : Smart Radio Code Calculator (Mercedes-derived systems)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Smart;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Smart Radio Code Calculator (Mercedes-Benz derived systems)
  /// </summary>
  /// <remarks>
  ///   Smart uses Mercedes-Benz Group systems (14 character alphanumeric)
  ///   Algorithm similar to Mercedes with Smart-specific adjustments
  /// </remarks>
  TOBDRadioCodeSmart = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeSmart.GetDescription: string;
begin
  Result := 'Calculate radio code for Smart radio''s (Audio 5/10/15, Becker, Navigation).';
end;

function TOBDRadioCodeSmart.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Smart serial: 14 characters alphanumeric (Mercedes format)
  if not ValidateLength(Sanitized, 14, ErrorMessage) then
    Exit(False);

  // All characters must be alphanumeric
  for var I := 1 to 14 do
  begin
    if not CharInSet(Sanitized[I], ['0'..'9', 'A'..'Z']) then
    begin
      ErrorMessage := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  end;
end;

function TOBDRadioCodeSmart.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Sum: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  // Calculate checksum from all alphanumeric characters
  Sum := 0;
  for var I := 1 to Length(Sanitized) do
  begin
    if CharInSet(Sanitized[I], ['0'..'9']) then
      Sum := Sum + (Ord(Sanitized[I]) - Ord('0'))
    else
      Sum := Sum + (Ord(Sanitized[I]) - Ord('A') + 10);
  end;

  // Smart algorithm (Mercedes-derived with compact encoding)
  Code[0] := ApplyModularTransform(Sum div 100 + 4, 10);
  Code[1] := ApplyModularTransform(Sum div 10 + 8, 10);
  Code[2] := ApplyModularTransform(Sum + 6, 10);
  Code[3] := ApplyModularTransform(Sum * 2 + 12, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
