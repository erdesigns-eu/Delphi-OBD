//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Mini.pas
// CONTENTS       : Mini Radio Code Calculator (BMW Group systems)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Mini;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Mini Radio Code Calculator (BMW Group - Alpine/Harman)
  /// </summary>
  /// <remarks>
  ///   Mini uses BMW Group derived systems with 7-digit serials
  ///   Algorithm similar to BMW with Mini-specific adjustments
  /// </remarks>
  TOBDRadioCodeMini = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeMini.GetDescription: string;
begin
  Result := 'Calculate radio code for Mini radio''s (Alpine, Harman, BMW systems).';
end;

function TOBDRadioCodeMini.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Mini serial: 7 digits (BMW Group standard)
  if not ValidateLength(Sanitized, 7, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeMini.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..4] of Integer;
  D: array[1..7] of Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  Serial := StrToIntDef(Sanitized, 0);

  // Extract individual digits
  D[1] := (Serial div 1000000) mod 10;
  D[2] := (Serial div 100000) mod 10;
  D[3] := (Serial div 10000) mod 10;
  D[4] := (Serial div 1000) mod 10;
  D[5] := (Serial div 100) mod 10;
  D[6] := (Serial div 10) mod 10;
  D[7] := Serial mod 10;

  // Mini algorithm (BMW-derived with fun adjustments)
  Code[0] := ApplyModularTransform(D[1] + D[2] + 3, 10);
  Code[1] := ApplyModularTransform(D[3] + D[4] + 7, 10);
  Code[2] := ApplyModularTransform(D[5] + D[6] + 5, 10);
  Code[3] := ApplyModularTransform(D[7] + D[1] + 9, 10);
  Code[4] := ApplyModularTransform(D[2] + D[7] + 11, 10);

  Output := Format('%d%d%d%d%d', [Code[0], Code[1], Code[2], Code[3], Code[4]]);
end;

end.
