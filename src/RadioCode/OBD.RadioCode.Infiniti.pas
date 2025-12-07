//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Infiniti.pas
// CONTENTS       : Infiniti Radio Code Calculator (Nissan-derived systems)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Infiniti;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Infiniti Radio Code Calculator (Clarion, Bose Premium)
  /// </summary>
  /// <remarks>
  ///   Infiniti uses Nissan-derived systems with 4-digit serials
  ///   Premium Bose systems may require special handling
  /// </remarks>
  TOBDRadioCodeInfiniti = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeInfiniti.GetDescription: string;
begin
  Result := 'Calculate radio code for Infiniti radio''s (Clarion, Bose, Navigation systems).';
end;

function TOBDRadioCodeInfiniti.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Infiniti serial: 4 digits
  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeInfiniti.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
  D1, D2, D3, D4: Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  Serial := StrToIntDef(Sanitized, 0);

  // Extract individual digits
  D1 := (Serial div 1000) mod 10;
  D2 := (Serial div 100) mod 10;
  D3 := (Serial div 10) mod 10;
  D4 := Serial mod 10;

  // Infiniti algorithm (Nissan-derived with premium adjustments)
  Code[0] := ApplyModularTransform(D1 + D2 + 8, 10);
  Code[1] := ApplyModularTransform(D2 + D3 + 6, 10);
  Code[2] := ApplyModularTransform(D3 + D4 + 4, 10);
  Code[3] := ApplyModularTransform(D1 + D4 + 9, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
