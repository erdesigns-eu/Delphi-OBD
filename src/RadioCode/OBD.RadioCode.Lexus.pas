//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Lexus.pas
// CONTENTS       : Lexus Radio Code Calculator (Toyota-derived systems)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Lexus;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Lexus Radio Code Calculator (Fujitsu Ten, Denso, Panasonic)
  /// </summary>
  /// <remarks>
  ///   Lexus uses Toyota-derived systems with 5-digit serials
  ///   Premium models may use different encoding
  /// </remarks>
  TOBDRadioCodeLexus = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeLexus.GetDescription: string;
begin
  Result := 'Calculate radio code for Lexus radio''s (Fujitsu Ten, Denso, Panasonic, Navigation).';
end;

function TOBDRadioCodeLexus.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Lexus serial: 5 digits
  if not ValidateLength(Sanitized, 5, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeLexus.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
  D1, D2, D3, D4, D5: Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  Serial := StrToIntDef(Sanitized, 0);

  // Extract individual digits
  D1 := (Serial div 10000) mod 10;
  D2 := (Serial div 1000) mod 10;
  D3 := (Serial div 100) mod 10;
  D4 := (Serial div 10) mod 10;
  D5 := Serial mod 10;

  // Lexus algorithm (Toyota-derived with premium encoding)
  Code[0] := ApplyModularTransform(D1 + D2 + 7, 10);
  Code[1] := ApplyModularTransform(D2 + D3 + 5, 10);
  Code[2] := ApplyModularTransform(D3 + D4 + 9, 10);
  Code[3] := ApplyModularTransform(D4 + D5 + 3, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
