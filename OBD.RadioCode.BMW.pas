//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.BMW.pas
// CONTENTS       : BMW Radio Code Calculator
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.BMW;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   BMW Radio Code Calculator (Business CD/Radio, Professional)
  /// </summary>
  TOBDRadioCodeBMW = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeBMW.GetDescription: string;
begin
  Result := 'Calculate radio code for BMW radio''s (Business CD/Radio, Professional).';
end;

function TOBDRadioCodeBMW.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // BMW serial: 7 digits
  if not ValidateLength(Sanitized, 7, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeBMW.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..4] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  Serial := StrToInt(Sanitized);

  // BMW algorithm - generates 5 digit code
  Code[0] := ApplyModularTransform((Serial div 10000) + 1, 10);
  Code[1] := ApplyModularTransform((Serial div 1000) * 3 + 7, 10);
  Code[2] := ApplyModularTransform((Serial div 100) + 5, 10);
  Code[3] := ApplyModularTransform((Serial div 10) * 2 + 3, 10);
  Code[4] := ApplyModularTransform(Serial + 9, 10);

  Output := Format('%d%d%d%d%d', [Code[0], Code[1], Code[2], Code[3], Code[4]]);
end;

end.
