//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Nissan.pas
// CONTENTS       : Nissan Radio Code Calculator (BP/Clarion Series)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.Nissan;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Nissan BP/Clarion Radio Code Calculator
  /// </summary>
  TOBDRadioCodeNissan = class(TOBDRadioCode)
  public
    /// <summary>
    ///   Get description for this calculator
    /// </summary>
    function GetDescription: string; override;
    /// <summary>
    ///   Radio Code input validator
    /// </summary>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    /// <summary>
    ///   Radio Code Calculator
    /// </summary>
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

//------------------------------------------------------------------------------
// GET DESCRIPTION
//------------------------------------------------------------------------------
function TOBDRadioCodeNissan.GetDescription: string;
begin
  Result := 'Calculate radio code for Nissan BP/Clarion radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeNissan.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Nissan codes are typically 4 digits
  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);

  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeNissan.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  D1, D2, D3, D4: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  // Extract digits
  D1 := StrToInt(Sanitized[1]);
  D2 := StrToInt(Sanitized[2]);
  D3 := StrToInt(Sanitized[3]);
  D4 := StrToInt(Sanitized[4]);

  // Nissan BP algorithm
  Code[0] := ApplyModularTransform((D1 + 3) * 7, 10);
  Code[1] := ApplyModularTransform((D2 + 5) * 3, 10);
  Code[2] := ApplyModularTransform((D3 + 7) * 5, 10);
  Code[3] := ApplyModularTransform((D4 + 1) * 9, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
