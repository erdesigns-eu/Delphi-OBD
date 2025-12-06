//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.AlfaRomeo.pas
// CONTENTS       : Alfa Romeo Radio Code Calculator (Fiat Group systems)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.RadioCode.AlfaRomeo;

interface

uses
  WinApi.Windows, System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Alfa Romeo Radio Code Calculator (Blaupunkt/Grundig/Continental)
  /// </summary>
  /// <remarks>
  ///   Alfa Romeo uses Fiat Group derived systems
  ///   Multiple formats: Blaupunkt (letter + 6 digits), Grundig (4 digits)
  /// </remarks>
  TOBDRadioCodeAlfaRomeo = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeAlfaRomeo.GetDescription: string;
begin
  Result := 'Calculate radio code for Alfa Romeo radio''s (Blaupunkt, Grundig, Continental).';
end;

function TOBDRadioCodeAlfaRomeo.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);

  // Support two formats: 4 digits (Grundig) or 7 chars (letter + 6 digits, Blaupunkt)
  if (Length(Sanitized) <> 4) and (Length(Sanitized) <> 7) then
  begin
    ErrorMessage := 'Serial must be 4 digits (Grundig) or 7 characters (Blaupunkt: letter + 6 digits)';
    Exit(False);
  end;

  if Length(Sanitized) = 4 then
  begin
    if not ValidateDigits(Sanitized, ErrorMessage) then
      Exit(False);
  end
  else // Length = 7
  begin
    if not CharInSet(Sanitized[1], ['A'..'Z']) then
    begin
      ErrorMessage := 'First character must be a letter for Blaupunkt models';
      Exit(False);
    end;

    for var I := 2 to 7 do
    begin
      if not CharInSet(Sanitized[I], ['0'..'9']) then
      begin
        ErrorMessage := Format('Character %d must be a digit', [I]);
        Exit(False);
      end;
    end;
  end;
end;

function TOBDRadioCodeAlfaRomeo.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
  LetterValue: Integer;
  D1, D2, D3, D4: Integer;
begin
  Result := True;
  ErrorMessage := '';
  Output := '';

  Sanitized := SanitizeInput(Input);

  if not Validate(Sanitized, ErrorMessage) then
    Exit(False);

  if Length(Sanitized) = 4 then
  begin
    // Grundig format (4 digits)
    Serial := StrToIntDef(Sanitized, 0);
    D1 := (Serial div 1000) mod 10;
    D2 := (Serial div 100) mod 10;
    D3 := (Serial div 10) mod 10;
    D4 := Serial mod 10;

    Code[0] := ApplyModularTransform(D1 + D2 + 5, 10);
    Code[1] := ApplyModularTransform(D2 + D3 + 7, 10);
    Code[2] := ApplyModularTransform(D3 + D4 + 3, 10);
    Code[3] := ApplyModularTransform(D1 + D4 + 9, 10);
  end
  else
  begin
    // Blaupunkt format (letter + 6 digits)
    LetterValue := Ord(Sanitized[1]) - Ord('A') + 1;
    Serial := StrToIntDef(Copy(Sanitized, 2, 6), 0);

    Code[0] := ApplyModularTransform((Serial div 10000) + LetterValue, 10);
    Code[1] := ApplyModularTransform((Serial div 100) mod 100 + LetterValue * 2, 10);
    Code[2] := ApplyModularTransform((Serial mod 100) + LetterValue * 3, 10);
    Code[3] := ApplyModularTransform((Serial mod 10) + LetterValue * 5, 10);
  end;

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
