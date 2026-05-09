//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Becker5.pas
// CONTENTS       : Becker Radio Code Calculator (5 Digits)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 13/04/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.RadioCode.Becker5;

interface

uses
  System.SysUtils,

  OBD.RadioCode;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Becker RadioCode Calculator (5 Digits). The serial-to-code
  ///   table (10,000 entries) is loaded from catalogs/radiocode-becker5.json
  ///   at unit init so a corrected entry can be shipped without recompiling.
  /// </summary>
  TOBDRadioCodeBecker5 = class(TOBDRadioCode)
  public
    /// <summary>
    ///   Get description.
    /// </summary>
    function GetDescription: string; override;
    /// <summary>
    ///   Validate.
    /// </summary>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    /// <summary>
    ///   Calculate.
    /// </summary>
    function Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean; override;
  end;

implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

const
  CatalogFileName = 'radiocode-becker5.json';
  TableSize = 10000;

var
  GDatabase: array[0..TableSize - 1] of string;
  GLoaded: Boolean = False;

//------------------------------------------------------------------------------
// LOAD CATALOG
//------------------------------------------------------------------------------
procedure LoadCatalog;
var
  Path, Raw: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  I: Integer;
begin
  Path := ResolveCatalogPath(CatalogFileName);
  // Bail if catalog path is missing
  if Path = '' then Exit;
  // Create stream
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    // Load file into stream
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    // Free the stream
    Stream.Free;
  end;
  // Parse JSON document
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('codes');
    if (Arr = nil) or (Arr.Count <> TableSize) then Exit;
    for I := 0 to TableSize - 1 do
      GDatabase[I] := Arr.Items[I].Value;
    GLoaded := True;
  finally
    // Free the document
    Doc.Free;
  end;
end;

//------------------------------------------------------------------------------
// GET DESCRIPTION
//------------------------------------------------------------------------------
function TOBDRadioCodeBecker5.GetDescription: string;
begin
  Result := 'Calculate radio code for Becker (5 Digits) radio''s.';
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodeBecker5.Validate(const Input: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  // Initialize result
  Result := True;
  // Clear the error message
  ErrorMessage := '';
  // Sanitize the input
  Sanitized := SanitizeInput(Input);
  if not ValidateLength(Sanitized, 4, ErrorMessage) then Exit(False);
  if not ValidateDigits(Sanitized, ErrorMessage) then Exit(False);
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodeBecker5.Calculate(const Input: string; var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  I: Integer;
begin
  // Initialize result
  Result := True;
  // Clear the output
  Output := '';
  // Clear the error message
  ErrorMessage := '';
  // Sanitize the input
  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);
  if not GLoaded then
  begin
    ErrorMessage := 'Becker5 code catalog not loaded; expected '
                  + 'catalogs/' + CatalogFileName;
    Exit(False);
  end;
  I := StrToInt(Sanitized);
  Output := GDatabase[I];
end;

initialization
  LoadCatalog;

end.
