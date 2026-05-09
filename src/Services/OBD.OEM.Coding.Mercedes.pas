//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.Mercedes.pas
// CONTENTS       : Mercedes SCN (Standard-Codierung-Nummer) parser
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The SCN is the master coding string XENTRY pushes
//                  to a Mercedes ECU after a flash. Wire format is
//                  one of:
//                    "0011223344-XYZ-A1B2C3"   (ASCII canonical)
//                  Decoding the meaning of each segment is FIN-keyed
//                  and lives inside XENTRY's per-vehicle data files
//                  (NDA-protected). The codec models the SCN as an
//                  opaque structured string — caller-supplied lookup
//                  tables resolve segment meanings.
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.Mercedes;

interface

uses
  System.SysUtils, OBD.OEM.Coding;

type
  /// <summary>
  ///   Structured view of an SCN string. The framework treats the
  ///   string as opaque and only validates the structure (3 segments,
  ///   each non-empty, alphanumeric). Production users overlay
  ///   per-segment semantics from their own data files.
  /// </summary>
  TOBDMercedesSCN = record
    /// <summary>
    ///   Hardware/dataset segment — typically a 10-digit
    ///   numeric prefix that identifies the ECU dataset.
    /// </summary>
    HardwareSegment: string;
    /// <summary>
    ///   Project / variant marker — 3-4 alphanumerics, e.g.
    ///   "212" for W212, "204" for W204.
    /// </summary>
    ProjectSegment: string;
    /// <summary>
    ///   Build / patch marker — 5-6 alphanumerics for the
    ///   specific calibration revision.
    /// </summary>
    BuildSegment: string;

    /// <summary>
    ///   Parse a wire-format SCN; raises on a malformed input.
    /// </summary>
    class function Parse(const S: string): TOBDMercedesSCN; static;

    /// <summary>
    ///   True if any segment is non-empty.
    /// </summary>
    function IsValid: Boolean;

    /// <summary>
    ///   Render in the canonical hyphenated form.
    /// </summary>
    function ToString: string;
  end;

implementation

const
  ALLOWED_CHARS = ['0'..'9', 'A'..'Z'];

//------------------------------------------------------------------------------
// NORMALIZE SEGMENT
//------------------------------------------------------------------------------
function NormalizeSegment(const S: string): string;
var
  C: Char;
begin
  Result := UpperCase(Trim(S));
  if Result = '' then
    raise EOBDCodingError.Create('SCN segment must not be empty');
  for C in Result do
    if not CharInSet(C, ALLOWED_CHARS) then
      raise EOBDCodingError.CreateFmt(
        'SCN segment contains illegal character "%s" in "%s"', [C, S]);
end;

//------------------------------------------------------------------------------
// PARSE
//------------------------------------------------------------------------------
class function TOBDMercedesSCN.Parse(const S: string): TOBDMercedesSCN;
var
  Parts: TArray<string>;
  Trimmed: string;
begin
  Trimmed := Trim(S);
  Parts := Trimmed.Split(['-', '/', ' '], TStringSplitOptions.ExcludeEmpty);
  if Length(Parts) <> 3 then
    raise EOBDCodingError.CreateFmt(
      'SCN must have 3 segments separated by "-", got "%s"', [S]);
  Result.HardwareSegment := NormalizeSegment(Parts[0]);
  Result.ProjectSegment  := NormalizeSegment(Parts[1]);
  Result.BuildSegment    := NormalizeSegment(Parts[2]);
end;

//------------------------------------------------------------------------------
// IS VALID
//------------------------------------------------------------------------------
function TOBDMercedesSCN.IsValid: Boolean;
begin
  Result := (HardwareSegment <> '') and
            (ProjectSegment  <> '') and
            (BuildSegment    <> '');
end;

//------------------------------------------------------------------------------
// TO STRING
//------------------------------------------------------------------------------
function TOBDMercedesSCN.ToString: string;
begin
  Result := Format('%s-%s-%s',
    [HardwareSegment, ProjectSegment, BuildSegment]);
end;

end.
