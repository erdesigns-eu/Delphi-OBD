//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.BMW.pas
// CONTENTS       : BMW FA (vehicle order) + I-Stufe parsers
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The FA (Fahrzeugauftrag, "vehicle order") is a
//                  comma-separated list of option codes — each token
//                  is a 3-4-character SALAPA code that flags an
//                  installed feature. E-Sys reads the FA from CAS
//                  and pushes it to every coding-aware module.
//                  I-Stufe (Integrationsstufe, "integration level")
//                  is a versioning triplet — Project + Build week +
//                  patch — that gates which CAFD files E-Sys writes.
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.BMW;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  OBD.OEM.Coding;

type
  /// <summary>
  ///   Mutable BMW FA / vehicle-order string. The wire format is
  ///   <c>"205E,8FA,255,2VB,2VL,4U6"</c> — a comma-separated set of
  ///   SALAPA option codes. The class normalises whitespace, sorts
  ///   tokens lexicographically on serialise, and de-duplicates on
  ///   add (so a repeated <c>AddOption</c> is a no-op).
  /// </summary>
  TOBDBMWFA = class
  strict private
    FOptions: TList<string>;
    function NormalizeCode(const Code: string): string;
  public
    constructor Create; overload;
    constructor Create(const FAString: string); overload;
    destructor Destroy; override;

    function HasOption(const Code: string): Boolean;
    procedure AddOption(const Code: string);
    procedure RemoveOption(const Code: string);
    procedure Clear;

    /// <summary>Number of distinct options in the order.</summary>
    function Count: Integer;

    /// <summary>Snapshot the option list (sorted ascending).</summary>
    function Tokens: TArray<string>;

    /// <summary>Render in the canonical comma-separated form. Tokens
    /// are sorted ascending so equal orders always produce equal
    /// strings — easy to diff in audit logs.</summary>
    function ToString: string; reintroduce;
  end;

  /// <summary>
  ///   I-Stufe — BMW's integration-level versioning. Wire format is
  ///   <c>"&lt;Project&gt;-&lt;YY&gt;-&lt;MM&gt;-&lt;Build&gt;"</c>:
  ///   <c>F020-21-03-630</c> = F020 (1-Series), week 21 of 2003 base,
  ///   build 630 (the actual flash). Newer = higher build at same
  ///   project/year/month, then higher month, then higher year.
  /// </summary>
  TOBDBMWIStufe = record
    Project: string;     // 'F020', 'G30', 'I20'
    Year: Byte;          // YY (00..99)
    Month: Byte;         // MM (1..12)
    Build: Word;         // 1..9999

    /// <summary>Parse a wire-format I-Stufe; throws on malformed input.</summary>
    class function Parse(const S: string): TOBDBMWIStufe; static;

    /// <summary>Render in canonical form.</summary>
    function ToString: string;

    /// <summary>Lexicographic comparison: returns -1 / 0 / 1 like
    /// <c>CompareStr</c>. Project is the primary key; ties break on
    /// Year, then Month, then Build.</summary>
    function CompareTo(const Other: TOBDBMWIStufe): Integer;

    /// <summary>True if this I-Stufe is at least as new as <c>Other</c>
    /// for the same project. Cross-project comparison is undefined and
    /// returns False (the caller should never compare an F-series to
    /// a G-series I-Stufe).</summary>
    function AtLeast(const Other: TOBDBMWIStufe): Boolean;
  end;

implementation

uses
  System.Classes;

//==============================================================================
// TOBDBMWFA
//==============================================================================
constructor TOBDBMWFA.Create;
begin
  inherited Create;
  FOptions := TList<string>.Create;
end;

constructor TOBDBMWFA.Create(const FAString: string);
var
  Parts: TArray<string>;
  Token: string;
begin
  Create;
  // Accept comma-separated, whitespace-separated, or mixed input —
  // E-Sys exports vary, and ISTA's "FA short form" uses hyphens.
  Parts := FAString.Split([',', ';', ' ', #9, #13, #10],
    TStringSplitOptions.ExcludeEmpty);
  for Token in Parts do
    AddOption(Token);
end;

destructor TOBDBMWFA.Destroy;
begin
  FOptions.Free;
  inherited;
end;

function TOBDBMWFA.NormalizeCode(const Code: string): string;
begin
  Result := UpperCase(Trim(Code));
  if Result = '' then
    raise EOBDCodingError.Create('FA option code cannot be empty');
end;

function TOBDBMWFA.HasOption(const Code: string): Boolean;
var
  Norm, Existing: string;
begin
  Norm := NormalizeCode(Code);
  for Existing in FOptions do
    if Existing = Norm then Exit(True);
  Result := False;
end;

procedure TOBDBMWFA.AddOption(const Code: string);
var
  Norm: string;
begin
  Norm := NormalizeCode(Code);
  if not FOptions.Contains(Norm) then FOptions.Add(Norm);
end;

procedure TOBDBMWFA.RemoveOption(const Code: string);
begin
  FOptions.Remove(NormalizeCode(Code));
end;

procedure TOBDBMWFA.Clear;
begin FOptions.Clear; end;

function TOBDBMWFA.Count: Integer;
begin Result := FOptions.Count; end;

function TOBDBMWFA.Tokens: TArray<string>;
begin
  Result := FOptions.ToArray;
  TArray.Sort<string>(Result, TComparer<string>.Default);
end;

function TOBDBMWFA.ToString: string;
begin
  Result := string.Join(',', Tokens);
end;

//==============================================================================
// TOBDBMWIStufe
//==============================================================================
class function TOBDBMWIStufe.Parse(const S: string): TOBDBMWIStufe;
var
  Parts: TArray<string>;
  Trimmed: string;
begin
  Trimmed := UpperCase(Trim(S));
  Parts := Trimmed.Split(['-']);
  if Length(Parts) <> 4 then
    raise EOBDCodingError.CreateFmt(
      'I-Stufe must have 4 dash-separated parts, got "%s"', [S]);

  Result.Project := Parts[0];
  if Result.Project = '' then
    raise EOBDCodingError.Create('I-Stufe project must not be empty');

  if not TryStrToInt(Parts[1], Integer(Result.Year)) or
     (Result.Year > 99) then
    raise EOBDCodingError.CreateFmt(
      'I-Stufe year must be 00..99: "%s"', [Parts[1]]);
  if not TryStrToInt(Parts[2], Integer(Result.Month)) or
     (Result.Month < 1) or (Result.Month > 12) then
    raise EOBDCodingError.CreateFmt(
      'I-Stufe month must be 01..12: "%s"', [Parts[2]]);
  if not TryStrToInt(Parts[3], Integer(Result.Build)) or
     (Result.Build = 0) then
    raise EOBDCodingError.CreateFmt(
      'I-Stufe build must be a positive integer: "%s"', [Parts[3]]);
end;

function TOBDBMWIStufe.ToString: string;
begin
  Result := Format('%s-%.2d-%.2d-%.3d', [Project, Year, Month, Build]);
end;

function TOBDBMWIStufe.CompareTo(const Other: TOBDBMWIStufe): Integer;
begin
  Result := CompareStr(Project, Other.Project);
  if Result <> 0 then Exit;
  Result := Integer(Year) - Integer(Other.Year);
  if Result <> 0 then Exit;
  Result := Integer(Month) - Integer(Other.Month);
  if Result <> 0 then Exit;
  Result := Integer(Build) - Integer(Other.Build);
end;

function TOBDBMWIStufe.AtLeast(const Other: TOBDBMWIStufe): Boolean;
begin
  if not SameText(Project, Other.Project) then Exit(False);
  Result := CompareTo(Other) >= 0;
end;

end.
