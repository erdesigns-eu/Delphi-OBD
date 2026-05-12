//------------------------------------------------------------------------------
//  OBD.OEM.DTC
//
//  TOBDDtcCatalog — DTC code → human-readable entry lookup
//  bound to one OEM extension. Hosts populate the catalogue
//  through <c>RegisterEntry</c>, optionally seeding from the
//  shipped <c>catalogs/dtc-iso-15031.json</c> generic P0xxx
//  baseline overlay.
//
//  Reentrant; safe to call from any thread.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//    2026-05-12  ERD  LoadFromFile reads every catalogue field
//                     (severity, possible_causes, symptoms,
//                     repair_guidance, monitor_type,
//                     freeze_frame_relevant, related_dids,
//                     related_routines, oem_bulletin, source,
//                     verified) plus file-level default_source.
//                     Added the J2012 helper functions
//                     (FormatDtc / EncodeDtc / ParseDtcSystem /
//                     IsManufacturerDtc / ParseSeverity /
//                     ParseMonitorType + format inverses) and
//                     EOBDDtcError.
//------------------------------------------------------------------------------

unit OBD.OEM.DTC;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.OEM.Types;

type
  /// <summary>
  ///   Per-OEM DTC catalogue.
  /// </summary>
  TOBDDtcCatalog = class
  strict private
    FLock: TCriticalSection;
    FEntries: TDictionary<string, TOBDDtcCatalogEntry>;
    function Normalise(const ACode: string): string;
  public
    /// <summary>Constructs an empty catalogue.</summary>
    constructor Create;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>Registers (or replaces) one DTC entry.</summary>
    /// <param name="AEntry">Entry to add. <c>Code</c> is
    /// normalised to uppercase before storage.</param>
    procedure RegisterEntry(const AEntry: TOBDDtcCatalogEntry);

    /// <summary>Looks up <c>ACode</c>.</summary>
    /// <param name="ACode">5-character DTC code.</param>
    /// <param name="AEntry">Out: matching entry on success.</param>
    /// <returns><c>True</c> when the code is catalogued.</returns>
    function TryFind(const ACode: string;
      out AEntry: TOBDDtcCatalogEntry): Boolean;

    /// <summary>Alias of <see cref="TryFind"/> using the
    /// catalogue-style parameter names that vendor extensions
    /// pass through from JSON loaders.</summary>
    /// <param name="Code">5-character DTC code.</param>
    /// <param name="Entry">Out: matching entry on success.</param>
    function FindByCode(const Code: string;
      out Entry: TOBDDtcCatalogEntry): Boolean;

    /// <summary>Removes a code from the catalogue.</summary>
    /// <param name="ACode">5-character DTC code.</param>
    procedure Remove(const ACode: string);

    /// <summary>Number of catalogued codes.</summary>
    function Count: Integer;

    /// <summary>Drops every entry.</summary>
    procedure Clear;

    /// <summary>Snapshot of every entry.</summary>
    /// <returns>Array of entries in arbitrary order.</returns>
    function Snapshot: TArray<TOBDDtcCatalogEntry>;

    /// <summary>
    ///   Merges entries from a JSON catalogue file. The file is a
    ///   top-level object with an optional <c>default_source</c>
    ///   string and a <c>dtcs</c> array of entry objects. Every
    ///   field in the v2 DTC schema (severity, possible_causes,
    ///   verified, symptoms, repair_guidance, monitor_type,
    ///   freeze_frame_relevant, related_dids, related_routines,
    ///   oem_bulletin, source) is read into the matching field on
    ///   <see cref="TOBDDtcCatalogEntry"/>. Existing codes are
    ///   replaced; new codes are appended.
    /// </summary>
    /// <param name="FilePath">Absolute path to the JSON file.</param>
    /// <exception cref="EOBDDtcCatalog">File missing or
    /// malformed.</exception>
    procedure LoadFromFile(const FilePath: string);
  end;

  /// <summary>Raised by <see cref="TOBDDtcCatalog.LoadFromFile"/>
  /// on file or parse errors.</summary>
  EOBDDtcCatalog = class(Exception);

  /// <summary>Raised by the J2012 helper functions on malformed
  /// input.</summary>
  EOBDDtcError = class(Exception);

/// <summary>Decodes a two-byte ISO 15031-5 DTC into its
/// 5-character form (e.g. <c>$03 $01</c> → <c>P0301</c>).</summary>
/// <param name="High">First byte (system + group + first
/// nibble).</param>
/// <param name="Low">Second byte (last two nibbles).</param>
function FormatDtc(const High, Low: Byte): string; overload;
/// <summary>Same as the two-byte form, reading the first two
/// bytes of <c>Bytes</c>.</summary>
/// <param name="Bytes">Source bytes (must be at least 2 long).</param>
/// <exception cref="EOBDDtcError">Buffer too short.</exception>
function FormatDtc(const Bytes: TBytes): string; overload;

/// <summary>Encodes a 5-character DTC (e.g. <c>P0301</c>) back
/// into the two ISO 15031-5 bytes.</summary>
/// <param name="Code">5-character code.</param>
/// <exception cref="EOBDDtcError">Malformed input.</exception>
function EncodeDtc(const Code: string): TBytes;

/// <summary>Parses the system letter (P/C/B/U) of a DTC into
/// the enum.</summary>
/// <param name="C">System letter (case-insensitive).</param>
/// <exception cref="EOBDDtcError">Unknown letter.</exception>
function ParseDtcSystem(const C: Char): TOBDDtcSystem;
/// <summary>Returns the system letter for <c>Sys</c>.</summary>
/// <param name="Sys">System enum value.</param>
function FormatDtcSystem(const Sys: TOBDDtcSystem): Char;

/// <summary>True if the DTC is in the manufacturer-specific
/// range (P1xxx, P3xxx, B1xxx, B3xxx, C1xxx, C3xxx, U1xxx,
/// U3xxx).</summary>
/// <param name="Code">5-character code.</param>
function IsManufacturerDtc(const Code: string): Boolean;

/// <summary>Maps the catalogue's text severity tag to the
/// enum.</summary>
/// <param name="S">Severity text (case-insensitive).</param>
function ParseSeverity(const S: string): TOBDDtcSeverity;
/// <summary>Inverse of <see cref="ParseSeverity"/>.</summary>
/// <param name="Severity">Severity enum value.</param>
function FormatSeverity(const Severity: TOBDDtcSeverity): string;

/// <summary>Maps the catalogue's <c>monitor_type</c> string
/// (continuous / non_continuous / comprehensive_component / "")
/// to the enum.</summary>
/// <param name="S">Monitor-type text.</param>
function ParseMonitorType(const S: string): TOBDDtcMonitorType;
/// <summary>Inverse of <see cref="ParseMonitorType"/>.</summary>
/// <param name="Mt">Monitor-type enum value.</param>
function FormatMonitorType(const Mt: TOBDDtcMonitorType): string;

implementation

constructor TOBDDtcCatalog.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FEntries := TDictionary<string, TOBDDtcCatalogEntry>.Create;
end;

destructor TOBDDtcCatalog.Destroy;
begin
  FEntries.Free;
  FLock.Free;
  inherited;
end;

function TOBDDtcCatalog.Normalise(const ACode: string): string;
begin
  Result := UpperCase(Trim(ACode));
end;

procedure TOBDDtcCatalog.RegisterEntry(const AEntry: TOBDDtcCatalogEntry);
var
  Stored: TOBDDtcCatalogEntry;
begin
  Stored := AEntry;
  Stored.Code := Normalise(AEntry.Code);
  if Stored.Code = '' then
    Exit;
  FLock.Enter;
  try
    FEntries.AddOrSetValue(Stored.Code, Stored);
  finally
    FLock.Leave;
  end;
end;

function TOBDDtcCatalog.TryFind(const ACode: string;
  out AEntry: TOBDDtcCatalogEntry): Boolean;
begin
  FLock.Enter;
  try
    Result := FEntries.TryGetValue(Normalise(ACode), AEntry);
  finally
    FLock.Leave;
  end;
end;

function TOBDDtcCatalog.FindByCode(const Code: string;
  out Entry: TOBDDtcCatalogEntry): Boolean;
begin
  Result := TryFind(Code, Entry);
end;

procedure TOBDDtcCatalog.Remove(const ACode: string);
begin
  FLock.Enter;
  try
    FEntries.Remove(Normalise(ACode));
  finally
    FLock.Leave;
  end;
end;

function TOBDDtcCatalog.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FEntries.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDDtcCatalog.Clear;
begin
  FLock.Enter;
  try
    FEntries.Clear;
  finally
    FLock.Leave;
  end;
end;

function TOBDDtcCatalog.Snapshot: TArray<TOBDDtcCatalogEntry>;
begin
  FLock.Enter;
  try
    Result := FEntries.Values.ToArray;
  finally
    FLock.Leave;
  end;
end;

function ReadStringArray(Item: TJSONObject; const Key: string): TArray<string>;
var
  Arr: TJSONArray;
  I: Integer;
begin
  Result := nil;
  Arr := Item.GetValue<TJSONArray>(Key);
  if Arr = nil then
    Exit;
  SetLength(Result, Arr.Count);
  for I := 0 to Arr.Count - 1 do
    Result[I] := Arr.Items[I].Value;
end;

procedure TOBDDtcCatalog.LoadFromFile(const FilePath: string);
var
  Text: string;
  Value: TJSONValue;
  Root: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDDtcCatalogEntry;
  DefaultSource: string;
begin
  if not TFile.Exists(FilePath) then
    raise EOBDDtcCatalog.CreateFmt(
      'DTC catalog file %s not found', [FilePath]);
  Text := TFile.ReadAllText(FilePath, TEncoding.UTF8);
  Value := TJSONObject.ParseJSONValue(Text);
  if not (Value is TJSONObject) then
  begin
    Value.Free;
    raise EOBDDtcCatalog.Create('DTC catalog root must be a JSON object');
  end;
  try
    Root := TJSONObject(Value);
    DefaultSource := Root.GetValue<string>('default_source', '');
    Arr := Root.GetValue<TJSONArray>('dtcs');
    if Arr = nil then
      Exit;
    for I := 0 to Arr.Count - 1 do
    begin
      if not (Arr.Items[I] is TJSONObject) then
        Continue;
      Item := TJSONObject(Arr.Items[I]);
      Entry := Default(TOBDDtcCatalogEntry);
      Entry.Code := Item.GetValue<string>('code', '');
      Entry.Severity := ParseSeverity(
        Item.GetValue<string>('severity', ''));
      Entry.Description := Item.GetValue<string>('description', '');
      Entry.PossibleCauses := ReadStringArray(Item, 'possible_causes');
      Entry.RepairHints := Item.GetValue<string>('repair_hints', '');
      Entry.Notes := Item.GetValue<string>('notes', '');
      Entry.Source := Item.GetValue<string>('source', DefaultSource);
      Entry.Verified := Item.GetValue<Boolean>('verified', False);
      Entry.Symptoms := ReadStringArray(Item, 'symptoms');
      Entry.RepairGuidance := ReadStringArray(Item, 'repair_guidance');
      Entry.MonitorType := ParseMonitorType(
        Item.GetValue<string>('monitor_type', ''));
      Entry.FreezeFrameRelevant :=
        Item.GetValue<Boolean>('freeze_frame_relevant', False);
      Entry.RelatedDIDs := ReadStringArray(Item, 'related_dids');
      Entry.RelatedRoutines := ReadStringArray(Item, 'related_routines');
      Entry.OemBulletin := Item.GetValue<string>('oem_bulletin', '');
      RegisterEntry(Entry);
    end;
  finally
    Value.Free;
  end;
end;

{ J2012 helpers ---------------------------------------------------- }

function ParseDtcSystem(const C: Char): TOBDDtcSystem;
begin
  case UpCase(C) of
    'P': Result := dtcPowertrain;
    'C': Result := dtcChassis;
    'B': Result := dtcBody;
    'U': Result := dtcNetwork;
  else
    raise EOBDDtcError.CreateFmt(
      'Unknown DTC system letter: %s', [C]);
  end;
end;

function FormatDtcSystem(const Sys: TOBDDtcSystem): Char;
begin
  case Sys of
    dtcPowertrain: Result := 'P';
    dtcChassis:    Result := 'C';
    dtcBody:       Result := 'B';
    dtcNetwork:    Result := 'U';
  else
    Result := '?';
  end;
end;

function FormatDtc(const High, Low: Byte): string;
const
  SYSTEMS: array[0..3] of Char = ('P', 'C', 'B', 'U');
var
  SysIdx, GroupBit, D2: Integer;
begin
  // Bits 7-6 of the high byte = system letter.
  // Bit 5 = code group (0: SAE, 1: manufacturer).
  // Digit 1 := 2 * GroupBit + D2 (SAE = 0/2, MFR = 1/3).
  SysIdx   := (High shr 6) and $03;
  GroupBit := (High shr 5) and $01;
  D2       := (High shr 4) and $01;
  Result := Format('%s%d%d%.2X', [
    SYSTEMS[SysIdx],
    GroupBit * 2 + D2,
    High and $0F,
    Low]);
end;

function FormatDtc(const Bytes: TBytes): string;
begin
  if Length(Bytes) < 2 then
    raise EOBDDtcError.Create('DTC needs 2 bytes');
  Result := FormatDtc(Bytes[0], Bytes[1]);
end;

function HexCharToNibble(C: Char): Byte;
begin
  case UpCase(C) of
    '0'..'9': Result := Byte(Ord(C) - Ord('0'));
    'A'..'F': Result := Byte(Ord(UpCase(C)) - Ord('A') + 10);
  else
    raise EOBDDtcError.CreateFmt('Not a hex digit: %s', [C]);
  end;
end;

function EncodeDtc(const Code: string): TBytes;
var
  S: string;
  Sys: TOBDDtcSystem;
  D1, D2, D3, D4: Byte;
  SysBits, GroupBit, FirstDigit: Byte;
begin
  S := UpperCase(Trim(Code));
  if Length(S) <> 5 then
    raise EOBDDtcError.CreateFmt('Malformed DTC: %s', [Code]);
  Sys := ParseDtcSystem(S[1]);
  case Sys of
    dtcPowertrain: SysBits := $00;
    dtcChassis:    SysBits := $01;
    dtcBody:       SysBits := $02;
    dtcNetwork:    SysBits := $03;
  else
    SysBits := $00;
  end;
  FirstDigit := HexCharToNibble(S[2]);
  if FirstDigit > 3 then
    raise EOBDDtcError.CreateFmt(
      'DTC first digit must be 0..3 (got %s)', [Code]);
  GroupBit := FirstDigit shr 1;
  D1 := FirstDigit and $01;
  D2 := HexCharToNibble(S[3]);
  D3 := HexCharToNibble(S[4]);
  D4 := HexCharToNibble(S[5]);
  SetLength(Result, 2);
  Result[0] := (SysBits shl 6) or (GroupBit shl 5) or
               (D1 shl 4) or D2;
  Result[1] := (D3 shl 4) or D4;
end;

function IsManufacturerDtc(const Code: string): Boolean;
var
  S: string;
begin
  S := UpperCase(Trim(Code));
  Result := (Length(S) = 5)
        and CharInSet(S[1], ['P', 'C', 'B', 'U'])
        and CharInSet(S[2], ['1', '3']);
end;

function ParseSeverity(const S: string): TOBDDtcSeverity;
var
  T: string;
begin
  T := LowerCase(Trim(S));
  if T = 'info'     then Result := dtcSeverityInfo
  else if T = 'warning'  then Result := dtcSeverityWarning
  else if T = 'critical' then Result := dtcSeverityCritical
  else                   Result := dtcSeverityUnknown;
end;

function FormatSeverity(const Severity: TOBDDtcSeverity): string;
begin
  case Severity of
    dtcSeverityInfo:     Result := 'info';
    dtcSeverityWarning:  Result := 'warning';
    dtcSeverityCritical: Result := 'critical';
  else
    Result := '';
  end;
end;

function ParseMonitorType(const S: string): TOBDDtcMonitorType;
var
  T: string;
begin
  T := LowerCase(Trim(S));
  if T = 'continuous' then
    Result := dmtContinuous
  else if T = 'non_continuous' then
    Result := dmtNonContinuous
  else if T = 'comprehensive_component' then
    Result := dmtComprehensiveComponent
  else
    Result := dmtUnknown;
end;

function FormatMonitorType(const Mt: TOBDDtcMonitorType): string;
begin
  case Mt of
    dmtContinuous:              Result := 'continuous';
    dmtNonContinuous:           Result := 'non_continuous';
    dmtComprehensiveComponent:  Result := 'comprehensive_component';
  else
    Result := '';
  end;
end;

end.
