//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.DTC.pas
// CONTENTS       : Diagnostic Trouble Code catalog — types, parser,
//                  JSON loader, lookup helpers
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Two-byte DTCs follow ISO 15031-5 / SAE J2012:
//                  - bits 15-14 = system letter (00=P 01=C 10=B 11=U)
//                  - bit  13    = code group (0 = SAE, 1 = manufacturer)
//                  - bits 12-8  = digit 2/3 (one nibble each)
//                  - bits  7-0  = digits 3/4 (two nibbles)
//                  Example: 0x0301 → P0301 (cylinder 1 misfire).
//                  The catalog format mirrors the v3.3 DID JSON
//                  schema (provenance via `source` + `verified`).
//------------------------------------------------------------------------------
unit OBD.OEM.DTC;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.SyncObjs, System.Generics.Collections;

type
  EOBDDtcError = class(Exception);

  /// <summary>Coarse severity hint for UI tinting / triage. The wire
  /// protocol doesn't carry severity — these are catalog metadata.</summary>
  TOBDDtcSeverity = (
    dtcSeverityUnknown,
    dtcSeverityInfo,
    dtcSeverityWarning,
    dtcSeverityCritical
  );

  /// <summary>SAE J2012 system letter — the first character of a DTC.</summary>
  TOBDDtcSystem = (
    dtcPowertrain,    // P
    dtcChassis,       // C
    dtcBody,          // B
    dtcNetwork        // U
  );

  TOBDDtcCatalogEntry = record
    /// <summary>Five-character code: <c>P0301</c>, <c>B22A8</c>, etc.</summary>
    Code: string;
    Severity: TOBDDtcSeverity;
    /// <summary>Short, single-line description ("Cylinder 1 misfire detected").</summary>
    Description: string;
    /// <summary>Free-form list of plausible causes shown to the user.</summary>
    PossibleCauses: TArray<string>;
    /// <summary>Repair-hint paragraph (often borrowed from service manuals).</summary>
    RepairHints: string;
    /// <summary>Provenance — same vocabulary as the DID catalog
    /// (<c>iso-15031-6</c>, <c>sae-j2012</c>, <c>ross-tech-wiki</c>,
    /// <c>esys-community</c>, <c>community-pr</c>, …).</summary>
    Source: string;
    /// <summary>True only when matched against an authoritative spec
    /// (SAE J2012, OEM service manual) or capture fixture.</summary>
    Verified: Boolean;
  end;

  /// <summary>
  ///   In-memory, lock-free DTC lookup. Owns its entries; pass
  ///   <c>nil</c> to <c>FindByCode</c> to discover whether a code
  ///   is catalogued without taking the entry.
  /// </summary>
  TOBDDtcCatalog = class
  strict private
    FEntries: TList<TOBDDtcCatalogEntry>;
    FByCode: TDictionary<string, Integer>;
    procedure RebuildIndex;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Add or replace an entry. Index is updated in lock-step.</summary>
    procedure Add(const Entry: TOBDDtcCatalogEntry);

    /// <summary>Bulk-load from a JSON array (entries are appended;
    /// existing codes are replaced).</summary>
    procedure LoadFromJSON(Arr: TJSONArray; const DefaultSource: string = '');
    procedure LoadFromFile(const FilePath: string);
    procedure LoadFromText(const JsonText: string);

    /// <summary>Find an entry by its 5-character code (case-insensitive).</summary>
    function FindByCode(const Code: string;
      out Entry: TOBDDtcCatalogEntry): Boolean;

    /// <summary>Number of catalogued entries.</summary>
    function Count: Integer;
    function Item(Index: Integer): TOBDDtcCatalogEntry;
    function ToArray: TArray<TOBDDtcCatalogEntry>;
    procedure Clear;
  end;

/// <summary>Decode a two-byte ISO 15031-5 DTC into its 5-character form.</summary>
function FormatDtc(const High, Low: Byte): string; overload;
function FormatDtc(const Bytes: TBytes): string; overload;

/// <summary>Encode a 5-character DTC (e.g. <c>P0301</c>) back into the
/// two ISO 15031-5 bytes. Throws <c>EOBDDtcError</c> on a malformed
/// input.</summary>
function EncodeDtc(const Code: string): TBytes;

/// <summary>Letter (P/C/B/U) parser.</summary>
function ParseDtcSystem(const C: Char): TOBDDtcSystem;
function FormatDtcSystem(const Sys: TOBDDtcSystem): Char;

/// <summary>True if the DTC is in the manufacturer-specific range
/// (P1xxx, P3xxx, B1xxx, B3xxx, C1xxx, C3xxx, U1xxx, U3xxx).</summary>
function IsManufacturerDtc(const Code: string): Boolean;

/// <summary>Map the catalog's text severity tags to the enum.</summary>
function ParseSeverity(const S: string): TOBDDtcSeverity;
function FormatSeverity(const Severity: TOBDDtcSeverity): string;

implementation

uses
  System.Character;

//==============================================================================
// Encoding
//==============================================================================
function ParseDtcSystem(const C: Char): TOBDDtcSystem;
begin
  case UpCase(C) of
    'P': Result := dtcPowertrain;
    'C': Result := dtcChassis;
    'B': Result := dtcBody;
    'U': Result := dtcNetwork;
  else
    raise EOBDDtcError.CreateFmt('Unknown DTC system letter: %s', [C]);
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
  // Bits 7-6 of the high byte = system letter index.
  // Bit 5 = code group (0: SAE/CARB defined, 1: manufacturer).
  // Bits 4-0 (across both bytes) hold the four hex digits.
  SysIdx   := (High shr 6) and $03;
  GroupBit := (High shr 5) and $01;
  D2       := (High shr 4) and $01;
  // Digit 1 = 2 * GroupBit + D2 (SAE = 0/2, MFR = 1/3).
  Result := Format('%s%d%d%.2X', [
    SYSTEMS[SysIdx],
    GroupBit * 2 + D2,
    High and $0F,
    Low
  ]);
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
  Norm: string;
  SysIdx, FirstDigit, GroupBit, D2: Integer;
  D3, D4, D5: Byte;
begin
  Norm := UpperCase(Trim(Code));
  if Length(Norm) <> 5 then
    raise EOBDDtcError.CreateFmt('DTC must be 5 characters: %s', [Code]);
  case Norm[1] of
    'P': SysIdx := 0;
    'C': SysIdx := 1;
    'B': SysIdx := 2;
    'U': SysIdx := 3;
  else
    raise EOBDDtcError.CreateFmt('Unknown DTC system letter: %s', [Code]);
  end;
  if not CharInSet(Norm[2], ['0'..'3']) then
    raise EOBDDtcError.CreateFmt('Invalid DTC group digit: %s', [Code]);
  FirstDigit := Ord(Norm[2]) - Ord('0');
  GroupBit := FirstDigit shr 1;     // 0/1 → 0, 2/3 → 1
  D2       := FirstDigit and $01;
  D3 := HexCharToNibble(Norm[3]);
  D4 := HexCharToNibble(Norm[4]);
  D5 := HexCharToNibble(Norm[5]);
  SetLength(Result, 2);
  Result[0] := (Byte(SysIdx) shl 6) or (Byte(GroupBit) shl 5) or
               (Byte(D2) shl 4) or D3;
  Result[1] := (D4 shl 4) or D5;
end;

function IsManufacturerDtc(const Code: string): Boolean;
begin
  if Length(Code) < 2 then Exit(False);
  Result := CharInSet(Code[2], ['1', '3']);
end;

//==============================================================================
// Severity
//==============================================================================
function ParseSeverity(const S: string): TOBDDtcSeverity;
var L: string;
begin
  L := LowerCase(Trim(S));
  if (L = 'info') or (L = 'information') then Exit(dtcSeverityInfo);
  if (L = 'warning') or (L = 'warn') then Exit(dtcSeverityWarning);
  if (L = 'critical') or (L = 'severe') then Exit(dtcSeverityCritical);
  Result := dtcSeverityUnknown;
end;

function FormatSeverity(const Severity: TOBDDtcSeverity): string;
begin
  case Severity of
    dtcSeverityInfo:     Result := 'info';
    dtcSeverityWarning:  Result := 'warning';
    dtcSeverityCritical: Result := 'critical';
  else
    Result := 'unknown';
  end;
end;

//==============================================================================
// TOBDDtcCatalog
//==============================================================================
constructor TOBDDtcCatalog.Create;
begin
  inherited Create;
  FEntries := TList<TOBDDtcCatalogEntry>.Create;
  FByCode := TDictionary<string, Integer>.Create;
end;

destructor TOBDDtcCatalog.Destroy;
begin
  FByCode.Free;
  FEntries.Free;
  inherited;
end;

procedure TOBDDtcCatalog.RebuildIndex;
var
  I: Integer;
begin
  FByCode.Clear;
  for I := 0 to FEntries.Count - 1 do
    FByCode.AddOrSetValue(UpperCase(FEntries[I].Code), I);
end;

procedure TOBDDtcCatalog.Add(const Entry: TOBDDtcCatalogEntry);
var
  ExistingIdx: Integer;
  Key: string;
begin
  Key := UpperCase(Entry.Code);
  if FByCode.TryGetValue(Key, ExistingIdx) then
    FEntries[ExistingIdx] := Entry
  else
  begin
    FEntries.Add(Entry);
    FByCode.AddOrSetValue(Key, FEntries.Count - 1);
  end;
end;

function ParseCausesArray(Arr: TJSONArray): TArray<string>;
var
  I: Integer;
begin
  if Arr = nil then Exit(nil);
  SetLength(Result, Arr.Count);
  for I := 0 to Arr.Count - 1 do
    Result[I] := Arr.Items[I].Value;
end;

procedure TOBDDtcCatalog.LoadFromJSON(Arr: TJSONArray;
  const DefaultSource: string);
var
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDDtcCatalogEntry;
begin
  if Arr = nil then Exit;
  for I := 0 to Arr.Count - 1 do
  begin
    if not (Arr.Items[I] is TJSONObject) then Continue;
    Item := TJSONObject(Arr.Items[I]);
    Entry := Default(TOBDDtcCatalogEntry);
    Entry.Code := UpperCase(Item.GetValue<string>('code', ''));
    if Entry.Code = '' then Continue;
    Entry.Description := Item.GetValue<string>('description', '');
    Entry.Severity := ParseSeverity(Item.GetValue<string>('severity', ''));
    Entry.PossibleCauses :=
      ParseCausesArray(Item.GetValue<TJSONArray>('possible_causes'));
    Entry.RepairHints := Item.GetValue<string>('repair_hints', '');
    Entry.Source := Item.GetValue<string>('source', DefaultSource);
    Entry.Verified := Item.GetValue<Boolean>('verified', False);
    Add(Entry);
  end;
end;

procedure TOBDDtcCatalog.LoadFromFile(const FilePath: string);
begin
  if not TFile.Exists(FilePath) then
    raise EOBDDtcError.CreateFmt('DTC catalog file %s not found', [FilePath]);
  LoadFromText(TFile.ReadAllText(FilePath, TEncoding.UTF8));
end;

procedure TOBDDtcCatalog.LoadFromText(const JsonText: string);
var
  Root: TJSONValue;
  RootObj: TJSONObject;
  Arr: TJSONArray;
  DefaultSource: string;
begin
  Root := TJSONObject.ParseJSONValue(JsonText);
  if Root = nil then
    raise EOBDDtcError.Create('DTC catalog JSON is invalid');
  try
    if Root is TJSONArray then
      LoadFromJSON(TJSONArray(Root), '')
    else if Root is TJSONObject then
    begin
      RootObj := TJSONObject(Root);
      DefaultSource := RootObj.GetValue<string>('default_source', '');
      Arr := RootObj.GetValue<TJSONArray>('dtcs');
      LoadFromJSON(Arr, DefaultSource);
    end
    else
      raise EOBDDtcError.Create('DTC catalog root must be object or array');
  finally
    Root.Free;
  end;
end;

function TOBDDtcCatalog.FindByCode(const Code: string;
  out Entry: TOBDDtcCatalogEntry): Boolean;
var
  Idx: Integer;
begin
  Result := FByCode.TryGetValue(UpperCase(Trim(Code)), Idx);
  if Result then Entry := FEntries[Idx];
end;

function TOBDDtcCatalog.Count: Integer;
begin Result := FEntries.Count; end;

function TOBDDtcCatalog.Item(Index: Integer): TOBDDtcCatalogEntry;
begin Result := FEntries[Index]; end;

function TOBDDtcCatalog.ToArray: TArray<TOBDDtcCatalogEntry>;
begin Result := FEntries.ToArray; end;

procedure TOBDDtcCatalog.Clear;
begin
  FEntries.Clear;
  FByCode.Clear;
end;

end.
