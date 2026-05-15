//------------------------------------------------------------------------------
//  OBD.KWP.Catalog
//
//  Loads the shipped KWP2000 (ISO 14230-3) identifier catalogue
//  (<c>catalogs/kwp/common-ids.json</c>) and exposes a typed
//  lookup over the two well-known identifier families:
//
//    - ECU-Identification (Service 0x1A) — the §6.7 block read
//      from <c>0x80</c> upwards (configuration, manufacturing
//      data, VIN, software banner, …).
//    - Common-Identifiers (Service 0x22) — the F180..F199 range
//      that overlays the equivalent ISO 14229 DIDs (boot software
//      ID, application software ID, programming date, …).
//
//  The catalogue is the seed; OEM-specific identifiers overlay
//  it via the OEM extension framework. A tool that scans a KWP
//  ECU's identifier table calls <see cref="FindKwpIdentifier"/>
//  with the byte / word it received and gets back a typed
//  <see cref="TOBDKwpIdentifier"/>. Unknown IDs return
//  <c>False</c>.
//
//  The catalogue file is optional: a missing or malformed file
//  leaves the registries empty. Build-time linting validates the
//  shipped catalogue against
//  <c>catalogs/_schema/text-catalog.schema.json</c>-style
//  conventions; this loader is tolerant by design so application
//  startup is never blocked.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.KWP.Catalog;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>Which family of KWP identifier an entry came
  /// from. Drives the service byte a tool sends to read the
  /// identifier.</summary>
  TOBDKwpIdentifierFamily = (
    /// <summary>ECU-Identification block — Service 0x1A.</summary>
    kifEcuIdentification,
    /// <summary>Common-Identifier — Service 0x22.</summary>
    kifCommonIdentifier);

  /// <summary>One identifier entry from the KWP catalogue.</summary>
  TOBDKwpIdentifier = record
    /// <summary>Identifier value. ECU-Identification IDs are
    /// 1 byte (0..255); Common-Identifiers are 16-bit.</summary>
    Id: Word;
    /// <summary>Human-readable label.</summary>
    Name: string;
    /// <summary>Which family this identifier belongs to. Also
    /// implies the read service byte (0x1A for
    /// <c>kifEcuIdentification</c>, 0x22 for
    /// <c>kifCommonIdentifier</c>).</summary>
    Family: TOBDKwpIdentifierFamily;
    /// <summary>Service byte parsed verbatim from the catalogue
    /// (<c>0x1A</c> / <c>0x22</c>).</summary>
    Service: Byte;
    /// <summary>Fixed payload length in bytes when the
    /// catalogue declares one; 0 when the entry is marked
    /// <c>"variable"</c> or omits the field.</summary>
    Length: Integer;
    /// <summary>True when the catalogue records the length as
    /// <c>"variable"</c> rather than a fixed number.</summary>
    LengthVariable: Boolean;
    /// <summary>Free-form notes from the catalogue.</summary>
    Notes: string;
  end;

/// <summary>Looks up an identifier by family + numeric ID.
/// </summary>
/// <param name="Family">Which family to search.</param>
/// <param name="Id">Identifier value.</param>
/// <param name="Entry">Out: matching entry on success.</param>
/// <returns><c>True</c> when the identifier is catalogued.</returns>
function FindKwpIdentifier(Family: TOBDKwpIdentifierFamily;
  Id: Word; out Entry: TOBDKwpIdentifier): Boolean;

/// <summary>Returns every ECU-Identification entry, in
/// ascending ID order.</summary>
function KwpEcuIdentifications: TArray<TOBDKwpIdentifier>;

/// <summary>Returns every Common-Identifier entry, in
/// ascending ID order.</summary>
function KwpCommonIdentifiers: TArray<TOBDKwpIdentifier>;

/// <summary>Re-reads the catalogue from disk. Use after
/// replacing the catalogue file at runtime; production startup
/// loads once at unit initialisation.</summary>
procedure ReloadKwpCatalogue;

implementation

uses
  System.Classes,
  System.JSON,
  System.IOUtils,
  OBD.OEM.Catalog.Loader;

var
  GEcuIds:    TDictionary<Word, TOBDKwpIdentifier> = nil;
  GCommonIds: TDictionary<Word, TOBDKwpIdentifier> = nil;

function ParseHexOrInt(const S: string): Cardinal;
var
  T: string;
begin
  T := Trim(S);
  if T = '' then
    Exit(0);
  if T.StartsWith('0x', True) or T.StartsWith('$') then
    Result := StrToInt('$' + T
      .Replace('0x', '', [rfIgnoreCase])
      .Replace('$', ''))
  else
    Result := StrToInt(T);
end;

procedure ReadLength(Item: TJSONObject; out Len: Integer;
  out Variable: Boolean);
var
  Raw: TJSONValue;
  S: string;
begin
  Len := 0;
  Variable := False;
  Raw := Item.GetValue('length');
  if Raw = nil then
    Exit;
  if Raw is TJSONNumber then
  begin
    Len := TJSONNumber(Raw).AsInt;
    Exit;
  end;
  S := Raw.Value;
  if SameText(Trim(S), 'variable') then
    Variable := True
  else if S <> '' then
    Len := StrToIntDef(S, 0);
end;

procedure LoadArray(Root: TJSONObject; const Key: string;
  Family: TOBDKwpIdentifierFamily;
  Target: TDictionary<Word, TOBDKwpIdentifier>);
var
  Arr: TJSONArray;
  ItemVal: TJSONValue;
  Item: TJSONObject;
  Entry: TOBDKwpIdentifier;
begin
  Arr := Root.GetValue<TJSONArray>(Key);
  if Arr = nil then
    Exit;
  for ItemVal in Arr do
  begin
    if not (ItemVal is TJSONObject) then
      Continue;
    Item := TJSONObject(ItemVal);
    Entry := Default(TOBDKwpIdentifier);
    Entry.Id := Word(ParseHexOrInt(Item.GetValue<string>('id', '0')));
    Entry.Name := Item.GetValue<string>('name', '');
    Entry.Family := Family;
    Entry.Service := Byte(ParseHexOrInt(
      Item.GetValue<string>('service', '0')));
    ReadLength(Item, Entry.Length, Entry.LengthVariable);
    Entry.Notes := Item.GetValue<string>('notes', '');
    Target.AddOrSetValue(Entry.Id, Entry);
  end;
end;

procedure ReloadKwpCatalogue;
var
  Path, Raw: string;
  Doc: TJSONValue;
  Root: TJSONObject;
begin
  GEcuIds.Clear;
  GCommonIds.Clear;
  // Catalogue lives under catalogs/kwp/common-ids.json; the
  // resolver searches the usual exe-dir / parent / cwd ladder.
  Path := ResolveCatalogPath(TPath.Combine('kwp', 'common-ids.json'));
  if Path = '' then
    Exit;
  try
    Raw := TFile.ReadAllText(Path, TEncoding.UTF8);
    Doc := TJSONObject.ParseJSONValue(Raw);
    if not (Doc is TJSONObject) then
    begin
      Doc.Free;
      Exit;
    end;
    try
      Root := TJSONObject(Doc);
      LoadArray(Root, 'ecu_identification',
        kifEcuIdentification, GEcuIds);
      LoadArray(Root, 'common_identifiers',
        kifCommonIdentifier, GCommonIds);
    finally
      Doc.Free;
    end;
  except
    // Malformed file: leave registries empty so startup is not
    // blocked. Build-time linting catches schema breakage.
  end;
end;

function FindKwpIdentifier(Family: TOBDKwpIdentifierFamily;
  Id: Word; out Entry: TOBDKwpIdentifier): Boolean;
begin
  case Family of
    kifEcuIdentification:
      Result := (GEcuIds <> nil)
            and GEcuIds.TryGetValue(Id, Entry);
    kifCommonIdentifier:
      Result := (GCommonIds <> nil)
            and GCommonIds.TryGetValue(Id, Entry);
  else
    Result := False;
  end;
end;

function SortAscending(Source: TDictionary<Word, TOBDKwpIdentifier>):
  TArray<TOBDKwpIdentifier>;
var
  Keys: TArray<Word>;
  I, J: Integer;
  Min: Integer;
  Tmp: Word;
begin
  if Source = nil then
    Exit(nil);
  Keys := Source.Keys.ToArray;
  // Selection sort — catalogues are tiny (<32 entries each).
  for I := 0 to High(Keys) - 1 do
  begin
    Min := I;
    for J := I + 1 to High(Keys) do
      if Keys[J] < Keys[Min] then
        Min := J;
    if Min <> I then
    begin
      Tmp := Keys[I];
      Keys[I] := Keys[Min];
      Keys[Min] := Tmp;
    end;
  end;
  SetLength(Result, Length(Keys));
  for I := 0 to High(Keys) do
    Result[I] := Source[Keys[I]];
end;

function KwpEcuIdentifications: TArray<TOBDKwpIdentifier>;
begin
  Result := SortAscending(GEcuIds);
end;

function KwpCommonIdentifiers: TArray<TOBDKwpIdentifier>;
begin
  Result := SortAscending(GCommonIds);
end;

initialization
  GEcuIds    := TDictionary<Word, TOBDKwpIdentifier>.Create;
  GCommonIds := TDictionary<Word, TOBDKwpIdentifier>.Create;
  ReloadKwpCatalogue;

finalization
  GEcuIds.Free;
  GCommonIds.Free;

end.
