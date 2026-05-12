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
    ///   top-level object with a <c>dtcs</c> array of
    ///   <c>{ "code": "P0420", "description": "...", "notes": "..." }</c>
    ///   objects. Existing codes are replaced; new codes are
    ///   appended.
    /// </summary>
    /// <param name="FilePath">Absolute path to the JSON file.</param>
    /// <exception cref="EOBDDtcCatalog">File missing or
    /// malformed.</exception>
    procedure LoadFromFile(const FilePath: string);
  end;

  /// <summary>Raised by <see cref="TOBDDtcCatalog.LoadFromFile"/>
  /// on file or parse errors.</summary>
  EOBDDtcCatalog = class(Exception);

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

procedure TOBDDtcCatalog.LoadFromFile(const FilePath: string);
var
  Text: string;
  Value: TJSONValue;
  Root: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  Item: TJSONObject;
  Entry: TOBDDtcCatalogEntry;
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
      Entry.Description := Item.GetValue<string>('description', '');
      Entry.Notes := Item.GetValue<string>('notes', '');
      RegisterEntry(Entry);
    end;
  finally
    Value.Free;
  end;
end;

end.
