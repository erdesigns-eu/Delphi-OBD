//------------------------------------------------------------------------------
//  OBD.Catalog
//
//  JSON catalogue loader and process-wide store. Catalogues describe
//  PIDs (modes 01/02/05/06/09), DTC text mappings, NRC text mappings,
//  J1939 PGNs/SPNs/FMIs, UDS DIDs, and OEM extensions. The runtime
//  reads JSON files at startup; community contributions to add an entry
//  are JSON-only edits.
//
//  Schema (every catalogue file):
//
//    {
//      "version": 1,
//      "type":    "obd2-pid"  | "obd2-dtc"  | "uds-nrc" | …,
//      "mode":    "01"        | …            (PID files only)
//      "entries": [ { … }, … ]
//    }
//
//  The loader rejects malformed files with a clear diagnostic naming
//  the offending file:line. Lookup is by <c>(type, mode, id)</c> tuple.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - https://www.json.org/
//    - ../catalogs/_schema/  (machine-readable JSON Schema)
//
//  History     :
//    2026-05-09  ERD  Initial Phase 1 loader: PID descriptors, DTC text,
//                     NRC text, in-memory store with case-insensitive
//                     type lookup.
//
//  Future work :
//    - Resource-embedded fallback so the package works without loose
//      JSON files at runtime.
//    - Schema-driven validation (currently structural-only).
//------------------------------------------------------------------------------

unit OBD.Catalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>
  ///   Logical catalogue family. Determined by the <c>type</c> field of
  ///   the JSON file.
  /// </summary>
  TOBDCatalogKind = (
    /// <summary>OBD-II PID definitions (Mode 01, 02, 05, 06, 09).</summary>
    ckOBD2PID,
    /// <summary>OBD-II / generic DTC code → text mapping.</summary>
    ckOBD2DTC,
    /// <summary>UDS Negative Response Code → text mapping.</summary>
    ckUDSNRC,
    /// <summary>J1939 PGN definitions.</summary>
    ckJ1939PGN,
    /// <summary>J1939 SPN definitions.</summary>
    ckJ1939SPN,
    /// <summary>J1939 FMI definitions.</summary>
    ckJ1939FMI,
    /// <summary>UDS DID definitions.</summary>
    ckUDSDID,
    /// <summary>Adapter capability matrix.</summary>
    ckAdapterCapabilities,
    /// <summary>Unrecognised type — file kept loaded but cannot be
    /// queried by typed accessor.</summary>
    ckUnknown
  );

  /// <summary>
  ///   A simple <c>code → text</c> entry used by DTC and NRC catalogues.
  /// </summary>
  TOBDCatalogTextEntry = record
    /// <summary>Numeric or hex code (e.g. <c>$22</c> for an NRC,
    /// <c>$0420</c> for a P-DTC suffix).</summary>
    Code: Cardinal;
    /// <summary>Human-readable text.</summary>
    Text: string;
  end;

  /// <summary>
  ///   A loaded JSON catalogue file.
  /// </summary>
  /// <remarks>
  ///   Owned by the <see cref="TOBDCatalogStore"/> that loaded it.
  ///   Consumers query the store via <c>FindPID</c> / <c>FindText</c>;
  ///   they do not normally hold a <c>TOBDCatalog</c> reference.
  /// </remarks>
  TOBDCatalog = class
  strict private
    FKind: TOBDCatalogKind;
    FTypeName: string;
    FMode: string;
    FFileName: string;
    FVersion: Integer;
    FPIDs: TList<TOBDPIDDescriptor>;
    FTexts: TList<TOBDCatalogTextEntry>;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    /// <summary>Logical catalogue kind.</summary>
    property Kind: TOBDCatalogKind read FKind write FKind;
    /// <summary>Verbatim <c>type</c> string from the JSON file.</summary>
    property TypeName: string read FTypeName write FTypeName;
    /// <summary>Verbatim <c>mode</c> string from the JSON file. Empty
    /// for non-PID catalogues.</summary>
    property Mode: string read FMode write FMode;
    /// <summary>Path the catalogue was loaded from.</summary>
    property FileName: string read FFileName;
    /// <summary>Schema version. Currently <c>1</c>.</summary>
    property Version: Integer read FVersion write FVersion;
    /// <summary>PID/DID descriptor list. Owned by this catalogue.</summary>
    property PIDs: TList<TOBDPIDDescriptor> read FPIDs;
    /// <summary>Code → text entry list. Owned by this catalogue.</summary>
    property Texts: TList<TOBDCatalogTextEntry> read FTexts;
  end;

  /// <summary>
  ///   Process-wide store of loaded catalogues.
  /// </summary>
  /// <remarks>
  ///   Components query the default instance to resolve a PID name to a
  ///   descriptor or a DTC code to text. Loading is idempotent — calling
  ///   <c>LoadFile</c> on an already-loaded path replaces the existing
  ///   entry.
  /// </remarks>
  TOBDCatalogStore = class
  strict private
    class var FInstance: TOBDCatalogStore;
    FCatalogs: TObjectList<TOBDCatalog>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>The shared process-wide store.</summary>
    class function Default: TOBDCatalogStore;
    /// <summary>Releases the shared instance. Called from this unit's
    /// finalisation.</summary>
    class procedure ReleaseDefault;

    /// <summary>Loads a single JSON catalogue file.</summary>
    /// <param name="AFileName">Absolute or relative path to the JSON
    /// file.</param>
    /// <returns>The loaded catalogue. Owned by the store.</returns>
    /// <exception cref="EOBDConfig">File missing, malformed JSON, or
    /// schema-incompatible.</exception>
    function LoadFile(const AFileName: string): TOBDCatalog;

    /// <summary>Loads every <c>*.json</c> file under a directory tree.
    /// Stops on the first malformed file with a clear diagnostic.</summary>
    /// <param name="ARoot">Root directory to walk.</param>
    /// <returns>Number of files loaded.</returns>
    /// <exception cref="EOBDConfig">A file under <c>ARoot</c> failed to
    /// load. Existing successfully-loaded catalogues remain.</exception>
    function LoadDirectory(const ARoot: string): Integer;

    /// <summary>Removes every loaded catalogue.</summary>
    procedure Clear;

    /// <summary>Number of catalogues currently loaded.</summary>
    function Count: Integer;
    /// <summary>Indexed access to loaded catalogues.</summary>
    function GetCatalog(AIndex: Integer): TOBDCatalog;

    /// <summary>Finds a PID descriptor by mode and ID.</summary>
    /// <param name="AMode">Mode string (e.g. <c>'01'</c>, <c>'09'</c>).</param>
    /// <param name="AID">PID identifier.</param>
    /// <param name="ADescriptor">Output descriptor, valid only when the
    /// function returns <c>True</c>.</param>
    /// <returns><c>True</c> if a descriptor exists for the given
    /// <c>(mode, id)</c> pair.</returns>
    function FindPID(const AMode: string; AID: Cardinal;
      out ADescriptor: TOBDPIDDescriptor): Boolean;

    /// <summary>Finds a code → text entry across all catalogues of a
    /// given kind.</summary>
    /// <param name="AKind">Catalogue kind to search.</param>
    /// <param name="ACode">Numeric code to look up.</param>
    /// <param name="AText">Output text, valid only when the function
    /// returns <c>True</c>.</param>
    /// <returns><c>True</c> when the code is found.</returns>
    function FindText(AKind: TOBDCatalogKind; ACode: Cardinal;
      out AText: string): Boolean;
  end;

/// <summary>
///   Maps a JSON <c>type</c> string to a <see cref="TOBDCatalogKind"/>.
/// </summary>
/// <param name="ATypeName">Verbatim string from the file.</param>
/// <returns><see cref="ckUnknown"/> for unrecognised values.</returns>
function CatalogKindFromString(const ATypeName: string): TOBDCatalogKind;

implementation

{ ---- helpers ----------------------------------------------------------------- }

function CatalogKindFromString(const ATypeName: string): TOBDCatalogKind;
var
  Key: string;
begin
  Key := LowerCase(Trim(ATypeName));
  if      Key = 'obd2-pid'              then Result := ckOBD2PID
  else if Key = 'obd2-dtc'              then Result := ckOBD2DTC
  else if Key = 'uds-nrc'               then Result := ckUDSNRC
  else if Key = 'j1939-pgn'             then Result := ckJ1939PGN
  else if Key = 'j1939-spn'             then Result := ckJ1939SPN
  else if Key = 'j1939-fmi'             then Result := ckJ1939FMI
  else if Key = 'uds-did'               then Result := ckUDSDID
  else if Key = 'adapter-capabilities'  then Result := ckAdapterCapabilities
  else                                       Result := ckUnknown;
end;

function GetJSONString(AObj: TJSONObject; const AName, ADefault: string): string;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AName);
  if V = nil then
    Exit(ADefault);
  Result := V.Value;
end;

function GetJSONInt(AObj: TJSONObject; const AName: string;
  ADefault: Integer): Integer;
var
  V: TJSONValue;
  N: Int64;
begin
  V := AObj.GetValue(AName);
  if V = nil then
    Exit(ADefault);
  if V is TJSONNumber then
    Exit((V as TJSONNumber).AsInt);
  if TryStrToInt64(V.Value, N) then
    Exit(N);
  Result := ADefault;
end;

function GetJSONDouble(AObj: TJSONObject; const AName: string;
  ADefault: Double): Double;
var
  V: TJSONValue;
  D: Double;
  FmtSettings: TFormatSettings;
begin
  V := AObj.GetValue(AName);
  if V = nil then
    Exit(ADefault);
  if V is TJSONNumber then
    Exit((V as TJSONNumber).AsDouble);
  FmtSettings := TFormatSettings.Invariant;
  if TryStrToFloat(V.Value, D, FmtSettings) then
    Exit(D);
  Result := ADefault;
end;

function ParseHexOrInt(const ARaw: string; out AOut: Cardinal): Boolean;
var
  S: string;
  N: Int64;
begin
  S := Trim(ARaw);
  if S = '' then
    Exit(False);
  if (Length(S) > 2) and (S[1] = '0') and ((S[2] = 'x') or (S[2] = 'X')) then
    S := '$' + Copy(S, 3, MaxInt)
  else if (S[1] <> '$') and TryStrToInt64('$' + S, N) and
          // Treat as hex if it looks hex (any letter present)
          (Pos('A', UpperCase(S)) + Pos('B', UpperCase(S)) +
           Pos('C', UpperCase(S)) + Pos('D', UpperCase(S)) +
           Pos('E', UpperCase(S)) + Pos('F', UpperCase(S)) > 0) then
    S := '$' + S;
  if TryStrToInt64(S, N) then
  begin
    AOut := Cardinal(N);
    Exit(True);
  end;
  Result := False;
end;

procedure RaiseConfig(const AFileName, AMessage: string);
begin
  raise EOBDConfig.CreateFmt('%s: %s', [AFileName, AMessage]);
end;

{ ---- TOBDCatalog ------------------------------------------------------------- }

constructor TOBDCatalog.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FPIDs := TList<TOBDPIDDescriptor>.Create;
  FTexts := TList<TOBDCatalogTextEntry>.Create;
end;

destructor TOBDCatalog.Destroy;
begin
  FPIDs.Free;
  FTexts.Free;
  inherited;
end;

{ ---- TOBDCatalogStore -------------------------------------------------------- }

constructor TOBDCatalogStore.Create;
begin
  inherited Create;
  FCatalogs := TObjectList<TOBDCatalog>.Create(True);
end;

destructor TOBDCatalogStore.Destroy;
begin
  FCatalogs.Free;
  inherited;
end;

class function TOBDCatalogStore.Default: TOBDCatalogStore;
begin
  if FInstance = nil then
    FInstance := TOBDCatalogStore.Create;
  Result := FInstance;
end;

class procedure TOBDCatalogStore.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDCatalogStore.Clear;
begin
  FCatalogs.Clear;
end;

function TOBDCatalogStore.Count: Integer;
begin
  Result := FCatalogs.Count;
end;

function TOBDCatalogStore.GetCatalog(AIndex: Integer): TOBDCatalog;
begin
  Result := FCatalogs[AIndex];
end;

procedure ParsePIDEntries(ACatalog: TOBDCatalog; AArr: TJSONArray);
var
  I: Integer;
  Entry: TJSONObject;
  D: TOBDPIDDescriptor;
  IDStr: string;
  IDNum: Cardinal;
begin
  for I := 0 to AArr.Count - 1 do
  begin
    if not (AArr.Items[I] is TJSONObject) then
      RaiseConfig(ACatalog.FileName,
        Format('entries[%d] is not an object', [I]));
    Entry := AArr.Items[I] as TJSONObject;
    IDStr := GetJSONString(Entry, 'id', '');
    if not ParseHexOrInt(IDStr, IDNum) then
      RaiseConfig(ACatalog.FileName,
        Format('entries[%d].id "%s" is not a number', [I, IDStr]));
    D.ID := IDNum;
    D.Name := GetJSONString(Entry, 'name', '');
    D.Length := Byte(GetJSONInt(Entry, 'length', 1));
    D.UnitName := GetJSONString(Entry, 'unit', '');
    D.DecoderName := GetJSONString(Entry, 'decoder', 'raw');
    D.Scale := GetJSONDouble(Entry, 'scale', 1);
    D.Offset := GetJSONDouble(Entry, 'offset', 0);
    D.Min := GetJSONDouble(Entry, 'min', 0);
    D.Max := GetJSONDouble(Entry, 'max', 0);
    D.Description := GetJSONString(Entry, 'description', '');
    ACatalog.PIDs.Add(D);
  end;
end;

procedure ParseTextEntries(ACatalog: TOBDCatalog; AArr: TJSONArray);
var
  I: Integer;
  Entry: TJSONObject;
  T: TOBDCatalogTextEntry;
  CodeStr: string;
  CodeNum: Cardinal;
begin
  for I := 0 to AArr.Count - 1 do
  begin
    if not (AArr.Items[I] is TJSONObject) then
      RaiseConfig(ACatalog.FileName,
        Format('entries[%d] is not an object', [I]));
    Entry := AArr.Items[I] as TJSONObject;
    CodeStr := GetJSONString(Entry, 'code', '');
    if not ParseHexOrInt(CodeStr, CodeNum) then
      RaiseConfig(ACatalog.FileName,
        Format('entries[%d].code "%s" is not a number', [I, CodeStr]));
    T.Code := CodeNum;
    T.Text := GetJSONString(Entry, 'text', '');
    ACatalog.Texts.Add(T);
  end;
end;

function TOBDCatalogStore.LoadFile(const AFileName: string): TOBDCatalog;
var
  Json: string;
  Doc: TJSONValue;
  Root: TJSONObject;
  EntriesArr: TJSONArray;
  Existing, NewCat: TOBDCatalog;
  I: Integer;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDConfig.CreateFmt('Catalogue file not found: %s', [AFileName]);

  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
  if Doc = nil then
    RaiseConfig(AFileName, 'invalid JSON');

  try
    if not (Doc is TJSONObject) then
      RaiseConfig(AFileName, 'root is not an object');
    Root := Doc as TJSONObject;

    NewCat := TOBDCatalog.Create(AFileName);
    try
      NewCat.Version := GetJSONInt(Root, 'version', 0);
      if NewCat.Version <> 1 then
        RaiseConfig(AFileName,
          Format('unsupported schema version %d (expected 1)',
            [NewCat.Version]));
      NewCat.TypeName := GetJSONString(Root, 'type', '');
      if NewCat.TypeName = '' then
        RaiseConfig(AFileName, '"type" field is required');
      NewCat.Kind := CatalogKindFromString(NewCat.TypeName);
      NewCat.Mode := GetJSONString(Root, 'mode', '');

      if not (Root.GetValue('entries') is TJSONArray) then
        RaiseConfig(AFileName, '"entries" must be an array');
      EntriesArr := Root.GetValue('entries') as TJSONArray;

      case NewCat.Kind of
        ckOBD2PID, ckUDSDID:
          ParsePIDEntries(NewCat, EntriesArr);
        ckOBD2DTC, ckUDSNRC,
        ckJ1939PGN, ckJ1939SPN, ckJ1939FMI:
          ParseTextEntries(NewCat, EntriesArr);
      else
        // Unknown / capabilities — store as text entries best-effort.
        ParseTextEntries(NewCat, EntriesArr);
      end;
    except
      NewCat.Free;
      raise;
    end;

    // Replace any existing catalogue loaded from the same path.
    for I := FCatalogs.Count - 1 downto 0 do
    begin
      Existing := FCatalogs[I];
      if SameText(Existing.FileName, AFileName) then
        FCatalogs.Delete(I);
    end;
    FCatalogs.Add(NewCat);
    Result := NewCat;
  finally
    Doc.Free;
  end;
end;

function TOBDCatalogStore.LoadDirectory(const ARoot: string): Integer;
var
  Files: TArray<string>;
  F: string;
begin
  Result := 0;
  if not TDirectory.Exists(ARoot) then
    Exit;
  Files := TDirectory.GetFiles(ARoot, '*.json', TSearchOption.soAllDirectories);
  for F in Files do
  begin
    LoadFile(F);
    Inc(Result);
  end;
end;

function TOBDCatalogStore.FindPID(const AMode: string; AID: Cardinal;
  out ADescriptor: TOBDPIDDescriptor): Boolean;
var
  I, J: Integer;
  Cat: TOBDCatalog;
  D: TOBDPIDDescriptor;
begin
  for I := 0 to FCatalogs.Count - 1 do
  begin
    Cat := FCatalogs[I];
    if Cat.Kind <> ckOBD2PID then
      Continue;
    if (AMode <> '') and not SameText(Cat.Mode, AMode) then
      Continue;
    for J := 0 to Cat.PIDs.Count - 1 do
    begin
      D := Cat.PIDs[J];
      if D.ID = AID then
      begin
        ADescriptor := D;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function TOBDCatalogStore.FindText(AKind: TOBDCatalogKind; ACode: Cardinal;
  out AText: string): Boolean;
var
  I, J: Integer;
  Cat: TOBDCatalog;
  T: TOBDCatalogTextEntry;
begin
  for I := 0 to FCatalogs.Count - 1 do
  begin
    Cat := FCatalogs[I];
    if Cat.Kind <> AKind then
      Continue;
    for J := 0 to Cat.Texts.Count - 1 do
    begin
      T := Cat.Texts[J];
      if T.Code = ACode then
      begin
        AText := T.Text;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

initialization

finalization
  TOBDCatalogStore.ReleaseDefault;

end.
