//------------------------------------------------------------------------------
//  OBD.Adapter.Capabilities
//
//  Adapter-capability registry. Maps an adapter-key (e.g. <c>'elm327'</c>,
//  <c>'obdlink_mx'</c>) to a <see cref="TOBDAdapterCapabilities"/> set
//  and a <c>MaxIsoTpFrameBytes</c> hint. Loads from
//  <c>catalogs/adapter/capabilities.json</c> with a small in-source
//  fallback for offline / no-catalogue deployments.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation: registry + JSON loader + built-in
//                     fallback rows for ELM327 / OBDLink MX / OBDLink EX /
//                     J2534 / DoIP.
//------------------------------------------------------------------------------

unit OBD.Adapter.Capabilities;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  OBD.Types,
  OBD.Adapter.Types;

type
  /// <summary>
  ///   Single row in the adapter-capability registry.
  /// </summary>
  TOBDAdapterCapabilityEntry = record
    /// <summary>Stable lower-case identifier (catalogue key).</summary>
    AdapterKey: string;
    /// <summary>Display name for UIs.</summary>
    DisplayName: string;
    /// <summary>Family this adapter is grouped under.</summary>
    Family: TOBDAdapterFamily;
    /// <summary>Set of declared capabilities.</summary>
    Capabilities: TOBDAdapterCapabilities;
    /// <summary>Largest ISO-TP frame this adapter can handle in bytes
    /// (7 for classic CAN, 62 for CAN-FD, larger for DoIP). 0 means
    /// unknown.</summary>
    MaxIsoTpFrameBytes: Cardinal;
  end;

  /// <summary>
  ///   Process-wide adapter-capability registry.
  /// </summary>
  /// <remarks>
  ///   Lookup is case-insensitive on <c>AdapterKey</c>. Built-in
  ///   entries are seeded in this unit's initialisation; additional
  ///   entries can be loaded from JSON via <see cref="LoadFromJSON"/>.
  /// </remarks>
  TOBDAdapterCapabilityRegistry = class
  strict private
    class var FInstance: TOBDAdapterCapabilityRegistry;
    FEntries: TList<TOBDAdapterCapabilityEntry>;
    procedure SeedBuiltins;
    function FamilyForKey(const AKey: string): TOBDAdapterFamily;
  public
    /// <summary>Constructs an empty registry.</summary>
    constructor Create;
    /// <summary>Releases internal storage.</summary>
    destructor Destroy; override;

    /// <summary>The shared process-wide instance.</summary>
    /// <returns>Singleton; created on first call.</returns>
    class function Default: TOBDAdapterCapabilityRegistry;
    /// <summary>Releases the shared instance.</summary>
    class procedure ReleaseDefault;

    /// <summary>
    ///   Loads or merges entries from a JSON catalogue file.
    /// </summary>
    /// <param name="AFileName">Path to <c>capabilities.json</c>.</param>
    /// <returns>Number of entries loaded.</returns>
    /// <exception cref="EOBDConfig">File missing or malformed JSON.</exception>
    function LoadFromJSON(const AFileName: string): Integer;

    /// <summary>Removes every entry, including built-ins.</summary>
    procedure Clear;
    /// <summary>Number of registered entries.</summary>
    function Count: Integer;
    /// <summary>Indexed access.</summary>
    function GetEntry(AIndex: Integer): TOBDAdapterCapabilityEntry;
    /// <summary>Adds (or replaces) an entry.</summary>
    /// <param name="AEntry">Entry to register. <c>AdapterKey</c> must
    /// be non-empty.</param>
    /// <exception cref="EOBDConfig"><c>AdapterKey</c> empty.</exception>
    procedure Register(const AEntry: TOBDAdapterCapabilityEntry);
    /// <summary>Looks up by adapter-key (case-insensitive).</summary>
    /// <param name="AAdapterKey">Key (e.g. <c>'elm327'</c>).</param>
    /// <param name="AEntry">Output entry, valid only when True.</param>
    /// <returns>True when a matching entry exists.</returns>
    function TryFind(const AAdapterKey: string;
      out AEntry: TOBDAdapterCapabilityEntry): Boolean;
  end;

implementation

const
  CAPABILITY_JSON_TYPE = 'adapter-capabilities';

{ ---- TOBDAdapterCapabilityRegistry ------------------------------------------- }

constructor TOBDAdapterCapabilityRegistry.Create;
begin
  inherited;
  FEntries := TList<TOBDAdapterCapabilityEntry>.Create;
  SeedBuiltins;
end;

destructor TOBDAdapterCapabilityRegistry.Destroy;
begin
  FEntries.Free;
  inherited;
end;

class function TOBDAdapterCapabilityRegistry.Default: TOBDAdapterCapabilityRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDAdapterCapabilityRegistry.Create;
  Result := FInstance;
end;

class procedure TOBDAdapterCapabilityRegistry.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDAdapterCapabilityRegistry.Clear;
begin
  FEntries.Clear;
end;

function TOBDAdapterCapabilityRegistry.Count: Integer;
begin
  Result := FEntries.Count;
end;

function TOBDAdapterCapabilityRegistry.GetEntry(
  AIndex: Integer): TOBDAdapterCapabilityEntry;
begin
  Result := FEntries[AIndex];
end;

function TOBDAdapterCapabilityRegistry.FamilyForKey(
  const AKey: string): TOBDAdapterFamily;
var
  Lower: string;
begin
  Lower := LowerCase(AKey);
  if Pos('obdlink', Lower) > 0 then
    Exit(afOBDLink);
  if (Pos('j2534', Lower) > 0) or (Pos('passthru', Lower) > 0) then
    Exit(afJ2534);
  if Pos('doip', Lower) > 0 then
    Exit(afDoIP);
  Result := afELM327;
end;

procedure TOBDAdapterCapabilityRegistry.Register(
  const AEntry: TOBDAdapterCapabilityEntry);
var
  I: Integer;
  Norm: TOBDAdapterCapabilityEntry;
begin
  if Trim(AEntry.AdapterKey) = '' then
    raise EOBDConfig.Create('AdapterKey cannot be empty');
  Norm := AEntry;
  Norm.AdapterKey := LowerCase(Trim(Norm.AdapterKey));
  for I := FEntries.Count - 1 downto 0 do
    if SameText(FEntries[I].AdapterKey, Norm.AdapterKey) then
      FEntries.Delete(I);
  FEntries.Add(Norm);
end;

function TOBDAdapterCapabilityRegistry.TryFind(const AAdapterKey: string;
  out AEntry: TOBDAdapterCapabilityEntry): Boolean;
var
  Needle: string;
  I: Integer;
begin
  Needle := LowerCase(Trim(AAdapterKey));
  for I := 0 to FEntries.Count - 1 do
    if SameText(FEntries[I].AdapterKey, Needle) then
    begin
      AEntry := FEntries[I];
      Exit(True);
    end;
  Result := False;
end;

procedure TOBDAdapterCapabilityRegistry.SeedBuiltins;
  procedure Add(const AKey, ADisplay: string; AFamily: TOBDAdapterFamily;
    ACaps: TOBDAdapterCapabilities; AMaxIsoTp: Cardinal);
  var
    E: TOBDAdapterCapabilityEntry;
  begin
    E.AdapterKey := AKey;
    E.DisplayName := ADisplay;
    E.Family := AFamily;
    E.Capabilities := ACaps;
    E.MaxIsoTpFrameBytes := AMaxIsoTp;
    Register(E);
  end;
begin
  Add('elm327',     'ELM327',     afELM327,
    [acCAN, acISOTP, acKLine, acVoltageMonitor, acProgrammableHeaders],
    7);
  Add('obdlink_mx', 'OBDLink MX', afOBDLink,
    [acCAN, acISOTP, acKLine, acVoltageMonitor, acProgrammableHeaders,
     acSTCommands, acMessageFilters, acAdapterMultiFrame],
    7);
  Add('obdlink_ex', 'OBDLink EX', afOBDLink,
    [acCAN, acISOTP, acKLine, acVoltageMonitor, acProgrammableHeaders,
     acSTCommands, acMessageFilters, acAdapterMultiFrame, acJ1939],
    7);
  Add('obdlink_cx', 'OBDLink CX', afOBDLink,
    [acCAN, acCANFD, acISOTP, acISOTPLongFrame, acKLine, acVoltageMonitor,
     acProgrammableHeaders, acSTCommands, acMessageFilters,
     acAdapterMultiFrame, acJ1939],
    62);
  Add('j2534',      'SAE J2534 PassThru', afJ2534,
    [acCAN, acISOTP, acISOTPLongFrame, acJ1939, acKLine, acJ2534,
     acMessageFilters, acAdapterMultiFrame],
    62);
  Add('j2534v2',    'SAE J2534-2 PassThru', afJ2534,
    [acCAN, acCANFD, acISOTP, acISOTPLongFrame, acJ1939, acKLine,
     acJ2534, acJ2534v2, acMessageFilters, acAdapterMultiFrame],
    62);
  Add('doip',       'Diagnostics over IP gateway', afDoIP,
    [acDoIP, acISOTPLongFrame, acAdapterMultiFrame],
    1500);
end;

function TOBDAdapterCapabilityRegistry.LoadFromJSON(
  const AFileName: string): Integer;
var
  Json: string;
  Doc: TJSONValue;
  Root: TJSONObject;
  EntriesArr: TJSONArray;
  TypeStr: string;
  Version: Integer;
  I, J: Integer;
  EntryObj: TJSONObject;
  CapsArr: TJSONArray;
  CapStr: string;
  Cap: TOBDAdapterCapability;
  E: TOBDAdapterCapabilityEntry;
  KeyVal: TJSONValue;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDConfig.CreateFmt('Adapter capability file not found: %s',
      [AFileName]);

  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
  if Doc = nil then
    raise EOBDConfig.CreateFmt('%s: invalid JSON', [AFileName]);

  Result := 0;
  try
    if not (Doc is TJSONObject) then
      raise EOBDConfig.CreateFmt('%s: root is not an object', [AFileName]);
    Root := Doc as TJSONObject;

    KeyVal := Root.GetValue('version');
    if (KeyVal <> nil) and (KeyVal is TJSONNumber) then
      Version := (KeyVal as TJSONNumber).AsInt
    else
      Version := 1;
    if Version <> 1 then
      raise EOBDConfig.CreateFmt('%s: unsupported schema version %d',
        [AFileName, Version]);

    KeyVal := Root.GetValue('type');
    if KeyVal <> nil then
      TypeStr := KeyVal.Value
    else
      TypeStr := CAPABILITY_JSON_TYPE;
    if not SameText(TypeStr, CAPABILITY_JSON_TYPE) then
      raise EOBDConfig.CreateFmt('%s: type "%s" is not "%s"',
        [AFileName, TypeStr, CAPABILITY_JSON_TYPE]);

    if not (Root.GetValue('entries') is TJSONArray) then
      raise EOBDConfig.CreateFmt('%s: "entries" must be an array',
        [AFileName]);
    EntriesArr := Root.GetValue('entries') as TJSONArray;

    for I := 0 to EntriesArr.Count - 1 do
    begin
      if not (EntriesArr.Items[I] is TJSONObject) then
        raise EOBDConfig.CreateFmt('%s: entries[%d] is not an object',
          [AFileName, I]);
      EntryObj := EntriesArr.Items[I] as TJSONObject;

      E.AdapterKey := '';
      E.DisplayName := '';
      E.Capabilities := [];
      E.MaxIsoTpFrameBytes := 0;

      KeyVal := EntryObj.GetValue('adapter_key');
      if KeyVal <> nil then E.AdapterKey := KeyVal.Value;
      if Trim(E.AdapterKey) = '' then
        raise EOBDConfig.CreateFmt('%s: entries[%d].adapter_key is empty',
          [AFileName, I]);

      KeyVal := EntryObj.GetValue('display_name');
      if KeyVal <> nil then E.DisplayName := KeyVal.Value;
      if E.DisplayName = '' then E.DisplayName := E.AdapterKey;

      KeyVal := EntryObj.GetValue('max_iso_tp_frame_bytes');
      if (KeyVal <> nil) and (KeyVal is TJSONNumber) then
        E.MaxIsoTpFrameBytes := (KeyVal as TJSONNumber).AsInt;

      E.Family := FamilyForKey(E.AdapterKey);

      if EntryObj.GetValue('capabilities') is TJSONArray then
      begin
        CapsArr := EntryObj.GetValue('capabilities') as TJSONArray;
        for J := 0 to CapsArr.Count - 1 do
        begin
          CapStr := CapsArr.Items[J].Value;
          if TryParseCapability(CapStr, Cap) then
            Include(E.Capabilities, Cap);
        end;
      end;

      Register(E);
      Inc(Result);
    end;
  finally
    Doc.Free;
  end;
end;

initialization

finalization
  TOBDAdapterCapabilityRegistry.ReleaseDefault;

end.
