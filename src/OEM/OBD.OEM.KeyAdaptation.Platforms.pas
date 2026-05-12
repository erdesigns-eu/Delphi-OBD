//------------------------------------------------------------------------------
//  OBD.OEM.KeyAdaptation.Platforms
//
//  Loads the OEM key-adaptation chassis / platform applicability
//  catalogues — <c>catalogs/key-platforms-ford.json</c>,
//  <c>catalogs/key-platforms-hmg.json</c>,
//  <c>catalogs/key-platforms-toyota.json</c> — and exposes a
//  per-OEM lookup so a host can decide whether a given chassis
//  can be unlocked over OBD, requires a PIN, or is gateway-
//  locked behind a dealer tool.
//
//  Each catalogue entry carries:
//    - <c>chassis_key</c> / <c>platform_key</c>   short identifier
//    - <c>display_name</c>                        human label
//    - <c>access</c>                              one of the
//                                                 OEM-specific
//                                                 access enums
//    - <c>notes</c>                               free-form text
//
//  Lookup is case-insensitive on the platform key. Unknown
//  platforms return a "gateway-locked / certificate-required"
//  fail-safe so callers default to refusing destructive
//  operations.
//
//  Catalogues are loaded once at unit initialisation. They are
//  optional: missing or malformed files leave the registries
//  empty (production deployments lint the catalogues at build
//  time).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.KeyAdaptation.Platforms;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>Ford chassis access classification.</summary>
  TFordPlatformAccess = (
    /// <summary>Open via OBD — no PIN or dealer tool
    /// required.</summary>
    fpaOpen,
    /// <summary>Requires an incode / outcode PIN
    /// pair.</summary>
    fpaPinRequired,
    /// <summary>Gateway-locked; needs Ford IDS / FDRS or a
    /// licensed equivalent.</summary>
    fpaGatewayLocked);

  /// <summary>One Ford chassis entry from
  /// <c>key-platforms-ford.json</c>.</summary>
  TFordPlatformInfo = record
    /// <summary>Lower-cased chassis key
    /// (e.g. <c>p552</c>).</summary>
    Key: string;
    /// <summary>Human label.</summary>
    DisplayName: string;
    /// <summary>Access classification.</summary>
    Access: TFordPlatformAccess;
    /// <summary>Free-form notes.</summary>
    Notes: string;
  end;

  /// <summary>HMG (Hyundai / Kia / Genesis) platform access
  /// classification.</summary>
  THMGPlatformAccess = (
    /// <summary>Open with a dealer-label or GDS-derived
    /// PIN.</summary>
    hpaOpenWithPIN,
    /// <summary>Gateway-protected on post-MY2020 platforms;
    /// dealer SST required.</summary>
    hpaGatewayLockedPostMY2020,
    /// <summary>Genesis-only certificate required; out of
    /// scope for plain OBD.</summary>
    hpaCertificateRequired);

  /// <summary>One HMG platform entry from
  /// <c>key-platforms-hmg.json</c>.</summary>
  THMGPlatformInfo = record
    /// <summary>Lower-cased platform key
    /// (e.g. <c>jf</c>, <c>ev_e_gmp</c>).</summary>
    Key: string;
    /// <summary>Human label.</summary>
    DisplayName: string;
    /// <summary>Access classification.</summary>
    Access: THMGPlatformAccess;
    /// <summary>Free-form notes.</summary>
    Notes: string;
  end;

  /// <summary>Toyota chassis access classification.</summary>
  TToyotaPlatformAccess = (
    /// <summary>Programs a new key by presenting the existing
    /// master key.</summary>
    tpaMasterKey,
    /// <summary>Requires a PIN from Techstream / dealer
    /// procedure.</summary>
    tpaPin,
    /// <summary>Certificate-bound; gateway-locked.</summary>
    tpaCertificateRequired);

  /// <summary>One Toyota chassis entry from
  /// <c>key-platforms-toyota.json</c>.</summary>
  TToyotaPlatformInfo = record
    /// <summary>Lower-cased chassis key.</summary>
    Key: string;
    /// <summary>Human label.</summary>
    DisplayName: string;
    /// <summary>Access classification.</summary>
    Access: TToyotaPlatformAccess;
    /// <summary>Free-form notes.</summary>
    Notes: string;
  end;

/// <summary>Looks up a Ford chassis. Returns the entry from
/// the catalogue when known; otherwise a synthetic
/// <c>fpaGatewayLocked</c> default so callers fail safe on
/// unknown chassis.</summary>
/// <param name="ChassisKey">Chassis key (case-insensitive).</param>
function FindFordPlatform(const ChassisKey: string): TFordPlatformInfo;

/// <summary>Looks up an HMG platform. Unknown platforms get a
/// synthetic <c>hpaCertificateRequired</c> default.</summary>
/// <param name="PlatformKey">Platform key
/// (case-insensitive).</param>
function FindHMGPlatform(const PlatformKey: string): THMGPlatformInfo;

/// <summary>Looks up a Toyota chassis. Unknown chassis get a
/// synthetic <c>tpaCertificateRequired</c> default.</summary>
/// <param name="ChassisKey">Chassis key (case-insensitive).</param>
function FindToyotaPlatform(
  const ChassisKey: string): TToyotaPlatformInfo;

/// <summary>Drops the cached catalogues and reloads from disk.
/// Use after replacing a catalogue file at runtime; the
/// production lifecycle loads once at startup.</summary>
procedure ReloadKeyPlatformCatalogues;

implementation

uses
  System.Classes,
  System.JSON,
  System.IOUtils,
  OBD.OEM.Catalog.Loader;

var
  GFordPlatforms:   TDictionary<string, TFordPlatformInfo>   = nil;
  GHMGPlatforms:    TDictionary<string, THMGPlatformInfo>    = nil;
  GToyotaPlatforms: TDictionary<string, TToyotaPlatformInfo> = nil;

function FordAccessFromString(const S: string): TFordPlatformAccess;
begin
  if SameText(S, 'open')         then Exit(fpaOpen);
  if SameText(S, 'pin_required') then Exit(fpaPinRequired);
  // Anything else (incl. 'gateway_locked' and unknown values) is
  // treated as gateway-locked — fail-safe default that prevents
  // a host from issuing destructive routines blindly.
  Result := fpaGatewayLocked;
end;

function HMGAccessFromString(const S: string): THMGPlatformAccess;
begin
  if SameText(S, 'open_with_pin')                then Exit(hpaOpenWithPIN);
  if SameText(S, 'gateway_locked_post_my2020')   then Exit(hpaGatewayLockedPostMY2020);
  Result := hpaCertificateRequired;
end;

function ToyotaAccessFromString(const S: string): TToyotaPlatformAccess;
begin
  if SameText(S, 'master_key') then Exit(tpaMasterKey);
  if SameText(S, 'pin')        then Exit(tpaPin);
  Result := tpaCertificateRequired;
end;

procedure ReadEntriesArray(const FileName: string;
  Visit: TProc<TJSONObject>);
var
  Path, Raw: string;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
begin
  Path := ResolveCatalogPath(FileName);
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
      Arr := TJSONObject(Doc).GetValue<TJSONArray>('entries');
      if Arr = nil then
        Exit;
      for Item in Arr do
        if Item is TJSONObject then
          Visit(TJSONObject(Item));
    finally
      Doc.Free;
    end;
  except
    // Malformed file: silently leave the registry untouched so
    // application startup is not blocked. Build-time linting
    // catches schema breakage.
  end;
end;

procedure LoadFordCatalog;
begin
  GFordPlatforms.Clear;
  ReadEntriesArray('key-platforms-ford.json',
    procedure(Obj: TJSONObject)
    var
      Info: TFordPlatformInfo;
    begin
      Info := Default(TFordPlatformInfo);
      Info.Key := LowerCase(Obj.GetValue<string>('chassis_key', ''));
      if Info.Key = '' then
        Exit;
      Info.DisplayName := Obj.GetValue<string>('display_name', '');
      Info.Access      := FordAccessFromString(
                            Obj.GetValue<string>('access', ''));
      Info.Notes       := Obj.GetValue<string>('notes', '');
      GFordPlatforms.AddOrSetValue(Info.Key, Info);
    end);
end;

procedure LoadHMGCatalog;
begin
  GHMGPlatforms.Clear;
  ReadEntriesArray('key-platforms-hmg.json',
    procedure(Obj: TJSONObject)
    var
      Info: THMGPlatformInfo;
    begin
      Info := Default(THMGPlatformInfo);
      Info.Key := LowerCase(Obj.GetValue<string>('platform_key', ''));
      if Info.Key = '' then
        Exit;
      Info.DisplayName := Obj.GetValue<string>('display_name', '');
      Info.Access      := HMGAccessFromString(
                            Obj.GetValue<string>('access', ''));
      Info.Notes       := Obj.GetValue<string>('notes', '');
      GHMGPlatforms.AddOrSetValue(Info.Key, Info);
    end);
end;

procedure LoadToyotaCatalog;
begin
  GToyotaPlatforms.Clear;
  ReadEntriesArray('key-platforms-toyota.json',
    procedure(Obj: TJSONObject)
    var
      Info: TToyotaPlatformInfo;
    begin
      Info := Default(TToyotaPlatformInfo);
      Info.Key := LowerCase(Obj.GetValue<string>('chassis_key', ''));
      if Info.Key = '' then
        Exit;
      Info.DisplayName := Obj.GetValue<string>('display_name', '');
      Info.Access      := ToyotaAccessFromString(
                            Obj.GetValue<string>('access', ''));
      Info.Notes       := Obj.GetValue<string>('notes', '');
      GToyotaPlatforms.AddOrSetValue(Info.Key, Info);
    end);
end;

function FindFordPlatform(const ChassisKey: string): TFordPlatformInfo;
var
  Lookup: string;
begin
  Lookup := LowerCase(ChassisKey);
  if (GFordPlatforms <> nil)
     and GFordPlatforms.TryGetValue(Lookup, Result) then
    Exit;
  Result := Default(TFordPlatformInfo);
  Result.Key := Lookup;
  Result.DisplayName := ChassisKey;
  Result.Access := fpaGatewayLocked;
  Result.Notes := 'Unknown chassis; assume gateway-locked.';
end;

function FindHMGPlatform(const PlatformKey: string): THMGPlatformInfo;
var
  Lookup: string;
begin
  Lookup := LowerCase(PlatformKey);
  if (GHMGPlatforms <> nil)
     and GHMGPlatforms.TryGetValue(Lookup, Result) then
    Exit;
  Result := Default(THMGPlatformInfo);
  Result.Key := Lookup;
  Result.DisplayName := PlatformKey;
  Result.Access := hpaCertificateRequired;
  Result.Notes := 'Unknown platform; assume certificate-required.';
end;

function FindToyotaPlatform(
  const ChassisKey: string): TToyotaPlatformInfo;
var
  Lookup: string;
begin
  Lookup := LowerCase(ChassisKey);
  if (GToyotaPlatforms <> nil)
     and GToyotaPlatforms.TryGetValue(Lookup, Result) then
    Exit;
  Result := Default(TToyotaPlatformInfo);
  Result.Key := Lookup;
  Result.DisplayName := ChassisKey;
  Result.Access := tpaCertificateRequired;
  Result.Notes := 'Unknown chassis; assume certificate-required.';
end;

procedure ReloadKeyPlatformCatalogues;
begin
  LoadFordCatalog;
  LoadHMGCatalog;
  LoadToyotaCatalog;
end;

initialization
  GFordPlatforms   := TDictionary<string, TFordPlatformInfo>.Create;
  GHMGPlatforms    := TDictionary<string, THMGPlatformInfo>.Create;
  GToyotaPlatforms := TDictionary<string, TToyotaPlatformInfo>.Create;
  ReloadKeyPlatformCatalogues;

finalization
  GFordPlatforms.Free;
  GHMGPlatforms.Free;
  GToyotaPlatforms.Free;

end.
