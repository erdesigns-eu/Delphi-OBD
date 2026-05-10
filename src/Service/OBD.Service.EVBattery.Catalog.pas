//------------------------------------------------------------------------------
//  OBD.Service.EVBattery.Catalog
//
//  Loads per-vendor BMS DID maps from
//  catalogs/ev-battery/<vendor>.json. One catalogue file per
//  vendor; each catalogue carries the BMS ECU's CAN IDs plus
//  an ordered list of decode rules.
//
//  Threading: process-wide single instance, lock-protected on
//  load. Lookups are read-only after the first vendor was
//  resolved.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Service.EVBattery.Catalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  OBD.Service.EVBattery.Types;

type
  TOBDEVBatteryCatalog = class
  strict private
    class var FCatalogs:   TDictionary<string, TOBDEVBatteryVendorCatalog>;
    class var FLoaded:     Boolean;
    class var FCatalogDir: string;
    class var FLock:       TObject;
    class procedure EnsureLoaded; static;
    class procedure LoadFile(const AFile: string); static;
  public
    class constructor Create;
    class destructor  Destroy;

    /// <summary>Catalogue root directory. Defaults to
    /// <c>catalogs/</c> next to the executable.</summary>
    class property CatalogDir: string read FCatalogDir write FCatalogDir;

    /// <summary>(Re)load every <c>ev-battery/*.json</c> file
    /// under <see cref="CatalogDir"/>.</summary>
    class procedure Reload; static;

    /// <summary>Returns the catalogue for the given vendor key
    /// (e.g. <c>"hmg"</c>). Sets <c>AOut</c> to a default-zero
    /// record and returns False when not registered.</summary>
    class function TryGet(const AVendor: string;
      out AOut: TOBDEVBatteryVendorCatalog): Boolean; static;

    /// <summary>Vendor keys with a catalogue registered.</summary>
    class function VendorKeys: TArray<string>; static;

    /// <summary>Inject a catalogue at runtime (overrides any
    /// JSON file with the same vendor key).</summary>
    class procedure Register(const ACatalog: TOBDEVBatteryVendorCatalog); static;
  end;

implementation

class constructor TOBDEVBatteryCatalog.Create;
begin
  FLock := TObject.Create;
  FCatalogDir := TPath.Combine(
    TPath.GetDirectoryName(ParamStr(0)), 'catalogs');
  FCatalogs := TDictionary<string, TOBDEVBatteryVendorCatalog>.Create;
end;

class destructor TOBDEVBatteryCatalog.Destroy;
begin
  FreeAndNil(FCatalogs);
  FreeAndNil(FLock);
end;

class procedure TOBDEVBatteryCatalog.LoadFile(const AFile: string);
var
  Doc:   TJSONObject;
  EcuObj: TJSONObject;
  Models: TJSONArray;
  RulesArr: TJSONArray;
  RuleObj:  TJSONObject;
  Cat:    TOBDEVBatteryVendorCatalog;
  Rule:   TOBDEVBatteryRule;
  V:      TJSONValue;
  I:      Integer;

  function HexInt(const ASource: TJSONObject;
    const AHexKey, ADecKey: string; ADefault: Integer = 0): Integer;
  var Hv, Dv: TJSONValue;
  begin
    Hv := ASource.GetValue(AHexKey);
    if Hv <> nil then Exit(StrToIntDef('$' + StringReplace(Hv.Value,
                                                            '0x', '', [rfIgnoreCase]), ADefault));
    Dv := ASource.GetValue(ADecKey);
    if Dv <> nil then Exit(StrToIntDef(Dv.Value, ADefault));
    Result := ADefault;
  end;

  function StrField(AObj: TJSONObject; const AKey: string): string;
  var W: TJSONValue;
  begin
    W := AObj.GetValue(AKey);
    if W <> nil then Result := W.Value else Result := '';
  end;

  function FloatField(AObj: TJSONObject; const AKey: string;
    ADefault: Double): Double;
  var W: TJSONValue; FS: TFormatSettings;
  begin
    W := AObj.GetValue(AKey);
    if W = nil then Exit(ADefault);
    FS := TFormatSettings.Create('en-US');
    if not TryStrToFloat(W.Value, Result, FS) then Result := ADefault;
  end;

  function IntField(AObj: TJSONObject; const AKey: string;
    ADefault: Integer): Integer;
  var W: TJSONValue;
  begin
    W := AObj.GetValue(AKey);
    if W <> nil then Result := StrToIntDef(W.Value, ADefault)
    else            Result := ADefault;
  end;

  function BoolField(AObj: TJSONObject; const AKey: string): Boolean;
  var W: TJSONValue;
  begin
    W := AObj.GetValue(AKey);
    Result := (W <> nil) and SameText(W.Value, 'true');
  end;

begin
  if not TFile.Exists(AFile) then Exit;
  Doc := TJSONObject.ParseJSONValue(
    TFile.ReadAllText(AFile, TEncoding.UTF8)) as TJSONObject;
  if Doc = nil then Exit;
  try
    Cat := Default(TOBDEVBatteryVendorCatalog);
    Cat.Vendor := StrField(Doc, 'vendor');
    if Cat.Vendor = '' then Exit;
    Cat.Label_ := StrField(Doc, 'label');

    EcuObj := Doc.GetValue<TJSONObject>('ecu');
    if EcuObj <> nil then
    begin
      Cat.RequestId  := HexInt(EcuObj, 'request_id_hex',  'request_id',  0);
      Cat.ResponseId := HexInt(EcuObj, 'response_id_hex', 'response_id', 0);
    end;

    Models := Doc.GetValue<TJSONArray>('applicable_models');
    if Models <> nil then
    begin
      SetLength(Cat.ApplicableModels, Models.Count);
      for I := 0 to Models.Count - 1 do
        Cat.ApplicableModels[I] := Models.Items[I].Value;
    end;

    RulesArr := Doc.GetValue<TJSONArray>('fields');
    if RulesArr <> nil then
    begin
      SetLength(Cat.Rules, RulesArr.Count);
      for I := 0 to RulesArr.Count - 1 do
      begin
        RuleObj := RulesArr.Items[I] as TJSONObject;
        Rule := Default(TOBDEVBatteryRule);
        Rule.FieldName   := StrField(RuleObj, 'field');
        Rule.Field       := FieldKindFromName(Rule.FieldName);
        Rule.Service     := HexInt(RuleObj, 'service_hex', 'service', $22);
        Rule.DIDOrPID    := HexInt(RuleObj, 'did_hex',     'did',     0);
        if Rule.DIDOrPID = 0 then
          Rule.DIDOrPID  := HexInt(RuleObj, 'pid_hex', 'pid', 0);
        Rule.Offset      := IntField(RuleObj, 'offset', 0);
        Rule.Length      := IntField(RuleObj, 'length', 1);
        Rule.Signed      := BoolField(RuleObj, 'signed');
        Rule.Scale       := FloatField(RuleObj, 'scale', 1.0);
        Rule.OffsetVal   := FloatField(RuleObj, 'offset_value', 0.0);
        Rule.Unit_       := StrField(RuleObj, 'unit');
        Rule.Source      := StrField(RuleObj, 'source');
        Rule.IsArray     := BoolField(RuleObj, 'array');
        Rule.ElementSize := IntField(RuleObj, 'element_size', 1);
        Cat.Rules[I] := Rule;
      end;
    end;

    FCatalogs.AddOrSetValue(LowerCase(Cat.Vendor), Cat);
  finally
    Doc.Free;
  end;
end;

class procedure TOBDEVBatteryCatalog.EnsureLoaded;
begin
  if FLoaded then Exit;
  Reload;
end;

class procedure TOBDEVBatteryCatalog.Reload;
var
  Dir:   string;
  Files: TArray<string>;
  F:     string;
begin
  TMonitor.Enter(FLock);
  try
    FCatalogs.Clear;
    Dir := TPath.Combine(FCatalogDir, 'ev-battery');
    if TDirectory.Exists(Dir) then
    begin
      Files := TDirectory.GetFiles(Dir, '*.json');
      for F in Files do
        LoadFile(F);
    end;
    FLoaded := True;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class function TOBDEVBatteryCatalog.TryGet(const AVendor: string;
  out AOut: TOBDEVBatteryVendorCatalog): Boolean;
begin
  EnsureLoaded;
  Result := FCatalogs.TryGetValue(LowerCase(AVendor), AOut);
  if not Result then AOut := Default(TOBDEVBatteryVendorCatalog);
end;

class function TOBDEVBatteryCatalog.VendorKeys: TArray<string>;
var Acc: TList<string>; K: string;
begin
  EnsureLoaded;
  Acc := TList<string>.Create;
  try
    for K in FCatalogs.Keys do Acc.Add(K);
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

class procedure TOBDEVBatteryCatalog.Register(
  const ACatalog: TOBDEVBatteryVendorCatalog);
begin
  EnsureLoaded;
  TMonitor.Enter(FLock);
  try
    FCatalogs.AddOrSetValue(LowerCase(ACatalog.Vendor), ACatalog);
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.
