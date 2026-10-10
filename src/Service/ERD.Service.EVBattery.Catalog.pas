// ------------------------------------------------------------------------------
// ERD.Service.EVBattery.Catalog
//
// Loads per-vendor BMS DID maps from
// catalogs/ev-battery/<vendor>.json. One catalogue file per
// vendor; each catalogue carries the BMS ECU's CAN IDs plus
// an ordered list of decode rules.
//
// Threading: process-wide single instance, lock-protected on
// load. Lookups are read-only after the first vendor was
// resolved.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.Service.EVBattery.Catalog;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  ERD.Types,
  ERD.JSON,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.IOUtils,
  System.JSON,
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  ERD.Service.EVBattery.Types;

type
  TOBDEVBatteryCatalog = class
  strict private
    class var FCatalogs: TDictionary<string, TOBDEVBatteryVendorCatalog>;
    class var FLoaded: Boolean;
    class var FCatalogDir: string;
    class var FLock: TCriticalSection;
    class procedure EnsureLoaded; static;
    class procedure LoadFile(const AFile: string); static;
  public
    class constructor Create;
    class destructor Destroy;

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
    class procedure Register(const ACatalog
      : TOBDEVBatteryVendorCatalog); static;
  end;

implementation

class constructor TOBDEVBatteryCatalog.Create;
begin
  FLock := TCriticalSection.Create;
  FCatalogDir := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'catalogs');
  FCatalogs := TDictionary<string, TOBDEVBatteryVendorCatalog>.Create;
end;

class destructor TOBDEVBatteryCatalog.Destroy;
begin
  FreeAndNil(FCatalogs);
  FreeAndNil(FLock);
end;

class procedure TOBDEVBatteryCatalog.LoadFile(const AFile: string);
var
  Doc: TJSONObject;
  EcuObj: TJSONObject;
  Models: TJSONArray;
  RulesArr: TJSONArray;
  RuleObj: TJSONObject;
  Cat: TOBDEVBatteryVendorCatalog;
  Rule: TOBDEVBatteryRule;
  I: Integer;

  function HexInt(const ASource: TJSONObject; const AHexKey, ADecKey: string;
    ADefault: Integer = 0): Integer;
  var
    Hv, Dv: TJSONValue;
    Text: string;
    Value: Int64;
    C: Char;
    Limit: Int64;
  begin
    Hv := ASource.GetValue(AHexKey);
    Dv := ASource.GetValue(ADecKey);
    Value := ADefault;
    if Hv <> nil then
    begin
      Text := Hv.Value;
      if (Length(Text) < 3) or not SameText(Copy(Text, 1, 2), '0x') then
        raise EOBDConfig.Create('Invalid hexadecimal EV catalog value: '
          + AHexKey);
      Text := Copy(Text, 3, MaxInt);
      for C in Text do
        if not CharInSet(C, ['0' .. '9', 'A' .. 'F', 'a' .. 'f']) then
          raise EOBDConfig.Create('Invalid hexadecimal EV catalog value: '
            + AHexKey);
      if not TryStrToInt64('$' + Text, Value) then
        raise EOBDConfig.Create('EV catalog value overflows: ' + AHexKey);
    end
    else if (Dv <> nil) and not TryStrToInt64(Dv.Value, Value) then
      raise EOBDConfig.Create('Invalid integer EV catalog value: ' + ADecKey);
    Limit := $FFFF;
    if Pos('request_id', AHexKey) > 0 then
      Limit := $1FFFFFFF
    else if Pos('response_id', AHexKey) > 0 then
      Limit := $1FFFFFFF
    else if (Pos('extended_', AHexKey) > 0) or (AHexKey = 'service_hex') then
      Limit := $FF
    else if AHexKey = 'pid_hex' then
      Limit := $FF;
    if (Value < 0) or (Value > Limit) then
      raise EOBDConfig.Create('EV catalog value outside physical range: '
        + AHexKey);
    Result := Integer(Value);
  end;

  function StrField(AObj: TJSONObject; const AKey: string): string;
  var
    W: TJSONValue;
  begin
    W := AObj.GetValue(AKey);
    if W <> nil then
      Result := W.Value
    else
      Result := '';
  end;

  function FloatField(AObj: TJSONObject; const AKey: string;
    ADefault: Double): Double;
  var
    W: TJSONValue;
    FS: TFormatSettings;
  begin
    W := AObj.GetValue(AKey);
    if W = nil then
      Exit(ADefault);
    FS := TFormatSettings.Create('en-US');
    if not TryStrToFloat(W.Value, Result, FS) then
      raise EOBDConfig.Create('Invalid EV catalog number: ' + AKey);
  end;

  function IntField(AObj: TJSONObject; const AKey: string;
    ADefault: Integer): Integer;
  var
    W: TJSONValue;
  begin
    W := AObj.GetValue(AKey);
    if W = nil then
      Exit(ADefault);
    if not TryStrToInt(W.Value, Result) then
      raise EOBDConfig.Create('Invalid EV catalog integer: ' + AKey);
  end;

  function BoolField(AObj: TJSONObject; const AKey: string): Boolean;
  var
    W: TJSONValue;
  begin
    W := AObj.GetValue(AKey);
    Result := (W <> nil) and SameText(W.Value, 'true');
  end;

begin
  if not TFile.Exists(AFile) then
    Exit;
  Doc := ParseOBDJSONObject(TFile.ReadAllText(AFile, TEncoding.UTF8));
  if Doc = nil then
    Exit;
  try
    Cat := Default (TOBDEVBatteryVendorCatalog);
    Cat.Vendor := StrField(Doc, 'vendor');
    if Cat.Vendor = '' then
      Exit;
    Cat.Label_ := StrField(Doc, 'label');

    EcuObj := Doc.GetValue<TJSONObject>('ecu');
    if EcuObj <> nil then
    begin
      Cat.RequestId := HexInt(EcuObj, 'request_id_hex', 'request_id', 0);
      Cat.ResponseId := HexInt(EcuObj, 'response_id_hex', 'response_id', 0);
      Cat.UseExtendedAddressing := SameText(StrField(EcuObj, 'addressing'),
        'ISOTP_EXTADR');
      if (StrField(EcuObj, 'addressing') <> '') and
        not Cat.UseExtendedAddressing and
        not SameText(StrField(EcuObj, 'addressing'), 'ISOTP_NORMAL') then
        raise EOBDConfig.Create('Unsupported EV battery addressing mode');
      if Cat.UseExtendedAddressing then
      begin
        if (EcuObj.GetValue('extended_target_hex') = nil) or
          (EcuObj.GetValue('extended_tester_hex') = nil) then
          raise EOBDConfig.Create
            ('Extended EV routing requires destination and tester bytes');
        Cat.ExtendedTarget := HexInt(EcuObj, 'extended_target_hex',
          'extended_target');
        Cat.ExtendedTester := HexInt(EcuObj, 'extended_tester_hex',
          'extended_tester');
      end;
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
        RuleObj := RequireOBDJSONObject(RulesArr.Items[I]);
        Rule := Default (TOBDEVBatteryRule);
        Rule.FieldName := StrField(RuleObj, 'field');
        Rule.Field := FieldKindFromName(Rule.FieldName);
        Rule.Service := HexInt(RuleObj, 'service_hex', 'service', $22);
        Rule.DIDOrPID := HexInt(RuleObj, 'did_hex', 'did', 0);
        if Rule.DIDOrPID = 0 then
          Rule.DIDOrPID := HexInt(RuleObj, 'pid_hex', 'pid', 0);
        Rule.RequestId := HexInt(RuleObj, 'ecu_request_id_hex',
          'ecu_request_id', Cat.RequestId);
        Rule.ResponseId := HexInt(RuleObj, 'ecu_response_id_hex',
          'ecu_response_id', Cat.ResponseId);
        Rule.UseExtendedAddressing := Cat.UseExtendedAddressing;
        Rule.ExtendedTarget := HexInt(RuleObj, 'ecu_extended_target_hex',
          'ecu_extended_target', Cat.ExtendedTarget);
        Rule.ExtendedTester := HexInt(RuleObj, 'ecu_extended_tester_hex',
          'ecu_extended_tester', Cat.ExtendedTester);
        Rule.MinModelYear := IntField(RuleObj, 'min_model_year', 0);
        Rule.MaxModelYear := IntField(RuleObj, 'max_model_year', 0);
        if (Rule.MinModelYear < 0) or (Rule.MaxModelYear < 0) or
          ((Rule.MaxModelYear > 0) and (Rule.MaxModelYear < Rule.MinModelYear))
        then
          raise EOBDConfig.Create('Invalid EV model-year range');
        Rule.Offset := IntField(RuleObj, 'offset', 0);
        Rule.Length := IntField(RuleObj, 'length', 1);
        Rule.Signed := BoolField(RuleObj, 'signed');
        Rule.Scale := FloatField(RuleObj, 'scale', 1.0);
        Rule.OffsetVal := FloatField(RuleObj, 'offset_value', 0.0);
        Rule.Unit_ := StrField(RuleObj, 'unit');
        Rule.Source := StrField(RuleObj, 'source');
        Rule.IsArray := BoolField(RuleObj, 'array');
        Rule.ElementSize := IntField(RuleObj, 'element_size', 1);
        if (Rule.Offset < 0) or (Rule.Length < 1) or
          ((not Rule.IsArray) and (Rule.Length > 8)) or (Rule.ElementSize < 1)
          or (Rule.ElementSize > 8) then
          raise EOBDConfig.Create('Invalid EV catalog byte slice');
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
  if FLoaded then
    Exit;
  Reload;
end;

class procedure TOBDEVBatteryCatalog.Reload;
var
  Dir: string;
  Files: TArray<string>;
  F: string;
begin
  FLock.Enter;
  try
    FLoaded := False;
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
    FLock.Leave;
  end;
end;

class function TOBDEVBatteryCatalog.TryGet(const AVendor: string;
  out AOut: TOBDEVBatteryVendorCatalog): Boolean;
begin
  EnsureLoaded;
  FLock.Enter;
  try
    Result := FCatalogs.TryGetValue(LowerCase(AVendor), AOut);
    if not Result then
      AOut := Default (TOBDEVBatteryVendorCatalog);
  finally
    FLock.Leave;
  end;
end;

class function TOBDEVBatteryCatalog.VendorKeys: TArray<string>;
var
  Acc: TList<string>;
  K: string;
begin
  EnsureLoaded;
  FLock.Enter;
  Acc := TList<string>.Create;
  try
    for K in FCatalogs.Keys do
      Acc.Add(K);
    Result := Acc.ToArray;
  finally
    Acc.Free;
    FLock.Leave;
  end;
end;

class procedure TOBDEVBatteryCatalog.Register(const ACatalog
  : TOBDEVBatteryVendorCatalog);
begin
  EnsureLoaded;
  FLock.Enter;
  try
    FCatalogs.AddOrSetValue(LowerCase(ACatalog.Vendor), ACatalog);
  finally
    FLock.Leave;
  end;
end;

end.
