//------------------------------------------------------------------------------
//  OBD.Service.Catalog
//
//  PID + DTC catalogue loader that consumes the JSON files shipped
//  under /catalogs/. Complements OBD.Catalog (which uses a stricter
//  v1 schema) by accepting the richer manufacturer-PID and DTC
//  shapes already in the repository:
//
//    obd2-pids.json — { "dids": [ { "did": "0x000C", "name": ...,
//                                     "decoder": { "kind": "uint16_be",
//                                                  "scale": 0.25,
//                                                  "offset": 0,
//                                                  "unit": "rpm" } } ] }
//
//    dtc-*.json — { "dtcs": [ { "code": "P0301",
//                                      "description": "...",
//                                      "severity": "warning" } ] }
//
//  TOBDLiveData and TOBDDTCs consult these catalogues at runtime to
//  resolve names, units, decoder formulas and human-readable text.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up — JSON-driven catalogues.
//------------------------------------------------------------------------------

unit OBD.Service.Catalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.JSON,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>Decoder configuration carried by a PID entry.</summary>
  TOBDPIDDecoderInfo = record
    /// <summary>Decoder kind from the JSON: <c>uint8</c>,
    /// <c>uint16_be</c>, <c>int8</c>, <c>int16_be</c>, <c>hex</c>,
    /// <c>ascii</c>, <c>bitfield</c>, or <c>raw</c>.</summary>
    Kind: string;
    /// <summary>Linear-decode scale.</summary>
    Scale: Double;
    /// <summary>Linear-decode offset.</summary>
    Offset: Double;
    /// <summary>Engineering unit string (e.g. <c>rpm</c>,
    /// <c>km/h</c>, <c>C</c>).</summary>
    Unit_: string;
  end;

  /// <summary>Single PID entry from <c>obd2-pids.json</c>.</summary>
  TOBDPIDInfo = record
    PID: Word;
    Name: string;
    Description: string;
    Decoder: TOBDPIDDecoderInfo;
  end;

  /// <summary>Single DTC entry from a <c>dtc-*.json</c> catalogue.</summary>
  TOBDDtcInfo = record
    Code: string;
    Description: string;
    Severity: string;
  end;

  /// <summary>
  ///   Process-wide singleton holding the loaded PID and DTC
  ///   catalogues. Thread-safe.
  /// </summary>
  TOBDServiceCatalog = class
  strict private
    class var FInstance: TOBDServiceCatalog;
    FLock: TCriticalSection;
    FPIDs: TDictionary<Word, TOBDPIDInfo>;
    FDTCs: TDictionary<string, TOBDDtcInfo>;
    procedure DoLoadPIDFile(const AFileName: string);
    procedure DoLoadDTCFile(const AFileName: string);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>The shared instance.</summary>
    class function Default: TOBDServiceCatalog;
    class procedure ReleaseDefault;

    /// <summary>Loads a PID catalogue file (e.g. <c>obd2-pids.json</c>).
    /// Replaces existing entries with the same PID.</summary>
    /// <param name="AFileName">Path to the JSON file.</param>
    /// <exception cref="EOBDConfig">File missing or malformed JSON.</exception>
    procedure LoadPIDFile(const AFileName: string);
    /// <summary>Loads a DTC catalogue file (e.g.
    /// <c>dtc-renault.json</c>). Replaces existing entries with the
    /// same code.</summary>
    procedure LoadDTCFile(const AFileName: string);

    /// <summary>Loads every <c>*-pids.json</c> and <c>dtc-*.json</c>
    /// file under <c>ARoot</c>. Returns the number of files loaded.
    /// Stops on the first malformed file.</summary>
    function LoadDirectory(const ARoot: string): Integer;

    /// <summary>Removes every loaded entry.</summary>
    procedure Clear;

    /// <summary>Looks up a Mode 01 PID. Returns False when no entry
    /// exists.</summary>
    function TryGetPID(APID: Word; out AInfo: TOBDPIDInfo): Boolean;
    /// <summary>Looks up a DTC code. The lookup is case-sensitive
    /// against the catalogue (e.g. <c>"P0301"</c>).</summary>
    function TryGetDTC(const ACode: string;
      out AInfo: TOBDDtcInfo): Boolean;

    /// <summary>Number of loaded PID entries.</summary>
    function PIDCount: Integer;
    /// <summary>Number of loaded DTC entries.</summary>
    function DTCCount: Integer;
  end;

/// <summary>
///   Evaluates a decoder against raw response bytes. Returns False
///   when the decoder kind is non-numeric (<c>hex</c>, <c>ascii</c>,
///   <c>bitfield</c>, <c>raw</c>) or when the buffer is too short
///   for the requested kind.
/// </summary>
/// <param name="ADecoder">Decoder configuration.</param>
/// <param name="ARaw">Raw value bytes (after the PID echo).</param>
/// <param name="AValue">Output engineering value, valid only when
/// the function returns <c>True</c>.</param>
function EvaluatePIDDecoder(const ADecoder: TOBDPIDDecoderInfo;
  const ARaw: TBytes; out AValue: Double): Boolean;

implementation

uses
  System.StrUtils;

{ ---- helpers ---------------------------------------------------------------- }

function ParseHexWord(const AText: string): Word;
var
  S: string;
  I: Integer;
begin
  S := Trim(AText);
  if (Length(S) >= 2) and (S[1] = '0') and CharInSet(S[2], ['x', 'X']) then
    S := '$' + Copy(S, 3, MaxInt)
  else if (Length(S) = 0) or (S[1] <> '$') then
    S := '$' + S;
  if not TryStrToInt(S, I) then
    I := 0;
  Result := Word(I);
end;

function GetJSONStr(AObj: TJSONObject; const AKey, ADefault: string): string;
var
  V: TJSONValue;
begin
  V := AObj.GetValue(AKey);
  if V is TJSONString then Result := TJSONString(V).Value
  else Result := ADefault;
end;

function GetJSONNum(AObj: TJSONObject; const AKey: string;
  ADefault: Double): Double;
var
  V: TJSONValue;
  N: Double;
begin
  V := AObj.GetValue(AKey);
  if V is TJSONNumber then Result := TJSONNumber(V).AsDouble
  else if (V is TJSONString) and TryStrToFloat(TJSONString(V).Value, N,
    TFormatSettings.Invariant) then Result := N
  else Result := ADefault;
end;

{ ---- TOBDServiceCatalog ----------------------------------------------------- }

constructor TOBDServiceCatalog.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FPIDs := TDictionary<Word, TOBDPIDInfo>.Create;
  FDTCs := TDictionary<string, TOBDDtcInfo>.Create;
end;

destructor TOBDServiceCatalog.Destroy;
begin
  FDTCs.Free;
  FPIDs.Free;
  FLock.Free;
  inherited;
end;

class function TOBDServiceCatalog.Default: TOBDServiceCatalog;
begin
  if FInstance = nil then
    FInstance := TOBDServiceCatalog.Create;
  Result := FInstance;
end;

class procedure TOBDServiceCatalog.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDServiceCatalog.Clear;
begin
  FLock.Enter;
  try
    FPIDs.Clear;
    FDTCs.Clear;
  finally
    FLock.Leave;
  end;
end;

function TOBDServiceCatalog.PIDCount: Integer;
begin
  FLock.Enter;
  try Result := FPIDs.Count;
  finally FLock.Leave; end;
end;

function TOBDServiceCatalog.DTCCount: Integer;
begin
  FLock.Enter;
  try Result := FDTCs.Count;
  finally FLock.Leave; end;
end;

function TOBDServiceCatalog.TryGetPID(APID: Word;
  out AInfo: TOBDPIDInfo): Boolean;
begin
  FLock.Enter;
  try Result := FPIDs.TryGetValue(APID, AInfo);
  finally FLock.Leave; end;
end;

function TOBDServiceCatalog.TryGetDTC(const ACode: string;
  out AInfo: TOBDDtcInfo): Boolean;
begin
  FLock.Enter;
  try Result := FDTCs.TryGetValue(UpperCase(Trim(ACode)), AInfo);
  finally FLock.Leave; end;
end;

procedure TOBDServiceCatalog.DoLoadPIDFile(const AFileName: string);
var
  Doc: TJSONValue;
  Root: TJSONObject;
  Arr: TJSONArray;
  Entry, DecObj: TJSONObject;
  I: Integer;
  Info: TOBDPIDInfo;
  Json: string;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDConfig.CreateFmt('PID catalogue not found: %s', [AFileName]);
  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
  if not (Doc is TJSONObject) then
  begin
    Doc.Free;
    raise EOBDConfig.CreateFmt('PID catalogue %s: root is not an object',
      [AFileName]);
  end;
  try
    Root := Doc as TJSONObject;
    if not (Root.GetValue('dids') is TJSONArray) then Exit;
    Arr := Root.GetValue('dids') as TJSONArray;
    FLock.Enter;
    try
      for I := 0 to Arr.Count - 1 do
      begin
        if not (Arr.Items[I] is TJSONObject) then Continue;
        Entry := Arr.Items[I] as TJSONObject;
        Info := Default(TOBDPIDInfo);
        Info.PID := ParseHexWord(GetJSONStr(Entry, 'did', '0x0000'));
        Info.Name := GetJSONStr(Entry, 'name', '');
        Info.Description := GetJSONStr(Entry, 'description', '');
        Info.Decoder.Scale := 1;
        Info.Decoder.Offset := 0;
        if Entry.GetValue('decoder') is TJSONObject then
        begin
          DecObj := Entry.GetValue('decoder') as TJSONObject;
          Info.Decoder.Kind   := LowerCase(GetJSONStr(DecObj, 'kind', 'raw'));
          Info.Decoder.Scale  := GetJSONNum(DecObj, 'scale',  1);
          Info.Decoder.Offset := GetJSONNum(DecObj, 'offset', 0);
          Info.Decoder.Unit_  := GetJSONStr(DecObj, 'unit', '');
        end;
        FPIDs.AddOrSetValue(Info.PID, Info);
      end;
    finally
      FLock.Leave;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TOBDServiceCatalog.DoLoadDTCFile(const AFileName: string);
var
  Doc: TJSONValue;
  Root: TJSONObject;
  Arr: TJSONArray;
  Entry: TJSONObject;
  I: Integer;
  Info: TOBDDtcInfo;
  Json: string;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDConfig.CreateFmt('DTC catalogue not found: %s', [AFileName]);
  Json := TFile.ReadAllText(AFileName, TEncoding.UTF8);
  Doc := TJSONObject.ParseJSONValue(Json);
  if not (Doc is TJSONObject) then
  begin
    Doc.Free;
    raise EOBDConfig.CreateFmt('DTC catalogue %s: root is not an object',
      [AFileName]);
  end;
  try
    Root := Doc as TJSONObject;
    if not (Root.GetValue('dtcs') is TJSONArray) then Exit;
    Arr := Root.GetValue('dtcs') as TJSONArray;
    FLock.Enter;
    try
      for I := 0 to Arr.Count - 1 do
      begin
        if not (Arr.Items[I] is TJSONObject) then Continue;
        Entry := Arr.Items[I] as TJSONObject;
        Info := Default(TOBDDtcInfo);
        Info.Code        := UpperCase(Trim(GetJSONStr(Entry, 'code', '')));
        Info.Description := GetJSONStr(Entry, 'description', '');
        Info.Severity    := GetJSONStr(Entry, 'severity', '');
        if Info.Code = '' then Continue;
        FDTCs.AddOrSetValue(Info.Code, Info);
      end;
    finally
      FLock.Leave;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TOBDServiceCatalog.LoadPIDFile(const AFileName: string);
begin
  DoLoadPIDFile(AFileName);
end;

procedure TOBDServiceCatalog.LoadDTCFile(const AFileName: string);
begin
  DoLoadDTCFile(AFileName);
end;

function TOBDServiceCatalog.LoadDirectory(const ARoot: string): Integer;
var
  Files: TArray<string>;
  F, Lower: string;
begin
  Result := 0;
  if not TDirectory.Exists(ARoot) then
    raise EOBDConfig.CreateFmt('Catalogue directory not found: %s', [ARoot]);
  Files := TDirectory.GetFiles(ARoot, '*.json', TSearchOption.soAllDirectories);
  for F in Files do
  begin
    Lower := LowerCase(TPath.GetFileName(F));
    if EndsText('-pids.json', Lower) or EndsText('pid.json', Lower) then
    begin
      DoLoadPIDFile(F);
      Inc(Result);
    end
    else if StartsText('dtc-', Lower) and EndsText('.json', Lower) then
    begin
      DoLoadDTCFile(F);
      Inc(Result);
    end;
  end;
end;

{ ---- decoder evaluator ------------------------------------------------------ }

function EvaluatePIDDecoder(const ADecoder: TOBDPIDDecoderInfo;
  const ARaw: TBytes; out AValue: Double): Boolean;
var
  Raw: Int64;
  Kind: string;
begin
  Result := False;
  AValue := NaN;
  Kind := LowerCase(ADecoder.Kind);
  if Kind = 'uint8' then
  begin
    if Length(ARaw) < 1 then Exit;
    Raw := ARaw[0];
  end
  else if Kind = 'int8' then
  begin
    if Length(ARaw) < 1 then Exit;
    Raw := ShortInt(ARaw[0]);
  end
  else if (Kind = 'uint16_be') or (Kind = 'uint16') then
  begin
    if Length(ARaw) < 2 then Exit;
    Raw := (Cardinal(ARaw[0]) shl 8) or Cardinal(ARaw[1]);
  end
  else if (Kind = 'int16_be') or (Kind = 'int16') then
  begin
    if Length(ARaw) < 2 then Exit;
    Raw := SmallInt((Cardinal(ARaw[0]) shl 8) or Cardinal(ARaw[1]));
  end
  else if Kind = 'uint32_be' then
  begin
    if Length(ARaw) < 4 then Exit;
    Raw := (Cardinal(ARaw[0]) shl 24) or (Cardinal(ARaw[1]) shl 16) or
           (Cardinal(ARaw[2]) shl 8)  or  Cardinal(ARaw[3]);
  end
  else
    Exit;
  AValue := Raw * ADecoder.Scale + ADecoder.Offset;
  Result := True;
end;

initialization

finalization
  TOBDServiceCatalog.ReleaseDefault;

end.
