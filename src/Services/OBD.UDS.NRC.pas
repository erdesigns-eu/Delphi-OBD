//------------------------------------------------------------------------------
// UNIT           : OBD.UDS.NRC.pas
// CONTENTS       : ISO 14229-1 UDS Negative Response Code catalog
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.UDS.NRC;

interface

uses
  System.SysUtils, System.Generics.Collections;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  TOBDUDSNrcCategory = (
    nrcGeneral,
    nrcSecurity,
    nrcRequestData,
    nrcCondition,
    nrcServer,
    nrcReserved
  );

  TOBDUDSNrcInfo = record
    /// <summary>Code.</summary>
    Code: Byte;
    /// <summary>Short name.</summary>
    ShortName: string;
    /// <summary>Description.</summary>
    Description: string;
    /// <summary>Category.</summary>
    Category: TOBDUDSNrcCategory;
  end;

/// <summary>Look up an NRC. Unknown / reserved codes return a record
/// with category=nrcReserved and a synthetic description; never raises.</summary>
function DescribeNRC(NRC: Byte): TOBDUDSNrcInfo;

/// <summary>One-line formatter convenient for log lines and exception
/// messages: "NRC 0x33 (SAD: securityAccessDenied)".</summary>
function FormatNRC(NRC: Byte): string;

/// <summary>True if the byte is in a category clients should retry
/// (busy / repeat-request, conditions-not-correct).</summary>
function IsTransientNRC(NRC: Byte): Boolean;

/// <summary>Total entries loaded from the catalog (excludes synthetic).</summary>
function NRCCatalogCount: Integer;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

uses
  System.Classes, System.JSON,
  OBD.Catalog.Path;

const
  CatalogFileName = 'uds-nrc.json';

var
  GMap: TDictionary<Byte, TOBDUDSNrcInfo> = nil;

//------------------------------------------------------------------------------
// CATEGORY FROM STRING
//------------------------------------------------------------------------------
function CategoryFromString(const S: string): TOBDUDSNrcCategory;
begin
  if SameText(S, 'general')      then Exit(nrcGeneral);
  if SameText(S, 'security')     then Exit(nrcSecurity);
  if SameText(S, 'request_data') then Exit(nrcRequestData);
  if SameText(S, 'condition')    then Exit(nrcCondition);
  if SameText(S, 'server')       then Exit(nrcServer);
  Result := nrcReserved;
end;

//------------------------------------------------------------------------------
// PARSE HEX BYTE
//------------------------------------------------------------------------------
function ParseHexByte(const S: string; out B: Byte): Boolean;
var
  V: Integer;
  T: string;
begin
  T := S;
  if T.StartsWith('$') then
    T := T  // already Delphi hex
  else if T.StartsWith('0x', True) then
    T := '$' + T.Substring(2);
  Result := TryStrToInt(T, V) and (V >= 0) and (V <= 255);
  if Result then B := Byte(V);
end;

//------------------------------------------------------------------------------
// LOAD CATALOG
//------------------------------------------------------------------------------
procedure LoadCatalog;
var
  Path, Raw: string;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  Info: TOBDUDSNrcInfo;
  Code: Byte;
  Stream: TStringStream;
begin
  Path := ResolveCatalogPath(CatalogFileName);
  if Path = '' then Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      if not ParseHexByte(Obj.GetValue<string>('code', ''), Code) then Continue;
      Info.Code        := Code;
      Info.ShortName   := Obj.GetValue<string>('short', '');
      Info.Description := Obj.GetValue<string>('description', '');
      Info.Category    := CategoryFromString(Obj.GetValue<string>('category', 'reserved'));
      GMap.AddOrSetValue(Code, Info);
    end;
  finally
    Doc.Free;
  end;
end;

//------------------------------------------------------------------------------
// DESCRIBE NRC
//------------------------------------------------------------------------------
function DescribeNRC(NRC: Byte): TOBDUDSNrcInfo;
begin
  if (GMap <> nil) and GMap.TryGetValue(NRC, Result) then Exit;
  Result.Code := NRC;
  Result.ShortName   := Format('NRC_0x%.2x', [NRC]);
  Result.Description := Format('reserved or manufacturer-specific NRC 0x%.2x', [NRC]);
  Result.Category    := nrcReserved;
end;

//------------------------------------------------------------------------------
// FORMAT NRC
//------------------------------------------------------------------------------
function FormatNRC(NRC: Byte): string;
var Info: TOBDUDSNrcInfo;
begin
  Info := DescribeNRC(NRC);
  Result := Format('NRC 0x%.2x (%s: %s)', [NRC, Info.ShortName, Info.Description]);
end;

//------------------------------------------------------------------------------
// IS TRANSIENT NRC
//------------------------------------------------------------------------------
function IsTransientNRC(NRC: Byte): Boolean;
begin
  Result := (NRC = $21) or (NRC = $22) or (NRC = $78) or (NRC = $94);
end;

function NRCCatalogCount: Integer;
begin
  if GMap = nil then Result := 0 else Result := GMap.Count;
end;

initialization
  GMap := TDictionary<Byte, TOBDUDSNrcInfo>.Create;
  LoadCatalog;

finalization
  GMap.Free;

end.
