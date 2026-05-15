//------------------------------------------------------------------------------
//  OBD.UDS.NRC
//
//  ISO 14229-1 UDS negative-response-code catalogue. Loads
//  <c>catalogs/uds-nrc.json</c> on init and exposes:
//
//    - <see cref="DescribeNRC"/>: typed lookup (returns a
//      synthetic record for unknown / manufacturer-specific
//      codes instead of raising).
//    - <see cref="FormatNRC"/>: one-line log formatter.
//    - <see cref="IsTransientNRC"/>: classifier for the
//      retry-friendly codes (busy / repeat-request /
//      conditions-not-correct).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1 Annex A Negative Response Codes
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.UDS.NRC;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>Coarse NRC category — derived from the
  /// catalogue, not on-wire.</summary>
  TOBDUDSNrcCategory = (
    /// <summary>General request / format / sub-function
    /// errors (0x10..0x14).</summary>
    nrcGeneral,
    /// <summary>SecurityAccess-related
    /// (0x33..0x37).</summary>
    nrcSecurity,
    /// <summary>Request-data range / out-of-range / upload-
    /// download errors (0x31, 0x70..0x73).</summary>
    nrcRequestData,
    /// <summary>Condition pre-checks (0x21..0x24,
    /// 0x7E..0x7F, voltage / temperature).</summary>
    nrcCondition,
    /// <summary>Server-side processing (0x78
    /// requestCorrectlyReceivedResponsePending, 0x94 …).</summary>
    nrcServer,
    /// <summary>Reserved / manufacturer-specific /
    /// unknown.</summary>
    nrcReserved);

  /// <summary>One NRC catalogue entry.</summary>
  TOBDUDSNrcInfo = record
    /// <summary>NRC byte.</summary>
    Code: Byte;
    /// <summary>Spec short name
    /// (e.g. <c>SAD</c> for SecurityAccessDenied).</summary>
    ShortName: string;
    /// <summary>Full description.</summary>
    Description: string;
    /// <summary>Category.</summary>
    Category: TOBDUDSNrcCategory;
  end;

/// <summary>Looks up an NRC. Unknown / reserved codes return
/// a synthetic record with category=<c>nrcReserved</c> and a
/// formatted description; never raises.</summary>
/// <param name="NRC">NRC byte.</param>
function DescribeNRC(NRC: Byte): TOBDUDSNrcInfo;

/// <summary>One-line formatter for log lines and exception
/// messages: <c>"NRC 0x33 (SAD: securityAccessDenied)"</c>.
/// </summary>
/// <param name="NRC">NRC byte.</param>
function FormatNRC(NRC: Byte): string;

/// <summary>True when the NRC is in a category clients should
/// retry: <c>0x21 busyRepeatRequest</c>,
/// <c>0x22 conditionsNotCorrect</c>,
/// <c>0x78 requestCorrectlyReceivedResponsePending</c>,
/// <c>0x94 resourceTemporarilyUnavailable</c>.</summary>
/// <param name="NRC">NRC byte.</param>
function IsTransientNRC(NRC: Byte): Boolean;

/// <summary>Number of catalogued entries loaded from
/// <c>uds-nrc.json</c>; excludes synthetic results.</summary>
function NRCCatalogCount: Integer;

implementation

uses
  System.Classes,
  System.JSON,
  OBD.OEM.Catalog.Loader;

const
  CatalogFileName = 'uds-nrc.json';

var
  GMap: TDictionary<Byte, TOBDUDSNrcInfo> = nil;

function CategoryFromString(const S: string): TOBDUDSNrcCategory;
begin
  if SameText(S, 'general')      then Exit(nrcGeneral);
  if SameText(S, 'security')     then Exit(nrcSecurity);
  if SameText(S, 'request_data') then Exit(nrcRequestData);
  if SameText(S, 'condition')    then Exit(nrcCondition);
  if SameText(S, 'server')       then Exit(nrcServer);
  Result := nrcReserved;
end;

function ParseHexByte(const S: string; out B: Byte): Boolean;
var
  V: Integer;
  T: string;
begin
  T := S;
  if T.StartsWith('0x', True) then
    T := '$' + T.Substring(2);
  Result := TryStrToInt(T, V) and (V >= 0) and (V <= 255);
  if Result then
    B := Byte(V);
end;

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
  if Path = '' then
    Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then
  begin
    Doc.Free;
    Exit;
  end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    if Arr = nil then
      Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then
        Continue;
      Obj := Item as TJSONObject;
      if not ParseHexByte(Obj.GetValue<string>('code', ''), Code) then
        Continue;
      Info.Code := Code;
      Info.ShortName := Obj.GetValue<string>('short', '');
      Info.Description := Obj.GetValue<string>('description', '');
      Info.Category := CategoryFromString(
        Obj.GetValue<string>('category', 'reserved'));
      GMap.AddOrSetValue(Code, Info);
    end;
  finally
    Doc.Free;
  end;
end;

function DescribeNRC(NRC: Byte): TOBDUDSNrcInfo;
begin
  if (GMap <> nil) and GMap.TryGetValue(NRC, Result) then
    Exit;
  Result.Code := NRC;
  Result.ShortName := Format('NRC_0x%.2x', [NRC]);
  Result.Description := Format(
    'reserved or manufacturer-specific NRC 0x%.2x', [NRC]);
  Result.Category := nrcReserved;
end;

function FormatNRC(NRC: Byte): string;
var
  Info: TOBDUDSNrcInfo;
begin
  Info := DescribeNRC(NRC);
  Result := Format('NRC 0x%.2x (%s: %s)',
    [NRC, Info.ShortName, Info.Description]);
end;

function IsTransientNRC(NRC: Byte): Boolean;
begin
  Result := (NRC = $21) or (NRC = $22)
         or (NRC = $78) or (NRC = $94);
end;

function NRCCatalogCount: Integer;
begin
  if GMap = nil then
    Result := 0
  else
    Result := GMap.Count;
end;

initialization
  GMap := TDictionary<Byte, TOBDUDSNrcInfo>.Create;
  LoadCatalog;

finalization
  GMap.Free;

end.
