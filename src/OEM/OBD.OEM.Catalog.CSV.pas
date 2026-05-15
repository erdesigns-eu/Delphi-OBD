//------------------------------------------------------------------------------
//  OBD.OEM.Catalog.CSV
//
//  CSV → JSON catalogue converter. Many community DID datasets
//  ship as flat CSV files; this importer normalises them into the
//  JSON catalogue shape that
//  <see cref="OBD.OEM.Catalog.Loader"/> consumes.
//
//  Recognised columns (case-insensitive, header row required):
//
//    did            mandatory, hex (<c>0xF187</c>) or decimal
//    name           mandatory, snake_case
//    description    mandatory
//    source         optional, falls back to default_source
//    verified       optional, <c>true</c> / <c>false</c> / blank
//    ecu_address    optional
//    decoder        optional, embedded JSON sub-object
//
//  Empty / missing fields are skipped. Lines starting with
//  <c>#</c> are comments. The parser is a minimal RFC-4180
//  reader: comma-separated, double-quoted fields, embedded
//  <c>""</c> decodes to a single quote.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Catalog.CSV;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections;

type
  /// <summary>Raised on CSV import errors.</summary>
  EOBDCsvCatalogError = class(Exception);

  /// <summary>
  ///   Converts CSV DID datasets to the JSON catalogue format
  ///   read by <see cref="OBD.OEM.Catalog.Loader"/>.
  /// </summary>
  TOBDCatalogCSVImporter = class
  strict private
    FManufacturerKey: string;
    FDisplayName: string;
    FApplicableWMIs: TArray<string>;
    FDefaultSource: string;
  public
    /// <summary>Constructs the importer.</summary>
    /// <param name="AManufacturerKey">Short manufacturer tag
    /// (required).</param>
    /// <param name="ADisplayName">Display name.</param>
    /// <param name="AApplicableWMIs">VIN WMI prefixes the
    /// catalogue claims.</param>
    /// <param name="ADefaultSource">Default provenance tag
    /// applied to entries without one.</param>
    /// <exception cref="EArgumentException">
    ///   <c>AManufacturerKey</c> is empty.
    /// </exception>
    constructor Create(const AManufacturerKey, ADisplayName: string;
      const AApplicableWMIs: TArray<string>;
      const ADefaultSource: string = '');

    /// <summary>
    ///   Reads <c>CsvPath</c> and writes a JSON catalogue to
    ///   <c>JsonPath</c>. Creates the target directory when
    ///   missing.
    /// </summary>
    /// <param name="CsvPath">CSV source path.</param>
    /// <param name="JsonPath">JSON destination path.</param>
    /// <exception cref="EOBDCsvCatalogError">CSV missing or
    /// malformed.</exception>
    procedure Convert(const CsvPath, JsonPath: string);

    /// <summary>
    ///   In-memory variant — useful for tests. Returns the
    ///   JSON catalogue as a string.
    /// </summary>
    /// <param name="CsvText">CSV source.</param>
    /// <exception cref="EOBDCsvCatalogError">CSV malformed or
    /// missing required headers.</exception>
    function ConvertText(const CsvText: string): string;

    /// <summary>Manufacturer key seeded at construction.</summary>
    property ManufacturerKey: string read FManufacturerKey;
    /// <summary>Display name seeded at construction.</summary>
    property DisplayName: string read FDisplayName;
    /// <summary>Default provenance tag seeded at
    /// construction.</summary>
    property DefaultSource: string read FDefaultSource;
  end;

implementation

uses
  System.IOUtils;

constructor TOBDCatalogCSVImporter.Create(
  const AManufacturerKey, ADisplayName: string;
  const AApplicableWMIs: TArray<string>;
  const ADefaultSource: string);
begin
  inherited Create;
  if AManufacturerKey = '' then
    raise EArgumentException.Create('manufacturer_key required');
  FManufacturerKey := AManufacturerKey;
  FDisplayName := ADisplayName;
  FApplicableWMIs := AApplicableWMIs;
  FDefaultSource := ADefaultSource;
end;

function ParseCSVLine(const Line: string): TArray<string>;
var
  I, Len: Integer;
  C: Char;
  InQuotes: Boolean;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    InQuotes := False;
    Len := Length(Line);
    I := 1;
    while I <= Len do
    begin
      C := Line[I];
      if InQuotes then
      begin
        if C = '"' then
        begin
          if (I < Len) and (Line[I + 1] = '"') then
          begin
            Builder.Append('"');
            Inc(I, 2);
            Continue;
          end
          else
          begin
            InQuotes := False;
            Inc(I);
            Continue;
          end;
        end
        else
        begin
          Builder.Append(C);
          Inc(I);
        end;
      end
      else
      begin
        if C = ',' then
        begin
          Result := Result + [Builder.ToString];
          Builder.Clear;
          Inc(I);
        end
        else if (C = '"') and (Builder.Length = 0) then
        begin
          InQuotes := True;
          Inc(I);
        end
        else
        begin
          Builder.Append(C);
          Inc(I);
        end;
      end;
    end;
    Result := Result + [Builder.ToString];
  finally
    Builder.Free;
  end;
end;

function FindIndex(const Headers: TArray<string>;
  const Name: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Headers) do
    if SameText(Trim(Headers[I]), Name) then
      Exit(I);
  Result := -1;
end;

function TOBDCatalogCSVImporter.ConvertText(
  const CsvText: string): string;
var
  Reader: TStringList;
  Headers, Fields: TArray<string>;
  IDx, NameI, DescI, SrcI, VerI, EcuI, DecI: Integer;
  I: Integer;
  Line: string;
  Root: TJSONObject;
  WMIs: TJSONArray;
  DIDs: TJSONArray;
  Entry: TJSONObject;
  DecoderText: string;
  DecoderObj: TJSONValue;
  WMI: string;
begin
  Reader := TStringList.Create;
  try
    Reader.Text := CsvText;
    if Reader.Count < 2 then
      raise EOBDCsvCatalogError.Create(
        'CSV must contain at least a header line and one row');

    Headers := ParseCSVLine(Reader[0]);
    IDx   := FindIndex(Headers, 'did');
    NameI := FindIndex(Headers, 'name');
    DescI := FindIndex(Headers, 'description');
    SrcI  := FindIndex(Headers, 'source');
    VerI  := FindIndex(Headers, 'verified');
    EcuI  := FindIndex(Headers, 'ecu_address');
    DecI  := FindIndex(Headers, 'decoder');

    if (IDx < 0) or (NameI < 0) or (DescI < 0) then
      raise EOBDCsvCatalogError.Create(
        'CSV must include did, name, description columns');

    Root := TJSONObject.Create;
    try
      Root.AddPair('version', TJSONNumber.Create(1));
      Root.AddPair('manufacturer_key', FManufacturerKey);
      Root.AddPair('display_name', FDisplayName);

      WMIs := TJSONArray.Create;
      for WMI in FApplicableWMIs do
        WMIs.AddElement(TJSONString.Create(WMI));
      Root.AddPair('applicable_wmis', WMIs);

      if FDefaultSource <> '' then
        Root.AddPair('default_source', FDefaultSource);

      DIDs := TJSONArray.Create;
      Root.AddPair('dids', DIDs);

      for I := 1 to Reader.Count - 1 do
      begin
        Line := Reader[I];
        if (Trim(Line) = '') or Line.StartsWith('#') then
          Continue;
        Fields := ParseCSVLine(Line);
        if Length(Fields) <= IDx then
          Continue;
        if (Length(Fields) <= NameI) or
           (Length(Fields) <= DescI) then
          Continue;

        Entry := TJSONObject.Create;
        Entry.AddPair('did',         Trim(Fields[IDx]));
        Entry.AddPair('name',        Trim(Fields[NameI]));
        Entry.AddPair('description', Trim(Fields[DescI]));
        if (SrcI >= 0) and (SrcI < Length(Fields)) and
           (Fields[SrcI] <> '') then
          Entry.AddPair('source', Trim(Fields[SrcI]));
        if (VerI >= 0) and (VerI < Length(Fields)) and
           (Fields[VerI] <> '') then
          Entry.AddPair('verified',
            TJSONBool.Create(SameText(Trim(Fields[VerI]), 'true')));
        if (EcuI >= 0) and (EcuI < Length(Fields)) and
           (Fields[EcuI] <> '') then
          Entry.AddPair('ecu_address', Trim(Fields[EcuI]));
        if (DecI >= 0) and (DecI < Length(Fields)) and
           (Fields[DecI] <> '') then
        begin
          DecoderText := Trim(Fields[DecI]);
          DecoderObj := TJSONObject.ParseJSONValue(DecoderText);
          if DecoderObj is TJSONObject then
            Entry.AddPair('decoder', DecoderObj)
          else
            DecoderObj.Free;
        end;
        DIDs.AddElement(Entry);
      end;

      Result := Root.Format(2);
    finally
      Root.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TOBDCatalogCSVImporter.Convert(
  const CsvPath, JsonPath: string);
var
  CsvText, JsonText: string;
begin
  if not TFile.Exists(CsvPath) then
    raise EOBDCsvCatalogError.CreateFmt(
      'CSV file %s not found', [CsvPath]);
  CsvText := TFile.ReadAllText(CsvPath, TEncoding.UTF8);
  JsonText := ConvertText(CsvText);
  ForceDirectories(TPath.GetDirectoryName(JsonPath));
  TFile.WriteAllText(JsonPath, JsonText, TEncoding.UTF8);
end;

end.
