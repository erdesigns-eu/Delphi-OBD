//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Catalog.CSV.pas
// CONTENTS       : CSV → JSON catalog converter
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Many community DID datasets ship as flat CSV. This
//                  converter ingests them and emits a JSON catalog
//                  matching docs/CATALOG_FORMAT.md. Used by the
//                  `import-csv` tool under tools/.
//
//                  Recognised columns (case-insensitive, header row
//                  required):
//                    did            mandatory, hex (`0xF187`) or decimal
//                    name           mandatory, snake_case
//                    description    mandatory
//                    source         optional, falls back to default_source
//                    verified       optional, "true"/"false"/blank
//                    ecu_address    optional
//                    decoder        optional, JSON sub-object as a string
//
//                  Empty / missing fields are skipped. Lines starting
//                  with `#` are comments.
//------------------------------------------------------------------------------
unit OBD.OEM.Catalog.CSV;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Generics.Collections;

type
  EOBDCsvCatalogError = class(Exception);

  TOBDCatalogCSVImporter = class
  strict private
    FManufacturerKey: string;
    FDisplayName: string;
    FApplicableWMIs: TArray<string>;
    FDefaultSource: string;
  public
    constructor Create(const AManufacturerKey, ADisplayName: string;
      const AApplicableWMIs: TArray<string>;
      const ADefaultSource: string = '');
    /// <summary>Read <c>CsvPath</c>, write a v1 JSON catalog to <c>JsonPath</c>.</summary>
    procedure Convert(const CsvPath, JsonPath: string);
    /// <summary>In-memory variant — useful for tests.</summary>
    function ConvertText(const CsvText: string): string;

    property ManufacturerKey: string read FManufacturerKey;
    property DisplayName: string read FDisplayName;
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
// Minimal RFC-4180-style parser: comma-separated, fields may be quoted with
// double-quotes, embedded `""` decodes to a single `"`. CSV data with
// embedded JSON (decoder column) typically arrives quoted from the
// upstream tool.
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

function FindIndex(const Headers: TArray<string>; const Name: string): Integer;
var I: Integer;
begin
  for I := 0 to High(Headers) do
    if SameText(Trim(Headers[I]), Name) then Exit(I);
  Result := -1;
end;

function TOBDCatalogCSVImporter.ConvertText(const CsvText: string): string;
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
      for WMI in FApplicableWMIs do WMIs.AddElement(TJSONString.Create(WMI));
      Root.AddPair('applicable_wmis', WMIs);

      if FDefaultSource <> '' then
        Root.AddPair('default_source', FDefaultSource);

      DIDs := TJSONArray.Create;
      Root.AddPair('dids', DIDs);

      for I := 1 to Reader.Count - 1 do
      begin
        Line := Reader[I];
        if (Trim(Line) = '') or Line.StartsWith('#') then Continue;
        Fields := ParseCSVLine(Line);
        if Length(Fields) <= IDx then Continue;
        if (Length(Fields) <= NameI) or (Length(Fields) <= DescI) then Continue;

        Entry := TJSONObject.Create;
        Entry.AddPair('did',         Trim(Fields[IDx]));
        Entry.AddPair('name',        Trim(Fields[NameI]));
        Entry.AddPair('description', Trim(Fields[DescI]));
        if (SrcI >= 0) and (SrcI < Length(Fields)) and (Fields[SrcI] <> '') then
          Entry.AddPair('source', Trim(Fields[SrcI]));
        if (VerI >= 0) and (VerI < Length(Fields)) and (Fields[VerI] <> '') then
          Entry.AddPair('verified',
            TJSONBool.Create(SameText(Trim(Fields[VerI]), 'true')));
        if (EcuI >= 0) and (EcuI < Length(Fields)) and (Fields[EcuI] <> '') then
          Entry.AddPair('ecu_address', Trim(Fields[EcuI]));
        if (DecI >= 0) and (DecI < Length(Fields)) and (Fields[DecI] <> '') then
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

procedure TOBDCatalogCSVImporter.Convert(const CsvPath, JsonPath: string);
var
  CsvText, JsonText: string;
begin
  if not TFile.Exists(CsvPath) then
    raise EOBDCsvCatalogError.CreateFmt('CSV file %s not found', [CsvPath]);
  CsvText := TFile.ReadAllText(CsvPath, TEncoding.UTF8);
  JsonText := ConvertText(CsvText);
  ForceDirectories(TPath.GetDirectoryName(JsonPath));
  TFile.WriteAllText(JsonPath, JsonText, TEncoding.UTF8);
end;

end.
