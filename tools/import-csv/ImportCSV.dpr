program ImportCSV;

//------------------------------------------------------------------------------
// PROGRAM        : ImportCSV
// CONTENTS       : Convert a CSV catalog into the JSON format documented in
//                  docs/CATALOG_FORMAT.md.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// USAGE          : ImportCSV <key> <display> <wmis> <input.csv> <output.json>
//                    <key>     manufacturer key (e.g. VAG, BMW)
//                    <display> display name in quotes
//                    <wmis>    comma-separated WMI list, e.g. "WVW,WAU,TMB"
//------------------------------------------------------------------------------

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  OBD.OEM.Catalog.CSV in '..\..\src\Services\OBD.OEM.Catalog.CSV.pas';

var
  Importer: TOBDCatalogCSVImporter;
  WMIs: TArray<string>;
begin
  if ParamCount < 5 then
  begin
    Writeln('Usage: ImportCSV <key> <display> <wmis> <input.csv> <output.json>');
    Writeln('  <wmis> example: "WVW,WAU,TMB,VSS"');
    ExitCode := 1;
    Exit;
  end;

  WMIs := ParamStr(3).Split([',']);
  try
    Importer := TOBDCatalogCSVImporter.Create(
      ParamStr(1), ParamStr(2), WMIs, 'csv-import');
    try
      Importer.Convert(ParamStr(4), ParamStr(5));
      Writeln(Format('Wrote %s', [ParamStr(5)]));
    finally
      Importer.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Error: ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
