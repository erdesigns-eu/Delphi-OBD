//------------------------------------------------------------------------------
//  Tests.ERD.Recorder.CSV
//
//  Coverage for the live-data CSV logger and recording CSV exporter:
//  quoting, locale separators, BOM output, wide live rows and long
//  recording export rows.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.ERD.Recorder.CSV;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.IOUtils,
  {$IFDEF FPC}DateUtils{$ELSE}System.DateUtils{$ENDIF},
  DUnitX.TestFramework,
  ERD.Recorder,
  ERD.Recorder.CSV,
  ERD.Service.LiveData;

type
  [TestFixture]
  TCSVWriterTests = class
  public
    [Test] procedure Quote_CommaQuoteAndLineBreak;
    [Test] procedure SemicolonSeparator_DoesNotQuoteCommas;
    [Test] procedure DecimalComma_IsInvariant;
    [Test] procedure UTF8BOM_IsWrittenBeforeFirstRow;
    [Test] procedure SeparatorDecimalConflict_Raises;
  end;

  [TestFixture]
  TCSVLoggerTests = class
  public
    [Test] procedure HeaderIncludesUnitsAndMissingValueIsEmpty;
  end;

  [TestFixture]
  TRecordingCSVExporterTests = class
  public
    [Test] procedure SyntheticEntryExportsDecodedMode01;
    [Test] procedure SyntheticJSONLinesExportDecodedMode01;
  end;

implementation

function MakeValue(APID: Byte; AValue: Double; const AUnit, ADescription: string): TOBDPIDValue;
begin
  Result := Default(TOBDPIDValue);
  Result.PID := APID;
  Result.Value := AValue;
  Result.Unit_ := AUnit;
  Result.Description := ADescription;
end;

function LocalTestFile(const APrefix: string): string;
begin
  Result := TPath.Combine(TDirectory.GetCurrentDirectory,
    APrefix + TGUID.NewGuid.ToString + '.csv');
end;

{ ---- Writer ---------------------------------------------------------------- }

procedure TCSVWriterTests.Quote_CommaQuoteAndLineBreak;
begin
  Assert.AreEqual('"alpha,beta"', TOBDCSVWriter.QuoteField('alpha,beta'));
  Assert.AreEqual('"a""b"', TOBDCSVWriter.QuoteField('a"b'));
  Assert.AreEqual('"a' + #13#10 + 'b"', TOBDCSVWriter.QuoteField('a' + #13#10 + 'b'));
end;

procedure TCSVWriterTests.SemicolonSeparator_DoesNotQuoteCommas;
begin
  Assert.AreEqual('alpha,beta;gamma',
    TOBDCSVWriter.BuildRow(['alpha,beta', 'gamma'], ';'));
  Assert.AreEqual('"alpha;beta";gamma',
    TOBDCSVWriter.BuildRow(['alpha;beta', 'gamma'], ';'));
end;

procedure TCSVWriterTests.DecimalComma_IsInvariant;
begin
  Assert.AreEqual('12,5', TOBDCSVWriter.FormatNumber(12.5, ','));
end;

procedure TCSVWriterTests.UTF8BOM_IsWrittenBeforeFirstRow;
var
  Stream: TMemoryStream;
  Writer: TOBDCSVWriter;
  Bytes: TBytes;
begin
  Stream := TMemoryStream.Create;
  try
    Writer := TOBDCSVWriter.Create(Stream, False);
    try
      Writer.IncludeUTF8BOM := True;
      Writer.WriteRow(['a', 'b']);
    finally
      Writer.Free;
    end;
    SetLength(Bytes, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(Bytes[0], Length(Bytes));
    Assert.AreEqual<Integer>($EF, Bytes[0]);
    Assert.AreEqual<Integer>($BB, Bytes[1]);
    Assert.AreEqual<Integer>($BF, Bytes[2]);
    Assert.AreEqual<Integer>(13, Bytes[Length(Bytes) - 2]);
    Assert.AreEqual<Integer>(10, Bytes[Length(Bytes) - 1]);
  finally
    Stream.Free;
  end;
end;

procedure TCSVWriterTests.SeparatorDecimalConflict_Raises;
var
  Stream: TMemoryStream;
  Writer: TOBDCSVWriter;
begin
  Stream := TMemoryStream.Create;
  try
    Writer := TOBDCSVWriter.Create(Stream, False);
    try
      Assert.WillRaise(
        procedure
        begin
          Writer.DecimalSeparator := ',';
        end,
        EOBDCSVError);
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

{ ---- Live logger ----------------------------------------------------------- }

procedure TCSVLoggerTests.HeaderIncludesUnitsAndMissingValueIsEmpty;
var
  FileName: string;
  Logger: TOBDCSVLogger;
  Lines: TStringList;
begin
  FileName := LocalTestFile('csvlogger-');
  Logger := TOBDCSVLogger.Create(nil);
  Lines := TStringList.Create;
  try
    Logger.FileName := FileName;
    Logger.IncludeUTF8BOM := False;
    Logger.SetPIDs(TBytes.Create($0C, $0D));
    Logger.Start;
    Logger.PushValue(MakeValue($0C, 1500.0, 'rpm', 'Engine RPM'));
    Logger.PushValue(MakeValue($0C, 1600.0, 'rpm', 'Engine RPM'));
    Logger.Stop;

    Lines.LoadFromFile(FileName, TEncoding.UTF8);
    Assert.AreEqual(3, Lines.Count);
    Assert.IsTrue(Pos('Engine RPM [rpm],Vehicle speed [km/h]', Lines[0]) > 0);
    Assert.IsTrue(Pos(',1500,', Lines[1]) > 0, 'First row has RPM and empty speed cell');
    Assert.IsTrue(Pos(',1600,', Lines[2]) > 0, 'Pending row flushes on Stop');
  finally
    Lines.Free;
    Logger.Free;
    if TFile.Exists(FileName) then
      TFile.Delete(FileName);
  end;
end;

{ ---- Recording exporter ---------------------------------------------------- }

procedure TRecordingCSVExporterTests.SyntheticEntryExportsDecodedMode01;
var
  Options: TOBDRecordingCSVExportOptions;
  Entry: TOBDLogEntry;
  Rows: TStringList;
begin
  Options := DefaultOBDRecordingCSVExportOptions;
  Options.IncludeUTF8BOM := False;
  Entry := Default(TOBDLogEntry);
  Entry.Timestamp := EncodeDateTime(2026, 10, 12, 12, 0, 0, 0);
  Entry.Kind := leResponse;
  Entry.ServiceID := $41;
  Entry.HasServiceID := True;
  Entry.Raw := TBytes.Create($0C, $1A, $F8);
  Rows := TStringList.Create;
  try
    TOBDRecordingCSVExporter.ExportEntriesToStrings([Entry], Rows, Options);
    Assert.AreEqual(2, Rows.Count);
    Assert.IsTrue(Pos('response,,0x41,0x0C,0C 1A F8,Engine RPM,1726,rpm', Rows[1]) > 0);
  finally
    Rows.Free;
  end;
end;

procedure TRecordingCSVExporterTests.SyntheticJSONLinesExportDecodedMode01;
var
  Options: TOBDRecordingCSVExportOptions;
  Lines, Rows: TStringList;
begin
  Options := DefaultOBDRecordingCSVExportOptions;
  Options.IncludeUTF8BOM := False;
  Lines := TStringList.Create;
  Rows := TStringList.Create;
  try
    Lines.Add('{"ts":"2026-10-12T12:00:00.000Z","kind":"response","elapsed_ms":42,"service_id":"0x41","raw":"DBr4"}');
    TOBDRecordingCSVExporter.ExportLogLinesToStrings(Lines, Rows, Options);
    Assert.AreEqual(2, Rows.Count);
    Assert.IsTrue(Pos('0x41,0x0C,0C 1A F8,Engine RPM,1726,rpm', Rows[1]) > 0);
  finally
    Rows.Free;
    Lines.Free;
  end;
end;

end.
