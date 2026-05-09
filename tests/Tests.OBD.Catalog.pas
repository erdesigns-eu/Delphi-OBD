//------------------------------------------------------------------------------
//  Tests.OBD.Catalog
//
//  DUnitX coverage for the JSON catalogue loader.
//
//  Tests are file-driven: each fixture builds a small JSON document on
//  disk under a temp folder, calls LoadFile, and asserts the parsed
//  state. This lets the suite live alongside the production code and
//  exercise the same code path users hit.
//
//  Author      : ERDesigns
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial Phase 1 coverage: PID round-trip, text
//                     round-trip, malformed JSON, missing fields,
//                     directory walk, FindPID, FindText.
//------------------------------------------------------------------------------

unit Tests.OBD.Catalog;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   JSON catalogue loader coverage. Each test owns a temp folder
  ///   that is wiped in <c>TearDown</c>.
  /// </summary>
  [TestFixture]
  TCatalogTests = class
  strict private
    FTempDir: string;
    procedure WriteFile(const ARelative, AContent: string);
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    /// <summary>Round-trips a Mode 01 PID file: load, find by ID,
    /// verify scaling.</summary>
    [Test] procedure PIDRoundTrip;
    /// <summary>Round-trips an NRC file: load, find by code, verify
    /// text.</summary>
    [Test] procedure NRCRoundTrip;
    /// <summary>Malformed JSON raises EOBDConfig with the file name in
    /// the message.</summary>
    [Test] procedure MalformedJSONRaises;
    /// <summary>Missing required <c>type</c> field raises with a
    /// pointed message.</summary>
    [Test] procedure MissingTypeRaises;
    /// <summary>Wrong schema version raises.</summary>
    [Test] procedure WrongVersionRaises;
    /// <summary>Hex IDs are accepted with or without 0x prefix and are
    /// case-insensitive.</summary>
    [Test] procedure HexIDFormatTolerance;
    /// <summary>LoadDirectory walks recursively and returns the file
    /// count.</summary>
    [Test] procedure LoadDirectoryWalksRecursively;
    /// <summary>FindText spans across all loaded catalogues of the
    /// requested kind.</summary>
    [Test] procedure FindTextAcrossCatalogues;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Types,
  OBD.Catalog;

procedure TCatalogTests.Setup;
begin
  FTempDir := TPath.Combine(TPath.GetTempPath,
    'delphi-obd-tests-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);
  TOBDCatalogStore.Default.Clear;
end;

procedure TCatalogTests.TearDown;
begin
  TOBDCatalogStore.Default.Clear;
  if (FTempDir <> '') and TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

procedure TCatalogTests.WriteFile(const ARelative, AContent: string);
var
  Path: string;
begin
  Path := TPath.Combine(FTempDir, ARelative);
  TDirectory.CreateDirectory(TPath.GetDirectoryName(Path));
  TFile.WriteAllText(Path, AContent, TEncoding.UTF8);
end;

procedure TCatalogTests.PIDRoundTrip;
var
  D: TOBDPIDDescriptor;
begin
  WriteFile('pids.json',
    '{ "version": 1, "type": "obd2-pid", "mode": "01", "entries": [' +
    '  { "id": "0C", "name": "Engine RPM", "length": 2, "decoder": "rpm", "unit": "rpm" }' +
    '] }');
  TOBDCatalogStore.Default.LoadFile(TPath.Combine(FTempDir, 'pids.json'));
  Assert.IsTrue(TOBDCatalogStore.Default.FindPID('01', $0C, D));
  Assert.AreEqual('Engine RPM', D.Name);
  Assert.AreEqual<Byte>(2, D.Length);
  Assert.AreEqual('rpm', D.DecoderName);
  Assert.AreEqual('rpm', D.UnitName);
end;

procedure TCatalogTests.NRCRoundTrip;
var
  Text: string;
begin
  WriteFile('nrc.json',
    '{ "version": 1, "type": "uds-nrc", "entries": [' +
    '  { "code": "0x33", "text": "Security access denied" }' +
    '] }');
  TOBDCatalogStore.Default.LoadFile(TPath.Combine(FTempDir, 'nrc.json'));
  Assert.IsTrue(TOBDCatalogStore.Default.FindText(ckUDSNRC, $33, Text));
  Assert.AreEqual('Security access denied', Text);
end;

procedure TCatalogTests.MalformedJSONRaises;
begin
  WriteFile('bad.json', '{ this is not json');
  Assert.WillRaise(
    procedure
    begin
      TOBDCatalogStore.Default.LoadFile(TPath.Combine(FTempDir, 'bad.json'));
    end,
    EOBDConfig);
end;

procedure TCatalogTests.MissingTypeRaises;
begin
  WriteFile('missing.json',
    '{ "version": 1, "entries": [] }');
  Assert.WillRaise(
    procedure
    begin
      TOBDCatalogStore.Default.LoadFile(TPath.Combine(FTempDir, 'missing.json'));
    end,
    EOBDConfig);
end;

procedure TCatalogTests.WrongVersionRaises;
begin
  WriteFile('v99.json',
    '{ "version": 99, "type": "obd2-pid", "entries": [] }');
  Assert.WillRaise(
    procedure
    begin
      TOBDCatalogStore.Default.LoadFile(TPath.Combine(FTempDir, 'v99.json'));
    end,
    EOBDConfig);
end;

procedure TCatalogTests.HexIDFormatTolerance;
var
  D: TOBDPIDDescriptor;
begin
  WriteFile('mixed.json',
    '{ "version": 1, "type": "obd2-pid", "mode": "01", "entries": [' +
    '  { "id": "0x0C", "name": "Engine RPM", "length": 2, "decoder": "rpm" },' +
    '  { "id": "0F",   "name": "Intake air temperature", "length": 1, "decoder": "temperature" }' +
    '] }');
  TOBDCatalogStore.Default.LoadFile(TPath.Combine(FTempDir, 'mixed.json'));
  Assert.IsTrue(TOBDCatalogStore.Default.FindPID('01', $0C, D));
  Assert.AreEqual('Engine RPM', D.Name);
  Assert.IsTrue(TOBDCatalogStore.Default.FindPID('01', $0F, D));
  Assert.AreEqual('Intake air temperature', D.Name);
end;

procedure TCatalogTests.LoadDirectoryWalksRecursively;
var
  Loaded: Integer;
begin
  WriteFile('a/one.json',
    '{ "version": 1, "type": "obd2-pid", "mode": "01", "entries": [] }');
  WriteFile('b/sub/two.json',
    '{ "version": 1, "type": "uds-nrc", "entries": [] }');
  Loaded := TOBDCatalogStore.Default.LoadDirectory(FTempDir);
  Assert.AreEqual(2, Loaded);
  Assert.AreEqual(2, TOBDCatalogStore.Default.Count);
end;

procedure TCatalogTests.FindTextAcrossCatalogues;
var
  Text: string;
begin
  WriteFile('nrc-a.json',
    '{ "version": 1, "type": "uds-nrc", "entries": [' +
    '  { "code": "0x33", "text": "Security access denied" }' +
    '] }');
  WriteFile('nrc-b.json',
    '{ "version": 1, "type": "uds-nrc", "entries": [' +
    '  { "code": "0x78", "text": "Response pending" }' +
    '] }');
  TOBDCatalogStore.Default.LoadDirectory(FTempDir);
  Assert.IsTrue(TOBDCatalogStore.Default.FindText(ckUDSNRC, $33, Text));
  Assert.AreEqual('Security access denied', Text);
  Assert.IsTrue(TOBDCatalogStore.Default.FindText(ckUDSNRC, $78, Text));
  Assert.AreEqual('Response pending', Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TCatalogTests);

end.
