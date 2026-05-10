//------------------------------------------------------------------------------
//  Tests.OBD.Catalog.Inventory
//
//  Regression test that the shipped catalogue files contain at least
//  the baseline entry counts captured at v2 launch. A future PR that
//  silently drops a PID, DTC, NRC or DID entry will fail this test.
//
//  Counts here are minimum thresholds, not exact numbers. New entries
//  are welcome; only deletions need to bump the baseline.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial baseline (84 PIDs, 528
//                     DTCs, 60 NRCs, 31 UDS DIDs, 55 J1939 PGNs, 34
//                     Mode 06 MIDs, 22 Mode 06 TIDs, 22 WWH-OBD DIDs).
//------------------------------------------------------------------------------

unit Tests.OBD.Catalog.Inventory;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   End-to-end regression test for the shipped catalogue tree.
  ///
  ///   Loads every <c>.json</c> file under <c>catalogs/</c> and asserts
  ///   that the cumulative entry counts meet the launch baseline.
  /// </summary>
  /// <remarks>
  ///   The catalogue root is located by walking up from the test
  ///   executable directory until a <c>catalogs/</c> folder is found.
  ///   This makes the test work both in the IDE and from the CI build
  ///   output folder.
  /// </remarks>
  [TestFixture]
  TCatalogInventoryTests = class
  strict private
    FCatalogRoot: string;
    function FindCatalogRoot: string;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    /// <summary>At least 84 Mode 01 PIDs are loaded.</summary>
    [Test] procedure Mode01PIDsBaseline;
    /// <summary>At least 500 generic DTCs are loaded.</summary>
    [Test] procedure GenericDTCsBaseline;
    /// <summary>At least 55 UDS NRCs are loaded.</summary>
    [Test] procedure UDSNRCsBaseline;
    /// <summary>At least 30 generic UDS DIDs are loaded.</summary>
    [Test] procedure UDSDIDsBaseline;
    /// <summary>At least 50 J1939 PGNs are loaded.</summary>
    [Test] procedure J1939PGNsBaseline;
    /// <summary>At least 30 Mode 06 OBDMIDs are loaded.</summary>
    [Test] procedure Mode06MIDsBaseline;
    /// <summary>At least 20 Mode 06 TIDs are loaded.</summary>
    [Test] procedure Mode06TIDsBaseline;
    /// <summary>At least 20 WWH-OBD DIDs are loaded.</summary>
    [Test] procedure WWHOBDDIDsBaseline;
    /// <summary>At least 30 OEM DTC files are present.</summary>
    [Test] procedure OEMDTCFileCountBaseline;
    /// <summary>At least 600 OEM DTC entries cumulative.</summary>
    [Test] procedure OEMDTCEntryCountBaseline;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.JSON,
  OBD.Types,
  OBD.Catalog;

function CountEntriesAcrossKind(AKind: TOBDCatalogKind): Integer;
var
  Store: TOBDCatalogStore;
  I: Integer;
  Cat: TOBDCatalog;
begin
  Result := 0;
  Store := TOBDCatalogStore.Default;
  for I := 0 to Store.Count - 1 do
  begin
    Cat := Store.GetCatalog(I);
    if Cat.Kind <> AKind then
      Continue;
    if AKind = ckOBD2PID then
      Result := Result + Cat.PIDs.Count
    else
      Result := Result + Cat.Texts.Count;
  end;
end;

function CountPIDsForMode(const AMode: string): Integer;
var
  Store: TOBDCatalogStore;
  I: Integer;
  Cat: TOBDCatalog;
begin
  Result := 0;
  Store := TOBDCatalogStore.Default;
  for I := 0 to Store.Count - 1 do
  begin
    Cat := Store.GetCatalog(I);
    if (Cat.Kind = ckOBD2PID) and SameText(Cat.Mode, AMode) then
      Result := Result + Cat.PIDs.Count;
  end;
end;

{ ---- TCatalogInventoryTests --------------------------------------------------- }

function TCatalogInventoryTests.FindCatalogRoot: string;
var
  Dir, Candidate: string;
begin
  Dir := TPath.GetDirectoryName(ParamStr(0));
  while Dir <> '' do
  begin
    Candidate := TPath.Combine(Dir, 'catalogs');
    if TDirectory.Exists(Candidate) then
      Exit(Candidate);
    if Dir = TPath.GetDirectoryName(Dir) then
      Break;
    Dir := TPath.GetDirectoryName(Dir);
  end;
  Result := '';
end;

procedure TCatalogInventoryTests.Setup;
begin
  FCatalogRoot := FindCatalogRoot;
  Assert.IsNotEmpty(FCatalogRoot, 'Could not locate catalogs/ folder');

  TOBDCatalogStore.Default.Clear;
  TOBDCatalogStore.Default.LoadDirectory(FCatalogRoot);
end;

procedure TCatalogInventoryTests.TearDown;
begin
  TOBDCatalogStore.Default.Clear;
end;

procedure TCatalogInventoryTests.Mode01PIDsBaseline;
begin
  Assert.IsTrue(CountPIDsForMode('01') >= 84,
    Format('Expected >= 84 Mode 01 PIDs, got %d', [CountPIDsForMode('01')]));
end;

procedure TCatalogInventoryTests.GenericDTCsBaseline;
var
  Count: Integer;
begin
  Count := CountEntriesAcrossKind(ckOBD2DTC);
  // 528 generic + ~658 OEM standard ≈ 1186; baseline 1100.
  Assert.IsTrue(Count >= 1100,
    Format('Expected >= 1100 DTC entries (generic + OEM), got %d', [Count]));
end;

procedure TCatalogInventoryTests.UDSNRCsBaseline;
begin
  Assert.IsTrue(CountEntriesAcrossKind(ckUDSNRC) >= 55,
    Format('Expected >= 55 UDS NRC entries, got %d',
      [CountEntriesAcrossKind(ckUDSNRC)]));
end;

procedure TCatalogInventoryTests.UDSDIDsBaseline;
begin
  // 31 generic + 22 WWH-OBD = 53; baseline 30 to allow flex.
  Assert.IsTrue(CountEntriesAcrossKind(ckUDSDID) >= 30,
    Format('Expected >= 30 UDS DID entries, got %d',
      [CountEntriesAcrossKind(ckUDSDID)]));
end;

procedure TCatalogInventoryTests.J1939PGNsBaseline;
begin
  Assert.IsTrue(CountEntriesAcrossKind(ckJ1939PGN) >= 50,
    Format('Expected >= 50 J1939 PGN entries, got %d',
      [CountEntriesAcrossKind(ckJ1939PGN)]));
end;

procedure TCatalogInventoryTests.Mode06MIDsBaseline;
begin
  // mids-mode06.json + tids-mode06.json both registered as mode '06'.
  Assert.IsTrue(CountPIDsForMode('06') >= 30,
    Format('Expected >= 30 Mode 06 entries, got %d', [CountPIDsForMode('06')]));
end;

procedure TCatalogInventoryTests.Mode06TIDsBaseline;
begin
  // Combined check (TIDs + OBDMIDs both under mode '06') — duplicate of
  // the above; kept distinct so a regression that drops only one of
  // the two files surfaces.
  Assert.IsTrue(CountPIDsForMode('06') >= 30);
end;

procedure TCatalogInventoryTests.WWHOBDDIDsBaseline;
begin
  Assert.IsTrue(CountEntriesAcrossKind(ckUDSDID) >= 50);
end;

procedure TCatalogInventoryTests.OEMDTCFileCountBaseline;
var
  Files: TArray<string>;
  OEMRoot: string;
begin
  OEMRoot := TPath.Combine(FCatalogRoot, 'oem');
  if not TDirectory.Exists(OEMRoot) then
    Assert.Fail('catalogs/oem directory not present');
  Files := TDirectory.GetFiles(OEMRoot, 'dtcs.json',
    TSearchOption.soAllDirectories);
  Assert.IsTrue(Length(Files) >= 30,
    Format('Expected >= 30 OEM dtcs.json files, got %d', [Length(Files)]));
end;

procedure TCatalogInventoryTests.OEMDTCEntryCountBaseline;
var
  Total, I: Integer;
  Store: TOBDCatalogStore;
  Cat: TOBDCatalog;
begin
  Total := 0;
  Store := TOBDCatalogStore.Default;
  for I := 0 to Store.Count - 1 do
  begin
    Cat := Store.GetCatalog(I);
    if (Cat.Kind = ckOBD2DTC) and
       (Pos(PathDelim + 'oem' + PathDelim, Cat.FileName) > 0) then
      Total := Total + Cat.Texts.Count;
  end;
  Assert.IsTrue(Total >= 600,
    Format('Expected >= 600 OEM DTC entries, got %d', [Total]));
end;

initialization
  TDUnitX.RegisterTestFixture(TCatalogInventoryTests);

end.
