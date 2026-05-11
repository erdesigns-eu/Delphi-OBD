//------------------------------------------------------------------------------
//  Tests.OBD.OEM
//
//  Coverage for the OEM-overlay surface:
//    - TOBDOEMOverlay   (per-OEM identifier maps)
//    - TOBDOEMRegistry  (process-wide singleton)
//    - TOBDOEMCatalog   (JSON loader component)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.Types,
  OBD.OEM.Registry,
  OBD.OEM.Catalog;

type
  /// <summary>
  ///   DUnitX fixture for the OEM-overlay surface.
  /// </summary>
  [TestFixture]
  TOEMTests = class
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;

    [Test] procedure Overlay_AddAndLookupDID;
    [Test] procedure Overlay_AddAndLookupDTC;
    [Test] procedure Overlay_AddAndLookupSPN;
    [Test] procedure Overlay_PIDLookupComposesKey;
    [Test] procedure Overlay_LookupMissReturnsFalse;

    [Test] procedure Registry_RegisterUnregisterCountsCorrectly;
    [Test] procedure Registry_ResolveWalksOverlaysInOrder;
    [Test] procedure Registry_ResolveFallbackOnMiss;
    [Test] procedure Registry_ResolveDTCFallbackReturnsCode;

    [Test] procedure Catalog_EmptyFileNameRaises;
    [Test] procedure Catalog_LoadsDIDsFromTempFile;
    [Test] procedure Catalog_RegistersOverlayWithRegistry;
    [Test] procedure Catalog_UnloadRemovesFromRegistry;
  end;

implementation

procedure TOEMTests.SetUp;
begin
  TOBDOEMRegistry.ReleaseInstance;
end;

procedure TOEMTests.TearDown;
begin
  TOBDOEMRegistry.ReleaseInstance;
end;

{ ---- TOBDOEMOverlay ----------------------------------------------------- }

procedure TOEMTests.Overlay_AddAndLookupDID;
var
  O: TOBDOEMOverlay;
  Name: string;
begin
  O := TOBDOEMOverlay.Create('test');
  try
    O.AddDID($F190, 'VIN');
    Assert.IsTrue(O.Lookup(ikDID, $F190, Name));
    Assert.AreEqual('VIN', Name);
  finally
    O.Free;
  end;
end;

procedure TOEMTests.Overlay_AddAndLookupDTC;
var
  O: TOBDOEMOverlay;
  Name: string;
begin
  O := TOBDOEMOverlay.Create('test');
  try
    O.AddDTC('P0420', 'Catalyst System Efficiency Below Threshold');
    Assert.IsTrue(O.LookupDTC('p0420', Name));
    Assert.Contains(Name, 'Catalyst');
  finally
    O.Free;
  end;
end;

procedure TOEMTests.Overlay_AddAndLookupSPN;
var
  O: TOBDOEMOverlay;
  Name: string;
begin
  O := TOBDOEMOverlay.Create('test');
  try
    O.AddSPN(190, 'Engine Speed');
    Assert.IsTrue(O.Lookup(ikSPN, 190, Name));
    Assert.AreEqual('Engine Speed', Name);
  finally
    O.Free;
  end;
end;

procedure TOEMTests.Overlay_PIDLookupComposesKey;
var
  O: TOBDOEMOverlay;
  Name: string;
begin
  O := TOBDOEMOverlay.Create('test');
  try
    O.AddPID($01, $0C, 'Engine RPM');
    Assert.IsTrue(O.Lookup(ikPID, $010C, Name));
    Assert.AreEqual('Engine RPM', Name);
  finally
    O.Free;
  end;
end;

procedure TOEMTests.Overlay_LookupMissReturnsFalse;
var
  O: TOBDOEMOverlay;
  Name: string;
begin
  O := TOBDOEMOverlay.Create('test');
  try
    Name := 'pristine';
    Assert.IsFalse(O.Lookup(ikDID, $1234, Name));
    Assert.AreEqual('pristine', Name);
  finally
    O.Free;
  end;
end;

{ ---- TOBDOEMRegistry ---------------------------------------------------- }

procedure TOEMTests.Registry_RegisterUnregisterCountsCorrectly;
var
  R: TOBDOEMRegistry;
  A: TOBDOEMOverlay;
  B: TOBDOEMOverlay;
begin
  R := TOBDOEMRegistry.Instance;
  A := TOBDOEMOverlay.Create('a');
  B := TOBDOEMOverlay.Create('b');
  try
    R.Register(A);
    R.Register(B);
    R.Register(A);                       // duplicate — should not double
    Assert.AreEqual(2, R.OverlayCount);
    R.Unregister(A);
    Assert.AreEqual(1, R.OverlayCount);
    R.Unregister(B);
    Assert.AreEqual(0, R.OverlayCount);
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TOEMTests.Registry_ResolveWalksOverlaysInOrder;
var
  A: TOBDOEMOverlay;
  B: TOBDOEMOverlay;
  R: TOBDOEMRegistry;
begin
  A := TOBDOEMOverlay.Create('a');
  B := TOBDOEMOverlay.Create('b');
  try
    A.AddDID($F190, 'A.VIN');
    B.AddDID($F190, 'B.VIN');           // shadowed — A wins
    R := TOBDOEMRegistry.Instance;
    R.Register(A);
    R.Register(B);
    Assert.AreEqual('A.VIN', R.Resolve(ikDID, $F190));
  finally
    TOBDOEMRegistry.Instance.Unregister(A);
    TOBDOEMRegistry.Instance.Unregister(B);
    A.Free;
    B.Free;
  end;
end;

procedure TOEMTests.Registry_ResolveFallbackOnMiss;
var
  R: TOBDOEMRegistry;
begin
  R := TOBDOEMRegistry.Instance;
  Assert.Contains(R.Resolve(ikDID, $F190), '0xf190');
  Assert.Contains(R.Resolve(ikSPN, 190), '190');
  Assert.Contains(R.Resolve(ikFMI, 31), '31');
end;

procedure TOEMTests.Registry_ResolveDTCFallbackReturnsCode;
var
  R: TOBDOEMRegistry;
begin
  R := TOBDOEMRegistry.Instance;
  Assert.AreEqual('P0420', R.ResolveDTC('P0420'));
end;

{ ---- TOBDOEMCatalog ----------------------------------------------------- }

procedure TOEMTests.Catalog_EmptyFileNameRaises;
var
  C: TOBDOEMCatalog;
begin
  C := TOBDOEMCatalog.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        C.Load;
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TOEMTests.Catalog_LoadsDIDsFromTempFile;
var
  C: TOBDOEMCatalog;
  Path: string;
  N: Integer;
begin
  Path := TPath.Combine(TPath.GetTempPath, 'oem-catalog-test.json');
  TFile.WriteAllText(Path,
    '{"oem":"test",'
    + '"dids":[{"id":"0xF190","name":"VIN"},'
    +          '{"id":"0xF18C","name":"Serial"}]}',
    TEncoding.UTF8);
  C := TOBDOEMCatalog.Create(nil);
  try
    C.FileName := Path;
    N := C.Load;
    Assert.AreEqual(2, N);
    Assert.IsNotNull(C.Overlay);
    Assert.AreEqual('test', C.Overlay.OEM);
  finally
    C.Free;
    TFile.Delete(Path);
  end;
end;

procedure TOEMTests.Catalog_RegistersOverlayWithRegistry;
var
  C: TOBDOEMCatalog;
  Path: string;
begin
  Path := TPath.Combine(TPath.GetTempPath, 'oem-catalog-test2.json');
  TFile.WriteAllText(Path,
    '{"oem":"reg-test","dids":[{"id":"0xF190","name":"VIN"}]}',
    TEncoding.UTF8);
  C := TOBDOEMCatalog.Create(nil);
  try
    C.FileName := Path;
    C.Load;
    Assert.AreEqual('VIN',
      TOBDOEMRegistry.Instance.Resolve(ikDID, $F190));
  finally
    C.Free;
    TFile.Delete(Path);
  end;
end;

procedure TOEMTests.Catalog_UnloadRemovesFromRegistry;
var
  C: TOBDOEMCatalog;
  Path: string;
begin
  Path := TPath.Combine(TPath.GetTempPath, 'oem-catalog-test3.json');
  TFile.WriteAllText(Path,
    '{"oem":"unload-test","dids":[{"id":"0xF190","name":"VIN"}]}',
    TEncoding.UTF8);
  C := TOBDOEMCatalog.Create(nil);
  try
    C.FileName := Path;
    C.Load;
    Assert.AreEqual(1, TOBDOEMRegistry.Instance.OverlayCount);
    C.Unload;
    Assert.AreEqual(0, TOBDOEMRegistry.Instance.OverlayCount);
  finally
    C.Free;
    TFile.Delete(Path);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMTests);

end.
