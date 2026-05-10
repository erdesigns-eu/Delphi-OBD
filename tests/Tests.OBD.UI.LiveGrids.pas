//------------------------------------------------------------------------------
//  Tests.OBD.UI.LiveGrids
//
//  Non-visual contract for the A2.6 live-data grids. The grids
//  descend from TListView; their public surface needs a valid
//  HWND to render rows, so tests focus on the lifecycle +
//  property contract rather than the on-screen output.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.LiveGrids;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.Service.LiveData,
  OBD.Service.FreezeFrame,
  OBD.UI.LiveGrids;

type
  [TestFixture]
  TMultiPidGridTests = class
  public
    [Test] procedure DefaultsReadOnlyReport;
    [Test] procedure PIDsAssignAndReadBack;
    [Test] procedure FreeNotificationClearsLiveData;
    [Test] procedure FreeIsClean;
  end;

  [TestFixture]
  TFreezeFrameTableTests = class
  public
    [Test] procedure DefaultsFrameIndexZero;
    [Test] procedure PIDsAssignAndReadBack;
    [Test] procedure RefreshWithoutFreezeFrameIsNoOp;
    [Test] procedure FreeNotificationClearsFreezeFrame;
  end;

implementation

{ TMultiPidGridTests ------------------------------------------------------ }

procedure TMultiPidGridTests.DefaultsReadOnlyReport;
var G: TOBDMultiPidGrid;
begin
  G := TOBDMultiPidGrid.Create(nil);
  try
    Assert.IsTrue(G.ReadOnly);
    Assert.IsTrue(G.GridLines);
    Assert.IsTrue(G.RowSelect);
  finally G.Free; end;
end;

procedure TMultiPidGridTests.PIDsAssignAndReadBack;
var G: TOBDMultiPidGrid;
begin
  G := TOBDMultiPidGrid.Create(nil);
  try
    G.PIDs.Add('0x0C');
    G.PIDs.Add('0x0D');
    G.PIDs.Add('15');     // decimal 21
    Assert.AreEqual(3, G.PIDs.Count);
    Assert.AreEqual('0x0C', G.PIDs[0]);
  finally G.Free; end;
end;

procedure TMultiPidGridTests.FreeNotificationClearsLiveData;
var
  G:  TOBDMultiPidGrid;
  LD: TOBDLiveData;
begin
  LD := TOBDLiveData.Create(nil);
  G  := TOBDMultiPidGrid.Create(nil);
  try
    G.LiveData := LD;
    Assert.IsNotNull(G.LiveData);
    LD.Free; LD := nil;
    Assert.IsNull(G.LiveData);
  finally
    G.Free;
    if LD <> nil then LD.Free;
  end;
end;

procedure TMultiPidGridTests.FreeIsClean;
var G: TOBDMultiPidGrid;
begin
  G := TOBDMultiPidGrid.Create(nil);
  G.PIDs.Add('0x0C');
  G.Free;
  Assert.Pass;
end;

{ TFreezeFrameTableTests -------------------------------------------------- }

procedure TFreezeFrameTableTests.DefaultsFrameIndexZero;
var T: TOBDFreezeFrameTable;
begin
  T := TOBDFreezeFrameTable.Create(nil);
  try
    Assert.AreEqual<Byte>(0, T.FrameIndex);
    Assert.IsTrue(T.GridLines);
    Assert.IsTrue(T.RowSelect);
  finally T.Free; end;
end;

procedure TFreezeFrameTableTests.PIDsAssignAndReadBack;
var T: TOBDFreezeFrameTable;
begin
  T := TOBDFreezeFrameTable.Create(nil);
  try
    T.PIDs.Add('0x05');     // ECT
    T.PIDs.Add('0x0C');     // RPM
    Assert.AreEqual(2, T.PIDs.Count);
  finally T.Free; end;
end;

procedure TFreezeFrameTableTests.RefreshWithoutFreezeFrameIsNoOp;
var T: TOBDFreezeFrameTable;
begin
  T := TOBDFreezeFrameTable.Create(nil);
  try
    T.Refresh;       // FreezeFrame is nil; must silently no-op
    Assert.Pass;
  finally T.Free; end;
end;

procedure TFreezeFrameTableTests.FreeNotificationClearsFreezeFrame;
var
  T: TOBDFreezeFrameTable;
  F: TOBDFreezeFrame;
begin
  F := TOBDFreezeFrame.Create(nil);
  T := TOBDFreezeFrameTable.Create(nil);
  try
    T.FreezeFrame := F;
    Assert.IsNotNull(T.FreezeFrame);
    F.Free; F := nil;
    Assert.IsNull(T.FreezeFrame);
  finally
    T.Free;
    if F <> nil then F.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMultiPidGridTests);
  TDUnitX.RegisterTestFixture(TFreezeFrameTableTests);

end.
