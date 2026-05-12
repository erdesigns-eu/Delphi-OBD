//------------------------------------------------------------------------------
//  Tests.OBD.UI.LegacyPorts
//
//  Coverage for the v1 visual components ported to v2 with native
//  OS-control faces:
//    - TOBDTerminal      (TListBox owner-draw face)
//    - TOBDLogViewer     (TOBDTerminal + level tags)
//    - TOBDDtcList       (TListView vsReport face)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.LegacyPorts;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.UI.Terminal,
  OBD.UI.LogViewer,
  OBD.UI.DtcList;

type
  /// <summary>DUnitX fixture for the ported visual components.</summary>
  [TestFixture]
  TLegacyVisualPortsTests = class
  public
    [Test] procedure Terminal_DefaultsMaxLinesAndFollowTail;
    [Test] procedure Terminal_LogAppendsLine;
    [Test] procedure Terminal_RingBufferDropsOldestWhenFull;
    [Test] procedure Terminal_ClearLogEmptiesBuffer;

    [Test] procedure LogViewer_DefaultsShowLevelTag;
    [Test] procedure LogViewer_WriteAppendsRow;
    [Test] procedure LogViewer_LevelTagPrefixed;

    [Test] procedure DtcList_DefaultsThreeColors;
    [Test] procedure DtcList_AddDtcExGrowsCount;
    [Test] procedure DtcList_ClearEmptiesItems;
    [Test] procedure DtcList_DtcAccessorRoundTrips;
  end;

implementation

procedure TLegacyVisualPortsTests.Terminal_DefaultsMaxLinesAndFollowTail;
var
  T: TOBDTerminal;
begin
  T := TOBDTerminal.Create(nil);
  try
    Assert.AreEqual(TERM_DEFAULT_MAX_LINES, T.MaxLines);
    Assert.IsTrue(T.FollowTail);
    Assert.IsTrue(T.ShowTimestamps);
    Assert.AreEqual(0, T.LineCount);
  finally
    T.Free;
  end;
end;

procedure TLegacyVisualPortsTests.Terminal_LogAppendsLine;
var
  T: TOBDTerminal;
begin
  T := TOBDTerminal.Create(nil);
  try
    T.LogSent('ATZ');
    T.LogReceived('ELM327 v2.3');
    Assert.AreEqual(2, T.LineCount);
    Assert.AreEqual(Ord(tdSent), Ord(T.Line(0).Direction));
    Assert.AreEqual('ATZ', T.Line(0).Text);
    Assert.AreEqual(Ord(tdReceived), Ord(T.Line(1).Direction));
  finally
    T.Free;
  end;
end;

procedure TLegacyVisualPortsTests.Terminal_RingBufferDropsOldestWhenFull;
var
  T: TOBDTerminal;
  I: Integer;
begin
  T := TOBDTerminal.Create(nil);
  try
    T.MaxLines := 3;
    for I := 1 to 5 do
      T.LogInfo('row ' + IntToStr(I));
    Assert.AreEqual(3, T.LineCount);
    Assert.AreEqual('row 3', T.Line(0).Text);
    Assert.AreEqual('row 5', T.Line(2).Text);
  finally
    T.Free;
  end;
end;

procedure TLegacyVisualPortsTests.Terminal_ClearLogEmptiesBuffer;
var
  T: TOBDTerminal;
begin
  T := TOBDTerminal.Create(nil);
  try
    T.LogError('boom');
    T.LogError('crash');
    Assert.AreEqual(2, T.LineCount);
    T.ClearLog;
    Assert.AreEqual(0, T.LineCount);
  finally
    T.Free;
  end;
end;

procedure TLegacyVisualPortsTests.LogViewer_DefaultsShowLevelTag;
var
  V: TOBDLogViewer;
begin
  V := TOBDLogViewer.Create(nil);
  try
    Assert.IsTrue(V.ShowLevelTag);
  finally
    V.Free;
  end;
end;

procedure TLegacyVisualPortsTests.LogViewer_WriteAppendsRow;
var
  V: TOBDLogViewer;
begin
  V := TOBDLogViewer.Create(nil);
  try
    V.Info('hello');
    V.Warn('be careful');
    V.Error('oops');
    Assert.AreEqual(3, V.LineCount);
  finally
    V.Free;
  end;
end;

procedure TLegacyVisualPortsTests.LogViewer_LevelTagPrefixed;
var
  V: TOBDLogViewer;
begin
  V := TOBDLogViewer.Create(nil);
  try
    V.Info('hello');
    Assert.Contains(V.Line(0).Text, '[INFO]');
    V.ShowLevelTag := False;
    V.Info('plain');
    Assert.AreEqual('plain', V.Line(1).Text);
  finally
    V.Free;
  end;
end;

procedure TLegacyVisualPortsTests.DtcList_DefaultsThreeColors;
var
  L: TOBDDtcList;
begin
  L := TOBDDtcList.Create(nil);
  try
    Assert.AreEqual(0, L.DtcCount);
    Assert.AreNotEqual(L.InfoColor, L.WarningColor);
    Assert.AreNotEqual(L.WarningColor, L.CriticalColor);
  finally
    L.Free;
  end;
end;

procedure TLegacyVisualPortsTests.DtcList_AddDtcExGrowsCount;
var
  L: TOBDDtcList;
begin
  L := TOBDDtcList.Create(nil);
  try
    L.AddDtcEx('P0301', 'Cylinder 1 misfire', dsCritical, dtActive);
    L.AddDtcEx('P0420', 'Catalyst efficiency low', dsWarning, dtPending);
    Assert.AreEqual(2, L.DtcCount);
  finally
    L.Free;
  end;
end;

procedure TLegacyVisualPortsTests.DtcList_ClearEmptiesItems;
var
  L: TOBDDtcList;
begin
  L := TOBDDtcList.Create(nil);
  try
    L.AddDtcEx('U0100', 'Lost comm with ECM');
    Assert.AreEqual(1, L.DtcCount);
    L.ClearDtcs;
    Assert.AreEqual(0, L.DtcCount);
  finally
    L.Free;
  end;
end;

procedure TLegacyVisualPortsTests.DtcList_DtcAccessorRoundTrips;
var
  L: TOBDDtcList;
  Row: TOBDDtcItem;
begin
  L := TOBDDtcList.Create(nil);
  try
    L.AddDtcEx('B1234', 'Body code', dsInfo, dtHistory);
    Row := L.Dtc(0);
    Assert.AreEqual('B1234', Row.Code);
    Assert.AreEqual(Ord(dsInfo), Ord(Row.Severity));
    Assert.AreEqual(Ord(dtHistory), Ord(Row.Status));
  finally
    L.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLegacyVisualPortsTests);

end.
