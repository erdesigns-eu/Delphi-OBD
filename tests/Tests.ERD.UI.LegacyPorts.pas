//------------------------------------------------------------------------------
//  Tests.ERD.UI.LegacyPorts
//
//  Coverage for the visual components rendered with native
//  OS-control faces:
//    - TOBDTerminal      (TListBox owner-draw face)
//    - TOBDLogViewer     (TOBDTerminal + level tags)
//    - TOBDDtcList       (TListView vsReport face)
//  and their optional TOBDTheme palette.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.LegacyPorts;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Theme,
  ERD.UI.Terminal,
  ERD.UI.LogViewer,
  ERD.UI.DtcList;

type
  /// <summary>DUnitX fixture for the ported visual components.</summary>
  [TestFixture]
  TLegacyVisualPortsTests = class
  public
    [Test] procedure Terminal_DefaultsMaxLinesAndFollowTail;
    [Test] procedure Terminal_LogAppendsLine;
    [Test] procedure Terminal_RingBufferDropsOldestWhenFull;
    [Test] procedure Terminal_ClearLogEmptiesBuffer;
    [Test] procedure Terminal_ThemeSetsPaletteColours;
    [Test] procedure Terminal_FreeingThemeClearsTheme;

    [Test] procedure LogViewer_DefaultsShowLevelTag;
    [Test] procedure LogViewer_WriteAppendsRow;
    [Test] procedure LogViewer_LevelTagPrefixed;
    [Test] procedure LogViewer_LevelsMapToDirections;

    [Test] procedure DtcList_DefaultsThreeColors;
    [Test] procedure DtcList_AddDtcExGrowsCount;
    [Test] procedure DtcList_ClearEmptiesItems;
    [Test] procedure DtcList_DtcAccessorRoundTrips;
    [Test] procedure DtcList_ThemeSetsPaletteColours;
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

procedure TLegacyVisualPortsTests.LogViewer_LevelsMapToDirections;
var
  V: TOBDLogViewer;
begin
  V := TOBDLogViewer.Create(nil);
  try
    V.Info('info');
    V.Warn('warning');
    V.Error('error');
    V.Critical('critical');
    Assert.AreEqual(Ord(tdInfo), Ord(V.Line(0).Direction));
    Assert.AreEqual(Ord(tdWarning), Ord(V.Line(1).Direction));
    Assert.AreEqual(Ord(tdError), Ord(V.Line(2).Direction));
    Assert.AreEqual(Ord(tdError), Ord(V.Line(3).Direction));
    Assert.AreNotEqual(Integer(V.InfoColor), Integer(V.WarningColor));
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

procedure TLegacyVisualPortsTests.Terminal_ThemeSetsPaletteColours;
var
  T: TOBDTerminal;
  Th: TOBDTheme;
begin
  Th := TOBDTheme.Create(nil);
  T := TOBDTerminal.Create(nil);
  try
    Th.Mode := tmDark;
    T.Theme := Th;
    Assert.AreEqual(Integer(BRAND_PALETTE_DARK.GaugeFace), Integer(T.Color));
    Assert.AreEqual(Integer(BRAND_PALETTE_DARK.ForegroundText),
      Integer(T.Font.Color));
    Th.Mode := tmLight;
    Assert.AreEqual(Integer(BRAND_PALETTE_LIGHT.GaugeFace), Integer(T.Color));
  finally
    T.Free;
    Th.Free;
  end;
end;

procedure TLegacyVisualPortsTests.Terminal_FreeingThemeClearsTheme;
var
  T: TOBDTerminal;
  Th: TOBDTheme;
begin
  Th := TOBDTheme.Create(nil);
  T := TOBDTerminal.Create(nil);
  try
    T.Theme := Th;
    FreeAndNil(Th);
    Assert.IsNull(T.Theme);
    T.LogInfo('still works');
    Assert.AreEqual(1, T.LineCount);
  finally
    T.Free;
    Th.Free;
  end;
end;

procedure TLegacyVisualPortsTests.DtcList_ThemeSetsPaletteColours;
var
  L: TOBDDtcList;
  Th: TOBDTheme;
begin
  Th := TOBDTheme.Create(nil);
  L := TOBDDtcList.Create(nil);
  try
    Th.Mode := tmLight;
    L.Theme := Th;
    Assert.AreEqual(Integer(BRAND_PALETTE_LIGHT.GaugeFace), Integer(L.Color));
    Assert.AreEqual(Integer(BRAND_PALETTE_LIGHT.ForegroundText),
      Integer(L.Font.Color));
  finally
    L.Free;
    Th.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLegacyVisualPortsTests);

end.
