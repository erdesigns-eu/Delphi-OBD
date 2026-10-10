//------------------------------------------------------------------------------
//  Tests.ERD.UI.Dashboard
//
//  Behaviour tests for TOBDDashboard: tile kinds, cell placement and
//  growth, removal, layout save / load round-trip, data-source
//  propagation and rendering of the empty grid.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Dashboard;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  ERD.UI.Control,
  ERD.UI.Gauges.Dial,
  ERD.UI.Gauges.Bar,
  ERD.UI.ValueTile,
  ERD.UI.TrendChart,
  ERD.UI.StatusLamp,
  ERD.UI.MatrixDisplay,
  ERD.UI.LiveDataGrid,
  ERD.UI.Dashboard,
  ERD.Service.LiveData,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDDashboardTests = class
  strict private
    FDash: TOBDDashboard;
    FLayoutEvents: Integer;
    procedure LayoutChangedHandler(Sender: TObject);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure EveryKindIsRegistered;
    [Test] procedure KindOfMapsClassesBack;
    [Test] procedure AddTileCreatesEachKind;
    [Test] procedure UnknownKindRaises;
    [Test] procedure AddTileHonoursCells;
    [Test] procedure OccupiedCellIsNotFree;
    [Test] procedure AutoPlacementFillsFreeCells;
    [Test] procedure FullGridGrowsDownwards;
    [Test] procedure OversizedSpanMovesToFreeArea;
    [Test] procedure RemoveTileFreesCell;
    [Test] procedure ClearTilesEmptiesGrid;
    [Test] procedure LayoutRoundTrip;
    [Test] procedure LoadLayoutRejectsOtherJson;
    [Test] procedure LoadLayoutSkipsUnknownKinds;
    [Test] procedure SourceReachesTiles;
    [Test] procedure LayoutChangeIsReported;
    [Test] procedure EmptyGridDraws;
  end;

implementation

procedure TOBDDashboardTests.Setup;
begin
  FDash := TOBDDashboard.Create(nil);
  FDash.SetBounds(0, 0, 800, 600);
  FLayoutEvents := 0;
end;

procedure TOBDDashboardTests.TearDown;
begin
  FreeAndNil(FDash);
end;

procedure TOBDDashboardTests.LayoutChangedHandler(Sender: TObject);
begin
  Inc(FLayoutEvents);
end;

procedure TOBDDashboardTests.EveryKindIsRegistered;
const
  Expected: array [0 .. 6] of string = ('dial', 'bar', 'value', 'trend',
    'lamp', 'matrix', 'grid');
var
  Kinds: TArray<string>;
  K: string;
  I: Integer;
  Found: Boolean;
begin
  Kinds := TOBDDashboard.TileKinds;
  for I := Low(Expected) to High(Expected) do
  begin
    Found := False;
    for K in Kinds do
      Found := Found or SameText(K, Expected[I]);
    Assert.IsTrue(Found, 'tile kind "' + Expected[I] + '" not registered');
  end;
end;

procedure TOBDDashboardTests.KindOfMapsClassesBack;
begin
  Assert.AreEqual('dial', TOBDDashboard.KindOf(TOBDDialGauge));
  Assert.AreEqual('bar', TOBDDashboard.KindOf(TOBDBarGauge));
  Assert.AreEqual('value', TOBDDashboard.KindOf(TOBDValueTile));
  Assert.AreEqual('trend', TOBDDashboard.KindOf(TOBDTrendChart));
  Assert.AreEqual('lamp', TOBDDashboard.KindOf(TOBDStatusLamp));
  Assert.AreEqual('matrix', TOBDDashboard.KindOf(TOBDMatrixDisplay));
  Assert.AreEqual('grid', TOBDDashboard.KindOf(TOBDLiveDataGrid));
end;

procedure TOBDDashboardTests.AddTileCreatesEachKind;
begin
  Assert.IsTrue(FDash.AddTile('dial') is TOBDDialGauge);
  Assert.IsTrue(FDash.AddTile('bar') is TOBDBarGauge);
  Assert.IsTrue(FDash.AddTile('value') is TOBDValueTile);
  Assert.IsTrue(FDash.AddTile('trend') is TOBDTrendChart);
  Assert.IsTrue(FDash.AddTile('lamp') is TOBDStatusLamp);
  Assert.IsTrue(FDash.AddTile('matrix') is TOBDMatrixDisplay);
  Assert.IsTrue(FDash.AddTile('grid') is TOBDLiveDataGrid);
  Assert.AreEqual(7, FDash.Tiles.Count);
  Assert.AreEqual(7, FDash.ControlCount);
end;

procedure TOBDDashboardTests.UnknownKindRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      FDash.AddTile('speedometer-3d');
    end, EOBDDashboard);
  Assert.AreEqual(0, FDash.Tiles.Count);
end;

procedure TOBDDashboardTests.AddTileHonoursCells;
var
  C: TOBDCustomControl;
  T: TOBDDashboardTile;
begin
  C := FDash.AddTile('dial', 1, 1, 2, 2);
  T := FDash.Tiles.FindControl(C);
  Assert.IsNotNull(T);
  Assert.AreEqual(1, T.Col);
  Assert.AreEqual(1, T.Row);
  Assert.AreEqual(2, T.ColSpan);
  Assert.AreEqual(2, T.RowSpan);
end;

procedure TOBDDashboardTests.OccupiedCellIsNotFree;
begin
  FDash.AddTile('dial', 1, 1, 2, 2);
  Assert.IsFalse(FDash.IsFree(2, 2, 1, 1));
  Assert.IsFalse(FDash.IsFree(0, 0, 2, 2));
  Assert.IsTrue(FDash.IsFree(0, 0, 1, 1));
  Assert.IsTrue(FDash.IsFree(3, 0, 1, 3));
  Assert.IsFalse(FDash.IsFree(3, 0, 2, 1), 'span past the last column');
end;

procedure TOBDDashboardTests.AutoPlacementFillsFreeCells;
var
  C: TOBDCustomControl;
  T: TOBDDashboardTile;
begin
  FDash.AddTile('value', 0, 0);
  C := FDash.AddTile('value');
  T := FDash.Tiles.FindControl(C);
  Assert.AreEqual(1, T.Col);
  Assert.AreEqual(0, T.Row);
  C := FDash.AddTile('value', 1, 0);
  T := FDash.Tiles.FindControl(C);
  Assert.IsFalse((T.Col = 1) and (T.Row = 0), 'two tiles share a cell');
end;

procedure TOBDDashboardTests.FullGridGrowsDownwards;
var
  I: Integer;
  C: TOBDCustomControl;
begin
  for I := 0 to FDash.Columns * FDash.Rows - 1 do
    FDash.AddTile('value');
  Assert.AreEqual(3, FDash.Rows);
  C := FDash.AddTile('value');
  Assert.AreEqual(4, FDash.Rows);
  Assert.AreEqual(3, FDash.Tiles.FindControl(C).Row);
end;

procedure TOBDDashboardTests.OversizedSpanMovesToFreeArea;
var
  T: TOBDDashboardTile;
begin
  T := FDash.Tiles.FindControl(FDash.AddTile('trend', 0, 2, 4, 2));
  Assert.AreEqual(0, T.Row, 'span past the last row not moved');
  Assert.AreEqual(3, FDash.Rows);
end;

procedure TOBDDashboardTests.RemoveTileFreesCell;
var
  C: TOBDCustomControl;
begin
  C := FDash.AddTile('lamp', 2, 0);
  Assert.IsFalse(FDash.IsFree(2, 0, 1, 1));
  FDash.RemoveTile(C);
  Assert.IsTrue(FDash.IsFree(2, 0, 1, 1));
  Assert.AreEqual(0, FDash.Tiles.Count);
  Assert.AreEqual(0, FDash.ControlCount);
end;

procedure TOBDDashboardTests.ClearTilesEmptiesGrid;
begin
  FDash.AddTile('dial');
  FDash.AddTile('bar');
  FDash.AddTile('matrix');
  FDash.ClearTiles;
  Assert.AreEqual(0, FDash.Tiles.Count);
  Assert.AreEqual(0, FDash.ControlCount);
end;

procedure TOBDDashboardTests.LayoutRoundTrip;
var
  Dial: TOBDDialGauge;
  Matrix: TOBDMatrixDisplay;
  Json: string;
  Other: TOBDDashboard;
  T: TOBDDashboardTile;
begin
  FDash.Columns := 6;
  FDash.Gap := 12;
  Dial := FDash.AddTile('dial', 0, 0, 2, 2) as TOBDDialGauge;
  Dial.Caption := 'Engine speed';
  Dial.Max := 8000;
  Matrix := FDash.AddTile('matrix', 2, 0, 4, 1) as TOBDMatrixDisplay;
  Matrix.Preset := mxpTicker;
  Matrix.Text := 'WELCOME';
  FDash.AddTile('grid', 2, 1, 4, 2);
  Json := FDash.SaveLayout;

  Other := TOBDDashboard.Create(nil);
  try
    Other.LoadLayout(Json);
    Assert.AreEqual(6, Other.Columns);
    Assert.AreEqual(12, Other.Gap);
    Assert.AreEqual(3, Other.Tiles.Count);

    T := Other.Tiles[0];
    Assert.IsTrue(T.Control is TOBDDialGauge);
    Assert.AreEqual(2, T.ColSpan);
    Assert.AreEqual(2, T.RowSpan);
    Assert.AreEqual('Engine speed', TOBDDialGauge(T.Control).Caption);
    Assert.AreEqual(Double(8000), TOBDDialGauge(T.Control).Max, 1e-9);

    T := Other.Tiles[1];
    Assert.IsTrue(T.Control is TOBDMatrixDisplay);
    Assert.AreEqual(2, T.Col);
    Assert.AreEqual(4, T.ColSpan);
    Assert.AreEqual('WELCOME', TOBDMatrixDisplay(T.Control).Text);
    Assert.AreEqual(Ord(mxsLeft), Ord(TOBDMatrixDisplay(T.Control).Scroll));

    T := Other.Tiles[2];
    Assert.IsTrue(T.Control is TOBDLiveDataGrid);
    Assert.AreEqual(1, T.Row);

    Assert.AreEqual(Json, Other.SaveLayout, 'layout not stable on reload');
  finally
    Other.Free;
  end;
end;

procedure TOBDDashboardTests.LoadLayoutRejectsOtherJson;
begin
  FDash.AddTile('dial');
  Assert.WillRaise(
    procedure
    begin
      FDash.LoadLayout('{"columns": 4}');
    end, EOBDDashboard);
  Assert.AreEqual(1, FDash.Tiles.Count, 'rejected layout cleared the grid');
end;

procedure TOBDDashboardTests.LoadLayoutSkipsUnknownKinds;
begin
  FDash.LoadLayout('{"columns": 4, "rows": 3, "tiles": [' +
    '{"kind": "dial", "col": 0, "row": 0},' +
    '{"kind": "hologram", "col": 1, "row": 0},' +
    '{"kind": "lamp", "col": 2, "row": 0}]}');
  Assert.AreEqual(2, FDash.Tiles.Count);
  Assert.IsTrue(FDash.Tiles[1].Control is TOBDStatusLamp);
end;

procedure TOBDDashboardTests.SourceReachesTiles;
var
  Live: TOBDLiveData;
  Dial: TOBDDialGauge;
  Bar: TOBDBarGauge;
begin
  Live := TOBDLiveData.Create(nil);
  try
    Dial := FDash.AddTile('dial') as TOBDDialGauge;
    FDash.Source := Live;
    Assert.AreSame(Live, Dial.Channel.Source);
    Bar := FDash.AddTile('bar') as TOBDBarGauge;
    Assert.AreSame(Live, Bar.Channel.Source, 'new tile not bound');
    FDash.Source := nil;
    Assert.IsNull(Dial.Channel.Source);
  finally
    FreeAndNil(FDash);
    Live.Free;
  end;
end;

procedure TOBDDashboardTests.LayoutChangeIsReported;
begin
  FDash.OnLayoutChanged := LayoutChangedHandler;
  FDash.AddTile('value');
  Assert.IsTrue(FLayoutEvents > 0);
end;

procedure TOBDDashboardTests.EmptyGridDraws;
begin
  Assert.IsTrue(InkRatio(FDash) > 0, 'empty dashboard draws nothing');
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDDashboardTests);

end.
