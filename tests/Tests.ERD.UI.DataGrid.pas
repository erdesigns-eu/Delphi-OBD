//------------------------------------------------------------------------------
//  Tests.ERD.UI.DataGrid
//
//  Rendering and behaviour tests for the common OBD Studio data grid.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.DataGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.DataGrid,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDDataGridTests = class
  strict private
    FGrid: TOBDDataGrid;
    FCheckCount: Integer;
    procedure HandleCheck(Sender: TObject; ARow, ACol: Integer);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddRowsAndCells;
    [Test] procedure SortNumericDescending;
    [Test] procedure SortKeepsSelection;
    [Test] procedure GroupCollapseHidesRows;
    [Test] procedure CheckToggleFiresEvent;
    [Test] procedure ColumnResizeRespectsMinWidth;
  end;

implementation

procedure TOBDDataGridTests.Setup;
begin
  FGrid := TOBDDataGrid.Create(nil);
  FGrid.SetBounds(0, 0, 720, 360);
  FGrid.EmptyTitle := 'No rows';
  FCheckCount := 0;
end;

procedure TOBDDataGridTests.TearDown;
begin
  FreeAndNil(FGrid);
end;

procedure TOBDDataGridTests.HandleCheck(Sender: TObject; ARow, ACol: Integer);
begin
  Inc(FCheckCount);
end;

procedure TOBDDataGridTests.PreviewDraws;
begin
  FGrid.ForcePreview := True;
  Assert.IsTrue(InkRatio(FGrid) > 0.02, 'preview data grid is empty');
end;

procedure TOBDDataGridTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FGrid) > 0.002, 'empty data grid draws nothing');
end;

procedure TOBDDataGridTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FGrid.ForcePreview := True;
  A := RenderControl(FGrid);
  try
    FGrid.Density := dnTablet;
    B := RenderControl(FGrid);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet data grid did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDDataGridTests.AddRowsAndCells;
begin
  Assert.AreEqual(0, FGrid.AddRow(['Run A', '2026-10-12', 'PASS']));
  Assert.AreEqual(1, FGrid.RowCount);
  Assert.AreEqual('Run A', FGrid.Cells[0, 0]);
  FGrid.Cells[1, 0] := '2026-10-13';
  FGrid.RowData[0] := 42;
  FGrid.RowGlyph[0] := glDyno;
  FGrid.RowEnabled[0] := False;
  FGrid.RowAccent[0] := True;
  FGrid.CellTone[2, 0] := skSuccess;
  Assert.AreEqual('2026-10-13', FGrid.Cells[1, 0]);
  Assert.AreEqual(NativeInt(42), FGrid.RowData[0]);
  Assert.AreEqual(Integer(glDyno), Integer(FGrid.RowGlyph[0]));
  Assert.IsFalse(FGrid.RowEnabled[0]);
  Assert.IsTrue(FGrid.RowAccent[0]);
  Assert.AreEqual(Integer(skSuccess), Integer(FGrid.CellTone[2, 0]));
end;

procedure TOBDDataGridTests.SortNumericDescending;
begin
  FGrid.DataColumns[0].Kind := dckNumber;
  FGrid.AddRow(['80.3']);
  FGrid.AddRow(['79.1']);
  FGrid.AddRow(['100']);
  FGrid.SortColumn := 0;
  FGrid.SortDescending := True;
  Assert.AreEqual('100', FGrid.Cells[0, 0]);
end;

procedure TOBDDataGridTests.SortKeepsSelection;
begin
  FGrid.DataColumns[0].Kind := dckNumber;
  FGrid.AddRow(['80.3']);
  FGrid.RowData[0] := 803;
  FGrid.AddRow(['100']);
  FGrid.RowData[1] := 1000;
  FGrid.ItemIndex := 0;
  FGrid.SortColumn := 0;
  FGrid.SortDescending := True;
  Assert.AreEqual(1, FGrid.ItemIndex);
end;

procedure TOBDDataGridTests.GroupCollapseHidesRows;
begin
  FGrid.AddGroup('Dyno runs');
  FGrid.AddRow(['A']);
  FGrid.AddRow(['B']);
  Assert.AreEqual(3, FGrid.ItemCount);
  FGrid.GroupExpanded[0] := False;
  Assert.AreEqual(1, FGrid.ItemCount);
  Assert.AreEqual(2, FGrid.RowCount);
end;

procedure TOBDDataGridTests.CheckToggleFiresEvent;
begin
  FGrid.DataColumns[0].Kind := dckCheck;
  FGrid.AddRow(['']);
  FGrid.OnCheckClick := HandleCheck;
  FGrid.ToggleCheck(0, 0);
  Assert.IsTrue(FGrid.RowChecked[0]);
  Assert.AreEqual(1, FCheckCount);
end;

procedure TOBDDataGridTests.ColumnResizeRespectsMinWidth;
begin
  FGrid.DataColumns[0].MinWidth := 64;
  FGrid.SetColumnWidth(0, 8);
  Assert.AreEqual(64, FGrid.DataColumns[0].Width);
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDDataGridTests);

end.
