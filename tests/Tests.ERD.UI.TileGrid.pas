//------------------------------------------------------------------------------
//  Tests.ERD.UI.TileGrid
//
//  Rendering and behaviour tests for TOBDTileGrid and TOBDKeyValueList.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.TileGrid;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.TileGrid,
  ERD.UI.KeyValueList,
  Tests.ERD.UI.RenderHelpers;

type
  TTileGridCracker = class(TOBDTileGrid)
  public
    procedure PressKey(AKey: Word);
  end;

  [TestFixture]
  TOBDTileGridTests = class
  strict private
    FGrid: TTileGridCracker;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure LargeStyleDiffers;
    [Test] procedure AddTilesAndGroups;
    [Test] procedure FilterHidesTiles;
    [Test] procedure DisabledTileNotSelectable;
    [Test] procedure KeyboardMovesSelection;
  end;

  [TestFixture]
  TOBDKeyValueListTests = class
  strict private
    FList: TOBDKeyValueList;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDraws;
    [Test] procedure TabletDiffers;
    [Test] procedure AddAndSetValue;
    [Test] procedure TwoColumnsDiffer;
    [Test] procedure ContentHeightGrowsWithWrappedValue;
    [Test] procedure RowAtFindsRow;
  end;

implementation

procedure TTileGridCracker.PressKey(AKey: Word);
var
  Key: Word;
begin
  Key := AKey;
  KeyDown(Key, []);
end;

procedure TOBDTileGridTests.Setup;
begin
  FGrid := TTileGridCracker.Create(nil);
  FGrid.SetBounds(0, 0, 760, 260);
end;

procedure TOBDTileGridTests.TearDown;
begin
  FreeAndNil(FGrid);
end;

procedure TOBDTileGridTests.PreviewDraws;
begin
  FGrid.ForcePreview := True;
  Assert.IsTrue(InkRatio(FGrid) > 0.02, 'preview tile grid is empty');
end;

procedure TOBDTileGridTests.EmptyStillDraws;
begin
  Assert.IsTrue(InkRatio(FGrid) >= 0.0, 'empty tile grid raised an error');
end;

procedure TOBDTileGridTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FGrid.ForcePreview := True;
  A := RenderControl(FGrid);
  try
    FGrid.Density := dnTablet;
    B := RenderControl(FGrid);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet tile grid did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDTileGridTests.LargeStyleDiffers;
var
  A, B: TBitmap;
begin
  FGrid.AddTile('Read codes', 'All control units', glCodes);
  FGrid.AddTile('Live data', 'Gauges and trends', glLive);
  A := RenderControl(FGrid);
  try
    FGrid.TileStyle := tgsLarge;
    B := RenderControl(FGrid);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'large tiles did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDTileGridTests.AddTilesAndGroups;
var
  R0, R2: TRect;
begin
  FGrid.AddTile('Oil service reset', '17 Instruments', glWrench,
    'Maintenance');
  FGrid.AddTile('Battery registration', '19 Gateway', glBattery,
    'Maintenance');
  FGrid.AddTile('Parking brake service', '53 Parking brake', glWrench,
    'Brakes and chassis');
  Assert.AreEqual(3, FGrid.Items.Count);
  R0 := FGrid.TileRect(0);
  R2 := FGrid.TileRect(2);
  Assert.IsTrue(R2.Top > R0.Bottom, 'second group did not start below first');
end;

procedure TOBDTileGridTests.FilterHidesTiles;
begin
  FGrid.AddTile('Oil service reset', '17 Instruments', glWrench,
    'Maintenance');
  FGrid.AddTile('Parking brake service', '53 Parking brake', glWrench,
    'Brakes and chassis');
  FGrid.Filter := 'brake';
  Assert.AreEqual(0, FGrid.TileRect(0).Width);
  Assert.IsTrue(FGrid.TileRect(1).Width > 0, 'matching tile is hidden');
end;

procedure TOBDTileGridTests.DisabledTileNotSelectable;
begin
  FGrid.AddTile('Air suspension calibration', 'Not fitted', glAlert,
    'Brakes and chassis').Enabled := False;
  FGrid.ItemIndex := 0;
  Assert.AreEqual(-1, FGrid.ItemIndex);
end;

procedure TOBDTileGridTests.KeyboardMovesSelection;
begin
  FGrid.AddTile('Read codes', 'All control units', glCodes);
  FGrid.AddTile('Live data', 'Gauges and trends', glLive);
  FGrid.ItemIndex := 0;
  FGrid.PressKey(VK_RIGHT);
  Assert.AreEqual(1, FGrid.ItemIndex);
end;

procedure TOBDKeyValueListTests.Setup;
begin
  FList := TOBDKeyValueList.Create(nil);
  FList.SetBounds(0, 0, 520, 260);
end;

procedure TOBDKeyValueListTests.TearDown;
begin
  FreeAndNil(FList);
end;

procedure TOBDKeyValueListTests.PreviewDraws;
begin
  FList.ForcePreview := True;
  Assert.IsTrue(InkRatio(FList) > 0.02, 'preview key/value list is empty');
end;

procedure TOBDKeyValueListTests.TabletDiffers;
var
  A, B: TBitmap;
begin
  FList.ForcePreview := True;
  A := RenderControl(FList);
  try
    FList.Density := dnTablet;
    B := RenderControl(FList);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet key/value list did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDKeyValueListTests.AddAndSetValue;
begin
  FList.Add('Identification', 'Part number', '5Q0937084CK', kvkMono);
  FList.SetValue('Part number', '5Q0937084AN');
  Assert.AreEqual(1, FList.Items.Count);
  Assert.AreEqual('5Q0937084AN', FList.Items[0].Value);
end;

procedure TOBDKeyValueListTests.TwoColumnsDiffer;
var
  A, B: TBitmap;
begin
  FList.Add('Chip', 'Type', '24C32 EEPROM');
  FList.Add('Chip', 'Size', '4 096 bytes', kvkMono);
  FList.Add('Chip', 'Bus', 'I2C - 400 kHz');
  FList.Add('Chip', 'Address', '0x50', kvkMono);
  A := RenderControl(FList);
  try
    FList.ColumnCount := 2;
    B := RenderControl(FList);
    try
      Assert.IsTrue(BitmapsDiffer(A, B, 20), 'two-column list did not differ');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TOBDKeyValueListTests.ContentHeightGrowsWithWrappedValue;
var
  H1, H2: Integer;
begin
  FList.Add('Coding', 'Long coding', '2D 0B 3C 84', kvkMono);
  H1 := FList.ContentHeight;
  FList.Items[0].Value := '2D 0B 3C 84' + #13#10 + '80 6C 03 12';
  H2 := FList.ContentHeight;
  Assert.IsTrue(H2 > H1, 'wrapped value did not increase height');
end;

procedure TOBDKeyValueListTests.RowAtFindsRow;
begin
  FList.ShowCategories := False;
  FList.Add('', 'Part number', '5Q0937084CK', kvkMono);
  Assert.AreEqual(0, FList.RowAt(20, 20));
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDTileGridTests);
  TDUnitX.RegisterTestFixture(TOBDKeyValueListTests);

end.
