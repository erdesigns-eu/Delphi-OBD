//------------------------------------------------------------------------------
//  Tests.ERD.UI.Dialogs
//
//  Tests for the themed dialogs and their building blocks: the item
//  list base / list box (selection, multi-select, layout, painting)
//  and the dialog components' defaults.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Dialogs;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.ItemList,
  ERD.UI.Dialogs,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDItemListTests = class
  strict private
    FChanges: Integer;
    procedure CountChange(Sender: TObject);
    function NewList(ACount: Integer): TOBDListBox;
  public
    [Setup] procedure Setup;
    [Test] procedure ListBoxDrawsItems;
    [Test] procedure ItemIndexSelectsOneItem;
    [Test] procedure MultiSelectKeepsSeveralItems;
    [Test] procedure ItemAtMatchesItemRect;
    [Test] procedure RemovingItemsClampsTheIndex;
  end;

  [TestFixture]
  TOBDDialogComponentTests = class
  public
    [Test] procedure MessageDialogKeepsItsDefaults;
  end;

implementation

{ TOBDItemListTests }

procedure TOBDItemListTests.Setup;
begin
  FChanges := 0;
end;

procedure TOBDItemListTests.CountChange(Sender: TObject);
begin
  Inc(FChanges);
end;

function TOBDItemListTests.NewList(ACount: Integer): TOBDListBox;
var
  I: Integer;
begin
  Result := TOBDListBox.Create(nil);
  Result.SetBounds(0, 0, 200, 120);
  for I := 0 to ACount - 1 do
    Result.Items.Add('Item ' + IntToStr(I));
  Result.OnChange := CountChange;
end;

procedure TOBDItemListTests.ListBoxDrawsItems;
var
  List: TOBDListBox;
begin
  List := NewList(40);
  try
    Assert.AreEqual(40, List.ItemCount);
    Assert.IsTrue(InkRatio(List) > 0.01, 'list box draws nothing');
  finally
    List.Free;
  end;
end;

procedure TOBDItemListTests.ItemIndexSelectsOneItem;
var
  List: TOBDListBox;
begin
  List := NewList(10);
  try
    List.ItemIndex := 3;
    Assert.AreEqual(3, List.ItemIndex);
    Assert.IsTrue(List.Selected[3]);
    Assert.AreEqual(1, List.SelCount);
    Assert.AreEqual(1, FChanges, 'OnChange');
    List.ItemIndex := 5;
    Assert.IsFalse(List.Selected[3], 'old item still selected');
    Assert.AreEqual(1, List.SelCount);
    List.ItemIndex := 99;
    Assert.AreEqual(-1, List.ItemIndex, 'out of range not cleared');
    Assert.AreEqual(0, List.SelCount);
  finally
    List.Free;
  end;
end;

procedure TOBDItemListTests.MultiSelectKeepsSeveralItems;
var
  List: TOBDListBox;
begin
  List := NewList(10);
  try
    List.MultiSelect := True;
    List.Selected[1] := True;
    List.Selected[4] := True;
    Assert.AreEqual(2, List.SelCount);
    List.SelectAll;
    Assert.AreEqual(10, List.SelCount);
    List.ClearSelection;
    Assert.AreEqual(0, List.SelCount);
    Assert.AreEqual(-1, List.ItemIndex);
  finally
    List.Free;
  end;
end;

procedure TOBDItemListTests.ItemAtMatchesItemRect;
var
  List: TOBDListBox;
  R: TRect;
begin
  List := NewList(3);
  try
    R := List.ItemRect(2);
    Assert.AreEqual(2, List.ItemAt(R.Left + 4, R.Top + 2));
    Assert.AreEqual(-1, List.ItemAt(R.Left + 4, R.Bottom + 40),
      'empty area below the items');
  finally
    List.Free;
  end;
end;

procedure TOBDItemListTests.RemovingItemsClampsTheIndex;
var
  List: TOBDListBox;
begin
  List := NewList(10);
  try
    List.ItemIndex := 8;
    List.Items.Delete(9);
    List.Items.Delete(8);
    Assert.AreEqual(-1, List.ItemIndex);
    Assert.AreEqual(0, List.SelCount);
  finally
    List.Free;
  end;
end;

{ TOBDDialogComponentTests }

procedure TOBDDialogComponentTests.MessageDialogKeepsItsDefaults;
var
  Dialog: TOBDDialog;
begin
  Dialog := TOBDDialog.Create(nil);
  try
    Assert.AreEqual(Ord(dkWarning), Ord(Dialog.Kind));
  finally
    Dialog.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDItemListTests);
  TDUnitX.RegisterTestFixture(TOBDDialogComponentTests);

end.
