//------------------------------------------------------------------------------
//  Tests.ERD.UI.Controls
//
//  Rendering and behaviour tests for the OBD Studio layout and input
//  controls: splitter, scroll box, spin edit, track bar, memo, calendar,
//  date picker and the collapsible card. Each control is drawn
//  off-screen and checked for ink; value controls are checked for
//  clamping and key handling.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Controls;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  Vcl.Controls,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Card,
  ERD.UI.Splitter,
  ERD.UI.ScrollBox,
  ERD.UI.SpinEdit,
  ERD.UI.TrackBar,
  ERD.UI.Memo,
  ERD.UI.Calendar,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDLayoutControlTests = class
  public
    [Test] procedure SplitterDrawsAndFollowsDensity;
    [Test] procedure SplitterTopAlignIsHorizontal;
    [Test] procedure ScrollBoxClampsAndMovesChildren;
    [Test] procedure CardCollapsesToHeaderAndBack;
  end;

  [TestFixture]
  TOBDInputControlTests = class
  strict private
    FChanges: Integer;
    procedure CountChange(Sender: TObject);
  public
    [Setup] procedure Setup;
    [Test] procedure SpinEditClampsAndSteps;
    [Test] procedure SpinEditShowsDecimals;
    [Test] procedure TrackBarClampsAndNotifies;
    [Test] procedure MemoDrawsFrameAndKeepsText;
    [Test] procedure CalendarKeysMoveTheDate;
    [Test] procedure CalendarIsLargerOnTablet;
    [Test] procedure DatePickerFormatsTheDate;
  end;

implementation

{ TOBDLayoutControlTests }

procedure TOBDLayoutControlTests.SplitterDrawsAndFollowsDensity;
var
  Splitter: TOBDSplitter;
  Desktop: Integer;
begin
  Splitter := TOBDSplitter.Create(nil);
  try
    Splitter.Height := 120;
    Assert.AreEqual(Ord(alLeft), Ord(Splitter.Align), 'default align');
    Splitter.Density := dnDesktop;
    Desktop := Splitter.Width;
    Assert.IsTrue(Desktop > 0, 'no thickness');
    Splitter.Density := dnTablet;
    Assert.IsTrue(Splitter.Width > Desktop, 'tablet splitter is not wider');
    Assert.IsTrue(InkRatio(Splitter) > 0, 'splitter draws no grip');
  finally
    Splitter.Free;
  end;
end;

procedure TOBDLayoutControlTests.SplitterTopAlignIsHorizontal;
var
  Splitter: TOBDSplitter;
begin
  Splitter := TOBDSplitter.Create(nil);
  try
    Splitter.Width := 200;
    Splitter.Align := alTop;
    Splitter.Thickness := 8;
    Assert.AreEqual(Integer(crVSplit), Integer(Splitter.Cursor), 'cursor');
    Assert.IsTrue(Splitter.Height <= 16, 'thickness not applied to height');
  finally
    Splitter.Free;
  end;
end;

procedure TOBDLayoutControlTests.ScrollBoxClampsAndMovesChildren;
var
  Box: TOBDScrollBox;
  Child: TOBDCard;
begin
  Box := TOBDScrollBox.Create(nil);
  try
    Box.SetBounds(0, 0, 200, 200);
    Child := TOBDCard.Create(Box);
    Child.Parent := Box;
    Child.SetBounds(0, 0, 180, 600);
    Box.ScrollTo(100000);
    Assert.AreEqual(400, Box.ScrollPosition, 'not clamped to content');
    Assert.AreEqual(-400, Child.Top, 'child did not scroll');
    Box.ScrollTo(-5);
    Assert.AreEqual(0, Box.ScrollPosition, 'not clamped to top');
    Assert.AreEqual(0, Child.Top, 'child did not scroll back');
  finally
    Box.Free;
  end;
end;

procedure TOBDLayoutControlTests.CardCollapsesToHeaderAndBack;
var
  Card: TOBDCard;
begin
  Card := TOBDCard.Create(nil);
  try
    Card.SetBounds(0, 0, 300, 300);
    Card.Title := 'Connection';
    Card.Collapsible := True;
    Card.Collapsed := True;
    Assert.IsTrue(Card.Height < 100, 'card did not fold');
    Assert.AreEqual(300, Card.ExpandedHeight, 'expanded height');
    Assert.IsTrue(InkRatio(Card) > 0, 'folded card draws nothing');
    Card.Toggle;
    Assert.IsFalse(Card.Collapsed);
    Assert.AreEqual(300, Card.Height, 'card did not unfold');
    Card.Collapsed := True;
    Card.Collapsible := False;
    Assert.IsFalse(Card.Collapsed, 'not collapsible but still folded');
    Assert.AreEqual(300, Card.Height);
  finally
    Card.Free;
  end;
end;

{ TOBDInputControlTests }

procedure TOBDInputControlTests.Setup;
begin
  FChanges := 0;
end;

procedure TOBDInputControlTests.CountChange(Sender: TObject);
begin
  Inc(FChanges);
end;

procedure TOBDInputControlTests.SpinEditClampsAndSteps;
var
  Spin: TOBDSpinEdit;
begin
  Spin := TOBDSpinEdit.Create(nil);
  try
    Spin.OnValueChange := CountChange;
    Spin.MinValue := 0;
    Spin.MaxValue := 10;
    Spin.Value := 25;
    Assert.AreEqual(10.0, Spin.Value, 0.0001, 'not clamped to max');
    Spin.Step(-3);
    Assert.AreEqual(7, Spin.IntValue, 'step down');
    Spin.Step(-100);
    Assert.AreEqual(0, Spin.IntValue, 'not clamped to min');
    Assert.AreEqual('0', Spin.Text, 'text');
    Assert.IsTrue(FChanges >= 3, 'OnValueChange not fired');
    Assert.IsTrue(InkRatio(Spin) > 0, 'spin edit draws nothing');
  finally
    Spin.Free;
  end;
end;

procedure TOBDInputControlTests.SpinEditShowsDecimals;
var
  Spin: TOBDSpinEdit;
begin
  Spin := TOBDSpinEdit.Create(nil);
  try
    Spin.Decimals := 1;
    Spin.Increment := 0.5;
    Spin.Value := 2.26;
    Assert.AreEqual(2.3, Spin.Value, 0.0001, 'not rounded to decimals');
    Assert.AreEqual(FormatFloat('0.0', 2.3), Spin.Text, 'text');
    Spin.Step(1);
    Assert.AreEqual(2.8, Spin.Value, 0.0001, 'step');
  finally
    Spin.Free;
  end;
end;

procedure TOBDInputControlTests.TrackBarClampsAndNotifies;
var
  Track: TOBDTrackBar;
begin
  Track := TOBDTrackBar.Create(nil);
  try
    Track.Width := 200;
    Track.OnChange := CountChange;
    Track.Min := 0;
    Track.Max := 10;
    Track.Position := 20;
    Assert.AreEqual(10, Track.Position, 'not clamped to max');
    Track.Position := -5;
    Assert.AreEqual(0, Track.Position, 'not clamped to min');
    Assert.AreEqual(2, FChanges, 'OnChange');
    Track.ShowValue := True;
    Track.Suffix := ' %';
    Track.Frequency := 2;
    Track.Position := 6;
    Assert.IsTrue(InkRatio(Track) > 0.01, 'track bar draws nothing');
  finally
    Track.Free;
  end;
end;

procedure TOBDInputControlTests.MemoDrawsFrameAndKeepsText;
var
  Memo: TOBDMemo;
begin
  Memo := TOBDMemo.Create(nil);
  try
    Memo.SetBounds(0, 0, 240, 120);
    Memo.Text := 'Customer reports a misfire when cold.';
    Assert.AreEqual('Customer reports a misfire when cold.', Memo.Text);
    Assert.IsTrue(InkRatio(Memo) > 0, 'memo draws no frame');
  finally
    Memo.Free;
  end;
end;

procedure TOBDInputControlTests.CalendarKeysMoveTheDate;
var
  Cal: TOBDCalendar;
begin
  Cal := TOBDCalendar.Create(nil);
  try
    Cal.Date := EncodeDate(2026, 2, 14);
    Cal.OnChange := CountChange;
    Assert.AreEqual(2, Integer(Cal.ViewMonth), 'view month');
    Assert.IsTrue(Cal.HandleKey(VK_RIGHT, []));
    Assert.AreEqual(Double(EncodeDate(2026, 2, 15)), Double(Cal.Date), 0.0001,
      'right');
    Assert.IsTrue(Cal.HandleKey(VK_DOWN, []));
    Assert.AreEqual(Double(EncodeDate(2026, 2, 22)), Double(Cal.Date), 0.0001,
      'down');
    Assert.IsTrue(Cal.HandleKey(VK_NEXT, []));
    Assert.AreEqual(Double(EncodeDate(2026, 3, 22)), Double(Cal.Date), 0.0001,
      'next month');
    Assert.AreEqual(3, Integer(Cal.ViewMonth), 'view follows the date');
    Assert.IsTrue(Cal.HandleKey(VK_END, []));
    Assert.AreEqual(Double(EncodeDate(2026, 3, 31)), Double(Cal.Date), 0.0001,
      'end');
    Assert.IsFalse(Cal.HandleKey(VK_F5, []), 'unused key taken');
    Assert.AreEqual(4, FChanges, 'OnChange');
    Assert.IsTrue(InkRatio(Cal) > 0.02, 'calendar draws nothing');
  finally
    Cal.Free;
  end;
end;

procedure TOBDInputControlTests.CalendarIsLargerOnTablet;
var
  Cal: TOBDCalendar;
  Desktop: Integer;
begin
  Cal := TOBDCalendar.Create(nil);
  try
    Cal.Density := dnDesktop;
    Desktop := Cal.Width;
    Assert.IsTrue(Desktop > 7 * 20, 'calendar too narrow');
    Cal.Density := dnTablet;
    Assert.IsTrue(Cal.Width > Desktop, 'tablet calendar is not larger');
  finally
    Cal.Free;
  end;
end;

procedure TOBDInputControlTests.DatePickerFormatsTheDate;
var
  Picker: TOBDDatePicker;
begin
  Picker := TOBDDatePicker.Create(nil);
  try
    Picker.OnChange := CountChange;
    Picker.DisplayFormat := 'yyyy-mm-dd';
    Picker.Date := EncodeDate(2025, 3, 7);
    Assert.AreEqual('2025-03-07', Picker.DisplayText);
    Assert.AreEqual(1, FChanges, 'OnChange');
    Assert.IsFalse(Picker.IsOpen);
    Assert.IsTrue(InkRatio(Picker) > 0.01, 'date picker draws nothing');
  finally
    Picker.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDLayoutControlTests);
  TDUnitX.RegisterTestFixture(TOBDInputControlTests);

end.
