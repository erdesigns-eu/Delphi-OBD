//------------------------------------------------------------------------------
//  Tests.ERD.UI.Chrome
//
//  Rendering and behaviour tests for the application chrome: title bar,
//  menu bar, ribbon, backstage, report preview, tabs, tool bar, status
//  bar, progress bar and scroll bar. Each control is drawn off-screen, and
//  the pixels are checked, so a control that paints nothing fails.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.Chrome;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Control,
  ERD.UI.Menus,
  ERD.UI.TitleBar,
  ERD.UI.Ribbon,
  ERD.UI.Backstage,
  ERD.UI.Tabs,
  ERD.UI.ToolBar,
  ERD.UI.StatusBar,
  ERD.UI.Progress,
  ERD.UI.ScrollBar,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDChromePreviewTests = class
  strict private
    procedure CheckPreview(AControl: TOBDCustomControl; AWidth,
      AHeight: Integer; const AName: string);
  public
    [Test] procedure TitleBarPreviewDraws;
    [Test] procedure MenuBarPreviewDraws;
    [Test] procedure RibbonPreviewDraws;
    [Test] procedure BackstagePreviewDraws;
    [Test] procedure ReportPreviewDraws;
    [Test] procedure TabsPreviewDraws;
    [Test] procedure ToolBarPreviewDraws;
    [Test] procedure StatusBarPreviewDraws;
    [Test] procedure ProgressBarPreviewDraws;
    [Test] procedure ScrollBarDraws;
  end;

  [TestFixture]
  TOBDChromeBehaviourTests = class
  public
    [Test] procedure CaptionButtonsAreACollection;
    [Test] procedure CommandStyleSwitchesTheRibbon;
    [Test] procedure ContextualTabsAreConfigurable;
    [Test] procedure ContextualTabChangesTheDrawing;
    [Test] procedure TabIndexIgnoresInvalidTabs;
    [Test] procedure ProgressClampsPosition;
    [Test] procedure ScrollBarClampsPosition;
    [Test] procedure TabletTitleBarIsTaller;
  end;

implementation

{ TOBDChromePreviewTests }

procedure TOBDChromePreviewTests.CheckPreview(AControl: TOBDCustomControl;
  AWidth, AHeight: Integer; const AName: string);
begin
  try
    AControl.SetBounds(0, 0, AWidth, AHeight);
    AControl.ForcePreview := True;
    Assert.IsTrue(InkRatio(AControl) > 0.01, AName + ' preview is empty');
  finally
    AControl.Free;
  end;
end;

procedure TOBDChromePreviewTests.TitleBarPreviewDraws;
begin
  CheckPreview(TOBDTitleBar.Create(nil), 1000, 40, 'title bar');
end;

procedure TOBDChromePreviewTests.MenuBarPreviewDraws;
begin
  CheckPreview(TOBDMenuBar.Create(nil), 600, 30, 'menu bar');
end;

procedure TOBDChromePreviewTests.RibbonPreviewDraws;
begin
  CheckPreview(TOBDRibbon.Create(nil), 1200, 130, 'ribbon');
end;

procedure TOBDChromePreviewTests.BackstagePreviewDraws;
begin
  CheckPreview(TOBDBackstage.Create(nil), 1200, 760, 'backstage');
end;

procedure TOBDChromePreviewTests.ReportPreviewDraws;
begin
  CheckPreview(TOBDReportPreview.Create(nil), 600, 700, 'report preview');
end;

procedure TOBDChromePreviewTests.TabsPreviewDraws;
begin
  CheckPreview(TOBDTabs.Create(nil), 600, 36, 'tabs');
end;

procedure TOBDChromePreviewTests.ToolBarPreviewDraws;
begin
  CheckPreview(TOBDToolBar.Create(nil), 800, 44, 'tool bar');
end;

procedure TOBDChromePreviewTests.StatusBarPreviewDraws;
begin
  CheckPreview(TOBDStatusBar.Create(nil), 1000, 28, 'status bar');
end;

procedure TOBDChromePreviewTests.ProgressBarPreviewDraws;
begin
  CheckPreview(TOBDProgressBar.Create(nil), 400, 60, 'progress bar');
end;

procedure TOBDChromePreviewTests.ScrollBarDraws;
var
  Bar: TOBDScrollBar;
begin
  Bar := TOBDScrollBar.Create(nil);
  Bar.Max := 100;
  Bar.PageSize := 20;
  CheckPreview(Bar, 12, 300, 'scroll bar');
end;

{ TOBDChromeBehaviourTests }

procedure TOBDChromeBehaviourTests.CaptionButtonsAreACollection;
var
  Bar: TOBDTitleBar;
  Button: TOBDCaptionButton;
begin
  Bar := TOBDTitleBar.Create(nil);
  try
    Bar.SetBounds(0, 0, 1000, 40);
    Button := Bar.Buttons.Add;
    Button.Glyph := glBell;
    Button.Hint := 'Notifications';
    Bar.Buttons.Add.Glyph := glHelp;
    Assert.AreEqual(2, Bar.Buttons.Count);
    Assert.AreEqual(Ord(glBell), Ord(Bar.Buttons[0].Glyph));
    Bar.Buttons.Delete(0);
    Assert.AreEqual(1, Bar.Buttons.Count);
    Assert.AreEqual(Ord(glHelp), Ord(Bar.Buttons[0].Glyph));
  finally
    Bar.Free;
  end;
end;

procedure TOBDChromeBehaviourTests.CommandStyleSwitchesTheRibbon;
var
  Bar: TOBDTitleBar;
  Ribbon: TOBDRibbon;
begin
  Bar := TOBDTitleBar.Create(nil);
  Ribbon := TOBDRibbon.Create(nil);
  try
    Bar.Ribbon := Ribbon;
    Bar.CommandStyle := csMenu;
    Assert.IsFalse(Ribbon.Visible, 'ribbon visible in menu mode');
    Bar.CommandStyle := csRibbon;
    Assert.IsTrue(Ribbon.Visible, 'ribbon hidden in ribbon mode');
    Bar.CommandStyle := csMenu;
    Assert.IsFalse(Ribbon.Visible, 'ribbon not hidden again');
  finally
    Bar.Free;
    Ribbon.Free;
  end;
end;

procedure TOBDChromeBehaviourTests.ContextualTabsAreConfigurable;
var
  Ribbon: TOBDRibbon;
  TabSet: TOBDContextualTabSet;
begin
  Ribbon := TOBDRibbon.Create(nil);
  try
    TabSet := Ribbon.ContextualTabs.Add;
    TabSet.Caption := 'Recording';
    TabSet.Color := ccSuccess;
    TabSet.Tabs.Add.Caption := 'Playback';
    Assert.IsFalse(TabSet.Visible, 'contextual tabs start hidden');
    TabSet.Visible := True;
    Assert.AreEqual(1, Ribbon.ContextualTabs.Count);
    Assert.AreEqual('Playback', string(Ribbon.ContextualTabs[0].Tabs[0].Caption));
  finally
    Ribbon.Free;
  end;
end;

procedure TOBDChromeBehaviourTests.ContextualTabChangesTheDrawing;
var
  Ribbon: TOBDRibbon;
  TabSet: TOBDContextualTabSet;
  Before, After: TBitmap;
begin
  Ribbon := TOBDRibbon.Create(nil);
  try
    Ribbon.SetBounds(0, 0, 1200, 130);
    Ribbon.Tabs.Add.Caption := 'Home';
    Ribbon.Tabs.Add.Caption := 'Diagnose';
    TabSet := Ribbon.ContextualTabs.Add;
    TabSet.Color := ccSuccess;
    TabSet.Tabs.Add.Caption := 'Playback';
    Before := RenderControl(Ribbon);
    try
      TabSet.Visible := True;
      After := RenderControl(Ribbon);
      try
        Assert.IsTrue(BitmapsDiffer(Before, After, 20),
          'contextual tab not drawn');
      finally
        After.Free;
      end;
    finally
      Before.Free;
    end;
  finally
    Ribbon.Free;
  end;
end;

procedure TOBDChromeBehaviourTests.TabIndexIgnoresInvalidTabs;
var
  Tabs: TOBDTabs;
begin
  Tabs := TOBDTabs.Create(nil);
  try
    Tabs.Tabs.Add.Caption := 'Codes';
    Tabs.Tabs.Add.Caption := 'Live data';
    Tabs.Tabs[1].Enabled := False;
    Tabs.TabIndex := 0;
    Assert.AreEqual(0, Tabs.TabIndex);
    Tabs.TabIndex := 1;
    Assert.AreEqual(0, Tabs.TabIndex, 'disabled tab selected');
    Tabs.TabIndex := 5;
    Assert.AreEqual(0, Tabs.TabIndex, 'out-of-range tab selected');
  finally
    Tabs.Free;
  end;
end;

procedure TOBDChromeBehaviourTests.ProgressClampsPosition;
var
  Bar: TOBDProgressBar;
begin
  Bar := TOBDProgressBar.Create(nil);
  try
    Bar.Position := 150;
    Assert.AreEqual(100, Bar.Position);
    Bar.Position := -5;
    Assert.AreEqual(0, Bar.Position);
  finally
    Bar.Free;
  end;
end;

procedure TOBDChromeBehaviourTests.ScrollBarClampsPosition;
var
  Bar: TOBDScrollBar;
begin
  Bar := TOBDScrollBar.Create(nil);
  try
    Bar.Max := 100;
    Bar.PageSize := 10;
    Bar.Position := 1000;
    Assert.IsTrue((Bar.Position >= 90) and (Bar.Position <= 100),
      'position not clamped to the last page');
    Bar.Position := -1;
    Assert.AreEqual(0, Bar.Position);
  finally
    Bar.Free;
  end;
end;

procedure TOBDChromeBehaviourTests.TabletTitleBarIsTaller;
var
  Bar: TOBDTitleBar;
  DesktopHeight: Integer;
begin
  Bar := TOBDTitleBar.Create(nil);
  try
    Bar.Density := dnDesktop;
    DesktopHeight := Bar.Height;
    Bar.Density := dnTablet;
    Assert.IsTrue(Bar.Height > DesktopHeight, 'tablet title bar not taller');
  finally
    Bar.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOBDChromePreviewTests);
  TDUnitX.RegisterTestFixture(TOBDChromeBehaviourTests);
end.
