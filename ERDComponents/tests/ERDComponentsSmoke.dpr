program ERDComponentsSmoke;
{$APPTYPE CONSOLE}
uses
  System.SysUtils, System.Classes, System.Types, System.Math, Vcl.Forms, Vcl.Controls,
  Winapi.Windows, Vcl.StdCtrls, Vcl.Graphics, Vcl.ImgList,
  ERD.Components.LinkLabel,
  ERD.Components.Calendar, ERD.Components.DateTimePicker,
  ERD.Components.TimePicker,
  ERD.Components.CheckListBox,
  ERD.Components.ScrollBar, ERD.Components.KeyValueList,
  ERD.Components.CommonControls, ERD.Components.MediaControls, ERD.Components.TrackBar,
  ERD.Components.ShellControls,
  ERD.Components.Types, ERD.Components.Icons, ERD.Components.IconCatalog,
  ERD.Components.Buttons, ERD.Components.Panel, ERD.Components.Chips,
  ERD.Components.VirtualTree, ERD.Components.VT.BaseTree, ERD.Components.VT.Types,
  Showcase in '../demo/Showcase.pas',
  ERD.Components.SpinEdit, ERD.Components.Edits, ERD.Components.PopupList, ERD.Components.TitleBar, ERD.Components.StatusBar, ERD.Components.Theme, ERD.Components.Ribbon;
type
  TLinkProbe = class(TERDLinkLabel)
  public
    procedure Activate;
    procedure PressEnter;
  end;
  TDateProbe = class(TERDDatePicker)
  public
    procedure TypeText(const Value: string);
    procedure PressKey(Value: Word);
  end;
  TTimeProbe = class(TERDTimePicker)
  public
    procedure TypeText(const Value: string);
    procedure PressKey(Value: Word);
    procedure ClickAt(X, Y: Integer);
  end;
  TCheckListProbe = class(TERDCheckListBox)
  public
    procedure PressAt(X, Y: Integer);
    procedure ReleaseAt(X, Y: Integer);
    procedure Space;
  end;
  TCheckListCounter = class
    Count, LastIndex: Integer;
    procedure Checked(Sender: TObject; Index: Integer);
  end;
  TSearchProbe = class(TERDSearchEdit)
  public
    procedure EnterSearch;
  end;
  TTrackProbe = class(TERDTrackBar)
  public
    procedure ClickAt(X, Y: Integer);
  end;
  TRangeProbe = class(TERDRangeSlider)
  public
    procedure Step(Key: Word);
  end;
  TFocusProbe = class(TERDButton)
  public
    function CuesVisible: Boolean;
  end;
  TChangeProbe = class
    Changes: Integer;
    procedure Changed(Sender: TObject);
  end;
var Checks: Integer;
procedure TLinkProbe.Activate;
begin Click end;
procedure TLinkProbe.PressEnter;
var Key: Word;
begin Key := VK_RETURN; KeyDown(Key, []) end;

procedure TDateProbe.TypeText(const Value: string);
var I: Integer; Key: Char;
begin
  for I := 1 to Length(Value) do begin Key := Value[I]; KeyPress(Key) end;
end;
procedure TDateProbe.PressKey(Value: Word);
begin KeyDown(Value, []) end;

procedure TTimeProbe.TypeText(const Value: string);
var I: Integer; Key: Char;
begin
  for I := 1 to Length(Value) do
  begin Key := Value[I]; KeyPress(Key) end;
end;
procedure TTimeProbe.PressKey(Value: Word);
begin
  KeyDown(Value, []);
end;
procedure TTimeProbe.ClickAt(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
  MouseUp(mbLeft, [], X, Y);
end;

procedure TCheckListProbe.PressAt(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
end;
procedure TCheckListProbe.ReleaseAt(X, Y: Integer);
begin
  MouseUp(mbLeft, [], X, Y);
end;
procedure TCheckListProbe.Space;
var Key: Word;
begin
  Key := VK_SPACE;
  KeyDown(Key, []);
end;
procedure TCheckListCounter.Checked(Sender: TObject; Index: Integer);
begin
  Inc(Count);
  LastIndex := Index;
end;

procedure TSearchProbe.EnterSearch;
var Key: Word;
begin
  Key := VK_RETURN;
  KeyDown(Key, []);
end;
procedure TTrackProbe.ClickAt(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
  MouseUp(mbLeft, [], X, Y);
end;
procedure TRangeProbe.Step(Key: Word);
begin
  KeyDown(Key, []);
end;

function TFocusProbe.CuesVisible: Boolean;
begin
  Result := FocusCuesVisible;
end;
procedure TChangeProbe.Changed(Sender: TObject);
begin
  Inc(Changes);
end;

procedure Check(Value: Boolean; const Text: string);
begin
  Inc(Checks);
  if not Value then raise Exception.Create(Text);
end;
procedure TestIconLifetimeAndStreaming;
var Button, CopyButton: TERDButton; Images: TImageList; Bitmap: TBitmap; Stream: TMemoryStream;
begin
  RegisterClass(TERDButton);
  Button := TERDButton.Create(nil);
  try
    Button.Icon.BuiltIn := ei249Teletext;
    Check(Button.Icon.Available, 'Catalog icon available');
    Button.Icon.Source := eisNone;
    Check(not Button.Icon.Available(glCheck), 'None does not fall back');
    Images := TImageList.Create(nil);
    try
      Bitmap := TBitmap.Create;
      try
        Bitmap.SetSize(16,16); Bitmap.Canvas.Brush.Color := clRed;
        Bitmap.Canvas.FillRect(Rect(0,0,16,16));
        Images.Add(Bitmap,nil);
        Button.Icon.Glyph.Assign(Bitmap);
      finally Bitmap.Free end;
      Button.Icon.Source := eisImageList; Button.Icon.Images := Images; Button.Icon.ImageIndex := 0;
      Check(Button.Icon.Available, 'ImageList source available');
      Button.Icon.ImageIndex := 50;
      Check(not Button.Icon.Available(glCheck), 'Invalid index does not change sources');
    finally Images.Free end;
    Check(Button.Icon.Images = nil, 'Freed ImageList clears reference');
    Button.Icon.Source := eisGlyph;
    Check(Button.Icon.Available, 'Own glyph survives source bitmap destruction');
    Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Button); Stream.Position := 0;
      CopyButton := TERDButton(Stream.ReadComponent(nil));
      try
        Check(CopyButton.Icon.Source = eisGlyph, 'Icon source streams');
        Check(CopyButton.Icon.Available, 'Picture streams');
        Check(CopyButton.Icon.Glyph.Width = 16, 'Picture dimensions preserved');
      finally CopyButton.Free end;
    finally Stream.Free end;
    Button.Icon.Source := eisBuiltIn;
    Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Button); Stream.Position := 0;
      CopyButton := TERDButton(Stream.ReadComponent(nil));
      try Check(CopyButton.Icon.BuiltIn = ei249Teletext, 'Catalog ID streams');
      finally CopyButton.Free end;
    finally Stream.Free end;
  finally Button.Free end;
end;
procedure TestHostedStatusPanels;
var
  Form: TForm; Status: TERDStatusBar; Host, OriginalParent: TERDPanel;
  First, Second: TERDStatusPanel; Badge: TERDBadge; Button: TERDButton;
  OriginalBounds: TRect;
begin
  Form := TForm.CreateNew(nil);
  try
    OriginalParent := TERDPanel.Create(Form); OriginalParent.Parent := Form;
    Host := TERDPanel.Create(Form); Host.Parent := OriginalParent; Host.SetBounds(10,12,250,44);
    OriginalBounds := Host.BoundsRect;
    Badge := TERDBadge.Create(Form); Badge.Parent := Host; Badge.Count := 4;
    Badge := TERDBadge.Create(Form); Badge.Parent := Host; Badge.Count := 12;
    Button := TERDButton.Create(Form); Button.Parent := Host; Button.Caption := 'Action';
    Status := TERDStatusBar.Create(Form); Status.Parent := Form;
    First := Status.Panels.Add; First.Width := 280; First.Control := Host;
    Check(First.Kind = spkControl, 'Hosting selects control panel');
    Check(Host.Parent = Status, 'Container is hosted by status bar');
    Check(Host.ControlCount = 3, 'One status panel contains two badges and a button');
    Second := Status.Panels.Add;
    try
      Second.Control := Host;
      Check(False, 'Duplicate host assignment must fail');
    except on E: EInvalidOperation do Check(True, 'Duplicate host rejected') end;
    try
      Second.Control := Form;
      Check(False, 'Ancestor host must fail');
    except on E: EInvalidOperation do Check(True, 'Ancestor hosting rejected') end;
    First.Control := nil;
    Check(Host.Parent = OriginalParent, 'Detach restores parent');
    Check((Host.Left = OriginalBounds.Left) and (Host.Top = OriginalBounds.Top) and
      (Host.Width = OriginalBounds.Width) and (Host.Height = OriginalBounds.Height), 'Detach restores bounds');
    First.Control := Host;
    OriginalParent.Free;
    First.Control := nil;
    Check(Host.Parent = nil, 'Destroyed original parent does not leave dangling reference');
    First.Control := Host;
    Host.Free;
    Check(First.Control = nil, 'Destroyed hosted control clears reference');
    // Status destruction with a live host must detach safely.
    Host := TERDPanel.Create(Form); Host.Parent := Form; First.Control := Host;
    Status.Free;
    Check(Host.Parent = Form, 'Status bar destruction restores child parent');
  finally Form.Free end;
end;
procedure TestThemesAndRibbon;
var Theme: TERDTheme; Ribbon: TERDRibbon; Mode: TERDThemeMode;
  Appearance: TERDRibbonAppearance; Layout: TERDRibbonStyle;
  Bitmap: TBitmap; P: TERDThemePalette;
begin
  Theme := TERDTheme.Create(nil); Ribbon := TERDRibbon.Create(nil); Bitmap := TBitmap.Create;
  try
    Check(Theme.Mode = tmERDesignsLight, 'Default is ERDesigns Light');
    Ribbon.Theme := Theme;
    Ribbon.Width := 900;
    with Ribbon.Tabs.Add do
    begin
      Caption := 'Test';
      with Groups.Add do
      begin
        Caption := 'Actions';
        with Items.Add do begin Caption := 'Play'; Icon.BuiltIn := ei161Afspelen end;
      end;
    end;
    for Mode := tmERDesignsLight to High(TERDThemeMode) do
    begin
      Theme.Mode := Mode; P := Theme.Palette;
      Check(P.Background <> P.ForegroundText, ERDThemeName(Mode) + ' readable foreground');
      for Appearance := Low(TERDRibbonAppearance) to High(TERDRibbonAppearance) do
        for Layout := Low(TERDRibbonStyle) to High(TERDRibbonStyle) do
        begin
          Ribbon.Appearance := Appearance; Ribbon.RibbonStyle := Layout;
          Ribbon.RenderTo(Bitmap);
          Check((Bitmap.Width > 0) and (Bitmap.Height > 0), 'Ribbon renders for theme/shape/layout');
          Check(Theme.Mode = Mode, 'Ribbon appearance leaves theme selection intact');
        end;
    end;
  finally Bitmap.Free; Ribbon.Free; Theme.Free end;
end;

procedure TestPopupRowsAndCommandSwitch;
var
  Form: TForm;
  Popup: TERDPopupList;
  TitleBar: TERDTitleBar;
  Ribbon: TERDRibbon;
  Items: TStringList;
  Bitmap: TBitmap;
  R: TRect;
  X, Y, InkPixels: Integer;
begin
  Form := TForm.CreateNew(nil);
  Items := TStringList.Create;
  Bitmap := TBitmap.Create;
  try
    TitleBar := TERDTitleBar.Create(Form);
    TitleBar.Parent := Form;
    Ribbon := TERDRibbon.Create(Form);
    Ribbon.Parent := Form;
    TitleBar.Ribbon := Ribbon;
    TitleBar.CommandStyle := csRibbon;
    Check(Ribbon.Visible, 'Ribbon shown in ribbon command mode');
    TitleBar.CommandStyle := csMenu;
    Check(not Ribbon.Visible, 'Ribbon hidden in menu command mode');
    TitleBar.CommandStyle := csRibbon;
    Check(Ribbon.Visible, 'Ribbon restored after switching back');
    Popup := TERDPopupList.Create(Form);
    Items.Add('First item');
    Items.Add('Visible second item');
    Popup.Popup(Form, Rect(100, 100, 320, 130), Items, 0,
      BRAND_PALETTE_LIGHT, 96, 28, 8);
    Bitmap.SetSize(Popup.ClientWidth, Popup.ClientHeight);
    SendMessage(Popup.Handle, WM_PRINTCLIENT, Bitmap.Canvas.Handle, PRF_CLIENT);
    SendMessage(Popup.Handle, LB_GETITEMRECT, 1, LPARAM(@R));
    InkPixels := 0;
    for Y := R.Top + 4 to R.Bottom - 5 do
      for X := R.Left + 8 to R.Right - 20 do
        if ColorToRGB(Bitmap.Canvas.Pixels[X, Y]) <>
          ColorToRGB(BRAND_PALETTE_LIGHT.GaugeFace) then Inc(InkPixels);
    Check(InkPixels > 10, 'Unselected popup row paints visible text');
    SendMessage(Popup.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    SendMessage(Popup.Handle, WM_LBUTTONUP, 0,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    Check(not Popup.IsOpen, 'Mouse selection closes the popup');
    Check(Popup.ItemIndex = 1, 'Mouse selection accepts the clicked row');
  finally
    Bitmap.Free;
    Items.Free;
    Form.Free;
  end;
end;

procedure TestComboMouseChange;
var
  Form: TForm;
  Combo: TERDComboBox;
  Popup: TERDPopupList;
  Probe: TChangeProbe;
  I: Integer;
  R: TRect;
begin
  Form := TForm.CreateNew(nil);
  Probe := TChangeProbe.Create;
  try
    Combo := TERDComboBox.Create(Form);
    Combo.Parent := Form;
    Combo.Items.Add('First');
    Combo.Items.Add('Second');
    Combo.ItemIndex := 0;
    Combo.OnChange := Probe.Changed;
    Combo.DropDown;
    Popup := nil;
    for I := 0 to Combo.ComponentCount - 1 do
      if Combo.Components[I] is TERDPopupList then
        Popup := TERDPopupList(Combo.Components[I]);
    Check(Popup <> nil, 'Combo creates its dropdown');
    SendMessage(Popup.Handle, LB_GETITEMRECT, 1, LPARAM(@R));
    SendMessage(Popup.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    SendMessage(Popup.Handle, WM_LBUTTONUP, 0,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    Check(Combo.ItemIndex = 1, 'Combo accepts a mouse-selected item');
    Check(Probe.Changes = 1, 'Mouse selection fires OnChange exactly once');
    Check(not Combo.DroppedDown, 'Combo closes after mouse selection');
  finally
    Form.Free;
    Probe.Free;
  end;
end;

procedure TestDemoLayoutAndSpinMargins;
var
  Form: TShowcaseForm;
  Spin: TERDSpinEdit;
  NativeEdit: TWinControl;
  Margins: LRESULT;
  Caret: TPoint;
  I: Integer;
begin
  Form := TShowcaseForm.Create(nil);
  try
    for I := 1 to 3 do
    begin
      Form.CommandChoice.ItemIndex := 1;
      Form.CommandSelected(Form.CommandChoice);
      Check(Form.Choices.Top = Form.TitleBar.Height, 'Choices follow titlebar in menu mode');
      Form.CommandChoice.ItemIndex := 0;
      Form.CommandSelected(Form.CommandChoice);
      Check(Form.Ribbon.Top = Form.TitleBar.Height, 'Ribbon follows titlebar');
      Check(Form.Choices.Top = Form.Ribbon.Top + Form.Ribbon.Height,
        'Choices remain below ribbon after command switching');
    end;
    Check(Form.Ribbon.Backstage = Form.DemoBackstage, 'Ribbon File is linked to the real backstage');
    Form.ShowBackstage(nil);
    Check(Form.DemoBackstage.Visible, 'Backstage opens');
    Check(not Form.Sidebar.Visible and not Form.Choices.Visible, 'Backstage replaces the demo navigation');
    Form.DemoBackstage.Close;
    Check(Form.Sidebar.Visible and Form.Choices.Visible, 'Closing backstage restores navigation');
    Check(Form.Choices.Top = Form.Ribbon.Top + Form.Ribbon.Height, 'Closing backstage restores chrome order');
    Form.Theme.Mode := tmERDesignsDark;
    Check(Form.ThemeChoice.Items.Count = Ord(High(TERDThemeMode)) - Ord(tmERDesignsLight) + 1,
      'Demo lists every explicit theme');
    Check(Form.DemoDialog.Palette.Background = Form.Theme.Palette.Background,
      'General dialog follows the selected theme');
    Check(Form.DemoOpenDialog.Palette.Background = Form.Theme.Palette.Background,
      'File dialog follows the selected theme');
    Check(Form.DemoToastManager.Palette.Background = Form.Theme.Palette.Background,
      'Toast follows the selected theme');
    Check(Form.DemoHintStyle.Palette.Background = Form.Theme.Palette.Background,
      'Hint follows the selected theme');
    Form.Theme.Mode := tmOfficeLight;
    Check(Form.DemoToastManager.Palette.Background = Form.Theme.Palette.Background,
      'Toast follows a later theme change');
    Form.PageTabs.TabIndex := 1;
    Check(Form.InputsPage.Visible and not Form.IconsPage.Visible, 'ERD tabs select the input page');
    Spin := Form.DemoSpinEdit;
    Spin.Alignment := taLeftJustify;
    Spin.Alignment := taRightJustify;
    Spin.Mono := True;
    NativeEdit := nil;
    for I := 0 to Spin.ControlCount - 1 do
      if Spin.Controls[I] is TCustomEdit then
        NativeEdit := TWinControl(Spin.Controls[I]);
    Check(NativeEdit <> nil, 'SpinEdit hosts a native edit');
    Margins := SendMessage(NativeEdit.Handle, EM_GETMARGINS, 0, 0);
    Check((Margins and $FFFF) >= 2, 'Native edit reserves left caret space');
    Check(((Margins shr 16) and $FFFF) >= 2, 'Right-aligned edit reserves end caret space');
    Form.Show;
    Spin.Value := 18;
    Spin.SetFocus;
    Spin.SelStart := Length(Spin.Text);
    Spin.SelLength := 0;
    Check(GetCaretPos(Caret), 'Native spin caret is available');
    Check(Caret.X = NativeEdit.ClientWidth - Integer((Margins shr 16) and $FFFF),
      'Right-aligned caret is at the text edge after the final digit');
  finally
    Form.Free;
  end;
end;

procedure TestWindowsFocusCuesAndAutoSave;
var
  Form: TForm;
  Button: TFocusProbe;
  Title: TERDTitleBar;
  Probe: TChangeProbe;
  AlwaysShow: BOOL;
  R, Wider: TRect;
  P: TPoint;
  Panel: TERDPanel;
  I: Integer;
  Mode: TERDThemeMode;
  SavedState: Boolean;
  Stream: TMemoryStream;
  Restored: TERDTitleBar;
begin
  Form := TForm.CreateNew(nil);
  Probe := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  try
    Form.ClientWidth := 1000;
    Button := TFocusProbe.Create(Form);
    Button.Parent := Form;
    AlwaysShow := False;
    SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @AlwaysShow, 0);
    SendMessage(Button.Handle, WM_QUERYUISTATE, 0, 0);
    SendMessage(Form.Handle, WM_CHANGEUISTATE, UIS_SET or (UISF_HIDEFOCUS shl 16), 0);
    Check(Button.CuesVisible = Boolean(AlwaysShow), 'Hidden focus cues respect Windows accessibility setting');
    SendMessage(Form.Handle, WM_CHANGEUISTATE, UIS_CLEAR or (UISF_HIDEFOCUS shl 16), 0);
    Check(Button.CuesVisible, 'Cleared Windows hide-focus state reveals cues');
    Title := TERDTitleBar.Create(Form);
    Title.Parent := Form;
    Title.Width := 900;
    Check(not Title.AutoSaveVisible, 'AutoSave starts hidden');
    Title.AutoSaveVisible := True;
    Title.OnAutoSaveChange := Probe.Changed;
    R := Title.AutoSaveBounds;
    Check(not R.IsEmpty, 'Visible AutoSave has a hit target');
    Check(Title.AutoSavePadding = 8, 'AutoSave defaults to 8 logical pixels between caption and toggle');
    Title.AutoSavePadding := 24;
    Wider := Title.AutoSaveBounds;
    Check(Wider.Width = R.Width + MulDiv(16, Title.CurrentPPI, 96), 'Padding adds the requested space without stretching the caption');
    Title.AutoSavePadding := 8;
    Title.AutoSaveCaption := 'Automatically save changes';
    Wider := Title.AutoSaveBounds;
    Check(Wider.Width > R.Width, 'Longer AutoSave captions reserve more space');
    Title.AutoSaveCaption := 'AutoSave';
    Panel := TERDPanel.Create(Form);
    Panel.Parent := Form;
    Panel.Align := alClient;
    Title.Parent := Panel;
    P := Title.ClientToScreen(Point(Title.Width div 2, Title.Height div 2));
    Check(SendMessage(Title.Handle, WM_NCHITTEST, 0,
      LPARAM(MakeLParam(P.X, P.Y))) = HTCAPTION, 'Nested titlebar exposes a native caption drag hit');
    SendMessage(Title.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
      LPARAM(MakeLParam(R.Left + R.Width div 2, R.Top + R.Height div 2)));
    SendMessage(Title.Handle, WM_LBUTTONUP, 0,
      LPARAM(MakeLParam(R.Left + R.Width div 2, R.Top + R.Height div 2)));
    Check(Title.AutoSave and (Probe.Changes = 1), 'Mouse toggle changes AutoSave exactly once');
    SendMessage(Title.Handle, WM_KEYDOWN, VK_SPACE, 0);
    SendMessage(Title.Handle, WM_KEYDOWN, VK_SPACE, $40000000);
    SendMessage(Title.Handle, WM_KEYUP, VK_SPACE, 0);
    Check(not Title.AutoSave and (Probe.Changes = 2), 'Space toggles AutoSave exactly once');
    Title.AutoSave := True;
    Title.AutoSaveVisible := False;
    Check(Title.AutoSaveBounds.IsEmpty, 'Hidden AutoSave releases caption space');
    Title.AutoSaveVisible := True;
    SavedState := Title.AutoSave;
    for I := 1 to 2 do
    begin
      Title.CommandStyle := csMenu;
      Title.CommandStyle := csRibbon;
    end;
    Check(Title.AutoSave = SavedState, 'Command switching preserves AutoSave state');
    Title.OnAutoSaveChange := nil;
    RegisterClass(TERDTitleBar);
    Stream.WriteComponent(Title);
    Stream.Position := 0;
    Restored := TERDTitleBar(Stream.ReadComponent(nil));
    try
      Check(Restored.AutoSaveVisible and Restored.AutoSave, 'AutoSave survives DFM streaming');
      Check(Restored.AutoSavePadding = 8, 'AutoSave padding survives DFM streaming');
    finally
      Restored.Free;
    end;
    for Mode := tmOfficeWordLight to High(TERDThemeMode) do
      Check(ERDThemeName(Mode) <> '', 'Every Office profile has a display name');
  finally
    Stream.Free;
    Form.Free;
    Probe.Free;
  end;
end;

procedure TestVirtualTreeThemeAndLazyNodes;
var
  Form: TShowcaseForm;
  Tree: TERDVirtualStringTree;
  Node, Child: PVirtualNode;
  Mode: TERDThemeMode;
  TextBefore, TextAfter: string;
  Rect: TRect;
  Snapshot: TBitmap;
begin
  Form := TShowcaseForm.Create(nil);
  Snapshot := TBitmap.Create;
  try
    Tree := Form.DemoVirtualStringTree;
    Check(Tree.RootNodeCount = 20, 'Virtual tree has twenty lazy root groups');
    Node := Tree.GetFirst;
    Check(Node <> nil, 'First virtual node exists');
    Tree.ValidateNode(Node, False);
    Check(vsHasChildren in Node.States, 'Root advertises lazy children');
    Check(Node.ChildCount = 0, 'Collapsed group has no initialized channel nodes');
    Tree.Expanded[Node] := True;
    Check(Tree.Handle <> 0, 'Tree window is created');
    Check(Tree.VerticalScrollBar.Visible, 'Expanded tree uses its ERD vertical scrollbar');
    Check((GetWindowLong(Tree.Handle, GWL_STYLE) and WS_VSCROLL) = 0, 'Native vertical scrollbar is absent');
    Tree.OffsetY := 0;
    Tree.Perform(WM_MOUSEWHEEL, WPARAM($FF880000), 0);
    Check(Tree.OffsetY < 0, 'Mouse wheel scrolls expanded virtual tree');
    Check(Tree.VerticalScrollBar.Position = -Tree.OffsetY, 'Tree and scrollbar positions agree after wheel');
    Tree.VerticalScrollBar.Position := 500;
    Check(Tree.OffsetY = -Tree.VerticalScrollBar.Position, 'ERD scrollbar scrolls the engine');
    Tree.OffsetY := 0;
    Check(Node.ChildCount = 1000, 'Expanding initializes the requested thousand child nodes');
    Child := Tree.GetFirstChild(Node);
    Check(Child <> nil, 'Expanded group exposes a channel');
    Form.TreeGetText(Tree, Child, 0, ttNormal, TextBefore);
    Tree.SortTree(0, sdDescending, False);
    Form.TreeGetText(Tree, Child, 0, ttNormal, TextAfter);
    Check(TextBefore = TextAfter, 'Sorting preserves node model identity');
    for Mode := tmERDesignsLight to High(TERDThemeMode) do
    begin
      Form.Theme.Mode := Mode;
      Check(Tree.Color = Form.Theme.Palette.GaugeFace, 'Tree background follows theme');
      Check(Tree.Header.Font.Color = Form.Theme.Palette.ForegroundText, 'Header text follows theme');
      Check(Tree.Colors.SelectionTextColor = Form.Theme.Palette.ForegroundText, 'Selection text follows theme');
      Snapshot.SetSize(Tree.Width, Tree.Height);
      Tree.PaintTo(Snapshot.Canvas.Handle, 0, 0);
    end;
    Tree.Selected[Child] := True;
    Check(Tree.Selected[Child], 'Native selection API remains available');
    Rect := Tree.GetDisplayRect(Child, 0, False);
    Check(Rect.Bottom > Rect.Top, 'Expanded child has layout geometry');
    Tree.Clear;
    Check(Tree.RootNodeCount = 0, 'Clear releases virtual nodes');
  finally
    Snapshot.Free;
    Form.Free;
  end;
end;

procedure TestCommonControls;
var
  Form: TForm;
  Search: TSearchProbe;
  Tags, TagsCopy: TERDTagEdit;
  Split: TERDSplitButton;
  Expander, ExpanderCopy: TERDExpander;
  Content, IntentionallyHidden: TERDButton;
  Track: TTrackProbe;
  Range: TRangeProbe;
  Counter: TChangeProbe;
  Stream: TMemoryStream;
  Bitmap: TBitmap;
  OriginalHeight: Integer;
begin
  RegisterClasses([TERDTagEdit, TERDExpander, TERDButton]);
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    Form.SetBounds(20, 20, 800, 600);
    Search := TSearchProbe.Create(Form);
    Search.Parent := Form;
    Search.OnSearch := Counter.Changed;
    Search.SearchDelay := 0;
    Search.Text := 'live';
    Check(Counter.Changes = 1, 'SearchDelay=0 dispatches a changed query');
    Search.EnterSearch;
    Check(Counter.Changes = 2, 'Enter executes search immediately');
    Search.SearchDelay := 40;
    Search.Text := 'live t';
    Search.Text := 'live tv';
    Check(Counter.Changes = 2, 'Debounce does not dispatch intermediate queries');
    Sleep(80);
    Application.ProcessMessages;
    Check(Counter.Changes = 3, 'Debounce dispatches one final query');

    Tags := TERDTagEdit.Create(Form);
    Tags.Parent := Form;
    Tags.Top := 50;
    Check(Tags.AddTag(' HD '), 'Tag accepts nonempty trimmed text');
    Check(Tags.Tags[0] = 'HD', 'Stored tag is trimmed');
    Check(not Tags.AddTag('hd'), 'Duplicate tags are case-insensitively rejected');
    Tags.Editor.Text := 'Live TV';
    Tags.CommitText;
    Check((Tags.Tags.Count = 2) and (Tags.Editor.Text = ''), 'Commit adds tag and clears input');
    Tags.ReadOnly := True;
    Tags.RemoveTag(0);
    Check((Tags.Tags.Count = 2) and not Tags.AddTag('Blocked'), 'ReadOnly blocks user/API mutations');
    Tags.ReadOnly := False;
    Stream.WriteComponent(Tags);
    Stream.Position := 0;
    TagsCopy := TERDTagEdit(Stream.ReadComponent(nil));
    try Check(TagsCopy.Tags.Text = Tags.Tags.Text, 'Tags survive component streaming')
    finally TagsCopy.Free end;
    Stream.Clear;

    Split := TERDSplitButton.Create(Form);
    Split.Parent := Form;
    Split.SetBounds(300, 10, 180, 40);
    Split.AutoSize := False;
    Split.OnClick := Counter.Changed;
    Split.OnDropDown := Counter.Changed;
    Counter.Changes := 0;
    Split.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(20, 20));
    Split.Perform(WM_LBUTTONUP, 0, MakeLParam(20, 20));
    Check(Counter.Changes = 1, 'Split primary region triggers one action');
    Split.OnClick := nil;
    Split.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Split.Width - 12, 20));
    Split.Perform(WM_LBUTTONUP, 0, MakeLParam(Split.Width - 12, 20));
    Check(Counter.Changes = 2, 'Split arrow dispatches dropdown without primary action');

    Expander := TERDExpander.Create(Form);
    Expander.Parent := Form;
    Expander.Top := 200;
    Content := TERDButton.Create(Expander);
    Content.Name := 'ContentSample';
    Content.Parent := Expander;
    Content.Top := 60;
    IntentionallyHidden := TERDButton.Create(Expander);
    IntentionallyHidden.Name := 'IntentionallyHidden';
    IntentionallyHidden.Parent := Expander;
    IntentionallyHidden.Visible := False;
    OriginalHeight := Expander.Height;
    Expander.Expanded := False;
    Check(not Content.Visible and (Expander.Height < OriginalHeight), 'Collapse hides content and reduces height');
    Expander.Expanded := True;
    Check(Content.Visible and not IntentionallyHidden.Visible, 'Expand preserves intentional child visibility');
    Check(Expander.Height = OriginalHeight, 'Expanded height is restored');
    Expander.Expanded := False;
    Stream.WriteComponent(Expander);
    Stream.Position := 0;
    ExpanderCopy := TERDExpander(Stream.ReadComponent(nil));
    try
      Check(not ExpanderCopy.Expanded, 'Collapsed state survives streaming');
      ExpanderCopy.Expanded := True;
      Check(ExpanderCopy.Height = OriginalHeight, 'Collapsed streaming preserves expanded height');
      Check(ExpanderCopy.FindComponent('ContentSample') <> nil, 'Expander streams editable children');
      Check(TControl(ExpanderCopy.FindComponent('ContentSample')).Visible,
        'Expanded copy restores child visibility after collapsed streaming');
      Check(not TControl(ExpanderCopy.FindComponent('IntentionallyHidden')).Visible,
        'Collapsed streaming preserves intentionally hidden children');
    finally ExpanderCopy.Free end;

    Track := TTrackProbe.Create(Form);
    Track.Parent := Form;
    Track.AutoSize := False;
    Track.Orientation := toVertical;
    Track.SetBounds(500, 10, 40, 220);
    Track.ClickAt(20, 10);
    Check(Track.Position > 90, 'Vertical track increases towards top');
    Track.ClickAt(20, 210);
    Check(Track.Position < 10, 'Vertical track decreases towards bottom');
    Track.RenderTo(Bitmap);
    Check((Bitmap.Width = Track.Width) and (Bitmap.Height = Track.Height), 'Vertical track renders at control dimensions');

    Range := TRangeProbe.Create(Form);
    Range.Parent := Form;
    Range.Position := 20;
    Range.UpperValue := 80;
    Range.ActiveThumb := rtUpper;
    Range.Step(VK_RIGHT);
    Check((Range.Position = 20) and (Range.UpperValue = 81), 'Upper-thumb keys preserve lower value');
    Range.Position := 99;
    Check(Range.Position = 81, 'Lower thumb cannot cross upper thumb');
    Range.UpperValue := 0;
    Check(Range.UpperValue = 81, 'Upper thumb cannot cross lower thumb');
    Range.Max := 50;
    Check((Range.Position = 50) and (Range.UpperValue = 50), 'Range bounds clamp both handles');
    Range.RenderTo(Bitmap);
  finally
    Bitmap.Free;
    Stream.Free;
    Form.Free;
    Counter.Free;
  end;
end;

procedure TestLinkLabel;
var Link: TLinkProbe; Counter: TChangeProbe; Bitmap: TBitmap;
begin
  Link := TLinkProbe.Create(nil);
  Counter := TChangeProbe.Create;
  Bitmap := TBitmap.Create;
  try
    Link.OnClick := Counter.Changed;
    Check(not Link.AutoOpen and not Link.Visited, 'Link does not auto-open by default');
    Link.URL := 'https://erdesigns.be';
    Link.Activate;
    Check(Link.Visited and (Counter.Changes = 1), 'Link click marks visited and notifies');
    Link.URL := 'https://erdesigns.be/products';
    Check(not Link.Visited, 'New URL resets visited state');
    Link.PressEnter;
    Check(Link.Visited and (Counter.Changes = 2), 'Enter activates link');
    Link.Enabled := False;
    Link.Activate;
    Link.PressEnter;
    Check((Counter.Changes = 2) and not Link.OpenLink, 'Disabled link cannot activate or launch URL');
    Link.Enabled := True;
    Link.URL := '';
    Check(not Link.OpenLink, 'Empty URL is not launched');
    Link.Icon.BuiltIn := ei028Info;
    Link.RenderTo(Bitmap);
    Check(not Link.HandleAllocated and (Bitmap.Width = Link.Width), 'Link with icon renders without a window');
  finally Bitmap.Free; Link.Free; Counter.Free end;
end;

procedure TestDateSegmentsAndDateTime;
var Picker: TDateProbe; Combined, Copy: TERDDateTimePicker; Counter: TChangeProbe;
  Stream: TMemoryStream; Bitmap: TBitmap; I: Integer; R: TRect;
begin
  RegisterClass(TERDDateTimePicker);
  Picker := TDateProbe.Create(nil);
  Combined := TERDDateTimePicker.Create(nil);
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    Picker.DisplayFormat := 'dd/mm/yyyy';
    Picker.Date := EncodeDate(2024, 1, 31);
    Picker.ActiveSegment := dsMonth;
    Picker.Step(1);
    Check(Picker.Date = EncodeDate(2024, 2, 29), 'Changing month clamps to leap-day');
    Picker.ActiveSegment := dsYear;
    Picker.Step(1);
    Check(Picker.Date = EncodeDate(2025, 2, 28), 'Changing leap year clamps to valid day');
    Picker.ActiveSegment := dsDay;
    Picker.Step(1);
    Check(Picker.Date = EncodeDate(2025, 2, 1), 'Day wraps within selected month');
    Picker.TypeText('27');
    Picker.TypeText('10');
    Picker.TypeText('2026');
    Check(Picker.Date = EncodeDate(2026, 10, 27), 'Date digits follow day/month/year segments');
    Picker.DisplayFormat := 'yyyy-mm-dd';
    Picker.ActiveSegment := dsYear;
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = dsMonth, 'Segment navigation follows custom format order');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = dsDay, 'ISO date navigation reaches day after month');
    Picker.DisplayFormat := 'dddd, dd mmmm yyyy';
    R := Picker.SegmentRect(dsDay);
    Check(R.Left > 10, 'Weekday text stays outside editable day segment');
    Picker.DisplayFormat := 'mmmm yyyy';
    Picker.ActiveSegment := dsDay;
    Check(Picker.ActiveSegment = dsMonth, 'Missing date field cannot remain active');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = dsYear, 'Month/year format skips missing day');
    Picker.ReadOnly := True;
    Picker.Step(1);
    Picker.TypeText('2030');
    Check(Picker.Date = EncodeDate(2026, 10, 27), 'Readonly date blocks keyboard and stepping');
    Picker.RenderTo(Bitmap);
    Check(not Picker.HandleAllocated, 'Segmented DatePicker renders without a window');
    Combined.DateFormat := 'dd/mm/yyyy';
    Combined.ShowSeconds := True;
    Combined.TimeFormat := tf12Hour;
    Combined.OnChange := Counter.Changed;
    Combined.DateTime := EncodeDate(2024, 2, 29) + EncodeTime(13, 14, 15, 321);
    Check(Counter.Changes = 1, 'DateTime assignment raises one event for both fields');
    Combined.DateEditor.ActiveSegment := dsYear;
    Combined.DateEditor.Step(1);
    Check(SameValue(Combined.DateTime, EncodeDate(2025, 2, 28) + EncodeTime(13, 14, 15, 321)),
      'Date editing preserves time and milliseconds');
    Combined.TimeEditor.ActiveSegment := tsMinute;
    Combined.TimeEditor.Step(1);
    Check(SameValue(Combined.DateTime, EncodeDate(2025, 2, 28) + EncodeTime(13, 15, 15, 321)),
      'Time editing preserves date');
    Combined.DateTime := -1.75;
    Check(SameValue(Combined.DateTime, -1.75), 'DateTime retains pre-epoch negative-date convention');
    I := Counter.Changes;
    Combined.DateTime := Combined.DateTime;
    Check(Counter.Changes = I, 'Unchanged DateTime does not notify');
    Combined.ReadOnly := True;
    Combined.DateEditor.Step(1);
    Combined.TimeEditor.Step(1);
    Check((Counter.Changes = I) and Combined.DateEditor.ReadOnly and Combined.TimeEditor.ReadOnly,
      'Combined readonly applies to both editors');
    Combined.Enabled := False;
    Check(not Combined.DateEditor.Enabled and not Combined.TimeEditor.Enabled, 'Combined disable reaches both fields');
    Combined.Enabled := True;
    Combined.RenderTo(Bitmap);
    Check(not Combined.HandleAllocated, 'DateTime preview renders without a window');
    Combined.OnChange := nil;
    Stream.WriteComponent(Combined);
    Stream.Position := 0;
    Copy := TERDDateTimePicker(Stream.ReadComponent(nil));
    try
      Check(SameValue(Copy.DateTime, Combined.DateTime) and Copy.ReadOnly and Copy.ShowSeconds and
        (Copy.DateFormat = Combined.DateFormat) and (Copy.TimeFormat = tf12Hour), 'DateTime DFM roundtrip');
      Check(Copy.ControlCount = 2, 'Streaming does not duplicate internal date/time editors');
    finally Copy.Free end;
  finally Bitmap.Free; Stream.Free; Picker.Free; Combined.Free; Counter.Free end;
end;

procedure TestTimePicker;
var Form: TForm; Picker: TTimeProbe; Stored, Copy: TERDTimePicker;
  Counter: TChangeProbe; Bitmap: TBitmap; Stream: TMemoryStream;
  Theme: TERDTheme; Mode: TERDThemeMode; R: TRect;
  SavedFormat: TFormatSettings; I: Integer; Rejected: Boolean;
  procedure CheckTime(Hour, Minute, Second, Millisecond: Word; const MessageText: string);
  var H, M, S, MS: Word;
  begin
    DecodeTime(Picker.Time, H, M, S, MS);
    Check((H = Hour) and (M = Minute) and (S = Second) and (MS = Millisecond), MessageText);
  end;
begin
  RegisterClass(TERDTimePicker);
  SavedFormat := FormatSettings;
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Bitmap := TBitmap.Create;
  Stream := TMemoryStream.Create;
  try
    Stored := TERDTimePicker.Create(nil);
    try
      Stored.Time := EncodeTime(13, 30, 45, 321);
      Stored.TimeFormat := tf12Hour;
      Stored.ShowSeconds := True;
      Stored.ShowSpinButtons := False;
      Stored.ReadOnly := True;
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'TimePicker renders before parenting without creating a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDTimePicker(Stream.ReadComponent(nil));
    try
      Check(SameValue(Copy.Time, EncodeTime(13, 30, 45, 321)) and
        (Copy.TimeFormat = tf12Hour) and Copy.ShowSeconds and not Copy.ShowSpinButtons and
        Copy.ReadOnly, 'TimePicker DFM preserves time, format, seconds, spin buttons and readonly');
    finally Copy.Free end;
    Picker := TTimeProbe.Create(Form);
    Picker.Parent := Form;
    Picker.OnChange := Counter.Changed;
    Check((Picker.Time = 0) and (Picker.TimeFormat = tf24Hour) and not Picker.ShowSeconds and
      Picker.ShowSpinButtons and Picker.TabStop, 'TimePicker defaults');
    Picker.Time := EncodeTime(23, 59, 45, 321);
    I := Counter.Changes;
    Picker.Time := Picker.Time;
    Check(Counter.Changes = I, 'Unchanged time does not raise OnChange');
    Picker.ActiveSegment := tsHour;
    Picker.Step(1);
    CheckTime(0, 59, 45, 321, 'Hour wraps without carrying into minutes');
    Picker.Step(-1);
    CheckTime(23, 59, 45, 321, 'Hour wraps backwards');
    Picker.ActiveSegment := tsMinute;
    Picker.Step(1);
    CheckTime(23, 0, 45, 321, 'Minute wraps without changing hour or seconds');
    Picker.ShowSeconds := True;
    Picker.ActiveSegment := tsSecond;
    Picker.Step(15);
    CheckTime(23, 0, 0, 321, 'Second wraps and retains milliseconds');
    Picker.ActiveSegment := tsHour;
    Picker.TypeText('13');
    Picker.TypeText('47');
    Picker.TypeText('52');
    CheckTime(13, 47, 52, 321, 'Digit input enters complete time through segments');
    Picker.ActiveSegment := tsMinute;
    Picker.PressKey(VK_HOME);
    CheckTime(13, 0, 52, 321, 'Home selects minimum for active field');
    Picker.PressKey(VK_END);
    CheckTime(13, 59, 52, 321, 'End selects maximum for active field');
    Picker.ActiveSegment := tsHour;
    Picker.TypeText('29');
    CheckTime(2, 59, 52, 321, 'Invalid second digit is rejected rather than creating hour 29');
    Picker.TimeFormat := tf12Hour;
    Picker.Time := EncodeTime(0, 15, 30, 0);
    Picker.ActiveSegment := tsHour;
    Picker.Step(1);
    CheckTime(1, 15, 30, 0, '12-hour field wraps from 12 to 1 within AM');
    Picker.ActiveSegment := tsPeriod;
    Picker.TypeText('p');
    CheckTime(13, 15, 30, 0, 'PM typing preserves minute and seconds');
    Picker.Step(1);
    CheckTime(1, 15, 30, 0, 'AM/PM stepping toggles by twelve hours');
    FormatSettings.TimeSeparator := '.';
    FormatSettings.TimeAMString := 'vorm';
    FormatSettings.TimePMString := 'nam';
    Check(Pos('01.15.30 vorm', Picker.DisplayText) = 1, 'Display respects locale separator and period labels');
    Picker.TypeText('n');
    CheckTime(13, 15, 30, 0, 'Locale period initial selects PM');
    Picker.ShowSeconds := False;
    Picker.ActiveSegment := tsSecond;
    Check((Picker.ActiveSegment = tsMinute) and Picker.SegmentRect(tsSecond).IsEmpty,
      'Hidden seconds cannot remain active or receive mouse hits');
    Picker.ActiveSegment := tsPeriod;
    Picker.TimeFormat := tf24Hour;
    Check(Picker.ActiveSegment = tsMinute, 'Hidden period cannot remain active');
    Picker.ReadOnly := True;
    I := Counter.Changes;
    Picker.TypeText('12');
    Picker.Step(1);
    Picker.PressKey(VK_END);
    Check(Counter.Changes = I, 'Readonly blocks digits, stepping and End');
    Picker.ReadOnly := False;
    Picker.Enabled := False;
    Picker.Step(1);
    Picker.TypeText('12');
    Check(Counter.Changes = I, 'Disabled picker blocks user edits');
    Picker.Enabled := True;
    Picker.Time := EncodeDate(2026, 10, 13) + 0.5;
    CheckTime(12, 0, 0, 0, 'Date part is normalized out of a time value');
    Picker.Time := -0.5;
    CheckTime(12, 0, 0, 0, 'Negative TDateTime follows absolute fractional time convention');
    Rejected := False;
    try Picker.Time := NaN except on E: EArgumentException do Rejected := True end;
    Check(Rejected, 'NaN time is rejected');
    Rejected := False;
    try Picker.Time := Infinity except on E: EArgumentException do Rejected := True end;
    Check(Rejected, 'Infinite time is rejected');
    Form.Show;
    Application.ProcessMessages;
    R := Picker.SegmentRect(tsHour);
    Picker.ClickAt((R.Left + R.Right) div 2, Picker.Height div 2);
    Check(Picker.Focused and (Picker.ActiveSegment = tsHour), 'Mouse selects and focuses hour segment');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = tsMinute, 'Right skips hidden seconds and navigates fields');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = tsHour, 'Right wraps through visible fields');
    Picker.ClickAt(Picker.Width - 10, Picker.Height div 4);
    CheckTime(13, 0, 0, 0, 'Spin button changes the active field once on click');
    Theme := TERDTheme.Create(Form);
    Picker.Theme := Theme;
    for Mode := Low(TERDThemeMode) to High(TERDThemeMode) do
    begin
      Theme.Mode := Mode;
      Picker.RenderTo(Bitmap);
      Check((Bitmap.Width = Picker.Width) and (Bitmap.Height = Picker.Height), 'TimePicker renders for each theme');
    end;
  finally
    FormatSettings := SavedFormat;
    Bitmap.Free;
    Stream.Free;
    Form.Free;
    Counter.Free;
  end;
end;

procedure TestCheckListBox;
var Form: TForm; List: TCheckListProbe; Copy: TERDCheckListBox;
  Counter: TCheckListCounter; Stream: TMemoryStream; Bitmap: TBitmap;
  Row: TRect; Item: TERDCheckListItem; I: Integer; Mode: TERDThemeMode;
  Theme: TERDTheme;
begin
  RegisterClass(TERDCheckListBox);
  Form := TForm.CreateNew(nil);
  Counter := TCheckListCounter.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    List := TCheckListProbe.Create(Form);
    List.Parent := Form;
    List.SetBounds(0, 0, 400, 160);
    List.OnClickCheck := Counter.Checked;
    Item := List.Items.Add;
    Item.Caption := 'Live TV';
    List.Items.Add.Caption := 'Movies';
    List.Items.Add.Caption := 'Unavailable';
    List.Items[2].Enabled := False;
    Check((List.ItemCount = 3) and (List.CheckedCount = 0), 'Checklist defaults and item count');
    Row := List.ItemRect(0);
    List.PressAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    Check((List.ItemIndex = 0) and not List.Checked[0], 'Caption selects without checking');
    List.PressAt(Row.Left + 14, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 14, (Row.Top + Row.Bottom) div 2);
    Check(List.Checked[0] and (Counter.Count = 1) and (Counter.LastIndex = 0),
      'Checkbox click checks once and reports index');
    List.PressAt(Row.Left + 14, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    Check(List.Checked[0] and (Counter.Count = 1), 'Release away from checkbox cancels toggle');
    List.AllowGrayed := True;
    List.Space;
    Check(List.State[0] = cbGrayed, 'Space cycles checked to grayed');
    List.Space;
    Check(List.State[0] = cbUnchecked, 'Space cycles grayed to unchecked');
    List.AllowGrayed := False;
    List.Space;
    List.Space;
    Check(List.State[0] = cbUnchecked, 'Without AllowGrayed Space uses two states');
    I := Counter.Count;
    List.Toggle(2);
    List.Enabled := False;
    List.Toggle(0);
    List.Enabled := True;
    Check((Counter.Count = I) and not List.Checked[2] and not List.Checked[0],
      'Disabled control and item block user checks');
    List.CheckOnClick := True;
    List.PressAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    Check(List.Checked[0], 'CheckOnClick enables caption toggle');
    I := Counter.Count;
    List.CheckAll(cbChecked);
    Check((List.CheckedCount = 2) and not List.Checked[2], 'CheckAll skips disabled items');
    List.CheckAll(cbChecked, True);
    Check((List.CheckedCount = 3) and (Counter.Count = I), 'Programmatic batch has no user event');
    List.Items[0].State := cbGrayed;
    Item.Index := 1;
    Check((List.Items[1] = Item) and (List.State[1] = cbGrayed), 'State follows item when reordered');
    List.Items.Delete(0);
    Check((List.Items[0] = Item) and (List.State[0] = cbGrayed), 'Removing earlier row preserves remaining state');
    // Stream the palette class rather than the test-only descendant.
    Copy := TERDCheckListBox.Create(nil);
    try
      Copy.Items.Assign(List.Items);
      Copy.AllowGrayed := True;
      Stream.WriteComponent(Copy);
    finally Copy.Free end;
    Stream.Position := 0;
    Copy := TERDCheckListBox(Stream.ReadComponent(nil));
    try
      Check((Copy.ItemCount = 2) and (Copy.State[0] = cbGrayed) and
        not Copy.Items[1].Enabled and Copy.Items[1].Checked, 'DFM stream preserves caption/state/enabled');
    finally Copy.Free end;
    Theme := TERDTheme.Create(Form);
    List.Theme := Theme;
    for Mode := Low(TERDThemeMode) to High(TERDThemeMode) do
    begin
      Theme.Mode := Mode;
      List.RenderTo(Bitmap);
      Check((Bitmap.Width = List.Width) and (Bitmap.Height = List.Height), 'Checklist renders in each theme');
    end;
    List.Items.BeginUpdate;
    try
      for I := 1 to 40 do List.Items.Add.Caption := 'Extra ' + IntToStr(I);
    finally List.Items.EndUpdate end;
    List.ItemIndex := List.ItemCount - 1;
    List.EnsureVisible(List.ItemIndex);
    Check(List.ScrollPos > 0, 'Checklist keyboard selection can reach rows below viewport');
    List.Items.Clear;
    Check((List.ItemCount = 0) and (List.CheckedCount = 0) and (List.ItemIndex = -1), 'Clear resets item selection');
  finally Bitmap.Free; Stream.Free; Form.Free; Counter.Free end;
end;

procedure TestFeedbackRegressions;
var Form: TForm; Values: TERDKeyValueList; Tags: TERDTagEdit;
  Token: TERDTagToken; Ribbon: TERDRibbon; Group: TERDRibbonGroup;
  First, Second, Independent: TERDRibbonItem; Y, I: Integer;
  FoundFirst, FoundSecond: Boolean;
begin
  Form := TForm.CreateNew(nil);
  try
    Values := TERDKeyValueList.Create(Form);
    Values.Parent := Form;
    Values.SetBounds(0, 0, 400, 160);
    Values.SetValue('Application', 'Demo');
    Values.SetValue('Theme', 'ERDesigns Light');
    FoundFirst := False;
    FoundSecond := False;
    for Y := 0 to Values.Height - 1 do
    begin
      I := Values.RowAt(50, Y);
      FoundFirst := FoundFirst or (I = 0);
      FoundSecond := FoundSecond or (I = 1);
    end;
    Check(FoundFirst and FoundSecond, 'Key/value rows occupy distinct reachable rows');
    Tags := TERDTagEdit.Create(Form);
    Tags.Parent := Form;
    Tags.SetBounds(0, 180, 400, 80);
    Tags.AddTag('Live TV');
    Token := nil;
    for I := 0 to Tags.ControlCount - 1 do
      if Tags.Controls[I] is TERDTagToken then Token := TERDTagToken(Tags.Controls[I]);
    Check(Token <> nil, 'Tag creates a themed token');
    Token.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(8, Token.Height div 2));
    Token.Perform(WM_LBUTTONUP, 0, MakeLParam(8, Token.Height div 2));
    Check(Tags.Tags.Count = 1, 'Clicking tag caption does not remove tag');
    Token.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Token.Width - 14, Token.Height div 2));
    Token.Perform(WM_LBUTTONUP, 0, MakeLParam(Token.Width - 14, Token.Height div 2));
    Application.ProcessMessages;
    Check(Tags.Tags.Count = 0, 'Clicking tag close glyph removes tag');
    Ribbon := TERDRibbon.Create(Form);
    Group := Ribbon.Tabs.Add.Groups.Add;
    First := Group.Items.Add;
    Second := Group.Items.Add;
    Independent := Group.Items.Add;
    First.Kind := rikToggle;
    Second.Kind := rikToggle;
    Independent.Kind := rikToggle;
    First.GroupIndex := 1;
    Second.GroupIndex := 1;
    First.Execute;
    Second.Execute;
    Check(Second.Down and not First.Down, 'Radio ribbon items exclude peers');
    Second.Execute;
    Check(Second.Down, 'Radio item remains selected without AllowAllUp');
    Independent.Execute;
    Check(Independent.Down and Second.Down, 'Independent toggle preserves radio choice');
    Independent.Execute;
    Check(not Independent.Down, 'Independent ribbon toggle can be unchecked');
  finally Form.Free end;
end;

begin
  try
    Application.Initialize;
    TestIconLifetimeAndStreaming;
    TestHostedStatusPanels;
    TestThemesAndRibbon;
    TestPopupRowsAndCommandSwitch;
    TestComboMouseChange;
    TestDemoLayoutAndSpinMargins;
    TestWindowsFocusCuesAndAutoSave;
    TestVirtualTreeThemeAndLazyNodes;
    TestCommonControls;
    TestFeedbackRegressions;
    TestCheckListBox;
    TestTimePicker;
    TestDateSegmentsAndDateTime;
    TestLinkLabel;
    Writeln(Checks, ' VCL runtime checks passed.');
  except on E: Exception do begin Writeln(E.ClassName, ': ', E.Message); Halt(1) end end;
end.
