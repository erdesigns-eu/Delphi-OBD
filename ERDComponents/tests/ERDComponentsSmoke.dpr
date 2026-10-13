program ERDComponentsSmoke;
{$APPTYPE CONSOLE}
uses
  System.SysUtils, System.Classes, System.Types, Vcl.Forms, Vcl.Controls,
  Winapi.Windows, Vcl.StdCtrls, Vcl.Graphics, Vcl.ImgList,
  ERD.Components.Types, ERD.Components.Icons, ERD.Components.IconCatalog,
  ERD.Components.Buttons, ERD.Components.Panel, ERD.Components.Chips,
  Showcase in '../demo/Showcase.pas',
  ERD.Components.SpinEdit, ERD.Components.Edits, ERD.Components.PopupList, ERD.Components.TitleBar, ERD.Components.StatusBar, ERD.Components.Theme, ERD.Components.Ribbon;
type
  TFocusProbe = class(TERDButton)
  public
    function CuesVisible: Boolean;
  end;
  TChangeProbe = class
    Changes: Integer;
    procedure Changed(Sender: TObject);
  end;
var Checks: Integer;
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
    Writeln(Checks, ' VCL runtime checks passed.');
  except on E: Exception do begin Writeln(E.ClassName, ': ', E.Message); Halt(1) end end;
end.
