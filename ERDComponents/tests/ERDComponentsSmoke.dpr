program ERDComponentsSmoke;
{$APPTYPE CONSOLE}
uses
  ERD.Components.Paint, ERD.Components.Control, ERD.Components.LanguageServices, ERD.Components.TextLayout, ERD.Components.CodeEditor, ERD.Components.Syntax,
  ERD.Components.TextModel, ERD.Components.TextInput,
  System.IOUtils,
  System.SysUtils, System.Classes, System.Types, System.Math, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls,
  Winapi.Windows, Vcl.Clipbrd, Vcl.ActnList, Vcl.StdCtrls, Vcl.Graphics, Vcl.ImgList,
  ERD.Components.Tabs, ERD.Components.Memo, ERD.Components.Menus,
  ERD.Components.Rating,
  ERD.Components.CommandPalette,
  ERD.Components.DropZone,
  ERD.Components.LoadingIndicator,
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
  TLanguageServiceProbe = class(TInterfacedObject, IERDCodeLanguageService)
  public
    Snapshot: TERDLanguageRequest;
    Responses: IERDLanguageResponses;
    Requests, Cancels: Integer;
    procedure Request(const Value: TERDLanguageRequest; const Reply: IERDLanguageResponses);
    procedure Cancel(RequestID: Int64);
  end;
  TCodeEditorProbe = class(TERDCodeEditor)
  public
    function RowCount: Integer;
    procedure FoldMouse(Action: Integer; Outside: Boolean = False);
    function FoldPixel(Guide: Boolean): TColor;
    function GutterBackground: TColor;
  end;
  TEditKeyFilter = class
  public
    Calls: Integer;
    procedure Filter(Sender: TObject; var Key: Char);
  end;
  TTextInputProbe = class(TERDTextInput)
  public
    function EditMenu: TERDPopupMenu;
    procedure RebuildWindow;
  end;
  TPalettePopupProbe = class
    OwnerForm: TForm;
    Timer: TTimer;
    SawPopup, OwnerEnabled, NoButtons, TopAnchored: Boolean;
    Attempts: Integer;
    procedure InspectPopup(Sender: TObject);
  end;
  TTabProbe = class(TERDTabs)
  public
    procedure PressAt(X, Y: Integer);
    procedure ReleaseAt(X, Y: Integer);
    procedure DragTo(X, Y: Integer);
    procedure PressKey(Key: Word; Shift: TShiftState);
  end;
  TTabCounter = class
    Changes, ClosedCount, Reorders: Integer;
    VetoClose, VetoMove, VetoDetach: Boolean;
    procedure Changed(Sender: TObject);
    procedure Closing(Sender: TObject; Index: Integer; var CanClose: Boolean);
    procedure Reordering(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
    procedure Reordered(Sender: TObject; OldIndex, NewIndex: Integer);
    procedure Detaching(Sender: TObject; Index: Integer; var Allow: Boolean);
    procedure Closed(Sender: TObject; TabID: Integer; Page: TWinControl);
  end;
  TEditProbe = class(TERDEdit)
  public
    procedure RevealByKeyboard;
  end;
  TValidationProbe = class
    Calls: Integer;
    procedure ValidateText(Sender: TObject; const Text: string;
      var State: TERDEditValidationState; var MessageText: string);
  end;
  TRatingProbe = class(TERDRating)
  public
    procedure PressAt(X, Y: Integer);
    procedure ReleaseAt(X, Y: Integer);
    procedure PressKey(Key: Word);
    procedure PreviewAt(X, Y: Integer);
  end;
  TCommandCounter = class
    Count: Integer;
    LastKey: string;
    procedure Executed(Sender: TObject; Command: TERDCommandItem);
  end;
  TDropCounter = class
    Count, FileCount: Integer;
    FirstFile: string;
    procedure Dropped(Sender: TObject; Files: TStrings);
  end;
  TLoadingProbe = class(TERDLoadingIndicator)
  public
    procedure NextFrame;
  end;
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
procedure TTabProbe.PressAt(X, Y: Integer);
begin MouseDown(mbLeft, [], X, Y) end;
procedure TTabProbe.ReleaseAt(X, Y: Integer);
begin MouseUp(mbLeft, [], X, Y) end;
procedure TTabProbe.DragTo(X, Y: Integer);
begin MouseMove([ssLeft], X, Y) end;
procedure TTabProbe.PressKey(Key: Word; Shift: TShiftState);
begin KeyDown(Key, Shift) end;
procedure TTabCounter.Changed(Sender: TObject);
begin Inc(Changes) end;
procedure TTabCounter.Closing(Sender: TObject; Index: Integer; var CanClose: Boolean);
begin CanClose := not VetoClose end;
procedure TTabCounter.Reordering(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
begin Allow := not VetoMove end;
procedure TTabCounter.Reordered(Sender: TObject; OldIndex, NewIndex: Integer);
begin Inc(Reorders) end;
procedure TTabCounter.Detaching(Sender: TObject; Index: Integer; var Allow: Boolean);
begin Allow := not VetoDetach end;
procedure TTabCounter.Closed(Sender: TObject; TabID: Integer; Page: TWinControl);
begin Inc(ClosedCount) end;

procedure TEditProbe.RevealByKeyboard;
var Key: Word;
begin Key := Ord('V'); KeyDown(Key, [ssAlt]) end;
procedure TValidationProbe.ValidateText(Sender: TObject; const Text: string;
  var State: TERDEditValidationState; var MessageText: string);
begin
  Inc(Calls);
  if Text = '' then begin State := evsError; MessageText := 'Required' end
  else if Length(Text) < 4 then begin State := evsWarning; MessageText := 'Use four characters' end
  else begin State := evsSuccess; MessageText := '' end;
  TERDEdit(Sender).Validate; // Reentrant request must not call the validator again.
end;

procedure TRatingProbe.PressAt(X, Y: Integer);
begin MouseDown(mbLeft, [], X, Y) end;
procedure TRatingProbe.ReleaseAt(X, Y: Integer);
begin MouseUp(mbLeft, [], X, Y) end;
procedure TRatingProbe.PressKey(Key: Word);
begin KeyDown(Key, []) end;
procedure TRatingProbe.PreviewAt(X, Y: Integer);
begin MouseMove([], X, Y) end;

procedure TCommandCounter.Executed(Sender: TObject; Command: TERDCommandItem);
begin Inc(Count); LastKey := Command.Key end;

procedure TDropCounter.Dropped(Sender: TObject; Files: TStrings);
begin Inc(Count); FileCount := Files.Count; FirstFile := Files[0] end;

procedure TLoadingProbe.NextFrame;
begin Advance end;

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

procedure TLanguageServiceProbe.Request(const Value: TERDLanguageRequest; const Reply: IERDLanguageResponses);
begin Snapshot := Value; Responses := Reply; Inc(Requests); end;
procedure TLanguageServiceProbe.Cancel(RequestID: Int64);
begin Inc(Cancels); end;

procedure TCodeEditorProbe.FoldMouse(Action: Integer; Outside: Boolean);
var View: TRect; X, Y: Integer;
begin
  View := TextViewport; X := View.Left - ScaleValue(13); Y := View.Top + RowHeight div 2;
  if Outside then X := View.Left + ScaleValue(30);
  case Action of
    0: Perform(WM_MOUSEMOVE, 0, MakeLParam(X, Y));
    1: Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(X, Y));
    2: Perform(WM_LBUTTONUP, 0, MakeLParam(X, Y));
  end;
end;
function TCodeEditorProbe.GutterBackground: TColor;
begin Result := ColorToRGB(ERDMixColor(Palette.Background, Palette.ForegroundText, 0.92)); end;

function TCodeEditorProbe.FoldPixel(Guide: Boolean): TColor;
var Bitmap: TBitmap; View: TRect; X, Y: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    RenderTo(Bitmap); View := TextViewport;
    X := View.Left - ScaleValue(13); Y := View.Top + RowHeight + 1;
    if not Guide then
    begin Dec(X, ScaleValue(3)); Y := View.Top + RowHeight div 2 - ScaleValue(3); end;
    Result := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
  finally Bitmap.Free; end;
end;
function TCodeEditorProbe.RowCount: Integer;
begin Result := Length(VisualRows); end;

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
procedure TestRibbonHostedControls;
var Form: TForm; Ribbon: TERDRibbon; Host: TERDPanel; Edit: TERDEdit;
  Group, Other: TERDRibbonGroup; Bitmap: TBitmap; Original: TRect; Rejected: Boolean;
begin
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
  try
    Form.ClientWidth := 900; Form.ClientHeight := 500;
    Host := TERDPanel.Create(Form); Host.Parent := Form; Host.Top := 180;
    Edit := TERDEdit.Create(Form); Edit.Parent := Host;
    Edit.SetBounds(10, 10, 180, 26); Edit.Text := 'Retained input';
    Original := Edit.BoundsRect;
    Ribbon := TERDRibbon.Create(Form); Ribbon.Parent := Form;
    Group := Ribbon.Tabs.Add.Groups.Add; Group.Caption := 'Input';
    Group.ControlWidth := 220; Group.Control := Edit;
    Other := Ribbon.Tabs[0].Groups.Add;
    Ribbon.ActiveTab := 0;
    Form.Show; Application.ProcessMessages;
    Bitmap.SetSize(Ribbon.Width, Ribbon.Height);
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check((Edit.Parent = Ribbon) and Edit.Visible and (Edit.Text = 'Retained input'),
      'Classic ribbon hosts the original input instance');
    Rejected := False;
    try Other.Control := Edit except on E: EArgumentException do Rejected := True end;
    Check(Rejected and (Other.Control = nil) and (Group.Control = Edit),
      'A control cannot bind to two ribbon groups');
    Group.Visible := False;
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check(not Edit.Visible, 'Hidden ribbon group hides its hosted input');
    Group.Visible := True;
    Ribbon.RibbonStyle := rsSimplified;
    Bitmap.SetSize(Ribbon.Width, Ribbon.Height);
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check(Edit.Visible and (Edit.Parent = Ribbon), 'Simplified ribbon hosts fitting controls');
    Ribbon.Collapsed := True;
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check(not Edit.Visible, 'Collapsed ribbon hides inline controls');
    Ribbon.Collapsed := False;
    Group.Control := nil;
    Check((Edit.Parent = Host) and Edit.Visible and EqualRect(Edit.BoundsRect, Original) and
      (Edit.Owner = Form) and (Edit.Text = 'Retained input'), 'Unbinding restores layout without destroying input');
    Group.Control := Edit;
    Edit.Free;
    Check(Group.Control = nil, 'FreeNotification clears freed hosted input');
  finally Bitmap.Free; Form.Free end;
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
  NativeEdit: TERDTextInput;
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
      if Spin.Controls[I] is TERDTextInput then
        NativeEdit := TERDTextInput(Spin.Controls[I]);
    Check(NativeEdit <> nil, 'SpinEdit hosts the self-painted ERD text engine');
    Form.Show;
    Spin.Value := 18;
    Spin.SetFocus;
    Spin.SelStart := Length(Spin.Text);
    Spin.SelLength := 0;
    Check(GetCaretPos(Caret), 'Native spin caret is available');
    Check(Caret.X = NativeEdit.ClientWidth - MulDiv(2, NativeEdit.CurrentPPI, 96),
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
  Input: TControl;
  function CellInput: TControl;
  var I: Integer; Host: TWinControl;
  begin
    Result := nil;
    for I := 0 to Tree.ControlCount - 1 do
      if Tree.Controls[I] is TERDPanel then
      begin
        Host := TWinControl(Tree.Controls[I]);
        if Host.Visible and (Host.ControlCount > 0) then Exit(Host.Controls[0]);
      end;
  end;
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
    Form.PageTabs.TabIndex := 3;
    Form.Show;
    Application.ProcessMessages;
    Tree.FocusedNode := Child;
    Check(Tree.EditNode(Child, 0), 'Tree starts its ERD text editor');
    Input := CellInput;
    Check(Input is TERDEdit, 'Name cell uses ERD Edit');
    TERDEdit(Input).Text := 'Renamed channel';
    Check(Tree.EndEditNode and (Tree.Text[Child, 0] = 'Renamed channel'),
      'Accepting editor persists text through OnNewText');
    Check(Tree.EditNode(Child, 0), 'Renamed cell can be edited again');
    TERDEdit(CellInput).Text := 'Discard this';
    Tree.CancelEditNode;
    Check(Tree.Text[Child, 0] = 'Renamed channel', 'Escape/cancel preserves model text');
    Check(Tree.EditNode(Child, 2), 'Quality cell starts a combo editor');
    Input := CellInput;
    Check(Input is TERDComboBox, 'Quality uses ERD searchable combo');
    Check(TERDComboBox(Input).Items.Count = 3, 'Column supplies independent combo choices');
    TERDComboBox(Input).Text := '4K';
    Check(Tree.EndEditNode and (Tree.Text[Child, 2] = '4K'), 'Combo writes accepted value');
    Application.ProcessMessages;
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
    Tags.CloseIconSize := 10;
    Tags.CloseSpacing := 6;
    Tags.TagColor := clRed;
    Tags.TagColorStrength := 40;
    Stream.WriteComponent(Tags);
    Stream.Position := 0;
    TagsCopy := TERDTagEdit(Stream.ReadComponent(nil));
    try
      Check(TagsCopy.Tags.Text = Tags.Tags.Text, 'Tags survive component streaming');
      Check((TagsCopy.CloseIconSize = 10) and (TagsCopy.CloseSpacing = 6) and
        (TagsCopy.TagColor = clRed) and (TagsCopy.TagColorStrength = 40), 'Tag appearance properties survive streaming')
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

procedure TestTextPainting;
var Form: TForm; Theme: TERDTheme; Edit: TERDEdit; Memo: TERDMemo;
  Other: TButton; Native: TWinControl; I, J, BeforeInk, AfterInk: Integer;
  Ch: Char; Back: TColor;
  procedure AssignValue(const S: string);
  begin
    if J = 0 then Edit.Text := S else Memo.Lines.Text := S;
  end;
  function CurrentValue: string;
  begin if J = 0 then Result := Edit.Text else Result := TrimRight(Memo.Lines.Text); end;
  function PixelCount(Window: TWinControl; Color: TColor; Match, Repaint: Boolean): Integer;
  var DC: HDC; X, Y: Integer; RGB: COLORREF;
  begin
    Result := 0;
    if Repaint then RedrawWindow(Window.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
    DC := GetDC(Window.Handle);
    try
      RGB := ColorToRGB(Color);
      for Y := 0 to Window.ClientHeight - 1 do
        for X := 0 to Window.ClientWidth - 1 do
          if (GetPixel(DC, X, Y) = RGB) = Match then Inc(Result);
    finally ReleaseDC(Window.Handle, DC) end;
  end;
begin
  Form := TForm.CreateNew(nil);
  try
    Form.SetBounds(100, 100, 520, 320);
    Theme := TERDTheme.Create(Form);
    Edit := TERDEdit.Create(Form); Edit.Parent := Form; Edit.Theme := Theme;
    Edit.SetBounds(12, 12, 400, 28);
    Memo := TERDMemo.Create(Form); Memo.Parent := Form; Memo.Theme := Theme;
    Memo.SetBounds(12, 56, 400, 100);
    Other := TButton.Create(Form); Other.Parent := Form; Other.Top := 180;
    Form.Show; Other.SetFocus; Application.ProcessMessages;
    for J := 0 to 1 do
    begin
      Native := nil;
      if J = 0 then
      begin
        for I := 0 to Edit.ControlCount - 1 do
          if Edit.Controls[I] is TERDTextInput then Native := TWinControl(Edit.Controls[I]);
      end
      else
        for I := 0 to Memo.ControlCount - 1 do
          if Memo.Controls[I] is TERDTextArea then Native := TWinControl(Memo.Controls[I]);
      Check(Native <> nil, 'Edit/Memo have a text window');
      Back := TERDTextInput(Native).Color;
      AssignValue('');
      BeforeInk := PixelCount(Native, Back, False, True);
      AssignValue('Visible before focus');
      AfterInk := PixelCount(Native, Back, False, True);
      Check(AfterInk > BeforeInk + 10, 'Unfocused native text is painted after invalidation');
      Native.SetFocus;
      AssignValue('');
      BeforeInk := PixelCount(Native, Back, False, True);
      for Ch in 'Typed text' do Native.Perform(WM_CHAR, Ord(Ch), 0);
      AfterInk := PixelCount(Native, Back, False, True);
      Check((CurrentValue = 'Typed text') and (AfterInk > BeforeInk + 10),
        'Typing updates both native text and visible glyphs for Edit and Memo');
      Native.Perform(EM_SETSEL, 0, 7);
      Native.Repaint;
      Check(PixelCount(Native, Theme.Palette.Accent, True, False) > 10,
        'Selection uses ERD accent in text engine paint');
      Other.SetFocus;
    end;
  finally Form.Free end;
end;

procedure TestDocumentTabs;
var Form: TForm; Tabs: TTabProbe; Host: TERDPanel;
  Pages: array[0..2] of TERDPanel; Items: array[0..2] of TERDTabItem;
  Counter: TTabCounter; Edit: TERDEdit; R, Target: TRect; I, SelectedID, Changes: Integer;
  Window: TCustomForm; Stored, Copy: TERDTabs; Stream: TMemoryStream;
begin
  RegisterClass(TERDTabs);
  Form := TForm.CreateNew(nil);
  Counter := TTabCounter.Create;
  Stream := TMemoryStream.Create;
  try
    Form.ClientWidth := 800; Form.ClientHeight := 420;
    Host := TERDPanel.Create(Form);
    Host.Parent := Form; Host.SetBounds(0, 44, 640, 300);
    Tabs := TTabProbe.Create(Form);
    Tabs.Parent := Form; Tabs.SetBounds(0, 0, 640, 36); Tabs.TabStyle := tsDocument;
    Check(not Tabs.AllowReorder and not Tabs.AllowDetach, 'Tab drag features are opt-in');
    Tabs.AllowReorder := True; Tabs.AllowDetach := True;
    for I := 0 to 2 do
    begin
      Pages[I] := TERDPanel.Create(Form); Pages[I].Parent := Host; Pages[I].Align := alClient;
      Items[I] := Tabs.Tabs.Add; Items[I].Caption := 'Document ' + IntToStr(I);
      Items[I].Page := Pages[I];
    end;
    Edit := TERDEdit.Create(Pages[1]); Edit.Parent := Pages[1]; Edit.Text := 'Retain this content';
    Tabs.TabIndex := 1;
    Tabs.OnChange := Counter.Changed; Tabs.OnClose := Counter.Closing;
    Tabs.OnClosed := Counter.Closed; Tabs.OnReordering := Counter.Reordering;
    Tabs.OnReordered := Counter.Reordered; Tabs.OnDetaching := Counter.Detaching;
    Form.Show; Application.ProcessMessages;
    SelectedID := Items[1].ID;
    Check(Pages[1].Visible and not Pages[0].Visible and not Pages[2].Visible, 'Only selected attached Page is visible');
    Check(Tabs.MoveTab(0, 2) and (Tabs.Tabs[Tabs.TabIndex].ID = SelectedID) and
      Pages[1].Visible and (Counter.Reorders = 1), 'Reordering retains selected item and Page');
    Counter.VetoMove := True;
    Check(not Tabs.MoveTab(0, 1) and (Counter.Reorders = 1), 'Reorder can be vetoed');
    Counter.VetoMove := False;
    R := Tabs.TabBounds(0); Target := Tabs.TabBounds(1);
    Tabs.Perform(WM_LBUTTONDOWN, MK_LBUTTON,
      MakeLParam((R.Left + R.Right) div 2, Tabs.Height div 2));
    Tabs.Perform(WM_MOUSEMOVE, MK_LBUTTON,
      MakeLParam((Target.Left + Target.Right) div 2, Tabs.Height div 2));
    Check(Tabs.DragInProgress, 'Native mouse movement starts tab drag');
    Tabs.Perform(WM_LBUTTONUP, 0,
      MakeLParam((Target.Left + Target.Right) div 2, Tabs.Height div 2));
    Check(Tabs.Tabs[1].ID = SelectedID, 'Native release over next tab reorders it');
    Check(Tabs.MoveTab(1, 0), 'Restore order after native drag regression');
    R := Tabs.TabBounds(0); Target := Tabs.TabBounds(1);
    Tabs.PressAt((R.Left + R.Right) div 2, Tabs.Height div 2);
    Tabs.DragTo(Target.Right - 24, Tabs.Height div 2);
    Check(Tabs.DragInProgress and (Tabs.Tabs[0].ID = SelectedID), 'Drag shows marker without moving data immediately');
    Tabs.PressKey(VK_ESCAPE, []);
    Tabs.ReleaseAt(Target.Right - 24, Tabs.Height div 2);
    Check((Tabs.Tabs[0].ID = SelectedID) and not Tabs.DragInProgress, 'Escape cancels pending drag');
    Counter.VetoDetach := True;
    Check(not Tabs.DetachTab(Items[1].Index, Point(200, 200)) and (Pages[1].Parent = Host), 'Detach can be vetoed');
    Counter.VetoDetach := False;
    Check(Tabs.DetachTab(Items[1].Index, Point(200, 200)), 'Page-backed tab detaches');
    Window := Tabs.DetachedWindow(Items[1].Index);
    Check((Window <> nil) and Items[1].Detached and (Pages[1].Parent = Window) and
      (Edit.Owner = Pages[1]) and (Edit.Text = 'Retain this content'), 'Detached host uses same Page and content objects');
    Window.Close;
    Application.ProcessMessages;
    Check(not Items[1].Detached and (Pages[1].Parent = Host) and (Pages[1].Align = alClient) and
      (Tabs.Tabs[Tabs.TabIndex].ID = SelectedID) and (Edit.Text = 'Retain this content'), 'Closing detached window reattaches and preserves content');
    R := Tabs.CloseButtonBounds(Items[0].Index);
    Tabs.PressAt((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
    Check(Tabs.Tabs.Count = 3, 'Close waits for release');
    Tabs.ReleaseAt(Tabs.Width + 5, Tabs.Height div 2);
    Check(Tabs.Tabs.Count = 3, 'Release away cancels close');
    Counter.VetoClose := True;
    Tabs.CloseTab(Items[0].Index);
    Check((Tabs.Tabs.Count = 3) and (Counter.ClosedCount = 0), 'Close veto preserves tab');
    Counter.VetoClose := False;
    R := Tabs.CloseButtonBounds(Items[0].Index);
    Tabs.Perform(WM_LBUTTONDOWN, MK_LBUTTON,
      MakeLParam((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2));
    Check(Tabs.Tabs.Count = 3, 'Native close press waits for release');
    Tabs.Perform(WM_LBUTTONUP, 0,
      MakeLParam((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2));
    Check((Tabs.Tabs.Count = 2) and (Tabs.Tabs[Tabs.TabIndex].ID = SelectedID) and not Pages[0].Visible,
      'Closing another tab keeps selected document');
    Changes := Counter.Changes;
    Tabs.CloseTab(Items[1].Index);
    Check((Tabs.Tabs.Count = 1) and (Counter.Changes = Changes + 1) and Pages[2].Visible,
      'Closing selected tab notifies when replacement has same index');
    // Destroying a tabstrip must not destroy a Page owned by the host form.
    Check(Tabs.DetachTab(0, Point(220, 220)), 'Last tab can detach');
    Tabs.OnChange := nil; Tabs.OnClosed := nil;
    Tabs.Free;
    Application.ProcessMessages;
    Check((Pages[2].Parent = Host) and (Pages[2].Owner = Form), 'Destroying source restores detached content before freeing host');
    Stored := TERDTabs.Create(nil);
    try
      Stored.TabStyle := tsDocument; Stored.AllowReorder := True; Stored.AllowDetach := True;
      Stored.Tabs.Add.Caption := 'Temporary'; Stored.TabIndex := 0;
      Stored.CloseTab(0);
      Check((Stored.Tabs.Count = 0) and (Stored.TabIndex = -1), 'Closing last tab leaves a valid empty selection');
      Stored.Tabs.Add.Caption := 'Stored'; Stored.TabIndex := 0;
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDTabs(Stream.ReadComponent(nil));
    try Check(Copy.AllowReorder and Copy.AllowDetach and (Copy.TabIndex = 0) and
      (Copy.Tabs.Count = 1) and not Copy.Tabs[0].Detached, 'Tab DFM persists config and starts attached')
    finally Copy.Free end;
  finally Stream.Free; Form.Free; Counter.Free end;
end;

procedure TestEditPasswordAndValidation;
var Form: TForm; Edit: TEditProbe; Native: TERDTextInput; Validator: TValidationProbe;
  Copy, Stored: TERDEdit; Counter: TChangeProbe; Stream: TMemoryStream;
  Bitmap: TBitmap; BaseHeight, Calls: Integer;
begin
  RegisterClass(TERDEdit);
  Form := TForm.CreateNew(nil);
  Validator := TValidationProbe.Create;
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    Edit := TEditProbe.Create(Form);
    Edit.Parent := Form;
    Edit.Text := 'Example';
    Edit.PasswordChar := #8226;
    Edit.ShowPasswordButton := True;
    Edit.OnChange := Counter.Changed;
    Form.Show; Application.ProcessMessages; Edit.SetFocus;
    Edit.SelStart := 1; Edit.SelLength := 3;
    Native := Edit.Editor;
    Check(Native.PasswordChar = #8226, 'Configured password character is applied to native editor');
    Edit.RevealByKeyboard;
    Check(Edit.PasswordVisible and Native.PasswordVisible and
      (Edit.SelStart = 1) and (Edit.SelLength = 3) and (Edit.Text = 'Example') and
      (Counter.Changes = 0), 'Password reveal preserves text and selection without OnChange');
    Edit.RevealByKeyboard;
    Check(not Edit.PasswordVisible and (Native.PasswordChar = #8226), 'Keyboard hides password again');
    Edit.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Check(not Edit.PasswordVisible, 'Eye waits for matching release');
    Edit.Perform(WM_LBUTTONUP, 0, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Check(Edit.PasswordVisible and Native.PasswordVisible, 'Native mouse release reveals password');
    Edit.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Edit.Perform(WM_LBUTTONUP, 0, MakeLParam(4, Edit.Height div 2));
    Check(Edit.PasswordVisible, 'Release away cancels eye toggle');
    Edit.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Edit.Perform(WM_LBUTTONUP, 0, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Check(not Edit.PasswordVisible and (Native.PasswordChar = #8226), 'Eye hides password on next click');

    Check(Native.Left + Native.Width <= Edit.Width - 32, 'Native edit reserves room for reveal button');
    Edit.PasswordChar := #0;
    Edit.OnValidate := Validator.ValidateText;
    Edit.ValidateOnChange := True;
    BaseHeight := Edit.Height;
    Edit.Text := '';
    Check((Edit.ValidationState = evsError) and (Edit.ValidationText = 'Required') and
      (Edit.Height > BaseHeight), 'Validation error grows message below field');
    Calls := Validator.Calls;
    Check(not Edit.Validate and (Validator.Calls = Calls + 1), 'Validation reports failure and guards recursion');
    Edit.Text := 'abc';
    Check((Edit.ValidationState = evsWarning) and Edit.Validate, 'Warning is displayed but not invalid');
    Edit.Text := 'valid';
    Check((Edit.ValidationState = evsSuccess) and (Edit.ValidationText = '') and
      (Edit.Height = BaseHeight), 'Successful validation clears message and restores field height');
    Stored := TERDEdit.Create(nil);
    try
      Stored.PasswordChar := #8226; Stored.ShowPasswordButton := True;
      Stored.PasswordVisible := True; Stored.Text := 'Example';
      Stored.ValidationState := evsError; Stored.ValidationText := 'Example message';
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'Password/validation preview works without a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDEdit(Stream.ReadComponent(nil));
    try
      Check((Copy.PasswordChar = #8226) and Copy.ShowPasswordButton and not Copy.PasswordVisible and
        (Copy.ValidationState = evsError) and (Copy.ValidationText = 'Example message') and
        (Copy.Text = 'Example') and (Copy.ControlCount = 1), 'Edit DFM preserves config and masks visibility on reload');
    finally Copy.Free end;
  finally Bitmap.Free; Stream.Free; Form.Free; Validator.Free; Counter.Free end;
end;

procedure TestEditableCombo;
var Form: TForm; Combo, Copy: TERDComboBox; Counter: TChangeProbe;
  Popup: TERDPopupList; I: Integer; Stream: TMemoryStream;
begin
  RegisterClass(TERDComboBox);
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  try
    Combo := TERDComboBox.Create(Form);
    Combo.Parent := Form;
    Combo.Items.Add('Belgium'); Combo.Items.Add('Netherlands');
    Combo.Items.Add('Belgium HD'); Combo.Items.Add('France');
    Check((Combo.Style = ecsDropDownList) and Combo.AutoComplete, 'Legacy combo defaults preserved');
    Combo.Style := ecsDropDown;
    Combo.FilterMode := cfsContains;
    Combo.FilterText := 'BEL';
    Check((Combo.FilteredCount = 2) and (Combo.FilteredIndex(0) = 0) and
      (Combo.FilteredIndex(1) = 2), 'Filtering retains original item indices');
    Combo.FilterText := 'land';
    Check((Combo.FilteredCount = 1) and (Combo.FilteredIndex(0) = 1), 'Contains filter can match middle of text');
    Combo.FilterMode := cfsPrefix;
    Check(Combo.FilteredCount = 0, 'Prefix filter does not match middle');
    Combo.FilterText := 'bel';
    Combo.OnChange := Counter.Changed;
    Form.Show; Application.ProcessMessages;
    Combo.SetFocus;
    Combo.DropDown;
    Popup := nil;
    for I := 0 to Combo.ComponentCount - 1 do if Combo.Components[I] is TERDPopupList then
      Popup := TERDPopupList(Combo.Components[I]);
    Check((Popup <> nil) and Popup.IsOpen and (Popup.Items.Count = 2), 'Editable combo opens filtered popup');
    Popup.ItemIndex := 1;
    Popup.CloseUp(True);
    Check((Combo.ItemIndex = 2) and (Combo.Text = 'Belgium HD') and (Counter.Changes = 1),
      'Filtered popup commits original index exactly once');
    Combo.FilterMode := cfsNone;
    Combo.Text := '';
    Combo.Editor.Perform(WM_CHAR, Ord('B'), 0);
    Check((Combo.Text = 'Belgium') and (Combo.Editor.SelLength = 6), 'Native typing completes and selects suffix');
    Combo.Editor.Perform(WM_CHAR, Ord('e'), 0);
    Check((Combo.Text = 'Belgium') and (Combo.Editor.SelLength = 5), 'Next character replaces completion suffix');
    Combo.Editor.Perform(WM_KEYDOWN, VK_BACK, 0);
    Combo.Editor.Perform(WM_CHAR, VK_BACK, 0);
    Check(Combo.Text = 'Be', 'Backspace removes suffix without immediately restoring it');
    Combo.Editor.SelStart := 1; Combo.Editor.SelLength := 1;
    Combo.Editor.Perform(WM_CLEAR, 0, 0);
    Check(Combo.Text = 'B', 'Native delete command is not undone by completion');
    Combo.Editor.Perform(WM_UNDO, 0, 0);
    Check(Combo.Text = 'Be', 'Native undo restores edit without completion');
    Combo.AutoComplete := False;
    Combo.Text := 'custom text';
    Check(Combo.ItemIndex = -1, 'Editable combo permits free text outside Items');
    Combo.OnChange := nil;
    Stream.WriteComponent(Combo);
    Stream.Position := 0;
    Copy := TERDComboBox(Stream.ReadComponent(nil));
    try
      Check((Copy.Style = ecsDropDown) and (Copy.Text = 'custom text') and (Copy.Items.Count = 4), 'Combo DFM preserves style and free text');
      Check(Copy.ControlCount = 1, 'Combo stream does not duplicate native editor');
    finally Copy.Free end;
  finally Stream.Free; Form.Free; Counter.Free end;
end;

procedure TestRating;
var Form: TForm; Rating: TRatingProbe; Copy, Stored: TERDRating;
  Counter: TChangeProbe; Bitmap: TBitmap; Stream: TMemoryStream; I: Integer;
begin
  RegisterClass(TERDRating);
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Bitmap := TBitmap.Create;
  Stream := TMemoryStream.Create;
  try
    Rating := TRatingProbe.Create(Form);
    Rating.Parent := Form;
    Rating.OnChange := Counter.Changed;
    Check((Rating.MaxStars = 5) and not Rating.AllowHalf and Rating.AllowClear and (Rating.Value = 0), 'Rating defaults');
    Rating.Value := 3.7;
    Check(Rating.Value = 4, 'Whole rating rounds to nearest integer');
    Rating.AllowHalf := True;
    Rating.Value := 3.7;
    Check(Rating.Value = 3.5, 'Half rating rounds to nearest half');
    Rating.Value := 20;
    Check(Rating.Value = 5, 'Rating clamps maximum');
    Rating.Value := -2;
    Check(Rating.Value = 0, 'Rating clamps minimum');
    Rating.PressKey(VK_RIGHT);
    Check(Rating.Value = 0.5, 'Keyboard advances a half step');
    Rating.PressKey(VK_END);
    Check(Rating.Value = 5, 'End chooses maximum rating');
    Rating.PressKey(VK_HOME);
    Check(Rating.Value = 0, 'Home clears when allowed');
    Rating.AllowClear := False;
    Rating.PressKey(VK_HOME);
    Check(Rating.Value = 0.5, 'Home respects minimum nonzero value when clearing disabled');
    Rating.Value := 0;
    I := Counter.Changes;
    Rating.PressKey(VK_DELETE);
    Check((Rating.Value = 0) and (Counter.Changes = I), 'Delete has no effect when clearing disabled');
    Rating.AllowClear := True;
    Form.Show; Application.ProcessMessages;
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(8, Rating.Height div 2);
    Check(Rating.Value = 0.5, 'Left half of first star selects half value');
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(8, Rating.Height div 2);
    Check(Rating.Value = 0, 'Clicking selected value clears rating');
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(Rating.Width + 2, Rating.Height div 2);
    Check(Rating.Value = 0, 'Releasing outside cancels rating selection');
    Rating.PreviewAt(32, Rating.Height div 2);
    Check((Rating.PreviewValue > 0) and (Rating.Value = 0), 'Hover previews without changing stored rating');
    Rating.ReadOnly := True;
    I := Counter.Changes;
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(8, Rating.Height div 2);
    Rating.PressKey(VK_END);
    Check(Counter.Changes = I, 'Readonly blocks mouse and keyboard changes');
    Rating.Value := 4.5;
    Rating.MaxStars := 3;
    Check(Rating.Value = 3, 'Changing star count clamps existing value');
    Stored := TERDRating.Create(nil);
    try
      Stored.AllowHalf := True; Stored.Value := 4.5; Stored.ReadOnly := True;
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'Filled and half-filled stars render without a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDRating(Stream.ReadComponent(nil));
    try Check(Copy.AllowHalf and Copy.ReadOnly and (Copy.Value = 4.5), 'Rating DFM roundtrip')
    finally Copy.Free end;
  finally Bitmap.Free; Stream.Free; Form.Free; Counter.Free end;
end;

procedure TPalettePopupProbe.InspectPopup(Sender: TObject);
var Window: TCustomForm; I: Integer; Search: TERDSearchEdit;
begin
  Window := Screen.ActiveCustomForm;
  Inc(Attempts);
  if (Window = nil) or (Window = OwnerForm) then
  begin
    if Attempts >= 40 then begin Timer.Enabled := False; PostQuitMessage(1) end;
    Exit;
  end;
  Timer.Enabled := False;
  SawPopup := TForm(Window).BorderStyle = bsNone;
  OwnerEnabled := IsWindowEnabled(OwnerForm.Handle);
  TopAnchored := Window.Top < OwnerForm.Top + OwnerForm.Height div 3;
  NoButtons := True;
  Search := nil;
  for I := 0 to Window.ControlCount - 1 do
  begin
    if Window.Controls[I] is TERDButton then NoButtons := False;
    if Window.Controls[I] is TERDSearchEdit then Search := TERDSearchEdit(Window.Controls[I]);
  end;
  if Search <> nil then
  begin
    Search.Text := 'playlist';
    SendMessage(GetFocus, WM_KEYDOWN, VK_RETURN, 0);
    if TForm(Window).ModalResult = mrNone then TForm(Window).ModalResult := mrCancel;
  end
  else TForm(Window).ModalResult := mrCancel;
end;

procedure TestCommandPalettePopup;
var Form: TForm; Palette: TERDCommandPalette; Probe: TPalettePopupProbe;
  Counter: TCommandCounter;
begin
  Form := TForm.CreateNew(nil);
  Probe := TPalettePopupProbe.Create;
  Counter := TCommandCounter.Create;
  try
    Form.SetBounds(100, 100, 800, 600);
    Palette := TERDCommandPalette.Create(Form);
    Palette.OnExecute := Counter.Executed;
    Palette.Commands.Add.Caption := 'Open playlist';
    Palette.Commands[0].Key := 'open';
    Probe.OwnerForm := Form;
    Probe.Timer := TTimer.Create(Form);
    Probe.Timer.Enabled := False;
    Probe.Timer.Interval := 50;
    Probe.Timer.OnTimer := Probe.InspectPopup;
    Form.Show;
    Probe.Timer.Enabled := True;
    Check(Palette.Execute and (Counter.Count = 1), 'Palette search Enter executes after popup closes');
    Check(Probe.SawPopup and Probe.OwnerEnabled and Probe.NoButtons and Probe.TopAnchored,
      'Command palette is a top-anchored popup without dialog buttons or disabled owner');
  finally Form.Free; Probe.Free; Counter.Free end;
end;

procedure TestCommandPalette;
var Palette, Copy: TERDCommandPalette; First, Other: TERDCommandItem;
  Counter: TCommandCounter; ActionCounter: TChangeProbe; Action: TAction;
  IDs: TArray<Integer>; SavedID: Integer; Stream: TMemoryStream;
begin
  RegisterClass(TERDCommandPalette);
  Palette := TERDCommandPalette.Create(nil);
  Counter := TCommandCounter.Create;
  ActionCounter := TChangeProbe.Create;
  Action := TAction.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Palette.OnExecute := Counter.Executed;
    First := Palette.Commands.Add;
    First.Key := 'open'; First.Caption := 'Open playlist'; First.Category := 'Files';
    First.Keywords := 'import m3u'; First.ShortcutText := 'Ctrl+O';
    Other := Palette.Commands.Add;
    Other.Key := 'tv'; Other.Caption := 'Play channel'; Other.Category := 'Live TV'; Other.Keywords := 'Belgium stream';
    IDs := Palette.Search('M3U files');
    Check((Length(IDs) = 1) and (IDs[0] = First.ID), 'Command search matches all case-insensitive words across fields');
    IDs := Palette.Search('Belgium live');
    Check((Length(IDs) = 1) and (IDs[0] = Other.ID), 'Command search covers category and keywords');
    Check(Length(Palette.Search('missing')) = 0, 'Unknown search gives no results');
    Other.Enabled := False;
    Check(Length(Palette.Search('')) = 1, 'Disabled commands omitted from palette');
    Check(not Palette.ExecuteCommand(Other.ID), 'Disabled commands cannot execute');
    First.Visible := False;
    Check(Length(Palette.Search('')) = 0, 'Hidden commands omitted');
    First.Visible := True;
    Check(Palette.ExecuteCommand(First.ID) and (Counter.Count = 1) and (Counter.LastKey = 'open') and
      (Palette.LastCommand = 'open'), 'Command without Action invokes OnExecute');
    Action.Caption := '&Save file'; Action.ShortCut := ShortCut(Ord('S'), [ssCtrl]);
    Action.OnExecute := ActionCounter.Changed;
    Other.Enabled := True; Other.Caption := ''; Other.Action := Action;
    Check((Other.DisplayCaption = 'Save file') and (Other.DisplayShortcut <> ''), 'Action supplies missing caption and shortcut');
    Check(Palette.ExecuteCommand(Other.ID) and (ActionCounter.Changes = 1) and (Counter.Count = 1),
      'Action-backed commands execute Action without duplicate OnExecute');
    Action.Enabled := False;
    Check(not Palette.ExecuteCommand(Other.ID), 'Disabled Action cannot execute');
    Action.Free; Action := nil;
    Check(Other.Action = nil, 'FreeNotification clears action reference');
    SavedID := First.ID;
    First.Index := 1;
    Check(Palette.ExecuteCommand(SavedID), 'Collection ID survives reordering');
    First.Free;
    Check(not Palette.ExecuteCommand(SavedID), 'Deleted command ID cannot execute another item');
    Palette.OnExecute := nil;
    Palette.Commands[0].Caption := 'Stored command';
    Palette.Commands[0].Icon.BuiltIn := ei028Info;
    Stream.WriteComponent(Palette);
    Stream.Position := 0;
    Copy := TERDCommandPalette(Stream.ReadComponent(nil));
    try
      Check((Copy.Commands.Count = 1) and (Copy.Commands[0].Key = 'tv') and
        (Copy.Commands[0].Icon.BuiltIn = ei028Info), 'Command collection and icon survive DFM streaming');
    finally Copy.Free end;
  finally Stream.Free; Action.Free; Palette.Free; Counter.Free; ActionCounter.Free end;
end;

procedure TestDropZone;
var Zone: TERDDropZone; Counter: TDropCounter; Files, Accepted: TStringList;
  Root, First, Second, Other: string; Id: TGUID; Bitmap: TBitmap;
begin
  CreateGUID(Id);
  Root := TPath.Combine(TPath.GetTempPath, 'ERD-drop-' + GUIDToString(Id));
  CreateDir(Root);
  First := TPath.Combine(Root, 'één.m3u');
  Second := TPath.Combine(Root, 'UPPER.M3U8');
  Other := TPath.Combine(Root, 'notes.txt');
  Zone := TERDDropZone.Create(nil);
  Counter := TDropCounter.Create;
  Files := TStringList.Create;
  Accepted := TStringList.Create;
  Bitmap := TBitmap.Create;
  try
    TFile.WriteAllText(First, '#EXTM3U');
    TFile.WriteAllText(Second, '#EXTM3U');
    TFile.WriteAllText(Other, 'notes');
    Zone.FileMasks := '*.m3u;*.m3u8';
    Zone.OnFilesDropped := Counter.Dropped;
    Check(Zone.AcceptsFile(First) and Zone.AcceptsFile(Second), 'Unicode paths and case-insensitive masks are accepted');
    Check(not Zone.AcceptsFile(Other) and not Zone.AcceptsFile(Root) and not Zone.AcceptsFile(Other + '.missing'),
      'Wrong mask, directories and missing files are rejected');
    Files.Add(Other); Files.Add(First); Files.Add(Second);
    Check(Zone.DropFiles(Files) and (Counter.FileCount = 2) and (Counter.FirstFile = First),
      'Drop delivers only matching files in source order');
    Check(Files.Count = 3, 'Source list remains owned and unchanged by caller');
    Zone.AllowMultiple := False;
    Check(not Zone.DropFiles(Files) and (Counter.Count = 1), 'Multiple matching files rejected in single mode');
    Files.Delete(2);
    Check(Zone.DropFiles(Files) and (Counter.FileCount = 1), 'Single mode accepts one remaining filtered file');
    Zone.AcceptDrops := False;
    Check(not Zone.DropFiles(Files), 'AcceptDrops false blocks delivery');
    Zone.AcceptDrops := True; Zone.Enabled := False;
    Check(not Zone.DropFiles(Files), 'Disabled drop zone blocks delivery');
    Zone.Enabled := True; Zone.AllowDirectories := True;
    Files.Clear; Files.Add(Root);
    Check(Zone.FilterFiles(Files, Accepted) and (Accepted[0] = Root), 'Opt-in directory drops are supported');
    Zone.RenderTo(Bitmap);
    Check(not Zone.HandleAllocated, 'DropZone preview does not initialize an OLE window');
  finally
    Bitmap.Free; Zone.Free; Files.Free; Accepted.Free; Counter.Free;
    TDirectory.Delete(Root, True);
  end;
end;

procedure TestLoadingIndicator;
var Form: TForm; Loading: TLoadingProbe; Stored, Copy: TERDLoadingIndicator;
  Bitmap: TBitmap; Stream: TMemoryStream; I, Frame: Integer; Style: TERDLoadingIndicatorStyle;
begin
  RegisterClass(TERDLoadingIndicator);
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
  Stream := TMemoryStream.Create;
  try
    Stored := TERDLoadingIndicator.Create(nil);
    try
      Check(Stored.Active and Stored.RespectAnimations and not Stored.IsAnimating,
        'Loading defaults: active, respects OS animation, no timer before showing');
      Stored.Caption := 'Loading';
      Stored.Style := lisBars;
      Stored.IndicatorSize := 48;
      Stored.AnimationInterval := 120;
      Stored.Active := False;
      Stored.RespectAnimations := False;
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'Loading indicator renders without a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDLoadingIndicator(Stream.ReadComponent(nil));
    try
      Check(not Copy.Active and not Copy.RespectAnimations and (Copy.Caption = 'Loading') and
        (Copy.Style = lisBars) and (Copy.IndicatorSize = 48) and (Copy.AnimationInterval = 120),
        'Loading indicator DFM stores active, animation option and caption');
    finally Copy.Free end;
    Loading := TLoadingProbe.Create(Form);
    Loading.Parent := Form;
    Loading.RespectAnimations := False;
    Loading.Caption := '';
    for Style := Low(TERDLoadingIndicatorStyle) to High(TERDLoadingIndicatorStyle) do
    begin
      Loading.Style := Style;
      for I := 1 to 4 do
      begin
        Loading.IndicatorSize := I * 16;
        Loading.RenderTo(Bitmap);
        Check((Loading.Width = Loading.Height) and (Bitmap.Width = Loading.Width),
          'Every loader style supports autosized 16/32/48/64 pixel boxes');
      end;
    end;
    Loading.Style := lisSpinner;
    Loading.IndicatorSize := 32;
    for I := 1 to 12 do Loading.NextFrame;
    Check(Loading.FrameIndex = 0, 'Spinner wraps after twelve frames');
    Loading.Active := False;
    Loading.NextFrame;
    Check(Loading.FrameIndex = 0, 'Inactive spinner does not advance');
    Loading.Active := True;
    Loading.Enabled := False;
    Loading.NextFrame;
    Check(Loading.FrameIndex = 0, 'Disabled spinner does not advance');
    Loading.Enabled := True;
    Form.Show;
    Application.ProcessMessages;
    Check(Loading.IsAnimating, 'Visible active spinner starts its timer');
    Loading.Visible := False;
    Application.ProcessMessages;
    Check(not Loading.IsAnimating, 'Hiding spinner stops timer');
    Loading.Visible := True;
    Application.ProcessMessages;
    Check(Loading.IsAnimating, 'Showing spinner resumes timer');
    Frame := Loading.FrameIndex;
    Loading.Active := False;
    Application.ProcessMessages;
    Check(not Loading.IsAnimating and (Loading.FrameIndex = Frame), 'Stopping preserves frame and disables timer');
    Loading.Active := True;
    Form.Hide;
    Application.ProcessMessages;
    Check(not Loading.IsAnimating, 'Hiding parent form stops spinner timer');
  finally Bitmap.Free; Stream.Free; Form.Free end;
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

procedure TestSplitMenuGeometry;
var Form: TForm; Theme: TERDTheme; Split: TERDSplitButton; Menu: TERDPopupMenu;
  Popup: TWinControl; Item: TERDMenuItem; Bounds: TRect; Anchor: TPoint;
  Bitmap: TBitmap; M, Y, FirstHot, LastHot: Integer; Fill: TColor;
begin
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
  try
    Form.SetBounds(100, 100, 500, 300);
    Theme := TERDTheme.Create(Form);
    Split := TERDSplitButton.Create(Form); Split.Parent := Form; Split.Theme := Theme;
    Split.AutoSize := False; Split.SetBounds(40, 40, 120, 40);
    Menu := TERDPopupMenu.Create(Form); Menu.Theme := Theme;
    Item := TERDMenuItem.Create(Menu); Item.Caption := 'Show dialog'; Menu.Items.Add(Item);
    Split.DropDownMenu := Menu;
    Form.Show; Application.ProcessMessages;
    Split.DropDown;
    Check(Split.ControlCount = 1, 'Split button owns one visible themed popup window');
    Popup := TWinControl(Split.Controls[0]);
    M := MulDiv(4, Split.CurrentPPI, 96);
    Anchor := Split.ClientToScreen(Point(M, Split.Height - M));
    GetWindowRect(Popup.Handle, Bounds);
    Check((Bounds.Left = Anchor.X) and (Bounds.Top = Anchor.Y),
      'Popup aligns with painted button left and bottom edges');
    Popup.Perform(WM_MOUSEMOVE, 0, MakeLParam(M + 2, M + 6));
    Bitmap.SetSize(Popup.Width, Popup.Height);
    Popup.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Fill := Bitmap.Canvas.Pixels[M + 2, Popup.Height div 2];
    FirstHot := -1; LastHot := -1;
    for Y := 1 to Popup.Height - 2 do
      if Bitmap.Canvas.Pixels[M + 2, Y] = Fill then
      begin
        if FirstHot < 0 then FirstHot := Y;
        LastHot := Y;
      end;
    Check((FirstHot > 0) and (FirstHot = Popup.Height - 1 - LastHot),
      'Single-item popup has balanced top and bottom row padding');
  finally ERDCloseThemedMenu; Bitmap.Free; Form.Free end;
end;

procedure TestFeedbackRegressions;
var Form: TForm; Values: TERDKeyValueList; Tags: TERDTagEdit;
  Token: TERDTagToken; Ribbon: TERDRibbon; Group: TERDRibbonGroup;
  First, Second, Independent: TERDRibbonItem; Y, I: Integer;
  FoundFirst, FoundSecond: Boolean;
  Bitmap: TBitmap; Normal, Hot, Down: TColor; TokenWidth: Integer;
begin
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
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
    TokenWidth := Token.Width;
    Tags.CloseSpacing := 10;
    Check((Token.Width = TokenWidth + 6) and (Token.CloseSpacing = 10),
      'Close spacing relayouts existing tokens without recreating them');
    Tags.CloseSpacing := 4;
    Tags.TagColorStrength := 40;
    Check(Token.TagColorStrength = 40, 'Appearance changes propagate to existing tags');
    Token.RenderTo(Bitmap);
    Normal := Bitmap.Canvas.Pixels[Token.Width - 8, Token.Height div 2 + 5];
    Token.Perform(WM_MOUSEMOVE, 0, MakeLParam(Token.Width - 12, Token.Height div 2));
    Token.RenderTo(Bitmap);
    Hot := Bitmap.Canvas.Pixels[Token.Width - 8, Token.Height div 2 + 5];
    Token.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Token.Width - 12, Token.Height div 2));
    Token.RenderTo(Bitmap);
    Down := Bitmap.Canvas.Pixels[Token.Width - 8, Token.Height div 2 + 5];
    Check((Normal <> Hot) and (Hot <> Down), 'Tag close button paints distinct hover and pressed states');
    Token.Perform(WM_LBUTTONUP, 0, MakeLParam(4, Token.Height div 2));
    Check(Tags.Tags.Count = 1, 'Releasing away from close icon does not delete tag');

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
  finally Bitmap.Free; Form.Free end;
end;


procedure TEditKeyFilter.Filter(Sender: TObject; var Key: Char);
begin
  if Key = #8 then begin Inc(Calls); Key := #0; end;
end;

function TTextInputProbe.EditMenu: TERDPopupMenu;
begin Result := DefaultEditMenu; end;
procedure TestTextInputEditMenu;
var Form: TForm; Input: TTextInputProbe; Menu: TERDPopupMenu;
begin
  Form := TForm.Create(nil);
  try
    Input := TTextInputProbe.Create(Form); Input.Parent := Form;
    Input.Text := 'Unicode ' + #$D83D#$DD12; Input.SelectAll;
    Menu := Input.EditMenu;
    Check(Menu.Items.Count = 9, 'Themed input menu contains editing commands and separators');
    Check(Menu.Items[3].Enabled and Menu.Items[4].Enabled, 'Selection enables Cut and Copy');
    Menu.Items[4].Click; Check(Clipboard.AsText = Input.Text, 'Menu copies Unicode text');
    Input.ReadOnly := True; Menu := Input.EditMenu;
    Check(Menu.Items[4].Enabled and not Menu.Items[3].Enabled and not Menu.Items[6].Enabled,
      'ReadOnly menu permits Copy but prevents modification');
    Input.ReadOnly := False; Input.PasswordChar := #$2022; Menu := Input.EditMenu;
    Check(not Menu.Items[3].Enabled and not Menu.Items[4].Enabled, 'Password menu disables Cut and Copy');
    Input.PasswordChar := #0; Input.SelectAll; Menu := Input.EditMenu;
    Menu.Items[6].Click; Check(Input.Text = '', 'Menu Delete removes selection');
    Menu := Input.EditMenu; Check(Menu.Items[0].Enabled, 'Menu Undo enabled after deletion');
    Menu.Items[0].Click; Check(Input.Text <> '', 'Menu Undo restores Unicode text');
    Menu := Input.EditMenu; Check(Menu.Items[1].Enabled, 'Menu Redo enabled after undo');
  finally Form.Free; end;
end;

procedure CheckGraphemes(const S: UnicodeString; const Expected: array of Integer);
var Actual: TArray<Integer>; I: Integer;
begin
  Actual := ERDGraphemeBoundaries(S);
  Check(Length(Actual) = Length(Expected), 'Unicode grapheme boundary count');
  for I := 0 to High(Expected) do
    Check(Actual[I] = Expected[I], 'Unicode grapheme UTF-16 boundary');
end;
procedure TestUnicodeTextModel;
var M: TERDTextModel; Family, Flag, Tone, Accent: UnicodeString;
begin
  {$I GraphemeConformance.inc}
  Family := #$D83D#$DC69#$200D#$D83D#$DC69#$200D#$D83D#$DC67;
  Flag := #$D83C#$DDE7#$D83C#$DDEA;
  Tone := #$D83D#$DC4D#$D83C#$DFFD;
  Accent := 'e' + #$0301;
  M := TERDTextModel.Create;
  try
    M.SetText('A' + Accent + Flag + Tone + Family);
    Check(M.Previous(Length(M.Text)) = 1 + Length(Accent + Flag + Tone), 'Family moves as one grapheme');
    M.DeleteBackward;
    Check(M.Text = 'A' + Accent + Flag + Tone, 'Backspace removes whole family');
    M.Undo; Check(M.Text = 'A' + Accent + Flag + Tone + Family, 'Unicode undo restores family');
    M.Redo; M.DeleteBackward;
    Check(M.Text = 'A' + Accent + Flag, 'Backspace removes skin tone with emoji');
    M.Select(1 + Length(Accent), 1 + Length(Accent)); M.DeleteForward;
    Check(M.Text = 'A' + Accent, 'Delete removes entire flag');
    M.Select(2, 2); Check(M.Caret = 1, 'Selection cannot split combining sequence');
    M.Select(Length(M.Text), Length(M.Text)); M.ReplaceSelection(Family, Length(M.Text) + 2);
    Check(M.Text = 'A' + Accent, 'MaxLength cannot insert partial emoji family');
    M.SetText('A' + #$D800 + 'B');
    Check(M.Text = 'A' + #$FFFD + 'B', 'Invalid surrogate replaced without corrupting UTF-16');
  finally M.Free; end;
end;
procedure TestWindowsEditInteractionParity;
var Form: TForm; Edit: TERDEdit; Reference: TEdit; P: TPoint; Filter: TEditKeyFilter;
    X, Y, ReferenceStart, ReferenceLength: Integer;
  procedure CheckState(const Action: string);
  begin
    Check((Edit.Text = Reference.Text) and (Edit.SelStart = Reference.SelStart) and
      (Edit.SelLength = Reference.SelLength), 'Windows edit parity: ' + Action);
  end;
  procedure Reset;
  begin
    Edit.Text := 'one two three'; Reference.Text := Edit.Text;
    Edit.SelStart := 6; Reference.SelStart := 6;
    Edit.SelLength := 0; Reference.SelLength := 0;
  end;
  procedure Key(KeyCode: Word);
  begin Edit.Editor.Perform(WM_KEYDOWN, KeyCode, 0); Reference.Perform(WM_KEYDOWN, KeyCode, 0); end;
  procedure DoubleClick(Control: TWinControl; AtX, AtY: Integer);
  begin
    Control.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(AtX, AtY));
    Control.Perform(WM_LBUTTONUP, 0, MakeLParam(AtX, AtY));
    Control.Perform(WM_LBUTTONDBLCLK, MK_LBUTTON, MakeLParam(AtX, AtY));
    Control.Perform(WM_LBUTTONUP, 0, MakeLParam(AtX, AtY));
  end;
begin
  Form := TForm.Create(nil); Filter := TEditKeyFilter.Create;
  try
    Edit := TERDEdit.Create(Form); Edit.Parent := Form; Edit.Width := 300;
    // Native edit exists only as the Windows behavioral reference in this test.
    Reference := TEdit.Create(Form); Reference.Parent := Form;
    Reference.SetBounds(0, 50, 300, 28); Reference.BorderStyle := bsNone;
    Reference.AutoSize := False; Reference.Font.Assign(Edit.Editor.Font);
    Form.Show; Reset;
    Key(VK_LEFT); CheckState('Left'); Key(VK_RIGHT); CheckState('Right');
    Key(VK_HOME); CheckState('Home'); Key(VK_END); CheckState('End');
    Reset; Edit.Editor.Perform(EM_SETSEL, 1, 4); Reference.Perform(EM_SETSEL, 1, 4);
    Edit.Editor.Perform(WM_CHAR, Ord('Z'), 0); Reference.Perform(WM_CHAR, Ord('Z'), 0);
    CheckState('typing replaces selection');
    Check(Edit.Editor.Modified = Reference.Modified, 'Modified flag matches native typing');
    Check(Edit.Editor.CanUndo, 'Edit exposes CanUndo after replacement');
    Key(VK_BACK); Edit.Editor.Perform(WM_CHAR, 8, 0); Reference.Perform(WM_CHAR, 8, 0);
    CheckState('Backspace'); Key(VK_DELETE); CheckState('Delete');
    Reset; Reference.SetFocus; Reference.SelStart := 6; Reference.SelLength := 0;
    Check(GetCaretPos(P), 'Native reference caret available');
    DoubleClick(Reference, P.X + 1, P.Y + 4);
    ReferenceStart := Reference.SelStart; ReferenceLength := Reference.SelLength;
    Edit.SetFocus; Edit.SelStart := 6; Edit.SelLength := 0;
    Check(GetCaretPos(P), 'ERD caret available for double-click test');
    X := P.X + 1; Y := P.Y + 4;
    DoubleClick(Edit.Editor, X, Y);
    Check((Edit.SelStart = ReferenceStart) and (Edit.SelLength = ReferenceLength),
      'Double-click selects same word range as native Windows edit');
    Check(Edit.SelLength > 0, 'Second mouse-down does not collapse double-click selection');
    Check(Edit.Editor.SelText = 'two', 'Double-click excludes trailing spaces and adjacent words');
    Edit.ReadOnly := True;
    Edit.Editor.Perform(WM_CHAR, Ord('X'), 0);
    Check(Edit.Text = 'one two three', 'ReadOnly edit remains selectable but blocks typing');
    Edit.ReadOnly := False; Edit.Editor.ClearUndo;
    Check(not Edit.Editor.CanUndo and not Edit.Editor.CanRedo, 'ClearUndo clears both histories');
    Edit.Text := 'reset'; Reference.Text := 'reset';
    Check(not Edit.Editor.Modified and not Reference.Modified, 'Programmatic Text resets Modified');
    Reset; Edit.OnKeyPress := Filter.Filter; Reference.OnKeyPress := Filter.Filter;
    Key(VK_BACK); Edit.Editor.Perform(WM_CHAR, 8, 0); Reference.Perform(WM_CHAR, 8, 0);
    CheckState('OnKeyPress can reject Backspace');
    Check((Filter.Calls = 2) and (Edit.Text = 'one two three'), 'Both controls dispatch cancellable Backspace KeyPress');
    Edit.OnKeyPress := nil; Reference.OnKeyPress := nil;
    Edit.NumbersOnly := True; Edit.Text := '123'; Edit.SelectAll;
    Edit.Editor.Perform(WM_CHAR, Ord('A'), 0);
    Check(Edit.Text = '123', 'Rejected numeric input does not delete selection');
  finally Form.Free; Filter.Free; end;
end;

procedure TestCodeLanguageServices;
var Form: TForm; Editor, Temporary: TERDCodeEditor; Service: TLanguageServiceProbe;
    ServiceReference: IERDCodeLanguageService; Sink: IERDLanguageResponses;
    Old, Current: TERDLanguageRequest; Items: TArray<TERDCompletionItem>;
    Target: TERDDefinitionTarget; Version: Int64; Worker: TThread; OffThreadAccepted: Boolean;
begin
  Form := TForm.Create(nil); Service := TLanguageServiceProbe.Create; ServiceReference := Service;
  try
    Editor := TERDCodeEditor.Create(Form); Editor.Parent := Form;
    Editor.LanguageService := ServiceReference; Editor.DocumentUri := 'memory:///test.ts';
    Editor.Text := 'con'; Editor.SelStart := 3;
    Check(Editor.RequestCompletion, 'Language provider receives completion request');
    Old := Service.Snapshot; Sink := Service.Responses;
    Check((Old.Text = 'con') and (Old.Caret = 3) and (Old.DocumentUri = Editor.DocumentUri), 'Language request contains full source snapshot and URI');
    Editor.RequestCompletion; Current := Service.Snapshot;
    SetLength(Items, 1); Items[0].Start := 0; Items[0].Count := 3;
    Items[0].LabelText := 'console'; Items[0].InsertText := 'console';
    Check(not Sink.Completion(Old, Items) and (Service.Cancels > 0), 'Superseded request is cancelled and rejected');
    Check(Sink.Completion(Current, Items), 'Newest completion result accepted');
    Items[0].InsertText := 'mutated';
    Check(Editor.Completions[0].InsertText = 'console', 'Completion results are copied');
    Check(Editor.ApplyCompletion(0) and (Editor.Text = 'console'), 'Completion replaces provider range');
    Editor.Undo; Check((Editor.Text = 'con') and (Editor.SelStart = 3), 'Completion undo restores source and original caret');
    Editor.RequestHover; Old := Service.Snapshot; Version := Editor.DocumentVersion;
    Editor.ReplaceSelection('x'); Editor.Undo;
    Check((Editor.Text = Old.Text) and (Editor.DocumentVersion > Version) and not Sink.Hover(Old, 'stale'),
      'Edit then Undo still rejects old result for identical source');
    Editor.RequestHover; Old := Service.Snapshot; Editor.SelStart := 0; Editor.SelStart := 3;
    Check(not Sink.Hover(Old, 'stale caret'), 'Caret moving away and back invalidates old request');
    Editor.RequestHover; Current := Service.Snapshot;
    OffThreadAccepted := True;
    Worker := TThread.CreateAnonymousThread(procedure begin OffThreadAccepted := Sink.Hover(Current, 'wrong thread'); end);
    Worker.FreeOnTerminate := False;
    try Worker.Start; Worker.WaitFor; finally Worker.Free; end;
    Check(not OffThreadAccepted, 'Provider response on worker thread rejected without touching VCL');
    Check(Sink.Hover(Current, 'console documentation') and (Editor.HoverText = 'console documentation'), 'Hover result is available to host');
    Editor.RequestHover; Old := Service.Snapshot; Editor.CancelLanguageRequests;
    Check(not Sink.Hover(Old, 'cancelled'), 'Explicit cancellation rejects delayed result');
    Editor.RequestCompletion; Current := Service.Snapshot; Items[0].InsertText := 'console';
    Sink.Completion(Current, Items); Editor.ReadOnly := True;
    Check(not Editor.ApplyCompletion(0) and (Editor.Text = 'con'), 'ReadOnly blocks completion application');
    Editor.ReadOnly := False; Editor.MaxLength := 3;
    Check(not Editor.ApplyCompletion(0), 'MaxLength rejects oversized completion'); Editor.MaxLength := 0;
    Editor.RequestDefinition; Current := Service.Snapshot; Target := Default(TERDDefinitionTarget);
    Target.Found := True; Target.Start := 0; Target.Count := 3;
    Check(Sink.Definition(Current, Target) and (Editor.SelStart = 0) and (Editor.SelLength = 3), 'Same-document definition selects target');
    Editor.RequestDefinition; Current := Service.Snapshot; Target := Default(TERDDefinitionTarget);
    Check(Sink.Definition(Current, Target), 'No-definition result completes request without navigation');
    Editor.Text := 'con'; Editor.SelStart := 3; Form.Show; Editor.SetFocus;
    Editor.RequestCompletion; Current := Service.Snapshot; Items[0].InsertText := 'console';
    Check(Sink.Completion(Current, Items), 'Focused completion result opens popup');
    Editor.Perform(WM_KEYDOWN, VK_RETURN, 0); Editor.Perform(WM_CHAR, 13, 0);
    Check(Editor.Text = 'console', 'Enter accepts completion without inserting newline');
    Editor.Undo; Check(Editor.Text = 'con', 'Keyboard completion remains one undo transaction');
    Editor.Text := 'console'; Editor.SelStart := 7; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check((Service.Requests = Version + 1) and (Service.Snapshot.TriggerCharacter = '.') and
      (Service.Snapshot.Text = 'console.') and (Service.Snapshot.Caret = 8), 'Dot requests member completion after insertion');
    Editor.Text := 'console?'; Editor.SelStart := 8; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check((Service.Requests = Version + 1) and (Service.Snapshot.Text = 'console?.'), 'Optional-chain dot requests member completion');
    Editor.Text := '// console'; Editor.SelStart := Length(Editor.Text); Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check(Service.Requests = Version, 'Dot in line comment does not trigger completion');
    Editor.Text := '"console"'; Editor.SelStart := 8; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check(Service.Requests = Version, 'Dot in string does not trigger completion');
    Editor.AutoMemberCompletion := False; Editor.Text := 'console'; Editor.SelStart := 7; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check(Service.Requests = Version, 'Automatic member completion can be disabled');
    Editor.AutoMemberCompletion := True; Editor.RequestCompletion;
    Check(Service.Snapshot.TriggerCharacter = #0, 'Manual completion has no trigger character');
    Temporary := TERDCodeEditor.Create(nil);
    try Temporary.LanguageService := ServiceReference; Temporary.RequestHover; Old := Service.Snapshot; Sink := Service.Responses;
    finally Temporary.Free; end;
    Check(not Sink.Hover(Old, 'late'), 'Retained response sink is safe after editor destruction');
  finally Form.Free; end;
end;

procedure TestCodeEditorSearch;
var Form: TForm; Editor: TCodeEditorProbe; Matches: TArray<TERDCodeSearchMatch>;
    Before: UnicodeString; Rows: Integer;
begin
  Form := TForm.Create(nil);
  try
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form; Editor.SetBounds(0, 0, 460, 180);
    Form.Show; Editor.SetFocus;
    Editor.Text := 'count Count counter $count';
    Check(Editor.SetSearch('count'), 'Literal search compiles'); Matches := Editor.SearchMatches;
    Check((Length(Matches) = 4) and (Matches[0].Start = 0) and (Matches[1].Start = 6),
      'Literal search reports zero-based UTF-16 positions');
    Check(Editor.SetSearch('count', [soWholeWord]) and (Length(Editor.SearchMatches) = 2),
      'Whole-word search excludes identifier suffix and dollar prefix');
    Editor.SetSearch('count', [soWholeWord, soMatchCase]);
    Check(Length(Editor.SearchMatches) = 1, 'Case-sensitive search filters uppercase variant');
    Editor.SetSearch('count'); Editor.SelStart := 0;
    Check(Editor.FindNext and (Editor.SelStart = 0) and (Editor.SelLength = 5), 'Find selects first match');
    Check(Editor.FindNext and (Editor.SelStart = 6), 'Find advances after selected match');
    Check(Editor.FindNext(True) and (Editor.SelStart = 0), 'Previous moves backwards');
    Check(Editor.FindNext(True) and (Editor.SelStart = 21), 'Previous wraps at document beginning');
    Matches := Editor.SearchMatches; Matches[0].Start := 999;
    Check(Editor.SearchMatches[0].Start = 0, 'Search results are copied');
    Editor.SetSearch('count', [], 6, 5);
    Check((Length(Editor.SearchMatches) = 1) and (Editor.SearchMatches[0].Start = 6), 'Selection-scoped search limits full matches');
    Editor.Text := 'a.b aXb'; Editor.SetSearch('a.b');
    Check(Length(Editor.SearchMatches) = 1, 'Literal search escapes regex metacharacters');
    Editor.SetSearch('a.b', [soRegex]);
    Check(Length(Editor.SearchMatches) = 2, 'Regex search recognises metacharacters');
    Before := Editor.Text;
    Check(not Editor.SetSearch('(', [soRegex]) and (Editor.SearchError <> '') and
      (Length(Editor.SearchMatches) = 0) and (Editor.Text = Before), 'Invalid regex reports error without editing document');
    Editor.Text := 'foo foo'; Editor.SelectAll; Editor.SetSearch('foo');
    Check(Editor.ReplaceAll('bar') = 2, 'Replace All reports replacement count');
    Check(Editor.Text = 'bar bar', 'Replace All changes all occurrences'); Editor.Undo;
    Check((Editor.Text = 'foo foo') and (Editor.SelLength = 7) and
      (Length(Editor.SearchMatches) = 2), 'Replace All undo restores text, selection and search results');
    Editor.SelLength := 0; Editor.SelStart := 0; Editor.FindNext;
    Check(Editor.ReplaceCurrent('longer') and (Editor.Text = 'longer foo'), 'Replace selected match');
    Editor.Undo; Editor.ReadOnly := True;
    Check((Editor.ReplaceAll('x') = 0) and not Editor.ReplaceCurrent('x') and (Editor.Text = 'foo foo'),
      'ReadOnly blocks search replacements'); Editor.ReadOnly := False;
    Editor.MaxLength := 7;
    Check((Editor.ReplaceAll('longer') = 0) and (Editor.Text = 'foo foo'), 'MaxLength rejects oversized replacement atomically');
    Editor.MaxLength := 0; Editor.Text := 'ab'; Editor.SetSearch('(?=.)', [soRegex]);
    Check((Editor.ReplaceAll('_') = 2) and (Editor.Text = '_a_b'), 'Zero-length regex matches replace without infinite loop');
    Editor.Text := #$D83D#$DC4D#$D83C#$DFFD; Editor.SetSearch(#$D83D#$DC4D);
    Check(Length(Editor.SearchMatches) = 0, 'Search cannot select part of an emoji grapheme');
    Editor.SetSearch(Editor.Text);
    Check((Length(Editor.SearchMatches) = 1) and (Editor.SearchMatches[0].Count = 4), 'Search matches complete emoji grapheme');
    Editor.Text := 'function run() {' + #13#10 + '  return true;' + #13#10 + '}';
    Editor.SelStart := 0; Rows := Editor.RowCount; Editor.ToggleFold(0);
    Editor.SetSearch('return'); Editor.FindNext;
    Check(Editor.RowCount = Rows, 'Finding hidden text expands its fold');
    Editor.ClearSearch; Check(Length(Editor.SearchMatches) = 0, 'Clear Search removes highlights');
  finally Form.Free; end;
end;

procedure TTextInputProbe.RebuildWindow;
begin RecreateWnd; end;

procedure TestSharedColorEmoji;
var Form: TForm; Input: TTextInputProbe; Area: TERDTextArea; Theme: TERDTheme;
    Bitmap: TBitmap; Layout: TERDTextLayoutLine; X, Y, L, R, CachedWidth: Integer;
  function ColorPixels: Integer;
  var X, Y: Integer; C: COLORREF;
  begin
    Result := 0;
    for Y := 4 to Bitmap.Height - 5 do
      for X := 7 to Bitmap.Width - 8 do
      begin
        C := ColorToRGB(Bitmap.Canvas.Pixels[X,Y]);
        if (GetRValue(C) > 150) and (GetGValue(C) > 160) and
          (GetBValue(C) < 100) then Inc(Result);
      end;
  end;
begin
  Check(ERDTextLayoutABIValid, 'DirectWrite and Direct2D ABI record sizes');
  Form := TForm.Create(nil); Bitmap := TBitmap.Create; Theme := TERDTheme.Create(nil);
  try
    Input := TTextInputProbe.Create(Form); Input.Parent := Form; Input.Theme := Theme;
    Input.SetBounds(8, 8, 220, 56); Input.Font.Size := 24;
    Input.Text := #$D83D#$DE00;
    Area := TERDTextArea.Create(Form); Area.Parent := Form; Area.Theme := Theme;
    Area.SetBounds(8, 72, 220, 80); Area.Font.Size := 24; Area.Text := Input.Text;
    Form.Show; Application.ProcessMessages;
    Check(IsWindow(Input.Handle), 'Shared Edit creates its window with emoji text');
    Input.RebuildWindow;
    Check(IsWindow(Input.Handle), 'Shared Edit recreates its window without a layout exception');
    Check(Input.Text = Area.Text, 'Window recreation preserves emoji text');
    Theme.Mode := tmERDesignsLight; Input.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Shared Edit base paints coloured emoji, not monochrome glyphs');
    Theme.Mode := tmERDesignsDark; Input.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Emoji intrinsic colours survive dark theme');
    Area.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Multiline base inherits colour emoji');
    Input.SetFocus; Input.SelectAll; Input.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Themed selection preserves emoji colours');
    Input.PasswordChar := #$2022; Input.RenderTo(Bitmap);
    // Exclude the selection's theme accent by clearing the selection first.
    Input.SelLength := 0; Input.RenderTo(Bitmap);
    Check(ColorPixels = 0, 'Password masking does not reveal coloured emoji');
    Input.PasswordChar := #0; Input.SelLength := 0;
    Bitmap.Canvas.Font.Assign(Input.Font);
    Layout := TERDTextLayoutLine.Create(Bitmap.Canvas, 'A' + #$D83D#$DE00);
    try
      X := Layout.X(1); Y := Layout.X(3); Layout.Range(1, 2, L, R);
      Check((Y > X) and (L <= X) and (R >= Y), 'Colour emoji caret and selection use the same layout');
      Check(Layout.Width >= Y, 'Layout width contains final emoji trailing edge');
      Check(Layout.Hit(Y + 20) = 3, 'Native hit testing stops at final emoji boundary');
      CachedWidth := Layout.Width;
    finally Layout.Free; end;
    Layout := TERDTextLayoutLine.Create(Bitmap.Canvas, 'A' + #$D83D#$DE00);
    try Check(Layout.Width = CachedWidth, 'Cached layout preserves shaped width'); finally Layout.Free; end;
    Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size * 2;
    Layout := TERDTextLayoutLine.Create(Bitmap.Canvas, 'A' + #$D83D#$DE00);
    try Check(Layout.Width > CachedWidth, 'Font size changes do not reuse stale cached layout'); finally Layout.Free; end;
  finally Theme.Free; Bitmap.Free; Form.Free; end;
end;

procedure TestSyntaxAndCodeEditor;
var Provider: IERDSyntaxProvider; Analysis: TERDSyntaxAnalysis; Span: TERDSyntaxSpan;
    Source, Original: UnicodeString; I, KeywordCount, Rows, Opening, Closing: Integer;
    Form: TForm; Editor: TCodeEditorProbe; Items: TArray<TERDCodeDiagnostic>;
    Diagnostic: TERDCodeDiagnostic; Stream: TMemoryStream; Restored: TERDCodeEditor;
  function TokenKind(const Token: string): TERDSyntaxKind;
  var Item: TERDSyntaxSpan; At: Integer;
  begin
    At := Pos(Token, Source) - 1; Result := skText;
    for Item in Analysis.Spans do
      if Item.Start = At then Exit(Item.Kind);
  end;
begin
  Provider := TERDBasicSyntaxProvider.Create(clTypeScript);
  Source := 'export interface Item {' + #13#10 +
    '  title: string; // } ignored' + #13#10 +
    '  value: number;' + #13#10 + '}' + #13#10 +
    'const pattern = /[{}]/g;' + #13#10 +
    'const text = `braces { }`;' + #13#10 + 'const sum = 1+2;';
  Analysis := Provider.Analyze(Source);
  Check(TokenKind('export') = skModuleKeyword, 'TypeScript module keyword highlighting');
  Check(TokenKind('interface') = skInterfaceKeyword, 'TypeScript interface keyword highlighting');
  Check(TokenKind('string') = skKeyword, 'TypeScript type highlighting');
  Check(TokenKind('//') = skComment, 'TypeScript line comments');
  Check(TokenKind('/[{}]/g') = skString, 'TypeScript regexp hides braces from folding');
  Check(TokenKind('`braces') = skString, 'TypeScript template string highlighting');
  Check(Length(Analysis.Folds) = 1, 'TypeScript folds ignore comments, regexps and strings');
  I := Pos('1+2', Source) - 1;
  for Span in Analysis.Spans do
    if Span.Start = I then Check((Span.Kind = skNumber) and (Span.Count = 1),
      'Numeric token stops before addition operator');
  Source := 'export class Example {}'; Analysis := Provider.Analyze(Source);
  Check(TokenKind('class') = skClassKeyword, 'TypeScript class keyword highlighting');
  Source := 'Const const'; Analysis := Provider.Analyze(Source); KeywordCount := 0;
  for Span in Analysis.Spans do if Span.Kind = skKeyword then Inc(KeywordCount);
  Check(KeywordCount = 1, 'TypeScript keywords are case sensitive');
  Provider := TERDBasicSyntaxProvider.Create(clPascal);
  Source := 'BEGIN' + #13#10 + '  S := ''end''; // end' + #13#10 + 'END;';
  Analysis := Provider.Analyze(Source);
  Check((TokenKind('BEGIN') = skKeyword) and (Length(Analysis.Folds) = 1),
    'Pascal provider ignores end inside comments and strings');
  Provider := TERDBasicSyntaxProvider.Create(clJSON);
  Source := '{' + #13#10 + ' "key": [true, 1]' + #13#10 + '}';
  Analysis := Provider.Analyze(Source);
  Check((TokenKind('true') = skKeyword) and (Length(Analysis.Folds) = 1), 'JSON provider folds multiline objects');

  RegisterClass(TERDCodeEditor); RegisterClass(TCodeEditorProbe); Form := TForm.Create(nil); Stream := TMemoryStream.Create;
  try
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form;
    Editor.SetBounds(8, 8, 480, 180); Form.Show; Editor.SetFocus;
    Original := 'function run() {' + #13#10 + '  return true;' + #13#10 + '}' + #13#10;
    Editor.Text := Original; Editor.SelStart := 0; Rows := Editor.RowCount;
    Editor.ShowFoldGuides := True;
    Check(Editor.FoldPixel(True) <> Editor.GutterBackground, 'Expanded fold has a visible connector');
    Editor.ShowFoldGuides := False;
    Check(Editor.FoldPixel(True) = Editor.GutterBackground, 'Guide property hides connector');
    Editor.ShowFoldGuides := True;
    Editor.FoldMouse(0); Check(Editor.Cursor = crArrow, 'Fold gutter uses the arrow cursor');
    Check(Editor.FoldPixel(False) <> Editor.GutterBackground, 'Fold hover paints a visible fill');
    Editor.FoldMouse(1); Check(Editor.RowCount = Rows, 'Fold waits for mouse release');
    Editor.FoldMouse(0, True); Editor.FoldMouse(2, True);
    Check(Editor.RowCount = Rows, 'Releasing outside cancels fold click');
    Editor.FoldMouse(0); Editor.FoldMouse(1); Editor.FoldMouse(2);
    Check((Editor.RowCount < Rows) and (Editor.Text = Original), 'Folding changes rows without changing document');
    Editor.SelStart := Pos('return', Original) - 1;
    Check(Editor.RowCount = Rows, 'Moving caret into hidden text expands fold');
    Editor.SelectAll; Editor.IndentSelection;
    Check(Editor.Text = '  function run() {' + #13#10 + '    return true;' + #13#10 + '  }' + #13#10,
      'Indent operates on selected logical lines');
    Editor.Undo;
    Check((Editor.Text = Original) and (Editor.SelStart = 0) and (Editor.SelLength = Length(Original)),
      'Indent is one undo transaction and restores original selection');
    Editor.Text := '  const value = 1;'; Editor.SelStart := Length(Editor.Text);
    Editor.Perform(WM_CHAR, 13, 0);
    Check(Editor.Text = '  const value = 1;' + #13#10 + '  ', 'Enter keeps leading indentation');
    Editor.Text := ''; Editor.Perform(WM_KEYDOWN, VK_TAB, 0); Editor.Perform(WM_CHAR, 9, 0);
    Check(Editor.Text = '  ', 'Tab inserts one configured indentation');
    Editor.ReadOnly := True; Editor.IndentSelection;
    Check(Editor.Text = '  ', 'ReadOnly prevents indentation'); Editor.ReadOnly := False;
    Editor.Text := Original;
    SetLength(Items, 2);
    Items[0].Start := Pos('return', Original) - 1; Items[0].Count := 6;
    Items[0].Severity := dsWarning; Items[0].MessageText := 'Host warning';
    Items[1] := Items[0]; Items[1].Severity := dsError; Items[1].MessageText := 'Host error';
    Check(Editor.PublishDiagnostics(Original, Items), 'Host diagnostics accept matching source snapshot');
    Items[0].MessageText := 'Mutated';
    Check(Editor.Diagnostics[0].MessageText = 'Host warning', 'Published diagnostics are copied');
    Check(Editor.DiagnosticAt(Items[0].Start, Diagnostic) and (Diagnostic.Severity = dsError),
      'Overlapping diagnostic lookup returns highest severity');
    Check(not Editor.PublishDiagnostics('old source', Items) and (Length(Editor.Diagnostics) = 2),
      'Stale diagnostic response rejected without replacing current ranges');
    Editor.SelStart := Length(Editor.Text); Editor.ReplaceSelection(' ');
    Check(Length(Editor.Diagnostics) = 0, 'Document edits clear outdated diagnostic offsets');
    Editor.Text := ''; Editor.Perform(WM_CHAR, Ord('('), 0);
    Check((Editor.Text = '()') and (Editor.SelStart = 1), 'Opening delimiter inserts pair and keeps caret inside');
    Editor.Perform(WM_CHAR, Ord(')'), 0);
    Check((Editor.Text = '()') and (Editor.SelStart = 2), 'Typing existing closing delimiter advances caret');
    Editor.Undo; Check(Editor.Text = '', 'Pair insertion is one undo transaction');
    Editor.Text := '()'; Editor.SelStart := 1;
    Editor.Perform(WM_KEYDOWN, VK_BACK, 0); Editor.Perform(WM_CHAR, 8, 0);
    Check(Editor.Text = '', 'Backspace between empty delimiters removes pair');
    Editor.Undo; Check((Editor.Text = '()') and (Editor.SelStart = 1), 'Paired deletion undo restores caret');
    Editor.Text := #$D83D#$DC69#$200D#$D83D#$DCBB; Editor.SelectAll;
    Editor.Perform(WM_CHAR, Ord('['), 0);
    Check((Editor.Text = '[' + #$D83D#$DC69#$200D#$D83D#$DCBB + ']') and (Editor.SelLength = 5),
      'Surround selection preserves Unicode grapheme');
    Editor.Undo; Check(Editor.SelLength = 5, 'Surround undo restores original selection');
    Editor.Text := '  const value = 1;'; Editor.SelStart := Length(Editor.Text);
    Editor.Perform(WM_KEYDOWN, VK_HOME, 0);
    Check(Editor.SelStart = 2, 'Smart Home moves to first non-whitespace character');
    Editor.Perform(WM_KEYDOWN, VK_HOME, 0);
    Check(Editor.SelStart = 0, 'Second Home moves to logical line start');
    Editor.Text := '{}'; Editor.SelStart := 1; Editor.Perform(WM_CHAR, 13, 0);
    Check((Editor.Text = '{' + #13#10 + '  ' + #13#10 + '}') and (Editor.SelStart = 5),
      'Enter between delimiters creates indented body and closing line');
    Editor.Text := '{ "}"; // }' + #13#10 + '}'; Editor.SelStart := 0;
    Check(Editor.MatchingBrackets(Opening, Closing) and (Opening = 0) and
      (Closing = Length(Editor.Text) - 1), 'Bracket matching ignores comment and string brackets');
    Editor.Text := '([)]'; Editor.SelStart := 0;
    Check(not Editor.MatchingBrackets(Opening, Closing), 'Mismatched nesting is not highlighted as a valid pair');
    Editor.Text := '// comment'; Editor.SelStart := Length(Editor.Text);
    Editor.Perform(WM_CHAR, Ord('('), 0);
    Check(Editor.Text = '// comment(', 'Delimiters inside line comments are ordinary input');
    Check(Editor.KeywordColors.Count = Length(ERDTypeScriptKeywords), 'Every TypeScript keyword has a colour entry');
    TERDKeywordColor(Editor.KeywordColors.Items[0]).Color := clFuchsia;
    Editor.SelectionPadding := 4;
    Editor.Language := clJSON; Editor.TabWidth := 4;
    Stream.WriteComponent(Editor); Stream.Position := 0;
    Restored := TERDCodeEditor(Stream.ReadComponent(nil));
    try
      Check((Restored.Language = clJSON) and (Restored.TabWidth = 4), 'Code editor settings stream correctly');
      Check((Restored.KeywordColors.Count = Editor.KeywordColors.Count) and
        (TERDKeywordColor(Restored.KeywordColors.Items[0]).Color = clFuchsia), 'Per-keyword colours stream without duplicate entries');
      Check(Restored.SelectionPadding = 4, 'Selection padding streams correctly');
    finally Restored.Free; end;
  finally Stream.Free; Form.Free; end;
end;

procedure TestMultilineTextEngine;
var Form: TForm; Memo, Restored: TERDMemo; Area: TERDTextArea; P, Q: TPoint; Count: Integer;
    Stream: TMemoryStream;
begin
  RegisterClass(TERDMemo);
  Form := TForm.Create(nil); Stream := TMemoryStream.Create;
  try
    Memo := TERDMemo.Create(Form); Memo.Parent := Form;
    Memo.SetBounds(8, 8, 180, 100); Area := Memo.Editor;
    Memo.Text := 'first' + #13#10 + 'second ' + #$D83D#$DC69#$200D#$D83D#$DCBB;
    Form.Show; Memo.SetFocus; Application.ProcessMessages;
    Check(GetWindow(Area.Handle, GW_CHILD) = 0, 'Memo text engine has no Windows EDIT child');
    Check(Memo.Lines.Count = 2, 'Memo Lines reflects multiline model');
    Check(Memo.Lines.Text = Memo.Text, 'Lines.Text does not add an extra newline');
    Area.SelStart := 0; Area.Perform(WM_KEYDOWN, VK_DOWN, 0);
    Check(Area.SelStart = 7, 'Down moves to same column on next row');
    Check(GetCaretPos(P), 'Memo has a native blinking caret');
    Area.Perform(WM_KEYDOWN, VK_UP, 0); Check(GetCaretPos(Q) and (Q.Y < P.Y), 'Up changes visual row');
    Area.SelStart := Length(Memo.Text); Area.Perform(WM_KEYDOWN, VK_BACK, 0); Area.Perform(WM_CHAR, 8, 0);
    Check(Memo.Text = 'first' + #13#10 + 'second ', 'Memo Backspace removes whole ZWJ emoji');
    Area.Undo; Check(Pos(#$200D, Memo.Text) > 0, 'Memo Undo restores emoji');
    Area.Perform(WM_CHAR, 13, 0); Area.Perform(WM_CHAR, Ord('X'), 0);
    Check(Copy(Memo.Text, Length(Memo.Text) - 2, 3) = #13#10 + 'X', 'Memo Enter inserts a real newline');
    Memo.Lines.Add('added'); Check(Pos('added', Memo.Text) > 0, 'Lines.Add updates document');
    Memo.Text := StringOfChar('W', 100);
    Memo.WordWrap := True; Count := Area.Perform(EM_GETLINECOUNT, 0, 0);
    Check(Count > 1, 'Long memo line wraps to multiple visual rows');
    Memo.WordWrap := False;
    Check(Area.Perform(EM_GETLINECOUNT, 0, 0) = 1, 'Disabling wrapping restores one visual row');
    Memo.Text := StringOfChar('A', 20) + #13#10 + StringOfChar('B', 20);
    Memo.WordWrap := True;
    Area.Perform(EM_LINESCROLL, 0, 1);
    Check(Area.FirstVisibleRow >= 0, 'Memo scrolling clamps within visual row range');
    Memo.Text := 'A' + #13#10 + 'B'; Area.SelectAll;
    Area.CopyToClipboard; Check(Clipboard.AsText = Memo.Text, 'Memo clipboard preserves multiline Unicode text');
    Memo.ReadOnly := True; Area.ReplaceSelection('blocked');
    Check(Memo.Text = 'A' + #13#10 + 'B', 'ReadOnly Memo prevents replacement');
    Memo.Text := 'A' + #13#10 + 'B' + #13#10;
    Check((Memo.Lines.Count = 3) and (Memo.Lines[2] = '') and (Memo.Lines.Text = Memo.Text),
      'Memo Lines preserves the final empty logical line');
    Stream.WriteComponent(Memo); Stream.Position := 0;
    Restored := TERDMemo(Stream.ReadComponent(nil));
    try Check(Restored.Text = Memo.Text, 'Memo DFM roundtrip preserves final newline without streaming private editor');
    finally Restored.Free; end;
  finally Stream.Free; Form.Free; end;
end;
procedure TestMigratedShowcaseTextWindows;
var Form: TShowcaseForm;
  procedure Inspect(Control: TWinControl);
  var I: Integer; ClassName: array[0..63] of Char;
  begin
    GetClassName(Control.Handle, ClassName, Length(ClassName));
    Check(not SameText(ClassName, 'EDIT'), 'Showcase contains no native Windows EDIT window');
    for I := 0 to Control.ControlCount - 1 do
      if Control.Controls[I] is TWinControl then Inspect(TWinControl(Control.Controls[I]));
  end;
begin
  Form := TShowcaseForm.Create(nil);
  try Form.Show; Application.ProcessMessages; Inspect(Form);
  finally Form.Free; end;
end;

procedure TestShowcaseUnicodePreviewWindows;
var Form: TShowcaseForm; DC: HDC; R: TRect;
  procedure CheckPreview(Input: TERDTextInput);
  begin
    Check(Input <> nil, 'Unicode preview is created into showcase field');
    Check(Input.Parent = Form.InputsScroll, 'Unicode preview belongs to Inputs scroll box');
    Check(Input.Visible and Input.Enabled, 'Unicode preview is visible and enabled');
    Check(IsWindowVisible(Input.Handle), 'Unicode preview HWND is visible after selecting Inputs');
    GetClientRect(Input.Handle, R);
    Check((R.Width > 0) and (R.Height > 0), 'Unicode preview HWND has nonzero client bounds');
    Input.Repaint;
    DC := GetDC(Input.Handle);
    try
      Check(GetPixel(DC,0,0) <> GetPixel(DC,3,R.Height div 2), 'Unicode preview paints its border on screen');
    finally ReleaseDC(Input.Handle, DC); end;
    Input.SetFocus;
    Check(GetFocus = Input.Handle, 'Unicode preview accepts focus in showcase');
  end;
begin
  Form := TShowcaseForm.Create(nil);
  try
    Form.Show; Form.PageTabs.TabIndex := 1; Form.PageSelected(Form);
    Application.ProcessMessages;
    CheckPreview(Form.UnicodeTextPreview);
    CheckPreview(Form.UnicodePasswordPreview);
  finally Form.Free; end;
end;

procedure TestSelfPaintedTextInput;
var Form: TForm; Input: TERDTextInput; Bitmap: TBitmap; Other: TERDTextInput; ClassName: array[0..63] of Char;
    X, Y, DarkPixels, FocusedClickPosition: Integer; C: TColor;
    BeforeEnd, AfterEnd, RepeatedEnd: TPoint;
begin
  Form := TForm.Create(nil); Bitmap := TBitmap.Create;
  try
    Input := TERDTextInput.Create(Form); Input.Parent := Form;
    Input.SetBounds(8, 8, 300, 28); Input.Text := 'Unicode ' + #$D83D#$DC4D#$D83C#$DFFD;
    Form.Show; Input.SetFocus; Application.ProcessMessages;
    GetClassName(Input.Handle, ClassName, Length(ClassName));
    Check(not SameText(ClassName, 'EDIT'), 'Custom input is not a Windows EDIT');
    Check(GetWindow(Input.Handle, GW_CHILD) = 0, 'Custom input has no native edit child');
    Input.Perform(WM_KEYDOWN, VK_BACK, 0); Input.Perform(WM_CHAR, 8, 0);
    Check(Input.Text = 'Unicode ', 'Custom input backspace removes emoji cluster');
    Input.Perform(WM_CHAR, Ord('A'), 0);
    Check(Input.Text = 'Unicode A', 'Self-painted input accepts typing');
    Input.Undo; Check(Input.Text = 'Unicode ', 'Self-painted input undo works');
    Input.SelectAll; Input.ReplaceSelection('Visible text'); Input.RenderTo(Bitmap);
    DarkPixels := 0;
    for Y := 5 to Bitmap.Height - 6 do for X := 8 to 100 do
    begin C := ColorToRGB(Bitmap.Canvas.Pixels[X,Y]);
      if (GetRValue(C) < 120) and (GetGValue(C) < 120) and (GetBValue(C) < 120) then Inc(DarkPixels);
    end;
    Check(DarkPixels > 15, 'Self-painted input renders visible glyphs');
    Input.PasswordChar := #$2022; Input.Text := 'secret'; Input.SelectAll;
    Clipboard.AsText := 'clipboard sentinel';
    Input.CopyToClipboard;
    Check(Clipboard.AsText = 'clipboard sentinel', 'Password contents cannot be copied');
    Input.PasswordVisible := True;
    Check(Input.Text = 'secret', 'Reveal preserves underlying password text');
    Input.ReadOnly := True; Input.ReplaceSelection('changed');
    Check(Input.Text = 'secret', 'ReadOnly blocks replacement');
    Input.ReadOnly := False; Input.PasswordChar := #0; Input.PasswordVisible := False;
    Input.Text := 'ABC' + #$D83D#$DD12;
    Input.SelStart := 3; Input.SelLength := 0;
    Check(GetCaretPos(BeforeEnd), 'Caret available before final emoji');
    Input.Perform(WM_KEYDOWN, VK_RIGHT, 0);
    Check(Input.SelStart = Length(Input.Text), 'Right arrow moves past whole final emoji');
    Check(GetCaretPos(AfterEnd) and (AfterEnd.X > BeforeEnd.X), 'End caret lies after final emoji');
    Input.Perform(WM_KEYDOWN, VK_RIGHT, 0);
    Check(GetCaretPos(RepeatedEnd) and (RepeatedEnd.X = AfterEnd.X), 'Right at end preserves caret position');
    Input.Perform(WM_KEYDOWN, VK_LEFT, 0);
    Check(Input.SelStart = 3, 'Left from end moves before whole final emoji');
    Other := TERDTextInput.Create(Form); Other.Parent := Form; Other.Top := 50;
    Input.Text := 'ABCDEFGHIJ';
    Input.Perform(WM_LBUTTONDOWN, MK_LBUTTON, LPARAM((14 shl 16) or 34));
    Input.Perform(WM_LBUTTONUP, 0, LPARAM((14 shl 16) or 34));
    FocusedClickPosition := Input.SelStart;
    Check(FocusedClickPosition > 0, 'Focused click places caret inside text');
    Other.SetFocus; Input.SelStart := Length(Input.Text);
    Input.Perform(WM_LBUTTONDOWN, MK_LBUTTON, LPARAM((14 shl 16) or 34));
    Input.Perform(WM_LBUTTONUP, 0, LPARAM((14 shl 16) or 34));
    Check(Input.Focused and (Input.SelStart = FocusedClickPosition), 'First unfocused click places caret at clicked position');

  finally Bitmap.Free; Form.Free; end;
end;

procedure TestCodeGutterThemeContrast;
var Form: TForm; Theme: TERDTheme; Editor: TCodeEditorProbe; C: COLORREF;
  function Brightness(Color: COLORREF): Integer;
  begin Result := (GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) div 3; end;
begin
  Form := TForm.Create(nil);
  try
    Theme := TERDTheme.Create(Form);
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form; Editor.Theme := Theme;
    Editor.SetBounds(0, 0, 360, 180);
    Editor.Text := 'function run() {' + #13#10 + '  return true;' + #13#10 + '}';
    Editor.ShowFoldGuides := False; Form.Show; Editor.FoldMouse(0, True);
    Theme.Mode := tmERDesignsLight;
    C := Editor.FoldPixel(True);
    Check(Brightness(C) > 180, 'Light-theme gutter stays light');
    Check(Brightness(Editor.FoldPixel(False)) > 180, 'Idle light fold button is neutral');
    Theme.Mode := tmERDesignsDark;
    C := Editor.FoldPixel(True);
    Check(Brightness(C) < 100, 'Dark-theme gutter stays dark');
    Check(Brightness(Editor.FoldPixel(False)) < 100, 'Idle dark fold button is neutral');
  finally Form.Free; end;
end;

begin
  try
    Application.Initialize;
    TestIconLifetimeAndStreaming;
    TestHostedStatusPanels;
    TestRibbonHostedControls;
    TestThemesAndRibbon;
    TestPopupRowsAndCommandSwitch;
    TestComboMouseChange;
    TestDemoLayoutAndSpinMargins;
    TestWindowsFocusCuesAndAutoSave;
    TestVirtualTreeThemeAndLazyNodes;
    TestCommonControls;
    TestFeedbackRegressions;
    TestSplitMenuGeometry;
    TestCheckListBox;
    TestTimePicker;
    TestDateSegmentsAndDateTime;
    TestLinkLabel;
    TestLoadingIndicator;
    TestDropZone;
    TestCommandPalette;
    TestCommandPalettePopup;
    TestRating;
    TestEditableCombo;
    TestEditPasswordAndValidation;
    TestDocumentTabs;
    TestTextPainting;
    TestUnicodeTextModel;
    TestSelfPaintedTextInput;
    TestMultilineTextEngine;
    TestSyntaxAndCodeEditor;
    TestCodeGutterThemeContrast;
    TestSharedColorEmoji;
    TestCodeEditorSearch;
    TestCodeLanguageServices;
    TestWindowsEditInteractionParity;
    TestMigratedShowcaseTextWindows;
    TestTextInputEditMenu;
    TestShowcaseUnicodePreviewWindows;
    Writeln(Checks, ' VCL runtime checks passed.');
  except on E: Exception do begin Writeln(E.ClassName, ': ', E.Message); Halt(1) end end;
end.
