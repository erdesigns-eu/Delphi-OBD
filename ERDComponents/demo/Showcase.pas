unit Showcase;
interface
uses
  System.Classes, System.SysUtils, System.Types, Vcl.Forms, Vcl.Controls,
  Vcl.Graphics, Vcl.ImgList, Vcl.Dialogs, Vcl.Menus,
  ERD.Components.Types, ERD.Components.Theme, ERD.Components.Icons,
  ERD.Components.IconCatalog, ERD.Components.Buttons, ERD.Components.Chips,
  ERD.Components.Panel, ERD.Components.StatusBar, ERD.Components.ScrollBox,
  ERD.Components.Ribbon, ERD.Components.Menus, ERD.Components.Dialogs,
  ERD.Components.FileDialogs, ERD.Components.Edits;
type
  TShowcaseForm = class(TForm)
  private
    FTheme: TERDTheme;
    FRibbon: TERDRibbon;
    FThemeChoice, FAppearanceChoice, FLayoutChoice: TERDComboBox;
    procedure ThemeSelected(Sender: TObject);
    procedure AppearanceSelected(Sender: TObject);
    procedure LayoutSelected(Sender: TObject);
    FStatus: TERDStatusBar;
    procedure ToggleTheme(Sender: TObject);
    procedure ShowDialog(Sender: TObject);
    procedure OpenFile(Sender: TObject);
    procedure IconClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;
var MainForm: TShowcaseForm;
implementation
constructor TShowcaseForm.Create(AOwner: TComponent);
var
  Ribbon: TERDRibbon; Tab: TERDRibbonTab; Group: TERDRibbonGroup;
  Menu: TMainMenu; MenuBar: TERDMenuBar; Item, Child: TERDMenuItem;
  Host: TERDPanel; Badge: TERDBadge; Button: TERDButton;
  Scroll: TERDScrollBox; Images: TImageList; Bitmap: TBitmap;
  I: Integer; Id: TERDIconId; Mode: TERDThemeMode; Choices: TERDPanel;
begin
  inherited CreateNew(AOwner);
  Caption := 'ERD Components — general VCL showcase';
  Width := 1080; Height := 760; Position := poScreenCenter;
  FTheme := TERDTheme.Create(Self); FTheme.Mode := tmERDesignsLight;
  Menu := TMainMenu.Create(Self);
  Item := TERDMenuItem.Create(Self); Item.Caption := 'File'; Menu.Items.Add(Item);
  Child := TERDMenuItem.Create(Self); Child.Caption := 'Open';
  Child.Icon.BuiltIn := ei002Openen; Child.OnClick := OpenFile; Item.Add(Child);
  Child := TERDMenuItem.Create(Self); Child.Caption := 'Dialog';
  Child.Icon.BuiltIn := ei028Info; Child.OnClick := ShowDialog; Item.Add(Child);
  MenuBar := TERDMenuBar.Create(Self); MenuBar.Parent := Self; MenuBar.Align := alTop;
  MenuBar.Menu := Menu;
  Ribbon := TERDRibbon.Create(Self); FRibbon := Ribbon; Ribbon.Parent := Self; Ribbon.Align := alTop;
  Tab := Ribbon.Tabs.Add; Tab.Caption := 'General'; Group := Tab.Groups.Add;
  Group.Caption := 'Application';
  with Group.Items.Add do
  begin Caption := 'Open'; Icon.BuiltIn := ei002Openen; Size := risLarge; OnClick := OpenFile end;
  with Group.Items.Add do
  begin Caption := 'Dialog'; Icon.BuiltIn := ei028Info; Size := risLarge; OnClick := ShowDialog end;
  with Group.Items.Add do
  begin Caption := 'Light / dark'; Icon.BuiltIn := ei027Instellingen; OnClick := ToggleTheme end;
  Tab := Ribbon.Tabs.Add; Tab.Caption := 'Media'; Group := Tab.Groups.Add; Group.Caption := 'Player';
  with Group.Items.Add do begin Caption := 'Play'; Icon.BuiltIn := ei161Afspelen; Size := risLarge end;
  with Group.Items.Add do begin Caption := 'Pause'; Icon.BuiltIn := ei162Pauzeren; Size := risLarge end;
  with Group.Items.Add do begin Caption := 'Teletext'; Icon.BuiltIn := ei249Teletext end;
  with Group.Items.Add do begin Caption := 'Subtitles'; Icon.BuiltIn := ei192Ondertiteling end;
  FStatus := TERDStatusBar.Create(Self); FStatus.Parent := Self; FStatus.Height := 44;
  with FStatus.Panels.Add do begin Kind := spkStatus; Text := 'Ready'; StatusKind := skSuccess; Width := 120 end;
  with FStatus.Panels.Add do begin Kind := spkProgress; Position := 35; Text := 'Example progress'; Width := 280 end;
  // One status panel hosts TWO badges and a button, as in Office-style chrome.
  Host := TERDPanel.Create(Self); Host.Parent := Self; Host.BorderWidth := 0; Host.Width := 300; Host.Height := 32;
  Badge := TERDBadge.Create(Self); Badge.Parent := Host; Badge.Count := 5; Badge.Left := 8; Badge.Top := 7;
  Badge := TERDBadge.Create(Self); Badge.Parent := Host; Badge.Count := 12; Badge.Kind := skSuccess;
  Badge.Left := 46; Badge.Top := 7;
  Button := TERDButton.Create(Self); Button.Parent := Host; Button.AutoSize := False;
  Button.SetBounds(96, 2, 170, 30); Button.Caption := 'Downloads'; Button.Kind := bkGhost;
  Button.Icon.BuiltIn := ei017Download; Button.OnClick := ShowDialog;
  with FStatus.Panels.Add do begin Width := 310; Alignment := paRight; Control := Host end;
  Scroll := TERDScrollBox.Create(Self); Scroll.Parent := Self; Scroll.Align := alClient;
  Images := TImageList.Create(Self); Images.Width := 24; Images.Height := 24;
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(24,24); Bitmap.Canvas.Brush.Color := clFuchsia; Bitmap.Canvas.FillRect(Rect(0,0,24,24));
    Bitmap.Canvas.Brush.Color := clLime; Bitmap.Canvas.Ellipse(3,3,21,21);
    Images.AddMasked(Bitmap, clFuchsia);
    Button := TERDButton.Create(Self); Button.Parent := Scroll; Button.AutoSize := False;
    Button.SetBounds(12,8,190,42); Button.Caption := 'ImageList source';
    Button.Icon.Source := eisImageList; Button.Icon.Images := Images; Button.Icon.ImageIndex := 0;
    Button := TERDButton.Create(Self); Button.Parent := Scroll; Button.AutoSize := False;
    Button.SetBounds(212,8,190,42); Button.Caption := 'Own glyph';
    Button.Icon.Glyph.Assign(Bitmap); Button.Icon.Source := eisGlyph;
  finally Bitmap.Free end;
  Button := TERDButton.Create(Self); Button.Parent := Scroll; Button.AutoSize := False;
  Button.SetBounds(412,8,190,42); Button.Caption := 'No icon'; Button.Icon.Source := eisNone;
  Choices := TERDPanel.Create(Self); Choices.Parent := Scroll; Choices.BorderWidth := 0;
  Choices.SetBounds(12, 58, 970, 40);
  FThemeChoice := TERDComboBox.Create(Self); FThemeChoice.Parent := Choices;
  FThemeChoice.SetBounds(0,2,220,34);
  for Mode := tmERDesignsLight to High(TERDThemeMode) do FThemeChoice.Items.Add(ERDThemeName(Mode));
  FThemeChoice.ItemIndex := 0; FThemeChoice.OnChange := ThemeSelected;
  FAppearanceChoice := TERDComboBox.Create(Self); FAppearanceChoice.Parent := Choices;
  FAppearanceChoice.SetBounds(240,2,220,34);
  FAppearanceChoice.Items.Add('ERDesigns ribbon'); FAppearanceChoice.Items.Add('Office ribbon');
  FAppearanceChoice.Items.Add('Flat ribbon'); FAppearanceChoice.ItemIndex := 0;
  FAppearanceChoice.OnChange := AppearanceSelected;
  FLayoutChoice := TERDComboBox.Create(Self); FLayoutChoice.Parent := Choices;
  FLayoutChoice.SetBounds(480,2,220,34); FLayoutChoice.Items.Add('Classic layout');
  FLayoutChoice.Items.Add('Simplified layout'); FLayoutChoice.ItemIndex := 0;
  FLayoutChoice.OnChange := LayoutSelected;
  for I := 1 to ERD_CATALOG_COUNT do
  begin
    Id := TERDIconId(I);
    Button := TERDButton.Create(Self); Button.Parent := Scroll; Button.AutoSize := False;
    Button.SetBounds(12 + ((I-1) mod 5)*194, 112 + ((I-1) div 5)*58, 184, 48);
    Button.Caption := Format('%.3d %s',[I, ERD_ICON_NAMES[Id]]);
    Button.Icon.BuiltIn := Id; Button.OnClick := IconClicked;
  end;
end;
procedure TShowcaseForm.ThemeSelected(Sender: TObject);
begin
  if FThemeChoice.ItemIndex >= 0 then
    FTheme.Mode := TERDThemeMode(FThemeChoice.ItemIndex + Ord(tmERDesignsLight));
end;
procedure TShowcaseForm.AppearanceSelected(Sender: TObject);
begin
  if FAppearanceChoice.ItemIndex >= 0 then
    FRibbon.Appearance := TERDRibbonAppearance(FAppearanceChoice.ItemIndex);
end;
procedure TShowcaseForm.LayoutSelected(Sender: TObject);
begin
  if FLayoutChoice.ItemIndex >= 0 then
    FRibbon.RibbonStyle := TERDRibbonStyle(FLayoutChoice.ItemIndex);
end;
procedure TShowcaseForm.ToggleTheme(Sender: TObject);
begin
  if FTheme.Mode = tmERDesignsDark then FTheme.Mode := tmERDesignsLight else FTheme.Mode := tmERDesignsDark;
end;
procedure TShowcaseForm.ShowDialog(Sender: TObject);
var Dialog: TERDDialog;
begin
  Dialog := TERDDialog.Create(Self);
  try
    Dialog.Title := 'ERD Components'; Dialog.Text := 'General dialog, without OBD dependencies.';
    Dialog.Kind := dkInfo; Dialog.Buttons := [mbOK]; Dialog.Execute;
  finally Dialog.Free end;
end;
procedure TShowcaseForm.OpenFile(Sender: TObject);
var Dialog: TERDOpenDialog;
begin
  Dialog := TERDOpenDialog.Create(Self);
  try if Dialog.Execute then FStatus.Panels[0].Text := 'File selected';
  finally Dialog.Free end;
end;
procedure TShowcaseForm.IconClicked(Sender: TObject);
begin
  FStatus.Panels[0].Text := TERDButton(Sender).Caption;
end;
end.
