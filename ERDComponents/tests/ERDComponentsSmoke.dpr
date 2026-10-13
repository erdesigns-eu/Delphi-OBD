program ERDComponentsSmoke;
{$APPTYPE CONSOLE}
uses
  System.SysUtils, System.Classes, System.Types, Vcl.Forms, Vcl.Controls,
  Vcl.Graphics, Vcl.ImgList,
  ERD.Components.Types, ERD.Components.Icons, ERD.Components.IconCatalog,
  ERD.Components.Buttons, ERD.Components.Panel, ERD.Components.Chips,
  ERD.Components.StatusBar, ERD.Components.Theme, ERD.Components.Ribbon;
var Checks: Integer;
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

begin
  try
    Application.Initialize;
    TestIconLifetimeAndStreaming;
    TestHostedStatusPanels;
    TestThemesAndRibbon;
    Writeln(Checks, ' VCL runtime checks passed.');
  except on E: Exception do begin Writeln(E.ClassName, ': ', E.Message); Halt(1) end end;
end.
