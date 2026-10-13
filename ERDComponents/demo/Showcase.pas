unit Showcase;
interface
uses
  ERD.Components.MediaControls, ERD.Components.CommonControls, ERD.Components.VirtualTree, ERD.Components.VT.BaseTree, ERD.Components.VT.Types, ERD.Components.VT.Header,
  Vcl.StdCtrls,
  ERD.Components.Backstage,
  ERD.Components.Buttons,
  ERD.Components.Calendar,
  ERD.Components.Card,
  ERD.Components.Chips,
  ERD.Components.ColorDialog,
  ERD.Components.DataGrid,
  ERD.Components.Dialogs,
  ERD.Components.Edits,
  ERD.Components.FileDialogs,
  ERD.Components.FindDialogs,
  ERD.Components.FontDialog,
  ERD.Components.Hint,
  ERD.Components.Histogram,
  ERD.Components.Inspector,
  ERD.Components.ItemList,
  ERD.Components.KeyValueList,
  ERD.Components.Memo,
  ERD.Components.Menus,
  ERD.Components.PrintDialogs,
  ERD.Components.Progress,
  ERD.Components.RangeBar,
  ERD.Components.ScrollBar,
  ERD.Components.Segmented,
  ERD.Components.ShellControls,
  ERD.Components.SpinEdit,
  ERD.Components.Splitter,
  ERD.Components.Tabs,
  ERD.Components.TaskDialog,
  ERD.Components.TileGrid,
  ERD.Components.Toast,
  ERD.Components.ToolBar,
  ERD.Components.TrackBar,
  System.Classes,
  System.SysUtils,
  System.Types,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.Dialogs,
  Vcl.Menus,
  ERD.Components.Paint,
  ERD.Components.Types,
  ERD.Components.Theme,
  ERD.Components.Icons,
  ERD.Components.IconCatalog,
  ERD.Components.Panel,
  ERD.Components.StatusBar,
  ERD.Components.ScrollBox,
  ERD.Components.Ribbon,
  ERD.Components.Sidebar,
  ERD.Components.TitleBar;
type
  TPlaylistDemoData = record
    Id, GroupId: Cardinal;
  end;
  PPlaylistDemoData = ^TPlaylistDemoData;

  TShowcaseForm = class(TForm)
    TitleBar: TERDTitleBar;
    Sidebar: TERDSidebar;
    CommandChoice: TERDComboBox;
    Theme: TERDTheme;
    Ribbon: TERDRibbon;
    ThemeChoice: TERDComboBox;
    AppearanceChoice: TERDComboBox;
    LayoutChoice: TERDComboBox;
    StatusBar: TERDStatusBar;
    StatusHost: TERDPanel;
    BadgeOne: TERDBadge;
    BadgeTwo: TERDBadge;
    DownloadButton: TERDButton;
    MainMenu: TMainMenu;
    FileMenu: TERDMenuItem;
    OpenMenu: TERDMenuItem;
    DialogMenu: TERDMenuItem;
    Gallery: TERDScrollBox;
    Choices: TERDPanel;
    Images: TImageList;
    ImageButton: TERDButton;
    GlyphButton: TERDButton;
    NoIconButton: TERDButton;
    IconButton001: TERDButton;
    IconButton002: TERDButton;
    IconButton003: TERDButton;
    IconButton004: TERDButton;
    IconButton005: TERDButton;
    IconButton006: TERDButton;
    IconButton007: TERDButton;
    IconButton008: TERDButton;
    IconButton009: TERDButton;
    IconButton010: TERDButton;
    IconButton011: TERDButton;
    IconButton012: TERDButton;
    IconButton013: TERDButton;
    IconButton014: TERDButton;
    IconButton015: TERDButton;
    IconButton016: TERDButton;
    IconButton017: TERDButton;
    IconButton018: TERDButton;
    IconButton019: TERDButton;
    IconButton020: TERDButton;
    IconButton021: TERDButton;
    IconButton022: TERDButton;
    IconButton023: TERDButton;
    IconButton024: TERDButton;
    IconButton025: TERDButton;
    IconButton026: TERDButton;
    IconButton027: TERDButton;
    IconButton028: TERDButton;
    IconButton029: TERDButton;
    IconButton030: TERDButton;
    IconButton031: TERDButton;
    IconButton032: TERDButton;
    IconButton033: TERDButton;
    IconButton034: TERDButton;
    IconButton035: TERDButton;
    IconButton036: TERDButton;
    IconButton037: TERDButton;
    IconButton038: TERDButton;
    IconButton039: TERDButton;
    IconButton040: TERDButton;
    IconButton041: TERDButton;
    IconButton042: TERDButton;
    IconButton043: TERDButton;
    IconButton044: TERDButton;
    IconButton045: TERDButton;
    IconButton046: TERDButton;
    IconButton047: TERDButton;
    IconButton048: TERDButton;
    IconButton049: TERDButton;
    IconButton050: TERDButton;
    IconButton051: TERDButton;
    IconButton052: TERDButton;
    IconButton053: TERDButton;
    IconButton054: TERDButton;
    IconButton055: TERDButton;
    IconButton056: TERDButton;
    IconButton057: TERDButton;
    IconButton058: TERDButton;
    IconButton059: TERDButton;
    IconButton060: TERDButton;
    IconButton061: TERDButton;
    IconButton062: TERDButton;
    IconButton063: TERDButton;
    IconButton064: TERDButton;
    IconButton065: TERDButton;
    IconButton066: TERDButton;
    IconButton067: TERDButton;
    IconButton068: TERDButton;
    IconButton069: TERDButton;
    IconButton070: TERDButton;
    IconButton071: TERDButton;
    IconButton072: TERDButton;
    IconButton073: TERDButton;
    IconButton074: TERDButton;
    IconButton075: TERDButton;
    IconButton076: TERDButton;
    IconButton077: TERDButton;
    IconButton078: TERDButton;
    IconButton079: TERDButton;
    IconButton080: TERDButton;
    IconButton081: TERDButton;
    IconButton082: TERDButton;
    IconButton083: TERDButton;
    IconButton084: TERDButton;
    IconButton085: TERDButton;
    IconButton086: TERDButton;
    IconButton087: TERDButton;
    IconButton088: TERDButton;
    IconButton089: TERDButton;
    IconButton090: TERDButton;
    IconButton091: TERDButton;
    IconButton092: TERDButton;
    IconButton093: TERDButton;
    IconButton094: TERDButton;
    IconButton095: TERDButton;
    IconButton096: TERDButton;
    IconButton097: TERDButton;
    IconButton098: TERDButton;
    IconButton099: TERDButton;
    IconButton100: TERDButton;
    IconButton101: TERDButton;
    IconButton102: TERDButton;
    IconButton103: TERDButton;
    IconButton104: TERDButton;
    IconButton105: TERDButton;
    IconButton106: TERDButton;
    IconButton107: TERDButton;
    IconButton108: TERDButton;
    IconButton109: TERDButton;
    IconButton110: TERDButton;
    IconButton111: TERDButton;
    IconButton112: TERDButton;
    IconButton113: TERDButton;
    IconButton114: TERDButton;
    IconButton115: TERDButton;
    IconButton116: TERDButton;
    IconButton117: TERDButton;
    IconButton118: TERDButton;
    IconButton119: TERDButton;
    IconButton120: TERDButton;
    IconButton121: TERDButton;
    IconButton122: TERDButton;
    IconButton123: TERDButton;
    IconButton124: TERDButton;
    IconButton125: TERDButton;
    IconButton126: TERDButton;
    IconButton127: TERDButton;
    IconButton128: TERDButton;
    IconButton129: TERDButton;
    IconButton130: TERDButton;
    IconButton131: TERDButton;
    IconButton132: TERDButton;
    IconButton133: TERDButton;
    IconButton134: TERDButton;
    IconButton135: TERDButton;
    IconButton136: TERDButton;
    IconButton137: TERDButton;
    IconButton138: TERDButton;
    IconButton139: TERDButton;
    IconButton140: TERDButton;
    IconButton141: TERDButton;
    IconButton142: TERDButton;
    IconButton143: TERDButton;
    IconButton144: TERDButton;
    IconButton145: TERDButton;
    IconButton146: TERDButton;
    IconButton147: TERDButton;
    IconButton148: TERDButton;
    IconButton149: TERDButton;
    IconButton150: TERDButton;
    IconButton151: TERDButton;
    IconButton152: TERDButton;
    IconButton153: TERDButton;
    IconButton154: TERDButton;
    IconButton155: TERDButton;
    IconButton156: TERDButton;
    IconButton157: TERDButton;
    IconButton158: TERDButton;
    IconButton159: TERDButton;
    IconButton160: TERDButton;
    IconButton161: TERDButton;
    IconButton162: TERDButton;
    IconButton163: TERDButton;
    IconButton164: TERDButton;
    IconButton165: TERDButton;
    IconButton166: TERDButton;
    IconButton167: TERDButton;
    IconButton168: TERDButton;
    IconButton169: TERDButton;
    IconButton170: TERDButton;
    IconButton171: TERDButton;
    IconButton172: TERDButton;
    IconButton173: TERDButton;
    IconButton174: TERDButton;
    IconButton175: TERDButton;
    IconButton176: TERDButton;
    IconButton177: TERDButton;
    IconButton178: TERDButton;
    IconButton179: TERDButton;
    IconButton180: TERDButton;
    IconButton181: TERDButton;
    IconButton182: TERDButton;
    IconButton183: TERDButton;
    IconButton184: TERDButton;
    IconButton185: TERDButton;
    IconButton186: TERDButton;
    IconButton187: TERDButton;
    IconButton188: TERDButton;
    IconButton189: TERDButton;
    IconButton190: TERDButton;
    IconButton191: TERDButton;
    IconButton192: TERDButton;
    IconButton193: TERDButton;
    IconButton194: TERDButton;
    IconButton195: TERDButton;
    IconButton196: TERDButton;
    IconButton197: TERDButton;
    IconButton198: TERDButton;
    IconButton199: TERDButton;
    IconButton200: TERDButton;
    IconButton201: TERDButton;
    IconButton202: TERDButton;
    IconButton203: TERDButton;
    IconButton204: TERDButton;
    IconButton205: TERDButton;
    IconButton206: TERDButton;
    IconButton207: TERDButton;
    IconButton208: TERDButton;
    IconButton209: TERDButton;
    IconButton210: TERDButton;
    IconButton211: TERDButton;
    IconButton212: TERDButton;
    IconButton213: TERDButton;
    IconButton214: TERDButton;
    IconButton215: TERDButton;
    IconButton216: TERDButton;
    IconButton217: TERDButton;
    IconButton218: TERDButton;
    IconButton219: TERDButton;
    IconButton220: TERDButton;
    IconButton221: TERDButton;
    IconButton222: TERDButton;
    IconButton223: TERDButton;
    IconButton224: TERDButton;
    IconButton225: TERDButton;
    IconButton226: TERDButton;
    IconButton227: TERDButton;
    IconButton228: TERDButton;
    IconButton229: TERDButton;
    IconButton230: TERDButton;
    IconButton231: TERDButton;
    IconButton232: TERDButton;
    IconButton233: TERDButton;
    IconButton234: TERDButton;
    IconButton235: TERDButton;
    IconButton236: TERDButton;
    IconButton237: TERDButton;
    IconButton238: TERDButton;
    IconButton239: TERDButton;
    IconButton240: TERDButton;
    IconButton241: TERDButton;
    IconButton242: TERDButton;
    IconButton243: TERDButton;
    IconButton244: TERDButton;
    IconButton245: TERDButton;
    IconButton246: TERDButton;
    IconButton247: TERDButton;
    IconButton248: TERDButton;
    IconButton249: TERDButton;
    IconButton250: TERDButton;
    IconButton251: TERDButton;
    IconButton252: TERDButton;
    IconButton253: TERDButton;
    IconButton254: TERDButton;
    IconButton255: TERDButton;
    IconButton256: TERDButton;
    IconButton257: TERDButton;
    IconButton258: TERDButton;
    IconButton259: TERDButton;
    IconButton260: TERDButton;
    IconButton261: TERDButton;
    IconButton262: TERDButton;
    IconButton263: TERDButton;
    IconButton264: TERDButton;
    IconButton265: TERDButton;
    IconButton266: TERDButton;
    IconButton267: TERDButton;
    IconButton268: TERDButton;
    IconButton269: TERDButton;
    IconButton270: TERDButton;
    IconButton271: TERDButton;
    IconButton272: TERDButton;
    IconButton273: TERDButton;
    IconButton274: TERDButton;
    IconButton275: TERDButton;
    IconButton276: TERDButton;
    IconButton277: TERDButton;
    IconButton278: TERDButton;
    IconButton279: TERDButton;
    IconButton280: TERDButton;
    IconButton281: TERDButton;
    IconButton282: TERDButton;
    IconButton283: TERDButton;
    IconButton284: TERDButton;
    IconButton285: TERDButton;
    IconButton286: TERDButton;
    IconButton287: TERDButton;
    IconButton288: TERDButton;
    IconButton289: TERDButton;
    IconButton290: TERDButton;
    IconButton291: TERDButton;
    IconButton292: TERDButton;
    IconButton293: TERDButton;
    IconButton294: TERDButton;
    IconButton295: TERDButton;
    IconButton296: TERDButton;
    IconButton297: TERDButton;
    IconButton298: TERDButton;
    IconButton299: TERDButton;
    IconButton300: TERDButton;
    IconButton301: TERDButton;
    IconButton302: TERDButton;
    IconButton303: TERDButton;
    IconButton304: TERDButton;
    IconButton305: TERDButton;
    IconButton306: TERDButton;
    IconButton307: TERDButton;
    IconButton308: TERDButton;
    IconButton309: TERDButton;
    IconButton310: TERDButton;
    IconButton311: TERDButton;
    IconButton312: TERDButton;
    IconButton313: TERDButton;
    IconButton314: TERDButton;
    IconButton315: TERDButton;
    IconButton316: TERDButton;
    IconButton317: TERDButton;
    IconButton318: TERDButton;
    IconButton319: TERDButton;
    IconButton320: TERDButton;
    Chrome: TERDPanel;
    PageTabs: TERDTabs;
    DemoPages: TERDPanel;
    IconsPage: TERDPanel;
    DemoCard: TERDCard;
    DemoCheckBox: TERDCheckBox;
    DemoRadioButton: TERDRadioButton;
    DemoChip: TERDChip;
    DemoSeekBar: TERDSeekBar;
    DemoRangeSlider: TERDRangeSlider;
    DemoSearchEdit: TERDSearchEdit;
    DemoSplitButton: TERDSplitButton;
    DemoTagEdit: TERDTagEdit;
    DemoExpander: TERDExpander;
    ExpanderText: TLabel;
    DemoVerticalTrackBar: TERDTrackBar;
    CommonControlsLabel: TLabel;
    DemoBanner: TERDBanner;
    DemoEdit: TERDEdit;
    DemoSegmented: TERDSegmented;
    DemoRangeBar: TERDRangeBar;
    DemoInspector: TERDInspector;
    DemoMenuBar: TERDMenuBar;
    DemoPopupMenu: TERDPopupMenu;
    DemoBackstage: TERDBackstage;
    DemoTabs: TERDTabs;
    DemoToolBar: TERDToolBar;
    DemoProgressBar: TERDProgressBar;
    DemoDialog: TERDDialog;
    DemoToastManager: TERDToastManager;
    DemoHintStyle: TERDHintStyle;
    DemoScrollBar: TERDScrollBar;
    DemoSplitter: TERDSplitter;
    DemoSpinEdit: TERDSpinEdit;
    DemoTrackBar: TERDTrackBar;
    DemoMemo: TERDMemo;
    DemoCalendar: TERDCalendar;
    DemoDatePicker: TERDDatePicker;
    DemoListBox: TERDListBox;
    DemoBreadcrumb: TERDBreadcrumb;
    DemoPlacesPane: TERDPlacesPane;
    DemoFolderTree: TERDFolderTree;
    DemoFileList: TERDFileList;
    DemoColorSwatches: TERDColorSwatches;
    DemoColorSpectrum: TERDColorSpectrum;
    DemoDataGrid: TERDDataGrid;
    DemoTileGrid: TERDTileGrid;
    DemoKeyValueList: TERDKeyValueList;
    DemoHistogram: TERDHistogram;
    DemoOpenDialog: TERDOpenDialog;
    DemoSaveDialog: TERDSaveDialog;
    DemoOpenPictureDialog: TERDOpenPictureDialog;
    DemoSavePictureDialog: TERDSavePictureDialog;
    DemoFolderDialog: TERDFolderDialog;
    DemoPrintDialog: TERDPrintDialog;
    DemoPageSetupDialog: TERDPageSetupDialog;
    DemoFontDialog: TERDFontDialog;
    DemoColorDialog: TERDColorDialog;
    DemoFindDialog: TERDFindDialog;
    DemoReplaceDialog: TERDReplaceDialog;
    DemoTaskDialog: TERDTaskDialog;
    InputsPage: TERDPanel;
    InputsScroll: TERDScrollBox;
    LayoutPage: TERDPanel;
    LayoutScroll: TERDScrollBox;
    DataPage: TERDPanel;
    DataScroll: TERDScrollBox;
    FilesColorsPage: TERDPanel;
    FilesColorsScroll: TERDScrollBox;
    DialogsPage: TERDPanel;
    DialogsScroll: TERDScrollBox;
    LabelCheckBox: TLabel;
    LabelRadioButton: TLabel;
    LabelChip: TLabel;
    LabelBanner: TLabel;
    LabelEdit: TLabel;
    LabelSegmented: TLabel;
    LabelRangeBar: TLabel;
    LabelSpinEdit: TLabel;
    LabelTrackBar: TLabel;
    LabelMemo: TLabel;
    LabelCalendar: TLabel;
    LabelDatePicker: TLabel;
    LabelListBox: TLabel;
    LabelCard: TLabel;
    LabelMenuBar: TLabel;
    LabelBackstage: TLabel;
    LabelTabs: TLabel;
    LabelToolBar: TLabel;
    LabelProgressBar: TLabel;
    LabelScrollBar: TLabel;
    LabelSplitter: TLabel;
    LabelInspector: TLabel;
    LabelDataGrid: TLabel;
    LabelTileGrid: TLabel;
    LabelKeyValueList: TLabel;
    LabelHistogram: TLabel;
    LabelBreadcrumb: TLabel;
    LabelPlacesPane: TLabel;
    LabelFolderTree: TLabel;
    LabelFileList: TLabel;
    LabelColorSwatches: TLabel;
    LabelColorSpectrum: TLabel;
    LaunchDialog: TERDButton;
    LaunchOpenDialog: TERDButton;
    LaunchSaveDialog: TERDButton;
    LaunchOpenPictureDialog: TERDButton;
    LaunchSavePictureDialog: TERDButton;
    LaunchFolderDialog: TERDButton;
    LaunchPrintDialog: TERDButton;
    LaunchPageSetupDialog: TERDButton;
    LaunchFontDialog: TERDButton;
    LaunchColorDialog: TERDButton;
    LaunchFindDialog: TERDButton;
    LaunchReplaceDialog: TERDButton;
    LaunchTaskDialog: TERDButton;
    LaunchShowToast: TERDButton;
    LaunchShowPopup: TERDButton;
    HintSample: TLabel;
    PopupAction: TERDMenuItem;
    BackstageInfo: TERDPanel;
    BackstageSettings: TERDPanel;
    BackstageInfoText: TLabel;
    OpenBackstageButton: TERDButton;
    DemoVirtualStringTree: TERDVirtualStringTree;
    LabelVirtualStringTree: TLabel;
    procedure TreeGetText(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeInitNode(Sender: TERDVTBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeInitChildren(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure TreeCompareNodes(Sender: TERDVTBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure TreeHeaderClick(Sender: TERDVTVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure QuickAccessClicked(Sender: TObject);
    procedure AutoSaveChanged(Sender: TObject);
    procedure ShowBackstage(Sender: TObject);
    procedure BackstageClosed(Sender: TObject);
    procedure LayoutChrome(Sender: TObject);
    procedure PageSelected(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CommandSelected(Sender: TObject);
    procedure ExecuteDemoDialog(Sender: TObject);
    procedure ShowToast(Sender: TObject);
    procedure ShowPopup(Sender: TObject);
    procedure SidebarSelected(Sender: TObject; AItem: TERDSidebarItem);
    procedure ThemeSelected(Sender: TObject);
    procedure AppearanceSelected(Sender: TObject);
    procedure LayoutSelected(Sender: TObject);
    procedure ToggleTheme(Sender: TObject);
    procedure ShowDialog(Sender: TObject);
    procedure SearchChannels(Sender: TObject);
    procedure OpenFile(Sender: TObject);
    procedure IconClicked(Sender: TObject);
  end;
var MainForm: TShowcaseForm;
implementation
{$R *.dfm}

procedure TShowcaseForm.FormCreate(Sender: TObject);
var Bitmap: TBitmap;
begin
  Menu := nil;
  TitleBar.Menu := MainMenu;
  TitleBar.Ribbon := Ribbon;
  TitleBar.CommandStyle := csRibbon;
  LayoutChrome(Self);
  PageSelected(Self);
  StatusBar.Panels[2].Control := StatusHost;
  DemoDataGrid.AddRow(['Example file', 'Ready']);
  DemoDataGrid.AddRow(['Another item', 'Pending']);
  DemoHistogram.AddSamples([12, 18, 20, 22, 25, 28, 30, 34]);
  DemoColorSwatches.AddSection('Example colors', [clRed, clGreen, clBlue, clYellow, clWhite, clBlack]);
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(24, 24);
    Bitmap.Canvas.Brush.Color := clFuchsia;
    Bitmap.Canvas.FillRect(Rect(0, 0, 24, 24));
    Bitmap.Canvas.Brush.Color := clLime;
    Bitmap.Canvas.Ellipse(3, 3, 21, 21);
    Images.AddMasked(Bitmap, clFuchsia);
    GlyphButton.Icon.Glyph.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TShowcaseForm.SidebarSelected(Sender: TObject; AItem: TERDSidebarItem);
begin
  if AItem.Tag < 0 then
  begin
    PageTabs.TabIndex := -AItem.Tag;
  end
  else
  begin
    PageTabs.TabIndex := 0;
    Gallery.ScrollPosition := AItem.Tag;
  end;
  StatusBar.Panels[0].Text := AItem.Caption;
end;

procedure TShowcaseForm.ExecuteDemoDialog(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: DemoDialog.Execute;
    1: DemoOpenDialog.Execute;
    2: DemoSaveDialog.Execute;
    3: DemoOpenPictureDialog.Execute;
    4: DemoSavePictureDialog.Execute;
    5: DemoFolderDialog.Execute;
    6: DemoPrintDialog.Execute;
    7: DemoPageSetupDialog.Execute;
    8: DemoFontDialog.Execute;
    9: DemoColorDialog.Execute;
    10: DemoFindDialog.Execute;
    11: DemoReplaceDialog.Execute;
    12: DemoTaskDialog.Execute;
  end;
end;

procedure TShowcaseForm.ShowToast(Sender: TObject);
begin
  DemoToastManager.Show(tkInfo, 'ERD Components', 'Example toast notification.');
end;

procedure TShowcaseForm.ShowPopup(Sender: TObject);
var P: TPoint;
begin
  P := TControl(Sender).ClientToScreen(Point(0, TControl(Sender).Height));
  DemoPopupMenu.Popup(P.X, P.Y);
end;

procedure TShowcaseForm.TreeGetText(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var Data: PPlaylistDemoData;
begin
  if not (vsInitialized in Node.States) then Sender.ValidateNode(Node, False);
  Data := Sender.GetNodeData(Node);
  if Data.GroupId = 0 then
  begin
    if Column = 0 then CellText := Format('Playlist %d', [Data.Id])
    else if Column = 1 then CellText := 'Group' else CellText := '';
  end
  else
    case Column of
      0: CellText := Format('Channel %d.%d', [Data.GroupId, Data.Id]);
      1: CellText := 'Live TV';
      2: if Odd(Data.Id) then CellText := 'HD' else CellText := '4K';
    end;
end;

procedure TShowcaseForm.TreeInitNode(Sender: TERDVTBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data, ParentData: PPlaylistDemoData;
begin
  Data := Sender.GetNodeData(Node);
  Data.Id := Node.Index + 1;
  Data.GroupId := 0;
  Node.CheckType := ctCheckBox;
  if Sender.GetNodeLevel(Node) = 0 then Include(InitialStates, ivsHasChildren)
  else
  begin
    ParentData := Sender.GetNodeData(Node.Parent);
    Data.GroupId := ParentData.Id;
  end;
end;

procedure TShowcaseForm.TreeInitChildren(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  if Sender.GetNodeLevel(Node) = 0 then ChildCount := 1000 else ChildCount := 0;
end;

procedure TShowcaseForm.TreeCompareNodes(Sender: TERDVTBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var LeftText, RightText: string;
begin
  TreeGetText(Sender, Node1, Column, ttNormal, LeftText);
  TreeGetText(Sender, Node2, Column, ttNormal, RightText);
  Result := CompareText(LeftText, RightText);
end;

procedure TShowcaseForm.TreeHeaderClick(Sender: TERDVTVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column < 0 then Exit;
  if Sender.SortColumn = HitInfo.Column then
  begin
    if Sender.SortDirection = sdAscending then Sender.SortDirection := sdDescending
    else Sender.SortDirection := sdAscending;
  end
  else
  begin
    Sender.SortColumn := HitInfo.Column;
    Sender.SortDirection := sdAscending;
  end;
  DemoVirtualStringTree.SortTree(Sender.SortColumn, Sender.SortDirection, False);
end;

procedure TShowcaseForm.QuickAccessClicked(Sender: TObject);
begin
  StatusBar.Panels[0].Text := TERDCaptionButton(Sender).Hint + ' command clicked';
end;

procedure TShowcaseForm.AutoSaveChanged(Sender: TObject);
begin
  if TitleBar.AutoSave then
    StatusBar.Panels[0].Text := 'AutoSave on'
  else
    StatusBar.Panels[0].Text := 'AutoSave off';
end;

procedure TShowcaseForm.ShowBackstage(Sender: TObject);
begin
  Sidebar.Visible := False;
  Choices.Visible := False;
  DemoBackstage.Show;
  DemoBackstage.BringToFront;
  LayoutChrome(Self);
end;

procedure TShowcaseForm.BackstageClosed(Sender: TObject);
begin
  Sidebar.Visible := True;
  Choices.Visible := True;
  LayoutChrome(Self);
end;

procedure TShowcaseForm.LayoutChrome(Sender: TObject);
var Y: Integer;
begin
  if csLoading in ComponentState then Exit;
  if (Chrome = nil) or (TitleBar = nil) or (Ribbon = nil) or (Choices = nil) then Exit;
  Chrome.DisableAlign;
  try
    TitleBar.SetBounds(0, 0, Chrome.ClientWidth, TitleBar.Height);
    Y := TitleBar.Height;
    Ribbon.SetBounds(0, Y, Chrome.ClientWidth, Ribbon.Height);
    if Ribbon.Visible then Inc(Y, Ribbon.Height);
    Choices.SetBounds(0, Y, Chrome.ClientWidth, Choices.Height);
    if Choices.Visible then Inc(Y, Choices.Height);
    Chrome.Height := Y;
  finally
    Chrome.EnableAlign;
  end;
end;

procedure TShowcaseForm.PageSelected(Sender: TObject);
begin
  IconsPage.Visible := PageTabs.TabIndex = 0;
  InputsPage.Visible := PageTabs.TabIndex = 1;
  LayoutPage.Visible := PageTabs.TabIndex = 2;
  DataPage.Visible := PageTabs.TabIndex = 3;
  FilesColorsPage.Visible := PageTabs.TabIndex = 4;
  DialogsPage.Visible := PageTabs.TabIndex = 5;
end;

procedure TShowcaseForm.CommandSelected(Sender: TObject);
begin
  if CommandChoice.ItemIndex = 1 then
    TitleBar.CommandStyle := csMenu
  else
    TitleBar.CommandStyle := csRibbon;
  LayoutChrome(Self);
end;

procedure TShowcaseForm.ThemeSelected(Sender: TObject);
begin
  if ThemeChoice.ItemIndex >= 0 then
    Theme.Mode := TERDThemeMode(ThemeChoice.ItemIndex + Ord(tmERDesignsLight));
end;
procedure TShowcaseForm.AppearanceSelected(Sender: TObject);
begin
  if AppearanceChoice.ItemIndex >= 0 then
    Ribbon.Appearance := TERDRibbonAppearance(AppearanceChoice.ItemIndex);
end;
procedure TShowcaseForm.LayoutSelected(Sender: TObject);
begin
  if LayoutChoice.ItemIndex >= 0 then
    Ribbon.RibbonStyle := TERDRibbonStyle(LayoutChoice.ItemIndex);
end;
procedure TShowcaseForm.ToggleTheme(Sender: TObject);
begin
  if Theme.Mode = tmERDesignsDark then Theme.Mode := tmERDesignsLight else Theme.Mode := tmERDesignsDark;
  ThemeChoice.ItemIndex := Ord(Theme.Mode) - Ord(tmERDesignsLight);
end;
procedure TShowcaseForm.ShowDialog(Sender: TObject);
var Dialog: TERDDialog;
begin
  Dialog := TERDDialog.Create(Self);
  try
    Dialog.Theme := Theme;
    Dialog.Title := 'ERD Components'; Dialog.Text := 'General dialog, without OBD dependencies.';
    Dialog.Kind := dkInfo; Dialog.Buttons := [mbOK]; Dialog.Execute;
  finally Dialog.Free end;
end;
procedure TShowcaseForm.SearchChannels(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Search: ' + DemoSearchEdit.Text;
end;
procedure TShowcaseForm.OpenFile(Sender: TObject);
var Dialog: TERDOpenDialog;
begin
  Dialog := TERDOpenDialog.Create(Self);
  try
    Dialog.Theme := Theme;
    if Dialog.Execute then StatusBar.Panels[0].Text := 'File selected';
  finally Dialog.Free end;
end;
procedure TShowcaseForm.IconClicked(Sender: TObject);
begin
  StatusBar.Panels[0].Text := TERDButton(Sender).Caption;
end;
end.
