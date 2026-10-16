unit Showcase;
interface
uses
  ConsolePreview, RuntimePreview, DebugPreview,
  ERD.Components.LSP, ERD.Components.LanguageServices, ERD.Components.CodeEditor, ERD.Components.Syntax,
  PCBPreview,
  ERD.Components.TextInput,
  ERD.Components.Rating,
  ERD.Components.CommandPalette,
  ERD.Components.DropZone,
  ERD.Components.LoadingIndicator,
  ERD.Components.LinkLabel,
  ERD.Components.DateTimePicker,
  ERD.Components.TimePicker,
  ERD.Components.CheckListBox,
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
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Math,
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
  ERD.Components.TitleBar, System.ImageList, ERD.Components.VT.BaseAncestorVCL,
  ERD.Components.VT.AncestorVCL, ERD.Components.VT, ERD.Components.Control;
type
  TPlaylistDemoData = record
    Id, GroupId: Cardinal;
    Edited: array[0..2] of Boolean;
    Values: array[0..2] of string;
  end;
  PPlaylistDemoData = ^TPlaylistDemoData;

  TShowcaseForm = class(TForm)
    LabelUnicodePreview: TLabel;
    RibbonInputs: TERDPanel;
    RibbonSearch: TERDSearchEdit;
    RibbonCombo: TERDComboBox;
    RibbonSpin: TERDSpinEdit;
    RibbonGallery: TERDTileGrid;
    DemoLoadingIndicator: TERDLoadingIndicator;
    LoadingStyleChoice: TERDComboBox;
    LoadingSizeChoice: TERDComboBox;
    LoadingToggle: TERDCheckBox;
    LabelLoadingIndicator: TLabel;
    DemoLinkLabel: TERDLinkLabel;
    LabelLinkLabel: TLabel;
    DemoDateTimePicker: TERDDateTimePicker;
    LabelDateTimePicker: TLabel;
    DemoTimePicker: TERDTimePicker;
    DemoTimePicker12: TERDTimePicker;
    LabelTimePicker: TLabel;
    DemoCheckListBox: TERDCheckListBox;
    LabelCheckListBox: TLabel;
    DemoDropZone: TERDDropZone;
    LabelDropZone: TLabel;
    DemoCommandPalette: TERDCommandPalette;
    ShowCommands: TERDButton;
    DemoRating: TERDRating;
    RatingReadOnly: TERDRating;
    RatingLabel: TLabel;
    SearchCombo: TERDComboBox;
    SearchComboLabel: TLabel;
    PasswordEdit: TERDEdit;
    ValidationEdit: TERDEdit;
    PasswordEditLabel: TLabel;
    ValidationEditLabel: TLabel;
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
    DocumentHost: TERDPanel;
    DocumentOne: TERDPanel;
    DocumentTwo: TERDPanel;
    DocumentOneEdit: TERDEdit;
    DocumentTwoEdit: TERDEdit;
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
    procedure RibbonInputChanged(Sender: TObject);
    procedure RibbonGalleryChanged(Sender: TObject);
    procedure TreeNewText(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: string);
    procedure TreeNodeDataSize(Sender: TERDVTBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeFreeNode(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode);
    procedure TreeCellEditor(Sender: TObject; Node: PVirtualNode; Column: TColumnIndex;
      var Kind: TERDTreeEditorKind; Choices: TStrings);
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
    procedure BreadcrumbChoices(Sender: TObject; const Path: string; Choices: TERDPlaces);
    procedure BreadcrumbNavigate(Sender: TObject; const Path: string);
    procedure SearchChannels(Sender: TObject);
    procedure CheckListChanged(Sender: TObject; Index: Integer);
    procedure TimeChanged(Sender: TObject);
    procedure DateTimeChanged(Sender: TObject);
    procedure LinkClicked(Sender: TObject);
    procedure LoadingStyleChanged(Sender: TObject);
    procedure LoadingSizeChanged(Sender: TObject);
    procedure LoadingChanged(Sender: TObject);
    procedure RatingChanged(Sender: TObject);
    procedure SearchComboChanged(Sender: TObject);
    procedure NewDocumentTab(Sender: TObject);
    procedure DocumentEdited(Sender: TObject);
    procedure DocumentTabClosing(Sender: TObject; Index: Integer; var CanClose: Boolean);
    procedure DocumentTabClosed(Sender: TObject; TabID: Integer; Page: TWinControl);
    procedure DocumentTabReordered(Sender: TObject; OldIndex, NewIndex: Integer);
    procedure DocumentTabDetached(Sender: TObject; Index: Integer; Window: TCustomForm);
    procedure DocumentTabAttached(Sender: TObject; Index: Integer);
    procedure ValidateExample(Sender: TObject; const Text: string;
      var State: TERDEditValidationState; var MessageText: string);
    procedure OpenCommands(Sender: TObject);
    procedure ShowcaseKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CommandExecuted(Sender: TObject; Command: TERDCommandItem);
    procedure FilesDropped(Sender: TObject; Files: TStrings);
    procedure BrowseDropFiles(Sender: TObject);
    procedure OpenFile(Sender: TObject);
    procedure IconClicked(Sender: TObject);
  public
    // Runtime-only previews: do not let the form designer manage these fields.
    PCBPage: TPCBPreview;
    DebugPage: TDebugPreview;
    RuntimePage: TRuntimePreview;
    ConsolePage: TConsolePreview;
    CodePage: TERDPanel;
    CodeEditor: TERDCodeEditor;
    CodeUsingNativeService: Boolean;
    CodeSearchQuery, CodeSearchReplacement: TERDEdit;
    CodeSearchRegex, CodeSearchCase, CodeSearchWord: TERDCheckBox;
    CodeSearchStatus: TLabel;
    UnicodeTextPreview: TERDTextInput;
    UnicodePasswordPreview: TERDTextInput;
    procedure CodeConnectCompiler(Sender: TObject);
    procedure CodeMinimapClicked(Sender: TObject);
    procedure CodeCompleteClicked(Sender: TObject);
    procedure CodeCompilerError(Sender: TObject);
    procedure CodeCompilerDiagnostics(Sender: TObject; const DocumentUri, SourceText: UnicodeString;
      const Diagnostics: TArray<TERDCodeDiagnostic>);
    procedure CodeLanguageResults(Sender: TObject);
    procedure CodeNavigated(Sender: TObject; const Target: TERDDefinitionTarget);
    procedure CodeSearchChanged(Sender: TObject);
    procedure CodeSearchClicked(Sender: TObject);
    procedure CodeDocumentChanged(Sender: TObject);
    procedure CodeSearchRequested(Sender: TObject; Replacing: Boolean);
    procedure CreateCodePreview;
    procedure CodeFoldClicked(Sender: TObject);
    procedure CodeDiagnosticsClicked(Sender: TObject);
    procedure CreateUnicodePreviews;
  end;
var MainForm: TShowcaseForm;
implementation
{$R *.dfm}
type
  TShowcaseLanguageService = class(TInterfacedObject, IERDCodeLanguageService)
  public
    procedure Request(const Snapshot: TERDLanguageRequest; const Responses: IERDLanguageResponses);
    procedure Cancel(RequestID: Int64);
  end;
procedure TShowcaseLanguageService.Cancel(RequestID: Int64);
begin end;
procedure TShowcaseLanguageService.Request(const Snapshot: TERDLanguageRequest; const Responses: IERDLanguageResponses);
const Names: array[0..5] of string = ('console', 'Command', 'CommandRegistry', 'commands', 'Promise', 'register');
var Start, Finish, I, At, ReceiverStart, ReceiverEnd: Integer; Prefix, Word, Marker, Receiver: UnicodeString;
    Candidates: TArray<string>;
    Items: TArray<TERDCompletionItem>; Target: TERDDefinitionTarget;
    Completion: TERDCompletionItem; Help: TERDSignatureHelp; SignatureResponses: IERDSignatureHelpResponses;
    Depth, Argument, OpenCall, LabelStart: Integer; Signature, ParameterLabel: string;
  function Identifier(C: Char): Boolean;
  begin Result := CharInSet(C, ['a'..'z','A'..'Z','0'..'9','_','$']); end;
begin
  // A small synchronous sample adapter, not a TypeScript analyser or TSVM.
  Start := Snapshot.Position; Finish := Start;
  while (Start > 0) and Identifier(Snapshot.Text[Start]) do Dec(Start);
  while (Finish < Length(Snapshot.Text)) and Identifier(Snapshot.Text[Finish + 1]) do Inc(Finish);
  Prefix := Copy(Snapshot.Text, Start + 1, Snapshot.Position - Start);
  Word := Copy(Snapshot.Text, Start + 1, Finish - Start);
  case Snapshot.Kind of
    lrCompletion:
      begin
        Items := nil; At := 0;
        if (Start > 0) and (Snapshot.Text[Start] = '.') then
        begin
          ReceiverEnd := Start - 1;
          if (ReceiverEnd > 0) and (Snapshot.Text[ReceiverEnd] = '?') then Dec(ReceiverEnd);
          ReceiverStart := ReceiverEnd;
          while (ReceiverStart > 0) and Identifier(Snapshot.Text[ReceiverStart]) do Dec(ReceiverStart);
          Receiver := Copy(Snapshot.Text, ReceiverStart + 1, ReceiverEnd - ReceiverStart);
          if Receiver = 'console' then Candidates := TArray<string>.Create('log', 'warn', 'error', 'info')
          else if Receiver = 'commands' then Candidates := TArray<string>.Create('get', 'set', 'has', 'clear')
          else if Receiver = 'command' then Candidates := TArray<string>.Create('id', 'title', 'run')
          else Candidates := nil;
        end
        else
        begin
          Candidates := ERDTypeScriptKeywords; At := Length(Candidates);
          SetLength(Candidates, At + Length(Names));
          for I := Low(Names) to High(Names) do Candidates[At + I] := Names[I];
        end;
        for I := 0 to High(Candidates) do
          if SameText(Copy(Candidates[I], 1, Length(Prefix)), Prefix) then
          begin
            SetLength(Items, Length(Items) + 1);
            Completion := Default(TERDCompletionItem);
            Completion.LabelText := Candidates[I]; Completion.InsertText := Candidates[I];
            Completion.Kind := 6; Completion.Detail := ': unknown';
            Completion.Documentation := 'Example identifier. Connect TSVM for compiler-inferred types and documentation.';
            if (Start > 0) and (Snapshot.Text[Start] = '.') then
            begin
              Completion.Kind := 2; Completion.Detail := '(...args): void';
              if Candidates[I] = 'id' then begin Completion.Kind := 10; Completion.Detail := ': string'; end
              else if Candidates[I] = 'title' then begin Completion.Kind := 10; Completion.Detail := ': string'; end
              else if Candidates[I] = 'run' then Completion.Detail := '(): Promise<void>'
              else if Candidates[I] = 'get' then Completion.Detail := '(key: string): Command | undefined'
              else if Candidates[I] = 'set' then Completion.Detail := '(key: string, value: Command): Map<string, Command>'
              else if Candidates[I] = 'has' then Completion.Detail := '(key: string): boolean'
              else if Candidates[I] = 'clear' then Completion.Detail := '(): void'
              else Completion.Detail := '(...data: unknown[]): void';
              Completion.Documentation := 'Example member of ' + Receiver + '. Connect TSVM for semantic results.';
            end
            else if I < At then
            begin Completion.Kind := 14; Completion.Detail := 'TypeScript keyword';
              Completion.Documentation := 'TypeScript language keyword.'; end
            else if Candidates[I] = 'Command' then begin Completion.Kind := 8; Completion.Detail := 'interface Command'; end
            else if Candidates[I] = 'CommandRegistry' then begin Completion.Kind := 7; Completion.Detail := 'class CommandRegistry'; end
            else if Candidates[I] = 'Promise' then begin Completion.Kind := 7; Completion.Detail := 'class Promise<T>'; end
            else if Candidates[I] = 'console' then begin Completion.Kind := 9; Completion.Detail := 'namespace console'; end
            else if Candidates[I] = 'commands' then begin Completion.Kind := 5; Completion.Detail := ': Map<string, Command>'; end
            else if Candidates[I] = 'register' then begin Completion.Kind := 2; Completion.Detail := '(command: Command): void'; end;
            Completion.Start := Start; Completion.Count := Finish - Start; Items[High(Items)] := Completion;
          end;
        Responses.Completion(Snapshot, Items);
      end;
    lrSignatureHelp:
      if Supports(Responses, IERDSignatureHelpResponses, SignatureResponses) then
      begin
        Help := Default(TERDSignatureHelp); Help.ActiveParameter := 0;
        Depth := 0; Argument := 0; OpenCall := 0;
        for I := Snapshot.Position downto 1 do
        begin
          if CharInSet(Snapshot.Text[I], [')', ']', '}']) then Inc(Depth)
          else if CharInSet(Snapshot.Text[I], ['(', '[', '{']) then
          begin
            if (Depth = 0) and (Snapshot.Text[I] = '(') then begin OpenCall := I; Break; end;
            Dec(Depth);
          end
          else if (Depth = 0) and (Snapshot.Text[I] = ',') then Inc(Argument);
        end;
        if OpenCall > 0 then
        begin
          At := OpenCall - 1;
          while (At > 0) and (Snapshot.Text[At] = ' ') do Dec(At);
          Start := At; while (Start > 0) and Identifier(Snapshot.Text[Start]) do Dec(Start);
          Word := Copy(Snapshot.Text, Start + 1, At - Start); Signature := '';
          if Word = 'set' then Signature := 'set(key: string, value: Command): Map<string, Command>'
          else if Word = 'execute' then Signature := 'execute(id: string): Promise<boolean>'
          else if Word = 'register' then Signature := 'register(command: Command): void'
          else if (Word = 'log') or (Word = 'warn') or (Word = 'error') or (Word = 'info') then
            Signature := Word + '(...data: unknown[]): void';
          if Signature <> '' then
          begin
            SetLength(Help.Signatures, 1); Help.Signatures[0].LabelText := Signature;
            Help.Signatures[0].Documentation := 'Example provider signature. Connect TSVM for inferred function types and overloads.';
            Help.Signatures[0].ActiveParameter := -1;
            if Word = 'set' then Candidates := TArray<string>.Create('key: string', 'value: Command')
            else if Word = 'execute' then Candidates := TArray<string>.Create('id: string')
            else if Word = 'register' then Candidates := TArray<string>.Create('command: Command')
            else Candidates := TArray<string>.Create('...data: unknown[]');
            SetLength(Help.Signatures[0].Parameters, Length(Candidates));
            for I := 0 to High(Candidates) do
            begin
              ParameterLabel := Candidates[I]; LabelStart := Pos(ParameterLabel, Signature) - 1;
              Help.Signatures[0].Parameters[I].Start := LabelStart;
              Help.Signatures[0].Parameters[I].Count := Length(ParameterLabel);
              Help.Signatures[0].Parameters[I].LabelText := ParameterLabel;
            end;
            Help.ActiveParameter := Min(Argument, High(Candidates));
          end;
        end;
        SignatureResponses.SignatureHelp(Snapshot, Help);
      end;
    lrHover: Responses.Hover(Snapshot, 'Example provider info: ' + Word + ' - replace this adapter with TSVM.');
    lrDefinition:
      begin
        Target := Default(TERDDefinitionTarget); At := 0; Marker := '';
        if Word <> '' then
        begin
          Marker := 'interface ' + Word; At := Pos(Marker, Snapshot.Text);
          if At = 0 then begin Marker := 'class ' + Word; At := Pos(Marker, Snapshot.Text); end;
        end;
        Target.Found := At > 0;
        if Target.Found then begin Target.Start := At - 1 + Length(Marker) - Length(Word); Target.Count := Length(Word); end;
        Responses.Definition(Snapshot, Target);
      end;
  end;
end;

procedure TShowcaseForm.CreateCodePreview;
var Header: TERDPanel; Button: TERDButton; Instructions: TLabel;
  procedure AddSearchButton(const Caption: string; Left, Width, Action: Integer);
  var B: TERDButton;
  begin
    B := TERDButton.Create(Self); B.Parent := Header; B.Caption := Caption;
    B.Tag := Action; B.OnClick := CodeSearchClicked;
    B.SetBounds(MulDiv(Left, CurrentPPI, 96), MulDiv(48, CurrentPPI, 96),
      MulDiv(Width, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  end;
  procedure AddSearchChoice(var Choice: TERDCheckBox; const Caption: string; Left, Width: Integer);
  begin
    Choice := TERDCheckBox.Create(Self); Choice.Parent := Header; Choice.Caption := Caption;
    Choice.SetBounds(MulDiv(Left, CurrentPPI, 96), MulDiv(48, CurrentPPI, 96),
      MulDiv(Width, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
    Choice.OnChange := CodeSearchChanged;
  end;
begin
  CodePage := TERDPanel.Create(Self);
  CodePage.Name := 'CodePreviewPage'; CodePage.Parent := DemoPages;
  CodePage.Align := alClient; CodePage.Visible := False;
  Header := TERDPanel.Create(Self); Header.Parent := CodePage;
  Header.Align := alTop; Header.Height := MulDiv(106, CurrentPPI, 96);
  Button := TERDButton.Create(Self); Button.Parent := Header;
  Button.Caption := 'Fold all'; Button.Tag := 1; Button.OnClick := CodeFoldClicked;
  Button.SetBounds(MulDiv(8, CurrentPPI, 96), MulDiv(8, CurrentPPI, 96),
    MulDiv(88, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  Button := TERDButton.Create(Self); Button.Parent := Header;
  Button.Caption := 'Unfold all'; Button.Tag := 0; Button.OnClick := CodeFoldClicked;
  Button.SetBounds(MulDiv(104, CurrentPPI, 96), MulDiv(8, CurrentPPI, 96),
    MulDiv(96, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  Button := TERDButton.Create(Self); Button.Parent := Header;
  Button.Caption := 'Diagnostics'; Button.OnClick := CodeDiagnosticsClicked;
  Button.SetBounds(MulDiv(208, CurrentPPI, 96), MulDiv(8, CurrentPPI, 96),
    MulDiv(100, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  Button := TERDButton.Create(Self); Button.Parent := Header; Button.Caption := 'Connect TSVM...';
  Button.OnClick := CodeConnectCompiler;
  Button.SetBounds(MulDiv(320, CurrentPPI, 96), MulDiv(8, CurrentPPI, 96),
    MulDiv(120, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  Button := TERDButton.Create(Self); Button.Parent := Header; Button.Caption := 'Hide minimap';
  Button.OnClick := CodeMinimapClicked;
  Button.SetBounds(MulDiv(448, CurrentPPI, 96), MulDiv(8, CurrentPPI, 96),
    MulDiv(104, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  Button := TERDButton.Create(Self); Button.Parent := Header; Button.Caption := 'Complete';
  Button.OnClick := CodeCompleteClicked;
  Button.ShowHint := True; Button.Hint := 'Completion: Ctrl+J / Ctrl+Space. Parameters: Ctrl+Shift+Space.';
  Button.SetBounds(MulDiv(560, CurrentPPI, 96), MulDiv(8, CurrentPPI, 96),
    MulDiv(88, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  Instructions := TLabel.Create(Self); Instructions.Parent := Header;
  Instructions.Caption := 'Fold UI v' + IntToStr(TERDCodeEditor.FoldInteractionRevision) +
    ' - hover / press +/-; console. / Ctrl+Space; F1 info; F12 definition';
  Instructions.Left := MulDiv(452, CurrentPPI, 96); Instructions.Top := MulDiv(15, CurrentPPI, 96);
  CodeSearchQuery := TERDEdit.Create(Self); CodeSearchQuery.Parent := Header;
  CodeSearchQuery.TextHint := 'Find'; CodeSearchQuery.Text := 'command';
  CodeSearchQuery.SetBounds(MulDiv(8, CurrentPPI, 96), MulDiv(48, CurrentPPI, 96),
    MulDiv(160, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  CodeSearchQuery.OnChange := CodeSearchChanged;
  CodeSearchReplacement := TERDEdit.Create(Self); CodeSearchReplacement.Parent := Header;
  CodeSearchReplacement.TextHint := 'Replace with'; CodeSearchReplacement.Text := 'action';
  CodeSearchReplacement.SetBounds(MulDiv(176, CurrentPPI, 96), MulDiv(48, CurrentPPI, 96),
    MulDiv(150, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
  AddSearchButton('Next', 334, 56, 0);
  AddSearchButton('Previous', 398, 76, 1);
  AddSearchButton('Replace', 482, 72, 2);
  AddSearchButton('All', 562, 48, 3);
  AddSearchChoice(CodeSearchRegex, 'Regex', 618, 76);
  AddSearchChoice(CodeSearchCase, 'Case', 702, 70);
  AddSearchChoice(CodeSearchWord, 'Word', 780, 74);
  CodeSearchStatus := TLabel.Create(Self); CodeSearchStatus.Parent := Header;
  CodeSearchStatus.SetBounds(MulDiv(8, CurrentPPI, 96), MulDiv(82, CurrentPPI, 96),
    MulDiv(820, CurrentPPI, 96), MulDiv(20, CurrentPPI, 96));
  CodeSearchStatus.AutoSize := False;
  CodeEditor := TERDCodeEditor.Create(Self); CodeEditor.Name := 'TypeScriptPreview';
  CodeEditor.Parent := CodePage; CodeEditor.Align := alClient;
  CodeEditor.AlignWithMargins := True;
  CodeEditor.Margins.SetBounds(8, 0, 8, 8);
  CodeEditor.Language := clTypeScript;
  CodeEditor.ShowFolding := True; CodeEditor.ShowFoldGuides := True;
  CodeEditor.ScrollBars := ssBoth; CodeEditor.ShowMinimap := True;
  CodeEditor.Text :=
    '// TypeScript preview - Unicode: Caf' + #$00E9 + ' ' + #$D83D#$DCBB + #13#10 +
    'export interface Command {' + #13#10 +
    '  readonly id: string;' + #13#10 +
    '  title: string;' + #13#10 +
    '  run(): Promise<void>;' + #13#10 +
    '}' + #13#10#13#10 +
    'export class CommandRegistry {' + #13#10 +
    '  private readonly commands = new Map<string, Command>();' + #13#10#13#10 +
    '  register(command: Command): void {' + #13#10 +
    '    this.commands.set(command.id, command);' + #13#10 +
    '  }' + #13#10#13#10 +
    '  async execute(id: string): Promise<boolean> {' + #13#10 +
    '    const command = this.commands.get(id);' + #13#10 +
    '    if (!command) {' + #13#10 +
    '      return false;' + #13#10 +
    '    }' + #13#10 +
    '    await command.run();' + #13#10 +
    '    return true;' + #13#10 +
    '  }' + #13#10 +
    '}' + #13#10;
  CodeEditor.LanguageService := TShowcaseLanguageService.Create;
  CodeEditor.OnLanguageResults := CodeLanguageResults; CodeEditor.OnNavigate := CodeNavigated;
  CodeEditor.OnSearchRequested := CodeSearchRequested;
  CodeEditor.OnChange := CodeDocumentChanged;
  CodeSearchChanged(Self);
  PageTabs.Tabs.Add.Caption := 'Code';
  PageTabs.BringToFront;
end;

procedure TShowcaseForm.CodeMinimapClicked(Sender: TObject);
begin
  CodeEditor.ShowMinimap := not CodeEditor.ShowMinimap;
  if CodeEditor.ShowMinimap then TERDButton(Sender).Caption := 'Hide minimap'
  else TERDButton(Sender).Caption := 'Show minimap';
end;
procedure TShowcaseForm.CodeCompleteClicked(Sender: TObject);
begin CodeEditor.SetFocus; CodeEditor.RequestCompletion; end;
procedure TShowcaseForm.CodeConnectCompiler(Sender: TObject);
var Dialog: TERDOpenDialog; Service: TERDStdioLanguageService;
    CompilerFile, Base, Candidate: string; I: Integer;
begin
  CompilerFile := ''; Base := ExtractFilePath(ParamStr(0));
  // Default IDE output is demo/Win32/Release (or Win64/Debug).
  // Also support a portable demo with compiler/ beside its executable.
  for I := 0 to 3 do
  begin
    Candidate := IncludeTrailingPathDelimiter(Base) + 'compiler\windows-x64\erd-compiler.exe';
    if FileExists(Candidate) then begin CompilerFile := Candidate; Break; end;
    Base := ExtractFileDir(ExcludeTrailingPathDelimiter(Base));
  end;
  if CompilerFile = '' then
  begin
    Dialog := TERDOpenDialog.Create(Self);
    try
      Dialog.Filter := 'ERD compiler|erd-compiler.exe|Programs|*.exe';
      if not Dialog.Execute then Exit;
      CompilerFile := Dialog.FileName;
    finally Dialog.Free; end;
  end;
  Service := TERDStdioLanguageService.Create(CompilerFile);
  Service.OnDiagnostics := CodeCompilerDiagnostics; Service.OnError := CodeCompilerError;
  CodeUsingNativeService := True;
  CodeEditor.DocumentUri := 'file:///showcase.ts'; CodeEditor.LanguageService := Service;
  CodeSearchStatus.Caption := 'TSVM connected - diagnostics, completion and parameter hints';
  CodeEditor.SetFocus;
end;
procedure TShowcaseForm.CodeCompilerError(Sender: TObject);
begin CodeSearchStatus.Caption := TERDStdioLanguageService(Sender).LastError; end;
procedure TShowcaseForm.CodeCompilerDiagnostics(Sender: TObject; const DocumentUri, SourceText: UnicodeString;
  const Diagnostics: TArray<TERDCodeDiagnostic>);
begin
  if DocumentUri <> CodeEditor.DocumentUri then Exit;
  if CodeEditor.PublishDiagnostics(SourceText, Diagnostics) then
    CodeSearchStatus.Caption := IntToStr(Length(Diagnostics)) + ' compiler diagnostics';
end;
procedure TShowcaseForm.CodeLanguageResults(Sender: TObject);
begin
  if CodeEditor.LanguageError <> '' then CodeSearchStatus.Caption := CodeEditor.LanguageError
  else case CodeEditor.LanguageResultKind of
    lrCompletion:
      if CodeUsingNativeService then CodeSearchStatus.Caption := IntToStr(Length(CodeEditor.Completions)) + ' compiler suggestions'
      else CodeSearchStatus.Caption := IntToStr(Length(CodeEditor.Completions)) + ' suggestions from example provider';
    lrSignatureHelp: CodeSearchStatus.Caption := 'Parameter hints - Ctrl+Shift+Space; Alt+Up/Down overload; Esc closes';
    lrHover: CodeSearchStatus.Caption := CodeEditor.HoverText;
    lrDefinition:
      if CodeUsingNativeService then CodeSearchStatus.Caption := 'Compiler definition request completed'
      else CodeSearchStatus.Caption := 'Definition request completed (example provider)';
  end;
end;
procedure TShowcaseForm.CodeNavigated(Sender: TObject; const Target: TERDDefinitionTarget);
begin CodeSearchStatus.Caption := 'Definition at UTF-16 offset ' + IntToStr(Target.Start); end;

procedure TShowcaseForm.CodeSearchChanged(Sender: TObject);
var Options: TERDCodeSearchOptions;
begin
  if CodeEditor = nil then Exit;
  Options := [];
  if CodeSearchRegex.Checked then Include(Options, soRegex);
  if CodeSearchCase.Checked then Include(Options, soMatchCase);
  if CodeSearchWord.Checked then Include(Options, soWholeWord);
  CodeEditor.SetSearch(CodeSearchQuery.Text, Options); CodeDocumentChanged(Self);
end;
procedure TShowcaseForm.CodeDocumentChanged(Sender: TObject);
begin
  if CodeSearchStatus = nil then Exit;
  if CodeEditor.SearchError <> '' then CodeSearchStatus.Caption := CodeEditor.SearchError
  else CodeSearchStatus.Caption := IntToStr(Length(CodeEditor.SearchMatches)) + ' matches - F3 / Shift+F3; Ctrl+F / Ctrl+H';
end;
procedure TShowcaseForm.CodeSearchClicked(Sender: TObject);
begin
  case TERDButton(Sender).Tag of
    0: CodeEditor.FindNext;
    1: CodeEditor.FindNext(True);
    2: begin CodeEditor.ReplaceCurrent(CodeSearchReplacement.Text); CodeEditor.FindNext; end;
    3: CodeEditor.ReplaceAll(CodeSearchReplacement.Text);
  end;
  CodeDocumentChanged(Self); CodeEditor.SetFocus;
end;
procedure TShowcaseForm.CodeSearchRequested(Sender: TObject; Replacing: Boolean);
begin
  if Replacing then begin CodeSearchReplacement.SetFocus; CodeSearchReplacement.SelectAll; end
  else
  begin
    if (CodeEditor.SelLength > 0) and (Pos(#10, CodeEditor.SelText) = 0) then
      CodeSearchQuery.Text := CodeEditor.SelText;
    CodeSearchQuery.SetFocus; CodeSearchQuery.SelectAll;
  end;
end;

procedure TShowcaseForm.CodeDiagnosticsClicked(Sender: TObject);
var Items: TArray<TERDCodeDiagnostic>; Source: UnicodeString; At: Integer;
begin
  Source := CodeEditor.Text; At := Pos('command.run()', Source);
  if At = 0 then Exit;
  SetLength(Items, 1); Items[0].Start := At - 1; Items[0].Count := Length('command.run()');
  Items[0].Severity := dsWarning; Items[0].Source := 'Showcase host';
  Items[0].Code := 'DEMO001';
  Items[0].MessageText := 'Example host diagnostic: edits clear stale ranges.';
  CodeEditor.PublishDiagnostics(Source, Items);
  CodeEditor.SelStart := At - 1; CodeEditor.SetFocus;
end;

procedure TShowcaseForm.CodeFoldClicked(Sender: TObject);
begin
  if TERDButton(Sender).Tag = 1 then CodeEditor.FoldAll
  else CodeEditor.UnfoldAll;
  CodeEditor.SetFocus;
end;

procedure TShowcaseForm.CreateUnicodePreviews;
var Preview: TERDTextInput;
  procedure Configure(AInput: TERDTextInput; const AName: string; ATop, ATabOrder: Integer);
  begin
    AInput.Name := AName;
    AInput.Parent := InputsScroll;
    AInput.SetBounds(MulDiv(16, CurrentPPI, 96), MulDiv(ATop, CurrentPPI, 96),
      MulDiv(460, CurrentPPI, 96), MulDiv(28, CurrentPPI, 96));
    AInput.TabOrder := ATabOrder;
  end;
begin
  // Also accept forms saved before the previews became runtime-only.
  if UnicodeTextPreview = nil then
  begin
    Preview := FindComponent('UnicodeTextPreview') as TERDTextInput;
    if Preview = nil then
    begin
      Preview := TERDTextInput.Create(Self);
      Configure(Preview, 'UnicodeTextPreview', 42, 0);
      Preview.Text := 'Caf' + #$00E9 + ' ' + #$D83D#$DC4D#$D83C#$DFFD +
        ' ' + #$D83C#$DDE7#$D83C#$DDEA + ' ' + #$D83D#$DC69#$200D#$D83D#$DCBB + ' ' + #$D83D#$DE00 + ' ' + #$D83D#$DE80;
    end;
    UnicodeTextPreview := Preview;
  end;
  if UnicodePasswordPreview = nil then
  begin
    Preview := FindComponent('UnicodePasswordPreview') as TERDTextInput;
    if Preview = nil then
    begin
      Preview := TERDTextInput.Create(Self);
      Configure(Preview, 'UnicodePasswordPreview', 82, 1);
      Preview.PasswordChar := #$2022;
      Preview.Text := 'Unicode ' + #$D83D#$DD13;
    end;
    UnicodePasswordPreview := Preview;
  end;
end;

procedure TShowcaseForm.FormCreate(Sender: TObject);
var Bitmap: TBitmap;
begin
  CreateUnicodePreviews;
  CreateCodePreview;
  PCBPage := TPCBPreview.Create(Self); PCBPage.Parent := DemoPages;
  PCBPage.Align := alClient; PCBPage.Visible := False; PCBPage.AttachRibbon(Ribbon);
  PageTabs.Tabs.Add.Caption := 'PCB designer';
  DebugPage := TDebugPreview.Create(Self); DebugPage.Parent := DemoPages;
  DebugPage.Align := alClient; DebugPage.Visible := False; DebugPage.AttachRibbon(Ribbon);
  PageTabs.Tabs.Add.Caption := 'Debugger components';
  RuntimePage := TRuntimePreview.Create(Self); RuntimePage.Parent := DemoPages;
  RuntimePage.Align := alClient; RuntimePage.Visible := False; RuntimePage.AttachRibbon(Ribbon);
  PageTabs.Tabs.Add.Caption := 'Runtime monitor';
  ConsolePage := TConsolePreview.Create(Self); ConsolePage.Parent := DemoPages;
  ConsolePage.Align := alClient; ConsolePage.Visible := False; ConsolePage.AttachRibbon(Ribbon);
  PageTabs.Tabs.Add.Caption := 'Debug console';
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

procedure TShowcaseForm.RibbonInputChanged(Sender: TObject);
begin
  if StatusBar = nil then Exit;
  if Sender = RibbonSearch then StatusBar.Panels[0].Text := 'Search: ' + RibbonSearch.Text
  else if Sender = RibbonCombo then StatusBar.Panels[0].Text := 'Style: ' + RibbonCombo.Text
  else if Sender = RibbonSpin then StatusBar.Panels[0].Text := 'Size: ' + RibbonSpin.Text;
end;

procedure TShowcaseForm.RibbonGalleryChanged(Sender: TObject);
begin
  if (StatusBar = nil) or (RibbonGallery.ItemIndex < 0) then Exit;
  RibbonCombo.Text := RibbonGallery.Items[RibbonGallery.ItemIndex].Caption;
  StatusBar.Panels[0].Text := 'Gallery: ' + RibbonCombo.Text;
end;

procedure TShowcaseForm.TreeGetText(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var Data: PPlaylistDemoData;
begin
  if not (vsInitialized in Node.States) then Sender.ValidateNode(Node, False);
  Data := Sender.GetNodeData(Node);
  if (Column >= 0) and (Column <= 2) and Data.Edited[Column] then
  begin CellText := Data.Values[Column]; Exit end;
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

procedure TShowcaseForm.TreeNewText(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const NewText: string);
var Data: PPlaylistDemoData;
begin
  if (Column < 0) or (Column > 2) then Exit;
  Data := Sender.GetNodeData(Node);
  Data.Values[Column] := NewText;
  Data.Edited[Column] := True;
end;

procedure TShowcaseForm.TreeNodeDataSize(Sender: TERDVTBaseVirtualTree; var NodeDataSize: Integer);
begin NodeDataSize := SizeOf(TPlaylistDemoData) end;

procedure TShowcaseForm.TreeFreeNode(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode);
var Data: PPlaylistDemoData;
begin
  Data := Sender.GetNodeData(Node);
  if Data <> nil then Finalize(Data^);
end;

procedure TShowcaseForm.TreeCellEditor(Sender: TObject; Node: PVirtualNode;
  Column: TColumnIndex; var Kind: TERDTreeEditorKind; Choices: TStrings);
begin
  Kind := tekText;
  if Column = 1 then
  begin Kind := tekCombo; Choices.Add('Group'); Choices.Add('Live TV'); Choices.Add('VOD') end;
  if Column = 2 then
  begin Kind := tekCombo; Choices.Add('SD'); Choices.Add('HD'); Choices.Add('4K') end;
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
  if CodePage <> nil then CodePage.Visible := PageTabs.TabIndex = 6;
  if PCBPage <> nil then
  begin PCBPage.Visible := PageTabs.TabIndex = 7; PCBPage.SetRibbonActive(PCBPage.Visible); end;
  if DebugPage <> nil then begin DebugPage.Visible := PageTabs.TabIndex = 8; DebugPage.SetRibbonActive(DebugPage.Visible); end;
  if RuntimePage <> nil then begin RuntimePage.Visible := PageTabs.TabIndex = 9; RuntimePage.SetRibbonActive(RuntimePage.Visible); end;
  if ConsolePage <> nil then begin ConsolePage.Visible := PageTabs.TabIndex = 10; ConsolePage.SetRibbonActive(ConsolePage.Visible); end;
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
procedure TShowcaseForm.BreadcrumbChoices(Sender: TObject; const Path: string; Choices: TERDPlaces);
  procedure AddChoice(const Caption, Id: string);
  var Item: TERDPlace;
  begin
    Item := Choices.Add;
    Item.Caption := Caption;
    Item.Path := Id;
  end;
begin
  if Path = 'playlists' then
  begin
    AddChoice('Belgium', 'country:be');
    AddChoice('Netherlands', 'country:nl');
  end
  else if Copy(Path, 1, 8) = 'country:' then
  begin
    AddChoice('Live TV', 'live');
    AddChoice('VOD', 'vod');
    AddChoice('Series', 'series');
  end
  else if Path = 'live' then
  begin
    AddChoice('Sports', 'sports');
    AddChoice('News', 'news');
    AddChoice('Music', 'music');
  end;
end;
procedure TShowcaseForm.BreadcrumbNavigate(Sender: TObject; const Path: string);
var Country: string;
  procedure AddPart(const Caption, Id: string);
  var Item: TERDPlace;
  begin
    Item := DemoBreadcrumb.Items.Add;
    Item.Caption := Caption;
    Item.Path := Id;
  end;
begin
  Country := 'country:be';
  if DemoBreadcrumb.Items.Count > 1 then Country := DemoBreadcrumb.Items[1].Path;
  if Copy(Path, 1, 8) = 'country:' then Country := Path;
  DemoBreadcrumb.Items.BeginUpdate;
  try
    DemoBreadcrumb.Items.Clear;
    AddPart('Playlists', 'playlists');
    if Path <> 'playlists' then
    begin
      if Country = 'country:nl' then AddPart('Netherlands', Country) else AddPart('Belgium', Country);
      if Path = 'live' then AddPart('Live TV', 'live')
      else if Path = 'vod' then AddPart('VOD', 'vod')
      else if Path = 'series' then AddPart('Series', 'series')
      else if (Path = 'sports') or (Path = 'news') or (Path = 'music') then
      begin
        AddPart('Live TV', 'live');
        if Path = 'sports' then AddPart('Sports', Path)
        else if Path = 'news' then AddPart('News', Path) else AddPart('Music', Path);
      end;
    end;
  finally DemoBreadcrumb.Items.EndUpdate end;
  StatusBar.Panels[0].Text := 'Navigate: ' + Path;
end;
procedure TShowcaseForm.FilesDropped(Sender: TObject; Files: TStrings);
begin
  StatusBar.Panels[0].Text := Format('%d file(s): %s', [Files.Count, ExtractFileName(Files[0])]);
end;
procedure TShowcaseForm.BrowseDropFiles(Sender: TObject);
begin
  if DemoOpenDialog.Execute then DemoDropZone.DropFiles(DemoOpenDialog.Files);
end;

procedure TShowcaseForm.ShowcaseKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('K')) and (Shift = [ssCtrl]) then
  begin Key := 0; OpenCommands(Self) end;
end;
procedure TShowcaseForm.OpenCommands(Sender: TObject);
begin
  DemoCommandPalette.Execute;
end;
procedure TShowcaseForm.CommandExecuted(Sender: TObject; Command: TERDCommandItem);
begin
  if Command.Key = 'theme' then ToggleTheme(Self)
  else if Command.Key = 'open' then OpenFile(Self)
  else if Command.Key = 'dialog' then ShowDialog(Self);
  StatusBar.Panels[0].Text := 'Command: ' + Command.DisplayCaption;
end;

procedure TShowcaseForm.ValidateExample(Sender: TObject; const Text: string;
  var State: TERDEditValidationState; var MessageText: string);
begin
  if Trim(Text) = '' then begin State := evsError; MessageText := 'A name is required.' end
  else if Length(Trim(Text)) < 4 then begin State := evsWarning; MessageText := 'Use at least four characters.' end
  else begin State := evsSuccess; MessageText := 'The name is valid.' end;
end;

procedure TShowcaseForm.DocumentEdited(Sender: TObject);
var Control: TControl; I: Integer;
begin
  Control := TControl(Sender);
  while Control <> nil do
  begin
    for I := 0 to DemoTabs.Tabs.Count - 1 do
      if DemoTabs.Tabs[I].Page = Control then
      begin DemoTabs.Tabs[I].Modified := True; Exit end;
    Control := Control.Parent;
  end;
end;

procedure TShowcaseForm.NewDocumentTab(Sender: TObject);
var Page: TERDPanel; Edit: TERDEdit; Text: TERDDialogText; Item: TERDTabItem;
begin
  Page := TERDPanel.Create(Self);
  Page.Parent := DocumentHost;
  Page.Align := alClient;
  Page.BorderWidth := 0;
  Page.Visible := False;
  Text := TERDDialogText.Create(Page);
  Text.Parent := Page;
  Text.SetBounds(16, 16, 420, 40);
  Text.Caption := 'Drag this tab to reorder or outside the strip to detach.';
  Edit := TERDEdit.Create(Page);
  Edit.Parent := Page;
  Edit.SetBounds(16, 76, 400, 32);
  Edit.Text := 'Editable document content';
  Edit.OnChange := DocumentEdited;
  Item := DemoTabs.Tabs.Add;
  Item.Caption := 'New document ' + IntToStr(Item.ID + 1);
  Item.Page := Page;
  DemoTabs.TabIndex := Item.Index;
end;
procedure TShowcaseForm.DocumentTabClosing(Sender: TObject; Index: Integer; var CanClose: Boolean);
var Dialog: TERDDialog;
begin
  if not DemoTabs.Tabs[Index].Modified then Exit;
  Dialog := TERDDialog.Create(Self);
  try
    Dialog.Theme := Theme;
    Dialog.Title := 'Close modified document?';
    Dialog.Text := 'This demo tab is marked as modified. Close it?';
    Dialog.Kind := dkWarning;
    Dialog.Buttons := [mbYes, mbNo];
    Dialog.DefaultButton := mbNo;
    CanClose := Dialog.Execute = mrYes;
  finally Dialog.Free end;
end;
procedure TShowcaseForm.DocumentTabClosed(Sender: TObject; TabID: Integer; Page: TWinControl);
begin
  Page.Free;
  StatusBar.Panels[0].Text := 'Document closed';
end;
procedure TShowcaseForm.DocumentTabReordered(Sender: TObject; OldIndex, NewIndex: Integer);
begin
  StatusBar.Panels[0].Text := Format('Tab moved: %d -> %d', [OldIndex + 1, NewIndex + 1]);
end;
procedure TShowcaseForm.DocumentTabDetached(Sender: TObject; Index: Integer; Window: TCustomForm);
begin
  StatusBar.Panels[0].Text := 'Detached: ' + DemoTabs.Tabs[Index].Caption;
end;
procedure TShowcaseForm.DocumentTabAttached(Sender: TObject; Index: Integer);
begin
  StatusBar.Panels[0].Text := 'Returned: ' + DemoTabs.Tabs[Index].Caption;
end;

procedure TShowcaseForm.SearchComboChanged(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Format('Combo: %s (Items index %d)', [SearchCombo.Text, SearchCombo.ItemIndex]);
end;

procedure TShowcaseForm.RatingChanged(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Format('Rating: %.1f / %d', [DemoRating.Value, DemoRating.MaxStars]);
end;

procedure TShowcaseForm.LoadingStyleChanged(Sender: TObject);
begin
  if LoadingStyleChoice.ItemIndex >= 0 then
    DemoLoadingIndicator.Style := TERDLoadingIndicatorStyle(LoadingStyleChoice.ItemIndex);
end;
procedure TShowcaseForm.LoadingSizeChanged(Sender: TObject);
const Sizes: array[0..4] of Integer = (16, 24, 32, 48, 64);
begin
  if (LoadingSizeChoice.ItemIndex >= 0) and (LoadingSizeChoice.ItemIndex <= High(Sizes)) then
    DemoLoadingIndicator.IndicatorSize := Sizes[LoadingSizeChoice.ItemIndex];
end;
procedure TShowcaseForm.LoadingChanged(Sender: TObject);
begin
  DemoLoadingIndicator.Active := LoadingToggle.Checked;
end;

procedure TShowcaseForm.LinkClicked(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Link clicked: ' + DemoLinkLabel.URL;
end;

procedure TShowcaseForm.DateTimeChanged(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Date/time: ' + DemoDateTimePicker.DisplayText;
end;

procedure TShowcaseForm.TimeChanged(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Time: ' + TERDTimePicker(Sender).DisplayText;
end;

procedure TShowcaseForm.CheckListChanged(Sender: TObject; Index: Integer);
begin
  StatusBar.Panels[0].Text := Format('%s: %d checked',
    [DemoCheckListBox.Items[Index].Caption, DemoCheckListBox.CheckedCount]);
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
