//------------------------------------------------------------------------------
//  ERD.UI.Ribbon
//
//  Themed ribbon for the OBD Studio application chrome.
//
//    TOBDRibbon                 tabs, contextual tabs, command groups,
//                               command search, File backstage button,
//                               collapsed and simplified modes.
//    TOBDRibbonTab              a normal or contextual tab with groups.
//    TOBDRibbonGroup            captioned group with large and small items.
//    TOBDRibbonItem             action-aware button, toggle, drop-down or
//                               split command.
//    TOBDContextualTabSet       coloured set of contextual tabs.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.Ribbon;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.StrUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ActnList,
  Vcl.AppEvnts,
  Winapi.GDIPAPI,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Menus;

type
  TOBDRibbon = class;
  TOBDRibbonTab = class;
  TOBDRibbonGroup = class;
  TOBDRibbonItem = class;
  TOBDContextualTabSet = class;

  /// <summary>Visual size of a ribbon item.</summary>
  TOBDRibbonItemSize = (
    /// <summary>Large stacked button in the classic body.</summary>
    risLarge,
    /// <summary>Small row button used in columns of three.</summary>
    risSmall);

  /// <summary>Interaction kind of a ribbon item.</summary>
  TOBDRibbonItemKind = (
    /// <summary>Command button.</summary>
    rikButton,
    /// <summary>Checked command button.</summary>
    rikToggle,
    /// <summary>Button that opens a menu.</summary>
    rikDropDown,
    /// <summary>Command with a separate drop-down area.</summary>
    rikSplit);

  /// <summary>Ribbon item accent colour.</summary>
  TOBDRibbonItemColor = (
    /// <summary>Theme foreground and accent colours.</summary>
    ricNormal,
    /// <summary>Accent command.</summary>
    ricAccent,
    /// <summary>Danger command.</summary>
    ricDanger);

  /// <summary>Contextual tab set colour source.</summary>
  TOBDContextColor = (
    /// <summary>Theme accent colour.</summary>
    ccAccent,
    /// <summary>Theme success colour.</summary>
    ccSuccess,
    /// <summary>Theme warning colour.</summary>
    ccWarning,
    /// <summary>Theme danger colour.</summary>
    ccDanger,
    /// <summary>CustomColor.</summary>
    ccCustom);

  /// <summary>Ribbon body layout style.</summary>
  TOBDRibbonStyle = (
    /// <summary>Classic ribbon with tall grouped body.</summary>
    rsClassic,
    /// <summary>Single row of small commands with overflow.</summary>
    rsSimplified);

  /// <summary>Fires when a ribbon tab changes.</summary>
  /// <param name="Sender">Ribbon.</param>
  /// <param name="ATab">New active tab.</param>
  TOBDRibbonTabEvent = procedure(Sender: TObject; ATab: TOBDRibbonTab)
    of object;

  /// <summary>Fires when a ribbon item is activated.</summary>
  /// <param name="Sender">Ribbon.</param>
  /// <param name="AItem">Activated item.</param>
  TOBDRibbonItemEvent = procedure(Sender: TObject; AItem: TOBDRibbonItem)
    of object;

  /// <summary>Owned collection of ribbon items.</summary>
  TOBDRibbonItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDRibbonItem;
    procedure SetItem(AIndex: Integer; AValue: TOBDRibbonItem);
  protected
    /// <summary>Notifies the ribbon when an item changes.</summary>
    /// <param name="Item">Changed item, or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates an item collection.</summary>
    /// <param name="AOwner">Owning group.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a ribbon item.</summary>
    /// <returns>New item.</returns>
    function Add: TOBDRibbonItem;
    /// <summary>Indexed ribbon items.</summary>
    property Items[AIndex: Integer]: TOBDRibbonItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Owned collection of ribbon groups.</summary>
  TOBDRibbonGroups = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDRibbonGroup;
    procedure SetItem(AIndex: Integer; AValue: TOBDRibbonGroup);
  protected
    /// <summary>Notifies the ribbon when a group changes.</summary>
    /// <param name="Item">Changed item, or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a group collection.</summary>
    /// <param name="AOwner">Owning tab.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a group.</summary>
    /// <returns>New group.</returns>
    function Add: TOBDRibbonGroup;
    /// <summary>Indexed groups.</summary>
    property Items[AIndex: Integer]: TOBDRibbonGroup read GetItem
      write SetItem; default;
  end;

  /// <summary>Owned collection of ribbon tabs.</summary>
  TOBDRibbonTabs = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDRibbonTab;
    procedure SetItem(AIndex: Integer; AValue: TOBDRibbonTab);
  protected
    /// <summary>Notifies the ribbon when a tab changes.</summary>
    /// <param name="Item">Changed item, or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a tab collection.</summary>
    /// <param name="AOwner">Owning ribbon or contextual set.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a tab.</summary>
    /// <returns>New tab.</returns>
    function Add: TOBDRibbonTab;
    /// <summary>Indexed tabs.</summary>
    property Items[AIndex: Integer]: TOBDRibbonTab read GetItem
      write SetItem; default;
  end;

  /// <summary>Owned collection of contextual tab sets.</summary>
  TOBDContextualTabSets = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDContextualTabSet;
    procedure SetItem(AIndex: Integer; AValue: TOBDContextualTabSet);
  protected
    /// <summary>Notifies the ribbon when a contextual set changes.</summary>
    /// <param name="Item">Changed item, or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a contextual set collection.</summary>
    /// <param name="AOwner">Owning ribbon.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a contextual tab set.</summary>
    /// <returns>New set.</returns>
    function Add: TOBDContextualTabSet;
    /// <summary>Indexed contextual sets.</summary>
    property Items[AIndex: Integer]: TOBDContextualTabSet read GetItem
      write SetItem; default;
  end;

  /// <summary>One command in a ribbon group.</summary>
  TOBDRibbonItem = class(TCollectionItem)
  strict private
    FAction: TBasicAction;
    FActionLink: TActionLink;
    FCaption: TCaption;
    FHint: string;
    FGlyph: TOBDGlyph;
    FImageIndex: Integer;
    FSize: TOBDRibbonItemSize;
    FKind: TOBDRibbonItemKind;
    FDropDownMenu: TPopupMenu;
    FDown: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    FColor: TOBDRibbonItemColor;
    FTag: NativeInt;
    FOnClick: TNotifyEvent;
    FRect: TRect;
    FDropRect: TRect;
    FOverflow: Boolean;
    procedure SetAction(AValue: TBasicAction);
    procedure SetCaption(const AValue: TCaption);
    procedure SetHint(const AValue: string);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetSize(AValue: TOBDRibbonItemSize);
    procedure SetKind(AValue: TOBDRibbonItemKind);
    procedure SetDropDownMenu(AValue: TPopupMenu);
    procedure SetDown(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetColor(AValue: TOBDRibbonItemColor);
    procedure ItemChanged;
  protected
    /// <summary>Returns the caption in the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an enabled visible ribbon item.</summary>
    /// <param name="Collection">Owning collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees action links.</summary>
    destructor Destroy; override;
    /// <summary>Copies a ribbon item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Executes the item action and OnClick.</summary>
    procedure Execute;
  published
    /// <summary>Optional action that supplies caption, enabled, checked,
    /// visible, hint, image index and execute behaviour.</summary>
    property Action: TBasicAction read FAction write SetAction;
    /// <summary>Text shown on the ribbon item.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>Hint shown over the item.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Built-in line glyph used when ImageIndex is -1.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image list index; -1 uses Glyph.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Large or small ribbon layout.</summary>
    property Size: TOBDRibbonItemSize read FSize write SetSize default risSmall;
    /// <summary>Button, toggle, drop-down or split command.</summary>
    property Kind: TOBDRibbonItemKind read FKind write SetKind default rikButton;
    /// <summary>Menu opened by drop-down and split items.</summary>
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    /// <summary>Checked state for toggle commands.</summary>
    property Down: Boolean read FDown write SetDown default False;
    /// <summary>Whether the item can be activated.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Whether the item participates in layout.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Normal, accent or danger colouring.</summary>
    property Color: TOBDRibbonItemColor read FColor write SetColor
      default ricNormal;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
    /// <summary>Fires when the item is activated.</summary>
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  /// <summary>One captioned group in a ribbon tab.</summary>
  TOBDRibbonGroup = class(TCollectionItem)
  strict private
    FCaption: TCaption;
    FVisible: Boolean;
    FShowLauncher: Boolean;
    FItems: TOBDRibbonItems;
    FTag: NativeInt;
    FOnLauncherClick: TNotifyEvent;
    FRect: TRect;
    FLauncherRect: TRect;
    procedure SetCaption(const AValue: TCaption);
    procedure SetVisible(AValue: Boolean);
    procedure SetShowLauncher(AValue: Boolean);
    procedure SetItems(AValue: TOBDRibbonItems);
    procedure GroupChanged;
  protected
    /// <summary>Returns the caption in the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a visible group with an item collection.</summary>
    /// <param name="Collection">Owning collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees the item collection.</summary>
    destructor Destroy; override;
    /// <summary>Copies a ribbon group.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Invokes OnLauncherClick.</summary>
    procedure ClickLauncher;
  published
    /// <summary>Caption centered below the group.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>Whether the group participates in layout.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Shows the bottom-right dialog launcher glyph.</summary>
    property ShowLauncher: Boolean read FShowLauncher write SetShowLauncher
      default False;
    /// <summary>Commands in this group.</summary>
    property Items: TOBDRibbonItems read FItems write SetItems;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
    /// <summary>Fires when the launcher is clicked.</summary>
    property OnLauncherClick: TNotifyEvent read FOnLauncherClick
      write FOnLauncherClick;
  end;

  /// <summary>One normal or contextual ribbon tab.</summary>
  TOBDRibbonTab = class(TCollectionItem)
  strict private
    FCaption: TCaption;
    FVisible: Boolean;
    FGroups: TOBDRibbonGroups;
    FTag: NativeInt;
    FRect: TRect;
    FContextColor: TColor;
    FIsContextual: Boolean;
    procedure SetCaption(const AValue: TCaption);
    procedure SetVisible(AValue: Boolean);
    procedure SetGroups(AValue: TOBDRibbonGroups);
    procedure TabChanged;
  protected
    /// <summary>Returns the caption in the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a visible tab with a group collection.</summary>
    /// <param name="Collection">Owning collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees the group collection.</summary>
    destructor Destroy; override;
    /// <summary>Copies a ribbon tab.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Tab caption.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>Whether the tab participates in layout.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Groups shown when this tab is active.</summary>
    property Groups: TOBDRibbonGroups read FGroups write SetGroups;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
  end;

  /// <summary>A coloured group of contextual tabs.</summary>
  TOBDContextualTabSet = class(TCollectionItem)
  strict private
    FCaption: TCaption;
    FColor: TOBDContextColor;
    FCustomColor: TColor;
    FVisible: Boolean;
    FTabs: TOBDRibbonTabs;
    FTag: NativeInt;
    procedure SetCaption(const AValue: TCaption);
    procedure SetColor(AValue: TOBDContextColor);
    procedure SetCustomColor(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
    procedure SetTabs(AValue: TOBDRibbonTabs);
    procedure SetChanged;
  protected
    /// <summary>Returns the caption in the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a hidden contextual set with tabs.</summary>
    /// <param name="Collection">Owning collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees the tab collection.</summary>
    destructor Destroy; override;
    /// <summary>Copies a contextual tab set.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Resolves the set colour from the palette.</summary>
    /// <param name="APalette">Theme palette.</param>
    /// <returns>Colour for the contextual tabs.</returns>
    function ResolveColor(const APalette: TOBDThemePalette): TColor;
  published
    /// <summary>Context caption for the collection editor.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>Theme or custom colour used by this set.</summary>
    property Color: TOBDContextColor read FColor write SetColor default ccAccent;
    /// <summary>Colour used when Color is ccCustom.</summary>
    property CustomColor: TColor read FCustomColor write SetCustomColor
      default clDefault;
    /// <summary>Whether the contextual set is shown.</summary>
    property Visible: Boolean read FVisible write SetVisible default False;
    /// <summary>Tabs shown while this context is visible.</summary>
    property Tabs: TOBDRibbonTabs read FTabs write SetTabs;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
  end;

  /// <summary>Themed application ribbon.</summary>
  TOBDRibbon = class(TOBDCustomControl)
  strict private
    FTabs: TOBDRibbonTabs;
    FContextualTabs: TOBDContextualTabSets;
    FPreviewTabs: TOBDRibbonTabs;
    FPreviewContextualTabs: TOBDContextualTabSets;
    FActiveTab: Integer;
    FShowFileButton: Boolean;
    FFileCaption: TCaption;
    FBackstage: TControl;
    FShowSearch: Boolean;
    FSearchHint: string;
    FRibbonStyle: TOBDRibbonStyle;
    FCollapsed: Boolean;
    FTemporaryExpanded: Boolean;
    FImages: TCustomImageList;
    FHoverTab: TOBDRibbonTab;
    FHoverItem: TOBDRibbonItem;
    FPressedItem: TOBDRibbonItem;
    FFocusedItem: TOBDRibbonItem;
    FHoverLauncher: TOBDRibbonGroup;
    FHoverCollapse: Boolean;
    FFileHover: Boolean;
    FFileDown: Boolean;
    FMoreHover: Boolean;
    FMoreDown: Boolean;
    FFileRect: TRect;
    FSearchRect: TRect;
    FMoreRect: TRect;
    FCollapseRect: TRect;
    FSearchEdit: TEdit;
    FSearchMenu: TOBDPopupMenu;
    FOverflowMenu: TOBDPopupMenu;
    FApplicationEvents: TApplicationEvents;
    FOnTabChange: TOBDRibbonTabEvent;
    FOnFileClick: TNotifyEvent;
    FOnItemClick: TOBDRibbonItemEvent;
    procedure SetTabs(AValue: TOBDRibbonTabs);
    procedure SetContextualTabs(AValue: TOBDContextualTabSets);
    procedure SetActiveTab(AValue: Integer);
    procedure SetShowFileButton(AValue: Boolean);
    procedure SetFileCaption(const AValue: TCaption);
    procedure SetBackstage(AValue: TControl);
    procedure SetShowSearch(AValue: Boolean);
    procedure SetSearchHint(const AValue: string);
    procedure SetRibbonStyle(AValue: TOBDRibbonStyle);
    procedure SetCollapsed(AValue: Boolean);
    procedure SetImages(AValue: TCustomImageList);
    function PreviewMode: Boolean;
    function ActiveTabs: TOBDRibbonTabs;
    function ActiveContextualTabs: TOBDContextualTabSets;
    procedure EnsurePreviewData;
    function FlatTabCount: Integer;
    function TabByFlatIndex(AIndex: Integer): TOBDRibbonTab;
    function FlatIndexOfTab(ATab: TOBDRibbonTab): Integer;
    function FirstVisibleTab: Integer;
    function BodyVisible: Boolean;
    function TabHeight: Integer;
    function ClassicBodyHeight: Integer;
    function SimplifiedBodyHeight: Integer;
    function BodyHeight: Integer;
    function DesiredHeight: Integer;
    procedure UpdateHeight;
    procedure LayoutSearchEdit;
    procedure RibbonChanged;
    procedure NormalizeActiveTab;
    procedure DoTabChange;
    procedure DrawTabRow(APainter: TOBDPainter);
    procedure DrawBody(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawClassicBody(APainter: TOBDPainter; ACanvas: TCanvas;
      ATab: TOBDRibbonTab; ATop: Integer);
    procedure DrawSimplifiedBody(APainter: TOBDPainter; ACanvas: TCanvas;
      ATab: TOBDRibbonTab; ATop: Integer);
    function DrawLargeItem(APainter: TOBDPainter; ACanvas: TCanvas;
      AItem: TOBDRibbonItem; X, Y, H: Integer): Integer;
    function DrawSmallItem(APainter: TOBDPainter; ACanvas: TCanvas;
      AItem: TOBDRibbonItem; X, Y, H: Integer): Integer;
    procedure DrawItemGlyph(APainter: TOBDPainter; ACanvas: TCanvas;
      AItem: TOBDRibbonItem; CX, CY: Integer; AColor: TColor;
      AScale: Single);
    procedure DrawChevron(APainter: TOBDPainter; CX, CY: Integer;
      ADown: Boolean; AColor: TColor);
    procedure DrawLauncher(APainter: TOBDPainter; CX, CY: Integer;
      AColor: TColor);
    procedure ClearLayout;
    function HitTab(X, Y: Integer): TOBDRibbonTab;
    function HitItem(X, Y: Integer): TOBDRibbonItem;
    function HitLauncher(X, Y: Integer): TOBDRibbonGroup;
    function ItemHintAt(X, Y: Integer): string;
    procedure ExecuteItem(AItem: TOBDRibbonItem; AFromMenu: Boolean);
    procedure PopupItemMenu(AItem: TOBDRibbonItem);
    procedure PopupMenuAt(AMenu: TPopupMenu; const AScreenRect: TRect);
    procedure ClickFile;
    procedure ToggleCollapsed;
    procedure FocusSearch;
    procedure SearchChanged(Sender: TObject);
    procedure SearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SearchMenuClick(Sender: TObject);
    procedure OverflowMenuClick(Sender: TObject);
    procedure PopulateCommandMenu(AMenu: TPopupMenu; const AFilter: string;
      AOnlyOverflow: Boolean);
    procedure AddCommandsFromTabs(AMenu: TPopupMenu; ATabs: TOBDRibbonTabs;
      const AFilter: string; AOnlyOverflow: Boolean);
    procedure AppShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Clears component references when they are freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints tabs and body.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Updates child layout.</summary>
    procedure Resize; override;
    /// <summary>Updates hover state.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Handles tab, command and File clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Finishes pressed item handling.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Keyboard navigation and shortcuts.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Re-sizes for a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Creates the ribbon with alTop alignment.</summary>
    /// <param name="AOwner">Owner component.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees owned collections and popups.</summary>
    destructor Destroy; override;
    /// <summary>Hides the backstage control when assigned.</summary>
    procedure CloseBackstage;
    /// <summary>Active tab object, or nil.</summary>
    /// <returns>Selected tab.</returns>
    function ActiveTabItem: TOBDRibbonTab;
  published
    /// <summary>Normal ribbon tabs.</summary>
    property Tabs: TOBDRibbonTabs read FTabs write SetTabs;
    /// <summary>Visible contextual tab sets are shown after normal tabs.</summary>
    property ContextualTabs: TOBDContextualTabSets read FContextualTabs
      write SetContextualTabs;
    /// <summary>Selected tab index across normal and visible contextual tabs.</summary>
    property ActiveTab: Integer read FActiveTab write SetActiveTab default 0;
    /// <summary>Shows the accent File button.</summary>
    property ShowFileButton: Boolean read FShowFileButton write SetShowFileButton
      default True;
    /// <summary>Caption of the File backstage button.</summary>
    property FileCaption: TCaption read FFileCaption write SetFileCaption;
    /// <summary>Control shown as a backstage page when File is clicked.</summary>
    property Backstage: TControl read FBackstage write SetBackstage;
    /// <summary>Shows command search on the tab row.</summary>
    property ShowSearch: Boolean read FShowSearch write SetShowSearch
      default True;
    /// <summary>Hint drawn in the command search box.</summary>
    property SearchHint: string read FSearchHint write SetSearchHint;
    /// <summary>Classic or simplified ribbon body.</summary>
    property RibbonStyle: TOBDRibbonStyle read FRibbonStyle write SetRibbonStyle
      default rsClassic;
    /// <summary>When True, only the tab row is normally visible.</summary>
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    /// <summary>Images used by ribbon items when ImageIndex is non-negative.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Focusable with Tab.</summary>
    property TabStop default True;
    /// <summary>Fires after ActiveTab changes.</summary>
    property OnTabChange: TOBDRibbonTabEvent read FOnTabChange write FOnTabChange;
    /// <summary>Fires after File is clicked.</summary>
    property OnFileClick: TNotifyEvent read FOnFileClick write FOnFileClick;
    /// <summary>Fires after a ribbon item is activated.</summary>
    property OnItemClick: TOBDRibbonItemEvent read FOnItemClick write FOnItemClick;
  end;

implementation

const
  LARGE_BODY_HEIGHT = 74;
  GROUP_CAPTION_HEIGHT = 22;
  CLASSIC_BODY_EXTRA = 8;
  TAB_TEXT_SIZE = 13;
  ITEM_TEXT_SIZE = 12;
  SEARCH_WIDTH = 260;

type
  TOBDRibbonActionLink = class(TActionLink)
  strict private
    FClient: TOBDRibbonItem;
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
  end;


{ TOBDRibbonActionLink ------------------------------------------------------- }

procedure TOBDRibbonActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TOBDRibbonItem;
end;

function TOBDRibbonActionLink.IsCaptionLinked: Boolean;
begin
  Result := True;
end;

function TOBDRibbonActionLink.IsCheckedLinked: Boolean;
begin
  Result := True;
end;

function TOBDRibbonActionLink.IsEnabledLinked: Boolean;
begin
  Result := True;
end;

function TOBDRibbonActionLink.IsHintLinked: Boolean;
begin
  Result := True;
end;

function TOBDRibbonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := True;
end;

function TOBDRibbonActionLink.IsVisibleLinked: Boolean;
begin
  Result := True;
end;

procedure TOBDRibbonActionLink.SetCaption(const Value: string);
begin
  if FClient <> nil then
  begin
    FClient.FCaption := Value;
    FClient.ItemChanged;
  end;
end;

procedure TOBDRibbonActionLink.SetChecked(Value: Boolean);
begin
  if FClient <> nil then
  begin
    FClient.FDown := Value;
    FClient.ItemChanged;
  end;
end;

procedure TOBDRibbonActionLink.SetEnabled(Value: Boolean);
begin
  if FClient <> nil then
  begin
    FClient.FEnabled := Value;
    FClient.ItemChanged;
  end;
end;

procedure TOBDRibbonActionLink.SetHint(const Value: string);
begin
  if FClient <> nil then
  begin
    FClient.FHint := Value;
    FClient.ItemChanged;
  end;
end;

procedure TOBDRibbonActionLink.SetImageIndex(Value: Integer);
begin
  if FClient <> nil then
  begin
    FClient.FImageIndex := Value;
    FClient.ItemChanged;
  end;
end;

procedure TOBDRibbonActionLink.SetVisible(Value: Boolean);
begin
  if FClient <> nil then
  begin
    FClient.FVisible := Value;
    FClient.ItemChanged;
  end;
end;

{ Helpers ------------------------------------------------------------------- }

function ItemCaption(AItem: TOBDRibbonItem): string;
begin
  Result := '';
  if AItem = nil then
    Exit;
  Result := AItem.Caption;
  if (Result = '') and (AItem.Action is TContainedAction) then
    Result := TContainedAction(AItem.Action).Caption;
end;

function ItemHint(AItem: TOBDRibbonItem): string;
begin
  Result := '';
  if AItem = nil then
    Exit;
  Result := AItem.Hint;
  if (Result = '') and (AItem.Action is TContainedAction) then
    Result := TContainedAction(AItem.Action).Hint;
end;

function ItemImageIndex(AItem: TOBDRibbonItem): Integer;
begin
  Result := -1;
  if AItem = nil then
    Exit;
  Result := AItem.ImageIndex;
  if (Result < 0) and (AItem.Action is TContainedAction) then
    Result := TContainedAction(AItem.Action).ImageIndex;
end;

function StripAccel(const S: string): string;
begin
  Result := StringReplace(S, '&', '', [rfReplaceAll]);
end;

{ Collections --------------------------------------------------------------- }

constructor TOBDRibbonItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDRibbonItem);
end;

function TOBDRibbonItems.Add: TOBDRibbonItem;
begin
  Result := TOBDRibbonItem(inherited Add);
end;

function TOBDRibbonItems.GetItem(AIndex: Integer): TOBDRibbonItem;
begin
  Result := TOBDRibbonItem(inherited GetItem(AIndex));
end;

procedure TOBDRibbonItems.SetItem(AIndex: Integer; AValue: TOBDRibbonItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDRibbonItems.Update(Item: TCollectionItem);
var
  G: TPersistent;
begin
  inherited;
  G := GetOwner;
  if G is TOBDRibbonGroup then
    TOBDRibbonGroup(G).GroupChanged;
end;

constructor TOBDRibbonGroups.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDRibbonGroup);
end;

function TOBDRibbonGroups.Add: TOBDRibbonGroup;
begin
  Result := TOBDRibbonGroup(inherited Add);
end;

function TOBDRibbonGroups.GetItem(AIndex: Integer): TOBDRibbonGroup;
begin
  Result := TOBDRibbonGroup(inherited GetItem(AIndex));
end;

procedure TOBDRibbonGroups.SetItem(AIndex: Integer; AValue: TOBDRibbonGroup);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDRibbonGroups.Update(Item: TCollectionItem);
var
  T: TPersistent;
begin
  inherited;
  T := GetOwner;
  if T is TOBDRibbonTab then
    TOBDRibbonTab(T).TabChanged;
end;

constructor TOBDRibbonTabs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDRibbonTab);
end;

function TOBDRibbonTabs.Add: TOBDRibbonTab;
begin
  Result := TOBDRibbonTab(inherited Add);
end;

function TOBDRibbonTabs.GetItem(AIndex: Integer): TOBDRibbonTab;
begin
  Result := TOBDRibbonTab(inherited GetItem(AIndex));
end;

procedure TOBDRibbonTabs.SetItem(AIndex: Integer; AValue: TOBDRibbonTab);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDRibbonTabs.Update(Item: TCollectionItem);
var
  O: TPersistent;
begin
  inherited;
  O := GetOwner;
  if O is TOBDRibbon then
    TOBDRibbon(O).RibbonChanged
  else if O is TOBDContextualTabSet then
    TOBDContextualTabSet(O).SetChanged;
end;

constructor TOBDContextualTabSets.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDContextualTabSet);
end;

function TOBDContextualTabSets.Add: TOBDContextualTabSet;
begin
  Result := TOBDContextualTabSet(inherited Add);
end;

function TOBDContextualTabSets.GetItem(AIndex: Integer): TOBDContextualTabSet;
begin
  Result := TOBDContextualTabSet(inherited GetItem(AIndex));
end;

procedure TOBDContextualTabSets.SetItem(AIndex: Integer;
  AValue: TOBDContextualTabSet);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDContextualTabSets.Update(Item: TCollectionItem);
var
  O: TPersistent;
begin
  inherited;
  O := GetOwner;
  if O is TOBDRibbon then
    TOBDRibbon(O).RibbonChanged;
end;

{ TOBDRibbonItem ------------------------------------------------------------ }

constructor TOBDRibbonItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FSize := risSmall;
  FKind := rikButton;
  FEnabled := True;
  FVisible := True;
  FGlyph := glNone;
end;

destructor TOBDRibbonItem.Destroy;
begin
  FActionLink.Free;
  inherited Destroy;
end;

procedure TOBDRibbonItem.Assign(Source: TPersistent);
var
  S: TOBDRibbonItem;
begin
  if Source is TOBDRibbonItem then
  begin
    S := TOBDRibbonItem(Source);
    Action := S.Action;
    Caption := S.Caption;
    Hint := S.Hint;
    Glyph := S.Glyph;
    ImageIndex := S.ImageIndex;
    Size := S.Size;
    Kind := S.Kind;
    DropDownMenu := S.DropDownMenu;
    Down := S.Down;
    Enabled := S.Enabled;
    Visible := S.Visible;
    Color := S.Color;
    Tag := S.Tag;
    OnClick := S.OnClick;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDRibbonItem.ItemChanged;
begin
  inherited Changed(False);
end;

procedure TOBDRibbonItem.SetAction(AValue: TBasicAction);
begin
  if FAction = AValue then
    Exit;
  if FActionLink = nil then
    FActionLink := TOBDRibbonActionLink.Create(Self);
  FAction := AValue;
  FActionLink.Action := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetSize(AValue: TOBDRibbonItemSize);
begin
  if FSize = AValue then
    Exit;
  FSize := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetKind(AValue: TOBDRibbonItemKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetDropDownMenu(AValue: TPopupMenu);
begin
  if FDropDownMenu = AValue then
    Exit;
  FDropDownMenu := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetDown(AValue: Boolean);
begin
  if FDown = AValue then
    Exit;
  FDown := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  ItemChanged;
end;

procedure TOBDRibbonItem.SetColor(AValue: TOBDRibbonItemColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  ItemChanged;
end;

function TOBDRibbonItem.GetDisplayName: string;
begin
  Result := StripAccel(ItemCaption(Self));
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDRibbonItem.Execute;
begin
  if not FEnabled then
    Exit;
  if FKind = rikToggle then
    Down := not Down;
  if FAction <> nil then
    FAction.Execute;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

{ TOBDRibbonGroup ----------------------------------------------------------- }

constructor TOBDRibbonGroup.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FItems := TOBDRibbonItems.Create(Self);
end;

destructor TOBDRibbonGroup.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TOBDRibbonGroup.Assign(Source: TPersistent);
var
  S: TOBDRibbonGroup;
begin
  if Source is TOBDRibbonGroup then
  begin
    S := TOBDRibbonGroup(Source);
    Caption := S.Caption;
    Visible := S.Visible;
    ShowLauncher := S.ShowLauncher;
    Items := S.Items;
    Tag := S.Tag;
    OnLauncherClick := S.OnLauncherClick;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDRibbonGroup.GroupChanged;
begin
  inherited Changed(False);
end;

procedure TOBDRibbonGroup.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  GroupChanged;
end;

procedure TOBDRibbonGroup.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  GroupChanged;
end;

procedure TOBDRibbonGroup.SetShowLauncher(AValue: Boolean);
begin
  if FShowLauncher = AValue then
    Exit;
  FShowLauncher := AValue;
  GroupChanged;
end;

procedure TOBDRibbonGroup.SetItems(AValue: TOBDRibbonItems);
begin
  FItems.Assign(AValue);
end;

function TOBDRibbonGroup.GetDisplayName: string;
begin
  Result := StripAccel(FCaption);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDRibbonGroup.ClickLauncher;
begin
  if Assigned(FOnLauncherClick) then
    FOnLauncherClick(Self);
end;

{ TOBDRibbonTab ------------------------------------------------------------- }

constructor TOBDRibbonTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FGroups := TOBDRibbonGroups.Create(Self);
end;

destructor TOBDRibbonTab.Destroy;
begin
  FGroups.Free;
  inherited Destroy;
end;

procedure TOBDRibbonTab.Assign(Source: TPersistent);
var
  S: TOBDRibbonTab;
begin
  if Source is TOBDRibbonTab then
  begin
    S := TOBDRibbonTab(Source);
    Caption := S.Caption;
    Visible := S.Visible;
    Groups := S.Groups;
    Tag := S.Tag;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDRibbonTab.TabChanged;
begin
  inherited Changed(False);
end;

procedure TOBDRibbonTab.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  TabChanged;
end;

procedure TOBDRibbonTab.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  TabChanged;
end;

procedure TOBDRibbonTab.SetGroups(AValue: TOBDRibbonGroups);
begin
  FGroups.Assign(AValue);
end;

function TOBDRibbonTab.GetDisplayName: string;
begin
  Result := StripAccel(FCaption);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TOBDContextualTabSet ------------------------------------------------------ }

constructor TOBDContextualTabSet.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := ccAccent;
  FCustomColor := clDefault;
  FVisible := False;
  FTabs := TOBDRibbonTabs.Create(Self);
end;

destructor TOBDContextualTabSet.Destroy;
begin
  FTabs.Free;
  inherited Destroy;
end;

procedure TOBDContextualTabSet.Assign(Source: TPersistent);
var
  S: TOBDContextualTabSet;
begin
  if Source is TOBDContextualTabSet then
  begin
    S := TOBDContextualTabSet(Source);
    Caption := S.Caption;
    Color := S.Color;
    CustomColor := S.CustomColor;
    Visible := S.Visible;
    Tabs := S.Tabs;
    Tag := S.Tag;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDContextualTabSet.SetChanged;
begin
  inherited Changed(False);
end;

procedure TOBDContextualTabSet.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  SetChanged;
end;

procedure TOBDContextualTabSet.SetColor(AValue: TOBDContextColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  SetChanged;
end;

procedure TOBDContextualTabSet.SetCustomColor(AValue: TColor);
begin
  if FCustomColor = AValue then
    Exit;
  FCustomColor := AValue;
  SetChanged;
end;

procedure TOBDContextualTabSet.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  SetChanged;
end;

procedure TOBDContextualTabSet.SetTabs(AValue: TOBDRibbonTabs);
begin
  FTabs.Assign(AValue);
end;

function TOBDContextualTabSet.GetDisplayName: string;
begin
  Result := StripAccel(FCaption);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TOBDContextualTabSet.ResolveColor(
  const APalette: TOBDThemePalette): TColor;
begin
  case FColor of
    ccSuccess:
      Result := APalette.Success;
    ccWarning:
      Result := APalette.Warning;
    ccDanger:
      Result := APalette.Danger;
    ccCustom:
      if FCustomColor <> clDefault then
        Result := FCustomColor
      else
        Result := APalette.Accent;
  else
    Result := APalette.GaugeNeedle;
  end;
end;

{ TOBDRibbon ---------------------------------------------------------------- }

constructor TOBDRibbon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alTop;
  Width := 640;
  Height := 132;
  TabStop := True;
  FTabs := TOBDRibbonTabs.Create(Self);
  FContextualTabs := TOBDContextualTabSets.Create(Self);
  FPreviewTabs := TOBDRibbonTabs.Create(nil);
  FPreviewContextualTabs := TOBDContextualTabSets.Create(nil);
  FActiveTab := 0;
  FShowFileButton := True;
  FFileCaption := 'File';
  FShowSearch := True;
  FSearchHint := 'Search commands  Alt+Q';
  FRibbonStyle := rsClassic;
  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Parent := Self;
  FSearchEdit.BorderStyle := bsNone;
  FSearchEdit.Visible := False;
  FSearchEdit.TextHint := 'Search commands';
  FSearchEdit.OnChange := SearchChanged;
  FSearchEdit.OnKeyDown := SearchKeyDown;
  FApplicationEvents := TApplicationEvents.Create(Self);
  FApplicationEvents.OnShortCut := AppShortCut;
  UpdateHeight;
end;

destructor TOBDRibbon.Destroy;
begin
  FApplicationEvents.OnShortCut := nil;
  FOverflowMenu.Free;
  FSearchMenu.Free;
  FPreviewContextualTabs.Free;
  FPreviewTabs.Free;
  FContextualTabs.Free;
  FTabs.Free;
  inherited Destroy;
end;

procedure TOBDRibbon.SetTabs(AValue: TOBDRibbonTabs);
begin
  FTabs.Assign(AValue);
end;

procedure TOBDRibbon.SetContextualTabs(AValue: TOBDContextualTabSets);
begin
  FContextualTabs.Assign(AValue);
end;

procedure TOBDRibbon.SetActiveTab(AValue: Integer);
begin
  if AValue < 0 then
    AValue := FirstVisibleTab;
  if FlatTabCount = 0 then
    AValue := -1
  else
    AValue := EnsureRange(AValue, 0, FlatTabCount - 1);
  if FActiveTab = AValue then
    Exit;
  FActiveTab := AValue;
  FFocusedItem := nil;
  FTemporaryExpanded := False;
  UpdateHeight;
  Invalidate;
  DoTabChange;
end;

procedure TOBDRibbon.SetShowFileButton(AValue: Boolean);
begin
  if FShowFileButton = AValue then
    Exit;
  FShowFileButton := AValue;
  RibbonChanged;
end;

procedure TOBDRibbon.SetFileCaption(const AValue: TCaption);
begin
  if FFileCaption = AValue then
    Exit;
  FFileCaption := AValue;
  RibbonChanged;
end;

procedure TOBDRibbon.SetBackstage(AValue: TControl);
begin
  if FBackstage = AValue then
    Exit;
  if FBackstage <> nil then
    FBackstage.RemoveFreeNotification(Self);
  FBackstage := AValue;
  if FBackstage <> nil then
    FBackstage.FreeNotification(Self);
end;

procedure TOBDRibbon.SetShowSearch(AValue: Boolean);
begin
  if FShowSearch = AValue then
    Exit;
  FShowSearch := AValue;
  LayoutSearchEdit;
  Invalidate;
end;

procedure TOBDRibbon.SetSearchHint(const AValue: string);
begin
  if FSearchHint = AValue then
    Exit;
  FSearchHint := AValue;
  FSearchEdit.TextHint := AValue;
  Invalidate;
end;

procedure TOBDRibbon.SetRibbonStyle(AValue: TOBDRibbonStyle);
begin
  if FRibbonStyle = AValue then
    Exit;
  FRibbonStyle := AValue;
  RibbonChanged;
end;

procedure TOBDRibbon.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed = AValue then
    Exit;
  FCollapsed := AValue;
  FTemporaryExpanded := False;
  UpdateHeight;
  Invalidate;
end;

procedure TOBDRibbon.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  if FImages <> nil then
    FImages.RemoveFreeNotification(Self);
  FImages := AValue;
  if FImages <> nil then
    FImages.FreeNotification(Self);
  Invalidate;
end;

procedure TOBDRibbon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      FImages := nil;
    if AComponent = FBackstage then
      FBackstage := nil;
  end;
end;

procedure TOBDRibbon.CloseBackstage;
begin
  if FBackstage <> nil then
    FBackstage.Visible := False;
end;

function TOBDRibbon.ActiveTabItem: TOBDRibbonTab;
begin
  Result := TabByFlatIndex(FActiveTab);
end;

function TOBDRibbon.PreviewMode: Boolean;
begin
  Result := (FTabs.Count = 0) and IsPreview;
end;

function TOBDRibbon.ActiveTabs: TOBDRibbonTabs;
begin
  if PreviewMode then
  begin
    EnsurePreviewData;
    Result := FPreviewTabs;
  end
  else
    Result := FTabs;
end;

function TOBDRibbon.ActiveContextualTabs: TOBDContextualTabSets;
begin
  if PreviewMode then
  begin
    EnsurePreviewData;
    Result := FPreviewContextualTabs;
  end
  else
    Result := FContextualTabs;
end;

procedure TOBDRibbon.EnsurePreviewData;
var
  T: TOBDRibbonTab;
  G: TOBDRibbonGroup;
  I: TOBDRibbonItem;
  C: TOBDContextualTabSet;
begin
  if FPreviewTabs.Count > 0 then
    Exit;

  T := FPreviewTabs.Add;
  T.Caption := 'Home';
  G := T.Groups.Add;
  G.Caption := 'Connection';
  G.ShowLauncher := True;
  I := G.Items.Add;
  I.Caption := 'Connect';
  I.Glyph := glConnect;
  I.Size := risLarge;
  I.Kind := rikDropDown;
  I := G.Items.Add;
  I.Caption := 'Settings';
  I.Glyph := glGear;
  I.Size := risSmall;
  I.Kind := rikButton;
  I := G.Items.Add;
  I.Caption := 'Help';
  I.Glyph := glHelp;
  I.Size := risSmall;

  T := FPreviewTabs.Add;
  T.Caption := 'Diagnose';
  G := T.Groups.Add;
  G.Caption := 'Codes';
  G.ShowLauncher := True;
  I := G.Items.Add;
  I.Caption := 'Read codes';
  I.Glyph := glRead;
  I.Size := risLarge;
  I.Kind := rikButton;
  I := G.Items.Add;
  I.Caption := 'Clear codes';
  I.Glyph := glClear;
  I.Size := risLarge;
  I.Kind := rikSplit;
  I.Color := ricDanger;
  I := G.Items.Add;
  I.Caption := 'Snapshot';
  I.Glyph := glSnapshot;
  I.Size := risSmall;
  I := G.Items.Add;
  I.Caption := 'Pending only';
  I.Glyph := glPending;
  I.Size := risSmall;
  I.Kind := rikToggle;
  G := T.Groups.Add;
  G.Caption := 'Report';
  I := G.Items.Add;
  I.Caption := 'Report';
  I.Glyph := glReport;
  I.Size := risLarge;
  I.Kind := rikSplit;
  I := G.Items.Add;
  I.Caption := 'Print...';
  I.Glyph := glPrint;
  I.Size := risSmall;
  I := G.Items.Add;
  I.Caption := 'Copy summary';
  I.Glyph := glCopy;
  I.Size := risSmall;

  T := FPreviewTabs.Add;
  T.Caption := 'Live data';
  G := T.Groups.Add;
  G.Caption := 'Capture';
  I := G.Items.Add;
  I.Caption := 'Record';
  I.Glyph := glRecord;
  I.Size := risLarge;
  I.Color := ricDanger;
  I := G.Items.Add;
  I.Caption := 'Filter';
  I.Glyph := glFilter;
  I.Size := risSmall;
  I.Kind := rikDropDown;

  T := FPreviewTabs.Add;
  T.Caption := 'Vehicle';
  T.Groups.Add.Caption := 'Vehicle';
  T := FPreviewTabs.Add;
  T.Caption := 'Report';
  T.Groups.Add.Caption := 'Report';

  C := FPreviewContextualTabs.Add;
  C.Caption := 'Recording open';
  C.Color := ccSuccess;
  C.Visible := True;
  T := C.Tabs.Add;
  T.Caption := 'Playback';
  G := T.Groups.Add;
  G.Caption := 'Playback';
  I := G.Items.Add;
  I.Caption := 'Play';
  I.Glyph := glPlay;
  I.Size := risLarge;
  I := G.Items.Add;
  I.Caption := 'Stop';
  I.Glyph := glStop;
  I.Size := risSmall;
end;

function TOBDRibbon.FlatTabCount: Integer;
var
  Tabs: TOBDRibbonTabs;
  Ctx: TOBDContextualTabSets;
  I, J: Integer;
begin
  Result := 0;
  Tabs := ActiveTabs;
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
      Inc(Result);
  Ctx := ActiveContextualTabs;
  for I := 0 to Ctx.Count - 1 do
    if Ctx[I].Visible then
      for J := 0 to Ctx[I].Tabs.Count - 1 do
        if Ctx[I].Tabs[J].Visible then
          Inc(Result);
end;

function TOBDRibbon.TabByFlatIndex(AIndex: Integer): TOBDRibbonTab;
var
  Tabs: TOBDRibbonTabs;
  Ctx: TOBDContextualTabSets;
  I, J, N: Integer;
  Col: TColor;
begin
  Result := nil;
  N := 0;
  Tabs := ActiveTabs;
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      if N = AIndex then
        Exit(Tabs[I]);
      Inc(N);
    end;
  Ctx := ActiveContextualTabs;
  for I := 0 to Ctx.Count - 1 do
    if Ctx[I].Visible then
    begin
      Col := Ctx[I].ResolveColor(Palette);
      for J := 0 to Ctx[I].Tabs.Count - 1 do
        if Ctx[I].Tabs[J].Visible then
        begin
          Ctx[I].Tabs[J].FIsContextual := True;
          Ctx[I].Tabs[J].FContextColor := Col;
          if N = AIndex then
            Exit(Ctx[I].Tabs[J]);
          Inc(N);
        end;
    end;
end;

function TOBDRibbon.FlatIndexOfTab(ATab: TOBDRibbonTab): Integer;
var
  I: Integer;
begin
  Result := -1;
  if ATab = nil then
    Exit;
  for I := 0 to FlatTabCount - 1 do
    if TabByFlatIndex(I) = ATab then
      Exit(I);
end;

function TOBDRibbon.FirstVisibleTab: Integer;
begin
  if FlatTabCount > 0 then
    Result := 0
  else
    Result := -1;
end;

function TOBDRibbon.BodyVisible: Boolean;
begin
  Result := not FCollapsed or FTemporaryExpanded;
end;

function TOBDRibbon.TabHeight: Integer;
begin
  Result := ScaleValue(Metrics.Tab);
end;

function TOBDRibbon.ClassicBodyHeight: Integer;
begin
  Result := ScaleValue(LARGE_BODY_HEIGHT + GROUP_CAPTION_HEIGHT +
    CLASSIC_BODY_EXTRA);
end;

function TOBDRibbon.SimplifiedBodyHeight: Integer;
begin
  Result := ScaleValue(Metrics.Button + 12);
end;

function TOBDRibbon.BodyHeight: Integer;
begin
  if FRibbonStyle = rsSimplified then
    Result := SimplifiedBodyHeight
  else
    Result := ClassicBodyHeight;
end;

function TOBDRibbon.DesiredHeight: Integer;
begin
  Result := TabHeight;
  if BodyVisible then
    Inc(Result, BodyHeight);
end;

procedure TOBDRibbon.UpdateHeight;
begin
  Height := DesiredHeight;
  LayoutSearchEdit;
end;

procedure TOBDRibbon.LayoutSearchEdit;
var
  R: TRect;
begin
  if FSearchEdit = nil then
    Exit;
  R := FSearchRect;
  InflateRect(R, -ScaleValue(30), -ScaleValue(5));
  R.Right := FSearchRect.Right - ScaleValue(54);
  if (not FShowSearch) or (R.Right <= R.Left) or (R.Bottom <= R.Top) then
  begin
    FSearchEdit.Visible := False;
    Exit;
  end;
  FSearchEdit.SetBounds(R.Left, R.Top + ScaleValue(1), R.Width,
    System.Math.Max(1, R.Height - ScaleValue(2)));
  FSearchEdit.Font.Name := 'Segoe UI';
  FSearchEdit.Font.Size := 9;
  FSearchEdit.Color := Palette.Background;
  FSearchEdit.Font.Color := Palette.ForegroundText;
  FSearchEdit.Visible := True;
end;

procedure TOBDRibbon.RibbonChanged;
begin
  NormalizeActiveTab;
  UpdateHeight;
  Invalidate;
end;

procedure TOBDRibbon.NormalizeActiveTab;
var
  N: Integer;
begin
  N := FlatTabCount;
  if N = 0 then
    FActiveTab := -1
  else if (FActiveTab < 0) or (FActiveTab >= N) or
    (TabByFlatIndex(FActiveTab) = nil) then
    FActiveTab := 0;
end;

procedure TOBDRibbon.DoTabChange;
var
  T: TOBDRibbonTab;
begin
  T := ActiveTabItem;
  if Assigned(FOnTabChange) then
    FOnTabChange(Self, T);
end;

procedure TOBDRibbon.ClearLayout;
var
  I, J, K: Integer;
  Tabs: TOBDRibbonTabs;
  Ctx: TOBDContextualTabSets;
  T: TOBDRibbonTab;
begin
  Tabs := ActiveTabs;
  for I := 0 to Tabs.Count - 1 do
  begin
    Tabs[I].FRect := Rect(0, 0, 0, 0);
    Tabs[I].FIsContextual := False;
    for J := 0 to Tabs[I].Groups.Count - 1 do
    begin
      Tabs[I].Groups[J].FRect := Rect(0, 0, 0, 0);
      Tabs[I].Groups[J].FLauncherRect := Rect(0, 0, 0, 0);
      for K := 0 to Tabs[I].Groups[J].Items.Count - 1 do
      begin
        Tabs[I].Groups[J].Items[K].FRect := Rect(0, 0, 0, 0);
        Tabs[I].Groups[J].Items[K].FDropRect := Rect(0, 0, 0, 0);
        Tabs[I].Groups[J].Items[K].FOverflow := False;
      end;
    end;
  end;
  Ctx := ActiveContextualTabs;
  for I := 0 to Ctx.Count - 1 do
    for J := 0 to Ctx[I].Tabs.Count - 1 do
    begin
      T := Ctx[I].Tabs[J];
      T.FRect := Rect(0, 0, 0, 0);
      T.FIsContextual := False;
      for K := 0 to T.Groups.Count - 1 do
        T.Groups[K].FRect := Rect(0, 0, 0, 0);
    end;
end;

procedure TOBDRibbon.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  NormalizeActiveTab;
  ClearLayout;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    DrawTabRow(P);
    if BodyVisible then
      DrawBody(P, ACanvas);
  finally
    P.Free;
  end;
  LayoutSearchEdit;
end;

procedure TOBDRibbon.DrawTabRow(APainter: TOBDPainter);
var
  X, RX, W, H, I, Tw: Integer;
  T: TOBDRibbonTab;
  Active: TOBDRibbonTab;
  Col: TColor;
  Weight: TOBDTextWeight;
  Tabs: TOBDRibbonTabs;
  Ctx: TOBDContextualTabSets;
  J, K: Integer;
  TextColor: TColor;
begin
  H := TabHeight;
  APainter.FillRect(Rect(0, 0, Width, H), Palette.GaugeFace);
  X := ScaleValue(6);
  FFileRect := Rect(0, 0, 0, 0);
  if FShowFileButton then
  begin
    W := APainter.TextWidth(FFileCaption, TAB_TEXT_SIZE, twSemibold) +
      ScaleValue(28);
    FFileRect := Rect(X, ScaleValue(5), X + W, H - ScaleValue(5));
    Col := Palette.Accent;
    if FFileDown then
      Col := OBDMixColor(Palette.ForegroundText, Col, 0.12)
    else if FFileHover then
      Col := OBDMixColor(clWhite, Col, 0.15);
    APainter.FillRect(FFileRect, Col);
    APainter.Text(FFileRect.Left + FFileRect.Width div 2,
      FFileRect.Top + FFileRect.Height div 2, FFileCaption, TAB_TEXT_SIZE,
      APainter.OnAccent, twSemibold, taCenter);
    X := FFileRect.Right + ScaleValue(6);
  end;

  RX := Width - ScaleValue(10);
  FCollapseRect := Rect(RX - ScaleValue(24), 0, RX, H);
  DrawChevron(APainter, FCollapseRect.Left + FCollapseRect.Width div 2,
    H div 2, FCollapsed, Palette.Subtle);
  RX := FCollapseRect.Left - ScaleValue(12);
  FSearchRect := Rect(0, 0, 0, 0);
  if FShowSearch then
  begin
    W := ScaleValue(SEARCH_WIDTH);
    FSearchRect := Rect(RX - W, ScaleValue(5), RX, H - ScaleValue(5));
    APainter.FillRect(FSearchRect, Palette.Background);
    APainter.FrameRect(FSearchRect, Palette.NeutralLight);
    APainter.Glyph(glSearch, FSearchRect.Left + ScaleValue(16), H div 2,
      Palette.Subtle, 0.55);
    if FSearchEdit.Text = '' then
      APainter.Text(FSearchRect.Left + ScaleValue(30), H div 2,
        'Search commands', 12.5, Palette.GaugeLabel, twRegular,
        taLeftJustify, FSearchRect.Width - ScaleValue(90));
    APainter.Text(FSearchRect.Right - ScaleValue(10), H div 2, 'Alt+Q',
      11.5, Palette.GaugeLabel, twRegular, taRightJustify);
    RX := FSearchRect.Left - ScaleValue(10);
  end;

  Active := ActiveTabItem;
  Tabs := ActiveTabs;
  for I := 0 to Tabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      T := Tabs[I];
      Tw := APainter.TextWidth(T.Caption, TAB_TEXT_SIZE, twRegular) +
        ScaleValue(28);
      if X + Tw > RX then
        Break;
      T.FRect := Rect(X, 0, X + Tw, H);
      if T = Active then
      begin
        TextColor := APainter.AccentText;
        Weight := twSemibold;
        APainter.FillRect(Rect(X + ScaleValue(10), H - ScaleValue(3),
          X + Tw - ScaleValue(10), H), APainter.AccentText);
      end
      else
      begin
        TextColor := Palette.ForegroundText;
        Weight := twRegular;
      end;
      if T = FHoverTab then
        APainter.FillRect(Rect(X, ScaleValue(5), X + Tw, H - ScaleValue(5)),
          OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06));
      APainter.Text(X + Tw div 2, H div 2, T.Caption, TAB_TEXT_SIZE,
        TextColor, Weight, taCenter);
      Inc(X, Tw);
    end;

  Ctx := ActiveContextualTabs;
  if Ctx.Count > 0 then
    Inc(X, ScaleValue(8));
  for J := 0 to Ctx.Count - 1 do
    if Ctx[J].Visible then
    begin
      Col := Ctx[J].ResolveColor(Palette);
      for K := 0 to Ctx[J].Tabs.Count - 1 do
        if Ctx[J].Tabs[K].Visible then
        begin
          T := Ctx[J].Tabs[K];
          T.FIsContextual := True;
          T.FContextColor := Col;
          Tw := APainter.TextWidth(T.Caption, TAB_TEXT_SIZE, twRegular) +
            ScaleValue(28);
          if X + Tw > RX then
            Break;
          T.FRect := Rect(X, 0, X + Tw, H);
          APainter.FillRect(T.FRect, APainter.Tint(Col,
            IfThen(APainter.Dark, 0.20, 0.14)));
          APainter.FillRect(Rect(X, 0, X + Tw, ScaleValue(3)), Col);
          if T = Active then
          begin
            Weight := twSemibold;
            APainter.FillRect(Rect(X + ScaleValue(10), H - ScaleValue(3),
              X + Tw - ScaleValue(10), H), Col);
          end
          else
            Weight := twRegular;
          APainter.Text(X + Tw div 2, H div 2 + ScaleValue(1), T.Caption,
            TAB_TEXT_SIZE, Col, Weight, taCenter);
          Inc(X, Tw + ScaleValue(2));
        end;
    end;
  APainter.HLine(0, H - ScaleValue(1), Width, Palette.NeutralLight);
end;

procedure TOBDRibbon.DrawBody(APainter: TOBDPainter; ACanvas: TCanvas);
var
  T: TOBDRibbonTab;
  Top: Integer;
begin
  T := ActiveTabItem;
  if T = nil then
    Exit;
  Top := TabHeight;
  if FRibbonStyle = rsSimplified then
    DrawSimplifiedBody(APainter, ACanvas, T, Top)
  else
    DrawClassicBody(APainter, ACanvas, T, Top);
end;

procedure TOBDRibbon.DrawClassicBody(APainter: TOBDPainter; ACanvas: TCanvas;
  ATab: TOBDRibbonTab; ATop: Integer);
var
  GX, BX, StartX, GW, I, J, BodyH, CapH, RH, ColW, ItemW: Integer;
  G: TOBDRibbonGroup;
  It: TOBDRibbonItem;
  HasSmall: Boolean;
  CY: Integer;
begin
  BodyH := ScaleValue(LARGE_BODY_HEIGHT);
  CapH := ScaleValue(GROUP_CAPTION_HEIGHT);
  APainter.FillRect(Rect(0, ATop, Width, ATop + ClassicBodyHeight),
    Palette.GaugeFace);
  GX := ScaleValue(8);
  for I := 0 to ATab.Groups.Count - 1 do
  begin
    G := ATab.Groups[I];
    if not G.Visible then
      Continue;
    StartX := GX;
    BX := GX;
    HasSmall := False;
    for J := 0 to G.Items.Count - 1 do
      if G.Items[J].Visible and (G.Items[J].Size = risLarge) then
      begin
        ItemW := DrawLargeItem(APainter, ACanvas, G.Items[J], BX,
          ATop + ScaleValue(4), BodyH);
        Inc(BX, ItemW + ScaleValue(2));
      end
      else if G.Items[J].Visible then
        HasSmall := True;
    if HasSmall then
    begin
      RH := BodyH div 3;
      ColW := 0;
      for J := 0 to G.Items.Count - 1 do
      begin
        It := G.Items[J];
        if It.Visible and (It.Size = risSmall) then
        begin
          ItemW := DrawSmallItem(APainter, ACanvas, It, BX + ScaleValue(2),
            ATop + ScaleValue(4) + (J mod 3) * RH, RH);
          ColW := System.Math.Max(ColW, ItemW);
          if (J mod 3) = 2 then
          begin
            Inc(BX, ColW + ScaleValue(4));
            ColW := 0;
          end;
        end;
      end;
      if ColW > 0 then
        Inc(BX, ColW + ScaleValue(6));
    end;
    GW := System.Math.Max(BX - StartX,
      APainter.TextWidth(G.Caption, 11.5, twRegular) + ScaleValue(36));
    G.FRect := Rect(StartX, ATop, StartX + GW, ATop + ClassicBodyHeight);
    CY := ATop + ScaleValue(4) + BodyH + CapH div 2 + ScaleValue(2);
    APainter.Text(StartX + GW div 2, CY, G.Caption, 11.5,
      Palette.GaugeLabel, twRegular, taCenter, GW - ScaleValue(20));
    G.FLauncherRect := Rect(0, 0, 0, 0);
    if G.ShowLauncher then
    begin
      G.FLauncherRect := Rect(StartX + GW - ScaleValue(16), CY - ScaleValue(8),
        StartX + GW, CY + ScaleValue(8));
      DrawLauncher(APainter, StartX + GW - ScaleValue(8), CY,
        Palette.Subtle);
    end;
    GX := StartX + GW + ScaleValue(6);
    APainter.VLine(GX, ATop + ScaleValue(8), ClassicBodyHeight -
      ScaleValue(16), Palette.NeutralLight);
    Inc(GX, ScaleValue(7));
  end;
  APainter.HLine(0, ATop + ClassicBodyHeight - ScaleValue(1), Width,
    Palette.NeutralLight);
end;

procedure TOBDRibbon.DrawSimplifiedBody(APainter: TOBDPainter; ACanvas: TCanvas;
  ATab: TOBDRibbonTab; ATop: Integer);
var
  BX, LimitX, I, J, H, RH, W: Integer;
  G: TOBDRibbonGroup;
  It: TOBDRibbonItem;
  Overflowing: Boolean;
begin
  H := SimplifiedBodyHeight;
  RH := ScaleValue(Metrics.Button);
  APainter.FillRect(Rect(0, ATop, Width, ATop + H), Palette.GaugeFace);
  BX := ScaleValue(8);
  LimitX := Width - ScaleValue(60);
  Overflowing := False;
  for I := 0 to ATab.Groups.Count - 1 do
  begin
    G := ATab.Groups[I];
    if not G.Visible then
      Continue;
    for J := 0 to G.Items.Count - 1 do
    begin
      It := G.Items[J];
      if not It.Visible then
        Continue;
      W := APainter.TextWidth(ItemCaption(It), ITEM_TEXT_SIZE, twRegular) +
        ScaleValue(34);
      if It.Kind in [rikDropDown, rikSplit] then
        Inc(W, ScaleValue(14));
      if BX + W > LimitX then
      begin
        It.FOverflow := True;
        Overflowing := True;
        Continue;
      end;
      DrawSmallItem(APainter, ACanvas, It, BX, ATop + ScaleValue(6), RH);
      Inc(BX, W + ScaleValue(2));
    end;
    if BX + ScaleValue(8) < LimitX then
    begin
      APainter.VLine(BX + ScaleValue(3), ATop + ScaleValue(10),
        H - ScaleValue(20), Palette.NeutralLight);
      Inc(BX, ScaleValue(8));
    end;
  end;
  FMoreRect := Rect(Width - ScaleValue(44), ATop + ScaleValue(6),
    Width - ScaleValue(12), ATop + ScaleValue(6) + RH);
  if Overflowing then
  begin
    if FMoreHover or FMoreDown then
      APainter.FillRect(FMoreRect, OBDMixColor(Palette.ForegroundText,
        Palette.GaugeFace, 0.07));
    APainter.FrameRect(FMoreRect, Palette.NeutralLight);
    APainter.Glyph(glMore, FMoreRect.Left + FMoreRect.Width div 2,
      FMoreRect.Top + FMoreRect.Height div 2, Palette.Subtle, 0.8);
  end
  else
    FMoreRect := Rect(0, 0, 0, 0);
  APainter.HLine(0, ATop + H - ScaleValue(1), Width, Palette.NeutralLight);
end;

function TOBDRibbon.DrawLargeItem(APainter: TOBDPainter; ACanvas: TCanvas;
  AItem: TOBDRibbonItem; X, Y, H: Integer): Integer;
var
  Lines: TStringDynArray;
  CaptionText: string;
  I, TW, CY: Integer;
  Fill, Ink, GlyphColor: TColor;
  R: TRect;
begin
  CaptionText := StripAccel(ItemCaption(AItem));
  Lines := SplitString(CaptionText, ' ');
  if Length(Lines) = 0 then
    SetLength(Lines, 1);
  if (Length(Lines) > 1) and (AItem.Kind <> rikSplit) then
    TW := System.Math.Max(APainter.TextWidth(Lines[0], ITEM_TEXT_SIZE),
      APainter.TextWidth(Copy(CaptionText, Length(Lines[0]) + 2,
      MaxInt), ITEM_TEXT_SIZE))
  else
    TW := APainter.TextWidth(CaptionText, ITEM_TEXT_SIZE);
  Result := System.Math.Max(TW + ScaleValue(16), ScaleValue(52));
  R := Rect(X, Y, X + Result, Y + H);
  AItem.FRect := R;
  AItem.FDropRect := Rect(0, 0, 0, 0);
  Fill := clNone;
  if (AItem = FHoverItem) or (AItem = FPressedItem) or AItem.Down then
    Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.07);
  if Fill <> clNone then
    APainter.FillRect(R, Fill);
  if (AItem = FHoverItem) or (AItem = FPressedItem) or AItem.Down then
    APainter.FrameRect(R, Palette.NeutralLight);
  GlyphColor := APainter.AccentText;
  if AItem.Color = ricDanger then
    GlyphColor := Palette.Danger;
  if not AItem.Enabled then
    GlyphColor := APainter.DisabledText;
  DrawItemGlyph(APainter, ACanvas, AItem, X + Result div 2,
    Y + ScaleValue(22), GlyphColor, 1.7);
  Ink := Palette.ForegroundText;
  if not AItem.Enabled then
    Ink := APainter.DisabledText;
  if AItem.Kind = rikSplit then
  begin
    APainter.Text(X + Result div 2, Y + ScaleValue(50), CaptionText,
      ITEM_TEXT_SIZE, Ink, twRegular, taCenter, Result - ScaleValue(8));
    APainter.HLine(X + ScaleValue(4), Y + ScaleValue(40), Result -
      ScaleValue(8), Palette.NeutralLight);
    AItem.FDropRect := Rect(X, Y + ScaleValue(42), X + Result, Y + H);
    DrawChevron(APainter, X + Result div 2, Y + ScaleValue(66), True,
      Palette.Subtle);
  end
  else
  begin
    CY := Y + ScaleValue(50);
    if Pos(' ', CaptionText) > 0 then
    begin
      APainter.Text(X + Result div 2, CY, Lines[0], ITEM_TEXT_SIZE, Ink,
        twRegular, taCenter, Result - ScaleValue(8));
      APainter.Text(X + Result div 2, CY + ScaleValue(15),
        Copy(CaptionText, Length(Lines[0]) + 2, MaxInt), ITEM_TEXT_SIZE,
        Ink, twRegular, taCenter, Result - ScaleValue(8));
    end
    else
      for I := 0 to Length(Lines) - 1 do
        APainter.Text(X + Result div 2, CY + I * ScaleValue(15), Lines[I],
          ITEM_TEXT_SIZE, Ink, twRegular, taCenter, Result - ScaleValue(8));
  end;
end;

function TOBDRibbon.DrawSmallItem(APainter: TOBDPainter; ACanvas: TCanvas;
  AItem: TOBDRibbonItem; X, Y, H: Integer): Integer;
var
  CaptionText: string;
  R: TRect;
  Ink, GlyphColor, Fill: TColor;
begin
  CaptionText := StripAccel(ItemCaption(AItem));
  Result := APainter.TextWidth(CaptionText, ITEM_TEXT_SIZE, twRegular) +
    ScaleValue(34);
  if AItem.Kind in [rikDropDown, rikSplit] then
    Inc(Result, ScaleValue(14));
  R := Rect(X, Y, X + Result, Y + H);
  AItem.FRect := R;
  AItem.FDropRect := Rect(0, 0, 0, 0);
  if AItem.Kind in [rikDropDown, rikSplit] then
    AItem.FDropRect := Rect(R.Right - ScaleValue(20), R.Top, R.Right,
      R.Bottom);
  if AItem.Down or (AItem = FPressedItem) then
  begin
    Fill := APainter.Tint(Palette.Accent, IfThen(APainter.Dark, 0.26, 0.16));
    APainter.FillRect(R, Fill);
    APainter.FrameRect(R, APainter.AccentText);
  end
  else if AItem = FHoverItem then
  begin
    APainter.FillRect(R, OBDMixColor(Palette.ForegroundText,
      Palette.GaugeFace, 0.07));
    APainter.FrameRect(R, Palette.NeutralLight);
  end;
  Ink := Palette.ForegroundText;
  GlyphColor := Palette.Subtle;
  if not AItem.Enabled then
  begin
    Ink := APainter.DisabledText;
    GlyphColor := Ink;
  end
  else if AItem.Color = ricDanger then
    GlyphColor := Palette.Danger
  else if AItem.Color = ricAccent then
    GlyphColor := APainter.AccentText;
  DrawItemGlyph(APainter, ACanvas, AItem, X + ScaleValue(12), Y + H div 2,
    GlyphColor, 0.75);
  APainter.Text(X + ScaleValue(26), Y + H div 2, CaptionText, ITEM_TEXT_SIZE,
    Ink, twRegular, taLeftJustify, Result - ScaleValue(32));
  if AItem.Kind in [rikDropDown, rikSplit] then
    DrawChevron(APainter, R.Right - ScaleValue(10), Y + H div 2, True,
      Palette.Subtle);
end;

procedure TOBDRibbon.DrawItemGlyph(APainter: TOBDPainter; ACanvas: TCanvas;
  AItem: TOBDRibbonItem; CX, CY: Integer; AColor: TColor; AScale: Single);
var
  Index, IW, IH: Integer;
begin
  Index := ItemImageIndex(AItem);
  if (FImages <> nil) and (Index >= 0) and (Index < FImages.Count) then
  begin
    IW := FImages.Width;
    IH := FImages.Height;
    FImages.Draw(ACanvas, CX - IW div 2, CY - IH div 2, Index,
      AItem.Enabled);
  end
  else
    APainter.Glyph(AItem.Glyph, CX, CY, AColor, AScale);
end;

procedure TOBDRibbon.DrawChevron(APainter: TOBDPainter; CX, CY: Integer;
  ADown: Boolean; AColor: TColor);
var
  S: Integer;
begin
  S := ScaleValue(4);
  if ADown then
    APainter.Lines([MakePoint(CX - S, CY - S div 2), MakePoint(CX, CY + S div 2),
      MakePoint(CX + S, CY - S div 2)], AColor, ScaleValue(16) / 10)
  else
    APainter.Lines([MakePoint(CX - S, CY + S div 2), MakePoint(CX, CY - S div 2),
      MakePoint(CX + S, CY + S div 2)], AColor, ScaleValue(16) / 10);
end;

procedure TOBDRibbon.DrawLauncher(APainter: TOBDPainter; CX, CY: Integer;
  AColor: TColor);
var
  S: Integer;
begin
  S := ScaleValue(6);
  APainter.FrameRect(Rect(CX - S, CY - S, CX + S, CY + S), AColor);
  APainter.Lines([MakePoint(CX - ScaleValue(2), CY + ScaleValue(2)),
    MakePoint(CX + ScaleValue(4), CY - ScaleValue(4))], AColor,
    ScaleValue(12) / 10);
end;

procedure TOBDRibbon.Resize;
begin
  inherited Resize;
  LayoutSearchEdit;
end;

procedure TOBDRibbon.DensityChanged;
begin
  inherited DensityChanged;
  UpdateHeight;
end;

function TOBDRibbon.HitTab(X, Y: Integer): TOBDRibbonTab;
var
  I: Integer;
  T: TOBDRibbonTab;
begin
  Result := nil;
  if Y >= TabHeight then
    Exit;
  for I := 0 to FlatTabCount - 1 do
  begin
    T := TabByFlatIndex(I);
    if (T <> nil) and PtInRect(T.FRect, Point(X, Y)) then
      Exit(T);
  end;
end;

function TOBDRibbon.HitItem(X, Y: Integer): TOBDRibbonItem;
var
  T: TOBDRibbonTab;
  I, J: Integer;
begin
  Result := nil;
  T := ActiveTabItem;
  if T = nil then
    Exit;
  for I := 0 to T.Groups.Count - 1 do
    for J := 0 to T.Groups[I].Items.Count - 1 do
      if T.Groups[I].Items[J].Visible and
        PtInRect(T.Groups[I].Items[J].FRect, Point(X, Y)) then
        Exit(T.Groups[I].Items[J]);
end;

function TOBDRibbon.HitLauncher(X, Y: Integer): TOBDRibbonGroup;
var
  T: TOBDRibbonTab;
  I: Integer;
begin
  Result := nil;
  T := ActiveTabItem;
  if T = nil then
    Exit;
  for I := 0 to T.Groups.Count - 1 do
    if T.Groups[I].Visible and T.Groups[I].ShowLauncher and
      PtInRect(T.Groups[I].FLauncherRect, Point(X, Y)) then
      Exit(T.Groups[I]);
end;

function TOBDRibbon.ItemHintAt(X, Y: Integer): string;
var
  It: TOBDRibbonItem;
begin
  Result := '';
  It := HitItem(X, Y);
  if It <> nil then
    Result := ItemHint(It);
end;

procedure TOBDRibbon.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewTab: TOBDRibbonTab;
  NewItem: TOBDRibbonItem;
  NewLauncher: TOBDRibbonGroup;
  P: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  NewTab := HitTab(X, Y);
  NewItem := HitItem(X, Y);
  NewLauncher := HitLauncher(X, Y);
  P := Point(X, Y);
  if (NewTab <> FHoverTab) or (NewItem <> FHoverItem) or
    (NewLauncher <> FHoverLauncher) or (FFileHover <>
    PtInRect(FFileRect, P)) or (FHoverCollapse <> PtInRect(FCollapseRect, P)) or
    (FMoreHover <> PtInRect(FMoreRect, P)) then
  begin
    FHoverTab := NewTab;
    FHoverItem := NewItem;
    FHoverLauncher := NewLauncher;
    FFileHover := PtInRect(FFileRect, P);
    FHoverCollapse := PtInRect(FCollapseRect, P);
    FMoreHover := PtInRect(FMoreRect, P);
    Invalidate;
  end;
end;

procedure TOBDRibbon.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  T: TOBDRibbonTab;
  It: TOBDRibbonItem;
  G: TOBDRibbonGroup;
  P: TPoint;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button <> mbLeft then
    Exit;
  SetFocus;
  P := Point(X, Y);
  if PtInRect(FFileRect, P) then
  begin
    FFileDown := True;
    Invalidate;
    Exit;
  end;
  if PtInRect(FCollapseRect, P) then
  begin
    ToggleCollapsed;
    Exit;
  end;
  if PtInRect(FMoreRect, P) then
  begin
    FMoreDown := True;
    if FOverflowMenu = nil then
      FOverflowMenu := TOBDPopupMenu.Create(Self);
    FOverflowMenu.Theme := Theme;
    FOverflowMenu.Density := Density;
    PopulateCommandMenu(FOverflowMenu, '', True);
    R := FMoreRect;
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    PopupMenuAt(FOverflowMenu, R);
    Exit;
  end;
  T := HitTab(X, Y);
  if T <> nil then
  begin
    ActiveTab := FlatIndexOfTab(T);
    if FCollapsed then
    begin
      FTemporaryExpanded := True;
      UpdateHeight;
      BringToFront;
      Invalidate;
    end;
    Exit;
  end;
  G := HitLauncher(X, Y);
  if G <> nil then
  begin
    G.ClickLauncher;
    Exit;
  end;
  It := HitItem(X, Y);
  if (It <> nil) and It.Enabled then
  begin
    FPressedItem := It;
    FFocusedItem := It;
    Invalidate;
  end
  else if FTemporaryExpanded and (Y >= Height) then
  begin
    FTemporaryExpanded := False;
    UpdateHeight;
    Invalidate;
  end;
end;

procedure TOBDRibbon.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  It: TOBDRibbonItem;
  DropClick: Boolean;
  P: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button <> mbLeft then
    Exit;
  P := Point(X, Y);
  if FFileDown then
  begin
    FFileDown := False;
    Invalidate;
    if PtInRect(FFileRect, P) then
      ClickFile;
    Exit;
  end;
  FMoreDown := False;
  It := FPressedItem;
  FPressedItem := nil;
  if (It <> nil) and (It = HitItem(X, Y)) then
  begin
    DropClick := PtInRect(It.FDropRect, P) or (It.Kind = rikDropDown);
    if DropClick and (It.DropDownMenu <> nil) then
      PopupItemMenu(It)
    else
      ExecuteItem(It, False);
  end;
  Invalidate;
end;

procedure TOBDRibbon.KeyDown(var Key: Word; Shift: TShiftState);
var
  N: Integer;
  T: TOBDRibbonTab;
  I, J, Start, Count: Integer;
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_F1) and (ssCtrl in Shift) then
  begin
    ToggleCollapsed;
    Key := 0;
    Exit;
  end;
  if (Key = Ord('Q')) and (ssAlt in Shift) then
  begin
    FocusSearch;
    Key := 0;
    Exit;
  end;
  case Key of
    VK_LEFT:
      begin
        if FlatTabCount > 0 then
          ActiveTab := EnsureRange(FActiveTab - 1, 0, FlatTabCount - 1);
        Key := 0;
      end;
    VK_RIGHT:
      begin
        if FlatTabCount > 0 then
          ActiveTab := EnsureRange(FActiveTab + 1, 0, FlatTabCount - 1);
        Key := 0;
      end;
    VK_TAB:
      begin
        T := ActiveTabItem;
        if T <> nil then
        begin
          Count := 0;
          Start := -1;
          for I := 0 to T.Groups.Count - 1 do
            for J := 0 to T.Groups[I].Items.Count - 1 do
              if T.Groups[I].Items[J].Visible and T.Groups[I].Items[J].Enabled
              then
              begin
                if T.Groups[I].Items[J] = FFocusedItem then
                  Start := Count;
                Inc(Count);
              end;
          if Count > 0 then
          begin
            if ssShift in Shift then
              N := (Start + Count - 1) mod Count
            else
              N := (Start + 1) mod Count;
            Count := 0;
            for I := 0 to T.Groups.Count - 1 do
              for J := 0 to T.Groups[I].Items.Count - 1 do
                if T.Groups[I].Items[J].Visible and T.Groups[I].Items[J].Enabled
                then
                begin
                  if Count = N then
                    FFocusedItem := T.Groups[I].Items[J];
                  Inc(Count);
                end;
            Invalidate;
          end;
        end;
        Key := 0;
      end;
    VK_RETURN, VK_SPACE:
      begin
        if FFocusedItem <> nil then
          ExecuteItem(FFocusedItem, False);
        Key := 0;
      end;
  end;
end;

procedure TOBDRibbon.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TOBDRibbon.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoverTab := nil;
  FHoverItem := nil;
  FHoverLauncher := nil;
  FFileHover := False;
  FHoverCollapse := False;
  FMoreHover := False;
  Invalidate;
end;

procedure TOBDRibbon.CMHintShow(var Message: TCMHintShow);
var
  S: string;
begin
  inherited;
  S := ItemHintAt(Message.HintInfo.CursorPos.X, Message.HintInfo.CursorPos.Y);
  if S <> '' then
  begin
    Message.HintInfo.HintStr := S;
    Message.Result := 0;
  end;
end;

procedure TOBDRibbon.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  if FTemporaryExpanded then
  begin
    FTemporaryExpanded := False;
    UpdateHeight;
    Invalidate;
  end;
end;

procedure TOBDRibbon.ExecuteItem(AItem: TOBDRibbonItem; AFromMenu: Boolean);
begin
  if (AItem = nil) or not AItem.Enabled then
    Exit;
  AItem.Execute;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, AItem);
  if FTemporaryExpanded then
  begin
    FTemporaryExpanded := False;
    UpdateHeight;
  end;
  Invalidate;
end;

procedure TOBDRibbon.PopupItemMenu(AItem: TOBDRibbonItem);
var
  R: TRect;
begin
  if (AItem = nil) or (AItem.DropDownMenu = nil) then
    Exit;
  R := AItem.FRect;
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  PopupMenuAt(AItem.DropDownMenu, R);
end;

procedure TOBDRibbon.PopupMenuAt(AMenu: TPopupMenu; const AScreenRect: TRect);
begin
  if AMenu = nil then
    Exit;
  if AMenu is TOBDPopupMenu then
  begin
    TOBDPopupMenu(AMenu).Theme := Theme;
    TOBDPopupMenu(AMenu).Density := Density;
    TOBDPopupMenu(AMenu).PopupAt(AScreenRect);
  end
  else
    AMenu.Popup(AScreenRect.Left, AScreenRect.Bottom);
end;

procedure TOBDRibbon.ClickFile;
begin
  if FBackstage <> nil then
  begin
    FBackstage.Visible := True;
    FBackstage.Align := alClient;
    FBackstage.BringToFront;
  end;
  if Assigned(FOnFileClick) then
    FOnFileClick(Self);
end;

procedure TOBDRibbon.ToggleCollapsed;
begin
  Collapsed := not Collapsed;
end;

procedure TOBDRibbon.FocusSearch;
begin
  if not FShowSearch then
    Exit;
  FSearchEdit.Visible := True;
  FSearchEdit.SetFocus;
  FSearchEdit.SelectAll;
end;

procedure TOBDRibbon.SearchChanged(Sender: TObject);
var
  R: TRect;
begin
  Invalidate;
  if FSearchEdit.Text = '' then
    Exit;
  if FSearchMenu = nil then
    FSearchMenu := TOBDPopupMenu.Create(Self);
  FSearchMenu.Theme := Theme;
  FSearchMenu.Density := Density;
  PopulateCommandMenu(FSearchMenu, FSearchEdit.Text, False);
  if FSearchMenu.Items.Count = 0 then
    Exit;
  R := FSearchRect;
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  FSearchMenu.PopupAt(R);
end;

procedure TOBDRibbon.SearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FSearchEdit.Text := '';
    SetFocus;
    Key := 0;
  end;
end;

procedure TOBDRibbon.SearchMenuClick(Sender: TObject);
var
  It: TOBDRibbonItem;
begin
  It := nil;
  if Sender is TMenuItem then
    It := TOBDRibbonItem(Pointer(TMenuItem(Sender).Tag));
  ExecuteItem(It, True);
  FSearchEdit.Text := '';
end;

procedure TOBDRibbon.OverflowMenuClick(Sender: TObject);
var
  It: TOBDRibbonItem;
begin
  It := nil;
  if Sender is TMenuItem then
    It := TOBDRibbonItem(Pointer(TMenuItem(Sender).Tag));
  ExecuteItem(It, True);
end;

procedure TOBDRibbon.PopulateCommandMenu(AMenu: TPopupMenu;
  const AFilter: string; AOnlyOverflow: Boolean);
var
  I: Integer;
  Ctx: TOBDContextualTabSets;
begin
  AMenu.Items.Clear;
  AddCommandsFromTabs(AMenu, ActiveTabs, AFilter, AOnlyOverflow);
  Ctx := ActiveContextualTabs;
  for I := 0 to Ctx.Count - 1 do
    if Ctx[I].Visible then
      AddCommandsFromTabs(AMenu, Ctx[I].Tabs, AFilter, AOnlyOverflow);
end;

procedure TOBDRibbon.AddCommandsFromTabs(AMenu: TPopupMenu; ATabs: TOBDRibbonTabs;
  const AFilter: string; AOnlyOverflow: Boolean);
var
  I, J, K: Integer;
  T: TOBDRibbonTab;
  G: TOBDRibbonGroup;
  It: TOBDRibbonItem;
  MI: TMenuItem;
  S, FilterText: string;
begin
  FilterText := AnsiLowerCase(AFilter);
  for I := 0 to ATabs.Count - 1 do
  begin
    T := ATabs[I];
    if not T.Visible then
      Continue;
    for J := 0 to T.Groups.Count - 1 do
    begin
      G := T.Groups[J];
      if not G.Visible then
        Continue;
      for K := 0 to G.Items.Count - 1 do
      begin
        It := G.Items[K];
        if not It.Visible or not It.Enabled then
          Continue;
        if AOnlyOverflow and not It.FOverflow then
          Continue;
        S := StripAccel(ItemCaption(It));
        if (FilterText <> '') and (Pos(FilterText, AnsiLowerCase(S)) = 0) then
          Continue;
        MI := TMenuItem.Create(AMenu);
        MI.Caption := S;
        MI.Enabled := It.Enabled;
        MI.Checked := It.Down;
        MI.ImageIndex := ItemImageIndex(It);
        MI.Tag := NativeInt(Pointer(It));
        if AOnlyOverflow then
          MI.OnClick := OverflowMenuClick
        else
          MI.OnClick := SearchMenuClick;
        AMenu.Items.Add(MI);
      end;
    end;
  end;
end;

procedure TOBDRibbon.AppShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  if Handled then
    Exit;
  if (Msg.CharCode = Ord('Q')) and (HiWord(Msg.KeyData) and KF_ALTDOWN <> 0)
  then
  begin
    FocusSearch;
    Handled := True;
  end;
end;

end.
