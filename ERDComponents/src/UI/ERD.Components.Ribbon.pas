//------------------------------------------------------------------------------
//  ERD.Components.Ribbon
//
//  Themed ribbon for the Application application chrome.
//
//    TERDRibbon                 tabs, contextual tabs, command groups,
//                               command search, File backstage button,
//                               collapsed and simplified modes.
//    TERDRibbonTab              a normal or contextual tab with groups.
//    TERDRibbonGroup            captioned group with large and small items.
//    TERDRibbonItem             action-aware button, toggle, drop-down or
//                               split command.
//    TERDContextualTabSet       coloured set of contextual tabs.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//    2026-10-12  ERD  Alt+Q is left alone while the ribbon is hidden (menu
//                     bar command style) or its form is not the active one.
//------------------------------------------------------------------------------

unit ERD.Components.Ribbon;

interface

uses
  ERD.Components.Icons,
  ERD.Components.Hint,
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.StrUtils,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ActnList,
  Vcl.AppEvnts,
  Winapi.GDIPAPI,
  System.Generics.Collections,
  ERD.Components.TextInput,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.Menus;

type
  TERDRibbon = class;
  TERDRibbonTab = class;
  TERDRibbonGroup = class;
  TERDRibbonItem = class;
  TERDContextualTabSet = class;

  /// <summary>Visual size of a ribbon item.</summary>
  TERDRibbonItemSize = (
    /// <summary>Large stacked button in the classic body.</summary>
    risLarge,
    /// <summary>Small row button used in columns of three.</summary>
    risSmall);

  /// <summary>Interaction kind of a ribbon item.</summary>
  TERDRibbonItemKind = (
    /// <summary>Command button.</summary>
    rikButton,
    /// <summary>Checked command button.</summary>
    rikToggle,
    /// <summary>Button that opens a menu.</summary>
    rikDropDown,
    /// <summary>Command with a separate drop-down area.</summary>
    rikSplit);

  /// <summary>Ribbon item accent colour.</summary>
  TERDRibbonItemColor = (
    /// <summary>Theme foreground and accent colours.</summary>
    ricNormal,
    /// <summary>Accent command.</summary>
    ricAccent,
    /// <summary>Danger command.</summary>
    ricDanger);

  /// <summary>Contextual tab set colour source.</summary>
  TERDContextColor = (
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
  TERDRibbonAppearance = (raERDesigns, raOffice, raFlat);

  TERDRibbonStyle = (
    /// <summary>Classic ribbon with tall grouped body.</summary>
    rsClassic,
    /// <summary>Single row of small commands with overflow.</summary>
    rsSimplified);

  /// <summary>Fires when a ribbon tab changes.</summary>
  /// <param name="Sender">Ribbon.</param>
  /// <param name="ATab">New active tab.</param>
  TERDRibbonTabEvent = procedure(Sender: TObject; ATab: TERDRibbonTab)
    of object;

  /// <summary>Fires when a ribbon item is activated.</summary>
  /// <param name="Sender">Ribbon.</param>
  /// <param name="AItem">Activated item.</param>
  TERDRibbonItemEvent = procedure(Sender: TObject; AItem: TERDRibbonItem)
    of object;

  /// <summary>Owned collection of ribbon items.</summary>
  TERDRibbonItems = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDRibbonItem;
    procedure SetItem(AIndex: Integer; AValue: TERDRibbonItem);
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
    function Add: TERDRibbonItem;
    /// <summary>Indexed ribbon items.</summary>
    property Items[AIndex: Integer]: TERDRibbonItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Owned collection of ribbon groups.</summary>
  TERDRibbonGroups = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDRibbonGroup;
    procedure SetItem(AIndex: Integer; AValue: TERDRibbonGroup);
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
    function Add: TERDRibbonGroup;
    /// <summary>Indexed groups.</summary>
    property Items[AIndex: Integer]: TERDRibbonGroup read GetItem
      write SetItem; default;
  end;

  /// <summary>Owned collection of ribbon tabs.</summary>
  TERDRibbonTabs = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDRibbonTab;
    procedure SetItem(AIndex: Integer; AValue: TERDRibbonTab);
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
    function Add: TERDRibbonTab;
    /// <summary>Indexed tabs.</summary>
    property Items[AIndex: Integer]: TERDRibbonTab read GetItem
      write SetItem; default;
  end;

  /// <summary>Owned collection of contextual tab sets.</summary>
  TERDContextualTabSets = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDContextualTabSet;
    procedure SetItem(AIndex: Integer; AValue: TERDContextualTabSet);
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
    function Add: TERDContextualTabSet;
    /// <summary>Indexed contextual sets.</summary>
    property Items[AIndex: Integer]: TERDContextualTabSet read GetItem
      write SetItem; default;
  end;

  /// <summary>One command in a ribbon group.</summary>
  TERDRibbonItem = class(TCollectionItem)
  private
    FAction: TBasicAction;
    FActionLink: TActionLink;
    FCaption: TCaption;
    FHint: string;
    FIcon: TERDIcon;
    FGlyph: TERDGlyph;
    FImageIndex: Integer;
    FSize: TERDRibbonItemSize;
    FKind: TERDRibbonItemKind;
    FDropDownMenu: TPopupMenu;
    FDown: Boolean;
    FGroupIndex: Integer;
    FAllowAllUp: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    FColor: TERDRibbonItemColor;
    FTag: NativeInt;
    FOnClick: TNotifyEvent;
    FRect: TRect;
    FDropRect: TRect;
    FOverflow: Boolean;
    procedure SetAction(AValue: TBasicAction);
    procedure SetCaption(const AValue: TCaption);
    procedure SetHint(const AValue: string);
    procedure SetIcon(Value: TERDIcon);
    procedure SetGlyph(AValue: TERDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetSize(AValue: TERDRibbonItemSize);
    procedure SetKind(AValue: TERDRibbonItemKind);
    procedure SetDropDownMenu(AValue: TPopupMenu);
    procedure SetGroupIndex(Value: Integer);
    procedure SetDown(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetColor(AValue: TERDRibbonItemColor);
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
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
    property Icon: TERDIcon read FIcon write SetIcon;
    /// <summary>Optional action that supplies caption, enabled, checked,
    /// visible, hint, image index and execute behaviour.</summary>
    property Action: TBasicAction read FAction write SetAction;
    /// <summary>Text shown on the ribbon item.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>Hint shown over the item.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Built-in line glyph used when ImageIndex is -1.</summary>
    property Glyph: TERDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image list index; -1 uses Glyph.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Large or small ribbon layout.</summary>
    property Size: TERDRibbonItemSize read FSize write SetSize default risSmall;
    /// <summary>Button, toggle, drop-down or split command.</summary>
    property Kind: TERDRibbonItemKind read FKind write SetKind default rikButton;
    /// <summary>Menu opened by drop-down and split items.</summary>
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    /// <summary>Checked state for toggle commands.</summary>
    property Down: Boolean read FDown write SetDown default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp default False;
    /// <summary>Whether the item can be activated.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Whether the item participates in layout.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Normal, accent or danger colouring.</summary>
    property Color: TERDRibbonItemColor read FColor write SetColor
      default ricNormal;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
    /// <summary>Fires when the item is activated.</summary>
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  /// <summary>One captioned group in a ribbon tab.</summary>
  TERDRibbonGroup = class(TCollectionItem)
  private
    FControl, FOriginalParent: TWinControl;
    FControlWidth: Integer;
    FControlRect, FOriginalBounds: TRect;
    FOriginalAlign: TAlign;
    FOriginalAnchors: TAnchors;
    FOriginalVisible: Boolean;
    FBindTheme: Boolean;
    FCaption: TCaption;
    FVisible: Boolean;
    FShowLauncher: Boolean;
    FItems: TERDRibbonItems;
    FTag: NativeInt;
    FOnLauncherClick: TNotifyEvent;
    FRect: TRect;
    FLauncherRect: TRect;
    procedure SetCaption(const AValue: TCaption);
    procedure SetVisible(AValue: Boolean);
    procedure SetShowLauncher(AValue: Boolean);
    procedure SetItems(AValue: TERDRibbonItems);
  private
    function OwnerRibbon: TERDRibbon;
    procedure SetControl(Value: TWinControl);
    procedure SetControlWidth(Value: Integer);
    procedure RestoreControl;
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
    /// <summary>Optional ERD input, panel or gallery hosted beside commands.</summary>
    property Control: TWinControl read FControl write SetControl;
    property ControlWidth: Integer read FControlWidth write SetControlWidth default 180;
    /// <summary>Caption centered below the group.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>Whether the group participates in layout.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Shows the bottom-right dialog launcher glyph.</summary>
    property ShowLauncher: Boolean read FShowLauncher write SetShowLauncher
      default False;
    /// <summary>Commands in this group.</summary>
    property Items: TERDRibbonItems read FItems write SetItems;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
    /// <summary>Fires when the launcher is clicked.</summary>
    property OnLauncherClick: TNotifyEvent read FOnLauncherClick
      write FOnLauncherClick;
  end;

  /// <summary>One normal or contextual ribbon tab.</summary>
  TERDRibbonTab = class(TCollectionItem)
  private
    FCaption: TCaption;
    FVisible: Boolean;
    FGroups: TERDRibbonGroups;
    FTag: NativeInt;
    FRect: TRect;
    FContextColor: TColor;
    FIsContextual: Boolean;
    procedure SetCaption(const AValue: TCaption);
    procedure SetVisible(AValue: Boolean);
    procedure SetGroups(AValue: TERDRibbonGroups);
  private
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
    property Groups: TERDRibbonGroups read FGroups write SetGroups;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
  end;

  /// <summary>A coloured group of contextual tabs.</summary>
  TERDContextualTabSet = class(TCollectionItem)
  strict private
    FCaption: TCaption;
    FColor: TERDContextColor;
    FCustomColor: TColor;
    FVisible: Boolean;
    FTabs: TERDRibbonTabs;
    FTag: NativeInt;
    procedure SetCaption(const AValue: TCaption);
    procedure SetColor(AValue: TERDContextColor);
    procedure SetCustomColor(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
    procedure SetTabs(AValue: TERDRibbonTabs);
  private
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
    function ResolveColor(const APalette: TERDThemePalette): TColor;
  published
    /// <summary>Context caption for the collection editor.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>Theme or custom colour used by this set.</summary>
    property Color: TERDContextColor read FColor write SetColor default ccAccent;
    /// <summary>Colour used when Color is ccCustom.</summary>
    property CustomColor: TColor read FCustomColor write SetCustomColor
      default clDefault;
    /// <summary>Whether the contextual set is shown.</summary>
    property Visible: Boolean read FVisible write SetVisible default False;
    /// <summary>Tabs shown while this context is visible.</summary>
    property Tabs: TERDRibbonTabs read FTabs write SetTabs;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
  end;

  /// <summary>Themed application ribbon.</summary>
  TERDRibbon = class(TERDCustomControl, IERDHintProvider)
  strict private
    FTabs: TERDRibbonTabs;
    FContextualTabs: TERDContextualTabSets;
    FPreviewTabs: TERDRibbonTabs;
    FPreviewContextualTabs: TERDContextualTabSets;
    FActiveTab: Integer;
    FShowFileButton: Boolean;
    FFileCaption: TCaption;
    FBackstage: TControl;
    FBackstageOpen: Boolean;
    FShowSearch: Boolean;
    FSearchHint: string;
    FAppearance: TERDRibbonAppearance;
    FRibbonStyle: TERDRibbonStyle;
    FCollapsed: Boolean;
    FTemporaryExpanded: Boolean;
    FImages: TCustomImageList;
    FHoverTab: TERDRibbonTab;
    FHoverItem: TERDRibbonItem;
    FPressedItem: TERDRibbonItem;
    FFocusedItem: TERDRibbonItem;
    FHoverLauncher: TERDRibbonGroup;
    FHoverCollapse: Boolean;
    FFileHover: Boolean;
    FFileDown: Boolean;
    FMoreHover: Boolean;
    FMoreDown: Boolean;
    FFileRect: TRect;
    FSearchRect: TRect;
    FMoreRect: TRect;
    FCollapseRect: TRect;
    FSearchEdit: TERDTextInput;
    FSearchMenu: TERDPopupMenu;
    FOverflowMenu: TERDPopupMenu;
    FApplicationEvents: TApplicationEvents;
    FPeek: TCustomControl;
    FOnTabChange: TERDRibbonTabEvent;
    FOnFileClick: TNotifyEvent;
    FOnItemClick: TERDRibbonItemEvent;
    procedure SetTabs(AValue: TERDRibbonTabs);
    procedure SetContextualTabs(AValue: TERDContextualTabSets);
    procedure SetActiveTab(AValue: Integer);
    procedure SetShowFileButton(AValue: Boolean);
    procedure SetFileCaption(const AValue: TCaption);
    procedure SetBackstage(AValue: TControl);
    procedure SetShowSearch(AValue: Boolean);
    procedure SetSearchHint(const AValue: string);
    procedure SetAppearance(Value: TERDRibbonAppearance);
    procedure SetRibbonStyle(AValue: TERDRibbonStyle);
    procedure SetCollapsed(AValue: Boolean);
    procedure SetImages(AValue: TCustomImageList);
    function PreviewMode: Boolean;
    function ActiveTabs: TERDRibbonTabs;
    function ActiveContextualTabs: TERDContextualTabSets;
    procedure EnsurePreviewData;
    function FlatTabCount: Integer;
    function TabByFlatIndex(AIndex: Integer): TERDRibbonTab;
    function FlatIndexOfTab(ATab: TERDRibbonTab): Integer;
    function FirstVisibleTab: Integer;
    function BodyVisible: Boolean;
    function TabHeight: Integer;
    function ClassicBodyHeight: Integer;
    function SimplifiedBodyHeight: Integer;
    function BodyHeight: Integer;
    function DesiredHeight: Integer;
    procedure UpdateHeight;
    procedure LayoutSearchEdit;
    procedure NormalizeActiveTab;
    procedure DoTabChange;
    procedure DrawTabRow(APainter: TERDPainter);
    procedure DrawBody(APainter: TERDPainter; ACanvas: TCanvas);
    procedure DrawClassicBody(APainter: TERDPainter; ACanvas: TCanvas;
      ATab: TERDRibbonTab; ATop: Integer);
    procedure DrawSimplifiedBody(APainter: TERDPainter; ACanvas: TCanvas;
      ATab: TERDRibbonTab; ATop: Integer);
    function DrawLargeItem(APainter: TERDPainter; ACanvas: TCanvas;
      AItem: TERDRibbonItem; X, Y, H: Integer): Integer;
    function DrawSmallItem(APainter: TERDPainter; ACanvas: TCanvas;
      AItem: TERDRibbonItem; X, Y, H: Integer): Integer;
    procedure DrawItemGlyph(APainter: TERDPainter; ACanvas: TCanvas;
      AItem: TERDRibbonItem; CX, CY: Integer; AColor: TColor;
      AScale: Single);
    procedure DrawChevron(APainter: TERDPainter; CX, CY: Integer;
      ADown: Boolean; AColor: TColor);
    procedure DrawLauncher(APainter: TERDPainter; CX, CY: Integer;
      AColor: TColor);
    procedure ClearLayout;
    function HitTab(X, Y: Integer): TERDRibbonTab;
    function HitItem(X, Y: Integer): TERDRibbonItem;
    function HitLauncher(X, Y: Integer): TERDRibbonGroup;
    function ItemHintAt(X, Y: Integer): string;
    procedure ExecuteItem(AItem: TERDRibbonItem; AFromMenu: Boolean);
    procedure PopupItemMenu(AItem: TERDRibbonItem);
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
    procedure AddCommandsFromTabs(AMenu: TPopupMenu; ATabs: TERDRibbonTabs;
      const AFilter: string; AOnlyOverflow: Boolean);
    procedure AppShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure AppDeactivate(Sender: TObject);
    procedure ShowPeek;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMERDBackstageVisible(var Message: TMessage);
      message CM_ERD_BACKSTAGEVISIBLE;
  private
    FHostedGroups: TList<TERDRibbonGroup>;
    procedure LayoutHostedControls(Host: TWinControl);
    procedure RibbonChanged;
    // Used by the peek panel, which shows the body of a collapsed ribbon
    // over the form.
    procedure ClosePeek;
    function PeekOffset: Integer;
    function PeekHeight: Integer;
    procedure PaintPeek(ACanvas: TCanvas; AHeight: Integer);
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
    function HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
    /// <summary>Re-sizes for a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Repaints the ribbon and, while it is open, the body that a
    /// collapsed ribbon shows over the form.</summary>
    procedure Invalidate; override;
    /// <summary>Creates the ribbon with alTop alignment.</summary>
    /// <param name="AOwner">Owner component.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees owned collections and popups.</summary>
    destructor Destroy; override;
    /// <summary>Hides the backstage control when assigned.</summary>
    procedure CloseBackstage;
    /// <summary>Active tab object, or nil.</summary>
    /// <returns>Selected tab.</returns>
    function ActiveTabItem: TERDRibbonTab;
  published
    /// <summary>Normal ribbon tabs.</summary>
    property Tabs: TERDRibbonTabs read FTabs write SetTabs;
    /// <summary>Visible contextual tab sets are shown after normal tabs.</summary>
    property ContextualTabs: TERDContextualTabSets read FContextualTabs
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
    /// <summary>Visual shape, independent of theme and classic/simplified layout.</summary>
    property Appearance: TERDRibbonAppearance read FAppearance write SetAppearance default raERDesigns;
    property RibbonStyle: TERDRibbonStyle read FRibbonStyle write SetRibbonStyle
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
    property OnTabChange: TERDRibbonTabEvent read FOnTabChange write FOnTabChange;
    /// <summary>Fires after File is clicked.</summary>
    property OnFileClick: TNotifyEvent read FOnFileClick write FOnFileClick;
    /// <summary>Fires after a ribbon item is activated.</summary>
    property OnItemClick: TERDRibbonItemEvent read FOnItemClick write FOnItemClick;
  end;

implementation

uses
  ERD.Components.Backstage;

const
  LARGE_BODY_HEIGHT = 74;
  GROUP_CAPTION_HEIGHT = 22;
  CLASSIC_BODY_EXTRA = 8;
  TAB_TEXT_SIZE = ERD_TEXT_BODY;
  ITEM_TEXT_SIZE = ERD_TEXT_BODY;
  SEARCH_WIDTH = 260;

type
  // Body of a collapsed ribbon, opened by a tab click. Like the Office
  // ribbon it lies over the form below the tab row and takes no layout
  // space; the ribbon still paints it and handles its mouse input, in
  // ribbon coordinates (peek Y + PeekOffset).
  TERDRibbonPeek = class(TCustomControl, IERDHintProvider)
  strict private
    FRibbon: TERDRibbon;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd);
      message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    function HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
    constructor CreatePeek(ARibbon: TERDRibbon);
  end;

  TERDRibbonActionLink = class(TActionLink)
  strict private
    FClient: TERDRibbonItem;
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


{ TERDRibbonActionLink ------------------------------------------------------- }

procedure TERDRibbonActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TERDRibbonItem;
end;

function TERDRibbonActionLink.IsCaptionLinked: Boolean;
begin
  Result := True;
end;

function TERDRibbonActionLink.IsCheckedLinked: Boolean;
begin
  Result := True;
end;

function TERDRibbonActionLink.IsEnabledLinked: Boolean;
begin
  Result := True;
end;

function TERDRibbonActionLink.IsHintLinked: Boolean;
begin
  Result := True;
end;

function TERDRibbonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := True;
end;

function TERDRibbonActionLink.IsVisibleLinked: Boolean;
begin
  Result := True;
end;

procedure TERDRibbonActionLink.SetCaption(const Value: string);
begin
  if FClient <> nil then
  begin
    FClient.FCaption := Value;
    FClient.ItemChanged;
  end;
end;

procedure TERDRibbonActionLink.SetChecked(Value: Boolean);
begin
  if FClient <> nil then
  begin
    FClient.SetDown(Value);
    FClient.ItemChanged;
  end;
end;

procedure TERDRibbonActionLink.SetEnabled(Value: Boolean);
begin
  if FClient <> nil then
  begin
    FClient.FEnabled := Value;
    FClient.ItemChanged;
  end;
end;

procedure TERDRibbonActionLink.SetHint(const Value: string);
begin
  if FClient <> nil then
  begin
    FClient.FHint := Value;
    FClient.ItemChanged;
  end;
end;

procedure TERDRibbonActionLink.SetImageIndex(Value: Integer);
begin
  if FClient <> nil then
  begin
    FClient.FImageIndex := Value;
    FClient.ItemChanged;
  end;
end;

procedure TERDRibbonActionLink.SetVisible(Value: Boolean);
begin
  if FClient <> nil then
  begin
    FClient.FVisible := Value;
    FClient.ItemChanged;
  end;
end;

{ Helpers ------------------------------------------------------------------- }

function ItemCaption(AItem: TERDRibbonItem): string;
begin
  Result := '';
  if AItem = nil then
    Exit;
  Result := AItem.Caption;
  if (Result = '') and (AItem.Action is TContainedAction) then
    Result := TContainedAction(AItem.Action).Caption;
end;

function ItemHint(AItem: TERDRibbonItem): string;
begin
  Result := '';
  if AItem = nil then
    Exit;
  Result := AItem.Hint;
  if (Result = '') and (AItem.Action is TContainedAction) then
    Result := TContainedAction(AItem.Action).Hint;
end;

function ItemImageIndex(AItem: TERDRibbonItem): Integer;
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

constructor TERDRibbonItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDRibbonItem);
end;

function TERDRibbonItems.Add: TERDRibbonItem;
begin
  Result := TERDRibbonItem(inherited Add);
end;

function TERDRibbonItems.GetItem(AIndex: Integer): TERDRibbonItem;
begin
  Result := TERDRibbonItem(inherited GetItem(AIndex));
end;

procedure TERDRibbonItems.SetItem(AIndex: Integer; AValue: TERDRibbonItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDRibbonItems.Update(Item: TCollectionItem);
var
  G: TPersistent;
begin
  inherited;
  G := GetOwner;
  if G is TERDRibbonGroup then
    TERDRibbonGroup(G).GroupChanged;
end;

constructor TERDRibbonGroups.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDRibbonGroup);
end;

function TERDRibbonGroups.Add: TERDRibbonGroup;
begin
  Result := TERDRibbonGroup(inherited Add);
end;

function TERDRibbonGroups.GetItem(AIndex: Integer): TERDRibbonGroup;
begin
  Result := TERDRibbonGroup(inherited GetItem(AIndex));
end;

procedure TERDRibbonGroups.SetItem(AIndex: Integer; AValue: TERDRibbonGroup);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDRibbonGroups.Update(Item: TCollectionItem);
var
  T: TPersistent;
begin
  inherited;
  T := GetOwner;
  if T is TERDRibbonTab then
    TERDRibbonTab(T).TabChanged;
end;

constructor TERDRibbonTabs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDRibbonTab);
end;

function TERDRibbonTabs.Add: TERDRibbonTab;
begin
  Result := TERDRibbonTab(inherited Add);
end;

function TERDRibbonTabs.GetItem(AIndex: Integer): TERDRibbonTab;
begin
  Result := TERDRibbonTab(inherited GetItem(AIndex));
end;

procedure TERDRibbonTabs.SetItem(AIndex: Integer; AValue: TERDRibbonTab);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDRibbonTabs.Update(Item: TCollectionItem);
var
  O: TPersistent;
begin
  inherited;
  O := GetOwner;
  if O is TERDRibbon then
    TERDRibbon(O).RibbonChanged
  else if O is TERDContextualTabSet then
    TERDContextualTabSet(O).SetChanged;
end;

constructor TERDContextualTabSets.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDContextualTabSet);
end;

function TERDContextualTabSets.Add: TERDContextualTabSet;
begin
  Result := TERDContextualTabSet(inherited Add);
end;

function TERDContextualTabSets.GetItem(AIndex: Integer): TERDContextualTabSet;
begin
  Result := TERDContextualTabSet(inherited GetItem(AIndex));
end;

procedure TERDContextualTabSets.SetItem(AIndex: Integer;
  AValue: TERDContextualTabSet);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDContextualTabSets.Update(Item: TCollectionItem);
var
  O: TPersistent;
begin
  inherited;
  O := GetOwner;
  if O is TERDRibbon then
    TERDRibbon(O).RibbonChanged;
end;

{ TERDRibbonItem ------------------------------------------------------------ }

procedure TERDRibbonItem.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

constructor TERDRibbonItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FIcon := TERDIcon.Create(Self);
  FImageIndex := -1;
  FSize := risSmall;
  FKind := rikButton;
  FEnabled := True;
  FVisible := True;
  FGlyph := glNone;
end;

destructor TERDRibbonItem.Destroy;
begin
  FIcon.Free;
  FActionLink.Free;
  inherited Destroy;
end;

procedure TERDRibbonItem.Assign(Source: TPersistent);
var
  S: TERDRibbonItem;
begin
  if Source is TERDRibbonItem then
  begin
    S := TERDRibbonItem(Source);
    Action := S.Action;
    Caption := S.Caption;
    Hint := S.Hint;
    Glyph := S.Glyph;
    FIcon.Assign(S.Icon);
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

procedure TERDRibbonItem.ItemChanged;
begin
  inherited Changed(False);
end;

procedure TERDRibbonItem.SetAction(AValue: TBasicAction);
begin
  if FAction = AValue then
    Exit;
  if FActionLink = nil then
    FActionLink := TERDRibbonActionLink.Create(Self);
  FAction := AValue;
  FActionLink.Action := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetGlyph(AValue: TERDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetSize(AValue: TERDRibbonItemSize);
begin
  if FSize = AValue then
    Exit;
  FSize := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetKind(AValue: TERDRibbonItemKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetDropDownMenu(AValue: TPopupMenu);
begin
  if FDropDownMenu = AValue then
    Exit;
  FDropDownMenu := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetGroupIndex(Value: Integer);
begin
  if Value < 0 then raise EArgumentOutOfRangeException.Create('GroupIndex must be nonnegative');
  if FGroupIndex = Value then Exit;
  FGroupIndex := Value;
  SetDown(FDown);
  ItemChanged;
end;
procedure TERDRibbonItem.SetDown(AValue: Boolean);
var I: Integer; Peer: TERDRibbonItem;
begin
  if AValue and (FGroupIndex > 0) and (Collection <> nil) then
    for I := 0 to Collection.Count - 1 do
    begin
      Peer := TERDRibbonItem(Collection.Items[I]);
      if (Peer <> Self) and (Peer.GroupIndex = FGroupIndex) and Peer.Down then Peer.Down := False;
    end;
  if FDown = AValue then
    Exit;
  FDown := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  ItemChanged;
end;

procedure TERDRibbonItem.SetColor(AValue: TERDRibbonItemColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  ItemChanged;
end;

function TERDRibbonItem.GetDisplayName: string;
begin
  Result := StripAccel(ItemCaption(Self));
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDRibbonItem.Execute;
begin
  if not FEnabled then
    Exit;
  if FKind = rikToggle then
    if (FGroupIndex > 0) and not FAllowAllUp then Down := True
    else Down := not Down;
  if FAction <> nil then
    FAction.Execute;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TERDRibbonGroup.OwnerRibbon: TERDRibbon;
var Owner: TPersistent;
begin
  Result := nil;
  if Collection = nil then Exit;
  Owner := Collection.Owner;
  if Owner is TERDRibbonTab then Owner := TERDRibbonTab(Owner).Collection.Owner;
  if Owner is TERDContextualTabSet then Owner := TERDContextualTabSet(Owner).Collection.Owner;
  if Owner is TERDRibbon then Result := TERDRibbon(Owner);
end;

procedure TERDRibbonGroup.RestoreControl;
var Ribbon: TERDRibbon;
begin
  Ribbon := OwnerRibbon;
  if Ribbon <> nil then Ribbon.FHostedGroups.Remove(Self);
  if FControl <> nil then
  begin
    if not (csDestroying in FControl.ComponentState) then
    begin
      FControl.Parent := FOriginalParent;
      FControl.Align := FOriginalAlign;
      FControl.Anchors := FOriginalAnchors;
      FControl.BoundsRect := FOriginalBounds;
      FControl.Visible := FOriginalVisible;
      if FBindTheme and (FControl is TERDCustomControl) then TERDCustomControl(FControl).Theme := nil;
    end;
    if Ribbon <> nil then FControl.RemoveFreeNotification(Ribbon);
  end;
  FControl := nil;
  FOriginalParent := nil;
end;

procedure TERDRibbonGroup.SetControl(Value: TWinControl);
var Ribbon: TERDRibbon; Parent: TWinControl; Group: TERDRibbonGroup;
begin
  if FControl = Value then Exit;
  Ribbon := OwnerRibbon;
  if (Value <> nil) and (Ribbon <> nil) then
  begin
    Parent := Ribbon;
    while Parent <> nil do
    begin
      if Parent = Value then raise EArgumentException.Create('Ribbon cannot host itself or an ancestor');
      Parent := Parent.Parent;
    end;
    for Group in Ribbon.FHostedGroups do
      if Group.Control = Value then raise EArgumentException.Create('Control is already bound to a ribbon group');
    if (Value.Parent is TERDRibbon) and (Value.Parent <> Ribbon) then
      raise EArgumentException.Create('Control is already hosted by another ribbon');
  end;
  RestoreControl;
  FControl := Value;
  if Value <> nil then
  begin
    FOriginalParent := Value.Parent;
    FOriginalBounds := Value.BoundsRect;
    FOriginalAlign := Value.Align;
    FOriginalAnchors := Value.Anchors;
    FOriginalVisible := Value.Visible;
    FBindTheme := (Value is TERDCustomControl) and (TERDCustomControl(Value).Theme = nil);
    if Ribbon <> nil then
    begin
      Value.FreeNotification(Ribbon);
      if (FOriginalParent <> nil) and (FOriginalParent <> Ribbon) then FOriginalParent.FreeNotification(Ribbon);
      Ribbon.FHostedGroups.Add(Self);
    end;
    // Ownership stays with the application form; only parenting changes.
  end;
  GroupChanged;
end;

procedure TERDRibbonGroup.SetControlWidth(Value: Integer);
begin
  Value := EnsureRange(Value, 32, 2000);
  if FControlWidth = Value then Exit;
  FControlWidth := Value;
  GroupChanged;
end;

procedure TERDRibbon.LayoutHostedControls(Host: TWinControl);
var Group: TERDRibbonGroup; R: TRect; Control: TWinControl; Visible: Boolean;
begin
  if Host = nil then Exit;
  for Group in FHostedGroups do
  begin
    Control := Group.Control;
    if (Control = nil) or (csDestroying in Control.ComponentState) then Continue;
    R := Group.FControlRect;
    Visible := not R.IsEmpty and Group.Visible and
      ((Host <> Self) or BodyVisible) and (R.Left >= 0) and (R.Right <= Host.ClientWidth);
    if not Visible then
    begin
      if Control.Visible then Control.Visible := False;
      Continue;
    end;
    if Control.Parent <> Host then Control.Parent := Host;
    if Control.Align <> alNone then Control.Align := alNone;
    if Control.Anchors <> [akLeft, akTop] then Control.Anchors := [akLeft, akTop];
    if Group.FBindTheme and (Control is TERDCustomControl) then TERDCustomControl(Control).Theme := Theme;
    if not EqualRect(Control.BoundsRect, R) then Control.BoundsRect := R;
    if not Control.Visible then Control.Visible := True;
  end;
end;

{ TERDRibbonGroup ----------------------------------------------------------- }

constructor TERDRibbonGroup.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FControlWidth := 180;
  FItems := TERDRibbonItems.Create(Self);
end;

destructor TERDRibbonGroup.Destroy;
begin
  RestoreControl;
  FItems.Free;
  inherited Destroy;
end;

procedure TERDRibbonGroup.Assign(Source: TPersistent);
var
  S: TERDRibbonGroup;
begin
  if Source is TERDRibbonGroup then
  begin
    S := TERDRibbonGroup(Source);
    ControlWidth := S.ControlWidth;
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

procedure TERDRibbonGroup.GroupChanged;
begin
  inherited Changed(False);
end;

procedure TERDRibbonGroup.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  GroupChanged;
end;

procedure TERDRibbonGroup.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  GroupChanged;
end;

procedure TERDRibbonGroup.SetShowLauncher(AValue: Boolean);
begin
  if FShowLauncher = AValue then
    Exit;
  FShowLauncher := AValue;
  GroupChanged;
end;

procedure TERDRibbonGroup.SetItems(AValue: TERDRibbonItems);
begin
  FItems.Assign(AValue);
end;

function TERDRibbonGroup.GetDisplayName: string;
begin
  Result := StripAccel(FCaption);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDRibbonGroup.ClickLauncher;
begin
  if Assigned(FOnLauncherClick) then
    FOnLauncherClick(Self);
end;

{ TERDRibbonTab ------------------------------------------------------------- }

constructor TERDRibbonTab.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FGroups := TERDRibbonGroups.Create(Self);
end;

destructor TERDRibbonTab.Destroy;
begin
  FGroups.Free;
  inherited Destroy;
end;

procedure TERDRibbonTab.Assign(Source: TPersistent);
var
  S: TERDRibbonTab;
begin
  if Source is TERDRibbonTab then
  begin
    S := TERDRibbonTab(Source);
    Caption := S.Caption;
    Visible := S.Visible;
    Groups := S.Groups;
    Tag := S.Tag;
  end
  else
    inherited Assign(Source);
end;

procedure TERDRibbonTab.TabChanged;
begin
  inherited Changed(False);
end;

procedure TERDRibbonTab.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  TabChanged;
end;

procedure TERDRibbonTab.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  TabChanged;
end;

procedure TERDRibbonTab.SetGroups(AValue: TERDRibbonGroups);
begin
  FGroups.Assign(AValue);
end;

function TERDRibbonTab.GetDisplayName: string;
begin
  Result := StripAccel(FCaption);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TERDContextualTabSet ------------------------------------------------------ }

constructor TERDContextualTabSet.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := ccAccent;
  FCustomColor := clDefault;
  FVisible := False;
  FTabs := TERDRibbonTabs.Create(Self);
end;

destructor TERDContextualTabSet.Destroy;
begin
  FTabs.Free;
  inherited Destroy;
end;

procedure TERDContextualTabSet.Assign(Source: TPersistent);
var
  S: TERDContextualTabSet;
begin
  if Source is TERDContextualTabSet then
  begin
    S := TERDContextualTabSet(Source);
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

procedure TERDContextualTabSet.SetChanged;
begin
  inherited Changed(False);
end;

procedure TERDContextualTabSet.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  SetChanged;
end;

procedure TERDContextualTabSet.SetColor(AValue: TERDContextColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  SetChanged;
end;

procedure TERDContextualTabSet.SetCustomColor(AValue: TColor);
begin
  if FCustomColor = AValue then
    Exit;
  FCustomColor := AValue;
  SetChanged;
end;

procedure TERDContextualTabSet.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  SetChanged;
end;

procedure TERDContextualTabSet.SetTabs(AValue: TERDRibbonTabs);
begin
  FTabs.Assign(AValue);
end;

function TERDContextualTabSet.GetDisplayName: string;
begin
  Result := StripAccel(FCaption);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TERDContextualTabSet.ResolveColor(
  const APalette: TERDThemePalette): TColor;
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

{ TERDRibbon ---------------------------------------------------------------- }

{ TERDRibbonPeek }

function TERDRibbonPeek.HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
begin Result := (FRibbon <> nil) and FRibbon.HintContentAt(ScreenPoint,Content); end;
constructor TERDRibbonPeek.CreatePeek(ARibbon: TERDRibbon);
begin
  inherited Create(ARibbon);
  FRibbon := ARibbon;
  ControlStyle := ControlStyle + [csOpaque];
  Visible := False;
end;

procedure TERDRibbonPeek.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TERDRibbonPeek.Paint;
var
  Buffer: TBitmap;
  Offset: Integer;
begin
  // Paint the whole ribbon as if it were expanded and show the body part;
  // the item rectangles it records are the ones the ribbon hit-tests.
  Offset := FRibbon.PeekOffset;
  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Width, Offset + Height);
    FRibbon.PaintPeek(Buffer.Canvas, Offset + Height);
    BitBlt(Canvas.Handle, 0, 0, Width, Height, Buffer.Canvas.Handle, 0,
      Offset, SRCCOPY);
  finally
    Buffer.Free;
  end;
end;

procedure TERDRibbonPeek.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FRibbon.MouseMove(Shift, X, Y + FRibbon.PeekOffset);
end;

procedure TERDRibbonPeek.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FRibbon.MouseDown(Button, Shift, X, Y + FRibbon.PeekOffset);
end;

procedure TERDRibbonPeek.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FRibbon.MouseUp(Button, Shift, X, Y + FRibbon.PeekOffset);
end;

procedure TERDRibbonPeek.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FRibbon.Perform(CM_MOUSELEAVE, 0, 0);
end;

procedure TERDRibbonPeek.CMHintShow(var Message: TCMHintShow);
var
  Offset: Integer;
begin
  Offset := FRibbon.PeekOffset;
  Inc(Message.HintInfo.CursorPos.Y, Offset);
  Message.Result := FRibbon.Perform(CM_HINTSHOW, 0,
    LPARAM(Message.HintInfo));
  Dec(Message.HintInfo.CursorPos.Y, Offset);
  OffsetRect(Message.HintInfo.CursorRect, 0, -Offset);
end;

{ TERDRibbon }

function TERDRibbon.HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
var Item: TERDRibbonItem; P: TPoint;
begin
  Content := Default(TERDHintContent); Content.ImageIndex := -1; P := ScreenToClient(ScreenPoint);
  if FTemporaryExpanded and (FPeek <> nil) and FPeek.Visible then begin P := FPeek.ScreenToClient(ScreenPoint); Inc(P.Y,PeekOffset); end;
  Item := HitItem(P.X,P.Y); Result := Item <> nil; if not Result then Exit;
  Content.Title := ItemCaption(Item); Content.Description := ItemHint(Item); Content.Glyph := Item.Glyph; Content.Icon := Item.Icon;
  Content.Images := Images; Content.ImageIndex := ItemImageIndex(Item);
  Content.Palette := Palette; Content.HasPalette := True; Content.Density := Density; Content.HasDensity := True;
  if Item.Action is TCustomAction then Content.Shortcut := ShortCutToText(TCustomAction(Item.Action).ShortCut);
end;

constructor TERDRibbon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alTop;
  Width := 640;
  Height := 132;
  TabStop := True;
  FHostedGroups := TList<TERDRibbonGroup>.Create;
  FTabs := TERDRibbonTabs.Create(Self);
  FContextualTabs := TERDContextualTabSets.Create(Self);
  FPreviewTabs := TERDRibbonTabs.Create(nil);
  FPreviewContextualTabs := TERDContextualTabSets.Create(nil);
  FActiveTab := 0;
  FShowFileButton := True;
  FFileCaption := 'File';
  FShowSearch := True;
  FSearchHint := 'Search commands  Alt+Q';
  FRibbonStyle := rsClassic;
  FSearchEdit := TERDTextInput.Create(Self);
  FSearchEdit.Parent := Self;
  FSearchEdit.BorderStyle := bsNone;
  FSearchEdit.AutoSelect := True;
  FSearchEdit.BorderStyle := bsNone;
  FSearchEdit.AutoSize := False;
  FSearchEdit.Visible := False;
  FSearchEdit.TextHint := 'Search commands';
  FSearchEdit.OnChange := SearchChanged;
  FSearchEdit.OnKeyDown := SearchKeyDown;
  FApplicationEvents := TApplicationEvents.Create(Self);
  FApplicationEvents.OnShortCut := AppShortCut;
  FApplicationEvents.OnMessage := AppMessage;
  FApplicationEvents.OnDeactivate := AppDeactivate;
  UpdateHeight;
end;

destructor TERDRibbon.Destroy;
begin
  FApplicationEvents.OnShortCut := nil;
  FApplicationEvents.OnMessage := nil;
  FApplicationEvents.OnDeactivate := nil;
  FOverflowMenu.Free;
  FSearchMenu.Free;
  FPreviewContextualTabs.Free;
  FPreviewTabs.Free;
  FContextualTabs.Free;
  FTabs.Free;
  FreeAndNil(FHostedGroups);
  inherited Destroy;
end;

procedure TERDRibbon.SetTabs(AValue: TERDRibbonTabs);
begin
  FTabs.Assign(AValue);
end;

procedure TERDRibbon.SetContextualTabs(AValue: TERDContextualTabSets);
begin
  FContextualTabs.Assign(AValue);
end;

procedure TERDRibbon.SetActiveTab(AValue: Integer);
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
  UpdateHeight;
  Invalidate;
  DoTabChange;
end;

procedure TERDRibbon.SetShowFileButton(AValue: Boolean);
begin
  if FShowFileButton = AValue then
    Exit;
  FShowFileButton := AValue;
  RibbonChanged;
end;

procedure TERDRibbon.SetFileCaption(const AValue: TCaption);
begin
  if FFileCaption = AValue then
    Exit;
  FFileCaption := AValue;
  RibbonChanged;
end;

procedure TERDRibbon.SetBackstage(AValue: TControl);
begin
  if FBackstage = AValue then
    Exit;
  if FBackstage <> nil then
  begin
    FBackstage.RemoveFreeNotification(Self);
    FBackstage.Perform(CM_ERD_BACKSTAGELINK, 0, 0);
  end;
  FBackstage := AValue;
  if FBackstageOpen then
  begin
    FBackstageOpen := False;
    UpdateHeight;
  end;
  if FBackstage <> nil then
  begin
    FBackstage.FreeNotification(Self);
    // A TERDBackstage answers by reporting its visibility, so the ribbon
    // steps aside while it is open.
    FBackstage.Perform(CM_ERD_BACKSTAGELINK, 0, LPARAM(Self));
  end;
end;

procedure TERDRibbon.CMERDBackstageVisible(var Message: TMessage);
var
  Open: Boolean;
begin
  // Like the Office backstage view, the open backstage replaces the tabs
  // and commands: the ribbon collapses to zero height until it closes.
  Open := (Message.WParam <> 0) and not (csDesigning in ComponentState);
  if FBackstageOpen = Open then
    Exit;
  FBackstageOpen := Open;
  if Open then
  begin
    ERDCloseThemedMenu;
    ClosePeek;
  end;
  UpdateHeight;
  Invalidate;
end;

procedure TERDRibbon.SetShowSearch(AValue: Boolean);
begin
  if FShowSearch = AValue then
    Exit;
  FShowSearch := AValue;
  LayoutSearchEdit;
  Invalidate;
end;

procedure TERDRibbon.SetSearchHint(const AValue: string);
begin
  if FSearchHint = AValue then
    Exit;
  FSearchHint := AValue;
  FSearchEdit.TextHint := AValue;
  Invalidate;
end;

procedure TERDRibbon.SetAppearance(Value: TERDRibbonAppearance);
begin
  if FAppearance = Value then Exit;
  FAppearance := Value;
  ClosePeek;
  Invalidate;
end;

procedure TERDRibbon.SetRibbonStyle(AValue: TERDRibbonStyle);
begin
  if FRibbonStyle = AValue then
    Exit;
  FRibbonStyle := AValue;
  RibbonChanged;
end;

procedure TERDRibbon.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed = AValue then
    Exit;
  FCollapsed := AValue;
  ClosePeek;
  UpdateHeight;
  Invalidate;
end;

procedure TERDRibbon.SetImages(AValue: TCustomImageList);
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

procedure TERDRibbon.Notification(AComponent: TComponent;
  Operation: TOperation);
var I: Integer; G: TERDRibbonGroup;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if FHostedGroups <> nil then
      for I := FHostedGroups.Count - 1 downto 0 do
      begin
        G := FHostedGroups[I];
        if G.FOriginalParent = AComponent then G.FOriginalParent := nil;
        if G.FControl = AComponent then
        begin G.FControl := nil; FHostedGroups.Delete(I) end;
      end;
    if AComponent = FImages then
      FImages := nil;
    if AComponent = FPeek then
    begin
      FPeek := nil;
      FTemporaryExpanded := False;
    end;
    if AComponent = FBackstage then
    begin
      FBackstage := nil;
      if FBackstageOpen and not (csDestroying in ComponentState) then
      begin
        FBackstageOpen := False;
        UpdateHeight;
      end;
    end;
  end;
end;

procedure TERDRibbon.CloseBackstage;
begin
  if (FBackstage = nil) or not FBackstage.Visible then
    Exit;
  if FBackstage is TERDBackstage then
    TERDBackstage(FBackstage).Close
  else
    FBackstage.Visible := False;
end;

function TERDRibbon.ActiveTabItem: TERDRibbonTab;
begin
  Result := TabByFlatIndex(FActiveTab);
end;

function TERDRibbon.PreviewMode: Boolean;
begin
  Result := (FTabs.Count = 0) and IsPreview;
end;

function TERDRibbon.ActiveTabs: TERDRibbonTabs;
begin
  if PreviewMode then
  begin
    EnsurePreviewData;
    Result := FPreviewTabs;
  end
  else
    Result := FTabs;
end;

function TERDRibbon.ActiveContextualTabs: TERDContextualTabSets;
begin
  if PreviewMode then
  begin
    EnsurePreviewData;
    Result := FPreviewContextualTabs;
  end
  else
    Result := FContextualTabs;
end;

procedure TERDRibbon.EnsurePreviewData;
var
  T: TERDRibbonTab;
  G: TERDRibbonGroup;
  I: TERDRibbonItem;
  C: TERDContextualTabSet;
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
  G.Caption := 'Files';
  G.ShowLauncher := True;
  I := G.Items.Add;
  I.Caption := 'Load files';
  I.Glyph := glDownload;
  I.Size := risLarge;
  I.Kind := rikButton;
  I := G.Items.Add;
  I.Caption := 'Clear list';
  I.Glyph := glClose;
  I.Size := risLarge;
  I.Kind := rikSplit;
  I.Color := ricDanger;
  I := G.Items.Add;
  I.Caption := 'Snapshot';
  I.Glyph := glCopy;
  I.Size := risSmall;
  I := G.Items.Add;
  I.Caption := 'Selected only';
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
  T.Caption := 'Activity';
  G := T.Groups.Add;
  G.Caption := 'Capture';
  I := G.Items.Add;
  I.Caption := 'Record';
  I.Glyph := glPlay;
  I.Size := risLarge;
  I.Color := ricDanger;
  I := G.Items.Add;
  I.Caption := 'Filter';
  I.Glyph := glFilter;
  I.Size := risSmall;
  I.Kind := rikDropDown;

  T := FPreviewTabs.Add;
  T.Caption := 'Project';
  T.Groups.Add.Caption := 'Project';
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

function TERDRibbon.FlatTabCount: Integer;
var
  Tabs: TERDRibbonTabs;
  Ctx: TERDContextualTabSets;
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

function TERDRibbon.TabByFlatIndex(AIndex: Integer): TERDRibbonTab;
var
  Tabs: TERDRibbonTabs;
  Ctx: TERDContextualTabSets;
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

function TERDRibbon.FlatIndexOfTab(ATab: TERDRibbonTab): Integer;
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

function TERDRibbon.FirstVisibleTab: Integer;
begin
  if FlatTabCount > 0 then
    Result := 0
  else
    Result := -1;
end;

function TERDRibbon.BodyVisible: Boolean;
begin
  Result := not FCollapsed or FTemporaryExpanded;
end;

function TERDRibbon.TabHeight: Integer;
begin
  Result := ScaleValue(Metrics.Tab);
end;

function TERDRibbon.ClassicBodyHeight: Integer;
begin
  Result := ScaleValue(LARGE_BODY_HEIGHT + GROUP_CAPTION_HEIGHT +
    CLASSIC_BODY_EXTRA);
end;

function TERDRibbon.SimplifiedBodyHeight: Integer;
begin
  Result := ScaleValue(Metrics.Button + 12);
end;

function TERDRibbon.BodyHeight: Integer;
begin
  if FRibbonStyle = rsSimplified then
    Result := SimplifiedBodyHeight
  else
    Result := ClassicBodyHeight;
end;

function TERDRibbon.DesiredHeight: Integer;
begin
  if FBackstageOpen then
    Exit(0);
  // A collapsed ribbon keeps the tab row height while its body is open:
  // the body is the peek panel over the form.
  Result := TabHeight;
  if not FCollapsed then
    Inc(Result, BodyHeight);
end;

procedure TERDRibbon.UpdateHeight;
begin
  Height := DesiredHeight;
  LayoutSearchEdit;
end;

procedure TERDRibbon.LayoutSearchEdit;
var
  R: TRect;
  FontHeight, EditHeight: Integer;
begin
  if FSearchEdit = nil then
    Exit;
  R := FSearchRect;
  InflateRect(R, -ScaleValue(32), -ScaleValue(3));
  R.Right := FSearchRect.Right - ScaleValue(54);
  if (not FShowSearch) or (R.Right <= R.Left) or (R.Bottom <= R.Top) then
  begin
    FSearchEdit.Visible := False;
    Exit;
  end;
  // Same 12.5 px text as the painted placeholder. A borderless TEdit draws
  // its text at the top of its client area, so size it to exactly one text
  // line (measured from the font) and centre that line in the search box
  // (it is much taller in tablet density).
  FontHeight := System.Math.Max(1, Round(12.5 * ScaleValue(96) / 96));
  if not SameText(FSearchEdit.Font.Name, 'Segoe UI') then
    FSearchEdit.Font.Name := 'Segoe UI';
  if FSearchEdit.Font.Height <> -FontHeight then
    FSearchEdit.Font.Height := -FontHeight;
  EditHeight := System.Math.Max(1, System.Math.Min(R.Height,
    ERDFontLineHeight(FSearchEdit.Font)));
  R.Top := FSearchRect.Top + (FSearchRect.Height - EditHeight) div 2;
  if (FSearchEdit.Left <> R.Left) or (FSearchEdit.Top <> R.Top) or
    (FSearchEdit.Width <> R.Width) or (FSearchEdit.Height <> EditHeight) then
    FSearchEdit.SetBounds(R.Left, R.Top, R.Width, EditHeight);
  if FSearchEdit.Color <> Palette.Background then
    FSearchEdit.Color := Palette.Background;
  if FSearchEdit.Font.Color <> Palette.ForegroundText then
    FSearchEdit.Font.Color := Palette.ForegroundText;
  FSearchEdit.Visible := True;
end;

procedure TERDRibbon.RibbonChanged;
begin
  if FTemporaryExpanded and (FPeek <> nil) then FPeek.Invalidate;
  NormalizeActiveTab;
  UpdateHeight;
  Invalidate;
end;

procedure TERDRibbon.NormalizeActiveTab;
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

procedure TERDRibbon.DoTabChange;
var
  T: TERDRibbonTab;
begin
  T := ActiveTabItem;
  if Assigned(FOnTabChange) then
    FOnTabChange(Self, T);
end;

procedure TERDRibbon.ClearLayout;
var
  I, J, K: Integer;
  Tabs: TERDRibbonTabs;
  Ctx: TERDContextualTabSets;
  T: TERDRibbonTab;
begin
  for I := 0 to FHostedGroups.Count - 1 do
    FHostedGroups[I].FControlRect := Rect(0, 0, 0, 0);
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

procedure TERDRibbon.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
begin
  NormalizeActiveTab;
  ClearLayout;
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if FAppearance = raERDesigns then P.FillRect(ClientRect, Palette.GaugeFace)
    else P.FillRect(ClientRect, Palette.Background);
    DrawTabRow(P);
    if BodyVisible then
      DrawBody(P, ACanvas);
  finally
    P.Free;
  end;
  LayoutSearchEdit;
  if not FTemporaryExpanded then LayoutHostedControls(Self);
end;

procedure TERDRibbon.DrawTabRow(APainter: TERDPainter);
var
  X, RX, W, H, I, Tw: Integer;
  T: TERDRibbonTab;
  Active: TERDRibbonTab;
  Col: TColor;
  Weight: TERDTextWeight;
  Tabs: TERDRibbonTabs;
  Ctx: TERDContextualTabSets;
  J, K: Integer;
  TextColor: TColor;
begin
  H := TabHeight;
  if FAppearance = raERDesigns then
    APainter.FillRect(Rect(0, 0, Width, H), Palette.GaugeFace)
  else APainter.FillRect(Rect(0, 0, Width, H), Palette.Background);
  X := ScaleValue(6);
  FFileRect := Rect(0, 0, 0, 0);
  if FShowFileButton then
  begin
    W := APainter.TextWidth(FFileCaption, TAB_TEXT_SIZE, twSemibold) +
      ScaleValue(28);
    FFileRect := Rect(X, ScaleValue(5), X + W, H - ScaleValue(5));
    Col := Palette.Accent;
    if FFileDown then
      Col := ERDMixColor(Palette.ForegroundText, Col, 0.12)
    else if FFileHover then
      Col := ERDMixColor(clWhite, Col, 0.15);
    if FAppearance = raERDesigns then APainter.FillRect(FFileRect, Col)
    else if FFileHover or FFileDown then
      APainter.RoundRect(FFileRect.Left, FFileRect.Top, FFileRect.Width,
        FFileRect.Height, ScaleValue(4), APainter.Tint(Palette.Accent, 0.10), clNone);
    if FAppearance <> raERDesigns then Col := Palette.ForegroundText
    else Col := APainter.OnAccent;
    APainter.Text(FFileRect.Left + FFileRect.Width div 2,
      FFileRect.Top + FFileRect.Height div 2, FFileCaption, TAB_TEXT_SIZE,
      Col, twSemibold, taCenter);
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
      Palette.Subtle, 0.9);
    if FSearchEdit.Text = '' then
      APainter.Text(FSearchRect.Left + ScaleValue(32), H div 2,
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
        if FAppearance = raOffice then TextColor := Palette.ForegroundText
        else TextColor := APainter.AccentText;
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
          ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06));
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

procedure TERDRibbon.DrawBody(APainter: TERDPainter; ACanvas: TCanvas);
var
  T: TERDRibbonTab;
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

procedure TERDRibbon.DrawClassicBody(APainter: TERDPainter; ACanvas: TCanvas;
  ATab: TERDRibbonTab; ATop: Integer);
var
  GX, BX, StartX, GW, I, J, BodyH, CapH, RH, ColW, ItemW: Integer;
  G: TERDRibbonGroup;
  It: TERDRibbonItem;
  HasSmall: Boolean;
  CY: Integer;
begin
  BodyH := ScaleValue(LARGE_BODY_HEIGHT);
  CapH := ScaleValue(GROUP_CAPTION_HEIGHT);
  if FAppearance = raOffice then
  begin
    APainter.FillRect(Rect(0, ATop, Width, ATop + ClassicBodyHeight), Palette.Background);
    APainter.RoundRect(ScaleValue(4), ATop + ScaleValue(4), Width - ScaleValue(8),
      ClassicBodyHeight - ScaleValue(5), ScaleValue(8),
      APainter.Tint(Palette.NeutralDark, 0.08), clNone);
    APainter.RoundRect(ScaleValue(4), ATop + ScaleValue(2), Width - ScaleValue(8),
      ClassicBodyHeight - ScaleValue(6), ScaleValue(8), Palette.GaugeFace,
      Palette.NeutralLight);
  end
  else APainter.FillRect(Rect(0, ATop, Width, ATop + ClassicBodyHeight), Palette.GaugeFace);
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
    if G.Control <> nil then
    begin
      G.FControlRect := Rect(BX + ScaleValue(2), ATop + ScaleValue(6),
        BX + ScaleValue(G.ControlWidth), ATop + ScaleValue(4) + BodyH);
      Inc(BX, ScaleValue(G.ControlWidth + 4));
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
  if FAppearance = raERDesigns then
    APainter.HLine(0, ATop + ClassicBodyHeight - ScaleValue(1), Width, Palette.NeutralLight);
end;

procedure TERDRibbon.DrawSimplifiedBody(APainter: TERDPainter; ACanvas: TCanvas;
  ATab: TERDRibbonTab; ATop: Integer);
var
  BX, LimitX, I, J, H, RH, W: Integer;
  G: TERDRibbonGroup;
  It: TERDRibbonItem;
  Overflowing: Boolean;
begin
  H := SimplifiedBodyHeight;
  RH := ScaleValue(Metrics.Button);
  if FAppearance = raOffice then
  begin
    APainter.FillRect(Rect(0, ATop, Width, ATop + H), Palette.Background);
    APainter.RoundRect(ScaleValue(4), ATop + ScaleValue(2), Width - ScaleValue(8),
      H - ScaleValue(4), ScaleValue(8), Palette.GaugeFace, Palette.NeutralLight);
  end
  else APainter.FillRect(Rect(0, ATop, Width, ATop + H), Palette.GaugeFace);
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
    if G.Control <> nil then
    begin
      W := ScaleValue(G.ControlWidth);
      if BX + W <= LimitX then
      begin
        G.FControlRect := Rect(BX, ATop + ScaleValue(6), BX + W,
          ATop + ScaleValue(6) + RH);
        Inc(BX, W + ScaleValue(4));
      end;
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
      APainter.FillRect(FMoreRect, ERDMixColor(Palette.ForegroundText,
        Palette.GaugeFace, 0.07));
    APainter.FrameRect(FMoreRect, Palette.NeutralLight);
    APainter.Glyph(glMore, FMoreRect.Left + FMoreRect.Width div 2,
      FMoreRect.Top + FMoreRect.Height div 2, Palette.Subtle, 0.8);
  end
  else
    FMoreRect := Rect(0, 0, 0, 0);
  APainter.HLine(0, ATop + H - ScaleValue(1), Width, Palette.NeutralLight);
end;

function TERDRibbon.DrawLargeItem(APainter: TERDPainter; ACanvas: TCanvas;
  AItem: TERDRibbonItem; X, Y, H: Integer): Integer;
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
  if AItem.Enabled and (AItem = FPressedItem) and (AItem = FHoverItem) then
    Fill := APainter.Tint(Palette.Accent, 0.3)
  else if AItem.Down then
    Fill := APainter.Tint(Palette.Accent, IfThen(AItem.Enabled, 0.16, 0.06))
  else if AItem.Enabled and (AItem = FHoverItem) then
    Fill := APainter.Tint(Palette.Accent, 0.08);
  if Fill <> clNone then
  begin
    APainter.FillRect(R, Fill);
    if AItem.Down or (AItem = FPressedItem) then APainter.FrameRect(R, APainter.AccentText)
    else APainter.FrameRect(R, Palette.NeutralLight);
  end;
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

function TERDRibbon.DrawSmallItem(APainter: TERDPainter; ACanvas: TCanvas;
  AItem: TERDRibbonItem; X, Y, H: Integer): Integer;
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
  if AItem.Down or (AItem.Enabled and (AItem = FPressedItem) and (AItem = FHoverItem)) then
  begin
    if AItem.Enabled and (AItem = FPressedItem) and (AItem = FHoverItem) then
      Fill := APainter.Tint(Palette.Accent, 0.3)
    else Fill := APainter.Tint(Palette.Accent, IfThen(AItem.Enabled, 0.16, 0.06));
    APainter.FillRect(R, Fill);
    APainter.FrameRect(R, APainter.AccentText);
  end
  else if AItem.Enabled and (AItem = FHoverItem) then
  begin
    APainter.FillRect(R, APainter.Tint(Palette.Accent, 0.08));
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
    GlyphColor, 1);
  APainter.Text(X + ScaleValue(26), Y + H div 2, CaptionText, ITEM_TEXT_SIZE,
    Ink, twRegular, taLeftJustify, Result - ScaleValue(32));
  if AItem.Kind in [rikDropDown, rikSplit] then
    DrawChevron(APainter, R.Right - ScaleValue(10), Y + H div 2, True,
      Palette.Subtle);
end;

procedure TERDRibbon.DrawItemGlyph(APainter: TERDPainter; ACanvas: TCanvas;
  AItem: TERDRibbonItem; CX, CY: Integer; AColor: TColor; AScale: Single);
begin
  AItem.Icon.Draw(ACanvas, Palette, APainter.PPI, CX, CY, AColor,
    AScale, AItem.Enabled, AItem.Glyph, FImages, ItemImageIndex(AItem));
end;

procedure TERDRibbon.DrawChevron(APainter: TERDPainter; CX, CY: Integer;
  ADown: Boolean; AColor: TColor);
var
  S: Integer;
begin
  S := ScaleValue(4);
  if ADown then
    APainter.Lines([ERDPointF(CX - S, CY - S div 2), ERDPointF(CX, CY + S div 2),
      ERDPointF(CX + S, CY - S div 2)], AColor, ScaleValue(16) / 10)
  else
    APainter.Lines([ERDPointF(CX - S, CY + S div 2), ERDPointF(CX, CY - S div 2),
      ERDPointF(CX + S, CY + S div 2)], AColor, ScaleValue(16) / 10);
end;

procedure TERDRibbon.DrawLauncher(APainter: TERDPainter; CX, CY: Integer;
  AColor: TColor);
begin
  APainter.GlyphLine(glLauncher, CX, CY, AColor, 0.75);
end;

procedure TERDRibbon.Resize;
begin
  inherited Resize;
  ClosePeek;
  LayoutSearchEdit;
end;

procedure TERDRibbon.Invalidate;
begin
  inherited Invalidate;
  if (FPeek <> nil) and FPeek.Visible then
    FPeek.Invalidate;
end;

function TERDRibbon.PeekOffset: Integer;
begin
  Result := TabHeight;
end;

function TERDRibbon.PeekHeight: Integer;
begin
  Result := BodyHeight;
end;

procedure TERDRibbon.PaintPeek(ACanvas: TCanvas; AHeight: Integer);
var
  P: TERDPainter;
begin
  NormalizeActiveTab;
  ClearLayout;
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if FAppearance = raERDesigns then P.FillRect(Rect(0,0,Width,AHeight), Palette.GaugeFace)
    else P.FillRect(Rect(0,0,Width,AHeight), Palette.Background);
    DrawTabRow(P);
    DrawBody(P, ACanvas);
    LayoutHostedControls(FPeek);
  finally
    P.Free;
  end;
end;

procedure TERDRibbon.ShowPeek;
var
  Host: TWinControl;
  P: TPoint;
begin
  if csDesigning in ComponentState then
    Exit;
  Host := GetParentForm(Self);
  if Host = nil then
    Host := Parent;
  if (Host = nil) or not HandleAllocated then
    Exit;
  if FPeek = nil then
    FPeek := TERDRibbonPeek.CreatePeek(Self);
  if FPeek.Parent <> Host then
    FPeek.Parent := Host;
  P := Host.ScreenToClient(ClientToScreen(Point(0, PeekOffset)));
  FPeek.SetBounds(P.X, P.Y, Width, PeekHeight);
  FPeek.ShowHint := ShowHint;
  FTemporaryExpanded := True;
  FPeek.Visible := True;
  FPeek.BringToFront;
  Invalidate;
end;

procedure TERDRibbon.ClosePeek;
begin
  if not FTemporaryExpanded then
    Exit;
  FTemporaryExpanded := False;
  if FPeek <> nil then
    FPeek.Visible := False;
  FHoverItem := nil;
  FHoverLauncher := nil;
  Invalidate;
end;

procedure TERDRibbon.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if not FTemporaryExpanded or (FPeek = nil) or not HandleAllocated then
    Exit;
  case Msg.message of
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN, WM_NCLBUTTONDBLCLK:
      begin
        // Presses on the ribbon and its body are handled there; menus
        // opened from the body are other top-level windows and keep it.
        if (Msg.hwnd = Handle) or IsChild(Handle, Msg.hwnd) then
          Exit;
        if FPeek.HandleAllocated and ((Msg.hwnd = FPeek.Handle) or
          IsChild(FPeek.Handle, Msg.hwnd)) then
          Exit;
        if GetAncestor(Msg.hwnd, GA_ROOT) = GetAncestor(Handle, GA_ROOT) then
          ClosePeek;
      end;
  end;
end;

procedure TERDRibbon.AppDeactivate(Sender: TObject);
begin
  ClosePeek;
end;

procedure TERDRibbon.DensityChanged;
begin
  inherited DensityChanged;
  UpdateHeight;
end;

function TERDRibbon.HitTab(X, Y: Integer): TERDRibbonTab;
var
  I: Integer;
  T: TERDRibbonTab;
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

function TERDRibbon.HitItem(X, Y: Integer): TERDRibbonItem;
var
  T: TERDRibbonTab;
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

function TERDRibbon.HitLauncher(X, Y: Integer): TERDRibbonGroup;
var
  T: TERDRibbonTab;
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

function TERDRibbon.ItemHintAt(X, Y: Integer): string;
var
  It: TERDRibbonItem;
begin
  Result := '';
  It := HitItem(X, Y);
  if It <> nil then
    Result := ItemHint(It);
end;

procedure TERDRibbon.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewTab: TERDRibbonTab;
  NewItem: TERDRibbonItem;
  NewLauncher: TERDRibbonGroup;
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

procedure TERDRibbon.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  T: TERDRibbonTab;
  It: TERDRibbonItem;
  G: TERDRibbonGroup;
  P: TPoint;
  Same: Boolean;
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
      FOverflowMenu := TERDPopupMenu.Create(Self);
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
    // A tab click leaves the backstage and shows that tab.
    // On a collapsed ribbon a tab click opens the body over the form;
    // clicking the open tab again closes it, as in Office.
    Same := FTemporaryExpanded and (FlatIndexOfTab(T) = FActiveTab);
    CloseBackstage;
    ActiveTab := FlatIndexOfTab(T);
    if FCollapsed then
    begin
      if Same then
        ClosePeek
      else
        ShowPeek;
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
    FHoverItem := It;
    FFocusedItem := It;
    Invalidate;
  end
  else if FTemporaryExpanded and (Y >= PeekOffset + PeekHeight) then
    ClosePeek;
end;

procedure TERDRibbon.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  It: TERDRibbonItem;
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

procedure TERDRibbon.KeyDown(var Key: Word; Shift: TShiftState);
var
  N: Integer;
  T: TERDRibbonTab;
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
    VK_ESCAPE:
      if FTemporaryExpanded then
      begin
        ClosePeek;
        Key := 0;
      end;
  end;
end;

procedure TERDRibbon.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TERDRibbon.CMMouseLeave(var Message: TMessage);
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

procedure TERDRibbon.CMHintShow(var Message: TCMHintShow);
var
  S: string;
  P: TPoint;
  It: TERDRibbonItem;
begin
  inherited;
  P := Message.HintInfo.CursorPos;
  It := HitItem(P.X, P.Y);
  // The hint belongs to the item under the cursor only. Without a
  // CursorRect the whole ribbon counts as one hint area, so moving to the
  // next item would not show its hint until the mouse left the ribbon.
  if It <> nil then
    Message.HintInfo.CursorRect := It.FRect
  else
    Message.HintInfo.CursorRect := Rect(P.X, P.Y, P.X + 1, P.Y + 1);
  S := ItemHintAt(P.X, P.Y);
  if S <> '' then
  begin
    Message.HintInfo.HintStr := S;
    Message.Result := 0;
  end;
end;

procedure TERDRibbon.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  // A press on the ribbon or its open body sends this too (the ribbon is
  // usually the active control); only other controls close the body.
  if (Message.Sender <> Self) and ((FPeek = nil) or
    (Message.Sender <> FPeek)) then
    ClosePeek;
end;

procedure TERDRibbon.ExecuteItem(AItem: TERDRibbonItem; AFromMenu: Boolean);
begin
  if (AItem = nil) or not AItem.Enabled then
    Exit;
  AItem.Execute;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, AItem);
  ClosePeek;
  Invalidate;
end;

procedure TERDRibbon.PopupItemMenu(AItem: TERDRibbonItem);
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

procedure TERDRibbon.PopupMenuAt(AMenu: TPopupMenu; const AScreenRect: TRect);
begin
  if AMenu = nil then
    Exit;
  if AMenu is TERDPopupMenu then
  begin
    TERDPopupMenu(AMenu).Theme := Theme;
    TERDPopupMenu(AMenu).Density := Density;
    TERDPopupMenu(AMenu).PopupAt(AScreenRect);
  end
  else
    AMenu.Popup(AScreenRect.Left, AScreenRect.Bottom);
end;

procedure TERDRibbon.ClickFile;
begin
  // File toggles the backstage, like Office.
  if (FBackstage <> nil) and FBackstage.Visible then
  begin
    CloseBackstage;
    Exit;
  end;
  if FBackstage <> nil then
  begin
    FBackstage.Visible := True;
    FBackstage.Align := alClient;
    FBackstage.BringToFront;
  end;
  if Assigned(FOnFileClick) then
    FOnFileClick(Self);
end;

procedure TERDRibbon.ToggleCollapsed;
begin
  Collapsed := not Collapsed;
end;

procedure TERDRibbon.FocusSearch;
begin
  if not FShowSearch then
    Exit;
  FSearchEdit.Visible := True;
  FSearchEdit.SetFocus;
  FSearchEdit.SelectAll;
end;

procedure TERDRibbon.SearchChanged(Sender: TObject);
var
  R: TRect;
begin
  Invalidate;
  if FSearchEdit.Text = '' then
    Exit;
  if FSearchMenu = nil then
    FSearchMenu := TERDPopupMenu.Create(Self);
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

procedure TERDRibbon.SearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FSearchEdit.Text := '';
    SetFocus;
    Key := 0;
  end;
end;

procedure TERDRibbon.SearchMenuClick(Sender: TObject);
var
  It: TERDRibbonItem;
begin
  It := nil;
  if Sender is TMenuItem then
    It := TERDRibbonItem(Pointer(TMenuItem(Sender).Tag));
  ExecuteItem(It, True);
  FSearchEdit.Text := '';
end;

procedure TERDRibbon.OverflowMenuClick(Sender: TObject);
var
  It: TERDRibbonItem;
begin
  It := nil;
  if Sender is TMenuItem then
    It := TERDRibbonItem(Pointer(TMenuItem(Sender).Tag));
  ExecuteItem(It, True);
end;

procedure TERDRibbon.PopulateCommandMenu(AMenu: TPopupMenu;
  const AFilter: string; AOnlyOverflow: Boolean);
var
  I: Integer;
  Ctx: TERDContextualTabSets;
begin
  AMenu.Items.Clear;
  AddCommandsFromTabs(AMenu, ActiveTabs, AFilter, AOnlyOverflow);
  Ctx := ActiveContextualTabs;
  for I := 0 to Ctx.Count - 1 do
    if Ctx[I].Visible then
      AddCommandsFromTabs(AMenu, Ctx[I].Tabs, AFilter, AOnlyOverflow);
end;

procedure TERDRibbon.AddCommandsFromTabs(AMenu: TPopupMenu; ATabs: TERDRibbonTabs;
  const AFilter: string; AOnlyOverflow: Boolean);
var
  I, J, K: Integer;
  T: TERDRibbonTab;
  G: TERDRibbonGroup;
  It: TERDRibbonItem;
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

procedure TERDRibbon.AppShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  Form: TCustomForm;
begin
  if Handled then
    Exit;
  // The shortcut is application wide: only the ribbon the user sees, on
  // the active form, takes it. Focusing the search of a hidden ribbon or
  // one behind a modal dialog would raise "Cannot focus".
  Form := GetParentForm(Self);
  if (Form = nil) or not Form.Active or not Showing or not CanFocus then
    Exit;
  if (Msg.CharCode = Ord('Q')) and (HiWord(Msg.KeyData) and KF_ALTDOWN <> 0)
  then
  begin
    FocusSearch;
    Handled := True;
  end;
end;

end.
