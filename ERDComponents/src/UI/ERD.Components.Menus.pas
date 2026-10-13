//------------------------------------------------------------------------------
//  ERD.Components.Menus
//
//  Themed menu controls for the Application application chrome.
//
//    TERDPopupMenu   TPopupMenu replacement that displays a themed popup
//                    window for a TMenuItem tree.
//    TERDMenuBar     Themed main-menu bar for forms that keep Form.Menu empty
//                    and host the menu visually in the client or caption area.
//
//  Separator items (Caption = '-') draw a divider. When such an item has a
//  non-empty Hint it draws that Hint as an upper-case group header instead.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation



//------------------------------------------------------------------------------

unit ERD.Components.Menus;

interface

uses
  ERD.Components.Icons,
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.GDIPAPI,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.AppEvnts,
  ERD.Components.Theme,
  ERD.Components.Control,
  ERD.Components.Paint;

type
  /// <summary>Menu item with the same explicit icon source as buttons/ribbon.</summary>
  TERDMenuItem = class(TMenuItem)
  private
    FIcon: TERDIcon;
    procedure SetIcon(Value: TERDIcon);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Icon: TERDIcon read FIcon write SetIcon;
  end;

  /// <summary>Allows callers to provide semantic glyph and danger styling for
  /// menu items without coupling the menu to application commands.</summary>
  /// <param name="Sender">Menu component or menu bar asking for a style.</param>
  /// <param name="AItem">Menu item being drawn.</param>
  /// <param name="AGlyph">Glyph to draw when the item has no image.</param>
  /// <param name="ADanger">True paints text and glyphs with the danger colour.</param>
  TERDMenuGetItemStyleEvent = procedure(Sender: TObject; AItem: TMenuItem;
    var AGlyph: TERDGlyph; var ADanger: Boolean) of object;

  /// <summary>Themed popup menu that renders <see cref="Items"/> through the
  /// Application palette instead of the native menu renderer.</summary>
  TERDPopupMenu = class(TPopupMenu)
  strict private
    FTheme: TERDTheme;
    FDensity: TERDDensity;
    FOnGetItemStyle: TERDMenuGetItemStyleEvent;
    procedure SetTheme(AValue: TERDTheme);
    procedure SetDensity(AValue: TERDDensity);
    function ResolveTheme: TERDTheme;
  protected
    /// <summary>Clears the theme reference when the theme component is freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    /// <summary>Creates a themed popup menu.</summary>
    /// <param name="AOwner">Owner component.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Shows the menu at a screen point.</summary>
    /// <param name="X">Screen X coordinate.</param>
    /// <param name="Y">Screen Y coordinate.</param>
    procedure Popup(X, Y: Integer); override;
    /// <summary>Opens below a screen rectangle, flipping above or shifting
    /// left when needed to stay inside the monitor work area.</summary>
    /// <param name="AControlRect">Anchor rectangle in screen coordinates.</param>
    procedure PopupAt(const AControlRect: TRect);
  published
    /// <summary>Optional explicit theme. nil auto-finds a theme on the owner.</summary>
    property Theme: TERDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet menu density.</summary>
    property Density: TERDDensity read FDensity write SetDensity default dnDesktop;
    /// <summary>Image list used when an item ImageIndex is greater than or equal to zero.</summary>
    property Images;
    /// <summary>Provides glyphs and danger styling for individual items.</summary>
    property OnGetItemStyle: TERDMenuGetItemStyleEvent read FOnGetItemStyle
      write FOnGetItemStyle;
  end;

  /// <summary>Themed main-menu bar. Keep the form's native <c>Menu</c>
  /// property empty; assign the <see cref="Menu"/> property here so drawing,
  /// shortcuts and Alt/F10 navigation stay themed.</summary>
  TERDMenuBar = class(TERDCustomControl)
  strict private
    FMenu: TMainMenu;
    FImages: TCustomImageList;
    FEmbedded: Boolean;
    FHoverIndex: Integer;
    FOpenIndex: Integer;
    FKeyboardMode: Boolean;
    FAppEvents: TApplicationEvents;
    FOnGetItemStyle: TERDMenuGetItemStyleEvent;
    procedure SetMenu(AValue: TMainMenu);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetEmbedded(AValue: Boolean);
    function EffectiveImages: TCustomImageList;
    function PopupTheme: TERDTheme;
    function EffectiveCount: Integer;
    function EffectiveItem(AIndex: Integer): TMenuItem;
    function EffectiveCaption(AIndex: Integer): string;
    function EffectiveVisible(AIndex: Integer): Boolean;
    function EffectiveEnabled(AIndex: Integer): Boolean;
    function ItemTextSize: Single;
    function ItemRect(AIndex: Integer): TRect;
    function IndexAt(X, Y: Integer): Integer;
    function FirstVisibleIndex: Integer;
    function LastVisibleIndex: Integer;
    function NextVisibleIndex(AIndex, ADirection: Integer): Integer;
    function IndexFromAccel(AChar: Char): Integer;
    procedure OpenMenu(AIndex: Integer; AKeyboard: Boolean);
    procedure CloseMenuState(Sender: TObject);
    procedure SwitchOpenMenu(Sender: TObject; ADirection: Integer);
    procedure MenuHotTrack(const AScreenPoint: TPoint);
    procedure AppMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure AppShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure AppDeactivate(Sender: TObject);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the menu bar and its top-level items.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Updates references when Menu or Images is freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    /// <summary>Handles pointer hover and menu switching.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Opens a menu from a top-level item.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Keyboard navigation for the focused menu bar.</summary>
    /// <param name="Key">Virtual key. Set to 0 when handled.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Clears keyboard accelerator display when focus leaves.</summary>
    procedure DoExit; override;
    /// <summary>Sizes the bar to its top-level items and the density's
    /// menu-bar height. A dimension driven by Align or by left and right
    /// anchors is left alone.</summary>
    /// <param name="NewWidth">Proposed width; replaced by the items width.</param>
    /// <param name="NewHeight">Proposed height; replaced by the metric.</param>
    /// <returns>Always True.</returns>
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    /// <summary>Applies AutoSize once the streamed Menu is known.</summary>
    procedure Loaded; override;
  public
    /// <summary>Creates a themed menu bar.</summary>
    /// <param name="AOwner">Owner component.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees application event hooks.</summary>
    destructor Destroy; override;
    /// <summary>Resizes the bar to the effective menu-bar metric.</summary>
    procedure DensityChanged; override;
    /// <summary>Width, in device pixels, occupied by all visible top-level
    /// menu items.</summary>
    /// <returns>Painted item width.</returns>
    function ItemsWidth: Integer;
  published
    /// <summary>Main menu tree to draw. Leave the form's native Menu property
    /// empty and assign it here.</summary>
    property Menu: TMainMenu read FMenu write SetMenu;
    /// <summary>Image list used for popup items; nil falls back to Menu.Images.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>True draws only the items, for embedding inside a title bar.</summary>
    property Embedded: Boolean read FEmbedded write SetEmbedded default False;
    /// <summary>Sizes the bar to its top-level items and the density's
    /// menu-bar height.</summary>
    property AutoSize default True;
    /// <summary>Provides glyphs and danger styling for popup items.</summary>
    property OnGetItemStyle: TERDMenuGetItemStyleEvent read FOnGetItemStyle
      write FOnGetItemStyle;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Focusable with Tab, F10 or Alt.</summary>
    property TabStop default True;
  end;

/// <summary>Shows any TMenuItem tree as a themed popup menu.</summary>
/// <param name="AItems">Root item whose visible children are displayed.</param>
/// <param name="AAnchor">Anchor rectangle in screen coordinates.</param>
/// <param name="ATheme">Theme used for painting; nil uses the default palette.</param>
/// <param name="ADensity">Desktop or tablet menu density.</param>
/// <param name="AImages">Images for ImageIndex values.</param>
/// <param name="AOnStyle">Optional glyph and danger-style callback.</param>
/// <returns>True when a popup was opened.</returns>
function ERDShowMenuItems(AItems: TMenuItem; const AAnchor: TRect;
  ATheme: TERDTheme; ADensity: TERDDensity; AImages: TCustomImageList;
  AOnStyle: TERDMenuGetItemStyleEvent): Boolean;

/// <summary>Closes the currently open themed menu, if any.</summary>
procedure ERDCloseThemedMenu;

implementation


constructor TERDMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  FIcon := TERDIcon.Create(Self);
end;
destructor TERDMenuItem.Destroy;
begin
  FIcon.Free;
  inherited;
end;
procedure TERDMenuItem.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

const
  ERD_MENU_MIN_WIDTH = 160;
  ERD_MENU_PAD_TOP = 4;
  ERD_MENU_GUTTER = 36;
  ERD_MENU_SIDE_PAD = 10;
  ERD_MENU_TEXT_SHORT_GAP = 32;
  ERD_MENU_SUB_WIDTH = 18;
  ERD_MENU_SUB_DELAY = 250;
  WM_ERD_MENU_CLICK = WM_USER + 347;
  WM_ERD_MENU_FREE = WM_USER + 348;

type
  TERDMenuRowKind = (mrItem, mrSeparator, mrHeader);

  TERDMenuNavigateEvent = procedure(Sender: TObject; ADirection: Integer)
    of object;
  TERDMenuSessionCloseEvent = procedure(Sender: TObject) of object;
  TERDMenuHotTrackEvent = procedure(const AScreenPoint: TPoint) of object;

  TERDMenuRow = record
    Kind: TERDMenuRowKind;
    Item: TMenuItem;
    Top: Integer;
    Height: Integer;
    Caption: string;
    ShortCutText: string;
    Accel: Char;
    AccelIndex: Integer;
    Glyph: TERDGlyph;
    Danger: Boolean;
  end;

  TERDMenuSession = class;

  TERDMenuPopupWindow = class(TCustomControl)
  strict private
    FSession: TERDMenuSession;
    FRoot: TMenuItem;
    FLevel: Integer;
    FRows: array of TERDMenuRow;
    FHotIndex: Integer;
    FPendingSubIndex: Integer;
    FPalette: TERDThemePalette;
    FDensity: TERDDensity;
    FPPI: Integer;
    FImages: TCustomImageList;
    FKeyboardMode: Boolean;
    procedure BuildRows;
    procedure UpdateSize;
    function ScalePixel(AValue: Integer): Integer;
    function TextSize: Single;
    function RowAt(Y: Integer): Integer;
    function FirstSelectable: Integer;
    function LastSelectable: Integer;
    function NextSelectable(AStart, ADirection: Integer): Integer;
    function RowScreenRect(AIndex: Integer): TRect;
    function IsSelectable(AIndex: Integer): Boolean;
    procedure SetHotIndex(AIndex: Integer; AOpenSubmenu: Boolean);
    procedure StartSubmenuTimer(AIndex: Integer);
    procedure StopSubmenuTimer;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMMouseActivate(var Message: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure DrawImageOrGlyph(APainter: TERDPainter; const ARow: TERDMenuRow;
      ACX, ACY: Integer; AColor: TColor; AEnabled: Boolean);
    procedure DrawAcceleratedText(APainter: TERDPainter; X, CY: Integer;
      const AText: string; ASize: Single; AColor: TColor;
      AWeight: TERDTextWeight; AMaxWidth, AAccelIndex: Integer);
    procedure DrawChevronRight(APainter: TERDPainter; CX, CY: Integer;
      AColor: TColor);
  protected
    /// <summary>Creates a no-activate popup window with drop shadow style.</summary>
    /// <param name="Params">Window creation parameters.</param>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>Paints all rows.</summary>
    procedure Paint; override;
    /// <summary>Routes mouse movement to row hover.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Handles row activation.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
  public
    /// <summary>Creates one popup level.</summary>
    /// <param name="AOwner">Owner component.</param>
    /// <param name="ASession">Owning menu session.</param>
    /// <param name="ARoot">Root item whose children are displayed.</param>
    /// <param name="ALevel">Popup nesting level.</param>
    constructor CreatePopup(AOwner: TComponent; ASession: TERDMenuSession;
      ARoot: TMenuItem; ALevel: Integer);
    /// <summary>Shows this popup near an anchor rectangle.</summary>
    /// <param name="AOwnerControl">Control used as VCL parent.</param>
    /// <param name="AAnchor">Anchor in screen coordinates.</param>
    /// <param name="ASubmenu">True positions as a child submenu.</param>
    procedure Popup(AOwnerControl: TWinControl; const AAnchor: TRect;
      ASubmenu: Boolean);
    /// <summary>Processes a key for this popup level.</summary>
    /// <param name="Key">Virtual key. Set to 0 when consumed.</param>
    /// <param name="Shift">Shift state.</param>
    /// <returns>True when the key was handled.</returns>
    function HandleKey(var Key: Word; Shift: TShiftState): Boolean;
    /// <summary>Processes an accelerator character.</summary>
    /// <param name="AChar">Character typed by the user.</param>
    /// <returns>True when a matching item was activated.</returns>
    function HandleChar(AChar: Char): Boolean;
    /// <summary>Activates or opens the hot item.</summary>
    /// <param name="AFromKeyboard">True when invoked from keyboard.</param>
    procedure ActivateHot(AFromKeyboard: Boolean);
    /// <summary>Mouse routing from the captured root window.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure RouteMouseMove(X, Y: Integer);
    /// <summary>Mouse-up routing from the captured root window.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure RouteMouseUp(X, Y: Integer);
    /// <summary>Popup level in the chain.</summary>
    property Level: Integer read FLevel;
    /// <summary>Root item whose children are displayed.</summary>
    property Root: TMenuItem read FRoot;
  end;

  TERDMenuSession = class(TComponent)
  private
    FOwnerControl: TWinControl;
    FSender: TObject;
    FWindows: TList;
    FAppEvents: TApplicationEvents;
    FPalette: TERDThemePalette;
    FTheme: TERDTheme;
    FDensity: TERDDensity;
    FImages: TCustomImageList;
    FOnStyle: TERDMenuGetItemStyleEvent;
    FOnClose: TERDMenuSessionCloseEvent;
    FOnNavigate: TERDMenuNavigateEvent;
    FOnHotTrack: TERDMenuHotTrackEvent;
    FKeyboardMode: Boolean;
    FClosing: Boolean;
    procedure AppDeactivate(Sender: TObject);
    procedure AppMessage(var Msg: tagMSG; var Handled: Boolean);
    function WindowAtPoint(const P: TPoint): TERDMenuPopupWindow;
    procedure ReleaseWindowsFrom(ALevel: Integer);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor CreateSession(AOwnerControl: TWinControl; ASender: TObject;
      ATheme: TERDTheme; ADensity: TERDDensity; AImages: TCustomImageList;
      AOnStyle: TERDMenuGetItemStyleEvent; AOnClose: TERDMenuSessionCloseEvent;
      AOnNavigate: TERDMenuNavigateEvent; AKeyboardMode: Boolean);
    destructor Destroy; override;
    function ShowRoot(AItems: TMenuItem; const AAnchor: TRect): Boolean;
    procedure OpenSubMenu(AParent: TERDMenuPopupWindow; AItem: TMenuItem;
      const AAnchor: TRect);
    procedure CloseSubMenus(AParent: TERDMenuPopupWindow);
    procedure ExecuteItem(AItem: TMenuItem);
    procedure CloseAll;
    procedure RequestNavigate(ADirection: Integer);
    procedure StyleFor(AItem: TMenuItem; out AGlyph: TERDGlyph;
      out ADanger: Boolean);
    procedure PrepareSubmenu(AItem: TMenuItem);
    function LastWindow: TERDMenuPopupWindow;
    property Palette: TERDThemePalette read FPalette;
    property Density: TERDDensity read FDensity;
    property Images: TCustomImageList read FImages;
    property KeyboardMode: Boolean read FKeyboardMode;
    /// <summary>Called for mouse moves outside the popups, so a menu bar
    /// can switch menus while one is open.</summary>
    property OnHotTrack: TERDMenuHotTrackEvent read FOnHotTrack
      write FOnHotTrack;
  end;

  TERDMenuClickQueue = class
  strict private
    FHandle: HWND;
    FItems: TList;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Post(AItem: TMenuItem);
    procedure PostFree;
  end;

var
  GMenuSession: TERDMenuSession = nil;
  GDeadSessions: TList = nil;
  GClickQueue: TERDMenuClickQueue = nil;

function CleanCaption(const ACaption: string; out AAccel: Char;
  out AAccelIndex: Integer): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  AAccel := #0;
  AAccelIndex := 0;
  I := 1;
  while I <= Length(ACaption) do
  begin
    C := ACaption[I];
    if C = '&' then
    begin
      Inc(I);
      if I <= Length(ACaption) then
      begin
        C := ACaption[I];
        if (C <> '&') and (AAccel = #0) then
        begin
          AAccel := UpCase(C);
          AAccelIndex := Length(Result) + 1;
        end;
        Result := Result + C;
      end;
    end
    else
      Result := Result + C;
    Inc(I);
  end;
end;

function CleanCaptionOnly(const ACaption: string): string;
var
  Accel: Char;
  AccelIndex: Integer;
begin
  Result := CleanCaption(ACaption, Accel, AccelIndex);
end;

function UpperChar(AChar: Char): Char;
begin
  Result := UpCase(AChar);
end;

function ResolvePaletteFromTheme(ATheme: TERDTheme): TERDThemePalette;
begin
  if ATheme <> nil then
    Result := ATheme.Palette
  else if TERDTheme.GetDefault <> nil then
    Result := TERDTheme.GetDefault.Palette
  else Result := BRAND_PALETTE_LIGHT;
end;

function ControlPPI(AControl: TControl): Integer;
begin
{$IF CompilerVersion >= 35}
  if AControl <> nil then
    Result := AControl.CurrentPPI
  else
    Result := Screen.PixelsPerInch;
{$ELSE}
  Result := Screen.PixelsPerInch;
{$ENDIF}
  if Result <= 0 then
    Result := 96;
end;

function FindOwnerWindow(AComponent: TComponent): TWinControl;
var
  Control: TControl;
begin
  Result := nil;
  if AComponent is TControl then
  begin
    Control := TControl(AComponent);
    if Control is TWinControl then
      Result := TWinControl(Control)
    else
      Result := Control.Parent;
  end;
  if Result = nil then
    Result := Screen.ActiveCustomForm;
  if Result = nil then
    Result := Application.MainForm;
end;

procedure PostThemedMenuClick(AItem: TMenuItem);
begin
  if AItem = nil then
    Exit;
  if GClickQueue = nil then
    GClickQueue := TERDMenuClickQueue.Create;
  GClickQueue.Post(AItem);
end;

procedure DeferFreeSession(ASession: TERDMenuSession);
begin
  if ASession = nil then
    Exit;
  if GDeadSessions = nil then
    GDeadSessions := TList.Create;
  if GDeadSessions.IndexOf(ASession) < 0 then
    GDeadSessions.Add(ASession);
end;

procedure FreeDeadSessions;
var
  I: Integer;
begin
  if GDeadSessions = nil then
    Exit;
  for I := GDeadSessions.Count - 1 downto 0 do
    TObject(GDeadSessions[I]).Free;
  GDeadSessions.Clear;
end;

procedure ERDCloseThemedMenu;
begin
  if GMenuSession <> nil then
    GMenuSession.CloseAll;
end;

function ERDShowMenuItemsEx(AOwnerControl: TWinControl; ASender: TObject;
  AItems: TMenuItem; const AAnchor: TRect; ATheme: TERDTheme;
  ADensity: TERDDensity; AImages: TCustomImageList;
  AOnStyle: TERDMenuGetItemStyleEvent; AOnClose: TERDMenuSessionCloseEvent;
  AOnNavigate: TERDMenuNavigateEvent; AKeyboardMode: Boolean): Boolean;
var
  Session: TERDMenuSession;
begin
  ERDCloseThemedMenu;
  Result := False;
  if AItems = nil then
    Exit;
  if AOwnerControl = nil then
    AOwnerControl := FindOwnerWindow(nil);
  if AOwnerControl = nil then
    Exit;

  Session := TERDMenuSession.CreateSession(AOwnerControl, ASender, ATheme,
    ADensity, AImages, AOnStyle, AOnClose, AOnNavigate, AKeyboardMode);
  GMenuSession := Session;
  Result := Session.ShowRoot(AItems, AAnchor);
  if not Result and (GMenuSession = Session) then
  begin
    GMenuSession := nil;
    Session.Free;
  end;
end;

function ERDShowMenuItems(AItems: TMenuItem; const AAnchor: TRect;
  ATheme: TERDTheme; ADensity: TERDDensity; AImages: TCustomImageList;
  AOnStyle: TERDMenuGetItemStyleEvent): Boolean;
begin
  Result := ERDShowMenuItemsEx(FindOwnerWindow(nil), nil, AItems, AAnchor,
    ATheme, ADensity, AImages, AOnStyle, nil, nil, False);
end;

{ TERDMenuClickQueue --------------------------------------------------------- }

constructor TERDMenuClickQueue.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FHandle := AllocateHWnd(WndProc);
end;

destructor TERDMenuClickQueue.Destroy;
begin
  if FHandle <> 0 then
    DeallocateHWnd(FHandle);
  FItems.Free;
  inherited;
end;

procedure TERDMenuClickQueue.Post(AItem: TMenuItem);
begin
  FItems.Add(AItem);
  PostMessage(FHandle, WM_ERD_MENU_CLICK, 0, 0);
end;

procedure TERDMenuClickQueue.PostFree;
begin
  PostMessage(FHandle, WM_ERD_MENU_FREE, 0, 0);
end;

procedure TERDMenuClickQueue.WndProc(var Message: TMessage);
var
  Item: TMenuItem;
begin
  if Message.Msg = WM_ERD_MENU_CLICK then
  begin
    if FItems.Count > 0 then
    begin
      Item := TMenuItem(FItems[0]);
      FItems.Delete(0);
      if Item <> nil then
        Item.Click;
    end;
    Message.Result := 0;
  end
  else if Message.Msg = WM_ERD_MENU_FREE then
  begin
    FreeDeadSessions;
    Message.Result := 0;
  end
  else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.WParam,
      Message.LParam);
end;

{ TERDMenuSession ------------------------------------------------------------ }

constructor TERDMenuSession.CreateSession(AOwnerControl: TWinControl;
  ASender: TObject; ATheme: TERDTheme; ADensity: TERDDensity;
  AImages: TCustomImageList; AOnStyle: TERDMenuGetItemStyleEvent;
  AOnClose: TERDMenuSessionCloseEvent; AOnNavigate: TERDMenuNavigateEvent;
  AKeyboardMode: Boolean);
begin
  inherited Create(nil);
  FOwnerControl := AOwnerControl;
  FSender := ASender;
  FTheme := ATheme;
  FDensity := ADensity;
  FImages := AImages;
  FOnStyle := AOnStyle;
  FOnClose := AOnClose;
  FOnNavigate := AOnNavigate;
  FKeyboardMode := AKeyboardMode;
  FPalette := ResolvePaletteFromTheme(FTheme);
  FWindows := TList.Create;
  FAppEvents := TApplicationEvents.Create(Self);
  FAppEvents.OnDeactivate := AppDeactivate;
  FAppEvents.OnMessage := AppMessage;
end;

destructor TERDMenuSession.Destroy;
begin
  ReleaseCapture;
  ReleaseWindowsFrom(0);
  FWindows.Free;
  inherited;
end;

procedure TERDMenuSession.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  // A popup is parented to the owner control; when that control (or its
  // form) is destroyed first, it frees the popup too.
  if (Operation = opRemove) and (FWindows <> nil) then
    FWindows.Remove(AComponent);
end;

procedure TERDMenuSession.AppDeactivate(Sender: TObject);
begin
  CloseAll;
end;

function TERDMenuSession.WindowAtPoint(const P: TPoint): TERDMenuPopupWindow;
var
  I: Integer;
  W: TERDMenuPopupWindow;
  R: TRect;
begin
  Result := nil;
  for I := FWindows.Count - 1 downto 0 do
  begin
    W := TERDMenuPopupWindow(FWindows[I]);
    if (W <> nil) and W.HandleAllocated and IsWindowVisible(W.Handle) then
    begin
      R := W.BoundsRect;
      if PtInRect(R, P) then
      begin
        Result := W;
        Exit;
      end;
    end;
  end;
end;

procedure TERDMenuSession.AppMessage(var Msg: tagMSG; var Handled: Boolean);
var
  P, C: TPoint;
  W: TERDMenuPopupWindow;
  Key: Word;
  Ch: Char;
begin
  if FClosing then
    Exit;

  case Msg.message of
    WM_MOUSEMOVE:
      begin
        // The popup holds the mouse capture, so Windows sends no
        // WM_SETCURSOR: set the arrow here. Otherwise a pointer hidden
        // while typing (e.g. in the ribbon search) stays hidden.
        Winapi.Windows.SetCursor(Screen.Cursors[crArrow]);
        GetCursorPos(P);
        W := WindowAtPoint(P);
        if W <> nil then
        begin
          C := W.ScreenToClient(P);
          W.RouteMouseMove(C.X, C.Y);
          Handled := True;
        end
        else if Assigned(FOnHotTrack) then
        begin
          Handled := True;
          // May close this session and open another one.
          FOnHotTrack(P);
        end;
      end;
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
      begin
        GetCursorPos(P);
        W := WindowAtPoint(P);
        if W = nil then
        begin
          Handled := True;
          CloseAll;
        end;
      end;
    WM_LBUTTONUP:
      begin
        GetCursorPos(P);
        W := WindowAtPoint(P);
        if W <> nil then
        begin
          C := W.ScreenToClient(P);
          W.RouteMouseUp(C.X, C.Y);
          Handled := True;
        end;
      end;
    WM_KEYDOWN, WM_SYSKEYDOWN:
      begin
        W := LastWindow;
        if W <> nil then
        begin
          Key := Word(Msg.wParam);
          if W.HandleKey(Key, KeyDataToShiftState(Msg.lParam)) then
          begin
            Handled := True;
            Msg.wParam := Key;
          end;
        end;
      end;
    WM_CHAR, WM_SYSCHAR:
      begin
        W := LastWindow;
        if W <> nil then
        begin
          Ch := Char(Msg.wParam and $FFFF);
          if W.HandleChar(Ch) then
            Handled := True;
        end;
      end;
  end;
end;

procedure TERDMenuSession.ReleaseWindowsFrom(ALevel: Integer);
var
  I: Integer;
  W: TERDMenuPopupWindow;
begin
  for I := FWindows.Count - 1 downto ALevel do
  begin
    W := TERDMenuPopupWindow(FWindows[I]);
    FWindows.Delete(I);
    W.Free;
  end;
end;

function TERDMenuSession.ShowRoot(AItems: TMenuItem;
  const AAnchor: TRect): Boolean;
var
  W: TERDMenuPopupWindow;
begin
  Result := False;
  if (AItems = nil) or (AItems.Count = 0) then
    Exit;
  PrepareSubmenu(AItems);
  W := TERDMenuPopupWindow.CreatePopup(Self, Self, AItems, 0);
  FWindows.Add(W);
  W.Popup(FOwnerControl, AAnchor, False);
  if W.HandleAllocated then
    SetCapture(W.Handle);
  Result := True;
end;

procedure TERDMenuSession.OpenSubMenu(AParent: TERDMenuPopupWindow;
  AItem: TMenuItem; const AAnchor: TRect);
var
  W: TERDMenuPopupWindow;
begin
  if (AParent = nil) or (AItem = nil) or (AItem.Count = 0) or
    (not AItem.Enabled) then
    Exit;
  PrepareSubmenu(AItem);
  ReleaseWindowsFrom(AParent.Level + 1);
  W := TERDMenuPopupWindow.CreatePopup(Self, Self, AItem, AParent.Level + 1);
  FWindows.Add(W);
  W.Popup(FOwnerControl, AAnchor, True);
  if FWindows.Count > 0 then
    SetCapture(TERDMenuPopupWindow(FWindows[0]).Handle);
end;

procedure TERDMenuSession.CloseSubMenus(AParent: TERDMenuPopupWindow);
begin
  if AParent <> nil then
    ReleaseWindowsFrom(AParent.Level + 1);
end;

procedure TERDMenuSession.ExecuteItem(AItem: TMenuItem);
begin
  if (AItem = nil) or (not AItem.Enabled) then
    Exit;
  CloseAll;
  PostThemedMenuClick(AItem);
end;

procedure TERDMenuSession.CloseAll;
var
  CloseEvent: TERDMenuSessionCloseEvent;
  I: Integer;
  W: TERDMenuPopupWindow;
begin
  if FClosing then
    Exit;
  FClosing := True;
  ReleaseCapture;
  if GMenuSession = Self then
    GMenuSession := nil;
  if FAppEvents <> nil then
  begin
    FAppEvents.OnMessage := nil;
    FAppEvents.OnDeactivate := nil;
  end;
  for I := 0 to FWindows.Count - 1 do
  begin
    W := TERDMenuPopupWindow(FWindows[I]);
    if (W <> nil) and W.HandleAllocated then
      ShowWindow(W.Handle, SW_HIDE);
  end;
  CloseEvent := FOnClose;
  if Assigned(CloseEvent) then
    CloseEvent(Self);
  DeferFreeSession(Self);
  // Free closed sessions (and their popup windows) once the current
  // message has been handled, not only at unit finalization.
  if GClickQueue = nil then
    GClickQueue := TERDMenuClickQueue.Create;
  GClickQueue.PostFree;
end;

procedure TERDMenuSession.RequestNavigate(ADirection: Integer);
var
  NavigateEvent: TERDMenuNavigateEvent;
begin
  NavigateEvent := FOnNavigate;
  if Assigned(NavigateEvent) then
  begin
    // The handler opens the neighbouring menu, which closes this session;
    // closing first would clear the owner's open index it navigates from.
    NavigateEvent(nil, ADirection);
    CloseAll;
  end;
end;

procedure TERDMenuSession.StyleFor(AItem: TMenuItem; out AGlyph: TERDGlyph;
  out ADanger: Boolean);
begin
  AGlyph := glNone;
  ADanger := False;
  if Assigned(FOnStyle) and (AItem <> nil) then
    FOnStyle(FSender, AItem, AGlyph, ADanger);
end;

procedure TERDMenuSession.PrepareSubmenu(AItem: TMenuItem);
begin
  if AItem = nil then
    Exit;
  AItem.InitiateAction;
  if Assigned(AItem.OnClick) then
    AItem.Click;
end;

function TERDMenuSession.LastWindow: TERDMenuPopupWindow;
begin
  Result := nil;
  if FWindows.Count > 0 then
    Result := TERDMenuPopupWindow(FWindows[FWindows.Count - 1]);
end;

{ TERDMenuPopupWindow -------------------------------------------------------- }

constructor TERDMenuPopupWindow.CreatePopup(AOwner: TComponent;
  ASession: TERDMenuSession; ARoot: TMenuItem; ALevel: Integer);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  // Hot tracking repaints the whole popup; paint off-screen so moving
  // over the items does not flicker.
  DoubleBuffered := True;
  TabStop := False;
  FSession := ASession;
  FRoot := ARoot;
  FLevel := ALevel;
  FHotIndex := -1;
  FPendingSubIndex := -1;
  FPalette := FSession.Palette;
  FDensity := FSession.Density;
  FImages := FSession.Images;
  FKeyboardMode := FSession.KeyboardMode;
  FPPI := 96;
  ParentColor := False;
  Color := FPalette.GaugeFace;
  BuildRows;
  UpdateSize;
end;

procedure TERDMenuPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := (Params.Style and not WS_CHILD) or WS_POPUP or
    WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_TOPMOST or
    WS_EX_NOACTIVATE;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS or
    CS_DROPSHADOW;
end;

procedure TERDMenuPopupWindow.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TERDMenuPopupWindow.WMActivate(var Message: TWMActivate);
begin
  Message.Result := 0;
end;

procedure TERDMenuPopupWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // Paint fills every pixel (double-buffered); erasing first flickers.
  Message.Result := 1;
end;

function TERDMenuPopupWindow.ScalePixel(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, FPPI, 96);
end;

function TERDMenuPopupWindow.TextSize: Single;
begin
  Result := ERD_TEXT_BODY;
end;

procedure TERDMenuPopupWindow.BuildRows;
var
  I, N: Integer;
  Item: TMenuItem;
  Row: TERDMenuRow;
begin
  SetLength(FRows, 0);
  if FRoot = nil then
    Exit;

  for I := 0 to FRoot.Count - 1 do
  begin
    Item := FRoot.Items[I];
    if (Item = nil) or (not Item.Visible) then
      Continue;

    Row.Item := Item;
    Row.Top := 0;
    Row.Height := 0;
    Row.ShortCutText := '';
    Row.Glyph := glNone;
    Row.Danger := False;
    Row.Accel := #0;
    Row.AccelIndex := 0;

    if Item.Caption = '-' then
    begin
      if Item.Hint <> '' then
      begin
        Row.Kind := mrHeader;
        Row.Caption := AnsiUpperCase(Item.Hint);
      end
      else
      begin
        Row.Kind := mrSeparator;
        Row.Caption := '';
      end;
    end
    else
    begin
      Row.Kind := mrItem;
      Row.Caption := CleanCaption(Item.Caption, Row.Accel, Row.AccelIndex);
      Row.ShortCutText := ShortCutToText(Item.ShortCut);
      FSession.StyleFor(Item, Row.Glyph, Row.Danger);
    end;

    N := Length(FRows);
    SetLength(FRows, N + 1);
    FRows[N] := Row;
  end;
end;

procedure TERDMenuPopupWindow.UpdateSize;
var
  I, TextW, ShortW, MaxText, MaxShort, W, H: Integer;
  HasSub: Boolean;
  Size: Single;
begin
  MaxText := 0;
  MaxShort := 0;
  HasSub := False;
  H := ScalePixel(ERD_MENU_PAD_TOP * 2);
  Size := TextSize;
  for I := 0 to Length(FRows) - 1 do
  begin
    FRows[I].Top := H;
    case FRows[I].Kind of
      mrSeparator:
        FRows[I].Height := ScalePixel(9);
      mrHeader:
        begin
          if FDensity = dnTablet then
            FRows[I].Height := ScalePixel(28)
          else
            FRows[I].Height := ScalePixel(24);
          TextW := ERDMeasureText(FRows[I].Caption, 10.5, twSemibold, FPPI);
          MaxText := System.Math.Max(MaxText, TextW);
        end;
    else
      FRows[I].Height := ScalePixel(DensityMetrics(FDensity).MenuItem);
      TextW := ERDMeasureText(FRows[I].Caption, Size, twRegular, FPPI);
      if FRows[I].Item.Default then
        TextW := ERDMeasureText(FRows[I].Caption, Size, twSemibold, FPPI);
      ShortW := ERDMeasureText(FRows[I].ShortCutText, ERD_TEXT_SMALL, twRegular,
        FPPI);
      MaxText := System.Math.Max(MaxText, TextW);
      MaxShort := System.Math.Max(MaxShort, ShortW);
      HasSub := HasSub or (FRows[I].Item.Count > 0);
    end;
    Inc(H, FRows[I].Height);
  end;

  W := ScalePixel(ERD_MENU_GUTTER) + MaxText +
    ScalePixel(ERD_MENU_TEXT_SHORT_GAP) + MaxShort + ScalePixel(14);
  if HasSub then
    Inc(W, ScalePixel(ERD_MENU_SUB_WIDTH));
  W := System.Math.Max(ScalePixel(ERD_MENU_MIN_WIDTH), W);
  SetBounds(Left, Top, W, H);
end;

procedure TERDMenuPopupWindow.Popup(AOwnerControl: TWinControl;
  const AAnchor: TRect; ASubmenu: Boolean);
var
  X, Y, W, H: Integer;
  Work: TRect;
  Mon: TMonitor;
begin
  if AOwnerControl = nil then
    Exit;
  FPPI := ControlPPI(AOwnerControl);
  BuildRows;
  UpdateSize;
  W := Width;
  H := Height;

  Mon := Screen.MonitorFromRect(AAnchor, mdNearest);
  if Mon <> nil then
    Work := Mon.WorkareaRect
  else
    Work := Screen.DesktopRect;

  if ASubmenu then
  begin
    X := AAnchor.Right - ScalePixel(4);
    Y := AAnchor.Top - ScalePixel(4);
    if X + W > Work.Right then
      X := AAnchor.Left - W + ScalePixel(4);
    if Y + H > Work.Bottom then
      Y := Work.Bottom - H;
    if Y < Work.Top then
      Y := Work.Top;
  end
  else
  begin
    X := AAnchor.Left;
    Y := AAnchor.Bottom;
    if (Y + H > Work.Bottom) and (AAnchor.Top - H >= Work.Top) then
      Y := AAnchor.Top - H;
    if X + W > Work.Right then
      X := Work.Right - W;
    if X < Work.Left then
      X := Work.Left;
  end;

  // Position the popup before it gets a parent: assigning the parent
  // creates and shows the (visible) window at its current Left/Top, which
  // for a new popup is the top-left corner of the desktop - a visible
  // flash when moving between menu bar items.
  SetBounds(X, Y, W, H);
  if Parent <> AOwnerControl then
    Parent := AOwnerControl;
  HandleNeeded;
  SetWindowPos(Handle, HWND_TOPMOST, X, Y, W, H, SWP_NOACTIVATE or
    SWP_SHOWWINDOW);
  Invalidate;
end;

function TERDMenuPopupWindow.RowAt(Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FRows) - 1 do
    if (Y >= FRows[I].Top) and (Y < FRows[I].Top + FRows[I].Height) then
    begin
      Result := I;
      Exit;
    end;
end;

function TERDMenuPopupWindow.IsSelectable(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(FRows)) and
    (FRows[AIndex].Kind = mrItem) and (FRows[AIndex].Item <> nil) and
    FRows[AIndex].Item.Enabled;
end;

function TERDMenuPopupWindow.FirstSelectable: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FRows) - 1 do
    if IsSelectable(I) then
    begin
      Result := I;
      Exit;
    end;
end;

function TERDMenuPopupWindow.LastSelectable: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Length(FRows) - 1 downto 0 do
    if IsSelectable(I) then
    begin
      Result := I;
      Exit;
    end;
end;

function TERDMenuPopupWindow.NextSelectable(AStart, ADirection: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := AStart + ADirection;
  while (I >= 0) and (I < Length(FRows)) do
  begin
    if IsSelectable(I) then
    begin
      Result := I;
      Exit;
    end;
    Inc(I, ADirection);
  end;
end;

function TERDMenuPopupWindow.RowScreenRect(AIndex: Integer): TRect;
begin
  if (AIndex >= 0) and (AIndex < Length(FRows)) then
    Result := ClientToScreen(Rect(0, FRows[AIndex].Top, Width,
      FRows[AIndex].Top + FRows[AIndex].Height))
  else
    Result := ClientToScreen(ClientRect);
end;

procedure TERDMenuPopupWindow.SetHotIndex(AIndex: Integer;
  AOpenSubmenu: Boolean);
begin
  if not IsSelectable(AIndex) then
    AIndex := -1;
  if FHotIndex = AIndex then
    Exit;
  FHotIndex := AIndex;
  Invalidate;
  if AIndex >= 0 then
  begin
    if (FRows[AIndex].Item.Count > 0) and AOpenSubmenu then
      StartSubmenuTimer(AIndex)
    else
    begin
      StopSubmenuTimer;
      FSession.CloseSubMenus(Self);
    end;
  end
  else
  begin
    StopSubmenuTimer;
    FSession.CloseSubMenus(Self);
  end;
end;

procedure TERDMenuPopupWindow.StartSubmenuTimer(AIndex: Integer);
begin
  FPendingSubIndex := AIndex;
  SetTimer(Handle, 1, ERD_MENU_SUB_DELAY, nil);
end;

procedure TERDMenuPopupWindow.StopSubmenuTimer;
begin
  if HandleAllocated then
    KillTimer(Handle, 1);
  FPendingSubIndex := -1;
end;

procedure TERDMenuPopupWindow.WMTimer(var Message: TWMTimer);
begin
  if Message.TimerID = 1 then
  begin
    StopSubmenuTimer;
    if IsSelectable(FHotIndex) and (FRows[FHotIndex].Item.Count > 0) then
      FSession.OpenSubMenu(Self, FRows[FHotIndex].Item,
        RowScreenRect(FHotIndex));
    Message.Result := 0;
  end
  else
    inherited;
end;

procedure TERDMenuPopupWindow.DrawChevronRight(APainter: TERDPainter;
  CX, CY: Integer; AColor: TColor);
var
  S: Integer;
begin
  S := ScalePixel(4);
  APainter.Lines([ERDPointF(CX - S / 2, CY - S), ERDPointF(CX + S / 2, CY),
    ERDPointF(CX - S / 2, CY + S)], AColor, ScalePixel(2));
end;

procedure TERDMenuPopupWindow.DrawImageOrGlyph(APainter: TERDPainter;
  const ARow: TERDMenuRow; ACX, ACY: Integer; AColor: TColor;
  AEnabled: Boolean);
var
  Bmp: TBitmap;
  X, Y: Integer;
begin
  if ARow.Item = nil then
    Exit;
  if ARow.Item is TERDMenuItem then
  begin
    TERDMenuItem(ARow.Item).Icon.Draw(Canvas, FPalette, APainter.PPI,
      ACX, ACY, AColor, 0.85, AEnabled, ARow.Glyph, FImages,
      ARow.Item.ImageIndex);
    Exit;
  end;

  if (FImages <> nil) and (ARow.Item.ImageIndex >= 0) and
    (ARow.Item.ImageIndex < FImages.Count) then
  begin
    X := ACX - FImages.Width div 2;
    Y := ACY - FImages.Height div 2;
    FImages.Draw(Canvas, X, Y, ARow.Item.ImageIndex, AEnabled);
    Exit;
  end;

  Bmp := ARow.Item.Bitmap;
  if (Bmp <> nil) and (not Bmp.Empty) then
  begin
    X := ACX - Bmp.Width div 2;
    Y := ACY - Bmp.Height div 2;
    Canvas.Draw(X, Y, Bmp);
    Exit;
  end;

  if ARow.Glyph <> glNone then
    APainter.Glyph(ARow.Glyph, ACX, ACY, AColor, 0.85);
end;

procedure TERDMenuPopupWindow.DrawAcceleratedText(APainter: TERDPainter;
  X, CY: Integer; const AText: string; ASize: Single; AColor: TColor;
  AWeight: TERDTextWeight; AMaxWidth, AAccelIndex: Integer);
var
  Prefix, Ch: string;
  UX, UW, UY: Integer;
begin
  APainter.Text(X, CY, AText, ASize, AColor, AWeight, taLeftJustify,
    AMaxWidth);
  if FKeyboardMode and (AAccelIndex > 0) and (AAccelIndex <= Length(AText)) then
  begin
    Prefix := Copy(AText, 1, AAccelIndex - 1);
    Ch := Copy(AText, AAccelIndex, 1);
    UX := X + APainter.TextWidth(Prefix, ASize, AWeight);
    UW := APainter.TextWidth(Ch, ASize, AWeight);
    UY := CY + ScalePixel(8);
    APainter.HLine(UX, UY, System.Math.Max(1, UW), AColor);
  end;
end;

procedure TERDMenuPopupWindow.Paint;
var
  P: TERDPainter;
  I, CY, TextX, ShortRight, MaxText: Integer;
  Fill, Ink, GlyphInk, ShortInk: TColor;
  Weight: TERDTextWeight;
  R: TRect;
  Enabled: Boolean;
begin
  P := TERDPainter.Create(Canvas, FPalette, FPPI);
  try
    P.FillRect(ClientRect, FPalette.GaugeFace);
    P.FrameRect(Rect(0, 0, Width, Height), FPalette.NeutralLight);
    for I := 0 to Length(FRows) - 1 do
    begin
      case FRows[I].Kind of
        mrSeparator:
          P.HLine(ScalePixel(10), FRows[I].Top + ScalePixel(4),
            Width - ScalePixel(20), FPalette.NeutralLight);
        mrHeader:
          P.Caps(ScalePixel(14), FRows[I].Top + FRows[I].Height div 2 +
            ScalePixel(1), FRows[I].Caption, taLeftJustify, clDefault,
            Width - ScalePixel(28));
      else
        CY := FRows[I].Top + FRows[I].Height div 2;
        Enabled := FRows[I].Item.Enabled;
        if (I = FHotIndex) and Enabled then
        begin
          if P.Dark then
            Fill := P.Tint(FPalette.Accent, 0.26)
          else
            Fill := P.Tint(FPalette.Accent, 0.16);
          R := Rect(ScalePixel(4), FRows[I].Top, Width - ScalePixel(4),
            FRows[I].Top + FRows[I].Height);
          P.FillRect(R, Fill);
        end;

        if not Enabled then
        begin
          Ink := ERDMixColor(FPalette.Subtle, FPalette.GaugeFace, 0.5);
          GlyphInk := Ink;
        end
        else if FRows[I].Danger then
        begin
          Ink := P.DangerInk;
          GlyphInk := Ink;
        end
        else
        begin
          Ink := FPalette.ForegroundText;
          GlyphInk := FPalette.Subtle;
        end;
        ShortInk := FPalette.GaugeLabel;
        if not Enabled then
          ShortInk := GlyphInk;

        if FRows[I].Item.Checked then
        begin
          if FRows[I].Item.RadioItem then
            P.Ellipse(ScalePixel(16), CY - ScalePixel(4), ScalePixel(8),
              ScalePixel(8), P.AccentText, clNone)
          else
            P.Glyph(glCheck, ScalePixel(20), CY, P.AccentText, 0.75);
        end
        else
          DrawImageOrGlyph(P, FRows[I], ScalePixel(20), CY, GlyphInk,
            Enabled);

        Weight := twRegular;
        if FRows[I].Item.Default then
          Weight := twSemibold;
        TextX := ScalePixel(ERD_MENU_GUTTER);
        ShortRight := Width - ScalePixel(12);
        if FRows[I].Item.Count > 0 then
        begin
          DrawChevronRight(P, Width - ScalePixel(16), CY, GlyphInk);
          Dec(ShortRight, ScalePixel(16));
        end;
        MaxText := ShortRight - ScalePixel(ERD_MENU_TEXT_SHORT_GAP) - TextX;
        DrawAcceleratedText(P, TextX, CY, FRows[I].Caption, TextSize, Ink,
          Weight, MaxText, FRows[I].AccelIndex);
        if FRows[I].ShortCutText <> '' then
          P.Text(ShortRight, CY, FRows[I].ShortCutText, ERD_TEXT_SMALL,
        ShortInk,
            twRegular, taRightJustify, ShortRight - TextX);
      end;
    end;
  finally
    P.Free;
  end;
end;

procedure TERDMenuPopupWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  RouteMouseMove(X, Y);
end;

procedure TERDMenuPopupWindow.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    RouteMouseUp(X, Y);
end;

procedure TERDMenuPopupWindow.RouteMouseMove(X, Y: Integer);
begin
  SetHotIndex(RowAt(Y), True);
end;

procedure TERDMenuPopupWindow.RouteMouseUp(X, Y: Integer);
var
  Idx: Integer;
begin
  Idx := RowAt(Y);
  if IsSelectable(Idx) then
  begin
    SetHotIndex(Idx, False);
    ActivateHot(False);
  end;
end;

function TERDMenuPopupWindow.HandleKey(var Key: Word;
  Shift: TShiftState): Boolean;
var
  Idx: Integer;
begin
  Result := True;
  case Key of
    VK_UP:
      begin
        Idx := NextSelectable(FHotIndex, -1);
        if Idx < 0 then
          Idx := LastSelectable;
        SetHotIndex(Idx, False);
      end;
    VK_DOWN:
      begin
        Idx := NextSelectable(FHotIndex, 1);
        if Idx < 0 then
          Idx := FirstSelectable;
        SetHotIndex(Idx, False);
      end;
    VK_HOME:
      SetHotIndex(FirstSelectable, False);
    VK_END:
      SetHotIndex(LastSelectable, False);
    VK_RIGHT:
      begin
        if IsSelectable(FHotIndex) and (FRows[FHotIndex].Item.Count > 0) then
          FSession.OpenSubMenu(Self, FRows[FHotIndex].Item,
            RowScreenRect(FHotIndex))
        else if FLevel = 0 then
          FSession.RequestNavigate(1);
      end;
    VK_LEFT:
      begin
        if FLevel > 0 then
        begin
          FSession.ReleaseWindowsFrom(FLevel);
          Exit(True);
        end
        else
          FSession.RequestNavigate(-1);
      end;
    VK_RETURN:
      ActivateHot(True);
    VK_ESCAPE:
      begin
        if FLevel > 0 then
          FSession.ReleaseWindowsFrom(FLevel)
        else
          FSession.CloseAll;
      end;
  else
    Result := False;
  end;
  if Result then
    Key := 0;
end;

function TERDMenuPopupWindow.HandleChar(AChar: Char): Boolean;
var
  I: Integer;
  Ch: Char;
begin
  Result := False;
  Ch := UpperChar(AChar);
  if Ch = #0 then
    Exit;
  for I := 0 to Length(FRows) - 1 do
    if IsSelectable(I) and (FRows[I].Accel <> #0) and
      (UpperChar(FRows[I].Accel) = Ch) then
    begin
      SetHotIndex(I, False);
      ActivateHot(True);
      Result := True;
      Exit;
    end;
end;

procedure TERDMenuPopupWindow.ActivateHot(AFromKeyboard: Boolean);
begin
  if not IsSelectable(FHotIndex) then
  begin
    if AFromKeyboard then
      SetHotIndex(FirstSelectable, False);
    Exit;
  end;

  if FRows[FHotIndex].Item.Count > 0 then
    FSession.OpenSubMenu(Self, FRows[FHotIndex].Item, RowScreenRect(FHotIndex))
  else
    FSession.ExecuteItem(FRows[FHotIndex].Item);
end;

{ TERDPopupMenu -------------------------------------------------------------- }

constructor TERDPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
end;

procedure TERDPopupMenu.SetTheme(AValue: TERDTheme);
begin
  if FTheme = AValue then
    Exit;
  if FTheme <> nil then
    FTheme.RemoveFreeNotification(Self);
  FTheme := AValue;
  if FTheme <> nil then
    FTheme.FreeNotification(Self);
end;

procedure TERDPopupMenu.SetDensity(AValue: TERDDensity);
begin
  FDensity := AValue;
end;

function TERDPopupMenu.ResolveTheme: TERDTheme;
begin
  Result := FTheme;
  if Result = nil then
    Result := TERDTheme.FindOnOwner(Self);
  if Result = nil then
    Result := TERDTheme.GetDefault;
end;

procedure TERDPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TERDPopupMenu.Popup(X, Y: Integer);
var
  R: TRect;
  OwnerControl: TWinControl;
begin
  if Assigned(OnPopup) then
    OnPopup(Self);
  R := Rect(X, Y, X, Y);
  OwnerControl := FindOwnerWindow(PopupComponent);
  ERDShowMenuItemsEx(OwnerControl, Self, Items, R, ResolveTheme, FDensity,
    Images, FOnGetItemStyle, nil, nil, False);
end;

procedure TERDPopupMenu.PopupAt(const AControlRect: TRect);
var
  OwnerControl: TWinControl;
begin
  if Assigned(OnPopup) then
    OnPopup(Self);
  OwnerControl := FindOwnerWindow(PopupComponent);
  ERDShowMenuItemsEx(OwnerControl, Self, Items, AControlRect, ResolveTheme,
    FDensity, Images, FOnGetItemStyle, nil, nil, False);
end;

{ TERDMenuBar --------------------------------------------------------------- }

constructor TERDMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  Width := ScaleValue(320);
  Height := ScaleValue(DensityMetrics(Density).MenuBar);
  FHoverIndex := -1;
  FOpenIndex := -1;
  FAppEvents := TApplicationEvents.Create(Self);
  FAppEvents.OnMessage := AppMessage;
  FAppEvents.OnShortCut := AppShortCut;
  FAppEvents.OnDeactivate := AppDeactivate;
  AutoSize := True;
end;

function TERDMenuBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  W: Integer;
begin
  Result := True;
  if not (Align in [alTop, alBottom, alClient]) and
    not ((akLeft in Anchors) and (akRight in Anchors)) then
  begin
    W := ItemsWidth;
    if not FEmbedded then
      Inc(W, 2 * ScaleValue(6));
    NewWidth := System.Math.Max(W, ScaleValue(40));
  end;
  if not (Align in [alLeft, alRight, alClient]) then
    NewHeight := ScaleValue(Metrics.MenuBar);
end;

procedure TERDMenuBar.Loaded;
begin
  inherited;
  if AutoSize then
    AdjustSize;
end;

destructor TERDMenuBar.Destroy;
begin
  if GMenuSession <> nil then
    ERDCloseThemedMenu;
  inherited;
end;

procedure TERDMenuBar.SetMenu(AValue: TMainMenu);
begin
  if FMenu = AValue then
    Exit;
  if FMenu <> nil then
    FMenu.RemoveFreeNotification(Self);
  FMenu := AValue;
  if FMenu <> nil then
    FMenu.FreeNotification(Self);
  FHoverIndex := -1;
  FOpenIndex := -1;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TERDMenuBar.SetImages(AValue: TCustomImageList);
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

procedure TERDMenuBar.SetEmbedded(AValue: Boolean);
begin
  if FEmbedded = AValue then
    Exit;
  FEmbedded := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

function TERDMenuBar.EffectiveImages: TCustomImageList;
begin
  Result := FImages;
  if (Result = nil) and (FMenu <> nil) then
    Result := FMenu.Images;
end;

function TERDMenuBar.PopupTheme: TERDTheme;
begin
  Result := Theme;
  if Result = nil then
    Result := TERDTheme.FindOnOwner(Self);
  if Result = nil then
    Result := TERDTheme.GetDefault;
end;

function TERDMenuBar.EffectiveCount: Integer;
begin
  if (FMenu <> nil) and (FMenu.Items <> nil) then
    Result := FMenu.Items.Count
  else if IsPreview then
    Result := 6
  else
    Result := 0;
end;

function TERDMenuBar.EffectiveItem(AIndex: Integer): TMenuItem;
begin
  Result := nil;
  if (FMenu <> nil) and (FMenu.Items <> nil) and (AIndex >= 0) and
    (AIndex < FMenu.Items.Count) then
    Result := FMenu.Items[AIndex];
end;

function TERDMenuBar.EffectiveCaption(AIndex: Integer): string;
const
  PREVIEW: array[0..5] of string = ('File', 'Edit', 'View', 'Project',
    'Tools', 'Help');
var
  Item: TMenuItem;
  Accel: Char;
  AccelIndex: Integer;
begin
  Result := '';
  Item := EffectiveItem(AIndex);
  if Item <> nil then
    Result := CleanCaption(Item.Caption, Accel, AccelIndex)
  else if IsPreview and (AIndex >= Low(PREVIEW)) and (AIndex <= High(PREVIEW)) then
    Result := PREVIEW[AIndex];
end;

function TERDMenuBar.EffectiveVisible(AIndex: Integer): Boolean;
var
  Item: TMenuItem;
begin
  Item := EffectiveItem(AIndex);
  if Item <> nil then
    Result := Item.Visible
  else
    Result := IsPreview;
end;

function TERDMenuBar.EffectiveEnabled(AIndex: Integer): Boolean;
var
  Item: TMenuItem;
begin
  Item := EffectiveItem(AIndex);
  if Item <> nil then
    Result := Item.Enabled
  else
    Result := True;
end;

function TERDMenuBar.ItemTextSize: Single;
begin
  Result := ERD_TEXT_BODY;
end;

function TERDMenuBar.ItemRect(AIndex: Integer): TRect;
var
  I, X, W, H: Integer;
  Caption: string;
begin
  Result := Rect(0, 0, 0, 0);
  X := ScaleValue(6);
  if FEmbedded then
    X := 0;
  H := Height;
  if FEmbedded then
    H := Height;
  for I := 0 to EffectiveCount - 1 do
  begin
    if not EffectiveVisible(I) then
      Continue;
    Caption := EffectiveCaption(I);
    W := ERDMeasureText(Caption, ItemTextSize, twRegular, ControlPPI(Self)) +
      ScaleValue(20);
    if I = AIndex then
    begin
      Result := Rect(X, 0, X + W, H);
      Exit;
    end;
    Inc(X, W);
  end;
end;

function TERDMenuBar.IndexAt(X, Y: Integer): Integer;
var
  I: Integer;
  R: TRect;
begin
  Result := -1;
  if (Y < 0) or (Y >= Height) then
    Exit;
  for I := 0 to EffectiveCount - 1 do
  begin
    if not EffectiveVisible(I) then
      Continue;
    R := ItemRect(I);
    if PtInRect(R, Point(X, Y)) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TERDMenuBar.FirstVisibleIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to EffectiveCount - 1 do
    if EffectiveVisible(I) then
    begin
      Result := I;
      Exit;
    end;
end;

function TERDMenuBar.LastVisibleIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := EffectiveCount - 1 downto 0 do
    if EffectiveVisible(I) then
    begin
      Result := I;
      Exit;
    end;
end;

function TERDMenuBar.NextVisibleIndex(AIndex, ADirection: Integer): Integer;
var
  I: Integer;
begin
  I := AIndex + ADirection;
  while (I >= 0) and (I < EffectiveCount) do
  begin
    if EffectiveVisible(I) then
    begin
      Result := I;
      Exit;
    end;
    Inc(I, ADirection);
  end;
  if ADirection > 0 then
    Result := FirstVisibleIndex
  else
    Result := LastVisibleIndex;
end;

function TERDMenuBar.IndexFromAccel(AChar: Char): Integer;
var
  I: Integer;
  Item: TMenuItem;
  Caption: string;
  Accel: Char;
  AccelIndex: Integer;
begin
  Result := -1;
  AChar := UpperChar(AChar);
  for I := 0 to EffectiveCount - 1 do
  begin
    if not EffectiveVisible(I) then
      Continue;
    Item := EffectiveItem(I);
    if Item <> nil then
      Caption := CleanCaption(Item.Caption, Accel, AccelIndex)
    else
    begin
      Caption := EffectiveCaption(I);
      if Caption <> '' then
        Accel := UpperChar(Caption[1])
      else
        Accel := #0;
    end;
    if (Accel <> #0) and (UpperChar(Accel) = AChar) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TERDMenuBar.OpenMenu(AIndex: Integer; AKeyboard: Boolean);
var
  Item: TMenuItem;
  R: TRect;
begin
  Item := EffectiveItem(AIndex);
  if (Item = nil) or (not Item.Visible) or (not Item.Enabled) then
    Exit;
  R := ItemRect(AIndex);
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  // Close the current menu first: its close callback clears FOpenIndex.
  ERDCloseThemedMenu;
  FOpenIndex := AIndex;
  FHoverIndex := AIndex;
  FKeyboardMode := AKeyboard;
  Invalidate;
  if ERDShowMenuItemsEx(Self, Self, Item, R, PopupTheme, Density,
    EffectiveImages, FOnGetItemStyle, CloseMenuState, SwitchOpenMenu,
    AKeyboard) then
  begin
    if GMenuSession <> nil then
      GMenuSession.OnHotTrack := MenuHotTrack;
  end
  else
  begin
    FOpenIndex := -1;
    Invalidate;
  end;
end;

procedure TERDMenuBar.MenuHotTrack(const AScreenPoint: TPoint);
var
  C: TPoint;
  Idx: Integer;
begin
  if (FOpenIndex < 0) or not HandleAllocated or not Visible then
    Exit;
  C := ScreenToClient(AScreenPoint);
  Idx := IndexAt(C.X, C.Y);
  if (Idx >= 0) and (Idx <> FOpenIndex) and EffectiveEnabled(Idx) then
    OpenMenu(Idx, FKeyboardMode);
end;

procedure TERDMenuBar.CloseMenuState(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;
  FOpenIndex := -1;
  Invalidate;
end;

procedure TERDMenuBar.SwitchOpenMenu(Sender: TObject; ADirection: Integer);
var
  Idx: Integer;
begin
  if FOpenIndex >= 0 then
    Idx := NextVisibleIndex(FOpenIndex, ADirection)
  else if ADirection >= 0 then
    Idx := FirstVisibleIndex
  else
    Idx := LastVisibleIndex;
  if Idx >= 0 then
    OpenMenu(Idx, True);
end;

procedure TERDMenuBar.AppMessage(var Msg: tagMSG; var Handled: Boolean);
var
  Form: TCustomForm;
  Idx: Integer;
  Ch: Char;
begin
  if Handled or (csDestroying in ComponentState) or (not Visible) or
    (not Enabled) then
    Exit;
  Form := GetParentForm(Self);
  if (Form = nil) or (Screen.ActiveCustomForm <> Form) then
    Exit;

  case Msg.message of
    WM_SYSKEYUP:
      if Msg.wParam = VK_MENU then
      begin
        SetFocus;
        FKeyboardMode := True;
        if FHoverIndex < 0 then
          FHoverIndex := FirstVisibleIndex;
        Invalidate;
        Handled := True;
      end;
    WM_KEYDOWN:
      if Msg.wParam = VK_F10 then
      begin
        SetFocus;
        FKeyboardMode := True;
        if FHoverIndex < 0 then
          FHoverIndex := FirstVisibleIndex;
        Invalidate;
        Handled := True;
      end;
    WM_SYSCHAR:
      begin
        Ch := Char(Msg.wParam and $FFFF);
        Idx := IndexFromAccel(Ch);
        if Idx >= 0 then
        begin
          SetFocus;
          OpenMenu(Idx, True);
          Handled := True;
        end;
      end;
  end;
end;

procedure TERDMenuBar.AppShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  Form: TCustomForm;
begin
  if Handled or (FMenu = nil) or (not Visible) or (not Enabled) then
    Exit;
  Form := GetParentForm(Self);
  if (Form = nil) or (Screen.ActiveCustomForm <> Form) then
    Exit;
  Handled := FMenu.IsShortCut(Msg);
end;

procedure TERDMenuBar.AppDeactivate(Sender: TObject);
begin
  FKeyboardMode := False;
  FHoverIndex := -1;
  FOpenIndex := -1;
  Invalidate;
end;

procedure TERDMenuBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FOpenIndex < 0 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

procedure TERDMenuBar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TERDMenuBar.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  I, X, Y, W, H, UY, UW, AccelIndex: Integer;
  Caption, Prefix, ChText: string;
  Ink, Fill: TColor;
  Weight: TERDTextWeight;
  Accel: Char;
begin
  P := TERDPainter.Create(ACanvas, Palette, ControlPPI(Self));
  try
    if not FEmbedded then
    begin
      P.FillRect(ClientRect, Palette.GaugeFace);
      P.HLine(0, Height - 1, Width, Palette.NeutralLight);
    end;

    for I := 0 to EffectiveCount - 1 do
    begin
      if not EffectiveVisible(I) then
        Continue;
      if EffectiveItem(I) <> nil then
        Caption := CleanCaption(EffectiveItem(I).Caption, Accel, AccelIndex)
      else
      begin
        Caption := EffectiveCaption(I);
        if Caption <> '' then
        begin
          Accel := UpperChar(Caption[1]);
          AccelIndex := 1;
        end
        else
        begin
          Accel := #0;
          AccelIndex := 0;
        end;
      end;
      if AccelIndex = 0 then
        AccelIndex := 1;
      W := ItemRect(I).Width;
      X := ItemRect(I).Left;
      H := Height;
      Y := 0;
      if I = FOpenIndex then
      begin
        if P.Dark then
          Fill := P.Tint(Palette.Accent, 0.26)
        else
          Fill := P.Tint(Palette.Accent, 0.16);
        P.FillRect(Rect(X, Y, X + W, Y + H), Fill);
        Ink := P.AccentText;
        P.HLine(X + ScaleValue(8), Height - ScaleValue(2),
          W - ScaleValue(16), P.AccentText);
      end
      else if I = FHoverIndex then
      begin
        Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.07);
        P.FillRect(Rect(X, Y, X + W, Y + H), Fill);
        Ink := Palette.ForegroundText;
      end
      else if EffectiveEnabled(I) then
        Ink := Palette.ForegroundText
      else
        Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);

      Weight := twRegular;
      P.Text(X + ScaleValue(10), Height div 2, Caption, ItemTextSize, Ink,
        Weight, taLeftJustify, W - ScaleValue(20));
      if FKeyboardMode and (Caption <> '') then
      begin
        if (AccelIndex < 1) or (AccelIndex > Length(Caption)) then
          AccelIndex := 1;
        Prefix := Copy(Caption, 1, AccelIndex - 1);
        ChText := Copy(Caption, AccelIndex, 1);
        UY := Height div 2 + ScaleValue(9);
        UW := P.TextWidth(ChText, ItemTextSize, Weight);
        P.HLine(X + ScaleValue(10) + P.TextWidth(Prefix, ItemTextSize,
          Weight), UY, System.Math.Max(1, UW), Ink);
      end;
    end;
  finally
    P.Free;
  end;
end;

procedure TERDMenuBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      FImages := nil;
    if AComponent = FMenu then
    begin
      FMenu := nil;
      if AutoSize and not (csDestroying in ComponentState) then
        AdjustSize;
    end;
  end;
end;

procedure TERDMenuBar.DensityChanged;
begin
  inherited;
  if AutoSize then
    AdjustSize
  else
    Height := ScaleValue(Metrics.MenuBar);
  Invalidate;
end;

procedure TERDMenuBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  inherited;
  Idx := IndexAt(X, Y);
  if FHoverIndex <> Idx then
  begin
    FHoverIndex := Idx;
    Invalidate;
    if (FOpenIndex >= 0) and (Idx >= 0) and (Idx <> FOpenIndex) then
      OpenMenu(Idx, False);
  end;
end;

procedure TERDMenuBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Idx: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  Idx := IndexAt(X, Y);
  if Idx >= 0 then
  begin
    SetFocus;
    if FOpenIndex = Idx then
      ERDCloseThemedMenu
    else
      OpenMenu(Idx, False);
  end;
end;

procedure TERDMenuBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  Idx: Integer;
begin
  inherited;
  case Key of
    VK_LEFT:
      begin
        if FHoverIndex < 0 then
          FHoverIndex := FirstVisibleIndex
        else
          FHoverIndex := NextVisibleIndex(FHoverIndex, -1);
        FKeyboardMode := True;
        Invalidate;
        Key := 0;
      end;
    VK_RIGHT:
      begin
        if FHoverIndex < 0 then
          FHoverIndex := FirstVisibleIndex
        else
          FHoverIndex := NextVisibleIndex(FHoverIndex, 1);
        FKeyboardMode := True;
        Invalidate;
        Key := 0;
      end;
    VK_DOWN, VK_RETURN:
      begin
        Idx := FHoverIndex;
        if Idx < 0 then
          Idx := FirstVisibleIndex;
        if Idx >= 0 then
          OpenMenu(Idx, True);
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        FKeyboardMode := False;
        FHoverIndex := -1;
        Invalidate;
        Key := 0;
      end;
  end;
end;

procedure TERDMenuBar.DoExit;
begin
  inherited;
  if FOpenIndex < 0 then
  begin
    FKeyboardMode := False;
    Invalidate;
  end;
end;

function TERDMenuBar.ItemsWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to EffectiveCount - 1 do
    if EffectiveVisible(I) then
      Inc(Result, ItemRect(I).Width);
end;

initialization

finalization
  // Runs before Vcl.Forms finalizes (and frees the forms), so a menu bar that
  // is destroyed later may still reach these globals: nil them.
  ERDCloseThemedMenu;
  FreeDeadSessions;
  FreeAndNil(GDeadSessions);
  FreeAndNil(GClickQueue);

end.
