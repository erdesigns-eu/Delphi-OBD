//------------------------------------------------------------------------------
//  ERD.Components.TitleBar
//
//  Themed application title bar for the Application controls.
//
//    TERDTitleBar        replaces the native form caption with a themed
//                        title bar, inline menu, ribbon quick access area,
//                        status chip, extra caption buttons and system
//                        caption buttons.
//    TERDCaptionButton   streamable extra or quick-access title button.
//    TERDCaptionButtons  owned collection of title buttons.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.TitleBar;

interface

uses
  ERD.Components.Icons,
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
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
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.Menus;

type
  TERDTitleBar = class;
  TERDCaptionButton = class;

  /// <summary>Where a classic menu is shown relative to the caption row.</summary>
  TERDMenuPlacement = (
    /// <summary>The menu is embedded in the caption row.</summary>
    mpTitleBar,
    /// <summary>The menu is shown in a second row below the caption.</summary>
    mpBelow);

  /// <summary>Which command surface the title bar coordinates.</summary>
  TERDCommandStyle = (
    /// <summary>Classic menu command surface.</summary>
    csMenu,
    /// <summary>Ribbon command surface with quick access buttons.</summary>
    csRibbon);

  /// <summary>What the app icon at the left of the caption shows.</summary>
  TERDAppIconStyle = (
    /// <summary>The <see cref="TERDTitleBar.AppInitials"/> on an accent
    /// tile.</summary>
    aiInitials,
    /// <summary>The form icon, else the application icon; the initials
    /// when neither has one.</summary>
    aiIcon);

  /// <summary>Internal system caption button hit.</summary>
  TERDSystemButton = (
    /// <summary>No system caption button.</summary>
    sbNone,
    /// <summary>Minimise button.</summary>
    sbMinimize,
    /// <summary>Maximise or restore button.</summary>
    sbMaximize,
    /// <summary>Close button.</summary>
    sbClose);

  /// <summary>Fires when an extra or quick-access title button is clicked.</summary>
  /// <param name="Sender">The title bar.</param>
  /// <param name="AButton">Clicked button.</param>
  TERDTitleButtonEvent = procedure(Sender: TObject;
    AButton: TERDCaptionButton) of object;

  /// <summary>One streamable title-bar button.</summary>
  TERDCaptionButton = class(TCollectionItem)
  strict private
    FIcon: TERDIcon;
    FGlyph: TERDGlyph;
    FImageIndex: Integer;
    FHint: string;
    FBadge: string;
    FBadgeKind: TERDStatusKind;
    FVisible: Boolean;
    FEnabled: Boolean;
    FDown: Boolean;
    FName: string;
    FTag: NativeInt;
    FOnClick: TNotifyEvent;
    procedure SetIcon(Value: TERDIcon);
    procedure SetGlyph(AValue: TERDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetHint(const AValue: string);
    procedure SetBadge(const AValue: string);
    procedure SetBadgeKind(AValue: TERDStatusKind);
    procedure SetVisible(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetDown(AValue: Boolean);
    procedure SetName(const AValue: string);
    procedure SetTag(AValue: NativeInt);
  protected
    /// <summary>Returns the button name in the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an enabled visible button.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    /// <summary>Copies all streamable fields from another button.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
    property Icon: TERDIcon read FIcon write SetIcon;
    /// <summary>Built-in line glyph used when no image list image is assigned.</summary>
    property Glyph: TERDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image list index; -1 uses <see cref="Glyph"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Hint shown when the pointer rests on the button.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Optional small counter or marker badge.</summary>
    property Badge: string read FBadge write SetBadge;
    /// <summary>Status colour used by <see cref="Badge"/>.</summary>
    property BadgeKind: TERDStatusKind read FBadgeKind write SetBadgeKind
      default skDanger;
    /// <summary>Whether the button participates in layout and hit testing.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Whether the button can be clicked.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>Shows the button with a toggled highlight.</summary>
    property Down: Boolean read FDown write SetDown default False;
    /// <summary>Application-defined button name.</summary>
    property Name: string read FName write SetName;
    /// <summary>Opaque application value stored with the button.</summary>
    property Tag: NativeInt read FTag write SetTag default 0;
    /// <summary>Fires when this button is clicked.</summary>
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  /// <summary>Owned collection of title-bar buttons.</summary>
  TERDCaptionButtons = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDCaptionButton;
    procedure SetItem(AIndex: Integer; AValue: TERDCaptionButton);
  protected
    /// <summary>Invalidates the owner when an item changes.</summary>
    /// <param name="Item">Changed item, or nil for a bulk change.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a collection owned by a title bar.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds and returns a typed title button.</summary>
    /// <returns>New button.</returns>
    function Add: TERDCaptionButton;
    /// <summary>Typed item access.</summary>
    property Items[AIndex: Integer]: TERDCaptionButton read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed application title bar that replaces the native form caption.</summary>
  TERDTitleBar = class(TERDCustomControl, IERDSurface)
  strict private
    FAutoSaveVisible: Boolean;
    FAutoSave: Boolean;
    FAutoSaveCaption: string;
    FAutoSaveWidth: Integer;
    FAutoSavePadding: Integer;
    FAutoSaveMeasuredText: string;
    FAutoSaveMeasuredPPI: Integer;
    FAutoSaveMeasuredWidth: Integer;
    FAutoSaveHot: Boolean;
    FAutoSavePressed: Boolean;
    FAutoSaveSpacePressed: Boolean;
    FOnAutoSaveChange: TNotifyEvent;
    FButtons: TERDCaptionButtons;
    FQuickAccess: TERDCaptionButtons;
    FImages: TCustomImageList;
    FMenu: TMainMenu;
    FMenuBar: TERDMenuBar;
    FMenuPlacement: TERDMenuPlacement;
    FCommandStyle: TERDCommandStyle;
    FRibbon: TControl;
    FTitle: string;
    FSubtitle: string;
    FAppInitials: string;
    FAppIconStyle: TERDAppIconStyle;
    FIconCache: HICON;
    FIconCacheSource: HICON;
    FIconCacheSize: Integer;
    FSystemMenu: TERDPopupMenu;
    FStatusCaption: string;
    FStatusKind: TERDStatusKind;
    FStatusVisible: Boolean;
    FActiveBorder: Boolean;
    FFormActive: Boolean;
    FHookedForm: TCustomForm;
    FOldFormWindowProc: TWndMethod;
    FHoverSystem: TERDSystemButton;
    FPressedSystem: TERDSystemButton;
    FHoverButton: TERDCaptionButton;
    FPressedButton: TERDCaptionButton;
    FHoverQuick: TERDCaptionButton;
    FPressedQuick: TERDCaptionButton;
    FOnButtonClick: TERDTitleButtonEvent;
    FOnCommandStyleChange: TNotifyEvent;
    procedure SetAutoSaveVisible(AValue: Boolean);
    procedure SetAutoSave(AValue: Boolean);
    procedure SetAutoSaveCaption(const AValue: string);
    procedure SetAutoSaveWidth(AValue: Integer);
    procedure SetAutoSavePadding(AValue: Integer);
    function AutoSaveTextWidth: Integer;
    function AutoSaveRect: TRect;
    procedure DrawAutoSave(APainter: TERDPainter);
    procedure SetButtons(AValue: TERDCaptionButtons);
    procedure SetQuickAccess(AValue: TERDCaptionButtons);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetMenu(AValue: TMainMenu);
    procedure SetMenuPlacement(AValue: TERDMenuPlacement);
    procedure SetCommandStyle(AValue: TERDCommandStyle);
    procedure SetRibbon(AValue: TControl);
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetAppInitials(const AValue: string);
    procedure SetAppIconStyle(AValue: TERDAppIconStyle);
    procedure BuildSystemMenu;
    procedure SystemMenuClick(Sender: TObject);
    procedure SystemMenuItemStyle(Sender: TObject; AItem: TMenuItem;
      var AGlyph: TERDGlyph; var ADanger: Boolean);
    procedure SetStatusCaption(const AValue: string);
    procedure SetStatusKind(AValue: TERDStatusKind);
    procedure SetStatusVisible(AValue: Boolean);
    procedure SetActiveBorder(AValue: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    function CaptionHitTest(const AScreenPoint: TPoint): Integer;
    function CaptionHeight: Integer;
    function MenuHeight: Integer;
    function TotalHeight: Integer;
    function SystemButtonWidth: Integer;
    function ExtraButtonWidth: Integer;
    function VisibleSystemButtonCount: Integer;
    function VisibleButtonCount(AButtons: TERDCaptionButtons): Integer;
    function PreviewButtonCount(AQuick: Boolean): Integer;
    function EffectiveTitle: string;
    function EffectiveActive: Boolean;
    function EffectiveMenuVisible: Boolean;
    function FormResizable: Boolean;
    function FormMaximized: Boolean;
    function FormBorderIcons: TBorderIcons;
    function SystemButtonsLeft: Integer;
    function ExtraButtonsLeft: Integer;
    function QuickAccessLeft: Integer;
    function MenuBarRect: TRect;
    function StatusRect: TRect;
    function AppIconRect: TRect;
    function ItemRect(AButtons: TERDCaptionButtons; AButton: TERDCaptionButton;
      AQuick: Boolean): TRect;
    function PreviewItemRect(AIndex: Integer; AQuick: Boolean): TRect;
    function SystemButtonRect(AButton: TERDSystemButton): TRect;
    function SystemButtonAt(X, Y: Integer): TERDSystemButton;
    function ButtonAt(AButtons: TERDCaptionButtons; X, Y: Integer): TERDCaptionButton;
    function QuickAt(X, Y: Integer): TERDCaptionButton;
    function PointInMenu(X, Y: Integer): Boolean;
    function PointInCommandButton(X, Y: Integer): Boolean;
    function PointInCaptionDragArea(X, Y: Integer): Boolean;
    procedure HookParentForm;
    procedure UnhookParentForm;
    procedure FormWindowProc(var Message: TMessage);
    procedure HandleNCCalcSize(var Message: TMessage);
    procedure HandleNCHitTest(var Message: TMessage);
    procedure HandleNCMouseMove(var Message: TMessage);
    procedure HandleNCLButtonDown(var Message: TMessage);
    procedure HandleNCLButtonUp(var Message: TMessage);
    procedure ShowSystemMenuAt(X, Y: Integer);
    procedure ShowKeyboardSystemMenu;
    procedure ToggleMaximizeRestore;
    procedure ExecuteSystemButton(AButton: TERDSystemButton);
    procedure ExecuteButton(AButton: TERDCaptionButton);
    procedure UpdateCommandSurface;
    procedure UpdateDwmFrame;
    procedure ApplyHeight;
    procedure DrawAppIcon(APainter: TERDPainter; ACanvas: TCanvas;
      const R: TRect);
    procedure DrawSystemButtons(APainter: TERDPainter);
    procedure DrawButtonCollection(APainter: TERDPainter; ACanvas: TCanvas;
      AButtons: TERDCaptionButtons; AQuick: Boolean);
    procedure DrawPreviewButtons(APainter: TERDPainter; ACanvas: TCanvas;
      AQuick: Boolean);
    procedure DrawOneButton(APainter: TERDPainter; ACanvas: TCanvas;
      const R: TRect; AGlyph: TERDGlyph; AImageIndex: Integer;
      const ABadge: string; ABadgeKind: TERDStatusKind; AEnabled, ADown,
      AHover: Boolean; AIcon: TERDIcon = nil);
    procedure DrawStatus(APainter: TERDPainter; const R: TRect);
    procedure DrawTitleText(APainter: TERDPainter; ALeft, ARight: Integer);
  private
    procedure UpdateChildLayout;
  protected
    /// <summary>Clears component references when they are freed.</summary>
    /// <param name="AComponent">Component being inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the title bar and caption controls.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    /// <summary>Applies the hook and menu layout after streaming.</summary>
    procedure Loaded; override;
    /// <summary>Docks the bar to the top and hooks the new parent form.
    /// </summary>
    /// <param name="AParent">New parent control, or nil.</param>
    procedure SetParent(AParent: TWinControl); override;
    /// <summary>Updates hover states for client-area buttons.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Starts a client-area button press or opens the system menu.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Completes a client-area button press.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Updates child bounds after a resize.</summary>
    procedure Resize; override;
  public
    /// <summary>Creates a top-aligned themed title bar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Restores the form window procedure and releases owned objects.</summary>
    destructor Destroy; override;
    /// <summary>Re-sizes child rows when the density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Caption fill, so the embedded menu bar blends in.</summary>
    /// <returns>Caption colour for the current activation state.</returns>
    function SurfaceColor: TColor;
    /// <summary>Re-measures and repaints the embedded menu bar. Call it
    /// after changing captions or the visibility of top-level menu items
    /// at run time (a language switch, a menu for the active page).</summary>
    procedure MenuChanged;
    /// <summary>Current hit rectangle; empty when hidden or there is no room.</summary>
    property AutoSaveBounds: TRect read AutoSaveRect;
  published
    /// <summary>Optional AutoSave toggle before the quick-access commands.</summary>
    property AutoSaveVisible: Boolean read FAutoSaveVisible write SetAutoSaveVisible default False;
    /// <summary>Application-defined AutoSave state; this control performs no file I/O.</summary>
    property AutoSave: Boolean read FAutoSave write SetAutoSave default False;
    property AutoSaveCaption: string read FAutoSaveCaption write SetAutoSaveCaption;
    /// <summary>Optional maximum width in logical pixels; zero sizes to the text.</summary>
    property AutoSaveWidth: Integer read FAutoSaveWidth write SetAutoSaveWidth default 0;
    /// <summary>Space between the caption and toggle, in 96-DPI logical pixels.</summary>
    property AutoSavePadding: Integer read FAutoSavePadding write SetAutoSavePadding default 8;
    /// <summary>Fires after the state changes, outside DFM loading.</summary>
    property OnAutoSaveChange: TNotifyEvent read FOnAutoSaveChange write FOnAutoSaveChange;
    /// <summary>Extra caption buttons between the status chip and system buttons.</summary>
    property Buttons: TERDCaptionButtons read FButtons write SetButtons;
    /// <summary>Ribbon quick-access buttons shown in <c>csRibbon</c> mode.</summary>
    property QuickAccess: TERDCaptionButtons read FQuickAccess
      write SetQuickAccess;
    /// <summary>Image list used by quick-access and extra caption buttons.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Main menu shown by the embedded menu bar.</summary>
    property Menu: TMainMenu read FMenu write SetMenu;
    /// <summary>Placement of the classic menu bar.</summary>
    property MenuPlacement: TERDMenuPlacement read FMenuPlacement
      write SetMenuPlacement default mpTitleBar;
    /// <summary>Classic menu or ribbon command surface.</summary>
    property CommandStyle: TERDCommandStyle read FCommandStyle
      write SetCommandStyle default csMenu;
    /// <summary>Ribbon control shown only when <see cref="CommandStyle"/> is <c>csRibbon</c>.</summary>
    property Ribbon: TControl read FRibbon write SetRibbon;
    /// <summary>Title text; empty uses the parent form caption.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Optional context text rendered before or beside the title.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Initials painted in the accent app icon.</summary>
    property AppInitials: string read FAppInitials write SetAppInitials;
    /// <summary>Shows the initials or the form / application icon at the
    /// left of the caption. Clicking it opens the themed system menu;
    /// double-clicking it closes the form.</summary>
    property AppIconStyle: TERDAppIconStyle read FAppIconStyle
      write SetAppIconStyle default aiInitials;
    /// <summary>Status chip text.</summary>
    property StatusCaption: string read FStatusCaption write SetStatusCaption;
    /// <summary>Status chip colour.</summary>
    property StatusKind: TERDStatusKind read FStatusKind write SetStatusKind
      default skSuccess;
    /// <summary>Shows the status chip in the caption row.</summary>
    property StatusVisible: Boolean read FStatusVisible write SetStatusVisible
      default False;
    /// <summary>Uses an accent form border while active and a neutral border while inactive.</summary>
    property ActiveBorder: Boolean read FActiveBorder write SetActiveBorder
      default True;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Whether the title bar follows the theme density.</summary>
    property ParentDensity;
    /// <summary>Top alignment is required for a custom form caption.</summary>
    property Align default alTop;
    /// <summary>The title bar itself does not accept focus.</summary>
    property TabStop default False;
    /// <summary>Fires when an extra or quick-access button is clicked.</summary>
    property OnButtonClick: TERDTitleButtonEvent read FOnButtonClick
      write FOnButtonClick;
    /// <summary>Fires after <see cref="CommandStyle"/> changes.</summary>
    property OnCommandStyleChange: TNotifyEvent read FOnCommandStyleChange
      write FOnCommandStyleChange;
  end;

implementation

const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  DWMWA_BORDER_COLOR = 34;
  DWMWA_COLOR_DEFAULT = $FFFFFFFF;
  DWMWA_COLOR_NONE = $FFFFFFFE;
  WINDOWS_11_BUILD = 22000;

type
  PERDNCCalcSizeParams = ^TERDNCCalcSizeParams;
  TERDNCCalcSizeParams = record
    rgrc: array[0..2] of TRect;
    lppos: PWindowPos;
  end;

  TERDDwmMargins = record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end;

  /// <summary>Exposes the protected form WndProc for the hook fallback.</summary>
  TERDHookedFormAccess = class(TCustomForm);

  TERDOSVersionInfo = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar;
  end;

  TERDDwmSetWindowAttribute = function(AHandle: HWND; AAttribute: DWORD;
    AValue: Pointer; ASize: DWORD): HRESULT; stdcall;
  TERDDwmExtendFrameIntoClientArea = function(AHandle: HWND;
    const AMargins: TERDDwmMargins): HRESULT; stdcall;
  TERDRtlGetVersion = function(var AInfo: TERDOSVersionInfo): LongInt; stdcall;

var
  GDwmApi: HMODULE = 0;
  GDwmSetWindowAttribute: TERDDwmSetWindowAttribute = nil;
  GDwmExtendFrameIntoClientArea: TERDDwmExtendFrameIntoClientArea = nil;
  GWindowsBuild: Integer = -1;

/// <summary>Creates an icon of ASize pixels from the best image of AIcon.
/// TIcon keeps a single (usually 16 or 32 px) handle, so drawing that
/// handle larger upscales it. This picks the closest image at or above the
/// size from the original .ico data (or the MAINICON resource for the
/// default application icon) and lets Windows scale it down.</summary>
function CreateSizedIcon(AIcon: TIcon; ASize: Integer): HICON;
type
  TIconDirEntry = packed record
    Width, Height, ColorCount, Reserved: Byte;
    Planes, BitCount: Word;
    BytesInRes, ImageOffset: Cardinal;
  end;
  PIconDirEntry = ^TIconDirEntry;
var
  Stream: TMemoryStream;
  Data: PByte;
  Count, I, W, BestW, BestBits, Best: Integer;
  Entry: PIconDirEntry;
  Better: Boolean;
begin
  Result := 0;
  if (AIcon = nil) or AIcon.Empty or (ASize <= 0) then
    Exit;
  if (AIcon = Application.Icon) and
    (AIcon.Handle = LoadIcon(MainInstance, 'MAINICON')) then
  begin
    Result := LoadImage(MainInstance, 'MAINICON', IMAGE_ICON, ASize, ASize,
      LR_DEFAULTCOLOR);
    if Result <> 0 then
      Exit;
  end;
  Stream := TMemoryStream.Create;
  try
    AIcon.SaveToStream(Stream);
    if Stream.Size < 6 then
      Exit;
    Data := Stream.Memory;
    if (PWord(Data + 2)^ <> 1) then
      Exit;
    Count := PWord(Data + 4)^;
    if (Count <= 0) or (6 + Count * SizeOf(TIconDirEntry) > Stream.Size) then
      Exit;
    Best := -1;
    BestW := 0;
    BestBits := 0;
    for I := 0 to Count - 1 do
    begin
      Entry := PIconDirEntry(Data + 6 + I * SizeOf(TIconDirEntry));
      if (Entry.BytesInRes = 0) or
        (Int64(Entry.ImageOffset) + Entry.BytesInRes > Stream.Size) then
        Continue;
      W := Entry.Width;
      if W = 0 then
        W := 256;
      // Prefer the smallest image at or above the size, then the largest
      // one below it; same size: more colours.
      if Best < 0 then
        Better := True
      else if (W >= ASize) <> (BestW >= ASize) then
        Better := W >= ASize
      else if W <> BestW then
        Better := ((W >= ASize) and (W < BestW)) or
          ((W < ASize) and (W > BestW))
      else
        Better := Entry.BitCount > BestBits;
      if Better then
      begin
        Best := I;
        BestW := W;
        BestBits := Entry.BitCount;
      end;
    end;
    if Best < 0 then
      Exit;
    Entry := PIconDirEntry(Data + 6 + Best * SizeOf(TIconDirEntry));
    Result := CreateIconFromResourceEx(Data + Entry.ImageOffset,
      Entry.BytesInRes, True, $00030000, ASize, ASize, LR_DEFAULTCOLOR);
  finally
    Stream.Free;
  end;
end;

function SameMethod(const A, B: TWndMethod): Boolean;
begin
  Result := (TMethod(A).Code = TMethod(B).Code) and
    (TMethod(A).Data = TMethod(B).Data);
end;

function PointFromMessage(const Message: TMessage): TPoint;
var
  L: DWORD;
begin
  L := DWORD(Message.LParam);
  Result.X := SmallInt(L and $FFFF);
  Result.Y := SmallInt((L shr 16) and $FFFF);
end;

procedure EnsureDwmApi;
begin
  if GDwmApi <> 0 then
    Exit;
  GDwmApi := LoadLibrary('dwmapi.dll');
  if GDwmApi <> 0 then
  begin
    @GDwmSetWindowAttribute := GetProcAddress(GDwmApi, 'DwmSetWindowAttribute');
    @GDwmExtendFrameIntoClientArea := GetProcAddress(GDwmApi,
      'DwmExtendFrameIntoClientArea');
  end;
end;

function WindowsBuild: Integer;
var
  Lib: HMODULE;
  RtlGetVersion: TERDRtlGetVersion;
  Info: TERDOSVersionInfo;
begin
  if GWindowsBuild >= 0 then
    Exit(GWindowsBuild);
  Result := 0;
  Lib := GetModuleHandle('ntdll.dll');
  if Lib <> 0 then
  begin
    @RtlGetVersion := GetProcAddress(Lib, 'RtlGetVersion');
    if Assigned(RtlGetVersion) then
    begin
      ZeroMemory(@Info, SizeOf(Info));
      Info.dwOSVersionInfoSize := SizeOf(Info);
      if RtlGetVersion(Info) = 0 then
        Result := Info.dwBuildNumber;
    end;
  end;
  GWindowsBuild := Result;
end;

function IsWindows11: Boolean;
begin
  Result := WindowsBuild >= WINDOWS_11_BUILD;
end;

function ColorRefFromColor(AColor: TColor): COLORREF;
var
  C: TColor;
begin
  C := ColorToRGB(AColor);
  Result := RGB(GetRValue(C), GetGValue(C), GetBValue(C));
end;

{ TERDCaptionButton ---------------------------------------------------------- }

destructor TERDCaptionButton.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TERDCaptionButton.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

constructor TERDCaptionButton.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIcon := TERDIcon.Create(Self);
  FImageIndex := -1;
  FBadgeKind := skDanger;
  FVisible := True;
  FEnabled := True;
end;

procedure TERDCaptionButton.Assign(Source: TPersistent);
var
  Button: TERDCaptionButton;
begin
  if Source is TERDCaptionButton then
  begin
    Button := TERDCaptionButton(Source);
    FGlyph := Button.FGlyph;
    FIcon.Assign(Button.Icon);
    FImageIndex := Button.FImageIndex;
    FHint := Button.FHint;
    FBadge := Button.FBadge;
    FBadgeKind := Button.FBadgeKind;
    FVisible := Button.FVisible;
    FEnabled := Button.FEnabled;
    FDown := Button.FDown;
    FName := Button.FName;
    FTag := Button.FTag;
    FOnClick := Button.FOnClick;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TERDCaptionButton.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then
    Result := FHint;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDCaptionButton.SetGlyph(AValue: TERDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetBadge(const AValue: string);
begin
  if FBadge = AValue then
    Exit;
  FBadge := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetBadgeKind(AValue: TERDStatusKind);
begin
  if FBadgeKind = AValue then
    Exit;
  FBadgeKind := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetDown(AValue: Boolean);
begin
  if FDown = AValue then
    Exit;
  FDown := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TERDCaptionButton.SetTag(AValue: NativeInt);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
  Changed(False);
end;

{ TERDCaptionButtons --------------------------------------------------------- }

constructor TERDCaptionButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDCaptionButton);
end;

function TERDCaptionButtons.Add: TERDCaptionButton;
begin
  Result := TERDCaptionButton(inherited Add);
end;

function TERDCaptionButtons.GetItem(AIndex: Integer): TERDCaptionButton;
begin
  Result := TERDCaptionButton(inherited Items[AIndex]);
end;

procedure TERDCaptionButtons.SetItem(AIndex: Integer; AValue: TERDCaptionButton);
begin
  inherited Items[AIndex] := AValue;
end;

procedure TERDCaptionButtons.Update(Item: TCollectionItem);
var
  Owner: TPersistent;
begin
  inherited;
  Owner := GetOwner;
  if Owner is TERDTitleBar then
  begin
    TERDTitleBar(Owner).UpdateChildLayout;
    TERDTitleBar(Owner).Invalidate;
  end;
end;

{ TERDTitleBar --------------------------------------------------------------- }

constructor TERDTitleBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alTop;
  TabStop := False;
  ShowHint := True;
  FButtons := TERDCaptionButtons.Create(Self);
  FQuickAccess := TERDCaptionButtons.Create(Self);
  FAutoSaveCaption := 'AutoSave';
  FAutoSavePadding := 8;
  FMenuPlacement := mpTitleBar;
  FCommandStyle := csMenu;
  FAppInitials := 'OS';
  FAppIconStyle := aiInitials;
  FStatusCaption := 'CONNECTED';
  FStatusKind := skSuccess;
  FActiveBorder := True;
  FFormActive := True;
  FHoverSystem := sbNone;
  FPressedSystem := sbNone;
  FMenuBar := TERDMenuBar.Create(Self);
  // The title bar places the embedded bar itself (UpdateChildLayout).
  FMenuBar.AutoSize := False;
  FMenuBar.Parent := Self;
  FMenuBar.Visible := False;
  FMenuBar.Embedded := True;
  Height := TotalHeight;
end;

destructor TERDTitleBar.Destroy;
begin
  UnhookParentForm;
  if FIconCache <> 0 then
    DestroyIcon(FIconCache);
  FreeAndNil(FButtons);
  FreeAndNil(FQuickAccess);
  FreeAndNil(FMenuBar);
  inherited Destroy;
end;

procedure TERDTitleBar.Loaded;
begin
  inherited;
  Align := alTop;
  ApplyHeight;
  HookParentForm;
  UpdateCommandSurface;
  UpdateChildLayout;
end;

procedure TERDTitleBar.Notification(AComponent: TComponent;
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
      if FMenuBar <> nil then
        FMenuBar.Menu := nil;
    end;
    if AComponent = FRibbon then
      FRibbon := nil;
    if AComponent = FHookedForm then
      // The form is still alive here and keeps receiving messages while
      // it tears down its children, so give it back its own window proc.
      UnhookParentForm;
  end;
end;

procedure TERDTitleBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Enabled and FAutoSaveVisible and not AutoSaveRect.IsEmpty and
    (Shift = []) and (Key = VK_SPACE) then
  begin
    FAutoSaveSpacePressed := True;
    Key := 0;
  end;
end;

procedure TERDTitleBar.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_SPACE) and FAutoSaveSpacePressed then
  begin
    FAutoSaveSpacePressed := False;
    if Enabled and FAutoSaveVisible and not AutoSaveRect.IsEmpty then
      AutoSave := not FAutoSave;
    Key := 0;
  end;
end;

procedure TERDTitleBar.DoExit;
begin
  FAutoSaveSpacePressed := False;
  inherited;
end;

procedure TERDTitleBar.CMDialogKey(var Message: TCMDialogKey);
begin
  if Focused and Enabled and FAutoSaveVisible and not AutoSaveRect.IsEmpty and
    (Message.CharCode = VK_RETURN) and (KeyDataToShiftState(Message.KeyData) = []) then
  begin
    if (Message.KeyData and $40000000) = 0 then AutoSave := not FAutoSave;
    Message.Result := 1;
  end
  else inherited;
end;

procedure TERDTitleBar.SetAutoSaveVisible(AValue: Boolean);
begin
  if FAutoSaveVisible = AValue then Exit;
  FAutoSaveVisible := AValue;
  if not AValue then
  begin
    FAutoSaveHot := False;
    FAutoSaveSpacePressed := False;
    if FAutoSavePressed then MouseCapture := False;
    FAutoSavePressed := False;
  end;
  UpdateChildLayout;
  Invalidate;
end;

procedure TERDTitleBar.SetAutoSave(AValue: Boolean);
begin
  if FAutoSave = AValue then Exit;
  FAutoSave := AValue;
  Invalidate;
  if not (csLoading in ComponentState) and Assigned(FOnAutoSaveChange) then
    FOnAutoSaveChange(Self);
end;

procedure TERDTitleBar.SetAutoSaveCaption(const AValue: string);
begin
  if FAutoSaveCaption = AValue then Exit;
  FAutoSaveCaption := AValue;
  UpdateChildLayout;
  Invalidate;
end;

procedure TERDTitleBar.SetAutoSaveWidth(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FAutoSaveWidth = AValue then Exit;
  FAutoSaveWidth := AValue;
  UpdateChildLayout;
  Invalidate;
end;

procedure TERDTitleBar.SetAutoSavePadding(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FAutoSavePadding = AValue then Exit;
  FAutoSavePadding := AValue;
  UpdateChildLayout;
  Invalidate;
end;

function TERDTitleBar.AutoSaveTextWidth: Integer;
var Buffer: TBitmap; Painter: TERDPainter; PPI: Integer;
begin
  PPI := ScaleValue(96);
  if (FAutoSaveMeasuredText <> FAutoSaveCaption) or (FAutoSaveMeasuredPPI <> PPI) then
  begin
    Buffer := TBitmap.Create;
    try
      Buffer.SetSize(1, 1);
      Painter := TERDPainter.Create(Buffer.Canvas, Palette, PPI);
      try
        FAutoSaveMeasuredWidth := Painter.TextWidth(FAutoSaveCaption, 11.5, twRegular);
      finally Painter.Free end;
    finally Buffer.Free end;
    FAutoSaveMeasuredText := FAutoSaveCaption;
    FAutoSaveMeasuredPPI := PPI;
  end;
  Result := FAutoSaveMeasuredWidth;
end;

function TERDTitleBar.AutoSaveRect: TRect;
var L, RightEdge, Count, DesiredWidth, Gap: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if not FAutoSaveVisible then Exit;
  L := AppIconRect.Right + ScaleValue(10);
  RightEdge := ExtraButtonsLeft - ScaleValue(80);
  if FCommandStyle = csRibbon then
  begin
    Count := VisibleButtonCount(FQuickAccess);
    if Count = 0 then Count := PreviewButtonCount(True);
    Dec(RightEdge, Count * ScaleValue(30) + ScaleValue(12));
  end;
  Gap := ScaleValue(FAutoSavePadding);
  if FAutoSaveCaption = '' then Gap := 0;
  DesiredWidth := AutoSaveTextWidth + Gap + ScaleValue(44 + 8);
  if FAutoSaveWidth > 0 then
    DesiredWidth := System.Math.Min(DesiredWidth, ScaleValue(FAutoSaveWidth));
  RightEdge := System.Math.Min(L + DesiredWidth, RightEdge);
  if RightEdge - L < ScaleValue(44 + 8) then Exit;
  Result := Rect(L, ScaleValue(6), RightEdge, CaptionHeight - ScaleValue(6));
end;

procedure TERDTitleBar.DrawAutoSave(APainter: TERDPainter);
var
  R: TRect;
  X, CY, W, H, Knob, KnobX, TextX, LabelWidth, Gap: Integer;
  Fill, Ink, LabelInk: TColor;
  StateText: string;
begin
  R := AutoSaveRect;
  if R.IsEmpty then Exit;
  CY := R.Top + R.Height div 2;
  W := ScaleValue(44);
  H := ScaleValue(20);
  Gap := ScaleValue(FAutoSavePadding);
  if FAutoSaveCaption = '' then Gap := 0;
  LabelWidth := System.Math.Min(AutoSaveTextWidth,
    System.Math.Max(0, R.Width - W - Gap - ScaleValue(8)));
  X := R.Left + ScaleValue(4) + LabelWidth + Gap;
  LabelInk := Palette.ForegroundText;
  if not Enabled then LabelInk := Palette.Subtle;
  if FAutoSaveHot and Enabled then
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(4),
      ERDMixColor(Palette.ForegroundText, SurfaceColor, 0.06), clNone);
  APainter.Text(R.Left + ScaleValue(4), CY, FAutoSaveCaption, 11.5,
    LabelInk, twRegular, taLeftJustify, LabelWidth);
  if FAutoSave and Enabled then
  begin
    Fill := Palette.Accent;
    Ink := APainter.OnAccent;
  end
  else
  begin
    Fill := Palette.NeutralLight;
    Ink := Palette.ForegroundText;
  end;
  APainter.RoundRect(X, CY - H div 2, W, H, H div 2, Fill, clNone);
  Knob := H - ScaleValue(6);
  if FAutoSave then
  begin
    KnobX := X + W - H div 2 - Knob div 2;
    TextX := X + ScaleValue(11);
    StateText := 'On';
  end
  else
  begin
    KnobX := X + H div 2 - Knob div 2;
    TextX := X + W - ScaleValue(12);
    StateText := 'Off';
  end;
  APainter.Ellipse(KnobX, CY - Knob div 2, Knob, Knob, Ink, clNone);
  APainter.Text(TextX, CY, StateText, 9, Ink, twSemibold, taCenter);
  if Focused and FocusCuesVisible then
    APainter.FocusRing(R.Left, R.Top, R.Width, R.Height, ScaleValue(4));
end;

procedure TERDTitleBar.SetButtons(AValue: TERDCaptionButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TERDTitleBar.SetQuickAccess(AValue: TERDCaptionButtons);
begin
  FQuickAccess.Assign(AValue);
end;

procedure TERDTitleBar.SetImages(AValue: TCustomImageList);
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

procedure TERDTitleBar.SetMenu(AValue: TMainMenu);
begin
  if FMenu = AValue then
    Exit;
  if FMenu <> nil then
    FMenu.RemoveFreeNotification(Self);
  FMenu := AValue;
  if FMenu <> nil then
    FMenu.FreeNotification(Self);
  if FMenuBar <> nil then
    FMenuBar.Menu := FMenu;
  UpdateChildLayout;
  Invalidate;
end;

procedure TERDTitleBar.SetMenuPlacement(AValue: TERDMenuPlacement);
begin
  if FMenuPlacement = AValue then
    Exit;
  FMenuPlacement := AValue;
  ApplyHeight;
  UpdateCommandSurface;
  UpdateChildLayout;
  Invalidate;
end;

procedure TERDTitleBar.SetCommandStyle(AValue: TERDCommandStyle);
begin
  if FCommandStyle = AValue then
    Exit;
  FCommandStyle := AValue;
  ApplyHeight;
  UpdateCommandSurface;
  UpdateChildLayout;
  Invalidate;
  if Assigned(FOnCommandStyleChange) then
    FOnCommandStyleChange(Self);
end;

procedure TERDTitleBar.SetRibbon(AValue: TControl);
begin
  if FRibbon = AValue then
    Exit;
  if FRibbon <> nil then
    FRibbon.RemoveFreeNotification(Self);
  FRibbon := AValue;
  if FRibbon <> nil then
    FRibbon.FreeNotification(Self);
  UpdateCommandSurface;
end;

procedure TERDTitleBar.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TERDTitleBar.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TERDTitleBar.SetAppInitials(const AValue: string);
begin
  if FAppInitials = AValue then
    Exit;
  FAppInitials := AValue;
  Invalidate;
end;

procedure TERDTitleBar.SetAppIconStyle(AValue: TERDAppIconStyle);
begin
  if FAppIconStyle = AValue then
    Exit;
  FAppIconStyle := AValue;
  Invalidate;
end;

procedure TERDTitleBar.SetStatusCaption(const AValue: string);
begin
  if FStatusCaption = AValue then
    Exit;
  FStatusCaption := AValue;
  UpdateChildLayout;
  Invalidate;
end;

procedure TERDTitleBar.SetStatusKind(AValue: TERDStatusKind);
begin
  if FStatusKind = AValue then
    Exit;
  FStatusKind := AValue;
  Invalidate;
end;

procedure TERDTitleBar.SetStatusVisible(AValue: Boolean);
begin
  if FStatusVisible = AValue then
    Exit;
  FStatusVisible := AValue;
  UpdateChildLayout;
  Invalidate;
end;

procedure TERDTitleBar.SetActiveBorder(AValue: Boolean);
begin
  if FActiveBorder = AValue then
    Exit;
  FActiveBorder := AValue;
  UpdateDwmFrame;
  Invalidate;
end;

procedure TERDTitleBar.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if csDestroying in ComponentState then
    Exit;
  Align := alTop;
  HookParentForm;
  UpdateChildLayout;
end;

procedure TERDTitleBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FAutoSaveHot := False;
  if FPressedButton = nil then
    FHoverButton := nil;
  if FPressedQuick = nil then
    FHoverQuick := nil;
  if FPressedSystem = sbNone then
    FHoverSystem := sbNone;
  Invalidate;
end;

procedure TERDTitleBar.CMHintShow(var Message: TCMHintShow);
var
  R: TRect;
  S: string;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint then
    Exit;
  S := '';
  R := Rect(0, 0, 0, 0);
  if (FHoverButton <> nil) and (FHoverButton.Hint <> '') then
  begin
    S := FHoverButton.Hint;
    R := ItemRect(FButtons, FHoverButton, False);
  end
  else if (FHoverQuick <> nil) and (FHoverQuick.Hint <> '') then
  begin
    S := FHoverQuick.Hint;
    R := ItemRect(FQuickAccess, FHoverQuick, True);
  end;
  if S = '' then
    Exit;
  Message.HintInfo^.HintStr := S;
  Message.HintInfo^.CursorRect := R;
end;

procedure TERDTitleBar.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) and (FHookedForm <> nil) and
    FHookedForm.HandleAllocated then
    case Message.Msg of
      WM_NCMOUSEMOVE, WM_NCMOUSELEAVE, WM_NCLBUTTONDOWN, WM_NCLBUTTONUP,
      WM_NCLBUTTONDBLCLK, WM_NCRBUTTONDOWN, WM_NCRBUTTONUP, WM_NCRBUTTONDBLCLK:
        begin
          // Non-client gestures belong to the top-level window even when
          // this caption control is nested in a panel or another container.
          Message.Result := SendMessage(FHookedForm.Handle, Message.Msg,
            Message.WParam, Message.LParam);
          Exit;
        end;
    end;
  inherited;
end;

procedure TERDTitleBar.WMNCHitTest(var Message: TWMNCHitTest);
var Hit: Integer;
begin
  inherited;
  if (csDesigning in ComponentState) or (Message.Result <> HTCLIENT) then Exit;
  Hit := CaptionHitTest(Point(Message.XPos, Message.YPos));
  if Hit <> HTCLIENT then Message.Result := Hit;
end;

function TERDTitleBar.CaptionHitTest(const AScreenPoint: TPoint): Integer;
var
  C: TPoint;
  WR: TRect;
  Edge, Corner: Integer;
begin
  Result := HTCLIENT;
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated or
    not HandleAllocated then
    Exit;
  if FormResizable and not FormMaximized then
  begin
    GetWindowRect(FHookedForm.Handle, WR);
    Edge := ScaleValue(5);
    Corner := Edge + ScaleValue(16);
    if AScreenPoint.Y < WR.Top + Edge then
    begin
      if AScreenPoint.X < WR.Left + Corner then
        Result := HTTOPLEFT
      else if AScreenPoint.X >= WR.Right - Corner then
        Result := HTTOPRIGHT
      else
        Result := HTTOP;
      Exit;
    end;
  end;
  C := ScreenToClient(AScreenPoint);
  if (C.X < 0) or (C.X >= Width) or (C.Y < 0) or (C.Y >= CaptionHeight) then
    Exit;
  if SystemButtonAt(C.X, C.Y) = sbMaximize then
    Result := HTMAXBUTTON
  else if PointInCaptionDragArea(C.X, C.Y) then
    Result := HTCAPTION;
end;

function TERDTitleBar.CaptionHeight: Integer;
begin
  Result := ScaleValue(Metrics.TitleBar);
end;

function TERDTitleBar.MenuHeight: Integer;
begin
  Result := ScaleValue(Metrics.MenuBar);
end;

function TERDTitleBar.TotalHeight: Integer;
begin
  Result := CaptionHeight;
  if (FCommandStyle = csMenu) and (FMenuPlacement = mpBelow) then
    Inc(Result, MenuHeight);
end;

function TERDTitleBar.SystemButtonWidth: Integer;
begin
  Result := ScaleValue(Metrics.CaptionButton);
end;

function TERDTitleBar.ExtraButtonWidth: Integer;
begin
  Result := System.Math.Max(ScaleValue(28), CaptionHeight - ScaleValue(4));
end;

function TERDTitleBar.VisibleSystemButtonCount: Integer;
var
  Icons: TBorderIcons;
begin
  Result := 0;
  Icons := FormBorderIcons;
  if biMinimize in Icons then
    Inc(Result);
  if biMaximize in Icons then
    Inc(Result);
  if biSystemMenu in Icons then
    Inc(Result);
end;

function TERDTitleBar.VisibleButtonCount(AButtons: TERDCaptionButtons): Integer;
var
  I: Integer;
begin
  Result := 0;
  if AButtons = nil then
    Exit;
  for I := 0 to AButtons.Count - 1 do
    if AButtons[I].Visible then
      Inc(Result);
end;

function TERDTitleBar.PreviewButtonCount(AQuick: Boolean): Integer;
begin
  Result := 0;
  if not IsPreview then
    Exit;
  if AQuick then
  begin
    if (FCommandStyle = csRibbon) and (FQuickAccess.Count = 0) then
      Result := 3;
  end
  else if FButtons.Count = 0 then
    Result := 3;
end;

function TERDTitleBar.EffectiveTitle: string;
var
  Form: TCustomForm;
begin
  Result := FTitle;
  if Result = '' then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Result := Form.Caption;
  end;
  if Result = '' then
    Result := 'Application';
end;

function TERDTitleBar.EffectiveActive: Boolean;
begin
  Result := FFormActive or (csDesigning in ComponentState);
end;

function TERDTitleBar.SurfaceColor: TColor;
begin
  if EffectiveActive then
    Result := Palette.GaugeFace
  else
    Result := Palette.Background;
end;

function TERDTitleBar.EffectiveMenuVisible: Boolean;
begin
  Result := (FCommandStyle = csMenu) and ((FMenu <> nil) or IsPreview);
end;

function TERDTitleBar.FormResizable: Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  Result := (Form <> nil) and (Form.BorderStyle in [bsSizeable, bsSizeToolWin]);
end;

function TERDTitleBar.FormMaximized: Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  Result := (Form <> nil) and Form.HandleAllocated and IsZoomed(Form.Handle);
end;

function TERDTitleBar.FormBorderIcons: TBorderIcons;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form is TForm then
    Result := TForm(Form).BorderIcons
  else
    Result := [biSystemMenu, biMinimize, biMaximize];
end;

function TERDTitleBar.SystemButtonsLeft: Integer;
begin
  Result := Width - VisibleSystemButtonCount * SystemButtonWidth;
end;

function TERDTitleBar.ExtraButtonsLeft: Integer;
var
  Count: Integer;
begin
  Count := VisibleButtonCount(FButtons);
  if Count = 0 then
    Count := PreviewButtonCount(False);
  Result := SystemButtonsLeft - ScaleValue(8) - Count * ExtraButtonWidth;
end;

function TERDTitleBar.QuickAccessLeft: Integer;
var R: TRect;
begin
  Result := AppIconRect.Right + ScaleValue(10);
  R := AutoSaveRect;
  if not R.IsEmpty then Result := R.Right + ScaleValue(8);
end;

function TERDTitleBar.MenuBarRect: TRect;
var
  L, W, MaxW: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if not EffectiveMenuVisible then
    Exit;
  if FMenuPlacement = mpBelow then
  begin
    Result := Rect(0, CaptionHeight, Width, CaptionHeight + MenuHeight);
    Exit;
  end;
  L := QuickAccessLeft;
  if (FCommandStyle = csRibbon) and (VisibleButtonCount(FQuickAccess) > 0) then
    L := L + VisibleButtonCount(FQuickAccess) * ScaleValue(30) + ScaleValue(12);
  W := FMenuBar.ItemsWidth;
  if W <= 0 then
    W := ScaleValue(320);
  MaxW := System.Math.Max(0, StatusRect.Left - L - ScaleValue(24));
  W := System.Math.Min(W, MaxW);
  Result := Rect(L, ScaleValue(6), L + W, CaptionHeight - ScaleValue(6));
end;

function TERDTitleBar.StatusRect: TRect;
var
  W, R: Integer;
  Text: string;
begin
  Result := Rect(ExtraButtonsLeft - ScaleValue(12), ScaleValue(10),
    ExtraButtonsLeft - ScaleValue(12), CaptionHeight - ScaleValue(10));
  if not FStatusVisible then
    Exit;
  Text := FStatusCaption;
  if Text = '' then
    Text := 'CONNECTED';
  W := ERDMeasureText(AnsiUpperCase(Text), 10.5, twSemibold,
    ScaleValue(96)) + ScaleValue(18);
  W := System.Math.Max(W, ScaleValue(36));
  R := ExtraButtonsLeft - ScaleValue(12);
  Result := Rect(R - W, (CaptionHeight - ScaleValue(20)) div 2, R,
    (CaptionHeight + ScaleValue(20)) div 2);
end;

function TERDTitleBar.AppIconRect: TRect;
var
  S: Integer;
begin
  S := System.Math.Max(ScaleValue(20), CaptionHeight - ScaleValue(16));
  Result := Rect(ScaleValue(12), ScaleValue(8), ScaleValue(12) + S,
    ScaleValue(8) + S);
end;

function TERDTitleBar.ItemRect(AButtons: TERDCaptionButtons;
  AButton: TERDCaptionButton; AQuick: Boolean): TRect;
var
  I, X, W, Top, H: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (AButtons = nil) or (AButton = nil) then
    Exit;
  if AQuick then
  begin
    X := QuickAccessLeft;
    W := ScaleValue(28);
    Top := ScaleValue(6);
    H := CaptionHeight - ScaleValue(12);
  end
  else
  begin
    X := ExtraButtonsLeft;
    W := ExtraButtonWidth;
    Top := 0;
    H := CaptionHeight;
  end;
  for I := 0 to AButtons.Count - 1 do
    if AButtons[I].Visible then
    begin
      if AButtons[I] = AButton then
      begin
        if AQuick then
          Result := Rect(X, Top, X + W, Top + H)
        else
          Result := Rect(X, Top, X + W, Top + H);
        Exit;
      end;
      if AQuick then
        Inc(X, ScaleValue(30))
      else
        Inc(X, W);
    end;
end;

function TERDTitleBar.PreviewItemRect(AIndex: Integer; AQuick: Boolean): TRect;
var
  X, W: Integer;
begin
  if AQuick then
  begin
    W := ScaleValue(28);
    X := QuickAccessLeft + AIndex * ScaleValue(30);
    Result := Rect(X, ScaleValue(6), X + W, CaptionHeight - ScaleValue(6));
  end
  else
  begin
    W := ExtraButtonWidth;
    X := ExtraButtonsLeft + AIndex * W;
    Result := Rect(X, 0, X + W, CaptionHeight);
  end;
end;

function TERDTitleBar.SystemButtonRect(AButton: TERDSystemButton): TRect;
var
  X, W: Integer;
  Icons: TBorderIcons;
begin
  Result := Rect(0, 0, 0, 0);
  W := SystemButtonWidth;
  X := SystemButtonsLeft;
  Icons := FormBorderIcons;
  if biMinimize in Icons then
  begin
    if AButton = sbMinimize then
      Exit(Rect(X, 0, X + W, CaptionHeight));
    Inc(X, W);
  end;
  if biMaximize in Icons then
  begin
    if AButton = sbMaximize then
      Exit(Rect(X, 0, X + W, CaptionHeight));
    Inc(X, W);
  end;
  if biSystemMenu in Icons then
  begin
    if AButton = sbClose then
      Exit(Rect(X, 0, X + W, CaptionHeight));
  end;
end;

function TERDTitleBar.SystemButtonAt(X, Y: Integer): TERDSystemButton;
var
  R: TRect;
begin
  Result := sbNone;
  if (Y < 0) or (Y >= CaptionHeight) then
    Exit;
  R := SystemButtonRect(sbMinimize);
  if not R.IsEmpty and PtInRect(R, Point(X, Y)) then
    Exit(sbMinimize);
  R := SystemButtonRect(sbMaximize);
  if not R.IsEmpty and PtInRect(R, Point(X, Y)) then
    Exit(sbMaximize);
  R := SystemButtonRect(sbClose);
  if not R.IsEmpty and PtInRect(R, Point(X, Y)) then
    Exit(sbClose);
end;

function TERDTitleBar.ButtonAt(AButtons: TERDCaptionButtons; X,
  Y: Integer): TERDCaptionButton;
var
  I: Integer;
  R: TRect;
begin
  Result := nil;
  if AButtons = nil then
    Exit;
  for I := 0 to AButtons.Count - 1 do
    if AButtons[I].Visible then
    begin
      R := ItemRect(AButtons, AButtons[I], False);
      if PtInRect(R, Point(X, Y)) then
        Exit(AButtons[I]);
    end;
end;

function TERDTitleBar.QuickAt(X, Y: Integer): TERDCaptionButton;
var
  I: Integer;
  R: TRect;
begin
  Result := nil;
  if FCommandStyle <> csRibbon then
    Exit;
  for I := 0 to FQuickAccess.Count - 1 do
    if FQuickAccess[I].Visible then
    begin
      R := ItemRect(FQuickAccess, FQuickAccess[I], True);
      if PtInRect(R, Point(X, Y)) then
        Exit(FQuickAccess[I]);
    end;
end;

function TERDTitleBar.PointInMenu(X, Y: Integer): Boolean;
var
  R: TRect;
begin
  R := MenuBarRect;
  Result := not R.IsEmpty and PtInRect(R, Point(X, Y));
end;

function TERDTitleBar.PointInCommandButton(X, Y: Integer): Boolean;
begin
  Result := (SystemButtonAt(X, Y) <> sbNone) or
    (ButtonAt(FButtons, X, Y) <> nil) or (QuickAt(X, Y) <> nil) or
    PtInRect(StatusRect, Point(X, Y)) or PtInRect(AppIconRect, Point(X, Y)) or
    PtInRect(AutoSaveRect, Point(X, Y));
end;

function TERDTitleBar.PointInCaptionDragArea(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < Width) and (Y >= 0) and (Y < CaptionHeight) and
    not PointInMenu(X, Y) and not PointInCommandButton(X, Y);
end;

procedure TERDTitleBar.ApplyHeight;
begin
  Align := alTop;
  Height := TotalHeight;
end;

procedure TERDTitleBar.UpdateChildLayout;
var
  R: TRect;
begin
  if FMenuBar = nil then
    Exit;
  FMenuBar.Menu := FMenu;
  FMenuBar.Embedded := FMenuPlacement = mpTitleBar;
  FMenuBar.Visible := EffectiveMenuVisible;
  if FMenuBar.Visible then
  begin
    R := MenuBarRect;
    FMenuBar.SetBounds(R.Left, R.Top, R.Width, R.Height);
  end;
end;

procedure TERDTitleBar.MenuChanged;
begin
  UpdateChildLayout;
  if FMenuBar <> nil then
    FMenuBar.Invalidate;
  Invalidate;
end;

procedure TERDTitleBar.UpdateCommandSurface;
begin
  if FRibbon <> nil then
    FRibbon.Visible := FCommandStyle = csRibbon;
  if FMenuBar <> nil then
    FMenuBar.Visible := EffectiveMenuVisible;
end;

procedure TERDTitleBar.DensityChanged;
begin
  inherited;
  ApplyHeight;
  UpdateChildLayout;
end;

procedure TERDTitleBar.Resize;
begin
  inherited;
  UpdateChildLayout;
end;

procedure TERDTitleBar.HookParentForm;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form = FHookedForm then
  begin
    UpdateDwmFrame;
    Exit;
  end;
  UnhookParentForm;
  FHookedForm := Form;
  if FHookedForm <> nil then
  begin
    FHookedForm.FreeNotification(Self);
    FOldFormWindowProc := FHookedForm.WindowProc;
    FHookedForm.WindowProc := FormWindowProc;
    FFormActive := (Screen.ActiveCustomForm = FHookedForm) or
      (csDesigning in ComponentState);
    UpdateDwmFrame;
  end;
end;

procedure TERDTitleBar.UnhookParentForm;
var
  Current, Mine: TWndMethod;
begin
  if FHookedForm <> nil then
  begin
    Current := FHookedForm.WindowProc;
    Mine := FormWindowProc;
    if SameMethod(Current, Mine) then
      FHookedForm.WindowProc := FOldFormWindowProc;
    FHookedForm.RemoveFreeNotification(Self);
  end;
  FHookedForm := nil;
  FOldFormWindowProc := nil;
end;

procedure TERDTitleBar.FormWindowProc(var Message: TMessage);
var
  ActiveNow: Boolean;
begin
  case Message.Msg of
    WM_NCCALCSIZE:
      begin
        HandleNCCalcSize(Message);
        Exit;
      end;
    WM_NCHITTEST:
      begin
        HandleNCHitTest(Message);
        Exit;
      end;
    WM_NCMOUSEMOVE:
      begin
        HandleNCMouseMove(Message);
        Exit;
      end;
    WM_NCLBUTTONDOWN:
      begin
        HandleNCLButtonDown(Message);
        Exit;
      end;
    WM_NCLBUTTONUP:
      begin
        HandleNCLButtonUp(Message);
        Exit;
      end;
    WM_NCMOUSELEAVE:
      if (FHoverSystem <> sbNone) or (FPressedSystem <> sbNone) then
      begin
        FHoverSystem := sbNone;
        FPressedSystem := sbNone;
        Invalidate;
      end;
    WM_NCRBUTTONUP:
      if Message.WParam = HTCAPTION then
      begin
        ShowSystemMenuAt(PointFromMessage(Message).X, PointFromMessage(Message).Y);
        Message.Result := 0;
        Exit;
      end;
    WM_NCACTIVATE:
      begin
        ActiveNow := Message.WParam <> 0;
        if FFormActive <> ActiveNow then
        begin
          FFormActive := ActiveNow;
          UpdateDwmFrame;
          Invalidate;
          if FMenuBar <> nil then
            FMenuBar.Invalidate;
        end;
      end;
    WM_ACTIVATE:
      begin
        ActiveNow := Word(Message.WParam and $FFFF) <> WA_INACTIVE;
        if FFormActive <> ActiveNow then
        begin
          FFormActive := ActiveNow;
          UpdateDwmFrame;
          Invalidate;
          if FMenuBar <> nil then
            FMenuBar.Invalidate;
        end;
      end;
    WM_SYSKEYDOWN:
      if Message.WParam = VK_SPACE then
      begin
        ShowKeyboardSystemMenu;
        Message.Result := 0;
        Exit;
      end;
    WM_SYSCOMMAND:
      if ((Message.WParam and $FFF0) = SC_KEYMENU) and
        ((Message.LParam = VK_SPACE) or (Message.LParam = Ord(' '))) then
      begin
        ShowKeyboardSystemMenu;
        Message.Result := 0;
        Exit;
      end;
    WM_CREATE, WM_STYLECHANGED, WM_SETTINGCHANGE:
      UpdateDwmFrame;
  end;
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message)
  else if FHookedForm <> nil then
    // Never fall back to the title bar's own WndProc: CM_INVALIDATE would
    // bounce to Parent.Perform (the form) and recurse forever.
    TERDHookedFormAccess(FHookedForm).WndProc(Message);
end;

procedure TERDTitleBar.HandleNCCalcSize(var Message: TMessage);
var
  Params: PERDNCCalcSizeParams;
  R: PRect;
  Mon: TMonitor;
  Work: TRect;
  WindowTop: Integer;
begin
  // The proposed window top, before the default handler moves it down
  // by the system caption and frame.
  WindowTop := 0;
  if Message.LParam <> 0 then
  begin
    if Message.WParam <> 0 then
      WindowTop := PERDNCCalcSizeParams(Message.LParam)^.rgrc[0].Top
    else
      WindowTop := PRect(Message.LParam)^.Top;
  end;
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message);
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  if Message.WParam <> 0 then
  begin
    Params := PERDNCCalcSizeParams(Message.LParam);
    if Params = nil then
      Exit;
    if IsZoomed(FHookedForm.Handle) then
    begin
      Mon := Screen.MonitorFromWindow(FHookedForm.Handle, mdNearest);
      if Mon <> nil then
        Work := Mon.WorkareaRect
      else
        Work := Screen.DesktopRect;
      Params^.rgrc[0] := Work;
    end
    else
      // Remove the whole system caption and top frame: the client area
      // starts at the window top and the title bar paints the caption.
      Params^.rgrc[0].Top := WindowTop;
  end
  else
  begin
    R := PRect(Message.LParam);
    if R <> nil then
      R^.Top := WindowTop;
  end;
  Message.Result := 0;
end;

procedure TERDTitleBar.HandleNCHitTest(var Message: TMessage);
var
  P, C: TPoint;
  Hit: Integer;
  OldResult: LRESULT;
begin
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message);
  OldResult := Message.Result;
  if (OldResult <> HTCLIENT) and (OldResult <> HTCAPTION) and
    (OldResult <> HTNOWHERE) then
    Exit;
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  P := PointFromMessage(Message);
  Hit := CaptionHitTest(P);
  if Hit <> HTCLIENT then
    Message.Result := Hit
  else
  begin
    C := ScreenToClient(P);
    if (C.X >= 0) and (C.X < Width) and (C.Y >= 0) and (C.Y < Height) then
      Message.Result := HTCLIENT;
  end;
end;

procedure TERDTitleBar.HandleNCMouseMove(var Message: TMessage);
var
  C: TPoint;
  NewHover: TERDSystemButton;
  Track: TTrackMouseEvent;
begin
  NewHover := sbNone;
  if FHookedForm <> nil then
  begin
    C := ScreenToClient(PointFromMessage(Message));
    if (C.X >= 0) and (C.X < Width) and (C.Y >= 0) and (C.Y < CaptionHeight) then
      NewHover := SystemButtonAt(C.X, C.Y);
  end;
  if FHoverSystem <> NewHover then
  begin
    FHoverSystem := NewHover;
    Invalidate;
  end;
  if (NewHover <> sbNone) and (FHookedForm <> nil) and
    FHookedForm.HandleAllocated then
  begin
    // Ask for WM_NCMOUSELEAVE so the maximize hover clears on exit.
    Track.cbSize := SizeOf(Track);
    Track.dwFlags := TME_LEAVE or TME_NONCLIENT;
    Track.hwndTrack := FHookedForm.Handle;
    Track.dwHoverTime := 0;
    TrackMouseEvent(Track);
  end;
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message);
end;

procedure TERDTitleBar.HandleNCLButtonDown(var Message: TMessage);
begin
  if Message.WParam = HTMAXBUTTON then
  begin
    FPressedSystem := sbMaximize;
    FHoverSystem := sbMaximize;
    Invalidate;
    Message.Result := 0;
    Exit;
  end;
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message);
end;

procedure TERDTitleBar.HandleNCLButtonUp(var Message: TMessage);
var
  C: TPoint;
  Hit: TERDSystemButton;
begin
  if FPressedSystem = sbMaximize then
  begin
    Hit := sbNone;
    C := ScreenToClient(PointFromMessage(Message));
    if (C.X >= 0) and (C.X < Width) and (C.Y >= 0) and (C.Y < CaptionHeight) then
      Hit := SystemButtonAt(C.X, C.Y);
    FPressedSystem := sbNone;
    if Hit = sbMaximize then
      ToggleMaximizeRestore;
    Invalidate;
    Message.Result := 0;
    Exit;
  end;
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message);
end;

procedure TERDTitleBar.BuildSystemMenu;

  procedure AddItem(const ACaption: string; ACommand: Integer;
    AEnabled: Boolean; AShortCut: TShortCut = 0);
  var
    Item: TMenuItem;
  begin
    Item := TMenuItem.Create(FSystemMenu);
    Item.Caption := ACaption;
    Item.Tag := ACommand;
    Item.Enabled := AEnabled;
    Item.ShortCut := AShortCut;
    Item.OnClick := SystemMenuClick;
    FSystemMenu.Items.Add(Item);
  end;

var
  Zoomed, Iconic, Sizable: Boolean;
  Icons: TBorderIcons;
  Separator: TMenuItem;
begin
  if FSystemMenu = nil then
  begin
    FSystemMenu := TERDPopupMenu.Create(Self);
    FSystemMenu.OnGetItemStyle := SystemMenuItemStyle;
  end;
  FSystemMenu.Theme := Theme;
  FSystemMenu.Density := Density;
  FSystemMenu.PopupComponent := Self;
  FSystemMenu.Items.Clear;
  Zoomed := IsZoomed(FHookedForm.Handle);
  Iconic := IsIconic(FHookedForm.Handle);
  Sizable := FormResizable;
  Icons := FormBorderIcons;
  // The same commands, states and order as the Windows system menu.
  AddItem('Restore', SC_RESTORE, Zoomed or Iconic);
  AddItem('Move', SC_MOVE, not Zoomed and not Iconic);
  AddItem('Size', SC_SIZE, Sizable and not Zoomed and not Iconic);
  AddItem('Minimise', SC_MINIMIZE, (biMinimize in Icons) and not Iconic);
  AddItem('Maximise', SC_MAXIMIZE, (biMaximize in Icons) and Sizable and
    not Zoomed);
  Separator := TMenuItem.Create(FSystemMenu);
  Separator.Caption := cLineCaption;
  FSystemMenu.Items.Add(Separator);
  AddItem('Close', SC_CLOSE, True, ShortCut(VK_F4, [ssAlt]));
end;

procedure TERDTitleBar.SystemMenuClick(Sender: TObject);
begin
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated or
    not (Sender is TMenuItem) then
    Exit;
  // Posted, so the menu has closed before the form moves, sizes or closes.
  PostMessage(FHookedForm.Handle, WM_SYSCOMMAND, WPARAM(TMenuItem(Sender).Tag),
    0);
end;

procedure TERDTitleBar.SystemMenuItemStyle(Sender: TObject; AItem: TMenuItem;
  var AGlyph: TERDGlyph; var ADanger: Boolean);
begin
  case AItem.Tag of
    SC_RESTORE:
      AGlyph := glRestore;
    SC_MINIMIZE:
      AGlyph := glMinimize;
    SC_MAXIMIZE:
      AGlyph := glMaximize;
    SC_CLOSE:
      AGlyph := glClose;
  end;
end;

procedure TERDTitleBar.ShowSystemMenuAt(X, Y: Integer);
begin
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated or
    (csDesigning in ComponentState) then
    Exit;
  // Our own themed menu instead of the native one, with the same commands.
  BuildSystemMenu;
  FSystemMenu.Popup(X, Y);
end;

procedure TERDTitleBar.ShowKeyboardSystemMenu;
var
  P: TPoint;
begin
  if FHookedForm = nil then
    Exit;
  P := FHookedForm.ClientToScreen(Point(ScaleValue(8), CaptionHeight));
  ShowSystemMenuAt(P.X, P.Y);
end;

procedure TERDTitleBar.ToggleMaximizeRestore;
begin
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  if IsZoomed(FHookedForm.Handle) then
    SendMessage(FHookedForm.Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
  else
    SendMessage(FHookedForm.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
end;

procedure TERDTitleBar.ExecuteSystemButton(AButton: TERDSystemButton);
begin
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  case AButton of
    sbMinimize:
      SendMessage(FHookedForm.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    sbMaximize:
      ToggleMaximizeRestore;
    sbClose:
      SendMessage(FHookedForm.Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
  end;
end;

procedure TERDTitleBar.ExecuteButton(AButton: TERDCaptionButton);
begin
  if (AButton = nil) or not AButton.Enabled then
    Exit;
  if Assigned(AButton.OnClick) then
    AButton.OnClick(AButton);
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, AButton);
end;

procedure TERDTitleBar.UpdateDwmFrame;
var
  Margins: TERDDwmMargins;
  Border: COLORREF;
  Dark: BOOL;
begin
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  EnsureDwmApi;
  if Assigned(GDwmExtendFrameIntoClientArea) then
  begin
    Margins.cxLeftWidth := 0;
    Margins.cxRightWidth := 0;
    Margins.cyTopHeight := 1;
    Margins.cyBottomHeight := 0;
    GDwmExtendFrameIntoClientArea(FHookedForm.Handle, Margins);
  end;
  if Assigned(GDwmSetWindowAttribute) and IsWindows11 then
  begin
    Dark := ERDIsDarkPalette(Palette);
    GDwmSetWindowAttribute(FHookedForm.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE,
      @Dark, SizeOf(Dark));
    if FActiveBorder then
    begin
      if EffectiveActive then
        Border := ColorRefFromColor(Palette.Accent)
      else
        Border := ColorRefFromColor(Palette.NeutralLight);
    end
    else
      Border := DWMWA_COLOR_DEFAULT;
    GDwmSetWindowAttribute(FHookedForm.Handle, DWMWA_BORDER_COLOR, @Border,
      SizeOf(Border));
  end;
end;

procedure TERDTitleBar.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Fill, LineColor: TColor;
  Active: Boolean;
  L, R: Integer;
begin
  UpdateDwmFrame;
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Active := EffectiveActive;
    Fill := SurfaceColor;
    P.FillRect(Rect(0, 0, Width, CaptionHeight), Fill);
    if (FCommandStyle <> csMenu) or (FMenuPlacement <> mpBelow) then
      P.HLine(0, CaptionHeight - 1, Width, Palette.NeutralLight);
    if FActiveBorder and not IsWindows11 then
    begin
      if Active then
        LineColor := Palette.Accent
      else
        LineColor := Palette.NeutralLight;
      P.HLine(0, 0, Width, LineColor);
    end;
    DrawAppIcon(P, ACanvas, AppIconRect);
    DrawAutoSave(P);
    DrawSystemButtons(P);
    DrawButtonCollection(P, ACanvas, FButtons, False);
    if FButtons.Count = 0 then
      DrawPreviewButtons(P, ACanvas, False);
    DrawStatus(P, StatusRect);
    if FCommandStyle = csRibbon then
    begin
      DrawButtonCollection(P, ACanvas, FQuickAccess, True);
      if FQuickAccess.Count = 0 then
        DrawPreviewButtons(P, ACanvas, True);
    end;
    L := QuickAccessLeft;
    if FCommandStyle = csRibbon then
    begin
      if VisibleButtonCount(FQuickAccess) > 0 then
        L := L + VisibleButtonCount(FQuickAccess) * ScaleValue(30) + ScaleValue(12)
      else if PreviewButtonCount(True) > 0 then
        L := L + PreviewButtonCount(True) * ScaleValue(30) + ScaleValue(12);
    end
    else if (FMenuPlacement = mpTitleBar) and EffectiveMenuVisible then
      L := MenuBarRect.Right + ScaleValue(12);
    R := StatusRect.Left - ScaleValue(8);
    if not FStatusVisible then
      R := ExtraButtonsLeft - ScaleValue(12);
    DrawTitleText(P, L, R);
  finally
    P.Free;
  end;
end;

procedure TERDTitleBar.DrawAppIcon(APainter: TERDPainter; ACanvas: TCanvas;
  const R: TRect);
var
  Form: TCustomForm;
  Icon: TIcon;
  S: string;
  C: TColor;
begin
  if FAppIconStyle = aiIcon then
  begin
    Form := GetParentForm(Self);
    Icon := nil;
    if (Form is TForm) and not TForm(Form).Icon.Empty then
      Icon := TForm(Form).Icon
    else if (Application <> nil) and not Application.Icon.Empty then
      Icon := Application.Icon;
    if Icon <> nil then
    begin
      if (FIconCacheSource <> Icon.Handle) or
        (FIconCacheSize <> R.Width) then
      begin
        if FIconCache <> 0 then
          DestroyIcon(FIconCache);
        FIconCache := CreateSizedIcon(Icon, R.Width);
        FIconCacheSource := Icon.Handle;
        FIconCacheSize := R.Width;
      end;
      if FIconCache <> 0 then
        DrawIconEx(ACanvas.Handle, R.Left, R.Top, FIconCache, R.Width,
          R.Height, 0, 0, DI_NORMAL)
      else
        DrawIconEx(ACanvas.Handle, R.Left, R.Top, Icon.Handle, R.Width,
          R.Height, 0, 0, DI_NORMAL);
      Exit;
    end;
  end;
  if EffectiveActive then
    C := Palette.Accent
  else
    C := Palette.NeutralLight;
  APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(4), C, clNone);
  S := FAppInitials;
  if S = '' then
    S := 'OS';
  if Length(S) > 3 then
    S := Copy(S, 1, 3);
  if EffectiveActive then
    C := APainter.OnAccent
  else
    C := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2, S,
    IfThen(R.Width < ScaleValue(30), 10, 12), C, twBold, taCenter);
end;

procedure TERDTitleBar.DrawSystemButtons(APainter: TERDPainter);
var
  Btn: TERDSystemButton;
  R: TRect;
  Fill, Ink: TColor;
  Glyph: TERDGlyph;
begin
  for Btn := sbMinimize to sbClose do
  begin
    R := SystemButtonRect(Btn);
    if R.IsEmpty then
      Continue;
    Fill := clNone;
    if EffectiveActive then
      Ink := Palette.ForegroundText
    else
      Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
    if (FHoverSystem = Btn) or (FPressedSystem = Btn) then
    begin
      if Btn = sbClose then
      begin
        Fill := Palette.Danger;
        Ink := clWhite;
      end
      else
        Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace,
          IfThen(FPressedSystem = Btn, 0.13, 0.08));
    end;
    if Fill <> clNone then
      APainter.FillRect(R, Fill);
    case Btn of
      sbMinimize:
        Glyph := glMinimize;
      sbMaximize:
        if FormMaximized then
          Glyph := glRestore
        else
          Glyph := glMaximize;
    else
      Glyph := glClose;
    end;
    APainter.Glyph(Glyph, R.Left + R.Width / 2, R.Top + R.Height / 2, Ink);
  end;
end;

procedure TERDTitleBar.DrawButtonCollection(APainter: TERDPainter;
  ACanvas: TCanvas; AButtons: TERDCaptionButtons; AQuick: Boolean);
var
  I: Integer;
  R: TRect;
  Hover: TERDCaptionButton;
begin
  if AButtons = nil then
    Exit;
  if AQuick then
    Hover := FHoverQuick
  else
    Hover := FHoverButton;
  for I := 0 to AButtons.Count - 1 do
    if AButtons[I].Visible then
    begin
      R := ItemRect(AButtons, AButtons[I], AQuick);
      DrawOneButton(APainter, ACanvas, R, AButtons[I].Glyph,
        AButtons[I].ImageIndex, AButtons[I].Badge, AButtons[I].BadgeKind,
        AButtons[I].Enabled, AButtons[I].Down, Hover = AButtons[I], AButtons[I].Icon);
    end;
end;

procedure TERDTitleBar.DrawPreviewButtons(APainter: TERDPainter;
  ACanvas: TCanvas; AQuick: Boolean);
var
  I: Integer;
  Glyphs: array[0..2] of TERDGlyph;
  Badges: array[0..2] of string;
begin
  if PreviewButtonCount(AQuick) = 0 then
    Exit;
  if AQuick then
  begin
    Glyphs[0] := glSave;
    Glyphs[1] := glUndo;
    Glyphs[2] := glRedo;
    Badges[0] := '';
    Badges[1] := '';
    Badges[2] := '';
  end
  else
  begin
    if ERDIsDarkPalette(Palette) then
      Glyphs[0] := glSun
    else
      Glyphs[0] := glMoon;
    Glyphs[1] := glBell;
    Glyphs[2] := glHelp;
    Badges[0] := '';
    Badges[1] := '3';
    Badges[2] := '';
  end;
  for I := 0 to 2 do
    DrawOneButton(APainter, ACanvas, PreviewItemRect(I, AQuick), Glyphs[I], -1,
      Badges[I], skDanger, True, False, False);
end;

procedure TERDTitleBar.DrawOneButton(APainter: TERDPainter; ACanvas: TCanvas;
  const R: TRect; AGlyph: TERDGlyph; AImageIndex: Integer; const ABadge: string;
  ABadgeKind: TERDStatusKind; AEnabled, ADown, AHover: Boolean; AIcon: TERDIcon);
var
  Fill, Ink: TColor;
begin
  Fill := clNone;
  if ADown then
  begin
    Fill := APainter.Tint(Palette.Accent, 0.16);
    Ink := APainter.AccentText;
  end
  else if AHover and AEnabled then
  begin
    Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.08);
    Ink := APainter.AccentText;
  end
  else if EffectiveActive then
    Ink := Palette.Subtle
  else
    Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55);
  if not AEnabled then
    Ink := APainter.DisabledText;
  if Fill <> clNone then
    APainter.RoundRect(R.Left + ScaleValue(3), R.Top + ScaleValue(5),
      R.Width - ScaleValue(6), R.Height - ScaleValue(10), ScaleValue(4),
      Fill, clNone);
  if AIcon <> nil then
    AIcon.Draw(ACanvas, Palette, APainter.PPI, R.Left + R.Width div 2,
      R.Top + R.Height div 2, Ink, 1, AEnabled, AGlyph, FImages, AImageIndex)
  else if AGlyph <> glNone then
    APainter.Glyph(AGlyph, R.Left + R.Width / 2, R.Top + R.Height / 2, Ink);
  if ABadge <> '' then
    // Keep clear of the window's top border: caption buttons span the
    // full caption height, so inset the badge like the hover fill.
    APainter.Badge(R.Right - ScaleValue(3),
      System.Math.Max(R.Top + ScaleValue(2), ScaleValue(6)), ABadge,
      APainter.StatusColor(ABadgeKind));
end;

procedure TERDTitleBar.DrawStatus(APainter: TERDPainter; const R: TRect);
var
  Text, Ink: string;
  C: TColor;
begin
  if not FStatusVisible then
    Exit;
  Text := FStatusCaption;
  if Text = '' then
    Text := 'CONNECTED';
  Ink := AnsiUpperCase(Text);
  C := APainter.StatusColor(FStatusKind);
  if EffectiveActive then
    APainter.Chip(R.Left, R.Top, Ink, C)
  else
  begin
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, R.Height / 2,
      clNone, Palette.NeutralLight);
    APainter.Text(R.Left + ScaleValue(8), R.Top + R.Height div 2, Ink,
      10.5, ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55),
      twSemibold);
  end;
end;

procedure TERDTitleBar.DrawTitleText(APainter: TERDPainter; ALeft,
  ARight: Integer);
var
  TitleText, Combined: string;
  Ink, SubInk: TColor;
  MaxW, Mid, W: Integer;
begin
  if ARight <= ALeft then
    Exit;
  TitleText := EffectiveTitle;
  MaxW := ARight - ALeft;
  if EffectiveActive then
  begin
    Ink := Palette.ForegroundText;
    SubInk := Palette.GaugeLabel;
  end
  else
  begin
    Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.7);
    SubInk := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55);
  end;
  if ((FCommandStyle = csRibbon) or ((FCommandStyle = csMenu) and
    (FMenuPlacement = mpTitleBar))) and (FSubtitle <> '') then
  begin
    Combined := FSubtitle + ' - ' + TitleText;
    Mid := Width div 2;
    W := APainter.TextWidth(Combined, 12.5, twRegular);
    if Mid - W div 2 < ALeft then
      Mid := ALeft + W div 2;
    APainter.Text(Mid, CaptionHeight div 2, Combined, 12.5, SubInk,
      twRegular, taCenter, MaxW);
  end
  else
  begin
    W := APainter.Text(ALeft + ScaleValue(2), CaptionHeight div 2, TitleText,
      14, Ink, twSemibold, taLeftJustify, MaxW);
    if (FSubtitle <> '') and (W + ScaleValue(16) < MaxW) then
      APainter.Text(ALeft + ScaleValue(2) + W + ScaleValue(14),
        CaptionHeight div 2, FSubtitle, 12.5, SubInk, twRegular,
        taLeftJustify, MaxW - W - ScaleValue(16));
  end;
end;

procedure TERDTitleBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSystem: TERDSystemButton;
  NewButton, NewQuick: TERDCaptionButton;
  AutoHot: Boolean;
begin
  inherited;
  AutoHot := PtInRect(AutoSaveRect, Point(X, Y));
  NewSystem := SystemButtonAt(X, Y);
  if NewSystem = sbMaximize then
    NewSystem := sbNone;
  NewButton := ButtonAt(FButtons, X, Y);
  NewQuick := QuickAt(X, Y);
  if (FAutoSaveHot <> AutoHot) or (FHoverSystem <> NewSystem) or (FHoverButton <> NewButton) or
    (FHoverQuick <> NewQuick) then
  begin
    FAutoSaveHot := AutoHot;
    FHoverSystem := NewSystem;
    FHoverButton := NewButton;
    FHoverQuick := NewQuick;
    Invalidate;
  end;
end;

procedure TERDTitleBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  inherited;
  if PtInRect(AppIconRect, Point(X, Y)) and (Button in [mbLeft, mbRight]) then
  begin
    // As with the Windows caption icon: a click opens the system menu
    // below the icon and a double-click closes the form.
    if (Button = mbLeft) and (ssDouble in Shift) then
    begin
      if (FHookedForm <> nil) and FHookedForm.HandleAllocated then
        PostMessage(FHookedForm.Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
      Exit;
    end;
    R := AppIconRect;
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    ShowSystemMenuAt(R.Left, R.Bottom + ScaleValue(4));
    Exit;
  end;
  if Button = mbRight then
  begin
    if PointInCaptionDragArea(X, Y) then
      ShowSystemMenuAt(ClientToScreen(Point(X, Y)).X, ClientToScreen(Point(X, Y)).Y);
    Exit;
  end;
  if Button <> mbLeft then
    Exit;
  if Enabled and PtInRect(AutoSaveRect, Point(X, Y)) then
  begin
    if TabStop and CanFocus then SetFocus;
    FAutoSavePressed := True;
    MouseCapture := True;
    Invalidate;
    Exit;
  end;
  FPressedSystem := SystemButtonAt(X, Y);
  if FPressedSystem = sbMaximize then
    FPressedSystem := sbNone;
  FPressedButton := ButtonAt(FButtons, X, Y);
  FPressedQuick := QuickAt(X, Y);
  if (FPressedSystem <> sbNone) or (FPressedButton <> nil) or
    (FPressedQuick <> nil) then
  begin
    MouseCapture := True;
    Invalidate;
  end;
end;

procedure TERDTitleBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  SystemHit: TERDSystemButton;
  ButtonHit, QuickHit: TERDCaptionButton;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if FAutoSavePressed then
  begin
    FAutoSavePressed := False;
    MouseCapture := False;
    if Enabled and PtInRect(AutoSaveRect, Point(X, Y)) then
      AutoSave := not FAutoSave;
    Invalidate;
    Exit;
  end;
  SystemHit := SystemButtonAt(X, Y);
  if SystemHit = sbMaximize then
    SystemHit := sbNone;
  ButtonHit := ButtonAt(FButtons, X, Y);
  QuickHit := QuickAt(X, Y);
  if (FPressedSystem <> sbNone) and (FPressedSystem = SystemHit) then
    ExecuteSystemButton(FPressedSystem)
  else if (FPressedButton <> nil) and (FPressedButton = ButtonHit) then
    ExecuteButton(FPressedButton)
  else if (FPressedQuick <> nil) and (FPressedQuick = QuickHit) then
    ExecuteButton(FPressedQuick);
  FPressedSystem := sbNone;
  FPressedButton := nil;
  FPressedQuick := nil;
  MouseCapture := False;
  Invalidate;
end;

end.
