//------------------------------------------------------------------------------
//  ERD.UI.TitleBar
//
//  Themed application title bar for the OBD Studio controls.
//
//    TOBDTitleBar        replaces the native form caption with a themed
//                        title bar, inline menu, ribbon quick access area,
//                        status chip, extra caption buttons and system
//                        caption buttons.
//    TOBDCaptionButton   streamable extra or quick-access title button.
//    TOBDCaptionButtons  owned collection of title buttons.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.TitleBar;

interface

uses
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
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Menus;

type
  TOBDTitleBar = class;
  TOBDCaptionButton = class;

  /// <summary>Where a classic menu is shown relative to the caption row.</summary>
  TOBDMenuPlacement = (
    /// <summary>The menu is embedded in the caption row.</summary>
    mpTitleBar,
    /// <summary>The menu is shown in a second row below the caption.</summary>
    mpBelow);

  /// <summary>Which command surface the title bar coordinates.</summary>
  TOBDCommandStyle = (
    /// <summary>Classic menu command surface.</summary>
    csMenu,
    /// <summary>Ribbon command surface with quick access buttons.</summary>
    csRibbon);

  /// <summary>Internal system caption button hit.</summary>
  TOBDSystemButton = (
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
  TOBDTitleButtonEvent = procedure(Sender: TObject;
    AButton: TOBDCaptionButton) of object;

  /// <summary>One streamable title-bar button.</summary>
  TOBDCaptionButton = class(TCollectionItem)
  strict private
    FGlyph: TOBDGlyph;
    FImageIndex: Integer;
    FHint: string;
    FBadge: string;
    FBadgeKind: TOBDStatusKind;
    FVisible: Boolean;
    FEnabled: Boolean;
    FDown: Boolean;
    FName: string;
    FTag: NativeInt;
    FOnClick: TNotifyEvent;
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetHint(const AValue: string);
    procedure SetBadge(const AValue: string);
    procedure SetBadgeKind(AValue: TOBDStatusKind);
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
    /// <summary>Copies all streamable fields from another button.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Built-in line glyph used when no image list image is assigned.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image list index; -1 uses <see cref="Glyph"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Hint shown when the pointer rests on the button.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Optional small counter or marker badge.</summary>
    property Badge: string read FBadge write SetBadge;
    /// <summary>Status colour used by <see cref="Badge"/>.</summary>
    property BadgeKind: TOBDStatusKind read FBadgeKind write SetBadgeKind
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
  TOBDCaptionButtons = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDCaptionButton;
    procedure SetItem(AIndex: Integer; AValue: TOBDCaptionButton);
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
    function Add: TOBDCaptionButton;
    /// <summary>Typed item access.</summary>
    property Items[AIndex: Integer]: TOBDCaptionButton read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed application title bar that replaces the native form caption.</summary>
  TOBDTitleBar = class(TOBDCustomControl)
  strict private
    FButtons: TOBDCaptionButtons;
    FQuickAccess: TOBDCaptionButtons;
    FImages: TCustomImageList;
    FMenu: TMainMenu;
    FMenuBar: TOBDMenuBar;
    FMenuPlacement: TOBDMenuPlacement;
    FCommandStyle: TOBDCommandStyle;
    FRibbon: TControl;
    FTitle: string;
    FSubtitle: string;
    FAppInitials: string;
    FStatusCaption: string;
    FStatusKind: TOBDStatusKind;
    FStatusVisible: Boolean;
    FActiveBorder: Boolean;
    FFormActive: Boolean;
    FHookedForm: TCustomForm;
    FOldFormWindowProc: TWndMethod;
    FHoverSystem: TOBDSystemButton;
    FPressedSystem: TOBDSystemButton;
    FHoverButton: TOBDCaptionButton;
    FPressedButton: TOBDCaptionButton;
    FHoverQuick: TOBDCaptionButton;
    FPressedQuick: TOBDCaptionButton;
    FOnButtonClick: TOBDTitleButtonEvent;
    FOnCommandStyleChange: TNotifyEvent;
    procedure SetButtons(AValue: TOBDCaptionButtons);
    procedure SetQuickAccess(AValue: TOBDCaptionButtons);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetMenu(AValue: TMainMenu);
    procedure SetMenuPlacement(AValue: TOBDMenuPlacement);
    procedure SetCommandStyle(AValue: TOBDCommandStyle);
    procedure SetRibbon(AValue: TControl);
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetAppInitials(const AValue: string);
    procedure SetStatusCaption(const AValue: string);
    procedure SetStatusKind(AValue: TOBDStatusKind);
    procedure SetStatusVisible(AValue: Boolean);
    procedure SetActiveBorder(AValue: Boolean);
    procedure CMParentChanged(var Message: TMessage); message CM_PARENTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    function CaptionHeight: Integer;
    function MenuHeight: Integer;
    function TotalHeight: Integer;
    function SystemButtonWidth: Integer;
    function ExtraButtonWidth: Integer;
    function VisibleSystemButtonCount: Integer;
    function VisibleButtonCount(AButtons: TOBDCaptionButtons): Integer;
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
    function ItemRect(AButtons: TOBDCaptionButtons; AButton: TOBDCaptionButton;
      AQuick: Boolean): TRect;
    function PreviewItemRect(AIndex: Integer; AQuick: Boolean): TRect;
    function SystemButtonRect(AButton: TOBDSystemButton): TRect;
    function SystemButtonAt(X, Y: Integer): TOBDSystemButton;
    function ButtonAt(AButtons: TOBDCaptionButtons; X, Y: Integer): TOBDCaptionButton;
    function QuickAt(X, Y: Integer): TOBDCaptionButton;
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
    procedure ExecuteSystemButton(AButton: TOBDSystemButton);
    procedure ExecuteButton(AButton: TOBDCaptionButton);
    procedure UpdateChildLayout;
    procedure UpdateCommandSurface;
    procedure UpdateDwmFrame;
    procedure ApplyHeight;
    procedure DrawAppIcon(APainter: TOBDPainter; ACanvas: TCanvas;
      const R: TRect);
    procedure DrawSystemButtons(APainter: TOBDPainter);
    procedure DrawButtonCollection(APainter: TOBDPainter; ACanvas: TCanvas;
      AButtons: TOBDCaptionButtons; AQuick: Boolean);
    procedure DrawPreviewButtons(APainter: TOBDPainter; ACanvas: TCanvas;
      AQuick: Boolean);
    procedure DrawOneButton(APainter: TOBDPainter; ACanvas: TCanvas;
      const R: TRect; AGlyph: TOBDGlyph; AImageIndex: Integer;
      const ABadge: string; ABadgeKind: TOBDStatusKind; AEnabled, ADown,
      AHover: Boolean);
    procedure DrawStatus(APainter: TOBDPainter; const R: TRect);
    procedure DrawTitleText(APainter: TOBDPainter; ALeft, ARight: Integer);
  protected
    /// <summary>Clears component references when they are freed.</summary>
    /// <param name="AComponent">Component being inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the title bar and caption controls.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Applies the hook and menu layout after streaming.</summary>
    procedure Loaded; override;
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
  published
    /// <summary>Extra caption buttons between the status chip and system buttons.</summary>
    property Buttons: TOBDCaptionButtons read FButtons write SetButtons;
    /// <summary>Ribbon quick-access buttons shown in <c>csRibbon</c> mode.</summary>
    property QuickAccess: TOBDCaptionButtons read FQuickAccess
      write SetQuickAccess;
    /// <summary>Image list used by quick-access and extra caption buttons.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Main menu shown by the embedded menu bar.</summary>
    property Menu: TMainMenu read FMenu write SetMenu;
    /// <summary>Placement of the classic menu bar.</summary>
    property MenuPlacement: TOBDMenuPlacement read FMenuPlacement
      write SetMenuPlacement default mpTitleBar;
    /// <summary>Classic menu or ribbon command surface.</summary>
    property CommandStyle: TOBDCommandStyle read FCommandStyle
      write SetCommandStyle default csMenu;
    /// <summary>Ribbon control shown only when <see cref="CommandStyle"/> is <c>csRibbon</c>.</summary>
    property Ribbon: TControl read FRibbon write SetRibbon;
    /// <summary>Title text; empty uses the parent form caption.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Optional context text rendered before or beside the title.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Initials painted in the accent app icon when the form has no icon.</summary>
    property AppInitials: string read FAppInitials write SetAppInitials;
    /// <summary>Status chip text.</summary>
    property StatusCaption: string read FStatusCaption write SetStatusCaption;
    /// <summary>Status chip colour.</summary>
    property StatusKind: TOBDStatusKind read FStatusKind write SetStatusKind
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
    property OnButtonClick: TOBDTitleButtonEvent read FOnButtonClick
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
  POBDNCCalcSizeParams = ^TOBDNCCalcSizeParams;
  TOBDNCCalcSizeParams = record
    rgrc: array[0..2] of TRect;
    lppos: PWindowPos;
  end;

  TOBDDwmMargins = record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end;

  TOBDOSVersionInfo = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar;
  end;

  TOBDDwmSetWindowAttribute = function(AHandle: HWND; AAttribute: DWORD;
    AValue: Pointer; ASize: DWORD): HRESULT; stdcall;
  TOBDDwmExtendFrameIntoClientArea = function(AHandle: HWND;
    const AMargins: TOBDDwmMargins): HRESULT; stdcall;
  TOBDRtlGetVersion = function(var AInfo: TOBDOSVersionInfo): LongInt; stdcall;

var
  GDwmApi: HMODULE = 0;
  GDwmSetWindowAttribute: TOBDDwmSetWindowAttribute = nil;
  GDwmExtendFrameIntoClientArea: TOBDDwmExtendFrameIntoClientArea = nil;
  GWindowsBuild: Integer = -1;

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
  RtlGetVersion: TOBDRtlGetVersion;
  Info: TOBDOSVersionInfo;
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

{ TOBDCaptionButton ---------------------------------------------------------- }

constructor TOBDCaptionButton.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
  FBadgeKind := skDanger;
  FVisible := True;
  FEnabled := True;
end;

procedure TOBDCaptionButton.Assign(Source: TPersistent);
var
  Button: TOBDCaptionButton;
begin
  if Source is TOBDCaptionButton then
  begin
    Button := TOBDCaptionButton(Source);
    FGlyph := Button.FGlyph;
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

function TOBDCaptionButton.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then
    Result := FHint;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDCaptionButton.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetBadge(const AValue: string);
begin
  if FBadge = AValue then
    Exit;
  FBadge := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetBadgeKind(AValue: TOBDStatusKind);
begin
  if FBadgeKind = AValue then
    Exit;
  FBadgeKind := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetDown(AValue: Boolean);
begin
  if FDown = AValue then
    Exit;
  FDown := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDCaptionButton.SetTag(AValue: NativeInt);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
  Changed(False);
end;

{ TOBDCaptionButtons --------------------------------------------------------- }

constructor TOBDCaptionButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDCaptionButton);
end;

function TOBDCaptionButtons.Add: TOBDCaptionButton;
begin
  Result := TOBDCaptionButton(inherited Add);
end;

function TOBDCaptionButtons.GetItem(AIndex: Integer): TOBDCaptionButton;
begin
  Result := TOBDCaptionButton(inherited Items[AIndex]);
end;

procedure TOBDCaptionButtons.SetItem(AIndex: Integer; AValue: TOBDCaptionButton);
begin
  inherited Items[AIndex] := AValue;
end;

procedure TOBDCaptionButtons.Update(Item: TCollectionItem);
var
  Owner: TPersistent;
begin
  inherited;
  Owner := GetOwner;
  if Owner is TOBDTitleBar then
  begin
    TOBDTitleBar(Owner).UpdateChildLayout;
    TOBDTitleBar(Owner).Invalidate;
  end;
end;

{ TOBDTitleBar --------------------------------------------------------------- }

constructor TOBDTitleBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alTop;
  TabStop := False;
  ShowHint := True;
  FButtons := TOBDCaptionButtons.Create(Self);
  FQuickAccess := TOBDCaptionButtons.Create(Self);
  FMenuPlacement := mpTitleBar;
  FCommandStyle := csMenu;
  FAppInitials := 'OS';
  FStatusCaption := 'CONNECTED';
  FStatusKind := skSuccess;
  FActiveBorder := True;
  FFormActive := True;
  FHoverSystem := sbNone;
  FPressedSystem := sbNone;
  FMenuBar := TOBDMenuBar.Create(Self);
  FMenuBar.Parent := Self;
  FMenuBar.Visible := False;
  FMenuBar.Embedded := True;
  Height := TotalHeight;
end;

destructor TOBDTitleBar.Destroy;
begin
  UnhookParentForm;
  FreeAndNil(FButtons);
  FreeAndNil(FQuickAccess);
  FreeAndNil(FMenuBar);
  inherited Destroy;
end;

procedure TOBDTitleBar.Loaded;
begin
  inherited;
  Align := alTop;
  ApplyHeight;
  HookParentForm;
  UpdateCommandSurface;
  UpdateChildLayout;
end;

procedure TOBDTitleBar.Notification(AComponent: TComponent;
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
    begin
      FHookedForm := nil;
      FOldFormWindowProc := nil;
    end;
  end;
end;

procedure TOBDTitleBar.SetButtons(AValue: TOBDCaptionButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TOBDTitleBar.SetQuickAccess(AValue: TOBDCaptionButtons);
begin
  FQuickAccess.Assign(AValue);
end;

procedure TOBDTitleBar.SetImages(AValue: TCustomImageList);
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

procedure TOBDTitleBar.SetMenu(AValue: TMainMenu);
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

procedure TOBDTitleBar.SetMenuPlacement(AValue: TOBDMenuPlacement);
begin
  if FMenuPlacement = AValue then
    Exit;
  FMenuPlacement := AValue;
  ApplyHeight;
  UpdateCommandSurface;
  UpdateChildLayout;
  Invalidate;
end;

procedure TOBDTitleBar.SetCommandStyle(AValue: TOBDCommandStyle);
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

procedure TOBDTitleBar.SetRibbon(AValue: TControl);
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

procedure TOBDTitleBar.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDTitleBar.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDTitleBar.SetAppInitials(const AValue: string);
begin
  if FAppInitials = AValue then
    Exit;
  FAppInitials := AValue;
  Invalidate;
end;

procedure TOBDTitleBar.SetStatusCaption(const AValue: string);
begin
  if FStatusCaption = AValue then
    Exit;
  FStatusCaption := AValue;
  UpdateChildLayout;
  Invalidate;
end;

procedure TOBDTitleBar.SetStatusKind(AValue: TOBDStatusKind);
begin
  if FStatusKind = AValue then
    Exit;
  FStatusKind := AValue;
  Invalidate;
end;

procedure TOBDTitleBar.SetStatusVisible(AValue: Boolean);
begin
  if FStatusVisible = AValue then
    Exit;
  FStatusVisible := AValue;
  UpdateChildLayout;
  Invalidate;
end;

procedure TOBDTitleBar.SetActiveBorder(AValue: Boolean);
begin
  if FActiveBorder = AValue then
    Exit;
  FActiveBorder := AValue;
  UpdateDwmFrame;
  Invalidate;
end;

procedure TOBDTitleBar.CMParentChanged(var Message: TMessage);
begin
  inherited;
  Align := alTop;
  HookParentForm;
  UpdateChildLayout;
end;

procedure TOBDTitleBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FPressedButton = nil then
    FHoverButton := nil;
  if FPressedQuick = nil then
    FHoverQuick := nil;
  if FPressedSystem = sbNone then
    FHoverSystem := sbNone;
  Invalidate;
end;

procedure TOBDTitleBar.CMHintShow(var Message: TCMHintShow);
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

function TOBDTitleBar.CaptionHeight: Integer;
begin
  Result := ScaleValue(Metrics.TitleBar);
end;

function TOBDTitleBar.MenuHeight: Integer;
begin
  Result := ScaleValue(Metrics.MenuBar);
end;

function TOBDTitleBar.TotalHeight: Integer;
begin
  Result := CaptionHeight;
  if (FCommandStyle = csMenu) and (FMenuPlacement = mpBelow) then
    Inc(Result, MenuHeight);
end;

function TOBDTitleBar.SystemButtonWidth: Integer;
begin
  Result := ScaleValue(Metrics.CaptionButton);
end;

function TOBDTitleBar.ExtraButtonWidth: Integer;
begin
  Result := System.Math.Max(ScaleValue(28), CaptionHeight - ScaleValue(4));
end;

function TOBDTitleBar.VisibleSystemButtonCount: Integer;
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

function TOBDTitleBar.VisibleButtonCount(AButtons: TOBDCaptionButtons): Integer;
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

function TOBDTitleBar.PreviewButtonCount(AQuick: Boolean): Integer;
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

function TOBDTitleBar.EffectiveTitle: string;
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
    Result := 'OBD Studio';
end;

function TOBDTitleBar.EffectiveActive: Boolean;
begin
  Result := FFormActive or (csDesigning in ComponentState);
end;

function TOBDTitleBar.EffectiveMenuVisible: Boolean;
begin
  Result := (FCommandStyle = csMenu) and ((FMenu <> nil) or IsPreview);
end;

function TOBDTitleBar.FormResizable: Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  Result := (Form <> nil) and (Form.BorderStyle in [bsSizeable, bsSizeToolWin]);
end;

function TOBDTitleBar.FormMaximized: Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  Result := (Form <> nil) and Form.HandleAllocated and IsZoomed(Form.Handle);
end;

function TOBDTitleBar.FormBorderIcons: TBorderIcons;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Result := Form.BorderIcons
  else
    Result := [biSystemMenu, biMinimize, biMaximize];
end;

function TOBDTitleBar.SystemButtonsLeft: Integer;
begin
  Result := Width - VisibleSystemButtonCount * SystemButtonWidth;
end;

function TOBDTitleBar.ExtraButtonsLeft: Integer;
var
  Count: Integer;
begin
  Count := VisibleButtonCount(FButtons);
  if Count = 0 then
    Count := PreviewButtonCount(False);
  Result := SystemButtonsLeft - ScaleValue(8) - Count * ExtraButtonWidth;
end;

function TOBDTitleBar.QuickAccessLeft: Integer;
begin
  Result := AppIconRect.Right + ScaleValue(10);
end;

function TOBDTitleBar.MenuBarRect: TRect;
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
  L := AppIconRect.Right + ScaleValue(10);
  if (FCommandStyle = csRibbon) and (VisibleButtonCount(FQuickAccess) > 0) then
    L := L + VisibleButtonCount(FQuickAccess) * ScaleValue(30) + ScaleValue(12);
  W := FMenuBar.ItemsWidth;
  if W <= 0 then
    W := ScaleValue(320);
  MaxW := System.Math.Max(0, StatusRect.Left - L - ScaleValue(24));
  W := System.Math.Min(W, MaxW);
  Result := Rect(L, ScaleValue(6), L + W, CaptionHeight - ScaleValue(6));
end;

function TOBDTitleBar.StatusRect: TRect;
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
  W := OBDMeasureText(AnsiUpperCase(Text), 10.5, twSemibold,
    ScaleValue(96)) + ScaleValue(18);
  W := System.Math.Max(W, ScaleValue(36));
  R := ExtraButtonsLeft - ScaleValue(12);
  Result := Rect(R - W, (CaptionHeight - ScaleValue(20)) div 2, R,
    (CaptionHeight + ScaleValue(20)) div 2);
end;

function TOBDTitleBar.AppIconRect: TRect;
var
  S: Integer;
begin
  S := System.Math.Max(ScaleValue(20), CaptionHeight - ScaleValue(16));
  Result := Rect(ScaleValue(12), ScaleValue(8), ScaleValue(12) + S,
    ScaleValue(8) + S);
end;

function TOBDTitleBar.ItemRect(AButtons: TOBDCaptionButtons;
  AButton: TOBDCaptionButton; AQuick: Boolean): TRect;
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

function TOBDTitleBar.PreviewItemRect(AIndex: Integer; AQuick: Boolean): TRect;
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

function TOBDTitleBar.SystemButtonRect(AButton: TOBDSystemButton): TRect;
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

function TOBDTitleBar.SystemButtonAt(X, Y: Integer): TOBDSystemButton;
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

function TOBDTitleBar.ButtonAt(AButtons: TOBDCaptionButtons; X,
  Y: Integer): TOBDCaptionButton;
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

function TOBDTitleBar.QuickAt(X, Y: Integer): TOBDCaptionButton;
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

function TOBDTitleBar.PointInMenu(X, Y: Integer): Boolean;
var
  R: TRect;
begin
  R := MenuBarRect;
  Result := not R.IsEmpty and PtInRect(R, Point(X, Y));
end;

function TOBDTitleBar.PointInCommandButton(X, Y: Integer): Boolean;
begin
  Result := (SystemButtonAt(X, Y) <> sbNone) or
    (ButtonAt(FButtons, X, Y) <> nil) or (QuickAt(X, Y) <> nil) or
    PtInRect(StatusRect, Point(X, Y)) or PtInRect(AppIconRect, Point(X, Y));
end;

function TOBDTitleBar.PointInCaptionDragArea(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < Width) and (Y >= 0) and (Y < CaptionHeight) and
    not PointInMenu(X, Y) and not PointInCommandButton(X, Y);
end;

procedure TOBDTitleBar.ApplyHeight;
begin
  Align := alTop;
  Height := TotalHeight;
end;

procedure TOBDTitleBar.UpdateChildLayout;
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

procedure TOBDTitleBar.UpdateCommandSurface;
begin
  if FRibbon <> nil then
    FRibbon.Visible := FCommandStyle = csRibbon;
  if FMenuBar <> nil then
    FMenuBar.Visible := EffectiveMenuVisible;
end;

procedure TOBDTitleBar.DensityChanged;
begin
  inherited;
  ApplyHeight;
  UpdateChildLayout;
end;

procedure TOBDTitleBar.Resize;
begin
  inherited;
  UpdateChildLayout;
end;

procedure TOBDTitleBar.HookParentForm;
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

procedure TOBDTitleBar.UnhookParentForm;
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

procedure TOBDTitleBar.FormWindowProc(var Message: TMessage);
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
  else
    inherited WndProc(Message);
end;

procedure TOBDTitleBar.HandleNCCalcSize(var Message: TMessage);
var
  Params: POBDNCCalcSizeParams;
  R: PRect;
  Mon: TMonitor;
  Work: TRect;
begin
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message);
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  if Message.WParam <> 0 then
  begin
    Params := POBDNCCalcSizeParams(Message.LParam);
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
      Dec(Params^.rgrc[0].Top, CaptionHeight);
  end
  else
  begin
    R := PRect(Message.LParam);
    if R <> nil then
      Dec(R^.Top, CaptionHeight);
  end;
  Message.Result := 0;
end;

procedure TOBDTitleBar.HandleNCHitTest(var Message: TMessage);
var
  P, C: TPoint;
  WR: TRect;
  Edge, Corner: Integer;
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
  GetWindowRect(FHookedForm.Handle, WR);
  if FormResizable and not FormMaximized then
  begin
    Edge := System.Math.Max(ScaleValue(5), GetSystemMetrics(SM_CYSIZEFRAME) +
      GetSystemMetrics(SM_CXPADDEDBORDER));
    Corner := Edge + ScaleValue(16);
    if P.Y < WR.Top + Edge then
    begin
      if P.X < WR.Left + Corner then
        Message.Result := HTTOPLEFT
      else if P.X >= WR.Right - Corner then
        Message.Result := HTTOPRIGHT
      else
        Message.Result := HTTOP;
      Exit;
    end;
  end;
  C := ScreenToClient(P);
  if (C.X < 0) or (C.X >= Width) or (C.Y < 0) or (C.Y >= Height) then
    Exit;
  if SystemButtonAt(C.X, C.Y) = sbMaximize then
  begin
    Message.Result := HTMAXBUTTON;
    Exit;
  end;
  if PointInCaptionDragArea(C.X, C.Y) then
    Message.Result := HTCAPTION
  else
    Message.Result := HTCLIENT;
end;

procedure TOBDTitleBar.HandleNCMouseMove(var Message: TMessage);
var
  C: TPoint;
  NewHover: TOBDSystemButton;
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
  if Assigned(FOldFormWindowProc) then
    FOldFormWindowProc(Message);
end;

procedure TOBDTitleBar.HandleNCLButtonDown(var Message: TMessage);
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

procedure TOBDTitleBar.HandleNCLButtonUp(var Message: TMessage);
var
  C: TPoint;
  Hit: TOBDSystemButton;
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

procedure TOBDTitleBar.ShowSystemMenuAt(X, Y: Integer);
var
  MenuHandle: HMENU;
  Cmd: Integer;
begin
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  MenuHandle := GetSystemMenu(FHookedForm.Handle, False);
  if MenuHandle = 0 then
    Exit;
  Cmd := TrackPopupMenu(MenuHandle, TPM_LEFTBUTTON or TPM_RIGHTBUTTON or
    TPM_RETURNCMD, X, Y, 0, FHookedForm.Handle, nil);
  if Cmd <> 0 then
    SendMessage(FHookedForm.Handle, WM_SYSCOMMAND, Cmd, 0);
end;

procedure TOBDTitleBar.ShowKeyboardSystemMenu;
var
  P: TPoint;
begin
  if FHookedForm = nil then
    Exit;
  P := FHookedForm.ClientToScreen(Point(ScaleValue(8), CaptionHeight));
  ShowSystemMenuAt(P.X, P.Y);
end;

procedure TOBDTitleBar.ToggleMaximizeRestore;
begin
  if (FHookedForm = nil) or not FHookedForm.HandleAllocated then
    Exit;
  if IsZoomed(FHookedForm.Handle) then
    SendMessage(FHookedForm.Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
  else
    SendMessage(FHookedForm.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
end;

procedure TOBDTitleBar.ExecuteSystemButton(AButton: TOBDSystemButton);
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

procedure TOBDTitleBar.ExecuteButton(AButton: TOBDCaptionButton);
begin
  if (AButton = nil) or not AButton.Enabled then
    Exit;
  if Assigned(AButton.OnClick) then
    AButton.OnClick(AButton);
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, AButton);
end;

procedure TOBDTitleBar.UpdateDwmFrame;
var
  Margins: TOBDDwmMargins;
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
    Dark := OBDIsDarkPalette(Palette);
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

procedure TOBDTitleBar.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Fill, LineColor: TColor;
  Active: Boolean;
  L, R: Integer;
begin
  UpdateDwmFrame;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Active := EffectiveActive;
    if Active then
      Fill := Palette.GaugeFace
    else
      Fill := Palette.Background;
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
    L := AppIconRect.Right + ScaleValue(10);
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

procedure TOBDTitleBar.DrawAppIcon(APainter: TOBDPainter; ACanvas: TCanvas;
  const R: TRect);
var
  Form: TCustomForm;
  S: string;
  C: TColor;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and (Form.Icon <> nil) and not Form.Icon.Empty then
  begin
    DrawIconEx(ACanvas.Handle, R.Left, R.Top, Form.Icon.Handle, R.Width,
      R.Height, 0, 0, DI_NORMAL);
    Exit;
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
    C := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2, S,
    IfThen(R.Width < ScaleValue(30), 10, 12), C, twBold, taCenter);
end;

procedure TOBDTitleBar.DrawSystemButtons(APainter: TOBDPainter);
var
  Btn: TOBDSystemButton;
  R: TRect;
  Fill, Ink: TColor;
  Glyph: TOBDGlyph;
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
      Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
    if (FHoverSystem = Btn) or (FPressedSystem = Btn) then
    begin
      if Btn = sbClose then
      begin
        Fill := Palette.Danger;
        Ink := clWhite;
      end
      else
        Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace,
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

procedure TOBDTitleBar.DrawButtonCollection(APainter: TOBDPainter;
  ACanvas: TCanvas; AButtons: TOBDCaptionButtons; AQuick: Boolean);
var
  I: Integer;
  R: TRect;
  Hover: TOBDCaptionButton;
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
        AButtons[I].Enabled, AButtons[I].Down, Hover = AButtons[I]);
    end;
end;

procedure TOBDTitleBar.DrawPreviewButtons(APainter: TOBDPainter;
  ACanvas: TCanvas; AQuick: Boolean);
var
  I: Integer;
  Glyphs: array[0..2] of TOBDGlyph;
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
    if OBDIsDarkPalette(Palette) then
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

procedure TOBDTitleBar.DrawOneButton(APainter: TOBDPainter; ACanvas: TCanvas;
  const R: TRect; AGlyph: TOBDGlyph; AImageIndex: Integer; const ABadge: string;
  ABadgeKind: TOBDStatusKind; AEnabled, ADown, AHover: Boolean);
var
  Fill, Ink: TColor;
  IX, IY: Integer;
begin
  Fill := clNone;
  if ADown then
  begin
    Fill := APainter.Tint(Palette.Accent, 0.16);
    Ink := APainter.AccentText;
  end
  else if AHover and AEnabled then
  begin
    Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.08);
    Ink := APainter.AccentText;
  end
  else if EffectiveActive then
    Ink := Palette.Subtle
  else
    Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55);
  if not AEnabled then
    Ink := APainter.DisabledText;
  if Fill <> clNone then
    APainter.RoundRect(R.Left + ScaleValue(3), R.Top + ScaleValue(5),
      R.Width - ScaleValue(6), R.Height - ScaleValue(10), ScaleValue(4),
      Fill, clNone);
  if (FImages <> nil) and (AImageIndex >= 0) and (AImageIndex < FImages.Count) then
  begin
    IX := R.Left + (R.Width - FImages.Width) div 2;
    IY := R.Top + (R.Height - FImages.Height) div 2;
    FImages.Draw(ACanvas, IX, IY, AImageIndex, AEnabled);
  end
  else if AGlyph <> glNone then
    APainter.Glyph(AGlyph, R.Left + R.Width / 2, R.Top + R.Height / 2, Ink);
  if ABadge <> '' then
    APainter.Badge(R.Right - ScaleValue(3), R.Top + ScaleValue(2), ABadge,
      APainter.StatusColor(ABadgeKind));
end;

procedure TOBDTitleBar.DrawStatus(APainter: TOBDPainter; const R: TRect);
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
      10.5, OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55),
      twSemibold);
  end;
end;

procedure TOBDTitleBar.DrawTitleText(APainter: TOBDPainter; ALeft,
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
    Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.7);
    SubInk := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.55);
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

procedure TOBDTitleBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSystem: TOBDSystemButton;
  NewButton, NewQuick: TOBDCaptionButton;
begin
  inherited;
  NewSystem := SystemButtonAt(X, Y);
  if NewSystem = sbMaximize then
    NewSystem := sbNone;
  NewButton := ButtonAt(FButtons, X, Y);
  NewQuick := QuickAt(X, Y);
  if (FHoverSystem <> NewSystem) or (FHoverButton <> NewButton) or
    (FHoverQuick <> NewQuick) then
  begin
    FHoverSystem := NewSystem;
    FHoverButton := NewButton;
    FHoverQuick := NewQuick;
    Invalidate;
  end;
end;

procedure TOBDTitleBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbRight then
  begin
    if PointInCaptionDragArea(X, Y) then
      ShowSystemMenuAt(ClientToScreen(Point(X, Y)).X, ClientToScreen(Point(X, Y)).Y);
    Exit;
  end;
  if Button <> mbLeft then
    Exit;
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

procedure TOBDTitleBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  SystemHit: TOBDSystemButton;
  ButtonHit, QuickHit: TOBDCaptionButton;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
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
