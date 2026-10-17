// ------------------------------------------------------------------------------
// ERD.Components.Control
//
// Custom-paint base classes every visual derives from.
//
// TERDCustomControl    windowed control (focusable, accepts
// keyboard input). Use for lists,
// interactive widgets, anything that
// needs Tab focus.
// TERDGraphicControl   lightweight non-windowed control.
// Use for indicator lamps, badges,
// sparklines — anything that can sit
// inside another control's client area
// transparently.
//
// Both bases satisfy the universal quality bar:
// - Theme-aware (auto-bind via TERDTheme.FindOnOwner,
// explicit Theme property, per-component Style overrides,
// resolution chain with VCL Style fallback and brand
// default).
// - HiDPI-aware (ScaleValue helper, ChangeScale override
// that invalidates on DPI change, geometry uses MulDiv).
// - Production-ready (double-buffered paint pipeline,
// thread-safe Value setter on subclasses, csDesigning /
// csLoading safe).
// - Testable (RenderTo paints into an off-screen bitmap
// without a window handle, so rendering tests can verify
// that a control draws something).
// - Dashboard-ready (IERDTileHost lets a parent dashboard
// take over mouse input and draw an edit overlay; tiles
// persist their settings through SaveSettings /
// LoadSettings).
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-10  ERD  Invalidate marks the paint buffer dirty. RenderTo,
//                  ForcePreview, UnitSystem, tile-host edit mode and
//                  settings persistence. Density / ParentDensity and
//                  Metrics for the Application controls. IERDSurface:
//                  children of a card take its surface colour.
// ------------------------------------------------------------------------------

unit ERD.Components.Control;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}Math{$ELSE}System.Math{$ENDIF},
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Themes,
  System.JSON,
  ERD.Components.Types,
  ERD.Components.Theme;

const
  /// <summary>Sent by a ribbon to the control assigned to its Backstage
  /// property. LParam is the ribbon, or 0 to unlink.</summary>
  CM_ERD_BACKSTAGELINK = WM_APP + $0B10;
  /// <summary>Sent by a backstage to its linked ribbon when it is shown
  /// (WParam 1) or hidden (WParam 0), so the ribbon can make room like
  /// the Office backstage view.</summary>
  CM_ERD_BACKSTAGEVISIBLE = WM_APP + $0B11;

type
  /// <summary>Hit-test result for a tile in dashboard edit mode.
  /// </summary>
  TERDTileHit = (
    /// <summary>Outside the control.</summary>
    thNone,
    /// <summary>Body of the tile: drag to move.</summary>
    thMove,
    /// <summary>Bottom-right grip: drag to resize.</summary>
    thResize,
    /// <summary>Top-right close glyph: click to remove.</summary>
    thClose);

  /// <summary>Implemented by containers (the dashboard) that lay out
  /// ERD Components controls as tiles and can put them into an edit mode
  /// where the user moves, resizes and removes tiles.</summary>
  /// <remarks>While <c>IsEditingTiles</c> returns True, every mouse
  /// message a child <see cref="TERDCustomControl"/> receives is
  /// forwarded to <c>TileMouseMessage</c> instead of the child's own
  /// handlers, and the child paints an edit overlay.</remarks>
  IERDTileHost = interface
    ['{A3D0C979-B612-5007-92C3-E63D8E26091C}']
    /// <summary>True while the host is in tile edit mode.</summary>
    /// <returns>Edit-mode flag.</returns>
    function IsEditingTiles: Boolean;
    /// <summary>Receives a mouse message from a child tile.</summary>
    /// <param name="ATile">Tile control that received the message.
    /// </param>
    /// <param name="AMessage">The original message; coordinates are in
    /// the tile's client space.</param>
    procedure TileMouseMessage(ATile: TControl; var AMessage: TMessage);
  end;

  /// <summary>Implemented by containers that paint their own surface
  /// (cards, banners, panels). A <see cref="TERDCustomControl"/>
  /// placed on such a container fills its background with the
  /// container's surface colour instead of the theme background, so a
  /// button on a card sits on the card face.</summary>
  IERDSurface = interface
    ['{B0FF264D-0976-5D1B-96B2-EDCB2C756E9D}']
    /// <summary>Colour child controls paint their background with.
    /// </summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
  end;

  /// <summary>Base for windowed visuals (focusable, can host
  /// keyboard input). Subclasses override <c>PaintControl</c>
  /// to draw onto the supplied <c>TCanvas</c>; the base class
  /// handles double-buffering, theme resolution, and DPI
  /// scaling.</summary>
  TERDCustomControl = class(TCustomControl, IERDThemeAware, IERDDensityAware, IERDDensitySource)
  strict private
    FTheme: TERDTheme;
    FStyle: TERDVisualStyle;
    FResolvedTheme: TERDTheme;
    FBuffer: TBitmap;
    FBufferDirty: Boolean;
    FDesignPPI: Integer;
    FForcePreview: Boolean;
    FDensity: TERDDensity;
    FParentDensity: Boolean;
    FEditHot: TERDTileHit;
    FEditDown: TERDTileHit;
    procedure SetTheme(AValue: TERDTheme);
    function GetDensity: TERDDensity;
    function GetParentDensity: Boolean;
    procedure SetDensity(AValue: TERDDensity);
    procedure SetParentDensity(AValue: Boolean);
    function IsDensityStored: Boolean;
    procedure ResolveTheme;
    procedure DetachFromTheme;
    function GetStyleBackground: TColor;
    function GetStyleForeground: TColor;
    function GetStyleAccent: TColor;
    function GetStyleBorder: TColor;
    procedure SetStyleBackground(AValue: TColor);
    procedure SetStyleForeground(AValue: TColor);
    procedure SetStyleAccent(AValue: TColor);
    procedure SetStyleBorder(AValue: TColor);
    procedure SetForcePreview(AValue: Boolean);
    function TileHost: IERDTileHost;
    function EditCloseRect: TRect;
    function EditGripSize: Integer;
    procedure DrawEditOverlay(ACanvas: TCanvas);
  protected
    /// <summary>Override and paint onto <c>ACanvas</c>. Bounds
    /// are <c>ClientRect</c>. Theme palette already resolved
    /// for you via <see cref="Palette"/>.</summary>
    procedure PaintControl(ACanvas: TCanvas); virtual; abstract;

    /// <summary>Currently-resolved palette. Walks the chain:
    /// explicit Theme ▸ auto-found owner Theme ▸ default Theme
    /// ▸ VCL Style ▸ brand default.</summary>
    function Palette: TERDThemePalette;

    /// <summary>Scales <c>N</c> from 96-DPI design pixels to
    /// the control's current rendering DPI. Use this for every
    /// pixel constant in paint code.</summary>
    function ScaleValue(N: Integer): Integer; inline;

    /// <summary>Helper for paint code: returns the effective
    /// background colour after Style ▸ Theme ▸ default
    /// resolution.</summary>
    function EffectiveBackground: TColor;
    function EffectiveForeground: TColor;
    function EffectiveAccent: TColor;
    function EffectiveBorder: TColor;

    /// <summary>Adds WS_CLIPCHILDREN so repainting the buffer never paints
    /// over child windows (inline edits, buttons), which would make them
    /// flash.</summary>
    /// <param name="Params">Window creation parameters.</param>
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure Paint; override;
    procedure Resize; override;
    /// <summary>Returns the bounds rectangle while no window handle
    /// exists, so layout code can read <c>ClientWidth</c> and
    /// <c>ClientHeight</c> before the control has a parent (for example
    /// in a constructor or while the IDE drops it on a form).</summary>
    function GetClientRect: TRect; override;
    /// <summary>Resolves the theme once the control has a parent, so
    /// controls created in code (and the parts of composite controls)
    /// follow the form's theme without an explicit
    /// <c>Theme</c>.</summary>
    procedure SetParent(AParent: TWinControl); override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    /// <summary>Skips background erasing. Paint always blits a fully
    /// opaque buffer, so erasing first (which, with csParentBackground,
    /// draws the parent straight onto the screen) only makes the control
    /// flash on every resize, realign or collapse.</summary>
    /// <param name="Message">The erase message; Result is set to 1.</param>
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WndProc(var Message: TMessage); override;

    /// <summary>True when the control should paint its realistic
    /// sample data instead of activity: at design time, or when
    /// <see cref="ForcePreview"/> is set.</summary>
    /// <returns>Preview flag.</returns>
    function IsPreview: Boolean;

    /// <summary>Unit system of the resolved theme; metric when no
    /// theme is bound.</summary>
    /// <returns>Effective unit system.</returns>
    function UnitSystem: TERDUnitSystem;

    /// <summary>Resolves an alert level to a palette colour.</summary>
    /// <param name="ALevel">Alert level.</param>
    /// <param name="ANormal">Colour returned for
    /// <c>alvNormal</c>.</param>
    /// <returns>Success / warning / danger colour.</returns>
    function AlertColor(ALevel: TERDAlertLevel; ANormal: TColor): TColor;

    /// <summary>Sizes for the effective <see cref="Density"/>, in
    /// 96-DPI logical pixels (pass them through
    /// <see cref="ScaleValue"/>).</summary>
    /// <returns>Density metrics.</returns>
    function Metrics: TERDDensityMetrics;

    function GetFocusCuesVisible: Boolean;
    property FocusCuesVisible: Boolean read GetFocusCuesVisible;

  public
    /// <summary>Resolved theme, including owner inheritance and the registered default.</summary>
    property EffectiveTheme: TERDTheme read FResolvedTheme;
    /// <summary>Effective density from the ERD owner or theme while
    /// ParentDensity is True. Setting Density disables inheritance.
    /// Public so composite controls can configure ERD parts through
    /// a TERDCustomControl reference.</summary>
    property Density: TERDDensity read GetDensity write SetDensity
      stored IsDensityStored;
    /// <summary>Follow the ERD owner or theme density instead of a
    /// local override.</summary>
    property ParentDensity: Boolean read FParentDensity
      write SetParentDensity default True;
    /// <summary>Creates the control with double-buffering enabled.
    /// </summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Detaches from the theme and frees the paint buffer.
    /// </summary>
    destructor Destroy; override;
    /// <summary>IERDThemeAware: repaints with the new palette.</summary>
    procedure ThemeChanged; virtual; // IERDThemeAware
    /// <summary>IERDDensityAware: the effective density changed.
    /// Subclasses that size or place child controls by density
    /// override this and call <c>inherited</c>.</summary>
    procedure DensityChanged; virtual; // IERDDensityAware
    /// <summary>Re-applies AutoSize. Public so parent panels can
    /// size child controls immediately after changing a caption,
    /// before placing them.</summary>
    procedure AdjustSize; override;
    /// <summary>Force a repaint at the next idle cycle (the
    /// double-buffer is invalidated; <c>Paint</c> redraws on
    /// next WM_PAINT).</summary>
    procedure Repaint; override;
    /// <summary>Marks the paint buffer dirty and schedules a repaint.
    /// Every property setter may call this safely.</summary>
    procedure Invalidate; override;

    /// <summary>Paints the control into <c>ABitmap</c> at the
    /// control's current size, without a window handle.</summary>
    /// <param name="ABitmap">Target bitmap. Resized to the control
    /// and switched to 32-bit pixels.</param>
    /// <remarks>Used by rendering tests and by hosts that print or
    /// export a dashboard snapshot (e.g. a before/after report).
    /// </remarks>
    procedure RenderTo(ABitmap: TBitmap);

    /// <summary>Hit-tests a point for dashboard edit mode.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Which edit handle the point is over.</returns>
    function EditHitTest(X, Y: Integer): TERDTileHit;
    /// <summary>Sets the hover and pressed edit handles painted by the
    /// dashboard edit overlay (called by the tile host).</summary>
    /// <param name="AHot">Handle under the mouse.</param>
    /// <param name="ADown">Handle being pressed or dragged.</param>
    procedure SetEditState(AHot, ADown: TERDTileHit);
    /// <summary>Edit handle under the mouse.</summary>
    property EditHot: TERDTileHit read FEditHot;
    /// <summary>Edit handle being pressed or dragged.</summary>
    property EditDown: TERDTileHit read FEditDown;

    /// <summary>Writes the control's user-facing settings (caption,
    /// channel, range, thresholds, ...) to a JSON object. Used by
    /// <c>TERDDashboard</c> to save layouts.</summary>
    /// <param name="AObject">Object to add pairs to. Not owned.
    /// </param>
    procedure SaveSettings(AObject: TJSONObject); virtual;

    /// <summary>Restores settings written by
    /// <see cref="SaveSettings"/>. Missing keys keep their current
    /// value.</summary>
    /// <param name="AObject">Object to read from. Not owned.</param>
    procedure LoadSettings(AObject: TJSONObject); virtual;

    /// <summary>Binds the control's channel(s) to a data source
    /// component (currently a <c>TERDLiveData</c>). The dashboard
    /// calls this for every tile it creates. Default: no-op.</summary>
    /// <param name="ASource">Data source, or nil to unbind.</param>
    procedure AssignDataSource(ASource: TComponent); virtual;

    /// <summary>Paint the design-time sample data at run time.
    /// Handy for screenshots and rendering tests.</summary>
    property ForcePreview: Boolean read FForcePreview write SetForcePreview;
  published
    /// <summary>Optional explicit theme. nil = auto-find on
    /// Owner ancestry.</summary>
    property Theme: TERDTheme read FTheme write SetTheme;
    /// <summary>Per-component colour overrides. Leave slots at
    /// <c>clDefault</c> to inherit from Theme.</summary>
    property StyleBackground: TColor read GetStyleBackground
      write SetStyleBackground default clDefault;
    property StyleForeground: TColor read GetStyleForeground
      write SetStyleForeground default clDefault;
    property StyleAccent: TColor read GetStyleAccent write SetStyleAccent
      default clDefault;
    property StyleBorder: TColor read GetStyleBorder write SetStyleBorder
      default clDefault;

    // Re-publish the bits a host typically wants.
    property Align;
    property AlignWithMargins;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property Margins;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint default False;
    property PopupMenu;
    property ShowHint default True;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  /// <summary>Lightweight non-windowed base. Use for small,
  /// non-focusable visuals (lamps, badges, sparklines). Owns
  /// no Win32 HWND so it composes transparently inside any
  /// other windowed control.</summary>
  TERDGraphicControl = class(TGraphicControl, IERDThemeAware)
  strict private
    FTheme: TERDTheme;
    FStyle: TERDVisualStyle;
    FResolvedTheme: TERDTheme;
    FDesignPPI: Integer;
    procedure SetTheme(AValue: TERDTheme);
    procedure ResolveTheme;
    procedure DetachFromTheme;
    function GetStyleBackground: TColor;
    function GetStyleForeground: TColor;
    function GetStyleAccent: TColor;
    function GetStyleBorder: TColor;
    procedure SetStyleBackground(AValue: TColor);
    procedure SetStyleForeground(AValue: TColor);
    procedure SetStyleAccent(AValue: TColor);
    procedure SetStyleBorder(AValue: TColor);
  protected
    procedure PaintControl(ACanvas: TCanvas); virtual; abstract;
    function Palette: TERDThemePalette;
    function ScaleValue(N: Integer): Integer; inline;
    function EffectiveBackground: TColor;
    function EffectiveForeground: TColor;
    function EffectiveAccent: TColor;
    function EffectiveBorder: TColor;

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged;
  published
    property Theme: TERDTheme read FTheme write SetTheme;
    property StyleBackground: TColor read GetStyleBackground
      write SetStyleBackground default clDefault;
    property StyleForeground: TColor read GetStyleForeground
      write SetStyleForeground default clDefault;
    property StyleAccent: TColor read GetStyleAccent write SetStyleAccent
      default clDefault;
    property StyleBorder: TColor read GetStyleBorder write SetStyleBorder
      default clDefault;

    property Align;
    property AlignWithMargins;
    property Anchors;
    property Enabled;
    property Hint;
    property Margins;
    property ParentShowHint default False;
    property ShowHint default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;


/// <summary>Reads a number from a settings object.</summary>
/// <param name="AObject">Object to read from; nil is allowed.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Receives the number when present.</param>
/// <returns>True when the pair exists and is a number.</returns>
function ERDJsonReadFloat(AObject: TJSONObject; const AKey: string;
  var AValue: Double): Boolean;

/// <summary>Reads an integer from a settings object.</summary>
/// <param name="AObject">Object to read from; nil is allowed.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Receives the integer when present.</param>
/// <returns>True when the pair exists and is a number.</returns>
function ERDJsonReadInt(AObject: TJSONObject; const AKey: string;
  var AValue: Integer): Boolean;

/// <summary>Reads a string from a settings object.</summary>
/// <param name="AObject">Object to read from; nil is allowed.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Receives the string when present.</param>
/// <returns>True when the pair exists and is a string.</returns>
function ERDJsonReadStr(AObject: TJSONObject; const AKey: string;
  var AValue: string): Boolean;

/// <summary>Reads a boolean from a settings object.</summary>
/// <param name="AObject">Object to read from; nil is allowed.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Receives the boolean when present.</param>
/// <returns>True when the pair exists and is true or false.</returns>
function ERDJsonReadBool(AObject: TJSONObject; const AKey: string;
  var AValue: Boolean): Boolean;

/// <summary>Adds a boolean pair (<c>true</c> / <c>false</c>).</summary>
/// <param name="AObject">Target object.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Value to write.</param>
procedure ERDJsonWriteBool(AObject: TJSONObject; const AKey: string;
  AValue: Boolean);

implementation

uses
  System.Types,
  ERD.Components.Paint,
  ERD.Components.Hint;

const
  DESIGN_PPI = 96;

  /// <summary>Paints a designer-only fallback when a subclass'
  /// <c>PaintControl</c> raises during csDesigning. Dashed rect
  /// with the class name + the exception message so the host
  /// sees something useful in the IDE form designer instead of
  /// a black hole.</summary>
procedure DrawDesignPlaceholder(ACanvas: TCanvas);
var
  E: Exception;
  Msg: string;
  R: TRect;
begin
  R := ACanvas.ClipRect;
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(R);
  ACanvas.Pen.Style := psDash;
  ACanvas.Pen.Color := clGrayText;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(R);
  ACanvas.Font.Color := clGrayText;
  E := Exception(ExceptObject);
  if Assigned(E) then
    Msg := '(design-time: ' + E.Message + ')'
  else
    Msg := '(design-time placeholder)';
  ACanvas.TextOut(R.Left + 4, R.Top + 4, Msg);
end;

function ERDJsonReadFloat(AObject: TJSONObject; const AKey: string;
  var AValue: Double): Boolean;
var
  V: TJSONValue;
begin
  Result := False;
  if AObject = nil then
    Exit;
  V := AObject.Values[AKey];
  if V is TJSONNumber then
  begin
    AValue := TJSONNumber(V).AsDouble;
    Result := True;
  end;
end;

function ERDJsonReadInt(AObject: TJSONObject; const AKey: string;
  var AValue: Integer): Boolean;
var
  D: Double;
begin
  D := 0;
  Result := ERDJsonReadFloat(AObject, AKey, D);
  if Result then
    AValue := Round(D);
end;

function ERDJsonReadStr(AObject: TJSONObject; const AKey: string;
  var AValue: string): Boolean;
var
  V: TJSONValue;
begin
  Result := False;
  if AObject = nil then
    Exit;
  V := AObject.Values[AKey];
  if V is TJSONString then
  begin
    AValue := TJSONString(V).Value;
    Result := True;
  end;
end;

function ERDJsonReadBool(AObject: TJSONObject; const AKey: string;
  var AValue: Boolean): Boolean;
var
  V: TJSONValue;
begin
  Result := False;
  if AObject = nil then
    Exit;
  V := AObject.Values[AKey];
  if V is TJSONTrue then
  begin
    AValue := True;
    Result := True;
  end
  else if V is TJSONFalse then
  begin
    AValue := False;
    Result := True;
  end;
end;

procedure ERDJsonWriteBool(AObject: TJSONObject; const AKey: string;
  AValue: Boolean);
begin
  if AValue then
    AObject.AddPair(AKey, TJSONTrue.Create)
  else
    AObject.AddPair(AKey, TJSONFalse.Create);
end;

{ ---- TERDCustomControl ----------------------------------------------------- }

procedure TERDCustomControl.CMHintShow(var Message: TCMHintShow);
begin
  inherited;
  if Message.HintInfo <> nil then Message.HintInfo.HintWindowClass := TERDHintWindow;
end;

constructor TERDCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShowHint := True;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csParentBackground];
  DoubleBuffered := True;
  FStyle.Reset;
  FDesignPPI := DESIGN_PPI;
  FBuffer := TBitmap.Create;
  FBufferDirty := True;
  FDensity := dnDesktop;
  FParentDensity := True;
end;

destructor TERDCustomControl.Destroy;
begin
  DetachFromTheme;
  FBuffer.Free;
  inherited;
end;

procedure TERDCustomControl.SetTheme(AValue: TERDTheme);
begin
  if FTheme = AValue then
    Exit;
  if FResolvedTheme <> nil then
    FResolvedTheme.Detach(Self);
  if FTheme <> nil then
    FTheme.RemoveFreeNotification(Self);
  FTheme := AValue;
  if FTheme <> nil then
    FTheme.FreeNotification(Self);
  ResolveTheme;
  if FParentDensity and not (csLoading in ComponentState) then
    DensityChanged;
  Invalidate;
end;

function TERDCustomControl.GetParentDensity: Boolean;
begin Result := FParentDensity; end;

function TERDCustomControl.GetDensity: TERDDensity;
var Source: IERDDensitySource;
begin
  if FParentDensity and Supports(Owner, IERDDensitySource, Source) then
    Result := Source.GetDensity
  else if FParentDensity and (FResolvedTheme <> nil) then
    Result := FResolvedTheme.Density
  else Result := FDensity;
end;

procedure TERDCustomControl.SetDensity(AValue: TERDDensity);
begin
  if (FDensity = AValue) and not FParentDensity then
    Exit;
  FDensity := AValue;
  FParentDensity := False;
  DensityChanged;
end;

procedure TERDCustomControl.SetParentDensity(AValue: Boolean);
begin
  if FParentDensity = AValue then
    Exit;
  FParentDensity := AValue;
  DensityChanged;
end;

function TERDCustomControl.IsDensityStored: Boolean;
begin
  Result := not FParentDensity;
end;

function TERDCustomControl.Metrics: TERDDensityMetrics;
begin
  Result := DensityMetrics(GetDensity);
end;

procedure TERDCustomControl.DensityChanged;
var I: Integer; Part: TComponent; Source: IERDDensitySource; Aware: IERDDensityAware;
begin
  // Include ERD engine adapters, not only descendants of this concrete class.
  for I := 0 to ComponentCount - 1 do
  begin
    Part := Components[I];
    if not (csDestroying in Part.ComponentState) and
      Supports(Part, IERDDensitySource, Source) and Source.GetParentDensity and
      Supports(Part, IERDDensityAware, Aware) then Aware.DensityChanged;
  end;
  FBufferDirty := True;
  Invalidate;
end;

procedure TERDCustomControl.ResolveTheme;
var
  Candidate, Previous: TERDTheme;
  Part: TERDCustomControl;
  I: Integer;
begin
  // Detach from any previously-resolved theme.
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;

  // Explicit Theme wins. Auto-find on Owner ancestry next.
  // Process-wide default last. nil = use VCL Style + brand.
  // The parts of a composite control use the composite's theme.
  Candidate := FTheme;
  if (Candidate = nil) and (Owner is TERDCustomControl) then
    Candidate := TERDCustomControl(Owner).FResolvedTheme;
  if Candidate = nil then
    Candidate := TERDTheme.FindOnOwner(Self);
  if Candidate = nil then
    Candidate := TERDTheme.GetDefault;
  FResolvedTheme := Candidate;
  if FResolvedTheme <> nil then
    FResolvedTheme.Attach(Self);
  // Re-resolve owned parts: they may have been parented before the
  // theme was streamed in.
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TERDCustomControl) and
      not (csDestroying in Components[I].ComponentState) then
    begin
      Part := TERDCustomControl(Components[I]);
      Previous := Part.FResolvedTheme;
      Part.ResolveTheme;
      if Part.FResolvedTheme <> Previous then
        Part.ThemeChanged;
    end;
end;

procedure TERDCustomControl.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent <> nil) and (FResolvedTheme = nil) and
    (ComponentState * [csLoading, csDestroying] = []) then
    ResolveTheme;
end;

procedure TERDCustomControl.DetachFromTheme;
begin
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;
end;

procedure TERDCustomControl.Loaded;
begin
  inherited;
  ResolveTheme;
  DensityChanged;
end;

procedure TERDCustomControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FTheme then
      FTheme := nil;
    if AComponent = FResolvedTheme then
      FResolvedTheme := nil;
  end;
end;

function TERDCustomControl.Palette: TERDThemePalette;
begin
  if FResolvedTheme <> nil then
    Exit(FResolvedTheme.Palette);
  // No theme — synthesize one from VCL Style + brand default.
  Result := BRAND_PALETTE_LIGHT;
end;

function TERDCustomControl.ScaleValue(N: Integer): Integer;
begin
  // CurrentPPI is the modern Delphi (10.4+) per-control DPI. In
  // older Delphi we fall back to Screen.PixelsPerInch which is
  // the form's design-time PPI.
{$IF CompilerVersion >= 35}
  Result := MulDiv(N, Self.CurrentPPI, FDesignPPI);
{$ELSE}
  Result := MulDiv(N, Self.Font.PixelsPerInch, FDesignPPI);
{$ENDIF}
end;

function TERDCustomControl.EffectiveBackground: TColor;
var
  Surface: IERDSurface;
begin
  if (FStyle.Background = clDefault) and (Parent <> nil) and
    Supports(Parent, IERDSurface, Surface) then
    Exit(Surface.SurfaceColor);
  Result := PickColor(FStyle.Background, Palette.Background);
end;

function TERDCustomControl.EffectiveForeground: TColor;
begin
  Result := PickColor(FStyle.Foreground, Palette.ForegroundText);
end;

function TERDCustomControl.EffectiveAccent: TColor;
begin
  Result := PickColor(FStyle.Accent, Palette.Accent);
end;

function TERDCustomControl.EffectiveBorder: TColor;
begin
  Result := PickColor(FStyle.Border, Palette.Subtle);
end;

procedure TERDCustomControl.Invalidate;
begin
  FBufferDirty := True;
  inherited;
end;

procedure TERDCustomControl.SetForcePreview(AValue: Boolean);
begin
  if FForcePreview = AValue then
    Exit;
  FForcePreview := AValue;
  Invalidate;
end;

function TERDCustomControl.IsPreview: Boolean;
begin
  Result := FForcePreview or (csDesigning in ComponentState);
end;

function TERDCustomControl.UnitSystem: TERDUnitSystem;
begin
  if FResolvedTheme <> nil then
    Result := FResolvedTheme.UnitSystem
  else
    Result := usMetric;
end;

function TERDCustomControl.AlertColor(ALevel: TERDAlertLevel;
  ANormal: TColor): TColor;
begin
  case ALevel of
    alvWarning:
      Result := Palette.Warning;
    alvAlarm:
      Result := Palette.Danger;
  else
    Result := ANormal;
  end;
end;

procedure TERDCustomControl.RenderTo(ABitmap: TBitmap);
begin
  ABitmap.PixelFormat := pf32bit;
  ABitmap.SetSize(Width, Height);
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.Brush.Color := EffectiveBackground;
  ABitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
  if (Width > 0) and (Height > 0) then
    PaintControl(ABitmap.Canvas);
end;

procedure TERDCustomControl.SaveSettings(AObject: TJSONObject);
begin
  // Base controls have no persisted settings.
end;

procedure TERDCustomControl.LoadSettings(AObject: TJSONObject);
begin
  // Base controls have no persisted settings.
end;

procedure TERDCustomControl.AssignDataSource(ASource: TComponent);
begin
  // Controls without a data channel ignore the source.
end;

function TERDCustomControl.TileHost: IERDTileHost;
begin
  Result := nil;
  if (Parent <> nil) and not(csDesigning in ComponentState) then
    if not Supports(Parent, IERDTileHost, Result) then
      Result := nil;
end;

function TERDCustomControl.EditCloseRect: TRect;
var
  M, D: Integer;
begin
  // Round close button inset from the top-right corner; larger touch
  // target in tablet density.
  M := ScaleValue(6);
  if Density = dnTablet then
    D := ScaleValue(30)
  else
    D := ScaleValue(22);
  Result := Rect(Width - M - D, M, Width - M, M + D);
end;

function TERDCustomControl.EditGripSize: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(28)
  else
    Result := ScaleValue(20);
end;

function TERDCustomControl.EditHitTest(X, Y: Integer): TERDTileHit;
var
  R: TRect;
  Grip: Integer;
begin
  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) then
    Exit(thNone);
  R := EditCloseRect;
  InflateRect(R, ScaleValue(3), ScaleValue(3));
  Grip := EditGripSize;
  if (X >= R.Left) and (X < R.Right) and (Y >= R.Top) and (Y < R.Bottom) then
    Result := thClose
  else if (X >= Width - Grip) and (Y >= Height - Grip) then
    Result := thResize
  else
    Result := thMove;
end;

procedure TERDCustomControl.SetEditState(AHot, ADown: TERDTileHit);
begin
  if (FEditHot = AHot) and (FEditDown = ADown) then
    Exit;
  FEditHot := AHot;
  FEditDown := ADown;
  Invalidate;
end;

procedure TERDCustomControl.DrawEditOverlay(ACanvas: TCanvas);
var
  P: TERDPainter;
  R: TRect;
  LW, CX, CY, K, Grip: Single;
  Fill, Outline, Ink: TColor;
  Pressed: Boolean;
  I: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    // Tile frame: thin dashed accent outline on the hovered or active
    // (dragged / resized) tile only; the other tiles keep just their
    // close button and resize grip.
    if (FEditDown <> thNone) or (FEditHot <> thNone) then
    begin
      if FEditDown in [thMove, thResize] then
        LW := P.SF(1.5)
      else
        LW := P.SF(1.25);
      P.DashedRoundRect(0, 0, Width, Height, P.SF(4), EffectiveAccent, LW);
    end;

    // Close button: soft danger tint at rest, solid danger on hover,
    // darker while pressed.
    R := EditCloseRect;
    Pressed := (FEditDown = thClose) and (FEditHot = thClose);
    if Pressed then
    begin
      Fill := ERDMixColor(Palette.Danger, clBlack, 0.8);
      Outline := clNone;
      Ink := P.ContrastInk(Palette.Danger);
    end
    else if FEditHot = thClose then
    begin
      Fill := Palette.Danger;
      Outline := clNone;
      Ink := P.ContrastInk(Fill);
    end
    else
    begin
      Fill := P.Tint(Palette.Danger, 0.12);
      Outline := P.Tint(Palette.Danger, 0.45);
      Ink := P.DangerInk;
    end;
    P.Ellipse(R.Left, R.Top, R.Width, R.Height, Fill, Outline, P.SF(1));
    CX := R.Left + R.Width / 2;
    CY := R.Top + R.Height / 2;
    K := R.Width * 0.18;
    P.Lines([ERDPointF(CX - K, CY - K), ERDPointF(CX + K, CY + K)], Ink,
      P.SF(1.75));
    P.Lines([ERDPointF(CX + K, CY - K), ERDPointF(CX - K, CY + K)], Ink,
      P.SF(1.75));

    // Resize grip: three diagonal strokes, bottom-right, on an accent
    // tint while hovered or dragged.
    Grip := EditGripSize;
    if (FEditHot = thResize) or (FEditDown = thResize) then
    begin
      P.RoundRect(Width - Grip - P.SF(2), Height - Grip - P.SF(2), Grip,
        Grip, P.SF(4), P.Tint(EffectiveAccent, 0.2), clNone);
      Ink := P.AccentText;
    end
    else
      Ink := EffectiveAccent;
    for I := 1 to 3 do
      P.Lines([ERDPointF(Width - P.SF(4) - I * Grip * 0.22, Height - P.SF(4)),
        ERDPointF(Width - P.SF(4), Height - P.SF(4) - I * Grip * 0.22)], Ink,
        P.SF(1.5));
  finally
    P.Free;
  end;
end;

procedure TERDCustomControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TERDCustomControl.GetFocusCuesVisible: Boolean;
var AlwaysShow: BOOL;
begin
  AlwaysShow := False;
  SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @AlwaysShow, 0);
  Result := AlwaysShow or (HandleAllocated and
    ((SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0));
end;

procedure TERDCustomControl.WndProc(var Message: TMessage);
var
  Host: IERDTileHost;
  UIRoot: HWND;
  UIAction: Integer;
begin
  // The tile host takes every mouse message in edit mode, so the VCL
  // never arms its own leave tracking: the host arms TrackMouseEvent and
  // WM_MOUSELEAVE (outside the WM_MOUSEFIRST..LAST range) clears hover.
  if ((Message.Msg = CM_MOUSELEAVE) or (Message.Msg = WM_MOUSELEAVE)) and
    (FEditHot <> thNone) and (GetCapture <> WindowHandle) then
    SetEditState(thNone, FEditDown);
  if (Message.Msg >= WM_MOUSEFIRST) and (Message.Msg <= WM_MOUSELAST) then
  begin
    Host := TileHost;
    if (Host <> nil) and Host.IsEditingTiles then
    begin
      Host.TileMouseMessage(Self, Message);
      Exit;
    end;
  end;
  UIAction := 0;
  if Message.Msg = WM_LBUTTONDOWN then UIAction := UIS_SET
  else if (Message.Msg = WM_KEYDOWN) and
    (Message.WParam in [VK_TAB, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
      VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_SPACE, VK_RETURN]) then
    UIAction := UIS_CLEAR;
  if (UIAction <> 0) and HandleAllocated then
  begin
    UIRoot := GetAncestor(Handle, GA_ROOT);
    if UIRoot = 0 then UIRoot := Handle;
    SendMessage(UIRoot, WM_CHANGEUISTATE,
      WPARAM(UIAction or (UISF_HIDEFOCUS shl 16)), 0);
  end;
  inherited WndProc(Message);
  if (Message.Msg = WM_SETFOCUS) or (Message.Msg = WM_KILLFOCUS) or
    (Message.Msg = CM_ENTER) or (Message.Msg = CM_EXIT) or
    (Message.Msg = WM_UPDATEUISTATE) or (Message.Msg = WM_SETTINGCHANGE) then
    Invalidate;
end;

procedure TERDCustomControl.Paint;
var
  Host: IERDTileHost;
begin
  if (Width <= 0) or (Height <= 0) then
    Exit;
  if FBufferDirty or (FBuffer.Width <> Width) or (FBuffer.Height <> Height) then
  begin
    FBuffer.SetSize(Width, Height);
    // Fill the buffer with the resolved background so subclass
    // paint can punch through with translucency where it wants.
    FBuffer.Canvas.Brush.Color := EffectiveBackground;
    FBuffer.Canvas.FillRect(Rect(0, 0, Width, Height));
    // Swallow paint exceptions at design-time so a half-wired
    // component (no bound LiveData yet, etc.) doesn't tear down
    // the IDE Designer. At run-time any paint exception
    // propagates so it can be debugged.
    if csDesigning in ComponentState then
      try
        PaintControl(FBuffer.Canvas);
      except
        DrawDesignPlaceholder(FBuffer.Canvas);
      end
    else
      PaintControl(FBuffer.Canvas);
    Host := TileHost;
    if (Host <> nil) and Host.IsEditingTiles then
      DrawEditOverlay(FBuffer.Canvas);
    FBufferDirty := False;
  end;
  Canvas.Draw(0, 0, FBuffer);
end;

procedure TERDCustomControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TERDCustomControl.Repaint;
begin
  FBufferDirty := True;
  inherited;
end;

procedure TERDCustomControl.AdjustSize;
begin
  inherited AdjustSize;
end;

procedure TERDCustomControl.Resize;
begin
  inherited;
  FBufferDirty := True;
end;

function TERDCustomControl.GetClientRect: TRect;
begin
  if HandleAllocated then
    Result := inherited GetClientRect
  else
    Result := Rect(0, 0, Width, Height);
end;

procedure TERDCustomControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  FBufferDirty := True;
  Invalidate;
end;

procedure TERDCustomControl.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  FBufferDirty := True;
  Invalidate;
end;

procedure TERDCustomControl.ThemeChanged;
begin
  FBufferDirty := True;
  Invalidate;
end;

function TERDCustomControl.GetStyleBackground: TColor;
begin
  Result := FStyle.Background;
end;

function TERDCustomControl.GetStyleForeground: TColor;
begin
  Result := FStyle.Foreground;
end;

function TERDCustomControl.GetStyleAccent: TColor;
begin
  Result := FStyle.Accent;
end;

function TERDCustomControl.GetStyleBorder: TColor;
begin
  Result := FStyle.Border;
end;

procedure TERDCustomControl.SetStyleBackground(AValue: TColor);
begin
  if FStyle.Background = AValue then
    Exit;
  FStyle.Background := AValue;
  FBufferDirty := True;
  Invalidate;
end;

procedure TERDCustomControl.SetStyleForeground(AValue: TColor);
begin
  if FStyle.Foreground = AValue then
    Exit;
  FStyle.Foreground := AValue;
  FBufferDirty := True;
  Invalidate;
end;

procedure TERDCustomControl.SetStyleAccent(AValue: TColor);
begin
  if FStyle.Accent = AValue then
    Exit;
  FStyle.Accent := AValue;
  FBufferDirty := True;
  Invalidate;
end;

procedure TERDCustomControl.SetStyleBorder(AValue: TColor);
begin
  if FStyle.Border = AValue then
    Exit;
  FStyle.Border := AValue;
  FBufferDirty := True;
  Invalidate;
end;

{ ---- TERDGraphicControl ---------------------------------------------------- }

procedure TERDGraphicControl.CMHintShow(var Message: TCMHintShow);
begin
  inherited;
  if Message.HintInfo <> nil then Message.HintInfo.HintWindowClass := TERDHintWindow;
end;

constructor TERDGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShowHint := True;
  FStyle.Reset;
  FDesignPPI := DESIGN_PPI;
end;

destructor TERDGraphicControl.Destroy;
begin
  DetachFromTheme;
  inherited;
end;

procedure TERDGraphicControl.SetTheme(AValue: TERDTheme);
begin
  if FTheme = AValue then
    Exit;
  if FResolvedTheme <> nil then
    FResolvedTheme.Detach(Self);
  if FTheme <> nil then
    FTheme.RemoveFreeNotification(Self);
  FTheme := AValue;
  if FTheme <> nil then
    FTheme.FreeNotification(Self);
  ResolveTheme;
  Invalidate;
end;

procedure TERDGraphicControl.ResolveTheme;
var
  Candidate: TERDTheme;
begin
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;
  Candidate := FTheme;
  if Candidate = nil then
    Candidate := TERDTheme.FindOnOwner(Self);
  if Candidate = nil then
    Candidate := TERDTheme.GetDefault;
  FResolvedTheme := Candidate;
  if FResolvedTheme <> nil then
    FResolvedTheme.Attach(Self);
end;

procedure TERDGraphicControl.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent <> nil) and (FResolvedTheme = nil) and
    (ComponentState * [csLoading, csDestroying] = []) then
    ResolveTheme;
end;

procedure TERDGraphicControl.DetachFromTheme;
begin
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;
end;

procedure TERDGraphicControl.Loaded;
begin
  inherited;
  ResolveTheme;
end;

procedure TERDGraphicControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FTheme then
      FTheme := nil;
    if AComponent = FResolvedTheme then
      FResolvedTheme := nil;
  end;
end;

function TERDGraphicControl.Palette: TERDThemePalette;
begin
  if FResolvedTheme <> nil then
    Exit(FResolvedTheme.Palette);
  Result := BRAND_PALETTE_LIGHT;
end;

function TERDGraphicControl.ScaleValue(N: Integer): Integer;
begin
{$IF CompilerVersion >= 35}
  Result := MulDiv(N, Self.CurrentPPI, FDesignPPI);
{$ELSE}
  Result := MulDiv(N, Self.Font.PixelsPerInch, FDesignPPI);
{$ENDIF}
end;

function TERDGraphicControl.EffectiveBackground: TColor;
begin
  Result := PickColor(FStyle.Background, Palette.Background);
end;

function TERDGraphicControl.EffectiveForeground: TColor;
begin
  Result := PickColor(FStyle.Foreground, Palette.ForegroundText);
end;

function TERDGraphicControl.EffectiveAccent: TColor;
begin
  Result := PickColor(FStyle.Accent, Palette.Accent);
end;

function TERDGraphicControl.EffectiveBorder: TColor;
begin
  Result := PickColor(FStyle.Border, Palette.Subtle);
end;

procedure TERDGraphicControl.Paint;
begin
  if (Width <= 0) or (Height <= 0) then
    Exit;
  if csDesigning in ComponentState then
    try
      PaintControl(Canvas);
    except
      DrawDesignPlaceholder(Canvas);
    end
  else
    PaintControl(Canvas);
end;

procedure TERDGraphicControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  Invalidate;
end;

procedure TERDGraphicControl.ThemeChanged;
begin
  Invalidate;
end;

function TERDGraphicControl.GetStyleBackground: TColor;
begin
  Result := FStyle.Background;
end;

function TERDGraphicControl.GetStyleForeground: TColor;
begin
  Result := FStyle.Foreground;
end;

function TERDGraphicControl.GetStyleAccent: TColor;
begin
  Result := FStyle.Accent;
end;

function TERDGraphicControl.GetStyleBorder: TColor;
begin
  Result := FStyle.Border;
end;

procedure TERDGraphicControl.SetStyleBackground(AValue: TColor);
begin
  if FStyle.Background <> AValue then
  begin
    FStyle.Background := AValue;
    Invalidate;
  end;
end;

procedure TERDGraphicControl.SetStyleForeground(AValue: TColor);
begin
  if FStyle.Foreground <> AValue then
  begin
    FStyle.Foreground := AValue;
    Invalidate;
  end;
end;

procedure TERDGraphicControl.SetStyleAccent(AValue: TColor);
begin
  if FStyle.Accent <> AValue then
  begin
    FStyle.Accent := AValue;
    Invalidate;
  end;
end;

procedure TERDGraphicControl.SetStyleBorder(AValue: TColor);
begin
  if FStyle.Border <> AValue then
  begin
    FStyle.Border := AValue;
    Invalidate;
  end;
end;

end.
