// ------------------------------------------------------------------------------
// ERD.UI.Control
//
// Custom-paint base classes every visual derives from.
//
// TOBDCustomControl    windowed control (focusable, accepts
// keyboard input). Use for lists,
// interactive widgets, anything that
// needs Tab focus.
// TOBDGraphicControl   lightweight non-windowed control.
// Use for indicator lamps, badges,
// sparklines — anything that can sit
// inside another control's client area
// transparently.
//
// Both bases satisfy the universal quality bar:
// - Theme-aware (auto-bind via TOBDTheme.FindOnOwner,
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
// - Dashboard-ready (IOBDTileHost lets a parent dashboard
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
//                  settings persistence.
// ------------------------------------------------------------------------------

unit ERD.UI.Control;

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
  ERD.UI.Types,
  ERD.UI.Theme;

type
  /// <summary>Hit-test result for a tile in dashboard edit mode.
  /// </summary>
  TOBDTileHit = (
    /// <summary>Outside the control.</summary>
    thNone,
    /// <summary>Body of the tile: drag to move.</summary>
    thMove,
    /// <summary>Bottom-right grip: drag to resize.</summary>
    thResize,
    /// <summary>Top-right close glyph: click to remove.</summary>
    thClose);

  /// <summary>Implemented by containers (the dashboard) that lay out
  /// Delphi-OBD controls as tiles and can put them into an edit mode
  /// where the mechanic moves, resizes and removes tiles.</summary>
  /// <remarks>While <c>IsEditingTiles</c> returns True, every mouse
  /// message a child <see cref="TOBDCustomControl"/> receives is
  /// forwarded to <c>TileMouseMessage</c> instead of the child's own
  /// handlers, and the child paints an edit overlay.</remarks>
  IOBDTileHost = interface
    ['{6F3A8C2E-1D47-4B9A-9E52-7C0D3B8F4A61}']
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

  /// <summary>Base for windowed visuals (focusable, can host
  /// keyboard input). Subclasses override <c>PaintControl</c>
  /// to draw onto the supplied <c>TCanvas</c>; the base class
  /// handles double-buffering, theme resolution, and DPI
  /// scaling.</summary>
  TOBDCustomControl = class(TCustomControl, IOBDThemeAware)
  strict private
    FTheme: TOBDTheme;
    FStyle: TOBDVisualStyle;
    FResolvedTheme: TOBDTheme;
    FBuffer: TBitmap;
    FBufferDirty: Boolean;
    FDesignPPI: Integer;
    FForcePreview: Boolean;
    procedure SetTheme(AValue: TOBDTheme);
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
    function TileHost: IOBDTileHost;
    procedure DrawEditOverlay(ACanvas: TCanvas);
  protected
    /// <summary>Override and paint onto <c>ACanvas</c>. Bounds
    /// are <c>ClientRect</c>. Theme palette already resolved
    /// for you via <see cref="Palette"/>.</summary>
    procedure PaintControl(ACanvas: TCanvas); virtual; abstract;

    /// <summary>Currently-resolved palette. Walks the chain:
    /// explicit Theme ▸ auto-found owner Theme ▸ default Theme
    /// ▸ VCL Style ▸ brand default.</summary>
    function Palette: TOBDThemePalette;

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

    procedure Paint; override;
    procedure Resize; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure WndProc(var Message: TMessage); override;

    /// <summary>True when the control should paint its realistic
    /// sample data instead of live data: at design time, or when
    /// <see cref="ForcePreview"/> is set.</summary>
    /// <returns>Preview flag.</returns>
    function IsPreview: Boolean;

    /// <summary>Unit system of the resolved theme; metric when no
    /// theme is bound.</summary>
    /// <returns>Effective unit system.</returns>
    function UnitSystem: TOBDUnitSystem;

    /// <summary>Resolves an alert level to a palette colour.</summary>
    /// <param name="ALevel">Alert level.</param>
    /// <param name="ANormal">Colour returned for
    /// <c>alvNormal</c>.</param>
    /// <returns>Success / warning / danger colour.</returns>
    function AlertColor(ALevel: TOBDAlertLevel; ANormal: TColor): TColor;
  public
    /// <summary>Creates the control with double-buffering enabled.
    /// </summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Detaches from the theme and frees the paint buffer.
    /// </summary>
    destructor Destroy; override;
    /// <summary>IOBDThemeAware: repaints with the new palette.</summary>
    procedure ThemeChanged; // IOBDThemeAware
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
    function EditHitTest(X, Y: Integer): TOBDTileHit;

    /// <summary>Writes the control's user-facing settings (caption,
    /// channel, range, thresholds, ...) to a JSON object. Used by
    /// <c>TOBDDashboard</c> to save layouts.</summary>
    /// <param name="AObject">Object to add pairs to. Not owned.
    /// </param>
    procedure SaveSettings(AObject: TJSONObject); virtual;

    /// <summary>Restores settings written by
    /// <see cref="SaveSettings"/>. Missing keys keep their current
    /// value.</summary>
    /// <param name="AObject">Object to read from. Not owned.</param>
    procedure LoadSettings(AObject: TJSONObject); virtual;

    /// <summary>Binds the control's channel(s) to a data source
    /// component (currently a <c>TOBDLiveData</c>). The dashboard
    /// calls this for every tile it creates. Default: no-op.</summary>
    /// <param name="ASource">Data source, or nil to unbind.</param>
    procedure AssignDataSource(ASource: TComponent); virtual;

    /// <summary>Paint the design-time sample data at run time.
    /// Handy for screenshots and rendering tests.</summary>
    property ForcePreview: Boolean read FForcePreview write SetForcePreview;
  published
    /// <summary>Optional explicit theme. nil = auto-find on
    /// Owner ancestry.</summary>
    property Theme: TOBDTheme read FTheme write SetTheme;
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
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property Margins;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
  TOBDGraphicControl = class(TGraphicControl, IOBDThemeAware)
  strict private
    FTheme: TOBDTheme;
    FStyle: TOBDVisualStyle;
    FResolvedTheme: TOBDTheme;
    FDesignPPI: Integer;
    procedure SetTheme(AValue: TOBDTheme);
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
    function Palette: TOBDThemePalette;
    function ScaleValue(N: Integer): Integer; inline;
    function EffectiveBackground: TColor;
    function EffectiveForeground: TColor;
    function EffectiveAccent: TColor;
    function EffectiveBorder: TColor;

    procedure Paint; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged;
  published
    property Theme: TOBDTheme read FTheme write SetTheme;
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
    property ParentShowHint;
    property ShowHint;
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
function OBDJsonReadFloat(AObject: TJSONObject; const AKey: string;
  var AValue: Double): Boolean;

/// <summary>Reads an integer from a settings object.</summary>
/// <param name="AObject">Object to read from; nil is allowed.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Receives the integer when present.</param>
/// <returns>True when the pair exists and is a number.</returns>
function OBDJsonReadInt(AObject: TJSONObject; const AKey: string;
  var AValue: Integer): Boolean;

/// <summary>Reads a string from a settings object.</summary>
/// <param name="AObject">Object to read from; nil is allowed.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Receives the string when present.</param>
/// <returns>True when the pair exists and is a string.</returns>
function OBDJsonReadStr(AObject: TJSONObject; const AKey: string;
  var AValue: string): Boolean;

/// <summary>Reads a boolean from a settings object.</summary>
/// <param name="AObject">Object to read from; nil is allowed.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Receives the boolean when present.</param>
/// <returns>True when the pair exists and is true or false.</returns>
function OBDJsonReadBool(AObject: TJSONObject; const AKey: string;
  var AValue: Boolean): Boolean;

/// <summary>Adds a boolean pair (<c>true</c> / <c>false</c>).</summary>
/// <param name="AObject">Target object.</param>
/// <param name="AKey">Pair name.</param>
/// <param name="AValue">Value to write.</param>
procedure OBDJsonWriteBool(AObject: TJSONObject; const AKey: string;
  AValue: Boolean);

implementation

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

function OBDJsonReadFloat(AObject: TJSONObject; const AKey: string;
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

function OBDJsonReadInt(AObject: TJSONObject; const AKey: string;
  var AValue: Integer): Boolean;
var
  D: Double;
begin
  D := 0;
  Result := OBDJsonReadFloat(AObject, AKey, D);
  if Result then
    AValue := Round(D);
end;

function OBDJsonReadStr(AObject: TJSONObject; const AKey: string;
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

function OBDJsonReadBool(AObject: TJSONObject; const AKey: string;
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

procedure OBDJsonWriteBool(AObject: TJSONObject; const AKey: string;
  AValue: Boolean);
begin
  if AValue then
    AObject.AddPair(AKey, TJSONTrue.Create)
  else
    AObject.AddPair(AKey, TJSONFalse.Create);
end;

{ ---- TOBDCustomControl ----------------------------------------------------- }

constructor TOBDCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable, csParentBackground];
  DoubleBuffered := True;
  FStyle.Reset;
  FDesignPPI := DESIGN_PPI;
  FBuffer := TBitmap.Create;
  FBufferDirty := True;
end;

destructor TOBDCustomControl.Destroy;
begin
  DetachFromTheme;
  FBuffer.Free;
  inherited;
end;

procedure TOBDCustomControl.SetTheme(AValue: TOBDTheme);
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

procedure TOBDCustomControl.ResolveTheme;
var
  Candidate: TOBDTheme;
begin
  // Detach from any previously-resolved theme.
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;

  // Explicit Theme wins. Auto-find on Owner ancestry next.
  // Process-wide default last. nil = use VCL Style + brand.
  Candidate := FTheme;
  if Candidate = nil then
    Candidate := TOBDTheme.FindOnOwner(Self);
  if Candidate = nil then
    Candidate := TOBDTheme.GetDefault;
  FResolvedTheme := Candidate;
  if FResolvedTheme <> nil then
    FResolvedTheme.Attach(Self);
end;

procedure TOBDCustomControl.DetachFromTheme;
begin
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;
end;

procedure TOBDCustomControl.Loaded;
begin
  inherited;
  ResolveTheme;
end;

procedure TOBDCustomControl.Notification(AComponent: TComponent;
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

function TOBDCustomControl.Palette: TOBDThemePalette;
begin
  if FResolvedTheme <> nil then
    Exit(FResolvedTheme.Palette);
  // No theme — synthesize one from VCL Style + brand default.
  if VCLStyleIsDark then
    Result := BRAND_PALETTE_DARK
  else
    Result := BRAND_PALETTE_LIGHT;
  if TStyleManager.IsCustomStyleActive then
  begin
    Result.Background := StyleColor(scWindow, Result.Background);
    Result.ForegroundText := StyleServices.GetSystemColor(clWindowText);
  end;
end;

function TOBDCustomControl.ScaleValue(N: Integer): Integer;
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

function TOBDCustomControl.EffectiveBackground: TColor;
begin
  Result := PickColor(FStyle.Background, Palette.Background);
end;

function TOBDCustomControl.EffectiveForeground: TColor;
begin
  Result := PickColor(FStyle.Foreground, Palette.ForegroundText);
end;

function TOBDCustomControl.EffectiveAccent: TColor;
begin
  Result := PickColor(FStyle.Accent, Palette.Accent);
end;

function TOBDCustomControl.EffectiveBorder: TColor;
begin
  Result := PickColor(FStyle.Border, Palette.Subtle);
end;

procedure TOBDCustomControl.Invalidate;
begin
  FBufferDirty := True;
  inherited;
end;

procedure TOBDCustomControl.SetForcePreview(AValue: Boolean);
begin
  if FForcePreview = AValue then
    Exit;
  FForcePreview := AValue;
  Invalidate;
end;

function TOBDCustomControl.IsPreview: Boolean;
begin
  Result := FForcePreview or (csDesigning in ComponentState);
end;

function TOBDCustomControl.UnitSystem: TOBDUnitSystem;
begin
  if FResolvedTheme <> nil then
    Result := FResolvedTheme.UnitSystem
  else
    Result := usMetric;
end;

function TOBDCustomControl.AlertColor(ALevel: TOBDAlertLevel;
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

procedure TOBDCustomControl.RenderTo(ABitmap: TBitmap);
begin
  ABitmap.PixelFormat := pf32bit;
  ABitmap.SetSize(Width, Height);
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.Brush.Color := EffectiveBackground;
  ABitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
  if (Width > 0) and (Height > 0) then
    PaintControl(ABitmap.Canvas);
end;

procedure TOBDCustomControl.SaveSettings(AObject: TJSONObject);
begin
  // Base controls have no persisted settings.
end;

procedure TOBDCustomControl.LoadSettings(AObject: TJSONObject);
begin
  // Base controls have no persisted settings.
end;

procedure TOBDCustomControl.AssignDataSource(ASource: TComponent);
begin
  // Controls without a data channel ignore the source.
end;

function TOBDCustomControl.TileHost: IOBDTileHost;
begin
  Result := nil;
  if (Parent <> nil) and not(csDesigning in ComponentState) then
    if not Supports(Parent, IOBDTileHost, Result) then
      Result := nil;
end;

function TOBDCustomControl.EditHitTest(X, Y: Integer): TOBDTileHit;
var
  Grip: Integer;
begin
  if (X < 0) or (Y < 0) or (X >= Width) or (Y >= Height) then
    Exit(thNone);
  Grip := ScaleValue(20);
  if (X >= Width - Grip) and (Y < Grip) then
    Result := thClose
  else if (X >= Width - Grip) and (Y >= Height - Grip) then
    Result := thResize
  else
    Result := thMove;
end;

procedure TOBDCustomControl.DrawEditOverlay(ACanvas: TCanvas);
var
  Grip, Pad, I: Integer;
  R: TRect;
begin
  Grip := ScaleValue(20);
  Pad := ScaleValue(5);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psDash;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := EffectiveAccent;
  ACanvas.Rectangle(0, 0, Width, Height);
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := ScaleValue(2);
  // Close glyph: an X in a filled square, top-right.
  R := Rect(Width - Grip, 0, Width, Grip);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Palette.Danger;
  ACanvas.FillRect(R);
  ACanvas.Pen.Color := clWhite;
  ACanvas.MoveTo(R.Left + Pad, R.Top + Pad);
  ACanvas.LineTo(R.Right - Pad, R.Bottom - Pad);
  ACanvas.MoveTo(R.Right - Pad, R.Top + Pad);
  ACanvas.LineTo(R.Left + Pad, R.Bottom - Pad);
  // Resize grip: three diagonal strokes, bottom-right.
  ACanvas.Pen.Color := EffectiveAccent;
  for I := 1 to 3 do
  begin
    ACanvas.MoveTo(Width - I * ScaleValue(5), Height - 2);
    ACanvas.LineTo(Width - 2, Height - I * ScaleValue(5));
  end;
end;

procedure TOBDCustomControl.WndProc(var Message: TMessage);
var
  Host: IOBDTileHost;
begin
  if (Message.Msg >= WM_MOUSEFIRST) and (Message.Msg <= WM_MOUSELAST) then
  begin
    Host := TileHost;
    if (Host <> nil) and Host.IsEditingTiles then
    begin
      Host.TileMouseMessage(Self, Message);
      Exit;
    end;
  end;
  inherited WndProc(Message);
end;

procedure TOBDCustomControl.Paint;
var
  Host: IOBDTileHost;
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

procedure TOBDCustomControl.Repaint;
begin
  FBufferDirty := True;
  inherited;
end;

procedure TOBDCustomControl.Resize;
begin
  inherited;
  FBufferDirty := True;
end;

procedure TOBDCustomControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  FBufferDirty := True;
  Invalidate;
end;

procedure TOBDCustomControl.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  FBufferDirty := True;
  Invalidate;
end;

procedure TOBDCustomControl.ThemeChanged;
begin
  FBufferDirty := True;
  Invalidate;
end;

function TOBDCustomControl.GetStyleBackground: TColor;
begin
  Result := FStyle.Background;
end;

function TOBDCustomControl.GetStyleForeground: TColor;
begin
  Result := FStyle.Foreground;
end;

function TOBDCustomControl.GetStyleAccent: TColor;
begin
  Result := FStyle.Accent;
end;

function TOBDCustomControl.GetStyleBorder: TColor;
begin
  Result := FStyle.Border;
end;

procedure TOBDCustomControl.SetStyleBackground(AValue: TColor);
begin
  if FStyle.Background = AValue then
    Exit;
  FStyle.Background := AValue;
  FBufferDirty := True;
  Invalidate;
end;

procedure TOBDCustomControl.SetStyleForeground(AValue: TColor);
begin
  if FStyle.Foreground = AValue then
    Exit;
  FStyle.Foreground := AValue;
  FBufferDirty := True;
  Invalidate;
end;

procedure TOBDCustomControl.SetStyleAccent(AValue: TColor);
begin
  if FStyle.Accent = AValue then
    Exit;
  FStyle.Accent := AValue;
  FBufferDirty := True;
  Invalidate;
end;

procedure TOBDCustomControl.SetStyleBorder(AValue: TColor);
begin
  if FStyle.Border = AValue then
    Exit;
  FStyle.Border := AValue;
  FBufferDirty := True;
  Invalidate;
end;

{ ---- TOBDGraphicControl ---------------------------------------------------- }

constructor TOBDGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle.Reset;
  FDesignPPI := DESIGN_PPI;
end;

destructor TOBDGraphicControl.Destroy;
begin
  DetachFromTheme;
  inherited;
end;

procedure TOBDGraphicControl.SetTheme(AValue: TOBDTheme);
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

procedure TOBDGraphicControl.ResolveTheme;
var
  Candidate: TOBDTheme;
begin
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;
  Candidate := FTheme;
  if Candidate = nil then
    Candidate := TOBDTheme.FindOnOwner(Self);
  if Candidate = nil then
    Candidate := TOBDTheme.GetDefault;
  FResolvedTheme := Candidate;
  if FResolvedTheme <> nil then
    FResolvedTheme.Attach(Self);
end;

procedure TOBDGraphicControl.DetachFromTheme;
begin
  if FResolvedTheme <> nil then
  begin
    FResolvedTheme.Detach(Self);
    FResolvedTheme := nil;
  end;
end;

procedure TOBDGraphicControl.Loaded;
begin
  inherited;
  ResolveTheme;
end;

procedure TOBDGraphicControl.Notification(AComponent: TComponent;
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

function TOBDGraphicControl.Palette: TOBDThemePalette;
begin
  if FResolvedTheme <> nil then
    Exit(FResolvedTheme.Palette);
  if VCLStyleIsDark then
    Result := BRAND_PALETTE_DARK
  else
    Result := BRAND_PALETTE_LIGHT;
end;

function TOBDGraphicControl.ScaleValue(N: Integer): Integer;
begin
{$IF CompilerVersion >= 35}
  Result := MulDiv(N, Self.CurrentPPI, FDesignPPI);
{$ELSE}
  Result := MulDiv(N, Self.Font.PixelsPerInch, FDesignPPI);
{$ENDIF}
end;

function TOBDGraphicControl.EffectiveBackground: TColor;
begin
  Result := PickColor(FStyle.Background, Palette.Background);
end;

function TOBDGraphicControl.EffectiveForeground: TColor;
begin
  Result := PickColor(FStyle.Foreground, Palette.ForegroundText);
end;

function TOBDGraphicControl.EffectiveAccent: TColor;
begin
  Result := PickColor(FStyle.Accent, Palette.Accent);
end;

function TOBDGraphicControl.EffectiveBorder: TColor;
begin
  Result := PickColor(FStyle.Border, Palette.Subtle);
end;

procedure TOBDGraphicControl.Paint;
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

procedure TOBDGraphicControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  Invalidate;
end;

procedure TOBDGraphicControl.ThemeChanged;
begin
  Invalidate;
end;

function TOBDGraphicControl.GetStyleBackground: TColor;
begin
  Result := FStyle.Background;
end;

function TOBDGraphicControl.GetStyleForeground: TColor;
begin
  Result := FStyle.Foreground;
end;

function TOBDGraphicControl.GetStyleAccent: TColor;
begin
  Result := FStyle.Accent;
end;

function TOBDGraphicControl.GetStyleBorder: TColor;
begin
  Result := FStyle.Border;
end;

procedure TOBDGraphicControl.SetStyleBackground(AValue: TColor);
begin
  if FStyle.Background <> AValue then
  begin
    FStyle.Background := AValue;
    Invalidate;
  end;
end;

procedure TOBDGraphicControl.SetStyleForeground(AValue: TColor);
begin
  if FStyle.Foreground <> AValue then
  begin
    FStyle.Foreground := AValue;
    Invalidate;
  end;
end;

procedure TOBDGraphicControl.SetStyleAccent(AValue: TColor);
begin
  if FStyle.Accent <> AValue then
  begin
    FStyle.Accent := AValue;
    Invalidate;
  end;
end;

procedure TOBDGraphicControl.SetStyleBorder(AValue: TColor);
begin
  if FStyle.Border <> AValue then
  begin
    FStyle.Border := AValue;
    Invalidate;
  end;
end;

end.
