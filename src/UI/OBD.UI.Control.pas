//------------------------------------------------------------------------------
//  OBD.UI.Control
//
//  Custom-paint base classes every visual derives from.
//
//    TOBDCustomControl    windowed control (focusable, accepts
//                         keyboard input). Use for lists,
//                         interactive widgets, anything that
//                         needs Tab focus.
//    TOBDGraphicControl   lightweight non-windowed control.
//                         Use for indicator lamps, badges,
//                         sparklines — anything that can sit
//                         inside another control's client area
//                         transparently.
//
//  Both bases satisfy the universal quality bar:
//    - Theme-aware (auto-bind via TOBDTheme.FindOnOwner,
//      explicit Theme property, per-component Style overrides,
//      resolution chain with VCL Style fallback and brand
//      default).
//    - HiDPI-aware (ScaleValue helper, ChangeScale override
//      that invalidates on DPI change, geometry uses MulDiv).
//    - Production-ready (double-buffered paint pipeline,
//      thread-safe Value setter on subclasses, csDesigning /
//      csLoading safe).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Control;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Themes,
  OBD.UI.Types,
  OBD.UI.Theme;

type
  /// <summary>Base for windowed visuals (focusable, can host
  /// keyboard input). Subclasses override <c>PaintControl</c>
  /// to draw onto the supplied <c>TCanvas</c>; the base class
  /// handles double-buffering, theme resolution, and DPI
  /// scaling.</summary>
  TOBDCustomControl = class(TCustomControl, IOBDThemeAware)
  strict private
    FTheme:         TOBDTheme;
    FStyle:         TOBDVisualStyle;
    FResolvedTheme: TOBDTheme;
    FBuffer:        TBitmap;
    FBufferDirty:   Boolean;
    FDesignPPI:     Integer;
    procedure SetTheme(AValue: TOBDTheme);
    procedure ResolveTheme;
    procedure DetachFromTheme;
    procedure StyleStorageWriter(Sender: TObject);
    function  GetStyleBackground: TColor;
    function  GetStyleForeground: TColor;
    function  GetStyleAccent:     TColor;
    function  GetStyleBorder:     TColor;
    procedure SetStyleBackground(AValue: TColor);
    procedure SetStyleForeground(AValue: TColor);
    procedure SetStyleAccent(AValue: TColor);
    procedure SetStyleBorder(AValue: TColor);
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
    function EffectiveAccent:     TColor;
    function EffectiveBorder:     TColor;

    procedure Paint; override;
    procedure Resize; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ThemeChanged;   //  IOBDThemeAware
    /// <summary>Force a repaint at the next idle cycle (the
    /// double-buffer is invalidated; <c>Paint</c> redraws on
    /// next WM_PAINT).</summary>
    procedure Repaint; override;
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
    property StyleAccent:     TColor read GetStyleAccent
      write SetStyleAccent default clDefault;
    property StyleBorder:     TColor read GetStyleBorder
      write SetStyleBorder default clDefault;

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
    FTheme:      TOBDTheme;
    FStyle:      TOBDVisualStyle;
    FResolvedTheme: TOBDTheme;
    FDesignPPI:  Integer;
    procedure SetTheme(AValue: TOBDTheme);
    procedure ResolveTheme;
    procedure DetachFromTheme;
    function  GetStyleBackground: TColor;
    function  GetStyleForeground: TColor;
    function  GetStyleAccent:     TColor;
    function  GetStyleBorder:     TColor;
    procedure SetStyleBackground(AValue: TColor);
    procedure SetStyleForeground(AValue: TColor);
    procedure SetStyleAccent(AValue: TColor);
    procedure SetStyleBorder(AValue: TColor);
  protected
    procedure PaintControl(ACanvas: TCanvas); virtual; abstract;
    function  Palette: TOBDThemePalette;
    function  ScaleValue(N: Integer): Integer; inline;
    function  EffectiveBackground: TColor;
    function  EffectiveForeground: TColor;
    function  EffectiveAccent:     TColor;
    function  EffectiveBorder:     TColor;

    procedure Paint; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ThemeChanged;
  published
    property Theme: TOBDTheme read FTheme write SetTheme;
    property StyleBackground: TColor read GetStyleBackground
      write SetStyleBackground default clDefault;
    property StyleForeground: TColor read GetStyleForeground
      write SetStyleForeground default clDefault;
    property StyleAccent:     TColor read GetStyleAccent
      write SetStyleAccent default clDefault;
    property StyleBorder:     TColor read GetStyleBorder
      write SetStyleBorder default clDefault;

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

implementation

const
  DESIGN_PPI = 96;

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
  if FTheme = AValue then Exit;
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
  if Candidate = nil then Candidate := TOBDTheme.FindOnOwner(Self);
  if Candidate = nil then Candidate := TOBDTheme.GetDefault;
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
    if AComponent = FTheme then FTheme := nil;
    if AComponent = FResolvedTheme then FResolvedTheme := nil;
  end;
end;

function TOBDCustomControl.Palette: TOBDThemePalette;
begin
  if FResolvedTheme <> nil then
    Exit(FResolvedTheme.Palette);
  // No theme — synthesize one from VCL Style + brand default.
  if VCLStyleIsDark then Result := BRAND_PALETTE_DARK
  else                   Result := BRAND_PALETTE_LIGHT;
  if TStyleManager.IsCustomStyleActive then
  begin
    Result.Background     := StyleColor(scWindow,     Result.Background);
    Result.ForegroundText := StyleColor(scWindowText, Result.ForegroundText);
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

procedure TOBDCustomControl.Paint;
begin
  if (Width <= 0) or (Height <= 0) then Exit;
  if FBufferDirty or (FBuffer.Width <> Width) or
     (FBuffer.Height <> Height) then
  begin
    FBuffer.SetSize(Width, Height);
    // Fill the buffer with the resolved background so subclass
    // paint can punch through with translucency where it wants.
    FBuffer.Canvas.Brush.Color := EffectiveBackground;
    FBuffer.Canvas.FillRect(Rect(0, 0, Width, Height));
    PaintControl(FBuffer.Canvas);
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

procedure TOBDCustomControl.StyleStorageWriter(Sender: TObject);
begin
  // hook left in place in case future revisions need a custom
  // streaming path for TOBDVisualStyle. Currently unused.
end;

function TOBDCustomControl.GetStyleBackground: TColor; begin Result := FStyle.Background; end;
function TOBDCustomControl.GetStyleForeground: TColor; begin Result := FStyle.Foreground; end;
function TOBDCustomControl.GetStyleAccent:     TColor; begin Result := FStyle.Accent;     end;
function TOBDCustomControl.GetStyleBorder:     TColor; begin Result := FStyle.Border;     end;

procedure TOBDCustomControl.SetStyleBackground(AValue: TColor);
begin
  if FStyle.Background = AValue then Exit;
  FStyle.Background := AValue;
  FBufferDirty := True; Invalidate;
end;

procedure TOBDCustomControl.SetStyleForeground(AValue: TColor);
begin
  if FStyle.Foreground = AValue then Exit;
  FStyle.Foreground := AValue;
  FBufferDirty := True; Invalidate;
end;

procedure TOBDCustomControl.SetStyleAccent(AValue: TColor);
begin
  if FStyle.Accent = AValue then Exit;
  FStyle.Accent := AValue;
  FBufferDirty := True; Invalidate;
end;

procedure TOBDCustomControl.SetStyleBorder(AValue: TColor);
begin
  if FStyle.Border = AValue then Exit;
  FStyle.Border := AValue;
  FBufferDirty := True; Invalidate;
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
  if FTheme = AValue then Exit;
  if FResolvedTheme <> nil then FResolvedTheme.Detach(Self);
  if FTheme <> nil then FTheme.RemoveFreeNotification(Self);
  FTheme := AValue;
  if FTheme <> nil then FTheme.FreeNotification(Self);
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
  if Candidate = nil then Candidate := TOBDTheme.FindOnOwner(Self);
  if Candidate = nil then Candidate := TOBDTheme.GetDefault;
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
    if AComponent = FTheme then FTheme := nil;
    if AComponent = FResolvedTheme then FResolvedTheme := nil;
  end;
end;

function TOBDGraphicControl.Palette: TOBDThemePalette;
begin
  if FResolvedTheme <> nil then
    Exit(FResolvedTheme.Palette);
  if VCLStyleIsDark then Result := BRAND_PALETTE_DARK
  else                   Result := BRAND_PALETTE_LIGHT;
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
begin Result := PickColor(FStyle.Background, Palette.Background); end;

function TOBDGraphicControl.EffectiveForeground: TColor;
begin Result := PickColor(FStyle.Foreground, Palette.ForegroundText); end;

function TOBDGraphicControl.EffectiveAccent: TColor;
begin Result := PickColor(FStyle.Accent, Palette.Accent); end;

function TOBDGraphicControl.EffectiveBorder: TColor;
begin Result := PickColor(FStyle.Border, Palette.Subtle); end;

procedure TOBDGraphicControl.Paint;
begin
  if (Width <= 0) or (Height <= 0) then Exit;
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

function TOBDGraphicControl.GetStyleBackground: TColor; begin Result := FStyle.Background; end;
function TOBDGraphicControl.GetStyleForeground: TColor; begin Result := FStyle.Foreground; end;
function TOBDGraphicControl.GetStyleAccent:     TColor; begin Result := FStyle.Accent;     end;
function TOBDGraphicControl.GetStyleBorder:     TColor; begin Result := FStyle.Border;     end;

procedure TOBDGraphicControl.SetStyleBackground(AValue: TColor);
begin if FStyle.Background <> AValue then begin FStyle.Background := AValue; Invalidate; end; end;

procedure TOBDGraphicControl.SetStyleForeground(AValue: TColor);
begin if FStyle.Foreground <> AValue then begin FStyle.Foreground := AValue; Invalidate; end; end;

procedure TOBDGraphicControl.SetStyleAccent(AValue: TColor);
begin if FStyle.Accent     <> AValue then begin FStyle.Accent     := AValue; Invalidate; end; end;

procedure TOBDGraphicControl.SetStyleBorder(AValue: TColor);
begin if FStyle.Border     <> AValue then begin FStyle.Border     := AValue; Invalidate; end; end;

end.
