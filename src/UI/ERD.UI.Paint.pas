//------------------------------------------------------------------------------
//  ERD.UI.Paint
//
//  The look of the OBD Studio controls in one place. TOBDPainter draws
//  the shared pieces - cards, status chips, counter badges, buttons,
//  check boxes, radio buttons, switches, banners, range bars, edit
//  frames and the small line glyphs - from a theme palette, so a
//  stand-alone TOBDChip and a chip inside a DTC panel row are the same
//  pixels. Shapes are anti-aliased with GDI+; text goes through GDI so
//  it gets ClearType.
//
//  Every coordinate handed to the painter is in device pixels. Fixed
//  design sizes (a chip is 20 px high, a card edge 4 px wide) are
//  96-DPI values that the painter scales by the PPI it was created
//  with.
//
//  Colour rules shared by every control:
//    - tint:        a status colour washed into the card face
//                   (13% light, 22% dark);
//    - header:      column-header fill, NeutralLight over the face;
//    - on-accent:   ink on an orange fill, the darker of text and
//                   background;
//    - accent text: orange used as text or outline (GaugeNeedle).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Paint;

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  Vcl.Graphics,
  Vcl.StdCtrls,
  ERD.UI.Types,
  ERD.UI.GDIP;

type
  /// <summary>Font weight of painted text.</summary>
  TOBDTextWeight = (
    /// <summary>Segoe UI.</summary>
    twRegular,
    /// <summary>Segoe UI Semibold.</summary>
    twSemibold,
    /// <summary>Segoe UI bold.</summary>
    twBold,
    /// <summary>Consolas (codes, VINs, IDs).</summary>
    twMono,
    /// <summary>Consolas bold.</summary>
    twMonoBold);

  /// <summary>Status a chip, badge or card edge stands for.</summary>
  TOBDStatusKind = (
    /// <summary>Muted grey: defaults, unsupported.</summary>
    skNeutral,
    /// <summary>ERDesigns orange: permanent codes, garage values.
    /// </summary>
    skAccent,
    /// <summary>Green: complete, connected.</summary>
    skSuccess,
    /// <summary>Amber: pending, incomplete.</summary>
    skWarning,
    /// <summary>Red: stored codes, faults.</summary>
    skDanger);

  /// <summary>Interaction state of a button or toggle.</summary>
  TOBDControlState = (
    /// <summary>At rest.</summary>
    cstNormal,
    /// <summary>Pointer over it.</summary>
    cstHover,
    /// <summary>Mouse button or key held down.</summary>
    cstPressed,
    /// <summary>Enabled = False.</summary>
    cstDisabled);

  /// <summary>Visual weight of a <c>TOBDButton</c>.</summary>
  TOBDButtonKind = (
    /// <summary>Orange fill: the main action of a panel.</summary>
    bkPrimary,
    /// <summary>Face fill with a border: Cancel and other side
    /// actions.</summary>
    bkSecondary,
    /// <summary>Red fill: confirms a destructive action.</summary>
    bkDanger,
    /// <summary>Red outline: opens a destructive action.</summary>
    bkDangerOutline,
    /// <summary>Orange text only: links such as "Edit ranges...".
    /// </summary>
    bkGhost);

  /// <summary>Built-in line glyphs.</summary>
  TOBDGlyph = (
    /// <summary>No glyph.</summary>
    glNone,
    /// <summary>Arrow into a tray: read codes / download.</summary>
    glRead,
    /// <summary>Waste bin: clear codes.</summary>
    glClear,
    /// <summary>Camera: freeze frame / snapshot.</summary>
    glSnapshot,
    /// <summary>Check mark.</summary>
    glCheck,
    /// <summary>Half-filled ring: in progress / incomplete.</summary>
    glPending,
    /// <summary>Dash: not supported.</summary>
    glDash,
    /// <summary>Warning triangle with an exclamation mark.</summary>
    glAlert);

  /// <summary>Colour and icon of a <c>TOBDBanner</c>.</summary>
  TOBDBannerKind = (
    /// <summary>Orange "i": neutral information.</summary>
    bnInfo,
    /// <summary>Green check.</summary>
    bnSuccess,
    /// <summary>Amber ring: something still to do.</summary>
    bnWarning,
    /// <summary>Red "!": a fault or a lost connection.</summary>
    bnDanger);

  /// <summary>Paints the shared OBD Studio pieces onto a canvas with
  /// the colours of one palette.</summary>
  TOBDPainter = class
  strict private
    FCanvas: TCanvas;
    FPalette: TOBDThemePalette;
    FPPI: Integer;
    FDark: Boolean;
    FFontName: string;
    function NewGraphics: TGPGraphics;
    procedure AddRoundRect(APath: TGPGraphicsPath; X, Y, W, H,
      ARadius: Single);
  public
    /// <summary>Creates a painter for one paint pass.</summary>
    /// <param name="ACanvas">Target canvas. Not owned.</param>
    /// <param name="APalette">Resolved theme palette.</param>
    /// <param name="APPI">Pixels per inch of the control (96 = 100%).
    /// </param>
    constructor Create(ACanvas: TCanvas; const APalette: TOBDThemePalette;
      APPI: Integer);

    /// <summary>Scales a 96-DPI length to device pixels, rounded.
    /// </summary>
    /// <param name="AValue">Design length.</param>
    /// <returns>Device pixels.</returns>
    function S(AValue: Single): Integer;
    /// <summary>Scales a 96-DPI length to device pixels, unrounded.
    /// </summary>
    /// <param name="AValue">Design length.</param>
    /// <returns>Device pixels.</returns>
    function SF(AValue: Single): Single;

    { Colours }

    /// <summary>A status colour washed into the card face: 13% on the
    /// light palette, 22% on the dark one.</summary>
    /// <param name="AColor">Status colour.</param>
    /// <returns>Tint colour.</returns>
    function Tint(AColor: TColor): TColor; overload;
    /// <summary>A colour washed into the card face.</summary>
    /// <param name="AColor">Colour to wash in.</param>
    /// <param name="AStrength">0 = face, 1 = AColor.</param>
    /// <returns>Tint colour.</returns>
    function Tint(AColor: TColor; AStrength: Single): TColor; overload;
    /// <summary>Column-header and category fill.</summary>
    /// <returns>Header colour.</returns>
    function HeaderFill: TColor;
    /// <summary>Ink for text and glyphs on an orange fill.</summary>
    /// <returns>The darker of text and background.</returns>
    function OnAccent: TColor;
    /// <summary>Orange used as text, outline or indicator.</summary>
    /// <returns>GaugeNeedle colour.</returns>
    function AccentText: TColor;
    /// <summary>Danger colour readable as text on the face.</summary>
    /// <returns>Danger, lifted towards white on the dark palette.
    /// </returns>
    function DangerInk: TColor;
    /// <summary>Text colour of a disabled control.</summary>
    /// <returns>Muted text colour.</returns>
    function DisabledText: TColor;
    /// <summary>Ink for text on a solid status fill.</summary>
    /// <param name="AFill">The fill colour.</param>
    /// <returns>Dark ink on orange (and on amber in the dark
    /// palette), white on the rest.</returns>
    function InkOn(AFill: TColor): TColor;
    /// <summary>Palette colour of a status kind.</summary>
    /// <param name="AKind">Status kind.</param>
    /// <returns>Colour.</returns>
    function StatusColor(AKind: TOBDStatusKind): TColor;
    /// <summary>Text colour of a value at an alert level.</summary>
    /// <param name="ALevel">Alert level.</param>
    /// <returns>Text colour for normal, warning or danger colour.
    /// </returns>
    function LevelColor(ALevel: TOBDAlertLevel): TColor;

    { Primitives }

    /// <summary>Solid rectangle, not anti-aliased.</summary>
    procedure FillRect(const R: TRect; AColor: TColor);
    /// <summary>Rectangle outline drawn inside R.</summary>
    procedure FrameRect(const R: TRect; AColor: TColor; AWidth: Integer = 1);
    /// <summary>One-pixel horizontal line.</summary>
    procedure HLine(X, Y, W: Integer; AColor: TColor);
    /// <summary>One-pixel vertical line.</summary>
    procedure VLine(X, Y, H: Integer; AColor: TColor);
    /// <summary>Anti-aliased rounded rectangle. Pass <c>clNone</c> to
    /// skip the fill or the outline.</summary>
    procedure RoundRect(X, Y, W, H, ARadius: Single; AFill, AOutline: TColor;
      AOutlineWidth: Single = 1);
    /// <summary>Anti-aliased ellipse. Pass <c>clNone</c> to skip the
    /// fill or the outline.</summary>
    procedure Ellipse(X, Y, W, H: Single; AFill, AOutline: TColor;
      AOutlineWidth: Single = 1);
    /// <summary>Anti-aliased pie slice; angles clockwise from 3
    /// o'clock.</summary>
    procedure Pie(X, Y, W, H, AStart, ASweep: Single; AColor: TColor);
    /// <summary>Open polyline with round joins and caps.</summary>
    procedure Lines(const APoints: array of TGPPointF; AColor: TColor;
      AWidth: Single);
    /// <summary>Filled polygon.</summary>
    procedure Polygon(const APoints: array of TGPPointF; AColor: TColor);

    { Text }

    /// <summary>Selects the font for a size and weight.</summary>
    /// <param name="ASize">Em height in 96-DPI pixels.</param>
    /// <param name="AWeight">Weight.</param>
    /// <param name="AColor">Text colour.</param>
    procedure SetFont(ASize: Single; AWeight: TOBDTextWeight; AColor: TColor);
    /// <summary>Width of a string in device pixels.</summary>
    function TextWidth(const AText: string; ASize: Single;
      AWeight: TOBDTextWeight = twRegular): Integer;
    /// <summary>Draws one line of text.</summary>
    /// <param name="X">Left edge (taLeftJustify), right edge
    /// (taRightJustify) or centre (taCenter).</param>
    /// <param name="CY">Vertical centre.</param>
    /// <param name="AText">Text.</param>
    /// <param name="ASize">Em height in 96-DPI pixels.</param>
    /// <param name="AColor">Colour.</param>
    /// <param name="AWeight">Weight.</param>
    /// <param name="AAlign">Horizontal anchor.</param>
    /// <param name="AMaxWidth">Device pixels; longer text ends in an
    /// ellipsis. 0 = no limit.</param>
    /// <returns>Drawn width in device pixels.</returns>
    function Text(X, CY: Integer; const AText: string; ASize: Single;
      AColor: TColor; AWeight: TOBDTextWeight = twRegular;
      AAlign: TAlignment = taLeftJustify; AMaxWidth: Integer = 0): Integer;
    /// <summary>Small upper-case caption (10.5 px semibold) in the
    /// label colour.</summary>
    /// <returns>Drawn width in device pixels.</returns>
    function Caps(X, CY: Integer; const AText: string;
      AAlign: TAlignment = taLeftJustify; AColor: TColor = clDefault;
      AMaxWidth: Integer = 0): Integer;
    /// <summary>Text wrapped into a rectangle, top aligned.</summary>
    /// <returns>Height used, in device pixels.</returns>
    function WrapText(const R: TRect; const AText: string; ASize: Single;
      AColor: TColor; AWeight: TOBDTextWeight = twRegular): Integer;

    { Glyphs: centre in device pixels, AScale multiplies the design
      size. }

    procedure GlyphCheck(CX, CY: Single; AColor: TColor; AScale: Single = 1);
    procedure GlyphPending(CX, CY: Single; AColor: TColor; AScale: Single = 1);
    procedure GlyphDash(CX, CY: Single; AColor: TColor; AScale: Single = 1);
    procedure GlyphRead(CX, CY: Single; AColor: TColor);
    procedure GlyphClear(CX, CY: Single; AColor: TColor);
    procedure GlyphSnapshot(CX, CY: Single; AColor: TColor);
    procedure GlyphChevron(CX, CY: Single; AColor: TColor; ADown: Boolean);
    /// <summary>Warning triangle with "!", 24 px wide.</summary>
    procedure GlyphAlert(CX, CY: Single; AColor: TColor);
    /// <summary>Draws any built-in glyph.</summary>
    procedure Glyph(AGlyph: TOBDGlyph; CX, CY: Single; AColor: TColor;
      AScale: Single = 1);

    { Composite pieces }

    /// <summary>Card surface: face fill, 1 px border and an optional
    /// 4 px status edge on the left.</summary>
    /// <param name="R">Card bounds.</param>
    /// <param name="AEdge">Edge colour, or clNone.</param>
    /// <param name="AFill">Fill, or clDefault for the face colour.
    /// </param>
    procedure Card(const R: TRect; AEdge: TColor = clNone;
      AFill: TColor = clDefault);
    /// <summary>Width of a status chip.</summary>
    function ChipWidth(const AText: string; AMono: Boolean = False;
      ASize: Single = 10.5): Integer;
    /// <summary>Status pill, 20 px high.</summary>
    /// <param name="X">Left edge.</param>
    /// <param name="Y">Top edge.</param>
    /// <param name="AText">Label, normally upper case.</param>
    /// <param name="AColor">Status colour.</param>
    /// <param name="AFilled">Solid fill instead of a tint.</param>
    /// <param name="AMono">Monospace label (DTC codes).</param>
    /// <param name="ASize">Label size in 96-DPI pixels.</param>
    /// <returns>Width in device pixels.</returns>
    function Chip(X, Y: Integer; const AText: string; AColor: TColor;
      AFilled: Boolean = False; AMono: Boolean = False;
      ASize: Single = 10.5): Integer;
    /// <summary>Width of a counter badge.</summary>
    function BadgeWidth(const AText: string): Integer;
    /// <summary>Counter bubble, 18 px high.</summary>
    /// <param name="ARight">Right edge.</param>
    /// <param name="ATop">Top edge.</param>
    procedure Badge(ARight, ATop: Integer; const AText: string;
      AColor: TColor);
    /// <summary>Fill, outline and ink of a button. clNone = none.
    /// </summary>
    procedure ButtonColors(AKind: TOBDButtonKind; AState: TOBDControlState;
      out AFill, AOutline, AInk: TColor);
    /// <summary>Natural width of a button: caption + 28 px, + 18 px
    /// with a glyph.</summary>
    function ButtonWidth(const ACaption: string; AGlyph: TOBDGlyph): Integer;
    /// <summary>Paints a button.</summary>
    procedure Button(const R: TRect; const ACaption: string;
      AKind: TOBDButtonKind; AState: TOBDControlState; AFocused: Boolean;
      AGlyph: TOBDGlyph);
    /// <summary>2 px orange ring 3 px outside a shape.</summary>
    procedure FocusRing(X, Y, W, H: Single; ARadius: Single = 0);
    /// <summary>Check box square.</summary>
    /// <param name="X">Left edge.</param>
    /// <param name="CY">Vertical centre.</param>
    /// <param name="ASize">Box size in 96-DPI pixels (density).</param>
    procedure CheckBox(X, CY: Integer; ASize: Integer;
      AState: TCheckBoxState; AEnabled, AHover, AFocused: Boolean);
    /// <summary>Radio button circle.</summary>
    procedure RadioButton(X, CY: Integer; ASize: Integer;
      AChecked, AEnabled, AHover, AFocused: Boolean);
    /// <summary>Width of a switch track in device pixels.</summary>
    /// <param name="AHeight">Track height in 96-DPI pixels.</param>
    function SwitchWidth(AHeight: Integer): Integer;
    /// <summary>On / off switch track and knob.</summary>
    procedure Switch(X, CY: Integer; AHeight: Integer;
      AOn, AEnabled, AFocused: Boolean);
    /// <summary>Colour a banner kind stands for.</summary>
    function BannerColor(AKind: TOBDBannerKind): TColor;
    /// <summary>Banner surface: tinted fill, mixed outline and a 4 px
    /// edge.</summary>
    procedure BannerFrame(const R: TRect; AKind: TOBDBannerKind);
    /// <summary>Banner icon centred on (CX, CY).</summary>
    /// <param name="AAlert">Draw the warning triangle instead of the
    /// kind's own icon.</param>
    procedure BannerIcon(CX, CY: Integer; AKind: TOBDBannerKind;
      AAlert: Boolean = False);
    /// <summary>A value against its normal band: a 6 px track, the
    /// band in green and a 3 x 14 px marker.</summary>
    /// <param name="X">Left edge.</param>
    /// <param name="CY">Vertical centre.</param>
    /// <param name="W">Width.</param>
    procedure RangeBar(X, CY, W: Integer; AMin, AMax, ALow, AHigh,
      AValue: Double; ALevel: TOBDAlertLevel);
    /// <summary>Edit / combo box frame.</summary>
    procedure EditFrame(const R: TRect; AFocused, AEnabled: Boolean);

    /// <summary>Target canvas.</summary>
    property Canvas: TCanvas read FCanvas;
    /// <summary>Palette the painter draws with.</summary>
    property Palette: TOBDThemePalette read FPalette;
    /// <summary>True on a dark palette.</summary>
    property Dark: Boolean read FDark;
    /// <summary>Pixels per inch the painter scales to.</summary>
    property PPI: Integer read FPPI;
    /// <summary>Base font family. Default Segoe UI.</summary>
    property FontName: string read FFontName write FFontName;
  end;

/// <summary>Blends two colours.</summary>
/// <param name="A">Colour at weight 1.</param>
/// <param name="B">Colour at weight 0.</param>
/// <param name="T">Weight of A, 0..1.</param>
/// <returns>Mixed colour.</returns>
function OBDMixColor(A, B: TColor; T: Single): TColor;

/// <summary>True when the palette has a dark background.</summary>
/// <param name="APalette">Palette.</param>
/// <returns>Dark flag.</returns>
function OBDIsDarkPalette(const APalette: TOBDThemePalette): Boolean;

/// <summary>Alert level of a value against a normal band.</summary>
/// <param name="AValue">Value.</param>
/// <param name="ALow">Low end of the normal band.</param>
/// <param name="AHigh">High end of the normal band.</param>
/// <param name="AAlarmMargin">How far outside the band, as a fraction
/// of the band width, a value turns from warning into alarm.</param>
/// <returns>alvNormal inside the band, alvWarning just outside,
/// alvAlarm further out.</returns>
function OBDRangeLevel(AValue, ALow, AHigh, AAlarmMargin: Double)
  : TOBDAlertLevel;

const
  /// <summary>Card status edge width, 96-DPI pixels.</summary>
  OBD_EDGE = 4;
  /// <summary>Horizontal padding inside cards, 96-DPI pixels.</summary>
  OBD_PAD = 16;
  /// <summary>Chip height, 96-DPI pixels.</summary>
  OBD_CHIP_HEIGHT = 20;

implementation

const
  DESIGN_PPI = 96;
  SEMIBOLD_SUFFIX = ' Semibold';
  MONO_FONT = 'Consolas';

function OBDMixColor(A, B: TColor; T: Single): TColor;
var
  CA, CB: Cardinal;
begin
  CA := ColorToRGB(A);
  CB := ColorToRGB(B);
  T := EnsureRange(T, 0, 1);
  Result := TColor(RGB(
    Round(GetRValue(CA) * T + GetRValue(CB) * (1 - T)),
    Round(GetGValue(CA) * T + GetGValue(CB) * (1 - T)),
    Round(GetBValue(CA) * T + GetBValue(CB) * (1 - T))));
end;

function Luma(AColor: TColor): Integer;
var
  C: Cardinal;
begin
  C := ColorToRGB(AColor);
  Result := (GetRValue(C) * 299 + GetGValue(C) * 587 + GetBValue(C) * 114)
    div 1000;
end;

function OBDIsDarkPalette(const APalette: TOBDThemePalette): Boolean;
begin
  Result := Luma(APalette.Background) < 128;
end;

function OBDRangeLevel(AValue, ALow, AHigh, AAlarmMargin: Double)
  : TOBDAlertLevel;
var
  Band, Outside: Double;
begin
  if ALow > AHigh then
  begin
    Band := ALow;
    ALow := AHigh;
    AHigh := Band;
  end;
  if (AValue >= ALow) and (AValue <= AHigh) then
    Exit(alvNormal);
  Band := AHigh - ALow;
  if AValue < ALow then
    Outside := ALow - AValue
  else
    Outside := AValue - AHigh;
  if (Band > 0) and (Outside <= Band * AAlarmMargin) then
    Result := alvWarning
  else
    Result := alvAlarm;
end;

{ TOBDPainter --------------------------------------------------------------- }

constructor TOBDPainter.Create(ACanvas: TCanvas;
  const APalette: TOBDThemePalette; APPI: Integer);
begin
  inherited Create;
  FCanvas := ACanvas;
  FPalette := APalette;
  FPPI := APPI;
  if FPPI <= 0 then
    FPPI := DESIGN_PPI;
  FDark := OBDIsDarkPalette(APalette);
  FFontName := 'Segoe UI';
end;

function TOBDPainter.S(AValue: Single): Integer;
begin
  Result := Round(AValue * FPPI / DESIGN_PPI);
end;

function TOBDPainter.SF(AValue: Single): Single;
begin
  Result := AValue * FPPI / DESIGN_PPI;
end;

function TOBDPainter.NewGraphics: TGPGraphics;
begin
  Result := TGPGraphics.Create(FCanvas.Handle);
  Result.SetSmoothingMode(SmoothingModeAntiAlias);
  // Pixel i covers [i, i + 1], so a fill from X to X + W covers
  // exactly the pixels a GDI FillRect would.
  Result.SetPixelOffsetMode(PixelOffsetModeHalf);
end;

{ Colours }

function TOBDPainter.Tint(AColor: TColor): TColor;
begin
  if FDark then
    Result := Tint(AColor, 0.22)
  else
    Result := Tint(AColor, 0.13);
end;

function TOBDPainter.Tint(AColor: TColor; AStrength: Single): TColor;
begin
  Result := OBDMixColor(AColor, FPalette.GaugeFace, AStrength);
end;

function TOBDPainter.HeaderFill: TColor;
begin
  Result := OBDMixColor(FPalette.NeutralLight, FPalette.GaugeFace, 0.45);
end;

function TOBDPainter.OnAccent: TColor;
begin
  if Luma(FPalette.ForegroundText) <= Luma(FPalette.Background) then
    Result := FPalette.ForegroundText
  else
    Result := FPalette.Background;
end;

function TOBDPainter.AccentText: TColor;
begin
  Result := FPalette.GaugeNeedle;
end;

function TOBDPainter.DangerInk: TColor;
begin
  if FDark then
    Result := OBDMixColor(FPalette.Danger, clWhite, 0.7)
  else
    Result := FPalette.Danger;
end;

function TOBDPainter.DisabledText: TColor;
begin
  Result := OBDMixColor(FPalette.Subtle, FPalette.GaugeFace, 0.6);
end;

function TOBDPainter.InkOn(AFill: TColor): TColor;
begin
  if (AFill = FPalette.Accent) or (FDark and (AFill = FPalette.Warning)) then
    Result := OnAccent
  else
    Result := clWhite;
end;

function TOBDPainter.StatusColor(AKind: TOBDStatusKind): TColor;
begin
  case AKind of
    skAccent:
      Result := AccentText;
    skSuccess:
      Result := FPalette.Success;
    skWarning:
      Result := FPalette.Warning;
    skDanger:
      Result := FPalette.Danger;
  else
    Result := FPalette.Subtle;
  end;
end;

function TOBDPainter.LevelColor(ALevel: TOBDAlertLevel): TColor;
begin
  case ALevel of
    alvWarning:
      Result := FPalette.Warning;
    alvAlarm:
      Result := FPalette.Danger;
  else
    Result := FPalette.ForegroundText;
  end;
end;

{ Primitives }

procedure TOBDPainter.FillRect(const R: TRect; AColor: TColor);
begin
  if (AColor = clNone) or R.IsEmpty then
    Exit;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.Color := AColor;
  FCanvas.FillRect(R);
end;

procedure TOBDPainter.FrameRect(const R: TRect; AColor: TColor;
  AWidth: Integer);
begin
  if (AColor = clNone) or (AWidth <= 0) then
    Exit;
  FillRect(Rect(R.Left, R.Top, R.Right, R.Top + AWidth), AColor);
  FillRect(Rect(R.Left, R.Bottom - AWidth, R.Right, R.Bottom), AColor);
  FillRect(Rect(R.Left, R.Top, R.Left + AWidth, R.Bottom), AColor);
  FillRect(Rect(R.Right - AWidth, R.Top, R.Right, R.Bottom), AColor);
end;

procedure TOBDPainter.HLine(X, Y, W: Integer; AColor: TColor);
begin
  FillRect(Rect(X, Y, X + W, Y + 1), AColor);
end;

procedure TOBDPainter.VLine(X, Y, H: Integer; AColor: TColor);
begin
  FillRect(Rect(X, Y, X + 1, Y + H), AColor);
end;

procedure TOBDPainter.AddRoundRect(APath: TGPGraphicsPath; X, Y, W, H,
  ARadius: Single);
var
  D: Single;
begin
  ARadius := System.Math.Min(ARadius, System.Math.Min(W, H) / 2);
  if ARadius <= 0.5 then
  begin
    APath.AddRectangle(MakeRect(X, Y, W, H));
    Exit;
  end;
  D := ARadius * 2;
  APath.AddArc(X, Y, D, D, 180, 90);
  APath.AddArc(X + W - D, Y, D, D, 270, 90);
  APath.AddArc(X + W - D, Y + H - D, D, D, 0, 90);
  APath.AddArc(X, Y + H - D, D, D, 90, 90);
  APath.CloseFigure;
end;

procedure TOBDPainter.RoundRect(X, Y, W, H, ARadius: Single;
  AFill, AOutline: TColor; AOutlineWidth: Single);
var
  G: TGPGraphics;
  Path: TGPGraphicsPath;
  Brush: TGPSolidBrush;
  Pen: TGPPen;
  Half: Single;
begin
  if (W <= 0) or (H <= 0) then
    Exit;
  G := NewGraphics;
  try
    if AFill <> clNone then
    begin
      Path := TGPGraphicsPath.Create;
      Brush := TGPSolidBrush.Create(ColorToARGB(AFill));
      try
        AddRoundRect(Path, X, Y, W, H, ARadius);
        G.FillPath(Brush, Path);
      finally
        Brush.Free;
        Path.Free;
      end;
    end;
    if (AOutline <> clNone) and (AOutlineWidth > 0) then
    begin
      // Stroke centred inside the shape so the outline covers the
      // same pixels as the fill's edge.
      Half := AOutlineWidth / 2;
      Path := TGPGraphicsPath.Create;
      Pen := TGPPen.Create(ColorToARGB(AOutline), AOutlineWidth);
      try
        AddRoundRect(Path, X + Half, Y + Half, W - AOutlineWidth,
          H - AOutlineWidth, ARadius - Half);
        G.DrawPath(Pen, Path);
      finally
        Pen.Free;
        Path.Free;
      end;
    end;
  finally
    G.Free;
  end;
end;

procedure TOBDPainter.Ellipse(X, Y, W, H: Single; AFill, AOutline: TColor;
  AOutlineWidth: Single);
var
  G: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen: TGPPen;
  Half: Single;
begin
  if (W <= 0) or (H <= 0) then
    Exit;
  G := NewGraphics;
  try
    if AFill <> clNone then
    begin
      Brush := TGPSolidBrush.Create(ColorToARGB(AFill));
      try
        G.FillEllipse(Brush, X, Y, W, H);
      finally
        Brush.Free;
      end;
    end;
    if (AOutline <> clNone) and (AOutlineWidth > 0) then
    begin
      Half := AOutlineWidth / 2;
      Pen := TGPPen.Create(ColorToARGB(AOutline), AOutlineWidth);
      try
        G.DrawEllipse(Pen, X + Half, Y + Half, W - AOutlineWidth,
          H - AOutlineWidth);
      finally
        Pen.Free;
      end;
    end;
  finally
    G.Free;
  end;
end;

procedure TOBDPainter.Pie(X, Y, W, H, AStart, ASweep: Single; AColor: TColor);
var
  G: TGPGraphics;
  Brush: TGPSolidBrush;
begin
  G := NewGraphics;
  try
    Brush := TGPSolidBrush.Create(ColorToARGB(AColor));
    try
      G.FillPie(Brush, X, Y, W, H, AStart, ASweep);
    finally
      Brush.Free;
    end;
  finally
    G.Free;
  end;
end;

procedure TOBDPainter.Lines(const APoints: array of TGPPointF;
  AColor: TColor; AWidth: Single);
var
  G: TGPGraphics;
  Pen: TGPPen;
begin
  if Length(APoints) < 2 then
    Exit;
  G := NewGraphics;
  try
    Pen := TGPPen.Create(ColorToARGB(AColor), AWidth);
    try
      Pen.SetStartCap(LineCapRound);
      Pen.SetEndCap(LineCapRound);
      Pen.SetLineJoin(LineJoinRound);
      G.DrawLines(Pen, PGPPointF(@APoints[0]), Length(APoints));
    finally
      Pen.Free;
    end;
  finally
    G.Free;
  end;
end;

procedure TOBDPainter.Polygon(const APoints: array of TGPPointF;
  AColor: TColor);
var
  G: TGPGraphics;
  Brush: TGPSolidBrush;
begin
  if Length(APoints) < 3 then
    Exit;
  G := NewGraphics;
  try
    Brush := TGPSolidBrush.Create(ColorToARGB(AColor));
    try
      G.FillPolygon(Brush, PGPPointF(@APoints[0]), Length(APoints));
    finally
      Brush.Free;
    end;
  finally
    G.Free;
  end;
end;

{ Text }

procedure TOBDPainter.SetFont(ASize: Single; AWeight: TOBDTextWeight;
  AColor: TColor);
begin
  case AWeight of
    twSemibold:
      FCanvas.Font.Name := FFontName + SEMIBOLD_SUFFIX;
    twMono, twMonoBold:
      FCanvas.Font.Name := MONO_FONT;
  else
    FCanvas.Font.Name := FFontName;
  end;
  if AWeight in [twBold, twMonoBold] then
    FCanvas.Font.Style := [fsBold]
  else
    FCanvas.Font.Style := [];
  FCanvas.Font.Height := -System.Math.Max(1, Round(SF(ASize)));
  FCanvas.Font.Color := AColor;
end;

function TOBDPainter.TextWidth(const AText: string; ASize: Single;
  AWeight: TOBDTextWeight): Integer;
begin
  SetFont(ASize, AWeight, FPalette.ForegroundText);
  Result := FCanvas.TextWidth(AText);
end;

function TOBDPainter.Text(X, CY: Integer; const AText: string;
  ASize: Single; AColor: TColor; AWeight: TOBDTextWeight; AAlign: TAlignment;
  AMaxWidth: Integer): Integer;
var
  R: TRect;
  S: string;
  W, LineH: Integer;
  Format: TTextFormat;
begin
  Result := 0;
  if AText = '' then
    Exit;
  SetFont(ASize, AWeight, AColor);
  W := FCanvas.TextWidth(AText);
  LineH := FCanvas.TextHeight('Ag');
  if (AMaxWidth > 0) and (W > AMaxWidth) then
    W := AMaxWidth;
  case AAlign of
    taRightJustify:
      R := Rect(X - W, CY - LineH, X, CY + LineH);
    taCenter:
      R := Rect(X - W div 2 - 1, CY - LineH, X - W div 2 + W + 1,
        CY + LineH);
  else
    R := Rect(X, CY - LineH, X + W, CY + LineH);
  end;
  // The extra pixel keeps DrawText from ellipsing a string that fits
  // exactly.
  if not (AAlign = taCenter) then
    if AAlign = taRightJustify then
      Dec(R.Left)
    else
      Inc(R.Right);
  Format := [tfSingleLine, tfVerticalCenter, tfNoPrefix, tfEndEllipsis];
  case AAlign of
    taRightJustify:
      Include(Format, tfRight);
    taCenter:
      Include(Format, tfCenter);
  end;
  S := AText;
  FCanvas.Brush.Style := bsClear;
  FCanvas.TextRect(R, S, Format);
  FCanvas.Brush.Style := bsSolid;
  Result := W;
end;

function TOBDPainter.Caps(X, CY: Integer; const AText: string;
  AAlign: TAlignment; AColor: TColor; AMaxWidth: Integer): Integer;
begin
  if AColor = clDefault then
    AColor := FPalette.GaugeLabel;
  Result := Text(X, CY, AnsiUpperCase(AText), 10.5, AColor, twSemibold,
    AAlign, AMaxWidth);
end;

function TOBDPainter.WrapText(const R: TRect; const AText: string;
  ASize: Single; AColor: TColor; AWeight: TOBDTextWeight): Integer;
var
  Box: TRect;
  S: string;
begin
  Result := 0;
  if (AText = '') or (R.Width <= 0) then
    Exit;
  SetFont(ASize, AWeight, AColor);
  Box := R;
  S := AText;
  FCanvas.Brush.Style := bsClear;
  FCanvas.TextRect(Box, S, [tfWordBreak, tfNoPrefix, tfCalcRect]);
  Result := Box.Height;
  Box := R;
  S := AText;
  FCanvas.TextRect(Box, S, [tfWordBreak, tfNoPrefix, tfEndEllipsis]);
  FCanvas.Brush.Style := bsSolid;
end;

{ Glyphs }

procedure TOBDPainter.GlyphCheck(CX, CY: Single; AColor: TColor;
  AScale: Single);
var
  K: Single;
begin
  K := SF(AScale);
  Lines([MakePoint(CX - 5 * K, CY), MakePoint(CX - 1.5 * K, CY + 3.5 * K),
    MakePoint(CX + 5 * K, CY - 4 * K)], AColor, 2 * K);
end;

procedure TOBDPainter.GlyphPending(CX, CY: Single; AColor: TColor;
  AScale: Single);
var
  R: Single;
begin
  R := SF(6 * AScale);
  Ellipse(CX - R, CY - R, 2 * R, 2 * R, clNone, AColor, SF(1.6 * AScale));
  Pie(CX - R, CY - R, 2 * R, 2 * R, 270, 180, AColor);
end;

procedure TOBDPainter.GlyphDash(CX, CY: Single; AColor: TColor;
  AScale: Single);
var
  K: Single;
begin
  K := SF(AScale);
  Lines([MakePoint(CX - 5 * K, CY), MakePoint(CX + 5 * K, CY)], AColor, 2 * K);
end;

procedure TOBDPainter.GlyphRead(CX, CY: Single; AColor: TColor);
var
  K: Single;
begin
  K := SF(1);
  Lines([MakePoint(CX - 5 * K, CY - K), MakePoint(CX, CY + 4 * K),
    MakePoint(CX + 5 * K, CY - K)], AColor, 1.8 * K);
  Lines([MakePoint(CX, CY - 6 * K), MakePoint(CX, CY + 4 * K)], AColor,
    1.8 * K);
end;

procedure TOBDPainter.GlyphClear(CX, CY: Single; AColor: TColor);
var
  K: Single;
begin
  K := SF(1);
  RoundRect(CX - 4 * K, CY - 3 * K, 8 * K, 9 * K, 0, clNone, AColor,
    1.4 * K);
  Lines([MakePoint(CX - 6 * K, CY - 4.5 * K), MakePoint(CX + 6 * K,
    CY - 4.5 * K)], AColor, 1.4 * K);
  Lines([MakePoint(CX - 1.5 * K, CY - 6.5 * K), MakePoint(CX + 1.5 * K,
    CY - 6.5 * K)], AColor, 1.4 * K);
end;

procedure TOBDPainter.GlyphSnapshot(CX, CY: Single; AColor: TColor);
var
  K: Single;
begin
  K := SF(1);
  RoundRect(CX - 7 * K, CY - 4 * K, 14 * K, 10 * K, 2 * K, clNone, AColor,
    1.3 * K);
  Ellipse(CX - 2.6 * K, CY - 1.6 * K, 5.2 * K, 5.2 * K, clNone, AColor,
    1.3 * K);
  RoundRect(CX - 3 * K, CY - 6 * K, 5 * K, 2 * K, 0, AColor, clNone);
end;

procedure TOBDPainter.GlyphChevron(CX, CY: Single; AColor: TColor;
  ADown: Boolean);
var
  K: Single;
begin
  K := SF(1);
  if ADown then
    Lines([MakePoint(CX - 4 * K, CY - 2 * K), MakePoint(CX, CY + 2 * K),
      MakePoint(CX + 4 * K, CY - 2 * K)], AColor, 1.6 * K)
  else
    Lines([MakePoint(CX - 2 * K, CY - 4 * K), MakePoint(CX + 2 * K, CY),
      MakePoint(CX - 2 * K, CY + 4 * K)], AColor, 1.6 * K);
end;

procedure TOBDPainter.GlyphAlert(CX, CY: Single; AColor: TColor);
var
  K: Single;
  Ink: TColor;
begin
  K := SF(1);
  Polygon([MakePoint(CX, CY - 11 * K), MakePoint(CX + 12 * K, CY + 9 * K),
    MakePoint(CX - 12 * K, CY + 9 * K)], AColor);
  if FDark then
    Ink := OnAccent
  else
    Ink := clWhite;
  Text(Round(CX), Round(CY + 2 * K), '!', 13, Ink, twBold, taCenter);
end;

procedure TOBDPainter.Glyph(AGlyph: TOBDGlyph; CX, CY: Single;
  AColor: TColor; AScale: Single);
begin
  case AGlyph of
    glRead:
      GlyphRead(CX, CY, AColor);
    glClear:
      GlyphClear(CX, CY, AColor);
    glSnapshot:
      GlyphSnapshot(CX, CY, AColor);
    glCheck:
      GlyphCheck(CX, CY, AColor, AScale);
    glPending:
      GlyphPending(CX, CY, AColor, AScale);
    glDash:
      GlyphDash(CX, CY, AColor, AScale);
    glAlert:
      GlyphAlert(CX, CY, AColor);
  end;
end;

{ Composite pieces }

procedure TOBDPainter.Card(const R: TRect; AEdge, AFill: TColor);
begin
  if AFill = clDefault then
    AFill := FPalette.GaugeFace;
  FillRect(R, AFill);
  FrameRect(R, FPalette.NeutralLight);
  if AEdge <> clNone then
    FillRect(Rect(R.Left, R.Top, R.Left + S(OBD_EDGE), R.Bottom), AEdge);
end;

function TOBDPainter.ChipWidth(const AText: string; AMono: Boolean;
  ASize: Single): Integer;
var
  Weight: TOBDTextWeight;
begin
  if AMono then
    Weight := twMonoBold
  else
    Weight := twBold;
  Result := TextWidth(AText, ASize, Weight) + S(16);
end;

function TOBDPainter.Chip(X, Y: Integer; const AText: string;
  AColor: TColor; AFilled, AMono: Boolean; ASize: Single): Integer;
var
  H: Integer;
  Ink: TColor;
  Weight: TOBDTextWeight;
begin
  Result := ChipWidth(AText, AMono, ASize);
  H := S(OBD_CHIP_HEIGHT);
  if AMono then
    Weight := twMonoBold
  else
    Weight := twBold;
  if AFilled then
  begin
    RoundRect(X, Y, Result, H, H / 2, AColor, clNone);
    Ink := InkOn(AColor);
  end
  else
  begin
    RoundRect(X, Y, Result, H, H / 2, Tint(AColor),
      OBDMixColor(AColor, FPalette.GaugeFace, 0.45));
    Ink := AColor;
  end;
  Text(X + S(8), Y + H div 2, AText, ASize, Ink, Weight);
end;

function TOBDPainter.BadgeWidth(const AText: string): Integer;
begin
  Result := System.Math.Max(S(20), TextWidth(AText, 10.5, twBold) + S(12));
end;

procedure TOBDPainter.Badge(ARight, ATop: Integer; const AText: string;
  AColor: TColor);
var
  W, H: Integer;
begin
  W := BadgeWidth(AText);
  H := S(18);
  RoundRect(ARight - W, ATop, W, H, H / 2, AColor, clNone);
  Text(ARight - W div 2, ATop + H div 2, AText, 10.5, InkOn(AColor), twBold,
    taCenter);
end;

procedure TOBDPainter.ButtonColors(AKind: TOBDButtonKind;
  AState: TOBDControlState; out AFill, AOutline, AInk: TColor);
var
  Fg: TColor;
  Lift: Single;
begin
  Fg := FPalette.ForegroundText;
  AOutline := clNone;
  case AState of
    cstHover:
      Lift := 0.12;
    cstPressed:
      Lift := 0.24;
  else
    Lift := 0;
  end;
  if AState = cstDisabled then
  begin
    case AKind of
      bkPrimary, bkDanger:
        begin
          AFill := FPalette.NeutralLight;
          AInk := OBDMixColor(FPalette.Subtle, FPalette.NeutralLight, 0.7);
        end;
      bkGhost:
        begin
          AFill := clNone;
          AInk := DisabledText;
        end;
    else
      AFill := FPalette.GaugeFace;
      AOutline := FPalette.NeutralLight;
      AInk := DisabledText;
    end;
    Exit;
  end;
  case AKind of
    bkPrimary:
      begin
        AFill := OBDMixColor(Fg, FPalette.Accent, Lift);
        AInk := OnAccent;
      end;
    bkDanger:
      begin
        AFill := OBDMixColor(Fg, FPalette.Danger, Lift);
        AInk := clWhite;
      end;
    bkDangerOutline:
      begin
        if Lift > 0 then
          AFill := Tint(FPalette.Danger, Lift * 0.8)
        else
          AFill := FPalette.GaugeFace;
        AOutline := FPalette.Danger;
        AInk := DangerInk;
      end;
    bkGhost:
      begin
        if Lift > 0 then
          AFill := Tint(FPalette.Accent, Lift)
        else
          AFill := clNone;
        AInk := AccentText;
      end;
  else
    if Lift > 0 then
    begin
      AFill := OBDMixColor(Fg, FPalette.GaugeFace, Lift * 0.5);
      AOutline := FPalette.NeutralDark;
    end
    else
    begin
      AFill := FPalette.GaugeFace;
      AOutline := FPalette.NeutralLight;
    end;
    AInk := Fg;
  end;
end;

function TOBDPainter.ButtonWidth(const ACaption: string;
  AGlyph: TOBDGlyph): Integer;
begin
  Result := TextWidth(ACaption, 12.5, twSemibold) + S(28);
  if AGlyph <> glNone then
    Inc(Result, S(18));
end;

procedure TOBDPainter.Button(const R: TRect; const ACaption: string;
  AKind: TOBDButtonKind; AState: TOBDControlState; AFocused: Boolean;
  AGlyph: TOBDGlyph);
var
  Fill, Outline, Ink: TColor;
  TW, IW, TX, CY: Integer;
begin
  ButtonColors(AKind, AState, Fill, Outline, Ink);
  FillRect(R, Fill);
  FrameRect(R, Outline);
  if AFocused and (AState <> cstDisabled) then
    FocusRing(R.Left, R.Top, R.Width, R.Height);
  TW := TextWidth(ACaption, 12.5, twSemibold);
  if AGlyph <> glNone then
    IW := S(18)
  else
    IW := 0;
  TX := R.Left + (R.Width - TW - IW) div 2;
  CY := R.Top + R.Height div 2;
  if AGlyph <> glNone then
  begin
    Glyph(AGlyph, TX + SF(6), CY, Ink);
    Inc(TX, IW);
  end;
  Text(TX, CY, ACaption, 12.5, Ink, twSemibold, taLeftJustify,
    R.Right - TX - S(4));
end;

procedure TOBDPainter.FocusRing(X, Y, W, H, ARadius: Single);
var
  Gap, Radius: Single;
begin
  Gap := SF(3);
  if ARadius > 0 then
    Radius := ARadius + Gap
  else
    Radius := SF(2);
  RoundRect(X - Gap, Y - Gap, W + 2 * Gap, H + 2 * Gap, Radius, clNone,
    AccentText, SF(2));
end;

procedure TOBDPainter.CheckBox(X, CY: Integer; ASize: Integer;
  AState: TCheckBoxState; AEnabled, AHover, AFocused: Boolean);
var
  Box, K, Top: Single;
  Fill, Outline, Ink: TColor;
  IsOn: Boolean;
  Width: Single;
begin
  Box := SF(ASize);
  Top := CY - Box / 2;
  IsOn := AState <> cbUnchecked;
  if not AEnabled then
  begin
    if IsOn then
      Fill := FPalette.NeutralLight
    else
      Fill := FPalette.Background;
    Outline := FPalette.NeutralLight;
    Ink := FPalette.Subtle;
  end
  else
  begin
    if IsOn then
      Fill := FPalette.Accent
    else
      Fill := FPalette.GaugeFace;
    if IsOn or AHover then
      Outline := AccentText
    else
      Outline := FPalette.NeutralDark;
    Ink := OnAccent;
  end;
  if AHover and not IsOn and AEnabled then
    Width := SF(1.5)
  else
    Width := SF(1);
  RoundRect(X, Top, Box, Box, SF(2), Fill, Outline, Width);
  K := ASize / 16;
  case AState of
    cbChecked:
      GlyphCheck(X + Box / 2, CY, Ink, 0.72 * K);
    cbGrayed:
      RoundRect(X + SF(4 * K), CY - SF(K), Box - SF(8 * K), SF(2 * K), 0, Ink,
        clNone);
  end;
  if AFocused and AEnabled then
    FocusRing(X, Top, Box, Box, SF(2));
end;

procedure TOBDPainter.RadioButton(X, CY: Integer; ASize: Integer;
  AChecked, AEnabled, AHover, AFocused: Boolean);
var
  Box, Dot, Top, Gap, Width: Single;
  Fill, Outline, Ink: TColor;
begin
  Box := SF(ASize);
  Top := CY - Box / 2;
  if not AEnabled then
  begin
    if AChecked then
      Fill := FPalette.NeutralLight
    else
      Fill := FPalette.Background;
    Outline := FPalette.NeutralLight;
    Ink := FPalette.Subtle;
  end
  else
  begin
    if AChecked then
      Fill := FPalette.Accent
    else
      Fill := FPalette.GaugeFace;
    if AChecked or AHover then
      Outline := AccentText
    else
      Outline := FPalette.NeutralDark;
    Ink := OnAccent;
  end;
  if AHover and not AChecked and AEnabled then
    Width := SF(1.5)
  else
    Width := SF(1);
  Ellipse(X, Top, Box, Box, Fill, Outline, Width);
  if AChecked then
  begin
    Dot := Box * 0.4;
    Ellipse(X + (Box - Dot) / 2, CY - Dot / 2, Dot, Dot, Ink, clNone);
  end;
  if AFocused and AEnabled then
  begin
    Gap := SF(3);
    Ellipse(X - Gap, Top - Gap, Box + 2 * Gap, Box + 2 * Gap, clNone,
      AccentText, SF(2));
  end;
end;

function TOBDPainter.SwitchWidth(AHeight: Integer): Integer;
begin
  Result := Round(SF(AHeight) * 1.9);
end;

procedure TOBDPainter.Switch(X, CY: Integer; AHeight: Integer;
  AOn, AEnabled, AFocused: Boolean);
var
  H, W, Top, Knob, KX: Single;
  KnobColor: TColor;
begin
  H := SF(AHeight);
  W := SwitchWidth(AHeight);
  Top := CY - H / 2;
  if not AEnabled then
  begin
    RoundRect(X, Top, W, H, H / 2, FPalette.NeutralLight, clNone);
    KnobColor := FPalette.Subtle;
  end
  else if AOn then
  begin
    RoundRect(X, Top, W, H, H / 2, FPalette.Accent, clNone);
    KnobColor := OnAccent;
  end
  else
  begin
    RoundRect(X, Top, W, H, H / 2, FPalette.GaugeFace, FPalette.NeutralDark,
      SF(1));
    KnobColor := FPalette.NeutralDark;
  end;
  Knob := H * 0.62;
  if AOn then
    KX := X + W - H / 2 - Knob / 2
  else
    KX := X + H / 2 - Knob / 2;
  Ellipse(KX, CY - Knob / 2, Knob, Knob, KnobColor, clNone);
  if AFocused and AEnabled then
    FocusRing(X, Top, W, H, H / 2);
end;

function TOBDPainter.BannerColor(AKind: TOBDBannerKind): TColor;
begin
  case AKind of
    bnSuccess:
      Result := FPalette.Success;
    bnWarning:
      Result := FPalette.Warning;
    bnDanger:
      Result := FPalette.Danger;
  else
    Result := AccentText;
  end;
end;

procedure TOBDPainter.BannerFrame(const R: TRect; AKind: TOBDBannerKind);
var
  C: TColor;
begin
  C := BannerColor(AKind);
  if FDark then
    FillRect(R, Tint(C, 0.14))
  else if AKind = bnInfo then
    FillRect(R, Tint(C, 0.14))
  else
    FillRect(R, Tint(C, 0.16));
  FrameRect(R, OBDMixColor(C, FPalette.GaugeFace, 0.5));
  FillRect(Rect(R.Left, R.Top, R.Left + S(OBD_EDGE), R.Bottom), C);
end;

procedure TOBDPainter.BannerIcon(CX, CY: Integer; AKind: TOBDBannerKind;
  AAlert: Boolean);
var
  C, Ink: TColor;
  R: Single;
begin
  C := BannerColor(AKind);
  if AAlert then
  begin
    GlyphAlert(CX, CY, C);
    Exit;
  end;
  case AKind of
    bnSuccess:
      GlyphCheck(CX, CY, C, 1.2);
    bnWarning:
      GlyphPending(CX, CY, C, 1.3);
  else
    R := SF(9);
    Ellipse(CX - R, CY - R, 2 * R, 2 * R, C, clNone);
    if AKind = bnDanger then
    begin
      Ink := clWhite;
      Text(CX, CY, '!', 12, Ink, twBold, taCenter);
    end
    else
    begin
      Ink := FPalette.GaugeFace;
      Text(CX, CY, 'i', 12, Ink, twBold, taCenter);
    end;
  end;
end;

procedure TOBDPainter.RangeBar(X, CY, W: Integer; AMin, AMax, ALow, AHigh,
  AValue: Double; ALevel: TOBDAlertLevel);

  function Px(AVal: Double): Integer;
  begin
    if AMax <= AMin then
      Exit(X);
    AVal := EnsureRange(AVal, AMin, AMax);
    Result := X + Round((AVal - AMin) / (AMax - AMin) * W);
  end;

var
  L, H, M: Integer;
  Marker: TColor;
begin
  if W <= 0 then
    Exit;
  FillRect(Rect(X, CY - S(3), X + W, CY + S(3)), FPalette.NeutralLight);
  L := Px(ALow);
  H := Px(AHigh);
  if H < L + 1 then
    H := L + 1;
  FillRect(Rect(L, CY - S(3), H, CY + S(3)),
    OBDMixColor(FPalette.Success, FPalette.GaugeFace, 0.45));
  M := Px(AValue);
  if ALevel = alvNormal then
    Marker := FPalette.ForegroundText
  else
    Marker := LevelColor(ALevel);
  FillRect(Rect(M - S(1.5), CY - S(7), M - S(1.5) + S(3), CY + S(7)), Marker);
end;

procedure TOBDPainter.EditFrame(const R: TRect; AFocused, AEnabled: Boolean);
begin
  if not AEnabled then
  begin
    FillRect(R, FPalette.Background);
    FrameRect(R, FPalette.NeutralLight);
  end
  else if AFocused then
  begin
    FillRect(R, FPalette.GaugeFace);
    FrameRect(R, AccentText, S(2));
  end
  else
  begin
    FillRect(R, FPalette.GaugeFace);
    FrameRect(R, FPalette.NeutralLight);
  end;
end;

end.
