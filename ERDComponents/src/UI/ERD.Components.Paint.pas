//------------------------------------------------------------------------------
//  ERD.Components.Paint
//
//  The look of the Application controls in one place. TERDPainter draws
//  the shared pieces - cards, status chips, counter badges, buttons,
//  check boxes, radio buttons, switches, banners, range bars, edit
//  frames and the small line glyphs - from a theme palette, so a
//  stand-alone TERDChip and a chip inside a DTC panel row are the same
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
//    2026-10-10  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.Paint;

interface

uses
  ERD.Components.Icons,
  ERD.Components.Types,
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
  ERD.Components.GDIP;

type
  /// <summary>Font weight of painted text.</summary>
  TERDTextWeight = (
    /// <summary>Segoe UI.</summary>
    twRegular,
    /// <summary>Segoe UI Semibold.</summary>
    twSemibold,
    /// <summary>Segoe UI bold.</summary>
    twBold,
    /// <summary>Consolas (files, VINs, IDs).</summary>
    twMono,
    /// <summary>Consolas bold.</summary>
    twMonoBold);

  /// <summary>Status a chip, badge or card edge stands for.</summary>
  TERDStatusKind = (
    /// <summary>Muted grey: defaults, unsupported.</summary>
    skNeutral,
    /// <summary>ERDesigns orange: permanent files, application values.
    /// </summary>
    skAccent,
    /// <summary>Green: complete, connected.</summary>
    skSuccess,
    /// <summary>Amber: pending, incomplete.</summary>
    skWarning,
    /// <summary>Red: stored files, faults.</summary>
    skDanger);

  /// <summary>Interaction state of a button or toggle.</summary>
  TERDControlState = (
    /// <summary>At rest.</summary>
    cstNormal,
    /// <summary>Pointer over it.</summary>
    cstHover,
    /// <summary>Mouse button or key held down.</summary>
    cstPressed,
    /// <summary>Enabled = False.</summary>
    cstDisabled);

  /// <summary>Visual weight of a <c>TERDButton</c>.</summary>
  TERDButtonKind = (
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


  /// <summary>Colour and icon of a <c>TERDBanner</c>.</summary>
  TERDBannerKind = (
    /// <summary>Orange "i": neutral information.</summary>
    bnInfo,
    /// <summary>Green check.</summary>
    bnSuccess,
    /// <summary>Amber ring: something still to do.</summary>
    bnWarning,
    /// <summary>Red "!": a fault or a lost connection.</summary>
    bnDanger);

  /// <summary>Paints the shared Application pieces onto a canvas with
  /// the colours of one palette.</summary>
  TERDPainter = class
  strict private
    FCanvas: TCanvas;
    FPalette: TERDThemePalette;
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
    constructor Create(ACanvas: TCanvas; const APalette: TERDThemePalette;
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
    /// <summary>Ink for text and glyphs on an accent fill.</summary>
    /// <returns>Text or background colour, whichever contrasts more
    /// with the accent (dark ink on the orange brand accent, light ink
    /// on the blue Windows highlight).</returns>
    function OnAccent: TColor;
    /// <summary>Ink for text and glyphs on an arbitrary fill.</summary>
    /// <param name="AFill">Fill colour behind the ink.</param>
    /// <returns>Text or background colour, whichever contrasts more
    /// with <c>AFill</c>.</returns>
    function ContrastInk(AFill: TColor): TColor;
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
    function StatusColor(AKind: TERDStatusKind): TColor;
    /// <summary>Text colour of a value at an alert level.</summary>
    /// <param name="ALevel">Alert level.</param>
    /// <returns>Text colour for normal, warning or danger colour.
    /// </returns>
    function LevelColor(ALevel: TERDAlertLevel): TColor;

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
    /// <summary>Anti-aliased dashed rounded-rectangle outline, stroked
    /// inside the shape. Dash and gap lengths are 96-DPI pixels.</summary>
    procedure DashedRoundRect(X, Y, W, H, ARadius: Single; AColor: TColor;
      AWidth: Single = 1; ADash: Single = 5; AGap: Single = 3);
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
    procedure SetFont(ASize: Single; AWeight: TERDTextWeight; AColor: TColor);
    /// <summary>Width of a string in device pixels.</summary>
    function TextWidth(const AText: string; ASize: Single;
      AWeight: TERDTextWeight = twRegular): Integer;
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
      AColor: TColor; AWeight: TERDTextWeight = twRegular;
      AAlign: TAlignment = taLeftJustify; AMaxWidth: Integer = 0): Integer;
    /// <summary>Small upper-case caption (10.5 px semibold) in the
    /// label colour.</summary>
    /// <returns>Drawn width in device pixels.</returns>
    /// <summary>Draws short text (an icon letter such as "i", "!" or a
    /// badge count) centred on its visible ink, not on the font's line
    /// box, so it sits in the optical centre of a circle or pill.</summary>
    /// <param name="CX">Centre X in device pixels.</param>
    /// <param name="CY">Centre Y in device pixels.</param>
    /// <param name="AText">Text.</param>
    /// <param name="ASize">Em height in 96-DPI pixels.</param>
    /// <param name="AColor">Colour.</param>
    /// <param name="AWeight">Weight.</param>
    procedure InkText(CX, CY: Single; const AText: string; ASize: Single;
      AColor: TColor; AWeight: TERDTextWeight = twBold);
    function Caps(X, CY: Integer; const AText: string;
      AAlign: TAlignment = taLeftJustify; AColor: TColor = clDefault;
      AMaxWidth: Integer = 0): Integer;
    /// <summary>Text wrapped into a rectangle, top aligned.</summary>
    /// <returns>Height used, in device pixels.</returns>
    function WrapText(const R: TRect; const AText: string; ASize: Single;
      AColor: TColor; AWeight: TERDTextWeight = twRegular): Integer;

    { Glyphs: centre in device pixels, AScale multiplies the design
      size. }

    procedure GlyphCheck(CX, CY: Single; AColor: TColor; AScale: Single = 1);
    procedure GlyphPending(CX, CY: Single; AColor: TColor; AScale: Single = 1);
    procedure GlyphDash(CX, CY: Single; AColor: TColor; AScale: Single = 1);
    procedure GlyphRead(CX, CY: Single; AColor: TColor);
    procedure GlyphClear(CX, CY: Single; AColor: TColor);
    procedure GlyphSnapshot(CX, CY: Single; AColor: TColor);
    procedure GlyphChevron(CX, CY: Single; AColor: TColor; ADown: Boolean);
    /// <summary>Draws built-in icon AIndex of ERD.Components.IconData (a
    /// TERDGlyph, shell or status icon) centred on (CX, CY).</summary>
    /// <param name="AScale">1 = 16 px. Not snapped; pass
    /// ERDIconScale for line icons.</param>
    /// <param name="AInk">Colour of marks on filled shapes; clNone uses
    /// the face colour.</param>
    /// <param name="AThin">Caption-button stroke weight.</param>
    procedure DrawIcon(AIndex: Integer; CX, CY: Single; AColor: TColor;
      AScale: Single; AInk: TColor = clNone; AThin: Boolean = False);
    /// <summary>Warning triangle with "!", 24 px wide.</summary>
    procedure GlyphAlert(CX, CY: Single; AColor: TColor; AScale: Single = 1);
    /// <summary>Line glyphs for menus, caption buttons, tool bars and
    /// the ribbon: 16 px at scales below 1.5, 32 px up to 2.5.</summary>
    procedure GlyphLine(AGlyph: TERDGlyph; CX, CY: Single; AColor: TColor;
      AScale: Single);
    /// <summary>Draws any built-in glyph.</summary>
    procedure Glyph(AGlyph: TERDGlyph; CX, CY: Single; AColor: TColor;
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
    /// <param name="AMono">Monospace label (DTC files).</param>
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
    procedure ButtonColors(AKind: TERDButtonKind; AState: TERDControlState;
      out AFill, AOutline, AInk: TColor);
    /// <summary>Natural width of a button: caption + 28 px, + 18 px
    /// with a glyph.</summary>
    function ButtonWidth(const ACaption: string; AGlyph: TERDGlyph): Integer;
    /// <summary>Paints a button.</summary>
    procedure Button(const R: TRect; const ACaption: string;
      AKind: TERDButtonKind; AState: TERDControlState; AFocused: Boolean;
      AGlyph: TERDGlyph; AIcon: TERDIcon = nil);
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
    function BannerColor(AKind: TERDBannerKind): TColor;
    /// <summary>Tinted fill of a banner kind.</summary>
    function BannerFill(AKind: TERDBannerKind): TColor;
    /// <summary>Banner surface: tinted fill, mixed outline and a 4 px
    /// edge.</summary>
    procedure BannerFrame(const R: TRect; AKind: TERDBannerKind);
    /// <summary>Banner icon centred on (CX, CY).</summary>
    /// <param name="AAlert">Draw the warning triangle instead of the
    /// kind's own icon.</param>
    /// <param name="AScale">Icon size relative to the default 18 px.
    /// </param>
    procedure BannerIcon(CX, CY: Integer; AKind: TERDBannerKind;
      AAlert: Boolean = False; AScale: Single = 1);
    /// <summary>A value against its normal band: a 6 px track, the
    /// band in green and a 3 x 14 px marker.</summary>
    /// <param name="X">Left edge.</param>
    /// <param name="CY">Vertical centre.</param>
    /// <param name="W">Width.</param>
    procedure RangeBar(X, CY, W: Integer; AMin, AMax, ALow, AHigh,
      AValue: Double; ALevel: TERDAlertLevel);
    /// <summary>Edit / combo box frame.</summary>
    procedure EditFrame(const R: TRect; AFocused, AEnabled: Boolean);

    /// <summary>Target canvas.</summary>
    property Canvas: TCanvas read FCanvas;
    /// <summary>Palette the painter draws with.</summary>
    property Palette: TERDThemePalette read FPalette;
    /// <summary>True on a dark palette.</summary>
    property Dark: Boolean read FDark;
    /// <summary>Pixels per inch the painter scales to.</summary>
    property PPI: Integer read FPPI;
    /// <summary>Base font family. Default Segoe UI.</summary>
    property FontName: string read FFontName write FFontName;
  end;

/// <summary>Builds a floating-point GDI+ point.</summary>
/// <remarks>Unlike the overloaded <c>MakePoint</c>, the result is always
/// a <c>TGPPointF</c>, even when both coordinates are integers.</remarks>
/// <param name="X">Horizontal coordinate.</param>
/// <param name="Y">Vertical coordinate.</param>
/// <returns>Point for the painter's polyline and polygon methods.</returns>
function ERDPointF(X, Y: Single): TGPPointF;

/// <summary>Blends two colours.</summary>
/// <param name="A">Colour at weight 1.</param>
/// <param name="B">Colour at weight 0.</param>
/// <param name="T">Weight of A, 0..1.</param>
/// <returns>Mixed colour.</returns>
function ERDMixColor(A, B: TColor; T: Single): TColor;

/// <summary>True when the palette has a dark background.</summary>
/// <param name="APalette">Palette.</param>
/// <returns>Dark flag.</returns>
function ERDIsDarkPalette(const APalette: TERDThemePalette): Boolean;

/// <summary>Picks the ink for text and glyphs on a fill: the lighter
/// ink wherever it reaches a 3:1 contrast (as Windows does on its blue
/// accent), otherwise whichever ink contrasts more.</summary>
/// <param name="AFill">Fill colour behind the ink.</param>
/// <param name="AInkA">First candidate ink.</param>
/// <param name="AInkB">Second candidate ink.</param>
/// <returns><c>AInkA</c> or <c>AInkB</c>.</returns>
function ERDContrastInk(AFill, AInkA, AInkB: TColor): TColor;

/// <summary>Alert level of a value against a normal band.</summary>
/// <param name="AValue">Value.</param>
/// <param name="ALow">Low end of the normal band.</param>
/// <param name="AHigh">High end of the normal band.</param>
/// <param name="AAlarmMargin">How far outside the band, as a fraction
/// of the band width, a value turns from warning into alarm.</param>
/// <returns>alvNormal inside the band, alvWarning just outside,
/// alvAlarm further out.</returns>
function ERDRangeLevel(AValue, ALow, AHigh, AAlarmMargin: Double)
  : TERDAlertLevel;

/// <summary>Width of a text in the painter fonts, without a canvas of
/// the caller (for AutoSize before a window handle exists).</summary>
/// <param name="AText">Text.</param>
/// <param name="ASize">Em height in 96-DPI pixels.</param>
/// <param name="AWeight">Weight.</param>
/// <param name="APPI">Pixels per inch to measure at.</param>
/// <returns>Width in device pixels.</returns>
function ERDMeasureText(const AText: string; ASize: Single;
  AWeight: TERDTextWeight; APPI: Integer): Integer;

/// <summary>Height of one text line in a font (GDI tmHeight), which is
/// what a borderless TEdit needs to show its text; used to centre child
/// editors vertically in rows and boxes.</summary>
/// <param name="AFont">Font to measure.</param>
/// <returns>Line height in device pixels.</returns>
function ERDFontLineHeight(AFont: TFont): Integer;

/// <summary>Scale factor for list columns the user has resized, so the
/// columns follow the control when it grows or shrinks (maximise,
/// restore) and always fit: the sized columns keep their proportions
/// and the filling column keeps at least <c>AReserve</c>.</summary>
/// <param name="ASized">Sum of the sized column widths, device pixels.</param>
/// <param name="ABase">Content width at the time the columns were sized,
/// device pixels; 0 keeps the widths as they are while they fit.</param>
/// <param name="AAvailable">Current content width, device pixels.</param>
/// <param name="AReserve">Minimum width of the filling column.</param>
/// <returns>Factor to multiply the sized widths with.</returns>
function ERDColumnScale(ASized, ABase, AAvailable, AReserve: Integer): Double;

/// <summary>Snaps an icon scale to the icon grid: 16 px (1) or 32 px
/// (2); scales above 2.5 are kept.</summary>
function ERDIconScale(AScale: Single): Single;

/// <summary>Stroke width of a built-in icon in 96-DPI pixels.</summary>
/// <param name="AScale">Icon scale (1 = 16 px).</param>
/// <param name="AThin">Caption-button weight.</param>
function ERDIconStroke(AScale: Single; AThin: Boolean = False): Single;

const
  /// <summary>Card status edge width, 96-DPI pixels.</summary>
  ERD_EDGE = 4;
  /// <summary>Horizontal padding inside cards, 96-DPI pixels.</summary>
  ERD_PAD = 16;
  /// <summary>Chip height, 96-DPI pixels.</summary>
  ERD_CHIP_HEIGHT = 20;

  { Shared type scale (96-DPI pixels). Every control paints its text in
    one of these sizes, in both densities, so the UI reads as one set;
    only large read-outs (gauge values, hero titles) use their own. }

  /// <summary>Upper-case captions, badges and chips.</summary>
  ERD_TEXT_CAPS = 10.5;
  /// <summary>Secondary text: hints, meta data, shortcuts, legends.</summary>
  ERD_TEXT_SMALL = 11.5;
  /// <summary>Body text: items, values, buttons, edits, bars, menus.</summary>
  ERD_TEXT_BODY = 12.5;
  /// <summary>Section headings, card titles and empty-state messages.</summary>
  ERD_TEXT_HEADING = 14;
  /// <summary>Panel and dialog titles.</summary>
  ERD_TEXT_TITLE = 16;

implementation

uses
  ERD.Components.IconData;

const
  DESIGN_PPI = 96;
  SEMIBOLD_SUFFIX = ' Semibold';
  MONO_FONT = 'Consolas';

function ERDPointF(X, Y: Single): TGPPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function ERDMixColor(A, B: TColor; T: Single): TColor;
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

function ERDIsDarkPalette(const APalette: TERDThemePalette): Boolean;
begin
  Result := Luma(APalette.Background) < 128;
end;

function RelativeLuminance(AColor: TColor): Double;

  function Channel(AValue: Byte): Double;
  var
    V: Double;
  begin
    V := AValue / 255;
    if V <= 0.03928 then
      Result := V / 12.92
    else
      Result := System.Math.Power((V + 0.055) / 1.055, 2.4);
  end;

var
  C: Cardinal;
begin
  // WCAG 2 relative luminance of an sRGB colour.
  C := ColorToRGB(AColor);
  Result := 0.2126 * Channel(GetRValue(C)) + 0.7152 * Channel(GetGValue(C)) +
    0.0722 * Channel(GetBValue(C));
end;

function ContrastRatio(A, B: TColor): Double;
var
  LA, LB: Double;
begin
  LA := RelativeLuminance(A);
  LB := RelativeLuminance(B);
  Result := (System.Math.Max(LA, LB) + 0.05) /
    (System.Math.Min(LA, LB) + 0.05);
end;

function ERDContrastInk(AFill, AInkA, AInkB: TColor): TColor;
var
  LightInk, DarkInk: TColor;
begin
  if RelativeLuminance(AInkA) >= RelativeLuminance(AInkB) then
  begin
    LightInk := AInkA;
    DarkInk := AInkB;
  end
  else
  begin
    LightInk := AInkB;
    DarkInk := AInkA;
  end;
  // Light ink wins on any fill where it reaches 3:1 (WCAG UI and large
  // text), as Windows does on its blue accent; on light fills such as
  // the brand orange or a yellow warning the higher-contrast ink is used.
  if (ContrastRatio(LightInk, AFill) >= 3.0) or
    (ContrastRatio(LightInk, AFill) > ContrastRatio(DarkInk, AFill)) then
    Result := LightInk
  else
    Result := DarkInk;
end;

function ERDColumnScale(ASized, ABase, AAvailable, AReserve: Integer): Double;
begin
  if ASized <= 0 then
    Exit(1);
  if ABase > 0 then
    Result := AAvailable / ABase
  else
    Result := 1;
  if ASized * Result > AAvailable - AReserve then
    Result := System.Math.Max(0, AAvailable - AReserve) / ASized;
end;

function ERDIconScale(AScale: Single): Single;
begin
  if AScale < 1.5 then
    Result := 1
  else if AScale <= 2.5 then
    Result := 2
  else
    Result := AScale;
end;

function ERDIconStroke(AScale: Single; AThin: Boolean): Single;
begin
  if AThin then
  begin
    Result := AScale * 0.75;
    if Result < 1 then
      Result := 1;
  end
  else if AScale <= 1 then
    Result := 1
  else
    Result := 1 + 0.75 * (AScale - 1);
end;

function ERDFontLineHeight(AFont: TFont): Integer;
var
  DC: HDC;
  Old: HGDIOBJ;
  TM: TTextMetric;
begin
  Result := Abs(AFont.Height);
  DC := GetDC(0);
  try
    Old := SelectObject(DC, AFont.Handle);
    try
      if GetTextMetrics(DC, TM) then
        Result := TM.tmHeight;
    finally
      SelectObject(DC, Old);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function ERDMeasureText(const AText: string; ASize: Single;
  AWeight: TERDTextWeight; APPI: Integer): Integer;
var
  Bmp: TBitmap;
  P: TERDPainter;
begin
  if AText = '' then
    Exit(0);
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(1, 1);
    P := TERDPainter.Create(Bmp.Canvas, Default(TERDThemePalette), APPI);
    try
      Result := P.TextWidth(AText, ASize, AWeight);
    finally
      P.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

function ERDRangeLevel(AValue, ALow, AHigh, AAlarmMargin: Double)
  : TERDAlertLevel;
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

{ TERDPainter --------------------------------------------------------------- }

constructor TERDPainter.Create(ACanvas: TCanvas;
  const APalette: TERDThemePalette; APPI: Integer);
begin
  inherited Create;
  FCanvas := ACanvas;
  FPalette := APalette;
  FPPI := APPI;
  if FPPI <= 0 then
    FPPI := DESIGN_PPI;
  FDark := ERDIsDarkPalette(APalette);
  FFontName := 'Segoe UI';
end;

function TERDPainter.S(AValue: Single): Integer;
begin
  Result := Round(AValue * FPPI / DESIGN_PPI);
end;

function TERDPainter.SF(AValue: Single): Single;
begin
  Result := AValue * FPPI / DESIGN_PPI;
end;

function TERDPainter.NewGraphics: TGPGraphics;
begin
  Result := TGPGraphics.Create(FCanvas.Handle);
  Result.SetSmoothingMode(SmoothingModeAntiAlias);
  // Pixel i covers [i, i + 1], so a fill from X to X + W covers
  // exactly the pixels a GDI FillRect would.
  Result.SetPixelOffsetMode(PixelOffsetModeHalf);
end;

{ Colours }

function TERDPainter.Tint(AColor: TColor): TColor;
begin
  if FDark then
    Result := Tint(AColor, 0.22)
  else
    Result := Tint(AColor, 0.13);
end;

function TERDPainter.Tint(AColor: TColor; AStrength: Single): TColor;
begin
  Result := ERDMixColor(AColor, FPalette.GaugeFace, AStrength);
end;

function TERDPainter.HeaderFill: TColor;
begin
  Result := ERDMixColor(FPalette.NeutralLight, FPalette.GaugeFace, 0.45);
end;

function TERDPainter.OnAccent: TColor;
begin
  Result := ContrastInk(FPalette.Accent);
end;

function TERDPainter.ContrastInk(AFill: TColor): TColor;
begin
  Result := ERDContrastInk(AFill, FPalette.ForegroundText,
    FPalette.Background);
end;

function TERDPainter.AccentText: TColor;
begin
  Result := FPalette.GaugeNeedle;
end;

function TERDPainter.DangerInk: TColor;
begin
  if FDark then
    Result := ERDMixColor(FPalette.Danger, clWhite, 0.7)
  else
    Result := FPalette.Danger;
end;

function TERDPainter.DisabledText: TColor;
begin
  Result := ERDMixColor(FPalette.Subtle, FPalette.GaugeFace, 0.6);
end;

function TERDPainter.InkOn(AFill: TColor): TColor;
begin
  if (AFill = FPalette.Accent) or (FDark and (AFill = FPalette.Warning)) then
    Result := ContrastInk(AFill)
  else
    Result := clWhite;
end;

function TERDPainter.StatusColor(AKind: TERDStatusKind): TColor;
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

function TERDPainter.LevelColor(ALevel: TERDAlertLevel): TColor;
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

procedure TERDPainter.FillRect(const R: TRect; AColor: TColor);
begin
  if (AColor = clNone) or R.IsEmpty then
    Exit;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.Color := AColor;
  FCanvas.FillRect(R);
end;

procedure TERDPainter.FrameRect(const R: TRect; AColor: TColor;
  AWidth: Integer);
begin
  if (AColor = clNone) or (AWidth <= 0) then
    Exit;
  FillRect(Rect(R.Left, R.Top, R.Right, R.Top + AWidth), AColor);
  FillRect(Rect(R.Left, R.Bottom - AWidth, R.Right, R.Bottom), AColor);
  FillRect(Rect(R.Left, R.Top, R.Left + AWidth, R.Bottom), AColor);
  FillRect(Rect(R.Right - AWidth, R.Top, R.Right, R.Bottom), AColor);
end;

procedure TERDPainter.HLine(X, Y, W: Integer; AColor: TColor);
begin
  FillRect(Rect(X, Y, X + W, Y + 1), AColor);
end;

procedure TERDPainter.VLine(X, Y, H: Integer; AColor: TColor);
begin
  FillRect(Rect(X, Y, X + 1, Y + H), AColor);
end;

procedure TERDPainter.AddRoundRect(APath: TGPGraphicsPath; X, Y, W, H,
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

procedure TERDPainter.DashedRoundRect(X, Y, W, H, ARadius: Single;
  AColor: TColor; AWidth: Single; ADash: Single; AGap: Single);
var
  G: TGPGraphics;
  Path: TGPGraphicsPath;
  Pen: TGPPen;
  Half: Single;
  Pattern: array[0..1] of Single;
begin
  if (W <= 0) or (H <= 0) or (AColor = clNone) or (AWidth <= 0) then
    Exit;
  G := NewGraphics;
  try
    Half := AWidth / 2;
    Path := TGPGraphicsPath.Create;
    Pen := TGPPen.Create(ColorToARGB(AColor), AWidth);
    try
      // GDI+ dash lengths are in multiples of the pen width.
      Pattern[0] := System.Math.Max(0.1, SF(ADash) / AWidth);
      Pattern[1] := System.Math.Max(0.1, SF(AGap) / AWidth);
      Pen.SetDashPattern(@Pattern[0], 2);
      AddRoundRect(Path, X + Half, Y + Half, W - AWidth, H - AWidth,
        ARadius - Half);
      G.DrawPath(Pen, Path);
    finally
      Pen.Free;
      Path.Free;
    end;
  finally
    G.Free;
  end;
end;

procedure TERDPainter.RoundRect(X, Y, W, H, ARadius: Single;
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

procedure TERDPainter.Ellipse(X, Y, W, H: Single; AFill, AOutline: TColor;
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

procedure TERDPainter.Pie(X, Y, W, H, AStart, ASweep: Single; AColor: TColor);
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

procedure TERDPainter.Lines(const APoints: array of TGPPointF;
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

procedure TERDPainter.Polygon(const APoints: array of TGPPointF;
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

procedure TERDPainter.SetFont(ASize: Single; AWeight: TERDTextWeight;
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

function TERDPainter.TextWidth(const AText: string; ASize: Single;
  AWeight: TERDTextWeight): Integer;
begin
  SetFont(ASize, AWeight, FPalette.ForegroundText);
  Result := FCanvas.TextWidth(AText);
end;

function TERDPainter.Text(X, CY: Integer; const AText: string;
  ASize: Single; AColor: TColor; AWeight: TERDTextWeight; AAlign: TAlignment;
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

procedure TERDPainter.InkText(CX, CY: Single; const AText: string;
  ASize: Single; AColor: TColor; AWeight: TERDTextWeight);
const
  IDENTITY: TMat2 = (
    eM11: (fract: 0; value: 1); eM12: (fract: 0; value: 0);
    eM21: (fract: 0; value: 0); eM22: (fract: 0; value: 1));
var
  DC: HDC;
  GM: TGlyphMetrics;
  I, Advance, L, T, InkL, InkT, InkR, InkB: Integer;
  Any: Boolean;
  OldAlign: UINT;
begin
  if AText = '' then
    Exit;
  SetFont(ASize, AWeight, AColor);
  FCanvas.Brush.Style := bsClear;
  DC := FCanvas.Handle;
  Advance := 0;
  Any := False;
  InkL := 0;
  InkT := 0;
  InkR := 0;
  InkB := 0;
  for I := 1 to Length(AText) do
  begin
    FillChar(GM, SizeOf(GM), 0);
    if GetGlyphOutline(DC, Ord(AText[I]), GGO_METRICS, GM, 0, nil,
      IDENTITY) = DWORD(GDI_ERROR) then
    begin
      FCanvas.Brush.Style := bsSolid;
      Text(Round(CX), Round(CY), AText, ASize, AColor, AWeight, taCenter);
      Exit;
    end;
    if (AText[I] <> ' ') and (GM.gmBlackBoxX > 0) and (GM.gmBlackBoxY > 0) then
    begin
      // Ink box relative to the pen start on the baseline (Y down).
      L := Advance + GM.gmptGlyphOrigin.X;
      T := -GM.gmptGlyphOrigin.Y;
      if not Any then
      begin
        InkL := L;
        InkT := T;
        InkR := L + Integer(GM.gmBlackBoxX);
        InkB := T + Integer(GM.gmBlackBoxY);
        Any := True;
      end
      else
      begin
        InkL := System.Math.Min(InkL, L);
        InkT := System.Math.Min(InkT, T);
        InkR := System.Math.Max(InkR, L + Integer(GM.gmBlackBoxX));
        InkB := System.Math.Max(InkB, T + Integer(GM.gmBlackBoxY));
      end;
    end;
    Inc(Advance, GM.gmCellIncX);
  end;
  if not Any then
  begin
    FCanvas.Brush.Style := bsSolid;
    Exit;
  end;
  OldAlign := SetTextAlign(DC, TA_LEFT or TA_BASELINE or TA_NOUPDATECP);
  try
    ExtTextOut(DC, Round(CX - (InkL + InkR) / 2), Round(CY - (InkT + InkB) / 2),
      0, nil, PChar(AText), Length(AText), nil);
  finally
    SetTextAlign(DC, OldAlign);
    FCanvas.Brush.Style := bsSolid;
  end;
end;

function TERDPainter.Caps(X, CY: Integer; const AText: string;
  AAlign: TAlignment; AColor: TColor; AMaxWidth: Integer): Integer;
begin
  if AColor = clDefault then
    AColor := FPalette.GaugeLabel;
  Result := Text(X, CY, AnsiUpperCase(AText), 10.5, AColor, twSemibold,
    AAlign, AMaxWidth);
end;

function TERDPainter.WrapText(const R: TRect; const AText: string;
  ASize: Single; AColor: TColor; AWeight: TERDTextWeight): Integer;
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

procedure TERDPainter.GlyphCheck(CX, CY: Single; AColor: TColor;
  AScale: Single);
var
  K: Single;
begin
  K := SF(AScale);
  Lines([ERDPointF(CX - 5 * K, CY), ERDPointF(CX - 1.5 * K, CY + 3.5 * K),
    ERDPointF(CX + 5 * K, CY - 4 * K)], AColor, 2 * K);
end;

procedure TERDPainter.GlyphPending(CX, CY: Single; AColor: TColor;
  AScale: Single);
var
  R: Single;
begin
  R := SF(6 * AScale);
  Ellipse(CX - R, CY - R, 2 * R, 2 * R, clNone, AColor, SF(1.6 * AScale));
  Pie(CX - R, CY - R, 2 * R, 2 * R, 270, 180, AColor);
end;

procedure TERDPainter.GlyphDash(CX, CY: Single; AColor: TColor;
  AScale: Single);
var
  K: Single;
begin
  K := SF(AScale);
  Lines([ERDPointF(CX - 5 * K, CY), ERDPointF(CX + 5 * K, CY)], AColor, 2 * K);
end;

procedure TERDPainter.DrawIcon(AIndex: Integer; CX, CY: Single;
  AColor: TColor; AScale: Single; AInk: TColor; AThin: Boolean);
var
  G: TGPGraphics;
  Pen, InkPen: TGPPen;
  Brush, InkBrush: TGPSolidBrush;
  K, LW: Single;
  P, Last, Q: Integer;
  State: GraphicsState;

  function V(I: Integer): Single;
  begin
    Result := ERD_ICON_DATA[I];
  end;

  function Op(I: Integer): Integer;
  begin
    Result := Round(ERD_ICON_DATA[I]);
  end;

  // Offset of the primitive after the one at I.
  function Next(I: Integer): Integer;
  begin
    case Op(I) of
      ERD_ICON_LINE, ERD_ICON_LOOP, ERD_ICON_FILL:
        Result := I + 2 + 2 * Round(V(I + 1));
      ERD_ICON_ARC, ERD_ICON_PIE:
        Result := I + 6;
      ERD_ICON_RING, ERD_ICON_DISC:
        Result := I + 4;
      ERD_ICON_ELLIPSE:
        Result := I + 5;
      ERD_ICON_KNOCK, ERD_ICON_INK:
        Result := Next(I + 1);
    else
      Result := Last;
    end;
  end;

  function Points(I: Integer): TArray<TGPPointF>;
  var
    J: Integer;
  begin
    SetLength(Result, Round(V(I + 1)));
    for J := 0 to High(Result) do
      Result[J] := ERDPointF(CX + V(I + 2 + 2 * J) * K,
        CY + V(I + 3 + 2 * J) * K);
  end;

  // Box of the circle (cx, cy, r) at I + 1.
  function Circle(I: Integer): TGPRectF;
  begin
    Result := RectF(CX + (V(I + 1) - V(I + 3)) * K,
      CY + (V(I + 2) - V(I + 3)) * K, 2 * V(I + 3) * K, 2 * V(I + 3) * K);
  end;

  procedure Draw(I: Integer; APen: TGPPen; ABrush: TGPSolidBrush);
  var
    Pts: TArray<TGPPointF>;
    R: TGPRectF;
  begin
    case Op(I) of
      ERD_ICON_LINE, ERD_ICON_LOOP, ERD_ICON_FILL:
        begin
          Pts := Points(I);
          if Length(Pts) < 2 then
            Exit;
          if Op(I) = ERD_ICON_LINE then
            G.DrawLines(APen, PGPPointF(@Pts[0]), Length(Pts))
          else if Op(I) = ERD_ICON_LOOP then
            G.DrawPolygon(APen, PGPPointF(@Pts[0]), Length(Pts))
          else
            G.FillPolygon(ABrush, PGPPointF(@Pts[0]), Length(Pts));
        end;
      ERD_ICON_ARC:
        begin
          R := Circle(I);
          G.DrawArc(APen, R.X, R.Y, R.Width, R.Height, V(I + 4), V(I + 5));
        end;
      ERD_ICON_PIE:
        begin
          R := Circle(I);
          G.FillPie(ABrush, R.X, R.Y, R.Width, R.Height, V(I + 4), V(I + 5));
        end;
      ERD_ICON_RING:
        begin
          R := Circle(I);
          G.DrawEllipse(APen, R.X, R.Y, R.Width, R.Height);
        end;
      ERD_ICON_DISC:
        begin
          R := Circle(I);
          G.FillEllipse(ABrush, R.X, R.Y, R.Width, R.Height);
        end;
      ERD_ICON_ELLIPSE:
        G.DrawEllipse(APen, CX + V(I + 1) * K, CY + V(I + 2) * K,
          V(I + 3) * K, V(I + 4) * K);
    end;
  end;

  // Excludes the knocked-out primitive at I (1 px wider on each side
  // than its stroke) from the clip.
  procedure Knock(I: Integer);
  var
    Path: TGPGraphicsPath;
    Wide: TGPPen;
    Pts: TArray<TGPPointF>;
    R: TGPRectF;
  begin
    Path := TGPGraphicsPath.Create(FillModeWinding);
    try
      case Op(I) of
        ERD_ICON_LINE, ERD_ICON_LOOP, ERD_ICON_FILL:
          begin
            Pts := Points(I);
            if Length(Pts) < 2 then
              Exit;
            if Op(I) = ERD_ICON_LINE then
              Path.AddLines(PGPPointF(@Pts[0]), Length(Pts))
            else
              Path.AddPolygon(PGPPointF(@Pts[0]), Length(Pts));
          end;
        ERD_ICON_ARC, ERD_ICON_PIE:
          begin
            R := Circle(I);
            if Op(I) = ERD_ICON_ARC then
              Path.AddArc(R.X, R.Y, R.Width, R.Height, V(I + 4), V(I + 5))
            else
              Path.AddPie(R.X, R.Y, R.Width, R.Height, V(I + 4), V(I + 5));
          end;
        ERD_ICON_RING, ERD_ICON_DISC:
          begin
            R := Circle(I);
            Path.AddEllipse(R.X, R.Y, R.Width, R.Height);
          end;
        ERD_ICON_ELLIPSE:
          Path.AddEllipse(CX + V(I + 1) * K, CY + V(I + 2) * K,
            V(I + 3) * K, V(I + 4) * K);
      end;
      if Op(I) in [ERD_ICON_LINE, ERD_ICON_LOOP, ERD_ICON_ARC, ERD_ICON_RING,
        ERD_ICON_ELLIPSE] then
      begin
        Wide := TGPPen.Create(ColorToARGB(clBlack), LW + SF(2));
        try
          Wide.SetStartCap(LineCapRound);
          Wide.SetEndCap(LineCapRound);
          Wide.SetLineJoin(LineJoinRound);
          Path.Widen(Wide);
        finally
          Wide.Free;
        end;
        Path.SetFillMode(FillModeWinding);
      end;
      G.SetClip(Path, CombineModeExclude);
    finally
      Path.Free;
    end;
  end;

  function NewPen(AInkColor: TColor; AWidth: Single): TGPPen;
  begin
    Result := TGPPen.Create(ColorToARGB(AInkColor), AWidth);
    Result.SetStartCap(LineCapRound);
    Result.SetEndCap(LineCapRound);
    Result.SetLineJoin(LineJoinRound);
  end;

begin
  if (AIndex < 0) or (AIndex >= ERD_ICON_COUNT) then
    Exit;
  P := ERD_ICON_START[AIndex];
  Last := ERD_ICON_START[AIndex + 1];
  if P >= Last then
    Exit;
  if AInk = clNone then
    AInk := FPalette.GaugeFace;
  // Grid points on pixel centres: 1 px strokes stay crisp.
  CX := Floor(CX) + 0.5;
  CY := Floor(CY) + 0.5;
  K := SF(AScale);
  LW := SF(ERDIconStroke(AScale, AThin));
  G := NewGraphics;
  Pen := nil;
  InkPen := nil;
  Brush := nil;
  InkBrush := nil;
  try
    Pen := NewPen(AColor, LW);
    InkPen := NewPen(AInk, LW);
    Brush := TGPSolidBrush.Create(ColorToARGB(AColor));
    InkBrush := TGPSolidBrush.Create(ColorToARGB(AInk));
    while P < Last do
    begin
      if Op(P) <> ERD_ICON_KNOCK then
      begin
        // Every knock-out after this primitive cuts into it.
        State := G.Save;
        Q := Next(P);
        while Q < Last do
        begin
          if Op(Q) = ERD_ICON_KNOCK then
            Knock(Q + 1);
          Q := Next(Q);
        end;
        if Op(P) = ERD_ICON_INK then
        begin
          // Ink marks on filled shapes read better a little bolder.
          if Op(P + 1) = ERD_ICON_LINE then
            InkPen.SetWidth(LW * 1.6)
          else
            InkPen.SetWidth(LW);
          Draw(P + 1, InkPen, InkBrush);
        end
        else
          Draw(P, Pen, Brush);
        G.Restore(State);
      end;
      P := Next(P);
    end;
  finally
    InkBrush.Free;
    Brush.Free;
    InkPen.Free;
    Pen.Free;
    G.Free;
  end;
end;

procedure TERDPainter.GlyphRead(CX, CY: Single; AColor: TColor);
begin
  DrawIcon(Ord(glDownload), CX, CY, AColor, 1);
end;

procedure TERDPainter.GlyphClear(CX, CY: Single; AColor: TColor);
begin
  DrawIcon(Ord(glClose), CX, CY, AColor, 1);
end;

procedure TERDPainter.GlyphSnapshot(CX, CY: Single; AColor: TColor);
begin
  DrawIcon(Ord(glCopy), CX, CY, AColor, 1);
end;

procedure TERDPainter.GlyphChevron(CX, CY: Single; AColor: TColor;
  ADown: Boolean);
begin
  if ADown then
    DrawIcon(Ord(glChevronDown), CX, CY, AColor, 1)
  else
    DrawIcon(Ord(glChevronRight), CX, CY, AColor, 1);
end;

procedure TERDPainter.GlyphAlert(CX, CY: Single; AColor: TColor;
  AScale: Single);
var
  Ink: TColor;
begin
  if FDark then
    Ink := OnAccent
  else
    Ink := clWhite;
  // The status triangle is 17 px wide; this glyph is 24 px.
  DrawIcon(ERD_ICON_STATUS_FIRST + Ord(bnWarning), CX, CY, AColor,
    AScale * 24 / 17, Ink);
end;

procedure TERDPainter.GlyphLine(AGlyph: TERDGlyph; CX, CY: Single;
  AColor: TColor; AScale: Single);
begin
  // The dialog launcher is the one icon with a smaller (12 px) size.
  if (AGlyph = glLauncher) and (AScale < 0.9) then
    AScale := 0.75
  else
    AScale := ERDIconScale(AScale);
  DrawIcon(Ord(AGlyph), CX, CY, AColor, AScale, clNone,
    AGlyph in [glMinimize, glMaximize, glRestore, glClose]);
end;

procedure TERDPainter.Glyph(AGlyph: TERDGlyph; CX, CY: Single;
  AColor: TColor; AScale: Single);
begin
  case AGlyph of
    glCheck:
      GlyphCheck(CX, CY, AColor, AScale);
    glPending:
      GlyphPending(CX, CY, AColor, AScale);
    glDash:
      GlyphDash(CX, CY, AColor, AScale);
    glAlert:
      GlyphAlert(CX, CY, AColor);
    glNone:
      ;
  else
    GlyphLine(AGlyph, CX, CY, AColor, AScale);
  end;
end;

{ Composite pieces }

procedure TERDPainter.Card(const R: TRect; AEdge, AFill: TColor);
begin
  if AFill = clDefault then
    AFill := FPalette.GaugeFace;
  FillRect(R, AFill);
  FrameRect(R, FPalette.NeutralLight);
  if AEdge <> clNone then
    FillRect(Rect(R.Left, R.Top, R.Left + S(ERD_EDGE), R.Bottom), AEdge);
end;

function TERDPainter.ChipWidth(const AText: string; AMono: Boolean;
  ASize: Single): Integer;
var
  Weight: TERDTextWeight;
begin
  if AMono then
    Weight := twMonoBold
  else
    Weight := twBold;
  Result := TextWidth(AText, ASize, Weight) + S(16);
end;

function TERDPainter.Chip(X, Y: Integer; const AText: string;
  AColor: TColor; AFilled, AMono: Boolean; ASize: Single): Integer;
var
  H: Integer;
  Ink: TColor;
  Weight: TERDTextWeight;
begin
  Result := ChipWidth(AText, AMono, ASize);
  H := S(ERD_CHIP_HEIGHT);
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
      ERDMixColor(AColor, FPalette.GaugeFace, 0.45));
    Ink := AColor;
  end;
  Text(X + S(8), Y + H div 2, AText, ASize, Ink, Weight);
end;

function TERDPainter.BadgeWidth(const AText: string): Integer;
begin
  Result := System.Math.Max(S(20), TextWidth(AText, 10.5, twBold) + S(12));
end;

procedure TERDPainter.Badge(ARight, ATop: Integer; const AText: string;
  AColor: TColor);
var
  W, H: Integer;
begin
  W := BadgeWidth(AText);
  H := S(18);
  RoundRect(ARight - W, ATop, W, H, H / 2, AColor, clNone);
  InkText(ARight - W / 2, ATop + H / 2, AText, 10.5, InkOn(AColor), twBold);
end;

procedure TERDPainter.ButtonColors(AKind: TERDButtonKind;
  AState: TERDControlState; out AFill, AOutline, AInk: TColor);
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
          AInk := ERDMixColor(FPalette.Subtle, FPalette.NeutralLight, 0.7);
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
        AFill := ERDMixColor(Fg, FPalette.Accent, Lift);
        AInk := OnAccent;
      end;
    bkDanger:
      begin
        AFill := ERDMixColor(Fg, FPalette.Danger, Lift);
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
      AFill := ERDMixColor(Fg, FPalette.GaugeFace, Lift * 0.5);
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

function TERDPainter.ButtonWidth(const ACaption: string;
  AGlyph: TERDGlyph): Integer;
begin
  Result := TextWidth(ACaption, 12.5, twSemibold) + S(28);
  if AGlyph <> glNone then
    Inc(Result, S(18));
end;

procedure TERDPainter.Button(const R: TRect; const ACaption: string;
  AKind: TERDButtonKind; AState: TERDControlState; AFocused: Boolean;
  AGlyph: TERDGlyph; AIcon: TERDIcon);
var
  Fill, Outline, Ink: TColor;
  TW, IW, TX, CY: Integer;
  HasIcon: Boolean;
begin
  HasIcon := AGlyph <> glNone;
  if AIcon <> nil then HasIcon := AIcon.Available(AGlyph);
  ButtonColors(AKind, AState, Fill, Outline, Ink);
  FillRect(R, Fill);
  FrameRect(R, Outline);
  if AFocused and (AState <> cstDisabled) then
    FocusRing(R.Left, R.Top, R.Width, R.Height);
  TW := TextWidth(ACaption, 12.5, twSemibold);
  if HasIcon then
    IW := S(18)
  else
    IW := 0;
  CY := R.Top + R.Height div 2;
  if ACaption = '' then
  begin
    // A glyph-only button: the glyph sits in the middle.
    if HasIcon then
      if AIcon <> nil then
        AIcon.Draw(FCanvas, FPalette, FPPI, R.Left + R.Width div 2, CY, Ink, 1, AState <> cstDisabled, AGlyph)
      else Glyph(AGlyph, R.Left + R.Width / 2, CY, Ink);
    Exit;
  end;
  TX := R.Left + (R.Width - TW - IW) div 2;
  if HasIcon then
  begin
    if AIcon <> nil then
      AIcon.Draw(FCanvas, FPalette, FPPI, TX + S(7), CY, Ink, 1, AState <> cstDisabled, AGlyph)
    else Glyph(AGlyph, TX + SF(7), CY, Ink);
    Inc(TX, IW);
  end;
  Text(TX, CY, ACaption, 12.5, Ink, twSemibold, taLeftJustify,
    R.Right - TX - S(4));
end;

procedure TERDPainter.FocusRing(X, Y, W, H, ARadius: Single);
var
  Inset, Radius: Single;
begin
  // Keep the stroke inside its control/item bounds, including rows that
  // touch a clipped viewport edge. A 2px stroke needs a 1px centre inset.
  Inset := SF(1);
  if (W <= 2 * Inset) or (H <= 2 * Inset) then Exit;
  if ARadius > 0 then
    Radius := System.Math.Max(0, ARadius - Inset)
  else
    Radius := SF(2);
  RoundRect(X + Inset, Y + Inset, W - 2 * Inset, H - 2 * Inset, Radius,
    clNone, AccentText, SF(2));
end;

procedure TERDPainter.CheckBox(X, CY: Integer; ASize: Integer;
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

procedure TERDPainter.RadioButton(X, CY: Integer; ASize: Integer;
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

function TERDPainter.SwitchWidth(AHeight: Integer): Integer;
begin
  Result := Round(SF(AHeight) * 1.9);
end;

procedure TERDPainter.Switch(X, CY: Integer; AHeight: Integer;
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

function TERDPainter.BannerColor(AKind: TERDBannerKind): TColor;
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

function TERDPainter.BannerFill(AKind: TERDBannerKind): TColor;
begin
  Result := Tint(BannerColor(AKind), 0.14);
end;

procedure TERDPainter.BannerFrame(const R: TRect; AKind: TERDBannerKind);
var
  C: TColor;
begin
  C := BannerColor(AKind);
  FillRect(R, BannerFill(AKind));
  FrameRect(R, ERDMixColor(C, FPalette.GaugeFace, 0.5));
  FillRect(Rect(R.Left, R.Top, R.Left + S(ERD_EDGE), R.Bottom), C);
end;

procedure TERDPainter.BannerIcon(CX, CY: Integer; AKind: TERDBannerKind;
  AAlert: Boolean; AScale: Single);
var
  C, Ink: TColor;
begin
  C := BannerColor(AKind);
  if AScale <= 0 then
    AScale := 1;
  if AAlert then
  begin
    GlyphAlert(CX, CY, C, AScale);
    Exit;
  end;
  case AKind of
    bnDanger:
      Ink := clWhite;
    bnSuccess:
      if FDark then
        Ink := OnAccent
      else
        Ink := clWhite;
  else
    Ink := OnAccent;
  end;
  DrawIcon(ERD_ICON_STATUS_FIRST + Ord(AKind), CX, CY, C, AScale, Ink);
end;

procedure TERDPainter.RangeBar(X, CY, W: Integer; AMin, AMax, ALow, AHigh,
  AValue: Double; ALevel: TERDAlertLevel);

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
    ERDMixColor(FPalette.Success, FPalette.GaugeFace, 0.45));
  M := Px(AValue);
  if ALevel = alvNormal then
    Marker := FPalette.ForegroundText
  else
    Marker := LevelColor(ALevel);
  FillRect(Rect(M - S(1.5), CY - S(7), M - S(1.5) + S(3), CY + S(7)), Marker);
end;

procedure TERDPainter.EditFrame(const R: TRect; AFocused, AEnabled: Boolean);
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
