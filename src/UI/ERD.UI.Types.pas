// ------------------------------------------------------------------------------
// ERD.UI.Types
//
// Shared types for the visual UI surface: theme palette, theme
// mode, per-component style overrides, brand defaults, and the
// resolution chain helpers every visual uses. Also the data-state,
// alert-level and unit-system enums shared by the dashboard set,
// and the desktop / tablet density metrics of the OBD Studio controls.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.UI.Types;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  System.UITypes,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Themes;

type
  /// <summary>Theme mode — auto follows the active VCL Style's
  /// background luma (dark style ⇒ dark palette); explicit
  /// modes force a palette.</summary>
  TOBDThemeMode = (
    /// <summary>ERDesigns light or dark palette, picked from the
    /// active VCL Style's background luma.</summary>
    tmAuto,
    /// <summary>ERDesigns light palette.</summary>
    tmLight,
    /// <summary>ERDesigns dark palette.</summary>
    tmDark,
    /// <summary>Windows / VCL system colours (clWindow, clWindowText,
    /// clHighlight, clBtnFace, ...), following the active VCL Style
    /// when one is applied. See <see cref="WindowsPalette"/>.</summary>
    tmWindows);

  /// <summary>One palette. Every visual reads only these slots —
  /// the resolution chain (per-component Style ▸ Theme ▸ active
  /// VCL Style ▸ built-in brand default) populates the record.
  /// </summary>
  TOBDThemePalette = record
    /// <summary>Component background fill.</summary>
    Background: TColor;
    /// <summary>Body / value text.</summary>
    ForegroundText: TColor;
    /// <summary>Accent fill: value bars, checked boxes, sparklines,
    /// selection. Default = ERDesigns orange.</summary>
    Accent: TColor;
    /// <summary>Subtle borders / dividers / disabled state.</summary>
    Subtle: TColor;
    /// <summary>OK / pass / completed.</summary>
    Success: TColor;
    /// <summary>Caution / approaching limit / pending.</summary>
    Warning: TColor;
    /// <summary>Failure / overrange / fault.</summary>
    Danger: TColor;
    /// <summary>Light neutral (panel cards on light theme,
    /// disabled controls on dark).</summary>
    NeutralLight: TColor;
    /// <summary>Dark neutral (panel cards on dark theme,
    /// disabled controls on light).</summary>
    NeutralDark: TColor;
    /// <summary>Gauge dial face.</summary>
    GaugeFace: TColor;
    /// <summary>Gauge tick marks.</summary>
    GaugeTick: TColor;
    /// <summary>Gauge needle / value indicator.</summary>
    GaugeNeedle: TColor;
    /// <summary>Gauge label text.</summary>
    GaugeLabel: TColor;
  end;

  /// <summary>Per-component colour / styling overrides. Each
  /// field defaults to <c>clDefault</c> (= "inherit from the
  /// resolved Theme palette"). Set any slot to override just
  /// that one — the rest still come from Theme.</summary>
  TOBDVisualStyle = record
    Background: TColor;
    Foreground: TColor;
    Accent: TColor;
    Border: TColor;
    /// <summary>Resets every slot to <c>clDefault</c>.</summary>
    procedure Reset;
    /// <summary>True iff at least one slot is non-default.</summary>
    function HasAny: Boolean;
  end;

  /// <summary>What a data-driven control currently knows about its
  /// value. Every dashboard control paints each state distinctly, so a
  /// control never shows a blank box.</summary>
  TOBDDataState = (
    /// <summary>No value has arrived yet (not connected, PID not
    /// polled, or the channel is unbound).</summary>
    dstNoData,
    /// <summary>A fresh value inside the configured range.</summary>
    dstLive,
    /// <summary>The last value is older than the channel's
    /// <c>StaleAfterMs</c>; it is still shown, but greyed.</summary>
    dstStale,
    /// <summary>The last value fell outside <c>Min..Max</c>; the
    /// control pins to the scale end and flags the overflow.</summary>
    dstOutOfRange);

  /// <summary>Severity of the current value against the control's
  /// warning / alarm thresholds.</summary>
  TOBDAlertLevel = (
    /// <summary>Inside the normal band.</summary>
    alvNormal,
    /// <summary>Past a warning threshold.</summary>
    alvWarning,
    /// <summary>Past an alarm threshold.</summary>
    alvAlarm);

  /// <summary>Measurement system used for displayed values. Values
  /// are always stored in the metric unit the PID decoder delivers;
  /// conversion happens at paint time only.</summary>
  TOBDUnitSystem = (
    /// <summary>SI / metric (km/h, degrees C, kPa, L).</summary>
    usMetric,
    /// <summary>US customary (mph, degrees F, psi, gal).</summary>
    usImperial);

  /// <summary>Row height and hit-target size of the OBD Studio
  /// controls. Fonts stay the same in both densities.</summary>
  TOBDDensity = (
    /// <summary>Compact rows tuned for mouse use.</summary>
    dnDesktop,
    /// <summary>Touch targets of 44 px or more, for workshop
    /// tablets.</summary>
    dnTablet);

  /// <summary>Sizes, in 96-DPI logical pixels, that a density
  /// resolves to. Controls scale them with <c>ScaleValue</c>.
  /// </summary>
  TOBDDensityMetrics = record
    /// <summary>List row (DTC panel, range editor).</summary>
    Row: Integer;
    /// <summary>Panel header with title and actions.</summary>
    Head: Integer;
    /// <summary>Column header strip.</summary>
    ColHead: Integer;
    /// <summary>Panel footer.</summary>
    Foot: Integer;
    /// <summary>Compact row (freeze frame, inspector).</summary>
    CompactRow: Integer;
    /// <summary>Cell of a value grid (inline freeze frame).</summary>
    Cell: Integer;
    /// <summary>Button height.</summary>
    Button: Integer;
    /// <summary>Navigation item (sidebar).</summary>
    Nav: Integer;
    /// <summary>Check box and radio button glyph.</summary>
    Check: Integer;
    /// <summary>Switch track height.</summary>
    Switch: Integer;
    /// <summary>Segmented control height.</summary>
    Segment: Integer;
    /// <summary>Edit and combo box height.</summary>
    Edit: Integer;
    /// <summary>Form caption (title bar) height.</summary>
    TitleBar: Integer;
    /// <summary>Menu bar below the caption.</summary>
    MenuBar: Integer;
    /// <summary>Item row of a popup menu.</summary>
    MenuItem: Integer;
    /// <summary>Status bar height.</summary>
    StatusBar: Integer;
    /// <summary>Tab strip height (tabs, ribbon tabs).</summary>
    Tab: Integer;
    /// <summary>Width of a system caption button (minimise,
    /// maximise, close).</summary>
    CaptionButton: Integer;
  end;

const
  /// <summary>ERDesigns logo orange, #F08818 (<c>--clr-primary</c>,
  /// light theme). A fill colour: needs dark ink on top.</summary>
  clOBDOrange: TColor = $001888F0;
  /// <summary>ERDesigns strong orange, #B4530A
  /// (<c>--clr-primary-strong</c>, light theme). The orange used as
  /// text, outlines and indicators on light surfaces.</summary>
  clOBDOrangeStrong: TColor = $000A53B4;
  /// <summary>ERDesigns dark-theme orange, #F0923A
  /// (<c>--clr-primary</c>, dark theme).</summary>
  clOBDOrangeDark: TColor = $003A92F0;
  /// <summary>ERDesigns dark page background, #1A1B1E
  /// (<c>--clr-bg</c>, dark theme).</summary>
  clOBDCharcoal: TColor = $001E1B1A;
  /// <summary>ERDesigns light card surface, #F8F9FA
  /// (<c>--clr-surface</c>, light theme).</summary>
  clOBDSilver: TColor = $00FAF9F8;

  /// <summary>ERDesigns palette, light mode. Taken from the light
  /// tokens of the ERDesigns design system (erdesigns.be site.css):
  /// Background <c>--clr-bg</c> #F1F3F5, text <c>--clr-text</c>
  /// #1A1A1A, Accent <c>--clr-primary</c> #F08818, Subtle and labels
  /// <c>--clr-text-muted</c> #5C636A, faces <c>--clr-surface</c>
  /// #F8F9FA, NeutralLight <c>--clr-border</c> #DEE2E6, needle
  /// <c>--clr-primary-strong</c> #B4530A, status
  /// <c>--clr-success</c> #28A745 / warning #856404 (the site's
  /// warning text colour; <c>--clr-warning</c> #FFC107 is too light
  /// on the light faces) / <c>--clr-error</c> #DC3545.</summary>
  BRAND_PALETTE_LIGHT: TOBDThemePalette = (Background: $00F5F3F1;
    ForegroundText: $001A1A1A; Accent: $001888F0; Subtle: $006A635C;
    Success: $0045A728; Warning: $00046485; Danger: $004535DC;
    NeutralLight: $00E6E2DE; NeutralDark: $006A635C; GaugeFace: $00FAF9F8;
    GaugeTick: $001A1A1A; GaugeNeedle: $000A53B4; GaugeLabel: $006A635C;);

  /// <summary>ERDesigns palette, dark mode. Taken from the
  /// <c>[data-theme="dark"]</c> tokens of the ERDesigns design system
  /// (erdesigns.be site.css): Background <c>--clr-bg</c> #1A1B1E,
  /// text <c>--clr-text</c> #C1C2C5, Accent and needle
  /// <c>--clr-primary</c> #F0923A, Subtle, labels and NeutralDark
  /// <c>--clr-text-muted</c> #B0B1B5, faces <c>--clr-surface</c>
  /// #25262B, NeutralLight <c>--clr-border</c> #373A40, status
  /// <c>--clr-success</c> #28A745 / <c>--clr-warning</c> #FFC107 /
  /// <c>--clr-error</c> #DC3545.</summary>
  BRAND_PALETTE_DARK: TOBDThemePalette = (Background: $001E1B1A;
    ForegroundText: $00C5C2C1; Accent: $003A92F0; Subtle: $00B5B1B0;
    Success: $0045A728; Warning: $0007C1FF; Danger: $004535DC;
    NeutralLight: $00403A37; NeutralDark: $00B5B1B0; GaugeFace: $002B2625;
    GaugeTick: $00C5C2C1; GaugeNeedle: $003A92F0; GaugeLabel: $00B5B1B0;);

  /// <summary>Returns the active VCL Style's "is dark" flag —
  /// True when the style's window background is darker than 50%
  /// luma. Falls back to System (light) when no style is active.
  /// Used by <c>TOBDTheme.Mode = tmAuto</c> to pick light vs
  /// dark.</summary>
function VCLStyleIsDark: Boolean;

/// <summary>Returns the active VCL Style's value for
/// <c>AStyleColor</c>, or <c>ADefault</c> when no style is
/// active. Wrapper for the verbose
/// <c>TStyleManager.ActiveStyle.GetStyleColor</c> call.</summary>
function StyleColor(AStyleColor: TStyleColor; ADefault: TColor): TColor;

/// <summary>Builds a palette from the Windows / VCL system colours:
/// background <c>clWindow</c>, text <c>clWindowText</c>, accent and
/// needle <c>clHighlight</c>, cards and dial face <c>clBtnFace</c>,
/// dividers <c>clGrayText</c>. Status colours use the Windows
/// success / caution / critical colours, in their light or dark
/// variant depending on the window colour. System colours are
/// resolved through the active VCL Style and returned as plain RGB,
/// so the palette is safe to hand to GDI+.</summary>
/// <returns>Palette for <c>TOBDTheme.Mode = tmWindows</c>.</returns>
function WindowsPalette: TOBDThemePalette;

/// <summary>Picks <c>AOverride</c> when it isn't
/// <c>clDefault</c>; otherwise returns <c>AInherit</c>.
/// One-liner for every resolution step inside paint code.</summary>
function PickColor(AOverride, AInherit: TColor): TColor; inline;

/// <summary>Sizes for a density.</summary>
/// <param name="ADensity">Density to resolve.</param>
/// <returns>Logical-pixel metrics at 96 DPI.</returns>
function DensityMetrics(ADensity: TOBDDensity): TOBDDensityMetrics;

implementation

{ TOBDVisualStyle ------------------------------------------------------------ }

procedure TOBDVisualStyle.Reset;
begin
  Background := clDefault;
  Foreground := clDefault;
  Accent := clDefault;
  Border := clDefault;
end;

function TOBDVisualStyle.HasAny: Boolean;
begin
  Result := (Background <> clDefault) or (Foreground <> clDefault) or
    (Accent <> clDefault) or (Border <> clDefault);
end;

{ Helpers -------------------------------------------------------------------- }

function VCLStyleIsDark: Boolean;
var
  C: TColor;
  R, G, B: Byte;
  Luma: Integer;
begin
  Result := False;
  try
    if TStyleManager.IsCustomStyleActive then
    begin
      C := TStyleManager.ActiveStyle.GetStyleColor(scWindow);
      R := GetRValue(ColorToRGB(C));
      G := GetGValue(ColorToRGB(C));
      B := GetBValue(ColorToRGB(C));
      // Rec. 709 luma. Below 50% mid-grey = dark style.
      Luma := (R * 21 + G * 72 + B * 7) div 100;
      Result := Luma < 128;
    end;
  except
    // Some constrained IDE / runtime configurations strip
    // TStyleManager. Fall back to "light" — better than
    // exceptioning out of a paint cycle.
    Result := False;
  end;
end;

function StyleColor(AStyleColor: TStyleColor; ADefault: TColor): TColor;
begin
  Result := ADefault;
  try
    if TStyleManager.IsCustomStyleActive then
      Result := TStyleManager.ActiveStyle.GetStyleColor(AStyleColor);
  except
    Result := ADefault;
  end;
end;

function SystemColor(AColor: TColor): TColor;
begin
  Result := AColor;
  try
    Result := StyleServices.GetSystemColor(AColor);
  except
    Result := AColor;
  end;
  Result := ColorToRGB(Result);
end;

function IsDarkColor(AColor: TColor): Boolean;
var
  C: TColor;
begin
  C := ColorToRGB(AColor);
  // Rec. 709 luma. Below 50% mid-grey = dark.
  Result := (GetRValue(C) * 21 + GetGValue(C) * 72 + GetBValue(C) * 7)
    div 100 < 128;
end;

function WindowsPalette: TOBDThemePalette;
begin
  Result.Background := SystemColor(clWindow);
  Result.ForegroundText := SystemColor(clWindowText);
  Result.Accent := SystemColor(clHighlight);
  Result.Subtle := SystemColor(clGrayText);
  Result.NeutralLight := SystemColor(clBtnFace);
  Result.NeutralDark := SystemColor(clBtnShadow);
  Result.GaugeFace := SystemColor(clBtnFace);
  Result.GaugeTick := SystemColor(clWindowText);
  Result.GaugeNeedle := SystemColor(clHighlight);
  Result.GaugeLabel := SystemColor(clWindowText);
  if IsDarkColor(Result.Background) then
  begin
    // Windows dark-mode status colours: #6CCB5F / #FCE100 / #FF99A4.
    Result.Success := $005FCB6C;
    Result.Warning := $0000E1FC;
    Result.Danger := $00A499FF;
  end
  else
  begin
    // Windows light-mode status colours: #0F7B0F / #9D5D00 / #C42B1C.
    Result.Success := $000F7B0F;
    Result.Warning := $00005D9D;
    Result.Danger := $001C2BC4;
  end;
end;

function PickColor(AOverride, AInherit: TColor): TColor;
begin
  if AOverride <> clDefault then
    Result := AOverride
  else
    Result := AInherit;
end;

function DensityMetrics(ADensity: TOBDDensity): TOBDDensityMetrics;
begin
  if ADensity = dnTablet then
  begin
    Result.Row := 56;
    Result.Head := 68;
    Result.ColHead := 32;
    Result.Foot := 60;
    Result.CompactRow := 44;
    Result.Cell := 44;
    Result.Button := 44;
    Result.Nav := 52;
    Result.Check := 22;
    Result.Switch := 24;
    Result.Segment := 44;
    Result.Edit := 44;
    Result.TitleBar := 48;
    Result.MenuBar := 44;
    Result.MenuItem := 44;
    Result.StatusBar := 36;
    Result.Tab := 48;
    Result.CaptionButton := 56;
  end
  else
  begin
    Result.Row := 44;
    Result.Head := 56;
    Result.ColHead := 28;
    Result.Foot := 40;
    Result.CompactRow := 30;
    Result.Cell := 32;
    Result.Button := 30;
    Result.Nav := 38;
    Result.Check := 16;
    Result.Switch := 16;
    Result.Segment := 24;
    Result.Edit := 26;
    Result.TitleBar := 40;
    Result.MenuBar := 30;
    Result.MenuItem := 30;
    Result.StatusBar := 28;
    Result.Tab := 36;
    Result.CaptionButton := 46;
  end;
end;

end.
