//------------------------------------------------------------------------------
//  OBD.UI.Types
//
//  Shared types for the visual UI surface: theme palette, theme
//  mode, per-component style overrides, brand defaults, and the
//  resolution chain helpers every visual uses.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Types;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics;

type
  /// <summary>Theme mode — auto follows the active VCL Style's
  /// background luma (dark style ⇒ dark palette); explicit
  /// modes force a palette.</summary>
  TOBDThemeMode = (tmAuto, tmLight, tmDark);

  /// <summary>One palette. Every visual reads only these slots —
  /// the resolution chain (per-component Style ▸ Theme ▸ active
  /// VCL Style ▸ built-in brand default) populates the record.
  /// </summary>
  TOBDThemePalette = record
    /// <summary>Component background fill.</summary>
    Background:     TColor;
    /// <summary>Body / value text.</summary>
    ForegroundText: TColor;
    /// <summary>Brand accent. Default = ERDesigns brand orange.
    /// Use for "active" / "selected" / "live" highlights only —
    /// not large fills.</summary>
    Accent:         TColor;
    /// <summary>Subtle borders / dividers / disabled state.</summary>
    Subtle:         TColor;
    /// <summary>OK / pass / completed.</summary>
    Success:        TColor;
    /// <summary>Caution / approaching limit / pending.</summary>
    Warning:        TColor;
    /// <summary>Failure / overrange / fault.</summary>
    Danger:         TColor;
    /// <summary>Light neutral (panel cards on light theme,
    /// disabled controls on dark).</summary>
    NeutralLight:   TColor;
    /// <summary>Dark neutral (panel cards on dark theme,
    /// disabled controls on light).</summary>
    NeutralDark:    TColor;
    /// <summary>Gauge dial face.</summary>
    GaugeFace:      TColor;
    /// <summary>Gauge tick marks.</summary>
    GaugeTick:      TColor;
    /// <summary>Gauge needle / value indicator.</summary>
    GaugeNeedle:    TColor;
    /// <summary>Gauge label text.</summary>
    GaugeLabel:     TColor;
  end;

  /// <summary>Per-component colour / styling overrides. Each
  /// field defaults to <c>clDefault</c> (= "inherit from the
  /// resolved Theme palette"). Set any slot to override just
  /// that one — the rest still come from Theme.</summary>
  TOBDVisualStyle = record
    Background: TColor;
    Foreground: TColor;
    Accent:     TColor;
    Border:     TColor;
    /// <summary>Resets every slot to <c>clDefault</c>.</summary>
    procedure Reset;
    /// <summary>True iff at least one slot is non-default.</summary>
    function HasAny: Boolean;
  end;

const
  /// <summary>ERDesigns brand orange. BGR = clOBDOrange.</summary>
  clOBDOrange:       TColor = $00298DF5;  // RGB(245, 141, 41)
  /// <summary>ERDesigns charcoal background.</summary>
  clOBDCharcoal:     TColor = $00262626;
  /// <summary>ERDesigns silver / light card.</summary>
  clOBDSilver:       TColor = $00F0F0F0;

  /// <summary>Brand-default palette, light mode.</summary>
  BRAND_PALETTE_LIGHT: TOBDThemePalette = (
    Background:     $00FFFFFF;
    ForegroundText: $00202020;
    Accent:         $00298DF5;   // clOBDOrange
    Subtle:         $00BFBFBF;
    Success:        $004CA64C;
    Warning:        $0000A8FF;
    Danger:         $003C3CCC;
    NeutralLight:   $00F0F0F0;
    NeutralDark:    $00606060;
    GaugeFace:      $00FAFAFA;
    GaugeTick:      $00404040;
    GaugeNeedle:    $00298DF5;
    GaugeLabel:     $00303030;
  );

  /// <summary>Brand-default palette, dark mode.</summary>
  BRAND_PALETTE_DARK: TOBDThemePalette = (
    Background:     $00262626;   // clOBDCharcoal
    ForegroundText: $00F0F0F0;
    Accent:         $00298DF5;
    Subtle:         $00505050;
    Success:        $0066CC66;
    Warning:        $0000B8FF;
    Danger:         $004444E0;
    NeutralLight:   $00606060;
    NeutralDark:    $00D0D0D0;
    GaugeFace:      $00303030;
    GaugeTick:      $00B0B0B0;
    GaugeNeedle:    $00298DF5;
    GaugeLabel:     $00D0D0D0;
  );

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

/// <summary>Picks <c>AOverride</c> when it isn't
/// <c>clDefault</c>; otherwise returns <c>AInherit</c>.
/// One-liner for every resolution step inside paint code.</summary>
function PickColor(AOverride, AInherit: TColor): TColor; inline;

implementation

uses
  Vcl.Themes;

{ TOBDVisualStyle ------------------------------------------------------------ }

procedure TOBDVisualStyle.Reset;
begin
  Background := clDefault;
  Foreground := clDefault;
  Accent     := clDefault;
  Border     := clDefault;
end;

function TOBDVisualStyle.HasAny: Boolean;
begin
  Result := (Background <> clDefault) or
            (Foreground <> clDefault) or
            (Accent     <> clDefault) or
            (Border     <> clDefault);
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

function PickColor(AOverride, AInherit: TColor): TColor;
begin
  if AOverride <> clDefault then Result := AOverride
  else                          Result := AInherit;
end;

end.
