//------------------------------------------------------------------------------
//  OBD.UI.GDIP
//
//  Shared GDI+ helpers used by every Delphi-OBD visual that
//  custom-paints with TGPGraphics. Extracted from the per-unit
//  copies that were duplicated across 26 paint units in the
//  P-A2 surface.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.GDIP;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  System.Classes,
  Vcl.Graphics;

/// <summary>Converts a VCL <c>TColor</c> to the GDI+ <c>ARGB</c>
/// representation with an optional alpha override.</summary>
function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;

/// <summary>Builds a <c>TGPRectF</c> from four floats. Convenience
/// over the field-by-field assignment used by every paint unit.
/// </summary>
function RectF(AX, AY, AW, AH: Single): TGPRectF; inline;

/// <summary>Resolves a per-component colour slot — returns the
/// explicit value when set, the supplied theme fallback when the
/// slot is <c>clDefault</c>. Centralises the `if Slot &lt;&gt;
/// clDefault then ... else ...` ternary every visual was
/// inlining.</summary>
function ResolveColor(ASlot, AFallback: TColor): TColor; inline;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte): ARGB;
var
  Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

function RectF(AX, AY, AW, AH: Single): TGPRectF;
begin
  Result.X      := AX;
  Result.Y      := AY;
  Result.Width  := AW;
  Result.Height := AH;
end;

function ResolveColor(ASlot, AFallback: TColor): TColor;
begin
  if ASlot <> clDefault then Result := ASlot
  else                       Result := AFallback;
end;

end.
