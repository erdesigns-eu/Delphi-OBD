//------------------------------------------------------------------------------
//  Tests.ERD.UI.RenderHelpers
//
//  Shared helpers for the dashboard rendering tests. A control is drawn
//  off-screen through TOBDCustomControl.RenderTo into a 32-bit bitmap and
//  the pixels are inspected, so a control that paints nothing (or paints
//  only its background) fails the suite.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//  2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit Tests.ERD.UI.RenderHelpers;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  ERD.UI.Control;

/// <summary>Draws <c>AControl</c> into a new 32-bit bitmap the size of
/// the control. The caller frees the bitmap.</summary>
/// <param name="AControl">Control to render.</param>
/// <returns>Rendered bitmap.</returns>
function RenderControl(AControl: TOBDCustomControl): TBitmap;

/// <summary>Most frequent RGB value in the bitmap, as $00RRGGBB.
/// For a dashboard control this is its background.</summary>
/// <param name="ABitmap">32-bit bitmap.</param>
/// <returns>Dominant colour.</returns>
function DominantPixel(ABitmap: TBitmap): Cardinal;

/// <summary>Number of pixels that differ from the dominant colour.</summary>
/// <param name="ABitmap">32-bit bitmap.</param>
/// <returns>Pixel count.</returns>
function InkPixels(ABitmap: TBitmap): Integer;

/// <summary>Renders <c>AControl</c> and returns the share of pixels that
/// are not background, 0..1.</summary>
/// <param name="AControl">Control to render.</param>
/// <returns>Ink ratio.</returns>
function InkRatio(AControl: TOBDCustomControl): Double;

/// <summary>Number of pixels within <c>ATolerance</c> per channel of
/// <c>AColor</c>.</summary>
/// <param name="ABitmap">32-bit bitmap.</param>
/// <param name="AColor">Colour to look for (RGB TColor).</param>
/// <param name="ATolerance">Allowed difference per channel.</param>
/// <returns>Pixel count.</returns>
function CountNear(ABitmap: TBitmap; AColor: TColor;
  ATolerance: Integer = 24): Integer;

/// <summary>Renders <c>AControl</c> and counts pixels near
/// <c>AColor</c>.</summary>
/// <param name="AControl">Control to render.</param>
/// <param name="AColor">Colour to look for (RGB TColor).</param>
/// <param name="ATolerance">Allowed difference per channel.</param>
/// <returns>Pixel count.</returns>
function RenderedNear(AControl: TOBDCustomControl; AColor: TColor;
  ATolerance: Integer = 24): Integer;

/// <summary>True when the two bitmaps differ in at least
/// <c>AMinPixels</c> pixels. Bitmaps of different size always
/// differ.</summary>
/// <param name="A">First bitmap.</param>
/// <param name="B">Second bitmap.</param>
/// <param name="AMinPixels">Minimum number of differing pixels.</param>
/// <returns>Whether the bitmaps differ.</returns>
function BitmapsDiffer(A, B: TBitmap; AMinPixels: Integer = 1): Boolean;

implementation

type
  TPixelRow = array[0..(MaxInt div 4) - 1] of Cardinal;
  PPixelRow = ^TPixelRow;

function RenderControl(AControl: TOBDCustomControl): TBitmap;
begin
  Result := TBitmap.Create;
  try
    AControl.RenderTo(Result);
  except
    Result.Free;
    raise;
  end;
end;

function DominantPixel(ABitmap: TBitmap): Cardinal;
var
  Counts: TDictionary<Cardinal, Integer>;
  Pair: TPair<Cardinal, Integer>;
  X, Y, N, Best: Integer;
  Line: PPixelRow;
  P: Cardinal;
begin
  Result := 0;
  Best := -1;
  Counts := TDictionary<Cardinal, Integer>.Create;
  try
    for Y := 0 to ABitmap.Height - 1 do
    begin
      Line := ABitmap.ScanLine[Y];
      for X := 0 to ABitmap.Width - 1 do
      begin
        P := Line[X] and $00FFFFFF;
        if Counts.TryGetValue(P, N) then
          Counts[P] := N + 1
        else
          Counts.Add(P, 1);
      end;
    end;
    for Pair in Counts do
      if Pair.Value > Best then
      begin
        Best := Pair.Value;
        Result := Pair.Key;
      end;
  finally
    Counts.Free;
  end;
end;

function InkPixels(ABitmap: TBitmap): Integer;
var
  Bg: Cardinal;
  X, Y: Integer;
  Line: PPixelRow;
begin
  Result := 0;
  Bg := DominantPixel(ABitmap);
  for Y := 0 to ABitmap.Height - 1 do
  begin
    Line := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
      if (Line[X] and $00FFFFFF) <> Bg then
        Inc(Result);
  end;
end;

function InkRatio(AControl: TOBDCustomControl): Double;
var
  Bmp: TBitmap;
  Total: Integer;
begin
  Bmp := RenderControl(AControl);
  try
    Total := Bmp.Width * Bmp.Height;
    if Total = 0 then
      Exit(0);
    Result := InkPixels(Bmp) / Total;
  finally
    Bmp.Free;
  end;
end;

function CountNear(ABitmap: TBitmap; AColor: TColor;
  ATolerance: Integer): Integer;
var
  C: TColor;
  R, G, B, X, Y: Integer;
  Line: PPixelRow;
  P: Cardinal;
begin
  Result := 0;
  C := ColorToRGB(AColor);
  R := C and $FF;
  G := (C shr 8) and $FF;
  B := (C shr 16) and $FF;
  for Y := 0 to ABitmap.Height - 1 do
  begin
    Line := ABitmap.ScanLine[Y];
    for X := 0 to ABitmap.Width - 1 do
    begin
      P := Line[X];
      if (Abs(Integer((P shr 16) and $FF) - R) <= ATolerance) and
        (Abs(Integer((P shr 8) and $FF) - G) <= ATolerance) and
        (Abs(Integer(P and $FF) - B) <= ATolerance) then
        Inc(Result);
    end;
  end;
end;

function RenderedNear(AControl: TOBDCustomControl; AColor: TColor;
  ATolerance: Integer): Integer;
var
  Bmp: TBitmap;
begin
  Bmp := RenderControl(AControl);
  try
    Result := CountNear(Bmp, AColor, ATolerance);
  finally
    Bmp.Free;
  end;
end;

function BitmapsDiffer(A, B: TBitmap; AMinPixels: Integer): Boolean;
var
  X, Y, N: Integer;
  LA, LB: PPixelRow;
begin
  if (A.Width <> B.Width) or (A.Height <> B.Height) then
    Exit(True);
  N := 0;
  for Y := 0 to A.Height - 1 do
  begin
    LA := A.ScanLine[Y];
    LB := B.ScanLine[Y];
    for X := 0 to A.Width - 1 do
      if (LA[X] and $00FFFFFF) <> (LB[X] and $00FFFFFF) then
      begin
        Inc(N);
        if N >= AMinPixels then
          Exit(True);
      end;
  end;
  Result := False;
end;

end.
