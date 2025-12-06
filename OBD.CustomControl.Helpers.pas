//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.Helpers.pas
// CONTENTS       : Shared Skia helper functions for custom components
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 15/07/2024
//------------------------------------------------------------------------------
unit OBD.CustomControl.Helpers;

interface

uses
  WinApi.Windows, System.SysUtils, System.Types, System.UITypes, System.Skia,
  Vcl.Graphics, Vcl.Themes;



//------------------------------------------------------------------------------
// FUNCTIONS
//------------------------------------------------------------------------------
/// <summary>
///   Resolves themed VCL colors to a concrete RGB value for drawing operations.
/// </summary>
function GetAppropriateColor(Color: TColor): TColor;
/// <summary>
///   Converts a themed VCL color to a Skia alpha color value.
/// </summary>
function SafeColorRefToSkColor(Color: TColor): TAlphaColor;
/// <summary>
///   Resolves the active style background fill color for Skia clears.
/// </summary>
function ResolveStyledBackgroundColor(const DefaultColor: TColor): TAlphaColor;
/// <summary>
///   Creates a Skia typeface that honors the weight and slant of a VCL font.
/// </summary>
function CreateSkTypeface(const AFont: TFont): ISkTypeface;
/// <summary>
///   Builds a Skia font instance from a VCL font for consistent glyph metrics.
/// </summary>
function CreateSkFont(const AFont: TFont): ISkFont;
/// <summary>
///   Measures text dimensions with Skia using the supplied font and alignment.
/// </summary>
function MeasureSkText(const Text: string; const AFont: TFont; const ATextAlign: TSkTextAlign = TSkTextAlign.Left): TSizeF;
/// <summary>
///   Trims and ellipsizes text so it fits within the requested width using Skia measurements.
/// </summary>
function FitTextToWidth(const Text: string; const AFont: TFont; const MaxWidth: Single): string;
/// <summary>
///   Draws text centered vertically within a rectangle using Skia font rendering.
/// </summary>
procedure DrawSkTextCentered(const Canvas: ISkCanvas; const Text: string; const AFont: TFont; const Rect: TRectF; const Color: TColor; const Align: TSkTextAlign = TSkTextAlign.Center);
/// <summary>
///   Creates a Skia rounded rectangle path with the specified corner radius.
/// </summary>
function CreateRoundRectPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Builds a Skia glare path for a rounded rectangle overlay.
/// </summary>
function CreateGlareRoundRectPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Constructs a Skia path representing a back-arrow button shape.
/// </summary>
function CreateBackButtonPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Creates the glare overlay path for the back-arrow button shape.
/// </summary>
function CreateGlareBackButtonPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Builds the left tab shape path for custom tab rendering.
/// </summary>
function CreateTabLeftPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Builds the glare overlay for the left tab shape.
/// </summary>
function CreateGlareTabLeftPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Builds the center tab shape path for a flat tab body.
/// </summary>
function CreateTabCenterPath(const Rect: TRectF): ISkPath;
/// <summary>
///   Builds the glare overlay for a center tab shape.
/// </summary>
function CreateGlareTabCenterPath(const Rect: TRectF): ISkPath;
/// <summary>
///   Builds the right tab shape path for mirrored tab corners.
/// </summary>
function CreateTabRightPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Builds the glare overlay for the right tab shape.
/// </summary>
function CreateGlareTabRightPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Builds the main battery outline path used by status indicators.
/// </summary>
function CreateBatteryPath(const Rect: TRectF; Corner: Single): ISkPath;
/// <summary>
///   Creates a fill path for the battery percentage region.
/// </summary>
function CreateBatteryPercentagePath(const Rect: TRectF; Percentage: Single; Distance: Single = 2): ISkPath;
/// <summary>
///   Builds a glare overlay path for the battery percentage region.
/// </summary>
function CreateGlareBatteryPercentagePath(const Rect: TRectF; Percentage: Single; Distance: Single = 2): ISkPath;
/// <summary>
///   Computes the glare rectangle used for the battery percentage fill.
/// </summary>
function CreateGlareBatteryPercentageRect(const Rect: TRectF; Percentage: Single; Distance: Single = 2): TRectF;
/// <summary>
///   Generates the vehicle battery pictogram path using Skia geometry.
/// </summary>
function CreateVehicleBatteryPath(const Rect: TRectF): ISkPath;
/// <summary>
///   Generates the J1962 connector pictogram path with Skia primitives.
/// </summary>
function CreateJ1962Path(const Rect: TRectF): ISkPath;
/// <summary>
///   Generates the internet globe pictogram path with Skia curves.
/// </summary>
function CreateInternetGlobePath(const Rect: TRectF): ISkPath;
/// <summary>
///   Generates the communication protocol pictogram path with Skia lines.
/// </summary>
function CreateProtocolPath(const Rect: TRectF): ISkPath;
/// <summary>
///   Builds a dotted square size grip path for resize hints using Skia rectangles.
/// </summary>
function CreateSizeGripPathSquare(const Rect: TRectF; DotSize: Single; DotSpacing: Single): ISkPath;
/// <summary>
///   Builds a dotted circular size grip path for resize hints using Skia ovals.
/// </summary>
function CreateSizeGripPathCircle(const Rect: TRectF; DotSize: Single; DotSpacing: Single): ISkPath;
/// <summary>
///   Builds a Skia round-rectangle path for reuse across Skia-accelerated painters.
/// </summary>
function CreateSkRoundRectPath(const Rect: TRectF; Corner: Single): ISkPath;

implementation

const
  /// <summary>
  ///   Skia font style width constant for normal width (used in TSkFontStyle.Create)
  /// </summary>
  SKIA_FONT_WIDTH_NORMAL = 5;
  /// <summary>
  ///   Skia font weight for normal text (400)
  /// </summary>
  SKIA_FONT_WEIGHT_NORMAL = 400;
  /// <summary>
  ///   Skia font weight for bold text (700)
  /// </summary>
  SKIA_FONT_WEIGHT_BOLD = 700;

//------------------------------------------------------------------------------
// GET APPROPRIATE COLOR (THEMED COLOR IF THEMES ENABLED)
//------------------------------------------------------------------------------
function GetAppropriateColor(Color: TColor): TColor;
begin
  // Check if VCL styles are enabled
  if StyleServices.Enabled then
    Result := StyleServices.GetSystemColor(Color)
  else
    Result := Color;
end;

//------------------------------------------------------------------------------
// CONVERT COLOR TO SKIA COLOR
//------------------------------------------------------------------------------
function SafeColorRefToSkColor(Color: TColor): TAlphaColor;
var
  RGBColor: COLORREF;
begin
  // Convert the color, honoring VCL styles so the Skia output matches themed colors
  RGBColor := ColorToRGB(GetAppropriateColor(Color));
  // Compose a fully opaque alpha color (Skia expects premultiplied ARGB ordering)
  Result := TAlphaColor($FF000000 or (GetRValue(RGBColor) shl 16) or (GetGValue(RGBColor) shl 8) or GetBValue(RGBColor));
end;

//------------------------------------------------------------------------------
// RESOLVE STYLED BACKGROUND COLOR TO SKIA
//------------------------------------------------------------------------------
function ResolveStyledBackgroundColor(const DefaultColor: TColor): TAlphaColor;
var
  StyledColor: TColor;
begin
  // Default to the provided color when no style color is available
  StyledColor := DefaultColor;

  // Try to pull the fill color from the active style so Skia clears match themed surfaces
  if TStyleManager.IsCustomStyleActive then
    StyleServices.GetElementColor(StyleServices.GetElementDetails(twWindowRoot), ecFillColor, StyledColor);

  // Convert the resolved VCL color into a Skia alpha color for fast clearing
  Result := SafeColorRefToSkColor(StyledColor);
end;

//------------------------------------------------------------------------------
// CREATE SKIA TYPEFACE FROM VCL FONT
//------------------------------------------------------------------------------
function CreateSkTypeface(const AFont: TFont): ISkTypeface;
var
  Weight: Integer;
  Slant: TSkFontSlant;
begin
  Weight := SKIA_FONT_WEIGHT_NORMAL;
  if fsBold in AFont.Style then
    Weight := SKIA_FONT_WEIGHT_BOLD;

  Slant := TSkFontSlant.Upright;
  if fsItalic in AFont.Style then
    Slant := TSkFontSlant.Italic;

  Result := TSkTypeface.MakeFromName(AFont.Name, TSkFontStyle.Create(Weight, SKIA_FONT_WIDTH_NORMAL, Slant));
end;

//------------------------------------------------------------------------------
// CREATE SKIA FONT FROM VCL FONT
//------------------------------------------------------------------------------
function CreateSkFont(const AFont: TFont): ISkFont;
begin
  // Build a Skia font from the matching typeface so downstream measurements stay consistent
  Result := TSkFont.Create(CreateSkTypeface(AFont), AFont.Size);
end;

//------------------------------------------------------------------------------
// MEASURE TEXT WITH SKIA
//------------------------------------------------------------------------------
function MeasureSkText(const Text: string; const AFont: TFont; const ATextAlign: TSkTextAlign = TSkTextAlign.Left): TSizeF;
var
  TextPaint: ISkPaint;
  TextFont: ISkFont;
  Metrics: TSkFontMetrics;
  Bounds: TRectF;
begin
  TextPaint := TSkPaint.Create;
  TextPaint.AntiAlias := True;
  TextPaint.Style := TSkPaintStyle.Fill;

  TextFont := TSkFont.Create(CreateSkTypeface(AFont), AFont.Size);
  TextFont.GetMetrics(Metrics);

  // Measure text width using MeasureText method
  TextFont.MeasureText(Text, Bounds, TextPaint);
  Result := TSizeF.Create(Bounds.Width, Metrics.Descent - Metrics.Ascent);
end;

//------------------------------------------------------------------------------
// FIT TEXT TO WIDTH WITH ELLIPSIS
//------------------------------------------------------------------------------
function FitTextToWidth(const Text: string; const AFont: TFont; const MaxWidth: Single): string;
var
  Candidate: string;
begin
  Candidate := Text;
  if MeasureSkText(Candidate, AFont).Width <= MaxWidth then
    Exit(Candidate);

  while (Candidate <> '') and (MeasureSkText(Candidate + '…', AFont).Width > MaxWidth) do
    Delete(Candidate, Length(Candidate), 1);

  if Candidate = '' then
    Result := ''
  else
    Result := Candidate + '…';
end;

//------------------------------------------------------------------------------
// DRAW CENTERED TEXT WITH SKIA
//------------------------------------------------------------------------------
procedure DrawSkTextCentered(const Canvas: ISkCanvas; const Text: string; const AFont: TFont; const Rect: TRectF; const Color: TColor; const Align: TSkTextAlign = TSkTextAlign.Center);
var
  Paint: ISkPaint;
  SkFont: ISkFont;
  Metrics: TSkFontMetrics;
  TextHeight: Single;
  TextBounds: TRectF;
  X, Y: Single;
begin
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := SafeColorRefToSkColor(Color);

  SkFont := TSkFont.Create(CreateSkTypeface(AFont), AFont.Size);
  SkFont.GetMetrics(Metrics);
  TextHeight := Metrics.Descent - Metrics.Ascent;

  // Measure text to calculate alignment
  SkFont.MeasureText(Text, TextBounds, Paint);

  case Align of
    TSkTextAlign.Center:
      X := Rect.Left + (Rect.Width - TextBounds.Width) / 2;
    TSkTextAlign.Right:
      X := Rect.Right - TextBounds.Width;
  else
    X := Rect.Left;
  end;

  Y := Rect.Top + ((Rect.Height - TextHeight) / 2) - Metrics.Ascent;
  Canvas.DrawSimpleText(Text, X, Y, SkFont, Paint);
end;

//------------------------------------------------------------------------------
// BUILD SKIA ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateRoundRectPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  PathBuilder: ISkPathBuilder;
  RoundRect: ISkRoundRect;
begin
  // Build a Skia round-rectangle using a reusable path builder
  PathBuilder := TSkPathBuilder.Create;
  RoundRect := TSkRoundRect.Create(Rect, Corner, Corner);
  PathBuilder.AddRoundRect(RoundRect);
  // Export the immutable Skia path for the caller
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA GLARE ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateGlareRoundRectPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  PathBuilder: ISkPathBuilder;
  GlareRect: TRectF;
  RoundRect: ISkRoundRect;
begin
  // Restrict the glare overlay to the upper half of the round-rectangle
  GlareRect := TRectF.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Top + (Rect.Height / 2));
  PathBuilder := TSkPathBuilder.Create;
  RoundRect := TSkRoundRect.Create(GlareRect, Corner, Corner);
  PathBuilder.AddRoundRect(RoundRect);
  // Finalize the glare path for Skia drawing
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA BACK BUTTON PATH
//------------------------------------------------------------------------------
function CreateBackButtonPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  ArrowRect: TRectF;
  PathBuilder: ISkPathBuilder;
  RoundRect: TSkRoundRect;
  BodyRect: TRectF;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Main round-rectangle body using half-width to keep the arrow head crisp
  BodyRect := TRectF.Create(Rect.Left, Rect.Top, Rect.Left + (Rect.Width / 2), Rect.Bottom);
  RoundRect := TSkRoundRect.Create(BodyRect, Corner, Corner);
  PathBuilder.AddRoundRect(RoundRect);

  // Arrow overlay centered vertically on the right half
  ArrowRect := TRectF.Create(
    Rect.Left + (Rect.Width / 2) - (Corner / 2),
    Rect.Top + (Rect.Height / 2) - (Corner * 1.5),
    Rect.Left + (Rect.Width / 2),
    Rect.Top + (Rect.Height / 2) + (Corner * 1.5)
  );
  PathBuilder.MoveTo(ArrowRect.Right, ArrowRect.Top);
  PathBuilder.LineTo(ArrowRect.Right, ArrowRect.Bottom);
  PathBuilder.LineTo(ArrowRect.Left, ArrowRect.Top + (ArrowRect.Height / 2));
  PathBuilder.Close;

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA BACK BUTTON GLARE PATH
//------------------------------------------------------------------------------
function CreateGlareBackButtonPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  ArrowRect: TRectF;
  GlareRect: TRectF;
  PathBuilder: ISkPathBuilder;
  RoundRect: TSkRoundRect;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Glare only spans the top half of the button body
  GlareRect := TRectF.Create(Rect.Left, Rect.Top, Rect.Left + (Rect.Width / 2), Rect.Top + (Rect.Height / 2));
  RoundRect := TSkRoundRect.Create(GlareRect, Corner, Corner);
  PathBuilder.AddRoundRect(RoundRect);

  // Arrow sheen running horizontally through the arrow center
  ArrowRect := TRectF.Create(
    Rect.Left + (Rect.Width / 2) - (Corner / 2),
    Rect.Top + (Rect.Height / 2) - (Corner * 1.5),
    Rect.Left + (Rect.Width / 2),
    Rect.Top + (Rect.Height / 2) + (Corner * 1.5)
  );
  PathBuilder.MoveTo(ArrowRect.Right, ArrowRect.Top + (ArrowRect.Height / 2));
  PathBuilder.LineTo(ArrowRect.Left, ArrowRect.Top + (ArrowRect.Height / 2));
  PathBuilder.Close;

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA TAB LEFT PATH
//------------------------------------------------------------------------------
function CreateTabLeftPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  PathBuilder: ISkPathBuilder;
  RoundRect: ISkRoundRect;
  Radii: TSkRoundRectRadii;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Round only the left corners while keeping the right edge straight
  RoundRect := TSkRoundRect.Create;
  Radii[TSkRoundRectCorner.UpperLeft] := PointF(Corner, Corner);
  Radii[TSkRoundRectCorner.UpperRight] := PointF(0, 0);
  Radii[TSkRoundRectCorner.LowerRight] := PointF(0, 0);
  Radii[TSkRoundRectCorner.LowerLeft] := PointF(Corner, Corner);
  RoundRect.SetRect(Rect, Radii);
  PathBuilder.AddRoundRect(RoundRect);
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA GLARE TAB LEFT PATH
//------------------------------------------------------------------------------
function CreateGlareTabLeftPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  GlareRect: TRectF;
  PathBuilder: ISkPathBuilder;
  RoundRect: ISkRoundRect;
  Radii: TSkRoundRectRadii;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Glare region only covers the upper half of the tab
  GlareRect := TRectF.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Top + (Rect.Height / 2));
  RoundRect := TSkRoundRect.Create;
  Radii[TSkRoundRectCorner.UpperLeft] := PointF(Corner, Corner);
  Radii[TSkRoundRectCorner.UpperRight] := PointF(0, 0);
  Radii[TSkRoundRectCorner.LowerRight] := PointF(0, 0);
  Radii[TSkRoundRectCorner.LowerLeft] := PointF(Corner, Corner);
  RoundRect.SetRect(GlareRect, Radii);
  PathBuilder.AddRoundRect(RoundRect);
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA TAB CENTER PATH
//------------------------------------------------------------------------------
function CreateTabCenterPath(const Rect: TRectF): ISkPath;
var
  PathBuilder: ISkPathBuilder;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Center tab is a simple rectangle with squared edges
  PathBuilder.AddRect(Rect);
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA GLARE TAB CENTER PATH
//------------------------------------------------------------------------------
function CreateGlareTabCenterPath(const Rect: TRectF): ISkPath;
var
  PathBuilder: ISkPathBuilder;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Glare runs across the top half of the center tab
  PathBuilder.AddRect(TRectF.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Top + (Rect.Height / 2)));
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA TAB RIGHT PATH
//------------------------------------------------------------------------------
function CreateTabRightPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  PathBuilder: ISkPathBuilder;
  RoundRect: ISkRoundRect;
  Radii: TSkRoundRectRadii;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Round only the right corners while keeping the left edge straight
  RoundRect := TSkRoundRect.Create;
  Radii[TSkRoundRectCorner.UpperLeft] := PointF(0, 0);
  Radii[TSkRoundRectCorner.UpperRight] := PointF(Corner, Corner);
  Radii[TSkRoundRectCorner.LowerRight] := PointF(Corner, Corner);
  Radii[TSkRoundRectCorner.LowerLeft] := PointF(0, 0);
  RoundRect.SetRect(Rect, Radii);
  PathBuilder.AddRoundRect(RoundRect);
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA GLARE TAB RIGHT PATH
//------------------------------------------------------------------------------
function CreateGlareTabRightPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  GlareRect: TRectF;
  PathBuilder: ISkPathBuilder;
  RoundRect: ISkRoundRect;
  Radii: TSkRoundRectRadii;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Glare region only covers the upper half of the tab
  GlareRect := TRectF.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Top + (Rect.Height / 2));
  RoundRect := TSkRoundRect.Create;
  Radii[TSkRoundRectCorner.UpperLeft] := PointF(0, 0);
  Radii[TSkRoundRectCorner.UpperRight] := PointF(Corner, Corner);
  Radii[TSkRoundRectCorner.LowerRight] := PointF(Corner, Corner);
  Radii[TSkRoundRectCorner.LowerLeft] := PointF(0, 0);
  RoundRect.SetRect(GlareRect, Radii);
  PathBuilder.AddRoundRect(RoundRect);
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA BATTERY PATH
//------------------------------------------------------------------------------
function CreateBatteryPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  BatteryTopRect: TRectF;
  H, W, X, Y, BW, BH: Single;
  PathBuilder: ISkPathBuilder;
  RoundRect: TSkRoundRect;
  BodyRect: TRectF;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.Left;
  Y := (Rect.Top + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery "top" dimensions
  BW := Rect.Width * 0.1;
  BH := H * 0.5;

  PathBuilder := TSkPathBuilder.Create;

  // Main battery body
  BodyRect := TRectF.Create(X, Y, X + W, Y + H);
  RoundRect := TSkRoundRect.Create(BodyRect, Corner, Corner);
  PathBuilder.AddRoundRect(RoundRect);

  // Battery post
  BatteryTopRect := TRectF.Create(X + W, Y + (BH / 2), X + W + BW, Y + (BH / 2) + BH);
  PathBuilder.AddRect(BatteryTopRect);

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA BATTERY PERCENTAGE PATH
//------------------------------------------------------------------------------
function CreateBatteryPercentagePath(const Rect: TRectF; Percentage: Single; Distance: Single = 2): ISkPath;
var
  BatterypercentageRect: TRectF;
  H, W, X, Y: Single;
  PathBuilder: ISkPathBuilder;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.Left;
  Y := (Rect.Top + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  BatterypercentageRect := TRectF.Create(
    X + Distance,
    Y + Distance,
    X + Distance + (((W - (Distance * 2)) / 100) * Percentage),
    Y + Distance + (H - (Distance * 2))
  );

  PathBuilder := TSkPathBuilder.Create;
  PathBuilder.AddRect(BatterypercentageRect);
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA GLARE BATTERY PERCENTAGE PATH
//------------------------------------------------------------------------------
function CreateGlareBatteryPercentagePath(const Rect: TRectF; Percentage: Single; Distance: Single = 2): ISkPath;
var
  BatterypercentageRect: TRectF;
  H, W, X, Y: Single;
  PathBuilder: ISkPathBuilder;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.Left;
  Y := (Rect.Top + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  BatterypercentageRect := TRectF.Create(
    X + Distance,
    Y + Distance,
    X + Distance + (((W - (Distance * 2)) / 100) * Percentage),
    Y + Distance + ((H - (Distance * 2)) / 2)
  );

  PathBuilder := TSkPathBuilder.Create;
  PathBuilder.AddRect(BatterypercentageRect);
  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA GLARE BATTERY PERCENTAGE RECT
//------------------------------------------------------------------------------
function CreateGlareBatteryPercentageRect(const Rect: TRectF; Percentage: Single; Distance: Single = 2): TRectF;
var
  H, W, X, Y: Single;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.Left;
  Y := (Rect.Top + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  Result := TRectF.Create(
    X + Distance,
    Y + Distance,
    X + Distance + (((W - (Distance * 2)) / 100) * Percentage),
    Y + Distance + ((H - (Distance * 2)) / 2)
  );
end;

//------------------------------------------------------------------------------
// BUILD SKIA VEHICLE BATTERY PATH
//------------------------------------------------------------------------------
function CreateVehicleBatteryPath(const Rect: TRectF): ISkPath;
var
  X, Y, H, PW, PH, PS: Single;
  PathBuilder: ISkPathBuilder;
begin
  // Initialize the path builder for the battery outline and poles
  PathBuilder := TSkPathBuilder.Create;

  X := Rect.Left;
  Y := Rect.Top + (Rect.Height / 2);

  // Battery box height
  H := Rect.Height * 0.6;

  // Pole width
  PW := Rect.Width / 4;
  // Pole height
  PH := Rect.Height / 8;
  // Space next and between poles
  PS := (Rect.Width / 2) / 3;

  // Start with the left line
  PathBuilder.MoveTo(X, Y + (H / 2));
  PathBuilder.LineTo(X, Y - (H / 2));

  // Move to the right
  X := X + (Rect.Width / 2);

  // Add vertical line for housing
  PathBuilder.MoveTo(X, Y - (H / 2));
  PathBuilder.LineTo(X, Y + (H / 2));

  // Add left post
  PathBuilder.MoveTo((Rect.Left + Rect.Width) - PW, Y - (H / 2) - PH);
  PathBuilder.LineTo((Rect.Left + Rect.Width) - PW, Y - (H / 2));
  PathBuilder.LineTo((Rect.Left + Rect.Width), Y - (H / 2));
  PathBuilder.LineTo((Rect.Left + Rect.Width), Y - (H / 2) - PH);
  PathBuilder.LineTo((Rect.Left + Rect.Width) - PW, Y - (H / 2) - PH);

  // Add right post
  PathBuilder.MoveTo((Rect.Left + Rect.Width) - PW - PS, Y - (H / 2) - PH);
  PathBuilder.LineTo((Rect.Left + Rect.Width) - PW - PS, Y - (H / 2));
  PathBuilder.LineTo((Rect.Left + Rect.Width) - PS, Y - (H / 2));
  PathBuilder.LineTo((Rect.Left + Rect.Width) - PS, Y - (H / 2) - PH);
  PathBuilder.LineTo((Rect.Left + Rect.Width) - PW - PS, Y - (H / 2) - PH);

  // Add line below the battery
  PathBuilder.MoveTo(Rect.Left, Y + (H / 2));
  PathBuilder.LineTo(Rect.Left + Rect.Width, Y + (H / 2));

  // Draw the triangle below the battery
  PathBuilder.MoveTo(Rect.Left + (Rect.Width / 2), Y + Rect.Height);
  PathBuilder.LineTo(Rect.Left + (Rect.Width / 4), Y + (Rect.Height / 2));
  PathBuilder.LineTo(Rect.Left + ((Rect.Width / 4) * 3), Y + (Rect.Height / 2));
  PathBuilder.LineTo(Rect.Left + (Rect.Width / 2), Y + Rect.Height);

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// GET J1962 PATH
//------------------------------------------------------------------------------
function CreateJ1962Path(const Rect: TRectF): ISkPath;
var
  I: Integer;
  LinePoints: array [0 .. 9] of TPointF;
  PathBuilder: ISkPathBuilder;
  PostRect: TRectF;
  Spacing: Single;
  TopRect: TRectF;
begin
  // Create a new path and start from the top left, just below the triangle
  PathBuilder := TSkPathBuilder.Create;
  PathBuilder.MoveTo(Rect.Left, Rect.Top + (Rect.Height / 4));

  // Left line (top)
  PathBuilder.LineTo(Rect.Left + Rect.Width / 3, Rect.Top + (Rect.Height / 8));

  // Upper-left triangle to center top
  PathBuilder.LineTo(Rect.Left + Rect.Width / 3, Rect.Top);
  PathBuilder.LineTo(Rect.Left + (Rect.Width / 2), Rect.Top + (Rect.Height / 8));

  // Upper line to top right
  PathBuilder.LineTo((Rect.Left + Rect.Width) - (Rect.Width / 3), Rect.Top + (Rect.Height / 8));

  // Upper-right triangle to center top
  PathBuilder.LineTo((Rect.Left + Rect.Width) - (Rect.Width / 3), Rect.Top);
  PathBuilder.LineTo((Rect.Left + Rect.Width), Rect.Top + (Rect.Height / 8));

  // Right line (top)
  PathBuilder.LineTo((Rect.Left + Rect.Width), Rect.Top + (Rect.Height / 4));

  // Right line (bottom)
  PathBuilder.LineTo((Rect.Left + Rect.Width) - (Rect.Width / 3), Rect.Top + (Rect.Height / 2));

  // Lower-right triangle to center bottom
  PathBuilder.LineTo((Rect.Left + Rect.Width) - (Rect.Width / 3), Rect.Top + ((Rect.Height / 2) + (Rect.Height / 8)));
  PathBuilder.LineTo((Rect.Left + Rect.Width / 2), Rect.Top + (Rect.Height / 2));

  // Lower line to bottom left
  PathBuilder.LineTo(Rect.Left + Rect.Width / 3, Rect.Top + (Rect.Height / 2));

  // Lower-left triangle to center bottom
  PathBuilder.LineTo(Rect.Left + Rect.Width / 3, Rect.Top + ((Rect.Height / 2) + (Rect.Height / 8)));
  PathBuilder.LineTo(Rect.Left, Rect.Top + (Rect.Height / 2));

  // Left line back to start
  PathBuilder.LineTo(Rect.Left, Rect.Top + (Rect.Height / 4));

  // Create the top holes
  TopRect := TRectF.Create(Rect.Left + (Rect.Width / 4), Rect.Top + 10, Rect.Left + ((Rect.Width / 4) * 3), Rect.Top + (Rect.Height / 4) - 10);
  Spacing := TopRect.Width / 9;

  // Draw the top holes
  for I := 0 to 9 do
  begin
    LinePoints[I] := TPointF.Create(TopRect.Left + (Spacing * I), TopRect.CenterPoint.Y);
  end;

  for I := 0 to 4 do
  begin
    PathBuilder.AddRect(TRectF.Create(LinePoints[I].X - 2, LinePoints[I].Y - 2, LinePoints[I].X + 2, LinePoints[I].Y + 2));
  end;

  for I := 5 to 9 do
  begin
    PathBuilder.AddRect(TRectF.Create(LinePoints[I].X - 2, LinePoints[I].Y + 4, LinePoints[I].X + 2, LinePoints[I].Y + 8));
  end;

  // Create the bottom holes
  Spacing := Rect.Width / 7;

  for I := 0 to 6 do
  begin
    LinePoints[I] := TPointF.Create(Rect.Left + (Spacing * I), Rect.Top + ((Rect.Height / 2) + (Rect.Height / 6)));
  end;

  for I := 0 to 3 do
  begin
    PathBuilder.AddRect(TRectF.Create(LinePoints[I].X - 2, LinePoints[I].Y - 2, LinePoints[I].X + 2, LinePoints[I].Y + 2));
  end;

  for I := 4 to 6 do
  begin
    PathBuilder.AddRect(TRectF.Create(LinePoints[I].X - 2, LinePoints[I].Y + 4, LinePoints[I].X + 2, LinePoints[I].Y + 8));
  end;

  // Center post
  PostRect := Rect;
  PostRect.Left := Rect.Left + (Rect.Width / 2) - 2;
  PostRect.Right := PostRect.Left + 4;
  PostRect.Top := PostRect.Top + (Rect.Height / 4);
  PathBuilder.AddRect(PostRect);

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// GET INTERNET GLOBE PATH
//------------------------------------------------------------------------------
function CreateInternetGlobePath(const Rect: TRectF): ISkPath;
var
  H1: Single;
  H2: Single;
  PathBuilder: ISkPathBuilder;
  W1: Single;
  W2: Single;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Draw the outer circle as the globe boundary
  PathBuilder.AddOval(Rect);

  // Two vertical lines dividing the globe
  W1 := Rect.Left + (Rect.Width / 4);
  W2 := Rect.Left + ((Rect.Width / 4) * 3);

  PathBuilder.MoveTo(W1, Rect.Top);
  PathBuilder.LineTo(W1, Rect.Top + Rect.Height);

  PathBuilder.MoveTo(W2, Rect.Top);
  PathBuilder.LineTo(W2, Rect.Top + Rect.Height);

  // Two horizontal lines dividing the globe
  H1 := Rect.Top + (Rect.Height / 3);
  H2 := Rect.Top + ((Rect.Height / 3) * 2);

  PathBuilder.MoveTo(Rect.Left, H1);
  PathBuilder.LineTo(Rect.Left + Rect.Width, H1);

  PathBuilder.MoveTo(Rect.Left, H2);
  PathBuilder.LineTo(Rect.Left + Rect.Width, H2);

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// GET PROTOCOL PATH
//------------------------------------------------------------------------------
function CreateProtocolPath(const Rect: TRectF): ISkPath;
var
  PathBuilder: ISkPathBuilder;
  StrokeDistance: Single;
  X1: Single;
  X2: Single;
  Y1: Single;
  Y2: Single;
begin
  PathBuilder := TSkPathBuilder.Create;
  // Determine the diagonal stroke positions relative to the rectangle
  StrokeDistance := Rect.Height / 4;
  X1 := Rect.Left + StrokeDistance;
  Y1 := Rect.Top + StrokeDistance;
  X2 := (Rect.Left + Rect.Width) - StrokeDistance;
  Y2 := (Rect.Top + Rect.Height) - StrokeDistance;

  // Bottom-left stroke
  PathBuilder.MoveTo(Rect.Left, Y1);
  PathBuilder.LineTo(Rect.Left, Y2);
  PathBuilder.LineTo(X1, Y2);

  // Top-left stroke
  PathBuilder.MoveTo(X1, Rect.Top);
  PathBuilder.LineTo(Rect.Left, Rect.Top);
  PathBuilder.LineTo(Rect.Left, Y1);

  // Top-right stroke
  PathBuilder.MoveTo(X2, Rect.Top);
  PathBuilder.LineTo(Rect.Left + Rect.Width, Rect.Top);
  PathBuilder.LineTo(Rect.Left + Rect.Width, Y1);

  // Bottom-right stroke
  PathBuilder.MoveTo(Rect.Left + Rect.Width, Y1);
  PathBuilder.LineTo(Rect.Left + Rect.Width, Y2);
  PathBuilder.LineTo(X2, Y2);

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA SIZE GRIP PATH (SQUARE DOTS)
//------------------------------------------------------------------------------
function CreateSizeGripPathSquare(const Rect: TRectF; DotSize: Single; DotSpacing: Single): ISkPath;
var
  I, J: Integer;
  StartX, StartY, PosX, PosY: Single;
  TotalWidth, TotalHeight: Single;
  PathBuilder: ISkPathBuilder;
begin
  PathBuilder := TSkPathBuilder.Create;

  // Calculate the total width and height so we can position
  // the X and Y positions.
  TotalWidth := ((DotSize + DotSpacing) * 3);
  TotalHeight := ((DotSize + DotSpacing) * 3);

  // Calculate the starting X and Y positions for the grip drawing
  // This will still place the grip in the bottom right
  StartX := (Rect.Left + Rect.Width) - TotalWidth;
  StartY := (Rect.Top + (Rect.Height / 2)) - (TotalHeight / 2);

  for I := 0 to 2 do // Rows
  for J := 0 to 2 do // Columns
  if I >= J then     // Ensures the grip pattern (diagonal)
  begin
    PosX := StartX + (2 - J) * (DotSize + DotSpacing);
    PosY := StartY + I * (DotSize + DotSpacing);

    PathBuilder.AddRect(TRectF.Create(PosX, PosY, PosX + DotSize, PosY + DotSize));
  end;

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// BUILD SKIA SIZE GRIP PATH (CIRCLE DOTS)
//------------------------------------------------------------------------------
function CreateSizeGripPathCircle(const Rect: TRectF; DotSize: Single; DotSpacing: Single): ISkPath;
var
  I, J: Integer;
  StartX, StartY, PosX, PosY: Single;
  TotalWidth, TotalHeight: Single;
  PathBuilder: ISkPathBuilder;
begin
  PathBuilder := TSkPathBuilder.Create;

  // Calculate the total width and height so we can position
  // the X and Y positions.
  TotalWidth := ((DotSize + DotSpacing) * 3);
  TotalHeight := ((DotSize + DotSpacing) * 3);

  // Calculate the starting X and Y positions for the grip drawing
  // This will still place the grip in the bottom right
  StartX := (Rect.Left + Rect.Width) - TotalWidth;
  StartY := (Rect.Top + (Rect.Height / 2)) - (TotalHeight / 2);

  for I := 0 to 2 do // Rows
  for J := 0 to 2 do // Columns
  if I >= J then     // Ensures the grip pattern (diagonal)
  begin
    PosX := StartX + (2 - J) * (DotSize + DotSpacing);
    PosY := StartY + I * (DotSize + DotSpacing);

    PathBuilder.AddOval(TRectF.Create(PosX, PosY, PosX + DotSize, PosY + DotSize));
  end;

  Result := PathBuilder.Detach;
end;

//------------------------------------------------------------------------------
// GET SKIA ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateSkRoundRectPath(const Rect: TRectF; Corner: Single): ISkPath;
var
  PathBuilder: ISkPathBuilder;
  RoundRect: TSkRoundRect;
begin
  // Initialize the Skia path builder so path creation remains thread-safe and allocation-light
  PathBuilder := TSkPathBuilder.Create;
  // Prepare the round-rectangle descriptor with uniform corner radii
  RoundRect := TSkRoundRect.Create(Rect, Corner, Corner);
  // Add the round-rectangle to the builder
  PathBuilder.AddRoundRect(RoundRect);
  // Detach produces an immutable path instance that can be shared across threads safely
  Result := PathBuilder.Detach;
end;

end.
